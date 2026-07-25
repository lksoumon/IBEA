# Carregar pacotes
library(terra)
library(sf)
library(dplyr)
library(quarto)
library(jsonlite)
library(stringr) 
library(readr)
library(digest) # Necessário para gerar os hashes dos arquivos
library(spdep)  # Necessário para estatística espacial (Índice de Moran)

# Defina seu diretório
setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")
mapa_municipios <- jsonlite::fromJSON("municipios.json")

# =========================================================================
# CONFIGURAÇÃO E LOG DE PROVENIÊNCIA (CORREÇÃO 3)
# =========================================================================
dir.create("logs", showWarnings = FALSE)
arquivo_log <- paste0("logs/execucao_ibea_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")

# Registro inicial de metadados da execução
meta_info <- list(
  data_execucao = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  ambiente = list(
    r_version = R.version.string,
    pacotes = list(
      terra = as.character(packageVersion("terra")),
      sf = as.character(packageVersion("sf")),
      dplyr = as.character(packageVersion("dplyr")),
      spdep = as.character(packageVersion("spdep")),
      quarto = as.character(packageVersion("quarto"))
    )
  ),
  parametros_ibea = list(
    lambda = 0.5,
    epsilon = 0.4,
    tau = -0.2,
    resolucao = 0.00027, #30x30
    crs_alvo = 4326
  ),
  municipios = list()
)

# =========================================================================
# SUBSEÇÃO DE HARMONIZAÇÃO ESPACIAL - Referências
# =========================================================================
urb_union <- st_read("urb_union.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

lista_codigos <- unique(urb_union$CD_MUN)
lista_codigos <- c('5107602') # MUNICÍPIO DE TESTE

lista_ibea_estado <- list()
lista_estado_completo <- list() 

# --- FUNÇÕES DE APOIO ---

carregar_e_corrigir <- function(caminho, referencia_vetorial, tipo_dado = "near") {
  if (!file.exists(caminho)) return(NULL)
  r <- rast(caminho)
  if (is.na(crs(r)) || crs(r) == "") crs(r) <- crs(referencia_vetorial)
  if (crs(r) != crs(referencia_vetorial)) r <- project(r, crs(referencia_vetorial), method = tipo_dado)
  return(r)
}

# FUNÇÃO DE VALIDAÇÃO COM LOGS
validar_componente <- function(r, nome) {
  if (is.null(r)) {
    cat(sprintf("  [-] %s: Ausente ou vazio.\n", nome))
    return(NULL)
  }
  
  if (nlyr(r) != 1) {
    stop(sprintf("\n[FALHA] %s: Esperado 1 banda, encontrado %d.", nome, nlyr(r)))
  }
  
  vals <- minmax(r)
  
  if (any(is.infinite(vals))) {
    stop(sprintf("\n[FALHA] %s: O arquivo contém valores infinitos.", nome))
  }
  
  if (nome != "Temperatura (Contínua)") {
    if (!is.na(vals[1,1]) && !is.na(vals[2,1])) {
      if (vals[1,1] < -1.001 || vals[2,1] > 1.001) {
        stop(sprintf("\n[FALHA] %s: Valores fora do limite [-1, 1]. Min: %f | Max: %f", nome, vals[1,1], vals[2,1]))
      }
    }
  }
  
  cat(sprintf("  [+] %s validado com sucesso (Min: %.2f, Max: %.2f).\n", nome, vals[1,1], vals[2,1]))
  return(r)
}

criar_banda_vazia <- function(template, nome_banda) {
  r_vazia <- rast(template)
  values(r_vazia) <- NA
  names(r_vazia) <- nome_banda
  return(r_vazia)
}

rasterizar_geojson <- function(pasta, id, template, nome_campo) {
  arquivo <- paste0(pasta, "/", id, '.geojson')
  if (!file.exists(arquivo)) return(NULL)
  
  vetor <- st_read(arquivo, quiet = TRUE)
  if (nrow(vetor) == 0) return(NULL)
  
  if (!(nome_campo %in% names(vetor))) {
    stop(sprintf("Erro: O campo '%s' não existe no arquivo '%s'.", nome_campo, arquivo))
  }
  if (!is.numeric(vetor[[nome_campo]])) {
    stop(sprintf("Erro: O campo '%s' no arquivo '%s' não é numérico.", nome_campo, arquivo))
  }
  if (st_crs(vetor) != st_crs(template)) {
    vetor <- st_transform(vetor, crs = crs(template))
  }
  
  r_out <- rasterize(vect(vetor), template, field = nome_campo, background = NA)
  return(r_out)
}

dadosCenso <- read_csv2("censo/Agregados_por_setores_demografia_BR.csv", locale = locale(encoding = "UTF-8"))
dadosCenso <- dadosCenso %>% select(CD_setor, V01006)
Setores <- st_read("MT_setores_CD2022.gpkg")
Setores <- Setores %>% filter(SITUACAO=='Urbana')

# --- LOOP PRINCIPAL ---
for (municipio_atual in lista_codigos) {
  
  cat(paste0("\n", paste(rep("-", 50), collapse = ""), "\n"))
  cat(paste0("Processando Município: ", municipio_atual, "\n"))
  
  meta_info$municipios[[as.character(municipio_atual)]] <- list(status = "Iniciado", falhas = list())
  
  tryCatch({
    # =========================================================================
    # CORREÇÃO DO CENSO
    # =========================================================================
    dadosCenso <- dadosCenso %>% mutate(CD_setor = as.character(CD_setor))
    Setores <- Setores %>% mutate(CD_SETOR = as.character(CD_SETOR))
    
    Setores_mun <- Setores %>% filter(CD_MUN == municipio_atual)
    setores_bairro_IDs <- Setores_mun$CD_SETOR
    
    censo_bairro <- dadosCenso %>% 
      filter(CD_setor %in% setores_bairro_IDs) %>% 
      distinct(CD_setor, .keep_all = TRUE)
    
    areas_mun <- Setores_mun %>% 
      as.data.frame() %>% 
      select(CD_SETOR, AREA_KM2) %>% 
      distinct(CD_SETOR, .keep_all = TRUE)
    
    censo_bairro <- censo_bairro %>%
      left_join(areas_mun, by = c("CD_setor" = "CD_SETOR")) %>%
      mutate(
        Pop_limpa = as.numeric(gsub("[^0-9.-]", "", V01006)),
        AREA_limpa = as.numeric(gsub(",", ".", AREA_KM2)),
        Densidade_Pop_m2 = if_else(is.na(Pop_limpa) | is.na(AREA_limpa) | AREA_limpa == 0, 
                                   0, Pop_limpa / AREA_limpa)
      )
    
    setores_sem_dados_demograficos <- censo_bairro %>% 
      filter(is.na(Pop_limpa) | is.na(AREA_limpa) | AREA_limpa == 0)
    
    if(nrow(setores_sem_dados_demograficos) > 0) {
      cat(sprintf("  [Aviso] Excluindo %d setores por ausência de dados populacionais no Censo.\n", nrow(setores_sem_dados_demograficos)))
      meta_info$municipios[[as.character(municipio_atual)]]$setores_excluidos_censo <- setores_sem_dados_demograficos$CD_setor
    }
    
    censo_bairro_valido <- censo_bairro %>% 
      filter(!is.na(Pop_limpa) & !is.na(AREA_limpa) & AREA_limpa > 0)
    
    Setores_mun_espacial <- Setores_mun %>%
      inner_join(select(censo_bairro_valido, CD_setor, Densidade_Pop_m2), by = c("CD_SETOR" = "CD_setor"))
    
    poligono_municipio <- urb_union[urb_union$CD_MUN == municipio_atual, ]
    ref_vect <- vect(poligono_municipio)
    
    # =========================================================================
    # CRIAÇÃO DO TEMPLATE FIXO
    # =========================================================================
    path_temp <- paste0("temperatura_solo_cidades/", municipio_atual, '.tif')
    
    if (file.exists(path_temp)) {
      r_base <- carregar_e_corrigir(path_temp, ref_vect, tipo_dado = "bilinear")
      template_raster <- rast(r_base)
    } else {
      template_raster <- rast(ext(ref_vect), crs = crs(ref_vect), resolution = 0.00027)
    }
    
    values(template_raster) <- NA
    
    r_densidade <- rasterize(vect(Setores_mun_espacial), template_raster, 
                             field = "Densidade_Pop_m2", background = NA)
    
    names(r_densidade) <- "DENSIDADE_HAB_KM2"
    
    # =========================================================================
    # CARREGAMENTO COM MÉTODOS AJUSTADOS
    # =========================================================================
    cat("  Validando componentes...\n")
    db_rasters <- list()
    
    db_rasters[['incendio']]  <- validar_componente(carregar_e_corrigir(paste0("incendio_cidades/", municipio_atual, '.tif'), ref_vect, "bilinear"), "Incêndios")
    db_rasters[['trafego']]   <- validar_componente(carregar_e_corrigir(paste0("trafego_cidades/", municipio_atual, '.tif'), ref_vect, "bilinear"), "Tráfego")
    db_rasters[['impacto_q']] <- validar_componente(carregar_e_corrigir(paste0("impacto_cidades/", municipio_atual, '_q.tif'), ref_vect, "bilinear"), "Impacto Químico")
    db_rasters[['impacto_f']] <- validar_componente(carregar_e_corrigir(paste0("impacto_cidades/", municipio_atual, '_f.tif'), ref_vect, "bilinear"), "Impacto Físico")
    db_rasters[['impacto_b']] <- validar_componente(carregar_e_corrigir(paste0("impacto_cidades/", municipio_atual, '_b.tif'), ref_vect, "bilinear"), "Impacto Biológico")
    
    if (file.exists(path_temp)) {
      r_temp <- carregar_e_corrigir(path_temp, ref_vect, tipo_dado = "bilinear")
      db_rasters[['temperatura']] <- validar_componente(r_temp, "Temperatura (Contínua)")
    } else {
      db_rasters[['temperatura']] <- NULL
    }
    
    db_rasters[['acesso_agua']] <- validar_componente(rasterizar_geojson("acesso_agua_cidades", municipio_atual, template_raster, "soma_valor"), "Acesso à Água")
    db_rasters[['arborizacao']] <- validar_componente(rasterizar_geojson("arborização_cidades", municipio_atual, template_raster, "soma_valor"), "Arborização")
    db_rasters[['esgoto']]      <- validar_componente(rasterizar_geojson("esgoto_cidades", municipio_atual, template_raster, "soma_valor"), "Esgoto")
    db_rasters[['floresta']]    <- validar_componente(rasterizar_geojson("florestaVegetacao_cidades", municipio_atual, template_raster, "mapbiomas_10m_collection2_integration_v1.classification_2023"), "Floresta")
    db_rasters[['lixo']]        <- validar_componente(rasterizar_geojson("lixo_cidades", municipio_atual, template_raster, "soma_valor"), "Coleta de Lixo")
    db_rasters[['massas_agua']] <- validar_componente(rasterizar_geojson("massasAgua_cidades", municipio_atual, template_raster, "mapbiomas_10m_collection2_integration_v1.classification_2023"), "Massas d'Água")
    
    todas_bandas <- c('incendio', 'trafego', 'impacto_q', 'impacto_f', 'impacto_b', 
                      'temperatura', 'acesso_agua', 'arborizacao', 'esgoto', 
                      'floresta', 'lixo', 'massas_agua')
    
    camadas_categoricas <- c("floresta", "massas_agua")
    
    db_final_alinhado <- list()
    for (nome in todas_bandas) {
      if (is.null(db_rasters[[nome]])) {
        db_final_alinhado[[nome]] <- criar_banda_vazia(template_raster, nome)
      } else {
        metodo_resample <- ifelse(nome %in% camadas_categoricas, "near", "bilinear")
        db_final_alinhado[[nome]] <- resample(db_rasters[[nome]], template_raster, method=metodo_resample)
        names(db_final_alinhado[[nome]]) <- nome
      }
    }
    
    if (!is.null(db_final_alinhado[['temperatura']])) {
      rcl_matrix <- matrix(c(-Inf,29,0, 
                             29,30,-0.1, 
                             30,31,-0.2, 
                             31,32,-0.3, 
                             32,33,-0.4, 
                             33,34.5,-0.5, 
                             34.5,35,-0.6, 
                             35,35.5,-0.7, 
                             35.5,36,-0.8, 
                             36,36.5,-0.9, 
                             36.5,Inf,-1), 
                           ncol = 3, byrow = TRUE)
      db_final_alinhado[['temperatura']] <- classify(db_final_alinhado[['temperatura']], rcl_matrix, 
                                                     right = FALSE, include.lowest = TRUE)
    }
    
    camadas_eventos_ou_cobertura <- c("incendio", "trafego", "impacto_q", "impacto_f", "impacto_b", "floresta", "massas_agua")
    
    b_zerado <- list()
    for (nome in names(db_final_alinhado)) {
      if (nome %in% camadas_eventos_ou_cobertura) {
        b_zerado[[nome]] <- classify(db_final_alinhado[[nome]], cbind(NA, 0))
      } else {
        b_zerado[[nome]] <- db_final_alinhado[[nome]]
      }
    }
    
    idx_quimico <- (  (1 * b_zerado[['trafego']]) + (1 * b_zerado[['impacto_q']]) +
                        (1 * b_zerado[['incendio']]) + (1 * b_zerado[['acesso_agua']]) +
                        (1 * b_zerado[['esgoto']]) + (1 * b_zerado[['lixo']]) +
                        (1 * b_zerado[['arborizacao']]) + (1 * b_zerado[['floresta']]) ) / 8
    
    idx_fisico <- (   (1 * b_zerado[['trafego']]) + (1 * b_zerado[['impacto_f']]) +
                        (1 * b_zerado[['temperatura']]) + (1 * b_zerado[['arborizacao']]) +
                        (1 * b_zerado[['massas_agua']]) + (1 * b_zerado[['floresta']]) ) / 6
    
    idx_biologico <- ((1 * b_zerado[['impacto_b']]) + (1 * b_zerado[['acesso_agua']]) +
                        (1 * b_zerado[['esgoto']]) + (1 * b_zerado[['lixo']]) +
                        (1 * b_zerado[['massas_agua']]) ) / 5
    
    lambda <- meta_info$parametros_ibea$lambda   
    epsilon <- meta_info$parametros_ibea$epsilon  
    tau <- meta_info$parametros_ibea$tau    
    
    w_q <- 8 / 19
    w_f <- 6 / 19
    w_b <- 5 / 19
    
    ibea_media <- (w_q * idx_quimico) + (w_f * idx_fisico) + (w_b * idx_biologico)
    
    variancia <- (w_q * (idx_quimico - ibea_media)^2) + 
      (w_f * (idx_fisico - ibea_media)^2) + 
      (w_b * (idx_biologico - ibea_media)^2)
    
    penalidade <- variancia / (abs(ibea_media) + epsilon)
    ibea_pen <- ibea_media - (lambda * penalidade)
    
    esgoto_raster <- b_zerado[['esgoto']]
    agua_raster <- b_zerado[['acesso_agua']]
    lixo_raster <- b_zerado[['lixo']]
    
    condicao_critica <- (esgoto_raster <= tau) | (agua_raster <= tau) | (lixo_raster <= tau)
    valor_critico <- min(esgoto_raster,agua_raster,lixo_raster)*0.1053
    
    ibea_veto <- ifel(condicao_critica, 
                      min(ibea_pen,valor_critico), 
                      ibea_pen)
    
    ibea_geral <- ifel(ibea_veto > 1, 1, ifel(ibea_veto < -1, -1, ibea_veto))
    
    vals_ibea <- minmax(ibea_geral)
    if(any(vals_ibea < -1.01) || any(vals_ibea > 1.01)) {
      stop(sprintf("Teste unitário falhou: IBEA_GERAL fora dos limites [-1, 1]. Min: %f, Max: %f", vals_ibea[1], vals_ibea[2]))
    }
    
    names(ibea_geral) <- "IBEA_GERAL"
    names(idx_quimico) <- "QUIMICO"
    names(idx_fisico) <- "FISICO"
    names(idx_biologico) <- "BIOLOGICO"
    
    log_dens <- log(r_densidade + 1)
    max_log_dens <- max(minmax(log_dens)[2,1], 1, na.rm = TRUE)
    
    dens_norm_log <- log_dens / max_log_dens
    ibea_ponderado_pop <- ibea_geral * dens_norm_log
    names(ibea_ponderado_pop) <- "IBEA_PONDERADO_POP"
    
    stack_completo <- c(ibea_geral, idx_quimico, idx_fisico, idx_biologico, rast(b_zerado), r_densidade, ibea_ponderado_pop)
    
    cat("  Finalizando e salvando TIFF...\n")
    stack_final_masked <- mask(stack_completo, ref_vect) %>% crop(ref_vect)
    
    dir.create("IBEA_cidades", showWarnings = FALSE)
    output_file <- paste0("IBEA_cidades/", municipio_atual, ".tif")
    
    writeRaster(stack_final_masked, output_file, 
                overwrite = TRUE, 
                datatype = "FLT4S", 
                gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"),
                NAflag = -9999)
    
    pixels_validos <- as.numeric(global(stack_final_masked[["IBEA_GERAL"]], "notNA"))
    hash_saida <- digest::digest(output_file, file = TRUE, algo = "sha256")
    
    meta_info$municipios[[as.character(municipio_atual)]]$pixels_validos <- pixels_validos
    meta_info$municipios[[as.character(municipio_atual)]]$hash_saida <- hash_saida
    meta_info$municipios[[as.character(municipio_atual)]]$status <- "Processado com Sucesso"
    
    cat(paste("  [OK] Raster gerado.\n"))
    
    # =========================================================================
    # CORREÇÃO AC-08: ESTATÍSTICA ESPACIAL E AGREGAÇÃO
    # =========================================================================
    cat("  Extraindo dados agregados e calculando estatística espacial...\n")
    dir.create("ibea_estatisticas", showWarnings = FALSE)
    
    nome_cidade <- mapa_municipios$nome[mapa_municipios$codigo == as.character(municipio_atual)]
    if(length(nome_cidade) == 0 || is.na(nome_cidade)) {
      nome_cidade <- as.character(municipio_atual)
    } else {
      nome_cidade <- stringr::str_to_title(tolower(nome_cidade))
    }
    
    camadas_interesse <- c("IBEA_GERAL", "FISICO", "QUIMICO", "BIOLOGICO", "DENSIDADE_HAB_KM2", "IBEA_PONDERADO_POP")
    stack_indices <- subset(stack_final_masked, camadas_interesse)
    
    # A Unidade Estatística passa a ser rigorosamente o Setor Censitário
    vetor_setores <- vect(Setores_mun_espacial)
    extracao_setores <- terra::extract(stack_indices, vetor_setores, fun=mean, na.rm=TRUE)
    df_filtrado <- cbind(CD_SETOR = Setores_mun_espacial$CD_SETOR, extracao_setores)
    
    if(nrow(df_filtrado) > 0) {
      arquivo_csv <- paste0("ibea_estatisticas/dados_", municipio_atual, ".csv")
      write.csv(df_filtrado, arquivo_csv, row.names = FALSE)
      
      # -----------------------------------------------------------------------
      # CÁLCULO DA AUTOCORRELAÇÃO ESPACIAL (ÍNDICE DE MORAN)
      # -----------------------------------------------------------------------
      setores_sf_resultados <- st_as_sf(vetor_setores) %>%
        left_join(df_filtrado, by = c("CD_SETOR" = "CD_SETOR")) %>%
        filter(!is.na(IBEA_GERAL)) # Remover polígonos sem valor calculado
      
      if(nrow(setores_sf_resultados) > 3) {
        tryCatch({
          # Define vizinhança espacial (Queen contiguity - bordas ou vértices)
          vizinhos <- spdep::poly2nb(setores_sf_resultados, queen = TRUE)
          pesos_espaciais <- spdep::nb2listw(vizinhos, style = "W", zero.policy = TRUE)
          
          # Testa o Índice Global de Moran para a variável principal
          teste_moran <- spdep::moran.test(setores_sf_resultados$IBEA_GERAL, pesos_espaciais, zero.policy = TRUE)
          
          cat(sprintf("  [Estatística] Moran's I: %.3f (p-valor: %.4f) | N = %d setores\n", 
                      teste_moran$estimate[1], teste_moran$p.value, nrow(setores_sf_resultados)))
          
          # Grava evidência matemática no log para justificar na dissertação
          meta_info$municipios[[as.character(municipio_atual)]]$estatistica_espacial <- list(
            unidade_amostral = "Setor Censitário",
            n_efetivo = nrow(setores_sf_resultados),
            indice_moran = teste_moran$estimate[1],
            p_valor = teste_moran$p.value
          )
        }, error = function(e) {
          cat("  [Aviso] Não foi possível calcular Índice de Moran (polígonos isolados ou erro topológico).\n")
        })
      }
      # -----------------------------------------------------------------------
      
      if("IBEA_GERAL" %in% names(df_filtrado)) {
        lista_ibea_estado[[as.character(municipio_atual)]] <- data.frame(
          Municipio = nome_cidade, 
          CD_SETOR = df_filtrado$CD_SETOR,
          IBEA_GERAL = df_filtrado$IBEA_GERAL,
          IBEA_PONDERADO_POP = df_filtrado$IBEA_PONDERADO_POP,
          DENSIDADE_HAB_KM2 = df_filtrado$DENSIDADE_HAB_KM2
        )
      }
      
      df_completo_extracao <- terra::extract(stack_final_masked, vetor_setores, fun=mean, na.rm=TRUE)
      df_completo <- cbind(CD_SETOR = Setores_mun_espacial$CD_SETOR, df_completo_extracao)
      
      if(nrow(df_completo) > 0) {
        df_completo <- data.frame(Municipio = nome_cidade, df_completo)
        lista_estado_completo[[as.character(municipio_atual)]] <- df_completo
      }
      
      nome_html <- paste0("relatorio_", municipio_atual, ".html")
      
      tryCatch({
        quarto::quarto_render(
          input = "modelo_relatorio.qmd",
          execute_params = list(municipio = nome_cidade, arquivo_dados = arquivo_csv),
          output_file = nome_html,
          quiet = TRUE 
        )
        
        if (file.exists(nome_html)) {
          file.rename(from = nome_html, to = paste0("ibea_estatisticas/", nome_html))
          cat(paste("  [OK] Relatório HTML gerado.\n"))
        } else {
          meta_info$municipios[[as.character(municipio_atual)]]$falhas <- append(meta_info$municipios[[as.character(municipio_atual)]]$falhas, "Relatório HTML não encontrado")
        }
      }, error = function(e) {
        meta_info$municipios[[as.character(municipio_atual)]]$falhas <- append(meta_info$municipios[[as.character(municipio_atual)]]$falhas, paste("Erro Quarto:", e$message))
      })
      
    } else {
      cat(paste("  [!] Sem dados válidos para extração em", nome_cidade, ".\n"))
    }
  }, error = function(e) {
    cat(sprintf("\n  [ERRO CRÍTICO] Falha ao processar município %s: %s\n", municipio_atual, e$message))
    meta_info$municipios[[as.character(municipio_atual)]]$status <- "Falhou"
    meta_info$municipios[[as.character(municipio_atual)]]$falhas <- paste("Erro de processamento:", e$message)
  })
  
  objetos_para_limpar <- c("db_rasters", "db_final_alinhado", "b_zerado", 
                           "stack_completo", "stack_final_masked", "r_densidade", 
                           "template_raster", "df_filtrado", "df_completo", "stack_indices", 
                           "dens_norm", "ibea_ponderado_pop", "extracao_setores", "df_completo_extracao",
                           "vetor_setores", "setores_sf_resultados", "pesos_espaciais", "vizinhos")
  rm(list = intersect(objetos_para_limpar, ls()))
  
  gc()
  terra::tmpFiles(remove = TRUE)
}

# =========================================================================
# GERAÇÃO DO RELATÓRIO ESTADUAL E EXPORTAÇÃO DE LOG
# =========================================================================
cat("\nGerando relatório consolidado do Estado...\n")
if(length(lista_ibea_estado) > 0) {
  df_estado <- bind_rows(lista_ibea_estado)
  arquivo_estado_csv <- "ibea_estatisticas/dados_estado.csv"
  write.csv(df_estado, arquivo_estado_csv, row.names = FALSE)
  
  nome_html_estado <- "relatorio_estado_comparativo.html"
  
  tryCatch({
    quarto::quarto_render(
      input = "modelo_relatorio_estado.qmd",
      execute_params = list(arquivo_dados = arquivo_estado_csv),
      output_file = nome_html_estado,
      quiet = TRUE
    )
    
    if (file.exists(nome_html_estado)) {
      file.rename(from = nome_html_estado, to = paste0("ibea_estatisticas/", nome_html_estado))
      cat("[OK] Relatório estadual gerado com sucesso!\n")
    } else {
      cat("[ERRO] Arquivo HTML do estado não foi criado.\n")
    }
  }, error = function(e) {
    cat(paste("[ERRO] Falha ao renderizar relatório estadual:", e$message, "\n"))
  })
}

meta_info$data_conclusao <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
write_json(meta_info, arquivo_log, pretty = TRUE, auto_unbox = TRUE)
cat(paste("\n[OK] Log de proveniência salvo em:", arquivo_log, "\n"))
