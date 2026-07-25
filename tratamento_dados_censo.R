library(dplyr)
library(readr)
library(terra)
library(sf)
library(ggplot2)
library(spdep)

# Diretórios
setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")

# Leitura dos dados
dadosCenso <- read_csv2("../db/censo/Agregados_por_setores_entorno_faces_BR.csv", locale = locale(encoding = "UTF-8"))
dadosCenso_cont <- read_csv2("../db/censo/Agregados_por_setores_caracteristicas_domicilio2_BR_20250417.csv", locale = locale(encoding = "UTF-8"))
Setores <- st_read("MT_setores_CD2022.gpkg")
Setores <- Setores %>% filter(SITUACAO == 'Urbana')

# Função auxiliar para preencher NA/NaN com média dos vizinhos
processar_imputacao <- function(mapa) {
  # 1. Garante que só existam geometrias válidas
  mapa <- st_make_valid(mapa)
  
  mapa$soma_valor[is.nan(mapa$soma_valor)] <- NA
  
  if (any(is.na(mapa$soma_valor))) {
    # snap = 0.0001 ajuda a "unir" fronteiras que não se tocam perfeitamente
    nb <- poly2nb(mapa, queen = TRUE, snap = 0.0001) 
    na_indices <- which(is.na(mapa$soma_valor))
    
    for (i in na_indices) {
      vizinhos <- nb[[i]]
      valores_vizinhos <- mapa$soma_valor[vizinhos]
      valores_vizinhos <- valores_vizinhos[!is.na(valores_vizinhos)]
      
      if (length(valores_vizinhos) > 0) {
        mapa$soma_valor[i] <- mean(valores_vizinhos)
      }
    }
  }
  return(mapa)
}

cod_mun <- '5107602' # '5002704' # '5107602'

processar_municipio_completo <- function(cod_mun) {
  
  # Filtra setores do município
  Setores_mun <- Setores %>% filter(CD_MUN == cod_mun)
  setores_bairro_IDs <- Setores_mun$CD_SETOR
  
  # --- 1. ARBORIZAÇÃO ---
  censo_bairro <- dadosCenso %>% filter(COD_SETOR_M22FINAL %in% setores_bairro_IDs)
  
  censo_index <- censo_bairro %>%
    mutate(across(starts_with("V054"), as.numeric)) %>%
    group_by(COD_SETOR_M22FINAL) %>%
    summarise(
      soma_valor = (0 * sum(V05430, na.rm=T) + 0.5 * sum(V05431, na.rm=T) + 0.75 * sum(V05432, na.rm=T) + 1 * sum(V05433, na.rm=T) + 0 * sum(V05434, na.rm=T)) / sum(V05400, na.rm=T)
    ) %>%
    mutate(COD_SETOR_M22FINAL = as.character(COD_SETOR_M22FINAL))
  
  mapa_setores <- Setores_mun %>%
    select(CD_SETOR, geom) %>%
    mutate(CD_SETOR = as.character(CD_SETOR)) %>%
    left_join(censo_index, by = c("CD_SETOR" = "COD_SETOR_M22FINAL")) %>%
    st_as_sf()
  
  mapa_setores <- processar_imputacao(mapa_setores)
  
  if (nrow(mapa_setores) > 0) {
    caminho_json <- file.path('arborização_cidades', paste0(cod_mun, ".geojson"))
    # 1. Tente remover o arquivo fisicamente, se ele existir
    if (file.exists(caminho_json)) {
      tryCatch({
        file.remove(caminho_json)
      }, error = function(e) {
        message("Não foi possível deletar o arquivo, talvez ele esteja aberto no QGIS.")
      })
    }
    st_write(mapa_setores, file.path('arborização_cidades', paste0(cod_mun, ".geojson")), driver = "GeoJSON", append = FALSE)
  }
  
  # --- 2. ACESSO ÁGUA ---
  censo_bairro_cont <- dadosCenso_cont %>% filter(setor %in% setores_bairro_IDs) %>%
    mutate(across(everything(), ~ ifelse(. == "X", 0, .)))
  
  # Usando rowSums para evitar NA na soma das linhas
  censo_index_agua <- censo_bairro_cont %>%
    mutate(across(V00111:V00118, as.numeric)) %>%
    mutate(V00001 = rowSums(select(., V00111:V00118), na.rm = TRUE)) %>%
    group_by(setor) %>%
    summarise(
      soma_valor = (0 * sum(V00111, na.rm=T) - 0.4 * sum(V00112, na.rm=T) - 0.8 * sum(V00113, na.rm=T) - 0.4 * sum(V00114, na.rm=T) - 0.5 * sum(V00115, na.rm=T) - 0.8 * sum(V00116, na.rm=T) - 1 * sum(V00117, na.rm=T) - 1 * sum(V00118, na.rm=T)) / sum(V00001, na.rm=T)
    ) %>%
    mutate(setor = as.character(setor))
  
  mapa_setores <- Setores_mun %>%
    select(CD_SETOR, geom) %>%
    mutate(CD_SETOR = as.character(CD_SETOR)) %>%
    left_join(censo_index_agua, by = c("CD_SETOR" = "setor")) %>%
    st_as_sf()
  
  mapa_setores <- processar_imputacao(mapa_setores)
  
  if (nrow(mapa_setores) > 0) {
    caminho_json <- file.path('acesso_agua_cidades', paste0(cod_mun, ".geojson"))
    # 1. Tente remover o arquivo fisicamente, se ele existir
    if (file.exists(caminho_json)) {
      tryCatch({
        file.remove(caminho_json)
      }, error = function(e) {
        message("Não foi possível deletar o arquivo, talvez ele esteja aberto no QGIS.")
      })
    }
    st_write(mapa_setores, file.path('acesso_agua_cidades', paste0(cod_mun, ".geojson")), driver = "GeoJSON", append = FALSE)
  }
  
  # --- 3. ESGOTO ---
  censo_index_esg <- censo_bairro_cont %>%
    mutate(across(V00309:V00316, as.numeric)) %>%
    mutate(V00001 = rowSums(select(., V00309:V00316), na.rm = TRUE)) %>%
    group_by(setor) %>%
    summarise(
      soma_valor = (0 * sum(V00309, na.rm=T) + 0 * sum(V00310, na.rm=T) - 0.2 * sum(V00311, na.rm=T) - 1 * sum(V00312, na.rm=T) - 1 * sum(V00313, na.rm=T) - 1 * sum(V00314, na.rm=T) - 1 * sum(V00315, na.rm=T) - 1 * sum(V00316, na.rm=T)) / sum(V00001, na.rm=T)
    ) %>%
    mutate(setor = as.character(setor))
  
  mapa_setores <- Setores_mun %>%
    select(CD_SETOR, geom) %>%
    mutate(CD_SETOR = as.character(CD_SETOR)) %>%
    left_join(censo_index_esg, by = c("CD_SETOR" = "setor")) %>%
    st_as_sf()
  
  mapa_setores <- processar_imputacao(mapa_setores)
  
  if (nrow(mapa_setores) > 0) {
    caminho_json <- file.path('esgoto_cidades', paste0(cod_mun, ".geojson"))
    # 1. Tente remover o arquivo fisicamente, se ele existir
    if (file.exists(caminho_json)) {
      tryCatch({
        file.remove(caminho_json)
      }, error = function(e) {
        message("Não foi possível deletar o arquivo, talvez ele esteja aberto no QGIS.")
      })
    }
    st_write(mapa_setores, file.path('esgoto_cidades', paste0(cod_mun, ".geojson")), driver = "GeoJSON", append = FALSE)
  }
  
  # --- 4. LIXO ---
  censo_index_lixo <- censo_bairro_cont %>%
    mutate(across(V00397:V00402, as.numeric)) %>%
    mutate(V00001 = rowSums(select(., V00397:V00402), na.rm = TRUE)) %>%
    group_by(setor) %>%
    summarise(
      soma_valor = (0 * sum(V00397, na.rm=T) - 0.25 * sum(V00398, na.rm=T) - 1 * sum(V00399, na.rm=T) - 1 * sum(V00400, na.rm=T) - 1 * sum(V00401, na.rm=T) - 1 * sum(V00402, na.rm=T)) / sum(V00001, na.rm=T)
    ) %>%
    mutate(setor = as.character(setor))
  
  mapa_setores <- Setores_mun %>%
    select(CD_SETOR, geom) %>%
    mutate(CD_SETOR = as.character(CD_SETOR)) %>%
    left_join(censo_index_lixo, by = c("CD_SETOR" = "setor")) %>%
    st_as_sf()
  
  mapa_setores <- processar_imputacao(mapa_setores)
  
  if (nrow(mapa_setores) > 0) {
    caminho_json <- file.path('lixo_cidades', paste0(cod_mun, ".geojson"))
    # 1. Tente remover o arquivo fisicamente, se ele existir
    if (file.exists(caminho_json)) {
      tryCatch({
        file.remove(caminho_json)
      }, error = function(e) {
        message("Não foi possível deletar o arquivo, talvez ele esteja aberto no QGIS.")
      })
    }
    
    st_write(mapa_setores, file.path('lixo_cidades', paste0(cod_mun, ".geojson")), driver = "GeoJSON", append = FALSE)
  }
  
  print(paste0("Processamento concluído para o município ", cod_mun))
}

# Execução
codigos_municipios <- unique(Setores$CD_MUN)
for (cod_mun in codigos_municipios) {
  processar_municipio_completo(cod_mun)
}
