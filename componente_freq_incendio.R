# ========================================
# 1️⃣ Configurações iniciais
# ========================================
library(terra)
library(sf)
library(dplyr)
library(jsonlite)

# Definir diretório de trabalho
setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")

# Criar pasta de saída se não existir
if (!dir.exists("incendio_cidades")) dir.create("incendio_cidades")

# ========================================
# 2️⃣ Carregar dados
# ========================================

# Municípios (EPSG:4326)
urb_union <- st_read("urb_union.geojson") %>% #dados com os setores censitários urbanos do estado, já previamente calculados e salvos em script anterior
  st_make_valid()

# Raster de queimadas
raster_data <- rast("../db/freqIncendio/mapbiomas_fire_col4_br_fire_frequency_1985_2024.tif")

# JSON para resultados
arquivo_json <- "incendio_cidades/indice_municipio.json"
if (file.exists(arquivo_json)) {
  dados <- fromJSON(arquivo_json, simplifyVector = FALSE)
} else {
  dados <- list()
}

# Matriz de reclassificação
intervalos <- c(0, 0.1, 5, 9, 19, 38, Inf)
categorias <- c(0, -0.1, -0.25, -0.5, -0.75, -1)
matriz_reclass <- cbind(intervalos[-length(intervalos)], intervalos[-1], categorias)

# ========================================
# 3️⃣ Loop de Processamento (Sem Buffer)
# ========================================

for(cod_cidade in unique(urb_union$CD_MUN)) {
  
  # 1. Filtrar cidade e projetar para o CRS do raster
  cidade <- urb_union %>% filter(CD_MUN == cod_cidade)
  cidade_proj <- st_transform(cidade, crs(raster_data))
  
  # 2. Recortar e Mascarar (apenas a área da cidade)
  # mask = TRUE garante que pixels fora do polígono virem NA
  r_crop <- crop(raster_data, cidade_proj, mask = TRUE)
  
  # Verificar se há dados no recorte
  if (all(is.na(values(r_crop)))) {
    cat("Cidade:", cod_cidade, "| Sem dados de incêndio.\n")
    next
  }
  
  # 3. Reclassificar
  r_classificado <- classify(r_crop, matriz_reclass, include.lowest = TRUE)
  
  # 4. Extrair estatísticas (Pior valor dentro da cidade)
  v_minmax <- minmax(r_classificado)
  pior_valor <- as.numeric(v_minmax[1])
  
  # 5. Salvar no JSON
  dados[[cod_cidade]] <- list(
    pior_indice_local = pior_valor,
    CD_MUN = cod_cidade
  )
  write(toJSON(dados, pretty = TRUE, auto_unbox = TRUE), arquivo_json)
  
  # 6. Salvar Raster (com compressão para evitar erro de espaço e trava de arquivo)
  caminho_tif <- paste0("incendio_cidades/", cod_cidade, ".tif")
  
  # Tenta deletar se já existir (resolve o erro de overwrite)
  if (file.exists(caminho_tif)) file.remove(caminho_tif)
  
  # Projeta para 4326 antes de salvar
  r_final <- project(r_classificado, "EPSG:4326", method = "near")
  
  writeRaster(r_final, caminho_tif, 
              overwrite = TRUE, 
              gdal = c("COMPRESS=LZW")) # LZW diminui muito o tamanho do arquivo
  
  cat("Cidade:", cod_cidade, "| Pior Risco Local:", pior_valor, "\n")
}

print("Processamento concluído!")
