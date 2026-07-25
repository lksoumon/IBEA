library(terra)
library(sf)
library(dplyr)

setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")



# --- 1. Carregar e Mesclar os Rasters (apenas uma vez) ---

cat("Carregando e mesclando os arquivos raster. Por favor, aguarde...\n")

# caminhos para os 4 arquivos raster obtidos do GEE
caminho_raster_1 <- "temperatura_solo/LST_MT_2024-0000000000-0000000000.tif"
caminho_raster_2 <- "temperatura_solo/LST_MT_2024-0000000000-0000023296.tif"
caminho_raster_3 <- "temperatura_solo/LST_MT_2024-0000023296-0000000000.tif"
caminho_raster_4 <- "temperatura_solo/LST_MT_2024-0000023296-0000023296.tif"

# Abra os rasters
raster_1 <- rast(caminho_raster_1)
raster_2 <- rast(caminho_raster_2)
raster_3 <- rast(caminho_raster_3)
raster_4 <- rast(caminho_raster_4)

# Mescla os 4 rasters em um só
raster_mesclado <- merge(raster_1, raster_2, raster_3, raster_4)

cat("Rasters mesclados com sucesso!\n\n")

# --- 2. Define a função de processamento ---
# A função agora recebe o raster mesclado como argumento
processar_raster_por_municipio <- function(municipio_sf, raster_principal, pasta_saida) {
  
  # A função espera uma geometria (objeto 'sf') de apenas um município.
  # Extraímos o código do município do objeto sf.
  # Se o seu objeto sf tiver um nome de coluna diferente, ajuste aqui.
  cod_mun <- municipio_sf$CD_MUN
  
  cat(paste0("Iniciando o recorte para o município ", cod_mun, "...\n"))
  
  # Recorta (crop) o raster usando a geometria do município
  # Usa a função `crop()` para recortar pela extensão do município
  # E depois a função `mask()` para recortar exatamente pela forma da geometria
  raster_recortado <- crop(raster_principal, municipio_sf)
  raster_recortado_mascarado <- mask(raster_recortado, municipio_sf)
  
  # Define o nome e o caminho do arquivo de saída
  # Crie a pasta de saída se ela não existir
  if (!dir.exists(pasta_saida)) {
    dir.create(pasta_saida, recursive = TRUE)
  }
  
  caminho_saida <- file.path(pasta_saida, paste0(cod_mun, ".tif"))
  
  # Salva o novo raster recortado
  writeRaster(raster_recortado_mascarado, caminho_saida, overwrite = TRUE)
  
  cat(paste0("Recorte concluído e salvo em: ", caminho_saida, "\n\n"))
}

# --- 3. função em um loop ---


Setores <- st_read("urb_union.geojson")

municipios_geometria <- Setores %>%
  st_transform(crs = 4326)


# Agora, iterar sobre o dataframe de municípios
for (i in 1:nrow(municipios_geometria)) { #
  
  # Pega a linha (município) atual do dataframe
  municipio_atual <- municipios_geometria[i, ]
  #print(municipio_atual)
  # Define a pasta de saída para este município
  pasta_de_saida_municipio <- file.path("temperatura_solo_cidades")
  
  # Chama a função para processar o raster para este município
  processar_raster_por_municipio(municipio_atual, raster_mesclado, pasta_de_saida_municipio)
  
}
