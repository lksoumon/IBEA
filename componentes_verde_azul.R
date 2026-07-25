library(terra)
library(sf)
library(dplyr)

setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")

pasta_saida_floresta <- "florestaVegetacao_cidades"
pasta_saida_agua <- "massasAgua_cidades"

# Criar pastas se não existirem
if (!dir.exists(pasta_saida_floresta)) dir.create(pasta_saida_floresta, recursive = TRUE)
if (!dir.exists(pasta_saida_agua)) dir.create(pasta_saida_agua, recursive = TRUE)

# carregar GeoJSON de municípios
urb_union <- st_read("urb_union.geojson") %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

caminho_saida <- "usoCobertura2023_10m"
if (!dir.exists(caminho_saida)) {
  dir.create(caminho_saida, recursive = TRUE)
}

raster_cobertura <- rast("../db/mapbiomas_10m_collection2_integration_v1-classification_2023.tif")

lista_codigos <- unique(urb_union$CD_MUN)

#codigo_teste <- '5107602' # apenas para testes, comentar quando rodar todo o estado
# Iniciar o loop para cada município
for (codigo_teste in lista_codigos) {
  
  print(paste0("Processando município: ", codigo_teste))
  
  cidade_teste <- urb_union %>% filter(CD_MUN == codigo_teste)
  
  if (nrow(cidade_teste) == 0) {
    warning(paste0("Não foi possível encontrar o polígono para o código: ", codigo_teste))
    next 
  }
  
  raster_recortado <- terra::crop(raster_cobertura, cidade_teste)
  raster_final <- terra::mask(raster_recortado, cidade_teste)
  
  nome_arquivo_saida_raster <- paste0(codigo_teste, ".tif")
  caminho_arquivo_saida_raster <- file.path(caminho_saida, nome_arquivo_saida_raster)
  writeRaster(raster_final, filename = caminho_arquivo_saida_raster, overwrite = TRUE)
  
  # =========================================================================
  # --- Processamento para Floresta/Vegetação (300m e Filtro de 1 ha)
  # =========================================================================
  valores_floresta <- c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50)
  raster_floresta_filtrado <- terra::ifel(raster_final %in% valores_floresta, 1, NA)
  
  poligonos_floresta <- as.polygons(raster_floresta_filtrado, na.rm = TRUE, dissolve = TRUE)
  
  if (!is.null(poligonos_floresta) && nrow(poligonos_floresta) > 0) {
    # 1. Converter para sf e reprojetar para sistema métrico (EPSG: 5880)
    sf_floresta <- st_as_sf(poligonos_floresta) %>% 
      st_make_valid() %>%
      st_transform(crs = 5880)
    
    # 2. O SEGREDO AQUI: Explodir o Multipolygon em "ilhas" (Polygons individuais)
    sf_floresta_ilhas <- suppressWarnings(st_cast(sf_floresta, "POLYGON"))
    
    # 3. Calcular a área de CADA ilha e aplicar o filtro de 1 hectare
    sf_floresta_filtrada <- sf_floresta_ilhas %>%
      mutate(area_m2 = as.numeric(st_area(.))) %>%
      filter(area_m2 >= 10000)
    
    # Verifica se restou algum polígono após o corte
    if (nrow(sf_floresta_filtrada) > 0) {
      
      # 4. Aplicar o buffer de 300 metros apenas nas ilhas validadas
      buffer_floresta <- st_buffer(sf_floresta_filtrada, dist = 300) %>% st_make_valid()
      
      # Opcional: Unir os buffers sobrepostos para limpar o GeoJSON final
      buffer_floresta <- st_union(buffer_floresta) %>% st_make_valid() %>% st_as_sf()
      
      # 5. Retornar para WGS 84 e recortar pelos limites da cidade
      buffer_floresta <- st_transform(buffer_floresta, crs = 4326)
      buffer_floresta_recortado <- suppressWarnings(st_intersection(buffer_floresta, cidade_teste))
      
      caminho_json_floresta <- file.path(pasta_saida_floresta, paste0(codigo_teste, ".geojson"))
      if (file.exists(caminho_json_floresta)) {
        file.remove(caminho_json_floresta)
      }
      st_write(buffer_floresta_recortado, caminho_json_floresta, driver = "GeoJSON", quiet = TRUE)
    } else {
      print(paste0("Áreas verdes encontradas, mas NENHUMA atingiu 1 ha no município ", codigo_teste))
    }
  } else {
    print(paste0("Nenhuma área de floresta encontrada para o município ", codigo_teste))
  }
  
  # =========================================================================
  # --- Processamento para Massas d'Água (100m)
  # =========================================================================
  valores_agua <- c(26, 33, 31)
  raster_agua_filtrado <- terra::ifel(raster_final %in% valores_agua, 1, NA)
  
  poligonos_agua <- as.polygons(raster_agua_filtrado, na.rm = TRUE, dissolve = TRUE)
  
  if (!is.null(poligonos_agua) && nrow(poligonos_agua) > 0) {
    # 1. Converter para sf e reprojetar para sistema métrico (EPSG: 5880)
    sf_agua <- st_as_sf(poligonos_agua) %>% 
      st_make_valid() %>%
      st_transform(crs = 5880)
    
    # 2. Aplicar o buffer de 100 metros reais
    buffer_agua <- st_buffer(sf_agua, dist = 100) %>% st_make_valid()
    
    # 3. Retornar para WGS 84 e recortar pelos limites da cidade
    buffer_agua <- st_transform(buffer_agua, crs = 4326)
    buffer_agua_recortado <- suppressWarnings(st_intersection(buffer_agua, cidade_teste))
    
    caminho_json_agua <- file.path(pasta_saida_agua, paste0(codigo_teste, ".geojson"))
    if (file.exists(caminho_json_agua)) {
      file.remove(caminho_json_agua)
    }
    st_write(buffer_agua_recortado, caminho_json_agua, driver = "GeoJSON", quiet = TRUE)
  } else {
    print(paste0("Nenhuma massa d'água encontrada para o município ", codigo_teste))
  }
  
  print(paste0("Processamento concluído para o município ", codigo_teste))
  
  # Limpar a memória no final de cada loop
  rm(list = ls(pattern = "^raster_recortado$|^raster_final$|^raster_floresta_filtrado$|^poligonos_floresta$|^sf_floresta$|^buffer_floresta$|^buffer_floresta_recortado$|^raster_agua_filtrado$|^poligonos_agua$|^sf_agua$|^buffer_agua$|^buffer_agua_recortado$"))
  gc()
}

print("Processamento de todos os municípios finalizado!")
