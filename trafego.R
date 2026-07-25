library(dplyr)
library(readr)
library(terra)
library(sf)
library(ggplot2)
library(leaflet)

# Define o diretório de trabalho do banco de dados
setwd("C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db")

# 1. Leitura dos dados de entrada
vias_MT <- st_read("MT_VIAS.geojson") 

urb_union <- st_read("urb_union.geojson") %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

# Cria a lista iterável com todos os códigos de municípios presentes no recorte urbano
lista_codigos <- unique(urb_union$CD_MUN)

# 2. Início do Loop: Processamento individualizado por município
for (municipio_atual in lista_codigos) {
  
  # Filtra o polígono urbano correspondente à iteração atual
  dados_cidade <- urb_union %>% filter(CD_MUN == municipio_atual)
  
  # Alinha o Sistema de Referência de Coordenadas (CRS) das vias com o do polígono da cidade
  vias_MT <- st_transform(vias_MT, st_crs(dados_cidade))
  
  # Realiza o corte espacial: retém apenas os segmentos de vias que interceptam a área urbana
  vias_cidade <- st_intersection(vias_MT, dados_cidade)
  
  # 3. Classificação e Atribuição de Pesos do IBEA
  vias_cidade <- vias_cidade %>%
    mutate(valor = case_when(
      highway %in% c("motorway", "motorway_link")   ~ -1.0,
      highway %in% c("trunk", "trunk_link")         ~ -0.8,
      highway %in% c("primary", "primary_link")     ~ -0.65,
      highway %in% c("secondary", "secondary_link") ~ -0.5,
      highway %in% c("tertiary", "tertiary_link")   ~ -0.3,
      TRUE ~ NA_real_        # Demais categorias recebem NA e serão ignoradas
    ))
  
  # Remove as geometrias que não entraram na hierarquia do índice
  vias_class <- vias_cidade %>% filter(!is.na(valor))
  
  # Verifica se sobraram vias; se não houver, pula para o próximo município
  if(nrow(vias_class) == 0) next
  
  # 4. Geração dos Buffers de Impacto
  # Reprojeção para o sistema métrico SIRGAS 2000 / UTM zone 22S (EPSG:31982)
  # Essencial para que o argumento de distância ('dist') do st_buffer represente metros reais
  vias_class_m <- st_transform(vias_class, 31982) 
  
  # Cria a área de influência de 150m (impacto sonoro e do ar)
  vias_buffer <- st_buffer(vias_class_m, dist = 150)
  
  # Simplifica o objeto mantendo apenas a coluna de interesse ('valor')
  vias_buffer <- vias_buffer %>% select(valor)
  
  # 5. Rasterização
  # Garante que o polígono delimitador da cidade também esteja no CRS métrico
  dados_cidade_m <- st_transform(dados_cidade, 31982)
  
  # Cria um raster base (molde) usando a extensão e o limite do município
  # Resolução espacial definida para 30x30 metros. Inicia com impacto zero.
  r_base <- rast(dados_cidade_m, resolution = 30)
  values(r_base) <- 0
  
  # Transfere os valores do buffer vetorial para os pixels do raster base.
  # fun = "min": Regra crucial para cruzamentos. Se uma motorway (-1) e uma tertiary (-0.3) 
  # se sobrepõem, o pixel assumirá -1.
  r_vias <- rasterize(vias_buffer, r_base, field = "valor", fun = "min")
  
  # O rasterize preenche áreas fora dos buffers com NA (Not Available).
  # A função cover() substitui esses NAs pelo valor do r_base (que é 0).
  r_final <- cover(r_vias, r_base)
  
  # Usa o contorno exato do município como máscara, cortando eventuais 
  # excessos retangulares gerados pela extensão (bbox) do raster base.
  r_final <- mask(r_final, vect(dados_cidade_m))
  
  # 6. Exportação do dado
  # Salva o resultado final em TIF para posterior integração com outros componentes do índice
  writeRaster(r_final, paste0("trafego_cidades/", municipio_atual, ".tif"), overwrite = TRUE)
}