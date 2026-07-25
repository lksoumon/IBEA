import pandas as pd
import geopandas as gpd
import os
import numpy as np
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_origin
from tqdm import tqdm

# --- 0. DIRETÓRIO E PASTAS ---
os.chdir('C:/Users/dhian/OneDrive/Área de Trabalho/mestrado/projeto/py/db')
os.makedirs("impacto_cidades", exist_ok=True)

# --- 1. BASE DE MUNICÍPIOS ---
gdf = gpd.read_file('MT_setores_CD2022.gpkg')
urbanos = gdf[gdf['SITUACAO'] == 'Urbana'][["CD_MUN","NM_MUN", "AREA_KM2", "geometry"]]
urb_union = urbanos.dissolve(by="CD_MUN", aggfunc="first")

# --- 2. DOWNLOAD DOS DADOS SEMA (GeoJSON direto via WFS) ---
# LICENÇA DE OPERAÇÃO PROVISÓRIA - LOP
url_lop = 'https://geo.sema.mt.gov.br/geoserver/Geoportal/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geoportal%3ASIMLAMGEO_LOP_ATIVA&authkey=541085de-9a2e-454e-bdba-eb3d57a2f492&outputFormat=application/json'
SIMLAMGEO_LOP_ATIVA = gpd.read_file(url_lop)

# LICENÇA DE OPERAÇÃO - LO
url_lo = 'https://geo.sema.mt.gov.br/geoserver/Geoportal/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Geoportal%3ASIMLAMGEO_LO_ATIVA&authkey=541085de-9a2e-454e-bdba-eb3d57a2f492&outputFormat=application/json'
SIMLAMGEO_LO_ATIVA = gpd.read_file(url_lo)

# Concatenação e Limpeza
df_all = pd.concat([SIMLAMGEO_LOP_ATIVA, SIMLAMGEO_LO_ATIVA], ignore_index=True)
df_all = df_all[["id","ATIVIDADE_LICENCIADA","ATIVIDADE_PRINCIPAL", "geometry"]]
df_all["ATIVIDADE_LICENCIADA"] = df_all["ATIVIDADE_LICENCIADA"].fillna(df_all["ATIVIDADE_PRINCIPAL"])

# --- 3. MATRIZ DE IMPACTOS ---
impactos = pd.read_csv("impactos.csv")
df_all = df_all.merge(impactos, on="ATIVIDADE_LICENCIADA", how="left")

# Trocar vírgula por ponto em todas as colunas numéricas
colunas_numericas = ["IMPACTO", "fisico", "quimico", "biologico"]
for col in colunas_numericas:
    if col in df_all.columns:
        df_all[col] = df_all[col].astype(str).str.replace(",", ".").astype(float)

# Remover empreendimentos que não possuem classificação de impacto
df_all = df_all[df_all["IMPACTO"].notna()]

# --- 4. PROJEÇÃO ESPACIAL (EPSG:5880 - Policônica do Brasil) ---
# Resolve o problema das faixas UTM 20, 21 e 22 para Mato Grosso
df_proj_points = df_all.to_crs(epsg=5880)
urb_union_proj = urb_union.to_crs(epsg=5880)

if 'CD_MUN' not in urb_union_proj.columns:
    urb_union_proj = urb_union_proj.reset_index()

# --- 5. CONFIGURAÇÕES DE RASTERIZAÇÃO ---
res = 30                # Resolução do pixel em metros
PASSOS_GRADIENTE = 20   # Qualidade da atenuação do risco
riscos_config = {
    "f": {"col": "fisico", "dist": 500},
    "q": {"col": "quimico", "dist": 1000},
    "b": {"col": "biologico", "dist": 500}
}

# --- 6. LOOP PRINCIPAL POR MUNICÍPIO ---
for idx, cidade in tqdm(urb_union_proj.iterrows(), total=len(urb_union_proj), desc="Processando cidades"):
    nome_cidade = str(cidade['CD_MUN'])
    cidade_geom = gpd.GeoDataFrame([cidade], crs=urb_union_proj.crs)
    
    # Área de busca estendida para 5km para capturar impactos externos à fronteira
    area_busca = cidade_geom.geometry.buffer(5000).union_all()
    pontos_vizinhos = df_proj_points[df_proj_points.geometry.intersects(area_busca)]

    if pontos_vizinhos.empty:
        continue

    for sufixo, config in riscos_config.items():
        coluna = config["col"]
        multiplicador = config["dist"]
        shapes_gradiente = []

        # Evita quebra de script se alguma coluna de risco estiver ausente no CSV
        if coluna not in pontos_vizinhos.columns:
            continue

        for _, ponto in pontos_vizinhos.iterrows():
            valor_original = ponto[coluna]
            
            # Pula nulos ou impactos não-negativos
            if pd.isna(valor_original) or valor_original >= 0:
                continue
            
            valor_por_camada = valor_original / PASSOS_GRADIENTE
            raio_maximo = abs(valor_original) * multiplicador
            
            for i in range(1, PASSOS_GRADIENTE + 1):
                fracao_raio = (PASSOS_GRADIENTE - i + 1) / PASSOS_GRADIENTE
                raio_atual = raio_maximo * fracao_raio
                
                if raio_atual <= 0: continue
                
                geom_buffer = ponto.geometry.buffer(raio_atual)
                shapes_gradiente.append((geom_buffer, valor_por_camada))

        if not shapes_gradiente:
            continue

        # Intersecção vetorial exata no limite administrativo do município
        gdf_camadas = gpd.GeoDataFrame(shapes_gradiente, columns=['geometry', 'valor_camada'], crs=urb_union_proj.crs)
        limite_cidade = cidade_geom.union_all()
        buffers_recortados = gdf_camadas.copy()
        buffers_recortados['geometry'] = gdf_camadas.geometry.intersection(limite_cidade)
        buffers_recortados = buffers_recortados[~buffers_recortados.geometry.is_empty]

        if buffers_recortados.empty:
            continue

        # Definição dinâmica das dimensões da matriz (Bounding Box)
        minx, miny, maxx, maxy = cidade_geom.total_bounds
        width = max(1, int((maxx - minx) / res))
        height = max(1, int((maxy - miny) / res))
        transform = from_origin(minx, maxy, res, res)

        shapes = [(geom, val) for geom, val in zip(buffers_recortados.geometry, buffers_recortados['valor_camada'])]
        
        raster_sum = rasterize(
            shapes,
            out_shape=(height, width),
            transform=transform,
            fill=0,
            dtype="float32",
            all_touched=True,
            merge_alg=rasterio.enums.MergeAlg.add
        )

        # Travamento matemático na escala negativa estrita [-1.0 a 0.0)
        raster_sum = np.clip(raster_sum, -1.0, 0.0)
        raster_sum[raster_sum == 0] = np.nan

        # Exportação
        out_path = f"impacto_cidades/{nome_cidade}_{sufixo}.tif"
        with rasterio.open(
            out_path, "w",
            driver="GTiff",
            height=height, width=width,
            count=1, dtype="float32",
            crs=cidade_geom.crs,
            transform=transform,
            nodata=np.nan
        ) as dst:
            dst.write(raster_sum, 1)
