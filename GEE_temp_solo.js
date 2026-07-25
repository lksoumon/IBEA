// 1. Carregar limite de MT
var mt = ee.FeatureCollection("FAO/GAUL/2015/level1")
  .filter(ee.Filter.eq('ADM1_NAME', 'Mato Grosso'));

// 2. Coleção Landsat 8/9 L2
var l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .filterBounds(mt)
  .filterDate('2024-01-01', '2024-12-31')
  .filter(ee.Filter.lt('CLOUD_COVER', 20));

// 3. Mosaico mediano
var image = l8.median();

// 4. LST em Kelvin -> Celsius
var lstKelvin = image.select('ST_B10').multiply(0.00341802).add(149.0);
var lstCelsius = lstKelvin.subtract(273.15).rename('LST');

// 5. Exportar para o Google Drive
Export.image.toDrive({
  image: lstCelsius.clip(mt),     // recorta para MT
  description: 'LST_MT_2025_Jun_Ago', // nome do job
  folder: 'EarthEngine',          // pasta no Google Drive
  fileNamePrefix: 'LST_MT_2025',  // prefixo do arquivo
  region: mt.geometry(),          // área de exportação
  scale: 1000,                    // resolução em metros (ajuste conforme necessário)
  crs: 'EPSG:4326',               // sistema de coordenadas
  maxPixels: 1e13                 // permite grandes áreas
});
