# Builfding a prehistoric vegetation map for Brazil
# Vinicius Tonetti - vrtonetti@gmail.com

# Loading packages

library(terra)
library(tidyverse)


# Cleaning directory -----------------------------------------------------------

rm(list = ls())


# Loading layers ---------------------------------------------------------------

br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_merged.shp") # Excluded a few islands to reduce extension
mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/brazil_1985.tif") # using MapBiomas collection 09
IBGE <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/Vegetacao_5000mil/Vegetacao_5000.shp") # Downloaded from ibge.gov.br
jung <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_lvl2_ver001.tif") # Jung et al. https://zenodo.org/records/4038749


# Re projecting IBGE to WGS84 (same as MapBiomas) ------------------------------

br_wgs84 <- terra::project(br, "EPSG:4326")


#terra::writeVector(br_wgs84, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_wgs84.shp")

mb_1985_cropBR <- mask(crop(mb_1985, br_wgs84), br_wgs84)

#writeRaster(mb_1985_cropBR, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


# Cropping Jung map to Brazil --------------------------------------------------

jung_br <- mask(crop(jung, br_wgs84), br_wgs84)
#writeRaster(jung_br, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br.tif")


# Rasterizing IBGE -------------------------------------------------------------

# Cleaning directory 

rm(list = ls())


# Setting CRS to WGS84

IBGE <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/Vegetacao_5000mil/Vegetacao_5000.shp")
crs(IBGE) <- "EPSG:4326"

#writeVector(IBGE, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_WGS84.shp")

IBGE_wgs84 <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_WGS84.shp")
br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_wgs84.shp")


# Reclassifying IBGE land cover types ------------------------------------------
# Reclassification was made based on my knowledge of vegetation types, and also on visual inspection comparing with satellite images from Google in QGIS

values(IBGE_wgs84) <- values(IBGE_wgs84) %>% 
  mutate(DSC_VEG_PR = case_when(
    DSC_VEG_PR == "Estepe" ~ "grasslands",
    DSC_VEG_PR == "Floresta Estacional Semidecidual" ~ "forest",
    DSC_VEG_PR == "Estepe/Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa" ~ "forest",
    DSC_VEG_PR == "Savana" ~ "savana",
    DSC_VEG_PR == "Savana/Floresta OmbrÃ³fila  Mista" ~ "transition",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa/Floresta OmbrÃ³fila Mista" ~ "forest",
    DSC_VEG_PR == "Savana EstÃ©pica/Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Savana/Savana EstÃ©pica" ~ "savana",
    DSC_VEG_PR == "Savana-EstÃ©pica"  ~ "savana",
    DSC_VEG_PR == "Ã\u0081reas das FormaÃ§Ãµes Pioneiras"  ~ "forest",
    DSC_VEG_PR == "Floresta Estacional Decidual" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Mista" ~ "forest",
    DSC_VEG_PR == "Estepe/Floresta OmbrÃ³fila Mista" ~ "transition",
    DSC_VEG_PR == "Savana/ Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "VegetaÃ§Ã£o OmbrÃ³fila Aberta" ~ "forest",
    DSC_VEG_PR == "Floresta  OmbrÃ³fila/Floresta Estacional" ~ "forest",
    DSC_VEG_PR == "Floresta Estacional/Floresta OmbrÃ³fila Mista" ~ "forest",
    DSC_VEG_PR == "Savana/Savana EstÃ©pica/Floresta Estacional" ~ "transition",
    TRUE ~ DSC_VEG_PR)) %>% 
  mutate(DSC_VEG_PR = ifelse(is.na(DSC_VEG_PR), DSC_CLASS_, DSC_VEG_PR)) # Some values have NA values in the column "DSC_VEG_PR" and the land cover information is in the column "DSC_CLASS_". For those, I considered information in the column "DSC_CLASS_" and reclassified again as below

# Checking unique values
unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])

# Reclassifying again for values that were previously NA and, thus, copied from the column "DSC_CLASS_"

values(IBGE_wgs84) <- values(IBGE_wgs84) %>% 
  mutate(DSC_VEG_PR = case_when(
    DSC_VEG_PR == "Massa Dagua Continental" ~ "water",
    DSC_VEG_PR == "Savana-EstÃ©pica Parque" ~ "savana",
    DSC_VEG_PR == "Estepe Gramineo-Lenhosa" ~ "grasslands",
    DSC_VEG_PR == "Massa Dagua Costeira - Mar Territorial, 12 milhas" ~ "water",
    DSC_VEG_PR == "Massa Dagua Costeira - Zona Contigua, 24 milhas"  ~ "water",
    DSC_VEG_PR == "Massa Dagua Costeira - Zona EconÃ´mica Exclusiva, 200 milhas"   ~ "water",
    DSC_VEG_PR == "Ã\u0081reas das FormaÃ§Ãµes Pioneiras VegetaÃ§Ã£o com InfluÃªncia Marinha" ~ "forest",
    DSC_VEG_PR == "Estepe Arborizada" ~ "grasslands",
    DSC_VEG_PR == "Estepe Parque" ~ "grasslands",
    DSC_VEG_PR == "Estepe/Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Floresta Estacional Semidecidual Submontana" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Mista Montana" ~ "forest",
    DSC_VEG_PR == "Floresta Estacional Semidecidual Montana" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa Submontana" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Mista Alto-Montana" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa Montana" ~ "forest",
    DSC_VEG_PR == "Savana/Savana EstÃ©pica" ~ "savana",
    DSC_VEG_PR == "Savana Arborizada" ~ "savana",
    DSC_VEG_PR == "Savana EstÃ©pica/Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Savana-EstÃ©pica Arborizada" ~ "savana",
    DSC_VEG_PR == "Savana-EstÃ©pica Gramineo-Lenhosa" ~ "savana",
    DSC_VEG_PR == "Floresta Estacional Decidual Terras Baixas" ~ "forest",
    DSC_VEG_PR == "Savana-EstÃ©pica Florestada" ~ "savana",
    DSC_VEG_PR == "Savana Parque" ~ "savana",
    DSC_VEG_PR == "Savana Florestada" ~ "savana",
    DSC_VEG_PR == "Savana/ Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Floresta Estacional Decidual Submontana" ~ "forest",
    DSC_VEG_PR == "Savana Gramineo-Lenhosa" ~ "savana",
    DSC_VEG_PR == "Floresta Estacional Semidecidual Aluvial" ~ "forest",
    DSC_VEG_PR == "Ã\u0081reas das FormaÃ§Ãµes Pioneiras VegetaÃ§Ã£o com InfluÃªncia Fluvial e/ou Lacustre" ~ "forest",
    DSC_VEG_PR == "VegetaÃ§Ã£o OmbrÃ³fila Aberta Aluvial" ~ "forest",
    DSC_VEG_PR == "Campinarana Florestada" ~ "forest",
    DSC_VEG_PR == "Campinarana Gramineo-Lenhosa" ~ "savana",
    DSC_VEG_PR == "VegetaÃ§Ã£o OmbrÃ³fila Aberta Submontana" ~ "forest",
    DSC_VEG_PR == "Savana/Floresta OmbrÃ³fila" ~ "transition",
    DSC_VEG_PR == "Campinarana/Floresta OmbrÃ³fila" ~ "transition",
    DSC_VEG_PR == "Campinarana Arborizada" ~ "forest",
    DSC_VEG_PR == "RefÃºgios Vegetacionais Montano" ~ "grasslands",
    DSC_VEG_PR == "Campinarana Arbustiva" ~ "forest",
    DSC_VEG_PR == "RefÃºgios Vegetacionais Alto-Montano" ~ "grasslands",
    DSC_VEG_PR == "Floresta  OmbrÃ³fila/Floresta Estacional" ~ "forest",
    DSC_VEG_PR == "Floresta Estacional Semidecidual Terras Baixas" ~ "forest",
    DSC_VEG_PR == "Ã\u0081reas das FormaÃ§Ãµes Pioneiras VegetaÃ§Ã£o com InfluÃªncia Fluvio- marinha" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa/Floresta OmbrÃ³fila Mista" ~ "forest",
    DSC_VEG_PR == "Floresta Estacional Decidual Montana" ~ "forest",
    DSC_VEG_PR == "Afloramento Rochoso" ~ "rock",
    DSC_VEG_PR == "Savana/Savana EstÃ©pica/Floresta Estacional" ~ "transition",
    DSC_VEG_PR == "Savana/FormaÃ§Ãµes Pioneiras" ~ "savana",
    DSC_VEG_PR == "Floresta Estacional/FormaÃ§Ãµes Pioneiras" ~ "savana",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa Terras Baixas" ~ "forest",
    DSC_VEG_PR == "VegetaÃ§Ã£o OmbrÃ³fila Aberta Terras Baixas" ~ "forest",
    DSC_VEG_PR == "Floresta OmbrÃ³fila Densa Aluvial" ~ "forest",
    TRUE ~ DSC_VEG_PR))

# Checking unique values
unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])

# IUCN codes for these forest cover types:

# Forest            - 100
# Savana            - 200
# Grasslands        - 400
# Transition        - 999 # I arbitrarily choose these value as it will be filled with Jung values
# Rock              - 1700
# Water             - 1700 # (not wetlands)
# Wetlands (inland) - 500


# Replacing land cover types with IUCN codes

values(IBGE_wgs84) <- values(IBGE_wgs84) %>% 
  mutate(DSC_VEG_PR = case_when(
    DSC_VEG_PR == "forest" ~ "100",
    DSC_VEG_PR == "grasslands" ~ "400",
    DSC_VEG_PR == "rock" ~ "1700",
    DSC_VEG_PR == "savana" ~ "200",
    DSC_VEG_PR == "transition"  ~ "999",
    DSC_VEG_PR == "water"  ~ "1700",
    TRUE ~ DSC_VEG_PR)) %>% 
  mutate(DSC_VEG_PR = as.numeric(DSC_VEG_PR)) %>% 
  rename(pixel_value = DSC_VEG_PR)


# Loading MapBiomas 1985 to use the same resolution
mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


# rasterizing IBGE
IBGE_rasterized <- terra::rasterize(IBGE_wgs84, mb_1985, field = "pixel_value")

# Cropping to Brazil polygon
IBGE_rasterized <- mask(crop(IBGE_rasterized , br), br)


#writeRaster(IBGE_rasterized, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Considering values of natural land cover types from MapBiomas 1985 to the prehistoric map
# ------------------------------------------------------------------------------

IBGE_raster <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized.tif")
mb_1985_cropBR <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


# Considering all grasslands in MapBiomas 1985 (code 12) as grasslands in IBGE (IUCN code 400)

IBGE_rasterized_grass <- terra::ifel(mb_1985_cropBR == 12, 400, IBGE_raster)


# and considering all wetlands in MapBiomas 1985 (code 11) as wetlands in IBGE (IUCN code 500)

IBGE_rasterized_grass_wet <- terra::ifel(mb_1985_cropBR == 11, 500, IBGE_rasterized_grass)


#terra::writeRaster(IBGE_rasterized_grass_wet, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized_grass_wet.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Considering Jung for transitional values for IBGE
# ------------------------------------------------------------------------------

# Cleaning directory 

rm(list = ls())


# Loading layers

jung_br <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br.tif")
mb_1985_cropBR <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")
IBGE_rasterized_grass_wet <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized_grass_wet.tif")


# Resampling Jung to the same resolution of MapBiomas

jung_br_30m <- terra::resample(jung_br, mb_1985_cropBR, method = "near") # re-sampling by the nearest pixel

#terra::writeRaster(jung_br_30m, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br_30m.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Adding Jung values in IBGE
# ------------------------------------------------------------------------------

# Cleaning directory 

rm(list = ls())


# Loading layers

jung_br30m <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br_30m.tif")
IBGE_rasterized_grass_wet <- terra::rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized_grass_wet.tif")

historic_BR <- terra::ifel(IBGE_rasterized_grass_wet == 999, jung_br30m, IBGE_rasterized_grass_wet)

#terra::writeRaster(historic_BR, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/historic_BR.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Reclassifying pixel values as Jung has different categories for each land cover type
# This classification was made mainly by checking pixel values against Google maps images in QGis

reclass_matrix <- matrix(c(0, 100,
                           104, 100,
                           105, 100,
                           106, 100,
                           107, 100,
                           108, 100,
                           109, 100,
                           201, 200,
                           202, 200,
                           300, 100,
                           304, 100,
                           305, 100,
                           306, 100,
                           307, 100,
                           404, 400,
                           405, 400,
                           406, 400,
                           407, 400,
                           502, 500,
                           503, 500,
                           505, 500,
                           506, 500,
                           507, 500,
                           513, 500,
                           802, 100,
                           1401, 100,
                           1402, 100,
                           1403, 100,
                           1405, 100),
                         ncol = 2, byrow = T)

historic_BR_final_values <- terra::classify(historic_BR, reclass_matrix)

#terra::writeRaster(historic_BR_final_values, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/historic_BR_final_values.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


