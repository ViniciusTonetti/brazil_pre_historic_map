# Builfding a prehistoric vegetation map for Brazil
# Vinicius Tonetti - vrtonetti@ufscar.br / vrtonetti@gmail.com

# Loading packages

library(terra)
library(tidyverse)


# Cleaning directory -----------------------------------------------------------

rm(list = ls())


# Loading layers ---------------------------------------------------------------

br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_merged.shp")
mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/brazil_1985.tif")
IBGE <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/Vegetacao_5000mil/Vegetacao_5000.shp")
jung <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_lvl2_ver001.tif")


# Re projecting IBGE -----------------------------------------------------------

br_wgs84 <- terra::project(br, "EPSG:4326")

#terra::writeVector(br_wgs84, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_wgs84.shp")

mb_1985_cropBR <- mask(crop(mb_1985, br_wgs84), br_wgs84)

#writeRaster(mb_1985_cropBR, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


# Cropping Jung map to Brazil --------------------------------------------------

jung_br <- mask(crop(jung, br_wgs84), br_wgs84)
#writeRaster(jung_br, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br.tif")


# Rasterizing IBGE -------------------------------------------------------------

# Cleaning directory -----------------------------------------------------------

rm(list = ls())


# Setting CRS

IBGE <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/Vegetacao_5000mil/Vegetacao_5000.shp")
crs(IBGE) <- "EPSG:4326"

#writeVector(IBGE, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_WGS84.shp")

IBGE_wgs84 <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_WGS84.shp")
br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_wgs84.shp")


unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])

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
  mutate(DSC_VEG_PR = ifelse(is.na(DSC_VEG_PR), DSC_CLASS_, DSC_VEG_PR))

# Checking unique values
unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])

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

# Forest      - 100
# Savana      - 200
# Grasslands  - 400
# Transition  - 999
# Rock        - 1700
# Water       - 1700
# (not wetlands)


values(IBGE_wgs84) <- values(IBGE_wgs84) %>% 
  mutate(DSC_VEG_PR = case_when(
    DSC_VEG_PR == "forest" ~ "100",
    DSC_VEG_PR == "savana" ~ "200",
    DSC_VEG_PR == "grasslands" ~ "400",
    DSC_VEG_PR == "transition" ~ "999",
    DSC_VEG_PR == "rock" ~ "1700",
    DSC_VEG_PR == "water" ~ "1700",
    TRUE ~ DSC_VEG_PR)) %>% 
  mutate(DSC_VEG_PR = ifelse(is.na(DSC_VEG_PR), DSC_CLASS_, DSC_VEG_PR))

# Checking unique values
unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])


# Rasterizing to the same resolution of MapBiomas

mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


IBGE_rasterized <- terra::rasterize(IBGE_wgs84, mb_1985, field = "DSC_VEG_PR")

IBGE_rasterized <- mask(crop(IBGE_rasterized , br), br)

plot(IBGE_rasterized)

#writeRaster(IBGE_rasterized, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/IBGE_rasterized.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"))



