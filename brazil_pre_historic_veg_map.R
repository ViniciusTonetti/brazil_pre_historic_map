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

unique(IBGE_wgs84[,])


# Rasterizing to the same resolution of MapBiomas

mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")

unique(values(IBGE_wgs84)[,"DSC_VEG_PR"])

values(IBGE_wgs84) %>% 
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
    DSC_VEG_PR == "VegetaÃ§Ã£o OmbrÃ³fila Aberta" ~ "transition",
    DSC_VEG_PR == "Floresta  OmbrÃ³fila/Floresta Estacional" ~ "forest",
    DSC_VEG_PR == "Floresta Estacional/Floresta OmbrÃ³fila Mista" ~ "forest",
    DSC_VEG_PR == "Savana/Savana EstÃ©pica/Floresta Estacional" ~ "transition",
    TRUE ~ DSC_VEG_PR
  ))


IBGE_rasterized <- terra::rasterize(IBG_wgs84, mb_1985, field = )




