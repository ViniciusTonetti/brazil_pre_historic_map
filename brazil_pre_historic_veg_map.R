# Builfding a prehistoric vegetation map for Brazil
# Vinicius Tonetti - vrtonetti@ufscar.br / vrtonetti@gmail.com

# Loading packages

library(terra)


# Loading layers ---------------------------------------------------------------

br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_merged.shp")
mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/brazil_1985.tif")
IBGE <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/IBGE/Vegetacao_5000mil/Vegetacao_5000.shp")
jung <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_lvl2_ver001.tif")


# Re projecting IBGE -----------------------------------------------------------

br_wgs84 <- terra::project(br, "EPSG:4326")

#terra::writeVector(br_wgs84, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_wgs84.shp")

mb_1985_cropBR <- mask(crop(mb_1985, br_wgs84), br_wgs84)

writeRaster(mb_1985_cropBR, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/mb_1985_crop_BR.tif")


# Cropping Jung map to Brazil

jung_br <- mask(crop(jung, br_wgs84), br_wgs84)
writeRaster(jung_br, "E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/jung/jung_br.tif")



