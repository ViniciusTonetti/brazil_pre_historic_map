# Builfding a prehistoric vegetation map for Brazil
# Vinicius Tonetti - vrtonetti@ufscar.br / vrtonetti@gmail.com

# Loading packages

library(terra)


# Loading layers ---------------------------------------------------------------

br <- vect("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/br_limite/br_merged.shp")
mb_1985 <- rast("E:/_PESSOAL/ViniciusT/prehistoric_veg_map_brazil/MapBiomascol09/brazil_1985.tif")


# Re projecting IBGE -----------------------------------------------------------

terra::project(br, )