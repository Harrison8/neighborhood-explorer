library(glpdata)
library(glptools)

map_tract <- sf::st_as_sf(glptools::map_tract)
map_nh <- sf::st_as_sf(glptools::map_nh)
map_muw <- sf::st_as_sf(glptools::map_muw)

save(degree_tract, degree_nh, degree_muw, file = "R/education_data.RData")
save(map_tract, map_nh, map_muw, file = "R/map_data.RData")
