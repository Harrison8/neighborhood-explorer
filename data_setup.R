library(glpdata)
library(glptools)

map_tract <- sf::st_as_sf(glptools::map_tract)
map_nh <- sf::st_as_sf(glptools::map_nh)
map_muw <- sf::st_as_sf(glptools::map_muw)
map_zip <- sf::st_as_sf(glptools::map_zip)
map_district <- sf::st_as_sf(glptools::map_district)

save(degree_tract, degree_nh, degree_muw, degree_county, file = "R/education_data.RData")
save(map_tract, map_nh, map_muw, map_district, file = "R/map_data.RData")
