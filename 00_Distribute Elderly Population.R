library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(tmap)
township_boundary <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp")
data1 <- read_excel("D://vaccination service accessibility/北京市基于社区的60岁以上老年人口数.xlsx")
head(data1)
data1$population <- as.numeric(data1$population)
class(data1$population) 
population_sf <- st_as_sf(data1, coords = c("long", "lat"), crs = 4326)
population_sf <- st_transform(population_sf, crs = st_crs(township_boundary))
map <- st_zm(township_boundary)
population_with_township <- st_join(population_sf, map)
invalid_geometries <- st_is_valid(township_boundary)

sum(!invalid_geometries) 
which(!invalid_geometries)
invalid_shapes <- map[!invalid_geometries, ]
plot(invalid_shapes, col = "red", main = "Invalid Geometries in Map")
map <- st_make_valid(map)
all(st_is_valid(map))  # TRUE
# Sum population(township)
township_population <- population_with_township %>%
  group_by(name) %>%  
  summarize(total_population = sum(population, na.rm = TRUE))
write_xlsx(township_population, "D://vaccination service accessibility/统计局60岁以上各乡镇街道人口数.xlsx")
