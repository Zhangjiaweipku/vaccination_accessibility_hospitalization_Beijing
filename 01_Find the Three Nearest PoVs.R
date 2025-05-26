pacman::p_load(tidyverse,data.table,magrittr,sf,stars,raster,mapview,tictoc,nngeo)
library(readxl) 
hospital_data <- read_excel("D:/vaccination service accessibility/vaccination service location result清理后版.xlsx")
head(hospital_data)
str(hospital_data)
hospital_sf <- hospital_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)
population_data <- read_excel("D://vaccination service accessibility/合并重复坐标的基于社区的60岁以上老年人口数.xlsx")

population_sf <- population_data %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

beijing_shp <- read_sf("D:/北京地图(1)/北京地图/北京地图.shp") %>% 
  st_transform(crs = 4326)

# `st_nn` find the 3 nearest vaccination sites
nearest_hospitals <- st_nn(population_sf, hospital_sf, k = 3, returnDist = TRUE)

nhosp_long <- tibble(
  origin_id = rep(1:length(nearest_hospitals$nn), each = 3),  # 人口点索引
  dest_id = unlist(nearest_hospitals$nn),  # 对应的医院索引
  distance = unlist(nearest_hospitals$dist)  # 对应的距离
)

origin_coords_df <- population_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(origin_id = row_number()) %>%
  rename(origin_long = X, origin_lat = Y)

dest_coords_df <- hospital_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(dest_id = row_number()) %>%
  rename(dest_long = X, dest_lat = Y)

coords <- nhosp_long %>%
  left_join(origin_coords_df, by = "origin_id") %>%
  left_join(dest_coords_df, by = "dest_id") %>%
  mutate(
    from = paste(origin_long, origin_lat, sep = ","),
    to = paste(dest_long, dest_lat, sep = ",")
  ) %>%
  dplyr::select(origin_id, dest_id, distance, from, to)

write_csv(coords, "D:/vaccination service accessibility/更新距离最近的三个疫苗接种点(周中).csv")
