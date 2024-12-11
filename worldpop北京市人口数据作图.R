library(terra)
library(sf)
library(tidyverse)

# 读取人口栅格数据（terra 格式）
dir_raster <- ("D:/vaccination service accessibility/")
fn_raster <- list.files(dir_raster, pattern = "constrained.tif")
paste0(dir_raster, fn_raster) %>%
  map(rast) %>% 
  setNames(fn_raster) -> list_raster

# 读取北京市边界数据（sf 格式）
beijing_boundary <- st_read("D:/北京地图(1)/北京地图/北京地图.shp")

# 检查人口栅格的范围
print(crs(list_raster$chn_f_30_2020_constrained.tif))
# 检查北京市边界的范围
print(st_crs(beijing_boundary))
beijing_boundary_transformed <- st_transform(beijing_boundary, crs(list_raster$chn_f_30_2020_constrained.tif))

# 将转换后的边界数据转换为 terra 的 SpatVector 对象
beijing_boundary_vect <- vect(beijing_boundary_transformed)

# 使用 terra 包中的 crop() 和 mask()
list_raster %<>% 
  map(~crop(., beijing_boundary_vect)) %>% 
  map(~mask(., beijing_boundary_vect)) 

gsub("_2020_constrained.tif", "", fn_raster) %>% 
  map(~gsub("chn_f_", "", .)) %>% 
  map(~gsub("chn_m_","",.)) %>% 
  map(as.numeric) %>% 
  map(~.<=50) %>% 
  unlist %>% 
  which -> find_under50

population_under50 <- Reduce(`+`, list_raster[find_under50]) 
plot(population_under50)
