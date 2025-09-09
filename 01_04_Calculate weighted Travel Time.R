# 加载必要的包
library(dplyr)
library(sf)
library(readxl)
####Replace "walking" with "driving" or "public_transportation" to obtain travel times for those modes
accessibility_data <- read_excel("D:/vaccination service accessibility/driving_accessibility_data.xlsx")
population_data <- read_excel("D:/vaccination service accessibility/合并重复坐标的基于社区的60岁以上老年人口数.xlsx")
street_shp <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp", quiet = TRUE)

accessibility_data <- accessibility_data %>%
  mutate(
    lon = as.numeric(sub(",.*", "", from)),  
    lat = as.numeric(sub(".*,", "", from))   
  )

merged_data <- accessibility_data %>%
  left_join(population_data, by = c("lon" = "long", "lat" = "lat"))

accessibility_sf <- merged_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(street_shp))
street_shp <- st_make_valid(street_shp)
result_sf <- st_join(accessibility_sf, street_shp, left = TRUE)


final_data <- result_sf %>%
  mutate(street = name) %>%  
  select(-geometry)         

street_accessibility <- final_data %>%
  filter(!is.na(street) & !is.na(population)) %>% 
  group_by(street) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),  
    weekday_morning_accessibility = sum(weekday_morning * population, na.rm = TRUE) / total_population,  
    weekday_afternoon_accessibility = sum(weekday_afternoon * population, na.rm = TRUE) / total_population, 
    weekend_morning_accessibility = sum(weekend_morning * population, na.rm = TRUE) / total_population,  
    weekend_afternoon_accessibility = sum(weekend_afternoon * population, na.rm = TRUE) / total_population  
  )
street_accessibility_df <- as.data.frame(street_accessibility)
street_accessibility_no_col7 <- street_accessibility_df[, -7]
library(openxlsx)
write.xlsx(
  street_accessibility_no_col7,
  "D:/vaccination service accessibility/wdriving_accessibility_street_weighted_accessibility.xlsx",
  rowNames = FALSE
)