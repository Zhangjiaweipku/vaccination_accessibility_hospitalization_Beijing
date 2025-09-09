library(dplyr)
library(readr)
library(sf)
library(openxlsx)
library(readxl)
library(MASS)
library(ggplot2)
patient_data_16 <- read_csv("D:/vaccination service accessibility/J_basy_beijing_16.csv", 
                            locale = locale(encoding = "GB2312"))
patient_data_17 <- read_csv("D:/vaccination service accessibility/J_basy_beijing_17.csv", 
                            locale = locale(encoding = "GB2312"))
patient_data_18 <- read_csv("D:/vaccination service accessibility/J_basy_beijing_18.csv", 
                            locale = locale(encoding = "GB2312"))
patient_data_19 <- read_csv("D:/vaccination service accessibility/J_basy_beijing_19.csv", 
                            locale = locale(encoding = "GB2312"))

filtered_data_16 <- patient_data_16 %>%
  filter(AGE >= 60, grepl("^J(0[0-9]|1[0-9]|2[0-2])", DISEASE_CODE1),city == "北京市")
filtered_data_17 <- patient_data_17 %>%
  filter(AGE >= 60, grepl("^J(0[0-9]|1[0-9]|2[0-2])", DISEASE_CODE1),city == "北京市")
filtered_data_18 <- patient_data_18 %>%
  filter(AGE >= 60, grepl("^J(0[0-9]|1[0-9]|2[0-2])", DISEASE_CODE1),city == "北京市")
filtered_data_19 <- patient_data_19 %>%
  filter(AGE >= 60, grepl("^J(0[0-9]|1[0-9]|2[0-2])", DISEASE_CODE1),city == "北京市")
write.csv(filtered_data_16, "elderly_J00_J22_beijing_16.csv", row.names = FALSE)
#15-16
filtered_data_15_peaktime <- filtered_data_16 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2016-01-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2016-04-30"))
#16-17
filtered_data_16_peaktime <- filtered_data_16 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2016-09-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2016-12-31"))
filtered_data_17_peaktime1 <- filtered_data_17 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2017-01-01") &
           as.Date(DATE_INHOSPITAL) <= as.Date("2017-04-30"))
#17-18
filtered_data_17_peaktime2 <- filtered_data_17 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2017-09-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2017-12-31"))
filtered_data_18_peaktime1 <- filtered_data_18 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2018-01-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2018-04-30"))
#18-19
filtered_data_18_peaktime2 <- filtered_data_18 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2018-09-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2018-12-31"))
filtered_data_19_peaktime <- filtered_data_19 %>%
  filter(as.Date(DATE_INHOSPITAL) >= as.Date("2019-01-01") & 
           as.Date(DATE_INHOSPITAL) <= as.Date("2019-04-30"))
combined_hospitalization_data_1516 <- bind_rows(filtered_data_15_peaktime)
combined_hospitalization_data_1617 <- bind_rows(filtered_data_17_peaktime1)
combined_hospitalization_data_1718 <- bind_rows(filtered_data_18_peaktime1)
combined_hospitalization_data_1819 <- bind_rows(filtered_data_19_peaktime)
write.csv(combined_hospitalization_data_1617, "D:/vaccination service accessibility/elderly_J00_J22_beijing_hospitalization_data_1617.csv", row.names = FALSE)
write.csv(combined_hospitalization_data_1718, "D:/vaccination service accessibility/elderly_J00_J22_beijing_hospitalization_data_1718.csv", row.names = FALSE)
write.csv(combined_hospitalization_data_1819, "D:/vaccination service accessibility/elderly_J00_J22_beijing_hospitalization_data_1819.csv", row.names = FALSE)

street_shp <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp", quiet = TRUE)
patient_sf_1516 <- st_as_sf(combined_hospitalization_data_1516, coords = c("long", "lat"), crs = st_crs(street_shp))
patient_sf_1617 <- st_as_sf(combined_hospitalization_data_1617, coords = c("long", "lat"), crs = st_crs(street_shp))
patient_sf_1718 <- st_as_sf(combined_hospitalization_data_1718, coords = c("long", "lat"), crs = st_crs(street_shp))
patient_sf_1819 <- st_as_sf(combined_hospitalization_data_1819, coords = c("long", "lat"), crs = st_crs(street_shp))
street_shp <- st_make_valid(street_shp)
patient_with_street_1516 <- st_join(patient_sf_1516, street_shp, left = TRUE)
patient_with_street_1617 <- st_join(patient_sf_1617, street_shp, left = TRUE)
patient_with_street_1718 <- st_join(patient_sf_1718, street_shp, left = TRUE)
patient_with_street_1819 <- st_join(patient_sf_1819, street_shp, left = TRUE)
total_visits_1516 <- patient_with_street_1516 %>%
  group_by(name) %>%
  summarise(total_visits = n())  
total_visits_1617 <- patient_with_street_1617 %>%
  group_by(name) %>%
  summarise(total_visits = n())  
total_visits_1718 <- patient_with_street_1718 %>%
  group_by(name) %>%
  summarise(total_visits = n())  
total_visits_1819 <- patient_with_street_1819 %>%
  group_by(name) %>%
  summarise(total_visits = n())  
total_visits_1516_df <- total_visits_1516 %>% st_drop_geometry()
total_visits_1617_df <- total_visits_1617 %>% st_drop_geometry()
total_visits_1718_df <- total_visits_1718 %>% st_drop_geometry()
total_visits_1819_df <- total_visits_1819 %>% st_drop_geometry()

library(tidyr)  
total_visits_combined <- total_visits_1617_df %>%
  dplyr::select(name, total_visits_1617 = total_visits) %>%
  full_join(total_visits_1718_df %>%
              dplyr::select(name, total_visits_1718 = total_visits), by = "name") %>%
  full_join(total_visits_1819_df %>%
              dplyr::select(name, total_visits_1819 = total_visits), by = "name") %>%
  full_join(total_visits_1516_df %>%
              dplyr::select(name, total_visits_1516 = total_visits), by = "name") %>%
  replace_na(list(total_visits_1516 = 0,total_visits_1617 = 0, total_visits_1718 = 0, total_visits_1819 = 0))  

write.xlsx(
  total_visits_combined,
  "D:/vaccination service accessibility/street_hospitalization_data_total_1519_1到4月.xlsx",
  rowNames = FALSE
)
