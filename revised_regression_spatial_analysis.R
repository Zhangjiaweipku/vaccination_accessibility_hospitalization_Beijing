# 加载包
invisible(lapply(c("sf","readxl","spdep","dplyr","tidyr","ggplot2","MASS","broom"), library, character.only = TRUE))

# 读取数据并合并
all_data <- read_excel("D:/vaccination service accessibility/all_data(总可及性按照三个交通方式占比相同计算).xlsx")
total_visits <- read_excel("D:/vaccination service accessibility/total_visits_sd.xlsx")
merged_data <- full_join(all_data, total_visits, by = c("street" = "name"))

# 标准化变量
data_std <- merged_data %>%
  mutate(
    education = scale(education),
    percentage_60_plus_population = scale(percentage_60_plus_population),
    percentage_migrant_population = scale(percentage_migrant_population),
    bed_accessibility = scale(bed_accessibility),
    acccess_level_30_dummy = ifelse(acccess_level_30=="0-30",1,0)
  )

# 拟合模型函数
fit_nb <- function(y_var, offset_var, data) {
  glm.nb(as.formula(paste0(y_var, "~ acccess_level_30_dummy + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + ", offset_var, "+ offset(log(population_60))")), data = data)
}

models <- list(
  glm_model_1_std = fit_nb("total_visits_1617","total_visits_1516_sd",data_std),
  glm_model_2_std = fit_nb("total_visits_1718","total_visits_1617_sd",data_std),
  glm_model_3_std = fit_nb("total_visits_1819","total_visits_1718_sd",data_std)
)
summary(models$glm_model_1_std)
summary(models$glm_model_2_std)
summary(models$glm_model_3_std)
# 读取街道 shapefile 并处理
street_sf <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp", quiet = TRUE) %>%
  mutate(name = if_else(OBJECTID==289, paste0(name,"_1"), name)) %>%
  left_join(data_std, by=c("name"="street")) %>%
  st_make_valid()
# 计算残差并 Moran's I
moran_results <- lapply(names(models), function(model_name) {
  model <- models[[model_name]]
  resid_vals <- residuals(model, type="pearson")
  model_rows <- as.numeric(rownames(model.frame(model)))  # 模型使用的行号
  resid_df <- data.frame(
    street = data_std$street[model_rows],
    residuals = resid_vals
  )
  street_sf_sub <- street_sf %>% left_join(resid_df, by=c("name"="street")) %>% filter(!is.na(residuals))
  lw <- nb2listw(poly2nb(street_sf_sub, queen=TRUE), style="W", zero.policy=TRUE)
  moran.test(street_sf_sub$residuals, lw, zero.policy=TRUE)
})
moran_results[[1]] 
moran_results[[2]] 
moran_results[[3]] 
###加入district
years <- list(
  "1617" = c("total_visits_1617","total_visits_1516_sd"),
  "1718" = c("total_visits_1718","total_visits_1617_sd"),
  "1819" = c("total_visits_1819","total_visits_1718_sd")
)
fit_nb_moran <- function(y_var, lag_var, sf) {
  sf_clean <- sf %>% filter(!is.na(.data[[y_var]]) & !is.na(.data[[lag_var]]) & !is.na(district_name))
  lw <- nb2listw(poly2nb(sf_clean, queen=TRUE), style="W", zero.policy=TRUE)
  formula_text <- paste0(y_var," ~ acccess_level_30_dummy + education + percentage_60_plus_population + ",
                         "percentage_migrant_population + bed_accessibility + ", lag_var,
                         " + district_name + offset(log(population_60))")
  model <- glm.nb(as.formula(formula_text), data=sf_clean)
  list(model=model, moran=moran.test(residuals(model, type="pearson"), lw, zero.policy=TRUE))
}
spatial_models <- lapply(names(years), function(y) fit_nb_moran(years[[y]][1], years[[y]][2], street_sf))
names(spatial_models) <- names(years)
lapply(names(spatial_models), function(y) {
  print(summary(spatial_models[[y]]$model))
  print(spatial_models[[y]]$moran)
})
