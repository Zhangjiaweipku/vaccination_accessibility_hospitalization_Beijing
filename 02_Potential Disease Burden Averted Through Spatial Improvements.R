library(readxl)   
library(dplyr)   
library(tidyr)     
library(ggplot2)   
library(MASS)
library(broom)

total_visit<- read_excel("D:/vaccination service accessibility/street_hospitalization_data_total_1519.xlsx")
total_visit <- total_visit %>%
  mutate(total_visits_1516full = round(total_visits_1516 * 1.73))
sum(total_visit$total_visits_1516full)
total_visit_sd <- total_visit %>%
  mutate(
    total_visits_1516_sd = scale(total_visits_1516full),
    total_visits_1617_sd = scale(total_visits_1617),
    total_visits_1718_sd = scale(total_visits_1718),
    total_visits_1819_sd = scale(total_visits_1819)
  )

all_data <- read_excel("D:/vaccination service accessibility/all_data(总可及性按照三个交通方式占比相同计算).xlsx")
total_visits_subset <- total_visit_sd %>%
  dplyr::select(name, total_visits_1516_sd, total_visits_1617_sd, total_visits_1718_sd, total_visits_1819_sd)

merged_data <- all_data %>%
  full_join(total_visits_subset, by = c("street" = "name"))
data_standardized <- merged_data %>%
  mutate(
    education = scale(education),
    percentage_60_plus_population = scale(percentage_60_plus_population),
    percentage_migrant_population = scale(percentage_migrant_population),
    bed_accessibility = scale(bed_accessibility),
    acccess_level_30_dummy = ifelse(acccess_level_30 == "0-30", 1, 0)
  ) %>% 
  .[complete.cases(.),]
  
dim(data_standardized)
  
data_standardized$acccess_level_30_dummy
data_standardized$total_visits_1617_sd<-as.numeric(data_standardized$total_visits_1617_sd)
data_standardized$total_visits_1718_sd<-as.numeric(data_standardized$total_visits_1718_sd)
data_standardized$total_visits_1819_sd<-as.numeric(data_standardized$total_visits_1819_sd)

glm_model_1_std <- glm.nb(total_visits_1617 ~ acccess_level_30_dummy + education + 
                            percentage_60_plus_population + percentage_migrant_population + 
                            bed_accessibility + total_visits_1516_sd+offset(log(population_60)), data = data_standardized)
summary(glm_model_1_std)

glm_model_2_std <- glm.nb(total_visits_1718 ~ acccess_level_30_dummy + education + 
                            percentage_60_plus_population + percentage_migrant_population + 
                            bed_accessibility + total_visits_1617_sd+offset(log(population_60)), data = data_standardized)
summary(glm_model_2_std)

glm_model_3_std <- glm.nb(total_visits_1819 ~ acccess_level_30_dummy + education + 
                            percentage_60_plus_population + percentage_migrant_population + 
                            bed_accessibility + total_visits_1718_sd+offset(log(population_60)), data = data_standardized)
summary(glm_model_3_std)




extract_effects <- function(model, year, data, varname = "acccess_level_30_dummy", outcome_var) {
  coef_est <- coef(model)[varname]
  ci <- confint(model, varname)
  
  RR <- exp(coef_est)
  RR_lower <- exp(ci[1])
  RR_upper <- exp(ci[2])
  
  baseline_total <- data %>%
    filter(.data[[varname]] == 0) %>%
    pull(.data[[outcome_var]]) %>%
    sum(na.rm = TRUE)
  
  visits_saved <- baseline_total * (1 - RR)
  visits_saved_lower <- baseline_total * (1 - RR_upper)
  visits_saved_upper <- baseline_total * (1 - RR_lower)
  
  return(data.frame(
    year = year,
    baseline_visits = baseline_total,
    RR = RR,
    RR_lower = RR_lower,
    RR_upper = RR_upper,
    visits_saved = visits_saved,
    visits_saved_lower = visits_saved_lower,
    visits_saved_upper = visits_saved_upper
  ))
}


result_1617 <- extract_effects(glm_model_1_std, "2016–17", data_standardized, "acccess_level_30_dummy", "total_visits_1617")
result_1718 <- extract_effects(glm_model_2_std, "2017–18", data_standardized, "acccess_level_30_dummy", "total_visits_1718")
result_1819 <- extract_effects(glm_model_3_std, "2018–19", data_standardized, "acccess_level_30_dummy", "total_visits_1819")

final_results <- bind_rows(result_1617, result_1718, result_1819)
print(final_results)
