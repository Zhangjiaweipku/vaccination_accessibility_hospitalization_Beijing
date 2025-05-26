library(dplyr)
library(readr)
library(sf)
library(openxlsx)
library(readxl) 
library(MASS)
library(ggplot2)
total_visits_combined<- read_excel("D:/vaccination service accessibility/street_hospitalization_data_total_1519.xlsx")
all_data <- read_excel("D:/vaccination service accessibility/all_data(总可及性按照三个交通方式占比相同计算).xlsx")
total_visits_subset <- total_visits_combined %>%
  dplyr::select(name, total_visits_1516)
merged_data <- all_data %>%
  full_join(total_visits_subset, by = c("street" = "name")) %>%
  mutate(total_visits_1516 = ifelse(is.na(total_visits_1516), 0, total_visits_1516)) 
data <- merged_data %>%
  mutate(total_visits_1516full = round(total_visits_1516 * 1.73))
sum(data$total_visits_1516full)

data_standardized <- data %>%
  mutate(
    education = scale(education),
    percentage_60_plus_population = scale(percentage_60_plus_population),
    percentage_migrant_population = scale(percentage_migrant_population),
    bed_accessibility = scale(bed_accessibility),
    total_visits_1516_std = scale(total_visits_1516full), 
    total_visits_1617_std = scale(total_visits_1617),  
    total_visits_1718_std = scale(total_visits_1718)   
  )

driving_results <- data.frame()
time_breaks <- seq(5, 50, by = 5)
for (time in time_breaks) {
  var_name <- paste0("access_level_", time)
  data_tmp <- data_standardized %>%
    mutate(!!var_name := ifelse(driving_accessibility <= time, 1, 0))
  
  for (year in c("1617", "1718", "1819")) {
    lag_var <- ifelse(year == "1617", "total_visits_1516_std",
                      ifelse(year == "1718", "total_visits_1617_std",
                             ifelse(year == "1819", "total_visits_1718_std", NA)))
    
    model_formula <- if (!is.na(lag_var)) {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + ",
                        lag_var,  
                        " + offset(log(population_60))"))
    } else {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + offset(log(population_60))"))
    }
    
    glm_model <- glm.nb(model_formula, data = data_tmp)
    
    model_summary <- summary(glm_model)
    model_coefs <- model_summary$coefficients
    
    for (coef_name in rownames(model_coefs)) {
      driving_results <- rbind(driving_results, data.frame(
        time = time,
        year = year,
        coef_name = coef_name,
        coef_value = model_coefs[coef_name, 1],  
        std_error = model_coefs[coef_name, 2],  
        z_value = model_coefs[coef_name, 3],    
        p_value = model_coefs[coef_name, 4],    
        residuals_mean = mean(abs(glm_model$residuals))  
      ))
    }
  }
}

driving_level_results <- driving_results %>%
  filter(grepl("level", coef_name)) %>%
  mutate(p_value_color = ifelse(p_value < 0.05, "red", "black"))

walking_results <- data.frame()
time_breaks <- seq(15, 120, by = 5)


for (time in time_breaks) {
  var_name <- paste0("access_level_", time)
  data_tmp <- data_standardized %>%
    mutate(!!var_name := ifelse(walking_accessibility <= time, 1, 0))
  
  for (year in c("1617", "1718", "1819")) {
    lag_var <- ifelse(year == "1617", "total_visits_1516_std",
                      ifelse(year == "1718", "total_visits_1617_std",
                             ifelse(year == "1819", "total_visits_1718_std", NA)))
    
    model_formula <- if (!is.na(lag_var)) {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + ",
                        lag_var, 
                        " + offset(log(population_60))"))
    } else {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + offset(log(population_60))"))
    }
    
    glm_model <- glm.nb(model_formula, data = data_tmp)
    
    model_summary <- summary(glm_model)
    model_coefs <- model_summary$coefficients
    
    for (coef_name in rownames(model_coefs)) {
      walking_results <- rbind(walking_results, data.frame(
        time = time,
        year = year,
        coef_name = coef_name,
        coef_value = model_coefs[coef_name, 1],  
        std_error = model_coefs[coef_name, 2],  
        z_value = model_coefs[coef_name, 3],    
        p_value = model_coefs[coef_name, 4],    
        residuals_mean = mean(abs(glm_model$residuals))  
      ))
    }
  }
}


walking_level_results <- walking_results %>%
  filter(grepl("level", coef_name)) %>%
  mutate(p_value_color = ifelse(p_value < 0.05, "red", "black"))


public_transport_results <- data.frame()

time_breaks <- seq(15, 120, by = 5)

for (time in time_breaks) {
  var_name <- paste0("access_level_", time)
  data_tmp <- data_standardized %>%
    mutate(!!var_name := ifelse(public_transport_accessibility <= time, 1, 0))
  
  for (year in c("1617", "1718", "1819")) {
    lag_var <- ifelse(year == "1617", "total_visits_1516_std",
                      ifelse(year == "1718", "total_visits_1617_std",
                             ifelse(year == "1819", "total_visits_1718_std", NA)))
    
    model_formula <- if (!is.na(lag_var)) {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + ",
                        lag_var, 
                        " + offset(log(population_60))"))
    } else {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + offset(log(population_60))"))
    }
    
    glm_model <- glm.nb(model_formula, data = data_tmp)
    
    model_summary <- summary(glm_model)
    model_coefs <- model_summary$coefficients
    
    for (coef_name in rownames(model_coefs)) {
      public_transport_results <- rbind(public_transport_results, data.frame(
        time = time,
        year = year,
        coef_name = coef_name,
        coef_value = model_coefs[coef_name, 1],  
        std_error = model_coefs[coef_name, 2],  
        z_value = model_coefs[coef_name, 3],    
        p_value = model_coefs[coef_name, 4],    
        residuals_mean = mean(abs(glm_model$residuals))  
      ))
    }
  }
}

public_transport_level_results <- public_transport_results %>%
  filter(grepl("level", coef_name)) %>%
  mutate(p_value_color = ifelse(p_value < 0.05, "red", "black"))
total_results <- data.frame()

time_breaks <- seq(15, 120, by = 5)
for (time in time_breaks) {
  var_name <- paste0("access_level_", time)
  data_tmp <- data_standardized %>%
    mutate(!!var_name := ifelse(total_accessibility <= time, 1, 0))
    for (year in c("1617", "1718", "1819")) {

    lag_var <- ifelse(year == "1617", "total_visits_1516_std",
                      ifelse(year == "1718", "total_visits_1617_std",
                             ifelse(year == "1819", "total_visits_1718_std", NA)))
    
    model_formula <- if (!is.na(lag_var)) {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + ",
                        lag_var,  
                        " + offset(log(population_60))"))
    } else {
      as.formula(paste0("total_visits_", year, " ~ ", var_name, 
                        " + education + percentage_60_plus_population + percentage_migrant_population + bed_accessibility + offset(log(population_60))"))
    }
    

    glm_model <- glm.nb(model_formula, data = data_tmp)
    

    model_summary <- summary(glm_model)
    model_coefs <- model_summary$coefficients
    

    for (coef_name in rownames(model_coefs)) {
      total_results <- rbind(total_results, data.frame(
        time = time,
        year = year,
        coef_name = coef_name,
        coef_value = model_coefs[coef_name, 1],  
        std_error = model_coefs[coef_name, 2],  
        z_value = model_coefs[coef_name, 3],    
        p_value = model_coefs[coef_name, 4],    
        residuals_mean = mean(abs(glm_model$residuals))  
      ))
    }
  }
}


total_level_results <- total_results %>%
  filter(grepl("level", coef_name)) %>%
  mutate(p_value_color = ifelse(p_value < 0.05, "red", "black"))

driving_level_results$mode <- "Driving"
public_transport_level_results$mode <- "Public Transport"
walking_level_results$mode <- "Walking"
total_level_results$mode <- "weighted"

all_results <- rbind(driving_level_results, public_transport_level_results, walking_level_results,total_level_results)
all_results$year_label <- case_when(
  all_results$year == "1617" ~ "2016-2017",
  all_results$year == "1718" ~ "2017-2018",
  all_results$year == "1819" ~ "2018-2019",
  TRUE ~ NA_character_  
)


all_results$year_label <- factor(all_results$year_label, levels = c("2016-2017", "2017-2018", "2018-2019"))
all_results$mode_year <- interaction(all_results$mode, all_results$year_label)

colors_lancet <- c("2016-2017" = adjustcolor("#00468B", alpha.f = 0.4),  
                   "2017-2018" = adjustcolor("#ED0000", alpha.f = 0.4),  
                   "2018-2019" = adjustcolor("#42B540", alpha.f = 0.4))  


all_results <- all_results %>%
  mutate(
    IRR = exp(coef_value),  
    coef_text = case_when(
      p_value <= 0.01 ~ paste0(sprintf("%.3f", round(IRR, 3)), "***"),  
      p_value <= 0.05 ~ paste0(sprintf("%.3f", round(IRR, 3)), "**"),
      p_value <= 0.10 ~ paste0(sprintf("%.3f", round(IRR, 3)), "*"),
      TRUE ~ format(round(IRR, 3), nsmall = 3)  
    ),
    font_weight = ifelse(p_value <= 0.10, "bold", "plain"),
    p_alpha = case_when(
      p_value <= 0.01 ~ 0.8,  
      p_value <= 0.05 ~ 0.7,  
      p_value <= 0.10 ~ 0.6,  
      TRUE ~ 0.5              
    )
  )


p5_part <- ggplot(all_results, aes(x = time, y = mode_year, fill = year_label)) +
  geom_tile(aes(alpha = p_alpha), width = 5, height = 0.8, color = "black", size = 0.3) +  
  geom_text(aes(label = ifelse(p_value <= 0.10, coef_text, "")), color = "black", fontface = "bold", size = 4) +  
  scale_fill_manual(values = colors_lancet, name = "Influenza season") +
  scale_alpha_continuous(range = c(0.2, 1.0), guide = "none") +  
  scale_x_continuous(
    breaks = seq(0, 60, by = 10),  
    limits = c(0, 64)
  )+  
  labs(x = "Travel Time (5-minute intervals)", y = "Transportation Mode") +
  scale_y_discrete(
    breaks = c("Driving.2018-2019", "Driving.2017-2018", "Driving.2016-2017", "Public Transport.2018-2019", 
               "Public Transport.2017-2018", "Public Transport.2016-2017", "Walking.2018-2019", 
               "Walking.2017-2018", "Walking.2016-2017", "weighted.2018-2019", 
               "weighted.2017-2018", "weighted.2016-2017"),
    labels = c("", "Driving", "", "", "Public Transport", "", "", "Walking", "", "", "Weighted", "")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    axis.line = element_line(size = 0.5, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = rel(1.2), face = "italic")
  ) +
  facet_wrap(~mode, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(caption = "*** p ≤ 0.01, ** 0.01 < p ≤ 0.05, * 0.05 < p ≤ 0.10") +
  theme(plot.caption = element_text(hjust = 0.5, size = rel(1.0)))

print(p5_part)
ggsave("Figure 6.png", p5_part, width = 12, height = 12, dpi = 300, bg = "white")  # 修改宽度、高度和分辨率

