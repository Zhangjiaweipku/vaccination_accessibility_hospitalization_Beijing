library(readxl)    
library(dplyr)     
library(tidyr)     
library(ggplot2)  
library(MASS)
library(broom)
library(readr)
library(sf)
library(openxlsx)

total_visits<- read_excel("D:/vaccination service accessibility/street_hospitalization_data_total_1519_103.xlsx")
total_visits <- total_visits %>%
  mutate(total_visits_1516full = round(total_visits_1516 * 1.70))
sum(total_visits$total_visits_1516full)

all_data <- read_excel("D:/vaccination service accessibility/all_data(总可及性按照三个交通方式占比相同计算).xlsx")
total_visits_combined <- total_visits %>%
  rename_with(~ paste0(.x, "_103"), starts_with("total_visits_"))
merged_data <- all_data %>%
  full_join(total_visits_combined, by = c("street" = "name"))%>%
  mutate(total_visits_1516_103 = ifelse(is.na(total_visits_1516_103), 0, total_visits_1516_103))  # 把 NA 赋值为 0

data_standardized <- merged_data %>%
  mutate(
    education = scale(education),
    percentage_60_plus_population = scale(percentage_60_plus_population),
    percentage_migrant_population = scale(percentage_migrant_population),
    bed_accessibility = scale(bed_accessibility),
    total_visits_1516_std = scale(total_visits_1516full_103), 
    total_visits_1617_std = scale(total_visits_1617_103),  # 对 1617 年住院人次标准化
    total_visits_1718_std = scale(total_visits_1718_103),   # 对 1718 年住院人次标准化
    acccess_level_30_dummy = ifelse(acccess_level_30 == "0-30", 1, 0)
    )

glm_model_1 <- glm.nb(total_visits_1617_103 ~ acccess_level_30_dummy+ education +percentage_60_plus_population +percentage_migrant_population+bed_accessibility+total_visits_1516_std+offset(log(population_60)), data = data_standardized)
summary(glm_model_1)
glm_model_2 <- glm.nb(total_visits_1718_103 ~ acccess_level_30_dummy+ education +percentage_60_plus_population +percentage_migrant_population+bed_accessibility+total_visits_1617_std+offset(log(population_60)), data = data_standardized)
summary(glm_model_2)
glm_model_3 <- glm.nb(total_visits_1819_103 ~ acccess_level_30_dummy+ education +percentage_60_plus_population +percentage_migrant_population+bed_accessibility+total_visits_1718_std+offset(log(population_60)), data = data_standardized)
summary(glm_model_3)

reg_results_1_std <- tidy(glm_model_1, conf.int = TRUE) %>% mutate(model = "2016-2017")
reg_results_2_std <- tidy(glm_model_2, conf.int = TRUE) %>% mutate(model = "2017-2018")
reg_results_3_std <- tidy(glm_model_3, conf.int = TRUE) %>% mutate(model = "2018-2019")


reg_results_std <- bind_rows(reg_results_1_std, reg_results_2_std, reg_results_3_std)
reg_results_std <- reg_results_std %>%
  filter(term %in% c("acccess_level_30_dummy", "education", "percentage_60_plus_population", 
                     "percentage_migrant_population", "bed_accessibility", 
                     "total_visits_1516_std", "total_visits_1617_std", "total_visits_1718_std")) %>%
  mutate(term = case_when(
    term == "acccess_level_30_dummy" ~ "Vaccination service accessibility(Ref: Inaccessible)",
    term == "education" ~ "Average years of education",
    term == "percentage_60_plus_population" ~ "Proportion of population aged 60+",
    term == "percentage_migrant_population" ~ "Proportion of migrant population",
    term == "bed_accessibility" ~ "Accessibility of inpatient services in secondary and tertiary hospitals",
    term %in% c("total_visits_1516_std", "total_visits_1617_std", "total_visits_1718_std") ~ "Total visits last season"
  ))

reg_results_std <- reg_results_std %>%
  mutate(term = recode(term,
                       "Vaccination service accessibility(Ref: Inaccessible)" = "Vaccination service \naccessibility(Ref: Inaccessible)",
                       "Average years of education" = "Average years\nof education",
                       "Proportion of population aged 60+" = "Proportion of\npopulation aged 60+",
                       "Proportion of migrant population" = "Proportion of\nmigrant population",
                       "Accessibility of inpatient services in secondary and tertiary hospitals" = "Accessibility of inpatient services\nin secondary and tertiary hospitals",
                       "Total visits last season" = "Total inpatient visits\nlast season"
  ))

reg_results_std$term <- factor(reg_results_std$term, levels = c(
  "Vaccination service \naccessibility(Ref: Inaccessible)", 
  "Average years\nof education", 
  "Proportion of\npopulation aged 60+", 
  "Proportion of\nmigrant population", 
  "Accessibility of inpatient services\nin secondary and tertiary hospitals",
  "Total inpatient visits\nlast season"
))

reg_results_std <- reg_results_std %>%
  mutate(
    IRR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    significance = case_when(
      p.value <= 0.01 ~ "***",
      p.value > 0.01 & p.value <= 0.05 ~ "**",
      p.value > 0.05 & p.value <= 0.10 ~ "*",
      TRUE ~ NA_character_
    )
  )
ggplot(reg_results_std, aes(x = IRR, y = term, color = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 1.2,position = position_dodge(width = 0.5)) +  
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  
  geom_text(aes(label = significance), vjust = -0.5, size = 5, position = position_dodge(width = 0.5), show.legend = FALSE) +  
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540"), labels = c("2016-2017", "2017-2018", "2018-2019")) +  
  scale_x_continuous(breaks = seq(0, max(reg_results_std$IRR, na.rm = TRUE), by = 0.2)) +  
  labs(
    x = "Incidence Rate Ratio (IRR)", 
    y = "", 
    color = "Influenza season(October-March)",  
    caption = "*** p ≤ 0.01, ** 0.01 < p ≤ 0.05, * 0.05 < p ≤ 0.10, No significance p > 0.10"
  ) +
  theme_classic() +  
  theme(
    plot.caption = element_text(hjust = 0.5, size = rel(1.2)), 
    legend.position = "bottom",  
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16, face = "bold"), 
    axis.text.y = element_text(size = 20, face = "bold", color = "black"),  
    axis.text.x = element_text(size = 18, face = "bold", color = "black"),  
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black")  
  )

ggsave("Figure S2.png",  width = 20, height = 16, dpi = 300, bg = "white") 


