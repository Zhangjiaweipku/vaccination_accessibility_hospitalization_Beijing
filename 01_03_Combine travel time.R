library(readxl)
library(dplyr)
library(writexl)
library(stringr)
####Replace "walking" with "driving" or "public_transportation" to obtain travel times for those modes

file_list <- list.files(pattern = "walking_paths_results_weekend_morning_batch_.*\\.xlsx$")  
all_results <- list()
for (file in file_list) {
  temp_data <- read_xlsx(file)
  all_results <- bind_rows(all_results, temp_data)
}

minimal_results <- all_results %>%
  group_by(from) %>%  
  slice_min(duration, with_ties = FALSE) %>%  
  ungroup()
 
write_xlsx(minimal_results, "walking_paths_results_weekend_morning_minimal_durations.xlsx")

file_list_weekendafternoon <- list.files(pattern = "walking_paths_results_weekend_afternoon_batch_.*\\.xlsx$")  
all_results <- list()

for (file in file_list_weekendafternoon) {
  temp_data <- read_xlsx(file)
  all_results <- bind_rows(all_results, temp_data)
}
minimal_results <- all_results %>%
  group_by(from) %>%  
  slice_min(duration, with_ties = FALSE) %>%  
  ungroup()

write_xlsx(minimal_results, "walking_paths_results_weekend_afternoon_minimal_durations.xlsx")

library(purrr)  

file_paths <- c("walking_paths_results_weekday_morning_minimal_durations.xlsx", "walking_paths_results_weekday_afternoon_minimal_durations.xlsx", "walking_paths_results_weekend_morning_minimal_durations.xlsx", "walking_paths_results_weekend_afternoon_minimal_durations.xlsx")  # 替换为实际文件路径
scenarios <- c("weekday_morning", "weekday_afternoon", "weekend_morning", "weekend_afternoon")

data_list <- list()

for (i in seq_along(file_paths)) {
  data_list[[i]] <- read_excel(file_paths[i]) %>%
    dplyr::select(from, duration) %>%  
    dplyr::rename(!!scenarios[i] := duration)  
}

merged_data <- data_list %>%
  reduce(full_join, by = "from")  

accessibility_data <- merged_data %>%
  mutate(across(-from, ~ round(as.numeric(.) / 60, 2)))  
write_xlsx(accessibility_data, "walking_accessibility_data.xlsx")

