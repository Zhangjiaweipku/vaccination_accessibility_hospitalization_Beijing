library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(ggplot2)
library(patchwork)
library(tidyr)
library(splines)

population_data <- read_excel("D://vaccination service accessibility/北京市各地区分年龄的常住人口.xlsx")
accessibility_data <- read_excel("D://vaccination service accessibility/all_data(总可及性按照三个交通方式占比相同计算).xlsx")

accessibility_data <- accessibility_data %>%
  mutate(name = str_replace_all(name, "办事处", ""))

merged_data <- accessibility_data %>%
  left_join(population_data, by = c("name" = "地区"))

filtered_data <- merged_data %>%
  dplyr::select(`all population`, population_60, total_accessibility) %>%
  dplyr::mutate(
    all_population = `all population`,
    elderly_population = population_60,
    Population_Group = ntile(all_population, 10)
  ) %>%
  dplyr::select(-`all population`)

population_summary <- filtered_data %>%
  group_by(Population_Group) %>%
  summarise(
    Group_Population = sum(all_population, na.rm = TRUE),
    Elderly_Population = sum(elderly_population, na.rm = TRUE)
  ) %>%
  mutate(
    Non_Elderly_Population = Group_Population - Elderly_Population,
    elderly_percentage = 100 * Elderly_Population / Group_Population,
    total_label = paste0(
      sprintf("%.1f", Group_Population / 10000), "\n",
      sprintf("%.1f", elderly_percentage), "%"
    )
  )

population_long <- population_summary %>%
  select(Population_Group, Elderly_Population, Non_Elderly_Population, Group_Population, total_label) %>%
  pivot_longer(cols = c("Elderly_Population", "Non_Elderly_Population"),
               names_to = "Age_Group", values_to = "Population") %>%
  mutate(
    Population = Population / 10000,
    Age_Group = factor(Age_Group,
                       levels = c("Non_Elderly_Population", "Elderly_Population"),
                       labels = c("Total population", "Older adults (60+)"))
  )

label_height <- max(population_long %>% group_by(Population_Group) %>%
                      summarise(total = sum(Population)) %>% pull(total)) + 5

percent_labels <- paste0(seq(0, 90, by = 10), "-", seq(10, 100, by = 10), "%")

bar_plot <- ggplot(population_long, aes(x = as.factor(Population_Group), y = Population, fill = Age_Group)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(
    data = population_summary,
    aes(x = as.factor(Population_Group), y = label_height, label = total_label),
    inherit.aes = FALSE,
    vjust = 0, size = 5, fontface = "bold", color = "black"
  ) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red", size = 1) +
  scale_x_discrete(labels = percent_labels) +  
  labs(
    y = "Total Population (ten thousand people)",
    x = NULL,
    fill = NULL
  ) +
  scale_fill_manual(
    values = c("Older adults (60+)" = "gray70", "Total population" = "white")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.ticks.x = element_blank(), 
    legend.position = "top",
    legend.text = element_text(size = 14)
  )


bar_plot

median_data <- filtered_data %>%
  group_by(Population_Group) %>%
  summarise(median_access = median(total_accessibility, na.rm = TRUE)) %>%
  mutate(percent_label = percent_labels)

smooth_model <- smooth.spline(x = median_data$Population_Group, y = median_data$median_access)
smoothed_curve <- data.frame(
  Population_Group = smooth_model$x,
  Smooth_Access = smooth_model$y
)

box_plot <- ggplot(filtered_data, aes(x = as.factor(Population_Group), y = total_accessibility)) +
  geom_boxplot(fill = "white", color = "black") +
  geom_line(data = smoothed_curve, aes(x = Population_Group, y = Smooth_Access),
            color = "black", size = 1.2) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red", size = 1) +  # Place the dashed line between 20-30% and 30-40%
  scale_x_discrete(labels = percent_labels) +
  labs(
    x = "Population Percentile Group",
    y = "Travel time (minute)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  )

final_plot <- bar_plot / box_plot + plot_layout(heights = c(1.2, 1.2))
final_plot
ggsave("Figure S2.png", final_plot, width = 10, height = 10, dpi = 300, bg = "white")
