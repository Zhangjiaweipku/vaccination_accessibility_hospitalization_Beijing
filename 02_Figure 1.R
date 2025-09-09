library(ggplot2)
library(ggridges)
library(readxl)
library(dplyr)
library(tidyr)
library(patchwork) 

driving_data <- read_excel("D:/vaccination service accessibility/driving_accessibility_data.xlsx")
walking_data <- read_excel("D:/vaccination service accessibility/walking_accessibility_data.xlsx")
public_transport_data <- read_excel("D:/vaccination service accessibility/public_transportation_accessibility_data.xlsx")

drive_long <- pivot_longer(driving_data, cols = c(weekday_morning, weekday_afternoon, weekend_morning, weekend_afternoon), 
                           names_to = "scenario", values_to = "time") %>% 
  mutate(mode = "Driving")

walk_long <- pivot_longer(walking_data, cols = c(weekday_morning, weekday_afternoon, weekend_morning, weekend_afternoon), 
                          names_to = "scenario", values_to = "time") %>% 
  mutate(mode = "Walking")

transit_long <- pivot_longer(public_transport_data, cols = c(weekday_morning, weekday_afternoon, weekend_morning, weekend_afternoon), 
                             names_to = "scenario", values_to = "time") %>% 
  mutate(mode = "Public Transport")


data_long <- bind_rows(drive_long, walk_long, transit_long)

data_avg <- data_long %>%
  filter(mode %in% c("Walking", "Public Transport")) %>%
  group_by(mode, week_type = ifelse(grepl("weekday", scenario), "Weekday", "Weekend"), from) %>%
  mutate(time = mean(time, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    scenario_label = case_when(
      mode == "Walking" ~ week_type, 
      mode == "Public Transport" ~ week_type
    )
  )


data_drive <- data_long %>%
  filter(mode == "Driving") %>%
  mutate(
    week_type = ifelse(grepl("weekday", scenario), "Weekday", "Weekend"),
    peak_type = ifelse(grepl("morning", scenario), "Peak", "Off-peak"),
    scenario_label = case_when(
      peak_type == "Peak" ~ paste(week_type, "Peak"),
      peak_type == "Off-peak" ~ paste(week_type, "Off-peak")
    )
  )

median_walk <- data_avg %>%
  filter(mode == "Walking") %>%
  group_by(scenario_label) %>%
  summarise(median_time = median(time, na.rm = TRUE))

median_transit <- data_avg %>%
  filter(mode == "Public Transport") %>%
  group_by(scenario_label) %>%
  summarise(median_time = median(time, na.rm = TRUE))


colors <- c("Weekday" = "#fca636", "Weekend" = "#440154") 
line_types <- c("Peak" = "solid", "Off-peak" = "dashed")   


custom_theme <- theme_minimal(base_size = 42) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 44, face = "bold"),  
    panel.grid = element_blank(),  
    panel.border = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.text.x = element_text(size = 22),  
    axis.text.y = element_text(size = 25),  
    legend.position = "right"
  )


p_walk <- ggplot(filter(data_avg, mode == "Walking"), aes(x = time, color = scenario_label)) +
  geom_density(size = 1) +  
  scale_color_manual(values = colors) +  
  scale_x_continuous(limits = c(0, 1300), breaks = seq(0, 1300, 100)) +  
  labs(title = "Walking", x = "Spatial accessibility (minutes)", y = "Density", color = "Scenario") +
  custom_theme +
  theme(legend.position = "none") +  # 不显示图例

  geom_vline(data = filter(median_walk, grepl("Weekday", scenario_label)), aes(xintercept = median_time, color = "Weekday"), 
             linetype = "solid", size = 1) +
  geom_vline(data = filter(median_walk, grepl("Weekend", scenario_label)), aes(xintercept = median_time, color = "Weekend"), 
             linetype = "solid", size = 1)
p_walk


p_transit <- ggplot(filter(data_avg, mode == "Public Transport"), aes(x = time, color  = scenario_label)) +
  geom_density(size = 1) +  
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  # scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +  
  labs(title = "Public transport", x = "Spatial accessibility (minutes)", y = "Density", fill = "Scenario") +
  custom_theme +
  theme(legend.position = "none")+  
  geom_vline(data = filter(median_transit, grepl("Weekday", scenario_label)), aes(xintercept = median_time, color = "Weekday"), 
             linetype = "solid", size = 1) +
  geom_vline(data = filter(median_transit, grepl("Weekend", scenario_label)), aes(xintercept = median_time, color = "Weekend"), 
             linetype = "solid", size = 1)
p_transit

data_drive <- data_drive %>%
  mutate(scenario_label = factor(scenario_label, 
                                 levels = c("Weekday Peak", "Weekday Off-peak", 
                                            "Weekend Peak", "Weekend Off-peak")))

colors <- c("Weekday" = "#fca636", "Weekend" = "#440154")  
line_types <- c("Peak" = "solid", "Off-peak" = "dashed")

data_drive <- data_drive %>%
  mutate(DayType = ifelse(grepl("Weekday", scenario_label), "Weekday", "Weekend"),
         PeakType = ifelse(grepl("Peak", scenario_label), "Peak", "Off-peak"))
data_drive$DayType <- factor(data_drive$DayType, levels = c("Weekday", "Weekend"))
data_drive$PeakType <- factor(data_drive$PeakType, levels = c("Peak", "Off-peak"))

median_drive <- data_drive %>%
  group_by(DayType, PeakType) %>%
  summarise(median_time = median(time, na.rm = TRUE))

p_drive_peak <- data_drive %>%
  filter(PeakType == "Peak") %>%
  left_join(median_drive %>% filter(PeakType == "Peak"), by = c("DayType", "PeakType")) %>%
  ggplot(aes(x = time, color = DayType)) +
  geom_density(size = 1) +
  geom_vline(aes(xintercept = median_time, color = DayType), size = 1, linetype = "solid", show.legend = FALSE) +
  scale_color_manual(name = "Day Type", values = colors) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(title = "Driving during peak hours",
       x = "Spatial accessibility (minutes)", y = "Density") +
  custom_theme +
  theme(legend.position = "none")

p_drive_offpeak <- data_drive %>%
  filter(PeakType == "Off-peak") %>%
  left_join(median_drive %>% filter(PeakType == "Off-peak"), by = c("DayType", "PeakType")) %>%
  ggplot(aes(x = time, color = DayType)) +
  geom_density(size = 1) +
  geom_vline(aes(xintercept = median_time, color = DayType), size = 1, linetype = "solid", show.legend = FALSE) +
  scale_color_manual(name = "Day Type", values = colors) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(title = "Driving during off-peak hours",
       x = "Spatial accessibility (minutes)", y = "Density") +
  custom_theme +
  theme(legend.position = "none")

# p_walk 和 p_transit

p_legend_base <- data_drive %>% 
  ggplot(aes(x = 1, y = time, color = DayType)) +
  geom_line(aes(group = DayType), size = 1.5) +
  scale_color_manual(name = "Day Type", values = colors, guide = guide_legend(order = 1)) +
  theme_void() +
  theme(
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 40),
    legend.key.size = unit(2, "cm"),
    legend.spacing.x = unit(1, "cm"),
    legend.spacing.y = unit(0.5, "cm")
  )

legend1 <- cowplot::get_legend(p_legend_base)
p_walk <- p_walk + annotate("text", x = 0, y = Inf, label = "A", hjust = 0.8, vjust = 1.2, size = 12, fontface = "bold")
p_transit <- p_transit + annotate("text", x = 0, y = Inf, label = "B", hjust = 0.8, vjust = 1.2, size = 12, fontface = "bold")
p_drive_peak <- p_drive_peak + annotate("text", x = 0, y = Inf, label = "C", hjust = 0.8, vjust = 1.2, size = 12, fontface = "bold")
p_drive_offpeak <- p_drive_offpeak + annotate("text", x = 0, y = Inf, label = "D", hjust = 0.8, vjust = 1.2, size = 12, fontface = "bold")

combined_plots <- (p_walk | p_transit) / (p_drive_peak | p_drive_offpeak) + 
  plot_layout(heights = c(2.5, 2.5))

final_plot <- cowplot::plot_grid(
  combined_plots,
  legend1,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

print(final_plot)
ggsave("Figure 1.png", plot = final_plot, width = 24, height =18, dpi = 300,bg = "white")

