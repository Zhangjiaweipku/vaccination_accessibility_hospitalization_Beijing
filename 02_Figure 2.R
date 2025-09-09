library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)
street_shp <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp", quiet = TRUE)

data <- read_excel("D:/vaccination service accessibility/merged_data(无空值整理重复值版).xlsx")
data <- data %>%
  mutate(
    driving_weekday_accessibility = (driving_weekday_morning_accessibility + driving_weekday_afternoon_accessibility) / 2,
    driving_weekend_accessibility = (driving_weekend_morning_accessibility + driving_weekend_afternoon_accessibility) / 2,
    
    walking_weekday_accessibility = (walking_weekday_morning_accessibility + walking_weekday_afternoon_accessibility) / 2,
    walking_weekend_accessibility = (walking_weekend_morning_accessibility + walking_weekend_afternoon_accessibility) / 2,
    
    public_transport_weekday_accessibility = (public_transport_weekday_morning_accessibility + public_transport_weekday_afternoon_accessibility) / 2,
    public_transport_weekend_accessibility = (public_transport_weekend_morning_accessibility + public_transport_weekend_afternoon_accessibility) / 2
  )

data <- data %>%
  mutate(
    driving_accessibility = (5 * driving_weekday_accessibility + 2 * driving_weekend_accessibility) / 7,
    walking_accessibility = (5 * walking_weekday_accessibility + 2 * walking_weekend_accessibility) / 7,
    public_transport_accessibility = (5 * public_transport_weekday_accessibility + 2 * public_transport_weekend_accessibility) / 7
  )
data <- data %>%
  mutate(
    total_accessibility = (driving_accessibility + walking_accessibility + public_transport_accessibility) / 3
  )
data<- data %>%
  mutate(
    total_weekday_accessibility = (driving_weekday_accessibility + walking_weekday_accessibility + public_transport_weekday_accessibility) / 3,
    total_weekend_accessibility = (driving_weekend_accessibility + walking_weekend_accessibility + public_transport_weekend_accessibility) / 3,
  )
street_data<-data
duplicated_names <- street_shp$name[duplicated(street_shp$name)]
print(duplicated_names)  
street_shp %>%
  group_by(name) %>%
  filter(n() > 1) %>%
  arrange(name)
street_shp <- street_shp %>%
  mutate(name = if_else(OBJECTID == 289, paste0(name, "_1"), name))
street_total_data <- street_shp %>%
  left_join(street_data, by = c("name" = "street"))
street_total_data$total_weekday_accessibility[is.na(street_total_data$total_weekday_accessibility)] <- max(street_total_data$total_weekday_accessibility, na.rm = TRUE)
street_total_data$total_weekend_accessibility[is.na(street_total_data$total_weekend_accessibility)] <- max(street_total_data$total_weekend_accessibility, na.rm = TRUE)
street_total_data$total_accessibility[is.na(street_total_data$total_accessibility)] <- max(street_total_data$total_accessibility, na.rm = TRUE)
sum(is.na(street_total_data$total_weekday_accessibility))
street_total_data$weekday_accessibility_group <- cut(
  street_total_data$total_weekday_accessibility,
  breaks = c(0, 15, 30, 60, 90, 120, Inf),
  labels = c("0-15", "15-30", "30-60", "60-90", "90-120", "120+"),
  include.lowest = TRUE, right = FALSE
)
street_total_data$weekend_accessibility_group <- cut(
  street_total_data$total_weekend_accessibility,
  breaks = c(0, 15, 30, 60, 90, 120, Inf),
  labels = c("0-15", "15-30", "30-60", "60-90", "90-120", "120+"),
  include.lowest = TRUE, right = FALSE
)
street_total_data$overall_accessibility_group <- cut(
  street_total_data$total_accessibility,
  breaks = c(0, 15, 30, 60, 90, 120, Inf),
  labels = c("0-15", "15-30", "30-60", "60-90", "90-120", "120+"),
  include.lowest = TRUE, right = FALSE
)
p1 <- ggplot(data = street_total_data) +
  geom_sf(aes(fill = weekday_accessibility_group), color = "black", size = 0.0001) +  
  scale_fill_manual(
    values = c("0-15" = "#f1a340", "15-30" = "#fee08b", "30-60" = "#d9ef8b", "60-90" = "#91bfdb", "90-120" = "#4575b4", "120+" = "#313695"),
    name = "Spatial accessibility (minutes)"
  ) +
  theme_minimal() +
  # labs(title = "Weighted Accessibility on Weekdays") +
  theme(
    legend.position = "none",  
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # 标题居中加粗
    panel.background = element_rect(fill = "white", color = NA),  
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, 1, 1)  
  )
p1
p3 <- ggplot(data = street_total_data) +
  geom_sf(aes(fill = overall_accessibility_group), color = "black", size = 0.0001) +  
  scale_fill_manual(
    values = c("0-15" = "#f1a340", "15-30" = "#fee08b", "30-60" = "#d9ef8b", "60-90" = "#91bfdb", "90-120" = "#4575b4", "120+" = "#313695"),
    name = "Spatial accessibility(minutes)"
  ) +
  theme_minimal() +
  # labs(title = "Overall Weighted Accessibility") +
  theme(
    legend.position = "none",  
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    panel.background = element_rect(fill = "white", color = NA),  
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, 1, 1)  
  )
p3
p2 <- ggplot(data = street_total_data) +
  geom_sf(aes(fill = weekend_accessibility_group), color = "black", size = 0.0001) +  
  scale_fill_manual(
    values = c("0-15" = "#f1a340", "15-30" = "#fee08b", "30-60" = "#d9ef8b", "60-90" = "#91bfdb", "90-120" = "#4575b4", "120+" = "#313695"),
    name = "Spatial accessibility (minutes)"
  ) +
  theme_minimal() +
  # labs(title = "Weighted Accessibility on Weekends") +
  theme(
    legend.position.inside = c(0.8, 0.40), 
    legend.justification = c(1, 0), 
    legend.key.size = unit(0.8, "cm"),  
    legend.title = element_text(size = 24, face = "bold"),  
    legend.text = element_text(size = 24), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    panel.background = element_rect(fill = "white", color = NA),  
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, 1, 1)
  )
p2

street_total_data <- street_total_data %>%
  mutate(accessibility_increase_weekend = total_weekend_accessibility - total_weekday_accessibility)

street_total_data$accessibility_increase_group <- cut(
  street_total_data$accessibility_increase_weekend,
  breaks = c(-Inf, -3,0, 15, 30, 60, 90, Inf),
  labels = c("<-3", "<0","0-15", "15-30", "30-60", "60-90", "90+"),
  include.lowest = TRUE
)

p4 <- ggplot(data = street_total_data) +
  geom_sf(aes(fill = accessibility_increase_group), color = "black", size = 0.0001) +
  scale_fill_manual(
    values = c(
      "<-3"    = "#1a9850",
      "<0"   = "#91cf60",
      "0-15"   = "#fee08b",
      "15-30"  = "#fdae61",
      "30-60"  = "#f46d43",
      "60-90"  = "#d73027",
      "90+"    = "#67001f"
    ),
    name = "Difference in \nSpatial Accessibility(minutes)"
  ) +
  theme_minimal() +
  # labs(title = "Accessibility Change Between Weekend and Weekday") +
  theme(
    legend.position.inside = c(0.80, 0.40),  
    legend.justification = c(1, 0), 
    legend.key.size = unit(0.8, "cm"),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 24),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, 1, 1)
  )

p4
p1_label <- p1 + 
  annotate("text", x = -Inf, y = Inf, label = "A", 
           hjust = -0.3, vjust = 1.3, size = 10, fontface = "bold") +
  theme(axis.title = element_blank())

p2_label <- p2 + 
  annotate("text", x = -Inf, y = Inf, label = "B", 
           hjust = -0.3, vjust = 1.3, size = 10, fontface = "bold") +
  theme(axis.title = element_blank())

p3_label <- p3 + 
  annotate("text", x = -Inf, y = Inf, label = "C", 
           hjust = -0.3, vjust = 1.3, size = 10, fontface = "bold") +
  theme(axis.title = element_blank())

p4_label <- p4 + 
  annotate("text", x = -Inf, y = Inf, label = "D", 
           hjust = -0.3, vjust = 1.3, size = 10, fontface = "bold") +
  theme(axis.title = element_blank())

p1_no_legend <- p1_label + theme(legend.position = "none", plot.margin = margin(5, 15, 5, 5))
p3_no_legend <- p3_label + theme(legend.position = "none", plot.margin = margin(5, 15, 5, 5))

p2_label <- p2_label + theme(plot.margin = margin(5, 5, 5, 15))
p4_label <- p4_label + theme(plot.margin = margin(5, 5, 5, 15))

row1 <- p1_no_legend | p2_label
row2 <- p3_no_legend | p4_label

combined_plot <- row1 / row2 +
  plot_layout(widths = c(1, 1), heights = c(1, 1))

print(combined_plot)
ggsave("Figure 2.png", plot = combined_plot, width = 18, height =12, dpi = 300)

