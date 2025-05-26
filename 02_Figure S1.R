library(sf)
library(ggplot2)
library(dplyr)

central_districts <- c("东城区", "西城区", "朝阳区", "海淀区", "丰台区", "石景山区")

street_shp <- street_total_data %>%
  mutate(area_type = if_else(district_name %in% central_districts, "urban areas", "suburban areas"))

fill_colors <- c(
  "urban areas" = "#fca636cc",
  "suburban areas" = "#440154cc"  
)

p_area <- ggplot(data = street_shp) +
  geom_sf(aes(fill = area_type), color = "black", size = 0.0001) +  
  scale_fill_manual(
    values = fill_colors,
    name = "Area Type"
  ) +
  theme_minimal() +
  labs(title = "Distribution of Urban and Suburban Areas in Beijing") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = c(0.5, 0),
    legend.key.size = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.title.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 0)),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )
print(p_area)
ggsave("beijing_urban_suburban_map.png", plot = p_area, width = 10, height = 8, dpi = 300,bg = "white")

