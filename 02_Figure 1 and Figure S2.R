library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)
street_shp <- st_read("D:/北京地图(1)/2023北京市街道地图/北京市_乡镇2020版.shp", quiet = TRUE)

street_data <- read_excel("D:/vaccination service accessibility/street_level_SES_merged_data（完整补充各年龄段人口数）.xlsx")
duplicated_names <- street_shp$name[duplicated(street_shp$name)]
print(duplicated_names)  
street_shp %>%
  group_by(name) %>%
  filter(n() > 1) %>%
  arrange(name)
street_shp <- street_shp %>%
  mutate(name = if_else(OBJECTID == 289, paste0(name, "_1"), name))
street_shp %>% filter(OBJECTID == 289) %>% select(name)
print(street_data %>%
        filter(name == "八里庄街道办事处"), width = Inf)
street_data <- street_data %>%
  mutate(name = if_else(district_name == "海淀区" & name == "八里庄街道办事处",
                        paste0(name, "_1"),
                        name))

street_shp <- street_shp %>%
  left_join(street_data, by = c("name" = "name"))


colnames(street_shp) <- gsub(" ", "_", colnames(street_shp)) 
str(street_shp$average_years_of_education)
sum(is.na(street_shp$average_years_of_education)) 
street_shp <- street_shp %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
unique(street_shp$average_years_of_education)
head(street_shp)

p1 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = average_years_of_education), color = "black", size = 0.0001) +  
  scale_fill_viridis_c(option = "plasma", 
                       name = "Average Years of Education", 
                       limits = c(5, max(street_shp$average_years_of_education, na.rm = TRUE)), 
                       oob = scales::squish) +  
  theme_minimal() +
  labs(title = "Distribution of Average Years of Education in Beijing") +
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

p2 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = percentage_60_plus_population), color = "black", size = 0.0001) +  
  scale_fill_viridis_c(option = "plasma", 
                       name = "Proportion of Population Aged 60+", 
                       limits = range(street_shp$percentage_60_plus_population, na.rm = TRUE), 
                       oob = scales::squish) +  
  theme_minimal() +
  labs(title = "Distribution of Proportion of Elderly Population (60+) in Beijing") +
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

p3 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = bed_accessibility), color = "black", size = 0.0001) +  
  scale_fill_viridis_c(option = "plasma", 
                       name = "Bed per 1,000 Population", 
                       limits = c(0, 9),  
                       oob = scales::squish) +  
  theme_minimal() +
  labs(title = "Distribution of Accessibility of Secondary and Tertiary hospitals") +
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

p3
p4<-ggplot(data = street_shp) +
  geom_sf(aes(fill = percentage_migrant_population), color = "black", size = 0.0001) +  
  scale_fill_viridis_c(option = "plasma", 
                       name = "Proportion of Migrant Population", 
                       limits = range(street_shp$percentage_migrant_population, na.rm = TRUE), 
                       oob = scales::squish) +  
  theme_minimal() +
  labs(title = "Distribution of Proportion of Migrant Population in Beijing") +
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

p5 <- ggplot(street_shp, aes(x = average_years_of_education)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "Average Years of Education", 
       y = "Density") +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05)) +  
  theme_minimal(base_size = 8) +  
  theme(
    panel.grid = element_blank(),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6), 
    legend.position = "none"  
  )

p6 <- ggplot(street_shp, aes(x = percentage_60_plus_population)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "Proportion of Population Aged 60+", 
       y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )

p7<- ggplot(street_shp, aes(x = bed_accessibility)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "Bed per 1,000 Population", 
       y = "Density") +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05)) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )

p8 <- ggplot(street_shp, aes(x = percentage_migrant_population)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "Proportion of Migrant Population", 
       y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )
p8
population_breaks <- quantile(street_shp$population, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
library(scales)
population_labels <- label_comma(accuracy = 1)(population_breaks)

p9 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = population), color = "black", size = 0.0001) +  
  scale_fill_viridis_b(
    option = "plasma",
    name = "All Population",
    breaks = population_breaks,
    labels = population_labels,
    n.breaks = 6
  ) +  
  theme_minimal() +
  labs(title = "Distribution of All Population in Beijing") +
  theme(
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.justification = c(0.5, 0),  
    legend.key.size = unit(0.6, "cm"),  
    legend.key.width = unit(2.0, "cm"),  
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

p10 <- ggplot(street_shp, aes(x = population)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "Population", 
       y = "Density") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )

visit_breaks <- c(0,15, 30, 45, 100, 400)
visit_labels <- label_comma(accuracy = 1)(visit_breaks)

p11 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = total_visits_1516), color = "black", size = 0.0001) +  
  scale_fill_viridis_b(
    option = "plasma",    
    name = "The number of inpatient visits",
    breaks = visit_breaks,
    ###              labels = visit_labels
  ) +
  labs(title = "Distribution of Inpatient Visits in 2015–16 influenza season") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = c(0.5, 0),
    legend.key.size = unit(0.6, "cm"),
    legend.key.width = unit(2.0, "cm"),
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

p12 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = total_visits_1617), color = "black", size = 0.0001) +  
  scale_fill_viridis_b(
    option = "plasma",         
    name = "The number of inpatient visits",
    breaks = visit_breaks,
  ) +
  labs(title = "Distribution of Inpatient Visits in 2016–17 influenza season") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = c(0.5, 0),
    legend.key.size = unit(0.6, "cm"),
    legend.key.width = unit(2.0, "cm"),
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

p13 <- ggplot(data = street_shp) +
  geom_sf(aes(fill = total_visits_1718), color = "black", size = 0.0001) +  
  scale_fill_viridis_b(
    option = "plasma",         
    name = "The number of inpatient visits",
    breaks = visit_breaks,
  ) +
  labs(title = "Distribution of Inpatient Visits in 2017–18 influenza season") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = c(0.5, 0),
    legend.key.size = unit(0.6, "cm"),
    legend.key.width = unit(2.0, "cm"),
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
  
p14 <- ggplot(street_shp, aes(x = total_visits_1516)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "The number of inpatient visits in 2015–2016 influenza season", 
       y = "Density") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )

p15 <- ggplot(street_shp, aes(x = total_visits_1617)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "The number of inpatient visits in 2016–2017 influenza season", 
       y = "Density") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )
  
p16 <- ggplot(street_shp, aes(x = total_visits_1718)) +
  geom_density(fill = "#808080", color = "#808080", alpha = 0.4, size = 0.8) + 
  labs(x = "The number of inpatient visits in 2017–2018 influenza season", 
       y = "Density") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    legend.position = "none"
  )

p_list <- list(p9, p1, p2, p4, p3, p11, p12, p13)

final_plot <- plot_grid(
  plot_grid(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]], nrow = 1, align = "hv", rel_widths = c(1,1,1,1), rel_heights = 1.8),
  plot_grid(p_list[[5]], p_list[[6]], p_list[[7]], p_list[[8]], nrow = 1, align = "hv", rel_widths = c(1,1,1,1), rel_heights = 1.8),
  ncol = 1, align = "v", rel_heights = c(1,1) 
)
ggsave("Figure 1.png", plot = final_plot, width = 32, height = 20, dpi = 300,bg = "white")

p_list <- list(p10, p5, p6, p8, p7, p14, p15, p16)

final_plot1 <- plot_grid(
  plot_grid(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]], nrow = 1, align = "hv", rel_widths = c(1,1,1,1), rel_heights = 1),
  plot_grid(p_list[[5]], p_list[[6]], p_list[[7]], p_list[[8]], nrow = 1, align = "hv", rel_widths = c(1,1,1,1), rel_heights = 1),
  ncol = 1, align = "v", rel_heights = c(1,1) 
)

print(final_plot1)   
ggsave("Figure S2.png", plot = final_plot1, width = 28, height = 14, dpi = 300,bg = "white")

