#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: PWI Plots
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 9/10/2025
# Purpose: Visualize headwater contributions to PWIs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Environment --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
remove(list=ls())

#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)
library(patchwork)

#downlaod VAA flowline data
pwi <- read_csv("output/pwi_output.csv")

#Load Public Water Intake Spatial Data
pwi_point <- st_read("data//TNC_WaterIntakes//wat_012_city_water_intakes//CWM_v2_2//Snapped_Withdrawal_Points.shp")

#Load US data
states <- states() %>% dplyr::filter(!(STUSPS %in% c('HI','VI','MP', 'GU', 'AK', 'AS', 'PR')))

#Rerpoject and clip to continental US
states <- st_transform(states, 5070)
pwi_point <- st_transform(pwi_point, 5070) 
pwi_point <- pwi_point[states,]

#plot for funzies
states %>% st_geometry() %>% plot()
pwi_point %>% st_geometry() %>% plot(., add=T, col="blue", pch=19)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Data wrangling -----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#First estimate proportion of stream length and proportion of annual runoff
pwi <- pwi %>% 
  mutate(
    upstream_length_prop = upstream_total_length/(downstream_total_length + upstream_total_length),
    upstream_runoff_prop = upstream_annual_runoff/(downstream_annual_runoff + upstream_annual_runoff)) 

#Now add pop data
pop <- read_csv("output/city_pop.csv") %>% dplyr::select(DVSN_ID, Population_2024)

#Left join to pwi points
pwi_point <- pwi_point %>% left_join(., pwi) %>% left_join(., pop)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Summary stats -------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Summary stats
summary_stats <- pwi %>% 
  #Summarise length stats
  summarise(
    upstream_length_q1     = quantile(upstream_length_prop, 0.25, na.rm=T),
    upstream_length_median = quantile(upstream_length_prop, 0.50, na.rm=T),
    upstream_length_q3     = quantile(upstream_length_prop, 0.75, na.rm=T),
    upstream_runoff_q1      = quantile(upstream_runoff_prop, 0.25, na.rm=T),
    upstream_runoff_median  = quantile(upstream_runoff_prop, 0.50, na.rm=T),
    upstream_runoff_q3      = quantile(upstream_runoff_prop, 0.75, na.rm=T))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Plots plots plots---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter out -9999 values from Population_2024 column
pwi_point_filtered <- pwi_point %>% 
  filter(Population_2024 != -9999 & !is.na(Population_2024))

# Create the map (panel a) with population-scaled points and legend
map_plot <- ggplot() +
  geom_sf(data = states, fill = "grey80", color = "gray20", size = 0.3, alpha = 0.8) +
  # Enhanced water intake points with glow effect
  geom_sf(data = pwi_point_filtered, 
          aes(size = Population_2024), 
          color = "#ffffff", 
          alpha = 0.8,
          stroke = 0.3) +
  geom_sf(data = pwi_point_filtered, 
          aes(size = Population_2024), 
          color = "#1e3a8a", 
          alpha = 0.9) +
  scale_size_continuous(
    name = "Population size",
    range = c(0.5, 4),
    labels = scales::comma_format(),
    guide = guide_legend(override.aes = list(alpha = 1, color = "#1e3a8a"))
  ) +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(hjust = 0, size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 10)
  ) 

# Create histogram for upstream_length_prop (panel b) - using filtered data
hist_length <- ggplot(pwi_point_filtered, aes(x = upstream_length_prop*100)) +
  geom_histogram(bins = 30, fill = "#861F41", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = quantile(upstream_length_prop*100, 0.5, na.rm = TRUE)), 
             linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", 
           x = Inf, y = Inf,
           label = paste0("Median =\n", sprintf("%.0f", quantile(pwi_point_filtered$upstream_length_prop*100, 0.5, na.rm = TRUE)), "%"),
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "black", lineheight = 0.9) +
  scale_x_continuous(limits = c(60, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 13, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray40", linewidth = 0.5)
  ) +
  labs(
    x = "Headwater Length [%]",
    y = "Number of Drinking\nWater Intakes"
  )

# Create histogram for upstream_runoff_prop (panel c) - using filtered data
hist_runoff <- ggplot(pwi_point_filtered, aes(x = upstream_runoff_prop*100)) +
  geom_histogram(bins = 30, fill = "#CF4420", color = "white", alpha = 0.8, linewidth = 0.3) +
  geom_vline(aes(xintercept = quantile(upstream_runoff_prop*100, 0.5, na.rm = TRUE)), 
             linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", 
           x = Inf, y = Inf,
           label = paste0("Median =\n", sprintf("%.0f", quantile(pwi_point_filtered$upstream_runoff_prop*100, 0.5, na.rm = TRUE)), "%"),
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "black", lineheight = 0.9) +
  scale_x_continuous(limits = c(60, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 13, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray40", linewidth = 0.5)
  ) +
  labs(
    x = "Headwater Runoff [%]",
    y = "Number of Drinking\nWater Intakes"
  )

# Simple patchwork layout - just the basic arrangement
final_plot <- map_plot / (hist_length | hist_runoff) + 
  plot_layout(heights = c(3.5, 1.5))

# Display the plot
print(final_plot)

# Save the plot - make it a bit wider to accommodate the legend
ggsave("output/pwi_analysis_figure.png", final_plot, width = 6, height = 5, dpi = 300)