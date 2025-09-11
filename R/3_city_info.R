#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: City Info
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 9/11/2025
# Purpose: Export city list to get population from claude. need to update later, 
#           but quick and dirty for now
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#clear memory
remove(list=ls())

#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)

#Download she ddata
pwi_shed  <- st_read("data//TNC_WaterIntakes//wat_012_city_water_intakes//CWM_v2_2//World_Watershed8.shp")

#Download US data
states <- states() %>% dplyr::filter(!(STUSPS %in% c('HI','VI','MP', 'GU', 'AK', 'AS', 'PR')))

#Rerpoject and clip to continental US
states <- st_transform(states, 5070)
pwi_shed <- st_transform(pwi_shed, 5070) 
pwi_shed <- pwi_shed[states,]

#pull out city name
output <- pwi_shed %>% st_drop_geometry() %>% dplyr::select(DVSN_ID, DVSN_Name, City_Name)
write_csv(output, "output/city_names.csv")

# sent to claude and asked for population of cities
# now pull that back in 
pop <- read_csv("output/city_pop.csv") %>% dplyr::select(DVSN_ID, Population_2024)


