#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Headwaters
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 9/9/2025
# Purpose: Examine how many headwater streams are upstream of surface drinking water intakes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Sources: 
#   Sample of Surface Water Intake Data (TNC): https://water.nature.org/waterblueprint
#   Sample of WWTP data (HydroWaste): https://www.hydrosheds.org/products/hydrowaste

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

#downlaod VAA flowline data
vaa <- read_csv('output//vaa_flow_length.csv')

#Load Public Water Intake Spatial Data
pwi_point <- st_read("data//TNC_WaterIntakes//wat_012_city_water_intakes//CWM_v2_2//Snapped_Withdrawal_Points.shp")
pwi_shed  <- st_read("data//TNC_WaterIntakes//wat_012_city_water_intakes//CWM_v2_2//World_Watershed8.shp")

#Load US data
states <- states() %>% dplyr::filter(!(STUSPS %in% c('HI','VI','MP', 'GU', 'AK', 'AS', 'PR')))

#Rerpoject and clip to continental US
states <- st_transform(states, 5070)
pwi_point <- st_transform(pwi_point, 5070) 
pwi_point <- pwi_point[states,]
pwi_shed <- st_transform(pwi_shed, 5070) 
pwi_shed <- pwi_shed[states,]

#plot for funzies
states %>% st_geometry() %>% plot()
pwi_point %>% st_geometry() %>% plot(., add=T, col="blue", pch=19)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Estimate headwater streamlength and runoff proportion per watershed ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function
fun <- function(n){

  #Step 1: Identify individual watershed shape
  shed <- pwi_shed[n,]
  
  #Step 2: reproject shed into decimal degree
  shed_proj <- shed %>% st_transform(., 4326) 
  
  #Step 3: find min and max coordinates for shed_proj
  shed_coords <- st_coordinates(shed_proj) %>% 
    as_tibble() %>% 
    summarise(
      x_min=  min(X, na.rm=T), 
      x_max = max(X, na.rm=T),
      y_min = min(Y, na.rm=T), 
      y_max = max(Y, na.rm=T))
  
  #Step 4: Filter VAA points to min and max coords
  vaa<-vaa %>% 
    filter(XCoord >= shed_coords$x_min) %>% 
    filter(XCoord <= shed_coords$x_max) %>% 
    filter(YCoord >= shed_coords$y_min) %>% 
    filter(YCoord <= shed_coords$y_max) 
    
  #Step 5: Convert VAA points to sf and reproject to same coordinate as shed
  vaa_pnts <- 
    st_as_sf(vaa, 
        coords = c("XCoord", "YCoord"), 
        crs = 4326) %>% 
    st_transform(., crs = st_crs(shed))
  
  #Step 6: Crop points to within the watershed area
  vaa_pnts <- vaa_pnts[shed,]
  
  #Step 7: Calculate stats
  output <- vaa_pnts %>% 
    st_drop_geometry() %>% 
    mutate(qincrama = as.numeric(paste(qincrama))) %>% 
    mutate(reach_type = ifelse(SO12 == 0, "downstream", "upstream")) %>% 
    group_by(reach_type) %>% 
    summarise(
      total_length  = sum(lengthkm,na.rm=T),
      annual_runoff = sum(qincrama, na.rm=T)
    ) %>% 
    pivot_longer(cols = c(total_length, annual_runoff),
                 names_to = "metric",
                 values_to = "value") %>%
    unite("category", reach_type, metric, sep = "_") %>%
    pivot_wider(names_from = category,
                values_from = value) %>% 
    mutate(DVSN_ID = shed$DVSN_ID) %>% 
    relocate(DVSN_ID)
  
  #Print output
  output
}

#Apply function
t0<-Sys.time()
df <- 
  lapply(
    X = seq(1, 25), 
    FUN = fun) %>% 
  bind_rows()
tf<-Sys.time()
tf-t0

