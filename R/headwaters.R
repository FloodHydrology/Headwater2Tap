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



# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 3.0 Plots --------------------------------------------------------------------
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 3.1 Density map --------------------------------------------------------------
# #prep pwi_point data
# pwi_point <- pwi_point %>% 
#   mutate(
#     lat = st_coordinates(pwi_point)[,1], 
#     lon = st_coordinates(pwi_point)[,2],
#     d_wwtps_100 = d_wwtps*100) %>% 
#   filter(d_wwtps_100<1)
# 
# #Create density map
# density_map <- pwi_point %>%
#   arrange(d_wwtps) %>% 
#   ggplot()+
#   geom_sf(data = states, lwd=0.5) + 
#   geom_point(
#     aes(
#       x=lat,
#       y=lon,
#       color = d_wwtps_100, 
#       size =  d_wwtps_100, 
#       alpha = d_wwtps_100, 
#       #shape= NP
#     )) +
#   scale_color_distiller(
#     palette = 'Spectral',
#     direction = -1, 
#     breaks = seq(0,1,25)) +
#   scale_size_continuous(
#     range = c(0.5, 3.5), 
#     breaks = seq(0,1,25)) +
#   scale_alpha_continuous(
#     range = c(0.5,.8) , 
#     breaks = seq(0,1,25)) +
#   #scale_shape(labels = c("Perennial", "Non-perennial")) +
#   guides(
#     color = guide_colorbar(
#       title ="WWTP Density", 
#       order = 1),
#     size="none", 
#     alpha = "none"
#   ) +
#   theme_bw() +
#   theme(
#     legend.position = 'right',
#     legend.justification = "center") +
#   xlab(NULL) +
#   ylab(NULL) 
# 
# density_map
# 
# # 3.2 Histogram ----------------------------------------------------------------
# pwi_point$d_wwpts_cut <- cut(pwi_point$d_wwtps_100, c(-1, .001, 0.1, 0.25, 0.5, 1)) #create flow categorices
# 
# pwi_density <- pwi_point %>% 
#   ggplot(aes(x=d_wwpts_cut)) +
#   geom_bar() +
#   scale_x_discrete(
#     name = "Upstream WWTP Density [WWTP/km^2]", 
#     labels = c("0", "0-0.1", "0.1-1.25", "0.25-0.5",">0.5")) +
#   scale_y_continuous(name = "Number of Drinking\nWater Intakes") +
#   theme_classic() +
#   theme(
#     axis.title = element_text(size = 14), 
#     axis.text  = element_text(size = 10)
#   )
# 
# # 3.2 Bama ---------------------------------------------------------------------
# #Create bama shape
# bama_shp <- states() %>% filter(STUSPS == "AL") %>% st_transform(., 5070)
# 
# #Subset shapes to bama
# wwtp_bama <- wwtp[bama_shp,]
# pwi_bama <- pwi_point[bama_shp,]
# 
# #PWI Sheds
# pwi_sheds_bama <- pwi_shed %>% filter(DVSN_ID %in% pwi_bama$DVSN_ID)
# pwi_sheds_bama <- st_crop(pwi_sheds_bama, bama_shp)
# 
# #Add coords to wwtp_bama
# wwtp_bama <- wwtp_bama %>% 
#   mutate(
#     lat = st_coordinates(wwtp_bama)[,1], 
#     lon = st_coordinates(wwtp_bama)[,2])
# 
# 
# #Create bama map
# #Create density map
# bama_map<-bama_shp %>%
#   ggplot()+
#   geom_sf(data = bama_shp, lwd=0.5) + 
#   geom_sf(data = pwi_sheds_bama, lwd=0.5, bg="light blue")+
#   geom_point(
#     data= wwtp_bama, 
#     aes(x = lat, y = lon), 
#     pch = 19, 
#     cex = 1, 
#     col = "brown", 
#     alpha = 0.6) + 
#   geom_point(
#     data = pwi_bama,
#     aes(x=lat,y=lon),
#     pch = 19,
#     cex = 4, 
#     color = "dark blue") +
#   theme_bw() +
#   theme(
#     legend.position = 'none',
#     legend.justification = "center") +
#   xlab(NULL) +
#   ylab(NULL) 
# 
# 
# # 3.3 Print! -------------------------------------------------------------------
# 
# #Write shape
# st_write(bama_shp, "C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\surface_water_intakes\\bama.shp")
# 
# #Plot
# ggsave(plot = pwi_density, file = 'docs/density.png', width = 4.25, height = 3, units = "in", dpi = 300)
# ggsave(plot = density_map, file = "docs/density_map.png", width = 6, height = 3.75, units="in", dpi = 300)
# ggsave(plot = bama_map, file = "docs/bama_map.png", width = 3, height = 4, units="in", dpi = 300)
