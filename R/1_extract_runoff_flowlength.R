#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Explore NHD VAA 
# Coders: Nate Jones (natejones@ua.edu)
# Date: 10/10/2024
# Purpose: Extract stream slope and stream density from VAA tables for headwaters 
#          and downstream areas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data source: https://www.usgs.gov/national-hydrography/access-national-hydrography-products
#VAA table definitions: https://www.usgs.gov/national-hydrography/value-added-attributes-vaas

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
remove(list=ls())

# library
library(sf)
library(ggplot2)
library(tidyverse)
library(patchwork)

# download data
df <- read_csv('output//vaa.csv', 
          col_names = c("XCoord","YCoord","OBJECTID","permanent_identifier","fdate",
          "resolution","gnis_id","gnis_name","lengthkm","reachcode","flowdir",
          "wbarea_permanent_identifier","ftype","fcode","mainpath","innetwork",
          "visibilityfilter","nhdplusid","vpuid","streamleve","streamorde",
          "streamcalc","fromnode","tonode","hydroseq","levelpathi","pathlength",
          "terminalpa","arbolatesu","divergence","startflag","terminalfl",
          "uplevelpat","uphydroseq","dnlevel","dnlevelpat","dnhydroseq","dnminorhyd",
          "dndraincou","frommeas","tomeas","rtndiv","thinner","vpuin","vpuout",
          "areasqkm","totdasqkm","divdasqkm","maxelevraw","minelevraw","maxelevsmo",
          "minelevsmo","slope","slopelenkm","elevfixed","hwtype","hwnodesqkm",
          "statusflag","qama","vama","qincrama","qbma","vbma","qincrbma","qcma",
          "vcma","qincrcma","qdma","vdma","qincrdma","qema","vema","qincrema","qfma",
          "qincrfma","arqnavma","petma","qlossma","qgadjma","qgnavma","gageadjma",
          "avgqadjma","gageidma","gageqma","Shape_Length"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: tidy data ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select variables of interest
df <- df %>% 
  dplyr::select(
    XCoord,
    YCoord,
    nhdplusid, 
    reachcode,
    streamorde, 
    lengthkm, 
    qincrama
  )

#Define headwaters
df <- df %>% 
  mutate(SO12 = if_else(streamorde <=2, 1, 0)) %>% 
  drop_na(SO12)

#Remove -9999
df <- df %>% 
  mutate(
    qincrama   = if_else(qincrama   >0,  qincrama,   NA), 
    lengthkm   = if_else(lengthkm   >0,  lengthkm,   NA)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Export csv -----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(df, 'output//vaa_flow_length.csv')
