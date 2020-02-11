#------------------------------------------------------------------------
# Analysis of protected areas for housing density and impervious surfaces
#------------------------------------------------------------------------


# Spatial analysis packages
library(sf)
library(sp)

# Data cleaning and processing packages
library(dplyr)
library(gridExtra)
library(readxl)
library(reshape2)

# Graphic packages
library(ggplot2)
library(ggthemes)


# Data directory
setwd("Drive:/directory/path")


# Read data
fed_prot_areas_atts <- st_read("path/name.shp")                                                                # Federally protected areas at GAP levels or for the Contiguous United States
fed_prot_areas_CONUS_bound_prj_data <- st_read("path/name.shp")                                                # Federally protected areas for the Contiguous United States
acrImpSur <- st_read("path/name.shp")                                                                          # Acre impervious surface data for the Contiguous United States
CONUS_bound_prj_bound <- st_read("path/name.shp")                                                              # Boundary of the Contiguous United States
CONUS_ecoRegions <- st_read("path/name.shp")                                                                   # UAS ecoregion shapefile

# Federally protected areas data attributes
fed_prot_areas <- fed_prot_areas_atts[,c("Id")]
selected_fed_prot_areas <- fed_prot_areas[sample(2409, 2409*1),]                                                # Select a certain percent of random polygons (Example, total polygons (x) multiplied by percent selected)
selected_fed_prot_areas_buff <- st_buffer(selected_fed_prot_areas, dist = 50000)                                # Create buffer (km). Unit is meter therefore use 1000 for 1-km. We used 1000, 10,000, 25,000, and 50,000 m in the study
st_agr(selected_fed_prot_areas_buff) <- "constant"                                                              # Make the assumption (that the attribute is constant throughout the geometry)

# Estimate difference between buffer and protected areas
fed_prot_areas_CONUS_bound_prj_data_slc_atts <- fed_prot_areas_CONUS_bound_prj_data[,c("Id")]
area_difference <- st_difference(selected_fed_prot_areas_buff, st_union(st_combine(st_buffer(fed_prot_areas_CONUS_bound_prj_data_slc_atts,0))))
st_agr(area_difference) <- "constant"

# Extract information from acre impervious surface using buffered areas
acrImpSur_buff_difference <- st_intersection(acrImpSur, st_buffer(area_difference,0))
st_agr(acrImpSur_buff_difference)<- 'constant'

CONUS_bound_prj <- st_transform(CONUS_bound_prj_bound, st_crs(fed_prot_areas_atts))                              # Project CONUS boundary to Federally protected areas shapefule
st_agr(CONUS_bound_prj) <- "constatnt"
cln_acrImpSur_buff_difference <- st_intersection(acrImpSur_buff_difference, CONUS_bound_prj)                     # Clip polygons falls outside CONUS_bound_prj area
st_write(cln_acrImpSur_buff_difference, "path/name.shp", delete_layer = TRUE)                                    # Write a shapefile for backup
  

imp_sur_and_hous_den <- cln_acrImpSur_buff_difference[,c("HD2Y90", "HD2Y00", "HD2Y10", "PCTIMP01", "PCTIMP")]    # Studied attributes (e.g., 1990, 2000, and 2010 housing density ("HD2Y90", "HD2Y00", "HD2Y10") and 2000 and 2010 impervious surface ("PCTIMP01", "PCTIMP"))
  
# Retrieve results as a text
sink("path/name.txt")
cat ("Summaries of studied parameters")
mean(imp_sur_and_hous_den$PCTIMP01)
mean(imp_sur_and_hous_den$PCTIMP)
mean(imp_sur_and_hous_den$HD2Y90)
mean(imp_sur_and_hous_den$HD2Y00)
mean(imp_sur_and_hous_den$HD2Y10)
cat("\n")

# Summarise impervious surface and housing density based on UAS ecoregions
st_agr(CONUS_ecoRegions) <- "constant"
att_UAS_ecoregion <- st_intersection(cln_acrImpSur_buff_difference, CONUS_ecoRegions)

keyVars <- att_UAS_ecoregion[,c("HD2Y90", "HD2Y00", "HD2Y10", "NA_L2NAME", "PCTIMP", "PCTIMP01")]                # Select required variables
keyVars_df <- keyVars %>% st_set_geometry(NULL)                                                                  # Drop geometry for summing each column for total values

HD2Y90<-keyVars_df %>% group_by(NA_L2NAME) %>% summarize(mean=mean(HD2Y90))
colnames( HD2Y90) <- c("name", " HD2Y90")
HD2Y00<-keyVars_df %>% group_by(NA_L2NAME) %>% summarize(mean=mean(HD2Y00))
colnames(HD2Y00) <- c("name", "HD2Y00")
HD2Y10<-keyVars_df %>% group_by(NA_L2NAME) %>% summarize(mean=mean(HD2Y10))
colnames(HD2Y10) <- c("name", "HD2Y10")
PCTIMP<-keyVars_df %>% group_by(NA_L2NAME) %>% summarize(mean=mean(PCTIMP))
colnames(PCTIMP) <- c("name", "PCTIMP")
PCTIMP01<-keyVars_df %>% group_by(NA_L2NAME) %>% summarize(mean=mean(PCTIMP01))
colnames(PCTIMP01) <- c("name", "PCTIMP01")
  
# Merge multiple data frames into one
att_summaries_ecoRegion <- Reduce(function(x, y) merge(x, y, all=TRUE), list(HD2Y90, HD2Y00, HD2Y10, PCTIMP, PCTIMP01))    
att_summaries_ecoRegion[-1] <- lapply(att_summaries_ecoRegion[-1], function(x) as.numeric(sub("\\s+\\D+$", "", x)))# Remove units and convert character to numeric
is.num <- sapply(att_summaries_ecoRegion, is.numeric)                                                               
att_summaries_ecoRegion[is.num] <- lapply(att_summaries_ecoRegion[is.num], round, 2)                               # Rounding to 2 Decimal Places
  
cat("Summaries of outcome by ecorgion names")
att_summaries_ecoRegion
cat("\n")
  
#---  