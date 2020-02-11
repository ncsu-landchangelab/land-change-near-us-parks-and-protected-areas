#---------------------------------------------------------------------------------
# Summaries of housing density and impervious surface at ecoregion and state scale
#---------------------------------------------------------------------------------

# Used packages
library(sf)
library(psych)

# Data directory
setwd("Drive:/directory/path")

# Summaries of housing density and impervious surface at ecoregion level-1
hd_imp_sum_ecoregion <- st_read("Drive:/directory/path/name.shp")                                       # Shapefile with housing density and impervious surface information
    
hd_imp_sum_ecoregion <- hd_imp_sum_ecoregion[which(hd_imp_sum_ecoregion$HD2Y90 < 600),]                 # Remove outliers
hd_imp_sum_ecoregion <- hd_imp_sum_ecoregion[which(hd_imp_sum_ecoregion$HD2Y00 < 600),]
hd_imp_sum_ecoregion <- hd_imp_sum_ecoregion[which(hd_imp_sum_ecoregion$HD2Y10 < 600),]
 
keyVars_hd_imp_sum_ecoregion <- hd_imp_sum_ecoregion %>% st_set_geometry(NULL)                          # Drop geometry for summing each column for total values
    
# Summarize data by ecoregion
HD2Y90summary <-describeBy(keyVars_hd_imp_sum_ecoregion$HD2Y90, keyVars_hd_imp_sum_ecoregion$NA_L1NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y90summary) <- c("name", "HD2Y90m", "HD2Y90sd", "HD2Y90median", "HD2Y90se")
HD2Y00summary <-describeBy(keyVars_hd_imp_sum_ecoregion$HD2Y00, keyVars_hd_imp_sum_ecoregion$NA_L1NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y00summary) <- c("name", "HD2Y00m", "HD2Y00sd", "HD2Y00median", "HD2Y00se")
HD2Y10summary <-describeBy(keyVars_hd_imp_sum_ecoregion$HD2Y10, keyVars_hd_imp_sum_ecoregion$NA_L1NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y10summary) <- c("name", "HD2Y10m", "HD2Y10sd", "HD2Y10median", "HD2Y10se")
PCTIMPsummary <-describeBy(keyVars_hd_imp_sum_ecoregion$PCTIMP, keyVars_hd_imp_sum_ecoregion$NA_L1NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(PCTIMPsummary) <- c("name", "PCTIMPm", "PCTIMPsd", "PCTIMPmedian", "PCTIMPse")
PCTIMP01summary <-describeBy(keyVars_hd_imp_sum_ecoregion$PCTIMP01, keyVars_hd_imp_sum_ecoregion$NA_L1NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(PCTIMP01summary) <- c("name", "PCTIMP01m", "PCTIMP01sd", "PCTIMP01median", "PCTIMP01se")

hd_imp_ecoregion<- Reduce(function(x, y) merge(x, y, all=TRUE), list(HD2Y90summary, HD2Y00summary, HD2Y10summary, PCTIMPsummary, PCTIMP01summary)) 

# Summaries of housing density and impervious surface for each state
hd_imp_sum_state <- st_read("Drive:/directory/path/name.shp")  

hd_imp_sum_state <- hd_imp_sum_state[which(hd_imp_sum_state$HD2Y90 < 600),]
hd_imp_sum_state <- hd_imp_sum_state[which(hd_imp_sum_state$HD2Y00 < 600),]
hd_imp_sum_state <- hd_imp_sum_state[which(hd_imp_sum_state$HD2Y10 < 600),]
    
keyVars_hd_imp_sum_state <- hd_imp_sum_state %>% st_set_geometry(NULL)                                  # Drop geometry for summing each column for total values
    
# Summarize data (mean) by states
HD2Y90STATEsummary <-describeBy(keyVars_hd_imp_sum_state$HD2Y90, keyVars_hd_imp_sum_state$STATE_NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y90STATEsummary) <- c("StateName", "HD2Y90m", "HD2Y90sd", "HD2Y90median", "HD2Y90se")
HD2Y00STATEsummary <-describeBy(keyVars_hd_imp_sum_state$HD2Y00, keyVars_hd_imp_sum_state$STATE_NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y00STATEsummary) <- c("StateName", "HD2Y00m", "HD2Y00sd", "HD2Y00median", "HD2Y00se")
HD2Y10STATEsummary <-describeBy(keyVars_hd_imp_sum_state$HD2Y10, keyVars_hd_imp_sum_state$STATE_NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(HD2Y10STATEsummary) <- c("StateName", "HD2Y10m", "HD2Y10sd", "HD2Y10median", "HD2Y10se")
PCTIMPSTATEsummary <-describeBy(keyVars_hd_imp_sum_state$PCTIMP, keyVars_hd_imp_sum_state$STATE_NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(PCTIMPSTATEsummary) <- c("StateName", "PCTIMPm", "PCTIMPsd", "PCTIMPmedian", "PCTIMPse")
PCTIMP01STATEsummary <-describeBy(keyVars_hd_imp_sum_state$PCTIMP01, keyVars_hd_imp_sum_state$STATE_NAME, mat = TRUE, digits=2) [c(-1, -(3:4), -(8:14))]
colnames(PCTIMP01STATEsummary) <- c("StateName", "PCTIMP01m", "PCTIMP01sd", "PCTIMP01median", "PCTIMP01se")
    
hd_imp_state <- Reduce(function(x, y) merge(x, y, all=TRUE), list(HD2Y90STATEsummary, HD2Y00STATEsummary, HD2Y10STATEsummary, PCTIMPSTATEsummary, PCTIMP01STATEsummary))

write.xlsx(hd_imp_ecoregion, file = "hd_imp_ecoregion.xlsx", sheetName = "hd_imp_ecoregion", append = TRUE)
write.xlsx(hd_imp_state, file = "hd_imp_state.xlsx", sheetName = "hd_imp_state", append = TRUE)

#---