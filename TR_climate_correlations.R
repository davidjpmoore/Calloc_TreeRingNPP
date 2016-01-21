########################################################################################
# Running Correlations between CRU climate data and various chronologies
########################################################################################
# Load in timeseries/chronologies
load("processed_data/valles_combined_climate_chronologies.rdata")

summary(valles.climate.cr)

# Load in climate data

t.mean <- read.csv("climate_data/VC_CRU_meanT.csv", header=T)
t.min <- read.csv("climate_data/VC_CRU_Tmin.csv", header=T)
t.max <- read.csv("climate_data/VC_CRU_Tmax.csv", header=T)
precip <- read.csv("climate_data/VC_CRU_precip.csv", header=T)
pdsi <- read.csv("climate_data/VC_PDSI.csv", header=T)

summary(t.mean)
summary(t.min)
summary(t.max)
summary(precip)
summary(pdsi)