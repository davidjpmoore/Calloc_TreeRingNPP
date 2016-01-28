########################################################################################
# Running Correlations between CRU climate data and various chronologies
########################################################################################
# Load in timeseries/chronologies
load("processed_data/valles_combined_climate_chronologies.rdata")

summary(valles.climate.cr)

# Taking just the residual chronologies
valles.res <- valles.climate.cr[,substr(names(valles.climate.cr),5,7)=="res"]
summary(valles.res)
valles.res$cm.res <- valles.climate.cr$cm.n

summary(valles.res)

# Load in climate data

t.mean <- read.csv("climate_data/VC_CRU_meanT.csv", header=T)
t.min <- read.csv("climate_data/VC_CRU_Tmin.csv", header=T)
t.max <- read.csv("climate_data/VC_CRU_Tmax.csv", header=T)
precip <- read.csv("climate_data/VC_CRU_precip.csv", header=T)
pdsi <- read.csv("climate_data/VC_PDSI.csv", header=T)

t.mean <- t.mean[t.mean$year >= 1980 & t.mean$year <=2007,]
summary(t.mean)

t.min <- t.min[t.min$year >= 1980 & t.min$year <=2007,]
summary(t.min)

t.max <- t.max[t.max$year >= 1980 & t.max$year <=2007,]
summary(t.max)

precip <- precip[precip$year >= 1980 & precip$year <=2007,]
summary(precip)

pdsi <- pdsi[pdsi$year >= 1980 & pdsi$year <=2007,]
summary(pdsi)

dim(valles.climate.cr)
dim(t.mean)
corr.tmean <- cor(valles.climate.cr$vuf.res, t.mean, method="pearson")
summary(corr.tmean)
########################################################################################
# Setting up for-loop to loop through the correlations of the different chronologies with different climate variables
##################################################################################################

#################################
# Mean Monthly Temperature
#################################
corr.tmean <- as.data.frame(matrix(NA,nrow=ncol(valles.res), ncol=ncol(t.mean)-1))
dim(corr.tmean)

names(corr.tmean)<- names(t.mean[,2:ncol(t.mean)]) 
row.names(corr.tmean) <- names(valles.res)
summary(corr.tmean) 


for(i in 1:ncol(valles.res)){
	corr.tmean[i,] <- cor(valles.res[,i], t.mean[,2:ncol(t.mean)], method="pearson")
}
head(corr.tmean)


#################################
# Maximum Monthly Temperature
#################################
corr.tmax <- corr.tmean
corr.tmax[,] <-NA

for(i in 1:ncol(valles.res)){
	corr.tmax[i,] <- cor(valles.res[,i], t.max[,2:ncol(t.max)], method="pearson")
}
head(corr.tmax)

#################################
# Minimum Monthly Temperature
#################################
corr.tmin <- corr.tmean
corr.tmin[,] <-NA

for(i in 1:ncol(valles.res)){
	corr.tmin[i,] <- cor(valles.res[,i], t.min[,2:ncol(t.min)], method="pearson")
}
head(corr.tmin)

#################################
# Precipitation
#################################
corr.precip <- corr.tmean
corr.precip[,] <-NA

for(i in 1:ncol(valles.res)){
	corr.precip[i,] <- cor(valles.res[,i], precip[,2:ncol(precip)], method="pearson")
}
head(corr.precip)

#################################
# PDSI
#################################
corr.pdsi <- corr.tmean
corr.pdsi[,] <-NA

for(i in 1:ncol(valles.res)){
	corr.pdsi[i,] <- cor(valles.res[,i], pdsi[,2:ncol(pdsi)], method="pearson")
}
head(corr.pdsi)




###################################################################################################
# Plotting the correlations as bar charts
###################################################################################################

# Stacking things to get them into ggplot format
tmean.stack <- stack(corr.tmean)
summary(tmean.stack)
names(tmean.stack) <- c("corr", "month")
tmean.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
tmean.stack$type <- as.factor("tmean" )

tmin.stack <- stack(corr.tmin)
summary(tmin.stack)
names(tmin.stack) <- c("corr", "month")
tmin.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
tmin.stack$type <- as.factor("tmin" )

tmax.stack <- stack(corr.tmax)
summary(tmax.stack)
names(tmax.stack) <- c("corr", "month")
tmax.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
tmax.stack$type <- as.factor("tmax" )

precip.stack <- stack(corr.precip)
summary(precip.stack)
names(precip.stack) <- c("corr", "month")
precip.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
precip.stack$type <- as.factor("precip" )

pdsi.stack <- stack(corr.pdsi)
summary(pdsi.stack)
names(pdsi.stack) <- c("corr", "month")
pdsi.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
pdsi.stack$type <- as.factor("pdsi" )

all.valles.climate.stack <- rbind(tmean.stack, tmin.stack, tmax.stack, precip.stack, pdsi.stack)
summary(all.valles.climate.stack)


# Setting the factoring so that it will display the  months correctly when we Facet
all.valles.climate.stack$month <- factor(all.valles.climate.stack$month, levels = names(corr.tmean))

all.valles.climate.stack$chron <- factor(all.valles.climate.stack$chron, levels = c("vuf.res", "bcw.res", "vlf.res", "cm.res"))

# Adding a significance column so that sig. corrs. will pop
all.valles.climate.stack$sig <- ifelse(all.valles.climate.stack$corr >=0.330 | all.valles.climate.stack$corr<= -0.330, "Y", "N")

all.valles.climate.stack$sig <- factor(all.valles.climate.stack$sig, levels = c("Y", "N"))
levels(all.valles.climate.stack$sig)

summary(all.valles.climate.stack)


save(all.valles.climate.stack, file="processed_data/valles_climate_plus_chron_stack.rdata")

#######################################################
# Plotting Things
#######################################################
library(ggplot2)
summary(all.valles.climate.stack)

# Critical Value for 28 years (n-2 = 26) 0.330


ggplot(data=all.valles.climate.stack) + facet_grid(chron ~ type , scales="fixed")+
	geom_bar(aes(x=month, y=corr, fill=sig), stat="identity", position="dodge") +
	scale_fill_manual(values=c("blue", "gray50"))
	







