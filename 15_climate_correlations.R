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

t.mean <- read.csv("climate_data/VC_CRU_meanT2.csv", header=T)
t.min <- read.csv("climate_data/VC_CRU_Tmin2.csv", header=T)
t.max <- read.csv("climate_data/VC_CRU_Tmax2.csv", header=T)
precip <- read.csv("climate_data/VC_CRU_precip2.csv", header=T)
pdsi <- read.csv("climate_data/VC_PDSI2.csv", header=T)

t.mean <- t.mean[t.mean$year >= 1980 & t.mean$year <=2007,]
summary(t.mean)

t.mean$pDJF <- rowMeans(t.mean[,c("pdec", "jan", "feb")], na.rm=T)
t.mean$MAM <- rowMeans(t.mean[,c("mar", "apr", "may")], na.rm=T)
t.mean$JJA <- rowMeans(t.mean[,c("jun", "jul", "aug")], na.rm=T)
t.mean$SON <- rowMeans(t.mean[,c("sep", "oct", "nov")], na.rm=T)
summary(t.mean)

t.min <- t.min[t.min$year >= 1980 & t.min$year <=2007,]
summary(t.min)

t.min$pDJF <- rowMeans(t.min[,c("pdec", "jan", "feb")], na.rm=T)
t.min$MAM <- rowMeans(t.min[,c("mar", "apr", "may")], na.rm=T)
t.min$JJA <- rowMeans(t.min[,c("jun", "jul", "aug")], na.rm=T)
t.min$SON <- rowMeans(t.min[,c("sep", "oct", "nov")], na.rm=T)
summary(t.min)

t.max <- t.max[t.max$year >= 1980 & t.max$year <=2007,]
summary(t.max)

t.max$pDJF <- rowMeans(t.max[,c("pdec", "jan", "feb")], na.rm=T)
t.max$MAM <- rowMeans(t.max[,c("mar", "apr", "may")], na.rm=T)
t.max$JJA <- rowMeans(t.max[,c("jun", "jul", "aug")], na.rm=T)
t.max$SON <- rowMeans(t.max[,c("sep", "oct", "nov")], na.rm=T)
summary(t.max)

precip <- precip[precip$year >= 1980 & precip$year <=2007,]
summary(precip)
precip$pDJF <- rowSums(precip[,c("pdec", "jan", "feb")], na.rm=T)
precip$MAM <- rowSums(precip[,c("mar", "apr", "may")], na.rm=T)
precip$JJA <- rowSums(precip[,c("jun", "jul", "aug")], na.rm=T)
precip$SON <- rowSums(precip[,c("sep", "oct", "nov")], na.rm=T)
summary(precip)

pdsi <- pdsi[pdsi$year >= 1980 & pdsi$year <=2007,]
summary(pdsi)
dim(pdsi)

pdsi$pDJF <- rowMeans(pdsi[,c("pdec", "jan", "feb")], na.rm=T)
pdsi$MAM <- rowMeans(pdsi[,c("mar", "apr", "may")], na.rm=T)
pdsi$JJA <- rowMeans(pdsi[,c("jun", "jul", "aug")], na.rm=T)
pdsi$SON <- rowMeans(pdsi[,c("sep", "oct", "nov")], na.rm=T)
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
corr.precip <- as.data.frame(matrix(NA,nrow=ncol(valles.res), ncol=ncol(precip)-1))
names(corr.precip)<- names(precip[,2:ncol(precip)]) 
row.names(corr.precip) <- names(valles.res)

for(i in 1:ncol(valles.res)){
	corr.precip[i,] <- cor(valles.res[,i], precip[,2:ncol(precip)], method="pearson")
}
head(corr.precip)

#################################
# PDSI
#################################
corr.pdsi <- as.data.frame(matrix(NA,nrow=ncol(valles.res), ncol=ncol(pdsi)-1))
names(corr.pdsi)<- names(pdsi[,2:ncol(t.mean)]) 
row.names(corr.pdsi) <- names(valles.res)
summary(corr.pdsi) 

for(i in 1:ncol(valles.res)){
	corr.pdsi[i,] <- cor(valles.res[,i], pdsi[,2:ncol(pdsi)], method="pearson")
}
head(corr.pdsi)
summary(corr.pdsi)



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

tmax.stack <- stack(corr.precip)
summary(tmax.stack)
names(tmax.stack) <- c("corr", "month")
tmax.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
tmax.stack$type <- as.factor("tmax" )

precip.stack <- stack(corr.precip)
summary(precip.stack)
names(precip.stack) <- c("corr", "month")
precip.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
precip.stack$type <- as.factor("precip" )
dim(precip.stack)
summary(precip.stack)

pdsi.stack <- stack(corr.pdsi)
summary(pdsi.stack)
names(pdsi.stack) <- c("corr", "month")
pdsi.stack$chron <- as.factor(c("vuf.res", "vlf.res", "bcw.res", "cm.res"))
pdsi.stack$type <- as.factor("pdsi")
summary(pdsi.stack)
dim(pdsi.stack)


all.valles.climate.stack <- rbind(tmean.stack, tmin.stack, tmax.stack, precip.stack, pdsi.stack)
summary(all.valles.climate.stack)


# Setting the factoring so that it will display the  months correctly when we Facet
all.valles.climate.stack$month <- factor(all.valles.climate.stack$month, levels = names(corr.tmean))
summary(all.valles.climate.stack$month)

all.valles.climate.stack$chron <- factor(all.valles.climate.stack$chron, levels = c("vuf.res", "bcw.res", "vlf.res", "cm.res"))

# Adding a significance column so that sig. corrs. will pop
all.valles.climate.stack$sig <- ifelse(all.valles.climate.stack$corr >=0.330 | all.valles.climate.stack$corr<= -0.330, "Y", "N")

all.valles.climate.stack$sig <- factor(all.valles.climate.stack$sig, levels = c("Y", "N"))
levels(all.valles.climate.stack$sig)

summary(all.valles.climate.stack)


save(all.valles.climate.stack, file="processed_data/valles_climate_plus_chron_stack.rdata")

#######################################################
# Plotting Correlations with climate chronologies
#######################################################
library(ggplot2)
summary(all.valles.climate.stack)

test <- all.valles.climate.stack[all.valles.climate.stack$month %in% c("pDJF", "MAM", "JJA", "SON"),]
summary(test)

levels(all.valles.climate.stack)

# Critical Value for 28 years (n-2 = 26) 0.330

pdf("figures/climate_chron_all_months.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack[all.valles.climate.stack$month %in% c("pDJF", "MAM", "JJA", "SON"),]) + facet_grid(chron ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=sig), stat="identity", position="dodge") +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()

pdf("figures/iclimate_chron_seasons.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack) + facet_grid(chron ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=sig), stat="identity", position="dodge") +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()

#######################################################
# loading in BM bootstraps for correlation runs
#######################################################
load("processed_data/vuf_bm_boot_tot_inc.Rdata")
load("processed_data/vlf_bm_boot_tot_inc.Rdata")

summary(vuf.inc.tot)
summary(vlf.inc.tot)


# subsetting to the extent of the previous timeseries
vuf.bm <- vuf.inc.tot[row.names(vuf.inc.tot)>=1980 & row.names(vuf.inc.tot)<=2007,]
vlf.bm <- vlf.inc.tot[row.names(vlf.inc.tot)>=1980 & row.names(vlf.inc.tot)<=2007,]


# ---------------------------------------
# Climate Correlations with BM arrays
# ---------------------------------------

#------------------------------
# Tmean
#------------------------------

## VUF

vuf.corr.tmean.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(t.mean)-1))
dim(vuf.corr.tmean.bm)

names(vuf.corr.tmean.bm)<- names(t.mean[,2:ncol(t.mean)]) 
row.names(vuf.corr.tmean.bm) <- names(vuf.bm)
summary(vuf.corr.tmean.bm) 

dim(t.mean)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmean.bm[i,] <- cor(vuf.bm[,i], t.mean[,2:ncol(t.mean)], method="pearson")
}
head(vuf.corr.tmean.bm)

vuf.tmean.mean.corr <- vuf.corr.tmean.bm
for(j in 1:ncol(vuf.tmean.mean.corr)){
	vuf.tmean.mean.corr[,j] <- mean(vuf.corr.tmean.bm[,j], na.rm=T)
}
head(vuf.tmean.mean.corr)

## VLF

vlf.corr.tmean.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(t.mean)-1))
dim(vlf.corr.tmean.bm)

names(vlf.corr.tmean.bm)<- names(t.mean[,2:ncol(t.mean)]) 
row.names(vlf.corr.tmean.bm) <- names(vlf.bm)
summary(vlf.corr.tmean.bm) 

dim(t.mean)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmean.bm[i,] <- cor(vlf.bm[,i], t.mean[,2:ncol(t.mean)], method="pearson")
}
head(vlf.corr.tmean.bm)

vlf.tmean.mean.corr <- vlf.corr.tmean.bm
for(j in 1:ncol(vlf.tmean.mean.corr)){
	vlf.tmean.mean.corr[,j] <- mean(vlf.corr.tmean.bm[,j], na.rm=T)
}
head(vlf.tmean.mean.corr)


#------------------------------
# Precipitation
#------------------------------

vuf.corr.precip.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(precip)-1))
dim(vuf.corr.precip.bm)

names(vuf.corr.precip.bm)<- names(precip[,2:ncol(precip)]) 
row.names(vuf.corr.precip.bm) <- names(vuf.bm)
summary(vuf.corr.precip.bm) 

dim(t.max)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.precip.bm[i,] <- cor(vuf.bm[,i], precip[,2:ncol(precip)], method="pearson")
}
head(vuf.corr.precip.bm)

vuf.precip.mean.corr <- vuf.corr.precip.bm
for(j in 1:ncol(vuf.precip.mean.corr)){
	vuf.precip.mean.corr[,j] <- mean(vuf.corr.precip.bm[,j], na.rm=T)
}
head(vuf.precip.mean.corr)

## VLF

vlf.corr.precip.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(precip)-1))
dim(vlf.corr.precip.bm)

names(vlf.corr.precip.bm)<- names(precip[,2:ncol(precip)]) 
row.names(vlf.corr.precip.bm) <- names(vlf.bm)
summary(vlf.corr.precip.bm) 

dim(t.max)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.precip.bm[i,] <- cor(vlf.bm[,i], precip[,2:ncol(precip)], method="pearson")
}
head(vlf.corr.precip.bm)

vlf.precip.mean.corr <- vlf.corr.precip.bm
for(j in 1:ncol(vlf.precip.mean.corr)){
	vlf.precip.mean.corr[,j] <- mean(vlf.corr.precip.bm[,j], na.rm=T)
}
head(vlf.precip.mean.corr)


#------------------------------
# T min
#------------------------------

## VUF
vuf.corr.tmin.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(t.min)-1))
dim(vuf.corr.tmin.bm)

names(vuf.corr.tmin.bm)<- names(t.min[,2:ncol(t.min)]) 
row.names(vuf.corr.tmin.bm) <- names(vuf.bm)
summary(vuf.corr.tmin.bm) 

dim(t.min)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmin.bm[i,] <- cor(vuf.bm[,i], t.min[,2:ncol(t.min)], method="pearson")
}
head(vuf.corr.tmin.bm)

vuf.tmin.mean.corr <- vuf.corr.tmin.bm
for(j in 1:ncol(vuf.tmin.mean.corr)){
	vuf.tmin.mean.corr[,j] <- mean(vuf.corr.tmin.bm[,j], na.rm=T)
}
head(vuf.tmin.mean.corr)

## VLF

vlf.corr.tmin.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(t.min)-1))
dim(vlf.corr.tmin.bm)

names(vlf.corr.tmin.bm)<- names(t.min[,2:ncol(t.min)]) 
row.names(vlf.corr.tmin.bm) <- names(vlf.bm)
summary(vlf.corr.tmin.bm) 

dim(t.min)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmin.bm[i,] <- cor(vlf.bm[,i], t.min[,2:ncol(t.min)], method="pearson")
}
head(vlf.corr.tmin.bm)

vlf.tmin.mean.corr <- vlf.corr.tmin.bm
for(j in 1:ncol(vlf.tmin.mean.corr)){
	vlf.tmin.mean.corr[,j] <- mean(vlf.corr.tmin.bm[,j], na.rm=T)
}
head(vlf.tmin.mean.corr)


#------------------------------
# Tmax
#------------------------------

## VUF

vuf.corr.tmax.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(t.max)-1))
dim(vuf.corr.tmax.bm)

names(vuf.corr.tmax.bm)<- names(t.max[,2:ncol(t.max)]) 
row.names(vuf.corr.tmax.bm) <- names(vuf.bm)
summary(vuf.corr.tmax.bm) 

dim(t.max)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmax.bm[i,] <- cor(vuf.bm[,i], t.max[,2:ncol(t.max)], method="pearson")
}
head(vuf.corr.tmax.bm)

vuf.tmax.mean.corr <- vuf.corr.tmax.bm
for(j in 1:ncol(vuf.tmax.mean.corr)){
	vuf.tmax.mean.corr[,j] <- mean(vuf.corr.tmax.bm[,j], na.rm=T)
}
head(vuf.tmax.mean.corr)


## VLF

vlf.corr.tmax.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(t.max)-1))
dim(vlf.corr.tmax.bm)

names(vlf.corr.tmax.bm)<- names(t.max[,2:ncol(t.max)]) 
row.names(vlf.corr.tmax.bm) <- names(vlf.bm)
summary(vlf.corr.tmax.bm) 

dim(t.max)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmax.bm[i,] <- cor(vlf.bm[,i], t.max[,2:ncol(t.max)], method="pearson")
}
head(vlf.corr.tmax.bm)

vlf.tmax.mean.corr <- vlf.corr.tmin.bm
for(j in 1:ncol(vlf.tmax.mean.corr)){
	vlf.tmax.mean.corr[,j] <- mean(vlf.corr.tmin.bm[,j], na.rm=T)
}
head(vlf.tmax.mean.corr)

#------------------------------
# PDSI
#------------------------------

## VUF
vuf.corr.pdsi.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(pdsi)-1))
dim(vuf.corr.pdsi.bm)

names(vuf.corr.pdsi.bm)<- names(pdsi[,2:ncol(pdsi)]) 
row.names(vuf.corr.pdsi.bm) <- names(vuf.bm)
summary(vuf.corr.pdsi.bm) 

dim(pdsi)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.pdsi.bm[i,] <- cor(vuf.bm[,i], pdsi[,2:ncol(pdsi)], method="pearson")
}
head(vuf.corr.pdsi.bm)

vuf.pdsi.mean.corr <- vuf.corr.pdsi.bm
for(j in 1:ncol(vuf.pdsi.mean.corr)){
	vuf.pdsi.mean.corr[,j] <- mean(vuf.corr.pdsi.bm[,j], na.rm=T)
}
head(vuf.pdsi.mean.corr)

## VLF

vlf.corr.pdsi.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(pdsi)-1))
dim(vlf.corr.pdsi.bm)

names(vlf.corr.pdsi.bm)<- names(pdsi[,2:ncol(pdsi)]) 
row.names(vlf.corr.pdsi.bm) <- names(vlf.bm)
summary(vlf.corr.pdsi.bm) 

dim(pdsi)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.pdsi.bm[i,] <- cor(vlf.bm[,i], pdsi[,2:ncol(pdsi)], method="pearson")
}
head(vlf.corr.pdsi.bm)

vlf.pdsi.mean.corr <- vlf.corr.pdsi.bm
for(j in 1:ncol(vlf.pdsi.mean.corr)){
	vlf.pdsi.mean.corr[,j] <- mean(vlf.corr.pdsi.bm[,j], na.rm=T)
}
head(vlf.pdsi.mean.corr)

#######################################################
# Stackign the BM correlations
#######################################################
# Tmean
vuf.bm.tmean.stack <- stack(vuf.corr.tmean.bm)
summary(vuf.bm.tmean.stack)
names(vuf.bm.tmean.stack) <- c("corr", "month")
vuf.bm.tmean.stack$site <- as.factor("VUF")
vuf.bm.tmean.stack$type <- as.factor("tmean")
summary(vuf.bm.tmean.stack)

vuf.tmean.mean.corr.stack <- stack(vuf.tmean.mean.corr)
summary(vuf.tmean.mean.corr.stack)
names(vuf.tmean.mean.corr.stack) <- c("mean.corr", "month")
vuf.tmean.mean.corr.stack$site <- as.factor("VUF")
vuf.tmean.mean.corr.stack$type <- as.factor("tmean")
summary(vuf.tmean.mean.corr.stack)

vuf.bm.tmean.stack <- merge(vuf.bm.tmean.stack, vuf.tmean.mean.corr.stack, all.x=T, all.y=T)
summary(vuf.bm.tmean.stack)

vlf.bm.tmean.stack <- stack(vlf.corr.tmean.bm)
summary(vlf.bm.tmean.stack)
names(vlf.bm.tmean.stack) <- c("corr", "month")
vlf.bm.tmean.stack$site <- as.factor("VLF")
vlf.bm.tmean.stack$type <- as.factor("tmean")
summary(vlf.bm.tmean.stack)

vlf.tmean.mean.corr.stack <- stack(vlf.tmean.mean.corr)
summary(vlf.tmean.mean.corr.stack)
names(vlf.tmean.mean.corr.stack) <- c("mean.corr", "month")
vlf.tmean.mean.corr.stack$site <- as.factor("VLF")
vlf.tmean.mean.corr.stack$type <- as.factor("tmean")
summary(vlf.tmean.mean.corr.stack)

vlf.bm.tmean.stack <- merge(vlf.bm.tmean.stack, vlf.tmean.mean.corr.stack, all.x=T, all.y=T)
summary(vlf.bm.tmean.stack)


# Tmin
vuf.bm.tmin.stack <- stack(vuf.corr.tmin.bm)
summary(vuf.bm.tmin.stack)
names(vuf.bm.tmin.stack) <- c("corr", "month")
vuf.bm.tmin.stack$site <- as.factor("VUF")
vuf.bm.tmin.stack$type <- as.factor("tmin")
summary(vuf.bm.tmin.stack)

vuf.tmin.mean.corr.stack <- stack(vuf.tmin.mean.corr)
summary(vuf.tmin.mean.corr.stack)
names(vuf.tmin.mean.corr.stack) <- c("mean.corr", "month")
vuf.tmin.mean.corr.stack$site <- as.factor("VUF")
vuf.tmin.mean.corr.stack$type <- as.factor("tmin")
summary(vuf.tmin.mean.corr.stack)

vuf.bm.tmin.stack <- merge(vuf.bm.tmin.stack, vuf.tmin.mean.corr.stack, all.x=T, all.y=T)
summary(vuf.bm.tmin.stack)


vlf.bm.tmin.stack <- stack(vlf.corr.tmin.bm)
summary(vlf.bm.tmin.stack)
names(vlf.bm.tmin.stack) <- c("corr", "month")
vlf.bm.tmin.stack$site <- as.factor("VLF")
vlf.bm.tmin.stack$type <- as.factor("tmin")
summary(vlf.bm.tmin.stack)

vlf.tmin.mean.corr.stack <- stack(vlf.tmin.mean.corr)
summary(vlf.tmin.mean.corr.stack)
names(vlf.tmin.mean.corr.stack) <- c("mean.corr", "month")
vlf.tmin.mean.corr.stack$site <- as.factor("VLF")
vlf.tmin.mean.corr.stack$type <- as.factor("tmin")
summary(vlf.tmin.mean.corr.stack)

vlf.bm.tmin.stack <- merge(vlf.bm.tmin.stack, vlf.tmin.mean.corr.stack, all.x=T, all.y=T)
summary(vlf.bm.tmin.stack)


# Tmax
vuf.bm.tmax.stack <- stack(vuf.corr.tmax.bm)
summary(vuf.bm.tmax.stack)
names(vuf.bm.tmax.stack) <- c("corr", "month")
vuf.bm.tmax.stack$site <- as.factor("VUF")
vuf.bm.tmax.stack$type <- as.factor("tmax")
summary(vuf.bm.tmax.stack)

vuf.tmax.mean.corr.stack <- stack(vuf.tmax.mean.corr)
summary(vuf.tmax.mean.corr.stack)
names(vuf.tmax.mean.corr.stack) <- c("mean.corr", "month")
vuf.tmax.mean.corr.stack$site <- as.factor("VUF")
vuf.tmax.mean.corr.stack$type <- as.factor("tmax")
summary(vuf.tmax.mean.corr.stack)

vuf.bm.tmax.stack <- merge(vuf.bm.tmax.stack, vuf.tmax.mean.corr.stack, all.x=T, all.y=T)
summary(vuf.bm.tmax.stack)


vlf.bm.tmax.stack <- stack(vlf.corr.tmax.bm)
summary(vlf.bm.tmax.stack)
names(vlf.bm.tmax.stack) <- c("corr", "month")
vlf.bm.tmax.stack$site <- as.factor("VLF")
vlf.bm.tmax.stack$type <- as.factor("tmax")
summary(vlf.bm.tmax.stack)

vlf.tmax.mean.corr.stack <- stack(vlf.tmax.mean.corr)
summary(vlf.tmax.mean.corr.stack)
names(vlf.tmax.mean.corr.stack) <- c("mean.corr", "month")
vlf.tmax.mean.corr.stack$site <- as.factor("VLF")
vlf.tmax.mean.corr.stack$type <- as.factor("tmax")
summary(vlf.tmax.mean.corr.stack)

vlf.bm.tmax.stack <- merge(vlf.bm.tmax.stack, vlf.tmax.mean.corr.stack, all.x=T, all.y=T)
summary(vlf.bm.tmax.stack)

# Precip
vuf.bm.precip.stack <- stack(vuf.corr.precip.bm)
summary(vuf.bm.precip.stack)
names(vuf.bm.precip.stack) <- c("corr", "month")
vuf.bm.precip.stack$site <- as.factor("VUF")
vuf.bm.precip.stack$type <- as.factor("precip")
summary(vuf.bm.precip.stack)

vuf.precip.mean.corr.stack <- stack(vuf.precip.mean.corr)
summary(vuf.precip.mean.corr.stack)
names(vuf.precip.mean.corr.stack) <- c("mean.corr", "month")
vuf.precip.mean.corr.stack$site <- as.factor("VUF")
vuf.precip.mean.corr.stack$type <- as.factor("precip")
summary(vuf.precip.mean.corr.stack)

vuf.bm.precip.stack <- merge(vuf.bm.precip.stack, vuf.precip.mean.corr.stack, all.x=T, all.y=T)
summary(vuf.bm.precip.stack)

vlf.bm.precip.stack <- stack(vlf.corr.precip.bm)
summary(vlf.bm.precip.stack)
names(vlf.bm.precip.stack) <- c("corr", "month")
vlf.bm.precip.stack$site <- as.factor("VLF")
vlf.bm.precip.stack$type <- as.factor("precip")
summary(vlf.bm.precip.stack)

vlf.precip.mean.corr.stack <- stack(vlf.precip.mean.corr)
summary(vlf.precip.mean.corr.stack)
names(vlf.precip.mean.corr.stack) <- c("mean.corr", "month")
vlf.precip.mean.corr.stack$site <- as.factor("VLF")
vlf.precip.mean.corr.stack$type <- as.factor("precip")
summary(vlf.precip.mean.corr.stack)

vlf.bm.precip.stack <- merge(vlf.bm.precip.stack, vlf.precip.mean.corr.stack, all.x=T, all.y=T)
summary(vlf.bm.precip.stack)

# PDSI

vuf.bm.pdsi.stack <- stack(vuf.corr.pdsi.bm)
summary(vuf.bm.pdsi.stack)
names(vuf.bm.pdsi.stack) <- c("corr", "month")
vuf.bm.pdsi.stack$site <- as.factor("VUF")
vuf.bm.pdsi.stack$type <- as.factor("pdsi")
summary(vuf.bm.pdsi.stack)

vuf.pdsi.mean.corr.stack <- stack(vuf.pdsi.mean.corr)
summary(vuf.pdsi.mean.corr.stack)
names(vuf.pdsi.mean.corr.stack) <- c("mean.corr", "month")
vuf.pdsi.mean.corr.stack$site <- as.factor("VUF")
vuf.pdsi.mean.corr.stack$type <- as.factor("pdsi")
summary(vuf.pdsi.mean.corr.stack)

vuf.bm.pdsi.stack <- merge(vuf.bm.pdsi.stack, vuf.pdsi.mean.corr.stack, all.x=T, all.y=T)
summary(vuf.bm.pdsi.stack)

vlf.bm.pdsi.stack <- stack(vlf.corr.pdsi.bm)
summary(vlf.bm.pdsi.stack)
names(vlf.bm.pdsi.stack) <- c("corr", "month")
vlf.bm.pdsi.stack$site <- as.factor("VLF")
vlf.bm.pdsi.stack$type <- as.factor("pdsi")
summary(vlf.bm.pdsi.stack)

vlf.pdsi.mean.corr.stack <- stack(vlf.pdsi.mean.corr)
summary(vlf.pdsi.mean.corr.stack)
names(vlf.pdsi.mean.corr.stack) <- c("mean.corr", "month")
vlf.pdsi.mean.corr.stack$site <- as.factor("VLF")
vlf.pdsi.mean.corr.stack$type <- as.factor("pdsi")
summary(vlf.pdsi.mean.corr.stack)

vlf.bm.pdsi.stack <- merge(vlf.bm.pdsi.stack, vlf.pdsi.mean.corr.stack, all.x=T, all.y=T)
summary(vlf.bm.pdsi.stack)


all.valles.bm.stack <- rbind(vuf.bm.tmean.stack, vlf.bm.tmean.stack, vuf.bm.tmin.stack, vlf.bm.tmin.stack, vuf.bm.tmax.stack, vlf.bm.tmax.stack, vuf.bm.precip.stack, vlf.bm.precip.stack, vuf.bm.pdsi.stack, vlf.bm.pdsi.stack)
summary(all.valles.bm.stack)
summary(all.valles.bm.stack$month)

# Setting the factoring so that it will display the  months correctly when we Facet
all.valles.bm.stack$month <- factor(all.valles.bm.stack$month, levels = names(vuf.corr.tmean.bm))
summary(all.valles.bm.stack$month)


# Adding a significance column so that sig. corrs. will pop
all.valles.bm.stack$sig <- ifelse(all.valles.bm.stack$mean.corr >=0.330 | all.valles.bm.stack$mean.corr<= -0.330, "Y", "N")

all.valles.bm.stack$sig <- factor(all.valles.bm.stack$sig, levels = c("Y", "N"))
levels(all.valles.bm.stack$sig)


#######################################################
# Plotting BM correlations
#######################################################
library(ggplot2)
summary(all.valles.bm.stack)

# Critical Value for 28 years (n-2 = 26) 0.330

pdf("figures/BMI_barplot_all_months.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pDJF", "MAM","JJA","SON"),]) + facet_grid(site ~ type , scales="free_x")+
	geom_boxplot(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()	

pdf("figures/BMI_barplot_seasons.pdf", width=13, height=8.5)	
ggplot(data=all.valles.bm.stack) + facet_grid(site ~ type , scales="free_x")+
	geom_boxplot(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()

pdf("figures/BMI_violin_all_months.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pDJF", "MAM","JJA","SON"),]) + facet_grid(site ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()

pdf("figures/BMI_violin_seasons.pdf", width=13, height=8.5)	
ggplot(data=all.valles.bm.stack) + facet_grid(site ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("blue", "gray50"))
dev.off()	






