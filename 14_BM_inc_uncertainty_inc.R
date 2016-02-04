##############################################################################
# Comparison of allometric uncertainties and cumulative uncertainty
##############################################################################
library(ggplot2)
library(grid)
library(car)
# -----------------------------------
# loading in & formatting the various datasets that will be needed
# -----------------------------------
# Reading in site level biomass so that we have something to scale all of the uncertainties toward
bm.site <- read.csv("processed_data/Biomass_Site_Total_kgm-2.csv", header=T)
summary(bm.site)
dim(bm.site)

vuf.site <- bm.site[bm.site$SiteID=="VU",c("Year", "BM.Mean")]

vuf.site <- vuf.site[order(vuf.site$Year, decreasing=T),]
head(vuf.site)
row.names(vuf.site)<- vuf.site$Year
vuf.site <- vuf.site[vuf.site$Year<=2011,]
summary(vuf.site)
names(vuf.site) <- c("Year", "vuf.base")


vuf.site.inc <- vuf.site

for(i in 1:(length(vuf.site.inc$vuf.base)-1)){
	vuf.site.inc[i,"vuf.base"] <- vuf.site[i,"vuf.base"] - vuf.site[i+1,"vuf.base"]
}
summary(vuf.site.inc)
head(vuf.site.inc)


vlf.site <- bm.site[bm.site$SiteID=="VL",c("Year", "BM.Mean")]

vlf.site <- vlf.site[order(vlf.site$Year, decreasing=T),]
head(vlf.site)
row.names(vlf.site)<- vlf.site$Year
vlf.site <- vlf.site[vlf.site$Year<=2011,]
summary(vlf.site)
names(vlf.site) <- c("Year", "vlf.base")


vlf.site.inc <- vlf.site

for(i in 1:(length(vlf.site.inc$vlf.base)-1)){
	vlf.site.inc[i,"vlf.base"] <- vlf.site[i,"vlf.base"] - vlf.site[i+1, "vlf.base"]
}
summary(vlf.site.inc)
head(vlf.site.inc)

valles.base.inc <- merge(vlf.site.inc, vuf.site.inc, by="Year", all.x=T, all.y=T)
summary(valles.base.inc)
head(valles.base.inc)

valles.base.inc <- valles.base.inc[order(valles.base.inc$Year, decreasing=T),]
head(valles.base.inc)

# Now we have increments for each site that are ordered in the same way that we ordered the various uncertainty matrices.

#---------------------------------------------------
# allometric uncertainty of BM at the site level
#---------------------------------------------------
load("processed_data/vuf_allom_inc.Rdata")
load("processed_data/vlf_allom_inc.Rdata")

dim(vuf.allom.inc)
summary(vuf.allom.inc[,1:10,1])

dim(vlf.allom.inc)
summary(vlf.allom.inc[,1:10,1])



#---------------------------------------------------
# Density component
#---------------------------------------------------
load(file="processed_data/dens_inc.Rdata")
dim(dens.inc)
summary(dens.inc)
head(dens.inc)


#---------------------------------------------------
# increment Component
#---------------------------------------------------
load("processed_data/vuf_inc.Rdata")
load("processed_data/vlf_inc.Rdata")

dim(vuf.inc)
summary(vuf.inc)
head(vuf.inc)

dim(vlf.inc)
summary(vlf.inc)
head(vlf.inc)


#---------------------------------------------------
# Mortality Component
# Composed of 500 pulls for the mortality for each site over the timespan 1905-2011
#---------------------------------------------------
load("processed_data/vuf_mort_inc.Rdata")
load("processed_data/vlf_mort_inc.Rdata")

dim(vuf.mort.inc)
summary(vuf.mort.inc)
row.names(vuf.mort.inc)

dim(vlf.mort.inc)
summary(vlf.mort.inc)
row.names(vlf.mort.inc)

#---------------------------------------------------
# Need to do three things
# 1) get the deviations of the wiggles of each area of uncertainty
#	 1a) Which means subtracting the wiggles of each area from the base wiggles
# 2) get random samples of each uncertainty deviation for each year at each site
# 3) add  up the different devaitions of uncertainty, relativeizing to the base increment for each site, to show how the uncertainty of the increment changes through time.
#

# This will create a data frame for each site (VLF, VUF) that we can then draw random samples from for the climate sensitivity analysis.



#---------------------------------------------------

# test <- vuf.allom.inc[,,sample(1:dim(vuf.allom.inc)[3], 1000, replace=T)]
# summary(test)
# dim(test)

#---------------------------------------------------
# Allometry Deviations
#---------------------------------------------------

par(new=F)
for(i in 1:length(dimnames(vuf.allom.inc)[[2]])){
	for(j in 1:5){
		plot(vuf.allom.inc[,i,j] ~ dimnames(vuf.allom.inc)[[1]], type="l", ylim=c(0,10))
		par(new=T)
	}
}
plot(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc)[[1]], type="l", ylim=c(0,10), col="red", lwd=3)

# We want to figure out how each run of the allometries differs from the base increment
# VUF
vuf.allom.inc.dev <- vuf.allom.inc
vuf.allom.inc.dev[,,] <- NA

vuf.allom.inc.dev <- vuf.allom.inc[,,] -  valles.base.inc$vuf.base  

dim(vuf.allom.inc.dev)
dimnames(vuf.allom.inc.dev)
summary(vuf.allom.inc.dev[,1:10,1])

# Condense dimensions down to mean tree at a site but keep iterations to retain the variability in the allometric equations used; this will help us add things together easier in the next step

#													c(,) ==> lists dimensions we want to KEEP!
vuf.allom.inc.dev <- data.frame(apply(vuf.allom.inc.dev, c(1,3), mean, na.rm=T))
dim(vuf.allom.inc.dev)
summary(vuf.allom.inc.dev[,1:10])

# VLF
vlf.allom.inc.dev <- vlf.allom.inc
vlf.allom.inc.dev[,,] <- NA

vlf.allom.inc.dev <- vlf.allom.inc[,,] -  valles.base.inc$vlf.base  

dim(vlf.allom.inc.dev)
dimnames(vlf.allom.inc.dev)
summary(vlf.allom.inc.dev[,1:10,1])

# Condensing down to just two dimensions like we did in ln. 159 above
vlf.allom.inc.dev <- data.frame(apply(vlf.allom.inc.dev, c(1,3), mean, na.rm=T))
dim(vlf.allom.inc.dev)
summary(vlf.allom.inc.dev[,1:10])

#---------------------------------------------------
# Density Deviations
#---------------------------------------------------

dim(dens.inc)
summary(dens.inc)
head(dens.inc)

vuf.dens <- dens.inc[,substr(names(dens.inc), 1,2)=="VU"]
summary(vuf.dens)

vuf.dens.dev <- vuf.dens - valles.base.inc$vuf.base
summary(vuf.dens.dev)
head(vuf.dens.dev)


vlf.dens <- dens.inc[,substr(names(dens.inc), 1,2)=="VL"]
summary(vlf.dens)

vlf.dens.dev <- vlf.dens - valles.base.inc$vlf.base
summary(vlf.dens.dev)
head(vlf.dens.dev)


#---------------------------------------------------
# Increment Deviations
#---------------------------------------------------

dim(vuf.inc)
summary(vuf.inc)
head(vuf.inc)


vuf.inc.dev <- vuf.inc - valles.base.inc$vuf.base
summary(vuf.inc.dev)

dim(vlf.inc)
summary(vlf.inc)
head(vlf.inc)

vlf.inc.dev <- vlf.inc - valles.base.inc$vlf.base
summary(vlf.inc.dev)


#---------------------------------------------------
# Mortality Deviations
#---------------------------------------------------

dim(vuf.mort.inc)
summary(vuf.mort.inc)
row.names(vuf.mort.inc)

# Adding a row of NA's for 1904 so the dimensions align
vuf.mort.inc <- rbind(vuf.mort.inc, NA) 
row.names(vuf.mort.inc)<- c(2011:1904)




vuf.mort.dev <- vuf.mort.inc[,] - valles.base.inc$vuf.base
summary(vuf.mort.dev)
vuf.mort.dev


dim(vlf.mort.inc)
summary(vlf.mort.inc)
row.names(vlf.mort.inc)
# Adding a row of NA's for 1904 so dimensions align.
vlf.mort.inc <- rbind(vlf.mort.inc, NA)
row.names(vlf.mort.inc) <- c(2011:1904)


vlf.mort.dev <- vlf.mort.inc[,] - valles.base.inc$vlf.base
summary(vlf.mort.dev)

#---------------------------------------------------
# Adding up in quadrature the deviations from the different areas while bootstrapping
#---------------------------------------------------
n.pulls=1000
set.seed(1117)
# VUF Site

vuf.inc.tot <- valles.base.inc$vuf.base + sqrt(vuf.allom.inc.dev[,sample(1:ncol(vuf.allom.inc.dev), n.pulls, replace=T)]^2) + sqrt(vuf.dens.dev[,sample(1:ncol(vuf.dens.dev), n.pulls, replace=T)]^2) + sqrt(vuf.inc.dev[,sample(1:ncol(vuf.inc.dev), n.pulls, replace=T)]^2)

summary(vuf.inc.tot)



par(new=F)
for(i in 1:ncol(vuf.inc.tot)){
		plot(vuf.inc.tot[,i] ~ row.names(vuf.inc.tot), type="l", ylim=c(0,10))
		par(new=T)
}
lines	(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc)[[1]], type="l", ylim=c(0,10), col="red", lwd=3)

save(vuf.inc.tot, file="processed_data/vuf_bm_boot_tot_inc.Rdata")
# VLF Site

vlf.inc.tot <- valles.base.inc$vlf.base + sqrt(vlf.allom.inc.dev[,sample(1:ncol(vlf.allom.inc.dev), n.pulls, replace=T)]^2) + sqrt(vlf.dens.dev[,sample(1:ncol(vlf.dens.dev), n.pulls, replace=T)]^2) + sqrt(vlf.inc.dev[,sample(1:ncol(vlf.inc.dev), n.pulls, replace=T)]^2) + sqrt(vlf.mort.dev[,sample(1:ncol(vlf.mort.dev), n.pulls, replace=T)]^2)

summary(vlf.inc.tot)

par(new=F)
for(i in 1:ncol(vlf.inc.tot)){
		plot(vlf.inc.tot[,i] ~ row.names(vlf.inc.tot), type="l", ylim=c(0,10))
		par(new=T)
}
lines	(valles.base.inc$vlf.base ~ dimnames(vlf.allom.inc)[[1]], type="l", ylim=c(0,10), col="red", lwd=3)

save(vlf.inc.tot, file="processed_data/vlf_bm_boot_tot_inc.Rdata")
