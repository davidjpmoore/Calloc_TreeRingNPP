##############################################################################
# Comparison of allometric uncertainties and cumulative uncertainty
##############################################################################
library(ggplot2)
library(grid)
library(car)
# -----------------------------------
# loading in & formatting the various datasets that will be needed
# -----------------------------------

# allometric uncertainty of BM at the site level
load("processed_data/valles_allometry_uncertainty.Rdata")
allom.uncert$range <- allom.uncert$UB - allom.uncert$LB
allom.uncert$LB.dev <- allom.uncert$Mean - allom.uncert$LB
allom.uncert$UB.dev <-  allom.uncert$UB - allom.uncert$Mean
allom.uncert$SiteID <- recode(allom.uncert$SiteID, "'VUF'='1'; 'VLF'='2'")
levels(allom.uncert$SiteID) <- c("VUF", "VLF")
allom.uncert <- allom.uncert[order(allom.uncert$Year),]
allom.uncert <- allom.uncert[order(allom.uncert$SiteID),]
summary(allom.uncert)
summary(allom.uncert[allom.uncert$SiteID=="VLF",])
summary(allom.uncert[allom.uncert$SiteID=="VUF",])

# density BM--uses mean allometric eqtn. and accounts for differences in density with just ROSS plots
load("processed_data/valles_density_uncertainty.Rdata")
dens.uncert$range <- dens.uncert$UB - dens.uncert$LB
dens.uncert$LB.dev <- dens.uncert$Mean - dens.uncert$LB
dens.uncert$UB.dev <-  dens.uncert$UB - dens.uncert$Mean
dens.uncert$SiteID <- recode(dens.uncert$SiteID, "'VUF'='1'; 'VLF'='2'")
levels(dens.uncert$SiteID) <- c("VUF", "VLF")
dens.uncert <- dens.uncert[order(dens.uncert$Year),]
dens.uncert <- dens.uncert[order(dens.uncert$SiteID),]
summary(dens.uncert)
summary(dens.uncert[dens.uncert$SiteID=="VLF",])
summary(dens.uncert[dens.uncert$SiteID=="VUF",])

# mortality Uncertainty of BM at the site level
load("processed_data/valles_mortality_uncertainty.Rdata")
names(uncert.mort) <- c("SiteID", "Mean", "Year", "SD", "LB", "UB", "Site")
# we're missing some years here that we need to add back in to make things play nice
dummy.year <- data.frame(Year=dens.uncert$Year, SiteID=dens.uncert$SiteID, Site=dens.uncert$Site) 
uncert.mort <- merge(uncert.mort, dummy.year, all.x=T, all.y=T)
uncert.mort <- uncert.mort[order(uncert.mort$Year),]
uncert.mort$SiteID <- recode(uncert.mort$SiteID, "'VUF'='1'; 'VLF'='2'")
levels(uncert.mort$SiteID) <- c("VUF", "VLF")
uncert.mort <- uncert.mort[order(uncert.mort$SiteID),]

uncert.mort$range <- uncert.mort$UB - uncert.mort$LB
uncert.mort$LB.dev <- uncert.mort$Mean - uncert.mort$LB
uncert.mort$UB.dev <-  uncert.mort$UB - uncert.mort$Mean
summary(uncert.mort)
summary(uncert.mort[uncert.mort$SiteID=="VLF",])
summary(uncert.mort[uncert.mort$SiteID=="VUF",])


# uncertainty in the increment
load("processed_data/valles_increment_uncertainty.Rdata")
names(uncert.increment) <- c("SiteID", "Year", "Mean", "LB", "UB", "Site")
uncert.increment$range <- uncert.increment$UB - uncert.increment$LB
uncert.increment$SiteID <- recode(uncert.increment$SiteID, "'VUF'='1'; 'VLF'='2'")
levels(uncert.increment$SiteID) <- c("VUF", "VLF")
uncert.increment <- uncert.increment[order(uncert.increment$Year),]
uncert.increment <- uncert.increment[order(uncert.increment$SiteID),]
uncert.increment$LB.dev <- uncert.increment$Mean - uncert.increment$LB
uncert.increment$UB.dev <- uncert.increment$UB - uncert.increment$Mean
summary(uncert.increment)
summary(uncert.increment[uncert.increment$SiteID=="VLF",])
summary(uncert.increment[uncert.increment$SiteID=="VUF",])
# -----------------------------------

# Need to change each element of uncertainty into an increment THEN add them together in quadrature

summary(uncert.increment)


# Allometry component
load("processed_data/Biomass_Array_Tree_kgm-2.RData")
dim(bm.array)

# Density component



# Mortality Component

# increment Component


inc[i] <- base + sqrt(plot.uncert[,,sample(1:dim(bm.array)[3])]^2 + mort.uncert.inc[i] + (base.incr-dens.uncert.inc[,sample(cols.vu)])^2 + inc.uncert[i])

inc.use.in.r2 <- base + sqrt(allom.inc.uncert[,,sample(XXX)]^2 + mort.inc.uncert[,sample(XXX)]^2 + [...])

# VLF increments first

for(j in 3:dim(vlf.tot.inc)[2]){
	
		for (i in 1:(length(vlf.tot.inc[,j])-1)){
		vlf.tot.inc[i,j]<- ifelse(!is.na(vlf.tot.dev[i,j]), vlf.tot.dev[i,j] - vlf.tot.dev[i+1,j], valles.tot.dev[i,j])
	}
}  

summary(vlf.tot.inc)

ggplot(data = vlf.tot.inc[vlf.tot.inc$Year>1990,]) + #facet_grid(SiteID~.) +
	geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=Base - LB.dev, ymax=Base + UB, fill="1"), alpha=0.6) +
