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

save(valles.base.inc, file="processed_data/valles_base_increment.Rdata")

# Now we have increments for each site that are ordered in the same way that we ordered the various uncertainty matrices.

#---------------------------------------------------
# allometric uncertainty of BM at the site level
#---------------------------------------------------
load("processed_data/vuf_allom_inc.Rdata")
load("processed_data/vlf_allom_inc.Rdata")

dim(vuf.allom.inc)
min(vuf.allom.inc)
summary(vuf.allom.inc[,1:10,1])

dim(vlf.allom.inc)
min(vlf.allom.inc)
summary(vlf.allom.inc[,1:10,1])



#---------------------------------------------------
# Density component
#---------------------------------------------------
load(file="processed_data/dens_inc.Rdata")
dim(dens.inc)
min(dens.inc, na.rm=T)
summary(dens.inc)
head(dens.inc)

vuf.dens <- dens.inc[,substr(names(dens.inc),1,2)=="VU"]
vlf.dens <- dens.inc[,substr(names(dens.inc),1,2)=="VL"]
#---------------------------------------------------
# increment Component
#---------------------------------------------------
load("processed_data/vuf_inc.Rdata")
load("processed_data/vlf_inc.Rdata")

dim(vuf.inc)
min(vuf.inc)
summary(vuf.inc)
head(vuf.inc)

dim(vlf.inc)
min(vlf.inc)
summary(vlf.inc)
head(vlf.inc)


#---------------------------------------------------
# Mortality Component
# Composed of 500 pulls for the mortality for each site over the timespan 1905-2011
#---------------------------------------------------
load("processed_data/vuf_mort_inc.Rdata")
load("processed_data/vlf_mort_inc.Rdata")

dim(vuf.mort.inc)
min(vuf.mort.inc, na.rm=T)
summary(vuf.mort.inc)
row.names(vuf.mort.inc)

# # vuf.mort.inc2 <- ifelse(vuf.mort.inc < 0, 0, vuf.mort.inc)
# min(vuf.mort.inc2, na.rm=T)


dim(vlf.mort.inc)
min(vlf.mort.inc, na.rm=T)
summary(vlf.mort.inc)
row.names(vlf.mort.inc)

# vlf.mort.inc2 <- ifelse(vlf.mort.inc < 0,0,vlf.mort.inc)
# min(vlf.mort.inc2, na.rm=T)

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

# We want to figure out how each run of the allometries differs from the base increment
# VUF
vuf.allom.inc2 <- data.frame(apply(vuf.allom.inc, c(1,3), mean, na.rm=T))
summary(vuf.allom.inc2)

par(new=F)
for(i in 1:100){
    plot(vuf.allom.inc2[,i] ~ dimnames(vuf.allom.inc2)[[1]], type="l", ylim=c(0, max(vuf.allom.inc2)))
    par(new=T)
}
lines(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc2)[[1]], type="l", col="red", lwd=3)


vuf.allom.inc.dev <- vuf.allom.inc2
vuf.allom.inc.dev[,] <- NA

vuf.allom.inc.dev.ci <- data.frame(Year = row.names(vuf.allom.inc.dev))

for(i in vuf.allom.inc.dev.ci$Year){
	vuf.allom.inc.dev.ci[vuf.allom.inc.dev.ci$Year==i,"UB.dev"] <- quantile(vuf.allom.inc2[i,], 0.975, na.rm=T)
	vuf.allom.inc.dev.ci[vuf.allom.inc.dev.ci$Year==i,"LB.dev"] <- quantile(vuf.allom.inc2[i,], 0.025, na.rm=T)
}
summary(vuf.allom.inc.dev.ci)

vuf.allom.inc.dev <-  data.frame(Year = vuf.allom.inc.dev.ci$Year,
								 UB.dev = vuf.allom.inc.dev.ci[,"UB.dev"] - 												apply(vuf.allom.inc2,1,FUN=mean),
								 LB.dev = apply(vuf.allom.inc2,1,FUN=mean) - 
								 				vuf.allom.inc.dev.ci[,"LB.dev"])

summary(vuf.allom.inc.dev)
vuf.allom.inc.dev$Year <- as.numeric(vuf.allom.inc.dev$Year)
vuf.allom.inc.dev$Year <- as.numeric(2011:1904)
vuf.allom.inc.dev$range <- vuf.allom.inc.dev$UB.dev + vuf.allom.inc.dev$LB.dev
vuf.allom.inc.dev$Site <- as.factor("VUF")
summary(vuf.allom.inc.dev)

plot(vuf.allom.inc.dev$UB.dev ~ vuf.allom.inc.dev$Year, type="l", ylim=range(vuf.allom.inc.dev$UB.dev))
lines(vuf.allom.inc.dev$LB.dev ~ vuf.allom.inc.dev$Year, type="l", col="red", lwd=3)

# VLF
vlf.allom.inc2 <- data.frame(apply(vlf.allom.inc, c(1,3), mean, na.rm=T))
dim(vlf.allom.inc2)


vlf.allom.inc.dev <- vlf.allom.inc2
vlf.allom.inc.dev[,] <- NA

vlf.allom.inc.dev.ci <- data.frame(Year = row.names(vlf.allom.inc.dev))

for(i in vlf.allom.inc.dev.ci$Year){
	vlf.allom.inc.dev.ci[vlf.allom.inc.dev.ci$Year==i,"UB.dev"] <- quantile(vlf.allom.inc2[i,], 0.975, na.rm=T)
	vlf.allom.inc.dev.ci[vlf.allom.inc.dev.ci$Year==i,"LB.dev"] <- quantile(vlf.allom.inc2[i,], 0.025, na.rm=T)
}
summary(vlf.allom.inc.dev.ci)

vlf.allom.inc.dev <-  data.frame(Year = vlf.allom.inc.dev.ci$Year,
								 UB.dev = vlf.allom.inc.dev.ci[,"UB.dev"] - 												apply(vlf.allom.inc2,1,FUN=mean),
								 LB.dev = apply(vlf.allom.inc2,1,FUN=mean) - 
								 				vlf.allom.inc.dev.ci[,"LB.dev"])
								 				
vlf.allom.inc.dev$Year <- as.numeric(vlf.allom.inc.dev$Year)
vlf.allom.inc.dev$Year <- as.numeric(2011:1904)
vlf.allom.inc.dev$Site <- as.factor("VLF")
vlf.allom.inc.dev$range <- vlf.allom.inc.dev$UB.dev + vlf.allom.inc.dev$LB.dev
summary(vlf.allom.inc.dev)

valles.allom.inc.dev <- merge(vlf.allom.inc.dev, vuf.allom.inc.dev, all.x=T, all.y=T)
summary(valles.allom.inc.dev)
valles.allom.inc.dev$type <- as.factor("allom")
summary(valles.allom.inc.dev)

#---------------------------------------------------
# Density Deviations
#---------------------------------------------------

vuf.dens.dev.ci <- data.frame(Year = row.names(vuf.dens))

for(i in vuf.dens.dev.ci$Year){
	vuf.dens.dev.ci[vuf.dens.dev.ci$Year==i,"UB.dev"] <- quantile(vuf.dens[i,], 0.975, na.rm=T)
	vuf.dens.dev.ci[vuf.dens.dev.ci$Year==i,"LB.dev"] <- quantile(vuf.dens[i,], 0.025, na.rm=T)
}
summary(vuf.dens.dev.ci)

vuf.dens.dev <-  data.frame(Year = vuf.dens.dev.ci$Year,
								 UB.dev = vuf.dens.dev.ci[,"UB.dev"] - 												apply(vuf.dens,1,FUN=mean),
								 LB.dev = apply(vuf.dens,1,FUN=mean) - 
								 				vuf.dens.dev.ci[,"LB.dev"])

summary(vuf.dens.dev)
vuf.dens.dev$Year <- as.numeric(vuf.dens.dev$Year)
vuf.dens.dev$Year <- as.numeric(2011:1904)
vuf.dens.dev$Site <- as.factor("VUF")
vuf.dens.dev$range <- vuf.dens.dev.ci$UB.dev + vuf.dens.dev.ci$LB.dev

plot(vuf.dens.dev$UB.dev ~ vuf.dens.dev$Year, type="l", ylim=range(vuf.dens.dev$UB.dev, na.rm=T))
lines(vuf.dens.dev$LB.dev ~ vuf.dens.dev$Year, type="l", col="red", lwd=3)

# VLF

vlf.dens.dev.ci <- data.frame(Year = row.names(vlf.dens))

for(i in vlf.dens.dev.ci$Year){
	vlf.dens.dev.ci[vlf.dens.dev.ci$Year==i,"UB.dev"] <- quantile(vlf.dens[i,], 0.975, na.rm=T)
	vlf.dens.dev.ci[vlf.dens.dev.ci$Year==i,"LB.dev"] <- quantile(vlf.dens[i,], 0.025, na.rm=T)
}
summary(vlf.dens.dev.ci)

vlf.dens.dev <-  data.frame(Year = vlf.dens.dev.ci$Year,
								 UB.dev = vlf.dens.dev.ci[,"UB.dev"] - 												apply(vlf.dens,1,FUN=mean),
								 LB.dev = apply(vlf.dens,1,FUN=mean) - 
								 				vlf.dens.dev.ci[,"LB.dev"])
								 				

summary(vlf.dens.dev)
vlf.dens.dev$Year <- as.numeric(vlf.dens.dev$Year)
vlf.dens.dev$Year <- as.numeric(2011:1904)
vlf.dens.dev$Site <- as.factor("VLF")
vlf.dens.dev$range <- vlf.dens.dev$UB.dev + vlf.dens.dev$LB.dev
summary(vlf.dens.dev)

valles.dens.dev <- merge(vlf.dens.dev, vuf.dens.dev, all.x=T, all.y=T)
summary(valles.dens.dev)
valles.dens.dev$type <- as.factor("dens")

#---------------------------------------------------
# Increment Deviations
#---------------------------------------------------
summary(vuf.inc)

vuf.inc.dev.ci <- data.frame(Year = row.names(vuf.inc))

for(i in vuf.inc.dev.ci$Year){
	vuf.inc.dev.ci[vuf.inc.dev.ci$Year==i,"UB.dev"] <- quantile(vuf.inc[i,], 0.975, na.rm=T)
	vuf.inc.dev.ci[vuf.inc.dev.ci$Year==i,"LB.dev"] <- quantile(vuf.inc[i,], 0.025, na.rm=T)
}
summary(vuf.inc.dev.ci)

vuf.inc.dev <-  data.frame(Year = vuf.inc.dev.ci$Year,
								 UB.dev = vuf.inc.dev.ci[,"UB.dev"] - 												apply(vuf.inc,1,FUN=mean),
								 LB.dev = apply(vuf.inc,1,FUN=mean) - 
								 				vuf.inc.dev.ci[,"LB.dev"])

summary(vuf.inc.dev)
vuf.inc.dev$Year <- as.numeric(vuf.inc.dev$Year)
vuf.inc.dev$Year <- as.numeric(2011:1904)
vuf.inc.dev$Site <- as.factor("VUF")
vuf.inc.dev$range <- vuf.inc.dev$UB.dev + vuf.inc.dev$LB.dev

plot(vuf.inc.dev$UB.dev ~ vuf.inc.dev$Year, type="l", ylim=range(vuf.inc.dev$UB.dev, na.rm=T))
lines(vuf.inc.dev$LB.dev ~ vuf.inc.dev$Year, type="l", col="red", lwd=3)

# VLF

vlf.inc.dev.ci <- data.frame(Year = row.names(vlf.inc))

for(i in vlf.inc.dev.ci$Year){
	vlf.inc.dev.ci[vlf.inc.dev.ci$Year==i,"UB.dev"] <- quantile(vlf.inc[i,], 0.975, na.rm=T)
	vlf.inc.dev.ci[vlf.inc.dev.ci$Year==i,"LB.dev"] <- quantile(vlf.inc[i,], 0.025, na.rm=T)
}
summary(vlf.inc.dev.ci)

vlf.inc.dev <-  data.frame(Year = vlf.inc.dev.ci$Year,
								 UB.dev = vlf.inc.dev.ci[,"UB.dev"] - 												apply(vlf.inc,1,FUN=mean),
								 LB.dev = apply(vlf.inc,1,FUN=mean) - 
								 				vlf.inc.dev.ci[,"LB.dev"])
								 				
vlf.inc.dev$Year <- as.numeric(vlf.inc.dev$Year)
vlf.inc.dev$Year <- as.numeric(2011:1904)
vlf.inc.dev$Site <- as.factor("VLF")
vlf.inc.dev$range <- vlf.inc.dev$UB.dev + vlf.inc.dev$LB.dev
summary(vlf.inc.dev)

valles.inc.dev <- merge(vlf.inc.dev, vuf.inc.dev, all.x=T, all.y=T)
summary(valles.inc.dev)
valles.inc.dev$type <- as.factor("inc")

#---------------------------------------------------
# Mortality Deviations
#---------------------------------------------------


# Adding a row of NA's for 1904 so the dimensions align
vuf.mort.inc <- rbind(vuf.mort.inc, NA) 
row.names(vuf.mort.inc)<- c(2011:1904)
vlf.mort.inc <- rbind(vlf.mort.inc, NA) 
row.names(vlf.mort.inc)<- c(2011:1904)




vuf.mort.inc.dev.ci <- data.frame(Year = row.names(vuf.mort.inc))

for(i in vuf.mort.inc.dev.ci$Year){
	vuf.mort.inc.dev.ci[vuf.mort.inc.dev.ci$Year==i,"UB.dev"] <- quantile(vuf.mort.inc[i,], 0.975, na.rm=T)
	vuf.mort.inc.dev.ci[vuf.mort.inc.dev.ci$Year==i,"LB.dev"] <- quantile(vuf.mort.inc[i,], 0.025, na.rm=T)
}
summary(vuf.mort.inc.dev.ci)

vuf.mort.inc.dev <-  data.frame(Year = vuf.mort.inc.dev.ci$Year,
								 UB.dev = vuf.mort.inc.dev.ci[,"UB.dev"] - 												apply(vuf.mort.inc,1,FUN=mean),
								 LB.dev = apply(vuf.mort.inc,1,FUN=mean) - 
								 				vuf.mort.inc.dev.ci[,"LB.dev"])

summary(vuf.mort.inc.dev)
vuf.mort.inc.dev$Year <- as.numeric(vuf.mort.inc.dev$Year)
vuf.mort.inc.dev$Year <- as.numeric(2011:1904)
vuf.mort.inc.dev$Site <- as.factor("VUF")
vuf.mort.inc.dev$range <- vuf.mort.inc.dev$UB.dev + vuf.mort.inc.dev$LB.dev


plot(vuf.mort.inc.dev$UB.dev ~ vuf.mort.inc.dev$Year, type="l", ylim=range(vuf.mort.inc.dev$UB.dev, na.rm=T))
lines(vuf.mort.inc.dev$LB.dev ~ vuf.mort.inc.dev$Year, type="l", col="red", lwd=3)

# VLF

vlf.mort.inc.dev.ci <- data.frame(Year = row.names(vlf.mort.inc2))

for(i in vlf.mort.inc.dev.ci$Year){
	vlf.mort.inc.dev.ci[vlf.mort.inc.dev.ci$Year==i,"UB.allom"] <- quantile(vlf.mort.inc[i,], 0.975, na.rm=T)
	vlf.mort.inc.dev.ci[vlf.mort.inc.dev.ci$Year==i,"LB.allom"] <- quantile(vlf.mort.inc[i,], 0.025, na.rm=T)
}
summary(vlf.mort.inc.dev.ci)

vlf.mort.inc.dev <-  data.frame(Year = vlf.mort.inc.dev.ci$Year,
								 UB.dev = vlf.mort.inc.dev.ci[,"UB.allom"] - 												apply(vlf.mort.inc,1,FUN=mean),
								 LB.dev = apply(vlf.mort.inc,1,FUN=mean) - 
								 				vlf.mort.inc.dev.ci[,"LB.allom"])
								 				
vlf.mort.inc.dev$Year <- as.numeric(vlf.mort.inc.dev$Year)
vlf.mort.inc.dev$Year <- as.numeric(2011:1904)
vlf.mort.inc.dev$Site <- as.factor("VLF")
vlf.mort.inc.dev$range <- vlf.mort.inc.dev$UB.dev + vlf.mort.inc.dev$LB.dev
summary(vlf.mort.inc.dev)

valles.mort.inc.dev <- merge(vlf.mort.inc.dev, vuf.mort.inc.dev, all.x=T, all.y=T)
summary(valles.mort.inc.dev)
valles.mort.inc.dev$type <- as.factor("mort")
#---------------------------------------------------
# Adding up in quadrature the deviations from the different areas while bootstrapping
#---------------------------------------------------
# n.pulls=100
# set.seed(0946)
# # VUF Site

# cols.inc.dev   <- sample(1:ncol(vuf.inc.dev), n.pulls, replace=T)
# cols.allom.dev <- sample(1:ncol(vuf.allom.inc.dev), n.pulls, replace=T)
# cols.dens.dev  <- sample(1:ncol(vuf.dens.dev), n.pulls, replace=T)
# cols.mort.dev  <- sample(1:ncol(vuf.mort.dev), n.pulls, replace=T)



# vuf.inc.tot <- sqrt(vuf.allom.inc.dev[,cols.allom.dev]^2 + 
                                               # vuf.dens.dev[,cols.dens.dev]^2 + 
                                               # vuf.inc.dev[,cols.inc.dev]^2 +
                                               # vuf.mort.dev[,cols.mort.dev]^2)

# So I think we need to add things in the same manner as we did in the cumulative uncertainty script. 
# 1) Take the UB and LB CI's for each area
# 2) Add the UB to the UB's and the LB's to the LB's
# This should solve the weird issues we were having on 3/4.

# Merging all component deviation arrays into one data frame

valles.all.inc <- rbind(valles.allom.inc.dev, valles.dens.dev, valles.inc.dev, valles.mort.inc.dev)

summary(valles.all.inc)


vuf.uncert <- data.frame(Year = row.names(vuf.allom.inc.dev))

vuf.uncert$UB.dev <- sqrt(vuf.allom.inc.dev$UB.dev^2 + vuf.inc.dev$UB.dev^2 + vuf.dens.dev$UB.dev^2 + vuf.mort.inc.dev$UB.dev^2)

vuf.uncert$LB.dev <- sqrt(vuf.allom.inc.dev$LB.dev^2 + vuf.inc.dev$LB.dev^2 + vuf.dens.dev$LB.dev^2 + vuf.mort.inc.dev$LB.dev^2)

vuf.uncert$Site <- as.factor("VUF")
vuf.uncert$type <- as.factor("total")
vuf.uncert$Year <- as.numeric(2011:1904)
vuf.uncert$range <- vuf.uncert$UB.dev + vuf.uncert$LB.dev
summary(vuf.uncert)


vlf.uncert <- data.frame(Year = row.names(vlf.allom.inc.dev))

vlf.uncert$UB.dev <- sqrt(vlf.allom.inc.dev$UB.dev^2 + vlf.inc.dev$UB.dev^2 + vlf.dens.dev$UB.dev^2 + vlf.mort.inc.dev$UB.dev^2)

vlf.uncert$LB.dev <- sqrt(vlf.allom.inc.dev$LB.dev^2 + vlf.inc.dev$LB.dev^2 + vlf.dens.dev$LB.dev^2 + vlf.mort.inc.dev$LB.dev^2)

vlf.uncert$Site <- as.factor("VLF")
vlf.uncert$type <- as.factor("total")
vlf.uncert$Year <- as.numeric(2011:1904)
vlf.uncert$range <- vlf.uncert$UB.dev + vlf.uncert$LB.dev
summary(vlf.uncert)

dim(vuf.uncert)
dim(vlf.uncert)
valles.uncert <- merge(vuf.uncert, vlf.uncert, all.x=T, all.y=T)
dim(valles.uncert)


dim(valles.all.inc)
summary(valles.all.inc)
summary(valles.uncert)

valles.all.uncert <- merge(valles.all.inc, valles.uncert, all.x=T, all.y=T)
dim(valles.all.uncert)
summary(valles.all.uncert)

summary(valles.base.inc)

vuf.base <- valles.base.inc[,c("Year", "vuf.base")]
names(vuf.base) <- c("Year", "base")
vuf.base$Site <- as.factor("VUF")

vlf.base <- valles.base.inc[,c("Year", "vlf.base")]
names(vlf.base) <- c("Year", "base")
vlf.base$Site <- as.factor("VLF")

valles.base<- merge(vuf.base, vlf.base, all.x=T, all.y=T)
dim(valles.base)
summary(valles.base)

valles.all.uncert <- merge(valles.all.uncert, valles.base, all.x=T, all.y=F)
summary(valles.all.uncert)

save(valles.all.uncert, file="processed_data/valles_bm_boot_tot_inc.Rdata")

#--------------------------------------------------------------------
################################################
# Graphing the cumulative increment uncertainty
################################################

summary(valles.all.uncert)
valles.all.uncert$Site <- factor(valles.all.uncert$Site, levels = c("VUF", "VLF"))

pdf("figures/bm_inc_uncert_quad.pdf", width=13, height=8.5)
ggplot(data=valles.all.uncert[!valles.all.uncert$type=="total",]) + facet_grid(type~Site) +
	
	geom_ribbon(aes(x=Year, ymin= base - LB.dev, ymax= UB.dev + base), fill="darkgrey", alpha=0.6) +
	
	geom_line(aes(x=Year, y=base), size=1.5, color="black") +
  #geom_line(aes(x=year, y=mean), size=1.5, color="black") +
	labs(title= "Biomass Increment Total Uncertainty", x="Year", y=expression(bold(paste("Biomass (kg m" ^ "-2)")))) +
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))


dev.off()     

################################################################################################
# Making a dataframe that has eacha area of uncertainty identified individaully (similar to graph produced in script #10) so that we can see the contribution of individual areas ot the overall uncertainty
################################################################################################

# Making stacked uncertainties graph
# Need to add up things first
summary(valles.all.uncert)

# Incerement only
# VUF
vuf.inc.graph <- vuf.inc.dev
names(vuf.inc.graph) <- c("Year", "UB.inc", "LB.inc", "range", "Site")
summary(vuf.inc.graph)


# VLF
vlf.inc.graph <- vlf.inc.dev
names(vlf.inc.graph) <- c("Year", "UB.inc", "LB.inc", "Site", "range")
summary(vlf.inc.graph)

valles.inc.graph <- merge(vuf.inc.graph, vlf.inc.graph, all.x=T, all.y=T)

#----------------------------------------------------------------
# Inc + allom
# VUF
vuf.allom.graph <- vuf.inc.dev 
names(vuf.allom.graph) <- c("Year", "UB.allom", "LB.allom", "range","Site")
vuf.allom.graph$UB.allom <- sqrt(vuf.inc.dev$UB.dev^2 + vuf.allom.inc.dev$UB.dev^2)

vuf.allom.graph$LB.allom <- sqrt(vuf.inc.dev$LB.dev^2 + vuf.allom.inc.dev$LB.dev^2)

summary(vuf.allom.graph)


# VLF
vlf.allom.graph <- vlf.inc.dev 
names(vlf.allom.graph) <- c("Year", "UB.allom", "LB.allom", "Site", "range")
vlf.allom.graph$UB.allom <- sqrt(vlf.inc.dev$UB.dev^2 + vlf.allom.inc.dev$UB.dev^2)

vlf.allom.graph$LB.allom <- sqrt(vlf.inc.dev$LB.dev^2 + vlf.allom.inc.dev$LB.dev^2)

summary(vlf.allom.graph)

valles.allom.graph <- merge(vuf.allom.graph, vlf.allom.graph, all.x=T, all.y=T)

#--------------------------------------------------------
# Inc + allom + dens
# VUF

vuf.dens.graph <- vuf.inc.dev 
names(vuf.dens.graph) <- c("Year", "UB.dens", "LB.dens", "range","Site")
vuf.dens.graph$UB.dens <- sqrt(vuf.inc.dev$UB.dev^2 + vuf.allom.inc.dev$UB.dev^2 + vuf.dens.dev$UB.dev^2)

vuf.dens.graph$LB.dens <- sqrt(vuf.inc.dev$LB.dev^2 + vuf.allom.inc.dev$LB.dev^2 + vuf.dens.dev$LB.dev^2)

summary(vuf.dens.graph)


# VLF
vlf.dens.graph <- vlf.inc.dev 
names(vlf.dens.graph) <- c("Year", "UB.dens", "LB.dens", "Site", "range")
vlf.dens.graph$UB.dens <- sqrt(vlf.inc.dev$UB.dev^2 + vlf.allom.inc.dev$UB.dev^2 + vlf.dens.dev$UB.dev^2)

vlf.dens.graph$LB.dens <- sqrt(vlf.inc.dev$LB.dev^2 + vlf.allom.inc.dev$LB.dev^2 + vlf.dens.dev$LB.dev^2)

summary(vlf.dens.graph)
valles.dens.graph <- merge(vuf.dens.graph, vlf.dens.graph, all.x=T, all.y=T)
summary(valles.dens.graph)
#--------------------------------------------------------
# Inc + allom + dens + mort
# VUF

vuf.mort.graph <- vuf.inc.dev 
names(vuf.mort.graph) <- c("Year", "UB.mort", "LB.mort", "range","Site")
vuf.mort.graph$UB.mort <- sqrt(vuf.inc.dev$UB.dev^2 + vuf.allom.inc.dev$UB.dev^2 + vuf.dens.dev$UB.dev^2 + vuf.mort.inc.dev$UB.dev^2)

vuf.mort.graph$LB.mort <- sqrt(vuf.inc.dev$LB.dev^2 + vuf.allom.inc.dev$LB.dev^2 + vuf.dens.dev$LB.dev^2 + vuf.mort.inc.dev$LB.dev^2)

summary(vuf.mort.graph)


# VLF
vlf.mort.graph <- vlf.inc.dev 
names(vlf.mort.graph) <- c("Year", "UB.mort", "LB.mort", "Site", "range")
vlf.mort.graph$UB.mort <- sqrt(vlf.inc.dev$UB.dev^2 + vlf.allom.inc.dev$UB.dev^2 + vlf.dens.dev$UB.dev^2 + vlf.mort.inc.dev$UB.dev^2)

vlf.mort.graph$LB.mort <- sqrt(vlf.inc.dev$LB.dev^2 + vlf.allom.inc.dev$LB.dev^2 + vlf.dens.dev$LB.dev^2 + vlf.mort.inc.dev$LB.dev^2)

summary(vlf.mort.graph)
valles.mort.graph <- merge(vuf.mort.graph, vlf.mort.graph, all.x=T, all.y=T)
summary(valles.mort.graph)

# Merging data frames together to make one valles.uncert.graph dataframe

valles.uncert.graph <- merge(valles.inc.graph, valles.allom.graph, all.x=T, all.y=T)
dim(valles.uncert.graph)

valles.uncert.graph <- merge(valles.uncert.graph, valles.dens.graph, all.x=T, all.y=T)

valles.uncert.graph <- merge(valles.uncert.graph, valles.mort.graph, all.x=T, all.y=T)
summary(valles.uncert.graph)

# Adding in the base increment

valles.uncert.graph <- merge(valles.base, valles.uncert.graph, all.x=T, all.y=T)
summary(valles.uncert.graph)

cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

pdf("figures/stacked_inc_uncertainties.pdf", width=13, height=8.5)
ggplot(valles.uncert.graph) + facet_grid(Site ~ .) +
  geom_line(aes(x=Year, y=base), size=1.5, color="black") +
  
  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=base - LB.inc, ymax= base + UB.inc, fill="1"), alpha=0.9) +
  
  #2) Allometric Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=base - LB.allom, ymax= base - LB.inc, fill="2"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=base + UB.allom, ymax=base + UB.inc, fill="2"), alpha=0.9) +
  
  #3) Density Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin= base - LB.dens, ymax= base - LB.allom, fill="3"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=base + UB.dens, ymax=base + UB.allom, fill="3"), alpha=0.9) +
  
  #4) Mortality Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin= base - LB.mort, ymax= base - LB.dens, fill="4"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=base + UB.mort, ymax=base + UB.dens, fill="4"), alpha=0.9) +
  
    
  # Reiterate mean line for clarity
  geom_line(aes(x=Year, y=base), size=1.5, color="black") +
  
  # add time slice lines
  #geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted") +
  
  # Legend Formatting
  labs(title= "Quad calculated Stacked Uncertainty Increment", x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2","yr"^"-1",")")))) +
  scale_fill_manual(name="Uncertainty", values=cbbPalette, labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
  #  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.2,0.85)) + 
  
  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))

dev.off()


#---------------------------------------------------------------------
# Needing to look at the relative contributions of each component relative to the overall uncertainty

summary(valles.all.uncert)

plot(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$type=="total", "range"] ~valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$type=="total", "range"])

valles.percents <- data.frame(Year = valles.all.uncert$Year,
							  type = valles.all.uncert$type,
							  Site = valles.all.uncert$Site)

for(i in valles.percents$Site){
	for(j in min(valles.percents$Year):max(valles.percents$Year)){
		total <- valles.all.uncert[valles.all.uncert$Site==i & valles.all.uncert$Year==j & valles.all.uncert$type=="total", "range"]
		total2 <- sum(valles.all.uncert[valles.all.uncert$Site==i & valles.all.uncert$Year==j & !valles.all.uncert$type=="total", "range"])
		for(t in unique(valles.percents$type)){
			valles.percents[valles.percents$Site==i & valles.percents$Year==j & valles.percents$type==t, "perc.uncert"] <- valles.all.uncert[valles.all.uncert$Site==i & valles.all.uncert$Year==j & valles.all.uncert$type==t, "range"] / total
			valles.percents[valles.percents$Site==i & valles.percents$Year==j & valles.percents$type==t, "perc.uncert.parts"] <- valles.all.uncert[valles.all.uncert$Site==i & valles.all.uncert$Year==j & valles.all.uncert$type==t, "range"] / total2
		}
	}
}
summary(valles.percents)

valles.percents$Site <- factor(valles.percents$Site, levels=c("VUF", "VLF"))
ggplot(data=valles.percents[!valles.percents$type=="total",])+ facet_grid(Site~.) +
	geom_line(aes(x=as.numeric(Year), y=perc.uncert.parts, color=type))

ggplot(data=valles.range[valles.range$type %in% c("total", "inc"),])+ facet_grid(site~.) +
	geom_line(aes(x=as.numeric(Year), y=range, color=type))



