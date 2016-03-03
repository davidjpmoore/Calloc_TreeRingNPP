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

# We want to figure out how each run of the allometries differs from the base increment
# VUF
vuf.allom.inc2 <- data.frame(apply(vuf.allom.inc, c(1,3), mean, na.rm=T))

par(new=F)
for(i in 1:100){
    plot(vuf.allom.inc2[,i] ~ dimnames(vuf.allom.inc2)[[1]], type="l", ylim=c(0, max(vuf.allom.inc2)))
    par(new=T)
}
lines(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc2)[[1]], type="l", col="red", lwd=3)


vuf.allom.inc.dev <- vuf.allom.inc2
vuf.allom.inc.dev[,] <- NA

vuf.allom.inc.dev <- vuf.allom.inc2[,] -  apply(vuf.allom.inc2,1, FUN=mean)  

dim(vuf.allom.inc.dev)
dimnames(vuf.allom.inc.dev)
summary(vuf.allom.inc.dev[,1:10])

par(new=F)
for(i in 1:100){
  plot(valles.base.inc$vuf.base + vuf.allom.inc.dev[,i] ~ dimnames(vuf.allom.inc.dev)[[1]], type="l", ylim=range(vuf.allom.inc.dev))
  par(new=T)
}
lines(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc2)[[1]], type="l", col="red", lwd=3)

# VLF
vlf.allom.inc2 <- data.frame(apply(vlf.allom.inc, c(1,3), mean, na.rm=T))
dim(vlf.allom.inc2)


vlf.allom.inc.dev <- vlf.allom.inc2
vlf.allom.inc.dev[,] <- NA

vlf.allom.inc.dev <- vlf.allom.inc2[,] -  apply(vlf.allom.inc2,1, FUN=mean)  

dim(vlf.allom.inc.dev)
dimnames(vlf.allom.inc.dev)
summary(vlf.allom.inc.dev[,1:10])


par(new=F)
for(i in 1:100){
  plot(valles.base.inc + vlf.allom.inc.dev[,i] ~ dimnames(vlf.allom.inc.dev)[[1]], type="l", ylim=range(vlf.allom.inc.dev))
  par(new=T)
}
lines(valles.base.inc$vlf.base ~ dimnames(vlf.allom.inc2)[[1]], type="l", col="red", lwd=3)


#---------------------------------------------------
# Density Deviations
#---------------------------------------------------

dim(dens.inc)
summary(dens.inc)
head(dens.inc)

vuf.dens <- dens.inc[,substr(names(dens.inc), 1,2)=="VU"]
summary(vuf.dens)

vuf.dens.dev <- vuf.dens - apply(vuf.dens, 1, FUN=mean, na.rm=T)
summary(vuf.dens.dev)
head(vuf.dens.dev)

par(new=F)
for(i in 1:2){
  plot(valles.base.inc$vuf.base + vuf.dens.dev[,i] ~ row.names(vuf.dens.dev), type="l", ylim=range(valles.base.inc$vuf.base + vuf.dens.dev, na.rm=T))
  par(new=T)
}
lines(valles.base.inc$vuf.base ~ row.names(vuf.dens.dev), type="l", col="red", lwd=3)



vlf.dens <- dens.inc[,substr(names(dens.inc), 1,2)=="VL"]
summary(vlf.dens)

vlf.dens.dev <- vlf.dens - apply(vlf.dens, 1, FUN=mean, na.rm=T)
summary(vlf.dens.dev)
head(vlf.dens.dev)

par(new=F)
for(i in 1:2){
  plot(valles.base.inc$vlf.base + vlf.dens.dev[,i] ~ row.names(vlf.dens.dev), type="l", ylim=range(valles.base.inc$vlf.base + vlf.dens.dev, na.rm=T))
  par(new=T)
}
lines(valles.base.inc$vlf.base ~ dimnames(vlf.allom.inc2)[[1]], type="l", col="red", lwd=3)

#---------------------------------------------------
# Increment Deviations
#---------------------------------------------------

dim(vuf.inc)
summary(vuf.inc)
head(vuf.inc)


vuf.inc.dev <- vuf.inc - apply(vuf.inc, 1, FUN=mean, na.rm=T)
summary(vuf.inc.dev)

dim(vlf.inc)
summary(vlf.inc)
head(vlf.inc)

vlf.inc.dev <- vlf.inc - apply(vlf.inc, 1, FUN=mean, na.rm=T)
summary(vlf.inc.dev)

par(new=F)
for(i in 1:100){
  plot(valles.base.inc$vlf.base + vlf.inc.dev[,i] ~ row.names(vlf.inc.dev), type="l", ylim=range(valles.base.inc$vlf.base + vlf.inc.dev, na.rm=T))
  par(new=T)
}
lines(valles.base.inc$vlf.base ~ dimnames(vlf.allom.inc2)[[1]], type="l", col="red", lwd=3)

#---------------------------------------------------
# Mortality Deviations
#---------------------------------------------------

dim(vuf.mort.inc)
summary(vuf.mort.inc)
row.names(vuf.mort.inc)

# Adding a row of NA's for 1904 so the dimensions align
vuf.mort.inc <- rbind(vuf.mort.inc, NA) 
row.names(vuf.mort.inc)<- c(2011:1904)




vuf.mort.dev <- vuf.mort.inc[,] - apply(vuf.mort.inc, 1, FUN=mean, na.rm=T)
summary(vuf.mort.dev)
vuf.mort.dev


dim(vlf.mort.inc)
summary(vlf.mort.inc)
row.names(vlf.mort.inc)
# Adding a row of NA's for 1904 so dimensions align.
vlf.mort.inc <- rbind(vlf.mort.inc, NA)
row.names(vlf.mort.inc) <- c(2011:1904)


vlf.mort.dev <- vlf.mort.inc[,] - apply(vlf.mort.inc, 1, FUN=mean, na.rm=T)
summary(vlf.mort.dev)

#---------------------------------------------------
# Adding up in quadrature the deviations from the different areas while bootstrapping
#---------------------------------------------------
n.pulls=100
set.seed(1117)
# VUF Site

vuf.inc.tot <- sqrt(vuf.allom.inc.dev[,sample(1:ncol(vuf.allom.inc.dev), n.pulls, replace=T)]^2 + 
                                               vuf.dens.dev[,sample(1:ncol(vuf.dens.dev), n.pulls, replace=T)]^2 + 
                                               vuf.inc.dev[,sample(1:ncol(vuf.inc.dev), n.pulls, replace=T)]^2 +
                                               vuf.mort.dev[,sample(1:ncol(vuf.mort.dev), n.pulls, replace=T)]^2)

dim(vuf.inc.tot)
summary(vuf.inc.tot)



par(new=F)
for(i in 1:ncol(vuf.inc.tot)){
		plot(vuf.inc.tot[,i] ~ row.names(vuf.inc.tot), type="l", ylim=c(0,10))
		par(new=T)
}
lines	(valles.base.inc$vuf.base ~ dimnames(vuf.allom.inc)[[1]], type="l", ylim=c(0,10), col="red", lwd=3)

save(vuf.inc.tot, file="processed_data/vuf_bm_boot_tot_inc.Rdata")
# VLF Site

vlf.inc.tot <- sqrt(vlf.allom.inc.dev[,sample(1:ncol(vlf.allom.inc.dev), n.pulls, replace=T)]^2 + 
				            vlf.dens.dev     [,sample(1:ncol(vlf.dens.dev     ), n.pulls, replace=T)]^2 + 
				            vlf.inc.dev      [,sample(1:ncol(vlf.inc.dev      ), n.pulls, replace=T)]^2 + 
				            vlf.mort.dev     [,sample(1:ncol(vlf.mort.dev     ), n.pulls, replace=T)]^2
				              )

dim(vlf.inc.tot)
summary(vlf.inc.tot)

par(new=F)
for(i in 1:ncol(vlf.inc.tot)){
		plot(vlf.inc.tot[,i] ~ row.names(vlf.inc.tot), type="l", ylim=c(0,10))
		par(new=T)
}
lines	(valles.base.inc$vlf.base ~ dimnames(vlf.allom.inc)[[1]], type="l", ylim=c(0,10), col="red", lwd=3)

save(vlf.inc.tot, file="processed_data/vlf_bm_boot_tot_inc.Rdata")




#--------------------------------------------------------------------
################################################
# Graphing the cumulative increment uncertainty
################################################

#---------------
# Upper Site VUF
#---------------

summary(vuf.inc.tot)
dim(vuf.inc.tot)
row.names(vuf.inc.tot)

vuf.inc.tot.graph <- data.frame(year = as.numeric(row.names(vuf.inc.tot)), 
                                mean = apply(vuf.inc.tot, 1,FUN=mean, na.rm=T), 
                                ci.lo = apply(vuf.inc.tot, 1, FUN=quantile, 0.025, na.rm=T), 
                                ci.hi = apply(vuf.inc.tot, 1, FUN=quantile, 0.975, na.rm=T),
                                base = valles.base.inc$vuf.base)

vuf.inc.tot.graph$site <- as.factor("VUF")
summary(vuf.inc.tot.graph)                                

vlf.inc.tot.graph <- data.frame(year = as.numeric(row.names(vlf.inc.tot)), 
                                mean = apply(vlf.inc.tot, 1,FUN=mean, na.rm=T), 
                                ci.lo = apply(vlf.inc.tot, 1, FUN=quantile, 0.025, na.rm=T), 
                                ci.hi = apply(vlf.inc.tot, 1, FUN=quantile, 0.975, na.rm=T),
                                base = valles.base.inc$vlf.base)

vlf.inc.tot.graph$site <- as.factor("VLF")
summary(vlf.inc.tot.graph)                                

valles.inc.tot <- rbind(vuf.inc.tot.graph, vlf.inc.tot.graph)
summary(valles.inc.tot)

pdf("figures/bm_inc_uncert_quad.pdf", width=13, height=8.5)
ggplot(data=valles.inc.tot) + facet_grid(site ~.) +
	
	geom_ribbon(aes(x=year, ymin= base - ci.lo, ymax= base + ci.hi), fill="darkgrey", alpha=0.6) +
	
	geom_line(aes(x=year, y=base), size=1.5, color="black") +
  #geom_line(aes(x=year, y=mean), size=1.5, color="black") +
	labs(title= "Biomass Increment Total Uncertainty", x="Year", y=expression(bold(paste("Biomass (kg/m2)")))) +
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))


dev.off()     

################################################################################################
# Making a dataframe that has eacha area of uncertainty identified individaully (similar to graph produced in script #10) so that we can see the contribution of individual areas ot the overall uncertainty
################################################################################################
n.pulls=100
set.seed(1117)
# vuf.inc.tot <- valles.base.inc$vuf.base + sqrt(vuf.allom.inc.dev[,sample(1:ncol(vuf.allom.inc.dev), n.pulls, replace=T)]^2 + 
#                                                  vuf.dens.dev[,sample(1:ncol(vuf.dens.dev), n.pulls, replace=T)]^2 + 
#                                                  vuf.inc.dev[,sample(1:ncol(vuf.inc.dev), n.pulls, replace=T)]^2 +
#                                                  vuf.mort.dev[,sample(1:ncol(vuf.mort.dev), n.pulls, replace=T)]^2)




set.seed(1117)
cols.inc.dev   <- sample(1:ncol(vuf.inc.dev), n.pulls, replace=T)
cols.allom.dev <- sample(1:ncol(vuf.allom.inc.dev), n.pulls, replace=T)
cols.dens.dev  <- sample(1:ncol(vuf.dens.dev), n.pulls, replace=T)
cols.mort.dev  <- sample(1:ncol(vuf.mort.dev), n.pulls, replace=T)

# TR Increment
set.seed(1117)

range(vuf.inc.dev)

vuf.tr.uncert <- sqrt(vuf.inc.dev[,cols.inc.dev]^2)
set.seed(1117)
vlf.tr.uncert <- sqrt(vlf.inc.dev[,cols.inc.dev]^2)


# Allometry
set.seed(1117)
vuf.allom.uncert <- sqrt(vuf.inc.dev[,cols.inc.dev]^2 + 
                         vuf.allom.inc.dev[,cols.allom.dev]^2)
set.seed(1117)
vlf.allom.uncert <- sqrt(vlf.inc.dev[,cols.inc.dev]^2 + 
                                                    vlf.allom.inc.dev[,cols.allom.dev]^2)

# Density
set.seed(1117)
vuf.dens.uncert <- sqrt(vuf.inc.dev[,cols.inc.dev]^2 + 
                                                   vuf.allom.inc.dev[,cols.allom.dev]^2 + 
                                                   vuf.dens.dev[,cols.dens.dev]^2)
set.seed(1117)
vlf.dens.uncert <- sqrt(vlf.inc.dev[,cols.inc.dev]^2 + 
                                                  vlf.allom.inc.dev[,cols.allom.dev]^2 + 
                                                  vlf.dens.dev[,cols.dens.dev]^2)

# Mortality
set.seed(1117)
vuf.mort.uncert <- sqrt(vuf.inc.dev[,cols.inc.dev]^2 + 
                                                   vuf.allom.inc.dev[,cols.allom.dev]^2 + 
                                                   vuf.dens.dev[,cols.dens.dev]^2 +
                                                   vuf.mort.dev[,cols.mort.dev]^2)
set.seed(1117)
vlf.mort.uncert <- sqrt(vlf.inc.dev[,cols.inc.dev]^2 + 
                                                    vlf.allom.inc.dev[,cols.allom.dev]^2 + 
                                                     vlf.dens.dev[,cols.dens.dev]^2 + 
                                                     vlf.mort.dev[,cols.mort.dev]^2)



# Adding different areas of uncertainty together for graphing purposes
vuf.uncert.graph <- data.frame(Year=valles.base.inc$Year,
                               Base = valles.base.inc$vuf.base,
                               LB.inc = apply(vuf.tr.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.allom = apply(vuf.allom.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.dens = apply(vuf.dens.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.mort = apply(vuf.mort.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               UB.inc = apply(vuf.tr.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.allom = apply(vuf.allom.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.dens = apply(vuf.dens.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.mort = apply(vuf.mort.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               Site = "VUF")
summary(vuf.uncert.graph)


vlf.uncert.graph <- data.frame(Year=valles.base.inc$Year,
                               Base = valles.base.inc$vlf.base,
                               LB.inc = apply(vlf.tr.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.allom = apply(vlf.allom.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.dens = apply(vlf.dens.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               LB.mort = apply(vlf.mort.uncert, 1, FUN=quantile, 0.025, na.rm=T),
                               UB.inc = apply(vlf.tr.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.allom = apply(vlf.allom.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.dens = apply(vlf.dens.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               UB.mort = apply(vlf.mort.uncert, 1, FUN=quantile, 0.975, na.rm=T),
                               Site = "VLF")
summary(vlf.uncert.graph)

# Merging two dataframes together to make it easier to graph
valles.uncert.graph <- merge(vuf.uncert.graph, vlf.uncert.graph, all.x=T, all.y=T)

summary(valles.uncert.graph)

cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")


pdf("figures/stacked_inc_uncertainties.pdf", width=13, height=8.5)
ggplot(valles.uncert.graph[valles.uncert.graph$Year >= 1900 & valles.uncert.graph$Year <=2011,]) + facet_grid(Site ~ .) +
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +
  
  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=Base - LB.inc, ymax= Base + UB.inc, fill="1"), alpha=0.9) +
  
  #2) Allometric Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=Base - LB.allom, ymax= Base - LB.inc, fill="2"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.allom, ymax=Base + UB.inc, fill="2"), alpha=0.9) +
  
  #3) Density Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin= Base - LB.dens, ymax= Base - LB.allom, fill="3"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.dens, ymax=Base + UB.allom, fill="3"), alpha=0.9) +
  
  #4) Mortality Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin= Base - LB.mort, ymax= Base - LB.dens, fill="4"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.mort, ymax=Base + UB.dens, fill="4"), alpha=0.9) +
  
    
  # Reiterate mean line for clarity
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +
  
  # add time slice lines
  #geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted") +
  
  # Legend Formatting
  labs(title= "Quad calculated Stacked Uncertainty Increment", x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")")))) +
  scale_fill_manual(name="Uncertainty", values=cbbPalette, labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
  #  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.2,0.85)) + 
  
  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))

dev.off()


################################################################################################
# Breaking down and looking at each area of uncertainty andn using in correlations in the next script; Still using 100 pulls.
################################################################################################

# Increment only
set.seed(1117)
vuf.inc.only <- valles.base.inc$vuf.base + vuf.inc.dev[,sample(1:ncol(vuf.inc.dev), n.pulls,   				replace=T)] 

dim(vuf.inc.only)
summary(vuf.inc.only)
save(vuf.inc.only, file="processed_data/vuf_bm_inc_only.Rdata")

set.seed(1117)
vlf.inc.only <- valles.base.inc$vlf.base + vlf.inc.dev[,sample(1:ncol(vlf.inc.dev), n.pulls, 					replace=T)] 

dim(vlf.inc.only)
summary(vlf.inc.only)
save(vlf.inc.only, file="processed_data/vlf_bm_inc_only.Rdata")
#--------------------------------------------------------------------

# Allometry only
set.seed(1117)
vuf.allom.only <- valles.base.inc$vuf.base + vuf.allom.inc.dev[,sample(1:ncol(vuf.allom.inc.dev), 					n.pulls, replace=T)]

dim(vuf.allom.only)
summary(vuf.allom.only)
save(vuf.allom.only, file="processed_data/vuf_bm_allom_only.Rdata")

set.seed(1117)
vlf.allom.only <- valles.base.inc$vlf.base + vlf.allom.inc.dev[,sample(1:ncol(vlf.allom.inc.dev), 					n.pulls, replace=T)] 

dim(vlf.allom.only)
summary(vlf.allom.only)
save(vlf.allom.only, file="processed_data/vlf_bm_allom_only.Rdata")
#--------------------------------------------------------------------

# Density only 
set.seed(1117)
vuf.dens.only <- valles.base.inc$vuf.base + vuf.dens.dev[,sample(1:ncol(vuf.dens.dev), n.pulls, 					replace=T)]
dim(vuf.dens.only)
summary(vuf.dens.only)
save(vuf.dens.only, file="processed_data/vuf_bm_dens_only.Rdata")

set.seed(1117)
vlf.dens.only <- valles.base.inc$vlf.base + vlf.dens.dev[,sample(1:ncol(vlf.dens.dev), n.pulls, 					replace=T)]
dim(vlf.dens.only)
summary(vlf.dens.only)
save(vlf.dens.only, file="processed_data/vlf_bm_dens_only.Rdata")
#--------------------------------------------------------------------

# Mortality Only
set.seed(1117)
vuf.mort.only <- valles.base.inc$vuf.base + vuf.mort.dev[,sample(1:ncol(vuf.mort.dev), n.pulls, 					replace=T)]
dim(vuf.mort.only)
summary(vuf.mort.only)
save(vuf.mort.only, file="processed_data/vuf_bm_mort_only.Rdata")


set.seed(1117)
vlf.mort.only <- valles.base.inc$vlf.base + vlf.mort.dev[,sample(1:ncol(vlf.mort.dev), n.pulls, 					replace=T)]
dim(vlf.mort.only)
summary(vlf.mort.only)
save(vlf.mort.only, file="processed_data/vlf_bm_mort_only.Rdata")
