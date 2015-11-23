# -----------------------------------------------------------
# Quantifying the uncertainty in biomass reconstruction from uncertainty in allometric equations 
#   for only the Valles Caldera
# Cleaned up from https://github.com/alexanderm10/Tree-Rings-and-Biomass/blob/master/Uncertainty_analysis/post_GF_TRW_to_biomass_CR.R
# 23 June, 2015
# -----------------------------------------------------------

# ------------------------
# Load libraries & read in data
# ------------------------
# Libraries:
library(ggplot2); library(grid)
library(car)

# Read in the full plot-based biomass estimates
load("processed_data/Biomass_Array_Tree_kgm-2.RData") # loads plot-level bm.array
# ------------------------

# ------------------------
# Subset Valles, take mean of allometric iterations, & figure out plot (density)-based 95% CI
# Note: the CIs here will be rather weird since we only have 2 plots with cores for each site
# ------------------------
# Subsetting and using the mean allometric biomass estimate to get a biomass/tree
biom.valles <- data.frame(apply(bm.array[,which(substr(dimnames(bm.array)[[2]],1,1)=="V" | substr(dimnames(bm.array)[[2]],1,1)=="M"),], c(1,2), mean))
biom.valles$Year <- as.numeric(dimnames(bm.array)[[1]])
summary(biom.valles)

# Going from trees to plots (ignoring any tree-level plot level)
# Note: Using a dummy code here so that we bassically have plot #0 and Plot #1
plots <- unique(substr(names(biom.valles), 1, 4))
plots <- plots[!plots == "Year"]


# Go from tree biomass (kg/m2) to plot (kg/m2)
biom.plot <- data.frame(array(dim=c(nrow(bm.array), length(plots)+1)))
names(biom.plot) <- c("Year", plots)
biom.plot$Year <- biom.valles$Year
summary(biom.plot)

for(p in plots){
	cols.plot <- which(substr(names(biom.valles),1,4)==p)
	if(substr(p, 1, 1) == "V"){
		biom.plot[,p] <- apply(biom.valles[,cols.plot], 1, FUN=mean)
		} else{ biom.plot[,p] <- apply(biom.valles[,cols.plot], 1, FUN=sum)
}
}
summary(biom.plot)

#  Plot to Site, get CI from differences among plots
cols.vu <- which(substr(names(biom.plot),1,2)=="VU") # creating an index for VUF
ci.vu <- data.frame(Year= biom.plot$Year, SiteID= "VUF",
                    Mean = apply(biom.plot[,cols.vu], 1, mean, na.rm=T),
                    LB   = apply(biom.plot[,cols.vu], 1, quantile, 0.025, na.rm=T), 
                    UB   = apply(biom.plot[,cols.vu], 1, quantile, 0.975, na.rm=T))

cols.vl <- which(substr(names(biom.plot),1,2)=="VL") # creating an index for VLF
ci.vl <- data.frame(Year= biom.plot$Year, SiteID= "VLF",
                    Mean = apply(biom.plot[,cols.vl], 1, mean, na.rm=T),
                    LB   = apply(biom.plot[,cols.vl], 1, quantile, 0.025, na.rm=T), 
                    UB   = apply(biom.plot[,cols.vl], 1, quantile, 0.975, na.rm=T))
                    
cols.mm <- which(substr(names(biom.plot),1,2)=="MM") # creating an index for MMF
ci.mm <- data.frame(Year= biom.plot$Year, SiteID= "MMF",
                    Mean = apply(biom.plot[,cols.mm], 1, mean, na.rm=T),
                    LB   = apply(biom.plot[,cols.mm], 1, quantile, 0.025, na.rm=T), 
                    UB   = apply(biom.plot[,cols.mm], 1, quantile, 0.975, na.rm=T))

# ------------------------


# ------------------------
# Package everything together, make a quick plot and save it for later use
# This is what goes to the stacked uncertainty figure
# ------------------------
dens.uncert <- data.frame(rbind(ci.vu, ci.vl, ci.mm))
dens.uncert$Site <- recode(dens.uncert$SiteID, "'VUF'='1';'VLF'='2'; 'MMF' = '3'")
levels(dens.uncert$Site) <- c("Valles Upper", "Valles Lower", "Morgan-Monroe")
summary(dens.uncert)
poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=21), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30))
# Poster Figure
# pdf("figures/Uncertainty_Density_TimeSeries.pdf", height= 8.5, width = 13)
# ggplot(dens.uncert[,]) + #facet_grid(Site ~.) +
  # geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  # geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  # labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Density Uncertainty") + 
  # theme(axis.ticks.length = unit(-0.25, "cm"),
        # axis.ticks.margin = unit(0.5, "cm")) +
  # # add time slice lines
  # geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) +
  # poster.theme
# dev.off()

# Publication Figure
#pdf("figures/Uncertainty_Density_TimeSeries.pdf", height= 8.5, width = 13)
ggplot(dens.uncert[,]) + #facet_grid(Site ~.) +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  labs(x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")")), title="Density Uncertainty") + 
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm")) +
  # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1) +
  
  # General Formatting
  	# General Formatting  
	theme(legend.position=c(0.15,0.85)) + 
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5)))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm"))
#dev.off()


save(dens.uncert, file="processed_data/valles_density_uncertainty_AGU2015.Rdata")
# ------------------------

# ------------------------
# Statistics on Density Uncertainty:
# ------------------------

# ------------------------

