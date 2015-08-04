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
library(ggplot2)
library(car)

# Read in the full MCMC-based indivudal tree biomass estimates
load("processed_data/Plot_Biomass_Array.Rdata") # loads plot-level bm.array
# ------------------------

# ------------------------
# Subset Valles, take mean of allometric iterations, & figure out plot (density)-based 95% CI
# Note: the CIs here will be rather weird since we only have 2 plots with cores for each site
# ------------------------
# Subsetting and using the mean allometric biomass estimate
biom.valles <- data.frame(apply(bm.array[,which(substr(dimnames(bm.array)[[2]],1,1)=="V"),], c(1,2), mean))
biom.valles$Year <- as.numeric(dimnames(bm.array)[[1]])
summary(biom.valles)

cols.vu <- which(substr(names(biom.valles),1,2)=="VU") # creating an index for VUF
ci.vu <- data.frame(Year=biom.valles$Year, SiteID= "VUF",
                    Mean = apply(biom.valles[,cols.vu], 1, mean, na.rm=T),
                    LB   = apply(biom.valles[,cols.vu], 1, quantile, 0.025, na.rm=T), 
                    UB   = apply(biom.valles[,cols.vu], 1, quantile, 0.975, na.rm=T))

cols.vl <- which(substr(names(biom.valles),1,2)=="VL") # creating an index for VLF
ci.vl <- data.frame(Year=biom.valles$Year, SiteID= "VLF",
                    Mean = apply(biom.valles[,cols.vl], 1, mean, na.rm=T),
                    LB   = apply(biom.valles[,cols.vl], 1, quantile, 0.025, na.rm=T), 
                    UB   = apply(biom.valles[,cols.vl], 1, quantile, 0.975, na.rm=T))
# ------------------------


# ------------------------
# Package everything together, make a quick plot and save it for later use
# This is what goes to the stacked uncertainty figure
# ------------------------
dens.uncert <- data.frame(rbind(ci.vu, ci.vl))
dens.uncert$Site <- recode(dens.uncert$SiteID, "'VUF'='1';'VLF'='2'")
levels(dens.uncert$Site) <- c("Upper", "Lower")
summary(dens.uncert)
poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=21), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30))

pdf("figures/Uncertainty_Density_TimeSeries.pdf", height= 8.5, width = 13)
ggplot(dens.uncert[,]) + #facet_grid(Site ~.) +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Density Uncertainty") + 
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm")) +
  # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) +
  poster.theme1
dev.off()

save(dens.uncert, file="processed_data/valles_density_uncertainty.Rdata")
# ------------------------

# ------------------------
# Statistics on Density Uncertainty:
# ------------------------

# ------------------------
