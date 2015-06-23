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
load("processed_data/Biomass_Array_Tree_kgm-2.RData")
# ------------------------

# ------------------------
# Get to site level by taking mean of trees (ignoring plots & potential pseudoreplication issues)
#    This mean tree approach is necessary because not all trees in the plot were sampled, so we 
#    can't just sum them
# bm.array==the array we read in above, 
# apply c(1,3) = preserve the dims 1 (years) & 3 (allometric iterations)
# ------------------------
vlf.mean <- apply(bm.array[,substr(dimnames(bm.array)[[2]],1,3)=="VLF",], c(1,3), mean) 
dim(vlf.mean)
summary(vlf.mean[,1:10])

vuf.mean <- apply(bm.array[,substr(dimnames(bm.array)[[2]],1,3)=="VUF",], c(1,3), mean) 
dim(vuf.mean)
summary(vuf.mean[,1:10])
# ------------------------

# ------------------------
# Find the allometry-based uncertainty around the mean of trees
# After running the apply above, allometric iterations are in columns
# Here we're producing a 95% CI around the mean
# ------------------------
vlf.ci <- data.frame(Year=as.numeric(row.names(vlf.mean)), SiteID="VLF", Mean=rowMeans(vlf.mean, na.rm=T), LB=apply(vlf.mean,1,quantile, 0.025, na.rm=T), UB=apply(vlf.mean,1,quantile, 0.975, na.rm=T))
summary(vlf.ci)

vuf.ci <- data.frame(Year=as.numeric(row.names(vuf.mean)), SiteID="VUF", Mean=rowMeans(vuf.mean, na.rm=T), LB=apply(vuf.mean,1,quantile, 0.025, na.rm=T), UB=apply(vuf.mean,1,quantile, 0.975, na.rm=T))
summary(vuf.ci)
# ------------------------

# ------------------------
# Package everything together, make a quick plot and save it for later use
# ------------------------
allom.uncert <- data.frame(rbind(vlf.ci, vuf.ci))
allom.uncert$Site <- recode(allom.uncert$SiteID, "'VUF'='1';'VLF'='2'")
levels(allom.uncert$Site) <- c("Upper", "Lower")
summary(allom.uncert)

pdf("figures/Uncertainty_Allometry.pdf")
ggplot(allom.uncert[,]) + #facet_grid(Site ~.) +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Allometric Uncertainty") + 
  theme_bw()
dev.off()

save(allom.uncert, file="processed_data/valles_allometry_uncertainty.Rdata")
# ------------------------


# ------------------------
# Statistics on Allometric Uncertainty:
# ------------------------

# ------------------------
