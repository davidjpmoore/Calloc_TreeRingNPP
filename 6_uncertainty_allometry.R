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
library(grid)
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
# Subsetting and using the mean allometric biomass estimate to get a biomass/tree
biom.valles <- bm.array[,which(substr(dimnames(bm.array)[[2]],1,1)=="V"),]
# biom.valles$Year <- as.numeric(dimnames(bm.array)[[1]])
dim(biom.valles)
# summary(biom.valles)

# Going from trees to plots (ignoring any tree-level plot level)
# Note: Using a dummy code here so that we bassically have plot #0 and Plot #1
plots <- unique(substr(dimnames(biom.valles)[[2]], 1, 4))
plots <- plots[!plots == "Year"]

# Go from tree biomass (kg/m2) to plot (kg/m2)
biom.plot <- array(dim=c(nrow(biom.valles), length(plots), dim(biom.valles)[3]))
dimnames(biom.plot)[[1]] <- dimnames(biom.valles)[[1]]
dimnames(biom.plot)[[2]] <- c(plots)
dim(biom.plot)

for(p in plots){
	cols.plot <- which(substr(dimnames(biom.valles)[[2]],1,4)==p)
	biom.plot[,p,] <- apply(biom.valles[,cols.plot,], c(1,3), FUN=mean)
}
summary(biom.plot[,,1])

# Go from plot to site while preserving the allometry iterations (currently dim #3)
vlf.mean <- apply(biom.plot[,substr(dimnames(biom.plot)[[2]],1,3)=="VLF",], c(1,3), mean) 
dim(vlf.mean)
summary(vlf.mean[,1:10])
row.names(vlf.mean)

vuf.mean <- apply(biom.plot[,substr(dimnames(biom.plot)[[2]],1,3)=="VUF",], c(1,3), mean) 
dim(vuf.mean)
summary(vuf.mean[,1:10])
row.names(vuf.mean)
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

# Poster Format
# pdf("figures/Uncertainty_Allometry.pdf", width= 13, height= 8.5)
# ggplot(allom.uncert[,]) + #facet_grid(Site ~.) +
  # geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  # geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  # labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Allometric Uncertainty") + 
  # #theme_bw()
  # theme(axis.ticks.length = unit(-0.25, "cm"),
        # axis.ticks.margin = unit(0.5, "cm")) +
  # # add time slice lines
  # geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) # +
  # # poster.theme1
# dev.off()

# Publication Format
pdf("figures/Uncertainty_Allometry.pdf", width= 13, height= 8.5)
ggplot(allom.uncert[,]) + #facet_grid(Site ~.) +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  labs(x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")")), title="Allometric Uncertainty") + 
    # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1) +
  
	# General Formatting  
	theme(legend.position=c(0.15,0.85)) + 
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5)))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm"))
  
  #poster.theme1
dev.off()

save(allom.uncert, file="processed_data/valles_allometry_uncertainty.Rdata")
# ------------------------


# ------------------------
# Statistics on Allometric Uncertainty:
# ------------------------

# ------------------------
