##############################################################################
# Script calculating the biomass increment per tree & finding the uncertainty around that increment
# because we use the mean tree response to do our biomass reconstruction
##############################################################################
library(ggplot2)
library(grid)

# -----------------------------------
# loading in & formatting the various datasets that will be needed
# -----------------------------------
# Read in the tree-based BM estimates
load("processed_data/Biomass_Array_Tree_kgm-2.RData") # loads object bm.array
dimnames(bm.array)

# Subset only the valles and take the mean BM estimate for each tree
bm.valles <- bm.array[,substr(dimnames(bm.array)[[2]],1,1)=="V",]
bm.valles <- apply(bm.valles, c(1,2), mean, na.rm=T)
dim(bm.valles)
summary(bm.valles[,1:10])
# -----------------------------------


# -----------------------------------
# Translating total biomass to biomass increment per tree
# NOTE: This is looking at dated vs. non-dated; it's all trees that go into calculating 
# total plot biomass estimate
# -----------------------------------
# setting up a blank data frame in which to insert the increment
bm.increment <- data.frame(array(dim=dim(bm.valles)))
row.names(bm.increment) <- dimnames(bm.valles)[[1]]
names(bm.increment)     <- dimnames(bm.valles)[[2]]

# Doing the increment calculation
for(j in 1:ncol(bm.increment)){
  # 1) inserting oldest biomass (right now that's at the bottom or the array)
  bm.increment[nrow(bm.increment),j] <- bm.valles[nrow(bm.valles),j]
  for(i in (nrow(bm.increment)-1):1){ 
  # 2) subtracting the previous year's growth from current biomass to get that year's biomass increment
  #    NOTE: this doesn't work if there are NAs in either year we're interested in, so lets just skip those
	if(!is.na(bm.valles[i,j]) & !is.na(bm.valles[i+1,j])){ 
		bm.increment[i,j] <- bm.valles[i,j] - bm.valles[i+1,j]
	}
  }
}
# -----------------------------------

# -----------------------------------
# Calculating mean & CI, graphing
# -----------------------------------
# Subsetting & summarizing by site (ignores plot)
increment.vlf <- data.frame(SiteID="VLF", Year=as.numeric(row.names(bm.increment)),
							Inc.Mean=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VL")], 1, mean, na.rm=T),
							Inc.CI.lo=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VL")], 1, quantile, 0.025, na.rm=T),
							Inc.CI.hi=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VL")], 1, quantile, 0.975, na.rm=T)
							)

increment.vuf <- data.frame(SiteID="VUF", Year=as.numeric(row.names(bm.increment)),
							Inc.Mean=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VU")], 1, mean, na.rm=T),
							Inc.CI.lo=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VU")], 1, quantile, 0.025, na.rm=T),
							Inc.CI.hi=apply(bm.increment[,which(substr(names(bm.increment),1,2)=="VU")], 1, quantile, 0.975, na.rm=T)
							)
uncert.increment <- rbind(increment.vlf, increment.vuf)
summary(uncert.increment)

#uncert.increment <- data.frame(rbind(ci.vu, ci.vl))
uncert.increment$Site <- recode(uncert.increment$SiteID, "'VUF'='1';'VLF'='2'")
levels(uncert.increment$Site) <- c("Upper", "Lower")
summary(uncert.increment)

poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=21), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30))

# Poster Graph
# pdf("figures/Uncertainty_Increment_TimeSeries.pdf", width = 13, height= 8.5)
# ggplot(data=uncert.increment) + facet_grid(Site~., scales="fixed") +
	# geom_ribbon(aes(x=Year, ymin=Inc.CI.lo, ymax=Inc.CI.hi, fill=Site), alpha=0.3) +
	# geom_line(aes(x=Year, y=Inc.Mean, color=Site), size=1.5) +
	# labs(title= "Biomass increment", x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2"," yr" ^"-1",")")))) +
	# #theme_bw()+
  # theme(axis.ticks.length = unit(-0.25, "cm"),
        # axis.ticks.margin = unit(0.5, "cm")) +
  # # add time slice lines
  # geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) +
  # poster.theme

# dev.off()

# Publication Graph
pdf("figures/Uncertainty_Increment_TimeSeries.pdf", width = 13, height= 8.5)
ggplot(data=uncert.increment) + facet_grid(Site~., scales="fixed") +
	geom_ribbon(aes(x=Year, ymin=Inc.CI.lo, ymax=Inc.CI.hi, fill=Site), alpha=0.3) +
	geom_line(aes(x=Year, y=Inc.Mean, color=Site), size=1.5) +
	labs(title= "Biomass increment", x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2"," yr" ^"-1",")"))) +
	#theme_bw()+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm")) +
  # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1) +
  
  # General Formatting
    
	theme(legend.position=c(0.15,0.85)) + 
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  theme(strip.text=element_text(size=rel(1.5)))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm"))
dev.off()        

save(uncert.increment, file="processed_data/valles_increment_uncertainty.Rdata")

# -----------------------------------


# -----------------------------------
# Statistics
# -----------------------------------
# -----------------------------------
