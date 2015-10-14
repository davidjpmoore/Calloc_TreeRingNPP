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







# -----------------------------------
#combine the different areas into one figure
# 
# Order of operations
# 1)variability in the tree rings
# 2)allometric uncertainty
# 3)density uncertainty
# 4)mortality adustment
# -----------------------------------


# Creating a dataframe that adds the uncertainties together
# will use the mean from the allometric uncertainty as our root
bm.final <- data.frame(Year=unique(allom.uncert$Year), Site=allom.uncert$Site, 
                        Base=allom.uncert[,"Mean"])
bm.final$LB.inc <- bm.final$Base - uncert.increment[,"LB.dev"]
bm.final$UB.inc <- bm.final$Base + uncert.increment[,"UB.dev"]
bm.final$LB.inc <- ifelse(bm.final$LB.inc<0, 0, bm.final$LB.inc)
bm.final$UB.inc <- ifelse(bm.final$UB.inc<0, 0, bm.final$UB.inc)

bm.final$LB.allom <- bm.final$LB.inc - allom.uncert[, "LB.dev"]
bm.final$UB.allom <- bm.final$UB.inc + allom.uncert[, "UB.dev"]
bm.final$LB.allom <- ifelse(bm.final$LB.allom<0, 0, bm.final$LB.allom)
bm.final$UB.allom <- ifelse(bm.final$UB.allom<0, 0, bm.final$UB.allom)

bm.final$LB.dens <- bm.final$LB.allom - dens.uncert[, "LB.dev"]
bm.final$UB.dens <- bm.final$UB.allom + dens.uncert[, "UB.dev"]
bm.final$LB.dens <- ifelse(bm.final$LB.dens<0, 0, bm.final$LB.dens)
bm.final$UB.dens <- ifelse(bm.final$UB.dens<0, 0, bm.final$UB.dens)

bm.final$LB.mort <- bm.final$LB.dens - uncert.mort[, "LB.dev"]
bm.final$UB.mort <- bm.final$UB.dens + uncert.mort[, "UB.dev"]
bm.final$LB.mort <- ifelse(bm.final$LB.mort<0, 0, bm.final$LB.mort)
bm.final$UB.mort <- ifelse(bm.final$UB.mort<0, 0, bm.final$UB.mort)

summary(bm.final)

summary(bm.final[bm.final$Site=="Lower",])   
summary(bm.final[bm.final$Site=="Upper",])   

# finding the range of the uncertainties
vlf.final <- bm.final[bm.final$Site=="Lower",]
vuf.final <- bm.final[bm.final$Site=="Upper",]

vlf.final$range <- vlf.final$UB.mort - vlf.final$LB.mort
vuf.final$range <- vuf.final$UB.mort - vuf.final$LB.mort

summary(vlf.final)
summary(vuf.final)
# -----------------------------------


# -----------------------------------
# The Figure!
# -----------------------------------
# levels(bm.final$SiteID) <- c("Lower Tower", "Upper Tower")

# png("~/Dropbox/PalEON CR/Tree Rings/Tree-Rings-and-Biomass/Uncertainty_analysis/StackedUncertainties.png", width=600, height=800, units="px")
png("figures/Uncertainty_Stacked.png", width=5, height=6, units="in", res=1200)
ggplot(bm.final[bm.final$Year >= 1925 & bm.final$Year <=2011,]) + facet_grid(Site ~ .) +
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=LB.inc, ymax=UB.inc, fill="1"), alpha=0.6) +

  #2) Allometric Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.allom, ymax=LB.inc, fill="2"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.allom, ymax=UB.inc, fill="2"), alpha=0.6) +
  
  #3) Density Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.dens, ymax=LB.allom, fill="3"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.dens, ymax=UB.allom, fill="3"), alpha=0.6) +
  
  #4) Mortality Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.mort, ymax=LB.dens, fill="4"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.mort, ymax=UB.dens, fill="4"), alpha=0.6) +
  
  # Reiterate mean line for clarity
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted") +
  
  # Legend Formatting
  labs(title= "Stacked Uncertainties", x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")")))) +
  scale_fill_manual(name="Uncertainty", values=c("green3", "blue", "red", "orange2"), labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.2,0.85)) + 

  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()
  

pdf("figures/Uncertainty_Stacked.pdf", width=13, height=8.5)
ggplot(bm.final[bm.final$Year >= 1925 & bm.final$Year <=2011,]) + facet_grid(Site ~ .) +
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=LB.inc, ymax=UB.inc, fill="1"), alpha=0.6) +

  #2) Allometric Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.allom, ymax=LB.inc, fill="2"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.allom, ymax=UB.inc, fill="2"), alpha=0.6) +
  
  #3) Density Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.dens, ymax=LB.allom, fill="3"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.dens, ymax=UB.allom, fill="3"), alpha=0.6) +
  
  #4) Mortality Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=LB.mort, ymax=LB.dens, fill="4"), alpha=0.6) +
  geom_ribbon(aes(x=Year, ymin=UB.mort, ymax=UB.dens, fill="4"), alpha=0.6) +
  
  # Reiterate mean line for clarity
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +
  
  # add time slice lines
  geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1) +

  # Legend Formatting
  labs(title= "Total Uncertainty", x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")"))) +
  scale_fill_manual(name="Uncertainty", values=c("green3", "blue", "red", "orange2"), labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.15,0.85)) + 

  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5)))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm")) 
  #poster.theme1
dev.off()
# -----------------------------------
  






########################################################
# Uncertainty percentages relative to the mean biomass
########################################################



# Looking at relative magnitudes of uncertainties

# Allometry
vlf.allom.rel <- allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "range"]/allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "Mean"]
vuf.allom.rel <- allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "range"]/allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "Mean"]
summary(vlf.allom.rel)
summary(vuf.allom.rel)

#Range Means
mean(allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "range"], na.rm=T); sd(allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "range"], na.rm=T)

mean(allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "range"], na.rm=T); sd(allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "range"], na.rm=T)


# Ratio Means
mean(vlf.allom.rel, na.rm=T); sd(vlf.allom.rel, na.rm=T)
mean(vuf.allom.rel, na.rm=T); sd(vuf.allom.rel, na.rm=T)


# Density
vlf.dens.rel <- dens.uncert[dens.uncert$SiteID=="VLF" & dens.uncert$Year >= 1980, "range"]/allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "Mean"]
vuf.dens.rel <- dens.uncert[dens.uncert$SiteID=="VUF" & dens.uncert$Year >= 1980, "range"]/allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "Mean"]
summary(vlf.dens.rel)
summary(vuf.dens.rel)

mean(vlf.dens.rel, na.rm=T); sd(vlf.dens.rel, na.rm=T)
mean(vuf.dens.rel, na.rm=T); sd(vuf.dens.rel, na.rm=T) 


# Mortality
vlf.mort.rel <- uncert.mort[uncert.mort$SiteID=="VLF" & uncert.mort$Year >=1980, "range"]/allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "Mean"]
vuf.mort.rel <- uncert.mort[uncert.mort$SiteID=="VUF" & uncert.mort$Year >=1980, "range"]/allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "Mean"]
summary(vlf.mort.rel)
summary(vuf.mort.rel)

mean(vlf.mort.rel, na.rm=T); sd(vlf.mort.rel, na.rm=T)
mean(vuf.mort.rel, na.rm=T); sd(vuf.mort.rel, na.rm=T)


# TR increment
vlf.inc.rel <- uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year >=1980, "range"]/allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year >=1980, "Mean"]
vuf.inc.rel <- uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year >=1980 , "range"]/allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year >=1980, "Mean"]
summary(vlf.inc.rel)
summary(vuf.inc.rel)

# Range Means
mean(uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year >=1980, "range"], na.rm=T); sd(uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year >=1980, "range"], na.rm=T)

mean(uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year >=1980, "range"], na.rm=T); sd(uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year >=1980, "range"], na.rm=T)


# Ratio Means
mean(vlf.inc.rel, na.rm=T); sd(vlf.inc.rel, na.rm=T)
mean(vuf.inc.rel, na.rm=T); sd(vuf.inc.rel, na.rm=T)

# Overall uncertainty

vlf.final.rel  <- vlf.final$range/vlf.final$Base
vuf.final.rel <- vuf.final$range/vuf.final$Base

mean(vlf.final.rel, na.rm=T); sd(vlf.final.rel, na.rm=T)
mean(vuf.final.rel, na.rm=T); sd(vuf.final.rel, na.rm=T)
  
#################################################################
# percentage of each uncertain ty relative to the total uncertainty
#################################################################

# Creating windows to assess changes in uncertainty
start <- 2011
window1 <- (1995-2) : (1995+2)
window2 <- (1980-2) : (1980+2)

# Mean Biomass at each window
	# Start
	mean(vlf.final[vlf.final$Year %in% start , "Base"], na.rm=T); sd(vlf.final[vlf.final$Year %in% start , "Base"], na.rm=T)
	mean(vuf.final[vuf.final$Year %in% start , "Base"], na.rm=T); sd(vuf.final[vuf.final$Year %in% start , "Base"], na.rm=T)
	
  # Window 1 (1995)
	mean(vlf.final[vlf.final$Year %in% window1 , "Base"], na.rm=T); sd(vlf.final[vlf.final$Year %in% window1 , "Base"], na.rm=T)	
	mean(vuf.final[vuf.final$Year %in% window1 , "Base"], na.rm=T); sd(vuf.final[vuf.final$Year %in% window1 , "Base"], na.rm=T)
	  
  # Window 2 (1980)
	mean(vlf.final[vlf.final$Year %in% window2 , "Base"], na.rm=T); sd(vlf.final[vlf.final$Year %in% window2 , "Base"], na.rm=T)	
	mean(vuf.final[vuf.final$Year %in% window2 , "Base"], na.rm=T); sd(vuf.final[vuf.final$Year %in% window2 , "Base"], na.rm=T)



# Overall uncertainty kg of biomass /m2
  #start
	mean(vlf.final[vlf.final$Year %in% start , "range"], na.rm=T); sd(vlf.final[vlf.final$Year %in% start , "range"], na.rm=T)
	mean(vuf.final[vuf.final$Year %in% start , "range"], na.rm=T); sd(vuf.final[vuf.final$Year %in% start , "range"], na.rm=T)
	
  # Window 1 (1995)
	mean(vlf.final[vlf.final$Year %in% window1 , "range"], na.rm=T); sd(vlf.final[vlf.final$Year %in% window1 , "range"], na.rm=T)	
	mean(vuf.final[vuf.final$Year %in% window1 , "range"], na.rm=T); sd(vuf.final[vuf.final$Year %in% window1 , "range"], na.rm=T)
	  
  # Window 2 (1980)
	mean(vlf.final[vlf.final$Year %in% window2 , "range"], na.rm=T); sd(vlf.final[vlf.final$Year %in% window2 , "range"], na.rm=T)	
	mean(vuf.final[vuf.final$Year %in% window2 , "range"], na.rm=T); sd(vuf.final[vuf.final$Year %in% window2 , "range"], na.rm=T)


# Allometry

  # start year 2011
	vlf.allom.perc.start <- allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year %in% start, "range"]/vlf.final[vlf.final	$Year %in% start, "range"]
	vuf.allom.perc.start <- allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year %in% start, "range"]/vuf.final[vuf.final	$Year %in% start, "range"]
	summary(vlf.allom.perc.start)
	summary(vuf.allom.perc.start)

	mean(vlf.allom.perc.start, na.rm=T); sd(vlf.allom.perc.start, na.rm=T)
	mean(vuf.allom.perc.start, na.rm=T); sd(vuf.allom.perc.start, na.rm=T)

  # Window 1 (1995)
	vlf.allom.perc.win1 <- allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year %in% window1, "range"]/vlf.final[vlf.final	$Year %in% window1, "range"]
	vuf.allom.perc.win1 <- allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year %in% window1, "range"]/vuf.final[vuf.final	$Year %in% window1, "range"]
	summary(vlf.allom.perc.win1)
	summary(vuf.allom.perc.win1)

	mean(vlf.allom.perc.win1, na.rm=T); sd(vlf.allom.perc.win1, na.rm=T)
	mean(vuf.allom.perc.win1, na.rm=T); sd(vuf.allom.perc.win1, na.rm=T)

  # Window 2 (1980)
	vlf.allom.perc.win2 <- allom.uncert[allom.uncert$SiteID=="VLF" & allom.uncert$Year %in% window2, "range"]/vlf.final[vlf.final	$Year %in% window2, "range"]
	vuf.allom.perc.win2 <- allom.uncert[allom.uncert$SiteID=="VUF" & allom.uncert$Year %in% window2, "range"]/vuf.final[vuf.final	$Year %in% window2, "range"]
	summary(vlf.allom.perc.win2)
	summary(vuf.allom.perc.win2)

	mean(vlf.allom.perc.win2, na.rm=T); sd(vlf.allom.perc.win2, na.rm=T)
	mean(vuf.allom.perc.win2, na.rm=T); sd(vuf.allom.perc.win2, na.rm=T)



# Density
  # start year 2011
	vlf.dens.perc.start <- dens.uncert[dens.uncert$SiteID=="VLF" & dens.uncert$Year %in% start, "range"]/vlf.final[vlf.final		$Year %in% start, "range"]
	vuf.dens.perc.start <- dens.uncert[dens.uncert$SiteID=="VUF" & dens.uncert$Year %in% start, "range"]/vuf.final[vuf.final		$Year %in% start, "range"]
	summary(vlf.dens.perc.start)
	summary(vuf.dens.perc.start)

	mean(vlf.dens.perc.start, na.rm=T); sd(vlf.dens.perc.start, na.rm=T)
	mean(vuf.dens.perc.start, na.rm=T); sd(vuf.dens.perc.start, na.rm=T)

  # Window 1 (1995)
	vlf.dens.perc.win1 <- dens.uncert[dens.uncert$SiteID=="VLF" & dens.uncert$Year %in% window1, "range"]/vlf.final[vlf.final		$Year %in% window1, "range"]
	vuf.dens.perc.win1 <- dens.uncert[dens.uncert$SiteID=="VUF" & dens.uncert$Year %in% window1, "range"]/vuf.final[vuf.final		$Year %in% window1, "range"]
	summary(vlf.dens.perc.win1)
	summary(vuf.dens.perc.win1)

	mean(vlf.dens.perc.win1, na.rm=T); sd(vlf.dens.perc.win1, na.rm=T)
	mean(vuf.dens.perc.win1, na.rm=T); sd(vuf.dens.perc.win1, na.rm=T)

  # Window 2 (1980)
	vlf.dens.perc.win2 <- dens.uncert[dens.uncert$SiteID=="VLF" & dens.uncert$Year %in% window2, "range"]/vlf.final[vlf.final		$Year %in% window2, "range"]
	vuf.dens.perc.win2 <- dens.uncert[dens.uncert$SiteID=="VUF" & dens.uncert$Year %in% window2, "range"]/vuf.final[vuf.final		$Year %in% window2, "range"]
	summary(vlf.dens.perc.win2)
	summary(vuf.dens.perc.win2)

	mean(vlf.dens.perc.win2, na.rm=T); sd(vlf.dens.perc.win2, na.rm=T)
	mean(vuf.dens.perc.win2, na.rm=T); sd(vuf.dens.perc.win2, na.rm=T)

# Mortality
  # start year 2011
	vlf.mort.perc.start <- uncert.mort[uncert.mort$SiteID=="VLF" & uncert.mort$Year %in% start, "range"]/vlf.final[vlf.final		$Year %in% start, "range"]
	vuf.mort.perc.start <- uncert.mort[uncert.mort$SiteID=="VUF" & uncert.mort$Year %in% start, "range"]/vuf.final[vuf.final		$Year %in% start, "range"]
	summary(vlf.mort.perc.start)
	summary(vuf.mort.perc.start)

	mean(vlf.mort.perc.start, na.rm=T); sd(vlf.mort.perc.start, na.rm=T)
	mean(vuf.mort.perc.start, na.rm=T); sd(vuf.mort.perc.start, na.rm=T)

  # Window 1 (1995)
	vlf.mort.perc.win1 <- uncert.mort[uncert.mort$SiteID=="VLF" & uncert.mort$Year %in% window1, "range"]/vlf.final[vlf.final		$Year %in% window1, "range"]
	vuf.mort.perc.win1 <- uncert.mort[uncert.mort$SiteID=="VUF" & uncert.mort$Year %in% window1, "range"]/vuf.final[vuf.final		$Year %in% window1, "range"]
	summary(vlf.mort.perc.win1)
	summary(vuf.mort.perc.win1)

	mean(vlf.mort.perc.win1, na.rm=T); sd(vlf.mort.perc.win1, na.rm=T)
	mean(vuf.mort.perc.win1, na.rm=T); sd(vuf.mort.perc.win1, na.rm=T)

  # Window 2 (1980)
	vlf.mort.perc.win2 <- uncert.mort[uncert.mort$SiteID=="VLF" & uncert.mort$Year %in% window2, "range"]/vlf.final[vlf.final		$Year %in% window2, "range"]
	vuf.mort.perc.win2 <- uncert.mort[uncert.mort$SiteID=="VUF" & uncert.mort$Year %in% window2, "range"]/vuf.final[vuf.final		$Year %in% window2, "range"]
	summary(vlf.mort.perc.win2)
	summary(vuf.mort.perc.win2)

	mean(vlf.mort.perc.win2, na.rm=T); sd(vlf.mort.perc.win2, na.rm=T)
	mean(vuf.mort.perc.win2, na.rm=T); sd(vuf.mort.perc.win2, na.rm=T)


# TR increment
vlf.inc.perc <- uncert.increment[uncert.increment$SiteID=="VLF", "range"]/vlf.final$range
vuf.inc.perc <- uncert.increment[uncert.increment$SiteID=="VUF", "range"]/vuf.final$range
summary(vlf.inc.perc)
summary(vuf.inc.perc)
mean(vlf.inc.perc, na.rm=T); sd(vlf.inc.perc, na.rm=T)
mean(vuf.inc.perc, na.rm=T); sd(vuf.inc.perc, na.rm=T)

  # start year 2011
	vlf.inc.perc.start <- uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year %in% start, "range"]/vlf.final[vlf.final		$Year %in% start, "range"]
	vuf.inc.perc.start <- uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year %in% start, "range"]/vuf.final[vuf.final		$Year %in% start, "range"]
	summary(vlf.inc.perc.start)
	summary(vuf.inc.perc.start)

	mean(vlf.inc.perc.start, na.rm=T); sd(vlf.inc.perc.start, na.rm=T)
	mean(vuf.inc.perc.start, na.rm=T); sd(vuf.inc.perc.start, na.rm=T)

  # Window 1 (1995)
	vlf.inc.perc.win1 <- uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year %in% window1, "range"]/vlf.final[vlf.final		$Year %in% window1, "range"]
	vuf.inc.perc.win1 <- uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year %in% window1, "range"]/vuf.final[vuf.final$Year %in% window1, "range"]
	summary(vlf.inc.perc.win1)
	summary(vuf.inc.perc.win1)

	mean(vlf.inc.perc.win1, na.rm=T); sd(vlf.inc.perc.win1, na.rm=T)
	mean(vuf.inc.perc.win1, na.rm=T); sd(vuf.inc.perc.win1, na.rm=T)

  # Window 2 (1980)
	vlf.inc.perc.win2 <- uncert.increment[uncert.increment$SiteID=="VLF" & uncert.increment$Year %in% window2, "range"]/vlf.final[vlf.final		$Year %in% window2, "range"]
	vuf.inc.perc.win2 <- uncert.increment[uncert.increment$SiteID=="VUF" & uncert.increment$Year %in% window2, "range"]/vuf.final[vuf.final		$Year %in% window2, "range"]
	summary(vlf.inc.perc.win2)
	summary(vuf.inc.perc.win2)

	mean(vlf.inc.perc.win2, na.rm=T); sd(vlf.inc.perc.win2, na.rm=T)
	mean(vuf.inc.perc.win2, na.rm=T); sd(vuf.inc.perc.win2, na.rm=T)