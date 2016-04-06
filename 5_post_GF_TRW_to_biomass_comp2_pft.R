# Transforming DBH reconstructions from Tree-ring Widths into biomass (kg/m2)

library(dplR)
library(ggplot2)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


# Load in diameter reconstrcutions generated from step 3.

g.filled.diam <- read.csv("processed_data/DOE_Allsites_GapFilling_DBHrecon_ALL.csv", header=T, row.names=1)


# summary(g.filled.diam)

# read in tree data
tree.data <- read.csv("processed_data/DOE_AllsitesTreeData.csv", header=T)
summary(tree.data)

#select sites for which to run the biomass reconstruction
# NOTE: the North Carolina coast doesn't have a density estimate, so we're going to ignore it for now
trees.use <- tree.data[tree.data$TreeID %in% names(g.filled.diam),] # If you want to do this later, it'll be a special case
# trees.use <- tree.data
summary(trees.use)

# Changing PCRU to PIRU as this is the correct terminology
trees.use$Species <- recode(trees.use$Species, "'PCRU' = 'PIRU'")

plot.data <- read.csv("raw_input_files/DOE_plus_Valles.csv")
plot.data$Year.Sample <- as.numeric(substr(plot.data$date.sample,7,10))
summary(plot.data)


##########################################################################
# Allometric Equations
##########################################################################


#Convert to biomass with the allometric equation
#using the PECAN generated bayesian equations
library(car)
load("processed_data/allometries/allometries_2.Rdata")
allom.2 <- allom.temp
allometries <- allom.2
summary(allometries)

#pft.vector <- as.factor(ifelse(trees.use$Species2=="PICO", "YAY", "BOO!"))
pft.vector <- vector(length=nrow(trees.use))
summary(pft.vector)

for(i in unique(trees.use$Species)[!is.na(unique(trees.use$Species))]){
  cols.now <- which(trees.use$Species==i)
  if(!(i %in% unique(pft.db$acronym))) {
    pft.vector[cols.now] <- NA
  } else {
    pft.now <- paste(unique(pft.db[pft.db$acronym==i,"CLM"])[1]) # take only the first if we have multiple entries                            
    pft.vector[cols.now] <- pft.now
  }
}
pft.vector <- as.factor(pft.vector)
summary(pft.vector)
trees.use$pft <- pft.vector
summary(trees.use)

summary(allom.temp)
# Find out what species we don't have allometries for that need to be renames
unique(trees.use$Species)[!(unique(trees.use$Species) %in% names(allometries))]

trees.use <- trees.use[!is.na(trees.use$TreeID),]
summary(trees.use[is.na(trees.use$TreeID),])
summary(trees.use)

# Need to recode the missing species to relate to genus level equations.

trees.use$spp.allom <- recode(trees.use$Species, "'ABLA' = 'ABIES'; 'PIAB' = 'PINUS';'PIPA' = 'PINUS'; 'QUBI' = 'QUERC'; 'LIST' = 'broad_decid';   
                              'QUPH' = 'QUERC'; 'OAK' = 'QUERC'; 'ULAL' = 'ULMUS'; 'FRAX'='FRAM'; 'CAOV' = 'CARYA'; 'CATO' = 'CARYA'; 'CAGL' = 'CARYA';
                              'RED OAK' = 'QUERC'; 'ASTR'='broad_decid'; 'CACO' = 'CARYA'; 'CATE' = 'CARYA'; 'CALA' = 'CARYA'; 'QUMU' = 'QUERC';
                              'ACSAC' = 'ACSA2'; 'QUIN' = 'QUERC'")
summary(trees.use)
plots <- unique(trees.use$PlotID) # find out what plots we need


# making the generalized allometry function to run
# log(AGB) = mu0 + mu1*log(DBH) --equaton form of PECAN allometrics
allom.eq <- function(mu0, mu1, DBH) { exp(mu0 + mu1 * log(DBH) )}

# A temporary allometry file that will get filled
allom.temp <- g.filled.diam
allom.temp[,] <- NA

# dbh=0 causes problems, so we're going to make those NA
g.filled.diam[g.filled.diam==0] <- 1e-6
min(g.filled.diam, na.rm=T)
# summary(g.filled.diam)
dim(g.filled.diam)

# First we're going to save the allometric iterations by tree because this gives us more flexibility in the analyses
bm.array <- array(NA, dim=c(nrow(g.filled.diam), ncol(g.filled.diam), nrow(allometries[[1]])))
dimnames(bm.array)[[1]] <- row.names(g.filled.diam)  #CRR Added
dimnames(bm.array)[[2]] <- names(g.filled.diam)
# summary(bm.array[,,1])
dim(bm.array)

tree.names <- names(g.filled.diam)

#--------------------------------------------------
# This is one big loop that goes through each layer of the 500 iterations 
# and uses it to calculate the biomass of each tree
# Part 2 (t-loop): converts biomass/tree to biomass/m2
#--------------------------------------------------
for(i in 1:nrow(allometries[[1]])){
  allom.temp <- g.filled.diam
  allom.temp[,] <- NA
  
# Species loop for calculating tree biomass
for(j in unique(trees.use$pft)){
  cols <- which(names(g.filled.diam) %in% trees.use[trees.use$pft==j, "TreeID"])

  # Pulling coefficients from the randomly pulled estimates from Pecan; 
  # Need to use Bg0 because the heierarchical means were being weird
  mu0=allometries[[j]][i,"Bg0"]
  mu1=allometries[[j]][i,"Bg1"]
  allom.temp[,cols] <- allom.eq(mu0=mu0, mu1 = mu1, DBH = g.filled.diam[,cols])
}
# summing to the plot level

allom.temp[is.na(allom.temp)] <- 0

# biomass loop for summing trees to plots in kg/m2
for(t in 1:ncol(allom.temp)){
  if(substr(tree.names[t],1,1)=="V" | substr(tree.names[t],1,1)=="N"){ 
     bm.array[,t,i] <- allom.temp[,t]*plot.data[plot.data$PlotID==paste(tree.data[tree.data$TreeID==tree.names[t], "PlotID"]),"Density.Total..stems.ha."]/10000
  } else {  
  	# Neil's sampling scheme has different densities based on the size of each tree, so we index it by tree rather than total plot area
     bm.array[,t,i] <- allom.temp[,t]*tree.data[tree.data$TreeID==tree.names[t],"Density..stems.ha."]/10000
  }

}
}
#--------------------------------------------------
plots <- paste(unique(tree.data[tree.data$TreeID %in% names(g.filled.diam),"PlotID"]))
years <- as.numeric(row.names(g.filled.diam))
for(p in 1:length(plots)){ # will go by plot
    cols <- which(dimnames(bm.array)[[2]] %in% tree.data[tree.data$PlotID==plots[p], "TreeID"])
	yr.sample <- plot.data[plot.data$PlotID==plots[p], "Year.Sample"]
	if(max(years) > yr.sample){ # if tree wasn't cored in the last year of sampling, make it's outer years NA
	  rows.na <- which(years > yr.sample-1)
	  bm.array[rows.na,cols,] <- NA
	}
}
summary(bm.array[,,1])



#bm.array[,,1]
summary(bm.array[,1:10,1])
save(bm.array, file="processed_data/Biomass_Array_Tree_kgm-2_component2_pft.RData")
load("processed_data/Biomass_Array_Tree_kgm-2_component2_pft.RData")
# ---------------------------------
# Create a data frame with biomass by tree
# ---------------------------------
bm.mean <- data.frame(apply(bm.array, c(1,2), mean, na.rm=T))
bm.lo <- data.frame(apply(bm.array, c(1,2), quantile, 0.025, na.rm=T))
bm.hi <- data.frame(apply(bm.array, c(1,2), quantile, 0.975, na.rm=T))
names(bm.mean) <- names(bm.lo) <- names(bm.hi) <- tree.names

# This is stacking the tree-level biomass to be easier to work with
bm.tree <- stack(bm.mean)[,c(2,1)]
names(bm.tree) <- c("TreeID", "BM.Mean")
bm.tree$Year <- as.numeric(row.names(g.filled.diam))
bm.tree$BM.CI.lo <- stack(bm.lo)[,1]
bm.tree$BM.CI.hi <- stack(bm.hi)[,1]
summary(bm.tree)

# Merge in a lot of useful data about each tree
bm.tree <- merge(bm.tree, trees.use[,c("TreeID","Site","PlotID", "plot", "spp.allom", "Canopy.Class", "DBH..cm.", "Live.Dead", "Distance", "Azimuth","Dated", "pft")], all.x=T, all.y=F)
summary(bm.tree)

write.csv(bm.tree, "processed_data/Biomass_Tree_kgm-2_component2_pft.csv", row.names=F)
# ---------------------------------


# ---------------------------------
# Aggregating to the total plot-level biomass
# ---------------------------------
# doing bm.plot wide first and then we'll stack it
plots <- unique(bm.tree$PlotID)
bm.plot <- array(dim=c(nrow(g.filled.diam), length(plots), 3)) # years x plots x (mean, ci.low, ci.hi)
dimnames(bm.plot)[[1]] <- row.names(g.filled.diam); 
dimnames(bm.plot)[[2]] <- plots; 
dimnames(bm.plot)[[3]] <- c("mean", "ci.low", "ci.hi")
for(p in 1:length(plots)){ # will go by plot
  cols <- which(dimnames(bm.mean)[[2]] %in% tree.data[tree.data$PlotID==plots[p], "TreeID"])
  if(substr(plots[p],1,1)=="V" | substr(plots[p],1,1)=="N"){ 
	bm.plot[,p,1] <- apply(bm.mean[,cols], 1, mean)
	bm.plot[,p,2] <- apply(bm.lo[,cols], 1, mean)
	bm.plot[,p,3] <- apply(bm.hi[,cols], 1, mean)
  } else {  
	bm.plot[,p,1] <- apply(bm.mean[,cols], 1, sum)
	bm.plot[,p,2] <- apply(bm.lo[,cols], 1, sum)
	bm.plot[,p,3] <- apply(bm.hi[,cols], 1, sum)
  }
}

summary(bm.plot[,,1])


# Stacking it to be a more user-friendly data frame
bm.plot2 <- stack(data.frame(bm.plot[,,1]))[,c(2,1)]
names(bm.plot2) <- c("PlotID", "BM.Mean")
bm.plot2$Plot <- as.factor(substr(bm.plot2$PlotID, 3,3))
bm.plot2$Year <- as.numeric(dimnames(bm.plot)[[1]])
bm.plot2$BM.CI.lo <- stack(data.frame(bm.plot[,,2]))[,1]
bm.plot2$BM.CI.hi <- stack(data.frame(bm.plot[,,3]))[,1]
summary(bm.plot2)

bm.plot2 <- merge(bm.plot2, plot.data[,c("PlotID", "Site..Tower.", "latitude", "longitude", "elevation", "Year.Sample")])
summary(bm.plot2)

# Visualizing everything to make our lives easier
pdf("figures/Biomass_Plot_Total_kgm-2.pdf")
ggplot(bm.plot2[,]) + facet_wrap(~Site..Tower., scales="free") +
  geom_ribbon(aes(x=Year, ymin= BM.CI.lo, ymax=BM.CI.hi, fill=PlotID), alpha=0.5) +
  geom_line(aes(x=Year, y=BM.Mean, color=PlotID)) +
  labs(x="Year", y="Biomass (kg m-2)") +
  theme_bw()
dev.off()

write.csv(bm.plot2, "processed_data/Biomass_Plot_Total_kgm-2_comp2_pft.csv", row.names=F)
# ---------------------------------


# ---------------------------------
# Aggregating to the species plot-level biomass
# ---------------------------------
# doing bm.plot wide first and then we'll stack it
# ---------------------------------
# Aggregating to the pft plot-level biomass
# ---------------------------------
# doing bm.plot wide first and then we'll stack it
plots <- unique(bm.tree$PlotID)
pft <- unique(bm.tree$pft)

bm.pft <- list()
bm.pft[["mean"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(plots))) # years x species x plot
  dimnames(bm.pft[["mean"]])[[1]] <- row.names(g.filled.diam); 
  dimnames(bm.pft[["mean"]])[[2]] <- pft 
  dimnames(bm.pft[["mean"]])[[3]] <- plots
bm.pft[["ci.lo"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(plots))) # years x species x plot
bm.pft[["ci.hi"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(plots))) # years x species x plot

for(s in 1:length(pft)){
  for(p in 1:length(plots)){ # will go by plot
    cols <- which(dimnames(bm.mean)[[2]] %in% trees.use[trees.use$PlotID==plots[p] & trees.use$pft==pft[s], "TreeID"])
	if(length(cols) > 0){
      if(substr(plots[p],1,1)=="V" | substr(plots[p],1,1)=="N"){ 
	    if(length(cols) > 1) {
  	      bm.pft[["mean"]][,s,p] <- apply(bm.mean[,cols], 1, mean)
	      bm.pft[["ci.lo"]][,s,p] <- apply(bm.lo[,cols], 1, mean)
	      bm.pft[["ci.hi"]][,s,p] <- apply(bm.hi[,cols], 1, mean)
	    } else {
  	      bm.pft[["mean"]][,s,p] <- bm.mean[,cols]
	      bm.pft[["ci.lo"]][,s,p] <- bm.lo[,cols]
	      bm.pft[["ci.hi"]][,s,p] <- bm.hi[,cols]
	    }	  
      } else {  
	    if(length(cols) > 1) {
  	      bm.pft[["mean"]][,s,p] <- apply(bm.mean[,cols], 1, sum)
	      bm.pft[["ci.lo"]][,s,p] <- apply(bm.lo[,cols], 1, sum)
	      bm.pft[["ci.hi"]][,s,p] <- apply(bm.hi[,cols], 1, sum)
  	    } else {
  	      bm.pft[["mean"]][,s,p] <- bm.mean[,cols]
	      bm.pft[["ci.lo"]][,s,p] <- bm.lo[,cols]
	      bm.pft[["ci.hi"]][,s,p] <- bm.hi[,cols]
	    }
      }
    }
  }
}

summary(bm.pft[["ci.hi"]][,,1])

# Getting the bm.spp into a friendly format
for(p in 1:length(plots)){
  temp <- stack(data.frame(bm.pft[["mean"]][,,p]))[,c(2,1)]
  names(temp) <- c("PFT", "BM.Mean")
  temp$PlotID <- as.factor(dimnames(bm.pft[["mean"]])[[3]][p])
  temp$Plot <- as.factor(substr(temp$PlotID, 3,3))
  temp$Year <- as.numeric(dimnames(bm.pft[["mean"]])[[1]])
  temp$BM.CI.lo <- stack(data.frame(bm.pft[["ci.lo"]][,,p]))[,1]
  temp$BM.CI.hi <- stack(data.frame(bm.pft[["ci.hi"]][,,p]))[,1]

  if(p == 1) bm.pft2 <- temp else bm.pft2 <- rbind(bm.pft2, temp)
}
summary(bm.pft2)

bm.pft2 <- merge(bm.pft2, plot.data[,c("PlotID", "Site..Tower.", "latitude", "longitude", "elevation", "Year.Sample")])
summary(bm.pft2)

pdf("figures/Biomass_Plot_Species_kgm-2.pdf") # NOTE: This will probably stop worrking well very quickly
ggplot(bm.pft2[,]) + facet_wrap(Site..Tower. ~ PlotID, scales="free") +
  geom_ribbon(aes(x=Year, ymin= BM.CI.lo, ymax=BM.CI.hi, fill=PFT), alpha=0.5) +
  geom_line(aes(x=Year, y=BM.Mean, color=PFT)) +
  labs(x="Year", y="Biomass (kg m-2)") +
  theme_bw()
dev.off()

write.csv(bm.pft2, "processed_data/Biomass_Plot_Species_kgm-2_comp1.csv", row.names=F)
# ---------------------------------


# ---------------------------------
# Aggregating to the species SITE-level biomass
# ---------------------------------
# doing bm.plot wide first and then we'll stack it
sites <- unique(substr(bm.pft2$PlotID, 1, 2))
spp <- unique(bm.pft2$PFT)

bm.site.pft <- list()
bm.site.pft[["mean"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(sites))) # years x species x plot
  dimnames(bm.site.pft[["mean"]])[[1]] <- row.names(g.filled.diam); 
  dimnames(bm.site.pft[["mean"]])[[2]] <- pft 
  dimnames(bm.site.pft[["mean"]])[[3]] <- sites
bm.site.pft[["sd"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(sites))) # years x species x plot
bm.site.pft[["ci.lo.mean"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(sites))) # years x species x plot
bm.site.pft[["ci.hi.mean"]] <- array(dim=c(nrow(g.filled.diam), length(pft), length(sites))) # years x species x plot

# Need to make NA 0 because that species doesn't exist in the record
bm.pft$mean[is.na(bm.pft$mean)] <- 0
bm.pft$ci.lo[is.na(bm.pft$ci.lo)] <- 0
bm.pft$ci.hi[is.na(bm.pft$ci.hi)] <- 0

# Altering our 0s to be NA for years more recent than the year the plot was sampled
plots <- paste(unique(tree.data[tree.data$TreeID %in% names(g.filled.diam),"PlotID"]))
years <- as.numeric(row.names(g.filled.diam))
for(p in 1:length(plots)){ # will go by plot
	yr.sample <- plot.data[plot.data$PlotID==plots[p], "Year.Sample"]
	if(max(years) > yr.sample){ # if tree wasn't cored in the last year of sampling, make it's outer years NA
	  rows.na <- which(years > yr.sample-1)

	  bm.pft$mean[rows.na,,p] <- NA
      bm.pft$ci.lo[rows.na,,p] <- NA
      bm.pft$ci.hi[rows.na,,p] <- NA

	}
}
summary(bm.pft$mean[,,6])

for(s in 1:length(pft)){
  for(p in 1:length(sites)){ # will go by plot
    cols <- which(substr(dimnames(bm.pft$mean)[[3]],1,2) == sites[p])

  	bm.site.pft[["mean"]][,s,p] <- apply(bm.pft$mean[,s,cols], 1, mean)
  	bm.site.pft[["sd"]][,s,p]   <- apply(bm.pft$mean[,s,cols], 1, sd)
	bm.site.pft[["ci.lo.mean"]][,s,p] <- apply(bm.pft$ci.lo[,s,cols], 1, mean)
	bm.site.pft[["ci.hi.mean"]][,s,p] <- apply(bm.pft$ci.hi[,s,cols], 1, mean)
  }
}


summary(bm.site.pft[["mean"]][,,1])


# Getting the bm.site.pft into a friendly format
for(p in 1:length(sites)){
  temp <- stack(data.frame(bm.site.pft[["mean"]][,,p]))[,c(2,1)]
  names(temp) <- c("PFT", "BM.Mean")
  temp$SiteID <- as.factor(dimnames(bm.site.pft[["mean"]])[[3]][p])
  # temp$Plot <- as.factor(substr(temp$PlotID, 3,3))
  temp$Year <- as.numeric(dimnames(bm.site.pft[["mean"]])[[1]])
  temp$BM.SD <- stack(data.frame(bm.site.pft[["sd"]][,,p]))[,1]
  temp$BM.CI.lo <- stack(data.frame(bm.site.pft[["ci.lo.mean"]][,,p]))[,1]
  temp$BM.CI.hi <- stack(data.frame(bm.site.pft[["ci.hi.mean"]][,,p]))[,1]

  temp$Site <- unique(plot.data[substr(plot.data$PlotID,1,2)==sites[p],"Site..Tower."])
  temp$latitude <- mean(plot.data[substr(plot.data$PlotID,1,2)==sites[p],"latitude"], na.rm=T)
  temp$longitude <- mean(plot.data[substr(plot.data$PlotID,1,2)==sites[p],"longitude"], na.rm=T)
  temp$elevation <- mean(plot.data[substr(plot.data$PlotID,1,2)==sites[p],"elevation"], na.rm=T)
  temp$n.plots <- length(plot.data[substr(plot.data$PlotID,1,2)==sites[p],"PlotID"])

  if(p == 1) bm.site.pft2 <- temp else bm.site.pft2 <- rbind(bm.site.pft2, temp)
}
summary(bm.site.pft2)

pdf("figures/Biomass_Site_Species_kgm-2_SE.pdf") # NOTE: This will probably stop worrking well very quickly
ggplot(bm.site.pft2) + facet_wrap(~Site, scales="free") +
  # geom_ribbon(aes(x=Year, ymin=BM.CI.lo, ymax=BM.CI.hi, fill=Species), alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=BM.Mean - BM.SD/sqrt(n.plots), ymax=BM.Mean + BM.SD/sqrt(n.plots), fill=PFT), alpha=0.5) +
  geom_line(aes(x=Year, y=BM.Mean, color=PFT)) +
  labs(x="Year", y="Biomass (kg m-2)") +
  theme_bw()
dev.off()


pdf("figures/Biomass_Site_Species_kgm-2_Allom_CI.pdf") # NOTE: This will probably stop worrking well very quickly
ggplot(bm.site.pft2) + facet_wrap(~Site, scales="free") +
  geom_ribbon(aes(x=Year, ymin=BM.CI.lo, ymax=BM.CI.hi, fill=PFT), alpha=0.5) +
  # geom_ribbon(aes(x=Year, ymin=BM.Mean - BM.SD/sqrt(n.plots), ymax=BM.Mean + BM.SD/sqrt(n.plots), fill=Species), alpha=0.5) +
  geom_line(aes(x=Year, y=BM.Mean, color=PFT)) +
  labs(x="Year", y="Biomass (kg m-2)") +
  theme_bw()
dev.off()

write.csv(bm.site.pft2, "processed_data/Biomass_Site_Species_kgm-2_comp2_pft.csv", row.names=F)

# ---------------------------------
# Aggregating to the TOTAL SITE-level biomass
# ---------------------------------
bm.site.pft2$n.pft <- ifelse(bm.site.pft2$BM.Mean>0,1,0)
summary(bm.site.pft2)


# # If not already done above to do the species-level BM, do this here
# # Need to make NA 0 because that species doesn't exist in the record
# bm.pft$mean[is.na(bm.pft$mean)] <- 0
# bm.pft$ci.lo[is.na(bm.pft$ci.lo)] <- 0
# bm.pft$ci.hi[is.na(bm.pft$ci.hi)] <- 0

# # Altering our 0s to be NA for years more recent than the year the plot was sampled
# plots <- paste(unique(tree.data[tree.data$TreeID %in% names(g.filled.diam),"PlotID"]))
# years <- as.numeric(row.names(g.filled.diam))
# for(p in 1:length(plots)){ # will go by plot
	# yr.sample <- plot.data[plot.data$PlotID==plots[p], "Year.Sample"]
	# if(max(years) > yr.sample){ # if tree wasn't cored in the last year of sampling, make it's outer years NA
	  # rows.na <- which(years > yr.sample-1)

	  # bm.pft$mean[rows.na,,p] <- NA
      # bm.pft$ci.lo[rows.na,,p] <- NA
      # bm.pft$ci.hi[rows.na,,p] <- NA

	# }
# }
# summary(bm.spp$mean[,,6])


sites <- unique(substr(bm.plot2$PlotID, 1, 2))
bm.site <- array(dim=c(nrow(g.filled.diam), length(sites), 4)) # years x plots x (mean, sd, ci.low, ci.hi)
dimnames(bm.site)[[1]] <- row.names(g.filled.diam); 
dimnames(bm.site)[[2]] <- sites; 
dimnames(bm.site)[[3]] <- c("mean", "sd", "ci.lo", "ci.hi")
for(p in 1:length(sites)){ # will go by plot
  cols <- which(substr(dimnames(bm.plot)[[2]],1,2) == sites[p])
	bm.site[,p,1] <- apply(bm.plot[,cols,1], 1, mean)
	bm.site[,p,2] <- apply(bm.plot[,cols,1], 1, sd)
	bm.site[,p,3] <- apply(bm.plot[,cols,2], 1, mean)
	bm.site[,p,4] <- apply(bm.plot[,cols,3], 1, mean)
}

summary(bm.site[,,2])


# Stacking it to be a more user-friendly data frame
bm.site2 <- stack(data.frame(bm.site[,,1]))[,c(2,1)]
names(bm.site2) <- c("SiteID", "BM.Mean")
bm.site2$Year <- as.numeric(dimnames(bm.site)[[1]])
bm.site2$BM.SD <- stack(data.frame(bm.site[,,2]))[,1]
bm.site2$BM.CI.lo <- stack(data.frame(bm.site[,,3]))[,1]
bm.site2$BM.CI.hi <- stack(data.frame(bm.site[,,4]))[,1]
summary(bm.site2)


# Getting some other site-level characteristics to tag along
bm.site3 <- aggregate(bm.site.pft2[,"n.pft"], by=list(bm.site.pft2$SiteID, bm.site.pft2$Site, bm.site.pft2$Year), FUN=sum)
names(bm.site3)[1:4] <- c("SiteID", "Site", "Year", "n.pft" )
summary(bm.site3)

bm.site2 <- merge(bm.site2, bm.site3)
summary(bm.site2)

bm.site4 <- aggregate(bm.site.pft2[,c("latitude", "longitude", "elevation", "n.plots")], by=list(bm.site.pft2$SiteID, bm.site.pft2$Site, bm.site.pft2$Year), FUN=mean) 
names(bm.site4)[1:3] <- c("SiteID", "Site", "Year")
summary(bm.site4)

bm.site2 <- merge(bm.site2, bm.site4, all.x=T, all.y=T)
summary(bm.site2)

pdf("figures/Biomass_Site_Total_kgm-2_SD.pdf") # NOTE: This will probably stop worrking well very quickly
ggplot(bm.site2) + facet_wrap(~Site) + 
  # geom_ribbon(aes(x=Year, ymin=BM.CI.lo, ymax=BM.CI.hi, fill=Site), alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=BM.Mean - BM.SD, ymax=BM.Mean + BM.SD, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=BM.Mean, color=Site), size=1.5) +
  labs(x="Year", y="Biomass (kg m-2)") +
  theme_bw()
dev.off()

write.csv(bm.site2, "processed_data/Biomass_Site_Total_kgm-2_comp2_pft.csv", row.names=F)

# ---------------------------------
