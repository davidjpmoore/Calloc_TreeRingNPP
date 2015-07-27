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
library(reshape2)

# Read in the tree data from the master data files
ross.trees <- read.csv("raw_input_files/tree_metadata_DOE_plus_valles.csv", na.strings=c("", "NA", "#VALUE!", "*"),header=T)
summary(ross.trees)

# read in Marcy's tree tree data
marcy.ppine <- read.csv("raw_input_files/marcy_ppine_2013.csv")
marcy.mcon <- read.csv("raw_input_files/marcy_mcon_2012.csv")
summary(marcy.ppine)
summary(marcy.mcon)

# ------------------------

# ------------------------
# Subset & format raw data sets
# ------------------------
# --------------
# Subsetting & Formatting Ross's tree data
# NOTE: summing all of the trees will not lead to the total plot density, so we're going to adjust it
#   Total Stem Tallies: 
#       VUA = 64
#       VUB = 112
#       VLA = 85
#       VLB = 56                
# --------------
ross.valles <- ross.trees[substr(ross.trees$PlotID,1,1)=="V",c("TreeID", "PlotID", "Species","DBH..cm.", "Density..stems.ha.")]
names(ross.valles)<- c("tree.id", "plot.id", "species", "dbh", "plot.density")
ross.valles$plot.density <- ross.valles$plot.density/10000 # convert density to stems/m2
ross.valles$site <- as.factor(ifelse(substr(ross.valles$plot.id,1,2)=="VU", "VUF", "VLF"))
ross.valles$project <- as.factor("Ross")
ross.valles$year <- 2012
ross.valles <- droplevels(ross.valles)
summary(ross.valles)

# density adjustment to allow us to sum to the right density
dens.adj <- data.frame(plot.id      = c("VUA", "VUB", "VLA", "VLB"),
                       trees.plot   = c(   64,   112,    85,    56), 
                       trees.sample = c(   50,    51,    50,    50),
                       plot.area    = c(24*24, 24*24, 12*12, 26*24))
dens.adj$p.sample <- dens.adj$trees.sample/dens.adj$trees.plot
dens.adj

for(p in unique(ross.valles$plot.id)){
	ross.valles[ross.valles$plot.id==p, "tree.density"] <- 1/dens.adj[dens.adj$plot.id==p, "plot.area"] * dens.adj[dens.adj$plot.id==p,"trees.plot"]/dens.adj[dens.adj$plot.id==p,"trees.sample"]
}
summary(ross.valles)
# --------------

# --------------
# Formatting Marcy's data
# --------------
# Merging Marcy's data together
marcy <- rbind(marcy.ppine[,names(marcy.ppine) %in%  names(marcy.mcon)], marcy.mcon)
marcy$PlotID <- as.factor(paste(substr(marcy$Site, 1,3), marcy$Plot_Name, sep="."))
names(marcy)[16] <- "DBH..cm." 
marcy$Tree_Tag_Number <- as.factor(paste0("X", marcy$Tree_Tag_Number))
summary(marcy)

# Calculating Marcy's plot density
marcy.density <- aggregate(marcy[,c("Plot_Radius")], by=list(marcy[,"PlotID"]), FUN=length)
names(marcy.density) <- c("PlotID", "n.stems")
marcy.density$plot.density <- marcy.density$n.stems/(pi*10^2)
summary(marcy.density)

# Merge Marcy's tree data & stem densities together
marcy <- merge(marcy, marcy.density)
marcy$tree.density <- 1/(pi*(marcy$Plot_Radius^2))
summary(marcy)

marcy.valles <- marcy[, c("Tree_Tag_Number", "PlotID", "Species", "DBH..cm.", "plot.density","tree.density", "Site")]
names(marcy.valles) <- c("tree.id", "plot.id", "species", "dbh", "plot.density", "tree.density", "site")
marcy.valles$year <- ifelse(marcy.valles$site == "PPINE", 2013, 2012)
marcy.valles$site <- recode(marcy.valles$site, "'PPINE'='VLF'; 'MCON'='VUF'")
marcy.valles$project <- as.factor("Marcy")
summary(marcy.valles)
# --------------

# --------------
# Merging Ross & Marcy Datasets together 
# --------------
all.valles <- merge(ross.valles, marcy.valles, all.x=T, all.y=T)
summary(all.valles)
# --------------

# --------------
# Convert DBH to biomass
# --------------
# read in allometry list
load("processed_data/allometries_list.Rdata") # reads in list names "allometries"

# Allometry function
allom.eq <- function(mu0, mu1, DBH) { exp(mu0 + mu1 * log(DBH) )}

# Check to see what species need a different title for allometric equations:
unique(all.valles$species)[!(unique(all.valles$species) %in% names(allometries))]

all.valles$species.allom <- recode(all.valles$species, "'PIEN'='picea.sp'; 'ABLA'='abies.sp'")

# Make a temporary object that will contain all of the allometric information
valles.allom <- array(dim=c(nrow(all.valles), nrow(allometries[[1]])))
row.names(valles.allom) <- all.valles$tree.id

for(s in unique(all.valles$species.allom)){
	trees <- which(all.valles$species.allom == s)
	for(i in 1:nrow(allometries[[s]])){
		mu0=allometries[[s]][i,"Bg0"]
		mu1=allometries[[s]][i,"Bg1"]
		valles.allom[trees,i] <- allom.eq(mu0, mu1, all.valles[trees, "dbh"])*all.valles[trees, "tree.density"]
	}
}
summary(valles.allom[,1:10])

all.valles$BM.mean <- apply(valles.allom, 1, mean, na.rm=T)
summary(all.valles)
# ------------------------


# ------------------------
# Some Aggregating the data in the ways necessary to do site-level graphing
# ------------------------
# Creating DBH bins -- this makes it easier to aggregate up
dbh.bins <- c(seq(0, max(all.valles$dbh, na.rm=T)-5, by=5), Inf)

# binning the trees in the data
all.valles$dbh.bin <- cut(all.valles$dbh, dbh.bins)
summary(all.valles$dbh.bin)

# --------------------
# Aggregating to the plot level
# --------------------
valles.plot <- aggregate(all.valles[,c("tree.density", "BM.mean")], by=list(all.valles$dbh.bin, all.valles$project, all.valles$site, all.valles$plot.id), FUN=sum)
names(valles.plot)[1:4] <- c("dbh.bin", "project", "site", "plot.id")
summary(valles.plot)

## Missing cells need to become 0; I'm doing this with a recast & stacking, but there's 
## probably a more elegant way to do this
# Recast so that plots are columns & dbh bins are rows; then add 0 in missing cells
valles.plot2 <- recast(valles.plot[,c("tree.density", "plot.id", "dbh.bin")], dbh.bin ~ plot.id)
valles.plot2[is.na(valles.plot2)] <- 0
summary(valles.plot2)

# stack the columns together... this should look similar to valles.plot, but witha couple extra rows
valles.plot3 <- stack(valles.plot2[,2:ncol(valles.plot2)])
names(valles.plot3) <- c("tree.density", "plot.id")
valles.plot3$dbh.bin <- valles.plot2$dbh.bin
valles.plot3$site <- as.factor(ifelse(substr(valles.plot3$plot.id,1,2)=="VU" | substr(valles.plot3$plot.id,1,3)=="MCO", "VUF", "VLF"))
valles.plot3$project <- as.factor(ifelse(substr(valles.plot3$plot.id,1,1)=="V", "Ross", "Marcy"))
summary(valles.plot3)

# merging the data together; this will add in our new rows with 0s
valles.plot <- merge(valles.plot, valles.plot3, all.x=T, all.y=T)
valles.plot[is.na(valles.plot$BM.mean), "BM.mean"] <- 0 # need to make sure if density == 0, then BM is 0 too
summary(valles.plot)


# Adding in the biomass estimate & 95% ci directly from allometry
for(p in unique(valles.plot$plot.id)){
	n.plots = 1 # this is from repeating the loop later where we need to basically average the densities
	for(s in unique(valles.plot$site)){
	for(d in unique(valles.plot$dbh.bin)){
		# Some indexing that will make the loop run faster
		trees   <- which(all.valles$plot.id==p & all.valles$dbh.bin==d & all.valles$site==s)
		out.row <- which(valles.plot$plot.id==p & valles.plot$dbh.bin==d & valles.plot$site==s)

		# This part subsets the portion of the allometry iterations that's most relevant to us at the
		# moment; doing this subsetting speeds things up and makes for cleaner code
		# In later loops, n.plots will vary, so this basically lets us take the average when we sum
		allom.temp <- valles.allom[trees,]/n.plots

		# finding the plot-level total biomass so we can find percentage of biomass belonging in each
		# size bin
		bm.total <- sum(valles.plot[which(valles.plot$plot.id==p & valles.plot$site==s), "BM.mean"],na.rm=T)

		if(length(trees)>1){
			# Here the apply statement is summing all trees in that DBH bin to get a total for the size class
			# Then we find the mean and 95% CI from the allometry MCMC iterations 
			# this doesn't work with only 1 tree, so that has to be slightly different
			out.mean  <-     mean(apply(allom.temp,2,sum, na.rm=T), na.rm=T)
			out.ci.lo <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.025, na.rm=T)
			out.ci.hi <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.975, na.rm=T)
		} else if(length(trees)==1){
			out.mean  <-     mean(allom.temp, na.rm=T)
			out.ci.lo <- quantile(allom.temp, 0.025, na.rm=T)
			out.ci.hi <- quantile(allom.temp, 0.975, na.rm=T)
		} else {
			out.mean  <- 0
			out.ci.lo <- 0
			out.ci.hi <- 0
		}		

		valles.plot[out.row,"BM.mean.allom"]  <- out.mean
		valles.plot[out.row,"BM.ci.lo.allom"] <- out.ci.lo
		valles.plot[out.row,"BM.ci.hi.allom"] <- out.ci.hi

		# To find the percent of the plot biomass in each size bin, we divide bin biomass by total biomass
		valles.plot[out.row,"BM.mean.plot.rel"]  <- out.mean/bm.total
		valles.plot[out.row,"BM.mean.allom.rel"]  <- out.mean/bm.total
		valles.plot[out.row,"BM.ci.lo.allom.rel"] <- out.ci.lo/bm.total
		valles.plot[out.row,"BM.ci.hi.allom.rel"] <- out.ci.hi/bm.total

	}
	}
}
summary(valles.plot)


# --------------------
# Aggregating to the site level by project
# We're not really interested in individual plots, so now we need to aggregate up to the site level
# --------------------
# Find the plot-based mean
valles.site.proj <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$project, valles.plot$site), FUN=mean, na.rm=T)
names(valles.site.proj)[1:3] <- c("dbh.bin", "project", "site")
summary(valles.site.proj)

# finding the plot-based 02.5%˚
valles.site.proj2 <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$project, valles.plot$site), FUN=quantile, 0.025, na.rm=T)
names(valles.site.proj2)[1:3] <- c("dbh.bin", "project", "site")
summary(valles.site.proj2)

# finding the plot-based 97.5%˚
valles.site.proj3 <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$project, valles.plot$site), FUN=quantile, 0.975, na.rm=T)
names(valles.site.proj3)[1:3] <- c("dbh.bin", "project", "site")
summary(valles.site.proj3)

# Putting the CIs into the main data frame
valles.site.proj$density.ci.lo.plot <- valles.site.proj2$tree.density
valles.site.proj$density.ci.hi.plot <- valles.site.proj3$tree.density
valles.site.proj$BM.ci.lo.plot <- valles.site.proj2$BM.mean
valles.site.proj$BM.ci.hi.plot <- valles.site.proj3$BM.mean
valles.site.proj$BM.ci.lo.plot.rel <- valles.site.proj2$BM.mean.plot.rel
valles.site.proj$BM.ci.hi.plot.rel <- valles.site.proj3$BM.mean.plot.rel
summary(valles.site.proj)

# ---------------
# Adding in the biomass estimate & 95% ci directly from allometry
# NOTE: relative biomass is inherently plot-based and cannot be done properly at the site level 
# (or not if we want to compare it with plot-based uncertainty) so we need to calculate 
# the relative plot biomass for each tree in the allometric data frame
allom.rel <- valles.allom
for(p in unique(valles.plot$plot.id)){
	trees.plot <- which(all.valles$plot.id==p)
	plot.total <- apply(allom.rel[trees.plot,],2,sum, na.rm=T) # total plot biomass for each iteration

	# calculating the relative tree biomass; there should be a way to do this a plot at a time,
	# but I had trouble getting it working
	for(i in trees.plot){ 
		allom.rel[i,] <- allom.rel[i,]/plot.total 
	}
}
summary(allom.rel[,1:10])
summary(valles.allom[,1:10])

# This is the same loop as we had to get things to the plot level, but now we're dividing by the 
# number of plots to get a mean value for the site
for(p in unique(valles.site.proj$project)){
	n.plots <- ifelse(p=="Ross", 2, 4)

	for(s in unique(valles.site.proj$site)){
		trees.site <- which(all.valles$project==p & all.valles$site==s)
		allom.total <- apply(valles.allom[trees.site,]/n.plots,2,sum, na.rm=T)

	for(d in unique(valles.site.proj$dbh.bin)){
		trees   <- which(all.valles$project==p & all.valles$dbh.bin==d & all.valles$site==s)
		out.row <- which(valles.site.proj$project==p & valles.site.proj$dbh.bin==d & valles.site.proj$site==s)
		allom.temp <- valles.allom[trees,]/n.plots
		allom.temp.rel <- allom.rel[trees,]/n.plots # this is subsetting the relative biomass we calculated above the 
		if(length(trees)>1){
			out.mean  <-     mean(apply(allom.temp,2,sum, na.rm=T), na.rm=T)
			out.ci.lo <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.025, na.rm=T)
			out.ci.hi <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.975, na.rm=T)

			out.mean.rel  <-     mean(apply(allom.temp.rel,2,sum, na.rm=T), na.rm=T)
			out.ci.lo.rel <- quantile(apply(allom.temp.rel,2,sum, na.rm=T), 0.025, na.rm=T)
			out.ci.hi.rel <- quantile(apply(allom.temp.rel,2,sum, na.rm=T), 0.975, na.rm=T)
		} else if(length(trees)==1){
			out.mean  <-     mean(allom.temp, na.rm=T)
			out.ci.lo <- quantile(allom.temp, 0.025, na.rm=T)
			out.ci.hi <- quantile(allom.temp, 0.975, na.rm=T)

			out.mean.rel  <-     mean(allom.temp.rel, na.rm=T)
			out.ci.lo.rel <- quantile(allom.temp.rel, 0.025, na.rm=T)
			out.ci.hi.rel <- quantile(allom.temp.rel, 0.975, na.rm=T)
		} else {
			out.mean  <- 0
			out.ci.lo <- 0
			out.ci.hi <- 0

			out.mean.rel  <- 0
			out.ci.lo.rel <- 0
			out.ci.hi.rel <- 0
		}		

		valles.site.proj[out.row,"BM.mean.allom"]  <- out.mean
		valles.site.proj[out.row,"BM.ci.lo.allom"] <- out.ci.lo
		valles.site.proj[out.row,"BM.ci.hi.allom"] <- out.ci.hi

		valles.site.proj[out.row,"BM.mean.allom.rel"]  <- out.mean.rel
		valles.site.proj[out.row,"BM.ci.lo.allom.rel"] <- out.ci.lo.rel
		valles.site.proj[out.row,"BM.ci.hi.allom.rel"] <- out.ci.hi.rel
	}

	}
}
summary(valles.site.proj)



# Now we need to do some subsetting to get the data in a way that easily makes a 3-paneled figure
valles.bm <- data.frame( 	site=c(paste(valles.site.proj$site),paste(valles.site.proj$site)), 
						  dbh.bin=c(paste(valles.site.proj$dbh.bin),paste(valles.site.proj$dbh.bin)),
						  project=c(paste(valles.site.proj$project),paste(valles.site.proj$project)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.proj)),rep("Allom", nrow(valles.site.proj))),
						  response="Biomass",
						  Mean=c(valles.site.proj$BM.mean, valles.site.proj$BM.mean.allom),
						  CI.lo=c(valles.site.proj$BM.ci.lo.plot, valles.site.proj$BM.ci.lo.allom),
						  CI.hi=c(valles.site.proj$BM.ci.hi.plot, valles.site.proj$BM.ci.hi.allom)
						  )


valles.bm.rel <- data.frame( site=c(paste(valles.site.proj$site),paste(valles.site.proj$site)), 
						  dbh.bin=c(paste(valles.site.proj$dbh.bin),paste(valles.site.proj$dbh.bin)),
						  project=c(paste(valles.site.proj$project),paste(valles.site.proj$project)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.proj)),rep("Allom", nrow(valles.site.proj))),
						  response="Relative Biomass",
						  Mean=c(valles.site.proj$BM.mean.plot.rel, valles.site.proj$BM.mean.allom.rel),
						  CI.lo=c(valles.site.proj$BM.ci.lo.plot.rel, valles.site.proj$BM.ci.lo.allom.rel),
						  CI.hi=c(valles.site.proj$BM.ci.hi.plot.rel, valles.site.proj$BM.ci.hi.allom.rel)
						  )

valles.dens <- data.frame( site=c(paste(valles.site.proj$site)), 
						  dbh.bin=c(paste(valles.site.proj$dbh.bin)),
						  project=c(paste(valles.site.proj$project)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.proj))),
						  response="Density",
						  Mean=c(valles.site.proj$tree.density),
						  CI.lo=c(valles.site.proj$density.ci.lo.plot),
						  CI.hi=c(valles.site.proj$density.ci.hi.plot)
						  )
valles.site <- rbind(valles.bm, valles.bm.rel, valles.dens)
valles.site$response.order <- recode(valles.site$response, "'Density'='1'; 'Biomass'='2'; 'Relative Biomass'='3'")
levels(valles.site$response.order) <- c("Density (stems/m2)", "Biomass (kg/m2)", "Relative Biomass (%)")
valles.site$site.common <- recode(valles.site$site, "'VUF'='1'; 'VLF'='2'")
levels(valles.site$site.common) <- c("Upper", "Lower")
summary(valles.site)

pdf("figures/Size_Distribution_Uncertainty_Comparison_RossOnly.pdf")
ggplot(data=valles.site[valles.site$project=="Ross",]) + facet_grid(response.order ~ site, scales="free") +
	geom_bar(aes(x=dbh.bin, y=Mean, fill= uncertainty.type), breaks=dbh.bins[1:(length(dbh.bins)-1)], fill="gray50", stat="identity", position="dodge") +
	geom_linerange(aes(x=dbh.bin, ymin=CI.lo, ymax= CI.hi, color=uncertainty.type), position=position_dodge(width=0.85), size=2, alpha=0.9) +
	scale_fill_manual(values=c("gray50", "gray50")) +
	scale_color_manual(values=c("blue", "red2"))+
	labs(x="DBH (cm)", y="", title="") +
	theme_bw() +	theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# --------------------
# Pulling out the Biomass panel for ESA poster
# --------------------
ggplot(data=valles.site[valles.site$project=="Ross" & valles.site$response.order=="Biomass (kg/m2)",]) +
	facet_grid(site.common~.) +
	geom_bar(aes(x=dbh.bin, y=Mean, fill= uncertainty.type), breaks=dbh.bins[1:(length(dbh.bins)-1)], fill="gray50", stat="identity", position="dodge") +
	geom_linerange(aes(x=dbh.bin, ymin=CI.lo, ymax= CI.hi, color=uncertainty.type), position=position_dodge(width=0.85), size=2, alpha=0.9) +
	scale_fill_manual(values=c("gray50", "gray50")) +
	scale_color_manual(name="Uncertainty",values=c("blue", "red2"))+
	labs(x="DBH (cm)", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")")))) +
	poster.theme2 +
	theme(legend.position=c(0.2,0.85))+
	scale_x_discrete(labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60"))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm"))
	#theme_bw() +	theme(axis.text.x = element_text(angle = 45, hjust = 1))


pdf("figures/Size_Distribution_Uncertainty_Comparison_RossMarcy.pdf")
ggplot(data=valles.site[,]) + facet_grid(response.order ~ site, scales="free") +
	geom_bar(aes(x=dbh.bin, y=Mean, fill=project), breaks=dbh.bins[1:(length(dbh.bins)-1)], stat="identity", position="dodge") +
	geom_linerange(data=valles.site[,], aes(x=dbh.bin, ymin=CI.lo, ymax= CI.hi, linetype=project, color=uncertainty.type), position=position_dodge(width=0.85), size=1, alpha=0.9) +
	scale_linetype_manual(values=c("solid", "solid")) +
	scale_fill_manual(values=c("gray33", "gray66")) +
	scale_color_manual(values=c("blue", "red"))+
	labs(x="DBH (cm)", y="", title="") +
	theme_bw() +	theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# -------------------------------------
# aggregating to the site level ignoring whose project the data is coming from
# -------------------------------------
# --------------------
# Aggregating to the site level by site
# --------------------
# Finding the plot-based mean
valles.plot$n.plot <- ifelse(valles.plot$site=="Ross", 2, 4)
summary(valles.plot)


valles.site.all <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$site), FUN=mean, na.rm=T)
names(valles.site.all)[1:2] <- c("dbh.bin", "site")
summary(valles.site.all)

# finding the plot-based 02.5%˚
valles.site.all2 <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$site), FUN=quantile, 0.025, na.rm=T)
names(valles.site.all2)[1:2] <- c("dbh.bin", "site")
summary(valles.site.all2)

# finding the plot-based 97.5%˚
valles.site.all3 <- aggregate(valles.plot[,c("tree.density", "BM.mean", "BM.mean.plot.rel")], by=list(valles.plot$dbh.bin, valles.plot$site), FUN=quantile, 0.975, na.rm=T)
names(valles.site.all3)[1:2] <- c("dbh.bin", "site")
summary(valles.site.all3)

valles.site.all$density.ci.lo.plot <- valles.site.all2$tree.density
valles.site.all$density.ci.hi.plot <- valles.site.all3$tree.density
valles.site.all$BM.ci.lo.plot <- valles.site.all2$BM.mean
valles.site.all$BM.ci.hi.plot <- valles.site.all3$BM.mean
valles.site.all$BM.ci.lo.plot.rel <- valles.site.all2$BM.mean.plot.rel
valles.site.all$BM.ci.hi.plot.rel <- valles.site.all3$BM.mean.plot.rel
summary(valles.site.all)


# ---------------
# Adding in the biomass estimate & 95% ci directly from allometry
# NOTE: relative biomass is inherently plot-based and cannot be done properly at the site level 
# (or not if we want to compare it with plot-based uncertainty) so we need to calculate 
# the relative plot biomass for each tree in the allometric data frame
allom.rel <- valles.allom
for(p in unique(valles.plot$plot.id)){
	trees.plot <- which(all.valles$plot.id==p)
	plot.total <- apply(allom.rel[trees.plot,],2,sum, na.rm=T) # total plot biomass for each iteration

	# calculating the relative tree biomass; there should be a way to do this a plot at a time,
	# but I had trouble getting it working
	for(i in trees.plot){ 
		allom.rel[i,] <- allom.rel[i,]/plot.total 
	}
}
summary(allom.rel[,1:10])
summary(valles.allom[,1:10])

n.plots <- 6
for(s in unique(valles.site.all$site)){
	trees.site <- which(all.valles$site==s)
	allom.total <- apply(valles.allom[trees.site,]/n.plots,2,sum, na.rm=T)

	for(d in unique(valles.site.all$dbh.bin)){
		trees   <- which(all.valles$dbh.bin==d & all.valles$site==s)
		out.row <- which(valles.site.all$dbh.bin==d & valles.site.all$site==s)
		allom.temp <- valles.allom[trees,]/n.plots
		allom.temp.rel <- allom.rel[trees,]/n.plots
		if(length(trees)>1){
			out.mean  <-     mean(apply(allom.temp,2,sum, na.rm=T), na.rm=T)
			out.ci.lo <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.025, na.rm=T)
			out.ci.hi <- quantile(apply(allom.temp,2,sum, na.rm=T), 0.975, na.rm=T)

			out.mean.rel  <-     mean(apply(allom.temp.rel,2,sum, na.rm=T), na.rm=T)
			out.ci.lo.rel <- quantile(apply(allom.temp.rel,2,sum, na.rm=T), 0.025, na.rm=T)
			out.ci.hi.rel <- quantile(apply(allom.temp.rel,2,sum, na.rm=T), 0.975, na.rm=T)
		} else if(length(trees)==1){
			out.mean  <-     mean(allom.temp, na.rm=T)
			out.ci.lo <- quantile(allom.temp, 0.025, na.rm=T)
			out.ci.hi <- quantile(allom.temp, 0.975, na.rm=T)

			out.mean.rel  <-     mean(allom.temp.rel, na.rm=T)
			out.ci.lo.rel <- quantile(allom.temp.rel, 0.025, na.rm=T)
			out.ci.hi.rel <- quantile(allom.temp.rel, 0.975, na.rm=T)
		} else {
			out.mean  <- 0
			out.ci.lo <- 0
			out.ci.hi <- 0

			out.mean.rel  <- 0
			out.ci.lo.rel <- 0
			out.ci.hi.rel <- 0
		}		

		valles.site.all[out.row,"BM.mean.allom"]  <- out.mean
		valles.site.all[out.row,"BM.ci.lo.allom"] <- out.ci.lo
		valles.site.all[out.row,"BM.ci.hi.allom"] <- out.ci.hi

		valles.site.all[out.row,"BM.mean.allom.rel"]  <- out.mean.rel
		valles.site.all[out.row,"BM.ci.lo.allom.rel"] <- out.ci.lo.rel
		valles.site.all[out.row,"BM.ci.hi.allom.rel"] <- out.ci.hi.rel
	}
}
summary(valles.site.all)


valles.bm2 <- data.frame( site=c(paste(valles.site.all$site),paste(valles.site.all$site)), 
						  dbh.bin=c(paste(valles.site.all$dbh.bin),paste(valles.site.all$dbh.bin)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.all)),rep("Allom", nrow(valles.site.all))),
						  response="Biomass",
						  Mean=c(valles.site.all$BM.mean, valles.site.all$BM.mean.allom),
						  CI.lo=c(valles.site.all$BM.ci.lo.plot, valles.site.all$BM.ci.lo.allom),
						  CI.hi=c(valles.site.all$BM.ci.hi.plot, valles.site.all$BM.ci.hi.allom)
						  )


valles.bm.rel2 <- data.frame( site=c(paste(valles.site.all$site),paste(valles.site.all$site)), 
						  dbh.bin=c(paste(valles.site.all$dbh.bin),paste(valles.site.all$dbh.bin)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.all)),rep("Allom", nrow(valles.site.all))),
						  response="Relative Biomass",
						  Mean=c(valles.site.all$BM.mean.plot.rel, valles.site.all$BM.mean.allom.rel),
						  CI.lo=c(valles.site.all$BM.ci.lo.plot.rel, valles.site.all$BM.ci.lo.allom.rel),
						  CI.hi=c(valles.site.all$BM.ci.hi.plot.rel, valles.site.all$BM.ci.hi.allom.rel)
						  )

valles.dens2 <- data.frame( site=c(paste(valles.site.all$site)), 
						  dbh.bin=c(paste(valles.site.all$dbh.bin)),
						  uncertainty.type=c(rep("Density", nrow(valles.site.all))),
						  response="Density",
						  Mean=c(valles.site.all$tree.density),
						  CI.lo=c(valles.site.all$density.ci.lo.plot),
						  CI.hi=c(valles.site.all$density.ci.hi.plot)
						  )
summary(valles.bm2)
summary(valles.dens2)
valles.all <- rbind(valles.bm2, valles.bm.rel2, valles.dens2)
valles.all$response.order <- recode(valles.all$response, "'Density'='1'; 'Biomass'='2'; 'Relative Biomass'='3'")
levels(valles.all$response.order) <- c("Density (stems/m2)", "Biomass (kg/m2)", "Relative Biomass (%)")
summary(valles.all)

pdf("figures/Size_Distribution_Uncertainty_Comparison_AllTogether.pdf")
ggplot(data=valles.all[,]) + facet_grid(response.order ~ site, scales="free") +
	geom_bar(aes(x=dbh.bin, y=Mean), breaks=dbh.bins[1:(length(dbh.bins)-1)], fill="gray50", stat="identity", position="dodge") +
	geom_linerange(aes(x=dbh.bin, ymin=CI.lo, ymax= CI.hi, color=uncertainty.type), position=position_dodge(width=0.85), size=2, alpha=0.9) +
	scale_fill_manual(values=c("gray50", "gray50")) +
	scale_color_manual(values=c("blue", "red2"))+
	labs(x="DBH (cm)", y="", title="") +
	theme_bw() +	theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
