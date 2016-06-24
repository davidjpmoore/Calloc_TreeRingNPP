##################################################################################
## Basic Components necessary for data management for doing dendro modelling
##################################################################################

# clear memory
rm(list=ls())

# importing libraries
library(dplR)
library(lattice)

# Getting Libraries
library(reshape)
library(car)
library(mgcv)
library(nlme)
library(lmeSplines)
#library(lme4)
library(splines)
library(MASS)
library(MuMIn)
library(ggplot2)
library(grid)
se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))


#################################################################################################
# STEP 1: Gap-filling measured trees
# STEP 1b: Pith-correction in measured trees (useful for stand dynamics; won't do unless you ask for it)
# STEP 2: Gap-filling missing trees
#
# BIG SELLING POINT OF THIS APPROACH: we can quantify different levels of uncertainty & variability
# Caveats: fitting the initial GAMM is very time-intensive (it may take hours with your full data) because current form fits a spline to each tree in a mixed model framework

#################################################################################################
# Previous workflow
# 1) Read in RWL, QA/QC
# 3) Aggregate to tree (factoring in whether cores were dated) level & decide if an entire tree is dated or not -- WRITE THIS AS A FILE!
# 4) Stack RWL & Merge with metadata (becomes "ring.data" or named equivalent)


# Ring.data format: stack all of the core BAI, so that data frame with a SIGNLE BAI column, and then all of the factors in other columns
# NOTE: as you go along, this may have
ring.data <- read.csv("processed_data/DOE_Har_how_combo_may2016_TreeRWL_AllSites_stacked.csv", header=T)
ring.data$Tree     <- as.factor(ring.data$Tree) 
ring.data$Measured <- as.factor("YES") # Adding a flag that will help us keep things straight down the line
summary(ring.data)

# -------------------------
# NOTE: ONLY DO THIS STEP IF YOU'RE ADDING NEW SITES TO WHAT'S ALREADY BEEN GAP-FILLED
# -------------------------
# These are the data you've already gapfilled (so if you're running this for the first time, it won't exist)
rings.filled <- read.csv("processed_data/RingData_All_Gapfilled.csv")
summary(rings.filled)
# -------------------------


# Tree Data
tree.data <- read.csv("raw_input_files/harvard_howland_may2016_tree_metadata.csv")
summary(tree.data)

# Site Data (for year cored) 
Site.data <- read.csv("raw_input_files/DOE_plus_valles.csv", na.strings="")
Site.data$Year.sample <- as.numeric(substr(Site.data$date.sample,7,10))
Site.data <- Site.data[!is.na(Site.data$PlotID),] # something was getting read in weird, so lets get rid of any false rows
summary(Site.data)

 # merging in the year sampled into the tree data & calculating age
tree.data <- merge(tree.data, Site.data[,c("PlotID", "Year.sample")], all.x=T, all.y=F)
tree.data$PlotID


# Filling some sort of age estimate
summary(tree.data)
summary(ring.data)
tree.data$Age <- NA
for(t in unique(tree.data$TreeID)[unique(tree.data$TreeID) %in% unique(ring.data$TreeID)]){
	# Some trees are missing ring widths because they weren't dated & Alex is as cautious as Ross, 
	# so lets skip those
	if(length(ring.data[ring.data$TreeID==t & !is.na(ring.data$RW), "Year"])==0) next 
	
	pith.est <- ifelse(!is.na(tree.data[tree.data$TreeID==t, "Pith"]), tree.data[tree.data$TreeID==t, "Pith"], min(ring.data[ring.data$TreeID==t & !is.na(ring.data$RW), "Year"], na.rm=T))
	tree.data[tree.data$TreeID==t, "Age"] <- tree.data[tree.data$TreeID==t, "Year.sample"] - pith.est 
}
# tree.data$Age <- ifelse(!is.na(tree.data$Pith), tree.data$Year.sample - tree.data$Pith, tree.data$Year.sample - )
summary(tree.data)

# We're going to run 2 sets of fillin models:
# 1) Model based on only DATED trees (m1d)
# 2) Model based on both DATED and UNDATED trees (m1u)

trees.dated <- ring.data[ring.data$Dated=="Y","TreeID"]

# using the gamm allows us to fit a smoothing spline to each tree, which allows us to basically gapfill taking into account unique tree temporal trends
#	current spline parameter: shrinkage version of cubic spline with 3 knots (stiff CR spline)
#	when we fit a generalized version for missing trees, we'll have to decide what to fit it to instead of TreeID; I think probably species|plot
# m1 <- gamm(RW ~ s(Year, bs="cs", k=3) + species + DBH..cm., random=list(Site=~1, PlotID=~1, TreeID=~1), data=ring.data, na.action=na.omit)

# ----------------------------------------------------------------
# IDEAL MODEL FORM (it won't work for many reasons)
#	-- won't predict outside range of years observed on each core
#	-- end up with singularity issues
#	-- would take FOREVER to fit even if it did work
# m1d <- gamm(RW ~ s(Year, bs="cs", k=3, by=TreeID) + species*DBH..cm.*canopy.class, random=list(Site=~1, PlotID=~1, TreeID=~1), data=trees.dated.full, na.action=na.omit)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# The spline doesn't fit outside the range of observed values, so we need to give it a "null" guess
# As a very very rough guess right now, filling missing with the measurement from the oldest ring
# 1) Create rough size-age relationships to give a narrow window of rings to fill (i.e. don't fill a 10 cm oak back to 1900 if our rings stop in 1980)
# 2) fill the modeling window with non-0 values... perhaps mean growth from last decade or past observed trend?

# ---------------------------------------
# 1) Create rough size-age relationships to give a narrow window of rings to fill (i.e. don't fill a 10 cm oak back to 1900 if our rings stop in 1980)
# ---------------------------------------
# What we need: core summary data (DBH..cm., estimated pith date)

# Ignoring all the Sites we don't have and doing some exploratory graphing
tree.data2 <- tree.data[tree.data$PlotID %in% unique(ring.data$PlotID),]
summary(tree.data2)

# Need to remove species for which we have no pith estimates for the time being
Species.pith <- unique(tree.data2[!is.na(tree.data2$Pith), "Species"])
tree.data3 <- tree.data2[tree.data2$Species %in%  Species.pith,]
summary(tree.data3)

qplot(DBH..cm., Age, color=Canopy.Class, data=tree.data3) + facet_wrap(~Species) +
	stat_smooth(method="lm", alpha=0.5, size=1.5) +
	theme_bw()
qplot(DBH..cm., Age, color=Species, data=tree.data3) + facet_wrap(~Species) +
	stat_smooth(method="lm", alpha=0.5, size=1.5) +
	theme_bw()


# Making a very basic linear model looking at Site-specific species-DBH..cm.-age relationships
# add *Site-1 when we have more than 1 site

dbh.age <- lm(Age ~ Species*DBH..cm., data=tree.data3)
summary(dbh.age)
summary(dbh.age)$r.squared # Note, this very basic model works pretty well!

# Using the prediction interval to get us a higher upper bound
age.pi <- predict(dbh.age, newdata=tree.data3, interval="predict")
summary(age.pi)
dim(age.pi); dim(tree.data3) # Making sure we didn't lose any rows along the way

tree.data3 <- cbind(tree.data3, age.pi)
summary(tree.data3)

# Setting the filling window to the upper p.i. limit
tree.data3$fill.year <- ifelse(is.na(tree.data3$Pith), tree.data3$Year.sample-tree.data3$upr, tree.data3$Pith)
summary(tree.data3)


# Merging this back into a data frame that contains info for all the trees we're modeling right now
tree.data.model <- merge(tree.data3, tree.data2, all.x=T, all.y=T)
summary(tree.data.model)

#--------------------- 
# QUESTION: what to do about the species with no pith estimates?
#	For now, I'm just going to do a species-naive fit within each plot
#--------------------- 
dbh.age.plot <- lm(Age ~ DBH..cm.*PlotID-1, data=tree.data.model)
summary(dbh.age.plot) 

age.pi.plot <- predict(dbh.age.plot, newdata= tree.data.model, interval="predict")
summary(age.pi.plot)
dim(age.pi.plot); dim(tree.data.model)

summary(tree.data.model)
# Filling missing fill.year with one caluclated form age.pi.plot fill 
tree.data.model[is.na(tree.data.model$fill.year),"fill.year"] <- tree.data.model[is.na(tree.data.model$fill.year),"Year.sample"] - age.pi.plot[which(is.na(tree.data.model$fill.year)),3]
summary(tree.data.model)
#--------------------- 
# ---------------------------------------

# ---------------------------------------
# Delete NAs for years way outside what we think we should be fitting -- helps constrain the model fitting
# ---------------------------------------
summary(tree.data.model)
summary(ring.data)
dim(ring.data)

# get rid of ring data for trees missing metadata
ring.data <- ring.data[ring.data$TreeID %in% unique(tree.data.model$TreeID),]

for(i in unique(ring.data$TreeID)){
	#------------------------------
	#year.fill = the oldest year to fill based on above step (prediction interval, size-species-Site relationships)
	#------------------------------
	yr.fill <- tree.data.model[tree.data.model$TreeID==i,"fill.year"]
	
	# If for some reason we're not able to create an estimated range of years we
	# could potentially have, just use whatever the oldest fill year there is
	# (this is not a great way to do it, but it keeps things from breaking)
	if(is.na(yr.fill)) yr.fill <- min(tree.data.model$fill.year, na.rm=T)
	#------------------------------

	#------------------------------
	# The actual insertion of the dummy fil value into the fill range
	#------------------------------
	ring.data <- ring.data[!(ring.data$TreeID==i & ring.data$Year<yr.fill),]
	#------------------------------
}
summary(ring.data)
dim(ring.data)
# ---------------------------------------
# ----------------------------------------------------------------

# ################################################################
# ################################################################
# RUNNING THE GAMM!!
# ################################################################
# ################################################################
# A generalized additive mixed model (gamm) allows us to fit splines in a mixed model context
# we can let these splines vary by tree which essentially detrends the core
# here's we're using our dummy-filled ring widths as a response so that the spline will fit over the whole time period of interest
# ################################################################

# ----------------------------------------------------------------
# Gapfilling trees for which we have at least some measurements
# ----------------------------------------------------------------
source("0_gapfill_gamm_function.R") # This is where the generalized function that runs the gamm & saves the diagnostic plots is


# ---------------------------------------
# Loop through by Site using measured trees only
# ---------------------------------------
ring.data$RW.modeled <- NA # making a placeholder vector that we're going to fill in

# -------------------------
# NOTE: ONLY DO THIS STEP IF YOU'RE ADDING NEW SITES TO WHAT'S ALREADY BEEN GAP-FILLED
# -------------------------
# Merging the already gapfilled & the new series together
ring.data <- merge(ring.data[!(ring.data$TreeID %in% rings.filled$TreeID), ], rings.filled, all.x=T, all.y=T)
summary(ring.data)
# -------------------------


# ---------------
# Select your site codes
# PAY ATTENTION HERE: YOU HAVE A CHOICE TO MAKE!
# ---------------
sites.ring.data <- unique(substr(ring.data$PlotID,1,2))
# summary(ring.data[is.na(ring.data$PlotID),])

# If starting from scratch:
site.codes <- sites.ring.data

# If adding new sites select those that haven't already been gapfilled
site.codes <- sites.ring.data[!(sites.ring.data %in% unique(substr(rings.filled$PlotID,1,2)))]
# ---------------
# Adding in a column to have species by plot
ring.data$spp.plot <- as.factor(paste(ring.data$Species, ring.data$PlotID, sep="."))
summary(ring.data)

# Something got off, so lets re-order our data
ring.data <- ring.data[order(ring.data$PlotID, ring.data$TreeID, ring.data$Year),]

# Running the gapfilling loop
for(s in site.codes){
	rows.site <- which(substr(ring.data$PlotID,1,2)==s) # figuring out which rows belong to a given site

	data.use <- ring.data[rows.site,] # subset the data for each plot
	data.use <- droplevels(data.use) # git rid of levels for factors like Species that no longer exist

	out.path <- paste0("processed_data/GapFill_Metadata/", s)

	# The ifelse is no longer necessary for our data, but an example of how to select special options  based on certain sites
	# if(substr(s,1,1)=="V" | substr(s,1,1)=="N"){ # This is no longer necessary, but an example of how to select special options  based on certain sites
		# gamm.out <- gapfill.gamm(data= data.use, DBH="DBH..cm.", Species.Use="Species", Canopy.Class="Canopy.Class", canopy=F, out.prefix=out.path)
	# } else {
#		gamm.out <- gapfill.gamm(data= data.use, smooth.by="spp.plot", DBH="DBH..cm.", Species.Use="Species", Canopy.Class="Canopy.Class", canopy=T, out.prefix=out.path)
		gamm.out <- gapfill.gamm(data= data.use, DBH="DBH..cm.", Species.Use="Species", smooth.by="spp.plot", Canopy.Class="Canopy.Class", canopy=T, out.prefix=out.path)

	# }
	ring.data[rows.site, "RW.modeled"] <- gamm.out$data$RW.modeled
}

# NOTE: at the end of this step, there should be no NAs in ring.data$RW.modeled
summary(ring.data)

save(ring.data, file="processed_data/may2016_ring_data_gapfill.Rdata")
load("processed_data/may2016_ring_data_gapfill.Rdata")

# ----------------------------------------------------------------
# Gapfilling trees that we have no measurements for (punky, dead...)
# ----------------------------------------------------------------
# Going to gapfill live trees only (won't try and figure out when dead trees welcome)
# We can't fit a TreeID spline for trees we don't have any measurements for, so we need something more generalizeable
#	The best options are probably species or PlotID; Ross votes PlotID because of variation in the Valles
# ----------------------------------------------------------------
# creating a data frame of blank ring widths to fill with the gamm
trees.missing <- tree.data.model[!(tree.data.model$TreeID %in% unique(ring.data$TreeID)), c("Species", "Site", "PlotID", "TreeID", "DBH..cm.", "Canopy.Class", "Live.Dead")]

# Harvard and Howland had CWD intermingled with their tree data.  We are removing anything without a species as it was likely a decayed log.
trees.missing <- trees.missing[!is.na(trees.missing$Species),]
summary(trees.missing)

summary(trees.missing) 
summary(trees.missing[trees.missing$Site=="Harvard", ])
dim(trees.missing) # number pf rows = number of missing trees

# Creating a vector of years and dummy ring widths we're going to want to fill
rw.dummy <- data.frame(Year=min(ring.data$Year):max(ring.data$Year), RW=NA)
summary(rw.dummy)

# making a data frame with blank rings widths for the full range of years for missing trees
trees.missing <- merge(trees.missing, rw.dummy, all.x=T, all.y=T)
summary(trees.missing)
trees.missing$Measured <- as.factor("NO")

# ---------------------------------------
# delete NAs for years way outside what we think we should be fitting (this was done earlier)
# ---------------------------------------
summary(tree.data.model)
summary(trees.missing)
dim(trees.missing)

for(i in unique(trees.missing$TreeID)){
	#------------------------------
	#year.fill = the oldest year to fill based on above step (prediction interval, size-species-Site relationships)
	#------------------------------
	yr.fill <- tree.data.model[tree.data.model$TreeID==i,"fill.year"]
	# If for some reason we're not able to create an estimated range of years we
	# could potentially have, just use whatever the oldest fill year there is
	# (this is not a great way to do it, but it keeps things from breaking)
	if(is.na(yr.fill)) yr.fill <- min(tree.data.model$fill.year, na.rm=T)
	
	#------------------------------

	#------------------------------
	# The actual insertion of the dummy fil value into the fill range
	#------------------------------
	trees.missing <- trees.missing[!(trees.missing$TreeID==i & trees.missing$Year<yr.fill),]
	#------------------------------
}
summary(trees.missing)

# ----------------------------------------------------------------
# Creating a gamm for missing trees
#	Because we don't have any ring measurements for missing trees, we can't use a TreeID spline (because we can't predict what we can't fit)
# 	After talking with Ross, we decided to fit the spline by plot, which should get us the general plot dynamics and the hierarchical effects should help fill in the species; The fixed DBH..cm. effect will also help adjust growth based on size; the alterative would be to fit by species or something like that if we think the signal is more regional rather than driven by gap dynamics
# ----------------------------------------------------------------

# putting missing & measured trees together
summary(tree.data)
summary(trees.missing)
dim(ring.data)
dim(trees.missing)

data.all <- merge(ring.data, trees.missing[,c("Species", "Site", "PlotID", "TreeID", "DBH..cm.", "Canopy.Class", "Live.Dead", "Year", "RW", "Measured")], all.x=T, all.y=T )
summary(data.all)
dim(data.all)

# NA's will be here now as we have merged in the missing trees


# Do the missing tree gapfilling based off of dated trees only or species that have no dated trees
species.keep <- unique(data.all$Species)[!(unique(data.all$Species) %in% unique(data.all[!data.all$Dated=="N","Species"]))] # exceptions to the dated only rule

fill.missing <- data.all[(data.all$Species %in% species.keep) | !(data.all$Dated=="N") | data.all$Measured=="NO",] # This will subset completely missing trees and the dated
summary(fill.missing)

fill.missing$Species.Model <- recode(fill.missing$Species, "'POTR'='POGR'") # there are no measurements for these species, so we have to pretend they're something else
summary(fill.missing)

out.path <- paste0("processed_data/GapFill_Metadata/", "MissingTrees")

# Run the GAMM for the missing trees; right now we're smoothing by individual plot
# gamm.missing <- gapfill.gamm(data=fill.missing, DBH="DBH..cm.", Species.Use="Species.Model", Canopy.Class="Canopy.Class", smooth.by="PlotID", canopy=T, out.prefix=out.path)

# Running site by site, because something isn't right
for(s in site.codes){
	rows.site <- which(substr(fill.missing$PlotID,1,2)==s) # figuring out which rows belong to a given site

	data.use <- fill.missing[rows.site,] # subset the data for each plot
	data.use <- droplevels(data.use) # git rid of levels for factors like Species that no longer exist

	out.path <- paste0("processed_data/GapFill_Metadata/", "MissingTrees")

	# The ifelse is no longer necessary for our data, but an example of how to select special options  based on certain sites
	# if(substr(s,1,1)=="V" | substr(s,1,1)=="N"){ # This is no longer necessary, but an example of how to select special options  based on certain sites
		# gamm.out <- gapfill.gamm(data= data.use, DBH="DBH..cm.", Species.Use="Species", Canopy.Class="Canopy.Class", canopy=F, out.prefix=out.path)
	# } else {
#		gamm.out <- gapfill.gamm(data= data.use, smooth.by="spp.plot", DBH="DBH..cm.", Species.Use="Species", Canopy.Class="Canopy.Class", canopy=T, out.prefix=out.path)
		gamm.out <- gapfill.gamm(data= data.use, DBH="DBH..cm.", Species.Use="Species.Model", smooth.by="PlotID", Canopy.Class="Canopy.Class", canopy=T, out.prefix=out.path)

	# }
	fill.missing[rows.site, "RW.modeled"] <- gamm.out$data$RW.modeled
}


summary(fill.missing)

# Merge
dim(data.all)
summary(data.all[is.na(data.all$Dated),])
dim(fill.missing)

vars.merge <- c("TreeID", "Year", "PlotID", "RW.modeled")

data.all2 <- merge(data.all[!is.na(data.all $RW.modeled),], fill.missing[fill.missing$Measured=="NO",vars.merge], all.x=T, all.y=T)
summary(data.all2) # Sanity Check: make sure there are no NAs in RW.modeled
dim(data.all2); dim(data.all)

##################################################################################
# Create a gapfilled data set
# Now we have a lot of different piece meal data sets that we need to put back together to have estimated or measured ring widths for everything
##################################################################################
data.all2$RW.gapfilled <- ifelse(is.na(data.all2$RW), data.all2$RW.modeled, data.all2$RW)
summary(data.all2)

plots <- unique(data.all2$PlotID)
plots <- plots[!is.na(plots)]
#summary(Site.data)

# Get rid of estimated ring widths for years beyond when the tree was cored (i.e. trees cored in 2011 should not have estimates for 2014)
for(p in plots){
	yrs.na <- which(data.all2$PlotID==p & data.all2$Year>Site.data[Site.data$PlotID==p, "Year.sample"])
	data.all2[yrs.na,"RW.gapfilled"] <- NA
}
summary(data.all2) # This should introduce a handful of NAs back in 

write.csv(data.all2, "processed_data/DOE_AllSites_may2016_Gapfilled.csv", row.names=F)
# ----------------------------------------------------------------



