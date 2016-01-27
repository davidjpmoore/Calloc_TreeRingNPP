library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}
rm(list=ls())


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

#load in core details data sheet.  Has living/dead, pith info, measurement info.
#loading the dplR to use the basal area reconstruction functions.
core.data <- read.csv("raw_input_files/Core_data_01202014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
names(core.data)
core.data$plot <- substr(core.data$plot.id, 3, 3)
core.data$plot <- as.factor(core.data$plot)

summary(core.data)

# Doing some stuff to Canopy Class to make our lives easier
#   1) Assume all Valles & Niwot trees are co-dominant
#   2) give all dead trees without an existing canopy class a "SNAG" class
core.data$canopy.class <- as.factor(ifelse(core.data$live.dead=="DEAD" & is.na(core.data$canopy.class), "SNAG", 
									ifelse(substr(core.data$TreeID,1,1)=="V" | substr(core.data$TreeID,1,1)=="N", "C", 
									paste(core.data$canopy.class)))) # Make a dead canopy class)
summary(core.data)
write.csv(core.data, file="processed_data/core_data.csv", row.names=F)

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
tree.data <- read.csv("raw_input_files/tree_metadata_DOE_plus_valles.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
names(tree.data)
tree.data$plot <- substr(tree.data$PlotID, 3, 3)
tree.data$plot <- as.factor(tree.data$plot)
summary(tree.data)

# Doing some stuff to Canopy Class to make our lives easier
#   1) Assume all Valles & Niwot trees are co-dominant
#   2) give all dead trees without an existing canopy class a "SNAG" class
tree.data$Canopy.Class <- as.factor(ifelse(tree.data$Live.Dead=="DEAD" & is.na(tree.data$Canopy.Class), "SNAG", 
									ifelse(substr(tree.data$TreeID,1,1)=="V" | substr(tree.data$TreeID,1,1)=="N", "C", 
									paste(tree.data$Canopy.Class)))) # Make a dead canopy class)
summary(tree.data)


#importing ring widths of dated samples as an object and making plot a factor since there were two distinct plots.  We may remove this for the nested design.  
#Removing NA's from the files
# NOTE: reading in a single rwl with all measured trees otherwise you're going to need to make sure to change the file paths for EVERYTHING otherwise you overwrite important files and make a lot more work for yourself
core.rw <- read.rwl("RWL/RWL_all_trees.rwl")
summary(core.rw)

#subsetting to just look at the trees from the valles
ross.valles.rw <- core.rw[,substr(names(core.rw),1,1)=="V"]
summary(ross.valles.rw)

#removing the extra character that tellervo adds
names(ross.valles.rw)<-substr(names(ross.valles.rw), 1, 7)
names(ross.valles.rw)

replace.b <- which(substr(names(ross.valles.rw),7,7)=="b") 
names(ross.valles.rw)[replace.b] <- paste0(substr(names(ross.valles.rw)[replace.b], 1,6), "B")

replace.a <- which(substr(names(ross.valles.rw),7,7)=="a") 
names(ross.valles.rw)[replace.a] <- paste0(substr(names(ross.valles.rw)[replace.a], 1,6), "A")
	
names(ross.valles.rw) 
summary(ross.valles.rw)

# subsetting only those cores that sufficiently dated
core.data.dated <- core.data[core.data$dated=="Y" & substr(core.data$CoreID,1,1)=="V",]
summary(core.data.dated)

ross.valles.dated <- ross.valles.rw[,names(ross.valles.rw) %in% core.data.dated$CoreID] 
summary(ross.valles.dated)


# detrending cores to make a composite site index
spag.plot(ross.valles.dated)
dim(ross.valles.dated)

ross.valles.i<- detrend(ross.valles.dated, method="Spline", nyr=30)
min(ross.valles.i, na.rm=T)
summary(ross.valles.i)
dim(ross.valles.i)

# Truncating to 1980 as that is the length of our analysis and 2007 as that is the common overlapping period with Ramzi Touchan's data
ross.valles.i <- ross.valles.i[row.names(ross.valles.i) >= "1980" & row.names(ross.valles.i) <= "2007",]

summary(ross.valles.i)

# first need to separate out the Upper site from the Lower site so that we have individual site chronologies

ross.upper.i <- ross.valles.i[,substr(names(ross.valles.i),1,3)=="VUF"]
names(ross.upper.i)
dim(ross.upper.i)

#excluding trees that do not overlap in this dataframe
tree.exclude <- "VUF105C"


ross.upper.i <- ross.upper.i[,!(names(ross.upper.i) %in% tree.exclude)]
dim(ross.upper.i)


ross.lower.i <- ross.valles.i[,substr(names(ross.valles.i),1,3)=="VLF"]
names(ross.lower.i)

# creating a composite site chronology

ross.upper.cr <- chron(ross.upper.i, prefix="VUF", prewhiten=T)
summary(ross.upper.cr)

ross.lower.cr <- chron(ross.lower.i, prefix="VLF", prewhiten=T)
summary(ross.lower.cr)
row.names(ross.lower.cr)


######################################################
# Loading in external Valles Climate Chronologies
######################################################

climate.upper.valles1 <- read.rwl("external_treering_data/nm586_touchan_PSME_bearcanyonwest_valles.rwl.txt")

# Detrending external climate series

climate.upper.valles1.i <- detrend(climate.upper.valles1, method="Spline", nyrs=30)
min(climate.upper.valles1.i, na.rm=T)
summary(climate.upper.valles1.i)
row.names(climate.upper.valles1.i)



# Truncate series to 1980 to most recent year
climate.upper.valles1.i <- climate.upper.valles1.i[row.names(climate.upper.valles1.i) >= "1980",]

# There are some trees that do not overlap with our 1980-2007 time period AT ALL.  We will need to remove these

summary(climate.upper.valles1.i)
dim(climate.upper.valles1.i)

# Series to exclude
tree.exclude <- c("BCW41", "BCW45", "BCW46", "BCW47", "BCW08A")
climate.upper.valles1.i <- climate.upper.valles1.i[,!(names(climate.upper.valles1.i) %in% tree.exclude)]
dim(climate.upper.valles1.i)

# Create site chronologies from each set of external climate series
climate.upper.valles1.cr <- chron(climate.upper.valles1.i, prefix="BCW", prewhiten=T)
summary(climate.upper.valles1.cr)


######################################################
# Loading in Griffin NADEF CatMesa chronologies
######################################################
# Dan's chronologies are already the residual chronologies from the Catmesa update in 2011

cat.mesa.chron <- read.csv("external_treering_data/Griffin_catmesa_CHRONS.csv", header=T)
summary(cat.mesa.chron)

row.names(cat.mesa.chron) <- cat.mesa.chron$Year
cat.mesa.chron <- cat.mesa.chron[row.names(cat.mesa.chron)>="1980" & row.names(cat.mesa.chron)<= "2007", "CMMt_RES"]
summary(cat.mesa.chron)

head(cat.mesa.chron)

# Loading in sample depth, because Dan already calculated the chronology for me
cat.mesa.depth <- read.csv("external_treering_data/Griffin_catmesa_total.csv", header=T, row.names=1)
summary(cat.mesa.depth)
row.names(cat.mesa.depth)

test <- chron(cat.mesa.depth, prefix="CAT")
summary(test)

cat.mesa.depth <- test[row.names(test)>=1980 & row.names(test)<=2007,]

summary(cat.mesa.depth)
names(cat.mesa.depth) <- c("CATstd", "cat.n")
summary(cat.mesa.depth)
dim(cat.mesa.depth)
###################################################################################################
# Merging all indecies together to form one dataframe on which we can run the climate correlations
###################################################################################################
valles.climate.cr <- cbind(ross.upper.cr, ross.lower.cr, climate.upper.valles1.cr, cat.mesa.chron, cat.mesa.depth[,"cat.n"]) 
names(valles.climate.cr) <- c("vuf.std", "vuf.res", "vuf.n", "vlf.std", "vlf.res", "vlf.n", "bcw.std","bcw.res", "bcw.n","cm.tot", "cm.n")

summary(valles.climate.cr)
head(valles.climate.cr)
# saving as Rdata file to be used for the climate correlations

save(valles.climate.cr, file="processed_data/valles_combined_climate_chronologies.rdata")
