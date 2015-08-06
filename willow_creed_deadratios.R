library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

#load in core details data sheet.  Has living/dead, pith info, measurement info.
#loading the dplR to use the basal area reconstruction functions.
core.data <- read.csv("raw_input_files/Core_data_DOE_summer_2014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
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

willow <- tree.data[substr(tree.data$PlotID, 1, 2)=="WC",]

summary(willow)

# ------------------------
# loading in CWD data to determine stump ratio
# ------------------------
cwd.data <- read.csv("raw_input_files/DOE_CWD_2014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
cwd.data$PlotID <- as.factor(substr(cwd.data$TreeID, 1, 3))
cwd.data$Live.Dead <- as.factor("DEAD")
cwd.data$DBH..cm. <- cwd.data$DBH
summary(cwd.data)

willow.cwd <- cwd.data[substr(cwd.data$TreeID, 1, 2)=="WC",]

summary(willow.cwd)
names(willow)
names(willow.cwd)

willow.all <- merge(willow.cwd, willow, all.x=T, all.y=T)
summary(willow.all)

# Determining stump ratio for willow creek samples
	# Plot A
		WCA <- willow.all[willow.all$PlotID=="WCA",]
		summary(WCA)
		17/58
		length(WCA[WCA$Status=="ST" & !is.na(WCA$Status), "Status"]) / length(unique(WCA$TreeID)) 
		
	# Plot B
		WCB <- willow.all[substr(willow.all$TreeID, 1, 3)=="WCB",]
		summary(WCB)
		27/70
		length(WCB[WCB$Status=="ST" & !is.na(WCB$Status),"Status"]) / length(unique(WCB$TreeID)) 
		
	# Plot C
		WCC <- willow.all[substr(willow.all$TreeID, 1, 3)=="WCC",]
		summary(WCC)
		4/71
		length(WCC[WCC$Status=="ST" & !is.na(WCC$Status), "Status"]) / length(unique(WCC$TreeID)) 

