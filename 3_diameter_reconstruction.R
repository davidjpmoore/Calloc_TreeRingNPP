##################################################################################
# DBH Reconstruction
##################################################################################
library(reshape)
# ---------------------------------------
# DBH..cm. Reconstruction
# ---------------------------------------
# Tree Data
tree.data <- read.csv("processed_data/HAR_HOW_TreeData.csv")
summary(tree.data)

# Site Data (for year cored) 
Site.data <- read.csv("raw_input_files/HAR_HOW_site_data.csv", na.strings="")
Site.data$Year.sample <- as.numeric(substr(Site.data$date.sample,7,10))
summary(Site.data)

# merging in the year sampled into the tree data & calculating age
tree.data <- merge(tree.data, Site.data[,c("PlotID", "Year.sample")], all.x=T, all.y=F)
tree.data$Age <- tree.data$Year.sample - tree.data$Pith
summary(tree.data)

core.data <- read.csv("processed_data/harvard_how.and_core_data2.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the Site the trees belong to
names(core.data)
summary(core.data)

#Load in the gap-filled data
ring.data <- read.csv("processed_data/HAR_HOW_RingData_All_Gapfilled.csv", header=T)
summary(ring.data)

# making a data frame with trees as columns and years as ros
ring.data$Year <- as.factor(ring.data$Year)
trees.gapfilled <- recast(ring.data[,c("Year", "TreeID", "RW.gapfilled")], Year ~ TreeID)
summary(trees.gapfilled)

row.names(trees.gapfilled) <- trees.gapfilled$Year
trees.gapfilled <- trees.gapfilled[,2:ncol(trees.gapfilled)]
trees.gapfilled[(nrow(trees.gapfilled)-10):nrow(trees.gapfilled), 1:10]
# ---------------------------------------

# Ordering the data
trees.gapfilled <- trees.gapfilled[order(row.names(trees.gapfilled), decreasing=T),order(names(trees.gapfilled))]
trees.gapfilled[1:10, 1:10]
trees.gapfilled[1:10, (ncol(trees.gapfilled)-10):ncol(trees.gapfilled)]

dbh.recon <- trees.gapfilled
trees.check <- vector() # trees with negative DBH..cm.
for(j in names(dbh.recon)){

	# Step 1: Replace filled years beyond the year in which a tree was sampled with NA (both trees.gapfilled & DBH..cm. recon); 
	# 	Gapfilled: filling the years where I changed 0 to 1e-6 back to 0
	#	DBH..cm.recon: fill year of sample with DBH..cm. when sampled
	trees.gapfilled[as.numeric(row.names(trees.gapfilled))>tree.data[tree.data$TreeID==j, "Year.sample"],j] <- NA
	trees.gapfilled[,j] <- ifelse(trees.gapfilled[,j]==1e-6, 0, trees.gapfilled[,j])

	dbh.recon[as.numeric(row.names(dbh.recon))>tree.data[tree.data$TreeID==j, "Year.sample"],j] <- NA
	dbh.recon[as.numeric(row.names(dbh.recon))==tree.data[tree.data$TreeID==j, "Year.sample"],j] <- tree.data[tree.data$TreeID==j, "DBH..cm."]
	
	# Doing the DBH..cm. reconstruction	
	for(i in 2:(length(dbh.recon[,j]))){
		dbh.recon[i,j] <- ifelse(!is.na(trees.gapfilled[i-1,j]), dbh.recon[i-1,j] - trees.gapfilled[i-1,j]*2, dbh.recon[i,j]) # subtracting the previous year's growth from DBH..cm. to get that year's DBH..cm.
	}
	
	# Get rid of DBH..cm. past the guestimated pith dates -- both DBH..cm. recon & gapfilled
	if(!is.na(tree.data[tree.data$TreeID==j, "Pith"])){
		dbh.recon[as.numeric(row.names(dbh.recon))<tree.data[tree.data$TreeID==j, "Pith"],j] <- NA
		trees.gapfilled[as.numeric(row.names(trees.gapfilled))<tree.data[tree.data$TreeID==j, "Pith"],j] <- NA
	} else 
	if(is.na(tree.data[tree.data$TreeID==j, "Pith"])){ # Get rid of negative modeled DBH..cm.
		dbh.recon[,j] <- ifelse(dbh.recon[,j]<0, NA, dbh.recon[,j]) 
		trees.gapfilled[,j] <- ifelse(dbh.recon[,j]<0, NA, trees.gapfilled[,j]) 		
	}
	# also getting rid of these rings in the stacked ring data too
	years.na <- row.names(trees.gapfilled)[which(is.na(trees.gapfilled[,j]))]
	ring.data[ring.data$TreeID==j & ring.data$Year %in% years.na,"RW.gapfilled"] <- NA

	if(min(dbh.recon[,j], na.rm=T)<0) trees.check <- c(trees.check, j)
}
dbh.recon[1:20, 1:10]
dbh.recon[1:20, (ncol(dbh.recon)-20):ncol(dbh.recon)]
min(dbh.recon, na.rm=T)
trees.check
summary(dbh.recon[,trees.check])

# for trees with negative DBH..cm., working from the inside out
	# If a tree has a negative DBH..cm., we're just going to add from the inside out (at time )
for(j in trees.check){
	if(min(dbh.recon[,j], na.rm=T)<0){
		dbh.recon[,j] <- trees.gapfilled[,j]
		for(i in (nrow(dbh.recon)-1):1){
			dbh.recon[i,j] <- sum(dbh.recon[i+1, j], trees.gapfilled[i,j]*2, na.rm=T)
			}
	}
}
min(dbh.recon, na.rm=T)
dbh.recon[1:10,trees.check]
summary(dbh.recon[, trees.check])
#trees.gapfilled[, trees.check]
tree.data[tree.data$TreeID %in% trees.check,]
# ---------------------------------------
write.csv(dbh.recon, "processed_data/HAR_HOW_GapFilling_DBHrecon_ALL.csv", row.names=T)
write.csv(trees.gapfilled, "processed_data/HAR_HOW_GapFilling_RingWidths_ALL.csv", row.names=T)

##################################################################################
# see next script for reconstructing basal area of trees with no samples 
##################################################################################
