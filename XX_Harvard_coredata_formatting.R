# Read in RWL
library(ggplot2)
library(dplR)

# Alex sent updated Howland RW and tree metadata in May 2016.  I am cutting old Howland data and merging in new howland data.

core.rw <- read.rwl("RWL/harv_how_all_trees.rwl")

harv.rw <- core.rw[,substr(names(core.rw),1,1)=="T"]
summary(harv.rw)
write.rwl(harv.rw, fname="RWL/harvard_only.rwl", format="tridas")

harv.rw$Year <- row.names(harv.rw)

# # matching core names with summary stats for core metadata
# write.csv(core.names, file="core_names.csv")

# write.csv(summary(core.rw), file="harv_how_summary.csv")

# loading in tree data
tree.data <- read.csv("raw_input_files/harvard_howland_tree_metadata.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)

summary(tree.data)

harv.tree.data <- tree.data[tree.data$Site %in% "Harvard",]
summary(harv.tree.data)
write.csv(harv.tree.data,file="raw_input_files/harvard_tree_metadata.csv", row.names=F)
harv.tree.data <- read.csv("raw_input_files/harvard_tree_metadata.csv",header=T)

summary(harv.tree.data)

# Loading in new Alex RW and tree data

how.tridas <- read.rwl("RWL/howland_May2016_combined.rwl", format="tridas")
summary(how.tridas)

how.rw <- how.tridas$measurements
how.rw$Year <- row.names(how.rw)

summary(how.rw)
head(how.rw)

how.tree.data <- read.csv("raw_input_files/howland_tree_metadata.csv", header=T)
summary(how.tree.data)


# Merging Harvard and howland RW data together

harv.how.rw <- merge(harv.rw, how.rw, by="Year", all.x=T, all.y=T)
summary(harv.how.rw)
row.names(harv.how.rw) <- harv.how.rw$Year
harv.how.rw <- harv.how.rw[,!names(harv.how.rw) %in% "Year"]
summary(harv.how.rw)
head(harv.how.rw)
names(harv.how.rw)

write.rwl(harv.how.rw, fname="RWL/harv_how_combo_may2016.rwl", format="tridas")
harv.how.tridas <- read.rwl("RWL/harv_how_combo_may2016.rwl", format="tridas")
summary(harv.how.tridas$measurements)

harv.how.rw <- harv.how.tridas$measurements

# Merging Harvard and Howland Tree data Together

dim(how.tree.data)
how.tree.data$plot <- as.factor(substr(how.tree.data$PlotID,4,4))

dim(harv.tree.data)

harv.how.tree.data <- merge(harv.tree.data, how.tree.data, all.x=T, all.y=T)
dim(harv.how.tree.data)

summary(harv.how.tree.data)
write.csv(harv.how.tree.data, file="raw_input_files/harvard_howland_may2016_tree_metadata.csv", row.names=F)

# Making 5cm diameter bins
dbh.bins1 <- seq(0,max(harv.how.tree.data$DBH..cm., na.rm=T),5)



# Making quick plot to see the species breakdown of DBH at each site
# All Trees
# DBH by site Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ . ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by plots Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ plot ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by canopy class Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ Canopy.Class) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")
	
# DBH by Site by species with Canopy fill
qplot(x=DBH..cm., data=harv.how.tree.data, geom="histogram", breaks=dbh.bins1, fill=Canopy.Class) +
	facet_grid(Site ~ Species) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")



# Living trees only
# DBH by site Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data[harv.how.tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ . ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by plots Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data[harv.how.tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ plot ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by canopy class Species Fill
qplot(x=DBH..cm., data=harv.how.tree.data[harv.how.tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ Canopy.Class) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")
	
# DBH by Site by species with Canopy fill
qplot(x=DBH..cm., data=harv.how.tree.data[harv.how.tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Canopy.Class) +
	facet_grid(Site ~ Species) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")


# Loading in core data
# Doing same thing here; splitting out old Howland data and updating it with the May 2016 stuff

core.data <- read.csv("raw_input_files/harvard_howland_core_data.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
summary(core.data)
core.data$pith.present <- as.factor("N")

harv.core.data <- core.data[core.data$Site %in% "TP",]
summary(harv.core.data)
harv.core.data$plot.id <- as.factor(substr(harv.core.data$TreeID, 1,3))

write.csv(harv.core.data, file="raw_input_files/harvard_core_data.csv", row.names=F)

how.core.data <- read.csv("raw_input_files/howland_core_data_may2016.csv", header=T)
summary(how.core.data)

dim(how.core.data)
dim(harv.core.data)
harv.how.core.data <- merge(harv.core.data, how.core.data, all.x=T, all.y=T)
dim(harv.how.core.data)
summary(harv.how.core.data)


# selecting columns from core.data and tree.data to merge

# core.data columnsx
cols1 <- c("TreeID", "CoreID", "Site", "plot.id", "tree.number", "stems", "total.cores", "core.id", "pith.present", "pith.yr", "inner.present", "inner.measured", "outer.measured", "bark.present", "inner.dated", "outer.dated", "dated", "Notes", "zombie")
# tree.data columns
cols2 <- c("TreeID", "Species", "Canopy.Class", "Live.Dead", "DBH..cm.")

# merging dataframes
harv.how.core.data <- merge(harv.how.core.data[,cols1], harv.how.tree.data[,cols2], all.x=T, all.y=F)
summary(harv.how.core.data)
harv.how.core.data$pith.present <- as.factor(harv.how.core.data$pith.present)
harv.how.core.data$pith.present <- "N"





head(harv.how.core.data)
head(harv.how.tree.data)

write.csv(harv.how.core.data, file="raw_input_files/harvard_howland_may2016_coredata.csv", row.names=F)
