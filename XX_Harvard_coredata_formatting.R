# Read in RWL
library(ggplot2)
library(dplR)
core.rw <- read.rwl("RWL/harv_how_all_trees.rwl")

# matching core names with summary stats for core metadata
write.csv(core.names, file="core_names.csv")

write.csv(summary(core.rw), file="harv_how_summary.csv")

# loading in tree data
tree.data <- read.csv("raw_input_files/harvard_howland_tree_metadata.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)

summary(tree.data)

# Making 5cm diameter bins
dbh.bins1 <- seq(0,max(tree.data$DBH..cm., na.rm=T),5)



# Making quick plot to see the species breakdown of DBH at each site
# All Trees
# DBH by site Species Fill
qplot(x=DBH..cm., data=tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ . ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by plots Species Fill
qplot(x=DBH..cm., data=tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ plot ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by canopy class Species Fill
qplot(x=DBH..cm., data=tree.data, geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ Canopy.Class) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")
	
# DBH by Site by species with Canopy fill
qplot(x=DBH..cm., data=tree.data, geom="histogram", breaks=dbh.bins1, fill=Canopy.Class) +
	facet_grid(Site ~ Species) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")



# Living trees only
# DBH by site Species Fill
qplot(x=DBH..cm., data=tree.data[tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ . ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by plots Species Fill
qplot(x=DBH..cm., data=tree.data[tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ plot ) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")

# DBH by canopy class Species Fill
qplot(x=DBH..cm., data=tree.data[tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Species) +
	facet_grid(Site ~ Canopy.Class) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")
	
# DBH by Site by species with Canopy fill
qplot(x=DBH..cm., data=tree.data[tree.data$Live.Dead=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Canopy.Class) +
	facet_grid(Site ~ Species) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution")


# Loading in core data
core.data <- read.csv("raw_input_files/harvard_howland_core_data.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
summary(core.data)
core.data$pith.present <- as.factor(core.data$pith.present)


# selecting columns from core.data and tree.data to merge

# core.data columnsx
cols1 <- c("TreeID", "CoreID", "Site", "plot.id", "tree.number", "stems", "total.cores", "core.id", "pith.present", "pith.yr", "inner.present", "inner.measured", "outer.measured", "bark.present", "inner.dated", "outer.dated", "dated", "Notes", "zombie")
# tree.data columns
cols2 <- c("TreeID", "Species", "Canopy.Class", "Live.Dead", "DBH..cm.")

# merging dataframes
core.data <- merge(core.data[,cols1], tree.data[,cols2], all.x=T, all.y=F)
summary(core.data)
core.data$pith.present <- as.factor(core.data$pith.present)
core.data$pith.present <- "N"



core.data <- core.data[,2:ncol(core.data)]

head(core.data)
head(tree.data)

write.csv(core.data, file="raw_input_files/harvard_how.and_core_data2.csv")
