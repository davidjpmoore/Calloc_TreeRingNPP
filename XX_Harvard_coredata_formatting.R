# Read in RWL

library(dplR)
core.rw <- read.rwl("RWL/harv_how_all_trees.rwl")

names(core.rw)

core.names <- names(core.rw)

# matching core names with summary stats for core metadata
write.csv(core.names, file="core_names.csv")

write.csv(summary(core.rw), file="harv_how_summary.csv")

# loading in tree data
tree.data <- read.csv("raw_input_files/harvard_howland_tree_metadata.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)

summary(tree.data)

# Loading in core data
core.data <- read.csv("raw_input_files/harvard_howland_core_data.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
summary(core.data)

# selecting columns from core.data and tree.data to merge

# core.data columns
cols1 <- c("TreeID", "CoreID", "Site", "plot.id", "tree.number", "stems", "total.cores", "core.id", "pith.present", "pith.yr", "inner.present", "inner.measured", "outer.measured", "bark.present", "inner.dated", "outer.dated", "dated", "Notes", "zombie")
# tree.data columns
cols2 <- c("TreeID", "Species", "Canopy.Class", "Live.Dead", "DBH..cm.")

# merging dataframes
core.data <- merge(core.data[,cols1], tree.data[,cols2], all.x=T, all.y=F)
summary(core.data)

core.data <- core.data[,2:ncol(core.data)]

head(core.data)
head(tree.data)

write.csv(core.data, file="raw_input_files/harvard_how.and_core_data2.csv")
