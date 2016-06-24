# Getting tree data set up for howland
# Things are kind of a mess

# Loading in Alex's file


alex.tree <- read.csv("raw_input_files/How.field.data_may2016.csv", header=T)
template <- read.csv("raw_input_files/harvard_howland_tree_metadata.csv", header=T)

names(template)
summary(alex.tree)



tree.data <- data.frame(PlotID = alex.tree$Site,
						Tree = str_pad(alex.tree$Tree, 3, pad="0"),	
						Species = alex.tree$Species,
						Canopy.Class = alex.tree$Canopy,
						DBH..cm. = alex.tree$DBH,
						Distance = alex.tree$Distance,
						Azimuth = alex.tree$Azimuth,
						status = alex.tree$Status,
						Site = "Howland")

summary(tree.data)

tree.data$TreeID <- as.factor(paste0(tree.data$PlotID, tree.data$Tree))

tree.data$Live.Dead <- as.factor(ifelse(tree.data$status== "LI"| tree.data$status=="Li", "LIVE", "DEAD"))
summary(tree.data)

tree.data <- tree.data[,!names(tree.data) %in% "status"]
summary(tree.data)

names(template)
names(tree.data)

write.csv(tree.data, file="raw_input_files/howland_tree_metadata.csv", row.names=F)

abba <- read.rwl("RWL/howland_raw_rw/HOW_ABBA.rw")
abba$Year <- row.names(abba)
acru <- read.rwl("RWL/howland_raw_rw/HOW_ACRU.rw")
acru$Year <- row.names(acru)
beal <- read.rwl("RWL/howland_raw_rw/HOW_BEAL.rw")
beal$Year <- row.names(beal)
pcru <- read.rwl("RWL/howland_raw_rw/HOW_PCRU.rw")
pcru$Year <- row.names(pcru)
pist <- read.rwl("RWL/howland_raw_rw/HOW_PIST.rw")
pist$Year <- row.names(pist)
thoc <- read.rwl("RWL/howland_raw_rw/HOW_THOC2.rwl")
thoc$Year <- row.names(thoc)
tsca <- read.rwl("RWL/howland_raw_rw/HOW_TSCA.rw")
tsca$Year <- row.names(tsca)


dim(abba)
dim(acru)

rw1 <- merge(abba, acru, by= "Year", all.x=T,all.y=T)
dim(rw1)

dim(beal)
rw2 <- merge(rw1, beal, by = "Year", all.x=T, all.y=T)
dim(rw2)

dim(pcru)
rw3 <- merge(rw2, pcru, by = "Year", all.x=T, all.y=T)
dim(rw3)

dim(pist)
rw4 <- merge(rw3, pist, by = "Year", all.x=T, all.y=T)
dim(rw4)

dim(thoc)
rw5 <- merge(rw4, thoc, by = "Year", all.x=T, all.y=T)
dim(rw5)

dim(tsca)
rw6 <- merge(rw5, tsca, by = "Year", all.x=T, all.y=T)
dim(rw6)

summary(rw6)

row.names(rw6) <- rw6$Year

rw6 <- rw6[!names(rw6) %in% "Year"]
summary(rw6)

names(rw6)

write.rwl(rw6, fname="RWL/howland_May2016_combined.rwl", format="tridas")

test <- read.rwl("RWL/howland_May2016_combined.rwl", format="tridas")

summary(test$measurements)

test2 <- test$measurements
summary(test2)

write.csv(summary(test2), file="processed_data/howland_may2016_core_summary.csv", row.names=F)

test3 <- read.csv("processed_data/howland_may2016_core_summary.csv", header=T)
summary(test3)

core.template <- read.csv("raw_input_files/harvard_how.and_core_data2.csv", header=T)
names(core.template) 
summary(core.template)    


core.data <- data.frame(CoreID = test3$series,
						TreeID = substr(test3$series, 1,7),
						Site = "HOW",
						plot.id = substr(test3$series,1,4),
						inner.measured = test3$first,
						outer.measured = test3$last,
						core.id = substr(test3$series,8,8),
						pith.present = "N",
						dated = "Y",
						pith.yr = NA)
summary(core.data)

dim(core.data)

core.data2 <- merge(core.data, tree.data[,c("TreeID", "DBH..cm.", "Species", "Live.Dead", "Canopy.Class")], all.x=T, all.y=F)
summary(core.data2)
dim(core.data2)

core.data2$inner.dated <- core.data2$inner.measured
core.data2$outer.dated <- core.data2$outer.measured
summary(core.data2)

summary(core.data2[is.na(core.data2$Canopy.Class),])

# Dropping HOW2126, becasue it is dead with no canopy class listed
core.data3 <- core.data2[!core.data2$TreeID %in% "HOW2126",]
dim(core.data3)

write.csv(core.data3, file="raw_input_files/howland_core_data_may2016.csv", row.names=F)
