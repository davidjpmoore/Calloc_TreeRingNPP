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

# Getting big and small trees fro the valles

tree.data.valles <- tree.data[substr(tree.data$TreeID,1,1)=="V",]
summary(tree.data.valles)

vuf.tree.data <- tree.data[tree.data$Site %in% "Valles Caldera Upper",]
summary(vuf.tree.data)

vlf.tree.data <- tree.data[tree.data$Site %in% "Valles Caldera Lower",]
summary(vlf.tree.data)

# VUF dated only
vuf.dated.trees <- core.data[core.data$Site %in% "Valles Caldera Upper" & core.data$dated=="Y","TreeID"]
vuf.dated.trees <- vuf.dated.trees[complete.cases(vuf.dated.trees)]

save(vuf.dated.trees, file="processed_data/vuf_dated_trees.Rdata")

# VLF dated only
vlf.dated.trees <- core.data[core.data$Site %in% "Valles Caldera Lower" & core.data$dated=="Y","TreeID"]
vlf.dated.trees <- vlf.dated.trees[complete.cases(vlf.dated.trees)]
save(vlf.dated.trees, file="processed_data/vlf_dated_trees.Rdata")

# VUF big trees
vuf.big.trees<- vuf.tree.data[vuf.tree.data$DBH..cm. >= quantile(vuf.tree.data$DBH..cm., 0.9, na.rm=T),"TreeID"]
vuf.small.trees<- vuf.tree.data[vuf.tree.data$DBH..cm. <= quantile(vuf.tree.data$DBH..cm., 0.1, na.rm=T), "TreeID"]
summary(vuf.big.trees)
summary(vuf.small.trees)


save(vuf.big.trees, file="processed_data/vuf_big_trees.Rdata")

save(vuf.small.trees, file="processed_data/vuf_small_trees.Rdata")

# VLF big trees
vlf.big.trees<- vlf.tree.data[vlf.tree.data$DBH..cm. >= quantile(vlf.tree.data$DBH..cm., 0.9, na.rm=T),"TreeID"]
vlf.small.trees <- vlf.tree.data[vlf.tree.data$DBH..cm. <= quantile(vlf.tree.data$DBH..cm., 0.1, na.rm=T), "TreeID"]

save(vlf.big.trees, file="processed_data/vlf_big_trees.Rdata")
save(vlf.small.trees, file="processed_data/vlf_small_trees.Rdata")




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

summary(core.data.dated)

vlf.cores <- core.data.dated[substr(core.data.dated$plot.id,1,2)=="VL",]
summary(vlf.cores)

vuf.cores <- core.data.dated[substr(core.data.dated$plot.id,1,2)=="VU",]
summary(vuf.cores)

# Taking the upper 10% and lower 10% of DBH to get a big tree and a small tree chronology for both sites.
vuf.dated.big <- vuf.cores[vuf.cores$dbh >= quantile(vuf.cores$dbh, 0.9, na.rm=T),]
vuf.dated.small <- vuf.cores[vuf.cores$dbh <= quantile(vuf.cores$dbh, 0.1, na.rm=T),]

summary(vuf.dated.big)
summary(vuf.dated.small)


vlf.dated.big <- vlf.cores[vlf.cores$dbh >= quantile(vlf.cores$dbh, 0.9, na.rm=T),]
vlf.dated.small <- vlf.cores[vlf.cores$dbh <= quantile(vlf.cores$dbh, 0.1, na.rm=T),]

summary(vlf.dated.big)
summary(vlf.dated.small)

vuf.big.cores <- ross.valles.dated[,names(ross.valles.dated) %in% vuf.dated.big$CoreID]
vuf.small.cores <- ross.valles.dated[,names(ross.valles.dated) %in% vuf.dated.small$CoreID]

vlf.big.cores <- ross.valles.dated[,names(ross.valles.dated) %in% vlf.dated.big$CoreID]
vlf.small.cores <- ross.valles.dated[,names(ross.valles.dated) %in% vlf.dated.small$CoreID]

#-----------------------------------------------------------------------------------------

# detrending cores to make a composite site index
spag.plot(ross.valles.dated)
dim(ross.valles.dated)

# ross.valles.i<- detrend(ross.valles.dated, method="Mean")
ross.valles.i<- detrend(ross.valles.dated, method="Spline", nyrs=30)

vuf.dated.big.i <- detrend(vuf.big.cores, method="Spline", nyrs=30)
vlf.dated.big.i <- detrend(vlf.big.cores, method="Spline", nyrs=30)

vuf.dated.small.i <- detrend(vuf.small.cores, method="Spline", nyrs=30) 
vlf.dated.small.i <- detrend(vlf.small.cores, method="Spline", nyrs=30)


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

# creating a composite site chronology using biweights robust mean

ross.upper.cr <- chron(ross.upper.i, prefix="VUF", prewhiten=T)
summary(ross.upper.cr)

ross.lower.cr <- chron(ross.lower.i, prefix="VLF", prewhiten=T)
summary(ross.lower.cr)
row.names(ross.lower.cr)

vuf.big.cr <- chron(vuf.dated.big.i, prefix="VUB", prewhiten=T)
vuf.small.cr <- chron(vuf.dated.small.i, prefix="VUS", prewhiten=T)

vlf.big.cr <- chron(vlf.dated.big.i, prefix="VLB", prewhiten=T)
vlf.small.cr <- chron(vlf.dated.small.i, prefix="VLS", prewhiten=T)

vuf.big.cr <- vuf.big.cr[row.names(vuf.big.cr)<=2007 & row.names(vuf.big.cr) >= 1980,]
vuf.small.cr <- vuf.small.cr[row.names(vuf.small.cr)<=2007 & row.names(vuf.small.cr) >= 1980,]

vlf.big.cr <- vlf.big.cr[row.names(vlf.big.cr)<=2007 & row.names(vlf.big.cr) >= 1980,]
vlf.small.cr <- vlf.small.cr[row.names(vlf.small.cr)<=2007 & row.names(vlf.small.cr) >= 1980,]


# creating a composite site chronology using an ARITHEMETIC MEAN
ross.upper.cr.mean <- chron(ross.upper.i, prefix="VUF", prewhiten=T, biweight=F)
summary(ross.upper.cr.mean)

ross.lower.cr.mean <- chron(ross.lower.i, prefix="VLF", prewhiten=T, biweight=F)
summary(ross.lower.cr.mean)
row.names(ross.lower.cr.mean)




######################################################
# Loading in external Valles Climate Chronologies
######################################################

climate.upper.valles1 <- read.rwl("external_treering_data/nm586_touchan_PSME_bearcanyonwest_valles.rwl.txt")

# Detrending external climate series

# climate.upper.valles1.i <- detrend(climate.upper.valles1, method="Mean")
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
head(climate.upper.valles1.cr)
######################################################
# Loading in Griffin NADEF CatMesa chronologies
######################################################
# Dan's chronologies are already the residual chronologies from the Catmesa update in 2011

cat.mesa.chron <- read.csv("external_treering_data/Griffin_catmesa_CHRONS.csv", header=T)
summary(cat.mesa.chron)

row.names(cat.mesa.chron) <- cat.mesa.chron$Year
cat.mesa.chron <- data.frame(cat.mesa.chron[row.names(cat.mesa.chron)>="1980" & row.names(cat.mesa.chron)<= "2007", ])
summary(cat.mesa.chron)
cat.mesa.chron <- cat.mesa.chron[,c("CMMt_RES", "Year")]

summary(cat.mesa.chron)
row.names(cat.mesa.chron) <- cat.mesa.chron$Year



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
head(cat.mesa.depth)

cat.mesa.tot <- cbind(cat.mesa.chron[,"CMMt_RES"], cat.mesa.depth)
summary(cat.mesa.tot)
names(cat.mesa.tot) <- c("cat.res", "cat.std", "cat.n")

######################################################
# Loading in Guitermann Canyon del Potero Chronology
######################################################
can.del.pot <- read.rwl("external_treering_data/nm588.rwl.txt", header=T)
summary(can.del.pot)

can.del.pot.i <- detrend(can.del.pot, method="Spline", nyr=30)
can.del.pot.chron <- chron(can.del.pot.i, prefix="CDP", prewhiten=T, biweight=T)
summary(can.del.pot.chron)
#can.del.pot.chron <- can.del.pot.chron[order(row.names(can.del.pot.chron), decreasing=T),]
row.names(can.del.pot.chron)

can.del.pot.chron <- can.del.pot.chron[row.names(can.del.pot.chron)>=1980 & row.names(can.del.pot.chron) <=2007,]



###################################################################################################
# Merging all indecies together to form one dataframe on which we can run the climate correlations
###################################################################################################
valles.climate.cr <- cbind(ross.upper.cr, ross.lower.cr, climate.upper.valles1.cr, cat.mesa.tot, ross.upper.cr.mean, ross.lower.cr.mean, can.del.pot.chron, vuf.big.cr, vuf.small.cr, vlf.big.cr, vlf.small.cr) 
names(valles.climate.cr) <- c("vuf.std", "vuf.res", "vuf.n", "vlf.std", "vlf.res", "vlf.n", "bcw.std","bcw.res", "bcw.n","cat.res", "cat.std", "cat.n", "vuf.mean.std", "vuf.mean.res", "vuf.mean.n", "vlf.mean.std", "vlf.mean.res", "vlf.mean.n", "chg.std", "chg.res", "chg.n", "VUB.std", "VUB.res", "VUB.n", "VUS.std", "VUS.res", "VUS.n", "VLB.std", "VLB.res", "VLB.n", "VLS.std", "VLS.res", "VLS.n")

summary(valles.climate.cr)
head(valles.climate.cr)
# saving as Rdata file to be used for the climate correlations

save(valles.climate.cr, file="processed_data/valles_combined_climate_chronologies.rdata")



#---------------------------------------------------
# Looking at how well the chronologies from the upper and lower site compare

test <- lm(valles.climate.cr[,"vuf.res"]~valles.climate.cr[,"vlf.res"])
summary(test)

# Plotting with R squared of the fit line
pdf("figures/chron_fit.pdf", width=13, height=8.5)
ggplot(data=valles.climate.cr)+
	geom_point(aes(x=vuf.res, y=vlf.res))+
	stat_smooth(aes(x=vuf.res, y=vlf.res), method="lm")+
	scale_x_continuous(limits=range(valles.climate.cr[,c("vuf.res", "vlf.res")]))+
	scale_y_continuous(limits=range(valles.climate.cr[,c("vuf.res", "vlf.res")]))+
	coord_equal(ratio=1)+
	annotate("text",x=0.25, y=1.5, label=paste("R^2 == 0.51"), parse=T, size= rel(10))
dev.off()




