#############################################
# Cleaning up Flurin's Mess, because clean Data is happy data and the swiss do not give you happy data
#############################################
library(car)

# Reading in the "metadata" that was given, but we're gonna make it better
# Only plot B
niwot.plot.b <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/metadata_niwot_plotB.txt", header=T)
summary(niwot.plot.b)

niwot.plot.b$Tree <- as.factor(niwot.plot.b$Tree)

niwot.plotb.clean.a<- data.frame(TreeID = paste0("NWB",niwot.plot.b$Tree), 
								dbh = niwot.plot.b$dbh,
								core.id = "A",
								pith.offset = niwot.plot.b$pithoffsetA,
								CoreID = paste0("NWB",niwot.plot.b$Tree,"A") 
								)
summary(niwot.plotb.clean.a)

niwot.plotb.clean.b<- data.frame(TreeID = paste0("NWB",niwot.plot.b$Tree), 
								dbh = niwot.plot.b$dbh,
								core.id = "B",
								pith.offset = niwot.plot.b$pithoffsetB,
								CoreID = paste0("NWB",niwot.plot.b$Tree,"B") 
								)
summary(niwot.plotb.clean.b)

niwot.plotb.clean.all <- rbind(niwot.plotb.clean.a, niwot.plotb.clean.b)
summary(niwot.plotb.clean.all)

# Loading in the template of how I want the data to look

niwot.plotb.template <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/Niwot_core_metadata_PlotB.txt", header=T)

summary(niwot.plotb.template)
niwot.plotb.template$tree.number <- as.factor(niwot.plotb.template$tree.number)

niwot.plotb.happy <- merge(niwot.plotb.clean.all[, c("TreeID", "CoreID", "dbh", "pith.offset")],
							niwot.plotb.template, all.x=T, all.y=T )

summary(niwot.plotb.happy)
head(niwot.plotb.happy)
names(niwot.plotb.happy$LIVE.dead) <- "live.dead" 


# Now Doing the same reorganziing for plot C
niwot.plot.c <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/metadata_niwot_plotC.txt", header=T)
summary(niwot.plot.c)

niwot.plot.c$Tree <- as.factor(niwot.plot.c$Tree)


#-------------------------------------------------------------------------
# Pulling out the columns that we actually want from Flurin's mess

niwot.plotc.clean.a<- data.frame(TreeID = paste0("NWC",niwot.plot.c$Tree), 
								dbh = niwot.plot.c$dbh,
								core.id = "A",
								pith.offset = niwot.plot.c$pithoffsetA,
								CoreID = paste0("NWC",niwot.plot.c$Tree,"A") 
								)
summary(niwot.plotc.clean.a)

niwot.plotc.clean.b<- data.frame(TreeID = paste0("NWC",niwot.plot.c$Tree), 
								dbh = niwot.plot.c$dbh,
								core.id = "B",
								pith.offset = niwot.plot.c$pithoffsetB,
								CoreID = paste0("NWC",niwot.plot.c$Tree,"B") 
								)
summary(niwot.plotc.clean.b)

#-------------------------------------------------------------------------
# Merging these new columns into one dataframe to merge with the plot C template
#-------------------------------------------------------------------------


niwot.plotc.clean.all <- rbind(niwot.plotc.clean.a, niwot.plotc.clean.b)
summary(niwot.plotc.clean.all)

# Loading in the template of how I want the data to look

niwot.plotc.template <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/Niwot_core_metadata_PlotC.txt", header=T)

summary(niwot.plotc.template)
niwot.plotc.template$tree.number <- as.factor(niwot.plotc.template$tree.number)

niwot.plotc.happy <- merge(niwot.plotc.clean.all[, c("TreeID", "CoreID", "dbh", "pith.offset")],
							niwot.plotc.template, all.x=T, all.y=T )

summary(niwot.plotc.happy)
head(niwot.plotc.happy)
niwot.plotc.happy$live.dead <- recode(niwot.plotc.happy$live.dead, "'2' = 'DEAD' ; 'live' = 'LIVE'")
summary(niwot.plotc.happy)


# Merging both the plot B and plot C metadata together into one file

niwot.all.happy <- merge(niwot.plotb.happy, niwot.plotc.happy, all.x=T, all.y=T)
summary(niwot.all.happy)

# things looks pretty good.  Now need to pull in the ring widths to get the first year and last year and the pith year for each core.
#--------------------------------------------------------
library(dplR)
# Plot B Ring widths

nwb.rw <- read.csv("~/PhD/Carbon Research/Niwot/Flurin_Niwot/ringwidth_niwot_plotB.csv", header=T)
names(nwb.rw)
nwc.rw <- read.csv("~/PhD/Carbon Research/Niwot/Flurin_Niwot/ringwidth_niwot_plotC.csv", header=T)
names(nwc.rw)

niwot.rw <- merge(nwb.rw, nwc.rw, all.x=T, all.y=T)
dim(nwb.rw)
dim(nwc.rw)
dim(niwot.rw)

write.csv(niwot.rw,"~/PhD/Carbon Research/Calloc_TreeRingNPP/raw_input_files/niwot_rw.csv", row.names=F)

summary(niwot.all.happy)

# Checking to make sure names of the rw file are happy
names(niwot.rw)[!(names(niwot.rw) %in% niwot.all.happy$CoreID)] # Year is okay for now
niwot.rw <- niwot.rw[, c("Year", names(niwot.rw)[(names(niwot.rw) %in% niwot.all.happy$CoreID)])] # get rid of f*cked up core names

# niwot.all.happy[,c("inner.measured", "outer.measured")] <- NA

for(j in names(niwot.rw)[!names(niwot.rw)=="Year"]){
	if(!length(which(!is.na(niwot.rw[,j])))>0) next # skip all cores that don't have any measurements
	niwot.all.happy[niwot.all.happy$CoreID==j,"inner.measured"] <- min(niwot.rw[which(!is.na(niwot.rw[,j])),"Year"])
	niwot.all.happy[niwot.all.happy$CoreID==j,"outer.measured"] <- max(niwot.rw[which(!is.na(niwot.rw[,j])),"Year"])
}

summary(niwot.all.happy)
summary(niwot.all.happy[is.na(niwot.all.happy$inner.measured),])
length(unique(niwot.all.happy[is.na(niwot.all.happy$inner.measured),"TreeID"]))
# Things look good

# calculating the pith.yr from teh inner measured and the pith.offset

niwot.all.happy$pith.yr <- niwot.all.happy$inner.measured - niwot.all.happy$pith.offset
summary(niwot.all.happy)

niwot.all.happy$inner.present <- niwot.all.happy$inner.measured
niwot.all.happy$outer.present <- niwot.all.happy$outer.measured

summary(niwot.all.happy)


for(i in 1:length(niwot.all.happy$CoreID)){
	niwot.all.happy$dated <-ifelse(is.na(niwot.all.happy$inner.measured), "N", "Y")
	}

niwot.all.happy$dated <- as.factor(niwot.all.happy$dated)
summary(niwot.all.happy) 

niwot.all.happy$species <- recode(niwot.all.happy$species, "'fir' = 'ABLA'; 'spruce' = 'PIEN'; 'pine' = 									'PICO'")

write.csv(niwot.all.happy, "~/PhD/Carbon Research/Calloc_TreeRingNPP/raw_input_files/niwot_core_metadata.csv", row.names=F)

# Making niwot Tree Metadata

summary(niwot.all.happy)


summary(niwot.plot.b)
niwot.plot.b$Tree <- as.factor(niwot.plot.b$Tree)
summary(niwot.plot.c)
niwot.plot.c$Tree <- as.factor(niwot.plot.c$Tree)

niwot.plotb.clean <- data.frame(TreeID = paste0("NWB", niwot.plot.b$Tree),
								PlotID = "NWB" ,
								Site = "Niwot" ,
								Species = niwot.plot.b$species,
								Canopy.Class = "C",
								Live.Dead = niwot.plot.b$alivedead ,
								DBH..cm. = niwot.plot.b$dbh ,
								Stems = 1 , 
								Total.Cores = 2)
								
niwot.plotc.clean <- data.frame(TreeID = paste0("NWC", niwot.plot.c$Tree),
								PlotID = "NWC" ,
								Site = "Niwot" ,
								Species = niwot.plot.c$species,
								Canopy.Class = "C",
								Live.Dead = niwot.plot.c$alivedead ,
								DBH..cm. = niwot.plot.c$dbh ,
								Stems = 1 , 
								Total.Cores = 2)
								
								

summary(niwot.plotb.clean)
summary(niwot.plotc.clean)
niwot.tree.data <- merge(niwot.plotb.clean, niwot.plotc.clean, all.x=T, all.y=T)

summary(niwot.tree.data)

niwot.tree.data$Live.Dead <- recode(niwot.tree.data$Live.Dead, "'a' = 'LIVE' ; 'd' = 'DEAD'")
niwot.tree.data$Species <- recode(niwot.tree.data$Species, "'fir' = 'ABLA'; 'spruce' = 'PIEN'; 'pine' = 									'PICO'")

summary(niwot.tree.data)

write.csv(niwot.tree.data, "~/PhD/Carbon Research/Calloc_TreeRingNPP/raw_input_files/niwot_tree_metadata.csv", row.names=F)