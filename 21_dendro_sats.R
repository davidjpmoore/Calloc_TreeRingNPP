library(dplR)
load("processed_data/all_valles_dated.Rdata")
summary(ross.valles.dated)

core.data <-read.csv("processed_data/core_data.csv", header=T)
summary(core.data)

vuf.core.data <- core.data[core.data$Site %in% "Valles Caldera Upper" & core.data$dated=="Y",]

vlf.core.data <- core.data[core.data$Site %in% "Valles Caldera Lower" & core.data$dated=="Y",]

# Percent Dated Trees are in 1_pregapfilling script

# doing some basic stats for the table showing the chronologies

# VUF
summary(vuf.core.data)          
summary(vuf.core.data[vuf.core.data$plot.id %in% "VUA",])
summary(vuf.core.data[vuf.core.data$plot.id %in% "VUB",])

#Gettign mean pith year
# VUF
mean(vuf.core.data[,"pith.yr"], na.rm=T); sd(vuf.core.data[,"pith.yr"], na.rm=T)
quantile(vuf.core.data[,"pith.yr"],c(0.025, 0.975), na.rm=T)

# VUA
mean(vuf.core.data[vuf.core.data$plot.id %in% "VUA","pith.yr"], na.rm=T); sd(vuf.core.data[vuf.core.data$plot.id %in% "VUA","pith.yr"], na.rm=T)
quantile(vuf.core.data[vuf.core.data$plot.id %in% "VUA","pith.yr"],c(0.025, 0.975), na.rm=T)

# VUB
mean(vuf.core.data[vuf.core.data$plot.id %in% "VUB","pith.yr"], na.rm=T); sd(vuf.core.data[vuf.core.data$plot.id %in% "VUB","pith.yr"], na.rm=T)
quantile(vuf.core.data[vuf.core.data$plot.id %in% "VUB","pith.yr"],c(0.025, 0.975), na.rm=T)

vuf.dated <- ross.valles.dated[,names(ross.valles.dated) %in% vuf.core.data$CoreID]

# General Dendro Stats
vuf.stats <-summary(vuf.dated)
mean(vuf.stats[,"sens1"])
mean(vuf.stats[,"first"]); sd(vuf.stats[,"first"])

# Getting ID's for cores and trees
vuf.id <- read.ids(vuf.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vuf.rwi.stats <- rwi.stats(vuf.dated, vuf.id, prewhiten=T)
vuf.rwi.stats

vuf.rwi.stats.running <- rwi.stats.running(vuf.dated, vuf.id, prewhiten=T)
vuf.rwi.stats.running

# interseries correlation
vuf.itc <- interseries.cor(vuf.dated)
mean(vuf.itc[,1])

#---------------------------------------
# VUA
summary(vuf.core.data[vuf.core.data$plot.id %in% "VUA",])
summary(vuf.dated)

vua.core.data <- vuf.core.data[vuf.core.data$plot.id %in% "VUA",]
summary(vua.core.data) 
vua.dated <- vuf.dated[,names(vuf.dated) %in% vua.core.data$CoreID]
summary(vua.dated)


# General Dendro Stats
vua.stats <-summary(vua.dated)
mean(vua.stats[,"sens1"])

# Getting ID's for cores and trees
vua.id <- read.ids(vua.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vua.rwi.stats <- rwi.stats(vua.dated, vua.id, prewhiten=T)
vua.rwi.stats

vua.rwi.stats.running <- rwi.stats.running(vua.dated, vua.id, prewhiten=T)
vua.rwi.stats.running

# interseries correlation
vua.itc <- interseries.cor(vua.dated)
mean(vua.itc[,1])




#---------------------------------------
# VUB
summary(vuf.core.data[vuf.core.data$plot.id %in% "VUB",])
summary(vuf.dated)

vub.core.data <- vuf.core.data[vuf.core.data$plot.id %in% "VUB",]
summary(vub.core.data) 
vub.dated <- vuf.dated[,names(vuf.dated) %in% vub.core.data$CoreID]
summary(vub.dated)


# General Dendro Stats
vub.stats <-summary(vub.dated)
mean(vub.stats[,"sens1"])

# Getting ID's for cores and trees
vub.id <- read.ids(vub.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vub.rwi.stats <- rwi.stats(vub.dated, vub.id, prewhiten=T)
vub.rwi.stats

vub.rwi.stats.running <- rwi.stats.running(vub.dated, vub.id, prewhiten=T)
vub.rwi.stats.running

# interseries correlation
vub.itc <- interseries.cor(vub.dated)
mean(vub.itc[,1])

#---------------------------------------
#---------------------------------------
# VLF
#---------------------------------------
#---------------------------------------

summary(vlf.core.data)          
summary(vlf.core.data[vlf.core.data$plot.id %in% "VLA",])
summary(vlf.core.data[vlf.core.data$plot.id %in% "VLB",])

# Getting mean pith year
# VLF
mean(vlf.core.data[,"pith.yr"], na.rm=T); sd(vlf.core.data[,"pith.yr"], na.rm=T)
quantile(vlf.core.data[,"pith.yr"],c(0.025, 0.975), na.rm=T)

# VLA
mean(vlf.core.data[vlf.core.data$plot.id %in% "VLA","pith.yr"], na.rm=T); sd(vlf.core.data[vlf.core.data$plot.id %in% "VLA","pith.yr"], na.rm=T)
quantile(vlf.core.data[vlf.core.data$plot.id %in% "VLA","pith.yr"],c(0.025, 0.975), na.rm=T)

# VLB
mean(vlf.core.data[vlf.core.data$plot.id %in% "VLB","pith.yr"], na.rm=T); sd(vlf.core.data[vlf.core.data$plot.id %in% "VLB","pith.yr"], na.rm=T)

quantile(vlf.core.data[vlf.core.data$plot.id %in% "VLB","pith.yr"],c(0.025, 0.975), na.rm=T)


vlf.dated <- ross.valles.dated[,names(ross.valles.dated) %in% vlf.core.data$CoreID]

# General Dendro Stats
vlf.stats <-summary(vlf.dated)
mean(vlf.stats[,"sens1"])

# Getting ID's for cores and trees
vlf.id <- read.ids(vlf.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vlf.rwi.stats <- rwi.stats(vlf.dated, vlf.id, prewhiten=T)
vlf.rwi.stats

vlf.rwi.stats.running <- rwi.stats.running(vlf.dated, vlf.id, prewhiten=T)
vlf.rwi.stats.running

# interseries correlation
vlf.itc <- interseries.cor(vlf.dated)
mean(vlf.itc[,1])



# VLA
summary(vlf.core.data[vlf.core.data$plot.id %in% "VLA",])

vla.core.data <- vlf.core.data[vlf.core.data$plot.id %in% "VLA",]
summary(vla.core.data) 

vla.dated <- vlf.dated[,names(vlf.dated) %in% vla.core.data$CoreID]
summary(vla.dated)


# General Dendro Stats
vla.stats <-summary(vla.dated)
mean(vla.stats[,"sens1"])

# Getting ID's for cores and trees
vla.id <- read.ids(vla.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vla.rwi.stats <- rwi.stats(vla.dated, vla.id, prewhiten=T)
vla.rwi.stats

vla.rwi.stats.running <- rwi.stats.running(vla.dated, vla.id, prewhiten=T)
vla.rwi.stats.running

# interseries correlation
vla.itc <- interseries.cor(vla.dated)
mean(vla.itc[,1])




# VLB
summary(vlf.core.data[vlf.core.data$plot.id %in% "VLB",])

vlb.core.data <- vlf.core.data[vlf.core.data$plot.id %in% "VLB",]
summary(vlb.core.data) 

vlb.dated <- vlf.dated[,names(vlf.dated) %in% vlb.core.data$CoreID]
summary(vlb.dated)


# General Dendro Stats
vlb.stats <-summary(vlb.dated)
mean(vlb.stats[,"sens1"])

# Getting ID's for cores and trees
vlb.id <- read.ids(vlb.dated, stc=c(3,3,1))

# Calculating dendro chronology stats
vlb.rwi.stats <- rwi.stats(vlb.dated, vlb.id, prewhiten=T)
vlb.rwi.stats

vlb.rwi.stats.running <- rwi.stats.running(vlb.dated, vlb.id, prewhiten=T)
vlb.rwi.stats.running

# interseries correlation
vlb.itc <- interseries.cor(vlb.dated)
mean(vlb.itc[,1])






summary(ross.valles.dated)
summary(ross.valles.dated[substr(names(ross.valles.dated), 1,3)=="VUF"])

length(ross.valles.dated[substr(names(ross.valles.dated), 1,3)=="VUF"])
