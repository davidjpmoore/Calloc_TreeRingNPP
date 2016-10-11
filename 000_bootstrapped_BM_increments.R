# Creating datasets that deviate from the mean
# Bootstrapping a random sample
# Adding up the bootstrapped timeseries to run in the climate correlation script


#---------------------------------------------------
# allometric uncertainty of BM at the site level
#---------------------------------------------------
load("processed_data/vuf_allom_inc.Rdata")
load("processed_data/vlf_allom_inc.Rdata")

dim(vuf.allom.inc)
min(vuf.allom.inc)
summary(vuf.allom.inc[,1:10,1])

vuf.allom.inc2 <- data.frame(apply(vuf.allom.inc, c(1,3), mean, na.rm=T))
summary(vuf.allom.inc2)

vuf.allom.inc.dev <- apply(vuf.allom.inc2,1,FUN=mean, na.rm=T) - vuf.allom.inc2
summary(vuf.allom.inc.dev)
head(vuf.allom.inc.dev)

dim(vlf.allom.inc)
min(vlf.allom.inc)
summary(vlf.allom.inc[,1:10,1])

vlf.allom.inc2 <- data.frame(apply(vlf.allom.inc, c(1,3), mean, na.rm=T))
summary(vlf.allom.inc2)

vlf.allom.inc.dev <- apply(vlf.allom.inc2, 1, FUN=mean, na.rm=T) - vuf.allom.inc2
summary(vlf.allom.inc.dev)
head(vlf.allom.inc.dev)


#---------------------------------------------------
# Density component
#---------------------------------------------------
load(file="processed_data/dens_inc.Rdata")
dim(dens.inc)
min(dens.inc, na.rm=T)
summary(dens.inc)
head(dens.inc)

vuf.dens <- dens.inc[,substr(names(dens.inc),1,2)=="VU"]
vlf.dens <- dens.inc[,substr(names(dens.inc),1,2)=="VL"]

vuf.dens.dev <- apply(vuf.dens, 1, FUN=mean, na.rm=T) - vuf.dens
head(vuf.dens.dev)

vlf.dens.dev <- apply(vlf.dens, 1, FUN=mean, na.rm=T) - vlf.dens
head(vlf.dens.dev)



#---------------------------------------------------
# increment Component
#---------------------------------------------------
load("processed_data/vuf_inc.Rdata")
load("processed_data/vlf_inc.Rdata")

dim(vuf.inc)
min(vuf.inc)
summary(vuf.inc)
head(vuf.inc)

vuf.inc.dev <- apply(vuf.inc, 1, FUN=mean, na.rm=T) - vuf.inc
head(vuf.inc.dev)


dim(vlf.inc)
min(vlf.inc)
summary(vlf.inc)
head(vlf.inc)

vlf.inc.dev <- apply(vlf.inc, 1, FUN=mean, na.rm=T) - vlf.inc
head(vlf.inc.dev)

#---------------------------------------------------
# Mortality Component
# Composed of 500 pulls for the mortality for each site over the timespan 1905-2011
#---------------------------------------------------
load("processed_data/vuf_mort_inc.Rdata")
load("processed_data/vlf_mort_inc.Rdata")

dim(vuf.mort.inc)
min(vuf.mort.inc, na.rm=T)
summary(vuf.mort.inc)
row.names(vuf.mort.inc)

vuf.mort.inc <- rbind(vuf.mort.inc, NA) 
row.names(vuf.mort.inc)<- c(2011:1904)
vlf.mort.inc <- rbind(vlf.mort.inc, NA) 
row.names(vlf.mort.inc)<- c(2011:1904)


vuf.mort.dev <- apply(vuf.mort.inc, 1, FUN=mean, na.rm=T) - vuf.mort.inc
head(vuf.mort.dev)



dim(vlf.mort.inc)
min(vlf.mort.inc, na.rm=T)
summary(vlf.mort.inc)
row.names(vlf.mort.inc)

vlf.mort.dev <- apply(vlf.mort.inc, 1, FUN=mean, na.rm=T) - vlf.mort.inc
head(vlf.mort.dev)


# Loading in big tres and small trees to analyze these as well

load("processed_data/vuf_big_trees.Rdata")
load("processed_data/vuf_small_trees.Rdata")
load("processed_data/vlf_big_trees.Rdata")
load("processed_data/vlf_small_trees.Rdata")

load("processed_data/vuf_dated_trees.Rdata")
load("processed_data/vlf_dated_trees.Rdata")


vuf.big.trees <- as.vector(vuf.big.trees)
vuf.small.trees <- as.vector(vuf.small.trees)

summary(vuf.big.trees)

vlf.big.trees <- as.vector(vlf.big.trees)
vlf.small.trees <- as.vector(vlf.small.trees)

vuf.dated.trees <- as.vector(vuf.dated.trees)
vlf.dated.trees <- as.vector(vlf.dated.trees)

#vuf.big.trees; vuf.small.trees; vlf.big.trees; vlf.small.trees


# Now taking 30^3 pulls from each of these dataframes to make simulated realizations of biomass that have been added in quadrature

n.pulls=30000
set.seed(0946)



cols.inc.dev   <- sample(1:ncol(vlf.inc.dev), n.pulls, replace=T)
cols.allom.dev <- sample(1:ncol(vlf.allom.inc.dev), n.pulls, replace=T)
cols.dens.dev  <- sample(1:ncol(vlf.dens.dev), n.pulls, replace=T)
cols.mort.dev  <- sample(1:ncol(vlf.mort.dev), n.pulls, replace=T)



vuf.cols.inc.big <- sample(which(names(vuf.inc.dev) %in% vuf.big.trees), n.pulls, replace=T)
vlf.cols.inc.big <- sample(which(names(vlf.inc.dev) %in% vlf.big.trees), n.pulls, replace=T)
vuf.cols.inc.small <- sample(which(names(vuf.inc.dev) %in% vuf.small.trees), n.pulls, replace=T)
vlf.cols.inc.small <- sample(which(names(vlf.inc.dev) %in% vlf.small.trees), n.pulls, replace=T)

vuf.cols.inc.dated <- sample(which(names(vuf.inc.dev) %in% vuf.dated.trees), n.pulls, replace=T) 
vlf.cols.inc.dated <- sample(which(names(vlf.inc.dev) %in% vlf.dated.trees), n.pulls, replace=T)



# VUF
# All trees
vuf.sim.bm <- sqrt(vuf.inc.dev[,cols.inc.dev]^2 + vuf.allom.inc.dev[,cols.allom.dev]^2 + vuf.dens.dev[,cols.dens.dev]^2 + vuf.mort.dev[,cols.mort.dev]^2)

# Big trees
vuf.big.bm <- sqrt(vuf.inc.dev[,vuf.cols.inc.big]^2 + vuf.allom.inc.dev[,cols.allom.dev]^2 + vuf.dens.dev[,cols.dens.dev]^2 + vuf.mort.dev[,cols.mort.dev]^2)

# Small trees
vuf.small.bm <- sqrt(vuf.inc.dev[,vuf.cols.inc.small]^2 + vuf.allom.inc.dev[,cols.allom.dev]^2 + vuf.dens.dev[,cols.dens.dev]^2 + vuf.mort.dev[,cols.mort.dev]^2)

# Dated only
vuf.dated.bm <- sqrt(vuf.inc.dev[,vuf.cols.inc.dated]^2 + vuf.allom.inc.dev[,cols.allom.dev]^2 + vuf.dens.dev[,cols.dens.dev]^2 + vuf.mort.dev[,cols.mort.dev]^2)


write.csv(vuf.sim.bm, file="processed_data/vuf_simulated_bm.csv", row.names=F)
write.csv(vuf.big.bm, file="processed_data/vuf_simulated_bm_big.csv", row.names=F)
write.csv(vuf.small.bm, file="processed_data/vuf_simulated_bm_small.csv", row.names=F)
write.csv(vuf.dated.bm, file="processed_data/vuf_simulated_bm_dated.csv", row.names=F)


# VLF
# All trees
vlf.sim.bm <- sqrt(vlf.inc.dev[,cols.inc.dev]^2 + vlf.allom.inc.dev[,cols.allom.dev]^2 + vlf.dens.dev[,cols.dens.dev]^2 + vlf.mort.dev[,cols.mort.dev]^2)

# Big trees
vlf.big.bm <- sqrt(vlf.inc.dev[,vlf.cols.inc.big]^2 + vlf.allom.inc.dev[,cols.allom.dev]^2 + vlf.dens.dev[,cols.dens.dev]^2 + vlf.mort.dev[,cols.mort.dev]^2)

# Small trees
vlf.small.bm <- sqrt(vlf.inc.dev[,vlf.cols.inc.small]^2 + vlf.allom.inc.dev[,cols.allom.dev]^2 + vlf.dens.dev[,cols.dens.dev]^2 + vlf.mort.dev[,cols.mort.dev]^2)

# Dated only
vlf.dated.bm <- sqrt(vlf.inc.dev[,vlf.cols.inc.dated]^2 + vlf.allom.inc.dev[,cols.allom.dev]^2 + vlf.dens.dev[,cols.dens.dev]^2 + vlf.mort.dev[,cols.mort.dev]^2)



write.csv(vlf.sim.bm, file="processed_data/vlf_simulated_bm.csv", row.names=F)
write.csv(vlf.big.bm, file="processed_data/vlf_simulated_bm_big.csv", row.names=F)
write.csv(vlf.small.bm, file="processed_data/vlf_simulated_bm_small.csv", row.names=F)
write.csv(vlf.dated.bm, file="processed_data/vlf_simulated_bm_dated.csv", row.names=F)
################################################################################################
# Breaking down and looking at each area of uncertainty andn using in correlations in the next script; Still using 5000 pulls.
################################################################################################

# Increment only
set.seed(0946)
vuf.inc.only <- vuf.inc.dev[,cols.inc.dev] 

dim(vuf.inc.only)
summary(vuf.inc.only)
save(vuf.inc.only, file="processed_data/vuf_bm_inc_only.Rdata")

set.seed(0946)
vlf.inc.only <- vlf.inc.dev[,cols.inc.dev] 

dim(vlf.inc.only)
summary(vlf.inc.only)
save(vlf.inc.only, file="processed_data/vlf_bm_inc_only.Rdata")
#--------------------------------------------------------------------

# Allometry only
set.seed(0946)
vuf.allom.only <-  vuf.allom.inc.dev[,cols.allom.dev]

dim(vuf.allom.only)
summary(vuf.allom.only)
save(vuf.allom.only, file="processed_data/vuf_bm_allom_only.Rdata")

set.seed(0946)
vlf.allom.only <- vlf.allom.inc.dev[,cols.allom.dev] 

dim(vlf.allom.only)
summary(vlf.allom.only)
save(vlf.allom.only, file="processed_data/vlf_bm_allom_only.Rdata")
#--------------------------------------------------------------------

# Density only 
set.seed(0946)
vuf.dens.only <- vuf.dens.dev[,cols.dens.dev]
dim(vuf.dens.only)
summary(vuf.dens.only)
save(vuf.dens.only, file="processed_data/vuf_bm_dens_only.Rdata")

set.seed(0946)
vlf.dens.only <- vlf.dens.dev[,cols.dens.dev]
dim(vlf.dens.only)
summary(vlf.dens.only)
save(vlf.dens.only, file="processed_data/vlf_bm_dens_only.Rdata")
#--------------------------------------------------------------------

# Mortality Only
set.seed(0946)
vuf.mort.only <- vuf.mort.dev[,cols.mort.dev]
dim(vuf.mort.only)
summary(vuf.mort.only)
save(vuf.mort.only, file="processed_data/vuf_bm_mort_only.Rdata")


set.seed(0946)
vlf.mort.only <- vlf.mort.dev[,cols.mort.dev]
dim(vlf.mort.only)
summary(vlf.mort.only)
save(vlf.mort.only, file="processed_data/vlf_bm_mort_only.Rdata")



