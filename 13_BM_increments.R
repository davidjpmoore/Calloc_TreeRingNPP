# Loading Rdata files to run the bootstrapping process

#---------------------------------------
# Allometry uncertainty array
#---------------------------------------

load("processed_data/Biomass_Array_Tree_kgm-2.Rdata")
summary(bm.array[,1:10,1])
dim(bm.array)

bm.array<- bm.array[dimnames(bm.array)[[1]]<=2011,,]
dim(bm.array)

vuf.allom <- bm.array[,substr(dimnames(bm.array)[[2]],1,2)=="VU",]
dim(vuf.allom)

vuf.allom.inc <- vuf.allom

for(j in 1:length(dimnames(vuf.allom.inc)[[2]])){
	for(i in 1:(length(dimnames(vuf.allom.inc)[[1]])-1)){
		vuf.allom.inc[i,j,] <- vuf.allom[i,j,] - vuf.allom[i+1,j,] 
	}
}

dim(vuf.allom.inc)
summary(vuf.allom.inc[,,])
head(vuf.allom.inc[,,1])


vlf.allom <- bm.array[,substr(dimnames(bm.array)[[2]],1,2)=="VL",]
dim(vlf.allom)
vlf.allom.inc <- vlf.allom

for(j in 1:length(dimnames(vlf.allom.inc)[[2]])){
		for(i in 1:(length(dimnames(vlf.allom.inc)[[1]])-1)){
		vlf.allom.inc[i,j,] <- vlf.allom[i,j,] - vlf.allom[i+1,j,] 
	}
}

dim(vlf.allom.inc)
summary(vlf.allom.inc[,,])
head(vlf.allom.inc[,,1])

save(vuf.allom.inc, file="processed_data/vuf_allom_inc.Rdata")
save(vlf.allom.inc, file="processed_data/vlf_allom_inc.Rdata")

#---------------------------------------
# Density uncertainty array
#---------------------------------------
load("processed_data/density_Biom_plot.Rdata")
summary(biom.plot)
dim(biom.plot)

biom.plot <- biom.plot[biom.plot$Year <= 2011,]
dens.inc <- biom.plot[,c(2:ncol(biom.plot))]
row.names(dens.inc) <- biom.plot$Year
dens.inc[,] <- NA
biom.plot <- biom.plot[,c(2:ncol(biom.plot))] 

head(dens.inc)

for(j in 1:ncol(dens.inc)){
	for(i in 1:(length(dens.inc[,j])-1)){
		dens.inc[i,j] <- biom.plot[i,j] - biom.plot[i+1,j]
	}
}	
		
dim(dens.inc)
head(dens.inc)
min(dens.inc, na.rm=T)

save(dens.inc, file="processed_data/dens_inc.Rdata")


#---------------------------------------
# Increment uncertainty array
# Already in increment.  Can leave alone
#---------------------------------------

load("processed_data/bm_increment.Rdata")
summary(bm.increment)
dim(bm.increment)
bm.increment <- bm.increment[row.names(bm.increment)<= 2011,]

vuf.inc <- bm.increment[,substr(names(bm.increment), 1,2)=="VU"]
dim(vuf.inc)

vlf.inc <- bm.increment[,substr(names(bm.increment), 1,2)=="VL"]
dim(vlf.inc)

save(vuf.inc, file="processed_data/vuf_inc.Rdata")
save(vlf.inc, file="processed_data/vlf_inc.Rdata")

#--------------------------------------- 
# Mortality uncertainty array
#---------------------------------------
load("processed_data/mort_boot_bm_plot.Rdata")
summary(bm.plot[,1:4,1])
dim(bm.plot)
dimnames(bm.plot)[[1]]


# Condensing down the array to site level mortality uncerainties while preserving the years and iteration matrix.
# # Upper site
vuf.mort <- apply(bm.plot[,c(1:2),], c(1,3), FUN="mean")
dim(vuf.mort)
summary(vuf.mort)

# Setting up dataframes to get the increment

# vuf.mort <- bm.plot[,substr(dimnames(bm.plot)[[2]],1,2)=="VU",]
# dim(vuf.mort)

vuf.mort.inc <- vuf.mort
vuf.mort.inc[,] <- NA

for(j in 1:ncol(vuf.mort.inc)){
	for (i in 1:(length(vuf.mort.inc[,j])-1)){
		vuf.mort.inc[i,j]<- vuf.mort[i,j] - vuf.mort[i+1,j]
	} 
}

dim(vuf.mort.inc)
summary(vuf.mort.inc)


# Now for lower site

vlf.mort <- apply(bm.plot[,c(3:4),], c(1,3), FUN="mean")
dim(vlf.mort)
summary(vlf.mort)

# vlf.mort <- bm.plot[,substr(dimnames(bm.plot)[[2]],1,2)=="VL",]
# dim(vlf.mort)

vlf.mort.inc <- vlf.mort
vlf.mort.inc[,] <- NA

for(j in 1:ncol(vlf.mort.inc)){
	for (i in 1:(length(vlf.mort.inc[,j])-1)){
		vlf.mort.inc[i,j]<- vlf.mort[i,j] - vlf.mort[i+1,j]
	} 
}

dim(vlf.mort.inc)
summary(vlf.mort.inc)

save(vuf.mort.inc, file="processed_data/vuf_mort_inc.Rdata")
save(vlf.mort.inc, file="processed_data/vlf_mort_inc.Rdata")

# there are some negatives here, but it is ok.  It is caused by the random nature of how we bootstrapped the mortality calculations from script #8

