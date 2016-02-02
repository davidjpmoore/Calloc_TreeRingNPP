# Loading Rdata files to run the bootstrapping process

#---------------------------------------
# Allometry uncertainty array
#---------------------------------------

load("processed_data/Biomass_Array_Tree_kgm-2.Rdata")
summary(bm.array[,1,])
dim(bm.array)

bm.array<- bm.array[dimnames(bm.array)[[1]]<=2011,,]
dim(bm.array)

vuf.allom <- bm.array[,substr(dimnames(bm.array)[[2]],1,2)=="VU",]
dim(vuf.allom)

vuf.allom.inc <- vuf.allom

for(j in 1:length(dimnames(vuf.allom.inc)[2])){
		for(i in 1:length(dimnames(vuf.allom.inc)[[1]]-1)){
		vuf.allom.inc[i,j,] <- vuf.allom[i,j,] - vuf.allom[i+1,j,] 
	}
}

dim(vuf.allom.inc)
summary(vuf.allom.inc[,1:10,1])
head(vuf.allom.inc[,1:10,1])


vlf.allom <- bm.array[,substr(dimnames(bm.array)[[2]],1,2)=="VL",]
dim(vlf.allom)

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
	if(!is.na(biom.plot[i,j])){
		for(i in 1:length(dens.inc[,j])-1){
			dens.inc[i,j] <- biom.plot[i,j] - biom.plot[i+1,j]
		}
	}
}	
		
dim(dens.inc)
head(dens.inc)
min(dens.inc, na.rm=T)
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

#--------------------------------------- 
# Mortality uncertainty array
#---------------------------------------
load("processed_data/mort_boot_bm_plot.Rdata")
summary(bm.plot)
dim(bm.plot)

# Condensing down the array to site level mortality unceraintties while preserving the years and iteration matrix.
# Upper site
mort.vuf <- apply(bm.plot[,c(1:2),], c(1,3), FUN="mean")
dim(mort.vuf)
summary(mort.vuf)

# Setting up dataframes to get the increment

vuf.mort.inc <- mort.vuf
vuf.mort.inc[,] <- NA

for(j in 1:ncol(vuf.mort.inc)){
	
		if(!is.na(mort.vuf[i,j])){
			for (i in 1:(length(vuf.mort.inc[,j])-1)){
		vuf.mort.inc[i,j]<- mort.vuf[i,j] - mort.vuf[i+1,j]
		} 
	} 
}

dim(vuf.mort.inc)
summary(vuf.mort.inc)


# Now for lower site

mort.vlf <- apply(bm.plot[,c(3:4),], c(1,3), FUN="mean")
dim(mort.vlf)
summary(mort.vlf)


vlf.mort.inc <- mort.vlf
vlf.mort.inc[,] <- NA

for(j in 1:ncol(vlf.mort.inc)){
	
		if(!is.na(mort.vlf[i,j])){
			for (i in 1:(length(vlf.mort.inc[,j])-1)){
		vlf.mort.inc[i,j]<- mort.vlf[i,j] - mort.vlf[i+1,j]
		} 
	} 
}
dim(vlf.mort.inc)
summary(vlf.mort.inc)
min(vlf.mort.inc, na.rm=T)
# there are some negatives here, but it is ok.  It is caused by the random nature of how we bootstrapped the mortality calculations from script #8

#---------------------------------------
# increment Component
#---------------------------------------

inc[i] <- base + sqrt(plot.uncert[,,sample(1:dim(bm.array)[3])]^2 + mort.uncert.inc[i] + (base.incr-dens.uncert.inc[,sample(cols.vu)])^2 + inc.uncert[i])

inc.use.in.r2 <- base + sqrt(allom.inc.uncert[,,sample(XXX)]^2 + mort.inc.uncert[,sample(XXX)]^2 + [...])
