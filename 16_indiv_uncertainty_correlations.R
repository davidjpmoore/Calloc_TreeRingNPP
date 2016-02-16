########################################################################################
# Running Correlations between individual areas of uncertainty with Climate data
########################################################################################
# Load in areas of uncertainty
load("processed_data/vuf_bm_dens_only.Rdata")
summary(vuf.dens.only)
vuf.dens.only <- vuf.dens.only[order(row.names(vuf.dens.only), decreasing=F),]
head(vuf.dens.only)


load("processed_data/vuf_bm_inc_only.Rdata")
summary(vuf.inc.only)
vuf.inc.only <- vuf.inc.only[order(row.names(vuf.inc.only), decreasing=F),]
head(vuf.inc.only)

load("processed_data/vuf_bm_allom_only.Rdata")
summary(vuf.allom.only)
vuf.allom.only <- vuf.allom.only[order(row.names(vuf.allom.only), decreasing=F),]
head(vuf.allom.only)

load("processed_data/vuf_bm_mort_only.Rdata")
summary(vuf.mort.only)
vuf.mort.only <- vuf.mort.only[order(row.names(vuf.mort.only), decreasing=F),]
head(vuf.mort.only)

load("processed_data/vlf_bm_dens_only.Rdata")
summary(vlf.dens.only)
vlf.dens.only <- vlf.dens.only[order(row.names(vlf.dens.only), decreasing=F),]
head(vlf.dens.only)


load("processed_data/vlf_bm_inc_only.Rdata")
summary(vlf.inc.only)
vlf.inc.only <- vlf.inc.only[order(row.names(vlf.inc.only), decreasing=F),]
head(vlf.inc.only)

load("processed_data/vlf_bm_allom_only.Rdata")
summary(vlf.allom.only)
vlf.allom.only <- vlf.allom.only[order(row.names(vlf.allom.only), decreasing=F),]
head(vlf.allom.only)

load("processed_data/vlf_bm_mort_only.Rdata")
summary(vlf.mort.only)
vlf.mort.only <- vlf.mort.only[order(row.names(vlf.mort.only), decreasing=F),]
head(vlf.mort.only)


# Trimming time series down to the same length as the full uncert. inc. 1980-2007

vuf.dens.only <- vuf.dens.only[row.names(vuf.dens.only)>= 1980 & row.names(vuf.dens.only)<=2007,]
vuf.inc.only <- vuf.inc.only[row.names(vuf.inc.only)>= 1980 & row.names(vuf.inc.only)<=2007,]
vuf.allom.only <- vuf.allom.only[row.names(vuf.allom.only)>= 1980 & 	   row.names(vuf.allom.only)<=2007,]
vuf.mort.only <- vuf.mort.only[row.names(vuf.mort.only)>= 1980 & row.names(vuf.mort.only)<=2007,]

dim(vuf.mort.only)


vlf.dens.only <- vlf.dens.only[row.names(vlf.dens.only)>= 1980 & row.names(vlf.dens.only)<=2007,]
vlf.inc.only <- vlf.inc.only[row.names(vlf.inc.only)>= 1980 & row.names(vlf.inc.only)<=2007,]
vlf.allom.only <- vlf.allom.only[row.names(vlf.allom.only)>= 1980 & 	   row.names(vlf.allom.only)<=2007,]
vlf.mort.only <- vlf.mort.only[row.names(vlf.mort.only)>= 1980 & row.names(vlf.mort.only)<=2007,]

dim(vlf.inc.only)


# Load in PRISM climate data

t.mean <- read.csv("climate_data/prism_met_sites_wide_tmean.csv", header=T)
t.min <- read.csv("climate_data/prism_met_sites_wide_tmin.csv", header=T)
t.max <- read.csv("climate_data/prism_met_sites_wide_tmax.csv", header=T)
precip <- read.csv("climate_data/prism_met_sites_wide_ppt.csv", header=T)

summary(t.mean)
unique(t.mean$Site.Name)
#----------------------------------------------
# Subsetting out the upper and the lower sites
# Upper Site
vuf.tmean <- t.mean[t.mean$Site.Name=="Valles Caldera Upper",]
summary(vuf.tmean)
vuf.tmean <- vuf.tmean[vuf.tmean$Year >= 1980 & vuf.tmean$Year <=2007,]
summary(vuf.tmean)

vuf.tmin <- t.min[t.min$Site.Name=="Valles Caldera Upper",]
summary(vuf.tmin)
vuf.tmin <- vuf.tmin[vuf.tmin$Year >= 1980 & vuf.tmin$Year <=2007,]
summary(vuf.tmin)

vuf.tmax <- t.max[t.max$Site.Name=="Valles Caldera Upper",]
summary(vuf.tmax)
vuf.tmax <- vuf.tmax[vuf.tmax$Year >= 1980 & vuf.tmax$Year <=2007,]
summary(vuf.tmax)

vuf.precip <- precip[precip$Site.Name=="Valles Caldera Upper",]
summary(vuf.precip)
vuf.precip <- vuf.precip[vuf.precip$Year >= 1980 & vuf.precip$Year <=2007,]
summary(vuf.precip)

# Lower Site
vlf.tmean <- t.mean[t.mean$Site.Name=="Valles Caldera Lower",]
summary(vlf.tmean)
vlf.tmean <- vlf.tmean[vlf.tmean$Year >= 1980 & vlf.tmean$Year <=2007,]
summary(vlf.tmean)

vlf.tmin <- t.min[t.min$Site.Name=="Valles Caldera Lower",]
summary(vlf.tmin)
vlf.tmin <- vlf.tmin[vlf.tmin$Year >= 1980 & vlf.tmin$Year <=2007,]
summary(vlf.tmin)

vlf.tmax <- t.max[t.max$Site.Name=="Valles Caldera Lower",]
summary(vlf.tmax)
vlf.tmax <- vlf.tmax[vlf.tmax$Year >= 1980 & vlf.tmax$Year <=2007,]
summary(vlf.tmax)

vlf.precip <- precip[precip$Site.Name=="Valles Caldera Lower",]
summary(vlf.precip)
vlf.precip <- vlf.precip[vlf.precip$Year >= 1980 & vlf.precip$Year <=2007,]
summary(vlf.precip)


#----------------------------------------------
# Making Seasons
# Upper Site
vuf.tmean$pFall <- rowMeans(vuf.tmean[,c("pSep", "pOct", "pNov")])
vuf.tmean$Winter <- rowMeans(vuf.tmean[,c("pDec", "Jan", "Feb")])
vuf.tmean$Spring <- rowMeans(vuf.tmean[,c("Mar", "Apr", "May")])
vuf.tmean$Summer <- rowMeans(vuf.tmean[,c("Jun", "Jul", "Aug")])

summary(vuf.tmean)

vuf.tmin$pFall <- rowMeans(vuf.tmin[,c("pSep", "pOct", "pNov")])
vuf.tmin$Winter <- rowMeans(vuf.tmin[,c("pDec", "Jan", "Feb")])
vuf.tmin$Spring <- rowMeans(vuf.tmin[,c("Mar", "Apr", "May")])
vuf.tmin$Summer <- rowMeans(vuf.tmin[,c("Jun", "Jul", "Aug")])

summary(vuf.tmin)

vuf.tmax$pFall <- rowMeans(vuf.tmax[,c("pSep", "pOct", "pNov")])
vuf.tmax$Winter <- rowMeans(vuf.tmax[,c("pDec", "Jan", "Feb")])
vuf.tmax$Spring <- rowMeans(vuf.tmax[,c("Mar", "Apr", "May")])
vuf.tmax$Summer <- rowMeans(vuf.tmax[,c("Jun", "Jul", "Aug")])

summary(vuf.tmax)

vuf.precip$pFall <- rowSums(vuf.precip[,c("pSep", "pOct", "pNov")])
vuf.precip$Winter <- rowSums(vuf.precip[,c("pDec", "Jan", "Feb")])
vuf.precip$Spring <- rowSums(vuf.precip[,c("Mar", "Apr", "May")])
vuf.precip$Summer <- rowSums(vuf.precip[,c("Jun", "Jul", "Aug")])

summary(vuf.precip)

# Lower Site

vlf.tmean$pFall <- rowMeans(vlf.tmean[,c("pSep", "pOct", "pNov")])
vlf.tmean$Winter <- rowMeans(vlf.tmean[,c("pDec", "Jan", "Feb")])
vlf.tmean$Spring <- rowMeans(vlf.tmean[,c("Mar", "Apr", "May")])
vlf.tmean$Summer <- rowMeans(vlf.tmean[,c("Jun", "Jul", "Aug")])

summary(vlf.tmean)

vlf.tmin$pFall <- rowMeans(vlf.tmin[,c("pSep", "pOct", "pNov")])
vlf.tmin$Winter <- rowMeans(vlf.tmin[,c("pDec", "Jan", "Feb")])
vlf.tmin$Spring <- rowMeans(vlf.tmin[,c("Mar", "Apr", "May")])
vlf.tmin$Summer <- rowMeans(vlf.tmin[,c("Jun", "Jul", "Aug")])

summary(vlf.tmin)

vlf.tmax$pFall <- rowMeans(vlf.tmax[,c("pSep", "pOct", "pNov")])
vlf.tmax$Winter <- rowMeans(vlf.tmax[,c("pDec", "Jan", "Feb")])
vlf.tmax$Spring <- rowMeans(vlf.tmax[,c("Mar", "Apr", "May")])
vlf.tmax$Summer <- rowMeans(vlf.tmax[,c("Jun", "Jul", "Aug")])

summary(vlf.tmax)

vlf.precip$pFall <- rowSums(vlf.precip[,c("pSep", "pOct", "pNov")])
vlf.precip$Winter <- rowSums(vlf.precip[,c("pDec", "Jan", "Feb")])
vlf.precip$Spring <- rowSums(vlf.precip[,c("Mar", "Apr", "May")])
vlf.precip$Summer <- rowSums(vlf.precip[,c("Jun", "Jul", "Aug")])

summary(vlf.precip)


#################################
# Mean Monthly Temperature
#################################
# Upper Site
# increment only
vuf.inc.tmean <- as.data.frame(matrix(NA,nrow=ncol(vuf.inc.only), ncol=ncol(vuf.tmean)-2))
dim(vuf.inc.tmean)

names(vuf.inc.tmean)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.inc.tmean) <- names(vuf.inc.only)
summary(vuf.inc.tmean) 


for(i in 1:ncol(vuf.inc.only)){
	vuf.inc.tmean[i,] <- cor(vuf.inc.only[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.inc.tmean)

vuf.tmean.upper.inc <- vuf.inc.tmean
vuf.tmean.lower.inc <- vuf.inc.tmean
for(j in 1:ncol(vuf.inc.tmean)){
	vuf.tmean.upper.inc[,j] <- quantile(vuf.inc.tmean[,j],0.975, na.rm=T)
	vuf.tmean.lower.inc[,j] <- quantile(vuf.inc.tmean[,j],0.025, na.rm=T)
}
head(vuf.tmean.upper.inc)
head(vuf.tmean.lower.inc)

#------------------------------------------------------
# Allometry only

vuf.allom.tmean <- as.data.frame(matrix(NA,nrow=ncol(vuf.allom.only), ncol=ncol(vuf.tmean)-2))
dim(vuf.allom.tmean)

names(vuf.allom.tmean)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.allom.tmean) <- names(vuf.allom.only)
summary(vuf.allom.tmean) 


for(i in 1:ncol(vuf.allom.only)){
	vuf.allom.tmean[i,] <- cor(vuf.allom.only[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.allom.tmean)

vuf.tmean.upper.allom <- vuf.allom.tmean
vuf.tmean.lower.allom <- vuf.allom.tmean
for(j in 1:ncol(vuf.allom.tmean)){
	vuf.tmean.upper.allom[,j] <- quantile(vuf.allom.tmean[,j],0.975, na.rm=T)
	vuf.tmean.lower.allom[,j] <- quantile(vuf.allom.tmean[,j],0.025, na.rm=T)
}
head(vuf.tmean.upper.allom)
head(vuf.tmean.lower.allom)

#------------------------------------------------------
# Density Only
vuf.dens.tmean <- as.data.frame(matrix(NA,nrow=ncol(vuf.dens.only), ncol=ncol(vuf.tmean)-2))
dim(vuf.dens.tmean)

names(vuf.dens.tmean)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.dens.tmean) <- names(vuf.dens.only)
summary(vuf.dens.tmean) 


for(i in 1:ncol(vuf.dens.only)){
	vuf.dens.tmean[i,] <- cor(vuf.dens.only[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.dens.tmean)

vuf.tmean.upper.dens <- vuf.dens.tmean
vuf.tmean.lower.dens <- vuf.dens.tmean
for(j in 1:ncol(vuf.dens.tmean)){
	vuf.tmean.upper.dens[,j] <- quantile(vuf.dens.tmean[,j],0.975, na.rm=T)
	vuf.tmean.lower.dens[,j] <- quantile(vuf.dens.tmean[,j],0.025, na.rm=T)
}
head(vuf.tmean.upper.dens)
head(vuf.tmean.lower.dens)
#------------------------------------------------------
# Mortality Only

vuf.mort.tmean <- as.data.frame(matrix(NA,nrow=ncol(vuf.mort.only), ncol=ncol(vuf.tmean)-2))
dim(vuf.mort.tmean)

names(vuf.mort.tmean)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.mort.tmean) <- names(vuf.mort.only)
summary(vuf.mort.tmean) 


for(i in 1:ncol(vuf.mort.only)){
	vuf.mort.tmean[i,] <- cor(vuf.mort.only[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.mort.tmean)

vuf.tmean.upper.mort <- vuf.mort.tmean
vuf.tmean.lower.mort <- vuf.mort.tmean
for(j in 1:ncol(vuf.mort.tmean)){
	vuf.tmean.upper.mort[,j] <- quantile(vuf.mort.tmean[,j],0.975, na.rm=T)
	vuf.tmean.lower.mort[,j] <- quantile(vuf.mort.tmean[,j],0.025, na.rm=T)
}
head(vuf.tmean.upper.mort)
head(vuf.tmean.lower.mort)
#------------------------------------------------------
#------------------------------------------------------
# Lower Site

vlf.inc.tmean <- as.data.frame(matrix(NA,nrow=ncol(vlf.inc.only), ncol=ncol(vlf.tmean)-2))
dim(vlf.inc.tmean)

names(vlf.inc.tmean)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.inc.tmean) <- names(vlf.inc.only)
summary(vlf.inc.tmean) 


for(i in 1:ncol(vlf.inc.only)){
	vlf.inc.tmean[i,] <- cor(vlf.inc.only[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.inc.tmean)

vlf.tmean.upper.inc <- vlf.inc.tmean
vlf.tmean.lower.inc <- vlf.inc.tmean
for(j in 1:ncol(vlf.inc.tmean)){
	vlf.tmean.upper.inc[,j] <- quantile(vlf.inc.tmean[,j],0.975, na.rm=T)
	vlf.tmean.lower.inc[,j] <- quantile(vlf.inc.tmean[,j],0.025, na.rm=T)
}
head(vlf.tmean.upper.inc)
head(vlf.tmean.lower.inc)

#------------------------------------------------------
# Allometry only

vlf.allom.tmean <- as.data.frame(matrix(NA,nrow=ncol(vlf.allom.only), ncol=ncol(vlf.tmean)-2))
dim(vlf.allom.tmean)

names(vlf.allom.tmean)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.allom.tmean) <- names(vlf.allom.only)
summary(vlf.allom.tmean) 


for(i in 1:ncol(vlf.allom.only)){
	vlf.allom.tmean[i,] <- cor(vlf.allom.only[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.allom.tmean)

vlf.tmean.upper.allom <- vlf.allom.tmean
vlf.tmean.lower.allom <- vlf.allom.tmean
for(j in 1:ncol(vlf.allom.tmean)){
	vlf.tmean.upper.allom[,j] <- quantile(vlf.allom.tmean[,j],0.975, na.rm=T)
	vlf.tmean.lower.allom[,j] <- quantile(vlf.allom.tmean[,j],0.025, na.rm=T)
}
head(vlf.tmean.upper.allom)
head(vlf.tmean.lower.allom)

#------------------------------------------------------
# Density Only
vlf.dens.tmean <- as.data.frame(matrix(NA,nrow=ncol(vlf.dens.only), ncol=ncol(vlf.tmean)-2))
dim(vlf.dens.tmean)

names(vlf.dens.tmean)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.dens.tmean) <- names(vlf.dens.only)
summary(vlf.dens.tmean) 


for(i in 1:ncol(vlf.dens.only)){
	vlf.dens.tmean[i,] <- cor(vlf.dens.only[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.dens.tmean)

vlf.tmean.upper.dens <- vlf.dens.tmean
vlf.tmean.lower.dens <- vlf.dens.tmean
for(j in 1:ncol(vlf.dens.tmean)){
	vlf.tmean.upper.dens[,j] <- quantile(vlf.dens.tmean[,j],0.975, na.rm=T)
	vlf.tmean.lower.dens[,j] <- quantile(vlf.dens.tmean[,j],0.025, na.rm=T)
}
head(vlf.tmean.upper.dens)
head(vlf.tmean.lower.dens)
#------------------------------------------------------
# Mortality Only

vlf.mort.tmean <- as.data.frame(matrix(NA,nrow=ncol(vlf.mort.only), ncol=ncol(vlf.tmean)-2))
dim(vlf.mort.tmean)

names(vlf.mort.tmean)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.mort.tmean) <- names(vlf.mort.only)
summary(vlf.mort.tmean) 


for(i in 1:ncol(vlf.mort.only)){
	vlf.mort.tmean[i,] <- cor(vlf.mort.only[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.mort.tmean)

vlf.tmean.upper.mort <- vlf.mort.tmean
vlf.tmean.lower.mort <- vlf.mort.tmean
for(j in 1:ncol(vlf.mort.tmean)){
	vlf.tmean.upper.mort[,j] <- quantile(vlf.mort.tmean[,j],0.975, na.rm=T)
	vlf.tmean.lower.mort[,j] <- quantile(vlf.mort.tmean[,j],0.025, na.rm=T)
}
head(vlf.tmean.upper.mort)
head(vlf.tmean.lower.mort)

#################################
# Maximum Monthly Temperature
#################################
# Upper Site
# increment only
vuf.inc.tmax <- as.data.frame(matrix(NA,nrow=ncol(vuf.inc.only), ncol=ncol(vuf.tmax)-2))
dim(vuf.inc.tmax)

names(vuf.inc.tmax)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.inc.tmax) <- names(vuf.inc.only)
summary(vuf.inc.tmax) 


for(i in 1:ncol(vuf.inc.only)){
	vuf.inc.tmax[i,] <- cor(vuf.inc.only[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.inc.tmax)

vuf.tmax.upper.inc <- vuf.inc.tmax
vuf.tmax.lower.inc <- vuf.inc.tmax
for(j in 1:ncol(vuf.inc.tmax)){
	vuf.tmax.upper.inc[,j] <- quantile(vuf.inc.tmax[,j],0.975, na.rm=T)
	vuf.tmax.lower.inc[,j] <- quantile(vuf.inc.tmax[,j],0.025, na.rm=T)
}
head(vuf.tmax.upper.inc)
head(vuf.tmax.lower.inc)

#------------------------------------------------------
# Allometry only

vuf.allom.tmax <- as.data.frame(matrix(NA,nrow=ncol(vuf.allom.only), ncol=ncol(vuf.tmax)-2))
dim(vuf.allom.tmax)

names(vuf.allom.tmax)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.allom.tmax) <- names(vuf.allom.only)
summary(vuf.allom.tmax) 


for(i in 1:ncol(vuf.allom.only)){
	vuf.allom.tmax[i,] <- cor(vuf.allom.only[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.allom.tmax)

vuf.tmax.upper.allom <- vuf.allom.tmax
vuf.tmax.lower.allom <- vuf.allom.tmax
for(j in 1:ncol(vuf.allom.tmax)){
	vuf.tmax.upper.allom[,j] <- quantile(vuf.allom.tmax[,j],0.975, na.rm=T)
	vuf.tmax.lower.allom[,j] <- quantile(vuf.allom.tmax[,j],0.025, na.rm=T)
}
head(vuf.tmax.upper.allom)
head(vuf.tmax.lower.allom)

#------------------------------------------------------
# Density Only
vuf.dens.tmax <- as.data.frame(matrix(NA,nrow=ncol(vuf.dens.only), ncol=ncol(vuf.tmax)-2))
dim(vuf.dens.tmax)

names(vuf.dens.tmax)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.dens.tmax) <- names(vuf.dens.only)
summary(vuf.dens.tmax) 


for(i in 1:ncol(vuf.dens.only)){
	vuf.dens.tmax[i,] <- cor(vuf.dens.only[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.dens.tmax)

vuf.tmax.upper.dens <- vuf.dens.tmax
vuf.tmax.lower.dens <- vuf.dens.tmax
for(j in 1:ncol(vuf.dens.tmax)){
	vuf.tmax.upper.dens[,j] <- quantile(vuf.dens.tmax[,j],0.975, na.rm=T)
	vuf.tmax.lower.dens[,j] <- quantile(vuf.dens.tmax[,j],0.025, na.rm=T)
}
head(vuf.tmax.upper.dens)
head(vuf.tmax.lower.dens)
#------------------------------------------------------
# Mortality Only

vuf.mort.tmax <- as.data.frame(matrix(NA,nrow=ncol(vuf.mort.only), ncol=ncol(vuf.tmax)-2))
dim(vuf.mort.tmax)

names(vuf.mort.tmax)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.mort.tmax) <- names(vuf.mort.only)
summary(vuf.mort.tmax) 


for(i in 1:ncol(vuf.mort.only)){
	vuf.mort.tmax[i,] <- cor(vuf.mort.only[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.mort.tmax)

vuf.tmax.upper.mort <- vuf.mort.tmax
vuf.tmax.lower.mort <- vuf.mort.tmax
for(j in 1:ncol(vuf.mort.tmax)){
	vuf.tmax.upper.mort[,j] <- quantile(vuf.mort.tmax[,j],0.975, na.rm=T)
	vuf.tmax.lower.mort[,j] <- quantile(vuf.mort.tmax[,j],0.025, na.rm=T)
}
head(vuf.tmax.upper.mort)
head(vuf.tmax.lower.mort)
#------------------------------------------------------
#------------------------------------------------------
# Lower Site

vlf.inc.tmax <- as.data.frame(matrix(NA,nrow=ncol(vlf.inc.only), ncol=ncol(vlf.tmax)-2))
dim(vlf.inc.tmax)

names(vlf.inc.tmax)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.inc.tmax) <- names(vlf.inc.only)
summary(vlf.inc.tmax) 


for(i in 1:ncol(vlf.inc.only)){
	vlf.inc.tmax[i,] <- cor(vlf.inc.only[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.inc.tmax)

vlf.tmax.upper.inc <- vlf.inc.tmax
vlf.tmax.lower.inc <- vlf.inc.tmax
for(j in 1:ncol(vlf.inc.tmax)){
	vlf.tmax.upper.inc[,j] <- quantile(vlf.inc.tmax[,j],0.975, na.rm=T)
	vlf.tmax.lower.inc[,j] <- quantile(vlf.inc.tmax[,j],0.025, na.rm=T)
}
head(vlf.tmax.upper.inc)
head(vlf.tmax.lower.inc)

#------------------------------------------------------
# Allometry only

vlf.allom.tmax <- as.data.frame(matrix(NA,nrow=ncol(vlf.allom.only), ncol=ncol(vlf.tmax)-2))
dim(vlf.allom.tmax)

names(vlf.allom.tmax)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.allom.tmax) <- names(vlf.allom.only)
summary(vlf.allom.tmax) 


for(i in 1:ncol(vlf.allom.only)){
	vlf.allom.tmax[i,] <- cor(vlf.allom.only[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.allom.tmax)

vlf.tmax.upper.allom <- vlf.allom.tmax
vlf.tmax.lower.allom <- vlf.allom.tmax
for(j in 1:ncol(vlf.allom.tmax)){
	vlf.tmax.upper.allom[,j] <- quantile(vlf.allom.tmax[,j],0.975, na.rm=T)
	vlf.tmax.lower.allom[,j] <- quantile(vlf.allom.tmax[,j],0.025, na.rm=T)
}
head(vlf.tmax.upper.allom)
head(vlf.tmax.lower.allom)

#------------------------------------------------------
# Density Only
vlf.dens.tmax <- as.data.frame(matrix(NA,nrow=ncol(vlf.dens.only), ncol=ncol(vlf.tmax)-2))
dim(vlf.dens.tmax)

names(vlf.dens.tmax)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.dens.tmax) <- names(vlf.dens.only)
summary(vlf.dens.tmax) 


for(i in 1:ncol(vlf.dens.only)){
	vlf.dens.tmax[i,] <- cor(vlf.dens.only[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.dens.tmax)

vlf.tmax.upper.dens <- vlf.dens.tmax
vlf.tmax.lower.dens <- vlf.dens.tmax
for(j in 1:ncol(vlf.dens.tmax)){
	vlf.tmax.upper.dens[,j] <- quantile(vlf.dens.tmax[,j],0.975, na.rm=T)
	vlf.tmax.lower.dens[,j] <- quantile(vlf.dens.tmax[,j],0.025, na.rm=T)
}
head(vlf.tmax.upper.dens)
head(vlf.tmax.lower.dens)
#------------------------------------------------------
# Mortality Only

vlf.mort.tmax <- as.data.frame(matrix(NA,nrow=ncol(vlf.mort.only), ncol=ncol(vlf.tmax)-2))
dim(vlf.mort.tmax)

names(vlf.mort.tmax)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.mort.tmax) <- names(vlf.mort.only)
summary(vlf.mort.tmax) 


for(i in 1:ncol(vlf.mort.only)){
	vlf.mort.tmax[i,] <- cor(vlf.mort.only[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.mort.tmax)

vlf.tmax.upper.mort <- vlf.mort.tmax
vlf.tmax.lower.mort <- vlf.mort.tmax
for(j in 1:ncol(vlf.mort.tmax)){
	vlf.tmax.upper.mort[,j] <- quantile(vlf.mort.tmax[,j],0.975, na.rm=T)
	vlf.tmax.lower.mort[,j] <- quantile(vlf.mort.tmax[,j],0.025, na.rm=T)
}
head(vlf.tmax.upper.mort)
head(vlf.tmax.lower.mort)

#################################
# Minimum Monthly Temperature
#################################
# Upper Site
# increment only
vuf.inc.tmin <- as.data.frame(matrix(NA,nrow=ncol(vuf.inc.only), ncol=ncol(vuf.tmin)-2))
dim(vuf.inc.tmin)

names(vuf.inc.tmin)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.inc.tmin) <- names(vuf.inc.only)
summary(vuf.inc.tmin) 


for(i in 1:ncol(vuf.inc.only)){
	vuf.inc.tmin[i,] <- cor(vuf.inc.only[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.inc.tmin)

vuf.tmin.upper.inc <- vuf.inc.tmin
vuf.tmin.lower.inc <- vuf.inc.tmin
for(j in 1:ncol(vuf.inc.tmin)){
	vuf.tmin.upper.inc[,j] <- quantile(vuf.inc.tmin[,j],0.975, na.rm=T)
	vuf.tmin.lower.inc[,j] <- quantile(vuf.inc.tmin[,j],0.025, na.rm=T)
}
head(vuf.tmin.upper.inc)
head(vuf.tmin.lower.inc)

#------------------------------------------------------
# Allometry only

vuf.allom.tmin <- as.data.frame(matrix(NA,nrow=ncol(vuf.allom.only), ncol=ncol(vuf.tmin)-2))
dim(vuf.allom.tmin)

names(vuf.allom.tmin)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.allom.tmin) <- names(vuf.allom.only)
summary(vuf.allom.tmin) 


for(i in 1:ncol(vuf.allom.only)){
	vuf.allom.tmin[i,] <- cor(vuf.allom.only[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.allom.tmin)

vuf.tmin.upper.allom <- vuf.allom.tmin
vuf.tmin.lower.allom <- vuf.allom.tmin
for(j in 1:ncol(vuf.allom.tmin)){
	vuf.tmin.upper.allom[,j] <- quantile(vuf.allom.tmin[,j],0.975, na.rm=T)
	vuf.tmin.lower.allom[,j] <- quantile(vuf.allom.tmin[,j],0.025, na.rm=T)
}
head(vuf.tmin.upper.allom)
head(vuf.tmin.lower.allom)

#------------------------------------------------------
# Density Only
vuf.dens.tmin <- as.data.frame(matrix(NA,nrow=ncol(vuf.dens.only), ncol=ncol(vuf.tmin)-2))
dim(vuf.dens.tmin)

names(vuf.dens.tmin)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.dens.tmin) <- names(vuf.dens.only)
summary(vuf.dens.tmin) 


for(i in 1:ncol(vuf.dens.only)){
	vuf.dens.tmin[i,] <- cor(vuf.dens.only[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.dens.tmin)

vuf.tmin.upper.dens <- vuf.dens.tmin
vuf.tmin.lower.dens <- vuf.dens.tmin
for(j in 1:ncol(vuf.dens.tmin)){
	vuf.tmin.upper.dens[,j] <- quantile(vuf.dens.tmin[,j],0.975, na.rm=T)
	vuf.tmin.lower.dens[,j] <- quantile(vuf.dens.tmin[,j],0.025, na.rm=T)
}
head(vuf.tmin.upper.dens)
head(vuf.tmin.lower.dens)
#------------------------------------------------------
# Mortality Only

vuf.mort.tmin <- as.data.frame(matrix(NA,nrow=ncol(vuf.mort.only), ncol=ncol(vuf.tmin)-2))
dim(vuf.mort.tmin)

names(vuf.mort.tmin)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.mort.tmin) <- names(vuf.mort.only)
summary(vuf.mort.tmin) 


for(i in 1:ncol(vuf.mort.only)){
	vuf.mort.tmin[i,] <- cor(vuf.mort.only[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.mort.tmin)

vuf.tmin.upper.mort <- vuf.mort.tmin
vuf.tmin.lower.mort <- vuf.mort.tmin
for(j in 1:ncol(vuf.mort.tmin)){
	vuf.tmin.upper.mort[,j] <- quantile(vuf.mort.tmin[,j],0.975, na.rm=T)
	vuf.tmin.lower.mort[,j] <- quantile(vuf.mort.tmin[,j],0.025, na.rm=T)
}
head(vuf.tmin.upper.mort)
head(vuf.tmin.lower.mort)
#------------------------------------------------------
#------------------------------------------------------
# Lower Site

vlf.inc.tmin <- as.data.frame(matrix(NA,nrow=ncol(vlf.inc.only), ncol=ncol(vlf.tmin)-2))
dim(vlf.inc.tmin)

names(vlf.inc.tmin)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.inc.tmin) <- names(vlf.inc.only)
summary(vlf.inc.tmin) 


for(i in 1:ncol(vlf.inc.only)){
	vlf.inc.tmin[i,] <- cor(vlf.inc.only[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.inc.tmin)

vlf.tmin.upper.inc <- vlf.inc.tmin
vlf.tmin.lower.inc <- vlf.inc.tmin
for(j in 1:ncol(vlf.inc.tmin)){
	vlf.tmin.upper.inc[,j] <- quantile(vlf.inc.tmin[,j],0.975, na.rm=T)
	vlf.tmin.lower.inc[,j] <- quantile(vlf.inc.tmin[,j],0.025, na.rm=T)
}
head(vlf.tmin.upper.inc)
head(vlf.tmin.lower.inc)

#------------------------------------------------------
# Allometry only

vlf.allom.tmin <- as.data.frame(matrix(NA,nrow=ncol(vlf.allom.only), ncol=ncol(vlf.tmin)-2))
dim(vlf.allom.tmin)

names(vlf.allom.tmin)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.allom.tmin) <- names(vlf.allom.only)
summary(vlf.allom.tmin) 


for(i in 1:ncol(vlf.allom.only)){
	vlf.allom.tmin[i,] <- cor(vlf.allom.only[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.allom.tmin)

vlf.tmin.upper.allom <- vlf.allom.tmin
vlf.tmin.lower.allom <- vlf.allom.tmin
for(j in 1:ncol(vlf.allom.tmin)){
	vlf.tmin.upper.allom[,j] <- quantile(vlf.allom.tmin[,j],0.975, na.rm=T)
	vlf.tmin.lower.allom[,j] <- quantile(vlf.allom.tmin[,j],0.025, na.rm=T)
}
head(vlf.tmin.upper.allom)
head(vlf.tmin.lower.allom)

#------------------------------------------------------
# Density Only
vlf.dens.tmin <- as.data.frame(matrix(NA,nrow=ncol(vlf.dens.only), ncol=ncol(vlf.tmin)-2))
dim(vlf.dens.tmin)

names(vlf.dens.tmin)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.dens.tmin) <- names(vlf.dens.only)
summary(vlf.dens.tmin) 


for(i in 1:ncol(vlf.dens.only)){
	vlf.dens.tmin[i,] <- cor(vlf.dens.only[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.dens.tmin)

vlf.tmin.upper.dens <- vlf.dens.tmin
vlf.tmin.lower.dens <- vlf.dens.tmin
for(j in 1:ncol(vlf.dens.tmin)){
	vlf.tmin.upper.dens[,j] <- quantile(vlf.dens.tmin[,j],0.975, na.rm=T)
	vlf.tmin.lower.dens[,j] <- quantile(vlf.dens.tmin[,j],0.025, na.rm=T)
}
head(vlf.tmin.upper.dens)
head(vlf.tmin.lower.dens)
#------------------------------------------------------
# Mortality Only

vlf.mort.tmin <- as.data.frame(matrix(NA,nrow=ncol(vlf.mort.only), ncol=ncol(vlf.tmin)-2))
dim(vlf.mort.tmin)

names(vlf.mort.tmin)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.mort.tmin) <- names(vlf.mort.only)
summary(vlf.mort.tmin) 


for(i in 1:ncol(vlf.mort.only)){
	vlf.mort.tmin[i,] <- cor(vlf.mort.only[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.mort.tmin)

vlf.tmin.upper.mort <- vlf.mort.tmin
vlf.tmin.lower.mort <- vlf.mort.tmin
for(j in 1:ncol(vlf.mort.tmin)){
	vlf.tmin.upper.mort[,j] <- quantile(vlf.mort.tmin[,j],0.975, na.rm=T)
	vlf.tmin.lower.mort[,j] <- quantile(vlf.mort.tmin[,j],0.025, na.rm=T)
}
head(vlf.tmin.upper.mort)
head(vlf.tmin.lower.mort)
#################################
# Precipitation
#################################
# Upper Site
# increment only
vuf.inc.precip <- as.data.frame(matrix(NA,nrow=ncol(vuf.inc.only), ncol=ncol(vuf.precip)-2))
dim(vuf.inc.precip)

names(vuf.inc.precip)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.inc.precip) <- names(vuf.inc.only)
summary(vuf.inc.precip) 


for(i in 1:ncol(vuf.inc.only)){
	vuf.inc.precip[i,] <- cor(vuf.inc.only[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.inc.precip)

vuf.precip.upper.inc <- vuf.inc.precip
vuf.precip.lower.inc <- vuf.inc.precip
for(j in 1:ncol(vuf.inc.precip)){
	vuf.precip.upper.inc[,j] <- quantile(vuf.inc.precip[,j],0.975, na.rm=T)
	vuf.precip.lower.inc[,j] <- quantile(vuf.inc.precip[,j],0.025, na.rm=T)
}
head(vuf.precip.upper.inc)
head(vuf.precip.lower.inc)

#------------------------------------------------------
# Allometry only

vuf.allom.precip <- as.data.frame(matrix(NA,nrow=ncol(vuf.allom.only), ncol=ncol(vuf.precip)-2))
dim(vuf.allom.precip)

names(vuf.allom.precip)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.allom.precip) <- names(vuf.allom.only)
summary(vuf.allom.precip) 


for(i in 1:ncol(vuf.allom.only)){
	vuf.allom.precip[i,] <- cor(vuf.allom.only[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.allom.precip)

vuf.precip.upper.allom <- vuf.allom.precip
vuf.precip.lower.allom <- vuf.allom.precip
for(j in 1:ncol(vuf.allom.precip)){
	vuf.precip.upper.allom[,j] <- quantile(vuf.allom.precip[,j],0.975, na.rm=T)
	vuf.precip.lower.allom[,j] <- quantile(vuf.allom.precip[,j],0.025, na.rm=T)
}
head(vuf.precip.upper.allom)
head(vuf.precip.lower.allom)

#------------------------------------------------------
# Density Only
vuf.dens.precip <- as.data.frame(matrix(NA,nrow=ncol(vuf.dens.only), ncol=ncol(vuf.precip)-2))
dim(vuf.dens.precip)

names(vuf.dens.precip)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.dens.precip) <- names(vuf.dens.only)
summary(vuf.dens.precip) 


for(i in 1:ncol(vuf.dens.only)){
	vuf.dens.precip[i,] <- cor(vuf.dens.only[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.dens.precip)

vuf.precip.upper.dens <- vuf.dens.precip
vuf.precip.lower.dens <- vuf.dens.precip
for(j in 1:ncol(vuf.dens.precip)){
	vuf.precip.upper.dens[,j] <- quantile(vuf.dens.precip[,j],0.975, na.rm=T)
	vuf.precip.lower.dens[,j] <- quantile(vuf.dens.precip[,j],0.025, na.rm=T)
}
head(vuf.precip.upper.dens)
head(vuf.precip.lower.dens)
#------------------------------------------------------
# Mortality Only

vuf.mort.precip <- as.data.frame(matrix(NA,nrow=ncol(vuf.mort.only), ncol=ncol(vuf.precip)-2))
dim(vuf.mort.precip)

names(vuf.mort.precip)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.mort.precip) <- names(vuf.mort.only)
summary(vuf.mort.precip) 


for(i in 1:ncol(vuf.mort.only)){
	vuf.mort.precip[i,] <- cor(vuf.mort.only[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.mort.precip)

vuf.precip.upper.mort <- vuf.mort.precip
vuf.precip.lower.mort <- vuf.mort.precip
for(j in 1:ncol(vuf.mort.precip)){
	vuf.precip.upper.mort[,j] <- quantile(vuf.mort.precip[,j],0.975, na.rm=T)
	vuf.precip.lower.mort[,j] <- quantile(vuf.mort.precip[,j],0.025, na.rm=T)
}
head(vuf.precip.upper.mort)
head(vuf.precip.lower.mort)
#------------------------------------------------------
#------------------------------------------------------
# Lower Site

vlf.inc.precip <- as.data.frame(matrix(NA,nrow=ncol(vlf.inc.only), ncol=ncol(vlf.precip)-2))
dim(vlf.inc.precip)

names(vlf.inc.precip)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.inc.precip) <- names(vlf.inc.only)
summary(vlf.inc.precip) 


for(i in 1:ncol(vlf.inc.only)){
	vlf.inc.precip[i,] <- cor(vlf.inc.only[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.inc.precip)

vlf.precip.upper.inc <- vlf.inc.precip
vlf.precip.lower.inc <- vlf.inc.precip
for(j in 1:ncol(vlf.inc.precip)){
	vlf.precip.upper.inc[,j] <- quantile(vlf.inc.precip[,j],0.975, na.rm=T)
	vlf.precip.lower.inc[,j] <- quantile(vlf.inc.precip[,j],0.025, na.rm=T)
}
head(vlf.precip.upper.inc)
head(vlf.precip.lower.inc)

#------------------------------------------------------
# Allometry only

vlf.allom.precip <- as.data.frame(matrix(NA,nrow=ncol(vlf.allom.only), ncol=ncol(vlf.precip)-2))
dim(vlf.allom.precip)

names(vlf.allom.precip)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.allom.precip) <- names(vlf.allom.only)
summary(vlf.allom.precip) 


for(i in 1:ncol(vlf.allom.only)){
	vlf.allom.precip[i,] <- cor(vlf.allom.only[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.allom.precip)

vlf.precip.upper.allom <- vlf.allom.precip
vlf.precip.lower.allom <- vlf.allom.precip
for(j in 1:ncol(vlf.allom.precip)){
	vlf.precip.upper.allom[,j] <- quantile(vlf.allom.precip[,j],0.975, na.rm=T)
	vlf.precip.lower.allom[,j] <- quantile(vlf.allom.precip[,j],0.025, na.rm=T)
}
head(vlf.precip.upper.allom)
head(vlf.precip.lower.allom)

#------------------------------------------------------
# Density Only
vlf.dens.precip <- as.data.frame(matrix(NA,nrow=ncol(vlf.dens.only), ncol=ncol(vlf.precip)-2))
dim(vlf.dens.precip)

names(vlf.dens.precip)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.dens.precip) <- names(vlf.dens.only)
summary(vlf.dens.precip) 


for(i in 1:ncol(vlf.dens.only)){
	vlf.dens.precip[i,] <- cor(vlf.dens.only[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.dens.precip)

vlf.precip.upper.dens <- vlf.dens.precip
vlf.precip.lower.dens <- vlf.dens.precip
for(j in 1:ncol(vlf.dens.precip)){
	vlf.precip.upper.dens[,j] <- quantile(vlf.dens.precip[,j],0.975, na.rm=T)
	vlf.precip.lower.dens[,j] <- quantile(vlf.dens.precip[,j],0.025, na.rm=T)
}
head(vlf.precip.upper.dens)
head(vlf.precip.lower.dens)
#------------------------------------------------------
# Mortality Only

vlf.mort.precip <- as.data.frame(matrix(NA,nrow=ncol(vlf.mort.only), ncol=ncol(vlf.precip)-2))
dim(vlf.mort.precip)

names(vlf.mort.precip)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.mort.precip) <- names(vlf.mort.only)
summary(vlf.mort.precip) 


for(i in 1:ncol(vlf.mort.only)){
	vlf.mort.precip[i,] <- cor(vlf.mort.only[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.mort.precip)

vlf.precip.upper.mort <- vlf.mort.precip
vlf.precip.lower.mort <- vlf.mort.precip
for(j in 1:ncol(vlf.mort.precip)){
	vlf.precip.upper.mort[,j] <- quantile(vlf.mort.precip[,j],0.975, na.rm=T)
	vlf.precip.lower.mort[,j] <- quantile(vlf.mort.precip[,j],0.025, na.rm=T)
}
head(vlf.precip.upper.mort)
head(vlf.precip.lower.mort)

###################################################################################################
# Plotting the correlations as bar charts
###################################################################################################
#--------------------------------------------
# Stacking things to get them into ggplot format
# Upper Site
#--------------------------------------------
# Tmean
#--------------------------------------------

# Increment only
vuf.tmean.inc.stack <- stack(vuf.inc.tmean)
summary(vuf.tmean.inc.stack)
names(vuf.tmean.inc.stack) <- c("corr", "month")
vuf.tmean.inc.stack$site <- as.factor("VUF")
vuf.tmean.inc.stack$type <- as.factor("tmean")
vuf.tmean.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmean.inc.stack)

vuf.tmean.upper.inc.stack <- stack(vuf.tmean.upper.inc)
summary(vuf.tmean.upper.inc.stack)
names(vuf.tmean.upper.inc.stack) <- c("Upper", "month")
vuf.tmean.upper.inc.stack$site <- as.factor("VUF")
vuf.tmean.upper.inc.stack$type <- as.factor("tmean")
vuf.tmean.upper.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmean.upper.inc.stack)

vuf.tmean.lower.inc.stack <- stack(vuf.tmean.lower.inc)
summary(vuf.tmean.lower.inc.stack)
names(vuf.tmean.lower.inc.stack) <- c("Lower", "month")
vuf.tmean.lower.inc.stack$site <- as.factor("VUF")
vuf.tmean.lower.inc.stack$type <- as.factor("tmean")
vuf.tmean.lower.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmean.lower.inc.stack)

vuf.inc.tmean.stack <- cbind(vuf.tmean.inc.stack, vuf.tmean.upper.inc.stack[,"Upper"], vuf.tmean.lower.inc.stack[,"Lower"])
summary(vuf.inc.tmean.stack)

names(vuf.inc.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmean.inc.stack <- stack(vlf.inc.tmean)
summary(vlf.tmean.inc.stack)
names(vlf.tmean.inc.stack) <- c("corr", "month")
vlf.tmean.inc.stack$site <- as.factor("vlf")
vlf.tmean.inc.stack$type <- as.factor("tmean")
vlf.tmean.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmean.inc.stack)

vlf.tmean.upper.inc.stack <- stack(vlf.tmean.upper.inc)
summary(vlf.tmean.upper.inc.stack)
names(vlf.tmean.upper.inc.stack) <- c("Upper", "month")
vlf.tmean.upper.inc.stack$site <- as.factor("vlf")
vlf.tmean.upper.inc.stack$type <- as.factor("tmean")
vlf.tmean.upper.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmean.upper.inc.stack)

vlf.tmean.lower.inc.stack <- stack(vlf.tmean.lower.inc)
summary(vlf.tmean.lower.inc.stack)
names(vlf.tmean.lower.inc.stack) <- c("Lower", "month")
vlf.tmean.lower.inc.stack$site <- as.factor("vlf")
vlf.tmean.lower.inc.stack$type <- as.factor("tmean")
vlf.tmean.lower.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmean.lower.inc.stack)

vlf.inc.tmean.stack <- cbind(vlf.tmean.inc.stack, vlf.tmean.upper.inc.stack[,"Upper"], vlf.tmean.lower.inc.stack[,"Lower"])
summary(vlf.inc.tmean.stack)

names(vlf.inc.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#----------------------------------------------
# Allometry Only

vuf.tmean.allom.stack <- stack(vuf.allom.tmean)
summary(vuf.tmean.allom.stack)
names(vuf.tmean.allom.stack) <- c("corr", "month")
vuf.tmean.allom.stack$site <- as.factor("VUF")
vuf.tmean.allom.stack$type <- as.factor("tmean")
vuf.tmean.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmean.allom.stack)

vuf.tmean.upper.allom.stack <- stack(vuf.tmean.upper.allom)
summary(vuf.tmean.upper.allom.stack)
names(vuf.tmean.upper.allom.stack) <- c("Upper", "month")
vuf.tmean.upper.allom.stack$site <- as.factor("VUF")
vuf.tmean.upper.allom.stack$type <- as.factor("tmean")
vuf.tmean.upper.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmean.upper.allom.stack)

vuf.tmean.lower.allom.stack <- stack(vuf.tmean.lower.allom)
summary(vuf.tmean.lower.allom.stack)
names(vuf.tmean.lower.allom.stack) <- c("Lower", "month")
vuf.tmean.lower.allom.stack$site <- as.factor("VUF")
vuf.tmean.lower.allom.stack$type <- as.factor("tmean")
vuf.tmean.lower.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmean.lower.allom.stack)

vuf.allom.tmean.stack <- cbind(vuf.tmean.allom.stack, vuf.tmean.upper.allom.stack[,"Upper"], vuf.tmean.lower.allom.stack[,"Lower"])
summary(vuf.allom.tmean.stack)

names(vuf.allom.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmean.allom.stack <- stack(vlf.allom.tmean)
summary(vlf.tmean.allom.stack)
names(vlf.tmean.allom.stack) <- c("corr", "month")
vlf.tmean.allom.stack$site <- as.factor("vlf")
vlf.tmean.allom.stack$type <- as.factor("tmean")
vlf.tmean.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmean.allom.stack)

vlf.tmean.upper.allom.stack <- stack(vlf.tmean.upper.allom)
summary(vlf.tmean.upper.allom.stack)
names(vlf.tmean.upper.allom.stack) <- c("Upper", "month")
vlf.tmean.upper.allom.stack$site <- as.factor("vlf")
vlf.tmean.upper.allom.stack$type <- as.factor("tmean")
vlf.tmean.upper.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmean.upper.allom.stack)

vlf.tmean.lower.allom.stack <- stack(vlf.tmean.lower.allom)
summary(vlf.tmean.lower.allom.stack)
names(vlf.tmean.lower.allom.stack) <- c("Lower", "month")
vlf.tmean.lower.allom.stack$site <- as.factor("vlf")
vlf.tmean.lower.allom.stack$type <- as.factor("tmean")
vlf.tmean.lower.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmean.lower.allom.stack)

vlf.allom.tmean.stack <- cbind(vlf.tmean.allom.stack, vlf.tmean.upper.allom.stack[,"Upper"], vlf.tmean.lower.allom.stack[,"Lower"])
summary(vlf.allom.tmean.stack)

names(vlf.allom.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#---------------------------------------------
# Density only

vuf.tmean.dens.stack <- stack(vuf.dens.tmean)
summary(vuf.tmean.dens.stack)
names(vuf.tmean.dens.stack) <- c("corr", "month")
vuf.tmean.dens.stack$site <- as.factor("VUF")
vuf.tmean.dens.stack$type <- as.factor("tmean")
vuf.tmean.dens.stack$uncert <- as.factor("density")
summary(vuf.tmean.dens.stack)

vuf.tmean.upper.dens.stack <- stack(vuf.tmean.upper.dens)
summary(vuf.tmean.upper.dens.stack)
names(vuf.tmean.upper.dens.stack) <- c("Upper", "month")
vuf.tmean.upper.dens.stack$site <- as.factor("VUF")
vuf.tmean.upper.dens.stack$type <- as.factor("tmean")
vuf.tmean.upper.dens.stack$uncert <- as.factor("density")
summary(vuf.tmean.upper.dens.stack)

vuf.tmean.lower.dens.stack <- stack(vuf.tmean.lower.dens)
summary(vuf.tmean.lower.dens.stack)
names(vuf.tmean.lower.dens.stack) <- c("Lower", "month")
vuf.tmean.lower.dens.stack$site <- as.factor("VUF")
vuf.tmean.lower.dens.stack$type <- as.factor("tmean")
vuf.tmean.lower.dens.stack$uncert <- as.factor("density")
summary(vuf.tmean.lower.dens.stack)

vuf.dens.tmean.stack <- cbind(vuf.tmean.dens.stack, vuf.tmean.upper.dens.stack[,"Upper"], vuf.tmean.lower.dens.stack[,"Lower"])
summary(vuf.dens.tmean.stack)

names(vuf.dens.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmean.dens.stack <- stack(vlf.dens.tmean)
summary(vlf.tmean.dens.stack)
names(vlf.tmean.dens.stack) <- c("corr", "month")
vlf.tmean.dens.stack$site <- as.factor("vlf")
vlf.tmean.dens.stack$type <- as.factor("tmean")
vlf.tmean.dens.stack$uncert <- as.factor("density")
summary(vlf.tmean.dens.stack)

vlf.tmean.upper.dens.stack <- stack(vlf.tmean.upper.dens)
summary(vlf.tmean.upper.dens.stack)
names(vlf.tmean.upper.dens.stack) <- c("Upper", "month")
vlf.tmean.upper.dens.stack$site <- as.factor("vlf")
vlf.tmean.upper.dens.stack$type <- as.factor("tmean")
vlf.tmean.upper.dens.stack$uncert <- as.factor("density")
summary(vlf.tmean.upper.dens.stack)

vlf.tmean.lower.dens.stack <- stack(vlf.tmean.lower.dens)
summary(vlf.tmean.lower.dens.stack)
names(vlf.tmean.lower.dens.stack) <- c("Lower", "month")
vlf.tmean.lower.dens.stack$site <- as.factor("vlf")
vlf.tmean.lower.dens.stack$type <- as.factor("tmean")
vlf.tmean.lower.dens.stack$uncert <- as.factor("density")
summary(vlf.tmean.lower.dens.stack)

vlf.dens.tmean.stack <- cbind(vlf.tmean.dens.stack, vlf.tmean.upper.dens.stack[,"Upper"], vlf.tmean.lower.dens.stack[,"Lower"])
summary(vlf.dens.tmean.stack)

names(vlf.dens.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#------------------------------------
# Mortality only

vuf.tmean.mort.stack <- stack(vuf.mort.tmean)
summary(vuf.tmean.mort.stack)
names(vuf.tmean.mort.stack) <- c("corr", "month")
vuf.tmean.mort.stack$site <- as.factor("VUF")
vuf.tmean.mort.stack$type <- as.factor("tmean")
vuf.tmean.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmean.mort.stack)

vuf.tmean.upper.mort.stack <- stack(vuf.tmean.upper.mort)
summary(vuf.tmean.upper.mort.stack)
names(vuf.tmean.upper.mort.stack) <- c("Upper", "month")
vuf.tmean.upper.mort.stack$site <- as.factor("VUF")
vuf.tmean.upper.mort.stack$type <- as.factor("tmean")
vuf.tmean.upper.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmean.upper.mort.stack)

vuf.tmean.lower.mort.stack <- stack(vuf.tmean.lower.mort)
summary(vuf.tmean.lower.mort.stack)
names(vuf.tmean.lower.mort.stack) <- c("Lower", "month")
vuf.tmean.lower.mort.stack$site <- as.factor("VUF")
vuf.tmean.lower.mort.stack$type <- as.factor("tmean")
vuf.tmean.lower.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmean.lower.mort.stack)

vuf.mort.tmean.stack <- cbind(vuf.tmean.mort.stack, vuf.tmean.upper.mort.stack[,"Upper"], vuf.tmean.lower.mort.stack[,"Lower"])
summary(vuf.mort.tmean.stack)

names(vuf.mort.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmean.mort.stack <- stack(vlf.mort.tmean)
summary(vlf.tmean.mort.stack)
names(vlf.tmean.mort.stack) <- c("corr", "month")
vlf.tmean.mort.stack$site <- as.factor("vlf")
vlf.tmean.mort.stack$type <- as.factor("tmean")
vlf.tmean.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmean.mort.stack)

vlf.tmean.upper.mort.stack <- stack(vlf.tmean.upper.mort)
summary(vlf.tmean.upper.mort.stack)
names(vlf.tmean.upper.mort.stack) <- c("Upper", "month")
vlf.tmean.upper.mort.stack$site <- as.factor("vlf")
vlf.tmean.upper.mort.stack$type <- as.factor("tmean")
vlf.tmean.upper.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmean.upper.mort.stack)

vlf.tmean.lower.mort.stack <- stack(vlf.tmean.lower.mort)
summary(vlf.tmean.lower.mort.stack)
names(vlf.tmean.lower.mort.stack) <- c("Lower", "month")
vlf.tmean.lower.mort.stack$site <- as.factor("vlf")
vlf.tmean.lower.mort.stack$type <- as.factor("tmean")
vlf.tmean.lower.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmean.lower.mort.stack)

vlf.mort.tmean.stack <- cbind(vlf.tmean.mort.stack, vlf.tmean.upper.mort.stack[,"Upper"], vlf.tmean.lower.mort.stack[,"Lower"])
summary(vlf.mort.tmean.stack)

names(vlf.mort.tmean.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#######################################
# Tmax
#######################################

# Increment only
vuf.tmax.inc.stack <- stack(vuf.inc.tmax)
summary(vuf.tmax.inc.stack)
names(vuf.tmax.inc.stack) <- c("corr", "month")
vuf.tmax.inc.stack$site <- as.factor("VUF")
vuf.tmax.inc.stack$type <- as.factor("tmax")
vuf.tmax.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmax.inc.stack)

vuf.tmax.upper.inc.stack <- stack(vuf.tmax.upper.inc)
summary(vuf.tmax.upper.inc.stack)
names(vuf.tmax.upper.inc.stack) <- c("Upper", "month")
vuf.tmax.upper.inc.stack$site <- as.factor("VUF")
vuf.tmax.upper.inc.stack$type <- as.factor("tmax")
vuf.tmax.upper.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmax.upper.inc.stack)

vuf.tmax.lower.inc.stack <- stack(vuf.tmax.lower.inc)
summary(vuf.tmax.lower.inc.stack)
names(vuf.tmax.lower.inc.stack) <- c("Lower", "month")
vuf.tmax.lower.inc.stack$site <- as.factor("VUF")
vuf.tmax.lower.inc.stack$type <- as.factor("tmax")
vuf.tmax.lower.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmax.lower.inc.stack)

vuf.inc.tmax.stack <- cbind(vuf.tmax.inc.stack, vuf.tmax.upper.inc.stack[,"Upper"], vuf.tmax.lower.inc.stack[,"Lower"])
summary(vuf.inc.tmax.stack)

names(vuf.inc.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmax.inc.stack <- stack(vlf.inc.tmax)
summary(vlf.tmax.inc.stack)
names(vlf.tmax.inc.stack) <- c("corr", "month")
vlf.tmax.inc.stack$site <- as.factor("vlf")
vlf.tmax.inc.stack$type <- as.factor("tmax")
vlf.tmax.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmax.inc.stack)

vlf.tmax.upper.inc.stack <- stack(vlf.tmax.upper.inc)
summary(vlf.tmax.upper.inc.stack)
names(vlf.tmax.upper.inc.stack) <- c("Upper", "month")
vlf.tmax.upper.inc.stack$site <- as.factor("vlf")
vlf.tmax.upper.inc.stack$type <- as.factor("tmax")
vlf.tmax.upper.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmax.upper.inc.stack)

vlf.tmax.lower.inc.stack <- stack(vlf.tmax.lower.inc)
summary(vlf.tmax.lower.inc.stack)
names(vlf.tmax.lower.inc.stack) <- c("Lower", "month")
vlf.tmax.lower.inc.stack$site <- as.factor("vlf")
vlf.tmax.lower.inc.stack$type <- as.factor("tmax")
vlf.tmax.lower.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmax.lower.inc.stack)

vlf.inc.tmax.stack <- cbind(vlf.tmax.inc.stack, vlf.tmax.upper.inc.stack[,"Upper"], vlf.tmax.lower.inc.stack[,"Lower"])
summary(vlf.inc.tmax.stack)

names(vlf.inc.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#----------------------------------------------
# Allometry Only

vuf.tmax.allom.stack <- stack(vuf.allom.tmax)
summary(vuf.tmax.allom.stack)
names(vuf.tmax.allom.stack) <- c("corr", "month")
vuf.tmax.allom.stack$site <- as.factor("VUF")
vuf.tmax.allom.stack$type <- as.factor("tmax")
vuf.tmax.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmax.allom.stack)

vuf.tmax.upper.allom.stack <- stack(vuf.tmax.upper.allom)
summary(vuf.tmax.upper.allom.stack)
names(vuf.tmax.upper.allom.stack) <- c("Upper", "month")
vuf.tmax.upper.allom.stack$site <- as.factor("VUF")
vuf.tmax.upper.allom.stack$type <- as.factor("tmax")
vuf.tmax.upper.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmax.upper.allom.stack)

vuf.tmax.lower.allom.stack <- stack(vuf.tmax.lower.allom)
summary(vuf.tmax.lower.allom.stack)
names(vuf.tmax.lower.allom.stack) <- c("Lower", "month")
vuf.tmax.lower.allom.stack$site <- as.factor("VUF")
vuf.tmax.lower.allom.stack$type <- as.factor("tmax")
vuf.tmax.lower.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmax.lower.allom.stack)

vuf.allom.tmax.stack <- cbind(vuf.tmax.allom.stack, vuf.tmax.upper.allom.stack[,"Upper"], vuf.tmax.lower.allom.stack[,"Lower"])
summary(vuf.allom.tmax.stack)

names(vuf.allom.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmax.allom.stack <- stack(vlf.allom.tmax)
summary(vlf.tmax.allom.stack)
names(vlf.tmax.allom.stack) <- c("corr", "month")
vlf.tmax.allom.stack$site <- as.factor("vlf")
vlf.tmax.allom.stack$type <- as.factor("tmax")
vlf.tmax.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmax.allom.stack)

vlf.tmax.upper.allom.stack <- stack(vlf.tmax.upper.allom)
summary(vlf.tmax.upper.allom.stack)
names(vlf.tmax.upper.allom.stack) <- c("Upper", "month")
vlf.tmax.upper.allom.stack$site <- as.factor("vlf")
vlf.tmax.upper.allom.stack$type <- as.factor("tmax")
vlf.tmax.upper.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmax.upper.allom.stack)

vlf.tmax.lower.allom.stack <- stack(vlf.tmax.lower.allom)
summary(vlf.tmax.lower.allom.stack)
names(vlf.tmax.lower.allom.stack) <- c("Lower", "month")
vlf.tmax.lower.allom.stack$site <- as.factor("vlf")
vlf.tmax.lower.allom.stack$type <- as.factor("tmax")
vlf.tmax.lower.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmax.lower.allom.stack)

vlf.allom.tmax.stack <- cbind(vlf.tmax.allom.stack, vlf.tmax.upper.allom.stack[,"Upper"], vlf.tmax.lower.allom.stack[,"Lower"])
summary(vlf.allom.tmax.stack)

names(vlf.allom.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#---------------------------------------------
# Density only

vuf.tmax.dens.stack <- stack(vuf.dens.tmax)
summary(vuf.tmax.dens.stack)
names(vuf.tmax.dens.stack) <- c("corr", "month")
vuf.tmax.dens.stack$site <- as.factor("VUF")
vuf.tmax.dens.stack$type <- as.factor("tmax")
vuf.tmax.dens.stack$uncert <- as.factor("density")
summary(vuf.tmax.dens.stack)

vuf.tmax.upper.dens.stack <- stack(vuf.tmax.upper.dens)
summary(vuf.tmax.upper.dens.stack)
names(vuf.tmax.upper.dens.stack) <- c("Upper", "month")
vuf.tmax.upper.dens.stack$site <- as.factor("VUF")
vuf.tmax.upper.dens.stack$type <- as.factor("tmax")
vuf.tmax.upper.dens.stack$uncert <- as.factor("density")
summary(vuf.tmax.upper.dens.stack)

vuf.tmax.lower.dens.stack <- stack(vuf.tmax.lower.dens)
summary(vuf.tmax.lower.dens.stack)
names(vuf.tmax.lower.dens.stack) <- c("Lower", "month")
vuf.tmax.lower.dens.stack$site <- as.factor("VUF")
vuf.tmax.lower.dens.stack$type <- as.factor("tmax")
vuf.tmax.lower.dens.stack$uncert <- as.factor("density")
summary(vuf.tmax.lower.dens.stack)

vuf.dens.tmax.stack <- cbind(vuf.tmax.dens.stack, vuf.tmax.upper.dens.stack[,"Upper"], vuf.tmax.lower.dens.stack[,"Lower"])
summary(vuf.dens.tmax.stack)

names(vuf.dens.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmax.dens.stack <- stack(vlf.dens.tmax)
summary(vlf.tmax.dens.stack)
names(vlf.tmax.dens.stack) <- c("corr", "month")
vlf.tmax.dens.stack$site <- as.factor("vlf")
vlf.tmax.dens.stack$type <- as.factor("tmax")
vlf.tmax.dens.stack$uncert <- as.factor("density")
summary(vlf.tmax.dens.stack)

vlf.tmax.upper.dens.stack <- stack(vlf.tmax.upper.dens)
summary(vlf.tmax.upper.dens.stack)
names(vlf.tmax.upper.dens.stack) <- c("Upper", "month")
vlf.tmax.upper.dens.stack$site <- as.factor("vlf")
vlf.tmax.upper.dens.stack$type <- as.factor("tmax")
vlf.tmax.upper.dens.stack$uncert <- as.factor("density")
summary(vlf.tmax.upper.dens.stack)

vlf.tmax.lower.dens.stack <- stack(vlf.tmax.lower.dens)
summary(vlf.tmax.lower.dens.stack)
names(vlf.tmax.lower.dens.stack) <- c("Lower", "month")
vlf.tmax.lower.dens.stack$site <- as.factor("vlf")
vlf.tmax.lower.dens.stack$type <- as.factor("tmax")
vlf.tmax.lower.dens.stack$uncert <- as.factor("density")
summary(vlf.tmax.lower.dens.stack)

vlf.dens.tmax.stack <- cbind(vlf.tmax.dens.stack, vlf.tmax.upper.dens.stack[,"Upper"], vlf.tmax.lower.dens.stack[,"Lower"])
summary(vlf.dens.tmax.stack)

names(vlf.dens.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#------------------------------------
# Mortality only

vuf.tmax.mort.stack <- stack(vuf.mort.tmax)
summary(vuf.tmax.mort.stack)
names(vuf.tmax.mort.stack) <- c("corr", "month")
vuf.tmax.mort.stack$site <- as.factor("VUF")
vuf.tmax.mort.stack$type <- as.factor("tmax")
vuf.tmax.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmax.mort.stack)

vuf.tmax.upper.mort.stack <- stack(vuf.tmax.upper.mort)
summary(vuf.tmax.upper.mort.stack)
names(vuf.tmax.upper.mort.stack) <- c("Upper", "month")
vuf.tmax.upper.mort.stack$site <- as.factor("VUF")
vuf.tmax.upper.mort.stack$type <- as.factor("tmax")
vuf.tmax.upper.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmax.upper.mort.stack)

vuf.tmax.lower.mort.stack <- stack(vuf.tmax.lower.mort)
summary(vuf.tmax.lower.mort.stack)
names(vuf.tmax.lower.mort.stack) <- c("Lower", "month")
vuf.tmax.lower.mort.stack$site <- as.factor("VUF")
vuf.tmax.lower.mort.stack$type <- as.factor("tmax")
vuf.tmax.lower.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmax.lower.mort.stack)

vuf.mort.tmax.stack <- cbind(vuf.tmax.mort.stack, vuf.tmax.upper.mort.stack[,"Upper"], vuf.tmax.lower.mort.stack[,"Lower"])
summary(vuf.mort.tmax.stack)

names(vuf.mort.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmax.mort.stack <- stack(vlf.mort.tmax)
summary(vlf.tmax.mort.stack)
names(vlf.tmax.mort.stack) <- c("corr", "month")
vlf.tmax.mort.stack$site <- as.factor("vlf")
vlf.tmax.mort.stack$type <- as.factor("tmax")
vlf.tmax.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmax.mort.stack)

vlf.tmax.upper.mort.stack <- stack(vlf.tmax.upper.mort)
summary(vlf.tmax.upper.mort.stack)
names(vlf.tmax.upper.mort.stack) <- c("Upper", "month")
vlf.tmax.upper.mort.stack$site <- as.factor("vlf")
vlf.tmax.upper.mort.stack$type <- as.factor("tmax")
vlf.tmax.upper.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmax.upper.mort.stack)

vlf.tmax.lower.mort.stack <- stack(vlf.tmax.lower.mort)
summary(vlf.tmax.lower.mort.stack)
names(vlf.tmax.lower.mort.stack) <- c("Lower", "month")
vlf.tmax.lower.mort.stack$site <- as.factor("vlf")
vlf.tmax.lower.mort.stack$type <- as.factor("tmax")
vlf.tmax.lower.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmax.lower.mort.stack)

vlf.mort.tmax.stack <- cbind(vlf.tmax.mort.stack, vlf.tmax.upper.mort.stack[,"Upper"], vlf.tmax.lower.mort.stack[,"Lower"])
summary(vlf.mort.tmax.stack)

names(vlf.mort.tmax.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")



###########################
# Tmin	
##########################
# Increment only
vuf.tmin.inc.stack <- stack(vuf.inc.tmin)
summary(vuf.tmin.inc.stack)
names(vuf.tmin.inc.stack) <- c("corr", "month")
vuf.tmin.inc.stack$site <- as.factor("VUF")
vuf.tmin.inc.stack$type <- as.factor("tmin")
vuf.tmin.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmin.inc.stack)

vuf.tmin.upper.inc.stack <- stack(vuf.tmin.upper.inc)
summary(vuf.tmin.upper.inc.stack)
names(vuf.tmin.upper.inc.stack) <- c("Upper", "month")
vuf.tmin.upper.inc.stack$site <- as.factor("VUF")
vuf.tmin.upper.inc.stack$type <- as.factor("tmin")
vuf.tmin.upper.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmin.upper.inc.stack)

vuf.tmin.lower.inc.stack <- stack(vuf.tmin.lower.inc)
summary(vuf.tmin.lower.inc.stack)
names(vuf.tmin.lower.inc.stack) <- c("Lower", "month")
vuf.tmin.lower.inc.stack$site <- as.factor("VUF")
vuf.tmin.lower.inc.stack$type <- as.factor("tmin")
vuf.tmin.lower.inc.stack$uncert <- as.factor("increment")
summary(vuf.tmin.lower.inc.stack)

vuf.inc.tmin.stack <- cbind(vuf.tmin.inc.stack, vuf.tmin.upper.inc.stack[,"Upper"], vuf.tmin.lower.inc.stack[,"Lower"])
summary(vuf.inc.tmin.stack)

names(vuf.inc.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmin.inc.stack <- stack(vlf.inc.tmin)
summary(vlf.tmin.inc.stack)
names(vlf.tmin.inc.stack) <- c("corr", "month")
vlf.tmin.inc.stack$site <- as.factor("vlf")
vlf.tmin.inc.stack$type <- as.factor("tmin")
vlf.tmin.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmin.inc.stack)

vlf.tmin.upper.inc.stack <- stack(vlf.tmin.upper.inc)
summary(vlf.tmin.upper.inc.stack)
names(vlf.tmin.upper.inc.stack) <- c("Upper", "month")
vlf.tmin.upper.inc.stack$site <- as.factor("vlf")
vlf.tmin.upper.inc.stack$type <- as.factor("tmin")
vlf.tmin.upper.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmin.upper.inc.stack)

vlf.tmin.lower.inc.stack <- stack(vlf.tmin.lower.inc)
summary(vlf.tmin.lower.inc.stack)
names(vlf.tmin.lower.inc.stack) <- c("Lower", "month")
vlf.tmin.lower.inc.stack$site <- as.factor("vlf")
vlf.tmin.lower.inc.stack$type <- as.factor("tmin")
vlf.tmin.lower.inc.stack$uncert <- as.factor("increment")
summary(vlf.tmin.lower.inc.stack)

vlf.inc.tmin.stack <- cbind(vlf.tmin.inc.stack, vlf.tmin.upper.inc.stack[,"Upper"], vlf.tmin.lower.inc.stack[,"Lower"])
summary(vlf.inc.tmin.stack)

names(vlf.inc.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#----------------------------------------------
# Allometry Only

vuf.tmin.allom.stack <- stack(vuf.allom.tmin)
summary(vuf.tmin.allom.stack)
names(vuf.tmin.allom.stack) <- c("corr", "month")
vuf.tmin.allom.stack$site <- as.factor("VUF")
vuf.tmin.allom.stack$type <- as.factor("tmin")
vuf.tmin.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmin.allom.stack)

vuf.tmin.upper.allom.stack <- stack(vuf.tmin.upper.allom)
summary(vuf.tmin.upper.allom.stack)
names(vuf.tmin.upper.allom.stack) <- c("Upper", "month")
vuf.tmin.upper.allom.stack$site <- as.factor("VUF")
vuf.tmin.upper.allom.stack$type <- as.factor("tmin")
vuf.tmin.upper.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmin.upper.allom.stack)

vuf.tmin.lower.allom.stack <- stack(vuf.tmin.lower.allom)
summary(vuf.tmin.lower.allom.stack)
names(vuf.tmin.lower.allom.stack) <- c("Lower", "month")
vuf.tmin.lower.allom.stack$site <- as.factor("VUF")
vuf.tmin.lower.allom.stack$type <- as.factor("tmin")
vuf.tmin.lower.allom.stack$uncert <- as.factor("allometry")
summary(vuf.tmin.lower.allom.stack)

vuf.allom.tmin.stack <- cbind(vuf.tmin.allom.stack, vuf.tmin.upper.allom.stack[,"Upper"], vuf.tmin.lower.allom.stack[,"Lower"])
summary(vuf.allom.tmin.stack)

names(vuf.allom.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmin.allom.stack <- stack(vlf.allom.tmin)
summary(vlf.tmin.allom.stack)
names(vlf.tmin.allom.stack) <- c("corr", "month")
vlf.tmin.allom.stack$site <- as.factor("vlf")
vlf.tmin.allom.stack$type <- as.factor("tmin")
vlf.tmin.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmin.allom.stack)

vlf.tmin.upper.allom.stack <- stack(vlf.tmin.upper.allom)
summary(vlf.tmin.upper.allom.stack)
names(vlf.tmin.upper.allom.stack) <- c("Upper", "month")
vlf.tmin.upper.allom.stack$site <- as.factor("vlf")
vlf.tmin.upper.allom.stack$type <- as.factor("tmin")
vlf.tmin.upper.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmin.upper.allom.stack)

vlf.tmin.lower.allom.stack <- stack(vlf.tmin.lower.allom)
summary(vlf.tmin.lower.allom.stack)
names(vlf.tmin.lower.allom.stack) <- c("Lower", "month")
vlf.tmin.lower.allom.stack$site <- as.factor("vlf")
vlf.tmin.lower.allom.stack$type <- as.factor("tmin")
vlf.tmin.lower.allom.stack$uncert <- as.factor("allometry")
summary(vlf.tmin.lower.allom.stack)

vlf.allom.tmin.stack <- cbind(vlf.tmin.allom.stack, vlf.tmin.upper.allom.stack[,"Upper"], vlf.tmin.lower.allom.stack[,"Lower"])
summary(vlf.allom.tmin.stack)

names(vlf.allom.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#---------------------------------------------
# Density only

vuf.tmin.dens.stack <- stack(vuf.dens.tmin)
summary(vuf.tmin.dens.stack)
names(vuf.tmin.dens.stack) <- c("corr", "month")
vuf.tmin.dens.stack$site <- as.factor("VUF")
vuf.tmin.dens.stack$type <- as.factor("tmin")
vuf.tmin.dens.stack$uncert <- as.factor("density")
summary(vuf.tmin.dens.stack)

vuf.tmin.upper.dens.stack <- stack(vuf.tmin.upper.dens)
summary(vuf.tmin.upper.dens.stack)
names(vuf.tmin.upper.dens.stack) <- c("Upper", "month")
vuf.tmin.upper.dens.stack$site <- as.factor("VUF")
vuf.tmin.upper.dens.stack$type <- as.factor("tmin")
vuf.tmin.upper.dens.stack$uncert <- as.factor("density")
summary(vuf.tmin.upper.dens.stack)

vuf.tmin.lower.dens.stack <- stack(vuf.tmin.lower.dens)
summary(vuf.tmin.lower.dens.stack)
names(vuf.tmin.lower.dens.stack) <- c("Lower", "month")
vuf.tmin.lower.dens.stack$site <- as.factor("VUF")
vuf.tmin.lower.dens.stack$type <- as.factor("tmin")
vuf.tmin.lower.dens.stack$uncert <- as.factor("density")
summary(vuf.tmin.lower.dens.stack)

vuf.dens.tmin.stack <- cbind(vuf.tmin.dens.stack, vuf.tmin.upper.dens.stack[,"Upper"], vuf.tmin.lower.dens.stack[,"Lower"])
summary(vuf.dens.tmin.stack)

names(vuf.dens.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmin.dens.stack <- stack(vlf.dens.tmin)
summary(vlf.tmin.dens.stack)
names(vlf.tmin.dens.stack) <- c("corr", "month")
vlf.tmin.dens.stack$site <- as.factor("vlf")
vlf.tmin.dens.stack$type <- as.factor("tmin")
vlf.tmin.dens.stack$uncert <- as.factor("density")
summary(vlf.tmin.dens.stack)

vlf.tmin.upper.dens.stack <- stack(vlf.tmin.upper.dens)
summary(vlf.tmin.upper.dens.stack)
names(vlf.tmin.upper.dens.stack) <- c("Upper", "month")
vlf.tmin.upper.dens.stack$site <- as.factor("vlf")
vlf.tmin.upper.dens.stack$type <- as.factor("tmin")
vlf.tmin.upper.dens.stack$uncert <- as.factor("density")
summary(vlf.tmin.upper.dens.stack)

vlf.tmin.lower.dens.stack <- stack(vlf.tmin.lower.dens)
summary(vlf.tmin.lower.dens.stack)
names(vlf.tmin.lower.dens.stack) <- c("Lower", "month")
vlf.tmin.lower.dens.stack$site <- as.factor("vlf")
vlf.tmin.lower.dens.stack$type <- as.factor("tmin")
vlf.tmin.lower.dens.stack$uncert <- as.factor("density")
summary(vlf.tmin.lower.dens.stack)

vlf.dens.tmin.stack <- cbind(vlf.tmin.dens.stack, vlf.tmin.upper.dens.stack[,"Upper"], vlf.tmin.lower.dens.stack[,"Lower"])
summary(vlf.dens.tmin.stack)

names(vlf.dens.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#------------------------------------
# Mortality only

vuf.tmin.mort.stack <- stack(vuf.mort.tmin)
summary(vuf.tmin.mort.stack)
names(vuf.tmin.mort.stack) <- c("corr", "month")
vuf.tmin.mort.stack$site <- as.factor("VUF")
vuf.tmin.mort.stack$type <- as.factor("tmin")
vuf.tmin.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmin.mort.stack)

vuf.tmin.upper.mort.stack <- stack(vuf.tmin.upper.mort)
summary(vuf.tmin.upper.mort.stack)
names(vuf.tmin.upper.mort.stack) <- c("Upper", "month")
vuf.tmin.upper.mort.stack$site <- as.factor("VUF")
vuf.tmin.upper.mort.stack$type <- as.factor("tmin")
vuf.tmin.upper.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmin.upper.mort.stack)

vuf.tmin.lower.mort.stack <- stack(vuf.tmin.lower.mort)
summary(vuf.tmin.lower.mort.stack)
names(vuf.tmin.lower.mort.stack) <- c("Lower", "month")
vuf.tmin.lower.mort.stack$site <- as.factor("VUF")
vuf.tmin.lower.mort.stack$type <- as.factor("tmin")
vuf.tmin.lower.mort.stack$uncert <- as.factor("mortality")
summary(vuf.tmin.lower.mort.stack)

vuf.mort.tmin.stack <- cbind(vuf.tmin.mort.stack, vuf.tmin.upper.mort.stack[,"Upper"], vuf.tmin.lower.mort.stack[,"Lower"])
summary(vuf.mort.tmin.stack)

names(vuf.mort.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.tmin.mort.stack <- stack(vlf.mort.tmin)
summary(vlf.tmin.mort.stack)
names(vlf.tmin.mort.stack) <- c("corr", "month")
vlf.tmin.mort.stack$site <- as.factor("vlf")
vlf.tmin.mort.stack$type <- as.factor("tmin")
vlf.tmin.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmin.mort.stack)

vlf.tmin.upper.mort.stack <- stack(vlf.tmin.upper.mort)
summary(vlf.tmin.upper.mort.stack)
names(vlf.tmin.upper.mort.stack) <- c("Upper", "month")
vlf.tmin.upper.mort.stack$site <- as.factor("vlf")
vlf.tmin.upper.mort.stack$type <- as.factor("tmin")
vlf.tmin.upper.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmin.upper.mort.stack)

vlf.tmin.lower.mort.stack <- stack(vlf.tmin.lower.mort)
summary(vlf.tmin.lower.mort.stack)
names(vlf.tmin.lower.mort.stack) <- c("Lower", "month")
vlf.tmin.lower.mort.stack$site <- as.factor("vlf")
vlf.tmin.lower.mort.stack$type <- as.factor("tmin")
vlf.tmin.lower.mort.stack$uncert <- as.factor("mortality")
summary(vlf.tmin.lower.mort.stack)

vlf.mort.tmin.stack <- cbind(vlf.tmin.mort.stack, vlf.tmin.upper.mort.stack[,"Upper"], vlf.tmin.lower.mort.stack[,"Lower"])
summary(vlf.mort.tmin.stack)

names(vlf.mort.tmin.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


##################################
# Precipitation
##################################

# Increment only
vuf.precip.inc.stack <- stack(vuf.inc.precip)
summary(vuf.precip.inc.stack)
names(vuf.precip.inc.stack) <- c("corr", "month")
vuf.precip.inc.stack$site <- as.factor("VUF")
vuf.precip.inc.stack$type <- as.factor("precip")
vuf.precip.inc.stack$uncert <- as.factor("increment")
summary(vuf.precip.inc.stack)

vuf.precip.upper.inc.stack <- stack(vuf.precip.upper.inc)
summary(vuf.precip.upper.inc.stack)
names(vuf.precip.upper.inc.stack) <- c("Upper", "month")
vuf.precip.upper.inc.stack$site <- as.factor("VUF")
vuf.precip.upper.inc.stack$type <- as.factor("precip")
vuf.precip.upper.inc.stack$uncert <- as.factor("increment")
summary(vuf.precip.upper.inc.stack)

vuf.precip.lower.inc.stack <- stack(vuf.precip.lower.inc)
summary(vuf.precip.lower.inc.stack)
names(vuf.precip.lower.inc.stack) <- c("Lower", "month")
vuf.precip.lower.inc.stack$site <- as.factor("VUF")
vuf.precip.lower.inc.stack$type <- as.factor("precip")
vuf.precip.lower.inc.stack$uncert <- as.factor("increment")
summary(vuf.precip.lower.inc.stack)

vuf.inc.precip.stack <- cbind(vuf.precip.inc.stack, vuf.precip.upper.inc.stack[,"Upper"], vuf.precip.lower.inc.stack[,"Lower"])
summary(vuf.inc.precip.stack)

names(vuf.inc.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.precip.inc.stack <- stack(vlf.inc.precip)
summary(vlf.precip.inc.stack)
names(vlf.precip.inc.stack) <- c("corr", "month")
vlf.precip.inc.stack$site <- as.factor("vlf")
vlf.precip.inc.stack$type <- as.factor("precip")
vlf.precip.inc.stack$uncert <- as.factor("increment")
summary(vlf.precip.inc.stack)

vlf.precip.upper.inc.stack <- stack(vlf.precip.upper.inc)
summary(vlf.precip.upper.inc.stack)
names(vlf.precip.upper.inc.stack) <- c("Upper", "month")
vlf.precip.upper.inc.stack$site <- as.factor("vlf")
vlf.precip.upper.inc.stack$type <- as.factor("precip")
vlf.precip.upper.inc.stack$uncert <- as.factor("increment")
summary(vlf.precip.upper.inc.stack)

vlf.precip.lower.inc.stack <- stack(vlf.precip.lower.inc)
summary(vlf.precip.lower.inc.stack)
names(vlf.precip.lower.inc.stack) <- c("Lower", "month")
vlf.precip.lower.inc.stack$site <- as.factor("vlf")
vlf.precip.lower.inc.stack$type <- as.factor("precip")
vlf.precip.lower.inc.stack$uncert <- as.factor("increment")
summary(vlf.precip.lower.inc.stack)

vlf.inc.precip.stack <- cbind(vlf.precip.inc.stack, vlf.precip.upper.inc.stack[,"Upper"], vlf.precip.lower.inc.stack[,"Lower"])
summary(vlf.inc.precip.stack)

names(vlf.inc.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#----------------------------------------------
# Allometry Only

vuf.precip.allom.stack <- stack(vuf.allom.precip)
summary(vuf.precip.allom.stack)
names(vuf.precip.allom.stack) <- c("corr", "month")
vuf.precip.allom.stack$site <- as.factor("VUF")
vuf.precip.allom.stack$type <- as.factor("precip")
vuf.precip.allom.stack$uncert <- as.factor("allometry")
summary(vuf.precip.allom.stack)

vuf.precip.upper.allom.stack <- stack(vuf.precip.upper.allom)
summary(vuf.precip.upper.allom.stack)
names(vuf.precip.upper.allom.stack) <- c("Upper", "month")
vuf.precip.upper.allom.stack$site <- as.factor("VUF")
vuf.precip.upper.allom.stack$type <- as.factor("precip")
vuf.precip.upper.allom.stack$uncert <- as.factor("allometry")
summary(vuf.precip.upper.allom.stack)

vuf.precip.lower.allom.stack <- stack(vuf.precip.lower.allom)
summary(vuf.precip.lower.allom.stack)
names(vuf.precip.lower.allom.stack) <- c("Lower", "month")
vuf.precip.lower.allom.stack$site <- as.factor("VUF")
vuf.precip.lower.allom.stack$type <- as.factor("precip")
vuf.precip.lower.allom.stack$uncert <- as.factor("allometry")
summary(vuf.precip.lower.allom.stack)

vuf.allom.precip.stack <- cbind(vuf.precip.allom.stack, vuf.precip.upper.allom.stack[,"Upper"], vuf.precip.lower.allom.stack[,"Lower"])
summary(vuf.allom.precip.stack)

names(vuf.allom.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.precip.allom.stack <- stack(vlf.allom.precip)
summary(vlf.precip.allom.stack)
names(vlf.precip.allom.stack) <- c("corr", "month")
vlf.precip.allom.stack$site <- as.factor("vlf")
vlf.precip.allom.stack$type <- as.factor("precip")
vlf.precip.allom.stack$uncert <- as.factor("allometry")
summary(vlf.precip.allom.stack)

vlf.precip.upper.allom.stack <- stack(vlf.precip.upper.allom)
summary(vlf.precip.upper.allom.stack)
names(vlf.precip.upper.allom.stack) <- c("Upper", "month")
vlf.precip.upper.allom.stack$site <- as.factor("vlf")
vlf.precip.upper.allom.stack$type <- as.factor("precip")
vlf.precip.upper.allom.stack$uncert <- as.factor("allometry")
summary(vlf.precip.upper.allom.stack)

vlf.precip.lower.allom.stack <- stack(vlf.precip.lower.allom)
summary(vlf.precip.lower.allom.stack)
names(vlf.precip.lower.allom.stack) <- c("Lower", "month")
vlf.precip.lower.allom.stack$site <- as.factor("vlf")
vlf.precip.lower.allom.stack$type <- as.factor("precip")
vlf.precip.lower.allom.stack$uncert <- as.factor("allometry")
summary(vlf.precip.lower.allom.stack)

vlf.allom.precip.stack <- cbind(vlf.precip.allom.stack, vlf.precip.upper.allom.stack[,"Upper"], vlf.precip.lower.allom.stack[,"Lower"])
summary(vlf.allom.precip.stack)

names(vlf.allom.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")


#---------------------------------------------
# Density only

vuf.precip.dens.stack <- stack(vuf.dens.precip)
summary(vuf.precip.dens.stack)
names(vuf.precip.dens.stack) <- c("corr", "month")
vuf.precip.dens.stack$site <- as.factor("VUF")
vuf.precip.dens.stack$type <- as.factor("precip")
vuf.precip.dens.stack$uncert <- as.factor("density")
summary(vuf.precip.dens.stack)

vuf.precip.upper.dens.stack <- stack(vuf.precip.upper.dens)
summary(vuf.precip.upper.dens.stack)
names(vuf.precip.upper.dens.stack) <- c("Upper", "month")
vuf.precip.upper.dens.stack$site <- as.factor("VUF")
vuf.precip.upper.dens.stack$type <- as.factor("precip")
vuf.precip.upper.dens.stack$uncert <- as.factor("density")
summary(vuf.precip.upper.dens.stack)

vuf.precip.lower.dens.stack <- stack(vuf.precip.lower.dens)
summary(vuf.precip.lower.dens.stack)
names(vuf.precip.lower.dens.stack) <- c("Lower", "month")
vuf.precip.lower.dens.stack$site <- as.factor("VUF")
vuf.precip.lower.dens.stack$type <- as.factor("precip")
vuf.precip.lower.dens.stack$uncert <- as.factor("density")
summary(vuf.precip.lower.dens.stack)

vuf.dens.precip.stack <- cbind(vuf.precip.dens.stack, vuf.precip.upper.dens.stack[,"Upper"], vuf.precip.lower.dens.stack[,"Lower"])
summary(vuf.dens.precip.stack)

names(vuf.dens.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.precip.dens.stack <- stack(vlf.dens.precip)
summary(vlf.precip.dens.stack)
names(vlf.precip.dens.stack) <- c("corr", "month")
vlf.precip.dens.stack$site <- as.factor("vlf")
vlf.precip.dens.stack$type <- as.factor("precip")
vlf.precip.dens.stack$uncert <- as.factor("density")
summary(vlf.precip.dens.stack)

vlf.precip.upper.dens.stack <- stack(vlf.precip.upper.dens)
summary(vlf.precip.upper.dens.stack)
names(vlf.precip.upper.dens.stack) <- c("Upper", "month")
vlf.precip.upper.dens.stack$site <- as.factor("vlf")
vlf.precip.upper.dens.stack$type <- as.factor("precip")
vlf.precip.upper.dens.stack$uncert <- as.factor("density")
summary(vlf.precip.upper.dens.stack)

vlf.precip.lower.dens.stack <- stack(vlf.precip.lower.dens)
summary(vlf.precip.lower.dens.stack)
names(vlf.precip.lower.dens.stack) <- c("Lower", "month")
vlf.precip.lower.dens.stack$site <- as.factor("vlf")
vlf.precip.lower.dens.stack$type <- as.factor("precip")
vlf.precip.lower.dens.stack$uncert <- as.factor("density")
summary(vlf.precip.lower.dens.stack)

vlf.dens.precip.stack <- cbind(vlf.precip.dens.stack, vlf.precip.upper.dens.stack[,"Upper"], vlf.precip.lower.dens.stack[,"Lower"])
summary(vlf.dens.precip.stack)

names(vlf.dens.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#------------------------------------
# Mortality only

vuf.precip.mort.stack <- stack(vuf.mort.precip)
summary(vuf.precip.mort.stack)
names(vuf.precip.mort.stack) <- c("corr", "month")
vuf.precip.mort.stack$site <- as.factor("VUF")
vuf.precip.mort.stack$type <- as.factor("precip")
vuf.precip.mort.stack$uncert <- as.factor("mortality")
summary(vuf.precip.mort.stack)

vuf.precip.upper.mort.stack <- stack(vuf.precip.upper.mort)
summary(vuf.precip.upper.mort.stack)
names(vuf.precip.upper.mort.stack) <- c("Upper", "month")
vuf.precip.upper.mort.stack$site <- as.factor("VUF")
vuf.precip.upper.mort.stack$type <- as.factor("precip")
vuf.precip.upper.mort.stack$uncert <- as.factor("mortality")
summary(vuf.precip.upper.mort.stack)

vuf.precip.lower.mort.stack <- stack(vuf.precip.lower.mort)
summary(vuf.precip.lower.mort.stack)
names(vuf.precip.lower.mort.stack) <- c("Lower", "month")
vuf.precip.lower.mort.stack$site <- as.factor("VUF")
vuf.precip.lower.mort.stack$type <- as.factor("precip")
vuf.precip.lower.mort.stack$uncert <- as.factor("mortality")
summary(vuf.precip.lower.mort.stack)

vuf.mort.precip.stack <- cbind(vuf.precip.mort.stack, vuf.precip.upper.mort.stack[,"Upper"], vuf.precip.lower.mort.stack[,"Lower"])
summary(vuf.mort.precip.stack)

names(vuf.mort.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------
# Lower Site
vlf.precip.mort.stack <- stack(vlf.mort.precip)
summary(vlf.precip.mort.stack)
names(vlf.precip.mort.stack) <- c("corr", "month")
vlf.precip.mort.stack$site <- as.factor("vlf")
vlf.precip.mort.stack$type <- as.factor("precip")
vlf.precip.mort.stack$uncert <- as.factor("mortality")
summary(vlf.precip.mort.stack)

vlf.precip.upper.mort.stack <- stack(vlf.precip.upper.mort)
summary(vlf.precip.upper.mort.stack)
names(vlf.precip.upper.mort.stack) <- c("Upper", "month")
vlf.precip.upper.mort.stack$site <- as.factor("vlf")
vlf.precip.upper.mort.stack$type <- as.factor("precip")
vlf.precip.upper.mort.stack$uncert <- as.factor("mortality")
summary(vlf.precip.upper.mort.stack)

vlf.precip.lower.mort.stack <- stack(vlf.precip.lower.mort)
summary(vlf.precip.lower.mort.stack)
names(vlf.precip.lower.mort.stack) <- c("Lower", "month")
vlf.precip.lower.mort.stack$site <- as.factor("vlf")
vlf.precip.lower.mort.stack$type <- as.factor("precip")
vlf.precip.lower.mort.stack$uncert <- as.factor("mortality")
summary(vlf.precip.lower.mort.stack)

vlf.mort.precip.stack <- cbind(vlf.precip.mort.stack, vlf.precip.upper.mort.stack[,"Upper"], vlf.precip.lower.mort.stack[,"Lower"])
summary(vlf.mort.precip.stack)

names(vlf.mort.precip.stack)<- c("corr", "month", "site", "type", "uncert", "upper", "lower")

#--------------------------------------------

# Binding all variables from both sites together
all.valles.uncert.stack <- rbind(vuf.inc.tmean.stack, vlf.inc.tmean.stack, vuf.inc.tmax.stack, vlf.inc.tmax.stack, vuf.inc.tmin.stack, vlf.inc.tmin.stack, vuf.inc.precip.stack, vlf.inc.precip.stack,

vuf.allom.tmean.stack, vlf.allom.tmean.stack, vuf.allom.tmax.stack, vlf.allom.tmax.stack, vuf.allom.tmin.stack, vlf.allom.tmin.stack, vuf.allom.precip.stack, vlf.allom.precip.stack,

vuf.dens.tmean.stack, vlf.dens.tmean.stack, vuf.dens.tmax.stack, vlf.dens.tmax.stack, vuf.dens.tmin.stack, vlf.dens.tmin.stack, vuf.dens.precip.stack, vlf.dens.precip.stack,

vuf.mort.tmean.stack, vlf.mort.tmean.stack, vuf.mort.tmax.stack, vlf.mort.tmax.stack, vuf.mort.tmin.stack, vlf.mort.tmin.stack, vuf.mort.precip.stack, vlf.mort.precip.stack)

summary(all.valles.uncert.stack)
summary(all.valles.uncert.stack$month)





# Adding a significance column so that sig. corrs. will pop
# using the Sidak correction (1-(1-0.05)^(1/19)), where 0.05 is our desired significance value, we get a corrected value of 0.00269.  So our df=n-2 (26 years); crit value = 0.559
# Edit: Don't need to correct

# all.valles.climate.stack$sig <- ifelse(all.valles.climate.stack$corr >=0.559 | all.valles.climate.stack$corr<= -0.559, "Sidak Corrected", ifelse(all.valles.climate.stack$corr >= 0.374 & all.valles.climate.stack$corr <= 0.559| all.valles.climate.stack$corr<=-0.374 & all.valles.climate.stack$corr >= -0.559, "Uncorrected", "Not sig.")) 

all.valles.uncert.stack$sig <- ifelse(all.valles.uncert.stack$upper < 0 | all.valles.uncert.stack$lower > 0, "Y", "N")


# all.valles.uncert.stack$sig <- factor(all.valles.uncert.stack$sig, levels = c("Sidak Corrected", "Uncorrected", "Not sig."))

all.valles.uncert.stack$sig <- factor(all.valles.uncert.stack$sig, levels = c("Y", "N"))

levels(all.valles.uncert.stack$sig)

summary(all.valles.uncert.stack)

all.valles.uncert.stack$month <- factor(all.valles.uncert.stack$month, levels= names(vlf.mort.precip))


save(all.valles.uncert.stack, file="processed_data/valles_uncert_stack.rdata")


#######################################################
# Plotting individual uncertainty areas Correlations 
#######################################################
library(ggplot2)

summary(all.valles.uncert.stack)

pdf("figures/climate_uncert_type_seasons.pdf", width=13, height=8.5)
ggplot(data=all.valles.uncert.stack[all.valles.uncert.stack$month %in% c("pFall", "Winter", "Spring", "Summer"),]) + facet_grid(uncert*site ~ type, scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("green","gray50")) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_hline(yintercept=0, linetype="dashed") +
	labs(title= "Uncertainty Area : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()

pdf("figures/climate_uncert_type_all_months.pdf", width=13, height=8.5)
ggplot(data=all.valles.uncert.stack) + facet_grid(uncert*site ~ type, scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("green","gray50")) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_hline(yintercept=0, linetype="dashed") +
	labs(title= "Uncertainty Area : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()



