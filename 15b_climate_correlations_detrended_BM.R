library(car)
library(ggplot2)


########################################################################################
# Running Correlations between CRU climate data and various chronologies
########################################################################################
# Load in timeseries/chronologies
load("processed_data/valles_combined_climate_chronologies.rdata")

summary(valles.climate.cr)

# Taking just the residual chronologies
valles.res <- valles.climate.cr[,substr(names(valles.climate.cr),5,7)=="res"]
summary(valles.res)
#valles.res$cm.res <- valles.climate.cr$cm.n
valles.res$vuf.mean.res <- valles.climate.cr$vuf.mean.res
valles.res$vlf.mean.res <- valles.climate.cr$vlf.mean.res


summary(valles.res)
row.names(valles.res)
head(valles.res)

# Loading in the mean biomass time series for each site.
load("processed_data/valles_base_increment.Rdata")
summary(valles.base.inc)
valles.base.inc <- valles.base.inc[valles.base.inc$Year >=1980 & valles.base.inc$Year <=2007,]
head(valles.base.inc)

valles.base.inc <- valles.base.inc[order(valles.base.inc$Year, decreasing=F),]
head(valles.base.inc)


valles.res <- cbind(valles.res, valles.base.inc[,c("vlf.base", "vuf.base")]) 
summary(valles.res)
save(valles.res, file="processed_data/all_valles_chron_combined.Rdata")


upper.res <- valles.res[,c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res")]
row.names(upper.res)<- row.names(valles.res)
summary(upper.res)
save(upper.res, file="processed_data/upper_chron_combined.Rdata")



test <- detrend(as.data.frame(upper.res$vuf.base), method="Spline",nyrs=30)
names(test)<- "VU.BM.res"
upper.res <- cbind(upper.res,test)


lower.res <- valles.res[,c("vlf.res", "cat.res", "vlf.base", "chg.res", "vlf.mean.res", "VLB.res", "VLS.res")]
row.names(lower.res)<- row.names(valles.res)
summary(lower.res)
save(lower.res, file="processed_data/lower_chron_combined.Rdata")
# Load in climate data

test2 <- detrend(as.data.frame(lower.res$vlf.base), method="Spline", nyrs=30)
names(test2) <- "VL.BM.res"
lower.res <- cbind(lower.res, test2)

t.mean <- read.csv("climate_data/prism_met_sites_wide_tmean.csv", header=T)
t.min <- read.csv("climate_data/prism_met_sites_wide_tmin.csv", header=T)
t.max <- read.csv("climate_data/prism_met_sites_wide_tmax.csv", header=T)
precip <- read.csv("climate_data/prism_met_sites_wide_ppt.csv", header=T)
# pdsi <- read.csv("climate_data/VC_PDSI2.csv", header=T)

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


# pdsi <- pdsi[pdsi$year >= 1980 & pdsi$year <=2007,]
# summary(pdsi)
# dim(pdsi)

# pdsi$pDJF <- rowMeans(pdsi[,c("pdec", "jan", "feb")], na.rm=T)
# pdsi$MAM <- rowMeans(pdsi[,c("mar", "apr", "may")], na.rm=T)
# pdsi$JJA <- rowMeans(pdsi[,c("jun", "jul", "aug")], na.rm=T)
# pdsi$SON <- rowMeans(pdsi[,c("sep", "oct", "nov")], na.rm=T)
# summary(pdsi)


# dim(valles.climate.cr)
# dim(t.mean)
# corr.tmean <- cor(valles.climate.cr$vuf.res, t.mean, method="pearson")
# summary(corr.tmean)
########################################################################################
# Setting up for-loop to loop through the correlations of the different chronologies with different climate variables
##################################################################################################

#################################
# Mean Monthly Temperature
#################################
# Upper Site
vuf.corr.tmean <- as.data.frame(matrix(NA,nrow=ncol(upper.res), ncol=ncol(vuf.tmean)-2))
dim(vuf.corr.tmean)

names(vuf.corr.tmean)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.corr.tmean) <- names(upper.res)
summary(vuf.corr.tmean) 


for(i in 1:ncol(upper.res)){
	vuf.corr.tmean[i,] <- cor(upper.res[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.corr.tmean)

# Lower Site
vlf.corr.tmean <- as.data.frame(matrix(NA,nrow=ncol(lower.res), ncol=ncol(vlf.tmean)-2))
dim(vlf.corr.tmean)

names(vlf.corr.tmean)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.corr.tmean) <- names(lower.res)
summary(vlf.corr.tmean) 


for(i in 1:ncol(lower.res)){
	vlf.corr.tmean[i,] <- cor(lower.res[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.corr.tmean)

#################################
# Maximum Monthly Temperature
#################################
vuf.corr.tmax <- as.data.frame(matrix(NA,nrow=ncol(upper.res), ncol=ncol(vuf.tmax)-2))
dim(vuf.corr.tmax)

names(vuf.corr.tmax)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.corr.tmax) <- names(upper.res)
summary(vuf.corr.tmax) 


for(i in 1:ncol(upper.res)){
	vuf.corr.tmax[i,] <- cor(upper.res[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.corr.tmax)

# Lower Site
vlf.corr.tmax <- as.data.frame(matrix(NA,nrow=ncol(lower.res), ncol=ncol(vlf.tmax)-2))
dim(vlf.corr.tmax)

names(vlf.corr.tmax)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.corr.tmax) <- names(lower.res)
summary(vlf.corr.tmax) 


for(i in 1:ncol(lower.res)){
	vlf.corr.tmax[i,] <- cor(lower.res[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.corr.tmax)

#################################
# Minimum Monthly Temperature
#################################
vuf.corr.tmin <- as.data.frame(matrix(NA,nrow=ncol(upper.res), ncol=ncol(vuf.tmin)-2))
dim(vuf.corr.tmin)

names(vuf.corr.tmin)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.corr.tmin) <- names(upper.res)
summary(vuf.corr.tmin) 


for(i in 1:ncol(upper.res)){
	vuf.corr.tmin[i,] <- cor(upper.res[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.corr.tmin)

# Lower Site
vlf.corr.tmin <- as.data.frame(matrix(NA,nrow=ncol(lower.res), ncol=ncol(vlf.tmin)-2))
dim(vlf.corr.tmin)

names(vlf.corr.tmin)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.corr.tmin) <- names(lower.res)
summary(vlf.corr.tmin) 


for(i in 1:ncol(lower.res)){
	vlf.corr.tmin[i,] <- cor(lower.res[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.corr.tmin)

#################################
# Precipitation
#################################
vuf.corr.precip <- as.data.frame(matrix(NA,nrow=ncol(upper.res), ncol=ncol(vuf.precip)-2))
dim(vuf.corr.precip)

names(vuf.corr.precip)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.corr.precip) <- names(upper.res)
summary(vuf.corr.precip) 


for(i in 1:ncol(upper.res)){
	vuf.corr.precip[i,] <- cor(upper.res[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.corr.precip)

# Lower Site
vlf.corr.precip <- as.data.frame(matrix(NA,nrow=ncol(lower.res), ncol=ncol(vlf.precip)-2))
dim(vlf.corr.precip)

names(vlf.corr.precip)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.corr.precip) <- names(lower.res)
summary(vlf.corr.precip) 


for(i in 1:ncol(lower.res)){
	vlf.corr.precip[i,] <- cor(lower.res[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.corr.precip)

# #################################
# # PDSI
# #################################
# corr.pdsi <- as.data.frame(matrix(NA,nrow=ncol(valles.res), ncol=ncol(pdsi)-1))
# names(corr.pdsi)<- names(pdsi[,2:ncol(t.mean)]) 
# row.names(corr.pdsi) <- names(valles.res)
# summary(corr.pdsi) 

# for(i in 1:ncol(valles.res)){
	# corr.pdsi[i,] <- cor(valles.res[,i], pdsi[,2:ncol(pdsi)], method="pearson")
# }
# head(corr.pdsi)
# summary(corr.pdsi)



###################################################################################################
# Plotting the correlations as bar charts
###################################################################################################
#--------------------------------------------
# Stacking things to get them into ggplot format
# Upper Site
vuf.tmean.stack <- stack(vuf.corr.tmean)
summary(vuf.tmean.stack)
names(vuf.tmean.stack) <- c("corr", "month")
vuf.tmean.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res", "VU.BM.res"))
vuf.tmean.stack$type <- as.factor("tmean")
vuf.tmean.stack$elevation <- as.factor("Upper")

vuf.tmin.stack <- stack(vuf.corr.tmin)
summary(vuf.tmin.stack)
names(vuf.tmin.stack) <- c("corr", "month")
vuf.tmin.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res", "VU.BM.res"))
vuf.tmin.stack$type <- as.factor("tmin")
vuf.tmin.stack$elevation <- as.factor("Upper")

vuf.tmax.stack <- stack(vuf.corr.tmax)
summary(vuf.tmax.stack)
names(vuf.tmax.stack) <- c("corr", "month")
vuf.tmax.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res", "VU.BM.res"))
vuf.tmax.stack$type <- as.factor("tmax")
vuf.tmax.stack$elevation <- as.factor("Upper")

vuf.precip.stack <- stack(vuf.corr.precip)
summary(vuf.precip.stack)
names(vuf.precip.stack) <- c("corr", "month")
vuf.precip.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res", "VU.BM.res"))
vuf.precip.stack$type <- as.factor("precip")
vuf.precip.stack$elevation <- as.factor("Upper")

# Lower Site
vlf.tmean.stack <- stack(vlf.corr.tmean)
summary(vlf.tmean.stack)
names(vlf.tmean.stack) <- c("corr", "month")
vlf.tmean.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res", "VL.BM.res"))
vlf.tmean.stack$type <- as.factor("tmean")
vlf.tmean.stack$elevation <- as.factor("Lower")

vlf.tmin.stack <- stack(vlf.corr.tmin)
summary(vlf.tmin.stack)
names(vlf.tmin.stack) <- c("corr", "month")
vlf.tmin.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res", "VL.BM.res"))
vlf.tmin.stack$type <- as.factor("tmin")
vlf.tmin.stack$elevation <- as.factor("Lower")

vlf.tmax.stack <- stack(vlf.corr.tmax)
summary(vlf.tmax.stack)
names(vlf.tmax.stack) <- c("corr", "month")
vlf.tmax.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res", "VL.BM.res"))
vlf.tmax.stack$type <- as.factor("tmax")
vlf.tmax.stack$elevation <- as.factor("Lower")

vlf.precip.stack <- stack(vlf.corr.precip)
summary(vlf.precip.stack)
names(vlf.precip.stack) <- c("corr", "month")
vlf.precip.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res", "VL.BM.res"))
vlf.precip.stack$type <- as.factor("precip")
vlf.precip.stack$elevation <- as.factor("Lower")


#--------------------------------------------

# Binding all variables from both sites together
all.valles.climate.stack <- rbind(vuf.tmean.stack, vuf.tmin.stack, vuf.tmax.stack, vuf.precip.stack, vlf.tmean.stack, vlf.tmin.stack, vlf.tmax.stack, vlf.precip.stack)
summary(all.valles.climate.stack)


# Setting the factoring so that it will display the  months correctly when we Facet
all.valles.climate.stack$month <- factor(all.valles.climate.stack$month, levels = names(vuf.corr.tmean))
summary(all.valles.climate.stack$month)

# all.valles.climate.stack$chron <- factor(all.valles.climate.stack$chron, levels = c("vuf.res", "vuf.mean.res", "vuf.base", "bcw.res", "vlf.res","vlf.mean.res", "vlf.base", "cm.res", "chg.res"))

all.valles.climate.stack$chron <- recode(all.valles.climate.stack$chron, "'vuf.res' = 'Upper All'; 'vlf.res' = 'Lower All'; 'vuf.mean.res' = 'Upper Ecology Mean'; 'vlf.mean.res' = 'Lower Ecology Mean'; 'vuf.base'= 'Upper BM'; 'vlf.base' = 'Lower BM'; 'bcw.res' = 'Upper Climate'; 'cat.res' = 'Lower Climate1'; 'chg.res' = 'Lower Climate2'; 'VUB.res' = 'Upper Big'; 'VUS.res' = 'Upper Small'; 'VLB.res' = 'Lower Big'; 'VLS.res' = 'Lower Small'   
")
summary(all.valles.climate.stack)

all.valles.climate.stack$chron <- factor(all.valles.climate.stack$chron, levels = c("Upper All", "Upper Big", "Upper Small", "Upper Ecology Mean", "Upper BM","VU.BM.res", "Upper Climate", "Lower All", "Lower Big", "Lower Small","Lower Ecology Mean", "Lower BM","VL.BM.res", "Lower Climate1", "Lower Climate2"))

all.valles.climate.stack$chron.type <- ifelse(all.valles.climate.stack$chron =="Upper All", "All",
											ifelse(all.valles.climate.stack$chron== "Upper Big", "Big",
											ifelse(all.valles.climate.stack$chron== "Upper Small", "Small",
											ifelse(all.valles.climate.stack$chron== "Upper Ecology Mean", "Mean",
											ifelse(all.valles.climate.stack$chron== "Upper BM", "BM",
											ifelse(all.valles.climate.stack$chron== "VU.BM.res", "BM.detrend",
											ifelse(all.valles.climate.stack$chron== "VL.BM.res", "BM.detrend",
											ifelse(all.valles.climate.stack$chron== "Upper Climate", "Climate",
											ifelse(all.valles.climate.stack$chron =="Lower All", "All",
											ifelse(all.valles.climate.stack$chron== "Lower Big", "Big",
											ifelse(all.valles.climate.stack$chron== "Lower Small", "Small",
											ifelse(all.valles.climate.stack$chron== "Lower Ecology Mean", "Mean",
											ifelse(all.valles.climate.stack$chron== "Lower BM", "BM",
											ifelse(all.valles.climate.stack$chron== "Lower Climate1", "Climate",  												ifelse(all.valles.climate.stack$chron== "Lower Climate2", "Climate",  	
											NA))))))))))))))) 


all.valles.climate.stack$chron.type <- as.factor(all.valles.climate.stack$chron.type)
summary(all.valles.climate.stack$chron.type)
summary(all.valles.climate.stack)

all.valles.climate.stack <- all.valles.climate.stack[! all.valles.climate.stack$chron=="Lower Climate2",]
all.valles.climate.stack$chron.type <- factor(all.valles.climate.stack$chron.type, levels = c("Big", "Small", "All", "BM","BM.detrend","Climate"))

all.valles.climate.stack$elevation <- factor(all.valles.climate.stack$elevation, levels = c("Upper", "Lower"))

# Adding a significance column so that sig. corrs. will pop
# using the Sidak correction (1-(1-0.05)^(1/19)), where 0.05 is our desired significance value, we get a corrected value of 0.00269.  So our df=n-2 (26 years); crit value = 0.559
# Edit: Don't need to correct

# all.valles.climate.stack$sig <- ifelse(all.valles.climate.stack$corr >=0.559 | all.valles.climate.stack$corr<= -0.559, "Sidak Corrected", ifelse(all.valles.climate.stack$corr >= 0.374 & all.valles.climate.stack$corr <= 0.559| all.valles.climate.stack$corr<=-0.374 & all.valles.climate.stack$corr >= -0.559, "Uncorrected", "Not sig.")) 

all.valles.climate.stack$sig <- ifelse(all.valles.climate.stack$corr >= 0.374 | all.valles.climate.stack$corr<=-0.374, "Y", "N")


# all.valles.climate.stack$sig <- factor(all.valles.climate.stack$sig, levels = c("Sidak Corrected", "Uncorrected", "Not sig."))

all.valles.climate.stack$sig <- factor(all.valles.climate.stack$sig, levels = c("Y", "N"))

levels(all.valles.climate.stack$sig)

summary(all.valles.climate.stack)


save(all.valles.climate.stack, file="processed_data/valles_climate_plus_chron_stack_supp2.rdata")

# all.valles.climate.stack$elevation <- (ifelse(all.valles.climate.stack$chron=="vuf.res" | all.valles.climate.stack$chron=="bcw.res", "upper elevation", "lower elevation"))

# all.valles.climate.stack$elevation <- factor(all.valles.climate.stack$elevation, levels = c("upper elevation", "lower elevation"))

#######################################################
# Plotting Correlations with climate chronologies
#######################################################
library(ggplot2)
 summary(all.valles.climate.stack)

# Removing arithemetic mean chronology
all.valles.climate.stack.short <- all.valles.climate.stack[all.valles.climate.stack$chron %in% c("Upper All", "Upper Big", "Upper Small","Upper BM", "Upper Climate", "Lower All","Lower BM", "Lower Climate1", "Lower Big", "Lower Small", "VU.BM.res", "VL.BM.res"),]

all.valles.climate.stack.short <- all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "pFall", "Winter", "Spring", "Summer"),]
 
summary(all.valles.climate.stack.short)

test <- all.valles.climate.stack[all.valles.climate.stack$month %in% c("pDJF", "MAM", "JJA", "SON"),]
summary(test)

levels(all.valles.climate.stack$sig)

chron.col <- read.csv("chron_colors.csv", header=T)
summary(chron.col)

save(all.valles.climate.stack, file="processed_data/valles_climate_corr_data_supp3.Rdata")

# Critical Value for 28 years (n-2 = 26) 0.330

pdf("figures/climate_chron_seasons_separate.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pFall", "Winter", "Spring", "Summer"),]) + 
	facet_grid(chron*elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=sig), stat="identity", position="dodge", colour="black") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Tree Ring : Climate Correlations", x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()



pdf("figures/climate_chron_seasons_together.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pFall", "Winter", "Spring", "Summer"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=chron.type), stat="identity", position="dodge", colour="black") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "blue", "red", "darkgreen", "olivedrab","dodgerblue")) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Tree Ring : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()

# Leaving out Big and Small Chrons
pdf("figures/climate_chron_seasons_together_no_size.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pFall", "Winter", "Spring", "Summer") & !all.valles.climate.stack.short$chron.type %in% c("Big", "Small"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
    geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type, fill=chron.type, alpha=sig), stat="identity", position="dodge") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_color_manual(values= c("green", "darkgreen","olivedrab", "dodgerblue")) +
	scale_fill_manual(values= c("green", "darkgreen", "olivedrab","dodgerblue")) +
	scale_alpha_manual(values = c(1, 0.4))+
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Tree Ring : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()





pdf("figures/climate_chron_all_months.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short) + facet_grid(chron*elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=sig), stat="identity", position="dodge") +
	scale_fill_manual(values= c("green", "grey50")) + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	labs(title= "Tree Ring : Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()


pdf("figures/climate_chron_all_months_BigSmall.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$chron.type=="Big" | all.valles.climate.stack.short$chron.type=="Small" | all.valles.climate.stack.short$chron.type=="Climate",]) + facet_grid(elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=chron.type), stat="identity", position="dodge") +
	scale_fill_manual(values= c("blue", "red", "dodgerblue")) + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	labs(title= "Tree Ring : Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()

pdf("figures/climate_chron_all_months_EcolBM.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$chron.type=="Ecology" | all.valles.climate.stack.short$chron.type=="BM" | all.valles.climate.stack.short$chron.type=="Climate",]) + facet_grid(elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=chron.type), stat="identity", position="dodge") +
	scale_fill_manual(values= c("green", "darkgreen", "dodgerblue")) + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	labs(title= "Tree Ring : Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()



plot(vuf.tmax[, "Summer"] ~ vuf.tmax[,"Year"], type="l", ylim=c(0,30))
	lines(vlf.tmax$Summer ~ vuf.tmax$Year, col="red")

plot(vuf.precip[, "Summer"] ~ vuf.precip[,"Year"], type="l")
	lines(vlf.precip$Summer ~ vuf.precip$Year, col="blue")


#######################################################
# loading in BM bootstraps for correlation runs
#######################################################
vuf.inc.tot <- read.csv("processed_data/vuf_simulated_bm.csv", header=T)
summary(vuf.inc.tot)
row.names(vuf.inc.tot) <- c(2011:1904)

vuf.inc.big <- read.csv("processed_data/vuf_simulated_bm_big.csv", header=T)
summary(vuf.inc.big)
row.names(vuf.inc.big) <- c(2011:1904)

vuf.inc.small <- read.csv("processed_data/vuf_simulated_bm_small.csv", header=T)
summary(vuf.inc.small)
row.names(vuf.inc.small) <- c(2011:1904)

vuf.inc.dated <- read.csv("processed_data/vuf_simulated_bm_dated.csv", header=T)
summary(vuf.inc.dated)
row.names(vuf.inc.dated) <- c(2011:1904)



vlf.inc.tot <- read.csv("processed_data/vlf_simulated_bm.csv", header=T)
summary(vlf.bm)
row.names(vlf.inc.tot) <- c(2011:1904)

vlf.inc.big <- read.csv("processed_data/vlf_simulated_bm_big.csv", header=T)
summary(vlf.inc.big)
row.names(vlf.inc.big) <- c(2011:1904)

vlf.inc.small <- read.csv("processed_data/vlf_simulated_bm_small.csv", header=T)
summary(vlf.inc.small)
row.names(vlf.inc.small) <- c(2011:1904)

vlf.inc.dated <- read.csv("processed_data/vlf_simulated_bm_dated.csv", header=T)
summary(vlf.inc.dated)
row.names(vlf.inc.dated) <- c(2011:1904)



# subsetting to the extent of the previous timeseries
vuf.bm <- vuf.inc.tot[row.names(vuf.inc.tot)>=1980 & row.names(vuf.inc.tot)<=2007,]
vlf.bm <- vlf.inc.tot[row.names(vlf.inc.tot)>=1980 & row.names(vlf.inc.tot)<=2007,]

vuf.big <- vuf.inc.big[row.names(vuf.inc.big)>=1980 & row.names(vuf.inc.big)<=2007,]
vuf.small <- vuf.inc.small[row.names(vuf.inc.small)>=1980 & row.names(vuf.inc.small)<=2007,]
vuf.dated <- vuf.inc.dated[row.names(vuf.inc.dated)>=1980 & row.names(vuf.inc.dated)<=2007,]

vlf.big <- vlf.inc.big[row.names(vlf.inc.big)>=1980 & row.names(vlf.inc.big)<=2007,]
vlf.small <- vlf.inc.small[row.names(vlf.inc.small)>=1980 & row.names(vlf.inc.small)<=2007,]
vlf.dated <- vlf.inc.dated[row.names(vlf.inc.dated)>=1980 & row.names(vlf.inc.dated)<=2007,]

head(vuf.bm)
head(vlf.bm)

head(vuf.big)
head(vuf.small)
head(vlf.big)
head(vlf.small)

head(vuf.dated)
head(vlf.dated)

vuf.bm <- vuf.bm[order(row.names(vuf.bm), decreasing=F),]
vlf.bm <- vlf.bm[order(row.names(vlf.bm), decreasing=F),]

vuf.big <- vuf.big[order(row.names(vuf.big), decreasing=F),]
vlf.big <- vlf.big[order(row.names(vlf.big), decreasing=F),]

vuf.small <- vuf.small[order(row.names(vuf.small), decreasing=F),]
vlf.small <- vlf.small[order(row.names(vlf.small), decreasing=F),]

vuf.dated <- vuf.dated[order(row.names(vuf.dated), decreasing=F),]
vlf.dated <- vlf.dated[order(row.names(vlf.dated), decreasing=F),]




head(vuf.bm)
head(vlf.bm)

head(vuf.big)
head(vuf.small)
head(vlf.big)
head(vlf.small)

head(vuf.dated)
head(vlf.dated)
# ---------------------------------------
# Climate Correlations with BM arrays
# ---------------------------------------

#------------------------------
# Tmean
#------------------------------

## VUF
# All
vuf.corr.tmean.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(vuf.tmean)-2))
dim(vuf.corr.tmean.bm)

names(vuf.corr.tmean.bm)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.corr.tmean.bm) <- names(vuf.bm)
summary(vuf.corr.tmean.bm) 

dim(vuf.tmean)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmean.bm[i,] <- cor(vuf.bm[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.corr.tmean.bm)

vuf.tmean.upper.corr <- vuf.corr.tmean.bm
vuf.tmean.lower.corr <- vuf.corr.tmean.bm
# vuf.tmean.mean.corr <- vuf.corr.tmean.bm
for(j in 1:ncol(vuf.corr.tmean.bm)){
	vuf.tmean.upper.corr[,j] <- quantile(vuf.corr.tmean.bm[,j],0.975, na.rm=T)
	vuf.tmean.lower.corr[,j] <- quantile(vuf.corr.tmean.bm[,j],0.025, na.rm=T)
	# vuf.tmean.mean.corr[,j] <- apply(vuf.corr.tmean.bm[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmean.upper.corr)
head(vuf.tmean.lower.corr)


# Big

vuf.corr.tmean.big <- as.data.frame(matrix(NA,nrow=ncol(vuf.big), ncol=ncol(vuf.tmean)-2))
dim(vuf.corr.tmean.big)

names(vuf.corr.tmean.big)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.corr.tmean.big) <- names(vuf.big)
summary(vuf.corr.tmean.big) 

dim(vuf.tmean)
dim(vuf.big)

for(i in 1:ncol(vuf.big)){
	vuf.corr.tmean.big[i,] <- cor(vuf.big[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.corr.tmean.big)

vuf.tmean.upper.corr.big <- vuf.corr.tmean.big
vuf.tmean.lower.corr.big <- vuf.corr.tmean.big
# vuf.tmean.mean.corr <- vuf.corr.tmean.big
for(j in 1:ncol(vuf.corr.tmean.big)){
	vuf.tmean.upper.corr.big[,j] <- quantile(vuf.corr.tmean.big[,j],0.975, na.rm=T)
	vuf.tmean.lower.corr.big[,j] <- quantile(vuf.corr.tmean.big[,j],0.025, na.rm=T)
	# vuf.tmean.mean.corr.big[,j] <- apply(vuf.corr.tmean.big[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmean.upper.corr.big)
head(vuf.tmean.lower.corr.big)


# Small

vuf.corr.tmean.small <- as.data.frame(matrix(NA,nrow=ncol(vuf.small), ncol=ncol(vuf.tmean)-2))
dim(vuf.corr.tmean.small)

names(vuf.corr.tmean.small)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.corr.tmean.small) <- names(vuf.small)
summary(vuf.corr.tmean.small) 

dim(vuf.tmean)
dim(vuf.small)

for(i in 1:ncol(vuf.small)){
	vuf.corr.tmean.small[i,] <- cor(vuf.small[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.corr.tmean.small)

vuf.tmean.upper.corr.small <- vuf.corr.tmean.small
vuf.tmean.lower.corr.small <- vuf.corr.tmean.small
# vuf.tmean.mean.corr <- vuf.corr.tmean.small
for(j in 1:ncol(vuf.corr.tmean.small)){
	vuf.tmean.upper.corr.small[,j] <- quantile(vuf.corr.tmean.small[,j],0.975, na.rm=T)
	vuf.tmean.lower.corr.small[,j] <- quantile(vuf.corr.tmean.small[,j],0.025, na.rm=T)
	# vuf.tmean.mean.corr.small[,j] <- apply(vuf.corr.tmean.small[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmean.upper.corr.small)
head(vuf.tmean.lower.corr.small)

# Dated

vuf.corr.tmean.dated <- as.data.frame(matrix(NA,nrow=ncol(vuf.dated), ncol=ncol(vuf.tmean)-2))
dim(vuf.corr.tmean.dated)

names(vuf.corr.tmean.dated)<- names(vuf.tmean[,3:ncol(vuf.tmean)]) 
row.names(vuf.corr.tmean.dated) <- names(vuf.dated)
summary(vuf.corr.tmean.dated) 

dim(vuf.tmean)
dim(vuf.dated)

for(i in 1:ncol(vuf.dated)){
	vuf.corr.tmean.dated[i,] <- cor(vuf.dated[,i], vuf.tmean[,3:ncol(vuf.tmean)], method="pearson")
}
head(vuf.corr.tmean.dated)

vuf.tmean.upper.corr.dated <- vuf.corr.tmean.dated
vuf.tmean.lower.corr.dated <- vuf.corr.tmean.dated
# vuf.tmean.mean.corr <- vuf.corr.tmean.dated
for(j in 1:ncol(vuf.corr.tmean.dated)){
	vuf.tmean.upper.corr.dated[,j] <- quantile(vuf.corr.tmean.dated[,j],0.975, na.rm=T)
	vuf.tmean.lower.corr.dated[,j] <- quantile(vuf.corr.tmean.dated[,j],0.025, na.rm=T)
	# vuf.tmean.mean.corr.dated[,j] <- apply(vuf.corr.tmean.dated[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmean.upper.corr.dated)
head(vuf.tmean.lower.corr.dated)



## VLF
# All Trees
vlf.corr.tmean.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(vlf.tmean)-2))
dim(vlf.corr.tmean.bm)

names(vlf.corr.tmean.bm)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.corr.tmean.bm) <- names(vlf.bm)
summary(vlf.corr.tmean.bm) 

dim(vlf.tmean)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmean.bm[i,] <- cor(vlf.bm[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.corr.tmean.bm)

vlf.tmean.upper.corr <- vlf.corr.tmean.bm
vlf.tmean.lower.corr <- vlf.corr.tmean.bm
for(j in 1:ncol(vlf.corr.tmean.bm)){
	vlf.tmean.upper.corr[,j] <- quantile(vlf.corr.tmean.bm[,j],0.975, na.rm=T)
	vlf.tmean.lower.corr[,j] <- quantile(vlf.corr.tmean.bm[,j],0.025, na.rm=T)
}
head(vlf.tmean.upper.corr)
head(vlf.tmean.lower.corr)



# Big

vlf.corr.tmean.big <- as.data.frame(matrix(NA,nrow=ncol(vlf.big), ncol=ncol(vlf.tmean)-2))
dim(vlf.corr.tmean.big)

names(vlf.corr.tmean.big)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.corr.tmean.big) <- names(vlf.big)
summary(vlf.corr.tmean.big) 

dim(vlf.tmean)
dim(vlf.big)

for(i in 1:ncol(vlf.big)){
	vlf.corr.tmean.big[i,] <- cor(vlf.big[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.corr.tmean.big)

vlf.tmean.upper.corr.big <- vlf.corr.tmean.big
vlf.tmean.lower.corr.big <- vlf.corr.tmean.big
# vlf.tmean.mean.corr <- vlf.corr.tmean.big
for(j in 1:ncol(vlf.corr.tmean.big)){
	vlf.tmean.upper.corr.big[,j] <- quantile(vlf.corr.tmean.big[,j],0.975, na.rm=T)
	vlf.tmean.lower.corr.big[,j] <- quantile(vlf.corr.tmean.big[,j],0.025, na.rm=T)
	# vlf.tmean.mean.corr.big[,j] <- apply(vlf.corr.tmean.big[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmean.upper.corr.big)
head(vlf.tmean.lower.corr.big)


# Small

vlf.corr.tmean.small <- as.data.frame(matrix(NA,nrow=ncol(vlf.small), ncol=ncol(vlf.tmean)-2))
dim(vlf.corr.tmean.small)

names(vlf.corr.tmean.small)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.corr.tmean.small) <- names(vlf.small)
summary(vlf.corr.tmean.small) 

dim(vlf.tmean)
dim(vlf.small)

for(i in 1:ncol(vlf.small)){
	vlf.corr.tmean.small[i,] <- cor(vlf.small[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.corr.tmean.small)

vlf.tmean.upper.corr.small <- vlf.corr.tmean.small
vlf.tmean.lower.corr.small <- vlf.corr.tmean.small
# vlf.tmean.mean.corr <- vlf.corr.tmean.small
for(j in 1:ncol(vlf.corr.tmean.small)){
	vlf.tmean.upper.corr.small[,j] <- quantile(vlf.corr.tmean.small[,j],0.975, na.rm=T)
	vlf.tmean.lower.corr.small[,j] <- quantile(vlf.corr.tmean.small[,j],0.025, na.rm=T)
	# vlf.tmean.mean.corr.small[,j] <- apply(vlf.corr.tmean.small[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmean.upper.corr.small)
head(vlf.tmean.lower.corr.small)

# Dated
vlf.corr.tmean.dated <- as.data.frame(matrix(NA,nrow=ncol(vlf.dated), ncol=ncol(vlf.tmean)-2))
dim(vlf.corr.tmean.dated)

names(vlf.corr.tmean.dated)<- names(vlf.tmean[,3:ncol(vlf.tmean)]) 
row.names(vlf.corr.tmean.dated) <- names(vlf.dated)
summary(vlf.corr.tmean.dated) 

dim(vlf.tmean)
dim(vlf.dated)

for(i in 1:ncol(vlf.dated)){
	vlf.corr.tmean.dated[i,] <- cor(vlf.dated[,i], vlf.tmean[,3:ncol(vlf.tmean)], method="pearson")
}
head(vlf.corr.tmean.dated)

vlf.tmean.upper.corr.dated <- vlf.corr.tmean.dated
vlf.tmean.lower.corr.dated <- vlf.corr.tmean.dated
# vlf.tmean.mean.corr <- vlf.corr.tmean.dated
for(j in 1:ncol(vlf.corr.tmean.dated)){
	vlf.tmean.upper.corr.dated[,j] <- quantile(vlf.corr.tmean.dated[,j],0.975, na.rm=T)
	vlf.tmean.lower.corr.dated[,j] <- quantile(vlf.corr.tmean.dated[,j],0.025, na.rm=T)
	# vlf.tmean.mean.corr.dated[,j] <- apply(vlf.corr.tmean.dated[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmean.upper.corr.dated)
head(vlf.tmean.lower.corr.dated)







#------------------------------
# Precipitation
#------------------------------
## VUF
# All Trees
vuf.corr.precip.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(vuf.precip)-2))
dim(vuf.corr.precip.bm)

names(vuf.corr.precip.bm)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.corr.precip.bm) <- names(vuf.bm)
summary(vuf.corr.precip.bm) 

dim(vuf.precip)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.precip.bm[i,] <- cor(vuf.bm[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.corr.precip.bm)

vuf.precip.upper.corr <- vuf.corr.precip.bm
vuf.precip.lower.corr <- vuf.corr.precip.bm
# vuf.precip.mean.corr <- vuf.corr.precip.bm
for(j in 1:ncol(vuf.corr.precip.bm)){
	vuf.precip.upper.corr[,j] <- quantile(vuf.corr.precip.bm[,j],0.975, na.rm=T)
	vuf.precip.lower.corr[,j] <- quantile(vuf.corr.precip.bm[,j],0.025, na.rm=T)
	# vuf.precip.mean.corr[,j] <- apply(vuf.corr.precip.bm[,j], FUN=mean, na.rm=T)
	
}
head(vuf.precip.upper.corr)
head(vuf.precip.lower.corr)


# Big

vuf.corr.precip.big <- as.data.frame(matrix(NA,nrow=ncol(vuf.big), ncol=ncol(vuf.precip)-2))
dim(vuf.corr.precip.big)

names(vuf.corr.precip.big)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.corr.precip.big) <- names(vuf.big)
summary(vuf.corr.precip.big) 

dim(vuf.precip)
dim(vuf.big)

for(i in 1:ncol(vuf.big)){
	vuf.corr.precip.big[i,] <- cor(vuf.big[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.corr.precip.big)

vuf.precip.upper.corr.big <- vuf.corr.precip.big
vuf.precip.lower.corr.big <- vuf.corr.precip.big
# vuf.precip.mean.corr <- vuf.corr.precip.big
for(j in 1:ncol(vuf.corr.precip.big)){
	vuf.precip.upper.corr.big[,j] <- quantile(vuf.corr.precip.big[,j],0.975, na.rm=T)
	vuf.precip.lower.corr.big[,j] <- quantile(vuf.corr.precip.big[,j],0.025, na.rm=T)
	# vuf.precip.mean.corr.big[,j] <- apply(vuf.corr.precip.big[,j], FUN=mean, na.rm=T)
	
}
head(vuf.precip.upper.corr.big)
head(vuf.precip.lower.corr.big)


# Small

vuf.corr.precip.small <- as.data.frame(matrix(NA,nrow=ncol(vuf.small), ncol=ncol(vuf.precip)-2))
dim(vuf.corr.precip.small)

names(vuf.corr.precip.small)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.corr.precip.small) <- names(vuf.small)
summary(vuf.corr.precip.small) 

dim(vuf.precip)
dim(vuf.small)

for(i in 1:ncol(vuf.small)){
	vuf.corr.precip.small[i,] <- cor(vuf.small[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.corr.precip.small)

vuf.precip.upper.corr.small <- vuf.corr.precip.small
vuf.precip.lower.corr.small <- vuf.corr.precip.small
# vuf.precip.mean.corr <- vuf.corr.precip.small
for(j in 1:ncol(vuf.corr.precip.small)){
	vuf.precip.upper.corr.small[,j] <- quantile(vuf.corr.precip.small[,j],0.975, na.rm=T)
	vuf.precip.lower.corr.small[,j] <- quantile(vuf.corr.precip.small[,j],0.025, na.rm=T)
	# vuf.precip.mean.corr.small[,j] <- apply(vuf.corr.precip.small[,j], FUN=mean, na.rm=T)
	
}
head(vuf.precip.upper.corr.small)
head(vuf.precip.lower.corr.small)

# Dated

vuf.corr.precip.dated <- as.data.frame(matrix(NA,nrow=ncol(vuf.dated), ncol=ncol(vuf.precip)-2))
dim(vuf.corr.precip.dated)

names(vuf.corr.precip.dated)<- names(vuf.precip[,3:ncol(vuf.precip)]) 
row.names(vuf.corr.precip.dated) <- names(vuf.dated)
summary(vuf.corr.precip.dated) 

dim(vuf.precip)
dim(vuf.dated)

for(i in 1:ncol(vuf.dated)){
	vuf.corr.precip.dated[i,] <- cor(vuf.dated[,i], vuf.precip[,3:ncol(vuf.precip)], method="pearson")
}
head(vuf.corr.precip.dated)

vuf.precip.upper.corr.dated <- vuf.corr.precip.dated
vuf.precip.lower.corr.dated <- vuf.corr.precip.dated
# vuf.precip.mean.corr <- vuf.corr.precip.dated
for(j in 1:ncol(vuf.corr.precip.dated)){
	vuf.precip.upper.corr.dated[,j] <- quantile(vuf.corr.precip.dated[,j],0.975, na.rm=T)
	vuf.precip.lower.corr.dated[,j] <- quantile(vuf.corr.precip.dated[,j],0.025, na.rm=T)
	# vuf.precip.mean.corr.dated[,j] <- apply(vuf.corr.precip.dated[,j], FUN=mean, na.rm=T)
	
}
head(vuf.precip.upper.corr.dated)
head(vuf.precip.lower.corr.dated)



## VLF
# All Trees
vlf.corr.precip.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(vlf.precip)-2))
dim(vlf.corr.precip.bm)

names(vlf.corr.precip.bm)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.corr.precip.bm) <- names(vlf.bm)
summary(vlf.corr.precip.bm) 

dim(vlf.precip)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.precip.bm[i,] <- cor(vlf.bm[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.corr.precip.bm)

vlf.precip.upper.corr <- vlf.corr.precip.bm
vlf.precip.lower.corr <- vlf.corr.precip.bm
for(j in 1:ncol(vlf.corr.precip.bm)){
	vlf.precip.upper.corr[,j] <- quantile(vlf.corr.precip.bm[,j],0.975, na.rm=T)
	vlf.precip.lower.corr[,j] <- quantile(vlf.corr.precip.bm[,j],0.025, na.rm=T)
}
head(vlf.precip.upper.corr)
head(vlf.precip.lower.corr)



# Big

vlf.corr.precip.big <- as.data.frame(matrix(NA,nrow=ncol(vlf.big), ncol=ncol(vlf.precip)-2))
dim(vlf.corr.precip.big)

names(vlf.corr.precip.big)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.corr.precip.big) <- names(vlf.big)
summary(vlf.corr.precip.big) 

dim(vlf.precip)
dim(vlf.big)

for(i in 1:ncol(vlf.big)){
	vlf.corr.precip.big[i,] <- cor(vlf.big[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.corr.precip.big)

vlf.precip.upper.corr.big <- vlf.corr.precip.big
vlf.precip.lower.corr.big <- vlf.corr.precip.big
# vlf.precip.mean.corr <- vlf.corr.precip.big
for(j in 1:ncol(vlf.corr.precip.big)){
	vlf.precip.upper.corr.big[,j] <- quantile(vlf.corr.precip.big[,j],0.975, na.rm=T)
	vlf.precip.lower.corr.big[,j] <- quantile(vlf.corr.precip.big[,j],0.025, na.rm=T)
	# vlf.precip.mean.corr.big[,j] <- apply(vlf.corr.precip.big[,j], FUN=mean, na.rm=T)
	
}
head(vlf.precip.upper.corr.big)
head(vlf.precip.lower.corr.big)


# Small

vlf.corr.precip.small <- as.data.frame(matrix(NA,nrow=ncol(vlf.small), ncol=ncol(vlf.precip)-2))
dim(vlf.corr.precip.small)

names(vlf.corr.precip.small)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.corr.precip.small) <- names(vlf.small)
summary(vlf.corr.precip.small) 

dim(vlf.precip)
dim(vlf.small)

for(i in 1:ncol(vlf.small)){
	vlf.corr.precip.small[i,] <- cor(vlf.small[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.corr.precip.small)

vlf.precip.upper.corr.small <- vlf.corr.precip.small
vlf.precip.lower.corr.small <- vlf.corr.precip.small
# vlf.precip.mean.corr <- vlf.corr.precip.small
for(j in 1:ncol(vlf.corr.precip.small)){
	vlf.precip.upper.corr.small[,j] <- quantile(vlf.corr.precip.small[,j],0.975, na.rm=T)
	vlf.precip.lower.corr.small[,j] <- quantile(vlf.corr.precip.small[,j],0.025, na.rm=T)
	# vlf.precip.mean.corr.small[,j] <- apply(vlf.corr.precip.small[,j], FUN=mean, na.rm=T)
	
}
head(vlf.precip.upper.corr.small)
head(vlf.precip.lower.corr.small)


# Dated
vlf.corr.precip.dated <- as.data.frame(matrix(NA,nrow=ncol(vlf.dated), ncol=ncol(vlf.precip)-2))
dim(vlf.corr.precip.dated)

names(vlf.corr.precip.dated)<- names(vlf.precip[,3:ncol(vlf.precip)]) 
row.names(vlf.corr.precip.dated) <- names(vlf.dated)
summary(vlf.corr.precip.dated) 

dim(vlf.precip)
dim(vlf.dated)

for(i in 1:ncol(vlf.dated)){
	vlf.corr.precip.dated[i,] <- cor(vlf.dated[,i], vlf.precip[,3:ncol(vlf.precip)], method="pearson")
}
head(vlf.corr.precip.dated)

vlf.precip.upper.corr.dated <- vlf.corr.precip.dated
vlf.precip.lower.corr.dated <- vlf.corr.precip.dated
# vlf.precip.mean.corr <- vlf.corr.precip.dated
for(j in 1:ncol(vlf.corr.precip.dated)){
	vlf.precip.upper.corr.dated[,j] <- quantile(vlf.corr.precip.dated[,j],0.975, na.rm=T)
	vlf.precip.lower.corr.dated[,j] <- quantile(vlf.corr.precip.dated[,j],0.025, na.rm=T)
	# vlf.precip.mean.corr.dated[,j] <- apply(vlf.corr.precip.dated[,j], FUN=mean, na.rm=T)
	
}
head(vlf.precip.upper.corr.dated)
head(vlf.precip.lower.corr.dated)


#------------------------------
# T min
#------------------------------

## VUF
# All
vuf.corr.tmin.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(vuf.tmin)-2))
dim(vuf.corr.tmin.bm)

names(vuf.corr.tmin.bm)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.corr.tmin.bm) <- names(vuf.bm)
summary(vuf.corr.tmin.bm) 

dim(vuf.tmin)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmin.bm[i,] <- cor(vuf.bm[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.corr.tmin.bm)

vuf.tmin.upper.corr <- vuf.corr.tmin.bm
vuf.tmin.lower.corr <- vuf.corr.tmin.bm
# vuf.tmin.mean.corr <- vuf.corr.tmin.bm
for(j in 1:ncol(vuf.corr.tmin.bm)){
	vuf.tmin.upper.corr[,j] <- quantile(vuf.corr.tmin.bm[,j],0.975, na.rm=T)
	vuf.tmin.lower.corr[,j] <- quantile(vuf.corr.tmin.bm[,j],0.025, na.rm=T)
	# vuf.tmin.mean.corr[,j] <- apply(vuf.corr.tmin.bm[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmin.upper.corr)
head(vuf.tmin.lower.corr)


# Big

vuf.corr.tmin.big <- as.data.frame(matrix(NA,nrow=ncol(vuf.big), ncol=ncol(vuf.tmin)-2))
dim(vuf.corr.tmin.big)

names(vuf.corr.tmin.big)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.corr.tmin.big) <- names(vuf.big)
summary(vuf.corr.tmin.big) 

dim(vuf.tmin)
dim(vuf.big)

for(i in 1:ncol(vuf.big)){
	vuf.corr.tmin.big[i,] <- cor(vuf.big[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.corr.tmin.big)

vuf.tmin.upper.corr.big <- vuf.corr.tmin.big
vuf.tmin.lower.corr.big <- vuf.corr.tmin.big
# vuf.tmin.mean.corr <- vuf.corr.tmin.big
for(j in 1:ncol(vuf.corr.tmin.big)){
	vuf.tmin.upper.corr.big[,j] <- quantile(vuf.corr.tmin.big[,j],0.975, na.rm=T)
	vuf.tmin.lower.corr.big[,j] <- quantile(vuf.corr.tmin.big[,j],0.025, na.rm=T)
	# vuf.tmin.mean.corr.big[,j] <- apply(vuf.corr.tmin.big[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmin.upper.corr.big)
head(vuf.tmin.lower.corr.big)


# Small

vuf.corr.tmin.small <- as.data.frame(matrix(NA,nrow=ncol(vuf.small), ncol=ncol(vuf.tmin)-2))
dim(vuf.corr.tmin.small)

names(vuf.corr.tmin.small)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.corr.tmin.small) <- names(vuf.small)
summary(vuf.corr.tmin.small) 

dim(vuf.tmin)
dim(vuf.small)

for(i in 1:ncol(vuf.small)){
	vuf.corr.tmin.small[i,] <- cor(vuf.small[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.corr.tmin.small)

vuf.tmin.upper.corr.small <- vuf.corr.tmin.small
vuf.tmin.lower.corr.small <- vuf.corr.tmin.small
# vuf.tmin.mean.corr <- vuf.corr.tmin.small
for(j in 1:ncol(vuf.corr.tmin.small)){
	vuf.tmin.upper.corr.small[,j] <- quantile(vuf.corr.tmin.small[,j],0.975, na.rm=T)
	vuf.tmin.lower.corr.small[,j] <- quantile(vuf.corr.tmin.small[,j],0.025, na.rm=T)
	# vuf.tmin.mean.corr.small[,j] <- apply(vuf.corr.tmin.small[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmin.upper.corr.small)
head(vuf.tmin.lower.corr.small)

# Dated

vuf.corr.tmin.dated <- as.data.frame(matrix(NA,nrow=ncol(vuf.dated), ncol=ncol(vuf.tmin)-2))
dim(vuf.corr.tmin.dated)

names(vuf.corr.tmin.dated)<- names(vuf.tmin[,3:ncol(vuf.tmin)]) 
row.names(vuf.corr.tmin.dated) <- names(vuf.dated)
summary(vuf.corr.tmin.dated) 

dim(vuf.tmin)
dim(vuf.dated)

for(i in 1:ncol(vuf.dated)){
	vuf.corr.tmin.dated[i,] <- cor(vuf.dated[,i], vuf.tmin[,3:ncol(vuf.tmin)], method="pearson")
}
head(vuf.corr.tmin.dated)

vuf.tmin.upper.corr.dated <- vuf.corr.tmin.dated
vuf.tmin.lower.corr.dated <- vuf.corr.tmin.dated
# vuf.tmin.mean.corr <- vuf.corr.tmin.dated
for(j in 1:ncol(vuf.corr.tmin.dated)){
	vuf.tmin.upper.corr.dated[,j] <- quantile(vuf.corr.tmin.dated[,j],0.975, na.rm=T)
	vuf.tmin.lower.corr.dated[,j] <- quantile(vuf.corr.tmin.dated[,j],0.025, na.rm=T)
	# vuf.tmin.mean.corr.dated[,j] <- apply(vuf.corr.tmin.dated[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmin.upper.corr.dated)
head(vuf.tmin.lower.corr.dated)



## VLF
# All Trees
vlf.corr.tmin.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(vlf.tmin)-2))
dim(vlf.corr.tmin.bm)

names(vlf.corr.tmin.bm)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.corr.tmin.bm) <- names(vlf.bm)
summary(vlf.corr.tmin.bm) 

dim(vlf.tmin)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmin.bm[i,] <- cor(vlf.bm[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.corr.tmin.bm)

vlf.tmin.upper.corr <- vlf.corr.tmin.bm
vlf.tmin.lower.corr <- vlf.corr.tmin.bm
for(j in 1:ncol(vlf.corr.tmin.bm)){
	vlf.tmin.upper.corr[,j] <- quantile(vlf.corr.tmin.bm[,j],0.975, na.rm=T)
	vlf.tmin.lower.corr[,j] <- quantile(vlf.corr.tmin.bm[,j],0.025, na.rm=T)
}
head(vlf.tmin.upper.corr)
head(vlf.tmin.lower.corr)



# Big

vlf.corr.tmin.big <- as.data.frame(matrix(NA,nrow=ncol(vlf.big), ncol=ncol(vlf.tmin)-2))
dim(vlf.corr.tmin.big)

names(vlf.corr.tmin.big)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.corr.tmin.big) <- names(vlf.big)
summary(vlf.corr.tmin.big) 

dim(vlf.tmin)
dim(vlf.big)

for(i in 1:ncol(vlf.big)){
	vlf.corr.tmin.big[i,] <- cor(vlf.big[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.corr.tmin.big)

vlf.tmin.upper.corr.big <- vlf.corr.tmin.big
vlf.tmin.lower.corr.big <- vlf.corr.tmin.big
# vlf.tmin.mean.corr <- vlf.corr.tmin.big
for(j in 1:ncol(vlf.corr.tmin.big)){
	vlf.tmin.upper.corr.big[,j] <- quantile(vlf.corr.tmin.big[,j],0.975, na.rm=T)
	vlf.tmin.lower.corr.big[,j] <- quantile(vlf.corr.tmin.big[,j],0.025, na.rm=T)
	# vlf.tmin.mean.corr.big[,j] <- apply(vlf.corr.tmin.big[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmin.upper.corr.big)
head(vlf.tmin.lower.corr.big)


# Small

vlf.corr.tmin.small <- as.data.frame(matrix(NA,nrow=ncol(vlf.small), ncol=ncol(vlf.tmin)-2))
dim(vlf.corr.tmin.small)

names(vlf.corr.tmin.small)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.corr.tmin.small) <- names(vlf.small)
summary(vlf.corr.tmin.small) 

dim(vlf.tmin)
dim(vlf.small)

for(i in 1:ncol(vlf.small)){
	vlf.corr.tmin.small[i,] <- cor(vlf.small[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.corr.tmin.small)

vlf.tmin.upper.corr.small <- vlf.corr.tmin.small
vlf.tmin.lower.corr.small <- vlf.corr.tmin.small
# vlf.tmin.mean.corr <- vlf.corr.tmin.small
for(j in 1:ncol(vlf.corr.tmin.small)){
	vlf.tmin.upper.corr.small[,j] <- quantile(vlf.corr.tmin.small[,j],0.975, na.rm=T)
	vlf.tmin.lower.corr.small[,j] <- quantile(vlf.corr.tmin.small[,j],0.025, na.rm=T)
	# vlf.tmin.mean.corr.small[,j] <- apply(vlf.corr.tmin.small[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmin.upper.corr.small)
head(vlf.tmin.lower.corr.small)

# Dated
vlf.corr.tmin.dated <- as.data.frame(matrix(NA,nrow=ncol(vlf.dated), ncol=ncol(vlf.tmin)-2))
dim(vlf.corr.tmin.dated)

names(vlf.corr.tmin.dated)<- names(vlf.tmin[,3:ncol(vlf.tmin)]) 
row.names(vlf.corr.tmin.dated) <- names(vlf.dated)
summary(vlf.corr.tmin.dated) 

dim(vlf.tmin)
dim(vlf.dated)

for(i in 1:ncol(vlf.dated)){
	vlf.corr.tmin.dated[i,] <- cor(vlf.dated[,i], vlf.tmin[,3:ncol(vlf.tmin)], method="pearson")
}
head(vlf.corr.tmin.dated)

vlf.tmin.upper.corr.dated <- vlf.corr.tmin.dated
vlf.tmin.lower.corr.dated <- vlf.corr.tmin.dated
# vlf.tmin.mean.corr <- vlf.corr.tmin.dated
for(j in 1:ncol(vlf.corr.tmin.dated)){
	vlf.tmin.upper.corr.dated[,j] <- quantile(vlf.corr.tmin.dated[,j],0.975, na.rm=T)
	vlf.tmin.lower.corr.dated[,j] <- quantile(vlf.corr.tmin.dated[,j],0.025, na.rm=T)
	# vlf.tmin.mean.corr.dated[,j] <- apply(vlf.corr.tmin.dated[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmin.upper.corr.dated)
head(vlf.tmin.lower.corr.dated)



#------------------------------
# Tmax
#------------------------------

## VUF
# All
vuf.corr.tmax.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(vuf.tmax)-2))
dim(vuf.corr.tmax.bm)

names(vuf.corr.tmax.bm)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.corr.tmax.bm) <- names(vuf.bm)
summary(vuf.corr.tmax.bm) 

dim(vuf.tmax)
dim(vuf.bm)

for(i in 1:ncol(vuf.bm)){
	vuf.corr.tmax.bm[i,] <- cor(vuf.bm[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.corr.tmax.bm)

vuf.tmax.upper.corr <- vuf.corr.tmax.bm
vuf.tmax.lower.corr <- vuf.corr.tmax.bm
# vuf.tmax.mean.corr <- vuf.corr.tmax.bm
for(j in 1:ncol(vuf.corr.tmax.bm)){
	vuf.tmax.upper.corr[,j] <- quantile(vuf.corr.tmax.bm[,j],0.975, na.rm=T)
	vuf.tmax.lower.corr[,j] <- quantile(vuf.corr.tmax.bm[,j],0.025, na.rm=T)
	# vuf.tmax.mean.corr[,j] <- apply(vuf.corr.tmax.bm[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmax.upper.corr)
head(vuf.tmax.lower.corr)


# Big

vuf.corr.tmax.big <- as.data.frame(matrix(NA,nrow=ncol(vuf.big), ncol=ncol(vuf.tmax)-2))
dim(vuf.corr.tmax.big)

names(vuf.corr.tmax.big)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.corr.tmax.big) <- names(vuf.big)
summary(vuf.corr.tmax.big) 

dim(vuf.tmax)
dim(vuf.big)

for(i in 1:ncol(vuf.big)){
	vuf.corr.tmax.big[i,] <- cor(vuf.big[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.corr.tmax.big)

vuf.tmax.upper.corr.big <- vuf.corr.tmax.big
vuf.tmax.lower.corr.big <- vuf.corr.tmax.big
# vuf.tmax.mean.corr <- vuf.corr.tmax.big
for(j in 1:ncol(vuf.corr.tmax.big)){
	vuf.tmax.upper.corr.big[,j] <- quantile(vuf.corr.tmax.big[,j],0.975, na.rm=T)
	vuf.tmax.lower.corr.big[,j] <- quantile(vuf.corr.tmax.big[,j],0.025, na.rm=T)
	# vuf.tmax.mean.corr.big[,j] <- apply(vuf.corr.tmax.big[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmax.upper.corr.big)
head(vuf.tmax.lower.corr.big)


# Small

vuf.corr.tmax.small <- as.data.frame(matrix(NA,nrow=ncol(vuf.small), ncol=ncol(vuf.tmax)-2))
dim(vuf.corr.tmax.small)

names(vuf.corr.tmax.small)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.corr.tmax.small) <- names(vuf.small)
summary(vuf.corr.tmax.small) 

dim(vuf.tmax)
dim(vuf.small)

for(i in 1:ncol(vuf.small)){
	vuf.corr.tmax.small[i,] <- cor(vuf.small[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.corr.tmax.small)

vuf.tmax.upper.corr.small <- vuf.corr.tmax.small
vuf.tmax.lower.corr.small <- vuf.corr.tmax.small
# vuf.tmax.mean.corr <- vuf.corr.tmax.small
for(j in 1:ncol(vuf.corr.tmax.small)){
	vuf.tmax.upper.corr.small[,j] <- quantile(vuf.corr.tmax.small[,j],0.975, na.rm=T)
	vuf.tmax.lower.corr.small[,j] <- quantile(vuf.corr.tmax.small[,j],0.025, na.rm=T)
	# vuf.tmax.mean.corr.small[,j] <- apply(vuf.corr.tmax.small[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmax.upper.corr.small)
head(vuf.tmax.lower.corr.small)

# Dated

vuf.corr.tmax.dated <- as.data.frame(matrix(NA,nrow=ncol(vuf.dated), ncol=ncol(vuf.tmax)-2))
dim(vuf.corr.tmax.dated)

names(vuf.corr.tmax.dated)<- names(vuf.tmax[,3:ncol(vuf.tmax)]) 
row.names(vuf.corr.tmax.dated) <- names(vuf.dated)
summary(vuf.corr.tmax.dated) 

dim(vuf.tmax)
dim(vuf.dated)

for(i in 1:ncol(vuf.dated)){
	vuf.corr.tmax.dated[i,] <- cor(vuf.dated[,i], vuf.tmax[,3:ncol(vuf.tmax)], method="pearson")
}
head(vuf.corr.tmax.dated)

vuf.tmax.upper.corr.dated <- vuf.corr.tmax.dated
vuf.tmax.lower.corr.dated <- vuf.corr.tmax.dated
# vuf.tmax.mean.corr <- vuf.corr.tmax.dated
for(j in 1:ncol(vuf.corr.tmax.dated)){
	vuf.tmax.upper.corr.dated[,j] <- quantile(vuf.corr.tmax.dated[,j],0.975, na.rm=T)
	vuf.tmax.lower.corr.dated[,j] <- quantile(vuf.corr.tmax.dated[,j],0.025, na.rm=T)
	# vuf.tmax.mean.corr.dated[,j] <- apply(vuf.corr.tmax.dated[,j], FUN=mean, na.rm=T)
	
}
head(vuf.tmax.upper.corr.dated)
head(vuf.tmax.lower.corr.dated)



## VLF
# All Trees
vlf.corr.tmax.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(vlf.tmax)-2))
dim(vlf.corr.tmax.bm)

names(vlf.corr.tmax.bm)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.corr.tmax.bm) <- names(vlf.bm)
summary(vlf.corr.tmax.bm) 

dim(vlf.tmax)
dim(vlf.bm)

for(i in 1:ncol(vlf.bm)){
	vlf.corr.tmax.bm[i,] <- cor(vlf.bm[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.corr.tmax.bm)

vlf.tmax.upper.corr <- vlf.corr.tmax.bm
vlf.tmax.lower.corr <- vlf.corr.tmax.bm
for(j in 1:ncol(vlf.corr.tmax.bm)){
	vlf.tmax.upper.corr[,j] <- quantile(vlf.corr.tmax.bm[,j],0.975, na.rm=T)
	vlf.tmax.lower.corr[,j] <- quantile(vlf.corr.tmax.bm[,j],0.025, na.rm=T)
}
head(vlf.tmax.upper.corr)
head(vlf.tmax.lower.corr)



# Big

vlf.corr.tmax.big <- as.data.frame(matrix(NA,nrow=ncol(vlf.big), ncol=ncol(vlf.tmax)-2))
dim(vlf.corr.tmax.big)

names(vlf.corr.tmax.big)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.corr.tmax.big) <- names(vlf.big)
summary(vlf.corr.tmax.big) 

dim(vlf.tmax)
dim(vlf.big)

for(i in 1:ncol(vlf.big)){
	vlf.corr.tmax.big[i,] <- cor(vlf.big[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.corr.tmax.big)

vlf.tmax.upper.corr.big <- vlf.corr.tmax.big
vlf.tmax.lower.corr.big <- vlf.corr.tmax.big
# vlf.tmax.mean.corr <- vlf.corr.tmax.big
for(j in 1:ncol(vlf.corr.tmax.big)){
	vlf.tmax.upper.corr.big[,j] <- quantile(vlf.corr.tmax.big[,j],0.975, na.rm=T)
	vlf.tmax.lower.corr.big[,j] <- quantile(vlf.corr.tmax.big[,j],0.025, na.rm=T)
	# vlf.tmax.mean.corr.big[,j] <- apply(vlf.corr.tmax.big[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmax.upper.corr.big)
head(vlf.tmax.lower.corr.big)


# Small

vlf.corr.tmax.small <- as.data.frame(matrix(NA,nrow=ncol(vlf.small), ncol=ncol(vlf.tmax)-2))
dim(vlf.corr.tmax.small)

names(vlf.corr.tmax.small)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.corr.tmax.small) <- names(vlf.small)
summary(vlf.corr.tmax.small) 

dim(vlf.tmax)
dim(vlf.small)

for(i in 1:ncol(vlf.small)){
	vlf.corr.tmax.small[i,] <- cor(vlf.small[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.corr.tmax.small)

vlf.tmax.upper.corr.small <- vlf.corr.tmax.small
vlf.tmax.lower.corr.small <- vlf.corr.tmax.small
# vlf.tmax.mean.corr <- vlf.corr.tmax.small
for(j in 1:ncol(vlf.corr.tmax.small)){
	vlf.tmax.upper.corr.small[,j] <- quantile(vlf.corr.tmax.small[,j],0.975, na.rm=T)
	vlf.tmax.lower.corr.small[,j] <- quantile(vlf.corr.tmax.small[,j],0.025, na.rm=T)
	# vlf.tmax.mean.corr.small[,j] <- apply(vlf.corr.tmax.small[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmax.upper.corr.small)
head(vlf.tmax.lower.corr.small)

# Dated
vlf.corr.tmax.dated <- as.data.frame(matrix(NA,nrow=ncol(vlf.dated), ncol=ncol(vlf.tmax)-2))
dim(vlf.corr.tmax.dated)

names(vlf.corr.tmax.dated)<- names(vlf.tmax[,3:ncol(vlf.tmax)]) 
row.names(vlf.corr.tmax.dated) <- names(vlf.dated)
summary(vlf.corr.tmax.dated) 

dim(vlf.tmax)
dim(vlf.dated)

for(i in 1:ncol(vlf.dated)){
	vlf.corr.tmax.dated[i,] <- cor(vlf.dated[,i], vlf.tmax[,3:ncol(vlf.tmax)], method="pearson")
}
head(vlf.corr.tmax.dated)

vlf.tmax.upper.corr.dated <- vlf.corr.tmax.dated
vlf.tmax.lower.corr.dated <- vlf.corr.tmax.dated
# vlf.tmax.mean.corr <- vlf.corr.tmax.dated
for(j in 1:ncol(vlf.corr.tmax.dated)){
	vlf.tmax.upper.corr.dated[,j] <- quantile(vlf.corr.tmax.dated[,j],0.975, na.rm=T)
	vlf.tmax.lower.corr.dated[,j] <- quantile(vlf.corr.tmax.dated[,j],0.025, na.rm=T)
	# vlf.tmax.mean.corr.dated[,j] <- apply(vlf.corr.tmax.dated[,j], FUN=mean, na.rm=T)
	
}
head(vlf.tmax.upper.corr.dated)
head(vlf.tmax.lower.corr.dated)


# #------------------------------
# # PDSI
# #------------------------------

# ## VUF
# vuf.corr.pdsi.bm <- as.data.frame(matrix(NA,nrow=ncol(vuf.bm), ncol=ncol(pdsi)-1))
# dim(vuf.corr.pdsi.bm)

# names(vuf.corr.pdsi.bm)<- names(pdsi[,2:ncol(pdsi)]) 
# row.names(vuf.corr.pdsi.bm) <- names(vuf.bm)
# summary(vuf.corr.pdsi.bm) 

# dim(pdsi)
# dim(vuf.bm)

# for(i in 1:ncol(vuf.bm)){
	# vuf.corr.pdsi.bm[i,] <- cor(vuf.bm[,i], pdsi[,2:ncol(pdsi)], method="pearson")
# }
# head(vuf.corr.pdsi.bm)

# vuf.pdsi.mean.corr <- vuf.corr.pdsi.bm
# for(j in 1:ncol(vuf.pdsi.mean.corr)){
	# vuf.pdsi.mean.corr[,j] <- mean(vuf.corr.pdsi.bm[,j], na.rm=T)
# }
# head(vuf.pdsi.mean.corr)

# ## VLF

# vlf.corr.pdsi.bm <- as.data.frame(matrix(NA,nrow=ncol(vlf.bm), ncol=ncol(pdsi)-1))
# dim(vlf.corr.pdsi.bm)

# names(vlf.corr.pdsi.bm)<- names(pdsi[,2:ncol(pdsi)]) 
# row.names(vlf.corr.pdsi.bm) <- names(vlf.bm)
# summary(vlf.corr.pdsi.bm) 

# dim(pdsi)
# dim(vlf.bm)

# for(i in 1:ncol(vlf.bm)){
	# vlf.corr.pdsi.bm[i,] <- cor(vlf.bm[,i], pdsi[,2:ncol(pdsi)], method="pearson")
# }
# head(vlf.corr.pdsi.bm)

# vlf.pdsi.mean.corr <- vlf.corr.pdsi.bm
# for(j in 1:ncol(vlf.pdsi.mean.corr)){
	# vlf.pdsi.mean.corr[,j] <- mean(vlf.corr.pdsi.bm[,j], na.rm=T)
# }
# head(vlf.pdsi.mean.corr)

#######################################################
# Stackign the BM correlations
#######################################################
# Tmean

# VUF
# All Trees

vuf.bm.tmean.stack <- stack(vuf.corr.tmean.bm)
summary(vuf.bm.tmean.stack)
names(vuf.bm.tmean.stack) <- c("corr", "month")
vuf.bm.tmean.stack$site <- as.factor("VUF")
vuf.bm.tmean.stack$type <- as.factor("tmean")
vuf.bm.tmean.stack$size <- as.factor("all")
summary(vuf.bm.tmean.stack)

vuf.tmean.upper.corr.stack <- stack(vuf.tmean.upper.corr)
summary(vuf.tmean.upper.corr.stack)
names(vuf.tmean.upper.corr.stack) <- c("Upper", "month")
vuf.tmean.upper.corr.stack$site <- as.factor("VUF")
vuf.tmean.upper.corr.stack$type <- as.factor("tmean")
vuf.tmean.upper.corr.stack$size <- as.factor("all")
summary(vuf.tmean.upper.corr.stack)

vuf.tmean.lower.corr.stack <- stack(vuf.tmean.lower.corr)
summary(vuf.tmean.lower.corr.stack)
names(vuf.tmean.lower.corr.stack) <- c("Lower", "month")
vuf.tmean.lower.corr.stack$site <- as.factor("VUF")
vuf.tmean.lower.corr.stack$type <- as.factor("tmean")
vuf.tmean.lower.corr.stack$size <- as.factor("all")
summary(vuf.tmean.lower.corr.stack)

vuf.bm.tmean.stack <- cbind(vuf.bm.tmean.stack, vuf.tmean.upper.corr.stack[,"Upper"], vuf.tmean.lower.corr.stack[,"Lower"])
names(vuf.bm.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.bm.tmean.stack)

# Big Trees
vuf.big.tmean.stack <- stack(vuf.corr.tmean.big)
summary(vuf.big.tmean.stack)
names(vuf.big.tmean.stack) <- c("corr", "month")
vuf.big.tmean.stack$site <- as.factor("VUF")
vuf.big.tmean.stack$type <- as.factor("tmean")
vuf.big.tmean.stack$size <- as.factor("big")
summary(vuf.big.tmean.stack)

vuf.tmean.upper.big.corr.stack <- stack(vuf.tmean.upper.corr.big)
summary(vuf.tmean.upper.corr.stack)
names(vuf.tmean.upper.big.corr.stack) <- c("Upper", "month")
vuf.tmean.upper.big.corr.stack$site <- as.factor("VUF")
vuf.tmean.upper.big.corr.stack$type <- as.factor("tmean")
vuf.tmean.upper.big.corr.stack$size <- as.factor("big")
summary(vuf.tmean.upper.big.corr.stack)

vuf.tmean.lower.big.corr.stack <- stack(vuf.tmean.lower.corr.big)
summary(vuf.tmean.lower.big.corr.stack)
names(vuf.tmean.lower.big.corr.stack) <- c("Lower", "month")
vuf.tmean.lower.big.corr.stack$site <- as.factor("VUF")
vuf.tmean.lower.big.corr.stack$type <- as.factor("tmean")
vuf.tmean.lower.big.corr.stack$size <- as.factor("big")
summary(vuf.tmean.lower.big.corr.stack)

vuf.big.tmean.stack <- cbind(vuf.big.tmean.stack, vuf.tmean.upper.big.corr.stack[,"Upper"], vuf.tmean.lower.big.corr.stack[,"Lower"])
names(vuf.big.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.big.tmean.stack)


# Small Trees

vuf.small.tmean.stack <- stack(vuf.corr.tmean.small)
summary(vuf.small.tmean.stack)
names(vuf.small.tmean.stack) <- c("corr", "month")
vuf.small.tmean.stack$site <- as.factor("VUF")
vuf.small.tmean.stack$type <- as.factor("tmean")
vuf.small.tmean.stack$size <- as.factor("small")
summary(vuf.small.tmean.stack)

vuf.tmean.upper.small.corr.stack <- stack(vuf.tmean.upper.corr.small)
summary(vuf.tmean.upper.corr.stack)
names(vuf.tmean.upper.small.corr.stack) <- c("Upper", "month")
vuf.tmean.upper.small.corr.stack$site <- as.factor("VUF")
vuf.tmean.upper.small.corr.stack$type <- as.factor("tmean")
vuf.tmean.upper.small.corr.stack$size <- as.factor("small")
summary(vuf.tmean.upper.small.corr.stack)

vuf.tmean.lower.small.corr.stack <- stack(vuf.tmean.lower.corr.small)
summary(vuf.tmean.lower.small.corr.stack)
names(vuf.tmean.lower.small.corr.stack) <- c("Lower", "month")
vuf.tmean.lower.small.corr.stack$site <- as.factor("VUF")
vuf.tmean.lower.small.corr.stack$type <- as.factor("tmean")
vuf.tmean.lower.small.corr.stack$size <- as.factor("small")
summary(vuf.tmean.lower.small.corr.stack)

vuf.small.tmean.stack <- cbind(vuf.small.tmean.stack, vuf.tmean.upper.small.corr.stack[,"Upper"], vuf.tmean.lower.small.corr.stack[,"Lower"])
names(vuf.small.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.small.tmean.stack)

# Dated
vuf.dated.tmean.stack <- stack(vuf.corr.tmean.dated)
summary(vuf.dated.tmean.stack)
names(vuf.dated.tmean.stack) <- c("corr", "month")
vuf.dated.tmean.stack$site <- as.factor("VUF")
vuf.dated.tmean.stack$type <- as.factor("tmean")
vuf.dated.tmean.stack$size <- as.factor("dated")
summary(vuf.dated.tmean.stack)

vuf.tmean.upper.dated.corr.stack <- stack(vuf.tmean.upper.corr.dated)
summary(vuf.tmean.upper.corr.stack)
names(vuf.tmean.upper.dated.corr.stack) <- c("Upper", "month")
vuf.tmean.upper.dated.corr.stack$site <- as.factor("VUF")
vuf.tmean.upper.dated.corr.stack$type <- as.factor("tmean")
vuf.tmean.upper.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmean.upper.dated.corr.stack)

vuf.tmean.lower.dated.corr.stack <- stack(vuf.tmean.lower.corr.dated)
summary(vuf.tmean.lower.dated.corr.stack)
names(vuf.tmean.lower.dated.corr.stack) <- c("Lower", "month")
vuf.tmean.lower.dated.corr.stack$site <- as.factor("VUF")
vuf.tmean.lower.dated.corr.stack$type <- as.factor("tmean")
vuf.tmean.lower.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmean.lower.dated.corr.stack)

vuf.dated.tmean.stack <- cbind(vuf.dated.tmean.stack, vuf.tmean.upper.dated.corr.stack[,"Upper"], vuf.tmean.lower.dated.corr.stack[,"Lower"])
names(vuf.dated.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.dated.tmean.stack)


# VLF
# All Trees

vlf.bm.tmean.stack <- stack(vlf.corr.tmean.bm)
summary(vlf.bm.tmean.stack)
names(vlf.bm.tmean.stack) <- c("corr", "month")
vlf.bm.tmean.stack$site <- as.factor("vlf")
vlf.bm.tmean.stack$type <- as.factor("tmean")
vlf.bm.tmean.stack$size <- as.factor("all")
summary(vlf.bm.tmean.stack)

vlf.tmean.upper.corr.stack <- stack(vlf.tmean.upper.corr)
summary(vlf.tmean.upper.corr.stack)
names(vlf.tmean.upper.corr.stack) <- c("Upper", "month")
vlf.tmean.upper.corr.stack$site <- as.factor("vlf")
vlf.tmean.upper.corr.stack$type <- as.factor("tmean")
vlf.tmean.upper.corr.stack$size <- as.factor("all")
summary(vlf.tmean.upper.corr.stack)

vlf.tmean.lower.corr.stack <- stack(vlf.tmean.lower.corr)
summary(vlf.tmean.lower.corr.stack)
names(vlf.tmean.lower.corr.stack) <- c("Lower", "month")
vlf.tmean.lower.corr.stack$site <- as.factor("vlf")
vlf.tmean.lower.corr.stack$type <- as.factor("tmean")
vlf.tmean.lower.corr.stack$size <- as.factor("all")
summary(vlf.tmean.lower.corr.stack)

vlf.bm.tmean.stack <- cbind(vlf.bm.tmean.stack, vlf.tmean.upper.corr.stack[,"Upper"], vlf.tmean.lower.corr.stack[,"Lower"])
names(vlf.bm.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.bm.tmean.stack)

# Big Trees
vlf.big.tmean.stack <- stack(vlf.corr.tmean.big)
summary(vlf.big.tmean.stack)
names(vlf.big.tmean.stack) <- c("corr", "month")
vlf.big.tmean.stack$site <- as.factor("vlf")
vlf.big.tmean.stack$type <- as.factor("tmean")
vlf.big.tmean.stack$size <- as.factor("big")
summary(vlf.big.tmean.stack)

vlf.tmean.upper.big.corr.stack <- stack(vlf.tmean.upper.corr.big)
summary(vlf.tmean.upper.corr.stack)
names(vlf.tmean.upper.big.corr.stack) <- c("Upper", "month")
vlf.tmean.upper.big.corr.stack$site <- as.factor("vlf")
vlf.tmean.upper.big.corr.stack$type <- as.factor("tmean")
vlf.tmean.upper.big.corr.stack$size <- as.factor("big")
summary(vlf.tmean.upper.big.corr.stack)

vlf.tmean.lower.big.corr.stack <- stack(vlf.tmean.lower.corr.big)
summary(vlf.tmean.lower.big.corr.stack)
names(vlf.tmean.lower.big.corr.stack) <- c("Lower", "month")
vlf.tmean.lower.big.corr.stack$site <- as.factor("vlf")
vlf.tmean.lower.big.corr.stack$type <- as.factor("tmean")
vlf.tmean.lower.big.corr.stack$size <- as.factor("big")
summary(vlf.tmean.lower.big.corr.stack)

vlf.big.tmean.stack <- cbind(vlf.big.tmean.stack, vlf.tmean.upper.big.corr.stack[,"Upper"], vlf.tmean.lower.big.corr.stack[,"Lower"])
names(vlf.big.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.big.tmean.stack)


# Small Trees

vlf.small.tmean.stack <- stack(vlf.corr.tmean.small)
summary(vlf.small.tmean.stack)
names(vlf.small.tmean.stack) <- c("corr", "month")
vlf.small.tmean.stack$site <- as.factor("vlf")
vlf.small.tmean.stack$type <- as.factor("tmean")
vlf.small.tmean.stack$size <- as.factor("small")
summary(vlf.small.tmean.stack)

vlf.tmean.upper.small.corr.stack <- stack(vlf.tmean.upper.corr.small)
summary(vlf.tmean.upper.corr.stack)
names(vlf.tmean.upper.small.corr.stack) <- c("Upper", "month")
vlf.tmean.upper.small.corr.stack$site <- as.factor("vlf")
vlf.tmean.upper.small.corr.stack$type <- as.factor("tmean")
vlf.tmean.upper.small.corr.stack$size <- as.factor("small")
summary(vlf.tmean.upper.small.corr.stack)

vlf.tmean.lower.small.corr.stack <- stack(vlf.tmean.lower.corr.small)
summary(vlf.tmean.lower.small.corr.stack)
names(vlf.tmean.lower.small.corr.stack) <- c("Lower", "month")
vlf.tmean.lower.small.corr.stack$site <- as.factor("vlf")
vlf.tmean.lower.small.corr.stack$type <- as.factor("tmean")
vlf.tmean.lower.small.corr.stack$size <- as.factor("small")
summary(vlf.tmean.lower.small.corr.stack)

vlf.small.tmean.stack <- cbind(vlf.small.tmean.stack, vlf.tmean.upper.small.corr.stack[,"Upper"], vlf.tmean.lower.small.corr.stack[,"Lower"])
names(vlf.small.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.small.tmean.stack)

# Dated
vlf.dated.tmean.stack <- stack(vlf.corr.tmean.dated)
summary(vlf.dated.tmean.stack)
names(vlf.dated.tmean.stack) <- c("corr", "month")
vlf.dated.tmean.stack$site <- as.factor("vlf")
vlf.dated.tmean.stack$type <- as.factor("tmean")
vlf.dated.tmean.stack$size <- as.factor("dated")
summary(vlf.dated.tmean.stack)

vlf.tmean.upper.dated.corr.stack <- stack(vlf.tmean.upper.corr.dated)
summary(vlf.tmean.upper.corr.stack)
names(vlf.tmean.upper.dated.corr.stack) <- c("Upper", "month")
vlf.tmean.upper.dated.corr.stack$site <- as.factor("vlf")
vlf.tmean.upper.dated.corr.stack$type <- as.factor("tmean")
vlf.tmean.upper.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmean.upper.dated.corr.stack)

vlf.tmean.lower.dated.corr.stack <- stack(vlf.tmean.lower.corr.dated)
summary(vlf.tmean.lower.dated.corr.stack)
names(vlf.tmean.lower.dated.corr.stack) <- c("Lower", "month")
vlf.tmean.lower.dated.corr.stack$site <- as.factor("vlf")
vlf.tmean.lower.dated.corr.stack$type <- as.factor("tmean")
vlf.tmean.lower.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmean.lower.dated.corr.stack)

vlf.dated.tmean.stack <- cbind(vlf.dated.tmean.stack, vlf.tmean.upper.dated.corr.stack[,"Upper"], vlf.tmean.lower.dated.corr.stack[,"Lower"])
names(vlf.dated.tmean.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.dated.tmean.stack)


#-----------------------------------------
# Tmin
# VUF
# All Trees

vuf.bm.tmin.stack <- stack(vuf.corr.tmin.bm)
summary(vuf.bm.tmin.stack)
names(vuf.bm.tmin.stack) <- c("corr", "month")
vuf.bm.tmin.stack$site <- as.factor("VUF")
vuf.bm.tmin.stack$type <- as.factor("tmin")
vuf.bm.tmin.stack$size <- as.factor("all")
summary(vuf.bm.tmin.stack)

vuf.tmin.upper.corr.stack <- stack(vuf.tmin.upper.corr)
summary(vuf.tmin.upper.corr.stack)
names(vuf.tmin.upper.corr.stack) <- c("Upper", "month")
vuf.tmin.upper.corr.stack$site <- as.factor("VUF")
vuf.tmin.upper.corr.stack$type <- as.factor("tmin")
vuf.tmin.upper.corr.stack$size <- as.factor("all")
summary(vuf.tmin.upper.corr.stack)

vuf.tmin.lower.corr.stack <- stack(vuf.tmin.lower.corr)
summary(vuf.tmin.lower.corr.stack)
names(vuf.tmin.lower.corr.stack) <- c("Lower", "month")
vuf.tmin.lower.corr.stack$site <- as.factor("VUF")
vuf.tmin.lower.corr.stack$type <- as.factor("tmin")
vuf.tmin.lower.corr.stack$size <- as.factor("all")
summary(vuf.tmin.lower.corr.stack)

vuf.bm.tmin.stack <- cbind(vuf.bm.tmin.stack, vuf.tmin.upper.corr.stack[,"Upper"], vuf.tmin.lower.corr.stack[,"Lower"])
names(vuf.bm.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.bm.tmin.stack)

# Big Trees
vuf.big.tmin.stack <- stack(vuf.corr.tmin.big)
summary(vuf.big.tmin.stack)
names(vuf.big.tmin.stack) <- c("corr", "month")
vuf.big.tmin.stack$site <- as.factor("VUF")
vuf.big.tmin.stack$type <- as.factor("tmin")
vuf.big.tmin.stack$size <- as.factor("big")
summary(vuf.big.tmin.stack)

vuf.tmin.upper.big.corr.stack <- stack(vuf.tmin.upper.corr.big)
summary(vuf.tmin.upper.corr.stack)
names(vuf.tmin.upper.big.corr.stack) <- c("Upper", "month")
vuf.tmin.upper.big.corr.stack$site <- as.factor("VUF")
vuf.tmin.upper.big.corr.stack$type <- as.factor("tmin")
vuf.tmin.upper.big.corr.stack$size <- as.factor("big")
summary(vuf.tmin.upper.big.corr.stack)

vuf.tmin.lower.big.corr.stack <- stack(vuf.tmin.lower.corr.big)
summary(vuf.tmin.lower.big.corr.stack)
names(vuf.tmin.lower.big.corr.stack) <- c("Lower", "month")
vuf.tmin.lower.big.corr.stack$site <- as.factor("VUF")
vuf.tmin.lower.big.corr.stack$type <- as.factor("tmin")
vuf.tmin.lower.big.corr.stack$size <- as.factor("big")
summary(vuf.tmin.lower.big.corr.stack)

vuf.big.tmin.stack <- cbind(vuf.big.tmin.stack, vuf.tmin.upper.big.corr.stack[,"Upper"], vuf.tmin.lower.big.corr.stack[,"Lower"])
names(vuf.big.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.big.tmin.stack)


# Small Trees

vuf.small.tmin.stack <- stack(vuf.corr.tmin.small)
summary(vuf.small.tmin.stack)
names(vuf.small.tmin.stack) <- c("corr", "month")
vuf.small.tmin.stack$site <- as.factor("VUF")
vuf.small.tmin.stack$type <- as.factor("tmin")
vuf.small.tmin.stack$size <- as.factor("small")
summary(vuf.small.tmin.stack)

vuf.tmin.upper.small.corr.stack <- stack(vuf.tmin.upper.corr.small)
summary(vuf.tmin.upper.corr.stack)
names(vuf.tmin.upper.small.corr.stack) <- c("Upper", "month")
vuf.tmin.upper.small.corr.stack$site <- as.factor("VUF")
vuf.tmin.upper.small.corr.stack$type <- as.factor("tmin")
vuf.tmin.upper.small.corr.stack$size <- as.factor("small")
summary(vuf.tmin.upper.small.corr.stack)

vuf.tmin.lower.small.corr.stack <- stack(vuf.tmin.lower.corr.small)
summary(vuf.tmin.lower.small.corr.stack)
names(vuf.tmin.lower.small.corr.stack) <- c("Lower", "month")
vuf.tmin.lower.small.corr.stack$site <- as.factor("VUF")
vuf.tmin.lower.small.corr.stack$type <- as.factor("tmin")
vuf.tmin.lower.small.corr.stack$size <- as.factor("small")
summary(vuf.tmin.lower.small.corr.stack)

vuf.small.tmin.stack <- cbind(vuf.small.tmin.stack, vuf.tmin.upper.small.corr.stack[,"Upper"], vuf.tmin.lower.small.corr.stack[,"Lower"])
names(vuf.small.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.small.tmin.stack)


# Dated
vuf.dated.tmin.stack <- stack(vuf.corr.tmin.dated)
summary(vuf.dated.tmin.stack)
names(vuf.dated.tmin.stack) <- c("corr", "month")
vuf.dated.tmin.stack$site <- as.factor("VUF")
vuf.dated.tmin.stack$type <- as.factor("tmin")
vuf.dated.tmin.stack$size <- as.factor("dated")
summary(vuf.dated.tmin.stack)

vuf.tmin.upper.dated.corr.stack <- stack(vuf.tmin.upper.corr.dated)
summary(vuf.tmin.upper.corr.stack)
names(vuf.tmin.upper.dated.corr.stack) <- c("Upper", "month")
vuf.tmin.upper.dated.corr.stack$site <- as.factor("VUF")
vuf.tmin.upper.dated.corr.stack$type <- as.factor("tmin")
vuf.tmin.upper.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmin.upper.dated.corr.stack)

vuf.tmin.lower.dated.corr.stack <- stack(vuf.tmin.lower.corr.dated)
summary(vuf.tmin.lower.dated.corr.stack)
names(vuf.tmin.lower.dated.corr.stack) <- c("Lower", "month")
vuf.tmin.lower.dated.corr.stack$site <- as.factor("VUF")
vuf.tmin.lower.dated.corr.stack$type <- as.factor("tmin")
vuf.tmin.lower.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmin.lower.dated.corr.stack)

vuf.dated.tmin.stack <- cbind(vuf.dated.tmin.stack, vuf.tmin.upper.dated.corr.stack[,"Upper"], vuf.tmin.lower.dated.corr.stack[,"Lower"])
names(vuf.dated.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.dated.tmin.stack)



# VLF
# All Trees

vlf.bm.tmin.stack <- stack(vlf.corr.tmin.bm)
summary(vlf.bm.tmin.stack)
names(vlf.bm.tmin.stack) <- c("corr", "month")
vlf.bm.tmin.stack$site <- as.factor("vlf")
vlf.bm.tmin.stack$type <- as.factor("tmin")
vlf.bm.tmin.stack$size <- as.factor("all")
summary(vlf.bm.tmin.stack)

vlf.tmin.upper.corr.stack <- stack(vlf.tmin.upper.corr)
summary(vlf.tmin.upper.corr.stack)
names(vlf.tmin.upper.corr.stack) <- c("Upper", "month")
vlf.tmin.upper.corr.stack$site <- as.factor("vlf")
vlf.tmin.upper.corr.stack$type <- as.factor("tmin")
vlf.tmin.upper.corr.stack$size <- as.factor("all")
summary(vlf.tmin.upper.corr.stack)

vlf.tmin.lower.corr.stack <- stack(vlf.tmin.lower.corr)
summary(vlf.tmin.lower.corr.stack)
names(vlf.tmin.lower.corr.stack) <- c("Lower", "month")
vlf.tmin.lower.corr.stack$site <- as.factor("vlf")
vlf.tmin.lower.corr.stack$type <- as.factor("tmin")
vlf.tmin.lower.corr.stack$size <- as.factor("all")
summary(vlf.tmin.lower.corr.stack)

vlf.bm.tmin.stack <- cbind(vlf.bm.tmin.stack, vlf.tmin.upper.corr.stack[,"Upper"], vlf.tmin.lower.corr.stack[,"Lower"])
names(vlf.bm.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.bm.tmin.stack)

# Big Trees
vlf.big.tmin.stack <- stack(vlf.corr.tmin.big)
summary(vlf.big.tmin.stack)
names(vlf.big.tmin.stack) <- c("corr", "month")
vlf.big.tmin.stack$site <- as.factor("vlf")
vlf.big.tmin.stack$type <- as.factor("tmin")
vlf.big.tmin.stack$size <- as.factor("big")
summary(vlf.big.tmin.stack)

vlf.tmin.upper.big.corr.stack <- stack(vlf.tmin.upper.corr.big)
summary(vlf.tmin.upper.corr.stack)
names(vlf.tmin.upper.big.corr.stack) <- c("Upper", "month")
vlf.tmin.upper.big.corr.stack$site <- as.factor("vlf")
vlf.tmin.upper.big.corr.stack$type <- as.factor("tmin")
vlf.tmin.upper.big.corr.stack$size <- as.factor("big")
summary(vlf.tmin.upper.big.corr.stack)

vlf.tmin.lower.big.corr.stack <- stack(vlf.tmin.lower.corr.big)
summary(vlf.tmin.lower.big.corr.stack)
names(vlf.tmin.lower.big.corr.stack) <- c("Lower", "month")
vlf.tmin.lower.big.corr.stack$site <- as.factor("vlf")
vlf.tmin.lower.big.corr.stack$type <- as.factor("tmin")
vlf.tmin.lower.big.corr.stack$size <- as.factor("big")
summary(vlf.tmin.lower.big.corr.stack)

vlf.big.tmin.stack <- cbind(vlf.big.tmin.stack, vlf.tmin.upper.big.corr.stack[,"Upper"], vlf.tmin.lower.big.corr.stack[,"Lower"])
names(vlf.big.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.big.tmin.stack)


# Small Trees

vlf.small.tmin.stack <- stack(vlf.corr.tmin.small)
summary(vlf.small.tmin.stack)
names(vlf.small.tmin.stack) <- c("corr", "month")
vlf.small.tmin.stack$site <- as.factor("vlf")
vlf.small.tmin.stack$type <- as.factor("tmin")
vlf.small.tmin.stack$size <- as.factor("small")
summary(vlf.small.tmin.stack)

vlf.tmin.upper.small.corr.stack <- stack(vlf.tmin.upper.corr.small)
summary(vlf.tmin.upper.corr.stack)
names(vlf.tmin.upper.small.corr.stack) <- c("Upper", "month")
vlf.tmin.upper.small.corr.stack$site <- as.factor("vlf")
vlf.tmin.upper.small.corr.stack$type <- as.factor("tmin")
vlf.tmin.upper.small.corr.stack$size <- as.factor("small")
summary(vlf.tmin.upper.small.corr.stack)

vlf.tmin.lower.small.corr.stack <- stack(vlf.tmin.lower.corr.small)
summary(vlf.tmin.lower.small.corr.stack)
names(vlf.tmin.lower.small.corr.stack) <- c("Lower", "month")
vlf.tmin.lower.small.corr.stack$site <- as.factor("vlf")
vlf.tmin.lower.small.corr.stack$type <- as.factor("tmin")
vlf.tmin.lower.small.corr.stack$size <- as.factor("small")
summary(vlf.tmin.lower.small.corr.stack)

vlf.small.tmin.stack <- cbind(vlf.small.tmin.stack, vlf.tmin.upper.small.corr.stack[,"Upper"], vlf.tmin.lower.small.corr.stack[,"Lower"])
names(vlf.small.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.small.tmin.stack)

# Dated Trees
vlf.dated.tmin.stack <- stack(vlf.corr.tmin.dated)
summary(vlf.dated.tmin.stack)
names(vlf.dated.tmin.stack) <- c("corr", "month")
vlf.dated.tmin.stack$site <- as.factor("vlf")
vlf.dated.tmin.stack$type <- as.factor("tmin")
vlf.dated.tmin.stack$size <- as.factor("dated")
summary(vlf.dated.tmin.stack)

vlf.tmin.upper.dated.corr.stack <- stack(vlf.tmin.upper.corr.dated)
summary(vlf.tmin.upper.corr.stack)
names(vlf.tmin.upper.dated.corr.stack) <- c("Upper", "month")
vlf.tmin.upper.dated.corr.stack$site <- as.factor("vlf")
vlf.tmin.upper.dated.corr.stack$type <- as.factor("tmin")
vlf.tmin.upper.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmin.upper.dated.corr.stack)

vlf.tmin.lower.dated.corr.stack <- stack(vlf.tmin.lower.corr.dated)
summary(vlf.tmin.lower.dated.corr.stack)
names(vlf.tmin.lower.dated.corr.stack) <- c("Lower", "month")
vlf.tmin.lower.dated.corr.stack$site <- as.factor("vlf")
vlf.tmin.lower.dated.corr.stack$type <- as.factor("tmin")
vlf.tmin.lower.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmin.lower.dated.corr.stack)

vlf.dated.tmin.stack <- cbind(vlf.dated.tmin.stack, vlf.tmin.upper.dated.corr.stack[,"Upper"], vlf.tmin.lower.dated.corr.stack[,"Lower"])
names(vlf.dated.tmin.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.dated.tmin.stack)

#-----------------------------
# Tmax
# VUF
# All Trees

vuf.bm.tmax.stack <- stack(vuf.corr.tmax.bm)
summary(vuf.bm.tmax.stack)
names(vuf.bm.tmax.stack) <- c("corr", "month")
vuf.bm.tmax.stack$site <- as.factor("VUF")
vuf.bm.tmax.stack$type <- as.factor("tmax")
vuf.bm.tmax.stack$size <- as.factor("all")
summary(vuf.bm.tmax.stack)

vuf.tmax.upper.corr.stack <- stack(vuf.tmax.upper.corr)
summary(vuf.tmax.upper.corr.stack)
names(vuf.tmax.upper.corr.stack) <- c("Upper", "month")
vuf.tmax.upper.corr.stack$site <- as.factor("VUF")
vuf.tmax.upper.corr.stack$type <- as.factor("tmax")
vuf.tmax.upper.corr.stack$size <- as.factor("all")
summary(vuf.tmax.upper.corr.stack)

vuf.tmax.lower.corr.stack <- stack(vuf.tmax.lower.corr)
summary(vuf.tmax.lower.corr.stack)
names(vuf.tmax.lower.corr.stack) <- c("Lower", "month")
vuf.tmax.lower.corr.stack$site <- as.factor("VUF")
vuf.tmax.lower.corr.stack$type <- as.factor("tmax")
vuf.tmax.lower.corr.stack$size <- as.factor("all")
summary(vuf.tmax.lower.corr.stack)

vuf.bm.tmax.stack <- cbind(vuf.bm.tmax.stack, vuf.tmax.upper.corr.stack[,"Upper"], vuf.tmax.lower.corr.stack[,"Lower"])
names(vuf.bm.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.bm.tmax.stack)

# Big Trees
vuf.big.tmax.stack <- stack(vuf.corr.tmax.big)
summary(vuf.big.tmax.stack)
names(vuf.big.tmax.stack) <- c("corr", "month")
vuf.big.tmax.stack$site <- as.factor("VUF")
vuf.big.tmax.stack$type <- as.factor("tmax")
vuf.big.tmax.stack$size <- as.factor("big")
summary(vuf.big.tmax.stack)

vuf.tmax.upper.big.corr.stack <- stack(vuf.tmax.upper.corr.big)
summary(vuf.tmax.upper.corr.stack)
names(vuf.tmax.upper.big.corr.stack) <- c("Upper", "month")
vuf.tmax.upper.big.corr.stack$site <- as.factor("VUF")
vuf.tmax.upper.big.corr.stack$type <- as.factor("tmax")
vuf.tmax.upper.big.corr.stack$size <- as.factor("big")
summary(vuf.tmax.upper.big.corr.stack)

vuf.tmax.lower.big.corr.stack <- stack(vuf.tmax.lower.corr.big)
summary(vuf.tmax.lower.big.corr.stack)
names(vuf.tmax.lower.big.corr.stack) <- c("Lower", "month")
vuf.tmax.lower.big.corr.stack$site <- as.factor("VUF")
vuf.tmax.lower.big.corr.stack$type <- as.factor("tmax")
vuf.tmax.lower.big.corr.stack$size <- as.factor("big")
summary(vuf.tmax.lower.big.corr.stack)

vuf.big.tmax.stack <- cbind(vuf.big.tmax.stack, vuf.tmax.upper.big.corr.stack[,"Upper"], vuf.tmax.lower.big.corr.stack[,"Lower"])
names(vuf.big.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.big.tmax.stack)


# Small Trees

vuf.small.tmax.stack <- stack(vuf.corr.tmax.small)
summary(vuf.small.tmax.stack)
names(vuf.small.tmax.stack) <- c("corr", "month")
vuf.small.tmax.stack$site <- as.factor("VUF")
vuf.small.tmax.stack$type <- as.factor("tmax")
vuf.small.tmax.stack$size <- as.factor("small")
summary(vuf.small.tmax.stack)

vuf.tmax.upper.small.corr.stack <- stack(vuf.tmax.upper.corr.small)
summary(vuf.tmax.upper.corr.stack)
names(vuf.tmax.upper.small.corr.stack) <- c("Upper", "month")
vuf.tmax.upper.small.corr.stack$site <- as.factor("VUF")
vuf.tmax.upper.small.corr.stack$type <- as.factor("tmax")
vuf.tmax.upper.small.corr.stack$size <- as.factor("small")
summary(vuf.tmax.upper.small.corr.stack)

vuf.tmax.lower.small.corr.stack <- stack(vuf.tmax.lower.corr.small)
summary(vuf.tmax.lower.small.corr.stack)
names(vuf.tmax.lower.small.corr.stack) <- c("Lower", "month")
vuf.tmax.lower.small.corr.stack$site <- as.factor("VUF")
vuf.tmax.lower.small.corr.stack$type <- as.factor("tmax")
vuf.tmax.lower.small.corr.stack$size <- as.factor("small")
summary(vuf.tmax.lower.small.corr.stack)

vuf.small.tmax.stack <- cbind(vuf.small.tmax.stack, vuf.tmax.upper.small.corr.stack[,"Upper"], vuf.tmax.lower.small.corr.stack[,"Lower"])
names(vuf.small.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.small.tmax.stack)

# Dated Trees
vuf.dated.tmax.stack <- stack(vuf.corr.tmax.dated)
summary(vuf.dated.tmax.stack)
names(vuf.dated.tmax.stack) <- c("corr", "month")
vuf.dated.tmax.stack$site <- as.factor("VUF")
vuf.dated.tmax.stack$type <- as.factor("tmax")
vuf.dated.tmax.stack$size <- as.factor("dated")
summary(vuf.dated.tmax.stack)

vuf.tmax.upper.dated.corr.stack <- stack(vuf.tmax.upper.corr.dated)
summary(vuf.tmax.upper.corr.stack)
names(vuf.tmax.upper.dated.corr.stack) <- c("Upper", "month")
vuf.tmax.upper.dated.corr.stack$site <- as.factor("VUF")
vuf.tmax.upper.dated.corr.stack$type <- as.factor("tmax")
vuf.tmax.upper.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmax.upper.dated.corr.stack)

vuf.tmax.lower.dated.corr.stack <- stack(vuf.tmax.lower.corr.dated)
summary(vuf.tmax.lower.dated.corr.stack)
names(vuf.tmax.lower.dated.corr.stack) <- c("Lower", "month")
vuf.tmax.lower.dated.corr.stack$site <- as.factor("VUF")
vuf.tmax.lower.dated.corr.stack$type <- as.factor("tmax")
vuf.tmax.lower.dated.corr.stack$size <- as.factor("dated")
summary(vuf.tmax.lower.dated.corr.stack)

vuf.dated.tmax.stack <- cbind(vuf.dated.tmax.stack, vuf.tmax.upper.dated.corr.stack[,"Upper"], vuf.tmax.lower.dated.corr.stack[,"Lower"])
names(vuf.dated.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.dated.tmax.stack)


# VLF
# All Trees

vlf.bm.tmax.stack <- stack(vlf.corr.tmax.bm)
summary(vlf.bm.tmax.stack)
names(vlf.bm.tmax.stack) <- c("corr", "month")
vlf.bm.tmax.stack$site <- as.factor("vlf")
vlf.bm.tmax.stack$type <- as.factor("tmax")
vlf.bm.tmax.stack$size <- as.factor("all")
summary(vlf.bm.tmax.stack)

vlf.tmax.upper.corr.stack <- stack(vlf.tmax.upper.corr)
summary(vlf.tmax.upper.corr.stack)
names(vlf.tmax.upper.corr.stack) <- c("Upper", "month")
vlf.tmax.upper.corr.stack$site <- as.factor("vlf")
vlf.tmax.upper.corr.stack$type <- as.factor("tmax")
vlf.tmax.upper.corr.stack$size <- as.factor("all")
summary(vlf.tmax.upper.corr.stack)

vlf.tmax.lower.corr.stack <- stack(vlf.tmax.lower.corr)
summary(vlf.tmax.lower.corr.stack)
names(vlf.tmax.lower.corr.stack) <- c("Lower", "month")
vlf.tmax.lower.corr.stack$site <- as.factor("vlf")
vlf.tmax.lower.corr.stack$type <- as.factor("tmax")
vlf.tmax.lower.corr.stack$size <- as.factor("all")
summary(vlf.tmax.lower.corr.stack)

vlf.bm.tmax.stack <- cbind(vlf.bm.tmax.stack, vlf.tmax.upper.corr.stack[,"Upper"], vlf.tmax.lower.corr.stack[,"Lower"])
names(vlf.bm.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.bm.tmax.stack)

# Big Trees
vlf.big.tmax.stack <- stack(vlf.corr.tmax.big)
summary(vlf.big.tmax.stack)
names(vlf.big.tmax.stack) <- c("corr", "month")
vlf.big.tmax.stack$site <- as.factor("vlf")
vlf.big.tmax.stack$type <- as.factor("tmax")
vlf.big.tmax.stack$size <- as.factor("big")
summary(vlf.big.tmax.stack)

vlf.tmax.upper.big.corr.stack <- stack(vlf.tmax.upper.corr.big)
summary(vlf.tmax.upper.corr.stack)
names(vlf.tmax.upper.big.corr.stack) <- c("Upper", "month")
vlf.tmax.upper.big.corr.stack$site <- as.factor("vlf")
vlf.tmax.upper.big.corr.stack$type <- as.factor("tmax")
vlf.tmax.upper.big.corr.stack$size <- as.factor("big")
summary(vlf.tmax.upper.big.corr.stack)

vlf.tmax.lower.big.corr.stack <- stack(vlf.tmax.lower.corr.big)
summary(vlf.tmax.lower.big.corr.stack)
names(vlf.tmax.lower.big.corr.stack) <- c("Lower", "month")
vlf.tmax.lower.big.corr.stack$site <- as.factor("vlf")
vlf.tmax.lower.big.corr.stack$type <- as.factor("tmax")
vlf.tmax.lower.big.corr.stack$size <- as.factor("big")
summary(vlf.tmax.lower.big.corr.stack)

vlf.big.tmax.stack <- cbind(vlf.big.tmax.stack, vlf.tmax.upper.big.corr.stack[,"Upper"], vlf.tmax.lower.big.corr.stack[,"Lower"])
names(vlf.big.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.big.tmax.stack)


# Small Trees

vlf.small.tmax.stack <- stack(vlf.corr.tmax.small)
summary(vlf.small.tmax.stack)
names(vlf.small.tmax.stack) <- c("corr", "month")
vlf.small.tmax.stack$site <- as.factor("vlf")
vlf.small.tmax.stack$type <- as.factor("tmax")
vlf.small.tmax.stack$size <- as.factor("small")
summary(vlf.small.tmax.stack)

vlf.tmax.upper.small.corr.stack <- stack(vlf.tmax.upper.corr.small)
summary(vlf.tmax.upper.corr.stack)
names(vlf.tmax.upper.small.corr.stack) <- c("Upper", "month")
vlf.tmax.upper.small.corr.stack$site <- as.factor("vlf")
vlf.tmax.upper.small.corr.stack$type <- as.factor("tmax")
vlf.tmax.upper.small.corr.stack$size <- as.factor("small")
summary(vlf.tmax.upper.small.corr.stack)

vlf.tmax.lower.small.corr.stack <- stack(vlf.tmax.lower.corr.small)
summary(vlf.tmax.lower.small.corr.stack)
names(vlf.tmax.lower.small.corr.stack) <- c("Lower", "month")
vlf.tmax.lower.small.corr.stack$site <- as.factor("vlf")
vlf.tmax.lower.small.corr.stack$type <- as.factor("tmax")
vlf.tmax.lower.small.corr.stack$size <- as.factor("small")
summary(vlf.tmax.lower.small.corr.stack)

vlf.small.tmax.stack <- cbind(vlf.small.tmax.stack, vlf.tmax.upper.small.corr.stack[,"Upper"], vlf.tmax.lower.small.corr.stack[,"Lower"])
names(vlf.small.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.small.tmax.stack)

# Dated Trees

vlf.dated.tmax.stack <- stack(vlf.corr.tmax.dated)
summary(vlf.dated.tmax.stack)
names(vlf.dated.tmax.stack) <- c("corr", "month")
vlf.dated.tmax.stack$site <- as.factor("vlf")
vlf.dated.tmax.stack$type <- as.factor("tmax")
vlf.dated.tmax.stack$size <- as.factor("dated")
summary(vlf.dated.tmax.stack)

vlf.tmax.upper.dated.corr.stack <- stack(vlf.tmax.upper.corr.dated)
summary(vlf.tmax.upper.corr.stack)
names(vlf.tmax.upper.dated.corr.stack) <- c("Upper", "month")
vlf.tmax.upper.dated.corr.stack$site <- as.factor("vlf")
vlf.tmax.upper.dated.corr.stack$type <- as.factor("tmax")
vlf.tmax.upper.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmax.upper.dated.corr.stack)

vlf.tmax.lower.dated.corr.stack <- stack(vlf.tmax.lower.corr.dated)
summary(vlf.tmax.lower.dated.corr.stack)
names(vlf.tmax.lower.dated.corr.stack) <- c("Lower", "month")
vlf.tmax.lower.dated.corr.stack$site <- as.factor("vlf")
vlf.tmax.lower.dated.corr.stack$type <- as.factor("tmax")
vlf.tmax.lower.dated.corr.stack$size <- as.factor("dated")
summary(vlf.tmax.lower.dated.corr.stack)

vlf.dated.tmax.stack <- cbind(vlf.dated.tmax.stack, vlf.tmax.upper.dated.corr.stack[,"Upper"], vlf.tmax.lower.dated.corr.stack[,"Lower"])
names(vlf.dated.tmax.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.dated.tmax.stack)

#-----------------------------
# Precip
# VUF
# All Trees

vuf.bm.precip.stack <- stack(vuf.corr.precip.bm)
summary(vuf.bm.precip.stack)
names(vuf.bm.precip.stack) <- c("corr", "month")
vuf.bm.precip.stack$site <- as.factor("VUF")
vuf.bm.precip.stack$type <- as.factor("precip")
vuf.bm.precip.stack$size <- as.factor("all")
summary(vuf.bm.precip.stack)

vuf.precip.upper.corr.stack <- stack(vuf.precip.upper.corr)
summary(vuf.precip.upper.corr.stack)
names(vuf.precip.upper.corr.stack) <- c("Upper", "month")
vuf.precip.upper.corr.stack$site <- as.factor("VUF")
vuf.precip.upper.corr.stack$type <- as.factor("precip")
vuf.precip.upper.corr.stack$size <- as.factor("all")
summary(vuf.precip.upper.corr.stack)

vuf.precip.lower.corr.stack <- stack(vuf.precip.lower.corr)
summary(vuf.precip.lower.corr.stack)
names(vuf.precip.lower.corr.stack) <- c("Lower", "month")
vuf.precip.lower.corr.stack$site <- as.factor("VUF")
vuf.precip.lower.corr.stack$type <- as.factor("precip")
vuf.precip.lower.corr.stack$size <- as.factor("all")
summary(vuf.precip.lower.corr.stack)

vuf.bm.precip.stack <- cbind(vuf.bm.precip.stack, vuf.precip.upper.corr.stack[,"Upper"], vuf.precip.lower.corr.stack[,"Lower"])
names(vuf.bm.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.bm.precip.stack)

# Big Trees
vuf.big.precip.stack <- stack(vuf.corr.precip.big)
summary(vuf.big.precip.stack)
names(vuf.big.precip.stack) <- c("corr", "month")
vuf.big.precip.stack$site <- as.factor("VUF")
vuf.big.precip.stack$type <- as.factor("precip")
vuf.big.precip.stack$size <- as.factor("big")
summary(vuf.big.precip.stack)

vuf.precip.upper.big.corr.stack <- stack(vuf.precip.upper.corr.big)
summary(vuf.precip.upper.corr.stack)
names(vuf.precip.upper.big.corr.stack) <- c("Upper", "month")
vuf.precip.upper.big.corr.stack$site <- as.factor("VUF")
vuf.precip.upper.big.corr.stack$type <- as.factor("precip")
vuf.precip.upper.big.corr.stack$size <- as.factor("big")
summary(vuf.precip.upper.big.corr.stack)

vuf.precip.lower.big.corr.stack <- stack(vuf.precip.lower.corr.big)
summary(vuf.precip.lower.big.corr.stack)
names(vuf.precip.lower.big.corr.stack) <- c("Lower", "month")
vuf.precip.lower.big.corr.stack$site <- as.factor("VUF")
vuf.precip.lower.big.corr.stack$type <- as.factor("precip")
vuf.precip.lower.big.corr.stack$size <- as.factor("big")
summary(vuf.precip.lower.big.corr.stack)

vuf.big.precip.stack <- cbind(vuf.big.precip.stack, vuf.precip.upper.big.corr.stack[,"Upper"], vuf.precip.lower.big.corr.stack[,"Lower"])
names(vuf.big.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.big.precip.stack)


# Small Trees

vuf.small.precip.stack <- stack(vuf.corr.precip.small)
summary(vuf.small.precip.stack)
names(vuf.small.precip.stack) <- c("corr", "month")
vuf.small.precip.stack$site <- as.factor("VUF")
vuf.small.precip.stack$type <- as.factor("precip")
vuf.small.precip.stack$size <- as.factor("small")
summary(vuf.small.precip.stack)

vuf.precip.upper.small.corr.stack <- stack(vuf.precip.upper.corr.small)
summary(vuf.precip.upper.corr.stack)
names(vuf.precip.upper.small.corr.stack) <- c("Upper", "month")
vuf.precip.upper.small.corr.stack$site <- as.factor("VUF")
vuf.precip.upper.small.corr.stack$type <- as.factor("precip")
vuf.precip.upper.small.corr.stack$size <- as.factor("small")
summary(vuf.precip.upper.small.corr.stack)

vuf.precip.lower.small.corr.stack <- stack(vuf.precip.lower.corr.small)
summary(vuf.precip.lower.small.corr.stack)
names(vuf.precip.lower.small.corr.stack) <- c("Lower", "month")
vuf.precip.lower.small.corr.stack$site <- as.factor("VUF")
vuf.precip.lower.small.corr.stack$type <- as.factor("precip")
vuf.precip.lower.small.corr.stack$size <- as.factor("small")
summary(vuf.precip.lower.small.corr.stack)

vuf.small.precip.stack <- cbind(vuf.small.precip.stack, vuf.precip.upper.small.corr.stack[,"Upper"], vuf.precip.lower.small.corr.stack[,"Lower"])
names(vuf.small.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.small.precip.stack)

# Dated trees

vuf.dated.precip.stack <- stack(vuf.corr.precip.dated)
summary(vuf.dated.precip.stack)
names(vuf.dated.precip.stack) <- c("corr", "month")
vuf.dated.precip.stack$site <- as.factor("VUF")
vuf.dated.precip.stack$type <- as.factor("precip")
vuf.dated.precip.stack$size <- as.factor("dated")
summary(vuf.dated.precip.stack)

vuf.precip.upper.dated.corr.stack <- stack(vuf.precip.upper.corr.dated)
summary(vuf.precip.upper.corr.stack)
names(vuf.precip.upper.dated.corr.stack) <- c("Upper", "month")
vuf.precip.upper.dated.corr.stack$site <- as.factor("VUF")
vuf.precip.upper.dated.corr.stack$type <- as.factor("precip")
vuf.precip.upper.dated.corr.stack$size <- as.factor("dated")
summary(vuf.precip.upper.dated.corr.stack)

vuf.precip.lower.dated.corr.stack <- stack(vuf.precip.lower.corr.dated)
summary(vuf.precip.lower.dated.corr.stack)
names(vuf.precip.lower.dated.corr.stack) <- c("Lower", "month")
vuf.precip.lower.dated.corr.stack$site <- as.factor("VUF")
vuf.precip.lower.dated.corr.stack$type <- as.factor("precip")
vuf.precip.lower.dated.corr.stack$size <- as.factor("dated")
summary(vuf.precip.lower.dated.corr.stack)

vuf.dated.precip.stack <- cbind(vuf.dated.precip.stack, vuf.precip.upper.dated.corr.stack[,"Upper"], vuf.precip.lower.dated.corr.stack[,"Lower"])
names(vuf.dated.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vuf.dated.precip.stack)


# VLF
# All Trees

vlf.bm.precip.stack <- stack(vlf.corr.precip.bm)
summary(vlf.bm.precip.stack)
names(vlf.bm.precip.stack) <- c("corr", "month")
vlf.bm.precip.stack$site <- as.factor("vlf")
vlf.bm.precip.stack$type <- as.factor("precip")
vlf.bm.precip.stack$size <- as.factor("all")
summary(vlf.bm.precip.stack)

vlf.precip.upper.corr.stack <- stack(vlf.precip.upper.corr)
summary(vlf.precip.upper.corr.stack)
names(vlf.precip.upper.corr.stack) <- c("Upper", "month")
vlf.precip.upper.corr.stack$site <- as.factor("vlf")
vlf.precip.upper.corr.stack$type <- as.factor("precip")
vlf.precip.upper.corr.stack$size <- as.factor("all")
summary(vlf.precip.upper.corr.stack)

vlf.precip.lower.corr.stack <- stack(vlf.precip.lower.corr)
summary(vlf.precip.lower.corr.stack)
names(vlf.precip.lower.corr.stack) <- c("Lower", "month")
vlf.precip.lower.corr.stack$site <- as.factor("vlf")
vlf.precip.lower.corr.stack$type <- as.factor("precip")
vlf.precip.lower.corr.stack$size <- as.factor("all")
summary(vlf.precip.lower.corr.stack)

vlf.bm.precip.stack <- cbind(vlf.bm.precip.stack, vlf.precip.upper.corr.stack[,"Upper"], vlf.precip.lower.corr.stack[,"Lower"])
names(vlf.bm.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.bm.precip.stack)

# Big Trees
vlf.big.precip.stack <- stack(vlf.corr.precip.big)
summary(vlf.big.precip.stack)
names(vlf.big.precip.stack) <- c("corr", "month")
vlf.big.precip.stack$site <- as.factor("vlf")
vlf.big.precip.stack$type <- as.factor("precip")
vlf.big.precip.stack$size <- as.factor("big")
summary(vlf.big.precip.stack)

vlf.precip.upper.big.corr.stack <- stack(vlf.precip.upper.corr.big)
summary(vlf.precip.upper.corr.stack)
names(vlf.precip.upper.big.corr.stack) <- c("Upper", "month")
vlf.precip.upper.big.corr.stack$site <- as.factor("vlf")
vlf.precip.upper.big.corr.stack$type <- as.factor("precip")
vlf.precip.upper.big.corr.stack$size <- as.factor("big")
summary(vlf.precip.upper.big.corr.stack)

vlf.precip.lower.big.corr.stack <- stack(vlf.precip.lower.corr.big)
summary(vlf.precip.lower.big.corr.stack)
names(vlf.precip.lower.big.corr.stack) <- c("Lower", "month")
vlf.precip.lower.big.corr.stack$site <- as.factor("vlf")
vlf.precip.lower.big.corr.stack$type <- as.factor("precip")
vlf.precip.lower.big.corr.stack$size <- as.factor("big")
summary(vlf.precip.lower.big.corr.stack)

vlf.big.precip.stack <- cbind(vlf.big.precip.stack, vlf.precip.upper.big.corr.stack[,"Upper"], vlf.precip.lower.big.corr.stack[,"Lower"])
names(vlf.big.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.big.precip.stack)


# Small Trees

vlf.small.precip.stack <- stack(vlf.corr.precip.small)
summary(vlf.small.precip.stack)
names(vlf.small.precip.stack) <- c("corr", "month")
vlf.small.precip.stack$site <- as.factor("vlf")
vlf.small.precip.stack$type <- as.factor("precip")
vlf.small.precip.stack$size <- as.factor("small")
summary(vlf.small.precip.stack)

vlf.precip.upper.small.corr.stack <- stack(vlf.precip.upper.corr.small)
summary(vlf.precip.upper.corr.stack)
names(vlf.precip.upper.small.corr.stack) <- c("Upper", "month")
vlf.precip.upper.small.corr.stack$site <- as.factor("vlf")
vlf.precip.upper.small.corr.stack$type <- as.factor("precip")
vlf.precip.upper.small.corr.stack$size <- as.factor("small")
summary(vlf.precip.upper.small.corr.stack)

vlf.precip.lower.small.corr.stack <- stack(vlf.precip.lower.corr.small)
summary(vlf.precip.lower.small.corr.stack)
names(vlf.precip.lower.small.corr.stack) <- c("Lower", "month")
vlf.precip.lower.small.corr.stack$site <- as.factor("vlf")
vlf.precip.lower.small.corr.stack$type <- as.factor("precip")
vlf.precip.lower.small.corr.stack$size <- as.factor("small")
summary(vlf.precip.lower.small.corr.stack)

vlf.small.precip.stack <- cbind(vlf.small.precip.stack, vlf.precip.upper.small.corr.stack[,"Upper"], vlf.precip.lower.small.corr.stack[,"Lower"])
names(vlf.small.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.small.precip.stack)

# Dated Trees

vlf.dated.precip.stack <- stack(vlf.corr.precip.dated)
summary(vlf.dated.precip.stack)
names(vlf.dated.precip.stack) <- c("corr", "month")
vlf.dated.precip.stack$site <- as.factor("vlf")
vlf.dated.precip.stack$type <- as.factor("precip")
vlf.dated.precip.stack$size <- as.factor("dated")
summary(vlf.dated.precip.stack)

vlf.precip.upper.dated.corr.stack <- stack(vlf.precip.upper.corr.dated)
summary(vlf.precip.upper.corr.stack)
names(vlf.precip.upper.dated.corr.stack) <- c("Upper", "month")
vlf.precip.upper.dated.corr.stack$site <- as.factor("vlf")
vlf.precip.upper.dated.corr.stack$type <- as.factor("precip")
vlf.precip.upper.dated.corr.stack$size <- as.factor("dated")
summary(vlf.precip.upper.dated.corr.stack)

vlf.precip.lower.dated.corr.stack <- stack(vlf.precip.lower.corr.dated)
summary(vlf.precip.lower.dated.corr.stack)
names(vlf.precip.lower.dated.corr.stack) <- c("Lower", "month")
vlf.precip.lower.dated.corr.stack$site <- as.factor("vlf")
vlf.precip.lower.dated.corr.stack$type <- as.factor("precip")
vlf.precip.lower.dated.corr.stack$size <- as.factor("dated")
summary(vlf.precip.lower.dated.corr.stack)

vlf.dated.precip.stack <- cbind(vlf.dated.precip.stack, vlf.precip.upper.dated.corr.stack[,"Upper"], vlf.precip.lower.dated.corr.stack[,"Lower"])
names(vlf.dated.precip.stack)<- c("corr", "month", "site", "type", "size", "upper", "lower")

summary(vlf.dated.precip.stack)

# # PDSI

# vuf.bm.pdsi.stack <- stack(vuf.corr.pdsi.bm)
# summary(vuf.bm.pdsi.stack)
# names(vuf.bm.pdsi.stack) <- c("corr", "month")
# vuf.bm.pdsi.stack$site <- as.factor("VUF")
# vuf.bm.pdsi.stack$type <- as.factor("pdsi")
# summary(vuf.bm.pdsi.stack)

# vuf.pdsi.mean.corr.stack <- stack(vuf.pdsi.mean.corr)
# summary(vuf.pdsi.mean.corr.stack)
# names(vuf.pdsi.mean.corr.stack) <- c("mean.corr", "month")
# vuf.pdsi.mean.corr.stack$site <- as.factor("VUF")
# vuf.pdsi.mean.corr.stack$type <- as.factor("pdsi")
# summary(vuf.pdsi.mean.corr.stack)

# vuf.bm.pdsi.stack <- merge(vuf.bm.pdsi.stack, vuf.pdsi.mean.corr.stack, all.x=T, all.y=T)
# summary(vuf.bm.pdsi.stack)

# vlf.bm.pdsi.stack <- stack(vlf.corr.pdsi.bm)
# summary(vlf.bm.pdsi.stack)
# names(vlf.bm.pdsi.stack) <- c("corr", "month")
# vlf.bm.pdsi.stack$site <- as.factor("VLF")
# vlf.bm.pdsi.stack$type <- as.factor("pdsi")
# summary(vlf.bm.pdsi.stack)

# vlf.pdsi.mean.corr.stack <- stack(vlf.pdsi.mean.corr)
# summary(vlf.pdsi.mean.corr.stack)
# names(vlf.pdsi.mean.corr.stack) <- c("mean.corr", "month")
# vlf.pdsi.mean.corr.stack$site <- as.factor("VLF")
# vlf.pdsi.mean.corr.stack$type <- as.factor("pdsi")
# summary(vlf.pdsi.mean.corr.stack)

# vlf.bm.pdsi.stack <- merge(vlf.bm.pdsi.stack, vlf.pdsi.mean.corr.stack, all.x=T, all.y=T)
# summary(vlf.bm.pdsi.stack)


all.valles.bm.stack <- rbind(vuf.bm.tmean.stack, vlf.bm.tmean.stack, vuf.bm.tmin.stack, vlf.bm.tmin.stack, vuf.bm.tmax.stack, vlf.bm.tmax.stack, vuf.bm.precip.stack, vlf.bm.precip.stack,
	
	vuf.big.tmean.stack, vlf.big.tmean.stack, vuf.big.tmin.stack, vlf.big.tmin.stack, vuf.big.tmax.stack, vlf.big.tmax.stack, vuf.big.precip.stack, vlf.big.precip.stack,
	
	vuf.small.tmean.stack, vlf.small.tmean.stack, vuf.small.tmin.stack, vlf.small.tmin.stack, vuf.small.tmax.stack, vlf.small.tmax.stack, vuf.small.precip.stack, vlf.small.precip.stack,
	
	vuf.dated.tmean.stack, vlf.dated.tmean.stack, vuf.dated.tmin.stack, vlf.dated.tmin.stack, vuf.dated.tmax.stack, vlf.dated.tmax.stack, vuf.dated.precip.stack, vlf.dated.precip.stack
	)
	
	
summary(all.valles.bm.stack)
summary(all.valles.bm.stack$month)

# Setting the factoring so that it will display the  months correctly when we Facet
all.valles.bm.stack$month <- factor(all.valles.bm.stack$month, levels = names(vuf.corr.tmean.bm))
# summary(all.valles.bm.stack$month)


# Adding a significance column so that sig. corrs. will pop
# Used Sidak correction Which asserts independence to come up wiht the new crit. value 0.559
# all.valles.bm.stack$sig <- ifelse(all.valles.bm.stack$mean.corr >=0.559 | all.valles.bm.stack$mean.corr<= -0.559, "Y", "N")

# Edit: Don't think we need to do corrections for multiple comparisons
all.valles.bm.stack$sig <- ifelse(all.valles.bm.stack$upper <= 0 | all.valles.bm.stack$lower >= 0, "Y", "N") 

# all.valles.bm.stack$sig <- ifelse(all.valles.bm.stack$mean.corr >=0.559 | all.valles.bm.stack$mean.corr<= -0.559, "Sidak corrected", ifelse(all.valles.bm.stack$mean.corr >= 0.374 & all.valles.bm.stack$mean.corr <= 0.559| all.valles.bm.stack$mean.corr<=-0.374 & all.valles.bm.stack$mean.corr >= -0.559, "Uncorrected", "Not sig.")) 

all.valles.bm.stack$sig <- factor(all.valles.bm.stack$sig, levels = c("Y", "N"))
levels(all.valles.bm.stack$sig)

# all.valles.bm.stack$sig <- factor(all.valles.bm.stack$sig, levels = c("Sidak corrected", "Uncorrected", "Not sig."))
# levels(all.valles.bm.stack$sig)

all.valles.bm.stack$elevation <- (ifelse(all.valles.bm.stack$site=="VUF" , "Upper elevation", "Lower elevation"))

all.valles.bm.stack$elevation <- factor(all.valles.bm.stack$elevation, levels = c("Upper elevation", "Lower elevation"))

all.valles.bm.stack$size <- factor(all.valles.bm.stack$size, levels= c("big", "small", "all", "dated"))

save(all.valles.bm.stack, file="processed_data/valles_BM_distrib_corr_data.Rdata")

#######################################################
# Plotting BM correlations
#######################################################
library(ggplot2)
summary(all.valles.bm.stack)

# Critical Value for 28 years (n-2 = 26) 0.330

pdf("figures/BMI_boxplot_seasons.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pFall", "Winter","Spring", "Summer"),]) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_boxplot(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Biomass Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)")))) #+
    # guides(fill=guide_legend(override.aes=list(alpha=0.15))) # +
# #  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  # theme(legend.position=c(0.2,0.85)) + 

  # # General Plot formatting
  # theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  # theme(strip.text=element_text(size=rel(1.5), face="bold"))

	
dev.off()	

pdf("figures/BMI_boxplot_all_months.pdf", width=13, height=8.5)	
ggplot(data=all.valles.bm.stack) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_boxplot(aes(x=month, y=corr, fill=sig)) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +

	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Biomass Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)")))) # +
    # guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
# #  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  # theme(legend.position=c(0.2,0.85)) + 

  # # General Plot formatting
  # theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=45, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  # theme(strip.text=element_text(size=rel(1.5), face="bold"))

dev.off()

pdf("figures/BMI_violin_seasons.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pFall", "Winter","Spring", "Summer"),]) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig), adjust=2.5) +
	stat_summary(aes(x=month, y=corr), fun.y="median", geom="point", shape="-", size=20) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	
	 labs(title= "Biomass Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))#+


dev.off()

pdf("figures/BMI_violin_seasons_big_small.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pFall", "Winter","Spring", "Summer") & all.valles.bm.stack$type %in% c("tmean", "precip"),]) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, color=size), adjust=2.5) +
	geom_violin(aes(x=month, y=corr, color=size), adjust=2.5) +
	geom_violin(aes(x=month, y=corr, fill=size, alpha=sig), adjust=2.5) +
	
	
	stat_summary(aes(x=month, y=corr, mapping = size), fun.y="median", geom="point", shape="-", size=20, position=position_dodge(width = 0.9)) +
	
	
	scale_color_manual(values=c("blue", "red", "darkgreen", "purple")) +
	scale_fill_manual(values=c("blue", "red", "darkgreen", "purple")) +
	scale_alpha_manual(values = c(1, 0.4))+
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	
	 labs(title= "Biomass Climate Correlations", x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))#+


dev.off()

geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type, fill=chron.type, alpha=sig), stat="identity", position="dodge") + 



pdf("figures/BMI_violin_all_months.pdf", width=13, height=8.5)	
ggplot(data=all.valles.bm.stack) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig), adjust=2.5) +
	stat_summary(aes(x=month, y=corr), fun.y="median", geom="point", shape="-", size=10) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Biomass Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()	






