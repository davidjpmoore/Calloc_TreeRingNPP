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


lower.res <- valles.res[,c("vlf.res", "cat.res", "vlf.base", "chg.res", "vlf.mean.res", "VLB.res", "VLS.res")]
row.names(lower.res)<- row.names(valles.res)
summary(lower.res)
save(lower.res, file="processed_data/lower_chron_combined.Rdata")
# Load in climate data

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
vuf.tmean.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res"))
vuf.tmean.stack$type <- as.factor("tmean")
vuf.tmean.stack$elevation <- as.factor("Upper")

vuf.tmin.stack <- stack(vuf.corr.tmin)
summary(vuf.tmin.stack)
names(vuf.tmin.stack) <- c("corr", "month")
vuf.tmin.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res"))
vuf.tmin.stack$type <- as.factor("tmin")
vuf.tmin.stack$elevation <- as.factor("Upper")

vuf.tmax.stack <- stack(vuf.corr.tmax)
summary(vuf.tmax.stack)
names(vuf.tmax.stack) <- c("corr", "month")
vuf.tmax.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res"))
vuf.tmax.stack$type <- as.factor("tmax")
vuf.tmax.stack$elevation <- as.factor("Upper")

vuf.precip.stack <- stack(vuf.corr.precip)
summary(vuf.precip.stack)
names(vuf.precip.stack) <- c("corr", "month")
vuf.precip.stack$chron <- as.factor(c("vuf.res", "bcw.res", "vuf.base", "vuf.mean.res", "VUB.res", "VUS.res"))
vuf.precip.stack$type <- as.factor("precip")
vuf.precip.stack$elevation <- as.factor("Upper")

# Lower Site
vlf.tmean.stack <- stack(vlf.corr.tmean)
summary(vlf.tmean.stack)
names(vlf.tmean.stack) <- c("corr", "month")
vlf.tmean.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res"))
vlf.tmean.stack$type <- as.factor("tmean")
vlf.tmean.stack$elevation <- as.factor("Lower")

vlf.tmin.stack <- stack(vlf.corr.tmin)
summary(vlf.tmin.stack)
names(vlf.tmin.stack) <- c("corr", "month")
vlf.tmin.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res"))
vlf.tmin.stack$type <- as.factor("tmin")
vlf.tmin.stack$elevation <- as.factor("Lower")

vlf.tmax.stack <- stack(vlf.corr.tmax)
summary(vlf.tmax.stack)
names(vlf.tmax.stack) <- c("corr", "month")
vlf.tmax.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res"))
vlf.tmax.stack$type <- as.factor("tmax")
vlf.tmax.stack$elevation <- as.factor("Lower")

vlf.precip.stack <- stack(vlf.corr.precip)
summary(vlf.precip.stack)
names(vlf.precip.stack) <- c("corr", "month")
vlf.precip.stack$chron <- as.factor(c("vlf.res", "cat.res","chg.res", "vlf.base", "vlf.mean.res", "VLB.res", "VLS.res"))
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

all.valles.climate.stack$chron <- recode(all.valles.climate.stack$chron, "'vuf.res' = 'Upper Ecology'; 'vlf.res' = 'Lower Ecology'; 'vuf.mean.res' = 'Upper Ecology Mean'; 'vlf.mean.res' = 'Lower Ecology Mean'; 'vuf.base'= 'Upper BM'; 'vlf.base' = 'Lower BM'; 'bcw.res' = 'Upper Climate'; 'cat.res' = 'Lower Climate1'; 'chg.res' = 'Lower Climate2'; 'VUB.res' = 'Upper Big'; 'VUS.res' = 'Upper Small'; 'VLB.res' = 'Lower Big'; 'VLS.res' = 'Lower Small'   
")

all.valles.climate.stack$chron <- factor(all.valles.climate.stack$chron, levels = c("Upper Ecology", "Upper Big", "Upper Small", "Upper Ecology Mean", "Upper BM", "Upper Climate", "Lower Ecology", "Lower Big", "Lower Small","Lower Ecology Mean", "Lower BM", "Lower Climate1", "Lower Climate2"))

all.valles.climate.stack$chron.type <- ifelse(all.valles.climate.stack$chron =="Upper Ecology", "Ecology",
											ifelse(all.valles.climate.stack$chron== "Upper Big", "Big",
											ifelse(all.valles.climate.stack$chron== "Upper Small", "Small",
											ifelse(all.valles.climate.stack$chron== "Upper Ecology Mean", "Mean",
											ifelse(all.valles.climate.stack$chron== "Upper BM", "BM",
											ifelse(all.valles.climate.stack$chron== "Upper Climate", "Climate",
											ifelse(all.valles.climate.stack$chron =="Lower Ecology", "Ecology",
											ifelse(all.valles.climate.stack$chron== "Lower Big", "Big",
											ifelse(all.valles.climate.stack$chron== "Lower Small", "Small",
											ifelse(all.valles.climate.stack$chron== "Lower Ecology Mean", "Mean",
											ifelse(all.valles.climate.stack$chron== "Lower BM", "BM",
											ifelse(all.valles.climate.stack$chron== "Lower Climate1", "Climate",  												
											all.valles.climate.stack$chron)))))))))))) 

all.valles.climate.stack$chron.type <- as.factor(all.valles.climate.stack$chron.type)
summary(all.valles.climate.stack$chron.type)

all.valles.climate.stack$chron.type <- factor(all.valles.climate.stack$chron.type, levels = c("Ecology", "Big", "Small", "BM", "Climate"))

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


save(all.valles.climate.stack, file="processed_data/valles_climate_plus_chron_stack.rdata")

# all.valles.climate.stack$elevation <- (ifelse(all.valles.climate.stack$chron=="vuf.res" | all.valles.climate.stack$chron=="bcw.res", "upper elevation", "lower elevation"))

# all.valles.climate.stack$elevation <- factor(all.valles.climate.stack$elevation, levels = c("upper elevation", "lower elevation"))

#######################################################
# Plotting Correlations with climate chronologies
#######################################################
library(ggplot2)
 summary(all.valles.climate.stack)

# Removing arithemetic mean chronology
all.valles.climate.stack.short <- all.valles.climate.stack[all.valles.climate.stack$chron %in% c("Upper Ecology", "Upper Big", "Upper Small","Upper BM", "Upper Climate", "Lower Ecology","Lower BM", "Lower Climate1", "Lower Big", "Lower Small"),]

all.valles.climate.stack.short <- all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "pFall", "Winter", "Spring", "Summer"),]
 
summary(all.valles.climate.stack.short)

test <- all.valles.climate.stack[all.valles.climate.stack$month %in% c("pDJF", "MAM", "JJA", "SON"),]
summary(test)

levels(all.valles.climate.stack$sig)

chron.col <- read.csv("chron_colors.csv", header=T)
summary(chron.col)
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
	
	labs(title= "Tree Ring : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()



pdf("figures/climate_chron_seasons_together.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pFall", "Winter", "Spring", "Summer"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
	geom_bar(aes(x=month, y=corr, fill=chron.type), stat="identity", position="dodge", colour="black") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "blue", "red", "darkgreen", "dodgerblue")) +
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
load("processed_data/vuf_bm_boot_tot_inc.Rdata")
load("processed_data/vlf_bm_boot_tot_inc.Rdata")

summary(vuf.inc.tot)
summary(vlf.inc.tot)


# subsetting to the extent of the previous timeseries
vuf.bm <- vuf.inc.tot[row.names(vuf.inc.tot)>=1980 & row.names(vuf.inc.tot)<=2007,]
vlf.bm <- vlf.inc.tot[row.names(vlf.inc.tot)>=1980 & row.names(vlf.inc.tot)<=2007,]

head(vuf.bm)
head(vlf.bm)

vuf.bm <- vuf.bm[order(row.names(vuf.bm), decreasing=F),]
vlf.bm <- vlf.bm[order(row.names(vlf.bm), decreasing=F),]

head(vuf.bm)
head(vlf.bm)

# ---------------------------------------
# Climate Correlations with BM arrays
# ---------------------------------------

#------------------------------
# Tmean
#------------------------------

## VUF

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
for(j in 1:ncol(vuf.corr.tmean.bm)){
	vuf.tmean.upper.corr[,j] <- quantile(vuf.corr.tmean.bm[,j],0.975, na.rm=T)
	vuf.tmean.lower.corr[,j] <- quantile(vuf.corr.tmean.bm[,j],0.025, na.rm=T)
}
head(vuf.tmean.upper.corr)
head(vuf.tmean.lower.corr)

## VLF

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


#------------------------------
# Precipitation
#------------------------------

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
for(j in 1:ncol(vuf.corr.precip.bm)){
	vuf.precip.upper.corr[,j] <- quantile(vuf.corr.precip.bm[,j],0.975, na.rm=T)
	vuf.precip.lower.corr[,j] <- quantile(vuf.corr.precip.bm[,j],0.025, na.rm=T)
}
head(vuf.precip.upper.corr)
head(vuf.precip.lower.corr)

## VLF

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


#------------------------------
# T min
#------------------------------

## VUF
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
for(j in 1:ncol(vuf.corr.tmin.bm)){
	vuf.tmin.upper.corr[,j] <- quantile(vuf.corr.tmin.bm[,j],0.975, na.rm=T)
	vuf.tmin.lower.corr[,j] <- quantile(vuf.corr.tmin.bm[,j],0.025, na.rm=T)
}
head(vuf.tmin.upper.corr)
head(vuf.tmin.lower.corr)

## VLF

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

#------------------------------
# Tmax
#------------------------------

## VUF

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
for(j in 1:ncol(vuf.corr.tmax.bm)){
	vuf.tmax.upper.corr[,j] <- quantile(vuf.corr.tmax.bm[,j],0.975, na.rm=T)
	vuf.tmax.lower.corr[,j] <- quantile(vuf.corr.tmax.bm[,j],0.025, na.rm=T)
}
head(vuf.tmax.upper.corr)
head(vuf.tmax.lower.corr)

## VLF

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
vuf.bm.tmean.stack <- stack(vuf.corr.tmean.bm)
summary(vuf.bm.tmean.stack)
names(vuf.bm.tmean.stack) <- c("corr", "month")
vuf.bm.tmean.stack$site <- as.factor("VUF")
vuf.bm.tmean.stack$type <- as.factor("tmean")
summary(vuf.bm.tmean.stack)

vuf.tmean.upper.corr.stack <- stack(vuf.tmean.upper.corr)
summary(vuf.tmean.upper.corr.stack)
names(vuf.tmean.upper.corr.stack) <- c("Upper", "month")
vuf.tmean.upper.corr.stack$site <- as.factor("VUF")
vuf.tmean.upper.corr.stack$type <- as.factor("tmean")
summary(vuf.tmean.upper.corr.stack)

vuf.tmean.lower.corr.stack <- stack(vuf.tmean.lower.corr)
summary(vuf.tmean.lower.corr.stack)
names(vuf.tmean.lower.corr.stack) <- c("Lower", "month")
vuf.tmean.lower.corr.stack$site <- as.factor("VUF")
vuf.tmean.lower.corr.stack$type <- as.factor("tmean")
summary(vuf.tmean.lower.corr.stack)

vuf.bm.tmean.stack <- cbind(vuf.bm.tmean.stack, vuf.tmean.upper.corr.stack[,"Upper"], vuf.tmean.lower.corr.stack[,"Lower"])
names(vuf.bm.tmean.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vuf.bm.tmean.stack)

vlf.bm.tmean.stack <- stack(vlf.corr.tmean.bm)
summary(vlf.bm.tmean.stack)
names(vlf.bm.tmean.stack) <- c("corr", "month")
vlf.bm.tmean.stack$site <- as.factor("vlf")
vlf.bm.tmean.stack$type <- as.factor("tmean")
summary(vlf.bm.tmean.stack)

vlf.tmean.upper.corr.stack <- stack(vlf.tmean.upper.corr)
summary(vlf.tmean.upper.corr.stack)
names(vlf.tmean.upper.corr.stack) <- c("Upper", "month")
vlf.tmean.upper.corr.stack$site <- as.factor("vlf")
vlf.tmean.upper.corr.stack$type <- as.factor("tmean")
summary(vlf.tmean.upper.corr.stack)

vlf.tmean.lower.corr.stack <- stack(vlf.tmean.lower.corr)
summary(vlf.tmean.lower.corr.stack)
names(vlf.tmean.lower.corr.stack) <- c("Lower", "month")
vlf.tmean.lower.corr.stack$site <- as.factor("vlf")
vlf.tmean.lower.corr.stack$type <- as.factor("tmean")
summary(vlf.tmean.lower.corr.stack)

vlf.bm.tmean.stack <- cbind(vlf.bm.tmean.stack, vlf.tmean.upper.corr.stack[,"Upper"], vlf.tmean.lower.corr.stack[,"Lower"])
names(vlf.bm.tmean.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vlf.bm.tmean.stack)



# Tmin
vuf.bm.tmin.stack <- stack(vuf.corr.tmin.bm)
summary(vuf.bm.tmin.stack)
names(vuf.bm.tmin.stack) <- c("corr", "month")
vuf.bm.tmin.stack$site <- as.factor("VUF")
vuf.bm.tmin.stack$type <- as.factor("tmin")
summary(vuf.bm.tmin.stack)

vuf.tmin.upper.corr.stack <- stack(vuf.tmin.upper.corr)
summary(vuf.tmin.upper.corr.stack)
names(vuf.tmin.upper.corr.stack) <- c("Upper", "month")
vuf.tmin.upper.corr.stack$site <- as.factor("VUF")
vuf.tmin.upper.corr.stack$type <- as.factor("tmin")
summary(vuf.tmin.upper.corr.stack)

vuf.tmin.lower.corr.stack <- stack(vuf.tmin.lower.corr)
summary(vuf.tmin.lower.corr.stack)
names(vuf.tmin.lower.corr.stack) <- c("Lower", "month")
vuf.tmin.lower.corr.stack$site <- as.factor("VUF")
vuf.tmin.lower.corr.stack$type <- as.factor("tmin")
summary(vuf.tmin.lower.corr.stack)

vuf.bm.tmin.stack <- cbind(vuf.bm.tmin.stack, vuf.tmin.upper.corr.stack[,"Upper"], vuf.tmin.lower.corr.stack[,"Lower"])
names(vuf.bm.tmin.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vuf.bm.tmin.stack)


vlf.bm.tmin.stack <- stack(vlf.corr.tmin.bm)
summary(vlf.bm.tmin.stack)
names(vlf.bm.tmin.stack) <- c("corr", "month")
vlf.bm.tmin.stack$site <- as.factor("vlf")
vlf.bm.tmin.stack$type <- as.factor("tmin")
summary(vlf.bm.tmin.stack)

vlf.tmin.upper.corr.stack <- stack(vlf.tmin.upper.corr)
summary(vlf.tmin.upper.corr.stack)
names(vlf.tmin.upper.corr.stack) <- c("Upper", "month")
vlf.tmin.upper.corr.stack$site <- as.factor("vlf")
vlf.tmin.upper.corr.stack$type <- as.factor("tmin")
summary(vlf.tmin.upper.corr.stack)

vlf.tmin.lower.corr.stack <- stack(vlf.tmin.lower.corr)
summary(vlf.tmin.lower.corr.stack)
names(vlf.tmin.lower.corr.stack) <- c("Lower", "month")
vlf.tmin.lower.corr.stack$site <- as.factor("vlf")
vlf.tmin.lower.corr.stack$type <- as.factor("tmin")
summary(vlf.tmin.lower.corr.stack)

vlf.bm.tmin.stack <- cbind(vlf.bm.tmin.stack, vlf.tmin.upper.corr.stack[,"Upper"], vlf.tmin.lower.corr.stack[,"Lower"])
names(vlf.bm.tmin.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vlf.bm.tmin.stack)


# Tmax
vuf.bm.tmax.stack <- stack(vuf.corr.tmax.bm)
summary(vuf.bm.tmax.stack)
names(vuf.bm.tmax.stack) <- c("corr", "month")
vuf.bm.tmax.stack$site <- as.factor("VUF")
vuf.bm.tmax.stack$type <- as.factor("tmax")
summary(vuf.bm.tmax.stack)

vuf.tmax.upper.corr.stack <- stack(vuf.tmax.upper.corr)
summary(vuf.tmax.upper.corr.stack)
names(vuf.tmax.upper.corr.stack) <- c("Upper", "month")
vuf.tmax.upper.corr.stack$site <- as.factor("VUF")
vuf.tmax.upper.corr.stack$type <- as.factor("tmax")
summary(vuf.tmax.upper.corr.stack)

vuf.tmax.lower.corr.stack <- stack(vuf.tmax.lower.corr)
summary(vuf.tmax.lower.corr.stack)
names(vuf.tmax.lower.corr.stack) <- c("Lower", "month")
vuf.tmax.lower.corr.stack$site <- as.factor("VUF")
vuf.tmax.lower.corr.stack$type <- as.factor("tmax")
summary(vuf.tmax.lower.corr.stack)

vuf.bm.tmax.stack <- cbind(vuf.bm.tmax.stack, vuf.tmax.upper.corr.stack[,"Upper"], vuf.tmax.lower.corr.stack[,"Lower"])
names(vuf.bm.tmax.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vuf.bm.tmax.stack)

vlf.bm.tmax.stack <- stack(vlf.corr.tmax.bm)
summary(vlf.bm.tmax.stack)
names(vlf.bm.tmax.stack) <- c("corr", "month")
vlf.bm.tmax.stack$site <- as.factor("vlf")
vlf.bm.tmax.stack$type <- as.factor("tmax")
summary(vlf.bm.tmax.stack)

vlf.tmax.upper.corr.stack <- stack(vlf.tmax.upper.corr)
summary(vlf.tmax.upper.corr.stack)
names(vlf.tmax.upper.corr.stack) <- c("Upper", "month")
vlf.tmax.upper.corr.stack$site <- as.factor("vlf")
vlf.tmax.upper.corr.stack$type <- as.factor("tmax")
summary(vlf.tmax.upper.corr.stack)

vlf.tmax.lower.corr.stack <- stack(vlf.tmax.lower.corr)
summary(vlf.tmax.lower.corr.stack)
names(vlf.tmax.lower.corr.stack) <- c("Lower", "month")
vlf.tmax.lower.corr.stack$site <- as.factor("vlf")
vlf.tmax.lower.corr.stack$type <- as.factor("tmax")
summary(vlf.tmax.lower.corr.stack)

vlf.bm.tmax.stack <- cbind(vlf.bm.tmax.stack, vlf.tmax.upper.corr.stack[,"Upper"], vlf.tmax.lower.corr.stack[,"Lower"])
names(vlf.bm.tmax.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vlf.bm.tmax.stack)

# Precip
vuf.bm.precip.stack <- stack(vuf.corr.precip.bm)
summary(vuf.bm.precip.stack)
names(vuf.bm.precip.stack) <- c("corr", "month")
vuf.bm.precip.stack$site <- as.factor("VUF")
vuf.bm.precip.stack$type <- as.factor("precip")
summary(vuf.bm.precip.stack)

vuf.precip.upper.corr.stack <- stack(vuf.precip.upper.corr)
summary(vuf.precip.upper.corr.stack)
names(vuf.precip.upper.corr.stack) <- c("Upper", "month")
vuf.precip.upper.corr.stack$site <- as.factor("VUF")
vuf.precip.upper.corr.stack$type <- as.factor("precip")
summary(vuf.precip.upper.corr.stack)

vuf.precip.lower.corr.stack <- stack(vuf.precip.lower.corr)
summary(vuf.precip.lower.corr.stack)
names(vuf.precip.lower.corr.stack) <- c("Lower", "month")
vuf.precip.lower.corr.stack$site <- as.factor("VUF")
vuf.precip.lower.corr.stack$type <- as.factor("precip")
summary(vuf.precip.lower.corr.stack)

vuf.bm.precip.stack <- cbind(vuf.bm.precip.stack, vuf.precip.upper.corr.stack[,"Upper"], vuf.precip.lower.corr.stack[,"Lower"])
names(vuf.bm.precip.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vuf.bm.precip.stack)

vlf.bm.precip.stack <- stack(vlf.corr.precip.bm)
summary(vlf.bm.precip.stack)
names(vlf.bm.precip.stack) <- c("corr", "month")
vlf.bm.precip.stack$site <- as.factor("vlf")
vlf.bm.precip.stack$type <- as.factor("precip")
summary(vlf.bm.precip.stack)

vlf.precip.upper.corr.stack <- stack(vlf.precip.upper.corr)
summary(vlf.precip.upper.corr.stack)
names(vlf.precip.upper.corr.stack) <- c("Upper", "month")
vlf.precip.upper.corr.stack$site <- as.factor("vlf")
vlf.precip.upper.corr.stack$type <- as.factor("precip")
summary(vlf.precip.upper.corr.stack)

vlf.precip.lower.corr.stack <- stack(vlf.precip.lower.corr)
summary(vlf.precip.lower.corr.stack)
names(vlf.precip.lower.corr.stack) <- c("Lower", "month")
vlf.precip.lower.corr.stack$site <- as.factor("vlf")
vlf.precip.lower.corr.stack$type <- as.factor("precip")
summary(vlf.precip.lower.corr.stack)

vlf.bm.precip.stack <- cbind(vlf.bm.precip.stack, vlf.precip.upper.corr.stack[,"Upper"], vlf.precip.lower.corr.stack[,"Lower"])
names(vlf.bm.precip.stack)<- c("corr", "month", "site", "type", "upper", "lower")

summary(vlf.bm.precip.stack)

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


all.valles.bm.stack <- rbind(vuf.bm.tmean.stack, vlf.bm.tmean.stack, vuf.bm.tmin.stack, vlf.bm.tmin.stack, vuf.bm.tmax.stack, vlf.bm.tmax.stack, vuf.bm.precip.stack, vlf.bm.precip.stack)
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
	geom_hline(yintercept=0, linetype="dashed") +
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
	geom_hline(yintercept=0, linetype="dashed") +
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
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	
	 labs(title= "Biomass Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)")))) #+
     #guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
# #  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  # theme(legend.position=c(0.2,0.85)) + 

  # General Plot formatting
  # theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=45, color="black", size=rel(1.5), hjust= 1), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5, angle = 45),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  # theme(strip.text=element_text(size=rel(1.5), face="bold"))

dev.off()

pdf("figures/BMI_violin_all_months.pdf", width=13, height=8.5)	
ggplot(data=all.valles.bm.stack) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig), adjust=2.5) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	
	labs(title= "Biomass Climate Correlations", x="Months", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()	






