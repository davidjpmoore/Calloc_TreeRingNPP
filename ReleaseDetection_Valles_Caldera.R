# importing libraries
library(lattice)

# Getting Libraries
library(reshape)
library(car)
library(mgcv)
library(nlme)
#library(lme4)
library(splines)
library(MASS)
library(MuMIn)
library(ggplot2)
library(grid)
library(dplR)
###################################################
se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))	

large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))


#################################################################################################
# Reading in & Formatting data
#################################################################################################
tree.data <- read.csv("processed_data/TreeData.csv", header=T)
summary(tree.data)
#trees.use <- tree.data[substr(tree.data$PlotID, 1, 1)=="V" | substr(tree.data$PlotID, 1, 2)=="MM",]
trees.use <- tree.data[substr(tree.data$PlotID, 1, 1)=="V" & tree.data$Dated=="Y",]
trees.use <- trees.use[!is.na(trees.use$TreeID),]
head(trees.use)
dim(trees.use)




tree.rw <- read.csv("processed_data/tree_rw.csv", header=T)
summary(tree.rw)
dated.rw <- tree.rw[substr(tree.rw$TreeID,1,1)=="V" & tree.rw$TreeID %in% trees.use$TreeID,]
summary(dated.rw)

#sea.df <- recast(gam2.weights[,c("Year", "TreeID", "BA.inc.Clim")], Year~TreeID)

dated.rw$Year <- as.factor(dated.rw$Year)
dated.rw2 <- recast(dated.rw[,c("Year", "TreeID", "RW")], Year~TreeID)
head(dated.rw2)
row.names(dated.rw2) <- dated.rw2$Year
dated.rw2 <- dated.rw2[,2:ncol(dated.rw2)] #dropping year

# Christy's file
# ring.data <- read.csv("Cores_FullData_Final2.csv") 
# ring.data$Plot <- as.factor(ring.data$Plot)
# ring.data$Tree <- as.factor(ring.data$Tree)
# summary(ring.data)
# 
# 
# cores.bai <- read.csv("Cores_BAI_Measured.csv", row.names=1)
# dim(cores.bai)
# names(cores.bai)
# cores.bai[1:10, 1:10]
# cores.bai[(nrow(cores.bai)-10):nrow(cores.bai), 1:10]




#################################################################################################
# Detecting & Quantifying release events
#################################################################################################

##############################################################################
# Release/Supression Function
##############################################################################
# A standard deviation-based approach
release.suppress <- function(x, r, p, thresh,...){
	# x = data frame of measurements with years as rows
	# r = number of years showing RELEASE-like growth
	# p = number of years in PREVIOUS window -- the reference frame
		# note: if y > 1 == release event, if y < 1 == supression
	y <- x
	y[,] <- 0
	for(j in names(x)){
		for(i in (2*p):(length(x[,1])-r)){
			y[i,j] <- ifelse(x[(i-p),j]<=0, 0, ifelse(mean(x[i:(i+r),j])>(mean(x[(i-p):i,j])+thresh*sd(x[(i-p):i,j])) | mean(x[i:(i+r),j])<(mean(x[(i-p):i,j])-(thresh*sd(x[(i-p):i,j]))) , mean(x[i:(i+r),j])/mean(x[(i-p):i,j]), 0))
			y[i,j] <- ifelse(y[i,j]==Inf, 0, y[i,j])
		}
	}
	return(y)
}

# Flexible model based off of Lorimer & Frelich 1989
# Notes: 1) this is designed for releases, not supressions; 2) r/p +/- 1 makes year indexing inclusive of i
lorimer.release <- function(x, r, p, thresh,...){
	# x = data frame of measurements with years as rows
	# r = number of years showing RELEASE-like growth (>=15 for sustatined, 10-15 for for temporary)
	# p = number of years in PREVIOUS window -- the reference frame (Lorimer & Frelich use 15)
	# thresh = growth threshold for critera; Frelich says 100% for major, 50-99% for minor release
	# Output = relative growth of window following year [i] to that preceding year [i] (i.e. if looking at major release events, all values should be >= 2 or NA)
	# note: if y > 1 == release event, if y < 1 == supression
	y <- data.frame(array(dim=dim(x)))
	names(y) <- names(x)
	row.names(y) <- row.names(x)
	for(j in names(x)){
		for(i in (p):(nrow(x)-r)){
			y[i,j] <- ifelse(!(min(x[(i-p+1):i,j]))>0, 0, ifelse(mean(x[i:(i+r-1),j])>(mean(x[(i-p+1):i,j])+thresh*mean(x[(i-p+1):i,j])), mean(x[i:(i+r-1),j])/mean(x[(i-p+1):i,j]), 0))
			y[i,j] <- ifelse(y[i,j]==0, NA, y[i,j])
		}
	}
	return(y)
}



##############################################################################
dim(dated.rw2)

# Calculating minor release events
release.minor <- lorimer.release(x=dated.rw2, r=10, p=15, thresh=0.5)
summary(release.minor[,1:15])
# release.minor[,1:5]
write.csv(release.minor, "processed_data/Trees_ReleaseEvents_MinorRelease.csv", row.names=T)

# Calculating major release events
release.major <- lorimer.release(x=dated.rw2, r=10, p=15, thresh=1)
summary(release.major[,1:20])

write.csv(release.major, "processed_data/Trees_ReleaseEvents_MajorRelease.csv", row.names=T)


##############################################################################
# Stacking & Merging Release Data
##############################################################################
# cores.bai <- read.csv("Cores_BAI_Measured.csv", row.names=1)
# cores.bai[1:25,1:10]


release.minor <- read.csv("processed_data/Trees_ReleaseEvents_MinorRelease.csv", row.names=1)
dim(release.minor)
release.minor[1:10, 1:11]
# years <- disturb$Year

release.major <- read.csv("processed_data/Trees_ReleaseEvents_MajorRelease.csv", row.names=1)
dim(release.major)
release.major[1:10, 1:11]

dim(dated.rw2)
dim(release.major)
dim(release.minor)

#########################
# Stacking & Merging Release Records
#########################

# Stacking RW measurements
dated.stack <- stack(dated.rw2)
names(dated.stack) <- c("RW", "TreeID")
dated.stack$Year <- as.numeric(paste(row.names(dated.rw2)))
dated.stack$Stems <- ifelse(dated.stack$RW>0, 1, NA)
dated.stack$PlotID <- as.factor(substr(dated.stack$TreeID,1,4)) 
dated.stack$Site <- as.factor(substr(dated.stack$TreeID,1,3)) 
summary(dated.stack)

# Stacking Minor Releases
minor.stack <- stack(release.minor)
names(minor.stack) <- c("Release.Minor", "TreeID")
# minor.stack$Disturb <- ifelse(minor.stack$Disturb==0, NA, minor.stack$Disturb)
minor.stack$Year <- as.numeric(paste(row.names(release.minor)))
summary(minor.stack)

# Stacking Major Releases
major.stack <- stack(release.major)
names(major.stack) <- c("Release.Major", "TreeID")
# minor.stack$Disturb <- ifelse(minor.stack$Disturb==0, NA, minor.stack$Disturb)
major.stack$Year <- as.numeric(paste(row.names(release.major)))
summary(major.stack)

minor.stack[1:20,]
major.stack[1:20,]

# Merging Releases
release.stack <- merge(minor.stack, major.stack, all.x=T, all.y=T)
release.stack$Release.Minor <- ifelse(release.stack$Release.Minor >= 2, NA, release.stack$Release.Minor)
summary(release.stack)
dim(release.stack);dim(minor.stack); dim(major.stack)

# Merging BAI measurements with Release Events
dated.stack2 <- merge(dated.stack, release.stack, all.x=T, all.y=T)
dated.stack2$n.Minor <- ifelse(release.stack$Release.Minor>0,1,NA)
dated.stack2$n.Major <- ifelse(release.stack$Release.Major>0,1,NA)
summary(dated.stack2)
dim(dated.stack)
dim(dated.stack2)

# #########################
# # Adding in Strata Info
# #########################
# cores.data <- read.csv("Cores_FullData_Final2.csv")
# summary(cores.data)
# 
# # Merging in Relative size
# cores.stack3 <- merge(cores.stack2, cores.data[,c("TreeID", "Year", "RelBA")], all.x=T, all.y=F)
# summary(cores.stack3)
# dim(cores.stack3); dim(cores.stack2)
# 
# hist(cores.stack3$RelBA)
# 
# # Creating a rough strata cuttoff
# cores.stack3$Size <- as.factor(ifelse(cores.stack3$RelBA > 0.75, "Large", "Small"))
# summary(cores.stack3)
# 
# write.csv(cores.stack3, "ReleaseEvents_Trees.csv", row.names=F)
# 

##############################################################################
# Aggregating to the Plot Level
##############################################################################
# cores.stack3 <- read.csv("ReleaseEvents_Trees.csv")
# summary(cores.stack3)

#########################
# Regardless of strata
#########################

# Calculate mean release event for trees that do not have NA
release.plot1 <- aggregate(dated.stack2[,c("Release.Minor", "Release.Major")], by=list(dated.stack2$Year, dated.stack2$PlotID, dated.stack2$Site), FUN=mean, na.rm=T)
names(release.plot1) <- c("Year", "PlotID", "Site", "Release.Minor", "Release.Major")
summary(release.plot1)

# Calculating the number of trees showing release
release.plot2 <- aggregate(dated.stack2[,c("n.Minor", "n.Major", "Stems")], by=list(dated.stack2$Year, dated.stack2$PlotID, dated.stack2$Site), FUN=sum, na.rm=T)
names(release.plot2) <- c("Year", "PlotID", "Site", "n.Minor", "n.Major", "n.Plot")
summary(release.plot2)

hist(release.plot2$n.Major)
hist(release.plot2$n.Plot)
unique(release.plot2$n.Major)

# Merge the data sets together and find the proportion of trees showing release
dim(release.plot1); dim(release.plot2)
release.plot <- merge(release.plot1, release.plot2, all.x=T, all.y=T)
release.plot$p.Minor <- release.plot$n.Minor/release.plot$n.Plot
release.plot$p.Major <- release.plot$n.Major/release.plot$n.Plot
# release.plot$Trans <- as.factor(substr(release.plot$PlotID,4,4))
release.plot$Plot <- as.factor(substr(release.plot$PlotID,4,4))
release.plot <- release.plot[,c("PlotID", "Site", "Plot", "Year", "Release.Minor", "Release.Major", "n.Minor", "n.Major", "n.Plot", "p.Minor", "p.Major")]
release.plot$Plot <- recode(release.plot$Plot, "'0'= 'A'; 1 = 'B' ")
summary(release.plot)
dim(release.plot)

# #########################
# # Large only
# #########################
# cores.large <- cores.stack3[cores.stack3$Size=="Large",]
# summary(cores.large)
# 
# # Calculate mean release event for trees that do not have NA
# release.large1 <- aggregate(cores.large[,c("Release.Minor", "Release.Major")], by=list(cores.large$Year, cores.large$PlotID, cores.large$Site), FUN=mean, na.rm=T)
# names(release.large1) <- c("Year", "PlotID", "Site", "Release.Minor.large", "Release.Major.large")
# summary(release.large1)
# 
# release.large2 <- aggregate(cores.large[,c("n.Minor", "n.Major", "Stems")], by=list(cores.large$Year, cores.large$PlotID, cores.large$Site), FUN=sum, na.rm=T)
# names(release.large2) <- c("Year", "PlotID", "Site", "n.Minor.large", "n.Major.large", "n.Plot.large")
# summary(release.large2)
# 
# hist(release.large2$n.Major.large)
# hist(release.large2$n.Plot.large)
# unique(release.large2$n.Major.large)
# 
# dim(release.large1); dim(release.large2)
# release.large <- merge(release.large1, release.large2, all.x=T, all.y=T)
# release.large$p.Minor.large <- release.large$n.Minor.large/release.large$n.Plot.large
# release.large$p.Major.large <- release.large$n.Major.large/release.large$n.Plot.large
# release.large$Trans <- as.factor(substr(release.large$PlotID,4,4))
# release.large$Plot <- as.factor(substr(release.large$PlotID,5,5))
# summary(release.large)
# dim(release.large)
# 
# 
# #########################
# # Small only
# #########################
# cores.small <- cores.stack3[cores.stack3$Size=="Small",]
# summary(cores.small)
# 
# # Calculate mean release event for trees that do not have NA
# release.small1 <- aggregate(cores.small[,c("Release.Minor", "Release.Major")], by=list(cores.small$Year, cores.small$PlotID, cores.small$Site), FUN=mean, na.rm=T)
# names(release.small1) <- c("Year", "PlotID", "Site", "Release.Minor.small", "Release.Major.small")
# summary(release.small1)
# 
# release.small2 <- aggregate(cores.small[,c("n.Minor", "n.Major", "Stems")], by=list(cores.small$Year, cores.small$PlotID, cores.small$Site), FUN=sum, na.rm=T)
# names(release.small2) <- c("Year", "PlotID", "Site", "n.Minor.small", "n.Major.small", "n.Plot.small")
# summary(release.small2)
# 
# hist(release.small2$n.Major.small)
# hist(release.small2$n.Plot.small)
# unique(release.small2$n.Major.small)
# 
# dim(release.small1); dim(release.small2)
# release.small <- merge(release.small1, release.small2, all.x=T, all.y=T)
# release.small$p.Minor.small <- release.small$n.Minor.small/release.small$n.Plot.small
# release.small$p.Major.small <- release.small$n.Major.small/release.small$n.Plot.small
# release.small$Trans <- as.factor(substr(release.small$PlotID,4,4))
# release.small$Plot <- as.factor(substr(release.small$PlotID,5,5))
# summary(release.small)
# dim(release.small)
# 
# #########################
# # Merging data sets together
# #########################
# release.strata <- merge(release.large, release.small, all.x=T, all.y=T)
# summary(release.strata)
# 
# release.plot3 <- merge(release.plot, release.strata, all.x=T, all.y=F)
# summary(release.plot3)
# dim(release.plot3); dim(release.plot)
# 
# write.csv(release.plot3, "ReleaseEvents_Plots.csv", row.names=F)
# 
# #####################
# ggplot(data=dated.stack2[dated.stack2$Site=="VLF",]) + geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + 
#   facet_grid(Plot ~ Site) + large.axes2 + scale_x_continuous(breaks=c(1900,2000))
# 
# ggplot(data=release.plot3[release.plot3$Site=="BLD",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Trans ~ Plot) + large.axes2 + scale_x_continuous(breaks=c(1900,2000))



##############################################################################
# Repeating Aggregation with Strata as factor
##############################################################################
#########################
# Regardless of strata
#########################

# Calculate mean release event for trees that do not have NA
release.plot1 <- aggregate(dated.stack2[,c("Release.Minor", "Release.Major")], by=list(dated.stack2$Year, dated.stack2$PlotID, dated.stack2$Site), FUN=mean, na.rm=T)
names(release.plot1) <- c("Year", "PlotID", "Site", "Release.Minor", "Release.Major")
summary(release.plot1)

release.plot2 <- aggregate(dated.stack2[,c("n.Minor", "n.Major", "Stems")], by=list(dated.stack2$Year, dated.stack2$PlotID, dated.stack2$Site), FUN=sum, na.rm=T)
names(release.plot2) <- c("Year", "PlotID", "Site","n.Minor", "n.Major", "n.Plot")
summary(release.plot2)

hist(release.plot2$n.Major)
hist(release.plot2$n.Plot)
unique(release.plot2$n.Major)

dim(release.plot1); dim(release.plot2)
release.plot <- merge(release.plot1, release.plot2, all.x=T, all.y=T)
release.plot$p.Minor <- release.plot$n.Minor/release.plot$n.Plot
release.plot$p.Major <- release.plot$n.Major/release.plot$n.Plot
# release.plot$Trans <- as.factor(substr(release.plot$PlotID,4,4))
release.plot$Plot <- as.factor(substr(release.plot$PlotID,4,4))
release.plot <- release.plot[,c("PlotID", "Site", "Plot", "Year","Release.Minor", "Release.Major", "n.Minor", "n.Major", "n.Plot", "p.Minor", "p.Major")]
release.plot$Plot <- recode(release.plot$Plot, "'0'='A'; '1'='B'")
summary(release.plot)
dim(release.plot)

write.csv(release.plot, "processed_data/ReleaseEvents_Plots_Size.csv", row.names=F)







##############################################################################
# Graphing with a minimum threshold
##############################################################################

# Setting a minimum threshold
release.plot2 <- release.plot
release.plot2$p.Minor <- ifelse(release.plot2$p.Minor<0.25, 0, release.plot2$p.Minor)
release.plot2$p.Major <- ifelse(release.plot2$p.Major<0.25, 0, release.plot2$p.Major)
summary(release.plot2)

min(release.plot2[release.plot2$p.Minor>0, "p.Minor"], na.rm=T)
hist(release.plot2[release.plot2$p.Minor>0, "p.Minor"])



ggplot(data=release.plot2[release.plot2$Site=="VUF",]) + 
  geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + 
  facet_grid(Site ~ Plot) + large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000))+
  labs(title="Upper Site Minor Release")

ggplot(data=release.plot2[release.plot2$Site=="VLF",]) + 
  geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + facet_grid(Site ~ Plot) + 
  large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000))+
  labs(title="Lower Site Minor Release")


ggplot(data=release.plot2[release.plot2$Site=="VLF",]) + 
  geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + 
  facet_grid(Site ~ Plot) + large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000))+
  labs(title="Lower Site Minor Release")

ggplot(data=release.plot2[release.plot2$Site=="VLF",]) + 
  geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Site ~ Plot) + 
  large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000)) +
  labs(title="Lower Site Major Release")

ggplot(data=release.plot2[release.plot2$Site=="VUF",]) + 
  geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Site ~ Plot) + 
  large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000)) +  scale_y_continuous(breaks=c(0.5))+
  labs(title="Upper Site Major Release")

ggplot(data=release.plot2[release.plot2$Site=="VUF",]) + 
  geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Site ~ Plot) + 
  large.axes2 + scale_x_continuous(breaks=c(1900,1925,1950,1975,2000)) +  scale_y_continuous(breaks=c(0.5))+
  labs(title="Upper Site Major Release")

##############################################################################
# Aggregating to the Site Level (mostly for graphing)
##############################################################################
release.plot <- read.csv("ReleaseEvents_Plots.csv")
release.plot[is.na(release.plot)] <- 0
summary(release.plot)

release.site <- aggregate(release.plot[,c("Release.Minor", "Release.Major", "p.Minor", "p.Major"),], by=list(release.plot$Site, release.plot$Year), FUN=mean, na.rm=T)
names(release.site) <- c("Site", "Year", names(release.site[,3:ncol(release.site)]))
release.site$Site.NS <- recode(release.site$Site, "'VLF'='1'; 'VUF'='2'")
release.site$Site <- factor(release.site$Site, levels=c("VUF", "VLF"))
summary(release.site)

# release.site.a <- release.site
# release.site.a$type <- as.factor("Major")
# 
# release.site.b <- release.site
# release.site.b$type <- as.factor("Minor")
# 
# release.site <- rbind(release.site.a, release.site.b)
release.site$Site <- recode(release.site$Site, "'VUF'='Upper Site';'VLF'='Lower Site'")

save(release.site, file="processed_data/release_detection.Rdata")

ggplot(data=release.site[release.site$p.Major>=0.25 & release.site$Year>=1980,]) + facet_grid(Site~.) + 
  geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + 
  large.axes2 + 
  scale_x_continuous(breaks=c(1980, 1990,2000, 2010)) +  scale_y_continuous(breaks=c(0,0.25))

pdf(file="figures/uncertainty_figures/valles_Major_Releases.pdf", width=11, height=8)
ggplot(data=release.site[release.site$Year>=1980,]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) +
  facet_grid(Site ~ .) + large.axes2 + 
  scale_x_continuous(expand=c(0,0),limits=c(1980, 2014),breaks=c(1980, 1990,2000, 2010)) +  
  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15, 0.30, 0.45), limit=c(0,0.5))+
  ggtitle("Major Releases")
dev.off()

pdf(file="figures/uncertainty_figures/valles_Minor_Releases.pdf", width=11, height=8)
ggplot(data=release.site[release.site$Year>=1980,]) + geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + 
  facet_grid(Site.NS ~ .) + large.axes2 + 
  scale_x_continuous(expand=c(0,0),limits=c(1980, 2014),breaks=c(1980, 1990,2000, 2010)) +  
  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15, 0.30, 0.45), limit=c(0,0.5))+
  ggtitle("Minor Releases")
dev.off()

# ggplot(data=release.site) + geom_histogram(aes(x=Year, weight=p.Minor.large), binwidth=1) + facet_grid(Site.NS ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15))
# 
# ggplot(data=release.site) + geom_histogram(aes(x=Year, weight=p.Minor.small), binwidth=1) + facet_grid(Site.NS ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15))


##############################################################################
# Smoothing Release Events (to match Estab)
##############################################################################
release.plot <- read.csv("ReleaseEvents_Plots.csv")
release.plot[is.na(release.plot)] <- 0
summary(release.plot)


release.smooth <- release.plot[,c("PlotID", "Site", "Year")]
summary(release.smooth)

print(Sys.time())
# Smoothing 
for(p in unique(release.smooth$PlotID)){
	data.plot <- release.plot[release.plot$PlotID==p,]
for(y in min(release.smooth$Year):max(release.smooth$Year)){

	release.smooth[release.smooth$PlotID==p & release.smooth$Year==y,"Release.Minor"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Release.Minor"], na.rm=T)
	release.smooth[release.smooth$PlotID==p & release.smooth$Year==y,"Release.Major"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Release.Major"], na.rm=T)
	release.smooth[release.smooth$PlotID==p & release.smooth$Year==y,"n.Minor"] <- sum(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"n.Minor"],na.rm=T)
	release.smooth[release.smooth$PlotID==p & release.smooth$Year==y,"n.Major"] <- sum(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"n.Major"],na.rm=T)
	release.smooth[release.smooth$PlotID==p & release.smooth$Year==y,"n.Plot"] <- sum(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"n.Plot"],na.rm=T)

}
}

release.smooth$p.Minor <- release.smooth$n.Minor/release.smooth$n.Plot
release.smooth$p.Major <- release.smooth$n.Major/release.smooth$n.Plot
release.smooth$p.Minor <- ifelse(is.na(release.smooth$p.Minor), 0, release.smooth$p.Minor)
release.smooth$p.Major <- ifelse(is.na(release.smooth$p.Major), 0, release.smooth$p.Major)
summary(release.smooth)
summary(release.plot)

write.csv(release.smooth, "ReleaseEvents_Plots_Smooth_5yrWin.csv", row.names=F)

ggplot(data=release.smooth[release.smooth$p.Major>=0.25,]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Site~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(breaks=c(0,0.25))

ggplot(data=release.smooth) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(Site ~ .) + large.axes2 + scale_x_continuous(limits=c(1950, 2014),breaks=c(1950,1960,1970,1980,1990,2000)) +  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15))+
  ggtitle("Major Releases")

ggplot(data=release.smooth) + geom_histogram(aes(x=Year, weight=p.Minor), binwidth=1) + facet_grid(Site ~ .) + large.axes2 + scale_x_continuous(limits=c(1950, 2014), breaks=c(1950,1960,1970,1980,1990,2000)) +  scale_y_continuous(name="Percent Trees Meeting Criteria", breaks=c(0,0.15))+
  ggtitle("Minor Releases")



##################################
# Calculating Time since Disturbance
##################################
release.time <- release.smooth[,]
summary(release.time)


################
# Changing Yes/No to Time elapsed
summary(release.time)

for(p in unique(release.time$PlotID)){
	#################
	# If there are no trees, don't say there hasn't been a disturbance; counting begins after first tree enters the record
	#################
	release.time[release.time$PlotID==p, "time.Minor"] <- ifelse(release.time[release.time$PlotID==p,"n.Plot"]<=1,0,1)
	release.time[release.time$PlotID==p, "time.Major"] <- ifelse(release.time[release.time$PlotID==p,"n.Plot"]<=1,0,1)
	
	######
	# Initializing Time elapsed characterization
	release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "peak.Minor.mag"] <- release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "Release.Minor"]

	release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "peak.Major.mag"] <- release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "Release.Major"]

	release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "peak.Minor.ext"] <- release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "p.Minor"]
	
	release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "peak.Major.ext"] <- release.time[release.time$PlotID==p & release.time$Year==min(release.time$Year), "p.Major"]
	
for(y in (min(release.time$Year)+1):max(release.time$Year)){
	#################
	# Start counting whent the percentage of trees showing a growth release decreases
	#################
	release.time[release.time$PlotID==p & release.time$Year==y,"time.Minor"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"p.Minor"]>release.time[release.time$PlotID==p & release.time$Year==(y-1),"p.Minor"], 0, release.time[release.time$PlotID==p & release.time$Year==(y-1),"time.Minor"]+1)

	release.time[release.time$PlotID==p & release.time$Year==y,"time.Major"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"p.Major"]>release.time[release.time$PlotID==p & release.time$Year==(y-1),"p.Major"], 0, release.time[release.time$PlotID==p & release.time$Year==(y-1),"time.Major"]+1)

	#################
	# Peak Magnitude = Mean release of trees showing release at peak 
	# Peak Extent 	 = maximum percentage of trees showing release
	#################
	# Peak Magnitude
	release.time[release.time$PlotID==p & release.time$Year==y,"peak.Minor.mag"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"time.Minor"]==0, release.time[release.time$PlotID==p & release.time$Year==y,"Release.Minor"],
	release.time[release.time$PlotID==p & release.time$Year==y-1,"peak.Minor.mag"])
	# release.time[release.time$PlotID==p & release.time$Year==(y-1),"peak.Minor.mag"])

	release.time[release.time$PlotID==p & release.time$Year==y,"peak.Major.mag"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"time.Major"]==0, release.time[release.time$PlotID==p & release.time$Year==y,"Release.Major"], release.time[release.time$PlotID==p & release.time$Year==(y-1),"peak.Major.mag"])

	# Peak Extent
	release.time[release.time$PlotID==p & release.time$Year==y,"peak.Minor.ext"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"time.Minor"]==0, release.time[release.time$PlotID==p & release.time$Year==y,"p.Minor"], release.time[release.time$PlotID==p & release.time$Year==(y-1),"peak.Minor.ext"])

	release.time[release.time$PlotID==p & release.time$Year==y,"peak.Major.ext"] <- ifelse(release.time[release.time$PlotID==p & release.time$Year==y,"time.Major"]==0, release.time[release.time$PlotID==p & release.time$Year==y,"p.Major"], release.time[release.time$PlotID==p & release.time$Year==(y-1),"peak.Major.ext"])
	
}
}
summary(release.time)
dim(release.time)
hist(release.time$time.Major)
hist(release.time$time.Minor)

write.csv(release.time, "ReleaseEvents_Plots_Smooth_TimeElapsed.csv", row.names=F)
