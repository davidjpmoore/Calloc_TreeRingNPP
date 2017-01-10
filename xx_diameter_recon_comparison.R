# Comparing my way of reconstructing diameter with Flurin's way of reconstructing diameter

##################################################################################
# DBH Reconstruction
##################################################################################
library(car)
library(reshape2)
library(ggplot2)
# ---------------------------------------
# DBH..cm. Reconstruction
# ---------------------------------------
# Tree Data
tree.data <- read.csv("processed_data/TreeData.csv")
summary(tree.data)

# Site Data (for year cored) 
Site.data <- read.csv("raw_input_files/DOE_plus_valles.csv", na.strings="")
Site.data$Year.sample <- as.numeric(substr(Site.data$date.sample,7,10))
summary(Site.data)

# merging in the year sampled into the tree data & calculating age
tree.data <- merge(tree.data, Site.data[,c("PlotID", "Year.sample")], all.x=T, all.y=F)
tree.data$Age <- tree.data$Year.sample - tree.data$Pith
summary(tree.data)

core.data <- read.csv("processed_data/core_data.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the Site the trees belong to
names(core.data)
summary(core.data)

#Load in the gap-filled data
ring.data <- read.csv("processed_data/RingData_All_Gapfilled.csv", header=T)
summary(ring.data)

# making a data frame with trees as columns and years as ros
ring.data$Year <- as.factor(ring.data$Year)
trees.gapfilled <- recast(ring.data[,c("Year", "TreeID", "RW.gapfilled")], Year ~ TreeID)
# summary(trees.gapfilled)

row.names(trees.gapfilled) <- trees.gapfilled$Year
trees.gapfilled <- trees.gapfilled[,2:ncol(trees.gapfilled)]
trees.gapfilled[(nrow(trees.gapfilled)-10):nrow(trees.gapfilled), 1:10]
# ---------------------------------------

# Ordering the data
trees.gapfilled <- trees.gapfilled[order(row.names(trees.gapfilled), decreasing=T),order(names(trees.gapfilled))]
trees.gapfilled[1:10, 1:10]
trees.gapfilled[1:10, (ncol(trees.gapfilled)-10):ncol(trees.gapfilled)]

dbh.recon <- trees.gapfilled
trees.check <- vector() # trees with negative DBH..cm.
############################################
# Flurin's Way of reconstructing DBH
# Using proportional Method found in Bakker 2005
############################################
summary(dbh.recon)
summary(tree.data)

dbh.recon2 <- dbh.recon
dbh.recon2[is.na(dbh.recon2)==T] <- 0

# Reorderind data so that the cumulative sums will be correct
dbh.recon2 <- dbh.recon2[order(row.names(dbh.recon2), decreasing=F),order(names(dbh.recon2))]

# Shorten things for now
# dbh.recon2 <- dbh.recon2[,c(1:20)]


xylem.radius <- as.data.frame(apply(dbh.recon2, 2, FUN="cumsum"))
summary(xylem.radius)

head(xylem.radius)
names(xylem.radius)

ip <- xylem.radius[nrow(xylem.radius),] #the full radius
head(ip)
names(ip)
diameter <- xylem.radius

for(i in names(xylem.radius)){
  for(t in 1:nrow(xylem.radius)){
    ih <- ip[names(ip)==i]-xylem.radius[t,i]
    
    diameter[t,i] <- tree.data[tree.data$TreeID==i,"DBH..cm."] *((ip[names(ip)==i]-ih)/ip[names(ip)==i])
  }
}
summary(diameter)
head(diameter)
plot(diameter[,"VLF001"])

diameter2 <- diameter[order(row.names(diameter), decreasing=T),order(names(diameter))]
head(diameter2)


diameter2[diameter2==0] <- NA
diameter2 <- diameter2[as.numeric(row.names(diameter2))<2012 & as.numeric(row.names(diameter2))> 1980,]
# Loading in Ross's way of Diameters

ross.diam <- read.csv("processed_data/GapFilling_DBHrecon_ALL.csv", header=T, row.names=1)
summary(ross.diam)
head(ross.diam)
ross.diam <- ross.diam[row.names(ross.diam)<2012 & row.names(ross.diam)>1980,]

length(ross.diam[,"VLF001"])
length(diameter2[,"VLF001"])

x <- stack(ross.diam)[,1]
y <- stack(diameter2)[,1]
plot(x~y, xlim=c(0,30), ylim=c(0,30))

test <- lm(x~y)
summary(test)

diam.diff = x-y
summary(diam.diff)

# Need to merge my stuff together with Flurin's.  
# Need to make some classy graphs

diam.comp <- data.frame(ross = stack(ross.diam)[,1],
                        flurin = stack(diameter2)[,1])

ross.stack <- stack(ross.diam)
names(ross.stack) <- c("diam", "TreeID")
ross.stack$type <- as.factor("ross")

flurin.stack <- stack(diameter2)
names(flurin.stack) <- c("diam", "TreeID")
flurin.stack$type <- as.factor("flurin")

diam.comp.stack <- rbind(ross.stack, flurin.stack)
summary(diam.comp.stack)

# Pretty graph of scatter plot between Bakker (Flurin), and our method

pdf("figures/uncertainty_figures/Supp_Fig6_scatter.pdf", height = 8, width = 13)
ggplot(diam.comp, aes(ross, flurin)) +
  geom_point(color="grey60", pch=1)+
  stat_smooth(method="lm", alpha=0.5, size=1, linetype="dashed", color="blue")+
  geom_abline(slope=1, intercept=0, color="red", size=1)+
  
  annotate("text", label= "paste(italic(R) ^ 2, \" = .99\")", parse=T,size = 8, x=10, y=50, color="black")+
  
  labs(x="Alexander method (cm)", y="Bakker method (cm)") +
  
  theme(axis.line=element_line(color="black", size=0.5), 
        panel.grid.major=element_blank(), 
        panel.grid.minor= element_blank(), 
        panel.border= element_blank(), 
        panel.background= element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1.5)),
        axis.text.y=element_text(color="black", size=rel(1.5)), 
        axis.title.x=element_text(size=15, vjust=-0.5),
        axis.title.y=element_text(size=15, vjust=1),
        plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
    theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
dev.off()    

# Density plot/histogram of diameters from each site
summary(tree.data)

valles.tree.data <- tree.data[tree.data$PlotID %in% c("VLA", "VLB", "VUA", "VUB"), c("Site", "PlotID", "DBH..cm.")]

summary(valles.tree.data)

valles.tree.data$PlotID <- recode(valles.tree.data$PlotID, "'VLA'='Plot 1';'VLB'='Plot 2';'VUA'='Plot 1';'VUB'='Plot 2'")
valles.tree.data$Site <- recode(valles.tree.data$Site, "'Valles Caldera Upper'='Upper Site';'Valles Caldera Lower'='Lower Site'")
valles.tree.data$Site <- factor(valles.tree.data$Site, levels=c("Upper Site", "Lower Site"))

#cbbPalette <- c("#009E73", "#CC79A7", "#0072B2", "#E69F00")
cbbPalette <- c("#0072B2", "#E69F00")

pdf("figures/uncertainty_figures/Supp_Fig7_hist.pdf", height = 8, width = 13)
ggplot(data=valles.tree.data) + facet_grid(PlotID ~ Site) +
  geom_histogram(aes(x=DBH..cm., fill=PlotID), binwidth=2, position = "stack") +
  scale_fill_manual(values=cbbPalette) +
  labs(x="DBH (cm)", y= "Count") +
  
  theme(axis.line=element_line(color="black", size=0.5), 
        panel.grid.major=element_blank(), 
        panel.grid.minor= element_blank(), 
        panel.border= element_blank(), 
        panel.background= element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1.5)),
        axis.text.y=element_text(color="black", size=rel(1.5)), 
        axis.title.x=element_text(size=15, vjust=-0.5),
        axis.title.y=element_text(size=15, vjust=1),
        plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        strip.text=element_text(face="bold"))
dev.off()