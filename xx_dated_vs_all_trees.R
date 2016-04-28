library(dplR)
library(ggplot2)
# Checkign the difference between dated and gapfilled trees

# Gapfilled Data
load("processed_data/vuf_inc.Rdata")
summary(vuf.inc)

load("processed_data/vlf_inc.Rdata")
summary(vlf.inc)
head(vlf.inc)

all.vuf <- data.frame(Year = as.numeric(row.names(vuf.inc)),
						 mean = apply(vuf.inc,1, FUN="mean", na.rm=T),
						 UB = apply(vuf.inc,1, FUN=quantile, 0.975, na.rm=T),
						 LB = apply(vuf.inc,1, FUN=quantile, 0.025, na.rm=T),
						 type="all.trees",
						 site = "VUF"
						 )
summary(all.vuf)


all.vlf <- data.frame(Year = as.numeric(row.names(vlf.inc)),
						 mean = apply(vlf.inc,1, FUN="mean", na.rm=T),
						 UB = apply(vlf.inc,1, FUN=quantile, 0.975, na.rm=T),
						 LB = apply(vlf.inc,1, FUN=quantile, 0.025, na.rm=T),
						 type = "all.trees",
						 site = "VLF"
						 )
summary(all.vlf)


# Only Dated trees

core.data <- read.csv("raw_input_files/Core_data_01202014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
names(core.data)
core.data$plot <- substr(core.data$plot.id, 3, 3)
core.data$plot <- as.factor(core.data$plot)

# Slimming down core.data to be just the valles
core.data <- core.data[substr(core.data$CoreID, 1, 1)=="V",]

summary(core.data)


core.rw <- read.rwl("RWL/RWL_all_trees.rwl")
summary(core.rw)

#removing the extra character that tellervo adds
names(core.rw)<-substr(names(core.rw), 1, 7)
names(core.rw)

#SUBSETTING JUST THE VALLES SAMPLES
core.rw <- core.rw [,substr(names(core.rw),1,1)=="V"]
summary(core.rw$series)

replace.b <- which(substr(names(core.rw),7,7)=="b") 
names(core.rw)[replace.b] <- paste0(substr(names(core.rw)[replace.b], 1,6), "B")

replace.a <- which(substr(names(core.rw),7,7)=="a") 
names(core.rw)[replace.a] <- paste0(substr(names(core.rw)[replace.a], 1,6), "A")
	
names(core.rw) 

dated.names <- core.data[core.data$dated=="Y" & !is.na(core.data$dated),]
summary(dated.names)
dim(dated.names)

# Getting just the dated trees from those trees that were measured
dated.rw <-  core.rw[,names(core.rw) %in% dated.names$CoreID]
head(dated.rw)
dim(dated.rw)

dated.rw <- dated.rw[order(row.names(dated.rw), decreasing=T),] 



# Separatign sites and getting the mean and 95% CI
dated.vuf<- dated.rw[,substr(names(dated.rw),1,3)=="VUF"]
head(dated.vuf)

dated.vlf<- dated.rw[,substr(names(dated.rw),1,3)=="VLF"]
head(dated.vlf)

dated.vuf2 <- data.frame(Year = as.numeric(row.names(dated.vuf)),
						 mean = apply(dated.vuf,1, FUN="mean", na.rm=T),
						 UB = apply(dated.vuf,1, FUN=quantile, 0.975, na.rm=T),
						 LB = apply(dated.vuf,1, FUN=quantile, 0.025, na.rm=T),
						 type = "dated",
						 site = "VUF"
						 )
summary(dated.vuf2)
head(dated.vuf2)

dated.vlf2 <- data.frame(Year = as.numeric(row.names(dated.vlf)),
						 mean = apply(dated.vlf,1, FUN="mean", na.rm=T),
						 UB = apply(dated.vlf,1, FUN=quantile, 0.975, na.rm=T),
						 LB = apply(dated.vlf,1, FUN=quantile, 0.025, na.rm=T),
						 type="dated",
						 site = "VLF"
						 )
summary(dated.vlf2)
head(dated.vlf2)

# Combine dated and all trees by site to make graphing easier

valles.compare <- rbind(dated.vuf2, all.vuf, dated.vlf2, all.vlf)
summary(valles.compare)


# Graphing sites

cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

pdf("figures/uncertainty_figures/dated_vs_all.pdf", width=13, height=8.5)
ggplot(data=valles.compare[valles.compare$Year >= 1980,]) + facet_grid(site~.)+
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=type), alpha=0.4)+
	geom_line(aes(x=Year, y=mean, color=type), size=1.5)+
	#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  #theme(legend.position=c(0.2,0.85)) + 

  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5), face="bold")) +
  
  labs(title= "Dated vs. Gap filled", x="Year", y=expression(bold(paste("Ring-width Increment (mm)")))) +
  scale_fill_manual(name="Uncertainty",values=cbbPalette, labels=c("Dated", "All Trees")) +
  scale_color_manual(name="Uncertainty",values=cbbPalette, labels=c("Dated", "All Trees")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15)))
dev.off()

