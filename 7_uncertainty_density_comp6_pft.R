# -----------------------------------------------------------
# Quantifying the uncertainty in biomass reconstruction from uncertainty in allometric equations 
#   for only the doe Caldera
# Cleaned up from https://github.com/alexanderm10/Tree-Rings-and-Biomass/blob/master/Uncertainty_analysis/post_GF_TRW_to_biomass_CR.R
# 23 June, 2015
# -----------------------------------------------------------

# ------------------------
# Load libraries & read in data
# ------------------------
# Libraries:
library(ggplot2); library(grid)
library(car)

# Read in the full plot-based biomass estimates
load("processed_data/Biomass_Array_Tree_kgm-2_component6.RData") # loads plot-level bm.array
# ------------------------

# ------------------------
# Subset doe, take mean of allometric iterations, & figure out plot (density)-based 95% CI
# Note: the CIs here will be rather weird since we only have 2 plots with cores for each site
# ------------------------
# Subsetting and using the mean allometric biomass estimate to get a biomass/tree
biom.doe <- data.frame(apply(bm.array, c(1,2), mean))
biom.doe$Year <- as.numeric(dimnames(bm.array)[[1]])
summary(biom.doe)

dim(biom.doe)
# summary(biom.doe)
names(biom.doe)<- ifelse(substr(names(biom.doe),1,4)=="VUF0", paste0("VUA", substr(names(biom.doe),4,6)),
                                 ifelse(substr(names(biom.doe),1,4)=="VUF1", paste0("VUB", substr(names(biom.doe),4,6)),
                                        ifelse(substr(names(biom.doe),1,4)=="VLF0", paste0("VLA", substr(names(biom.doe),4,6)),
                                               ifelse(substr(names(biom.doe),1,4)=="VLF1", paste0("VLB", substr(names(biom.doe),4,6)),
                                                      
                                                 ifelse(substr(names(biom.doe),1,4)=="HOW1", paste0("HOA", substr(names(biom.doe),4,6)),
                                                            ifelse(substr(names(biom.doe),1,4)=="HOW2", paste0("HOB", substr(names(biom.doe),4,6)),
                                                                ifelse(substr(names(biom.doe),1,4)=="HOW3", paste0("HOC", substr(names(biom.doe),4,6)),
                                                                    
                                                      paste(names(biom.doe)))))))))
names(biom.doe)




# Going from trees to plots (ignoring any tree-level plot level)
# Note: Using a dummy code here so that we bassically have plot #0 and Plot #1
plots <- unique(substr(names(biom.doe), 1, 3))
plots <- plots[!plots == "Yea"]


# Go from tree biomass (kg/m2) to plot (kg/m2)
biom.plot <- data.frame(array(dim=c(nrow(bm.array), length(plots))))
names(biom.plot) <- plots
biom.plot$Year <- biom.doe$Year
summary(biom.plot)

for(p in 1:length(plots)){
	cols.plot <- which(substr(names(biom.doe),1,3)==plots[p])
	if(substr(plots[p],1,1)=="V" | substr(plots[p],1,1)=="N"){
  biom.plot[,p] <- apply(biom.doe[,cols.plot], 1, FUN=mean)
	} else{
	  biom.plot[,p] <- apply(biom.doe[,cols.plot], 1, FUN=sum)
	}
}
summary(biom.plot)

#  Plot to Site, get CI from differences among plots
site <- unique(substr(names(biom.plot),1,2))
site <- site[!site == "Ye"]
ci.site <- list()

for(s in site){
  cols.site <- which(substr(names(biom.plot),1,2)==s) # creating an index for VUF
  ci.site[[s]] <- data.frame(Year= biom.plot$Year, SiteID= s,
                      Mean = apply(biom.plot[,cols.site], 1, mean, na.rm=T),
                      LB   = apply(biom.plot[,cols.site], 1, quantile, 0.025, na.rm=T), 
                      UB   = apply(biom.plot[,cols.site], 1, quantile, 0.975, na.rm=T))

}
names(ci.site)<- c("Austin-Cary", "Duke_HW", "Duke_ll", "Howland", "UMBS", "Morgan-Monroe", "Missouri", "Niwot", "Oak_openings", "Savannah_River", "Harvard", "VLF", "VUF")
sites <- c("Austin-Cary", "Duke_HW", "Duke_ll", "Howland", "UMBS", "Morgan-Monroe", "Missouri", "Niwot", "Oak_openings", "Savannah_River", "Harvard", "VLF", "VUF")


summary(ci.site)

# ------------------------
# Splitting out the different sites
ac <- ci.site[[1]]
dh <- ci.site[[2]]
dl <- ci.site[[3]]
how <- ci.site[[4]]
mi <- ci.site[[5]]
mm <- ci.site[[6]]
mo <- ci.site[[7]]
niw <- ci.site[[8]]
oo <- ci.site[[9]]
sr <- ci.site[[10]]
har <- ci.site[[11]]
vlf <- ci.site[[12]]
vuf <- ci.site[[13]]

# ------------------------
# Package everything together, make a quick plot and save it for later use
# This is what goes to the stacked uncertainty figure
# ------------------------
dens.uncert <- data.frame(rbind(ac, dh, dl, how, mi, mm, mo, niw, oo, sr, har, vlf,vuf))
dim(dens.uncert)
summary(dens.uncert)

dens.uncert$Site <- recode(dens.uncert$SiteID, "'AC' = 'Austin-Cary'; 'DH' = 'Duke_HW'; 'DL' = 'Duke_ll'; 'HO' = 'Howland'; 'MA' = 'UMBS'; 
                           'MM' = 'Morgan-Monroe'; 'MO' = 'Missouri'; 'NW' = 'Niwot'; 'OO' = 'Oak_openings'; 'SR' = 'Savannah_River';
                           'TP' = 'Harvard'; 'VL' = 'VLF';'VU' = 'VUF'")
summary(dens.uncert)
unique(dens.uncert$Site)

# removing last two years from Harvard and Howland as they are not real
dens.uncert[dens.uncert$Site %in% c("Howland", "Harvard") & dens.uncert$Year>=(max(dens.uncert$Year)-1),c("Mean", "LB", "UB")] <- NA
summary(dens.uncert)

# Removing data once the upperbound hits 0
dens.uncert[!is.na(dens.uncert$UB) & dens.uncert$UB<=1e-3, c("Mean", "LB", "UB")] <- NA
summary(dens.uncert)


poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=21), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30))
# Poster Figure
# pdf("figures/Uncertainty_Density_TimeSeries.pdf", height= 8.5, width = 13)
# ggplot(dens.uncert[,]) + #facet_grid(Site ~.) +
  # geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  # geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  # labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Density Uncertainty") + 
  # theme(axis.ticks.length = unit(-0.25, "cm"),
        # axis.ticks.margin = unit(0.5, "cm")) +
  # # add time slice lines
  # geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) +
  # poster.theme
# dev.off()
summary(dens.uncert)
# Publication Figure
# pdf("figures/Uncertainty_Density_TimeSeries.pdf", height= 8.5, width = 13)

ggplot(data=dens.uncert) + facet_wrap(~Site, scale="free") +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  labs(x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")")), title="Density Uncertainty") +
  # add time slice lines
  
  # General Formatting
  	# General Formatting  
	theme(legend.position=c(0.15,0.85)) + 
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))
# dev.off()


save(dens.uncert, file="processed_data/doe_density_uncertainty_comp6.Rdata")
# ------------------------

# ------------------------
# Statistics on Density Uncertainty:
# ------------------------

# ------------------------
