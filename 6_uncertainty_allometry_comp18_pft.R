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
library(ggplot2)
library(car)
library(grid)
# Read in the full MCMC-based indivudal tree biomass estimates
load("processed_data/Biomass_Array_Tree_kgm-2_component18.RData")
# ------------------------

# ------------------------
# Get to site level by taking mean of trees (ignoring plots & potential pseudoreplication issues)
#    This mean tree approach is necessary because not all trees in the plot were sampled, so we 
#    can't just sum them
# bm.array==the array we read in above, 
# apply c(1,3) = preserve the dims 1 (years) & 3 (allometric iterations)
# ------------------------
# Subsetting and using the mean allometric biomass estimate to get a biomass/tree
biom.doe <- bm.array

# biom.doe$Year <- as.numeric(dimnames(bm.array)[[1]])
dim(biom.doe)
# summary(biom.doe)
dimnames(biom.doe)[[2]]<- ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="VUF0", paste0("VUA", substr(dimnames(biom.doe)[[2]],4,6)),
                                 ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="VUF1", paste0("VUB", substr(dimnames(biom.doe)[[2]],4,6)),
                                        ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="VLF0", paste0("VLA", substr(dimnames(biom.doe)[[2]],4,6)),
                                               ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="VLF1", paste0("VLB", substr(dimnames(biom.doe)[[2]],4,6)),
                                               		
                                               		ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="HOW1", paste0("HOA", substr(dimnames(biom.doe)[[2]],4,6)),
                                 						ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="HOW2", paste0("HOB", substr(dimnames(biom.doe)[[2]],4,6)),
                                    						ifelse(substr(dimnames(biom.doe)[[2]],1,4)=="HOW3", paste0("HOC", substr(dimnames(biom.doe)[[2]],4,6)),
                                               						paste(dimnames(biom.doe)[[2]]))))))))
dimnames(biom.doe)[[2]] 


# Going from trees to plots (ignoring any tree-level plot level)
# Note: Using a dummy code here so that we bassically have plot #0 and Plot #1
plots <- unique(substr(dimnames(biom.doe)[[2]], 1, 3))


plots <- plots[!plots == "Year"]

# Go from tree biomass (kg/m2) to plot (kg/m2)
biom.plot <- array(dim=c(nrow(biom.doe), length(plots), dim(biom.doe)[3]))
dimnames(biom.plot)[[1]] <- dimnames(biom.doe)[[1]]
dimnames(biom.plot)[[2]] <- c(plots)
dim(biom.plot)

for(p in 1:length(plots)){
	cols.plot <- which(substr(dimnames(biom.doe)[[2]],1,3)==plots[p])
   if(substr(plots[p],1,1)=="V" | substr(plots[p],1,1)=="N"){
	biom.plot[,p,] <- apply(biom.doe[,cols.plot,], c(1,3), FUN=mean, na.rm=T)
		} else {
			biom.plot[,p,] <- apply(biom.doe[,cols.plot,], c(1,3), FUN=sum, na.rm=T)		
		}
}
summary(biom.plot[,,1])






# Go from plot to site while preserving the allometry iterations (currently dim #3)
Site <- unique(substr(dimnames(biom.doe)[[2]], 1, 2))

biom.Site <- list()


for(s in Site){
  cols.site <- which(substr(dimnames(biom.plot)[[2]],1,2)==s)
  biom.Site[[s]] <- apply(biom.plot[,cols.site,], c(1,3), FUN=mean)
}

summary(biom.Site[[1]])
names(biom.Site)<- c("Austin-Cary", "Duke_HW", "Duke_ll", "Howland", "UMBS", "Morgan-Monroe", "Missouri", "Niwot", "Oak_openings", "Savannah_River", "Harvard", "VLF", "VUF")
sites <- c("Austin-Cary", "Duke_HW", "Duke_ll", "Howland", "UMBS", "Morgan-Monroe", "Missouri", "Niwot", "Oak_openings", "Savannah_River", "Harvard", "VLF", "VUF")
# ------------------------

# ------------------------
# Find the allometry-based uncertainty around the mean of trees
# After running the apply above, allometric iterations are in columns
# Here we're producing a 95% CI around the mean
# ------------------------
biom.ci <- list()

for(s in sites){
  biom.ci[[s]] <- data.frame(Year=as.numeric(dimnames(biom.doe)[[1]]), SiteID=s, Mean=rowMeans(biom.Site[[s]], na.rm=T), 
                             LB=apply(biom.Site[[s]],1,quantile, 0.025, na.rm=T), 
                             UB=apply(biom.Site[[s]],1,quantile, 0.975, na.rm=T))
}
biom.ci[[1]][1:10,]

# ------------------------

# ------------------------
# Package everything together, make a quick plot and save it for later use
# ------------------------

ac <- biom.ci[[1]]
summary(ac)
dh <- biom.ci[[2]]
summary(dh)
dl <- biom.ci[[3]]
summary(dl)
how <- biom.ci[[4]]
summary(how)
mi <- biom.ci[[5]]
summary(mi)
mm <- biom.ci[[6]] 
summary(mm)
mo <- biom.ci[[7]]
summary(mo)
niw <- biom.ci[[8]]
summary(niw)
oo <- biom.ci[[9]]
summary(oo)
sr <- biom.ci[[10]]
summary(sr)
har <- biom.ci[[11]]
summary(har)
vlf <- biom.ci[[12]]
summary(vlf)
vuf <- biom.ci[[13]]
summary(vuf)


allom.uncert <- data.frame(rbind(ac, dh, dl, how, mi, mm, mo, niw, oo, sr, har, vlf,vuf))
dim(allom.uncert)
summary(allom.uncert)

# removing last two years from Harvard and Howland as they are not real
allom.uncert[allom.uncert$SiteID %in% c("Howland", "Harvard") & allom.uncert$Year>=(max(allom.uncert$Year)-1),c("Mean", "LB", "UB")] <- NA
summary(allom.uncert)

# Removing data once the upperbound hits 0
allom.uncert[!is.na(allom.uncert$UB) & allom.uncert$UB<=1e-3, c("Mean", "LB", "UB")] <- NA
summary(allom.uncert)



# Poster Format
# pdf("figures/Uncertainty_Allometry.pdf", width= 13, height= 8.5)
# ggplot(allom.uncert[,]) + #facet_grid(Site ~.) +
  # geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5) +
  # geom_line(aes(x=Year, y=Mean, color= Site), size=1.5) + 
  # labs(x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")"))), title="Allometric Uncertainty") + 
  # #theme_bw()
  # theme(axis.ticks.length = unit(-0.25, "cm"),
        # axis.ticks.margin = unit(0.5, "cm")) +
  # # add time slice lines
  # geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted", size=1.5) # +
  # # poster.theme1
# dev.off()

# Publication Format
# pdf("figures/Uncertainty_Allometry.pdf", width= 13, height= 8.5)
ggplot(allom.uncert[,]) +# facet_grid(SiteID~.) +
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=SiteID), alpha=0.5) +
  geom_line(aes(x=Year, y=Mean, color= SiteID), size=1.5) + 
  labs(x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")")), title="Allometric Uncertainty") + 
      
	# General Formatting  
	theme(legend.position=c(0.15,0.85)) + 
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
        panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), 
        axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(vjust=-0.5),  axis.title.y=element_text(size=rel(1.5), vjust=1), 
        plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))

#   theme(strip.text=element_text(size=rel(1.5)))+
#   theme(axis.ticks.length = unit(-0.25, "cm"),
#         axis.ticks.margin = unit(0.5, "cm"))
  
  #poster.theme1
# dev.off()


save(allom.uncert, file="processed_data/doe_allometry_uncertainty_comp18_pft.Rdata")
# ------------------------


# ------------------------
# Statistics on Allometric Uncertainty:
# ------------------------

# ------------------------
