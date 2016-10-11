library(ggplot2)
library(car)
# Scripts for figures to be generated for the uncertainty publication

# Figure 1
# cumulative biomass and the associated areas of uncertainty

# Saving as .Rdata to be recalled later
#save(valles.ind.dev, file="processed_data/ind_uncert_areas_quad.Rdata")
load("processed_data/ind_uncert_areas_quad.Rdata")
summary(valles.ind.dev)

# Updating SiteID names to reflect publication nomenclature
valles.ind.dev$SiteID <- recode(valles.ind.dev$SiteID, "'VUF'='Upper Site';'VLF'='Lower Site'")

valles.ind.dev$SiteID <- factor(valles.ind.dev$SiteID, levels= c("Upper Site", "Lower Site"))
# -----------------------------------
# -----------------------------------

# Plotting up the individual areas of uncertainty that were added in quadrature

# -----------------------------------
# -----------------------------------
# cbbPalette <- c("#CC79A7", "#0072B2", "#E69F00","#009E73")

cbbPalette <- c("#009E73", "#CC79A7", "#0072B2", "#E69F00")

pdf("figures/uncertainty_figures/indiv_uncert_areas_quad.pdf", width=13, height=8.5)
ggplot(valles.ind.dev[valles.ind.dev$Year >= 1980 & valles.ind.dev$Year <=2011,]) + facet_grid(SiteID ~ .) +
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  #1) Increment Uncertainty
  geom_ribbon(aes(x=Year, ymin=Base - LB.inc, ymax= Base + UB.inc, fill="1"), alpha=0.9) +

  #2) Allometric Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=Base - LB.allom, ymax= Base - LB.inc, fill="2"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.allom, ymax=Base + UB.inc, fill="2"), alpha=0.9) +
  
  #3) Density Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=Base - LB.dens, ymax=Base - LB.allom, fill="3"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.dens, ymax=Base + UB.allom, fill="3"), alpha=0.9) +
  
  #4) Mortality Uncertainty -- separate for upper & lower to make things clearer
  geom_ribbon(aes(x=Year, ymin=Base - LB.mort, ymax= Base - LB.dens, fill="4"), alpha=0.9) +
  geom_ribbon(aes(x=Year, ymin=Base + UB.mort, ymax=Base + UB.dens, fill="4"), alpha=0.9) +
  
  # Reiterate mean line for clarity
  geom_line(aes(x=Year, y=Base), size=1.5, color="black") +

  # add time slice lines
  #geom_vline(xintercept=c(1980, 1995, 2011), linetype="dotted") +
  
  # Legend Formatting
  labs(#title= "Quad calculated Stacked Uncertainties", 
  x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")")))) +
  scale_fill_manual(name="Uncertainty", values=cbbPalette, labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.09,0.92)) + 

  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5), face="bold"))+
  poster.theme2+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  scale_x_continuous(expand=c(0,0))

dev.off()



##########################################################################################
##########################################################################################

# Figure 2
# Increment biomass and the contributing areas of uncertainty broken out to be viewed individually


#save(valles.all.uncert, file="processed_data/valles_bm_boot_tot_inc.Rdata")
load("processed_data/valles_bm_boot_tot_inc.Rdata")

summary(valles.all.uncert)

valles.all.uncert$Site <- recode(valles.all.uncert$Site, "'VUF'='Upper Site';'VLF'='Lower Site'")
valles.all.uncert$Site <- factor(valles.all.uncert$Site, levels = c("Upper Site", "Lower Site"))
valles.all.uncert$type <- factor(valles.all.uncert$type, levels = c("inc", "allom", "dens", "mort", "total"))

valles.all.uncert$name <-valles.all.uncert$type
valles.all.uncert$name <- recode(valles.all.uncert$name, "'inc'='Inc. Upscaling';'allom'='Allometric';'dens'='Stand Density';'mort'='Mortality'")
valles.all.uncert$name <- factor(valles.all.uncert$name, levels = c("Inc. Upscaling", "Allometric", "Stand Density", "Mortality", "total"))

# graphing period from 1980-2011 for paper
pdf("figures/uncertainty_figures/bm_inc_uncert_separate.pdf", width=13, height=8.5)
ggplot(data=valles.all.uncert[!valles.all.uncert$type=="total" & valles.all.uncert$Year > 1980,]) + facet_grid(name~Site) +
	
	geom_ribbon(aes(x=Year, ymin= base + LB.dev, ymax= UB.dev + base), fill="darkgrey", alpha=0.6) +
	geom_hline(aes(yintercept=0), linetype="dashed") +	
	geom_line(aes(x=Year, y=base), size=0.5, color="black") +

  #geom_line(aes(x=year, y=mean), size=1.5, color="black") +
	labs(#title= "Biomass Increment Total Uncertainty", 
	x="Year", y=expression(bold(paste("Biomass (kg m" ^ "-2",")")))) +
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))+
  poster.theme2 +
   theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))


dev.off()     

##########################################################################################
##########################################################################################

# Figure 3
# Correlation bar charts of mean BM chronology along with other ecology and climate chronologies
# outside Chronologies: Touchan et al. 2010: upper; Brice et al 2013:Lower
# Correlations from 1980-2007 (common period)
# Critical Value for 28 years (n-2 = 26) 0.330
require(ggalt)

#save(all.valles.climate.stack, file="processed_data/valles_climate_corr_data.Rdata")
load("processed_data/valles_climate_corr_data.Rdata")
summary(all.valles.climate.stack)
all.valles.climate.stack$chron.type <- factor(all.valles.climate.stack$chron.type, levels = c("Big", "Small", "All", "BM", "Climate", "Big", "Small"))

# Leaving out Big and Small Chrons, but they could be recalled if necessary
pdf("figures/uncertainty_figures/climate_chron_seasons_together_yes_size.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack[all.valles.climate.stack$month %in% c("pFall", "Winter", "Spring", "Summer") & all.valles.climate.stack$type %in% c("tmean", "precip"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
    geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type, fill=chron.type, alpha=sig), stat="identity", position="dodge") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_color_manual(values= c("blue", "red","green", "darkgreen", "dodgerblue")) +
	scale_fill_manual(values= c("blue", "red", "green", "darkgreen", "dodgerblue")) +
	scale_alpha_manual(values = c(1, 1))+
  poster.theme2+
	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
	
	labs(#title= "Tree Ring : Climate Correlations", 
	x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

dev.off()

##########################################################################################
##########################################################################################

# Figure 4
# Violin plots of climate correlations of the 100 random pulls from the full range of BM uncertainty
# same significance critical value as above


# save(all.valles.bm.stack, file="processed_data/valles_BM_distrib_corr_data.Rdata")
load("processed_data/valles_BM_distrib_corr_data.Rdata")
summary(all.valles.bm.stack)

all.valles.bm.stack$site <- recode(all.valles.bm.stack$site, "'VUF'='Upper Site';'vlf'='Lower Site'")

all.valles.bm.stack$site <- factor(all.valles.bm.stack$site, levels=c("Upper Site", "Lower Site"))

pdf("figures/uncertainty_figures/BMI_violin_seasons_big_small.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pFall", "Winter","Spring", "Summer") & all.valles.bm.stack$type %in% c("tmean", "precip") & all.valles.bm.stack$size %in% c("big", "small", "all"),]) + facet_grid(site ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, color=size), adjust=4, scale="width") +
	geom_violin(aes(x=month, y=corr, color=size), adjust=4, scale="width") +
	geom_violin(aes(x=month, y=corr, fill=size, alpha=sig), adjust=4, scale="width") +
	
	
	stat_summary(aes(x=month, y=corr, mapping = size), fun.y="mean", geom="point", shape="-", size=20, position=position_dodge(width = 0.9)) +
	
	
	scale_color_manual(values=c("blue", "red", "darkgreen", "purple")) +
	scale_fill_manual(values=c("blue", "red", "darkgreen", "purple")) +
	scale_alpha_manual(values = c(1, 0.4))+
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	 poster.theme2 +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
	
	
	 labs(#title= "Biomass Climate Correlations", 
	 x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))+
	 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

	

dev.off()


########################################################################################### 
#Supplemental Figure 1	
##########################################################################################


#Time series with mean and 95% CI of ring width increment for dated only vs. gapfilled trees

load("processed_data/dated_vs_all_comparison.Rdata")

summary(valles.compare)

cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

valles.compare$site.name <- valles.compare$site
valles.compare$site.name <- recode(valles.compare$site.name, "'VUF'='Upper Site';'VLF'='Lower Site'")

valles.compare$site.name <- factor(valles.compare$site.name, levels=c("Upper Site", "Lower Site"))

pdf("figures/uncertainty_figures/dated_vs_all.pdf", width=13, height=8.5)
ggplot(data=valles.compare[valles.compare$Year >= 1980 & valles.compare$Year <= 2011,]) + facet_grid(site.name~.)+
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=type), alpha=0.4)+
	geom_hline(aes(yintercept=0), linetype="dashed") +
	geom_line(aes(x=Year, y=mean, color=type), size=1.5)+
	#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  #theme(legend.position=c(0.2,0.85)) + 
	theme(legend.position=c(0.9,0.85)) +
  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5), face="bold")) +
  
  labs(#title= "Dated vs. Gap filled", 
  x="Year", y=expression(bold(paste("Ring-width Increment (mm)")))) +
  scale_fill_manual(name="Time Series",values=cbbPalette, labels=c("Dated", "All Trees")) +
  scale_color_manual(name="Time Series",values=cbbPalette, labels=c("Dated", "All Trees")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15)))+
  poster.theme2+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

  
dev.off()


#############################################
##########################################################################################

# Supplemental Figure 2	

load("Valles_plot_bm.Rdata")
summary(bm.plot2)

bm.plot2$site <- bm.plot2$Site..Tower.
bm.plot2$site <- recode(bm.plot2$Site..Tower., "'Valles Caldera Upper'='Upper Site';'Valles Caldera Lower'='Lower Site'")

bm.plot2$site <- factor(bm.plot2$site, levels=c("Upper Site", "Lower Site"))

pdf("figures/uncertainty_figures/valles_plot_bm.pdf", width=13, height=8.5)
ggplot(bm.plot2[bm.plot2$Year >1980 & bm.plot2$Year <= 2011,]) + facet_grid(site~., scales="fixed") +
  geom_ribbon(aes(x=Year, ymin= BM.CI.lo, ymax=BM.CI.hi, fill=Plot), alpha=0.4) +
  geom_line(aes(x=Year, y=BM.Mean, color=Plot), size=1.5) +
  labs(x="Year", y="Biomass (kg m-2)") +
  poster.theme2+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

dev.off()



##########################################################################################
##########################################################################################

# Supplemental Figure 3	
#climate correlations with detrended Biomass time series
# Looking at just the BM and BM.detrend
# No significant differences with the exception of Summer Precip at Upper site adn Spring temp at lower site.

load("processed_data/valles_climate_corr_data_supp3.Rdata")
summary(all.valles.climate.stack)

all.valles.climate.stack$chron.type <- factor(all.valles.climate.stack$chron.type, levels = c("Big", "Small", "All", "BM", "BM.detrend", "Climate"))

# Leaving out Big and Small Chrons, but they could be recalled if necessary
# pdf("figures/uncertainty_figures/climate_chron_seasons_together_yes_size.pdf", width=13, height=8.5)

pdf("figures/uncertainty_figures/detrendedBM_vs_meanBM.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack[all.valles.climate.stack$month %in% c("pFall", "Winter", "Spring", "Summer") & all.valles.climate.stack$type %in% c("tmean", "precip") & all.valles.climate.stack$chron.type %in% c("BM", "BM.detrend"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
    geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type, fill=chron.type, alpha=sig), stat="identity", position="dodge") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_color_manual(values= c("darkgreen", "olivedrab")) +
	scale_fill_manual(values= c("darkgreen", "olivedrab")) +
	scale_alpha_manual(values = c(1, 0.4))+
  poster.theme2+
	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
	
	labs(#title= "Tree Ring : Climate Correlations", 
	x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

dev.off()

