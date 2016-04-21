# Scripts for figures to be generated for the uncertainty publication

# Figure 1
# cumulative biomass and the associated areas of uncertainty

# Saving as .Rdata to be recalled later
#save(valles.ind.dev, file="processed_data/ind_uncert_areas_quad.Rdata")
load("processed_data/ind_uncert_areas_quad.Rdata")
summary(valles.ind.dev)

# -----------------------------------
# -----------------------------------

# Plotting up the individual areas of uncertainty that were added in quadrature

# -----------------------------------
# -----------------------------------
cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

pdf("figures/indiv_uncert_areas_quad.pdf", width=13, height=8.5)
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
  labs(title= "Quad calculated Stacked Uncertainties", x="Year", y=expression(bold(paste("Aboveground Biomass (kg m"^"-2",")")))) +
  scale_fill_manual(name="Uncertainty", values=cbbPalette, labels=c("Increment", "Allometry", "Plot Density", "Mortality")) +
  guides(fill=guide_legend(override.aes=list(alpha=0.15))) +
#  theme(legend.position=c(0.2,0.85), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.25)))  + 
  theme(legend.position=c(0.2,0.85)) + 

  # General Plot formatting
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5), face="bold"))
dev.off()



##########################################################################################
##########################################################################################

# Figure 2
# Increment biomass and the contributing areas of uncertainty broken out to be viewed individually


#save(valles.all.uncert, file="processed_data/valles_bm_boot_tot_inc.Rdata")
load("processed_data/valles_bm_boot_tot_inc.Rdata")

summary(valles.all.uncert)
valles.all.uncert$Site <- factor(valles.all.uncert$Site, levels = c("VUF", "VLF"))
# graphing period from 1980-2011 for paper
pdf("figures/bm_inc_uncert_separate.pdf", width=13, height=8.5)
ggplot(data=valles.all.uncert[!valles.all.uncert$type=="total" & valles.all.uncert$Year > 1980,]) + facet_grid(type~Site) +
	
	geom_ribbon(aes(x=Year, ymin= base + LB.dev, ymax= UB.dev + base), fill="darkgrey", alpha=0.6) +
	
	geom_line(aes(x=Year, y=base), size=0.5, color="black") +
  #geom_line(aes(x=year, y=mean), size=1.5, color="black") +
	labs(title= "Biomass Increment Total Uncertainty", x="Year", y=expression(bold(paste("Biomass (kg m" ^ "-2)")))) +
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +
  
  theme(strip.text=element_text(size=rel(1.5), face="bold"))


dev.off()     

##########################################################################################
##########################################################################################

# Figure 3
# Correlation bar charts of mean BM chronology along with other ecology and climate chronologies
# outside Chronologies: Touchan et al. 2010: upper; Brice et al 2013:Lower
# Correlations from 1980-2007 (common period)
# Critical Value for 28 years (n-2 = 26) 0.330


#save(all.valles.climate.stack, file="processed_data/valles_climate_corr_data.Rdata")
load("processed_data/valles_climate_corr_data.Rdata")
summary(all.valles.climate.stack)

# Leaving out Big and Small Chrons, but they could be recalled if necessary
pdf("figures/climate_chron_seasons_together_no_size.pdf", width=13, height=8.5)
ggplot(data=all.valles.climate.stack.short[all.valles.climate.stack.short$month %in% c("pFall", "Winter", "Spring", "Summer") & !all.valles.climate.stack.short$chron.type %in% c("Big", "Small"),]) + 
	facet_grid(elevation ~ type, scales="free_x")+
    geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type), stat="identity", position="dodge", fill=NA) + 
	geom_bar(aes(x=month, y=corr, color=chron.type, fill=chron.type, alpha=sig), stat="identity", position="dodge") + 
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	geom_hline(yintercept=0, linetype="solid") +
	scale_color_manual(values= c("green", "darkgreen", "dodgerblue")) +
	scale_fill_manual(values= c("green", "darkgreen", "dodgerblue")) +
	scale_alpha_manual(values = c(1, 0.4))+
	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
	
	labs(title= "Tree Ring : Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()

##########################################################################################
##########################################################################################

# Figure 4
# Violin plots of climate correlations of the 100 random pulls from the full range of BM uncertainty
# same significance critical value as above


# save(all.valles.bm.stack, file="processed_data/valles_BM_distrib_corr_data.Rdata")
load("processed_data/valles_BM_distrib_corr_data.Rdata")
summary(all.valles.bm.stack)

pdf("figures/BMI_violin_seasons.pdf", width=13, height=8.5)
ggplot(data=all.valles.bm.stack[all.valles.bm.stack$month %in% c("pFall", "Winter","Spring", "Summer"),]) + facet_grid(site*elevation ~ type , scales="free_x")+
	geom_violin(aes(x=month, y=corr, fill=sig), adjust=2.5) +
	stat_summary(aes(x=month, y=corr), fun.y="median", geom="point", shape="-", size=20) +
	scale_fill_manual(values=c("green","gray50")) +
	geom_hline(yintercept=0, linetype="solid") +
	geom_hline(yintercept=0.374, linetype="dashed") +
	geom_hline(yintercept=-0.374, linetype="dashed") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
	
	
	 labs(title= "Biomass Climate Correlations", x="Seaons", y=expression(bold(paste("Correlation Value (r)")))) #+
     
dev.off()





