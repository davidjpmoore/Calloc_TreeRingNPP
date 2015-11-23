# Merging allometry and density for Francesc for AGU2015

load(file="processed_data/valles_allometry_uncertainty_AGU2015.Rdata")
load(file="processed_data/valles_density_uncertainty_AGU2015.Rdata")

summary(allom.uncert)
summary(dens.uncert)

allom.uncert$type <- as.factor("allom")
dens.uncert$type <- as.factor("dens")

agu2015 <- rbind(allom.uncert, dens.uncert)
summary(agu2015)

agu2015$LB.dev <- agu2015$LB - agu2015$Mean
agu2015$UB.dev <-  agu2015$UB - agu2015$Mean

summary(agu2015)

# Creating a base dataframe with the mean biomass per site per year
agu2015.final <- aggregate(agu2015$Mean, by=list(agu2015$Site, agu2015$SiteID, agu2015$Year), FUN=mean, na.rm=F)
names(agu2015.final) <- c("Site", "SiteID", "Year", "Mean")
summary(agu2015.final)

# Adding the lower and upperbounds of the TOTAL uncertainty to our agu2015.final dataframe
agu2015.final[,c("LB.dev", "UB.dev")] <- aggregate(agu2015[,c("LB.dev", "UB.dev")], by=list(agu2015$Site, agu2015$SiteID, agu2015$Year), FUN=sum, na.rm=F)[,c(4,5)]
summary(agu2015.final)

# Getting the actual lower and upper uncertainty bounds of biomass estimates
agu2015.final$LB <- agu2015.final$Mean + agu2015.final$LB.dev
agu2015.final$UB <- agu2015.final$Mean + agu2015.final$UB.dev
agu2015.final$type <- as.factor("combined")
summary(agu2015.final)

# Reordering the columns to match the individual uncertainties
agu2015.final <- agu2015.final[,names(agu2015)]
summary(agu2015.final)

# Adding individual uncertainty types into the final dataframe
agu2015.final <- rbind(agu2015.final, agu2015)
summary(agu2015.final)

library(ggplot2)
library(grid)
ggplot(agu2015.final) + facet_grid(type ~.)+
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Site), alpha=0.5)+
	geom_line(aes(x=Year, y=Mean, color=Site), size=1.5)+
	
	labs(title= "Total Uncertainty", x="Year", y=expression(paste("Aboveground Biomass (kg m"^"-2",")"))) +
	
	  theme(axis.line=element_line(color="black", size=0.5), 
	        panel.grid.major=element_blank(), 
	        panel.grid.minor= element_blank(), 
	        panel.border= element_blank(), 
	        panel.background= element_blank(), 
	        axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), 
	        axis.text.y=element_text(color="black", size=rel(1.5)), 
	        axis.title.x=element_text(vjust=-0.5, size=rel(1.5)),  
	        axis.title.y=element_text(size=rel(1.5), vjust=1), 
	        plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines")) +

  theme(strip.text=element_text(size=rel(1.5)))+
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks.margin = unit(0.5, "cm")) 