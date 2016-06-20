library(ggplot2)
library(car)
load(file="processed_data/gamm_weights/gam1_weights.Rdata")

summary(gam1.weights)
factors.fits <- c("fit.tmean", "fit.precip", "fit.dbh.recon", "fit.full", "BA.inc")
factors.weights <- c("weight.tmean", "weight.dbh.recon", "weight.precip")

# Transforming things back to BA.inc rather than log
gam1.weights[,which(substr(names(gam1.weights),1,3)=="fit")] <- exp(gam1.weights[,which(substr(names(gam1.weights),1,3)=="fit")] )

othervars <- c("Year", "Site", "group", "Model")

data.graph1 <- aggregate(gam1.weights[,factors.fits], by = gam1.weights[,othervars], FUN= mean, na.rm=T)

data.graph1[,paste(factors.fits, "upr", sep=".")] <- aggregate(gam1.weights[,factors.fits], by = gam1.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.fits]

data.graph1[,paste(factors.fits, "lwr", sep=".")] <- aggregate(gam1.weights[,factors.fits], by = gam1.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.fits]

summary(data.graph1)

data.graph2 <- aggregate(abs(gam1.weights[,factors.weights]), by = gam1.weights[,othervars], FUN= mean, na.rm=T)

data.graph2[,paste(factors.weights, "upr", sep=".")] <- aggregate(abs(gam1.weights[,factors.weights]), by = gam1.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "lwr", sep=".")] <- aggregate(abs(gam1.weights[,factors.weights]), by = gam1.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.weights]

summary(data.graph2)

data.graph <- merge(data.graph1, data.graph2, all.x=T, all.y=T)

# data.graph <- gam1.weights[gam1.weights$TreeID== "MMA014",]
summary(data.graph)
gam1.weights$wts.check <- rowSums(abs(gam1.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))
data.graph$wts.check <- rowSums(abs(data.graph[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))

summary(gam1.weights)
summary(data.graph)

# Ordering the data for graphing

data.graph<- data.graph[order(data.graph$Year, data.graph$group, data.graph$Site, decreasing=F),]
summary(data.graph)

plot.rgb <- function(STATE, SPP, SIZE){	geom_line(data=data.graph[data.graph$State==STATE & data.graph$group==SPP,],aes(x=Year, y=fit.full), size=SIZE,
  		        color=rgb(abs(data.graph[data.graph$State==STATE & data.graph$group==SPP,"weight.tmean"     ]), # red
                        abs(data.graph[data.graph$State==STATE & data.graph$group==SPP,"weight.dbh.recon"     ]), # green
                        abs(data.graph[data.graph$State==STATE & data.graph$group==SPP,"weight.precip"   ]))) }   # blue

# Plotting the Obs and modeled with influence coloring
data.graph$State <- recode(data.graph$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

summary(data.graph[!data.graph$group %in% c("BETULA", "CARYA", "FAGR", "FRAX", "SAAL"),])

pdf("figures/gam1_Species_BAI_limiting_factor_ACER.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$group %in% "ACRU" & data.graph$State %in% "ME",]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	plot.rgb("ME", "ACRU", 3) +
	
	labs(title= "Species", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	poster.theme2
dev.off()