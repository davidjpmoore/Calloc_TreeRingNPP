########################################
# Some Diagnostic scatterplots to walk through the data
########################################

# Load in timeseries/chronologies
load("processed_data/valles_combined_climate_chronologies.rdata")

summary(valles.climate.cr)
head(valles.climate.cr)

# Taking just the residual chronologies
valles.res <- valles.climate.cr[,substr(names(valles.climate.cr),5,7)=="res"]
summary(valles.res)
valles.res$cm.res <- valles.climate.cr$cm.n

summary(valles.res)
row.names(valles.res)

# Loading in the mean biomass time series for each site.
load("processed_data/valles_base_increment.Rdata")
summary(valles.base.inc)
valles.base.inc <- valles.base.inc[valles.base.inc$Year >=1980 & valles.base.inc$Year <=2007,]
head(valles.base.inc)
valles.base.inc <- valles.base.inc[order(valles.base.inc$Year, decreasing=F),]




upper.chron.bm <- lm(valles.climate.cr$vuf.res ~ valles.base.inc$vuf.base)
summary(upper.chron.bm)
plot(valles.climate.cr$vuf.res ~ valles.base.inc$vuf.base)
	abline(upper.chron.bm)


lower.chron.bm <- lm(valles.climate.cr$vlf.res ~ valles.base.inc$vlf.base)
summary(lower.chron.bm)
plot(valles.climate.cr$vlf.res ~ valles.base.inc$vlf.base)
	abline(lower.chron.bm)

plot(vuf.tmax[, "Summer"] ~ vuf.tmax[,"Year"], type="l", ylim=c(0,30))
	lines(vlf.tmax$Summer ~ vuf.tmax$Year, col="red")

plot(vuf.precip[, "Summer"] ~ vuf.precip[,"Year"], type="l")
	lines(vlf.precip$Summer ~ vuf.precip$Year, col="blue")


# PLot the ring widths by the biomass
load("processed_data/")