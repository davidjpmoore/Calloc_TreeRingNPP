##############################################################
# Some basic time series tests on chronologies and mean biomass
##############################################################

load("processed_data/all_valles_chron_combined.Rdata")
summary(valles.res)
ncol(valles.res)


pdf("figures/autocorrelation_analysis.pdf", width=13, height=8.5)
par(mfrow=c(2,3))

test1 <- acf(valles.res$vuf.res, lag.max=10, type="correlation", plot=T, main="Upper Ecology")

test8 <- acf(valles.res$vuf.base, lag.max=10, type="correlation", plot=T, main="Upper BM")

test3 <- acf(valles.res$bcw.res, lag.max=10, type="correlation", plot=T, main = "Upper Climate")

test2 <- acf(valles.res$vlf.res, lag.max=10, type="correlation", plot=T, main = "Lower Ecology")

test9 <- acf(valles.res$vlf.base,lag.max=10, type="correlation", plot=T, main = "Lower BM")


test4 <- acf(valles.res$cat.res, lag.max=10, type="correlation", plot=T, main = "Lower Climate1")

# test5 <- acf(valles.res$chg.res, lag.max=10, type="correlation", plot=T, main = "Lower Climate2")

# test6 <- acf(valles.res$vuf.mean.res, lag.max=10, type="correlation", plot=T)

# test7 <- acf(valles.res$vlf.mean.res, lag.max=10, type="correlation", plot=T)
dev.off()





valles.ccf <- list(c(test1, test2, test3. test4, test5, test6, test7, test8, test9))

