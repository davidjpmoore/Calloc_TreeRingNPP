##############################################################
# Some basic time series tests on chronologies and mean biomass
##############################################################

load("processed_data/all_valles_chron_combined.Rdata")
summary(valles.res)
ncol(valles.res)

test1 <- acf(valles.res$vuf.res, lag.max=10, type="correlation", plot=T)

test8 <- acf(valles.res$vuf.base, lag.max=10, type="correlation", plot=T)

test2 <- acf(valles.res$vlf.res, lag.max=10, type="correlation", plot=T)

test9 <- acf(valles.res$vlf.base,lag.max=10, type="correlation", plot=T)

test3 <- acf(valles.res$bcw.res, lag.max=10, type="correlation", plot=T)

test4 <- acf(valles.res$cat.res, lag.max=10, type="correlation", plot=T)

test5 <- acf(valles.res$chg.res, lag.max=10, type="correlation", plot=T)

test6 <- acf(valles.res$vuf.mean.res, lag.max=10, type="correlation", plot=T)

test7 <- acf(valles.res$vlf.mean.res, lag.max=10, type="correlation", plot=T)






valles.ccf <- list(c(test1, test2, test3. test4, test5, test6, test7, test8, test9))

