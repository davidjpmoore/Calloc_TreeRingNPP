# Getting numbers for the different components for the results
load("processed_data/vuf_tree_data.Rdata")
load("processed_data/vlf_tree_data.Rdata")

load("processed_data/vuf_big_trees.Rdata")
load("processed_data/vuf_small_trees.Rdata")

load("processed_data/vlf_big_trees.Rdata")
load("processed_data/vlf_small_trees.Rdata")

# DBH ranges

# VUF all trees
summary(vuf.tree.data)
	# DBH range: 6.8 - 53.6 cm; n = 101

# VUF big trees
summary(vuf.tree.data[vuf.tree.data$TreeID %in% vuf.big.trees,])
	# DBH range: 34.8 - 53.6 cm; n=11 

# VUF small trees
summary(vuf.tree.data[vuf.tree.data$TreeID %in% vuf.small.trees,])	
	# DBH range: 6.8 - 10cm n = 11

# VLF all trees
summary(vlf.tree.data)
	# DBH range: 10.2 - 40.5 cm; n = 100
# VLF big trees
summary(vlf.tree.data[vlf.tree.data$TreeID %in% vlf.big.trees,])
	# DBH range: 34.5 - 40.5 cm; n = 10

# VLF small trees
summary(vlf.tree.data[vlf.tree.data$TreeID %in% vlf.small.trees,])
	# DBH range: 10.2 - 13.1; n = 10

#----------------------------------------------------
#----------------------------------------------------

# Increment uncertainty percentages for 1980-2011
valles.percents <- read.csv("processed_data/valles_percent_contrib_BMI.csv", header=T)

summary(valles.percents)

# Allometry
mean(valles.percents[valles.percents$type=="allom" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="allom" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])


mean(valles.percents[valles.percents$type=="allom" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="allom" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])

# Increment
mean(valles.percents[valles.percents$type=="inc" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="inc" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])


mean(valles.percents[valles.percents$type=="inc" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="inc" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])



# Density
mean(valles.percents[valles.percents$type=="dens" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="dens" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])


mean(valles.percents[valles.percents$type=="dens" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="dens" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])



# Mortality
mean(valles.percents[valles.percents$type=="mort" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="mort" & valles.percents$Site=="VUF" & valles.percents$Year > 1980, "perc.uncert.parts"])


mean(valles.percents[valles.percents$type=="mort" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])
sd(valles.percents[valles.percents$type=="mort" & valles.percents$Site=="VLF" & valles.percents$Year > 1980, "perc.uncert.parts"])

###########################################################
############################################################

# Cumulative percentages
# Period 1980-2011

###########################################################
###########################################################
valles.cum.percents <- read.csv("processed_data/valles_cumulative_uncert_percentages.csv", header=T)
summary(valles.cum.percents)
valles.cum.percents <- valles.cum.percents[valles.cum.percents$Year < 2012,]

# Allometry
mean(valles.cum.percents[valles.cum.percents$type=="allom" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="allom" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)


mean(valles.cum.percents[valles.cum.percents$type=="allom" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="allom" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)

# Increment
mean(valles.cum.percents[valles.cum.percents$type=="inc" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="inc" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)


mean(valles.cum.percents[valles.cum.percents$type=="inc" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="inc" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)



# Density
mean(valles.cum.percents[valles.cum.percents$type=="dens" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="dens" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)


mean(valles.cum.percents[valles.cum.percents$type=="dens" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="dens" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)



# Mortality
mean(valles.cum.percents[valles.cum.percents$type=="mort" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="mort" & valles.cum.percents$Site=="VUF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)


mean(valles.cum.percents[valles.cum.percents$type=="mort" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)
sd(valles.cum.percents[valles.cum.percents$type=="mort" & valles.cum.percents$Site=="VLF" & valles.cum.percents$Year > 1980, "perc.uncert.parts"], na.rm=T)



###########################################################
############################################################

# Correlation numbers for the different chronologies
# Period 1980-2007

###########################################################
###########################################################

load("processed_data/valles_climate_plus_chron_stack.rdata")

summary(all.valles.climate.stack)


# Upper Site
	# All
all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="All" & !is.na(all.valles.climate.stack$chron.type),]
	# BM
all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="BM" & !is.na(all.valles.climate.stack$chron.type),]

	# Climate
all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="Climate" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Big
all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="Big" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Small
	all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="Small" & !is.na(all.valles.climate.stack$chron.type),]


#--------------------------------------------------------------------------------

# Lower Site
	# All
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="All" & !is.na(all.valles.climate.stack$chron.type),]

	# BM
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="BM" & !is.na(all.valles.climate.stack$chron.type),]

	# Climate
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Climate" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Big
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Big" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Small
	all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Small" & !is.na(all.valles.climate.stack$chron.type),]


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Getting numbers for the detrended biomass time series

load("processed_data/valles_climate_corr_data_supp3b.Rdata") 
# same file as in the #20 script, but had to rename "...supp3b" due to naming issues with all.valles.climate.stack

summary(all.valles.climate.stack2)
# VUF
all.valles.climate.stack2[all.valles.climate.stack2$elevation=="Upper" & all.valles.climate.stack2$chron.type=="BM.detrend" & !is.na(all.valles.climate.stack2$chron.type),]
# Biggest diff in Summer Precip: BM.detrend=0.236 vs BM=0.405


# VLF
# BM.detrend
all.valles.climate.stack2[all.valles.climate.stack2$elevation=="Lower" & all.valles.climate.stack2$chron.type=="BM.detrend" & !is.na(all.valles.climate.stack2$chron.type),]

# Spring is biggest diff BM.detrend = -0.48 vs BM=-0.33

###########################################################
############################################################

# Range of cumulative biomass
# Period 1980-2007

###########################################################
###########################################################
valles.ranges <- read.csv("processed_data/valles_total_uncert_ranges.csv", header=T)

summary(valles.ranges)
valles.ranges <- valles.ranges[valles.ranges$Year < 2012,]

load("processed_data/total_uncert_quad.Rdata")
summary(valles.tot.dev)
valles.tot.dev <- valles.tot.dev[valles.tot.dev$Year < 2012,]

# Comparison of range of uncertainty vs. the mean uncertainty
# 1980
# VUF
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==1980, "Base"]
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==1980, "UB"]
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==1980, "LB"]
valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VUF" & valles.ranges$Year==1980, "range.dev" ]






# VLF
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==1980, "Base"]
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==1980, "UB"]
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==1980, "LB"]
valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VLF" & valles.ranges$Year==1980, "range.dev" ]




# 2011
# VUF
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==2011, "Base"]
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==2011, "UB"]
valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year==2011, "LB"]
valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VUF" & valles.ranges$Year==2011, "range.dev" ]






# VLF
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==2011, "Base"]
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==2011, "UB"]
valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year==2011, "LB"]
valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VLF" & valles.ranges$Year==2011, "range.dev" ]


# Mean and SD of the base, CI, and ranges from 1980-2011

# VUF
mean(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "Base"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "Base"], na.rm=T)

mean(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "UB"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "UB"], na.rm=T)

mean(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "LB"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VUF" & valles.tot.dev$Year>1980 , "LB"], na.rm=T)

mean(valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VUF" & valles.ranges$Year> 1980, "range.dev" ], na.rm=T); sd(valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VUF" & valles.ranges$Year> 1980, "range.dev" ], na.rm=T)


# VLF

mean(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "Base"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "Base"], na.rm=T)

mean(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "UB"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "UB"], na.rm=T)

mean(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "LB"], na.rm=T); sd(valles.tot.dev[valles.tot.dev$SiteID=="VLF" & valles.tot.dev$Year>1980 , "LB"], na.rm=T)

mean(valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VLF" & valles.ranges$Year> 1980, "range.dev" ], na.rm=T); sd(valles.ranges[valles.ranges$type=="total" & valles.ranges$SiteID=="VLF" & valles.ranges$Year> 1980, "range.dev" ], na.rm=T)

#--------------------------------------------------------------------
# Mean and SD of ranges of diff. sources of uncertainty from 1980-2011

# Allometry

mean(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "allom", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "allom", "range.dev"], na.rm=T)

mean(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "allom", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "allom", "range.dev"], na.rm=T)

# TR increment
mean(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "inc", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "inc", "range.dev"], na.rm=T)

mean(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "inc", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "inc", "range.dev"], na.rm=T)


# Stem Density

mean(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "dens", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "dens", "range.dev"], na.rm=T)

mean(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "dens", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "dens", "range.dev"], na.rm=T)

# Mortaltiy

mean(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "mort", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VUF" & valles.ranges$Year>1980 & valles.ranges$type %in% "mort", "range.dev"], na.rm=T)

mean(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "mort", "range.dev"], na.rm=T); sd(valles.ranges[valles.ranges$SiteID=="VLF" & valles.ranges$Year>1980 & valles.ranges$type %in% "mort", "range.dev"], na.rm=T)

###########################################################
############################################################

# Range of Incremental biomass
# Period 1980-2007

###########################################################
###########################################################

load("processed_data/valles_bm_boot_tot_inc.Rdata")
summary(valles.all.uncert)

valles.all.uncert$UB <- valles.all.uncert$UB.dev + valles.all.uncert$base
valles.all.uncert$LB <- valles.all.uncert$base + valles.all.uncert$LB.dev

test <- valles.all.uncert$UB - valles.all.uncert$LB
summary(test)
summary(valles.all.uncert)

plot(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$type=="total", "LB"] ~ valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$type=="total", "Year"], type="l", ylim=range(valles.all.uncert[,c("UB", "LB")], na.rm=T)) 
lines(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$type=="total", "UB"] ~ valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$type=="total", "Year"], type="l")


# Stats of the BM increment and the increment uncertainty

# 1980
# VUF
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "base"]
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "UB"]
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "LB"]
valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "range"]






# VLF
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "base"]
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "UB"]
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "LB"]
valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==1980 & valles.all.uncert$type=="total", "range" ]




# 2011
# VUF
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "base"]
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "UB"]
valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "LB"]
valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "range" ]



# VLF
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "base"]
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "UB"]
valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "LB"]
valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year==2011 & valles.all.uncert$type=="total", "range" ]


# Mean and SD of the base, CI, and ranges from 1980-2011

# VUF
# Mean
mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T)

# UB
mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T)

# LB
mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T)

# Range
mean(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T)


# VLF

# Mean
mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T)

# UB
mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T)

# LB
mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T)

# Range
mean(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980 , "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)


# Mean and SD for different sources of uncertainty

# Allometry

mean(valles.all.uncert[valles.all.uncert$type=="allom" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="allom" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="allom" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="allom" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)


# TR Increment

mean(valles.all.uncert[valles.all.uncert$type=="inc" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="inc" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="inc" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="inc" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

# Stem Density

mean(valles.all.uncert[valles.all.uncert$type=="dens" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="dens" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="dens" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="dens" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

# Mortality

mean(valles.all.uncert[valles.all.uncert$type=="mort" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="mort" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="mort" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="mort" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980, "range" ], na.rm=T)