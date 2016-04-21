# Getting numbers for the different components for the results


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
# Period 1980-2007

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
	# Ecology
all.valles.climate.stack[all.valles.climate.stack$elevation=="Upper" & all.valles.climate.stack$chron.type=="Ecology" & !is.na(all.valles.climate.stack$chron.type),]
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
	# Ecology
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Ecology" & !is.na(all.valles.climate.stack$chron.type),]

	# BM
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="BM" & !is.na(all.valles.climate.stack$chron.type),]

	# Climate
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Climate" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Big
all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Big" & !is.na(all.valles.climate.stack$chron.type),]
	
	# Small
	all.valles.climate.stack[all.valles.climate.stack$elevation=="Lower" & all.valles.climate.stack$chron.type=="Small" & !is.na(all.valles.climate.stack$chron.type),]


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
mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VUF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VUF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T)


# VLF

mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "base"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "UB"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T); sd(valles.all.uncert[valles.all.uncert$Site=="VLF" & valles.all.uncert$Year>1980 & valles.all.uncert$type=="total", "LB"], na.rm=T)

mean(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T); sd(valles.all.uncert[valles.all.uncert$type=="total" & valles.all.uncert$Site=="VLF" & valles.all.uncert$Year> 1980 & valles.all.uncert$type=="total", "range" ], na.rm=T)
