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





