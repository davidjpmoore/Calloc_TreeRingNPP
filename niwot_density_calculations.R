##################################################################
# Calculating Niwot Ridge Tree Densities for the PCQ sampling method
##################################################################

# Read in flurins tree metadata as it has the distance from the point to each tree

niwot.plot.b <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/metadata_niwot_plotB.txt", header=T)
niwot.plot.c <- read.table("~/PhD/Carbon Research/Niwot/Flurin_Niwot/metadata_niwot_plotC.txt", header=T)

summary(niwot.plot.b)
summary(niwot.plot.c)


# Get the mean density for PCQ for each plot
# Formula: 1/ (sum of distances/number of trees)^2

# Plot B

nwb.density <- 1/(sum(niwot.plot.b$dist_to_gridpoint)/144)^2
nwb.density

# Plot C
nwc.density <- 1/(sum(niwot.plot.c$dist_to_gridpoint)/144)^2
nwc.density

niwot.density <- data.frame(plot = c("NWB", "NWC"),
							density = c(nwb.density, nwc.density))
							
niwot.density

# Fancy density Calculation
# Formula: 4(#of trees -1) / pi * (sum of distances)^2

# Plot B

niwot.plot.b$square_dist <- niwot.plot.b$dist_to_gridpoint^2
summary(niwot.plot.b)

nwb.fancy.density <- (4*((4*36)-1)) / (pi * sum(niwot.plot.b$square_dist))
nwb.fancy.density

# Variance in plot B
# Formula: dens^2 / #trees-2

nwb.fancy.var <- nwb.fancy.density^2 / (144-2)
nwb.fancy.var

# Confidence intervals of plot B
# Lower
# Formula: -1.96 + sqrt(16*(number points)-1) / pi * sum of squared distances

nwb.lower <- (-1.96 + sqrt(16*36-1))^2 / (pi *  sum(niwot.plot.b$square_dist))
nwb.lower

# Upper

nwb.upper <- (1.96 + sqrt(16*36-1))^2 / (pi *  sum(niwot.plot.b$square_dist))
nwb.upper







# Plot C

niwot.plot.c$square_dist <- niwot.plot.c$dist_to_gridpoint^2
summary(niwot.plot.c)



nwc.fancy.density <- (4*(144-1)) / (pi * sum(niwot.plot.c$square_dist))
nwc.fancy.density

# Variance in plot C

nwc.fancy.var <- nwc.fancy.density^2 / (144-2)
nwc.fancy.var

# CI of plot C

# Lower
nwc.lower <- (-1.96 + sqrt(16*36-1))^2 / (pi *  sum(niwot.plot.c$square_dist))
nwc.lower


# Upper
nwc.upper <- (1.96 + sqrt(16*36-1))^2 / (pi *  sum(niwot.plot.c$square_dist))
nwc.upper


niwot.density <- data.frame(plot = c("NWB", "NWC"),
							plain.density = c(nwb.density, nwc.density),
							fancy.density = c(nwb.fancy.density, nwc.fancy.density),
							fancy.var = c(nwb.fancy.var, nwc.fancy.var),
							fancy.lower = c(nwb.lower, nwc.lower),
							fancy.upper = c(nwb.upper, nwc.upper))

niwot.density

write.csv(niwot.density, "processed_data/niwot_density.csv", row.names=F)

