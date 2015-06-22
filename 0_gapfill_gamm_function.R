# -----------------------------------
# Function for simple gapfilling of tree ring data
# This function will return a list with 2 levels: the data with gapfilling
# -----------------------------------

gapfill.gamm <- function(data, DBH="DBH", Species.Use="Species", smooth.by="TreeID", Canopy.Class="Canopy.Class", canopy=F, plot.diagnostics=T, out.prefix){
#measured=T,
# -----------------------------------
# Model Arguments
# -----------------------------------
# data             = Tree ring data set, already subsetted with what you want to fit; 
#                    must have the following columns:
#                        Year 
#                        DBH  == DBH at time of sampling
#                        Species
#                        Site
#                        PlotID
#                        Canopy == Canopy class (Optional)
#                        TreeID (measured == T only)
#                        Species.Model (measured == F only)
# measured         = are we creating a model for trees which have at least some measurements (measured==T) or missing trees (measured==F); indicates whether to fit by 
# canopy           = Use a canopy random effect
# plot.diagnostics = plot the measurements + modeled data for gapfilling
# save.gamm        = save .RData file of the gamm fit
# out.prefix       = file path and naming strategy for the plots and gamm RData file
# smooth.by        = what you're runnign the cubic smoothling spline on; should be by TreeID if you have measurements or by Plot or Species if you don't
# -----------------------------------

# Making generic columns that will work with our model; not very elegant, but it works
data$Canopy       <- data[,Canopy.Class]
data$DBH          <- data[,DBH]
data$Species.Use  <- data[,Species.Use]
data$smooth.by    <- data[,smooth.by]


# Note: the log link with the gaussian family to prevent fitting negative values requires that you can't actually have 0 growth so we're going to make it really really small instead
data$RW <- ifelse(data$RW==0, 1e-6, data$RW)

if(canopy==T){
	gamm.fill <- gamm(log(RW) ~ s(Year, bs="cs", k=3, by=smooth.by) + DBH, random=list(Species.Use=~1, Site=~1, PlotID=~1, Canopy=~1), data= data, na.action=na.omit)
} else {
	gamm.fill <- gamm(log(RW) ~ s(Year, bs="cs", k=3, by=smooth.by) + DBH, random=list(Species.Use=~1, Site=~1, PlotID=~1), data= data, na.action=na.omit)
}

# Making Predicted ring widths
data$RW.modeled <- exp(predict(gamm.fill, data))

# Graphing of the modeled rings we will use to fill the data (note this isn't truncating ones that are past where we think pith actually is)
if(plot.diagnostics==T)
pdf(paste0(out.prefix, "_gapfill.pdf"), height=7.5, width=10)
print(
ggplot() + facet_wrap(~Species) +
	geom_path(aes(x=Year, y=RW, color=Species.Use), data=data, size=0.5) +
	geom_point(aes(x=Year, y=RW.modeled), data[is.na(data$RW),], size=0.5) +
	scale_y_continuous(limits=c(0,1.25)) +
	theme_bw()
)
dev.off()

out <- list()
out[["data"]] <- data
out[["gamm"]] <- gamm.fill
save(out, file=paste0(out.prefix, "_data_gamm.RData"))

return(out)

}