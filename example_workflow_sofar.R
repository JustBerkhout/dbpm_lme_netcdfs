library(raster)
#load nc file into raster::brick object and plot first layer
my_brick <- raster::brick("/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/rcp26/ipsl-cm5a-lr_rcp26_lphy_zint_monthly_200601_21001231.nc4")
plot(my_brick[[1]]) #plots the firstlayer of the world lphy brick



#load LME shape file
#plot all LMEs in 11 colours
LMEs <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)
ElevenColours = RColorBrewer::brewer.pal(11, "Set3")
plot(LMEs, col=ElevenColours[LMEs$LME_NUMBER%%11], border="white", lwd=1)

#pick a random LME and plot it
iLME <- sample(nrow(LMEs), 1)
iLME <- 26
selectedLME <- LMEs[LMEs$LME_NUMBER==iLME,]
plot(selectedLME, lwd=1)

#crop the brick by the polygon
cropped_brick <- raster::crop(my_brick, selectedLME) 
masked_brick <-  raster::mask(cropped_brick, selectedLME) 
# masking appears necessary, e.g. when the selected LME = 39

#plot 1st layer of masked brick, in polygon extent, with polygon overlay
plot(extent(selectedLME), type="n", asp=1, xaxt='n', yaxt='n', ann=FALSE)
#plot(selectedLME, lwd=1, add = TRUE)
plot(my_brick[[1]], col=colorspace::sequential_hcl(1), legend=FALSE, add=TRUE)
plot(masked_brick[[1]], legend=FALSE, add=TRUE) #plots the first layer of the selectedLME cropped lphy brick
plot(selectedLME, lwd=1, add = TRUE)



#this logic needs vetting:
# iLME = 13  (humboldt current, appears empty)
# LME spanning dateline, break, I think
# for some LMEs this error occurs:
# > cropped_brick <- raster::crop(my_brick, selectedLME) 
#  Error in .local(x, y, ...) : extents do not overlap

