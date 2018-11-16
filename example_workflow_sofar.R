#load nc file into raster::brick object and plot first layer
my_brick <- raster::brick("/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/rcp26/ipsl-cm5a-lr_rcp26_lphy_zint_monthly_200601_21001231.nc4")
plot(my_brick[[1]]) #plots the firstlayer of the world lphy brick


#load LME shape file
LMEs <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)

#plot all LMEs in 11 colours
ElevenColours = RColorBrewer::brewer.pal(11, "Set3")
plot(LMEs, col=ElevenColours[LMEs$LME_NUMBER%%11], border="white", lwd=1)

#pick a random LME and plot it
iLME <- sample(nrow(LMEs), 1)
selectedLME <- LMEs[iLME,]
plot(selectedLME, col=ElevenColours[iLME%%11], lwd=1)

#crop the brick by the polygon
cropped_brick <- raster::crop(my_brick, selectedLME) 
masked_brick <-  raster::mask(cropped_brick, LMEs[iLME,]) 
# masking appears necessary, e.g. when the selected LME = 39

#plot 1st layer of msked brick and sleected LME polygon on fitting extent
#I think this needs projections logic
ext = merge(extent(masked_brick),extent(selectedLME))
plot(ext, type="n")
plot(masked_brick[[1]], legend=FALSE, add=TRUE) #plots the first layer of the selectedLME cropped lphy brick
plot(selectedLME, lwd=1, add=TRUE) #plots (overlay) the LME polygon over the gri cells


#this logic needs vetting:
# iLME = 13  (humboldt current, appears empty)
# LME spanning dateline, break, I think
# for some LMEs this error occurs:
# > cropped_brick <- raster::crop(my_brick, selectedLME) 
#  Error in .local(x, y, ...) : extents do not overlap
