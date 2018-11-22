library(raster)

source("R/shared.R")

dep <- raster("/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/misc/deptho_fx_IPSL-CM5A-LR_1.0deg.nc4")
dep <- rotate(dep)
plot(dep)

lmes <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)
lme_numbers <- sort(lmes$LME_NUMBER)

#testing
lme_numbers <- sample(lme_numbers, 1)

#for each LME
for (lme_number in lme_numbers) {
  selected_lme <- lmes[lmes$LME_NUMBER==lme_number,]
  res <- .cropmask_brick(dep, selected_lme)
}

