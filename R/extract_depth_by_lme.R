library(raster)

source("R/shared.R")

.rasterdep2df <- function(lme_number, dep, lmes){

  selected_lme <- lmes[lmes$LME_NUMBER==lme_number,]
  my_raster <- .cropmask(dep, selected_lme)
  
  included_cells <- my_raster@data@values
  included_cells[!is.na(included_cells)]<-1
  included_cells[is.na(included_cells)]<-0
  
  area_grid <- raster::area(x = my_raster, na.rm=FALSE)
  cell_areas <- included_cells * area_grid@data@values
  
  aw_mean <- sum(cell_areas * my_raster@data@values, na.rm = TRUE) /sum(cell_areas)
  
  df <- data.frame(
    lme = lme_number,
    min = my_raster@data@min,
    max = my_raster@data@max,
    mean = mean(my_raster@data@values, na.rm=TRUE),
    aw_mean = aw_mean
  )
  return(df)
}

dep <- raster("/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/misc/deptho_fx_IPSL-CM5A-LR_1.0deg.nc4")
dep <- rotate(dep)

lmes <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)
lme_numbers <- sort(lmes$LME_NUMBER)

mylist <- lapply(lme_numbers, .rasterdep2df, dep, lmes)

depdf <- dplyr::bind_rows(mylist)

rm(dep, mylist, lmes, lme_numbers)

out_file_name <- "/rd/gem/private/users/justb/gcm_lmescale/lme_depths.rds"
saveRDS(depdf, file=out_file_name)



