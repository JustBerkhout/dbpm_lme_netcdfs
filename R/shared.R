.cropmask <- function(src, msk){
  cropped_brick <- raster::crop(src, msk) 
  masked_brick <-  raster::mask(cropped_brick, msk)   
  return(masked_brick)
}