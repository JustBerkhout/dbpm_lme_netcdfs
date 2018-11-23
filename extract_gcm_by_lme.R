
# as soon as possible
# dataframe
# from old GCM data
# for hist and rcp 26, 45, 60, 85
# zint
#lphy, sphy
# for each timestep
# 1 layer (x,y)
# associate bathymetric depth (find)
# spatial average with cell size weight
source("R/shared.R")

.get_filename <- function(sce, var){
  #/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/historical/ipsl-cm5a-lr_historical_lphy_zint_monthly_195001_200512.nc4
  #/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/rcp26/ipsl-cm5a-lr_rcp26_lphy_zint_monthly_200601_21001231.nc4
  #SST --> _to_zs
  #SBT --> _to_zb
  
  zthing <- "zint"
  
  if (var=="sbt"){
    zthing <- "zb"
    var <- "to"
  }
  
  if (var=="sst"){
    zthing <- "zs"
    var <- "to"
  }
  
  period <- ifelse(sce=="historical", "195001_200512", "200601_21001231")
  
  my_path_pattern <- "/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/%s/ipsl-cm5a-lr_%s_%s_%s_monthly_%s.nc4"
  
  file_name <- sprintf(my_path_pattern, sce, sce, var, zthing, period)
  
  assertthat::assert_that(file.exists(file_name), msg= sprintf("%s does not exist", file_name))
  
  return(file_name)
}

.brick2df <- function(brick, sce, var, lme){
  
  timesteps <- 1:raster::nlayers(brick)
  
  included_cells <- brick$X0@data@values
  included_cells[!is.na(included_cells)]<-1
  included_cells[is.na(included_cells)]<-0
  
  area_grid <- raster::area(x = brick, na.rm=FALSE)
  cell_areas <- included_cells * area_grid@data@values

  results <- sapply(timesteps, function(x, ca, br) {sum(ca * br[[x]]@data@values, na.rm = TRUE) /sum(ca)}, cell_areas, brick)
  
  df <- data.frame(
    ts = timesteps, 
    scenario = rep(sce, length(timesteps)), 
    variable = rep(var, length(timesteps)),
    lme = rep(lme, length(timesteps)),
    value = results)
  return(df)
}

#testing with reduced data
# scenarios <- sample(scenarios, 1)
# variables <- sample(variables, 1)


.worker <- function(sce, var, output_path){
  #load LME shape file
  lmes <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)
  lme_numbers <- sort(lmes$LME_NUMBER)  
  out_file_name <- sprintf("%s/%s_%s.rds", output_path, sce, var)
  if(!file.exists(out_file_name)) {
    list_out <- list()
    i <- 1        
    my_file <- .get_filename(sce, var)
    my_brick <- raster::brick(my_file)
    my_brick <- raster::rotate(my_brick) #change coords from 0 to 360 to -180 to 180
    for (lme_number in lme_numbers) {
      selected_lme <- lmes[lmes$LME_NUMBER==lme_number,]
      res <- .cropmask(my_brick, selected_lme)
      list_out[[i]]  <- .brick2df(res, sce, var, lme_number)
      i <- i + 1
    }#lme_number
    df_out <- dplyr::bind_rows(list_out)
    saveRDS(df_out, file=out_file_name)
    rm(list_out)
  }
  
}


main <- function(){
    my_working_path <- "/rd/gem/private/users/justb/gcm_lmescale_v2"
  
    scenarios <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
    variables <- c("lphy", "sphy", "sbt", "sst")
  
    g <- expand.grid(scenarios=scenarios, variables=variables)
    
    cl <- parallel::makeCluster(10)
    parallel::clusterExport(cl, c(".brick2df", ".cropmask", ".get_filename") )
    results <- parallel::clusterMap(cl, .worker, sce=g$scenarios, var=g$variables, output_path=my_working_path )
    
    parallel::stopCluster(cl)
}

main()



