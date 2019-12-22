land.points <- function(TA, land=countries, verbose=TRUE) {
  countries <- land
  data <- MEDITS.to.dd(TA)
  start_coord <- data
  end_coord <- data
  coordinates(start_coord) <- ~ SHOOTING_LONGITUDE + SHOOTING_LATITUDE
  coordinates(end_coord) <- ~ HAULING_LONGITUDE + HAULING_LATITUDE
  # proj4string(countries) <- CRS("+proj=longlat")
  proj4string(start_coord) <- proj4string(countries)
  proj4string(end_coord) <- proj4string(countries)
  res_start <-over(countries,start_coord)
  res_start <- res_start[!is.na(res_start$HAUL_NUMBER),]
  res_end <- over(countries, end_coord)
  res_end <- res_end[!is.na(res_end$HAUL_NUMBER),]
  l_start <- length(res_start[,1])
  l_end <- length(res_end[,1])

  if (verbose){
  if(l_start > 0) {
    message("Check the shooting coordinates in the following hauls:\n")
  }
  if(l_end > 0) {
    message("Check the hauling coordinates in the following hauls:\n")
  }
    } # verbose
  if (l_start > 0 & l_end > 0 ){
  results <- list(res_start, res_end)
  } else {
    if(l_start > 0){ results <- res_start} else {
      if(l_end > 0){ results <- res_end}
    }
  }
  if (l_start == 0 & l_end == 0 ){
    results <- NA
    if (verbose){
    message("\nNone of the coordinates is on the land\n")
    }
    }
  return(results)
}

# land.points(TA)
