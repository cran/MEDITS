### dd.to.MEDITS function trasforms coordinates in dd.ddd format to MEDITS format

dd.to.MEDITS <- function(data)  {

  deg <- floor(data$SHOOTING_LATITUDE)
  dec <- data$SHOOTING_LATITUDE - (deg)
  dd  <- (deg*100)+(dec*60)
  data$SHOOTING_LATITUDE <- abs(dd)

  ##
  deg <- floor(data$SHOOTING_LONGITUDE)
  dec <- data$SHOOTING_LONGITUDE - (deg)
  dd  <- (deg*100)+(dec*60)
  data$SHOOTING_LONGITUDE <- abs(dd)

  ##
  deg <- floor(data$HAULING_LATITUDE)
  dec <- data$HAULING_LATITUDE - (deg)
  dd  <- (deg*100)+(dec*60)
  data$HAULING_LATITUDE <- abs(dd)

  ##
  deg <- floor(data$HAULING_LONGITUDE)
  dec <- data$HAULING_LONGITUDE - (deg)
  dd  <- (deg*100)+(dec*60)
  data$HAULING_LONGITUDE <- abs(dd)

  return(data)
}

