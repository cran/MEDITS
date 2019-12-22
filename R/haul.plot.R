haul.plot <- function(TA, GSA =NA, country=NA, year=NA, type="m", labels=FALSE, land=countries, format="MEDITS") {
countries <- land
if (format=="MEDITS"){
  data <- MEDITS.to.dd(TA)
} else {
  if (format=="degrees"){
    data <- TA
  }
}
if (!is.na(GSA)){
  data <- data[data$AREA %in% GSA,]
}

if (!is.na(country)){
  data <- data[data$COUNTRY %in% country,]
}

if (is.na(year[1])){
x_lim <- c(min(c(data$SHOOTING_LONGITUDE,data$HAULING_LONGITUDE)), max(c(data$SHOOTING_LONGITUDE,data$HAULING_LONGITUDE)))
y_lim <- c(min(c(data$SHOOTING_LATITUDE,data$HAULING_LATITUDE)),   max(c(data$SHOOTING_LATITUDE,data$HAULING_LATITUDE)))

if (type=="b"){
start_coord <- data
end_coord <- data
coordinates(start_coord) <- ~ SHOOTING_LONGITUDE + SHOOTING_LATITUDE
coordinates(end_coord) <- ~ HAULING_LONGITUDE + HAULING_LATITUDE

plot(1, type="n", xlab="Longitude (deg)", ylab="Latitude (deg)", xlim=c(x_lim), ylim=c(y_lim))
plot(countries, add=TRUE, col="grey")
plot(start_coord, add=TRUE, pch=16, col="green", cex=0.7)
plot(end_coord, add=TRUE, pch=16, col="red", cex=0.7)
if(labels){
lab <- as.data.frame(start_coord)
text(lab$SHOOTING_LONGITUDE,lab$SHOOTING_LATITUDE, labels=lab$HAUL_NUMBER, cex=0.7, pos=4)
}
}

if (type=="m"){
  coord <- data
  coord$X <- (coord$SHOOTING_LONGITUDE + coord$HAULING_LONGITUDE)/2
  coord$Y <- (coord$SHOOTING_LATITUDE + coord$HAULING_LATITUDE)/2
  coordinates(coord) <- ~ X + Y

  plot(1, type="n", xlab="Longitude (deg)", ylab="Latitude (deg)", xlim=c(x_lim), ylim=c(y_lim))
  plot(countries, add=TRUE, col="grey")
  plot(coord, add=TRUE, pch=16, col="blue", cex=0.7)
  if(labels){
  lab <- as.data.frame(coord)
  text(lab$X,lab$Y, labels=lab$HAUL_NUMBER, cex=0.7, pos=4)
  }

}
} # chiusura if(is.na(year))

else {   # !is.na(year)
  year <- as.numeric(year)
  data <- data[data$YEAR %in% year,]
  x_lim <- c(min(c(data$SHOOTING_LONGITUDE,data$HAULING_LONGITUDE)), max(c(data$SHOOTING_LONGITUDE,data$HAULING_LONGITUDE)))
  y_lim <- c(min(c(data$SHOOTING_LATITUDE,data$HAULING_LATITUDE)), max(c(data$SHOOTING_LATITUDE,data$HAULING_LATITUDE)))

  if (type=="b"){
    start_coord <- data
    end_coord <- data
    coordinates(start_coord) <- ~ SHOOTING_LONGITUDE + SHOOTING_LATITUDE
    coordinates(end_coord) <- ~ HAULING_LONGITUDE + HAULING_LATITUDE

    plot(1, type="n", xlab="Longitude (deg)", ylab="Latitude (deg)", xlim=c(x_lim), ylim=c(y_lim))
    plot(countries, add=TRUE, col="grey")
    plot(start_coord, add=TRUE, pch=16, col="green", cex=0.7)
    plot(end_coord, add=TRUE, pch=16, col="red", cex=0.7)
    if(labels){
    lab <- as.data.frame(start_coord)
    text(lab$SHOOTING_LONGITUDE,lab$SHOOTING_LATITUDE, labels=lab$HAUL_NUMBER, cex=0.7, pos=4)
    }
    }

  if (type=="m"){
    coord <- data
    coord$X <- (coord$SHOOTING_LONGITUDE + coord$HAULING_LONGITUDE)/2
    coord$Y <- (coord$SHOOTING_LATITUDE + coord$HAULING_LATITUDE)/2
    coordinates(coord) <- ~ X + Y

    plot(1, type="n", xlab="Longitude (deg)", ylab="Latitude (deg)", xlim=c(x_lim), ylim=c(y_lim))
    plot(countries, add=TRUE, col="grey")
    plot(coord, add=TRUE, pch=16, col="blue", cex=0.7)
    if(labels){
    lab <- as.data.frame(coord)
    text(lab$X,lab$Y, labels=lab$HAUL_NUMBER, cex=0.7, pos=4)
    }
  }
}

}




