
sp.index <- function(TA,TB,sspp, type, range=NA, GSA=NA, country=NA, threshold=NA, #, save=FALSE
                     grid.sf=cgpmgrid,land=countries){

  if (FALSE) {
    sspp = "ARISFOL"
    type <- "abundance"
    range <- c(10, 20, 38, 42)
    range=NA
    save=FALSE
    GSA=NA
    country=NA
    threshold=NA
    grid.sf=cgpmgrid
    land=countries
  }

  cgpmgrid <- grid.sf
  countries <- land

  if (!is.na(GSA)){
    TA <- TA[TA$AREA == GSA , ]
    TB <- TB[TB$AREA == GSA , ]
  }

  if (!is.na(country)){
    TA <- TA[TA$COUNTRY == country , ]
    TB <- TB[TB$COUNTRY == country , ]
  }

metaDB <- TATB.grid(TA,TB,sspp)
GENERE <- as.character(unique(metaDB$GENUS)[unique(metaDB$GENUS) != -1])
SPECIE <- as.character(unique(metaDB$SPECIES)[unique(metaDB$SPECIES) != -1])
sspp <- paste(GENERE,SPECIE, sep="")

years <- (unique(metaDB$YEAR))

if (length(years) < 10) {
  n_years <- length(years)
  firstyear <- years[length(years)-(n_years-1)]
  last_10year <- years[years >= firstyear]
} else {
  firstyear <- years[length(years)-9]
  last_10year <- years[years >= firstyear]
}

metaDB <- metaDB[ metaDB$YEAR %in% last_10year,]

if (is.na(range[1])){
  x_min <- min(metaDB$MEAN_LONGITUDE_DEC)
  x_max <- max(metaDB$MEAN_LONGITUDE_DEC)

  y_min <- min(metaDB$MEAN_LATITUDE_DEC)
  y_max <- max(metaDB$MEAN_LATITUDE_DEC)
  range <- c(x_min,x_max,y_min,y_max)
}

colnames(metaDB)[colnames(metaDB) == "cgpmgrid_id"] <- "CGPMgrid"
colnames(metaDB)[colnames(metaDB) == "kg_km2"] <- "B_km2"
colnames(metaDB)[colnames(metaDB) == "kg_h"] <- "B_h"

# Create Mean Fish Weight by haul
metaDB$MEAN_WEIGHT <- round(metaDB$TOTAL_WEIGHT_IN_THE_HAUL/metaDB$TOTAL_NUMBER_IN_THE_HAUL,3)/1000

if (type == "abundance" | type == "invCV"){
#----------------------------------------
# CALCULATION OF THE ABUNDANCE and saving the tables
#----------------------------------------
nhauls.grid <- aggregate(metaDB$N_km2, by=list(metaDB$CGPMgrid, metaDB$x_center, metaDB$y_center), length) # [,4]
colnames(nhauls.grid) <- c("CGPMgrid", "x_center", "y_center", "nhauls")

cgpmgridlevel <- unique(metaDB$CGPMgrid)
ntab <- data.frame(CGPMgrid = cgpmgridlevel, GSA = unique(metaDB$GSA))
ntab$meanNkm2 <- NA
i=1
for (i in 1:length(cgpmgridlevel)){
  ddd <- metaDB[metaDB$CGPMgrid == cgpmgridlevel[i], ]
  ntab[i,3] <- mean(ddd[!is.na(ddd$N_km2), "N_km2"])
  ssd <- (ddd$N_km2 - ntab[i,3])^2   # sum of squared differences
  variance <- sum(ssd)/(length(ddd$N_km2)-1) # variance
  ntab$sdNkm2[i] <- sqrt(variance)
  ntab$cvNkm2[i] <- sqrt(variance)/ntab[i,3]
  ntab$inverse_cvNkm2[i] <- 1/ntab$cvNkm2[i]
  ntab$nhauls_in_square[i] <- length(ddd[, "HAUL_NUMBER"])
  ntab$nhauls_positive[i] <- length(ddd[ddd$N_km2 > 0, "HAUL_NUMBER"])
}

summary.grid <- merge(ntab, nhauls.grid[,1:3])
colnames(summary.grid) <- c("cgpmgridlevel", "GSA", "meanNkm2", "sdNkm2", "cvNkm2", "inverse_cvNkm2","nhauls","positive_hauls", "lon", "lat" )

abundance_grid <- summary.grid

# Plotting indices
#----------------------------------------
# plot.grid.abundance(summary.grid,metaDB, range,sspp, save)
# source(paste(wd, "/scripts/plotGFCM_Abundance.r", sep=""))
#----------------------------------------
# Saving maps of the ABUNDANCE index
#----------------------------------------                                                                          # [summary.grid$meanNkm2>5]
cat_lim <- as.numeric(quantile(summary.grid[!is.na(summary.grid$meanNkm2), "meanNkm2"], probs = seq(0, 1,0.20) ) )

cate_label <-   round(cat_lim,1)
cate_label_min <- cate_label
#cate_label_min[1] <- 0
cate_label_max <- cate_label[2:length(cate_label)]


cate_label <- paste("(", cate_label_min, ", ", cate_label_max, "]", sep="")
cate_label[1] <- paste("[", cate_label_min[1], ", ", cate_label_max[1], "]", sep="")

#cate_label[length(cate_label)] <- paste(">", cate_label_max[length(cate_label_max)] )
cate_label <- cate_label[1: (length(cate_label)-1)]

palette_cate <- heat.colors(length(cate_label))
palette_cate <- palette_cate[length(palette_cate):1]

summary.grid_to_plot <- summary.grid
summary.grid_to_plot$cate <- "0"

summary.grid_to_plot$color <- "black"
summary.grid_to_plot$to_plot <- 0

summary.grid_to_plot$color[ summary.grid_to_plot$meanNkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"]) & summary.grid_to_plot$meanNkm2 <=cat_lim[2]] <- palette_cate[1]
summary.grid_to_plot$color[ summary.grid_to_plot$meanNkm2 >cat_lim[2] & summary.grid_to_plot$meanNkm2 <=cat_lim[3]] <- palette_cate[2]
summary.grid_to_plot$color[ summary.grid_to_plot$meanNkm2 >cat_lim[3] & summary.grid_to_plot$meanNkm2 <=cat_lim[4]] <- palette_cate[3]
summary.grid_to_plot$color[ summary.grid_to_plot$meanNkm2 >cat_lim[4] & summary.grid_to_plot$meanNkm2 <=cat_lim[5]] <- palette_cate[4]
summary.grid_to_plot$color[ summary.grid_to_plot$meanNkm2 >cat_lim[5] & summary.grid_to_plot$meanNkm2 <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"])] <- palette_cate[5]

summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"]) & summary.grid_to_plot$meanNkm2 <=cat_lim[2]] <- cate_label[1]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >cat_lim[2] & summary.grid_to_plot$meanNkm2 <=cat_lim[3]] <- cate_label[2]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >cat_lim[3] & summary.grid_to_plot$meanNkm2 <=cat_lim[4]] <- cate_label[3]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >cat_lim[4] & summary.grid_to_plot$meanNkm2 <=cat_lim[5]] <- cate_label[4]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >cat_lim[5] & summary.grid_to_plot$meanNkm2 <max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"])] <- cate_label[5]

summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanNkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"]) & summary.grid_to_plot$meanNkm2 <=cat_lim[2]] <- 2
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanNkm2 >cat_lim[2] & summary.grid_to_plot$meanNkm2 <=cat_lim[3]] <- 3
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanNkm2 >cat_lim[3] & summary.grid_to_plot$meanNkm2 <=cat_lim[4]] <- 4
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanNkm2 >cat_lim[4] & summary.grid_to_plot$meanNkm2 <=cat_lim[5]] <- 6
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanNkm2 >cat_lim[5] & summary.grid_to_plot$meanNkm2 <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanNkm2), "meanNkm2"])] <- 7

if (type == "abundance"){
  lx =range[2] - range[1]
  ly =range[4] - range[3]
  ratio <- ly/lx*1.3
  img_width <- par()$fin[1]
  img_height <- img_width*ratio
  oldoptions <- options()$warn
  old_par <- list()
  old_par$mar <-par()$mar
  old_par$fin <-par()$fin

  if (img_height > old_par$fin[2]){
    img_height <- old_par$fin[2]
    img_width  <- img_height/ratio
  }

on.exit(c(par(mar=old_par$mar,fin=old_par$fin), options(warn=oldoptions)))
options(warn=-1)
par( mar=c(4, 5, 4, 2), fin=c(img_width,img_height)) #c(bottom, left, top, right)
plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3],range[4]) ,  xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=paste(paste("Abundance  (n/km^2)", sep=" "), sspp))
plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), border="grey", col="light grey", add=TRUE)

symbols(summary.grid_to_plot$lon[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], summary.grid_to_plot$lat[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], circles=as.numeric(as.character(summary.grid_to_plot $to_plot[order(summary.grid_to_plot $to_plot, decreasing = TRUE)])), inches=0.12, fg="grey", bg=summary.grid_to_plot$color[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], add=TRUE)   # inches=1
points(summary.grid_to_plot$lon[summary.grid_to_plot $to_plot==0], summary.grid_to_plot$lat[summary.grid_to_plot $to_plot==0], pch=4)   # inches=1 ,
legend(range[1],range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title = expression(paste("Abundance ", (n/km^2), sep=" ")), bty = "n")

box()

}
#----------------------------------------
# Saving maps of the inverse of CV of ABUNDANCE index
#----------------------------------------
cat_lim <- quantile(summary.grid$inverse_cvNkm2[!is.na(summary.grid$inverse_cvNkm2)], probs = seq(0, 1,0.20) )
cate_label <-  round(cat_lim,3)

cate_label_min <- cate_label
cate_label_max <- cate_label[2:length(cate_label)]


cate_label <- paste("(", cate_label_min, ", ", cate_label_max, "]", sep="")
cate_label[1] <- paste("[", cate_label_min[1], ", ", cate_label_max[1], "]", sep="")

cate_label <- cate_label[1: (length(cate_label)-1)]

palette_cate <- heat.colors(length(cate_label))
palette_cate <- palette_cate[length(palette_cate):1]

summary.grid_to_plot <- summary.grid
summary.grid_to_plot$cateCV <- "0"

summary.grid_to_plot$colorCV <- "black"
summary.grid_to_plot$to_plotCV <- 0

to_plot <- summary.grid_to_plot[!is.na(summary.grid_to_plot$inverse_cvNkm2) & is.finite(summary.grid_to_plot$inverse_cvNkm2),]
to_plot_NA <- summary.grid_to_plot[is.na(summary.grid_to_plot$inverse_cvNkm2) | !is.finite(summary.grid_to_plot$inverse_cvNkm2),]

to_plot$colorCV[ to_plot$inverse_cvNkm2 >= min(to_plot$inverse_cvNkm2) & to_plot$inverse_cvNkm2 <=cat_lim[2]] <- palette_cate[1]
to_plot$colorCV[ to_plot$inverse_cvNkm2 >cat_lim[2] & to_plot$inverse_cvNkm2 <=cat_lim[3]] <- palette_cate[2]
to_plot$colorCV[ to_plot$inverse_cvNkm2 >cat_lim[3] & to_plot$inverse_cvNkm2 <=cat_lim[4]] <- palette_cate[3]
to_plot$colorCV[ to_plot$inverse_cvNkm2 >cat_lim[4] & to_plot$inverse_cvNkm2 <=cat_lim[5]] <- palette_cate[4]
to_plot$colorCV[ to_plot$inverse_cvNkm2 >cat_lim[5] & to_plot$inverse_cvNkm2 <=max(to_plot$inverse_cvNkm2)] <- palette_cate[5]

to_plot$cateCV[ to_plot$inverse_cvNkm2 >= min(to_plot$inverse_cvNkm2) & to_plot$inverse_cvNkm2 <=cat_lim[2]] <- cate_label[1]
to_plot$cateCV[ to_plot$inverse_cvNkm2 >cat_lim[2] & to_plot$inverse_cvNkm2 <=cat_lim[3]] <- cate_label[2]
to_plot$cateCV[ to_plot$inverse_cvNkm2 >cat_lim[3] & to_plot$inverse_cvNkm2 <=cat_lim[4]] <- cate_label[3]
to_plot$cateCV[ to_plot$inverse_cvNkm2 >cat_lim[4] & to_plot$inverse_cvNkm2 <=cat_lim[5]] <- cate_label[4]
to_plot$cateCV[ to_plot$inverse_cvNkm2 >cat_lim[5] & to_plot$inverse_cvNkm2 <=max(to_plot$inverse_cvNkm2)] <- cate_label[5]

to_plot$to_plotCV[ to_plot$inverse_cvNkm2 >= min(to_plot$inverse_cvNkm2) & to_plot$inverse_cvNkm2 <=cat_lim[2]] <- 2
to_plot$to_plotCV[ to_plot$inverse_cvNkm2 >cat_lim[2] & to_plot$inverse_cvNkm2 <=cat_lim[3]] <- 3
to_plot$to_plotCV[ to_plot$inverse_cvNkm2 >cat_lim[3] & to_plot$inverse_cvNkm2 <=cat_lim[4]] <- 4
to_plot$to_plotCV[ to_plot$inverse_cvNkm2 >cat_lim[4] & to_plot$inverse_cvNkm2 <=cat_lim[5]] <- 6
to_plot$to_plotCV[ to_plot$inverse_cvNkm2 >cat_lim[5] & to_plot$inverse_cvNkm2 <=max(to_plot$inverse_cvNkm2)] <- 7

if (type == "invCV"){
  lx =range[2] - range[1]
  ly =range[4] - range[3]
  ratio <- ly/lx*1.3
  img_width <- par()$fin[1]
  img_height <- img_width*ratio
  oldoptions <- options()$warn
  old_par <- list()
  old_par$mar <-par()$mar
  old_par$fin <-par()$fin

  if (img_height > old_par$fin[2]){
    img_height <- old_par$fin[2]
    img_width  <- img_height/ratio
  }

on.exit(c(par(mar=old_par$mar, fin=old_par$fin), options(warn=oldoptions)))
options(warn=-1)
par( mar=c(4, 5, 4, 2), fin=c(img_width,img_height))
plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=paste("Inverse of Abundance CV -", sspp) )
plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), border="grey", col="light grey", add=TRUE)
symbols(to_plot$lon[order(to_plot $to_plotCV, decreasing = TRUE)], to_plot$lat[order(to_plot $to_plotCV, decreasing = TRUE)], circles=as.numeric(as.character(to_plot $to_plotCV[order(to_plot $to_plotCV, decreasing = TRUE)])), inches=0.12, fg="grey", bg=to_plot$colorCV[order(to_plot $to_plotCV, decreasing = TRUE)], add=TRUE)   # inches=1
points(to_plot_NA$lon, to_plot_NA$lat, pch=4)   # inches=1 ,
legend(range[1],range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title = "Inverse of Abundance CV", bty = "n")

box()

  }
}





if (type == "biomass"){
#----------------------------------------
# CALCULATION OF THE BIOMASS and saving the tables
#----------------------------------------
# metaDB <- TATB.grid(TA,TB,sspp)
nhauls.grid <- aggregate(metaDB$B_km2, by=list(metaDB$CGPMgrid, metaDB$x_center, metaDB$y_center), length) # [,4]
colnames(nhauls.grid) <- c("CGPMgrid", "x_center", "y_center", "nhauls")

cgpmgridlevel <- unique(metaDB$CGPMgrid)
Btab <- data.frame(CGPMgrid = cgpmgridlevel, GSA = unique(metaDB$GSA))
Btab$meanBkm2 <- NA
i=1
for (i in 1:length(cgpmgridlevel)){
  ddd <- metaDB[metaDB$CGPMgrid == cgpmgridlevel[i], ]
  Btab[i,3] <- mean(ddd[!is.na(ddd$B_km2), "B_km2"])
  ssd <- (ddd$B_km2 - Btab[i,3])^2   # sum of squared differences
  variance <- sum(ssd)/(length(ddd$B_km2)-1) # variance
  Btab$sdBkm2[i] <- sqrt(variance)
  Btab$cvBkm2[i] <- sqrt(variance)/Btab[i,3]
  Btab$inverse_cvBkm2[i] <- 1/Btab$cvBkm2[i]
  Btab$nhauls_in_square[i] <- length(ddd[, "HAUL_NUMBER"])
  Btab$nhauls_positive[i] <- length(ddd[ddd$B_km2 > 0, "HAUL_NUMBER"])
}

summary.grid <- merge(Btab, nhauls.grid[,1:3])
colnames(summary.grid) <- c("cgpmgridlevel", "GSA", "meanBkm2", "sdBkm2", "cvBkm2", "inverse_cvBkm2","nhauls","positive_hauls", "lon", "lat" )

biomass_grid <- summary.grid

cat_lim <- quantile(summary.grid[!is.na(summary.grid$meanBkm2), "meanBkm2"], probs = seq(0, 1,0.20) )
cate_label <-  round(cat_lim,1)

cate_label_min <- cate_label
# cate_label_min[1] <- 0
cate_label_max <- cate_label[2:length(cate_label)]


cate_label <- paste("(", cate_label_min, ", ", cate_label_max, "]", sep="")
cate_label[1] <- paste("[", cate_label_min[1], ", ", cate_label_max[1], "]", sep="")

cate_label <- cate_label[1: (length(cate_label)-1)]
# cate_label[length(cate_label)] <- paste(">", cate_label_max[length(cate_label_max)] )


palette_cate <- heat.colors(length(cate_label))
palette_cate <- palette_cate[length(palette_cate):1]

summary.grid_to_plot <- summary.grid
summary.grid_to_plot$cate <- "0"

summary.grid_to_plot$color <- "black"
summary.grid_to_plot$to_plot <- 0

summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"]) & summary.grid_to_plot$meanBkm2 <=cat_lim[2]] <- palette_cate[1]
summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >cat_lim[2] & summary.grid_to_plot$meanBkm2 <=cat_lim[3]] <- palette_cate[2]
summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >cat_lim[3] & summary.grid_to_plot$meanBkm2 <=cat_lim[4]] <- palette_cate[3]
summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >cat_lim[4] & summary.grid_to_plot$meanBkm2 <=cat_lim[5]] <- palette_cate[4]
summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >cat_lim[5] & summary.grid_to_plot$meanBkm2 <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"])] <- palette_cate[5]
#summary.grid_to_plot$color[ summary.grid_to_plot$meanBkm2 >cat_lim[6] ] <- palette_cate[6]

summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"])  & summary.grid_to_plot$meanBkm2 <=cat_lim[2]] <- cate_label[1]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >cat_lim[2] & summary.grid_to_plot$meanBkm2 <=cat_lim[3]] <- cate_label[2]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >cat_lim[3] & summary.grid_to_plot$meanBkm2 <=cat_lim[4]] <- cate_label[3]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >cat_lim[4] & summary.grid_to_plot$meanBkm2 <=cat_lim[5]] <- cate_label[4]
summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >cat_lim[5] & summary.grid_to_plot$meanBkm2 <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"])] <- cate_label[5]
#summary.grid_to_plot$cate[ summary.grid_to_plot$meanBkm2 >cat_lim[6] ] <- cate_label[6]

summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"])  & summary.grid_to_plot$meanBkm2 <=cat_lim[2]] <- 2
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >cat_lim[2] & summary.grid_to_plot$meanBkm2 <=cat_lim[3]] <- 3
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >cat_lim[3] & summary.grid_to_plot$meanBkm2 <=cat_lim[4]] <- 4
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >cat_lim[4] & summary.grid_to_plot$meanBkm2 <=cat_lim[5]] <- 6
summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >cat_lim[5] & summary.grid_to_plot$meanBkm2 <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$meanBkm2), "meanBkm2"])] <- 7
#summary.grid_to_plot$to_plot[ summary.grid_to_plot$meanBkm2 >cat_lim[6] ] <- 8

lx =range[2] - range[1]
ly =range[4] - range[3]
ratio <- ly/lx*1.3
img_width <- par()$fin[1]
img_height <- img_width*ratio
  oldoptions <- options()$warn
old_par <- list()
old_par$mar <-par()$mar
old_par$fin <-par()$fin

if (img_height > old_par$fin[2]){
  img_height <- old_par$fin[2]
  img_width  <- img_height/ratio
}

on.exit(c(par(mar=old_par$mar, fin=old_par$fin), options(warn=oldoptions)))
options(warn=-1)
par( mar=c(4, 5, 4, 2), fin=c(img_width,img_height))
plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=paste("Biomass (Kg/km2) -", sspp) )
plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), border="grey", col="light grey", add=TRUE)
symbols(summary.grid_to_plot$lon[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], summary.grid_to_plot$lat[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], circles=as.numeric(as.character(summary.grid_to_plot $to_plot[order(summary.grid_to_plot $to_plot, decreasing = TRUE)])), inches=0.12, fg="grey", bg=summary.grid_to_plot$color[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], add=TRUE)   # inches=1
points(summary.grid_to_plot$lon[summary.grid_to_plot $to_plot==0], summary.grid_to_plot$lat[summary.grid_to_plot $to_plot==0], pch=4)   # inches=1 ,
legend(range[1],range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title = "Biomass (Kg/km2)", bty = "n")
box()

}



if (type == "MIW"){
#----------------------------------------
# Calculating mean weight
#----------------------------------------

nhauls.grid <- aggregate(metaDB$N_km2, by=list(metaDB$CGPMgrid, metaDB$x_center, metaDB$y_center), length) # [,4]
colnames(nhauls.grid) <- c("CGPMgrid", "x_center", "y_center", "nhauls")

cgpmgridlevel <- unique(metaDB$CGPMgrid)
MIWtab <- data.frame(CGPMgrid = cgpmgridlevel, GSA = unique(metaDB$GSA))
MIWtab$MIW <- NA
i=1
for (i in 1:length(cgpmgridlevel)){
  ddd <- metaDB[metaDB$CGPMgrid == cgpmgridlevel[i], ]
  MIWtab[i,3] <- sum(ddd[!is.na(ddd$MEAN_WEIGHT) & ddd$MEAN_WEIGHT>0, "MEAN_WEIGHT"])/length(ddd[!is.na(ddd$MEAN_WEIGHT) & ddd$MEAN_WEIGHT>0, "MEAN_WEIGHT"])
  ssd <- (ddd[!is.na(ddd$MEAN_WEIGHT) & ddd$MEAN_WEIGHT >0, "MEAN_WEIGHT"] - MIWtab[i,3])^2   # sum of squared differences
  variance <- sum(ssd)/(length(ddd[!is.na(ddd$MEAN_WEIGHT) & ddd$MEAN_WEIGHT>0, "MEAN_WEIGHT"])-1) # variance
  MIWtab$sdNkm2[i] <- sqrt(variance)
  MIWtab$cvNkm2[i] <- sqrt(variance)/MIWtab[i,3]
  MIWtab$inverse_cvNkm2[i] <- 1/MIWtab$cvNkm2[i]
  MIWtab$nhauls_in_square[i] <- length(ddd[, "HAUL_NUMBER"])
  MIWtab$nhauls_positive[i] <- length(ddd[!is.na(ddd$N_km2) & ddd$N_km2 > 0, "HAUL_NUMBER"])
}

summary.grid <- merge(MIWtab, nhauls.grid[,1:3])
colnames(summary.grid) <- c("cgpmgridlevel", "GSA", "MIW", "sdMIW", "cvMIW", "inverse_cvMIW","nhauls","positive_hauls", "lon", "lat" )

meanWEIGHT_grid <- summary.grid

cat_lim <- as.numeric(quantile(summary.grid[!is.na(summary.grid$MIW), "MIW"], probs = seq(0, 1,0.20) ) )

cate_label <-   round(cat_lim,3)
cate_label_min <- cate_label
#cate_label_min[1] <- 0
cate_label_max <- cate_label[2:length(cate_label)]


cate_label <- paste("(", cate_label_min, ", ", cate_label_max, "]", sep="")
cate_label[1] <- paste("[", cate_label_min[1], ", ", cate_label_max[1], "]", sep="")

#cate_label[length(cate_label)] <- paste(">", cate_label_max[length(cate_label_max)] )
cate_label <- cate_label[1: (length(cate_label)-1)]

palette_cate <- heat.colors(length(cate_label))
palette_cate <- palette_cate[length(palette_cate):1]

summary.grid_to_plot <- summary.grid
summary.grid_to_plot$cate <- "0"

summary.grid_to_plot$color <- "black"
summary.grid_to_plot$to_plot <- 0

summary.grid_to_plot$color[ summary.grid_to_plot$MIW >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"]) & summary.grid_to_plot$MIW <=cat_lim[2]] <- palette_cate[1]
summary.grid_to_plot$color[ summary.grid_to_plot$MIW >cat_lim[2] & summary.grid_to_plot$MIW <=cat_lim[3]] <- palette_cate[2]
summary.grid_to_plot$color[ summary.grid_to_plot$MIW >cat_lim[3] & summary.grid_to_plot$MIW <=cat_lim[4]] <- palette_cate[3]
summary.grid_to_plot$color[ summary.grid_to_plot$MIW >cat_lim[4] & summary.grid_to_plot$MIW <=cat_lim[5]] <- palette_cate[4]
summary.grid_to_plot$color[ summary.grid_to_plot$MIW >cat_lim[5] & summary.grid_to_plot$MIW <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"])] <- palette_cate[5]
#summary.grid_to_plot$color[ summary.grid_to_plot$MIW >cat_lim[6] ] <- palette_cate[6]

summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"]) & summary.grid_to_plot$MIW <=cat_lim[2]] <- cate_label[1]
summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >cat_lim[2] & summary.grid_to_plot$MIW <=cat_lim[3]] <- cate_label[2]
summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >cat_lim[3] & summary.grid_to_plot$MIW <=cat_lim[4]] <- cate_label[3]
summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >cat_lim[4] & summary.grid_to_plot$MIW <=cat_lim[5]] <- cate_label[4]
summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >cat_lim[5] & summary.grid_to_plot$MIW <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"])] <- cate_label[5]
#summary.grid_to_plot$cate[ summary.grid_to_plot$MIW >cat_lim[6] ] <- cate_label[6]

summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"]) & summary.grid_to_plot$MIW <=cat_lim[2]] <- 2
summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >cat_lim[2] & summary.grid_to_plot$MIW <=cat_lim[3]] <- 3
summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >cat_lim[3] & summary.grid_to_plot$MIW <=cat_lim[4]] <- 4
summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >cat_lim[4] & summary.grid_to_plot$MIW <=cat_lim[5]] <- 6
summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >cat_lim[5] & summary.grid_to_plot$MIW <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot$MIW), "MIW"])] <- 7
#summary.grid_to_plot$to_plot[ summary.grid_to_plot$MIW >cat_lim[6] ] <- 8

lx =range[2] - range[1]
ly =range[4] - range[3]
ratio <- ly/lx*1.3
img_width <- par()$fin[1]
img_height <- img_width*ratio
oldoptions <- options()$warn
old_par <- list()
old_par$mar <-par()$mar
old_par$fin <-par()$fin

if (img_height > old_par$fin[2]){
  img_height <- old_par$fin[2]
  img_width  <- img_height/ratio
}

on.exit(c(par(mar=old_par$mar, fin=old_par$fin), options(warn=oldoptions)))
options(warn=-1)
par( mar=c(4, 5, 4, 2), fin=c(img_width,img_height))
plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3],range[4]),  xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=paste(paste("Mean Individual Weight  (kg)", sep=" "), sspp))
plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), border="grey", col="light grey", add=TRUE)
symbols(summary.grid_to_plot$lon[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], summary.grid_to_plot$lat[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], circles=as.numeric(as.character(summary.grid_to_plot $to_plot[order(summary.grid_to_plot $to_plot, decreasing = TRUE)])), inches=0.12, fg="grey", bg=summary.grid_to_plot$color[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], add=TRUE)   # inches=1
points(summary.grid_to_plot$lon[summary.grid_to_plot $to_plot==0], summary.grid_to_plot$lat[summary.grid_to_plot $to_plot==0], pch=4)   # inches=1 ,
legend(range[1],range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title = expression(paste("MIW ", (kg), sep=" ")), bty = "n")
box()

}

if (type == "sex ratio"){
  #----------------------------------------
  # Calculating sex ratio
  #----------------------------------------

  if (!is.na(threshold)){
    metaDBnew <- metaDB[metaDB$YEAR %in% last_10year & metaDB$TOTAL_NUMBER_IN_THE_HAUL > threshold, ]  #
  } else{
    if (is.na(threshold)){
      metaDBnew <- metaDB[metaDB$YEAR %in% last_10year, ]  #
    }
  }
  grid_id <- unique(metaDBnew$CGPMgrid)
  sex_ratio <- data.frame(matrix(NA, nrow=length(grid_id), ncol=1))
  colnames(sex_ratio) <- c("cgpmgrid_id")
  sex_ratio[,1] <-grid_id

  i=2
  for (i in 1:length(grid_id)){
    dd <- metaDBnew[metaDBnew$CGPMgrid == grid_id[i], ]
    dd$nkm2_F <- dd$NB_OF_FEMALES/dd$SWEPT_AREA
    dd$nkm2_M <- dd$NB_OF_MALES/dd$SWEPT_AREA
    dd$nkm_FM <- (dd$NB_OF_MALES + dd$NB_OF_FEMALES)/dd$SWEPT_AREA

    nkm_F <- sum(dd$nkm2_F)
    nkm_M <- sum(dd$nkm2_M)
    nkm_FM <- sum(dd$nkm_FM)
    sex_ratio$GSA[i] <- unique(dd$GSA)
    sex_ratio$x_center[i] <- unique(dd$x_center)
    sex_ratio$y_center[i] <- unique(dd$y_center)
    sex_ratio$ratio[i] <- nkm_F/nkm_FM
  }

  summary.grid <- sex_ratio

  #----------------------------------------
  # Saving maps of the sex-ratio by grid
  #----------------------------------------
  cat_lim <- quantile(sex_ratio$ratio[!is.na(sex_ratio$ratio)], probs = seq(0, 1,0.20) )
  # centroidi <- data.frame(readOGR(paste(getwd(), "/scripts/utilities/gfcm-centroidi/gfcm-centroidi.shp", sep=""), verbose=F))
  centroidi_coords <- data.frame(summary.grid$cgpmgrid_id , summary.grid$x_center , summary.grid$y_center)
  colnames(centroidi_coords) <- c("cgpmgrid_id", "lon", "lat")

  cate_label <-  round(cat_lim,3)

  cate_label_min <- cate_label
  cate_label_max <- cate_label[2:length(cate_label)]


  cate_label <- paste("(", cate_label_min, ", ", cate_label_max, "]", sep="")
  cate_label[1] <- paste("[", cate_label_min[1], ", ", cate_label_max[1], "]", sep="")

  #cate_label[length(cate_label)] <- paste(">", cate_label_max[length(cate_label_max)] )
  cate_label <- cate_label[1: (length(cate_label)-1)]

  palette_cate <- heat.colors(length(cate_label))
  palette_cate <- palette_cate[length(palette_cate):1]

  sex_ratio <- merge(sex_ratio, centroidi_coords)
  summary.grid_to_plot <- sex_ratio
  summary.grid_to_plot$cateCV <- "0"

  summary.grid_to_plot$colorCV <- "black"
  summary.grid_to_plot$to_plotCV <- 0

  to_plot <- summary.grid_to_plot[!is.na(summary.grid_to_plot$ratio) & is.finite(summary.grid_to_plot$ratio),]
  to_plot_NA <- summary.grid_to_plot[is.na(summary.grid_to_plot$ratio) | !is.finite(summary.grid_to_plot$ratio),]

  to_plot$colorCV[ to_plot$ratio >= min(to_plot$ratio) & to_plot$ratio <=cat_lim[2]] <- palette_cate[1]
  to_plot$colorCV[ to_plot$ratio >cat_lim[2] & to_plot$ratio <=cat_lim[3]] <- palette_cate[2]
  to_plot$colorCV[ to_plot$ratio >cat_lim[3] & to_plot$ratio <=cat_lim[4]] <- palette_cate[3]
  to_plot$colorCV[ to_plot$ratio >cat_lim[4] & to_plot$ratio <=cat_lim[5]] <- palette_cate[4]
  to_plot$colorCV[ to_plot$ratio >cat_lim[5] & to_plot$ratio <=max(to_plot$ratio)] <- palette_cate[5]
  #to_plot$color[ to_plot$meanNkm2 >cat_lim[6] ] <- palette_cate[6]

  to_plot$cateCV[ to_plot$ratio >= min(to_plot$ratio) & to_plot$ratio <=cat_lim[2]] <- cate_label[1]
  to_plot$cateCV[ to_plot$ratio >cat_lim[2] & to_plot$ratio <=cat_lim[3]] <- cate_label[2]
  to_plot$cateCV[ to_plot$ratio >cat_lim[3] & to_plot$ratio <=cat_lim[4]] <- cate_label[3]
  to_plot$cateCV[ to_plot$ratio >cat_lim[4] & to_plot$ratio <=cat_lim[5]] <- cate_label[4]
  to_plot$cateCV[ to_plot$ratio >cat_lim[5] & to_plot$ratio <=max(to_plot$ratio)] <- cate_label[5]
  #to_plot$cate[ to_plot$meanNkm2 >cat_lim[6] ] <- cate_label[6]

  to_plot$to_plotCV[ to_plot$ratio >= min(to_plot$ratio) & to_plot$ratio <=cat_lim[2]] <- 2
  to_plot$to_plotCV[ to_plot$ratio >cat_lim[2] & to_plot$ratio <=cat_lim[3]] <- 3
  to_plot$to_plotCV[ to_plot$ratio >cat_lim[3] & to_plot$ratio <=cat_lim[4]] <- 4
  to_plot$to_plotCV[ to_plot$ratio >cat_lim[4] & to_plot$ratio <=cat_lim[5]] <- 6
  to_plot$to_plotCV[ to_plot$ratio >cat_lim[5] & to_plot$ratio <=max(to_plot$ratio)] <- 7

  lx =range[2] - range[1]
  ly =range[4] - range[3]
  ratio <- ly/lx*1.3
  img_width <- par()$fin[1]
  img_height <- img_width*ratio
  oldoptions <- options()$warn
  old_par <- list()
  old_par$mar <-par()$mar
  old_par$fin <-par()$fin

  if (img_height > old_par$fin[2]){
    img_height <- old_par$fin[2]
    img_width  <- img_height/ratio
  }

  on.exit(c(par(mar=old_par$mar, fin=old_par$fin), options(warn=oldoptions)))
  options(warn=-1)
  par(mar=c(4, 5, 4, 2), fin=c(img_width,img_height)) #c(bottom, left, top, right)
  plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=paste("sex ratio -", sspp) )
  plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
  plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3], range[4]), border="grey", col="light grey", add=TRUE)
  symbols(to_plot$lon[order(to_plot $to_plotCV, decreasing = TRUE)], to_plot$lat[order(to_plot $to_plotCV, decreasing = TRUE)], circles=as.numeric(as.character(to_plot $to_plotCV[order(to_plot $to_plotCV, decreasing = TRUE)])), inches=0.12, fg="grey", bg=to_plot$colorCV[order(to_plot $to_plotCV, decreasing = TRUE)], add=TRUE)   # inches=1
  points(to_plot_NA$lon, to_plot_NA$lat, pch=4)   # inches=1 ,
  legend(range[1], range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title = "sex ratio", bty = "n")
  box()

}

return(summary.grid)
}
