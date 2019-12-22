######################
## Haul Bubble plot ##
######################
# x <- data$SHOOTING_LONGITUDE
# y <- data$SHOOTING_LATITUDE
# data <- MEDITS.to.dd(TA)
# variable <- "SHOOTING_DEPTH"
# range <- NA # c(15.5,20,40,42.5)
# interval="equal"
# bubbleplot(x,y,data,variable, interval="equal")

bubbleplot <- function(x,y,data, variable, range=NA,inches=0.1, interval="equal",
                       land=countries, d1 = depth_1, d2= depth_2, d3 = depth_3){

  if (FALSE){
    x=m$SHOOTING_LONGITUDE;y=m$SHOOTING_LATITUDE;data=m; variable="SHOOTING_DEPTH"; range=NA; interval="quantiles";
    land=countries; d1 = depth_1; d2= depth_2; d3 = depth_3
  }
  countries <- land
  depth_1 <- d1
  depth_2 <- d2
  depth_3 <- d3

  summary.grid <- data.frame(x,y,data[,variable])
  colnames(summary.grid) <- c("MEAN_LONGITUDE_DEC","MEAN_LATITUDE_DEC", variable)

  if (is.na(range[1])){
    x_min <- min(summary.grid$MEAN_LONGITUDE_DEC)
    x_max <- max(summary.grid$MEAN_LONGITUDE_DEC)

    y_min <- min(summary.grid$MEAN_LATITUDE_DEC)
    y_max <- max(summary.grid$MEAN_LATITUDE_DEC)

    xdif <- 10*(x_max - x_min)/100
    ydif <- 10*(y_max - y_min)/100
    # range <- c(x_min,x_max,y_min,y_max)
    range <- c((x_min-xdif),(x_max+xdif),(y_min-ydif),(y_max+ydif))
  }

  #----------------------------------------
  # Plotting indices
  #----------------------------------------
  if (interval=="quantiles") {
    cat_lim <- as.numeric(quantile(summary.grid[!is.na(summary.grid[,variable]), variable], probs = seq(0, 1,0.20) ) )
  }
  if (interval=="equal") {
    max_val <- max(summary.grid[!is.na(summary.grid[,variable]), variable])
    break_size <- max_val/5
    cat_lim <- seq(0, max_val, break_size)
  }
  cate_label <-   round(cat_lim,2)
  cate_label_min <- cate_label
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

  summary.grid_to_plot$color[ summary.grid_to_plot[,variable] >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable]) & summary.grid_to_plot[,variable] <=cat_lim[2]] <- palette_cate[1]
  summary.grid_to_plot$color[ summary.grid_to_plot[,variable] >cat_lim[2] & summary.grid_to_plot[,variable] <=cat_lim[3]] <- palette_cate[2]
  summary.grid_to_plot$color[ summary.grid_to_plot[,variable] >cat_lim[3] & summary.grid_to_plot[,variable] <=cat_lim[4]] <- palette_cate[3]
  summary.grid_to_plot$color[ summary.grid_to_plot[,variable] >cat_lim[4] & summary.grid_to_plot[,variable] <=cat_lim[5]] <- palette_cate[4]
  summary.grid_to_plot$color[ summary.grid_to_plot[,variable] >cat_lim[5] & summary.grid_to_plot[,variable] <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable])] <- palette_cate[5]

  summary.grid_to_plot$cate[ summary.grid_to_plot[,variable] >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable]) & summary.grid_to_plot[,variable] <=cat_lim[2]] <- cate_label[1]
  summary.grid_to_plot$cate[ summary.grid_to_plot[,variable] >cat_lim[2] & summary.grid_to_plot[,variable] <=cat_lim[3]] <- cate_label[2]
  summary.grid_to_plot$cate[ summary.grid_to_plot[,variable] >cat_lim[3] & summary.grid_to_plot[,variable] <=cat_lim[4]] <- cate_label[3]
  summary.grid_to_plot$cate[ summary.grid_to_plot[,variable] >cat_lim[4] & summary.grid_to_plot[,variable] <=cat_lim[5]] <- cate_label[4]
  summary.grid_to_plot$cate[ summary.grid_to_plot[,variable] >cat_lim[5] & summary.grid_to_plot[,variable] <max(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable])] <- cate_label[5]

  #summary.grid_to_plot$cate[ summary.grid_to_plot$meanNkm2 >cat_lim[6] ] <- cate_label[6]

  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] >= min(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable]) & summary.grid_to_plot[,variable] <=cat_lim[2]] <- 2
  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] >cat_lim[2] & summary.grid_to_plot[,variable] <=cat_lim[3]] <- 3
  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] >cat_lim[3] & summary.grid_to_plot[,variable] <=cat_lim[4]] <- 4
  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] >cat_lim[4] & summary.grid_to_plot[,variable] <=cat_lim[5]] <- 6
  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] >cat_lim[5] & summary.grid_to_plot[,variable] <=max(summary.grid_to_plot[!is.na(summary.grid_to_plot[,variable]), variable])] <- 7
  summary.grid_to_plot$to_plot[ summary.grid_to_plot[,variable] == 0] <- 0

  plot.new()
  # polygon(c(-min(summary.grid_to_plot[,"MEAN_LONGITUDE_DEC"])^2,-min(summary.grid_to_plot[,"MEAN_LONGITUDE_DEC"])^2,
  #           max(summary.grid_to_plot[,"MEAN_LONGITUDE_DEC"])^2,max(summary.grid_to_plot[,"MEAN_LONGITUDE_DEC"])^2),
  #         c(-min(summary.grid_to_plot[,"MEAN_LATITUDE_DEC"])^2,max(summary.grid_to_plot[,"MEAN_LATITUDE_DEC"])^2,
  #           max(summary.grid_to_plot[,"MEAN_LATITUDE_DEC"])^2,-min(summary.grid_to_plot[,"MEAN_LATITUDE_DEC"])^2), col="#1E90FF")

  lx =range[2] - range[1]
  ly =range[4] - range[3]
  ratio <- ly/lx*1.1
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

  on.exit(c(par(mar=old_par$mar,fin=old_par$fin),options(warn=oldoptions)))
  options(warn=-1)
  par(new=TRUE, mar=c(4, 5, 4, 2), fin=c(img_width,img_height)) #c(bottom, left, top, right)  ,bg = 'blue'
  plot(1,1,type="n",xlim=c(range[1],range[2]), ylim=c(range[3],range[4]) ,  xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"N)")), main=variable)
  #plot(cgpmgrid, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), add=TRUE, border="light grey")                                                                                 #data = centroidi_coords,
  plot(countries, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), border="grey", col="light grey", add=TRUE)
  plot(depth_1, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), col="#C0C0C0", add=TRUE)
  plot(depth_2, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), col="#A9A9A9", add=TRUE)
  plot(depth_3, xlim=c(range[1],range[2]), ylim=c(range[3],range[4]), col="#808080", add=TRUE)

  symbols(summary.grid_to_plot$MEAN_LONGITUDE_DEC[order(summary.grid_to_plot $to_plot, decreasing = TRUE)],
          summary.grid_to_plot$MEAN_LATITUDE_DEC[order(summary.grid_to_plot $to_plot, decreasing = TRUE)],
          circles=as.numeric(as.character(summary.grid_to_plot $to_plot[order(summary.grid_to_plot $to_plot, decreasing = TRUE)])), inches=inches, fg="black",
          bg=summary.grid_to_plot$color[order(summary.grid_to_plot $to_plot, decreasing = TRUE)], add=TRUE)   # inches=1
  points(summary.grid_to_plot$MEAN_LONGITUDE_DEC[summary.grid_to_plot $to_plot==0], summary.grid_to_plot$MEAN_LATITUDE_DEC[summary.grid_to_plot $to_plot==0], pch=4, cex=0.65)   # inches=1 ,
  legend(range[1],range[4], cate_label, col=palette_cate, pch = 19, pt.cex=c(1,2,2.5,3,3.3), title =variable, bty = "n")
  #with(centroidi_coords, text(centroidi_coords$coords.x1 - 0.5, centroidi_coords$coords.x2, labels = centroidi_coords$cgpmgrid_id, pos = 4, cex = 0.2, col="grey"))
  box()


}
