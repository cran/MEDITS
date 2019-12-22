# sspp =NA
# GSA=NA
#
# buffer =NA
# save = FALSE
# stage ="recruits"
# country=NA
# wm=wmap
# land=countries
# d1=depth_1
# d2=depth_2
# d3=depth_3
# bubble.rs(TA, TB, TC, GSA=10, sspp ="ARISFOL", stage ="recruits", cutoff=24, buffer =5000, country ="ITA", save =FALSE)

# if(getRversion() >= "3.0.0")  utils::globalVariables(c("."))

bubble.rs <- function(TA, TB, TC, GSA=NA, sspp =NA, stage ="recruits",
                      cutoff=NA, buffer =NA, country =NA,wm=wmap,  #  save =FALSE, folder=NA,
                      land=countries, d1=depth_1, d2=depth_2, d3=depth_3) {

  if(FALSE){
    TA=TA
    TB=TB
    TC=TC
    GSA=NA
    sspp=NA
    stage ="recruits"
    cutoff=29
    buffer=NA
    country=NA
    # save=TRUE
    # folder="C:"
    wm=wmap
    land=countries
    d1=depth_1
    d2=depth_2
    d3=depth_3
  }

  # if (save){
  #   if(is.na(folder)){folder <- tempdir()} else {folder=folder}
  # }

  wmap <- wm
  countries <- land
  depth_1 <- d1
  depth_2 <- d2
  depth_3 <- d3

    if (is.na(sspp)){
      genus <- unique(TB$GENUS)
      species <- unique(TB$SPECIES)
      sspp <- paste(genus, species,sep="")
    }
    if (is.na(GSA)){
      GSA <- unique(TA$AREA)
    }
    TA_filtered <- TA[TA$AREA==GSA, ]
    TB_filtered <- TB[TB$AREA==GSA & TB$GENUS == substr(sspp,1,4) & TB$SPECIES == substr(sspp,5,7),]
    TC_filtered <- TC[TC$AREA==GSA & TC$GENUS == substr(sspp,1,4) & TC$SPECIES == substr(sspp,5,7),]

    metaDB <- m.TATB(TA_filtered,TB_filtered, sspp)
    merge_TATC <- m.TATC(TA_filtered,TC_filtered,sspp)

    if (exists("cutoff")){
                threshold <- cutoff
    }

    ###############################

    if (stage == "all.stages"){

      ddd_r <- NA
      ddd_r <- merge_TATC[ , ]
      pivot_r <- aggregate(as.numeric(as.character(ddd_r$N_km2)), by=list(ddd_r$id, ddd_r$MEAN_LONGITUDE_DEC, ddd_r$MEAN_LATITUDE_DEC), sum)
      colnames(pivot_r) <- c("id", "lon", "lat", "indices")
      loc <- pivot_r
      br <- as.numeric(summary(loc$indices)[-4])
      labels <- as.character(round(as.numeric(summary(loc$indices)[-4]),3))
      ###############
      wmap_laea<- spTransform(wmap, CRS("+proj=laea"))
      countries_laea<-spTransform(countries, CRS("+proj=laea"))
      depth_1_laea<-spTransform(depth_1, CRS("+proj=laea"))
      depth_2_laea<-spTransform(depth_2, CRS("+proj=laea"))
      depth_3_laea<-spTransform(depth_3, CRS("+proj=laea"))

      coordinates(loc)<-c("lon","lat")
      proj4string(loc) <- CRS("+proj=longlat")
      loc_laea<-spTransform(loc, CRS("+proj=laea"))

      wmap_laea_df<-ggplot2::fortify(wmap_laea)
      countries_laea_df<-ggplot2::fortify(countries_laea)
      depth_1_laea_df<-ggplot2::fortify(depth_1_laea)
      depth_2_laea_df<-ggplot2::fortify(depth_2_laea)
      depth_3_laea_df<-ggplot2::fortify(depth_3_laea)

      loc_laea_df<-data.frame(loc_laea)

      theme_opts<-list(ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                      panel.grid.major = ggplot2::element_blank(),
                                      panel.background = ggplot2::element_rect(fill = 'light blue',linetype="solid",color="black"),
                                      plot.background = ggplot2::element_rect(fill="white",
                                                                              size=1,linetype="solid",color="black"),
                                      axis.line = ggplot2::element_blank(),
                                      axis.text.x = ggplot2::element_blank(),
                                      axis.text.y = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(),
                                      axis.title.x = ggplot2::element_blank(),
                                      axis.title.y = ggplot2::element_blank(),
                                      plot.title = ggplot2::element_text(size=22)))

      xmin<-min(loc_laea_df$lon)
      xmax<-max(loc_laea_df$lon)
      ymin<-min(loc_laea_df$lat)
      ymax<-max(loc_laea_df$lat)
      if (is.na(buffer)){
        buffer <- 5000
      }
      buff<-buffer
      dep_text <-expression(paste("Abundance ", (n/km^2), sep=" "))  # ," - ",depth_range[1],"-",depth_range[2]," m"

      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data=wmap_laea_df, ggplot2::aes(wmap_laea_df$long,wmap_laea_df$lat,group=wmap_laea_df$group), fill="light grey")+
        ggplot2::geom_path(data=countries_laea_df, ggplot2::aes(countries_laea_df$long,countries_laea_df$lat, group=countries_laea_df$group), color="black",size=0.1) +
        ggplot2::geom_path(data=depth_1_laea_df, ggplot2::aes(depth_1_laea_df$long,depth_1_laea_df$lat, group=depth_1_laea_df$group), color="#C0C0C0",size=0.4) +
        ggplot2::geom_path(data=depth_2_laea_df, ggplot2::aes(depth_2_laea_df$long,depth_2_laea_df$lat, group=depth_2_laea_df$group), color="#A9A9A9",size=0.4) +
        ggplot2::geom_path(data=depth_3_laea_df, ggplot2::aes(depth_3_laea_df$long,depth_3_laea_df$lat, group=depth_3_laea_df$group), color="#808080",size=0.4) +
        ggplot2::geom_point(data=loc_laea_df, ggplot2::aes(loc_laea_df$lon, loc_laea_df$lat, group=NULL,fill=NULL,size=loc_laea_df$indices),shape=21,
                            color="black",fill="blue",alpha=I(4/10)) +
        ggplot2::scale_size(range=c(1,14), breaks =br, labels = labels, guide = "legend",labs(size="n/km^2")) +
        ggplot2::coord_cartesian(xlim = c((xmin-buff),(xmax+buff)), ylim = c((ymin-buff),(ymax+buff))) +
        ggplot2::theme(aspect.ratio=1)+
        ggplot2::ggtitle(dep_text)+
        theme_opts
      # +
      #   scale_color_manual(name = "depth (m)", values = c("200" = "#C0C0C0", "500 m" = "#A9A9A9", "800 m" = "#808080"))

      # if(save){
      #   ggplot2::ggsave(paste(folder, "\\",sspp,"_GSA",GSA," -Abundance.jpg", sep=""),
      #                   width = 20, height = 20, units ="cm", dpi = 300)
      #
      #   cat("Bubble plot of recruits correctly saved \n")
      # }# close save recruits tiff
    }# close recruits analysis










    ###############
    if (stage == "recruits"){

      ddd_r <- NA
      ddd_r <- merge_TATC[merge_TATC$LENGTH_CLASS <= threshold & merge_TATC$LENGTH_CLASS != -1 , ]
      pivot_r <- aggregate(as.numeric(as.character(ddd_r$N_km2)), by=list(ddd_r$id, ddd_r$MEAN_LONGITUDE_DEC, ddd_r$MEAN_LATITUDE_DEC), sum)
      colnames(pivot_r) <- c("id", "lon", "lat", "indices")
      loc <- pivot_r
      br <- as.numeric(summary(loc$indices)[-4])
      labels <- as.character(round(as.numeric(summary(loc$indices)[-4]),3))
      ###############
      wmap_laea<- spTransform(wmap, CRS("+proj=laea"))
      countries_laea<-spTransform(countries, CRS("+proj=laea"))
      depth_1_laea<-spTransform(depth_1, CRS("+proj=laea"))
      depth_2_laea<-spTransform(depth_2, CRS("+proj=laea"))
      depth_3_laea<-spTransform(depth_3, CRS("+proj=laea"))

      coordinates(loc)<-c("lon","lat")
      proj4string(loc) <- CRS("+proj=longlat")
      loc_laea<-spTransform(loc, CRS("+proj=laea"))

      wmap_laea_df<-ggplot2::fortify(wmap_laea)
      countries_laea_df<-ggplot2::fortify(countries_laea)
      depth_1_laea_df<-ggplot2::fortify(depth_1_laea)
      depth_2_laea_df<-ggplot2::fortify(depth_2_laea)
      depth_3_laea_df<-ggplot2::fortify(depth_3_laea)

      loc_laea_df<-data.frame(loc_laea)

      theme_opts<-list(ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                      panel.grid.major = ggplot2::element_blank(),
                                      panel.background = ggplot2::element_rect(fill = 'light blue',linetype="solid",color="black"),
                                      plot.background = ggplot2::element_rect(fill="white",
                                                                              size=1,linetype="solid",color="black"),
                                      axis.line = ggplot2::element_blank(),
                                      axis.text.x = ggplot2::element_blank(),
                                      axis.text.y = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(),
                                      axis.title.x = ggplot2::element_blank(),
                                      axis.title.y = ggplot2::element_blank(),
                                      plot.title = ggplot2::element_text(size=22)))

      xmin<-min(loc_laea_df$lon)
      xmax<-max(loc_laea_df$lon)
      ymin<-min(loc_laea_df$lat)
      ymax<-max(loc_laea_df$lat)
      if (is.na(buffer)){
        buffer <- 5000
      }
      buff<-buffer
      dep_text <-expression(paste("Abundance of recruits ", (n/km^2), sep=" "))  # ," - ",depth_range[1],"-",depth_range[2]," m"

      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data=wmap_laea_df, ggplot2::aes(wmap_laea_df$long,wmap_laea_df$lat,group=wmap_laea_df$group), fill="light grey")+
        ggplot2::geom_path(data=countries_laea_df, ggplot2::aes(countries_laea_df$long,countries_laea_df$lat, group=countries_laea_df$group), color="black",size=0.1) +
        ggplot2::geom_path(data=depth_1_laea_df, ggplot2::aes(depth_1_laea_df$long,depth_1_laea_df$lat, group=depth_1_laea_df$group), color="#C0C0C0",size=0.4) +
        ggplot2::geom_path(data=depth_2_laea_df, ggplot2::aes(depth_2_laea_df$long,depth_2_laea_df$lat, group=depth_2_laea_df$group), color="#A9A9A9",size=0.4) +
        ggplot2::geom_path(data=depth_3_laea_df, ggplot2::aes(depth_3_laea_df$long,depth_3_laea_df$lat, group=depth_3_laea_df$group), color="#808080",size=0.4) +
        ggplot2::geom_point(data=loc_laea_df, ggplot2::aes(loc_laea_df$lon, loc_laea_df$lat, group=NULL,fill=NULL,size=loc_laea_df$indices),shape=21,
                            color="black",fill="blue",alpha=I(4/10)) +
        ggplot2::scale_size(range=c(1,14), breaks =br, labels = labels, guide = "legend",labs(size="n/km^2")) +
        ggplot2::coord_cartesian(xlim = c((xmin-buff),(xmax+buff)), ylim = c((ymin-buff),(ymax+buff))) +
        ggplot2::theme(aspect.ratio=1)+
        ggplot2::ggtitle(dep_text)+
        theme_opts
      
    }# close recruits analysis
    ##################
    ##################
    ##################



    if (stage == "spawners"){
      ddd_s <- NA
      ddd_s <- merge_TATC[merge_TATC$LENGTH_CLASS >= threshold & merge_TATC$LENGTH_CLASS != -1 & merge_TATC$SEX =="F", ]
      pivot_s <- aggregate(as.numeric(as.character(ddd_s$N_km2)), by=list(ddd_s$id, ddd_s$MEAN_LONGITUDE_DEC, ddd_s$MEAN_LATITUDE_DEC), sum)
      colnames(pivot_s) <- c("id", "lon", "lat", "indices")
      loc <- pivot_s   # [-7,]
      br <- as.numeric(summary(loc$indices)[-4])
      labels <- as.character(round(as.numeric(summary(loc$indices)[-4]),3))
      ###############
      wmap_laea<- spTransform(wmap, CRS("+proj=laea"))
      countries_laea<-spTransform(countries, CRS("+proj=laea"))
      depth_1_laea<-spTransform(depth_1, CRS("+proj=laea"))
      depth_2_laea<-spTransform(depth_2, CRS("+proj=laea"))
      depth_3_laea<-spTransform(depth_3, CRS("+proj=laea"))

      coordinates(loc)<-c("lon","lat")
      proj4string(loc) <- CRS("+proj=longlat")
      loc_laea<-spTransform(loc, CRS("+proj=laea"))

      wmap_laea_df<-ggplot2::fortify(wmap_laea)
      countries_laea_df<-ggplot2::fortify(countries_laea)
      depth_1_laea_df<-ggplot2::fortify(depth_1_laea)
      depth_2_laea_df<-ggplot2::fortify(depth_2_laea)
      depth_3_laea_df<-ggplot2::fortify(depth_3_laea)

      loc_laea_df<-data.frame(loc_laea)


      theme_opts<-list(ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                      panel.grid.major = ggplot2::element_blank(),
                                      panel.background = ggplot2::element_rect(fill = 'light blue',linetype="solid",color="black"),
                                      plot.background = ggplot2::element_rect(fill="white",
                                                                              size=1,linetype="solid",color="black"),
                                      axis.line = ggplot2::element_blank(),
                                      axis.text.x = ggplot2::element_blank(),
                                      axis.text.y = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(),
                                      axis.title.x = ggplot2::element_blank(),
                                      axis.title.y = ggplot2::element_blank(),
                                      plot.title = ggplot2::element_text(size=22)))

      xmin<-min(loc_laea_df$lon)
      xmax<-max(loc_laea_df$lon)
      ymin<-min(loc_laea_df$lat)
      ymax<-max(loc_laea_df$lat)
      buff<-50000

      dep_text <-expression(paste("Abundance of spawners ", (n/km^2), sep=" "))  # ," - ",depth_range[1],"-",depth_range[2]," m"

      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data=wmap_laea_df, ggplot2::aes(wmap_laea_df$long,wmap_laea_df$lat,group=wmap_laea_df$group), fill="light grey")+
        ggplot2::geom_path(data=countries_laea_df, ggplot2::aes(countries_laea_df$long,countries_laea_df$lat, group=countries_laea_df$group), color="black",size=0.1) +
        ggplot2::geom_path(data=depth_1_laea_df, ggplot2::aes(depth_1_laea_df$long,depth_1_laea_df$lat, group=depth_1_laea_df$group), color="#C0C0C0",size=0.4) +
        ggplot2::geom_path(data=depth_2_laea_df, ggplot2::aes(depth_2_laea_df$long,depth_2_laea_df$lat, group=depth_2_laea_df$group), color="#A9A9A9",size=0.4) +
        ggplot2::geom_path(data=depth_3_laea_df, ggplot2::aes(depth_3_laea_df$long,depth_3_laea_df$lat, group=depth_3_laea_df$group), color="#808080",size=0.4) +
        ggplot2::geom_point(data=loc_laea_df, ggplot2::aes(loc_laea_df$lon, loc_laea_df$lat, group=NULL,fill=NULL,size=loc_laea_df$indices),shape=21,
                            color="black",fill="blue",alpha=I(4/10)) +
        ggplot2::scale_size(range=c(1,14), breaks =br, labels = labels, guide = "legend",labs(size="n/km^2")) +
        ggplot2::coord_cartesian(xlim = c((xmin-buff),(xmax+buff)), ylim = c((ymin-buff),(ymax+buff))) +
        ggplot2::theme(aspect.ratio=1)+
        ggplot2::ggtitle(dep_text)+
        theme_opts

      # if(save){
      #   ggplot2::ggsave(paste(folder, "\\",sspp,"_GSA",GSA," -indices of SPAWNERS.jpg", sep=""),
      #                   width = 20, height = 20, units ="cm", dpi = 300)
      #
      #   cat("Bubble plot of spawners correctly saved \n")
      # }# close save spawners tiff
    }# close spawners analysis
    return(p)
  }
