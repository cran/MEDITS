index.ts <- function (merge, GSA = NA, sspp = NA, index = "abundance", depth_range, sex="c", str.scheme=strata_scheme,
                      surf=stratification_scheme, sampling="RSS", country =NA, plot=TRUE,verbose=FALSE ) {   #,  strata_scheme, stratification

  if (FALSE){
    merge = m.TATB(TA,TB,"ARISFOL")
    GSA = NA
    sspp = NA
    index = "MIW"
    depth_range=c(200,800)
    sex="c"
    str.scheme=strata_scheme
    surf=stratification_scheme
    sampling="RSS"
    country =NA
    plot=TRUE
    verbose=TRUE
  }
  strata_scheme <- str.scheme
  stratification_scheme <- surf
  stratification <- stratification_scheme
  # require(ggplot2)
  merge_TATB <- merge

  if (sex=="f"){
    merge_TATB$TOTAL_NUMBER_IN_THE_HAUL <- merge_TATB$NB_OF_FEMALES
    merge_TATB$N_km2 <- merge_TATB$NB_OF_FEMALES/merge_TATB$SWEPT_AREA
  }

  if (sex=="m"){
    merge_TATB$TOTAL_NUMBER_IN_THE_HAUL <- merge_TATB$NB_OF_MALES
    merge_TATB$N_km2 <- merge_TATB$NB_OF_MALES/merge_TATB$SWEPT_AREA
  }

  if(sex != "f" & sex != "m"){ sex="c"}

  if (is.na(sspp)) {
    GENERE <- as.character(unique(merge_TATB$GENUS)[unique(merge_TATB$GENUS) != -1])
    SPECIE <- as.character(unique(merge_TATB$SPECIES)[unique(merge_TATB$SPECIES) != -1])
    sspp <- paste(GENERE,SPECIE, sep="")
    species <- sspp
  }


  if (is.na(GSA)) {
    GSA <- unique(merge_TATB$GSA)
  }

  dependent <- index

  #-----------------------
  # ABUNDANCE INDICES
  #-----------------------


if (dependent == "abundance"){

  #-----------------------
  # Abundance INDICES
  #-----------------------

  dep_text <-expression(paste("Abundance ", (n/km^2), sep=" "))
  varcol <- which(colnames(merge_TATB)=="N_km2")
  col_response <- which(colnames(merge_TATB)=="N_km2")
  positive_hauls<- 0
  total_hauls<- 0
  species <- sspp

  #------------------------

  #------------------------------------------
  # plotting species' depth range information
  # g1 <- ggplot(merge_TATB[merge_TATB[,varcol] >0,], aes(x=MEAN_DEPTH)) + geom_density()+xlab("depth (m)")+ggtitle(paste(sspp, " - GSA",GSA,sep="" ))
  # ggsave(paste(wd,"/output/depth.distribution_",sspp,"_GSA",GSA,".jpg", sep=""),width=5, height=5)
  # g2 <- ggplot(merge_TATB[merge_TATB[,varcol]>0,], aes(x=species, y=MEAN_DEPTH)) + geom_boxplot() + xlab("") +ylab("depth (m)")+ggtitle(paste(sspp, " - GSA",GSA,sep="" ))
  # ggsave(paste(wd,"/output/depth.distribution_(boxplot)",sspp,"_GSA",GSA,".jpg", sep=""),width=5, height=5)
  # grid.arrange(g1, g2, ncol=2)
  #------------------------------------------

  #------------------------------------------
  # selection of depth range
  if (is.na(country)){
  data <- merge_TATB[merge_TATB$GSA == GSA , ]
  strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == as.character(unique(merge_TATB$COUNTRY)[1]), ]
  }else{
    if (!is.na(country)){
      data <- merge_TATB[merge_TATB$GSA == GSA &  merge_TATB$COUNTRY == country, ]
      strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == country, ]
    }
  }

  depth <- data.frame(matrix(NA,ncol=4, nrow=length(strata.scheme$CODE)))
  colnames(depth) <- c("strata", "min", "max", "bul")
  depth$strata <- strata.scheme$CODE # c(1,2,3,4,5)
  depth$min <-strata.scheme$MIN_DEPTH  # c(10,50,100,200,500)
  depth$max <-strata.scheme$MAX_DEPTH  # c(50,100,200,500,800)


  depth_range <- data.frame(depth_range); depth_range <- as.numeric(as.character(depth_range[,1]))
  if (depth_range[2] != 800) {depth_range[2] <- depth_range[2]}
  data <-  data[data$MEAN_DEPTH>=depth_range[1] & data$MEAN_DEPTH<depth_range[2],]
  strata_range <- seq(which(depth[,"min"]==depth_range[1]),which(depth[,"max"]==depth_range[2]),1)
  depth[depth$strata %in% strata_range, "bul"] <- TRUE
  depth[!(depth$strata %in% strata_range), "bul"] <- FALSE

  analysis_stratum1 <- NA
  analysis_stratum2 <- NA
  analysis_stratum3 <- NA
  analysis_stratum4 <- NA
  analysis_stratum5 <- NA
  analysis_stratum5 <- NA

  analysis_stratum1 <- depth[1,4]
  analysis_stratum2 <- depth[2,4]
  analysis_stratum3 <- depth[3,4]
  analysis_stratum4 <- depth[4,4]
  analysis_stratum5 <- depth[5,4]
  analysis_stratum6 <- depth[6,4]

  if (is.na(analysis_stratum1)){depth[1,4]<- FALSE; analysis_stratum1 <- FALSE}
  if (is.na(analysis_stratum2)){depth[2,4]<- FALSE; analysis_stratum2 <- FALSE}
  if (is.na(analysis_stratum3)){depth[3,4]<- FALSE; analysis_stratum3 <- FALSE}
  if (is.na(analysis_stratum4)){depth[4,4]<- FALSE; analysis_stratum4 <- FALSE}
  if (is.na(analysis_stratum5)){depth[5,4]<- FALSE; analysis_stratum5 <- FALSE}
  if (is.na(analysis_stratum6)){depth[6,4]<- FALSE; analysis_stratum6 <- FALSE}

  #------------------------------------------


  ddd <- data
  year_range <- data.frame(year = sort(unique(data$YEAR)))

  res_table_cala2 <- data.frame(year=sort(unique(data$YEAR)))
  res_table_cala2$index_1 <- NA
  res_table_cala2$index_2 <- NA
  res_table_cala2$index_3 <- NA
  res_table_cala2$index_4 <- NA
  res_table_cala2$index_5 <- NA
  res_table_cala2$index_6 <- NA

  se_table2  <-  data.frame(year=sort(unique(data$YEAR)))
  se_table2$sum_1 <- NA
  se_table2$sum_2 <- NA
  se_table2$sum_3 <- NA
  se_table2$sum_4 <- NA
  se_table2$sum_5 <- NA
  se_table2$sum_6 <- NA
  se_table2$sd <- NA

  sem_table  <-  data.frame(year=sort(unique(data$YEAR)))
  sem_table$sem_1 <- NA
  sem_table$sem_2 <- NA
  sem_table$sem_3 <- NA
  sem_table$sem_4 <- NA
  sem_table$sem_5 <- NA
  sem_table$sem_6 <- NA
  sem_table$sem <- NA

  n_table  <-  data.frame(year=sort(unique(data$YEAR)))
  n_table$n_1 <- NA
  n_table$n_2 <- NA
  n_table$n_3 <- NA
  n_table$n_4 <- NA
  n_table$n_5 <- NA
  n_table$n_6 <- NA
  n_table$n <- NA

  if (is.na(country)){
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==6,5])} else {area_s6 <- 0}
  } else {
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==6,5])} else {area_s6 <- 0}
                 }

  peso_s1 <- area_s1/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s2 <- area_s2/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s3 <- area_s3/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s4 <- area_s4/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s5 <- area_s5/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s6 <- area_s6/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
i=1
  if (sampling =="RSS") {      # open RSS Abundance
  for(i in 1:length(res_table_cala2[,1])) {

    data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]

    if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
    if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
    if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
    if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
    if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
    if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

    if (analysis_stratum1 == TRUE){
      s_n <- sum(s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s1$SWEPT_AREA)
      res_table_cala2[i,2] <- s_n/s_a
      n_table[i,2] <- length(s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])


      mean_ind <- mean(s1[!is.na(s1$N_km2), "N_km2" ])
      sq <- (s1[!is.na(s1$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s1$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s1[,1])-1)
      f1 <- s_a/area_s1
      se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)
      sem_table[i,2] <- sqrt(se_table2[i,2])/sqrt(n_table[i,2])

    } else {area_s1 = 0}

    if (analysis_stratum2 == TRUE){
      s_n <- sum(s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s2$SWEPT_AREA)
      res_table_cala2[i,3] <- s_n/s_a
      n_table[i,3] <- length(s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

      mean_ind <- mean(s2[!is.na(s2$N_km2), "N_km2" ])
      sq <- (s2[!is.na(s2$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s2$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s2[,1])-1)
      f2 <- s_a/area_s2
      se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
      sem_table[i,3] <- sqrt(se_table2[i,3])/sqrt(n_table[i,3])

    } else {area_s2 = 0}

    if (analysis_stratum3 == TRUE){
      s_n <- sum(s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s3$SWEPT_AREA)
      res_table_cala2[i,4] <- s_n/s_a
      n_table[i,4] <- length(s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

      mean_ind <- mean(s3[!is.na(s3$N_km2), "N_km2" ])
      sq <- (s3[!is.na(s3$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s3$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s3[,1])-1)
      f3 <- s_a/area_s3
      se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
      sem_table[i,4] <- sqrt(se_table2[i,4])/sqrt(n_table[i,4])
    } else {area_s3 = 0}

    if (analysis_stratum4 == TRUE){
      s_n <- sum(s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s4$SWEPT_AREA)
      res_table_cala2[i,5] <- s_n/s_a
      n_table[i,5] <- length(s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

      mean_ind <- mean(s4[!is.na(s4$N_km2), "N_km2" ])
      sq <- (s4[!is.na(s4$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s4$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s4[,1])-1)
      f4 <- s_a/area_s4
      se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
      sem_table[i,5] <- sqrt(se_table2[i,5])/sqrt(n_table[i,5])
    } else {area_s4 = 0}

    if (analysis_stratum5 == TRUE){
      #index computation
      s_n <- sum(s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s5$SWEPT_AREA)
      res_table_cala2[i,6] <- s_n/s_a
      n_table[i,6] <- length(s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

      #se computation
      mean_ind <- mean(s5[!is.na(s5$N_km2), "N_km2" ])
      sq <- (s5[!is.na(s5$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s5$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s5[,1])-1)
      f5 <- s_a/area_s5
      se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
      sem_table[i,6] <- sqrt(se_table2[i,6])/sqrt(n_table[i,6])
    } else {area_s5 = 0}

    if (analysis_stratum6 == TRUE){
      #index computation
      s_n <- sum(s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s6$SWEPT_AREA)
      res_table_cala2[i,7] <- s_n/s_a
      n_table[i,7] <- length(s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

      #se computation
      mean_ind <- mean(s6[!is.na(s6$N_km2), "N_km2" ])
      sq <- (s6[!is.na(s6$N_km2),"N_km2"] - mean_ind)^2
      sqA <- sq*s6$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s6[,1])-1)
      f6 <- s_a/area_s6
      se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
      sem_table[i,7] <- sqrt(se_table2[i,7])/sqrt(n_table[i,7])
    } else {area_s6 = 0}

    positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
    total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

    sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
    res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
    colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
    se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
    sem_table[i, "sem"] <-rowSums(sem_table[i, 2:7], na.rm = TRUE)
  }
} # close RSS Abundance


  if (sampling =="RPS") {     # open RPS Abundance
    for(i in 1:length(res_table_cala2[,1])) {

      data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]

      n <- sum(ddd$SWEPT_AREA)
      n2 <- n^2

      if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
      if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
      if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
      if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
      if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
      if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

      if (analysis_stratum1 == TRUE){
        s_n <- sum(s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s1$SWEPT_AREA)
        res_table_cala2[i,2] <- s_n/s_a
        n_table[i,2] <- length(s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        mean_ind <- mean(s1[!is.na(s1$N_km2), "N_km2" ])
        sq <- (s1[!is.na(s1$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s1$SWEPT_AREA
        # ssq <- sum(sqA)  #
        sum_sqA <- sum(sqA)/(length(s1[,1])-1) # S2
        f1 <- s_a/area_s1
        se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1) #varianza total area per strato
        coef_s1 <- ((1-peso_s1) * sum_sqA)/n2
        se_table2[i,2] <- se_table2[i,2] + coef_s1
        sem_table[i,2] <- sqrt(se_table2[i,2])/sqrt(n_table[i,2])
      } else {area_s1 = 0}

      if (analysis_stratum2 == TRUE){
        s_n <- sum(s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s2$SWEPT_AREA)
        res_table_cala2[i,3] <- s_n/s_a
        n_table[i,3] <- length(s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        mean_ind <- mean(s2[!is.na(s2$N_km2), "N_km2" ])
        sq <- (s2[!is.na(s2$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s2$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s2[,1])-1)
        f2 <- s_a/area_s2
        se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
        coef_s2 <- ((1-peso_s2) * sum_sqA)/n2
        se_table2[i,3] <- se_table2[i,3] + coef_s2
        sem_table[i,3] <- sqrt(se_table2[i,3])/sqrt(n_table[i,3])
      } else {area_s2 = 0}

      if (analysis_stratum3 == TRUE){
        s_n <- sum(s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s3$SWEPT_AREA)
        res_table_cala2[i,4] <- s_n/s_a
        n_table[i,4] <- length(s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        mean_ind <- mean(s3[!is.na(s3$N_km2), "N_km2" ])
        sq <- (s3[!is.na(s3$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s3$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s3[,1])-1)
        f3 <- s_a/area_s3
        se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
        coef_s3 <- ((1-peso_s3) * sum_sqA)/n2
        se_table2[i,4] <- se_table2[i,4] + coef_s3
        sem_table[i,4] <- sqrt(se_table2[i,4])/sqrt(n_table[i,4])
      } else {area_s3 = 0}

      if (analysis_stratum4 == TRUE){
        s_n <- sum(s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s4$SWEPT_AREA)
        res_table_cala2[i,5] <- s_n/s_a
        n_table[i,5] <- length(s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        mean_ind <- mean(s4[!is.na(s4$N_km2), "N_km2" ])
        sq <- (s4[!is.na(s4$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s4$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s4[,1])-1)
        f4 <- s_a/area_s4
        se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
        coef_s4 <- ((1-peso_s4) * sum_sqA)/n2
        se_table2[i,5] <- se_table2[i,5] + coef_s4
        sem_table[i,5] <- sqrt(se_table2[i,5])/sqrt(n_table[i,5])
      } else {area_s4 = 0}

      if (analysis_stratum5 == TRUE){
        #index computation
        s_n <- sum(s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s5$SWEPT_AREA)
        res_table_cala2[i,6] <- s_n/s_a
        n_table[i,6] <- length(s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        #se computation
        mean_ind <- mean(s5[!is.na(s5$N_km2), "N_km2" ])
        sq <- (s5[!is.na(s5$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s5$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s5[,1])-1)
        f5 <- s_a/area_s5
        se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
        coef_s5 <- ((1-peso_s5) * sum_sqA)/n2
        se_table2[i,6] <- se_table2[i,6] + coef_s5
        sem_table[i,6] <- sqrt(se_table2[i,6])/sqrt(n_table[i,6])
      } else {area_s5 = 0}

      if (analysis_stratum6 == TRUE){
        #index computation
        s_n <- sum(s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s6$SWEPT_AREA)
        res_table_cala2[i,7] <- s_n/s_a
        n_table[i,7] <- length(s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL),"TOTAL_NUMBER_IN_THE_HAUL"])

        #se computation
        mean_ind <- mean(s6[!is.na(s6$N_km2), "N_km2" ])
        sq <- (s6[!is.na(s6$N_km2),"N_km2"] - mean_ind)^2
        sqA <- sq*s6$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s6[,1])-1)
        f6 <- s_a/area_s6
        se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
        coef_s6 <- ((1-peso_s6) * sum_sqA)/n2
        se_table2[i,7] <- se_table2[i,7] + coef_s6
        sem_table[i,7] <- sqrt(se_table2[i,7])/sqrt(n_table[i,7])
      } else {area_s6 = 0}

      # source(paste(wd, "/scripts/Index_estimation_cala.R", sep=""), encoding = 'UTF-8')
      positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
      total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

      sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
      res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
      colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
      se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
      sem_table[i, "sem"] <-rowSums(sem_table[i, 2:7], na.rm = TRUE)
    }
    } # close RPS Abundance


  timeseries <- data.frame(year = res_table_cala2[,1], abundance = res_table_cala2[,8], sd= se_table2[, "sd"], se = sem_table[, "sem"])
  timeseries$CV <- timeseries[,"sd"] / timeseries[,"abundance"]*100
  timeseries$invCV <- 1/timeseries$CV
  timeseries$positive_hauls_perc <- positive_hauls / total_hauls * 100
  colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")

  # plot timeseries of mean indices
  if(sex=="c"){
  main <- paste(sspp,"_GSA",GSA,"_(",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  main.lab <- paste(sspp," GSA",GSA," (",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  }
  if(sex=="f"){
    main <- paste(sspp,"_GSA",GSA,"_(",dependent,")_Females_",depth_range[1],"-",depth_range[2], " m", sep="")
    main.lab <- paste(sspp," GSA",GSA," (",dependent,")_Females_",depth_range[1],"-",depth_range[2], " m", sep="")
  }
  if(sex=="m"){
    main <- paste(sspp,"_GSA",GSA,"_(",dependent,")_Males_",depth_range[1],"-",depth_range[2], " m", sep="")
    main.lab <- paste(sspp," GSA",GSA," (",dependent,")_Males_",depth_range[1],"-",depth_range[2], " m", sep="")
  }
  max_index <- max(timeseries[,"abundance"]) + (max(timeseries[!is.na(timeseries$sd),"se"])*1.2)
  # write.table(timeseries, paste(wd,"/output/",main,"_Timeseries.csv", sep=""), sep=";", row.names = F)

  if (plot){
	oldoptions <- options()$warn
    old_par <- list()
    old_par$mfrow <-par()$mfrow
    old_par$oma <-par()$oma
    old_par$mgp <-par()$mgp
	on.exit(c(par(mfrow=old_par$mfrow, oma=old_par$oma, mgp=old_par$mgp), options(warn=oldoptions)))
  options(warn=-1)
  par(mfrow=c(1,1),oma=c(1,1,1,1), mgp=c(2, 1,0))
  plot(timeseries[,"year"],  timeseries[,"abundance"], type="b", col="black", pch=16, xlab="year", ylim=c(0,max_index*1.2), ylab=dep_text, main=main.lab) # ylim=c(0,max_index*1.2)
  lines(timeseries[,"year"], (timeseries[,"abundance"]-1.96*timeseries[,"se"]), type="l",lty=2, col="red" )
  lines(timeseries[,"year"], (timeseries[,"abundance"]+1.96*timeseries[,"se"]), type="l",lty=2, col="red" )
  legend("topright", c("time series", "CI"), lty=c(1,1), pch=c(16, NA), col=c("black","red"))
  }

if (verbose){
  message("\n Estimation of abundance indices completed \n")
  }
} #close abundance estimation


  #-----------------------
  # BIOMASS INDICES
  #-----------------------

if (dependent == "biomass"){

  #------------------------
  # Variable definitions #
  # dependent <- "biomass"
  dep_text <-expression(paste("Biomass ", (kg/km^2), sep=" "))
  varcol <- which(colnames(merge_TATB)=="kg_km2")
  col_response <- which(colnames(merge_TATB)=="kg_km2")
  # GENERE <- as.character(unique(merge_TATB$GENUS)[unique(merge_TATB$GENUS) != -1])
  # SPECIE <- as.character(unique(merge_TATB$SPECIES)[unique(merge_TATB$SPECIES) != -1])
  # sspp <- paste(GENERE,SPECIE, sep="")
  # GSA <- unique(merge_TATB$GSA)
  # species <- sspp
  positive_hauls<- 0
  total_hauls<- 0

  #------------------------
  # selection of depth range
  if (is.na(country)){
    data <- merge_TATB[merge_TATB$GSA == GSA , ]
    strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == as.character(unique(merge_TATB$COUNTRY)[1]), ]
  }else{
    if (!is.na(country)){
      data <- merge_TATB[merge_TATB$GSA == GSA &  merge_TATB$COUNTRY == country, ]
      strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == country, ]
    }
  }

  depth <- data.frame(matrix(NA,ncol=4, nrow=length(strata.scheme$CODE)))
  colnames(depth) <- c("strata", "min", "max", "bul")
  depth$strata <- strata.scheme$CODE # c(1,2,3,4,5)
  depth$min <-strata.scheme$MIN_DEPTH  # c(10,50,100,200,500)
  depth$max <-strata.scheme$MAX_DEPTH  # c(50,100,200,500,800)
  # print("Select the depth range for the analysis")
  # depth_range <- dlgInput("Depth range for the analysis: ", default="10,100",Sys.info()[""])$res
  depth_range <- data.frame(depth_range); depth_range <- as.numeric(as.character(depth_range[,1]))
  if (depth_range[2] != 800) {depth_range[2] <- depth_range[2]}
  data <-  data[data$MEAN_DEPTH>=depth_range[1] & data$MEAN_DEPTH<depth_range[2],]
  strata_range <- seq(which(depth[,"min"]==depth_range[1]),which(depth[,"max"]==depth_range[2]),1)
  depth[depth$strata %in% strata_range, "bul"] <- TRUE
  depth[!(depth$strata %in% strata_range), "bul"] <- FALSE


  analysis_stratum1 <- NA
  analysis_stratum2 <- NA
  analysis_stratum3 <- NA
  analysis_stratum4 <- NA
  analysis_stratum5 <- NA
  analysis_stratum5 <- NA

  analysis_stratum1 <- depth[1,4]
  analysis_stratum2 <- depth[2,4]
  analysis_stratum3 <- depth[3,4]
  analysis_stratum4 <- depth[4,4]
  analysis_stratum5 <- depth[5,4]
  analysis_stratum6 <- depth[6,4]

  if (is.na(analysis_stratum1)){depth[1,4]<- FALSE; analysis_stratum1 <- FALSE}
  if (is.na(analysis_stratum2)){depth[2,4]<- FALSE; analysis_stratum2 <- FALSE}
  if (is.na(analysis_stratum3)){depth[3,4]<- FALSE; analysis_stratum3 <- FALSE}
  if (is.na(analysis_stratum4)){depth[4,4]<- FALSE; analysis_stratum4 <- FALSE}
  if (is.na(analysis_stratum5)){depth[5,4]<- FALSE; analysis_stratum5 <- FALSE}
  if (is.na(analysis_stratum6)){depth[6,4]<- FALSE; analysis_stratum6 <- FALSE}

  #------------------------------------------

  ddd <- data # ddd <- merge_TATB
  year_range <- data.frame(year = sort(unique(data$YEAR)))

  res_table_cala2 <- data.frame(year=sort(unique(data$YEAR)))
  res_table_cala2$index_1 <- NA
  res_table_cala2$index_2 <- NA
  res_table_cala2$index_3 <- NA
  res_table_cala2$index_4 <- NA
  res_table_cala2$index_5 <- NA
  res_table_cala2$index_6 <- NA

  se_table2  <-  data.frame(year=sort(unique(data$YEAR)))
  se_table2$sum_1 <- NA
  se_table2$sum_2 <- NA
  se_table2$sum_3 <- NA
  se_table2$sum_4 <- NA
  se_table2$sum_5 <- NA
  se_table2$sum_6 <- NA
  se_table2$sd <- NA

  n_table  <-  data.frame(year=sort(unique(data$YEAR)))
  n_table$n_1 <- NA
  n_table$n_2 <- NA
  n_table$n_3 <- NA
  n_table$n_4 <- NA
  n_table$n_5 <- NA
  n_table$n_6 <- NA
  n_table$n <- NA

  sem_table  <-  data.frame(year=sort(unique(data$YEAR)))
  sem_table$sem_1 <- NA
  sem_table$sem_2 <- NA
  sem_table$sem_3 <- NA
  sem_table$sem_4 <- NA
  sem_table$sem_5 <- NA
  sem_table$sem_6 <- NA
  sem_table$sem <- NA

  if (is.na(country)){
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==6,5])} else {area_s6 <- 0}
  } else {
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==6,5])} else {area_s6 <- 0}
  }


  peso_s1 <- area_s1/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s2 <- area_s2/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s3 <- area_s3/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s4 <- area_s4/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s5 <- area_s5/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s6 <- area_s6/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)

  if (sampling =="RSS") { # open RSS biomass
  for(i in 1:length(res_table_cala2[,1])) {

    data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]

    if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
    if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
    if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
    if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
    if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
    if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

    if (analysis_stratum1 == TRUE){
      s_n <- sum(s1[!is.na(s1$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s1$SWEPT_AREA)
      res_table_cala2[i,2] <- s_n/s_a
      n_table[i,2] <- length(s1[!is.na(s1$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      mean_ind <- mean(s1[!is.na(s1$kg_km2), "kg_km2" ])
      sq <- (s1[!is.na(s1$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s1$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s1[,1])-1)
      f1 <- s_a/area_s1
      se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)
      sem_table[i,2] <- sqrt(se_table2[i,2])/sqrt(n_table[i,2])
    } else {area_s1 = 0}

    if (analysis_stratum2 == TRUE){
      s_n <- sum(s2[!is.na(s2$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s2$SWEPT_AREA)
      res_table_cala2[i,3] <- s_n/s_a
      n_table[i,3] <- length(s2[!is.na(s2$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      mean_ind <- mean(s2[!is.na(s2$kg_km2), "kg_km2" ])
      sq <- (s2[!is.na(s2$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s2$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s2[,1])-1)
      f2 <- s_a/area_s2
      se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
      sem_table[i,3] <- sqrt(se_table2[i,3])/sqrt(n_table[i,3])
    } else {area_s2 = 0}

    if (analysis_stratum3 == TRUE){
      s_n <- sum(s3[!is.na(s3$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s3$SWEPT_AREA)
      res_table_cala2[i,4] <- s_n/s_a
      n_table[i,4] <- length(s3[!is.na(s3$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      mean_ind <- mean(s3[!is.na(s3$kg_km2), "kg_km2" ])
      sq <- (s3[!is.na(s3$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s3$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s3[,1])-1)
      f3 <- s_a/area_s3
      se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
      sem_table[i,4] <- sqrt(se_table2[i,4])/sqrt(n_table[i,4])
    } else {area_s3 = 0}

    if (analysis_stratum4 == TRUE){
      s_n <- sum(s4[!is.na(s4$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s4$SWEPT_AREA)
      res_table_cala2[i,5] <- s_n/s_a
      n_table[i,5] <- length(s4[!is.na(s4$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      mean_ind <- mean(s4[!is.na(s4$kg_km2), "kg_km2" ])
      sq <- (s4[!is.na(s4$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s4$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s4[,1])-1)
      f4 <- s_a/area_s4
      se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
      sem_table[i,5] <- sqrt(se_table2[i,5])/sqrt(n_table[i,5])
    } else {area_s4 = 0}

    if (analysis_stratum5 == TRUE){
      #index computation
      s_n <- sum(s5[!is.na(s5$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s5$SWEPT_AREA)
      res_table_cala2[i,6] <- s_n/s_a
      n_table[i,6] <- length(s5[!is.na(s5$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      #se computation
      mean_ind <- mean(s5[!is.na(s5$kg_km2), "kg_km2" ])
      sq <- (s5[!is.na(s5$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s5$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s5[,1])-1)
      f5 <- s_a/area_s5
      se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
      sem_table[i,6] <- sqrt(se_table2[i,6])/sqrt(n_table[i,6])
    } else {area_s5 = 0}

    if (analysis_stratum6 == TRUE){
      #index computation
      s_n <- sum(s6[!is.na(s6$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
      s_a <- sum(s6$SWEPT_AREA)
      res_table_cala2[i,7] <- s_n/s_a
      n_table[i,7] <- length(s6[!is.na(s6$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

      #se computation
      mean_ind <- mean(s6[!is.na(s6$kg_km2), "kg_km2" ])
      sq <- (s6[!is.na(s6$kg_km2),"kg_km2"] - mean_ind)^2
      sqA <- sq*s6$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s6[,1])-1)
      f6 <- s_a/area_s6
      se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
      sem_table[i,7] <- sqrt(se_table2[i,7])/sqrt(n_table[i,7])
    } else {area_s6 = 0}

    positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
    total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

    sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
    res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
    colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
    se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
    sem_table[i, "sem"] <-rowSums(sem_table[i, 2:7], na.rm = TRUE)
  }
  } # close RSS biomass

  if (sampling =="RPS") { # open RPS biomass
    for(i in 1:length(res_table_cala2[,1])) {

      data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]
      n <- sum(ddd$SWEPT_AREA)
      n2 <- n^2

      if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
      if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
      if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
      if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
      if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
      if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

      if (analysis_stratum1 == TRUE){
        s_n <- sum(s1[!is.na(s1$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s1$SWEPT_AREA)
        res_table_cala2[i,2] <- s_n/s_a
        n_table[i,2] <- length(s1[!is.na(s1$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s1[!is.na(s1$kg_km2), "kg_km2" ])
        sq <- (s1[!is.na(s1$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s1$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s1[,1])-1)
        f1 <- s_a/area_s1
        se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)
        coef_s1 <- ((1-peso_s1) * sum_sqA)/n2
        se_table2[i,2] <- se_table2[i,2] + coef_s1 # Variance
        sem_table[i,2] <- sqrt(se_table2[i,2])/sqrt(n_table[i,2])
      } else {area_s1 = 0}

      if (analysis_stratum2 == TRUE){
        s_n <- sum(s2[!is.na(s2$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s2$SWEPT_AREA)
        res_table_cala2[i,3] <- s_n/s_a
        n_table[i,3] <- length(s2[!is.na(s2$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s2[!is.na(s2$kg_km2), "kg_km2" ])
        sq <- (s2[!is.na(s2$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s2$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s2[,1])-1)
        f2 <- s_a/area_s2
        se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
        coef_s2 <- ((1-peso_s2) * sum_sqA)/n2
        se_table2[i,3] <- se_table2[i,3] + coef_s2 # Variance
        sem_table[i,3] <- sqrt(se_table2[i,3])/sqrt(n_table[i,3])
      } else {area_s2 = 0}

      if (analysis_stratum3 == TRUE){
        s_n <- sum(s3[!is.na(s3$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s3$SWEPT_AREA)
        res_table_cala2[i,4] <- s_n/s_a
        n_table[i,4] <- length(s3[!is.na(s3$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s3[!is.na(s3$kg_km2), "kg_km2" ])
        sq <- (s3[!is.na(s3$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s3$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s3[,1])-1)
        f3 <- s_a/area_s3
        se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
        coef_s3 <- ((1-peso_s3) * sum_sqA)/n2
        se_table2[i,4] <- se_table2[i,4] + coef_s3  # Variance
        sem_table[i,4] <- sqrt(se_table2[i,4])/sqrt(n_table[i,4])
      } else {area_s3 = 0}

      if (analysis_stratum4 == TRUE){
        s_n <- sum(s4[!is.na(s4$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s4$SWEPT_AREA)
        res_table_cala2[i,5] <- s_n/s_a
        n_table[i,5] <- length(s4[!is.na(s4$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s4[!is.na(s4$kg_km2), "kg_km2" ])
        sq <- (s4[!is.na(s4$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s4$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s4[,1])-1)
        f4 <- s_a/area_s4
        se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
        coef_s4 <- ((1-peso_s4) * sum_sqA)/n2
        se_table2[i,5] <- se_table2[i,5] + coef_s4  # Variance
        sem_table[i,5] <- sqrt(se_table2[i,5])/sqrt(n_table[i,5])
      } else {area_s4 = 0}

      if (analysis_stratum5 == TRUE){
        #index computation
        s_n <- sum(s5[!is.na(s5$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s5$SWEPT_AREA)
        res_table_cala2[i,6] <- s_n/s_a
        n_table[i,6] <- length(s5[!is.na(s5$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s5[!is.na(s5$kg_km2), "kg_km2" ])
        sq <- (s5[!is.na(s5$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s5$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s5[,1])-1)
        f5 <- s_a/area_s5
        se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
        coef_s5 <- ((1-peso_s5) * sum_sqA)/n2
        se_table2[i,6] <- se_table2[i,6] + coef_s5    # Variance
        sem_table[i,6] <- sqrt(se_table2[i,6])/sqrt(n_table[i,6])
      } else {area_s5 = 0}

      if (analysis_stratum6 == TRUE){
        #index computation
        s_n <- sum(s6[!is.na(s6$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)
        s_a <- sum(s6$SWEPT_AREA)
        res_table_cala2[i,7] <- s_n/s_a
        n_table[i,7] <- length(s6[!is.na(s6$TOTAL_WEIGHT_IN_THE_HAUL),"TOTAL_WEIGHT_IN_THE_HAUL"])

        #variance computation
        mean_ind <- mean(s6[!is.na(s6$kg_km2), "kg_km2" ])
        sq <- (s6[!is.na(s6$kg_km2),"kg_km2"] - mean_ind)^2
        sqA <- sq*s6$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s6[,1])-1)
        f6 <- s_a/area_s6
        se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
        coef_s6 <- ((1-peso_s6) * sum_sqA)/n2
        se_table2[i,7] <- se_table2[i,7] + coef_s6    # Variance
        sem_table[i,7] <- sqrt(se_table2[i,7])/sqrt(n_table[i,7])
      } else {area_s6 = 0}

      positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
      total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

      sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
      res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
      colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
      se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
      sem_table[i, "sem"] <-rowSums(sem_table[i, 2:7], na.rm = TRUE)
    }
  } # close RPS biomass

  timeseries <- data.frame(year = res_table_cala2[,1], biomass = res_table_cala2[,8], sd= se_table2[, "sd"], se = sem_table[, "sem"])
  timeseries$CV <- timeseries[,"sd"] / timeseries[,"biomass"]*100
  timeseries$invCV <- 1/timeseries$CV
  timeseries$positive_hauls_perc <- positive_hauls / total_hauls * 100
  colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")

  # is.na(timeseries) <- do.call(cbind,lapply(timeseries, is.infinite))
  if (plot){
  # plot timeseries of mean indices
  main <- paste(sspp,"_GSA",GSA,"_(",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  main.lab <- paste(sspp," GSA",GSA," (",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  max_index <- max(timeseries[,"biomass"]) + max(timeseries[!is.na(timeseries$sd),"se"]*1.2)

  oldoptions <- options()$warn
  old_par <- list()
  old_par$mfrow <-par()$mfrow
  old_par$oma <-par()$oma
  old_par$mgp <-par()$mgp
  on.exit(c(par(mfrow=old_par$mfrow, oma=old_par$oma, mgp=old_par$mgp), options(warn=oldoptions)))
    options(warn=-1)
  par(mfrow=c(1,1),oma=c(1,1,1,1), mgp=c(2, 1,0))
  plot(timeseries[,"year"],  timeseries[,"biomass"], type="b", col="black", pch=16, xlab="year", ylim=c(0,max_index*1.2), ylab=dep_text, main=main.lab) # ylim=c(0,max_index*1.2)
  lines(timeseries[,"year"], (timeseries[,"biomass"]-1.96*timeseries[,"se"]), type="l",lty=2, col="red" )
  lines(timeseries[,"year"], (timeseries[,"biomass"]+1.96*timeseries[,"se"]), type="l",lty=2, col="red" )
  legend("topright", c("time series", "CI"), lty=c(1,2), pch=c(16, NA), col=c("black","red"))
}
  if (verbose){
    message("\n Estimation of biomass indices completed \n")

  if (sex == "f" | sex=="m"){
    warning('The selection of sex is not allowed for the estimation of biomass indices
            \nThe default value (sex="c") will be used',immediate. = TRUE)
  }
  }
} # close extimation of biomass indices


  #-----------------------
  # MIW INDICES
  #-----------------------


  if (dependent == "MIW"){


  # dependent <- "MIW"
  dep_text <-"MIW (kg)"
  # varcol <- which(colnames(merge_TATB)=="kg_km2")
  # col_response <- which(colnames(merge_TATB)=="kg_km2")
  GENERE <- as.character(unique(merge_TATB$GENUS)[unique(merge_TATB$GENUS) != -1])
  SPECIE <- as.character(unique(merge_TATB$SPECIES)[unique(merge_TATB$SPECIES) != -1])
  sspp <- paste(GENERE,SPECIE, sep="")
  GSA <- unique(merge_TATB$GSA)
  species <- sspp
  positive_hauls<- 0
  total_hauls<- 0

  #------------------------
  # selection of depth range
  if (is.na(country)){
    data <- merge_TATB[merge_TATB$GSA == GSA , ]
    strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == as.character(unique(merge_TATB$COUNTRY)[1]), ]
  }else{
    if (!is.na(country)){
      data <- merge_TATB[merge_TATB$GSA == GSA &  merge_TATB$COUNTRY == country, ]
      strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == country, ]
    }
  }

  depth <- data.frame(matrix(NA,ncol=4, nrow=length(strata.scheme$CODE)))
  colnames(depth) <- c("strata", "min", "max", "bul")
  depth$strata <- strata.scheme$CODE # c(1,2,3,4,5)
  depth$min <-strata.scheme$MIN_DEPTH  # c(10,50,100,200,500)
  depth$max <-strata.scheme$MAX_DEPTH  # c(50,100,200,500,800)
  # print("Select the depth range for the analysis")
  # depth_range <- dlgInput("Depth range for the analysis: ", default="10,100",Sys.info()[""])$res
  depth_range <- data.frame(depth_range); depth_range <- as.numeric(as.character(depth_range[,1]))
  if (depth_range[2] != 800) {depth_range[2] <- depth_range[2]}
  data <-  data[data$MEAN_DEPTH>=depth_range[1] & data$MEAN_DEPTH<depth_range[2],]
  strata_range <- seq(which(depth[,"min"]==depth_range[1]),which(depth[,"max"]==depth_range[2]),1)
  depth[depth$strata %in% strata_range, "bul"] <- TRUE
  depth[!(depth$strata %in% strata_range), "bul"] <- FALSE


  analysis_stratum1 <- NA
  analysis_stratum2 <- NA
  analysis_stratum3 <- NA
  analysis_stratum4 <- NA
  analysis_stratum5 <- NA
  analysis_stratum5 <- NA

  analysis_stratum1 <- depth[1,4]
  analysis_stratum2 <- depth[2,4]
  analysis_stratum3 <- depth[3,4]
  analysis_stratum4 <- depth[4,4]
  analysis_stratum5 <- depth[5,4]
  analysis_stratum6 <- depth[6,4]

  if (is.na(analysis_stratum1)){depth[1,4]<- FALSE; analysis_stratum1 <- FALSE}
  if (is.na(analysis_stratum2)){depth[2,4]<- FALSE; analysis_stratum2 <- FALSE}
  if (is.na(analysis_stratum3)){depth[3,4]<- FALSE; analysis_stratum3 <- FALSE}
  if (is.na(analysis_stratum4)){depth[4,4]<- FALSE; analysis_stratum4 <- FALSE}
  if (is.na(analysis_stratum5)){depth[5,4]<- FALSE; analysis_stratum5 <- FALSE}
  if (is.na(analysis_stratum6)){depth[6,4]<- FALSE; analysis_stratum6 <- FALSE}

  #------------------------------------------

  ddd <- data # merge_TATB

  year_range <- data.frame(year = sort(unique(data$YEAR)))

  res_table_cala2 <- data.frame(year=sort(unique(data$YEAR)))
  res_table_cala2$index_1 <- NA
  res_table_cala2$index_2 <- NA
  res_table_cala2$index_3 <- NA
  res_table_cala2$index_4 <- NA
  res_table_cala2$index_5 <- NA
  res_table_cala2$index_6 <- NA

  se_table2  <-  data.frame(year=sort(unique(data$YEAR)))
  se_table2$sum_1 <- NA
  se_table2$sum_2 <- NA
  se_table2$sum_3 <- NA
  se_table2$sum_4 <- NA
  se_table2$sum_5 <- NA
  se_table2$sum_6 <- NA
  se_table2$sd <- NA

  if (is.na(country)){
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$CODE==6,5])} else {area_s6 <- 0}
  } else {
    if (analysis_stratum1 == TRUE){area_s1 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==1,5])} else {area_s1 <- 0}
    if (analysis_stratum2 == TRUE){area_s2 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==2,5])} else {area_s2 <- 0}
    if (analysis_stratum3 == TRUE){area_s3 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==3,5])} else {area_s3 <- 0}
    if (analysis_stratum4 == TRUE){area_s4 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==4,5])} else {area_s4 <- 0}
    if (analysis_stratum5 == TRUE){area_s5 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==5,5])} else {area_s5 <- 0}
    if (analysis_stratum6 == TRUE){area_s6 <- sum(stratification[stratification$GSA == GSA & stratification$COUNTRY == country & stratification$CODE==6,5])} else {area_s6 <- 0}
  }

  peso_s1 <- area_s1/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s2 <- area_s2/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s3 <- area_s3/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s4 <- area_s4/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s5 <- area_s5/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)
  peso_s6 <- area_s6/sum(area_s1,area_s2,area_s3,area_s4,area_s5,area_s6)

  if (sampling =="RSS"){ # open RSS MIW
  for(i in 1:length(res_table_cala2[,1])) {

    data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]

    # s1 <- data2[data2$MEAN_DEPTH >= 10 & data2$MEAN_DEPTH < 50, ]
    # s2 <- data2[data2$MEAN_DEPTH >= 50 & data2$MEAN_DEPTH < 100, ]
    # s3 <- data2[data2$MEAN_DEPTH >= 100 & data2$MEAN_DEPTH < 200, ]
    # s4 <- data2[data2$MEAN_DEPTH >= 200 & data2$MEAN_DEPTH < 500, ]
    # s5 <- data2[data2$MEAN_DEPTH >=  500 & data2$MEAN_DEPTH <= 800,]

    if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
    if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
    if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
    if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
    if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
    if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}


    if (analysis_stratum1 == TRUE){
      #index computation
      s1 <- s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL ) & s1$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s1$mean_MIW <- (s1[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s1[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s1[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s1[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s1$SWEPT_AREA)
      res_table_cala2[i,2] <- s_MIW

      #se computation
      sq <- (s1$mean_MIW - s_MIW)^2
      sqA <- sq*s1$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s1[,1])-1)
      f1 <- s_a/area_s1
      se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)
    } else {area_s1 = 0}

    if (analysis_stratum2 == TRUE){
      #index computation
      s2 <- s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL ) & s2$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s2$mean_MIW <- (s2[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s2[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s2[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s2[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s2$SWEPT_AREA)
      res_table_cala2[i,3] <- s_MIW

      #se computation
      sq <- (s2$mean_MIW - s_MIW)^2
      sqA <- sq*s2$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s2[,1])-1)
      f2 <- s_a/area_s2
      se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
    } else {area_s2 = 0}

    if (analysis_stratum3 == TRUE){
      #index computation
      s3 <- s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL ) & s3$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s3$mean_MIW <- (s3[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s3[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s3[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s3[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s3$SWEPT_AREA)
      res_table_cala2[i,4] <- s_MIW

      #se computation
      sq <- (s3$mean_MIW - s_MIW)^2
      sqA <- sq*s3$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s3[,1])-1)
      f3 <- s_a/area_s3
      se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
    } else {area_s3 = 0}

    if (analysis_stratum4 == TRUE){
      #index computation
      s4 <- s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL ) & s4$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s4$mean_MIW <- (s4[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s4[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s4[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s4[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s4$SWEPT_AREA)
      res_table_cala2[i,5] <- s_MIW

      #se computation
      sq <- (s4$mean_MIW - s_MIW)^2
      sqA <- sq*s4$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s4[,1])-1)
      f4 <- s_a/area_s4
      se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
    } else {area_s4 = 0}

    if (analysis_stratum5 == TRUE){
      #index computation
      s5 <- s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL ) & s5$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s5$mean_MIW <- (s5[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s5[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s5[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s5[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s5$SWEPT_AREA)
      res_table_cala2[i,6] <- s_MIW

      #se computation
      sq <- (s5$mean_MIW - s_MIW)^2
      sqA <- sq*s5$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s5[,1])-1)
      f5 <- s_a/area_s5
      se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
    } else {area_s5 = 0}

    if (analysis_stratum6 == TRUE){
      #index computation
      s6 <- s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL ) & s6$TOTAL_NUMBER_IN_THE_HAUL>0, ]
      s6$mean_MIW <- (s6[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s6[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
      s_MIW<- sum(s6[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s6[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
      s_a <- sum(s6$SWEPT_AREA)
      res_table_cala2[i,7] <- s_MIW

      #se computation
      sq <- (s6$mean_MIW - s_MIW)^2
      sqA <- sq*s6$SWEPT_AREA
      sum_sqA <- sum(sqA)/(length(s6[,1])-1)
      f6 <- s_a/area_s6
      se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
    } else {area_s6 = 0}

    positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
    total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

    sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
    res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
    colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
    se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
  }
} # close RSS MIW

  if (sampling =="RPS"){ # open RPS MIW

    for(i in 1:length(res_table_cala2[,1])) {

      data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]
      n <- sum(ddd$SWEPT_AREA)
      n2 <- n^2

      if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
      if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
      if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
      if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
      if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
      if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}


      if (analysis_stratum1 == TRUE){
        #index computation
        s1 <- s1[!is.na(s1$TOTAL_NUMBER_IN_THE_HAUL ) & s1$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s1$mean_MIW <- (s1[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s1[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s1[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s1[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s1$SWEPT_AREA)
        res_table_cala2[i,2] <- s_MIW

        #se computation
        sq <- (s1$mean_MIW - s_MIW)^2
        sqA <- sq*s1$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s1[,1])-1)
        f1 <- s_a/area_s1
        se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)
        coef_s1 <- ((1-peso_s1) * sum_sqA)/n2
        se_table2[i,2] <- se_table2[i,2] + coef_s1
      } else {area_s1 = 0}

      if (analysis_stratum2 == TRUE){
        #index computation
        s2 <- s2[!is.na(s2$TOTAL_NUMBER_IN_THE_HAUL ) & s2$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s2$mean_MIW <- (s2[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s2[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s2[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s2[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s2$SWEPT_AREA)
        res_table_cala2[i,3] <- s_MIW

        #se computation
        sq <- (s2$mean_MIW - s_MIW)^2
        sqA <- sq*s2$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s2[,1])-1)
        f2 <- s_a/area_s2
        se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
        coef_s2 <- ((1-peso_s2) * sum_sqA)/n2
        se_table2[i,3] <- se_table2[i,3] + coef_s2
      } else {area_s2 = 0}

      if (analysis_stratum3 == TRUE){
        #index computation
        s3 <- s3[!is.na(s3$TOTAL_NUMBER_IN_THE_HAUL ) & s3$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s3$mean_MIW <- (s3[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s3[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s3[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s3[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s3$SWEPT_AREA)
        res_table_cala2[i,4] <- s_MIW

        #se computation
        sq <- (s3$mean_MIW - s_MIW)^2
        sqA <- sq*s3$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s3[,1])-1)
        f3 <- s_a/area_s3
        se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
        coef_s3 <- ((1-peso_s3) * sum_sqA)/n2
        se_table2[i,4] <- se_table2[i,4] + coef_s3
      } else {area_s3 = 0}

      if (analysis_stratum4 == TRUE){
        #index computation
        s4 <- s4[!is.na(s4$TOTAL_NUMBER_IN_THE_HAUL ) & s4$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s4$mean_MIW <- (s4[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s4[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s4[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s4[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s4$SWEPT_AREA)
        res_table_cala2[i,5] <- s_MIW

        #se computation
        sq <- (s4$mean_MIW - s_MIW)^2
        sqA <- sq*s4$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s4[,1])-1)
        f4 <- s_a/area_s4
        se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
        coef_s4 <- ((1-peso_s4) * sum_sqA)/n2
        se_table2[i,5] <- se_table2[i,5] + coef_s4
      } else {area_s4 = 0}

      if (analysis_stratum5 == TRUE){
        #index computation
        s5 <- s5[!is.na(s5$TOTAL_NUMBER_IN_THE_HAUL ) & s5$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s5$mean_MIW <- (s5[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s5[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s5[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s5[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s5$SWEPT_AREA)
        res_table_cala2[i,6] <- s_MIW

        #se computation
        sq <- (s5$mean_MIW - s_MIW)^2
        sqA <- sq*s5$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s5[,1])-1)
        f5 <- s_a/area_s5
        se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
        coef_s5 <- ((1-peso_s5) * sum_sqA)/n2
        se_table2[i,6] <- se_table2[i,6] + coef_s5
      } else {area_s5 = 0}

      if (analysis_stratum6 == TRUE){
        #index computation
        s6 <- s6[!is.na(s6$TOTAL_NUMBER_IN_THE_HAUL ) & s6$TOTAL_NUMBER_IN_THE_HAUL>0, ]
        s6$mean_MIW <- (s6[ , "TOTAL_WEIGHT_IN_THE_HAUL" ]/1000) / s6[ , "TOTAL_NUMBER_IN_THE_HAUL" ]
        s_MIW<- sum(s6[ ,"TOTAL_WEIGHT_IN_THE_HAUL"]/1000)/sum(s6[ ,"TOTAL_NUMBER_IN_THE_HAUL"])
        s_a <- sum(s6$SWEPT_AREA)
        res_table_cala2[i,7] <- s_MIW

        #se computation
        sq <- (s6$mean_MIW - s_MIW)^2
        sqA <- sq*s6$SWEPT_AREA
        sum_sqA <- sum(sqA)/(length(s6[,1])-1)
        f6 <- s_a/area_s6
        se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
        coef_s6 <- ((1-peso_s6) * sum_sqA)/n2
        se_table2[i,7] <- se_table2[i,7] + coef_s6
      } else {area_s6 = 0}

      positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"TOTAL_NUMBER_IN_THE_HAUL"])
      total_hauls[i] <- length(data2[data2$YEAR == res_table_cala2[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

      sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
      res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
      colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
      se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
    }

  } # close RPS MIW

  timeseries <- data.frame(year = res_table_cala2[,"year"], MIW = res_table_cala2[,"Indices"], sd= se_table2[, "sd"])
  timeseries$CV <- timeseries[,"sd"] / timeseries[,"MIW"]*100
  timeseries$invCV <- 1/timeseries$CV
  timeseries$positive_hauls_perc <- positive_hauls / total_hauls * 100
  colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")

  if (plot){
  # plot timeseries of mean indices
  main <- paste(sspp,"_GSA",GSA,"_(",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  main.lab <- paste(sspp," GSA",GSA," (",dependent,")_",depth_range[1],"-",depth_range[2], " m", sep="")
  max_index <- max(timeseries[,"MIW"]) + max(timeseries[!is.na(timeseries$sd),"sd"]*1.2)

  oldoptions <- options()$warn
  old_par <- list()
  old_par$mfrow <-par()$mfrow
  old_par$oma <-par()$oma
  old_par$mgp <-par()$mgp
  on.exit(c(par(mfrow=old_par$mfrow, oma=old_par$oma, mgp=old_par$mgp), options(warn=oldoptions)))
  options(warn=-1)
  par(mfrow=c(1,1),oma=c(1,1,1,1), mgp=c(2, 1,0))
  plot(timeseries[,"year"],  timeseries[,"MIW"], type="b", col="black", pch=16, xlab="year", ylim=c(0,max_index*1.2), ylab=dep_text, main=main.lab) # ylim=c(0,max_index*1.2)
  lines(timeseries[,"year"], (timeseries[,"MIW"]-1.96*timeseries[,"sd"]), type="l",lty=2, col="red" )
  lines(timeseries[,"year"], (timeseries[,"MIW"]+1.96*timeseries[,"sd"]), type="l",lty=2, col="red" )
  legend("topright", c("time series", "CI"), lty=c(1,2), pch=c(16, NA), col=c("black","red"))
  }

  if (verbose){
    message("\n Estimation of MIW completed \n")

  if (sex == "f" | sex=="m"){
    warning('The selection of sex is not allowed for the estimation of MIW
            \nThe default value (sex="c") will be used',immediate. = TRUE)
  }
  }
  }

  return(timeseries)
}
