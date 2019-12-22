
sexratio.ts <- function(merge, GSA = NA, sspp = NA, depth_range,  strata_scheme, stratification, country=NA, plot=TRUE ) {

  if (FALSE) {
    merge <- m.TATB(TA,TB,"ARISFOL")
    depth_range = c(200,800)
    country <- NA
    sspp <- "ARISFOL"
    GSA <- NA
    strata_scheme <- strata_scheme
    stratification <- stratification_scheme
    plot <- TRUE
  }

merge_TATB <- merge

if (is.na(sspp)) {
  GENERE <- as.character(unique(merge_TATB$GENUS)[unique(merge_TATB$GENUS) != -1])
  SPECIE <- as.character(unique(merge_TATB$SPECIES)[unique(merge_TATB$SPECIES) != -1])
  sspp <- paste(GENERE,SPECIE, sep="")
  species <- sspp
}


if (is.na(GSA)) {
  GSA <- unique(merge_TATB$GSA)
}

species <- sspp

merge_TATB <- merge_TATB[merge_TATB$GSA == GSA , ]

#------------------------------------------
# selection of depth range

if (is.na(country)){
  ddd <- merge_TATB[merge_TATB$GSA == GSA , ]
  strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == as.character(unique(merge_TATB$COUNTRY)[1]), ]
}else{
  if (!is.na(country)){
    ddd <- merge_TATB[merge_TATB$GSA == GSA &  merge_TATB$COUNTRY == country, ]
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
ddd <-  ddd[ddd$MEAN_DEPTH>=depth_range[1] & ddd$MEAN_DEPTH<depth_range[2],]
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

# ddd <- merge_TATB
ddd$NB_FM <- ddd$NB_OF_FEMALES + ddd$NB_OF_MALES
year_range <- data.frame(year = sort(unique(merge_TATB$YEAR)))

res_table_calaF <- data.frame(year=sort(unique(merge_TATB$YEAR)))
res_table_calaF$index_1 <- NA
res_table_calaF$index_2 <- NA
res_table_calaF$index_3 <- NA
res_table_calaF$index_4 <- NA
res_table_calaF$index_5 <- NA
res_table_calaF$index_6 <- NA

res_table_calaFM <- data.frame(year=sort(unique(merge_TATB$YEAR)))
res_table_calaFM$index_1 <- NA
res_table_calaFM$index_2 <- NA
res_table_calaFM$index_3 <- NA
res_table_calaFM$index_4 <- NA
res_table_calaFM$index_5 <- NA
res_table_calaFM$index_6 <- NA

SR <- data.frame(year=sort(unique(merge_TATB$YEAR)))
SR$Indices_F <- NA
SR$Indices_FM <- NA
SR$sex.ratio <- NA
SR$variance <- NA
SR$sd <- NA
SR$positive_hauls_perc <- NA

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

positive_hauls <- 0
total_hauls <- 0
i=1
for(i in 1:length(res_table_calaFM[,1])) {

  data2 <- ddd[ddd$YEAR == res_table_calaFM[i,1] , ]

  if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
  if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
  if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
  if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
  if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
  if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

  if (analysis_stratum1 == TRUE){
    s_nF <- sum(s1[!is.na(s1$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s1[!is.na(s1$NB_FM),"NB_FM"])
    s_a <- sum(s1$SWEPT_AREA)
    res_table_calaF[i,2] <- s_nF/s_a
    res_table_calaFM[i,2] <- s_nFM/s_a
  } else {area_s1 = 0}

  if (analysis_stratum2 == TRUE){
    s_nF <- sum(s2[!is.na(s2$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s2[!is.na(s2$NB_FM),"NB_FM"])
    s_a <- sum(s2$SWEPT_AREA)
    res_table_calaF[i,3] <- s_nF/s_a
    res_table_calaFM[i,3] <- s_nFM/s_a
  } else {area_s2 = 0}

  if (analysis_stratum3 == TRUE){
    s_nF <- sum(s3[!is.na(s3$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s3[!is.na(s3$NB_FM),"NB_FM"])
    s_a <- sum(s3$SWEPT_AREA)
    res_table_calaF[i,4] <- s_nF/s_a
    res_table_calaFM[i,4] <- s_nFM/s_a
  } else {area_s3 = 0}

  if (analysis_stratum4 == TRUE){
    s_nF <- sum(s4[!is.na(s4$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s4[!is.na(s4$NB_FM),"NB_FM"])
    s_a <- sum(s4$SWEPT_AREA)
    res_table_calaF[i,5] <- s_nF/s_a
    res_table_calaFM[i,5] <- s_nFM/s_a
  } else {area_s4 = 0}

  if (analysis_stratum5 == TRUE){
    #index computation
    s_nF <- sum(s5[!is.na(s5$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s5[!is.na(s5$NB_FM),"NB_FM"])
    s_a <- sum(s5$SWEPT_AREA)
    res_table_calaF[i,6] <- s_nF/s_a
    res_table_calaFM[i,6] <- s_nFM/s_a
  } else {area_s5 = 0}

  if (analysis_stratum6 == TRUE){
    #index computation
    s_nF <- sum(s6[!is.na(s6$NB_OF_FEMALES),"NB_OF_FEMALES"])
    s_nFM <- sum(s6[!is.na(s6$NB_FM),"NB_FM"])
    s_a <- sum(s6$SWEPT_AREA)
    res_table_calaF[i,7] <- s_nF/s_a
    res_table_calaFM[i,7] <- s_nFM/s_a
  } else {area_s6 = 0}

  positive_hauls[i] <- length(data2[data2$TOTAL_NUMBER_IN_THE_HAUL>0 & data2$YEAR == res_table_calaFM[i,1],"TOTAL_NUMBER_IN_THE_HAUL"])
  total_hauls[i] <- length(data2[data2$YEAR == res_table_calaFM[i,1] & data2$MEAN_DEPTH >= depth_range[1] & data2$MEAN_DEPTH < depth_range[2],"HAUL_NUMBER"])

  sum_res_est_F <- c(res_table_calaF[i,2]*peso_s1,res_table_calaF[i,3]*peso_s2,res_table_calaF[i,4]*peso_s3,res_table_calaF[i,5]*peso_s4,res_table_calaF[i,6]*peso_s5,res_table_calaF[i,7]*peso_s6)
  sum_res_est_FM <- c(res_table_calaFM[i,2]*peso_s1,res_table_calaFM[i,3]*peso_s2,res_table_calaFM[i,4]*peso_s3,
                      res_table_calaFM[i,5]*peso_s4,res_table_calaFM[i,6]*peso_s5,res_table_calaFM[i,7]*peso_s6)

  res_table_calaF[i, 8]<- sum(sum_res_est_F[!is.na(sum_res_est_F)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
  res_table_calaFM[i, 8]<- sum(sum_res_est_FM[!is.na(sum_res_est_FM)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
  colnames(res_table_calaF) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices_F")
  colnames(res_table_calaFM) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices_FM")
  SR$year[i] <- res_table_calaF$year[i]
  SR$Indices_F[i] <- res_table_calaF$Indices_F[i]
  SR$Indices_FM[i] <- res_table_calaFM$Indices_FM[i]
  SR$sex.ratio[i] <- SR$Indices_F[i] / SR$Indices_FM[i]
  SR$variance[i] <- sqrt(SR$sex.ratio[i]*(1-SR$sex.ratio[i]))/res_table_calaFM[i,8]
  SR$sd[i] <- sqrt(  SR$variance[i]  )
  SR$positive_hauls_perc[i] <- positive_hauls[i] / total_hauls[i] * 100
}

timeseries <-SR


# plot sex-ratio
    main <- paste(sspp,"_GSA",GSA,"_(Sex ratio)_",depth_range[1],"-",depth_range[2], "m", sep="")
    main.lab <- paste(sspp," GSA",GSA," (Sex ratio)_",depth_range[1],"-",depth_range[2], "m", sep="")
    max_index <- 1+max(sqrt(timeseries[!is.na(timeseries$sd),5]))

  if (plot){
    oldoptions <- options()$warn
    old_par <- list()
    old_par$mfrow <-par()$mfrow
    old_par$oma <-par()$oma
    old_par$mgp <-par()$mgp
    on.exit(c(par(mfrow=old_par$mfrow, oma=old_par$oma, mgp=old_par$mgp), options(warn=oldoptions)))
	  options(warn=-1)
    par(mfrow=c(1,1),oma=c(1,1,1,1), mgp=c(2, 1,0))
    plot(timeseries[,1],  timeseries[,4], type="b", col="black", pch=16, xlab="year", ylim=c(0,max_index*1.2), ylab="sex ratio (FF/(FF+MM))", main=main.lab) # ylim=c(0,max_index*1.2)
    lines(timeseries[,1], (timeseries[,4]-1.96*sqrt(timeseries[,5])), type="l",lty=2, col="red" )
    lines(timeseries[,1], (timeseries[,4]+1.96*sqrt(timeseries[,5])), type="l",lty=2, col="red" )
    legend("topright", c("Sex ratio"), lty=c(1,2), pch=c(16), col=c("black"))
   }

    return(timeseries)
}
