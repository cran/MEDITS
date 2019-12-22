

index.rs <- function (TA,TB,TC, GSA = NA, sspp , stage ="recruits", cutoff,  depth_range,  str.scheme=strata_scheme, surf=stratification, country=NA, plot=TRUE ) {   #,  strata_scheme, stratification
  if (FALSE){
    # require(ggplot2)
    GSA = NA
    sspp="ARISFOL"
    stage ="recruits"
    cutoff=29
    depth_range=c(200,800)
    str.scheme=strata_scheme
    surf=stratification_scheme
    country=NA
    plot=TRUE
    }
  strata_scheme <- str.scheme
  stratification <- surf
  if (is.na(sspp)) {
    GENERE <- as.character(unique(TB$GENUS)[unique(TB$GENUS) != -1])
    SPECIE <- as.character(unique(TB$SPECIES)[unique(TB$SPECIES) != -1])
    sspp <- paste(GENERE,SPECIE, sep="")
    species <- sspp
  } else {
    species <- sspp
  }


  if (is.na(GSA)) {
    GSA <- unique(TA$AREA)
  }


  merge_TATB <- m.TATB(TA,TB,sspp)
  merge_TATC <- m.TATC(TA,TC,sspp)

  merge_TATB <- merge_TATB[merge_TATB$GSA == GSA , ]
  merge_TATC <- merge_TATC[merge_TATC$GSA == GSA , ]

  #-----------------------
  # ABUNDANCE INDICES
  #-----------------------

  dependent <- "abundance"


dep_text <-expression(paste(Abundance, (n/km^2), sep=" "))
varcol <- which(colnames(merge_TATC)=="N_km2")
col_response <- which(colnames(merge_TATC)=="N_km2")


threshold <- cutoff

sex <- "F"

#------------------------------------------
# selection of depth range

if (is.na(country)){
  data <- merge_TATC[merge_TATC$GSA == GSA , ]
  strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == as.character(unique(merge_TATB$COUNTRY)[1]), ]
}else{
  if (!is.na(country)){
    data <- merge_TATC[merge_TATC$GSA == GSA &  merge_TATC$COUNTRY == country, ]
    strata.scheme <- strata_scheme[strata_scheme$GSA == GSA & strata_scheme$COUNTRY == country, ]
  }
}

# strata <- unique(strata.scheme$CODE)
# s <- data.frame(matrix(NA,ncol=5,nrow=1))
# colnames(s) <- colnames(strata.scheme)
# i=1
# for (i in 1:length(strata)){
#   s[i,] <- strata.scheme[(strata.scheme$CODE == strata[i]),][1,]
#   s[i,2] <- as.character(strata.scheme[(strata.scheme$CODE == strata[i]),2][1])
# }
#
# strata.scheme <- s

depth <- data.frame(matrix(NA,ncol=4, nrow=length(strata.scheme$CODE)))
colnames(depth) <- c("strata", "min", "max", "bul")
depth$strata <- strata.scheme$CODE # c(1,2,3,4,5)
depth$min <-strata.scheme$MIN_DEPTH  # c(10,50,100,200,500)
depth$max <-strata.scheme$MAX_DEPTH  # c(50,100,200,500,800)


depth_range <- data.frame(depth_range); depth_range <- as.numeric(as.character(depth_range[,1]))
if (depth_range[2] != 800) {depth_range[2] <- depth_range[2]}
data <-  data[data$MEAN_DEPTH>depth_range[1] & data$MEAN_DEPTH<=depth_range[2],]
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


data$n_raised <- data$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE * data$RAISING_FACTOR

if (stage=="recruits"){
ddd <- data[data$LENGTH_CLASS <= threshold,]
} else { if (stage=="spawners"){
  ddd <- data[data$LENGTH_CLASS >= threshold  & data$SEX ==sex  ,]
}
}


ddd_n <- aggregate(ddd$n_raised, by=list(ddd$id), sum) # [,1])
colnames(ddd_n) <- c("id","sum_N")
ddd_nkm2 <-  aggregate(ddd$N_km2, by=list(ddd$id), sum)
colnames(ddd_nkm2) <- c("id","sum_nkm2")
merge_N <- merge(ddd_n, ddd_nkm2, by.x="id")


ddd <- merge(merge_TATB[,c("id","GSA","YEAR","HAUL_NUMBER","GENUS","SPECIES","MEAN_DEPTH","SWEPT_AREA")], merge_N, by.y="id",all.x =TRUE)   # 1,3,4,6,22,23,34,35
ddd <- ddd[ddd$MEAN_DEPTH >=depth_range[1] & ddd$MEAN_DEPTH < depth_range[2],]

for (i in 1:length(ddd$sum_N)){
  if (is.na(ddd[i,"sum_N" ])) {ddd[i,"sum_N" ] <- 0} else {ddd[i,"sum_N" ] <- ddd[i,"sum_N" ]}
  if (is.na(ddd[i,"sum_nkm2" ])) {ddd[i,"sum_nkm2" ] <- 0} else {ddd[i,"sum_nkm2" ] <- ddd[i,"sum_nkm2" ]}
}

year_range <- data.frame(year = sort(unique(merge_TATC$YEAR)))

res_table_cala2 <- data.frame(year=sort(unique(merge_TATC$YEAR)))
res_table_cala2$index_1 <- NA
res_table_cala2$index_2 <- NA
res_table_cala2$index_3 <- NA
res_table_cala2$index_4 <- NA
res_table_cala2$index_5 <- NA
res_table_cala2$index_6 <- NA

se_table2  <-  data.frame(year=sort(unique(merge_TATC$YEAR)))
se_table2$sum_1 <- NA
se_table2$sum_2 <- NA
se_table2$sum_3 <- NA
se_table2$sum_4 <- NA
se_table2$sum_5 <- NA
se_table2$sum_6 <- NA
se_table2$sd <- NA

se_table_a  <-  data.frame(year=sort(unique(merge_TATC$YEAR)))
se_table_a$sum_1 <- NA
se_table_a$sum_2 <- NA
se_table_a$sum_3 <- NA
se_table_a$sum_4 <- NA
se_table_a$sum_5 <- NA
se_table_a$sum_6 <- NA
se_table_a$sd <- NA

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
for(i in 1:length(res_table_cala2[,1])) {


  data2 <- ddd[ddd$YEAR == res_table_cala2[i,1] , ]

  if (analysis_stratum1 == TRUE) {s1 <- data2[data2$MEAN_DEPTH >= depth[1,2] & data2$MEAN_DEPTH < depth[1,3], ]}
  if (analysis_stratum2 == TRUE) {s2 <- data2[data2$MEAN_DEPTH >= depth[2,2] & data2$MEAN_DEPTH < depth[2,3], ]}
  if (analysis_stratum3 == TRUE) {s3 <- data2[data2$MEAN_DEPTH >= depth[3,2] & data2$MEAN_DEPTH < depth[3,3], ]}
  if (analysis_stratum4 == TRUE) {s4 <- data2[data2$MEAN_DEPTH >= depth[4,2] & data2$MEAN_DEPTH < depth[4,3], ]}
  if (analysis_stratum5 == TRUE) {s5 <- data2[data2$MEAN_DEPTH >= depth[5,2] & data2$MEAN_DEPTH < depth[5,3], ]}
  if (analysis_stratum6 == TRUE) {s6 <- data2[data2$MEAN_DEPTH >= depth[6,2] & data2$MEAN_DEPTH < depth[6,3], ]}

  if (analysis_stratum1 == TRUE){
    s_n <- sum(s1[!is.na(s1$sum_N),"sum_N"])
    s_a <- sum(s1$SWEPT_AREA)
    res_table_cala2[i,2] <- s_n/s_a

    mean_ind <- mean(s1[!is.na(s1$sum_nkm2), "sum_nkm2" ])
    sq <- (s1[!is.na(s1$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s1$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s1[,1])-1)
    f1 <- s_a/area_s1
    se_table2[i,2] <- (((peso_s1)^2 * sum_sqA)/ s_a)* (1-f1)

  } else {area_s1 = 0}

  if (analysis_stratum2 == TRUE){
    s_n <- sum(s2[!is.na(s2$sum_N),"sum_N"])
    s_a <- sum(s2$SWEPT_AREA)
    res_table_cala2[i,3] <- s_n/s_a

    mean_ind <- mean(s2[!is.na(s2$sum_nkm2), "sum_nkm2" ])
    sq <- (s2[!is.na(s2$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s2$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s2[,1])-1)
    f2 <- s_a/area_s2
    se_table2[i,3] <- (((peso_s2)^2 * sum_sqA)/ s_a)* (1-f2)
  } else {area_s2 = 0}

  if (analysis_stratum3 == TRUE){
    s_n <- sum(s3[!is.na(s3$sum_N),"sum_N"])
    s_a <- sum(s3$SWEPT_AREA)
    res_table_cala2[i,4] <- s_n/s_a

    mean_ind <- mean(s3[!is.na(s3$sum_nkm2), "sum_nkm2" ])
    sq <- (s3[!is.na(s3$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s3$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s3[,1])-1)
    f3 <- s_a/area_s3
    se_table2[i,4] <- (((peso_s3)^2 * sum_sqA)/ s_a)* (1-f3)
  } else {area_s3 = 0}

  if (analysis_stratum4 == TRUE){
    s_n <- sum(s4[!is.na(s4$sum_N),"sum_N"])
    s_a <- sum(s4$SWEPT_AREA)
    res_table_cala2[i,5] <- s_n/s_a

    mean_ind <- mean(s4[!is.na(s4$sum_nkm2), "sum_nkm2" ])
    sq <- (s4[!is.na(s4$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s4$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s4[,1])-1)
    f4 <- s_a/area_s4
    se_table2[i,5] <- (((peso_s4)^2 * sum_sqA)/ s_a)* (1-f4)
  } else {area_s4 = 0}

  if (analysis_stratum5 == TRUE){
    #index computation
    s_n <- sum(s5[!is.na(s5$sum_N),"sum_N"])
    s_a <- sum(s5$SWEPT_AREA)
    res_table_cala2[i,6] <- s_n/s_a

    #se computation
    mean_ind <- mean(s5[!is.na(s5$sum_nkm2), "sum_nkm2" ])
    sq <- (s5[!is.na(s5$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s5$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s5[,1])-1)
    f5 <- s_a/area_s5
    se_table2[i,6] <- (((peso_s5)^2 * sum_sqA)/ s_a)* (1-f5)
  } else {area_s5 = 0}

  if (analysis_stratum6 == TRUE){
    #index computation
    s_n <- sum(s6[!is.na(s6$sum_N),"sum_N"])
    s_a <- sum(s6$SWEPT_AREA)
    res_table_cala2[i,7] <- s_n/s_a

    #se computation
    mean_ind <- mean(s6[!is.na(s6$sum_nkm2), "sum_nkm2" ])
    sq <- (s6[!is.na(s6$sum_nkm2),"sum_nkm2"] - mean_ind)^2
    sqA <- sq*s6$SWEPT_AREA
    sum_sqA <- sum(sqA)/(length(s6[,1])-1)
    f6 <- s_a/area_s6
    se_table2[i,7] <- (((peso_s6)^2 * sum_sqA)/ s_a)* (1-f6)
  } else {area_s6 = 0}

  # source(paste(wd, "/scripts/Index_estimation_cala_recruits.R", sep=""), encoding = 'UTF-8')
  sum_res_est <- c(res_table_cala2[i,2]*peso_s1,res_table_cala2[i,3]*peso_s2,res_table_cala2[i,4]*peso_s3,res_table_cala2[i,5]*peso_s4,res_table_cala2[i,6]*peso_s5,res_table_cala2[i,7]*peso_s6)
  res_table_cala2[i, 8]<- sum(sum_res_est[!is.na(sum_res_est)])# /sum(area_s1,area_s2,area_s3,area_s4,area_s5)
  colnames(res_table_cala2) <- c("year", "stratum 1","stratum 2", "stratum 3", "stratum 4", "stratum 5", "stratum 6", "Indices")
  se_table2[i, "sd"] <-sqrt(rowSums(se_table2[i, 2:7], na.rm = TRUE))
}

timeseries <- data.frame(year = res_table_cala2[,1], abundance = res_table_cala2[,8], sd= se_table2[, "sd"])

if (plot){
  oldoptions <- options()$warn
  old_par <- list()
  old_par$mfrow <-par()$mfrow
  old_par$oma <-par()$oma
  old_par$mgp <-par()$mgp

# plot timeseries of mean spawners indices
main <- paste(sspp,"_GSA",GSA,"_(abundance of ", stage,")_",depth_range[1],"-",depth_range[2], " m", sep="")
main.lab <- paste(sspp," GSA",GSA," (abundance of ", stage,")_",depth_range[1],"-",depth_range[2], " m", sep="")
max_index <- max(timeseries[,2]) + max(timeseries[!is.na(timeseries$sd),3]*1.5)

on.exit(c(par(mfrow=old_par$mfrow, oma=old_par$oma, mgp=old_par$mgp), options(warn=oldoptions)))
  options(warn=-1)
par(mfrow=c(1,1),oma=c(1,1,1,1), mgp=c(2, 1,0))
plot(timeseries[,1],  timeseries[,2], type="b", col="black", pch=16, xlab="year", ylim=c(0,max_index*1.2), ylab=dep_text, main=main.lab) # ylim=c(0,max_index*1.2)
lines(timeseries[,1], (timeseries[,2]-1.96*timeseries[,3]), type="l",lty=2, col="red" )
lines(timeseries[,1], (timeseries[,2]+1.96*timeseries[,3]), type="l",lty=2, col="red" )
legend("topright", c("time series", "CI"), lty=c(1,2), pch=c(16, NA), col=c("black","red"))

}

return(timeseries)
}
