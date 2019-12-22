
### MERGE TA-TC files for a given species
# TA <- TA18
# TB <- TB18
# TC <- TC18

# sspp <- "MERLMER"
# TA <- read.csv("~/__ DATI MEDITS AGGIORNATI __/GSA18 2017 TA.csv", sep=";")
# TC <- read.csv("~/__ DATI MEDITS AGGIORNATI __/GSA18 2017 TC.csv", sep=";")
# mmm <- m.TATC(TA, TC, "ARISFOL")

m.TATC <- function(TA,TC, sspp, str.scheme=strata_scheme, verbose = FALSE) {
  strata_scheme <- str.scheme
  # require(hms)
  # require(tibble)
  id_TA <- data.frame(id = paste(TA$AREA,TA$COUNTRY,TA$YEAR,"_", TA$VESSEL, TA$MONTH, TA$DAY,"_", TA$HAUL_NUMBER, sep = ""))
  id_TC <- data.frame(id = paste(TC$AREA,TC$COUNTRY,TC$YEAR,"_", TC$VESSEL, TC$MONTH, TC$DAY,"_", TC$HAUL_NUMBER, sep = ""))
  colnames(TA)[which(colnames(TA) == "AREA")] <- "GSA"
  colnames(TC)[which(colnames(TC) == "AREA")] <- "GSA"
  TA_merge <- cbind(id_TA,TA)
  TC_merge <- cbind(id_TC,TC)
  TC_merge$GENUS <- as.character(TC_merge$GENUS)
  TC_merge$SPECIES <- as.character(TC_merge$SPECIES)

  sspp <- toupper(sspp)
  species <- substr(sspp,1,4)
  species[2] <- substr(sspp,5,7)

  TA_merge <- TA_merge[,which(colnames(TA_merge) %in% TA_cols)]
  TC_merge <- TC_merge[TC_merge$GENUS == species[1] & TC_merge$SPECIES == species[2], which(colnames(TC_merge) %in% TC_cols)]

  ########################
  ##    MERGE TA-TC     ##
  ########################
  if (verbose) {message("Merging TA-TC files\n")}

  merge_TATC <- merge(TA_merge, TC_merge, by.x = "id", by.y = "id", all.x = TRUE, all.y = TRUE)
  merge_TATC$MEDITS_CODE <- paste(merge_TATC$GENUS, merge_TATC$SPECIES)
  merge_TATC <- tibble::add_column(merge_TATC, TYPE_OF_FILE = "TATC", .after = "id")


  merge_TATC$LENGTH_CLASSES_CODE <- as.character(merge_TATC$LENGTH_CLASSES_CODE)
  merge_TATC$SEX <- as.character(merge_TATC$SEX)
  merge_TATC$MATURITY <- as.character(merge_TATC$MATURITY)
  merge_TATC$MATSUB <- toupper(as.character(merge_TATC$MATSUB))
  l_TATC <- length (merge_TATC[,1])
  i=2
  for (i in 1:l_TATC){
    if (merge_TATC[i, "MEDITS_CODE"] == "NA NA"){
      merge_TATC[i, "MEDITS_CODE"              ] <- -1
      merge_TATC[i, "GENUS"                    ] <- -1
      merge_TATC[i, "SPECIES"                  ] <- -1
      merge_TATC[i, "SEX"                      ] <- -1
      merge_TATC[i, "LENGTH_CLASSES_CODE"      ] <- -1
      merge_TATC[i, "WEIGHT_OF_THE_FRACTION"   ] <-  0
      merge_TATC[i, "WEIGHT_OF_THE_SAMPLE_MEASURED"   ] <- 0
      merge_TATC[i, "MATURITY"                 ] <- -1
      merge_TATC[i, "MATSUB"                   ] <- -1
      merge_TATC[i, "LENGTH_CLASS"             ] <- -1
      merge_TATC[i, "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE"] <- 0
    }

    merge_TATC[i,]

  }

  coord <- MEDITS.to.dd(merge_TATC)
  merge_TATC$MEAN_LATITUDE_DEC <- (coord$SHOOTING_LATITUDE+coord$HAULING_LATITUDE)/2
  merge_TATC$MEAN_LONGITUDE_DEC <- (coord$SHOOTING_LONGITUDE+coord$HAULING_LONGITUDE)/2
  merge_TATC$MEAN_DEPTH <- (merge_TATC$SHOOTING_DEPTH+merge_TATC$HAULING_DEPTH)/2
  merge_TATC$SWEPT_AREA <- merge_TATC$DISTANCE * merge_TATC$WING_OPENING/10000000

  strata_scheme_GSA <- strata_scheme[strata_scheme$GSA == unique(merge_TATC$GSA) , ]

  i=1
  hour_shooting <- 0
  min_shooting <- 0
  hour_hauling <- 0
  min_hauling <- 0

  for (i in 1:l_TATC){

    for (j in 1:length(strata_scheme_GSA$CODE)){
      if  (merge_TATC$MEAN_DEPTH[i] > strata_scheme_GSA[j,4] & merge_TATC$MEAN_DEPTH[i] <= strata_scheme_GSA[j,5]) {merge_TATC$STRATUM_CODE[i] <- strata_scheme_GSA[j,3]}
    }

    if (nchar(merge_TATC$SHOOTING_TIME[i])==4) {hour_shooting[i] <- substr(merge_TATC$SHOOTING_TIME[i],1,2); min_shooting[i] <- substr(merge_TATC$SHOOTING_TIME[i],3,4)} else {
      if (nchar(merge_TATC$SHOOTING_TIME[i])==3) {hour_shooting[i] <- substr(merge_TATC$SHOOTING_TIME[i],1,1); min_shooting[i] <- substr(merge_TATC$SHOOTING_TIME[i],2,3)}}

    if (nchar(merge_TATC$HAULING_TIME[i])==4) {hour_hauling[i] <- substr(merge_TATC$HAULING_TIME[i],1,2); min_hauling[i] <- substr(merge_TATC$HAULING_TIME[i],3,4)} else {
      if (nchar(merge_TATC$HAULING_TIME[i])==3) {hour_hauling[i] <- substr(merge_TATC$HAULING_TIME[i],1,1); min_hauling[i] <- substr(merge_TATC$HAULING_TIME[i],2,3)}}
  }



  hms_shooting <- hms::hms( rep (0, length(hour_shooting)), as.numeric(min_shooting), as.numeric(hour_shooting))
  hms_hauling  <- hms::hms( rep (0, length(hour_shooting)), as.numeric(min_hauling), as.numeric(hour_hauling))

  duration <- 0
  k <- 176
  for (k in 1:length(hms_shooting)){
    if (hms_hauling[k] > hms_shooting[k]) {
      duration[k] <-  as.numeric(hms_hauling[k] - hms_shooting[k])/3600
    } else {
      duration[k] <- as.numeric((hms_hauling[k]+ hms(0,0,24)) - hms_shooting[k])/3600
    }
  }

  merge_TATC$SHOOTING_TIME <- hms_shooting
  merge_TATC$HAULING_TIME  <- hms_hauling

  i=2
  for (i in 1:l_TATC){
    if(merge_TATC[i, "MATURITY"] == -1) {merge_TATC[i, "MATURITY_STAGE"] <- -1 }
    if(merge_TATC[i, "MATURITY"] == 0) {merge_TATC[i, "MATURITY_STAGE"] <- 0 }
    if(merge_TATC[i, "MATURITY"] == 1) {merge_TATC[i, "MATURITY_STAGE"] <- 1 }
    if(merge_TATC[i, "MATURITY"] == 2 & !(merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F"))) {merge_TATC[i, "MATURITY_STAGE"] <- 2 } else {
      if (merge_TATC[i, "MATURITY"] == 2 & merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F")) {
        merge_TATC[i, "MATURITY_STAGE"] <- paste(merge_TATC[i, "MATURITY"],merge_TATC[i, "MATSUB"],sep="")}
    }
    if(merge_TATC[i, "MATURITY"] == 3 & !(merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F"))) {merge_TATC[i, "MATURITY_STAGE"] <- 3 } else {
      if (merge_TATC[i, "MATURITY"] == 3 & merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F")) {
        merge_TATC[i, "MATURITY_STAGE"] <- paste(merge_TATC[i, "MATURITY"],merge_TATC[i, "MATSUB"],sep="")}
    }
    if(merge_TATC[i, "MATURITY"] == 4 & !(merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F"))) {merge_TATC[i, "MATURITY_STAGE"] <- 4 } else {
      if (merge_TATC[i, "MATURITY"] == 4 & merge_TATC[i, "MATSUB"] %in% c("A","B","C","D","E","F")) {
        merge_TATC[i, "MATURITY_STAGE"] <- paste(merge_TATC[i, "MATURITY"],merge_TATC[i, "MATSUB"],sep="")}
    }

    if (merge_TATC[i, "WEIGHT_OF_THE_FRACTION"] > 0){merge_TATC[i, "RAISING_FACTOR"] <- merge_TATC[i, "WEIGHT_OF_THE_SAMPLE_MEASURED"] / merge_TATC[i, "WEIGHT_OF_THE_FRACTION"]
    } else {merge_TATC[i, "RAISING_FACTOR"] <- 0}
    if (merge_TATC[i, "RAISING_FACTOR"] >0 & merge_TATC[i, "RAISING_FACTOR"] < 1) {merge_TATC[i, "RAISING_FACTOR"] <- 1}
  }

  merge_TATC$N_h <- merge_TATC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE*merge_TATC$RAISING_FACTOR/duration
  merge_TATC$N_km2 <- merge_TATC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE*merge_TATC$RAISING_FACTOR/merge_TATC$SWEPT_AREA
  merge_TATC$kg_h <- merge_TATC$WEIGHT_OF_THE_SAMPLE_MEASURED/duration/1000
  merge_TATC$kg_km2 <- merge_TATC$WEIGHT_OF_THE_SAMPLE_MEASURED/merge_TATC$SWEPT_AREA/1000

  if (verbose) {message("TA-TC files correctly merged\n")}
  return(merge_TATC)
}
