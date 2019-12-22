# ### MERGE TA-TB files for a given species
# TA <- data.MEDITS::TA
# TB <- data.MEDITS::TB

# sspp <- "MERLMER"
# TA <- read.csv("~/__ DATI MEDITS AGGIORNATI __/GSA18 2017 TA.csv", sep=";")
# TB <- read.csv("~/__ DATI MEDITS AGGIORNATI __/GSA18 2017 TB.csv", sep=";")
# mmm <- m.TATB(TA, TB, "ARISFOL")


m.TATB <- function (TA, TB, sspp, str.scheme=strata_scheme, verbose = FALSE) {
  strata_scheme <- str.scheme
  TA_cols <- c(TA_cols)
  id_TA <- data.frame(id = paste(TA$AREA,TA$COUNTRY,TA$YEAR,"_", TA$VESSEL, TA$MONTH, TA$DAY,"_", TA$HAUL_NUMBER, sep = ""))
  id_TB <- data.frame(id = paste(TB$AREA,TB$COUNTRY,TB$YEAR,"_", TB$VESSEL, TB$MONTH, TB$DAY,"_", TB$HAUL_NUMBER, sep = ""))

  colnames(TA)[which(colnames(TA) == "AREA")] <- "GSA"
  colnames(TB)[which(colnames(TB) == "AREA")] <- "GSA"

  TA_merge <- cbind(id_TA,TA)
  TB_merge <- cbind(id_TB,TB)
  TB_merge$GENUS <- as.character(TB_merge$GENUS)
  TB_merge$SPECIES <- as.character(TB_merge$SPECIES)
  if (sspp =="all"){
    TA_merge <- TA_merge[, which(colnames(TA_merge) %in% TA_cols)]
    TB_merge <- TB_merge[, which(colnames(TB_merge) %in% TB_cols)]
  } else {
  sspp <- toupper(sspp)
  species <- substr(sspp,1,4)
  species[2] <- substr(sspp,5,7)

  TA_merge <- TA_merge[,which(colnames(TA_merge) %in% TA_cols)]
  TB_merge <- TB_merge[TB_merge$GENUS == species[1] & TB_merge$SPECIES == species[2], which(colnames(TB_merge) %in% TB_cols)]
  }

  ########################
  ##    MERGE TA- TB    ##
  ########################
  if (verbose) {message("Merging TA-TB files\n")}

  merge_TATB <- merge(TA_merge, TB_merge, by.x = "id", by.y = "id", all.x = TRUE)
  merge_TATB$MEDITS_CODE <- as.character(paste(merge_TATB$GENUS, merge_TATB$SPECIES))
  merge_TATB <- tibble::add_column(merge_TATB, TYPE_OF_FILE = "TATB", .after = "id")

  l_TATB <- length (merge_TATB[,1])
  i=1
  for (i in 1:l_TATB){
    if (merge_TATB[i, "MEDITS_CODE"] == "NA NA"){
      merge_TATB[i, "MEDITS_CODE"              ] <- "NA"
      merge_TATB[i, "GENUS"                    ] <- -1
      merge_TATB[i, "SPECIES"                  ] <- -1
      merge_TATB[i, "TOTAL_WEIGHT_IN_THE_HAUL" ] <- 0
      merge_TATB[i, "TOTAL_NUMBER_IN_THE_HAUL" ] <- 0
      merge_TATB[i, "NB_OF_FEMALES"            ] <- 0
      merge_TATB[i, "NB_OF_MALES"              ] <- 0
      merge_TATB[i, "NB_OF_UNDETERMINED"       ] <- 0
    }
  }

  Data <- merge_TATB
  # source(paste(wd,"scripts/utilities/COORD_Medits - to - WGS84.R", sep="/"))
  coord <- MEDITS.to.dd(merge_TATB)

  merge_TATB$MEAN_LATITUDE_DEC <- (coord$SHOOTING_LATITUDE+coord$HAULING_LATITUDE)/2
  merge_TATB$MEAN_LONGITUDE_DEC <- (coord$SHOOTING_LONGITUDE+coord$HAULING_LONGITUDE)/2
  merge_TATB$MEAN_DEPTH <- (merge_TATB$SHOOTING_DEPTH+merge_TATB$HAULING_DEPTH)/2
  merge_TATB$SWEPT_AREA <- merge_TATB$DISTANCE * merge_TATB$WING_OPENING/10000000
  i=1
  hour_shooting <- 0
  min_shooting <- 0
  hour_hauling <- 0
  min_hauling <- 0

  strata_scheme_GSA <- strata_scheme[strata_scheme$GSA == unique(merge_TATB$GSA) , ]  # & strata_scheme$COUNTRY %in% as.character(unique(merge_TATB$COUNTRY))

  for (i in 1:l_TATB){

    for (j in 1:length(strata_scheme_GSA$CODE)){
      if  (merge_TATB$MEAN_DEPTH[i] > strata_scheme_GSA[j,4] & merge_TATB$MEAN_DEPTH[i] <= strata_scheme_GSA[j,5]) {merge_TATB$STRATUM_CODE[i] <- strata_scheme_GSA[j,3]}
    }

    if (nchar(merge_TATB$SHOOTING_TIME[i])==4) {hour_shooting[i] <- substr(merge_TATB$SHOOTING_TIME[i],1,2); min_shooting[i] <- substr(merge_TATB$SHOOTING_TIME[i],3,4)} else {
      if (nchar(merge_TATB$SHOOTING_TIME[i])==3) {hour_shooting[i] <- substr(merge_TATB$SHOOTING_TIME[i],1,1); min_shooting[i] <- substr(merge_TATB$SHOOTING_TIME[i],2,3)}}

    if (nchar(merge_TATB$HAULING_TIME[i])==4) {hour_hauling[i] <- substr(merge_TATB$HAULING_TIME[i],1,2); min_hauling[i] <- substr(merge_TATB$HAULING_TIME[i],3,4)} else {
      if (nchar(merge_TATB$HAULING_TIME[i])==3) {hour_hauling[i] <- substr(merge_TATB$HAULING_TIME[i],1,1); min_hauling[i] <- substr(merge_TATB$HAULING_TIME[i],2,3)}}
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

  merge_TATB$SHOOTING_TIME <- hms_shooting
  merge_TATB$HAULING_TIME  <- hms_hauling

  merge_TATB$N_h <- merge_TATB$TOTAL_NUMBER_IN_THE_HAUL/duration
  merge_TATB$N_km2 <- merge_TATB$TOTAL_NUMBER_IN_THE_HAUL/merge_TATB$SWEPT_AREA
  merge_TATB$kg_h <- merge_TATB$TOTAL_WEIGHT_IN_THE_HAUL/duration/1000
  merge_TATB$kg_km2 <- merge_TATB$TOTAL_WEIGHT_IN_THE_HAUL/merge_TATB$SWEPT_AREA/1000

  merge_TATB$MEDITS_CODE <- as.character(merge_TATB$MEDITS_CODE)

  # write.table(merge_TATB, paste(wd, "/output_GAM/mergeTATB_",species[1], species[2],"_standardization.csv", sep=""), sep=";", row.names=F)

  if (verbose) {message("TA-TB files correctly merged\n")}
  # message(paste("Merge TA-TB files saved in the following folder: '",wd, "/output_GAM/mergeTATB_",species[1], species[2],"_standardization.csv'","\n", sep=""))

  return(merge_TATB)
}
