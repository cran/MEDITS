check.format <- function (TA, TB, TC) {

{  # TA checks
  refTA <- c("TYPE_OF_FILE", "COUNTRY", "AREA", "VESSEL", "GEAR", "RIGGING", "DOORS", "YEAR", "MONTH", "DAY", "HAUL_NUMBER",
             "CODEND_CLOSING","PART_OF_THE_CODEND", "SHOOTING_TIME", "SHOOTING_QUADRANT", "SHOOTING_LATITUDE", "SHOOTING_LONGITUDE",
             "SHOOTING_DEPTH","HAULING_TIME","HAULING_QUADRANT","HAULING_LATITUDE","HAULING_LONGITUDE","HAULING_DEPTH","HAUL_DURATION",
             "VALIDITY","COURSE","RECORDED_SPECIES","DISTANCE","VERTICAL_OPENING","WING_OPENING",
             "GEOMETRICAL_PRECISION","BRIDLES_LENGTH","WARP_LENGTH","WARP_DIAMETER","HYDROLOGICAL_STATION","OBSERVATIONS",
             "BOTTOM_TEMPERATURE_BEGINNING","BOTTOM_TEMPERATURE_END","MEASURING_SYSTEM","NUMBER_OF_THE_STRATUM",
             "BOTTOM_SALINITY_BEGINNING","BOTTOM_SALINITY_END","MEASURING_SYSTEM_SALINITY")
  checkTA <-colnames(TA)
  chTA <- NA
  missingTA <- NA
  moreInTA <- NA
  for (i in 1:length(refTA)) {
    if (refTA[i] %in% checkTA) {
      chTA[i] <- refTA[i]
    } else {
      missingTA[i] <- refTA[i]
    }
  }


  for (i in 1:length(checkTA)){
    if (!(checkTA[i] %in% refTA)) {
      moreInTA[i] <- checkTA[i]
    } else { moreInTA[i] <- NA}
  }
  if( length(missingTA[!is.na(missingTA)]) >= 1 ) {
    missingTA <- missingTA[!is.na(missingTA)]
    # warning('missing columns in TA\n')
    for (K in 1:length(missingTA[!is.na(missingTA)])){
      message(paste("the following column was not found in TA: ", missingTA[K],"\n"))
    }
  }

  if( length(moreInTA[!is.na(moreInTA)]) >= 1) {
    moreInTA <- moreInTA[!is.na(moreInTA)]
    for (n in 1:length(moreInTA[!is.na(moreInTA)])){
      # warning('not expected columns in TC\n')
      message(paste("the following column was not expected in TA: ", moreInTA[n],"\n"))
    }
  }

} #close check TA


{  # TB checks
  refTB <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","CODEND_CLOSING",
             "PART_OF_THE_CODEND","FAUNISTIC_CATEGORY","GENUS","SPECIES","NAME_OF_THE_REFERENCE_LIST",
             "TOTAL_WEIGHT_IN_THE_HAUL","TOTAL_NUMBER_IN_THE_HAUL","NB_OF_FEMALES","NB_OF_MALES","NB_OF_UNDETERMINED")
  checkTB <-colnames(TB)
  chTB <- NA
  missingTB <- NA
  moreInTB <- NA
  for (i in 1:length(refTB)) {
    if (refTB[i] %in% checkTB) {
      chTB[i] <- refTB[i]
    } else {
      missingTB[i] <- refTB[i]
    }
  }


  for (i in 1:length(checkTB)){
    if (!(checkTB[i] %in% refTB)) {
      moreInTB[i] <- checkTB[i]
    } else { moreInTB[i] <- NA}
  }
  if( length(missingTB[!is.na(missingTB)]) >= 1 ) {
    missingTB <- missingTB[!is.na(missingTB)]
    # warning('missing columns in TB\n')
    for (K in 1:length(missingTB[!is.na(missingTB)])){
      message(paste("the following column was not found in TB: ", missingTB[K],"\n"))
    }
  }

  if( length(moreInTB[!is.na(moreInTB)]) >= 1) {
    moreInTB <- moreInTB[!is.na(moreInTB)]
    for (n in 1:length(moreInTB[!is.na(moreInTB)])){
      # warning('not expected columns in TC\n')
      message(paste("the following column was not expected in TB: ", moreInTB[n],"\n"))
    }
  }
} #close check TB


{  # TC checks
  refTC <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
             "FAUNISTIC_CATEGORY","GENUS","SPECIES","LENGTH_CLASSES_CODE","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED",
             "SEX","NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","LENGTH_CLASS","MATURITY","MATSUB",
             "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE")
   checkTC <-colnames(TC)
   chTC <- NA
   missingTC <- NA
   moreInTC <- NA
   for (i in 1:length(refTC)) {
     if (refTC[i] %in% checkTC) {
       chTC[i] <- refTC[i]
     } else {
       missingTC[i] <- refTC[i]
     }
   }


   for (i in 1:length(checkTC)){
     if (!(checkTC[i] %in% refTC)) {
       moreInTC[i] <- checkTC[i]
     } else { moreInTC[i] <- NA}
   }
   if( length(missingTC[!is.na(missingTC)]) >= 1 ) {
     missingTC <- missingTC[!is.na(missingTC)]
     # warning('missing columns in TC\n')
     for (K in 1:length(missingTC[!is.na(missingTC)])){
       message(paste("the following column was not found in TC: ", missingTC[K],"\n"))
     }
   }

   if( length(moreInTC[!is.na(moreInTC)]) >= 1) {
     moreInTC <- moreInTC[!is.na(moreInTC)]
     for (n in 1:length(moreInTC[!is.na(moreInTC)])){
       # warning('not expected columns in TC\n')
       message(paste("the following column was not expected in TC: ", moreInTC[n],"\n"))
     }
   }

} #close check TC

}
