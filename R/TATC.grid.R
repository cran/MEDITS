# cgpmgrid <- readOGR("D:\\R_BioIndex\\scripts\\utilities\\gfcmstatp\\gfcmstatp_centr.shp", verbose=F)
# cgpmgrid <- spTransform(cgpmgrid, CRS("+proj=longlat +ellps=WGS84"))
# centroidi <- readOGR("D:\\R_BioIndex\\scripts\\utilities\\gfcm-centroidi\\gfcm-centroidi.shp", verbose=F)
# proj4string(centroidi) <- CRS("+proj=longlat +ellps=WGS84")
# save(centroidi, cgpmgrid, countries, depth_1, depth_2, depth_3, wmap, file = "data/shapefiles.rda",compress="xz")
# sspp <- "ARISFOL"

# TATC.grid(TA,TC,"ARISFOL")

TATC.grid <- function(TA,TC,sspp, grid.sf=cgpmgrid){
  cgpmgrid <- grid.sf
  metaDB <- m.TATC(TA,TC,sspp)
  GENERE <- as.character(unique(metaDB$GENUS)[unique(metaDB$GENUS) != -1])
  SPECIE <- as.character(unique(metaDB$SPECIES)[unique(metaDB$SPECIES) != -1])
  sspp <- paste(GENERE,SPECIE, sep="")
  coordinates(metaDB) <- c("MEAN_LONGITUDE_DEC","MEAN_LATITUDE_DEC")
  proj4string(metaDB) <- CRS("+proj=longlat +ellps=WGS84") # Set the coordinate system

  # Overlay metaDB with the grid
  overlay <- over(metaDB, cgpmgrid, returnlist=TRUE)
  metaDBnew_georef <- data.frame(metaDB, x_center= overlay$gfcm.cen_5,y_center=overlay$gfcm.cen_6 ,cgpmgrid_id=overlay$GFCM_ID)


  return(metaDBnew_georef)
}
