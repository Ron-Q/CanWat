#+
# NAME*:    transX.R
# PURPOSE*: Transform projections 
#           Steps: create spatial object, assign projection and transform projection
#   Standards
#     Raumbezugssystem bestehend aus Datum und           |  Ellipsoid  | Method
#     WGS 84:  World Geodetic System 1984                |  WGS 84     | 
#     ETRS89:  European Terrestrial Reference System 1989|  GRS 1980   | Geocentric translations (geog2D domain)
#     (basiert auf ETRF European Terrestrial Reference Frame,  konkreter Satz von Vermessungspunktkoordinaten)
#   used from RQ
#     EPSG:4326  : WGS 84-Ellipsoid
#     EPSG:25833 : ETRS89/UTM33N (Lage) und als Höhenreferenzsystem das Deutsche Haupthöhennetz 2016 (DHHN2016)
#     ... das Höhenreferenzsystem setzt auf dem Ellipsoid auf
#   Axis Order ####
#     in sf you can check whether lon,lat (traditional in R) or lat,lon (new in rgdal) is used by "st_axis_order"
#   Projection issue #### 
#     Coordinate reference system is represented by a list with two components: 
#     in Proj4:  epsg and proj4string  ,in Proj6: input and wkt 
#     since 2020 the use of "proj4string" is discouraged 
#     the old PROJ.4 and the new PROJ.7 will support two different APIs, thus abruptly breaking cross-version compatibility
#     SRID   CRS Name               Poject strings          proj4                     =>              proj6
#     4326,  WGS 84: 	              +proj=longlat +datum=WGS84 +no_defs               => +proj=longlat +datum=WGS84 +no_defs +type=crs
#     32632  WGS 84 / UTM zone 32N: +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs => +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs
 # Warnings occur warnings using raster and sp
    # "Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition"
    # R packages that support GDAL 3 and PROJ ≥ 6 ignore and drop some parts of the proj4strings 
    # (such as +datum and +towgs84 keys), i.e. the Datum is not recognized, 
    # ===> The default datum appears to be NAD83 <====
    # The difference between NAD83 and WGS84 datum is only few metres, but the NAD27 datum is very different
    # see https://gis.stackexchange.com/questions/385114/how-to-update-crs-r-object-from-proj4-to-proj6
	# In order to remain compatible with GDAL 2 and PROJ 4, some packages will still internally 
	#  derive a proj4string as well (notably by calling the core package rgdal). 
	# This happens even while you did not enter a proj4string. Note that the derived proj4string will not be used further 
	# if you’re on GDAL 3 / PROJ ≥ 6, and a WKT2 string will be generated as well for actual use. 
	# In the presence of GDAL 3 / PROJ ≥ 6, (at the time of writing), 
	# you will get a warning about dropped keys in the generated proj4strings,
	# but in the meantime, for most geospatial R packages you can safely ignore this warning.
    # only the newer sf package works smoothly => switch   # <==== recommended <========
    # https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/
    #   recommend is: specify the CRS by using the EPSG code, but do so without using a proj4string.
    #   Don’t use proj4strings, such as +init=epsg:????, +proj=longlat, … !!!!!!!!!
    # check the minimal PROJ/GDAL versions with 
    # sf-Package   sf::sf_extSoftVersion()   it should be
    #           GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H 
    #         "3.8.0"        "3.0.4"        "6.3.1"         "true"         "true" 
    # sp-Package    rgdal::rgdal_extSoftVersion()   it should be
    #           GDAL GDAL_with_GEOS           PROJ             sp 
    #         "3.0.4"         "TRUE"        "6.3.1"        "1.4-2" 
  # Defining and setting CRS in different packages ####
    #     CRS objects in sp and  crs objects in sf      
    # example:  cities <- st_read(system.file("vectors/cities.shp", package = "rgdal"))
    # # sf 
    # crs_wgs84 <- st_crs(4326)                         # defining a crs ( WGS84 has EPSG code 4326)
    # cities2 <- st_as_sf(cities, coords = c("X", "Y")) # setting up a sf objekt
    # st_crs(cities2) <- 4326                           # setting the crs
    # st_crs(cities2)                                   # reading the crs
    # 
    # # sp :  
    # crs_wgs84 <- CRS(SRS_string = "EPSG:4326")          # defining a crs ( WGS84 has EPSG code 4326)
    # cities2 <- cities; coordinates(cities2) <-  ~ X + Y # setting up a sp objekt
    # proj4string(cities2) <- crs_wgs84                   # setting the crs
    # cat(wkt(cities2))                                   # reading the crs
    # 
    # # raster package
    # within_belgium <- raster(extent(188500, 190350, 227550, 229550), res = 50)  # dummy raster
    # values(within_belgium) <- 1:ncell(within_belgium)
    # crs(within_belgium) <- 31370                        # defining and setting the crs
    # crs(within_belgium) <- "EPSG:31370"                 # the 4 method are equal  
    # crs(within_belgium) <- st_crs(31370)$wkt            # a WKT string from sf definition
    # crs(within_belgium) <- CRS(SRS_string = "EPSG:31370") # an sp CRS object
    # cat(wkt(within_belgium))
  # exchange / switch from one package to another
    # from sp to sf use (x2 <- st_crs(x))      
    # from sf to sp use (x3 <- CRS(SRS_string = x2$wkt))   or    (x4 <- as(x2, "CRS"))
  # practice workflow with sp (https://gis.stackexchange.com/questions/374508/spproj4string-equivalent-in-the-proj6-framework): 
    # x <- CRS(SRS_string='EPSG:4326') # Define the CRS using an EPSG Code
    # cat(comment(x), "\n")            # Display the stored CRS using comment()
    # wkt <- comment(x)                # Store the wkt in a variable
    # y <- CRS(SRS_string = wkt)       # Use this to assign the CRS of another sp-object
    # y1 <- CRS(SRS_string = comment(CRS(SRS_string='EPSG:4326')))       # in short
    # all.equal(y,y1)
# RELEVANCY*: GIS, spatial data
# CALLING SEQUENCE:    source(paste0(rlib,"s_GIS/transX.r"))
# EXAMPLE:   ;  
#   source(paste0(rlib,"s_GIS/transX.r"))   # transforms projections
#   transX()
#   river mouth of the Weißeritz, coordinates: 13.686690, 51.063338
#   transX(lonE=13.686690, latN=51.063338)
#   transX(lonE=13.686690, latN=51.063338, srsIn = st_crs(4326), srsOut = st_crs(25833))
#   transX(lonE=407972.774, latN=5657688.81, srsIn = st_crs(25833), srsOut = st_crs(4326))
#       => Output Coordinates: east = 407972.774, north = 5657688.81
#   transX(lone=13.686690, latN=51.063338, proIn = CRS(SRS_string = "EPSG:4326"), proOut = CRS(SRS_string = "EPSG:32633"))
#       => Output Coordinates: east = 407972.774, north = 5657688.81
#   https://epsg.io/transform#s_srs=4326&t_srs=32633&x=13.6866900&y=51.0633380
#   EPSG:32633 WGS 84 / UTM zone 33N => 407972.77, 5657688.82
#   EPSG:25833 ETRS89 / UTM zone 33N => 407972.77, 5657688.82
#   http://tool-online.com/en/coordinate-converter.php 
#   WGS 84 / UTM zone 33N => 407972.774, 5657688.809
#   Es scheint egal zu sein welches Output coordinate system genutzt wird! 
# INPUTS*: lonE, latN: coordinates in "degree" or in "m" according to the projection "proIn"
#          proIn, proOut: proj4 CRS definition (not recommended anymore)
#          srsIn, srsOut: proj6 CRS definition
#             epsg:4258 : European Terrestrial Reference System 1989
#             epsg:4326 : Google Earth             : geodetic coordinate system with the wgs84 datum (in "°")
#             epsg:3857 : Google Maps, OSM, Leaflet: projected coordinate system that is based on the wgs84 datum (see: https://en.wikipedia.org/wiki/Web_Mercator_projection)
#                 (epsg:4326) The data in Open Street Map database is stored in a gcs with units decimal degrees & datum of wgs84. 
#                 (epsg:3857) The Open Street Map tiles and the WMS webservice, are in the projected coordinate system that is based on the wgs84 datum. 
#             Dresden is "epsg:32633": WGS 84 / UTM zone 33N (+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs)(Transverse Mercator)
#     ==>           oder "epsg:25833": ETRS89 / UTM zone 33N (+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs)
#             Hamburg, proOut = st_crs(32632)  # +proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m (UTM32N)
#     !!!     # UC2_Datenstandard_V1.0.pdf , S.7 u. 14
#             die Angabe als UTM-Rechtswert in Meter mit Bezug auf das Referenzsystem ETRS89 mit dem GRS80-Ellipsoid!
#             Für Deutschland gibt es drei UTM-Zonen (31, 32 und 33), welche sich auf die Zentralmeridiane 3, 9 und 15 Grad beziehen 
#             => (EPSG-Projektionen 25831, 25832 und 25833; siehe z.B. spatialreference.org/ref/epsg/25831/). Für Stadtregionen in anderen Ländern sind die entsprechenden UTM-Zonen auszuwählen.
#    - projGeo = CRS(SRS_string = "EPSG:4258") is ETRS89 proj4.defs("EPSG:4258","+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs");
#    - Leaflet expects all point, line, and shape data to be specified in latitude and longitude using 
#             WGS 84 (a.k.a. EPSG:4326). 
#             By default, when displaying this data it projects everything to EPSG:3857
#             expects that any map tiles are also displayed in EPSG:3857.
# OUTPUT*: transformed coordinates within a spatial object or a dataframe
# REFERENCE: http://spatialreference.org
#            https://epsg.io/25833
#            https://www.r-spatial.org/r/2020/03/17/wkt.html  , 2020 • Edzer Pebesma, Roger Bivand
#            https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
#            Project strings: https://www.gaia-gis.it/fossil/libspatialite/wiki?name=PROJ.6
#            see also https://cran.r-project.org/web/packages/PROJ/vignettes/PROJ.html
#            https://inbo.github.io/tutorials/tutorials/spatial_transform_crs/
#            https://rstudio.github.io/leaflet/projections.html
#            http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html    Roger Bivand, 2020, Migration to PROJ6/GDAL3
# REVISION HISTORY*:
#   2017-09-13, R Queck (TUD, IHM): assembled
#   2020-03-08, RQ : more desciption added CRS of Google Maps etc. CRS("+init=epsg:3857")
#   2020-11-09  RQ : proj4string discarded, obviously not completely
#   2021-03-20, RQ : switch to sf
#-
transX <- function(  lonE=c(16.8, 14.2, 12.9, NA, 14.2, 15.4, 17.7)
                   , latN=c(41.3, 42.9, 42.4, 59.8, 57.6, NA, 57.6)
                   , proIn = NA, proOut = NA # proj4string:  not recommended anymore, use srsIn and srsOUT
                   , srsIn  = NA   # SRID, default '4326'  , geodetic coordinates in "°"
                   , srsOut = NA   # SRID, default '25833' , projected coordinates UTM32N
                   , spo = FALSE  # output as a spatial object (without NAs)? If FALSE then as a dataframe
                   ){   
  options(digits = 9)
  # library(rgdal)
  # library(raster)
  library(sf)
  if (!is.na(srsIn))   crsIn <- st_crs(srsIn) else {
    if (!is.na(proIn)) crsIn <- st_crs(proIn) else {
                       crsIn <- st_crs(4326) }}  # CRS(SRS_string='EPSG:4326') 
  if (!is.na(srsOut))   crsOut <- st_crs(srsOut) else 
    if (!is.na(proOut)) crsOut <- st_crs(proOut) else
                        crsOut <- st_crs(25833)
  iOK <- which(!is.na(lonE) & !is.na(latN) )
  tco <- data.frame(lon=lonE, lat=latN)
  tmp <- tco[iOK,]
  tmp <- st_as_sf(tmp, coords = c(1, 2))   # create an sf class object
  st_crs(tmp) <- crsIn                     # add coordinate reference system
  tmp <- st_transform(tmp, crsOut)         # change coordinate reference system
  prs <- st_crs(tmp)                       # read crs
  tco[] <- NA  # ; tco <- as.data.frame(tco)
  if (spo) {tco <- tmp} else {
    tco[iOK,] <- st_coordinates(tmp)
    if ((grepl("UTM", prs$wkt) > 0) | (grepl("Mercator", prs$input) > 0)) names(tco) <- c("E", "N")
  }
  return(tco)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+
# NAME*:    N2D
# PURPOSE*: Transforms coordinates according to NMEA 0183 into simple degree 
# RELEVANCY*: spatial data
# CALLING SEQUENCE:    source(paste(p_rlib,"s_GIS/transX.r", sep=""))
# EXAMPLE:  N2D(c(50587273,13348822))  # => 50.97879 13.58137
# INPUTS*:  XXYYZZZZ => XX°YY'(0.ZZZZ * 60)"
# OUTPUT*:  coordinates in degree
# REFERENCE: see also  https://de.wikipedia.org/wiki/NMEA_0183
# REVISION HISTORY*:
#           02.02.2018 : R Queck (TUD, IHM)
#-
N2D <- function(x){
  ex <- ceiling(log10(x))
  gg <- floor(x/(10^(ex-2)))
  mm <- (x-gg*10^(ex-2))/10^(ex-4)
  return(gg+mm/60)
}
