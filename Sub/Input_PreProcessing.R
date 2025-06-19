#+
# NAME*:  Input_PreProcessing.R
# PURPOSE*: pre processing and check of the input parameter given in paste0(act,"_input.R") 
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"Input_PreProcessing.R"))
# INPUTS*: works in global environment
# OUTPUT*: parameter for dynamic and static driver
# REVISION HISTORY*:
#    2021-01-24 : Ronald Queck
#    2023-07-02 : Ronald Queck : uppercase letters ...
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("---- CALL: Input_PreProcessing.R ---> check and pre-process input data")

if (!exists("Test")) Test <- F

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PARAMETERS ####################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check if all necessary parameters are given, if not associate standard values

if (!exists("eq.rb")) eq.rb = 1
if (!exists("sc.min")) sc.min <- 0.01 # part of storage from that evaporation occurs at the least, if there is water in the storage
                                      # increasing sc.min reduces the resistance and increases ETR
if (!exists("sc.f")) sc.f <- 12       # non-linearity of the drying process, i.e. of the saturation of the air near the surface, check with the following line
                                      # the higher sc.f the faster the saturation increases with the first drops on the surface 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STATIC DRIVER ####################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fnam.sd <- substr(fnam.SD,1, nchar(fnam.SD)-4)

# position of the reference point within the static driver in m
  xrefll <- (xref.UTM-xll.UTM)
  yrefll <- (yref.UTM-yll.UTM)
  zrefll <- (zref.UTM-zll.UTM)  # locale height of reference in the given domain  

  # domain selection  
  # indexe within the static driver
  # ix1 <- min(max(round((xrefll+x1.ref)/dx)+1,1), SD.dims[3])
  # ix2 <- min(max(round((xrefll+x2.ref)/dx),1), SD.dims[3])
  ix1 <- min(max(     ((xrefll+x1.ref)%/%dx),1), SD.dims[3]) # between 1 and dim-size
  ix2 <- min(max(ceiling((xrefll+x2.ref)/dx),1), SD.dims[3])
  iy1 <- min(max(     ((yrefll+y1.ref)%/%dy),1), SD.dims[2])
  iy2 <- min(max(ceiling((yrefll+y2.ref)/dy),1), SD.dims[2])
  iz1 <- min(max(     ((zrefll+z1.ref)%/%dz),1), SD.dims[1])
  iz2 <- min(max(ceiling((zrefll+z2.ref)/dz),1), SD.dims[1])
  # cat(ix1,ix2,iy1,iy2,iz1,iz2)
  
  # index of the reference point within the domain
  ixref <- max(round(-x1.ref/dx),1)   # xyz1.ref border of the domain, with respect to reference point
  iyref <- max(round(-y1.ref/dy),1)    
  izref <- max(round(-z1.ref/dz),1) + 2
  
  nx <- ix2-ix1+1
  ny <- iy2-iy1+1
  nz <- iz2-iz1+1 + 2 
  imref <- iWSref
  
  if (!exists("treetype")) treetype <- "picea_abies"
  if (is.na(treetype)) treetype <- "picea_abies"
  if (!exists("CWpara_name")) CWpara_name <- paste0('CW_parameter-',treetype, '_ASTW2008-2010.R')   
  if (is.na("CWpara_name")) CWpara_name <- paste0('CW_parameter-',treetype, '_ASTW2008-2010.R')   
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DYNAMIC DRIVER - Meteorolocial Data ################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* Switches ####
  if (!exists("wind.homo")) wind.homo <- F   # if TRUE, wind speed is directly taken from measurements in each grid cell, 
  if (!exists("gapf")) gapf <- F             # switch for simple gap filling in input data
  if (!exists("EvNo.file")) EvNo.file <- NA      
  if (!exists("EvNo.selected")) EvNo.selected <- NA         
  if (!exists("Event.ext.b")) Event.ext.b <- 0      
  if (!exists("Event.ext.a")) Event.ext.a <- 0      
  if (!exists("Event.ext.round")) Event.ext.round <- dt_met                # in s, full time interval (3600s rounds the start and end time to the full hour) )
  if (!exists("bigleaf")) bigleaf <- F
    
  if (Event.ext.round < dt_met) Event.ext.round <- dt_met
  
#* Times and Intervals ####

  dt0 <- dt_met  # time step of input data
  if (!exists("dt1")) dt1 <- dt0
   
  # move the given timestemp to the start of the intervals
  ToffUTC <- ToffUTC - Tsme*dt_met - Tsoff  # CanWat works with time stemps at the start of the interval
  
  # Interval selection is done in Load_Dynamic_Driver.R
  
  if (!is.na(sum(EvNo.selected))) {
    EvNo.ToffUTC <- EvNo.ToffUTC - EvNo.Tsme*EvNo.dt_met - EvNo.Tsoff  
    Event.ext.b.i <- ceiling(Event.ext.b/dt_met)
    Event.ext.a.i <- ceiling(Event.ext.a/dt_met)
  } # else  ntu <- it2-it1+1   # this is moved to "Load_Dynamic_Driver.R"
  
#* Positioning of the measurements ####
  # imref <- 6  # index of the reference measurement 
  # horizontal position within the selected domain 
  insideD <- function(a, ref, da){max(round(( a - ref)/da),1)}
  iyWS <- sapply(yWS,  insideD,  ref=y1.ref, da=dy );  iyWD <- iyWS     # North
  iyDD <- sapply(yDD,  insideD,  ref=y1.ref, da=dy ); iyTa <- sapply(yTa,  insideD,  ref=y1.ref, da=dy )
  iyRn <- sapply(yRn,  insideD,  ref=y1.ref, da=dy ); iyRg <- sapply(yRg,  insideD,  ref=y1.ref, da=dy )
  ixWS <- sapply(xWS,  insideD,  ref=x1.ref, da=dx ); ixWD <- ixWS     # East
  ixDD <- sapply(xDD,  insideD,  ref=x1.ref, da=dx ); ixTa <- sapply(xTa,  insideD,  ref=x1.ref, da=dx )
  ixRn <- sapply(xRn,  insideD,  ref=x1.ref, da=dx ); ixRg <- sapply(xRg,  insideD,  ref=x1.ref, da=dx )

  # the names are used from CanWat (TREAD CAREFULLY) 
  if (length(PP_namo) == 2) PP_nam <- c("PF","PB") else PP_nam <- c("PF")   # precipitation, troughfall
  WS_nam <- paste0("u",as.character(hWS))  # wind speed in m/s
  Ta_nam <- paste0("T",as.character(hTa))  # temperature in °C
  DD_nam <- paste0("DD",as.character(hDD)) # water vapor pressure in hPa
  Rn_nam <- paste0("Rn",as.character(hRn)) # radiation balance in W/m²
  Rg_nam <- paste0("Rg",as.character(hRg)) # radiation shortwave (solar) in W/m²
  Pa_nam <- paste0("Pa")                   # atmospheric pressure in hPa
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT DEFINITION #################################################
# standard mean output is 
#  c("UTC","dt","C_3D","dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf")
# within "output.mean.usr" you can name additional output variables 
# Please define this character string in your input file
  
# silent <- 0     # Output control flag: 1: supress print output within the time loop,  2: supress print output completely
# plotMeanTS <- T # control plot of the water budget after each time step, this might be slow 
if (!exists("plotlife")) plotlife <- F   # control plot of the water budget after each time step, this might be even slower

#* Standard Output that is called each time steps ########
# this output is defined in output_timestep.R 
# output will be written to path_output
if (!exists("field.out")) field.out <- F     
if (!exists("xcut.out")) xcut.out <- NA
if (!exists("ycut.out")) ycut.out <- NA
if (!exists("zcut.out")) zcut.out <- NA
if (!is.na(xcut.out)) {xcut.outg <- ixref + round(xcut.out/dx); if ((xcut.outg < 1) | (xcut.outg > nx)) xcut.out <- NA }
if (!is.na(ycut.out)) {ycut.outg <- iyref + round(ycut.out/dy); if ((ycut.outg < 1) | (ycut.outg > ny)) ycut.out <- NA }  
if (!is.na(zcut.out)) {zcut.outg <- izref + round(zcut.out/dz); if ((zcut.outg < 1) | (zcut.outg > nz)) zcut.out <- NA }  

if (field.out | any(!is.na(xcut.out),!is.na(ycut.out),!is.na(zcut.out) ) ){
  dir.create(file.path(path_output_act,"fields"), showWarnings = FALSE)
}

# # output layer means (Mittel über die Bestandesschicht)
  if (!exists("C_lmean.S")) C_lmean.S <- F ; if (!C_lmean.S & exists("C_lmean")) rm(C_lmean)
  if (!exists("ETR_lmean.S")) ETR_lmean.S <- F ; if (!ETR_lmean.S & exists("ETR_lmean")) rm(ETR_lmean)
  if (!exists("ET_lmean.S"))  ET_lmean.S <- F  ; if (!ET_lmean.S & exists("ET_lmean")) rm(ET_lmean)
  if (!exists("EV_lmean.S"))  EV_lmean.S <- F  ; if (!EV_lmean.S & exists("EV_lmean")) rm(EV_lmean)
  if (any(c( field.out, !is.na(xcut.out), !is.na(ycut.out), C_lmean.S, ETR_lmean.S, ET_lmean.S, EV_lmean.S))) layer.out <- T else  layer.out <- F

  
#* individual output #########
# check whether an individual output file exists and if, then create an "usr_output" directory 
if (!exists("fnam.out")) fnam.out <- NA else {
  if (!is.na(fnam.out) & !(Test)) if (nchar(fnam.out)>0) {
    if (!file.exists(file.path(path_Cases, fnam.out ))) {
      fnam.out <- NA
      ErrMes <- paste0("Tried to activate individual output,  "
                       , " but steering file ", fnam.out, "  "
                       , "does not exists in folder ", path_Cases, "   "
                       , "Please use the existing examples there \"*_output.R\") to create an individual output description  ")
      cat(ErrMes, "  "); journal <- c(journal, paste0(ErrMes, "  ") )
    } else {
      path_output_act_user <- file.path(path_output_act,paste0("usr_output_", Tstamp))
      dir.create(path_output_act_user, showWarnings = FALSE)
    }
  }
}
