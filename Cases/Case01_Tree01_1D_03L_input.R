#+
# NAME*:        Tree01_1D_03L_input.R
# PURPOSE*:     CanWat input parameters for the Anchorstation Tharandter Wald 
# RELEVANCY*:   CanWat
# CALLING SEQ.: source(file.path(path_sub,"Template_input.R"))
# INPUTS*: 
# OUTPUT*:      Case specific parameters
# REVISION HISTORY*:
#    2021-01-10 RQ :
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("     -> read input file")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STATIC DRIVER ####################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #* name and range ####
  fnam.SD <- "Tree01_1D_03L.csv" # -, name of the static driver
  SD.dims = c( 3, 1,  1)      # -, [z,y,x]  dimensions
  dz <- 3.333; dy <- 1; dx <- 1    # m, grid resolution     
  xll.UTM  <- 0                # m, East coordinate of the lower left corner (south-west corner!!!)
  yll.UTM  <- 0                # m, North coordinate of the lower left corner
  zll.UTM  <- 0                # m, ASL, lowest point of the 3D domain
  
  #* position of the reference point 
  xref.UTM <- 0                # m, East coordinate of the reference point
  yref.UTM <- 0                # m, North coordinate of the reference point
  zref.UTM <- 0                # m, height above see level of the reference point
  
  #* region  of interest ####
  # borders of the domain with respect to the reference point
  # distance to the reference point in "m", 
  x1.ref <-  0;  x2.ref <-  1  # east of the reference point
  y1.ref <-  0;  y2.ref <-  1  # north of the reference point
  z1.ref <-  0;  z2.ref <-  9     
  
  # correction/clumping factor for PAD  (called in Load_Static_Driver.R )
  PADfunction <- function(PAD){ PAD + 0 }
  
  # # stemflow? #### => not active
  # sflow.s <- T   # switch for stem flow
  # SAI <- 1       # stem area index
    
  #* tree type ####
  treetype <- "picea_abies"
  CWpara_name <- paste0("CW_parameter-",treetype, ".R")   
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DYNAMIC DRIVER - Meteorolocial Data ################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fnam.MetData <- "Case02_DynD.csv" # -, name of the dynamic driver
  dt_met <- 600                     # s, length of a time step

#* Event Selection ####
  it1 <-   1;  it2 <-   48          # number of time steps, selected interval
  
#* Date description ####
  namTime <- "UTC"                  #  , name of the date column
  Tformat <- "%Y-%m-%d  %H:%M"      #  , format of the date column , e.g. "%Y-%m-%d  %H:%M:%S"
  ToffUTC <- 0                      # s, time offset to UTC (for CET it is -3600)
  Tsme    <- 0                      # -, the given time is at the <s>tart 0, <m>id 0.5, or <e>nd 1 the interval
  Tsoff   <- 0                      # s, offset from the interval-<s>tart of the given time 
  
#* Positioning of the measurements ####
   # heights for meteo measurements, from bottom to top
   # >>>>> in cm above ground level (AGL) <<<<<!!!!
   hWS <- c(1000)       # cm AGL, wind speed
   hWD <- hWS                                    # cm AGL, wind direction (not in use currently) 
   hDD <- c(1000)                            # cm AGL, water vapour pressure
   hTa <- c(1000)                            # cm AGL, temperature
   hRn <- c(1000)                                # cm AGL, net radiation
   hRg <- c(1000)                            # cm AGL, direct shortwave radiation
   # in m above sea level (ASL)
   hPa <- c(384)                                 # m ASL, air pressure
 
   # horizontal position of the measurements in 'm' with respect to the reference point
   # !!! Currently, the profile calculation excepts only one horizontal position
   yWS <- rep(0, length(hWS))       # m, north position , wind speed
   yWD <- rep(0, length(hWD))       # m, north position , wind direction
   yDD <- rep(0, length(hDD))       # m, north position , vapour pressure
   yTa <- rep(0, length(hTa))       # m, north position , temperature
   yRn <- rep(0, length(hRn))       # m, north position , net radiation
   yRg <- rep(0, length(hRg))       # m, north position , direct shortwave radiation
   xWS <- rep(0, length(hWS))       # m, east position , wind speed
   xWD <- rep(0, length(hWD))       # m, east position , wind direction
   xDD <- rep(0, length(hDD))       # m, east position , vapour pressure
   xTa <- rep(0, length(hTa))       # m, east position , temperature
   xRn <- rep(0, length(hRn))       # m, east position , net radiation
   xRg <- rep(0, length(hRg))       # m, east position , direct shortwave radiation   
   
#* variable names ####
   # names within the supplied data frame       
   PP_namo <- c("PF","PB")                      # precipitation, troughfall
   WS_namo <- paste0("u",as.character(hWS))     # wind speed in m/s
   Ta_namo <- paste0("T",as.character(hTa))     # temperature in °C
   DD_namo <- paste0("DD",as.character(hDD))    # water vapor pressure in hPa
   Rn_namo <- paste0("Rn",as.character(hRn))    # radiation balance in W/m²
   Rg_namo <- paste0("Rg",as.character(hRg))    # radiation shortwave (solar) in W/m²
   Pa_namo <- paste0("Pa")                      # atmospheric pressure in hPa
   

#* additional specifications #
   iWSref <- length(hWS)      # -, index of the wind measurement that is used of as a reference in the wind field calculation 
   
   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT DEFINITION #################################################
#* Standard Output Variables ####
   # the output file <case name>_WB_<time stamp>.csv and <case name>_WBa_<time stamp>.csv contains the means of:
   # c("UTC","dt","C_3D","dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf")
   # the output is given in means over the domain, unit is always: [variable] = mm/timestep 
   # exceptions are [UTC] = s; [dt] = s; [C_3D] = mm 
   # UTC: start time of time steps, dt: length of timesteps
   # C_3D: storage content, dC: storage change
   # PF: gross precipitation, Pintercept: intercepted precipitation, Pthroughf: throughfall precipitation
   # EV: Evaporation, ET: Transpiration
   # Drainage: Drainage, Dintercept: reintercepted Drainage, Dthroughf: througfall drainage
   # Rn: net radiation, sH: sensible heatflux
   # within the variable "output.mean.usr" you can name additional output variables 
   # Please define this character vector in your input file
      
#* User extension of the Standard output ####
   # Within the character vector "output.mean.usr" you can name additional output variables.
   # The mean of these variables is added to the output files named above
   # e.g. output.mean.usr <- c("Rn_3D","Rg_3D","Ta_3D","DD_3D","u_3D", "rLE_3D", "rH_3D")  
   # with Rn_3D: net radiation in W/m², Rg_3D: downward shortwave radiation in W/m²
   # Ta_3D: temperature, DD_3D: water vapour pressure, u_3D: wind speed
   # rLE_3D: resistance against Evapotranspiration, "rH_3D: resistance against sensible heat flux (averaged using the conductivity)
   output.mean.usr <- c("rLE_3D", "rH_3D")

# General output switches ####
  silent <- 0     # Output control flag: 1: suppress print output within the time loop,  2: suppress print output completely
  plotMeanTS <- T # plot of the water budget, this might be slow
  plotlife <- T   # control plot of the water budget after each time step, this might be even slower
  
# Numerical Output #########
  # switches for the output defined in the script output_timestep.R 
  # that is called in each time steps 
  
  #* Means of horizontal layers  ####
  # one vertical profile for the whole domain for each time step 
  C_lmean.S <- F     # FALSE or TRUE   
  ETR_lmean.S <- F   # FALSE or TRUE 
  ET_lmean.S  <- F   # FALSE or TRUE 
  EV_lmean.S  <- F   # FALSE or TRUE 
  
  #* Instantaneous Values ####
  # this takes a lot of time and some storage!    
  field.out <- F  # FALSE or TRUE 
  # the following variables can be set to "NA" or hold a number,
  # which indicates the position of the cross-section in relatione to the reference point (see above)
  xcut.out <- NA  # NA = no output or the distance of the yz-cross-section from reference
  ycut.out <- NA  # NA = no output or the distance of the xz-cross-section from reference
  zcut.out <- NA  # NA = no output or the distance of the xy-cross-section from reference
  
# Individual output #########    
  # the following script is called each time step, 
  # it must be located in the case folder
  fnam.out <- "Output_yz-cross-section.R"   #paste0(act, "_TS_output.R")
  
  
