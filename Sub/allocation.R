#+
# NAME: allocation.R
# Purpose: Declaration of variables, i.e. allocates the storage for the arrays 
  # Note: the first index stays nearest, => should be the fastest / the innermost loop
  # initialization with NA sets the type of "logic"
# source(file.path(path_sub,"allocation.R",sep=""))
# Input:  Dimensions
# Output:
# REVISION HISTORY*:
#   2014-06-xx, MK+RQ: 
#   2019-11-22, RQ:    completely rewritten
#   2019-11-30, RQ: Feldstruktur: nz,ny,nx => nz,ny,nx
#   2021-04-17, RQ: pTc <- array(1,c(nz,nz,ny,nx)) => pTc <- array(0,c(nz,nz,ny,nx))

# -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pryr::mem_used() 
########################################### Declaration #########################################
  zz <- (0:(nz-1))*dz   # (1:nz)*dz
  zmax <- nz*dz
  area_xy <- nx*dx*ny*dy
  
  addNrows <- 0   # extention of the result fields if more time steps necessary

  # PAD_all <- aperm(PAD_all,c(3,2,1))   # transpose array to desired form
  

#  Vegetation ####
  # PAD <- array(NA,c(nz,ny,nx))  => Load_Veg_xxx.R
  # PADc <- array(NA,c(nz,ny,nx))

  
# Meteo ####
  Ta_3D <- array(NA,c(nz,ny,nx))              # Air temperature
  Ts_3D <- array(NA,c(nz,ny,nx))              # Surface temperature
  DDs_3D <- array(NA,c(nz,ny,nx))             # Saettigungsdampfdruck
  DD_3D <- array(NA,c(nz,ny,nx))              # Dampfdruck
  Rn_3D <- array(NA,c(nz,ny,nx))              # Nettostrahlung
  Rg_3D <- array(NA,c(nz,ny,nx))              # Globalstrahlung
  u_3D <- array(NA,c(nz,ny,nx))               # wind speed calculated at each grid point
  # usm <- array(NA, c(nz,ny,nx))               # friction velocity (def. in Meteo_Wind_prof.R)
  # uzm <- array(NA, c(nz,ny,nx))               # wind speed measurements (def. in Meteo_Wind_prof.R)
  
  # mixing_length <- array(NA,c(nz,ny,nx))    # => PAD2lm.R
  # pa_3D <- array(NA,c(nz,ny,nx))            # air pressure
  pa_3D <- NA

  clf <- 0                                    # Cloud Fraction
  
  
# Precipitation ###############################################
  # the following variables are defined in P_dist_matrix.R and P_distribution.R
  # pTc <- array(0,c(nz,nz,ny,nx))              # pTc[k,l,j,i] probability for throughfall of rain from point [l,j,i] in layer k
  # pIc <- array(0,c(nz,nz,ny,nx))              # pIc[k,l,j,i] probability for intercepting of rain from point [l,j,i] in layer k
  # Pthroughf <- array(0,c(nz,ny,nx))          # Throughfall
  # Pintercept <- array(0,c(nz,ny,nx))         # intercepted precipitation
  
  
# Storage & Drainage ###############################################
  C_3D  <-    array(0,c(nz,ny,nx))            # current storage
  sc0    <-    array(0,c(nz,ny,nx))           # filling of the canopy storage
  # sc1    <-    array(0,c(nz,ny,nx))           # proxy the partly saturation of the laminar boundary layer
  # S_3D <- array(NA,c(nz,ny,nx))             # canopy storage capacity per voxel (Benetzungskapazitaet)
  
  EvOn <- F                                  # logic flag, Event has started or ceased
  EvNo <- 0                                  # Event number
  EvOn.prev <- F                             # logic flag, Event has started or ceased in the previous time step
  DrOn <- F                                  # logic flag, Drainage has started or ceased
  a_3D <- array(NA,c(nz,ny,nx))              # Drainagekoeffizient
  Drainage <- array(0,c(nz,ny,nx))           # Drainage
  Dthroughf <- array(0,c(nz,ny,nx))          # durchtropfender Niederschlag
  Dintercept <- array(0,c(nz,ny,nx))         # interzipierter Niederschlag
  

# Verdunstung ##############################################
  ETR_3D <- array(NA,c(nz,ny,nx))               # Reale Verdunstung
  ET <- array(NA,c(nz,ny,nx))                   # Transpiration
  EV <- array(NA,c(nz,ny,nx))                   # Evaporation
  # des_3D <- array(NA,c(nz,ny,nx))               # Anstieg der Saettigungsdampfdruckkurve
  # rhoa_3D <- array(NA,c(nz,ny,nx))              # Luftdichte
  # L_3D <- array(NA,c(nz,ny,nx))                 # spezielle Verdampfungswaerme

  
#* Widerstände ####
  # GR <- array(0,c(nz,ny,nx))                    # weighting function for the canopy resistance
  rc_3D <- array(Rinf,c(nz,ny,nx))              # canopy resistance
  rs_3D <- array(Rinf,c(nz,ny,nx))              # stomatal resistance
  rac_3D <- array(Rinf,c(nz,ny,nx))             # 
  rb_3D <- array(Rinf,c(nz,ny,nx))              # laminar boundary layer resistance
  ra_3D <- array(Rinf,c(nz,ny,nx))              # aerodynamic resistance
  rw <- array(Rinf,c(nz,ny,nx))                 # resistance of wet surfaces
#  rLEw_3D <- array(Rinf,c(nz,ny,nx))           # total resistance for wet surfaces


  
  
  # Statistic 
  EV.max  <-  0;  Drainage.max  <-  0; C_3D.max <-  0;  Pinter.max <-  0
  
  
# Model variables ####  
  Dtest <- 0  
  Cmax <- 0 
  time.calc.ev.start <- 0 # used in CanWat.R to calculate the computing time/event
  
# end ###########################################################################  
# pryr::mem_used() 
  
  
  
  
