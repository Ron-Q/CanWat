#+
# NAME*: CanWat.R
# PURPOSE*: main routine of 'CanWat'
#   Numerical model for the calculation of the incanopy water balance based on 
#   gross rainfall and a vegetation model
# RELEVANCY*: Water balance of forests
# CALLED BY: CanWat_Start.R
# CALLING SEQ.: source(file.path(path.CW,"CanWat.R"))
# INCLUDED SUBROUTINES:  see ../Sub/  
# REFERENCE:
#   Rutter (1971)
#   Queck  (1997)
#   Kunath (2015)
# REVISION HISTORY*:
#   the idea was born within the diploma thesis of Queck (1997)
#   it grew in several student projects under the supervision of Dr. Ronald Queck
#   important steps were:
#   2011: Project work of Stefan Ploetner (spaghetty program which combined different approaches)
#   2014: Diploma Thesis of Martin Kunath (time consuming laborious job in every aspect)
#   2015: DFG proposal "Interception: Localization of plant–atmosphere interactions during and after rain events." 
#         (like always, there is to little time to reduce clutter and make the subroutines easier to read)
#   2019 (RQ): 2th Phase, reviving CanWat, or more exactly, a new one is written
#   2019-10-01 (RQ): numerical solution of the central differential equation by Runge-Kutta approach (4th order)
#   2019-11-29 (RQ): variable time steps introduced
#   2020-12-28 (RQ): revision of time step calculation and several minor changes in subroutines 
#   2021-01-27 (RQ): revision of radiation transfer, and some minor errors (including switch to UTC and the wrong WBa output)
#   Version 202101: in the following the version number is equal to the date of the last change
#   2021-03-25 (RQ): domain extended for an empty bottom layer, revision of wind module
#   2021-03-27 (RQ): switched unit of PAD input to m²/m³
#   2022-05-10 (RQ): calculation of maxima moved output definition files to main routine (CanWat)
#   2022-07-11 (RQ): changes in Meteo_Rn.R, Meteo_Wind_prof.R and windpro.R and in CanWat.R small changes in definition of graphical output device
#   2022-09-29 (RQ): time output in outer time loop was last start time from inner loop => corrected to start time of outer time loop
#   2023-07-19 (RQ): event Handling integrated.
#   2023-08-09 (RQ): replaced paste0() with file.path(), corrected and added journal entries, added option to extend events fitting to other time step lengths
#   2025-01-22 (RQ): radiation handling reviced + diverse small changes and bug fixes
#   2025-03-15 (RQ): event output as standard
#   2025-05-15 (RQ): CHECK INPUT after parameter input, as parameter input is also edited by the users 
CWversion <- 20250515
#-
#-- main program ---------------------------------------------------------------

# CanWat_Start.R ###############################################################
#stop("check start values")
  if (!exists("CanWatStart")) CanWatStart <- F
  if (!exists("Test")) Test <- F # else if (!Test) CanWatStart <- F
  if (!exists("RunControlFine")) RunControlFine <- F # if TRUE there is a higher frequency of controll outputs
  
# INPUT, if CanWat starts without CanWat_Start.R ###############################
  if (!CanWatStart) {
    #* CLEAR the WORKSPACE? ####
    # if TRUE, every variable will be deleted !
    if (TRUE) {remove (list = ls())             ; print("######## Reset #########")}
    if (FALSE) {for (i in dev.list()) dev.off(i) ; print("## all devices closed ##")}

    #* Control Variables ####
    # select a case/incident
     act <- "Case01_Tree01_1D_01L" # select a case/incident
    act <- "Case01_Tree01_1D_02L"
    # act <- "Case01_Tree01_2D_10L" # select a case/incident
    # act <- "ASTW0"
    # act <- "ASTW0_exmpl_07"
    # act <- "ASTW_exmpl_07"
    # act <- "ASTW0_sonnyDay"
    act <- "ASTW_CW_dynamic_driver_2010"

    #* Paths ###################################################################
    switch(Sys.info()["nodename"]
           , "MICROPC16" = {path.CW <- "d:/b_Programming/0_R_lib/y_CanWat/"}
           , "User2" = {path.CW <- "d:/y_CanWat/"}
    )
    path_DriverStat <- file.path(path.CW , "Driver_static")
    path_DriverDyn <- file.path(path.CW , "Driver_dynamic")
    path_Cases <- file.path(path.CW , "Cases")
    path_output <- file.path(path.CW , "Output")
    path_sub <- file.path(path.CW , "Sub")
  }
  path_output_act <- file.path(path_output, paste0(act, "_output"))
  if (!dir.exists(path_output)) dir.create(path_output, showWarnings = FALSE)
  if (!dir.exists(path_output_act)) dir.create(path_output_act, showWarnings = FALSE) 

  
  
# INPUT END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# @User: Don't change the following! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @Developer: you know what you do, don't you? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  
  
#* Load Packages and Subroutines ################################################
  xlib <- c("data.table", "png", "rasterImage", "colorRamps") # "raster" , "plotrix") 
  # xlib <- c("data.table", "png") 
  lapply(xlib, require, character.only = TRUE)
  source(file.path(path_sub,"Meteo_Ta.R"))
  source(file.path(path_sub,"Meteo_DD.R"))
  source(file.path(path_sub,"Meteo_Pressure.R"))
  source(file.path(path_sub,"Meteo_Rg.R"))
  source(file.path(path_sub,"Meteo_Rn.R"))
  source(file.path(path_sub,"ETR_funct.R"))
  source(file.path(path_sub,"Drain_funct.R"))
  source(file.path(path_sub,"plot_timestep_sum.R"))  
  source(file.path(path_sub,"P_dist_matrix.R"))
  source(file.path(path_sub,"storage_parameter.R"))
  source(file.path(path_sub,"windpro.R"))
  source(file.path(path_sub,"time_processing.R"))
  
  intToHex<-function(x){x<-as.integer(x);class(x)<-"octmode";as.character(x)}
  stopQuietly <- function(...) {
    blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
    stop(simpleError(blankMsg));
  }
  
  # assign auxiliary variables
  start_time <- Sys.time()
  Tstamp <- strftime(start_time, format("%Y%m%d_%H%M"))   ; if (!exists("Tstamp_serie")) Tstamp_serie <- Tstamp else Tstamp_serie <- paste(Tstamp_serie, "\n", Tstamp)
  BERR <- 0 # multi purpose test variable

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# ~~~ START ~~~ ##################################################################
  print(paste(""))
  print(paste("#############################################"))
  print(paste("### CanWat ### Canopy Water Balance Model ###"))
  print(paste("############## Version: ", CWversion, "############"))
  print(paste("Time stemp: ",Tstamp))
  print(paste("for details see journal: ", file.path(path_output_act, "CanWat_journal_",Tstamp,".txt  " )))
  print(paste("started from folder: ",path_Cases, ", Case: ",act))
  # Log File "journal" ---------------------------------------------------------
  journal <- c(paste0("title: CanWat - run log: ",Tstamp, "  "), paste0("---  "), paste0("  ") 
               , "# CanWat - Canopy Water Balance Model"
               , paste0("author: ronald.queck@tu-dresden.de  ")
               , paste0("Version: ",CWversion, "  ")
               , paste("started from folder: ",path_Cases, "  ")
               , paste("on machine '", Sys.info()["nodename"],"'") 
               , paste("Case: ",act, "  ")
               , paste0("log output for the run with timestamp: ",Tstamp  , "  ")
               , paste0("  "))
  
# READ INPUT ###################################################################

#* load names of dynamic and static driver, and parameters ----------------------
  source(file.path(path_Cases, paste0(act, "_input.R")))    

#* Parameters & Constants -------------------------------------------------------
  source(file.path(path_CWpara, CWpara_name))    

  
# CHECK INPUT ##################################################################  
#* preprocess the parameter ----------------------------------------------------
  source(file.path(path_sub, "Input_PreProcessing.R"))    


#* case "journal" ---------------------------------------------------------------
  starttime <- Sys.time()
  journal <- c(journal, "  "
              , paste("# Case:  ")
              , paste("Case name: ",act, "  ")
              , paste("started from folder: ",path_Cases, "  ")
              , paste0("dynamic driver (Event):   ", fnam.MetData, "  ")
              , paste0("static driver (Location): ", fnam.SD, "  ") 
              , " "
  )
  if (field.out) journal <- c(journal, "output fields at each time step is set  ")
  if (!is.na(xcut.out)) journal <- c(journal, paste("yz-intersections at x = ", xcut.out, "  "))
  if (!is.na(ycut.out)) journal <- c(journal, paste("xz-intersections at y = ", ycut.out, "  "))
  journal <- c(journal, "  ")

    
# READ DATA ####################################################################
#* Static Driver - Vegetation data ---------------------------------------------
  journal <- c(journal, "# Static Driver (vegetation data) ####  "
               , paste0("file name ", path_DriverStat,"/",fnam.SD, "  ")
#               , paste0("Data from ", path_DriverStat,"/",fnam.SD)
               )
  source(file.path(path_sub, "Load_Static_Driver.R" ))
  # Output: PAD, PADc
  
  
#* Meteo-data ------------------------------------------------------------------
  journal <- c(journal, "# Dynamic Driver (meteorological data, events)  "
               , paste0("file name: ", fnam.MetData, "  ")
               , paste0("data path: ",path_DriverDyn , "  "))
  source(file.path(path_sub,"Load_Dynamic_Driver.R"))
  # Output: metPP, metWS, metTa, metDD, metRn, metRg, metPa, EvNo.in
  

#* Def. of Standard Output -----------------------------------------------------
  source(file.path(path_sub,"allocation.R"))


# CALC FACTOR FIELDS ###########################################################
  #* Generic wind profiles ####
  # calculated from PAD and some parameters
  fresult=NA;  try(fresult <- windpro(PAD, zz, z0, hWS, imref, ztop, dz, dy, dx, nz, ny, nx, iyWS=iyWS, ixWS=ixWS))
  if ( !is.list(fresult) ) stopQuietly() else list2env(fresult,env=environment())
  # Output: uz : generic wind profiles
  
  #* Interception Model Parameter ####
  # relations between LAD and C, S and p (p is the part of the throughfall per layer)
  fresult=NA;  try(fresult <- storage_parameter(S0, Dmin0, bd0, PAI.S0, PAD, dz))
  if ( !is.list(fresult) ) stopQuietly() else list2env(fresult,env=environment())
  # Output: S_3D saturation capacity
  #         Dmin0, bd : drainage parameters

  #* P distribution matrix ####
  # Generic function of PAD to distribute precipitation and drainage to the subjacent layers
  fresult=NA;  try(fresult <- P_dist_matrix(PAD, cc1, cc2, nz))
  if ( !is.list(fresult) ) stopQuietly() else list2env(fresult,env=environment())
  # Output: pTc throughfall fraction
  #         pIc intercepted fraction

  #* Radiation distribution matrix ####
  # Generic function to calculate that part of the radiation 
  # that is transmitted through the canopy between layer i and the top
  # deliverers the part at the bottom of the layer (at the top of the next lower layer)
  # pRtrans <- function(PADc, extinct=0.5){ Rfrac <- exp(-(extinct*PADc)) }
  pRtrans <- exp(-(extinct*PADc))   # the higher PADc the lower pRtrans
  pRtrans[nz,,] <- 1                # top layer is empty, all radiation goes trough
  
  # !!!!!!!!!!!!!!!!!!!!
  # pRtrans[nz-1,,] <- 1                # Test für Rechnungen mit einem LAYER
  # !!!!!! there is no radiation within bigleaf forests !!!!!!!!!!!!!!!!!!
  # <<<<<< here is a problem !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # !!!!!! should be a mean value between top and bottom of the grid cell !

  # EVENT HANDLING ###########################################
  # Select Events ####
  if (!is.na(sum(EvNo.selected))){
    nEvents <- length(EvNo.selected)
    iit1a <- rep(1, nEvents)
    iit2a <- rep(2, nEvents)
    for (i.Ev in 1:nEvents) {
      ii <- which(met2Ev$EvNo == EvNo.selected[i.Ev])   # select an event
      timeest1 <- met2Ev$UTC[min(ii)-Event.ext.b.i]       # available timesteps
      timeest2 <- met2Ev$UTC[max(ii)+Event.ext.a.i]
      # time interval regarding the timesteps that are demanded
      Ev.ts12.0 <- round.timestep(c(timeest1, timeest2), tsl = Event.ext.round, toff = 0, boundary = c("floor","ceiling"))
      iit1a[i.Ev] <- max(1, which(met2Ev$UTC <= Ev.ts12.0[1]))    # met2Ev$UTC[iit1a[i.Ev]]
      iit2a[i.Ev] <- min(ndat, which(met2Ev$UTC >= (Ev.ts12.0[2] - dt_met))) # met2Ev$UTC[iit2a[i.Ev]]
    }
  } else {
    nEvents <- 1
    iit1a <- 1
    iit2a <- ntu
  }
  
  

  #* Setup Output Tables 1 --------------------------------------------------------
  headim <- c("UTC","EvNo", "dt", "C_3D", "dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf", "Rn", "sH")  # output of the inner loop
  headom <- headim
  if (!exists("output.mean.usr")) output.mean.usr <- NA
  if (!anyNA(output.mean.usr)) headom <- c(headim, output.mean.usr)
  novar <- length(headom)   # number of output variables
  WBev  <- as.data.table(array(0 , c(nEvents, novar+1)));   setnames(WBev,names(WBev),c("UTCstart","UTCend",headom[-1]))
        # WBev$UTCend includes the last interval
  
  i.Ev <- 1L  # counter for predefined events
  for (i.Ev in 1:nEvents){ # ~> Event loop ~>~>~> #####
    
    iit1 <- iit1a[i.Ev]
    iit2 <- iit2a[i.Ev]
    iit12 <- iit1a[i.Ev]:iit2a[i.Ev]
    ntu  <- iit2-iit1+1  
    if (!is.na(sum(EvNo.selected))) {
      EvNo <- EvNo.selected[i.Ev]
      EvNo_str <- paste0("_EvNo", formatC(EvNo, width=3, flag = "0"))
      print(paste0(" ~~~~~~~~~ Event ",  EvNo , ", calculation starts ~~~~~~~~~~~~~~~~~~"))
      journal <- c(journal, "  "
                   , paste0("# ~~ Event ",  EvNo , " ~~~~~~~~~~~~~~~~~~  ")
                   , paste0("calculation starts at", Sys.time(), "  ") )
    } else EvNo_str <- ""

        
    #* Graphic Setup -----------------------------------------------------------
    bgcol <- "white"  # background colour
    if (plotMeanTS) { # plot of the water budget, this might be slow
      figsize = 1500
      if (plotlife) { # control plot of the water budget after each time step, this might be even slower
        devW <- dev.list()[match("windows",names(dev.list()))]
        if (is.null(devW) | anyNA(devW)) {x11(width = 14, height = 9); devW <- dev.cur()}
        op <- par(bg = bgcol)
        # try to control the appearance of the device => imho, is not possible up to now, ==> to it by hand
        # msgWindow(type="maximize", which = devW); bringToTop(which = devW, stay = FALSE)
      } else {
        png(file.path(path_output_act, paste0(act, EvNo_str ,"_", Tstamp,".png")), bg = bgcol
            , width = figsize, height = figsize/2, units = "px", pointsize = 20)
        devW <- dev.cur()
      }
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
    }
    
    
    #* Data selection ----------------------------------------------------------
    # if (!is.na(sum(EvNo.selected))) metEv <- met2Ev[iit12] # not used
    metPP <- met2PP[iit12]
    metWS <- met2WS[iit12]
    metTa <- met2Ta[iit12]
    metDD <- met2DD[iit12]
    metRn <- met2Rn[iit12]
    metRg <- met2Rg[iit12]
    metPa <- met2Pa[iit12]
    metWS.top <- met2WS[[WS_nam[dim(met2WS)[2]-1] ]][iit12]
    metTa.top <- met2Ta[[Ta_nam[dim(met2Ta)[2]-1] ]][iit12]
    metDD.top <- met2DD[[DD_nam[dim(met2DD)[2]-1] ]][iit12]
    metRn.top <- met2Rn[[Rn_nam[dim(met2Rn)[2]-1] ]][iit12]
    metRg.top <- met2Rg[[Rg_nam[dim(met2Rg)[2]-1] ]][iit12]
    metPa.top <- met2Pa[[Pa_nam[dim(met2Pa)[2]-1] ]][iit12]
    (datr <- range(metPP$UTC))
    
    #* extend the number of time steps with the expected number of "sub" time steps
    nta <- ntu + length(which(metPP$PFi > 0)) * 2 * ceiling(dt0/dt1)
    
    
    #* Setup Output Tables 2 --------------------------------------------------------
    WF  <- as.data.table(array(0 , c( ntu, novar)));   setnames(WF,names(WF),headom) # Fluxes, input time step
    WB  <- as.data.table(array(0 , c( ntu, novar)));   setnames(WB,names(WB),headom) # Totals, input time step
    WFs <- rep(0,length(WF)-3); names(WFs) <- headom[-(1:3)]  # Fluxes, temporarily bucket
    WBs <- rep(0,length(WB)-3); names(WBs) <- headom[-(1:3)]  # Total, temporarily bucket
    WBa <- as.data.table(array(0 , c(nta,novar)));   setnames(WBa,names(WBa),headom) # Fluxes, all computed time steps
    WFa <- as.data.table(array(0 , c(nta,novar)));   setnames(WFa,names(WFa),headom) # Totals, all computed time steps
    WBev0  <- as.data.table(array(0 , c(ntu/2, novar+1)));   setnames(WBev0,names(WBev0),c("UTCstart","UTCend",headom[-1]))
    i.Ev0 <- 0L # internal event counter
        
    # fields for layer means (Mittel über die Bestandesschicht) 
    if (C_lmean.S)   C_lmean   <- array(NA, c(nta,nz))
    if (ETR_lmean.S) ETR_lmean <- array(NA, c(nta,nz)) 
    if (ET_lmean.S)  ET_lmean  <- array(NA, c(nta,nz))
    if (EV_lmean.S)  EV_lmean  <- array(NA, c(nta,nz))
    
# stop("TEST")
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~> ~> TIME LOOP  ~>~>~> ####
    if (plotMeanTS) ylimC <- plot_timestep_sum_0(devW, ntu=ntu)      # create plot in devW and return y-range
    dtc <- dt0      # initialization of the current time step size  
    dtcm <- dt0     # initialization of the moving average of the time step size  
    itt <- 0L       # current number of calculated time steps including the spreading during precipitation
    iti <- 1L       # initiation of the inner loop counter
    ito <- 1L       # selection of a time step, definition needed for tests only
    pTS <- "."      # character to monitor the status
    for (ito in 1:ntu) {
      if (exists("ito.stop")) if (ito > ito.stop) stop(paste("debug stop at timestep", ito))
      if (silent < 2) if (RunControlFine){ 
        print(paste0("outer time step ",ito, ", with ",dt0,"s at ", metPP$UTC[ito], " #############################"))
      } else {
        if (((ito %% 100) == 0) | ito == 1) {cat("\n ")
          print(paste0("outer time step ",ito, " of ",ntu,", at ", metPP$UTC[ito], ": "))
        } else cat(pTS)
      }
  
      #* available Measurements
      PFi <- metPP$PFi[ito]    
      Tadat <- as.numeric(metTa[ito,Ta_nam, with=F]) ; Tatop <- Tadat[length(Tadat)]
      DDdat <- as.numeric(metDD[ito,DD_nam, with=F]) ; DDtop <- DDdat[length(DDdat)]
      padat <- as.numeric(metPa[ito,Pa_nam, with=F]) ; if (is.na(padat)) padat <- 1013
      Rgdat <- as.numeric(metRg[ito,Rg_nam, with=F]) ; Rgtop <- Rgdat[length(Rgdat)]
      Rndat <- as.numeric(metRn[ito,Rn_nam, with=F]) ; Rntop <- Rndat[length(Rndat)]
  
      #* Meteorological Fields ####
      # the calcuation of meteorological fields and the resistances against evaporation is done for the outer time step only
      source(file.path(path_sub,"Meteo_Wind_prof.R"))  # u_3D, usm
      Ta_3D   <- Meteo_Ta( Tadat, hTa/100, Ta_3D, zz)
      Ts_3D   <- Ta_3D             # surface temperature (wood and leaves)
      DD_3D   <- Meteo_DD( DDdat, hDD/100, DD_3D, zz)
        DDs_3D  <- (6.11*exp((17.62*Ta_3D)/(243.12+Ta_3D)))
      pa_3D   <- Meteo_pressure(padat, pa_3D, zz)
      Rg_3D   <- Meteo_Rg( Rgdat, hRg/100, PADc, pRtrans, dz, Rg_3D) # Extinction by Beers law !?!?!?!?!
      Rn_3D   <- Meteo_Rn( Rndat, hRn/100, Rg_3D, Rgtop, Ts_3D, Tatop, DDtop, PAD, PADc, pRtrans, dz, nz, Rn_3D, padat) 
# stop("Stop at Meteorological Fields")  
      
      #* Parameter and resistances ####
      source(file.path(path_sub,"Humidity.R"))
      source(file.path(path_sub,"ET_resistances.R")) 
    
      # precipitation distribution within the canopy into "Pthroughf" and "Pintercept"
      source(file.path(path_sub,"P_distribution.R"))
  
      if (plotMeanTS & ntu<50){dev.set(devW); abline(v=metPP$UTC[ito], col="gray" )}   # vertical gray line
      # WFs <- WFs*0 # rep(NA, novar-2)
      WBs <- WBs*0 # rep(NA, novar-2)
      dt0r <- as.integer(dt0)  # start value for the rest of the outer time step
      iti <- 0L
      
# cat(ito, "\n")
# if (!exists("istop")) istop <- 1 else if (istop > 6) stop("istop") else istop <- istop + 1; paste(istop, metRn[ito,UTC], sep=", ")
      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # > > > INNER LOOP >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ----
      while(dt0r > 0){
        iti <- iti +1L   # counter of the inner loop
        itt <- itt +1L   # global counter of the time steps
        
        WFa[itt] <- NA;      WBa[itt] <- NA

        
        # Distribution of Drainage from the last time step !!!! very slow !!!! ####
        source(file.path(path_sub,"Drainage_distribution.R"))     
        
        
        # Time Step Calc ####################################################
        # dt will be changed according to PFi, storage filling and Drainage
        dtc.old <- dtc
        if ((PFi > 0) | any(Drainage > 0)) { 
          
          ##* calculate an estimate of the maximal possible step size "dtmax" ####
          # what is the index of the current time step in the vector of possible time steps
          idtc <- max(which(dtc >= dts))
          iCmaxe <- which.max((C_3D + (Pintercept+Dintercept)*dtc)-S_3D) # what is the critical cell
          Pinterc.Cmaxe <- Pintercept[iCmaxe]
          DintercB.Cmaxe <- Dintercept[iCmaxe]*dtc  # as budget due to, 
            # drainage was regarded in the budget of last time step, with the length of that time step, 
            # thus this amount is must be going into the budget of the next (this) time step
          C_3D.Cmaxe <- C_3D[iCmaxe]; S_3D.Cmaxe <- S_3D[iCmaxe]
          Cmaxe <- C_3D.Cmaxe + Pinterc.Cmaxe*dtc + DintercB.Cmaxe   # max storage using dtc
          Dtest <- Drain_funct(Cmaxe, S_3D.Cmaxe, Dmin=Dmin, bd=bd)  # max Drainage
          if (Dtest != 0) {
            dtmax <- min(max(Cmaxe/Dtest, 1), dt1) 
            dtvar <- dtc
            while(abs(dtvar-dtmax)>5) {
              (dtvar <- dtvar - (dtvar-dtmax)/10)
              (Cmaxe <- C_3D.Cmaxe + Pinterc.Cmaxe*dtvar + DintercB.Cmaxe )
              (Dtest <- Drain_funct(Cmaxe, S_3D.Cmaxe, Dmin=Dmin, bd=bd))
              (dtmax <- max(Cmaxe/Dtest,1))
            }
            dtmax <- ceiling(Cmaxe/Dtest/fdtmax)                # no risk: reduce the maximal time step by factor fdtmax
          } else dtmax <- dts[idtc+1]                           # dtc could increase, but slowly
          
          if (dtmax < dtc) dtcm <- dtmax else {
            dtmax <- dtmax*0.2 + dtcm*0.8                       # don't increase to fast
            dtcm <- dtmax                                       # trace of the time step size
            if (dtmax > dts[idtc+1]) dtmax <- dts[idtc+1]       # max increase is one step
          }
          
          ##* choose a time step ####
          if (dtmax > dt1) dtc <- dt1 else                      # maximum time step if there occurs rain or drainage
            if (dtmax < 1) dtc <- 1 else                        # minimum of the time step
              dtc <- dtmax # dts[max(which(dtmax >= dts))]
          # Cmaxe <- C_3D.Cmaxe + Pinterc.Cmaxe*dtc + DintercB.Cmaxe
          # Dtest <- Drain_funct(Cmaxe, S_3D.Cmaxe, Dmin=Dmin, bd=bd); if (Dtest > 5) stop("Dtest > 5")
          
          #~~ fit it to the remaining time
          if (dtc > dt0r) dtc <- dt0r else {
            if ((dt0r %% dtc) > 0) {
              if (dtc < dtc.old){ 
                ndt <- dt0r %/% dtc + 1
                while((dt0r %% ndt) > 0)  ndt <- ndt + 1
                dtc <- dt0r %/% ndt
              }
            }
          }
          
        } else  {dtc <- dt0r} #  {dtc <- dt0}
        dtc <- as.integer(dtc)                        
        
        #cat(dt0r, ", ", dtc, "| ")        
        ##* expand WBa ####
        # If so, then add not only the needed rows but the 10-fold number
        ni <- (dt0r/dtc)  # expected number of steps within the current base time step dt0 
        if ((itt + ni) > nta)  {
          addNrows <- 10*ceiling(dt0/dt1);  nta <- nta + addNrows
          WBa <- rbindlist(list(WBa, as.data.table(array(NA, c(addNrows,novar) )  )),use.names=FALSE) 
          WFa <- rbindlist(list(WFa, as.data.table(array(NA, c(addNrows,novar) )  )),use.names=FALSE) 
        }
        
        ##* correction of Drainage for time step change ####
        # the drained water amount of the last time step is Drainage*dtc.old
        Dthroughf <- Dthroughf*dtc.old/dtc; Dintercept <- Dintercept*dtc.old/dtc
        
        sUTC <- metPP$UTC[ito] + dt0 - dt0r # start time of the interval
        dt0r <- dt0r - dtc    
        # sUTC <- metPP$UTC[ito] + dt0 - dt0r-dtc # start time of the interval
        # cat(sUTC, dtc, PFi, max(C_3D), max(Drainage), "Cmaxe", Cmaxe, "Dtest",Dtest, "\n")
        # if (silent < 1) if (iti > 2) print(paste0("    inner step ",iti, ", with ",dtc,"s : ", sUTC, " #############################"))
        
  
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Runge-Kutta approach to solve dC/dt  = PInt(t) + Dint(t) - E(t) - D(t) ####
        #stop("at Runge-Kutta")        
        interc <- (Pintercept + Dintercept)
        source(file.path(path_sub,"Runge-Kutta-approach.R"))      
        # ==>  dC = dtc * (interc - EV - Drainage )
        if ( any(C_3D < -dC) ) stop("in balance !!!!!!!!!!", call. = F) else C_3D <- C_3D+dC
        rest <- which(C_3D < 1e-18)      # storage values less than the threshold are moved into evaporation 
        EV[rest] <- EV[rest]+C_3D[rest]; C_3D[rest] <- 0
        sH_3D <- Rn_3D/L_3D - EV - ET  # sens. heat as remainder term
        # (is not done in ETR_funct.R to save computing time)
        # cat("WBii", sum(dC- (interc - EV - Drainage)*dtc) )
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        

        
        # OUTPUT ####
        #* Calculate extremes #### 
        EV.max <- max(EV,EV.max);     Drainage.max <- max(Drainage,Drainage.max)
        if (max(C_3D) > C_3D.max){
          C_3D.max <- max(C_3D)
          C_3D.max.date <- as.numeric(sUTC)
        }
        Pinter.max <- max(Pintercept,Pinter.max)
  
        #* Calculate mean fluxes and sums #### 
        # mean fluxes in mm/s  (i.e. the single fluxes are in mm/s for each voxel, then the function "sum(3D_Domain)/Domain_area)" gives mean vertical sums 
        C.mean <- sum(C_3D)/(nx*ny)  # mean Canopy Storage
        fluxi <- c(sum(dC)/(nx*ny)/dtc,  PFi                                        # "1,dC"         "2,PFi"
                    , c(  sum(Pintercept), sum(Pthroughf[1,,]), sum(EV), sum(ET)    # "3,Pintercept" "4.Pthroughf" "5,EV"   "6,ET" 
                        , sum(Drainage),   sum(Dintercept),     sum(Dthroughf[1,,]) # "7, Drainage"  "8, Dintercept-1" "9, Dthroughf"
                        , sum(Rn_3D/L_3D), sum(sH_3D)                               # "10, Rn", "11, sH"  
                       )/(nx*ny)    )
        # headim <- c("UTC", "dt", "C_3D" , "dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf", "Rn", "sH") # see allocation
        if (iti == 1) toti <- fluxi*dtc else toti <- toti + fluxi*dtc
        
        #* Status ####
        if ((fluxi[2] > 0) & !EvOn) {EvOn <- T
          if (!is.na(sum(EvNo.selected))) EvNo <- EvNo.selected[i.Ev] else EvNo <- EvNo+1
          if (silent < 2) if (RunControlFine) print(paste("++++++++++++++++++++++++++ Event", EvNo,"has started ++++++++++++++++++++++++++")) else pTS <- "E"
        }
        if ((C.mean < 0.00001) & (PFi == 0) & EvOn) {EvOn <- F; if (silent < 2) if (RunControlFine) print(paste("-------------------------- Event", EvNo,"has ended - Canopy is dried ----------")) else pTS <- "."}
        if ((fluxi[7] > 0) & !DrOn) {DrOn <- T; if (silent < 2) if (RunControlFine) print("++++++++++++++++++++++++++ Drainage has started +++++++++++++++++++++++") else pTS <- "D"}
        if ((fluxi[7] == 0) & DrOn) {DrOn <- F; if (silent < 2) if (RunControlFine) print("-------------------------- Drainage has ceased ------------------------") else pTS <- "E"}
        
        
        # write the current variable timestep to output table
        WFa.i <- as.list(c(as.numeric(sUTC), EvNo*EvOn,dtc, C.mean, fluxi))
        set(WFa, i=itt, j=headim, value=WFa.i)  # WFa[itt]
        WBa.i <- as.list(c(as.numeric(sUTC), EvNo*EvOn, dtc, C.mean, fluxi*dtc))
        set(WBa, i=itt, j=headim, value=WBa.i)  # WBa[itt]
 
        #* Control Graphic ####
        if (plotMeanTS){plot_timestep_sum(WFa[itt], ds=devW, DrOn)}   
        
        #* Optional Output ####
        # call output routine
        if (layer.out) source(file.path(path_sub, "output_timestep.R"))
  
        #* Individual Output ####
        if (!anyNA(output.mean.usr)){
          for(usr.vars in output.mean.usr){
            vart <- get(usr.vars)
            if ( !(usr.vars  %in% c("L_3D")) ) { # if it is not a single value calculate mean
              vart <- vart[-c(1,nz),,]
              if ( usr.vars %in% c("rH_3D", "rLE_3D", "rw_3D", "rET_3D", "rc_3D", "rb_3D", "rab")){
                vart <- round(1/mean(1/vart, na.rm=T)) # especially for resistances
              } else {
                vart <- mean(vart, na.rm=T) 
              }
            }
            set(WBa, i=itt, j=usr.vars, vart)
          }
        }
        # if (itt > 4) stop("stop at individual output")
        
        #* User defined Output ####
        if (!is.na(fnam.out) & !(Test)) source(file.path(path_Cases, fnam.out))

        
        #* Operational Checks ####
        if (silent < 1) { 
          # print(paste("ET",round(c(iti,dtc, sum(ET)/(nx*ny), fluxi[6], toti[6]/dtc),6)  ))
          
          #* Check balance of the current time step 
          # WBF <-   WFa[ito]$dC - (WFa[ito]$Pintercept + WFa[ito]$Dintercept - WFa[ito]$EV - WFa[ito]$Drainage)
          # WBB <-   WBa[ito]$dC -  WBa[ito]$Pintercept - WBa[ito]$Dintercept + WBa[ito]$EV + WBa[ito]$Drainage 
          # cat(paste("WB inner ERROR: ",     WBF, WBB,"\n"))   # => OK
          
          # pWB <- as.numeric(round(c(                                        WBa$PF[itt], WBa$Pthroughf[itt], WBa$C_3D[itt], WBa$Dthroughf[itt], WFa$Drainage[itt], WBa$EV[itt], WBa$ET[itt]  ), 3))
          # print(paste(iti," : ", sUTC,", dt=", dtc, " , dtmax=", dtmax ,",  PF=", pWB[1], ", Pthr=", pWB[2], ", C=", pWB[3], ", Dthr=", pWB[4], ", D in air=", pWB[5], ", EV=", pWB[6], ", ET=", pWB[7]  ))
          # pWF <- round(as.numeric(c( WFa$PF[itt], WFa$Pthroughf[itt], WFa$Pinterc[itt], WFa$Dthroughf[itt], WFa$Drainage[itt], WFa$EV[itt], WFa$ET[itt]  ))*dtc, 4)
          # print(paste(ito, iti, itt," : ", sUTC,", dt=", dtc, ", C=", round(WFa$C_3D[itt],4), ", Pint-EV=", pWF[3]-pWF[6] ,",  PF=", pWF[1], ", Pthr=", pWF[2], ", Pint=", pWF[3], ", Dthr=", pWF[4], ", D in air=", pWF[5], ", EV=", pWF[6]  ))
          # print(paste(ito, iti, itt," : ", sUTC,", dt=", dtc, ", C=", round(WFa$C_3D[itt],4),",  PF=", pWF[1], ", Pthr=", pWF[2], ", Pint=", pWF[3], ", Dthr=", pWF[4], ", D in air=", pWF[5], ", EV=", pWF[6], ", ET=", pWF[7]  ))
          # print(paste(ito, iti, itt," : ", sUTC,", dt=", dtc,",  PF=", pWF[1], ", C=", pWF[3], ", D in air=", pWF[5], " , dtmax=", dtmax   ))
        }
        
        addNrows <- 0
      }  # < < < INNER LOOP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ----
#cat("\n")
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if (dt0r > 0) stop("sub time steps didn't fill outer time step")
      # if ((sUTCo+dt0) != metPP$UTC[ito]) stop("sub time steps didn't fill outer time step")
      
      # print(paste("toti[6]/dt0",round(toti[6]/dt0,6), dt0 ))
  
      WF.i <- as.list(c(as.numeric(metPP$UTC[ito]), EvNo*EvOn, dt0, C.mean, toti/dt0))   # mean fluxes
      set(WF, i=as.integer(ito), j=headim, value=WF.i)  # WF[ito]
      WB.i <- as.list(c(as.numeric(metPP$UTC[ito]), EvNo*EvOn, dt0, C.mean, toti))
      set(WB, i=as.integer(ito), j=headim, value=WB.i)  # WB[ito]
      
      #* Individual Output ####
      if (!anyNA(output.mean.usr)){
        for(usr.vars in output.mean.usr){
          # cat(usr.vars, " = ", mean(get(usr.vars)), "\n")
          vart <- get(usr.vars)
          if ( !(usr.vars  %in% c("L_3D")) ) { # if it is not a single value calculate mean
            vart <- vart[-c(1,nz),,]
            if ( usr.vars %in% c("rH_3D", "rLE_3D", "rw_3D", "rET_3D", "rc_3D", "rb_3D", "rab")){
              vart <- round(1/mean(1/vart, na.rm=T)) # especially for resistances
            } else {
              vart <- mean(vart, na.rm=T) 
            }
          }
          set(WB, i=as.integer(ito), j=usr.vars, vart)
        }
      }
      
      #* Event Statistic (not predefined events) ##########################################################
      
      if (!EvOn & EvOn.prev){    # EvOn is TRUE if it has rained in the timestep, EvOn.prev is T if it has rained in the  previous time step
        i.Ev0 <- i.Ev0+1L
        i.ev.WB <- which(WB$EvNo == EvNo)
        ptime0 <- as.POSIXct(range(WB[i.ev.WB, UTC], na.rm = T ), origin="1970-01-01", tz="GMT")
        ptime <- strftime(c(ptime0[1], ptime0[2]+dtc), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
        WBev0$UTCstart[i.Ev0] <- ptime[1]
        WBev0$UTCend[i.Ev0]   <- ptime[2] 
        WBev0$EvNo[i.Ev0] <- EvNo
        WBev0$dt[i.Ev0]   <- (ptime0[2]-ptime0[1])
        WBev0$PF[i.Ev0]   <- round(sum(WB[i.ev.WB, PF]), 3)
        WBev0$dC[i.Ev0]   <- round(sum(WB[i.ev.WB, dC]), 3)
        WBev0$Pintercept[i.Ev0] <- round(sum(WB[i.ev.WB, Pintercept]), 3)  
        WBev0$Pthroughf[i.Ev0]  <- round(sum(WB[i.ev.WB, Pthroughf]), 3)
        WBev0$Dthroughf[i.Ev0]  <- round(sum(WB[i.ev.WB, Dthroughf]), 3)
        # WBev0$Troughfall[i.Ev0] <- round(sum(WB[i.ev.WB, Pthroughf])+sum(WB[i.ev.WB, Dthroughf]),2)
        WBev0$EV[i.Ev0]   <- round(sum(WB[i.ev.WB, EV]), 3)
        WBev0$ET[i.Ev0]   <- round(sum(WB[i.ev.WB, ET]), 3)
        WBev0$Rn[i.Ev0]   <- round(sum(WB[i.ev.WB, Rn]), 3)
        WBev0$sH[i.Ev0]   <- round(sum(WB[i.ev.WB, sH]), 3)
        if (!anyNA(output.mean.usr)){
          for(usr.vars in output.mean.usr){
            if ( usr.vars %in% c("rH_3D", "rLE_3D", "rw_3D", "rET_3D", "rc_3D", "rb_3D", "rab")){
              vart <- round(WB[i.ev.WB, mean(1/eval(as.name(usr.vars)))], 2) # especially for resistances
            } else {
              vart <- round(WB[i.ev.WB, mean(eval(as.name(usr.vars)))], 2) 
            }
            set(WBev0, i=i.Ev0, j=usr.vars, vart)
          }
        }
        # 2D fields ####
        # plots ####
        
        
        
        
        
      }
      EvOn.prev <- EvOn
      
      
      
      
      #ito <- ito+1
    }   # < < TIME LOOP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ---- 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  # Check Balance ##############################################################
    # source(file.path(path_sub,"Balance.R"))
    WBc <- WB[ito]$PF - WB[ito]$Pthroughf - WB[ito]$dC - WB[ito]$EV - WB[ito]$Dthroughf-sum(Drainage)*dtc/(nx*ny)
    print(paste("WB: ",WBc))
    journal <- c(journal, "  " )
    journal <- c(journal, "# Water Balance  ")
    journal <- c(journal, "Check as mean value in mm = l/m²  ")
    journal <- c(journal, paste("WB = PF - Pthroughf - dC - EV - Dthroughf - sum(Drainage) dtc / (nx ny)  "))
    journal <- c(journal, paste("WB = ", WBc, " mm  "))
    
    print( "EV.max, Drainage.max, C_3D.max, Pinter.max for graphics")
    print(paste( EV.max, Drainage.max, C_3D.max, Pinter.max))
  
    # Event Statistic ##########################################################
    ptime <- as.POSIXct(range(WB$UTC, na.rm = T ), origin="1970-01-01", tz="GMT")
    ptime <- strftime(c(ptime[1], ptime[2]+dtc), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    WBev$UTCstart[i.Ev] <- ptime[1]
    WBev$UTCend[i.Ev]   <- ptime[2] 
    WBev$EvNo[i.Ev] <- EvNo
    WBev$PF[i.Ev]   <- round(sum(WB$PF),2)
    WBev$dC[i.Ev]   <- round(sum(WB$dC),2)
    WBev$Pintercept[i.Ev] <- round(sum(WB$Pintercept),2)  
    WBev$Pthroughf[i.Ev]  <- round(sum(WB$Pthroughf),2)
    WBev$Dthroughf[i.Ev]  <- round(sum(WB$Dthroughf),2)
    # WBev$Troughfall[i.Ev] <- round(sum(WB$Pthroughf)+sum(WB$Dthroughf),2)
    WBev$EV[i.Ev]   <- round(sum(WB$EV),2)
    WBev$ET[i.Ev]   <- round(sum(WB$ET),2)
    WBev$Rn[i.Ev]   <- round(sum(WB$Rn),2)
    WBev$sH[i.Ev]   <- round(sum(WB$sH),2)
    if (!anyNA(output.mean.usr)){
      for(usr.vars in output.mean.usr){
        if ( usr.vars %in% c("rH_3D", "rLE_3D", "rw_3D", "rET_3D", "rc_3D", "rb_3D", "rab")){
          vart <- round(WB[, mean(1/eval(as.name(usr.vars)))],2) # especially for resistances
        } else {
          vart <- round(WB[, mean(eval(as.name(usr.vars)))], 2) 
        }
        set(WBev, i=i.Ev, j=usr.vars, vart)
      }
    }
    

  # Save Totals ################################################################
    WF <- WF[,UTC := as.POSIXct(UTC, origin="1970-01-01", tz="GMT")  ]  # time is still a numeric 
    WFa <- WFa[1:itt]; WFa <- WFa[ ,UTC := as.POSIXct(UTC, origin="1970-01-01", tz="GMT")  ]
    WB <- WB[,UTC := as.POSIXct(UTC, origin="1970-01-01", tz="GMT")  ]
    WBa <- WBa[1:itt]; WBa <- WBa[ ,UTC := as.POSIXct(UTC, origin="1970-01-01", tz="GMT")  ]
    fwrite(WB,  file.path(path_output_act, paste0(act, EvNo_str, "_WB_",Tstamp,".csv") ))
    fwrite(WBa, file.path(path_output_act, paste0(act, EvNo_str,"_WBa_",Tstamp,".csv" )))
    fwrite(WBev0[1:i.Ev0], file.path(path_output_act, paste0(act, EvNo_str,"_WBev0_",Tstamp,".csv" )))
    if (C_lmean.S)   fwrite(C_lmean, file.path(path_output_act, paste0(act, EvNo_str,"_C_lmean_",Tstamp,".csv" )))
    if (ETR_lmean.S) fwrite(ETR_lmean, file.path(path_output_act, paste0(act, EvNo_str,"_ETR_lmean_",Tstamp,".csv") ))
    if (ET_lmean.S)  fwrite(ET_lmean, file.path(path_output_act, paste0(act, EvNo_str,"_ET_lmean_",Tstamp,".csv" )))
    if (EV_lmean.S)  fwrite(EV_lmean, file.path(path_output_act, paste0(act, EvNo_str,"_EV_lmean_",Tstamp,".csv" ))) 
  
    
  # Save Plots #################################################################
    if (plotMeanTS) { 
      # add event sums
      dev.set(devW); par(mfcol=c(1,1), fig=c(0, 1, 0, 1))
      plot_event_sum(WB, ds=devW)    
      if (plotlife) { # make a copy
        dev.copy(png, file.path(path_output_act, paste0(act, EvNo_str,"_", Tstamp,".png")), width=figsize, height=figsize/2, pointsize = 12 )
        par(op)
      }
      dev.off()
    }
  

  # write Journal ###############################################################
    time.calc <- round(Sys.time()-start_time,2)
    journal <- c(journal, "  " )
    journal <- c(journal, "# Water and Energy balance  ")
    journal <- c(journal, "## Mean sums")
    journal <- c(journal, "Output of mean sums in mm (egual to l/m²) per time step  ")
    journal <- c(journal, paste0("Output path: ", path_output_act,"/",act , "  ") ) 
    journal <- c(journal, paste0(act, EvNo_str,"_WB_",Tstamp,".csv in resolution of the input data  " ) )
    journal <- c(journal, paste0(act, EvNo_str,"_WBa_",Tstamp,".csv all calculated steps  " ) )
    journal <- c(journal, paste0("EvNo = ", EvNo , "  ") )
    journal <- c(journal, paste0("UTCstart = ", ptime[1] , "  ") )
    journal <- c(journal, paste0("UTCend = ", ptime[2]  , "  ") )
    journal <- c(journal, paste0("PF = ", round(sum(WB$PF),2) , "  ") )
    journal <- c(journal, paste0("dC = ", round(sum(WB$dC),2) , "  ") )
    journal <- c(journal, paste0("Pintercept = ", round(sum(WB$Pintercept),2) , "  ") )  
    journal <- c(journal, paste0("Pthroughf = ", round(sum(WB$Pthroughf),2) , "  ") )
    journal <- c(journal, paste0("Dthroughf = ", round(sum(WB$Dthroughf),2) , "  ") )
    journal <- c(journal, paste0("Troughfall = ", round(sum(WB$Pthroughf)+sum(WB$Dthroughf),2) , "  ") )
    journal <- c(journal, paste0("EV = ", round(sum(WB$EV),2) , "  ") )
    journal <- c(journal, paste0("ET = ", round(sum(WB$ET),2) , "  ") )
    journal <- c(journal, paste0("Rn = ", round(sum(WB$Rn),2) , "  ") )
    journal <- c(journal, paste0("sH = ", round(sum(WB$sH),2) , "  ") )
    journal <- c(journal, "  " )
    journal <- c(journal, "## Maximal values (for plotting)  " )
    journal <- c(journal, "Please note, that are maximal values of single grid cells." )
    journal <- c(journal, "They depend on the grid resolution and can be much bigger than the spatial averages." )
    journal <- c(journal, paste("EV.max =", signif(EV.max, 3),  " mm/s  ") )
    journal <- c(journal, paste("Drainage.max =", signif(Drainage.max, 3),  " mm/s ") )
    journal <- c(journal, paste("C_3D.max =", signif(C_3D.max, 3),  " mm  ") )
    journal <- c(journal, paste("Pinter.max =", signif(Pinter.max, 3),  " mm/s  ") )
    journal <- c(journal, "  " )

    # if (!is.na(sum(EvNo.selected))) {
    #   journal <- c(journal, "# Output  ")
    #   journal <- c(journal, "Output of mean sums in mm (egual to l/m²) per time step  ")
    #   journal <- c(journal, paste0("Output path: ", path_output_act,"/",act , "  ") ) 
    #   journal <- c(journal, paste0(act, EvNo_str,"_WB_",Tstamp,".csv in resolution of the input data" , "  ") )
    #   journal <- c(journal, paste0(act, EvNo_str,"_WBa_",Tstamp,".csv all calculated steps" , "  ") )
    #   journal <- c(journal, "  " )
    #   
    #   journal <- c(journal, "# Statistics")
    #   journal <- c(journal, paste0("system time = ",  Sys.time() ))
    #   time.calc.ev.start <- time.calc
    #   journal <- c(journal, paste0("used time per event = ",  time.calc-time.calc.ev.start," ", attributes(time.calc-time.calc.ev.start)$unit ))
    #   journal <- c(journal, paste0("~~~~~~~~~ Event ",  i.Ev , " is finished. ~~~~~~~~~~~~~~~~~~"))
    #   journal <- c(journal, "\n" )
    # }    
    # system(paste('"C:/Program Files/Typora/Typora.exe"', 'd:/b_Programming/0_R_lib/y_CanWat/Output/Case02_Tree01_3D_20L_CanWat_journal_20211123_1120.md'), intern=FALSE, wait = FALSE )
    # system(paste('"C:/Program Files/Typora/Typora.exe"', 'd:/b_Programming/0_R_lib/y_CanWat/Output/Case02_Tree01_3D_20L_CanWat_journal_20211123_1120.md'), intern=FALSE, wait = FALSE )
    # system(paste('"C:/Program Files/Typora/Typora.exe"', 'd:/b_Programming/0_R_lib/y_CanWat/Output/Case02_Tree01_3D_20L_CanWat_journal_20211123_1120.md'), intern=FALSE, wait = FALSE )  
  #  system(paste('"C:/Program Files/Typora/Typora.exe"'), intern=FALSE, wait = FALSE )  
    # WF.l <- WF[,UTC := as.POSIXct(UTC, origin="1970-01-01")  ]
    # WFa.l <- WFa[1:itt]; WFa <- WFa[ ,UTC := as.POSIXct(UTC, origin="1970-01-01")  ]
    # stop("Stop after output")
  
    print(paste0("~~~~~~~~~ Event ",  EvNo.selected[i.Ev] , " is finished. ~~~~~~~~~~~~~~~~~~"))

  } # < Event loop <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ####  
  
  
  # Save Journal ###############################################################
  time.calc <- round(Sys.time()-start_time,2)
  journal <- c(journal, "  " )
  journal <- c(journal, "# Performance Metrics  ")
  journal <- c(journal, paste0("start time = ",  starttime , "  "))
  journal <- c(journal, paste0("end time = ",  Sys.time() , "  "))
  journal <- c(journal, paste0("used time = ",  time.calc," ", attributes(time.calc)$unit , "  "))
  
  write(journal, file.path(path_output_act, paste0(act, "_CanWat_journal_",Tstamp,".md" )))
  print(paste0("the LOG-file is saved in ==> ", path_output_act, paste0("/", act, "_CanWat_journal_",Tstamp,".md") ))
  
    
# Save Event Statisics ####  
  if (!is.na(sum(EvNo.selected))) fwrite(WBev,  file.path(path_output_act, paste0(act, "_WBev_",Tstamp,".csv" )))
  
  
  cat("               |\n               #\n              ###\n            #######\n         #############\n      ###################\n   #########################\n###### End of Simulation ######\n ############################\n    ######################\n          ##########\n \n")

  

  # END OF PROGRAM #############################################################
  # ~ ##########################################################################  
  # ~ ##########################################################################  
  # ~ ##########################################################################  
  # ~ ##########################################################################  
  # for checks in post processing
  if (F){
    chk.field <- function(tst, main="tst", meas=NA, measH=NA, dlim = NA , ixreft=2, iyreft=2, devW = 4){
      library(rasterImage) 
      library(zeallot) 
      if (!exists("devW")){
        devW <- dev.list()[match("windows",names(dev.list()))]
        if (is.na(devW)) {x11(); devW <- dev.cur()}
      }
      dev.set(devW)
      c(nztst, nytst, nxtst) %<-% dim(tst)
      if (!exists("tst")) print("no tst") else  print( "tst is da")
      tst.s <- summary(as.numeric(tst)); print(tst.s)
      if (any(is.na(dlim))) dlim = c(0,ceiling(tst.s[6]))
      par(mfcol= c(1,3))
      rasterImage2(z=t(tst[,,ixreft]), zlim = dlim,cex.axis=1.5,z.cex=1.5, cex.lab=1.5, main=paste(main,", section along y at x=",ixreft, ", facing West"), xlab="y", ylab="z"); abline(v=iyreft)
      rasterImage2(z=t(tst[,iyreft,]), zlim = dlim,cex.axis=1.5,z.cex=1.5, cex.lab=1.5, main=paste(main,", section along x at y=",iyreft, ", facing North"), xlab="x", ylab="z"); abline(v=ixreft)
      plot(tst[,iyreft,ixreft],1:nztst, xlim= dlim,cex.axis=1.5, cex.lab=1.5, main=paste(main,"-profiles (red~tower)"))
      for (i in 1:nx)  for (j in 1:ny) lines(tst[,j,i],1:nztst)
      lines(tst[,iyreft,ixreft],1:nztst, col=2, lwd=3); abline(v=0, col=3)
      if (!is.na(meas)) points(meas,measH, pch=16, cex=3, col=2)
      par(mfcol= c(1,1))
    }
    
    ixreft <- ixref; iyreft <- iyref # position of the tower
    # stop("bis hierher gings gud")
  }


  
#  dev.off()