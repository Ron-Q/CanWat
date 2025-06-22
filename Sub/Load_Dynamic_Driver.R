#+
# NAME*: Load_Dynamic_Driver.R
# PURPOSE: load meteorological variables from Wildacker
# CALLING: source(file.path(path_sub,"Load_Meteo_Wil_01.R")
# INPUT:
  # path_DriverDyn, fnam.MetData    # path and file name of the Dynamic Driver
  #    Windgeschwindigkeit (u4200), Temperatur (T200...T4025), Dampfdruck (DD200...DD4025),
  #    Netto- und Globalstrahlung, fuehlbarer und latenter Waermestrom
  # it1, it2 : selected interval
  # EvNo.selected   # ID-number of the events within the interval it1, it2
# OUTPUT:
#    pressure, u4200, WVEK, T200, T3300, T3700, T4025, DD200, DD3300, DD3700, DD4025, Rn, Rg, WVEK, H, LE
# REVISION HISTORY*:
#   2014          : Diloma Thesis of Martin Kunath (time consuming laborious job in every aspect)
#   2019-08-07, RQ: completely rewritten, now selection of the data to the interval which is used
#       use of data.tables and simple numerics instead of data frames 
#   2019-11-21, RQ: 
#       separate arrays for each meteorological variable
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (silent < 2) cat("---- load Dynamic Driver \n")

  # journal <- c(journal, paste(   "  "))

  # Read the data ####
  dmet0 <- fread(file.path(path_DriverDyn,fnam.MetData),header=TRUE)
  # dati <- unlist(dmet0[,..namTime]);   attributes(dati) <- NULL
  # dmet0[, UTC := as.POSIXct(dati, origin="1970-01-01 00:00", tz = "GMT") + ToffUTC ]
  dmet0[, UTC := as.POSIXct(get(namTime), origin="1970-01-01 00:00", format = Tformat, tz = "GMT") + ToffUTC ]
  ndat <- dim(dmet0)[1]

  if (diff(range(diff(dmet0$UTC))) != 0){
    stop("there are irregular time steps in the timeseries of the dynamic input data")
  } else {
    dt_met.dtct <- mean(range(diff(as.numeric(dmet0$UTC))))
    if (dt_met != dt_met.dtct){
      print(paste("### The given time step is dt_met =", dt_met, ", but the detected time step is", dt_met.dtct, ". We will use this time step! ###"))
      journal <- c(journal, paste(   "### The given time step is dt_met =", dt_met, ", but the detected time step is", dt_met.dtct, ". We will use this time step! ###"))
      dt_met <- dt_met.dtct
    }
  }
  
  # select interval ####
  # the requested interval is usally it1:it2  
  if(!exists("it1")) it1 <- 1
  if (it1 > ndat) stop("the start index it1 is bigger than the number of available time steps")
  if(!exists("it2")) it2 <- ndat
  if (it2 > ndat) {
    it2 <- ndat
    cat("the end index it2 is bigger than the number of available time steps, the number of time steps is reduced to: it2=", ndat, "\n")
  }  
  if (!is.na(sum(EvNo.selected))){it1 <- 1;  it2 <- ndat}   # take all if event selection is active
  ntu<- it2-it1+1          # adjust the number of the timesteps ntu according to the available data
  dmet1 <- dmet0[it1:it2]
  UTCna <- which(is.na(dmet1$UTC))
  if (length(UTCna > 0)) stop("Dynamic Driver: UTC is NA")
  (ts.est <- as.numeric(names(which.max(table(diff(as.numeric(dmet1$UTC)))))))    # s, length of the time steps
  (datr2 <- range(dmet1$UTC))
  dt_mete <- diff(as.numeric(datr2))/(ntu-1)    # time step in Seconds
  if (dt_met != dt_mete) {
    print( "!!! meteo data contain gabs or a time error error occured !!!")
    if (gapf) {gapf <- F; print( "==== gap filling is switched of ====") # stop("time gaps")
    }
  }
  

  # Read event numbers ####  
  if (!is.na(sum(EvNo.selected))){
    if (!any(EvNo.cnam == "")){
      snam <- c(EvNo.namTime, EvNo.cnam)
      if (EvNo.file != fnam.MetData) {
        Ev.in0 <- fread(file.path(EvNo.path, EvNo.file),header=TRUE)
        Ev.in0[, UTC := as.POSIXct(get(EvNo.namTime), origin="1970-01-01 00:00", format = EvNo.Tformat, tz = "GMT") + EvNo.ToffUTC ]
        Ev.in1 <- Ev.in0[((Ev.in0$UTC >= datr2[1]) & (Ev.in0$UTC <= datr2[2])),]
        UTCna <- which(is.na(Ev.in1$UTC))
        if (length(UTCna > 0)) stop("Event table UTC contains NA")
        met2Ev <- Ev.in1[, ..snam]; setnames(met2Ev, EvNo.cnam, "EvNo")
        dmet2 <- merge(dmet1, met2Ev, by="UTC", all.x = T )
        met2Ev <- dmet2[ , .(UTC, EvNo)]
      }
    } else {stop("Please, give a column name for the event number") }
  } else dmet2 <- dmet1
  
  
  # Select Variables ####
  if (any(!is.na(PP_namo))){snam <- c("UTC", PP_namo); met2PP <- dmet2[, ..snam]; setnames(met2PP, PP_namo, PP_nam)  # Precipitation, totals
                } else {stop("precipitation input is not defined") }
  if (any(!is.na(WS_namo))){snam <- c("UTC", WS_namo); met2WS <- dmet2[, ..snam]; setnames(met2WS, WS_namo, WS_nam)  # Wind speed and Direktion
                } else {met2WS <- data.table(UTC = dmet2$UTC, u00=0.5)
                        journal <- c(journal, "no wind data! -> set on u00=0.5 m/s  ")}
  if (any(!is.na(Ta_namo))){snam <- c("UTC", Ta_namo); met2Ta <- dmet2[, ..snam]; setnames(met2Ta, Ta_namo, Ta_nam)  # Temperature
                } else {met2WS <- data.table(UTC = dmet2$UTC, T00=15)
                        journal <- c(journal, "no temperature data! -> set on T00=15°C  ")}
  if (any(!is.na(DD_namo))){snam <- c("UTC", DD_namo); met2DD <- dmet2[, ..snam]; setnames(met2DD, DD_namo, DD_nam)  # Water vapour pressure
                } else {met2WS <- data.table(UTC = dmet2$UTC, DD00=15)
                        journal <- c(journal, "no vapor pressure data! -> set on DD=15 hPa  ")}
  if (any(!is.na(Rn_namo))){snam <- c("UTC", Rn_namo); met2Rn <- dmet2[, ..snam]; setnames(met2Rn, Rn_namo, Rn_nam)  # net radiation
                } else {met2WS <- data.table(UTC = dmet2$UTC, Rn=150)
                        journal <- c(journal, "no net radiation data! -> set on Rn=150 W/m²  ")}
  if (any(!is.na(Rg_namo))){snam <- c("UTC", Rg_namo); met2Rg <- dmet2[, ..snam]; setnames(met2Rg, Rg_namo, Rg_nam)  # global radiation
                } else {met2WS <- data.table(UTC = dmet2$UTC, Rg=200)
                        journal <- c(journal, "no shortwave radiation data! -> set on Rg=200 W/m²  ")}
  if (any(!is.na(Pa_namo))) { snam <- c("UTC", Pa_namo); met2Pa <- dmet2[, ..snam]; setnames(met2Pa, Pa_namo, Pa_nam)  # air pressure
                } else {met2Pa <- data.table(UTC = dmet2$UTC, Pa = 1012.00)
                        journal <- c(journal, "no pressure data! -> set on Pa = 1012 hPa  ")}

    
  # gapfilling ####
  if (gapf){
    ## events ####
    if (exists("EvNo.in")) met2Ev$EvNo[is.na(EvNo.in$EvNo)] <- 0

    ## Precipitation, totals ####
    # met2PP,  PP_nam[1]
    for (i.nam in PP_nam) met2PP[[i.nam]][is.na(met2PP[[i.nam]])] <- 0

    ## Wind ####
    # met2WS, WS_nam
    for (i.nam in WS_nam) {
      if ( !any(!is.na(met2WS[[i.nam]])) ) met2WS[[i.nam]] <- 1   # if no data then 1 m/s
      met2WS[[i.nam]] <- approx(met2WS$UTC, met2WS[[i.nam]], met2WS$UTC, rule = 2)$y
    }
    
    ## Air Temperature ####
    # met2Ta, Ta_nam
    for (i.nam in Ta_nam) {
      if ( !any(!is.na(met2Ta[[i.nam]])) ) met2Ta[[i.nam]] <- 10   # if no data then 10 °C
      met2Ta[[i.nam]] <- approx(met2Ta$UTC, met2Ta[[i.nam]], met2Ta$UTC, rule = 2)$y
    }
  
    ## vapor pressure in air ####
    # met2DD, DD_nam
    for (i.nam in DD_nam) {
      if ( !any(!is.na(met2DD[[i.nam]])) ) met2DD[[i.nam]] <- 15   # if no data then 15 hPa
      met2DD[[i.nam]] <- approx(met2DD$UTC, met2DD[[i.nam]], met2DD$UTC, rule = 2)$y
    }
    
    ## Net Radiation ####
    #  met2Rn, Rn_nam
    for (i.nam in Rn_nam) {
      if ( !any(!is.na(met2Rn[[i.nam]])) ) met2Rn[[i.nam]] <- 50   # if no data then 50 W/m2
      met2Rn[[i.nam]] <- approx(met2Rn$UTC, met2Rn[[i.nam]], met2Rn$UTC, rule = 2)$y
    }
    
    ## shortwave radiation ####
    # met2Rg, Rg_nam
    for (i.nam in Rg_nam) {
      if ( !any(!is.na(met2Rg[[i.nam]])) ) met2Rg[[i.nam]] <- 50   # if no data then 50 W/m2
      met2Rg[[i.nam]] <- approx(met2Rg$UTC, met2Rg[[i.nam]], met2Rg$UTC, rule = 2)$y
    }
    
    ## Air Pressure #### 
    # met2Pa, Pa_nam
    for (i.nam in Pa_nam) {
      if ( !any(!is.na(met2Pa[[i.nam]])) ) met2Pa[[i.nam]] <- 1013   # if no data then hPa
      met2Pa[[i.nam]] <- approx(met2Pa$UTC, met2Pa[[i.nam]], met2Pa$UTC, rule = 2)$y
    }
    
    ## another possible method #### 
    # still to test
    # use of the runMean, runSD and a randomn number to calculate fill values
    # see despike_filter.r
    # dtl <- rbind(dt[nf:1],dt,dt[nr:(nr-(nf-1))])                             # Verlängerung der Zeitreihe um gespiegelte Enden
    # dtM  <- dtl[,lapply(.SD,runMedian,nf)]; dfM <- dtM[(1.5*nf+1):(nr+1.5*nf)]
    # dtSD <- dtl[,lapply(.SD,runSD,nf)]; dfSD <- dtSD[(1.5*nf+1):(nr+1.5*nf)]
    # ina <- which(is.na(dt$lat) | is.na(dt$lon))
    # for(i in 1:nc) dt[[ndt[i]]][ina] <- rnorm(length(ina), mean=dfM[[ndt[i]]][ina], sd=dfSD[[ndt[i]]][ina] )
    
    
    ## ## Test of a advanced method ####    
    # ==> no better results as linear interpolation
    # library(Rssa)
    # library(lattice)
    # library(latticeExtra)
    # library(plyr)
    # library(fma)
    # i.nam  <-  Rg_nam[2]
    # Fr <- metRg[[i.nam]] 
    # loc <- c(11:17, 61:67, 71:77, 101:107)
    # Fr[loc] <- NA;
    # aa <- approx(metRg$UTC, Fr, metRg$UTC)$y
    # sr <- ssa(Fr, L = 200)
    # frm <- mean(Fr, na.rm=T)
    # gr <- gapfill(sr
    #               , groups = list(c(1:6))    # 	list, the grouping of eigentriples to be used in the forecast
    #               , method = "simultaneous" # "sequential" means to filling by a recurrent forecast from complete parts; "simultaneous" tries to build a projections onto the signal subspace. 
    #               , base = "original"       # series used as a 'seed' for gapfilling: original or reconstructed according to the value of groups argument
    #               )
    # igr <- igapfill(sr
    #                 , groups = list(c(1:6))
    #                 , fill = frm            # initial values for missed entries, 
    #                 , base = "original"
    #                 , maxiter = 10)
    # pr <- (min(loc)-10):(max(loc)+10)
    # # pr <- 1:1000
    # plot(Fr[pr], type="l", col=2, lwd=8)
    # lines(metRg[[i.nam]][pr], lwd=5, col="gray")
    # # lines(gr[pr], col=1, lwd=2)
    # lines(igr[pr], col=4, lwd=2)
    # lines(aa[pr], col=3, lwd=2)
    
        
  }

  
  # add precipitation intensities ####
  # metPP[, ":=" (PFi = PF/dt_mete, PBi = PB/dt_mete) ]           
  met2PP$PFi  <-  met2PP$PF/dt_mete
  met2PP$PBi  <-  met2PP$PB/dt_mete
  
  
  # # values at the top of the canopy ####
  # met2WS.top <- met2WS[[WS_nam[dim(met2WS)[2]-1] ]]
  # met2Ta.top <- met2Ta[[Ta_nam[dim(met2Ta)[2]-1] ]]
  # met2DD.top <- met2DD[[DD_nam[dim(met2DD)[2]-1] ]]
  # met2Rn.top <- met2Rn[[Rn_nam[dim(met2Rn)[2]-1] ]]
  # met2Rg.top <- met2Rg[[Rg_nam[dim(met2Rg)[2]-1] ]]
  # met2Pa.top <- met2Pa[[Pa_nam[dim(met2Pa)[2]-1] ]]
  
  
  journal <- c(journal
               , paste0("dynamic driver (Events):   ", fnam.MetData, "  ")
               , paste("started from folder: ",path_DriverDyn , "  ")
               , paste0("data are available", "  ")
               , paste0("from: ", min(dmet0$UTC), ", to: ",  max(dmet0$UTC) , "  ")
               , paste0("number of lines ", ndat ,", detected timestep length: ", ts.est, "  ")
               , paste0("selected interval", "  ")
               , paste0("time: ", it1," .. ", it2," -> ", (it2-it1)*dt_met/3600 , " hours", "  ") 
  )
  if (dt_met != dt_mete) { journal <- c(journal
                 , paste0( "!!! ", fnam.MetData, " contains gabs or a time error error occured !!!")
                 , paste0( "===> gap filling is switched of")
  )}
  

  rm(dmet0,dmet1,dmet2)
  
