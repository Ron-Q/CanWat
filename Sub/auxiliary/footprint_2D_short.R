#+
# NAME*: footprint_2D.R
# PURPOSE*: 
  # script for auxiliary means used in Fischer et al. (2025) https://editor.copernicus.org/EGUsphere/ms_records/egusphere-2025-2118
  # Calculates 2D footprints for rain events, according to Kljun et al. 2015
  # => wind direction and times are checked, 
  # => take care some of the dates are read as CET, 
  #    but CanWat result is UTC, i.e. date of event numbers is 1 h before the other data
  #    here we work with UTC
# RELEVANCY*: 
# CALLING SEQ.:
# CALLED BY:
# INPUTS*:
  # Output of Eddypro
    # umean    in m/s, Mean wind speed at zm
    # MOL      in m,   Obukhov length [m]
    # sigmav   in m/s, standard deviation of lateral velocity fluctuations
    # ustar    in m/s, friction velocity [ms-1]
    # wind_dir in °
  # fname.cwo: table with events, should contain Event number with start and end time 
  # parameter
# OUTPUT*: data mask 
# RESTRICTIONS: 
# VARIABLES:
# SUBROUTINES: see http://footprint.kljun.net/index.php
  # "calc_footprint_FFP.R"   
  # "calc_footprint_FFP_climatology.R"
# PACKAGES:
  # install.packages("BiocManager") 
  # BiocManager::install("EBImage")
  # library(EBImage)
  pacman::p_load(ncdf4, dplyr, data.table, sf, terra, raster, Thermimage, png, leaflet
                 , magrittr, colorRamps, RColorBrewer, rasterImage, readxl
                 , htmlwidgets, mapview, webshot,  utils, readr, MASS, lubridate
                 )
# REFERENCE:
    # https://www.licor.com/env/support/EddyPro/topics/estimating-flux-footprint.html
    # https://www.licor.com/support/EddyPro/topics/output-files-full-output.html
    # https://footprint.kljun.net/download_2.php
    # Kljun, N., Calanca, P., Rotach, M.W., Schmid, H.P., 2015. A simple two-dimensional parameterisation for Flux Footprint Prediction (FFP). Geoscientific Model Development 8, 3695–3713. https://doi.org/10.5194/gmd-8-3695-2015
    # ... Kormann, R., Meixner, F., 2001. An Analytical Footprint Model For Non-Neutral Stratification. Boundary-Layer Meteorology 99, 207–224. https://doi.org/10.1023/A:1018991015119
# REVISION HISTORY*:
  # 2025-03-02 RQueck (TUD) : created
  VDate <- "2025-03-02"
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INPUT ############################################################
## Control variables ##############################
  z_d <- 20.5                  # m,   displacement height
  z_m <- 42-z_d                # m,   Measurement height above displacement height (i.e. z-d)
  z_0 <- 1.7                  # m,   Roughness length [m] - enter [NaN] if not known

  new.start <- T
  re.start <- F
  event1 <- 1              # start with event1 

## Input file names and headers ####
  fname.eddyproFull <- "eddypro_tha_full_output_2008-2010.txt"
  header.epFull <- c("filename", "date", "time", "DOY", "daytime", "file_records", "used_records", "Tau", "qc_Tau", "rand_err_Tau", "H", "qc_H"
                     , "rand_err_H", "LE", "qc_LE", "rand_err_LE", "co2_flux", "qc_co2_flux", "rand_err_co2_flux", "h2o_flux", "qc_h2o_flux"
                     , "rand_err_h2o_flux", "ch4_flux", "qc_ch4_flux", "rand_err_ch4_flux", "none_flux", "qc_none_flux", "rand_err_none_flux"
                     , "H_strg", "LE_strg", "co2_strg", "h2o_strg", "ch4_strg", "none_strg", "co2_v-adv", "h2o_v-adv", "ch4_v-adv", "none_v-adv"
                     , "co2_molar_density", "co2_mole_fraction", "co2_mixing_ratio", "co2_time_lag", "co2_def_timelag"
                     , "h2o_molar_density", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_time_lag", "h2o_def_timelag"
                     , "ch4_molar_density", "ch4_mole_fraction", "ch4_mixing_ratio", "ch4_time_lag", "ch4_def_timelag"
                     , "none_molar_density", "none_mole_fraction", "none_mixing_ratio", "none_time_lag", "none_def_timelag"
                     , "sonic_temperature", "air_temperature", "air_pressure", "air_density", "air_heat_capacity", "air_molar_volume"
                     , "ET", "water_vapor_density", "e", "es", "specific_humidity", "RH", "VPD", "Tdew"
                     , "u_unrot", "v_unrot", "w_unrot", "u_rot", "v_rot", "w_rot", "wind_speed", "max_wind_speed", "wind_dir", "yaw", "pitch", "roll"
                     , "ustar", "TKE", "L", "(z-d)/L", "bowen_ratio", "T*", "model", "x_peak", "x_offset", "x_10", "x_30", "x_50", "x_70", "x_90"
                     , "un_Tau", "Tau_scf", "un_H", "H_scf", "un_LE", "LE_scf", "un_co2_flux", "co2_scf", "un_h2o_flux", "h2o_scf"
                     , "un_ch4_flux", "ch4_scf", "un_none_flux", "un_none_scf", "spikes_hf", "amplitude_resolution_hf", "drop_out_hf", "absolute_limits_hf"
                     , "skewness_kurtosis_hf", "skewness_kurtosis_sf", "discontinuities_hf", "discontinuities_sf", "timelag_hf", "timelag_sf", "attack_angle_hf", "non_steady_wind_hf"
                     , "u_spikes", "v_spikes", "w_spikes", "ts_spikes", "co2_spikes", "h2o_spikes", "ch4_spikes", "none_spikes", "head_detect_LI-7200"
                     , "t_out_LI-7200", "t_in_LI-7200", "aux_in_LI-7200", "delta_p_LI-7200", "chopper_LI-7200", "detector_LI-7200", "pll_LI-7200", "sync_LI-7200"
                     , "chopper_LI-7500", "detector_LI-7500", "pll_LI-7500", "sync_LI-7500", "not_ready_LI-7700", "no_signal_LI-7700", "re_unlocked_LI-7700", "bad_temp_LI-7700"
                     , "laser_temp_unregulated_LI-7700", "block_temp_unregulated_LI-7700", "motor_spinning_LI-7700", "pump_on_LI-7700", "top_heater_on_LI-7700", "bottom_heater_on_LI-7700"
                     , "calibrating_LI-7700", "motor_failure_LI-7700", "bad_aux_tc1_LI-7700", "bad_aux_tc2_LI-7700", "bad_aux_tc3_LI-7700", "box_connected_LI-7700", "mean_value_RSSI_LI-7200"
                     , "mean_value_LI-7500", "u_var", "v_var", "w_var", "ts_var", "co2_var", "h2o_var", "ch4_var", "none_var", "w/ts_cov", "w/co2_cov", "w/h2o_cov", "w/ch4_cov", "w/none_cov")
  header.epFulls <- c("date", "time", "file_records", "used_records", "Tau", "qc_Tau", "rand_err_Tau", "H", "qc_H"
                     , "rand_err_H", "LE", "qc_LE", "rand_err_LE", "h2o_flux", "qc_h2o_flux", "rand_err_h2o_flux"
                     , "H_strg", "LE_strg", "h2o_v-adv"
                     , "h2o_molar_density", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_time_lag", "h2o_def_timelag"
                     , "sonic_temperature", "air_temperature", "air_pressure", "air_density", "air_heat_capacity", "air_molar_volume"
                     , "ET", "water_vapor_density", "e", "es", "specific_humidity", "RH", "VPD", "Tdew"
                     , "u_unrot", "v_unrot", "w_unrot", "u_rot", "v_rot", "w_rot", "wind_speed", "max_wind_speed", "wind_dir", "yaw", "pitch", "roll"
                     , "ustar", "TKE", "L", "(z-d)/L", "bowen_ratio", "T*", "model", "x_peak", "x_offset", "x_10", "x_30", "x_50", "x_70", "x_90"
                     , "spikes_hf", "amplitude_resolution_hf", "drop_out_hf", "absolute_limits_hf", "skewness_kurtosis_hf", "skewness_kurtosis_sf", "discontinuities_hf", "discontinuities_sf", "timelag_hf", "timelag_sf", "attack_angle_hf", "non_steady_wind_hf"
                     , "u_spikes", "v_spikes", "w_spikes", "ts_spikes", "h2o_spikes"
                     , "u_var", "v_var", "w_var", "ts_var", "h2o_var", "w/ts_cov", "w/h2o_cov")
  
  fname.cwo <- "ASTW_intChute_2008-2010_0D_output/ASTW_intChute_2008-2010_0D_WB_20230709_0236.csv"    # preprocessed CanWat output 
  header.cwo <- c("UTC", "EvNo", "dt", "C_3D", "dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf", "Rn", "sH", "rLE_3D", "rH_3D")

## Paths and subroutines #### gsub("\\\\", "/", (readClipboard())) ####
  Rechner <- Sys.info()["nodename"]
  switch(Rechner
         , "FMELAP78" = {
           rlib <- "d:/b_Programming/0_R_lib"; 
           path.h <- "d:/p_Interzeption/2025_Interception_EC"
         }
         , "COLDAIRSIMULATI"   = {       # TUD virtual machine
            rlib <- "c:/Projects/b_Programming/0_R_lib"
            path.h <- "c:/Projects/2025_Interception_EC"
         }
         , "my_computer" = {  # your computer
           rlib <- "d:/R_lib"
           path.h <- "C:/Project-new/canwat-main"
         }
         
  )

  path.d1 <- file.path(path.h,"Data/ICOS_Fluxnet") # path to footprint data
  path.d2 <- file.path(path.h,"Data/ICOS_Fluxnet") # path to fluxes
  path.d.cwo <- file.path(rlib, "y_CanWat/Output")               # path to CanWat Output
  
  path.r <- file.path(path.h,"EC_2D_footprints/Results") 
  if (!file.exists(path.r)) {dir.create(path.r, showWarnings = FALSE)}
  
  lfi <- list.files(path = file.path(path.d1), pattern = "^eddypro_(.*)txt$") # %>%  sort
  nf <- length(lfi)                                       

## Libraries, User Functions ####################################################
  source(file.path(rlib,"s_FluxFoodPrint/calc_footprint_FFP.R"))   
  source(file.path(rlib,"s_FluxFoodPrint/calc_footprint_FFP_climatology.R"))   
    
## Process variables / Cases / Parameters ###########################
  Tstamp <- strftime(Sys.time(), format("%Y%m%d_%H%M"))
  FTstamp <- paste("footprint.R, ", Tstamp)

  ### FFP contour output for ####
  fpc <- seq(10,90,10)         # %, Percentage of source area for which to provide contours, must be between 10% and 90%
  
  ### Coordinates ####
  projGeo  = 4326              # epsg code of the coordinate systems
  projUTM = 25833
  
  #### of the tower ###
  xref.UTM <- 399248           # m, East coordinate of the reference point
  yref.UTM <- 5646644          # m, North coordinate of the reference point
  zref.UTM <- 385              # m, heigth above see level of the reference point
  lonlat <- transX(lonE = xref.UTM, latN = yref.UTM, srsIn = projUTM, srsOut = projGeo )
  lon <- lonlat$lon;       lat <- lonlat$lat
  
  #### FFP field ###
  mdiscr <- 10                 # grid resolution
  ext <- 1000                  # m, extend of the footprints from the reference point
  xr.ffp.UTM <- xref.UTM + c(-ext, ext)  # range of the footprint calculation (ffp domain)
  yr.ffp.UTM <- yref.UTM + c(-ext, ext)            
  field.dom <- transX(lonE = xr.ffp.UTM, latN = yr.ffp.UTM, srsIn = projUTM, srsOut = projGeo )
  
  #### mask array ###
  xr.mask.UTM <- c(398644, 399444)     # xref.UTM + c(-604, 196)
  yr.mask.UTM <- c(5646108, 5647248)   # yref.UTM + c(-536, 604)          
  xr.mask.ref <- xr.mask.UTM - xref.UTM
  yr.mask.ref <- yr.mask.UTM - yref.UTM
  mask.dom <- transX(lonE = xr.mask.UTM, latN = yr.mask.UTM, srsIn = projUTM, srsOut = projGeo )
  
  ####  interception gutter ###
  x1.gut.ref <- 1 ;  x2.gut.ref <- 22  # east of the reference point
  y1.gut.ref <- -13;  y2.gut.ref <- 2  # north of the reference point
  xr.intgutter.UTM <- xref.UTM + c(x1.gut.ref, x2.gut.ref)            
  yr.intgutter.UTM <- yref.UTM + c(y1.gut.ref, y2.gut.ref)            
  gutter.dom <- transX(lonE = xr.intgutter.UTM, latN = yr.intgutter.UTM, srsIn = projUTM, srsOut = projGeo )
  
  ## Output Preparation #####################################
  
  ### output dirs ####
  path.r.m  <- file.path(path.r, Tstamp)              # main result path 
  path.r.d0  <- file.path(path.r.m, "FFP_results")
  path.r.dc  <- file.path(path.r.m, "FFP_contours")
  path.r.d  <- file.path(path.r.m, "mask_data")
  path.r.p  <- file.path(path.r.m, "masks_png")
  path.r.pi <- file.path(path.r.m, "masks_html")
  dir.create(path.r.m, showWarnings = FALSE)
  dir.create(path.r.d0, showWarnings = FALSE)
  dir.create(path.r.dc, showWarnings = FALSE)
  dir.create(path.r.d, showWarnings = FALSE)
  dir.create(path.r.p, showWarnings = FALSE)
  dir.create(path.r.pi, showWarnings = FALSE)
  
  
  ### journal setup ####
  journal <- c(paste0("# ",FTstamp) 
               , paste0("**author**: ronald.queck@tu-dresden.de")
               , paste0("**Version**: ",VDate)
               , paste0(""), paste0("---")
               , "# Input "
               , paste0("**home folder**: ", path.h)
               , paste0("**path to footprint data**: ", path.d1, "/", fname.eddyproFull )
               , paste0("**path to CanWat Output**: ", path.d.cwo ,"/", fname.cwo)
  )
  journal <- c(journal, "# Parameters",
               "Positions of each 10%-flux part is calculated at ", paste(fpc, collapse = ", "))
  

  ### Plot Parameter ####
  pal0 <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  pal.r <- pal0(32)
  figsize <- 2000
  cex <- figsize/1000
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ DATA #########################################################
  
  i.file <- 1
#  for (i.file in 1:nf ){ # ~~> i.file loop ###
#  fname.foot <- lfi[i.file]

      
  ## read eddypro data ####  
    fp0 <- fread(file.path(path.d1, fname.eddyproFull), skip = 3); names(fp0)<-header.epFull
    # outlier tests
    fp0[fp0 == -9999] <- NA
    fp1 <- fp0[, .SD, .SDcols=header.epFulls]
    fp1 <- fp0 
    fp1$UTC  <-  as.POSIXct(paste(fp1$date,fp1$time, sep=" "), "%Y-%m-%d %H:%M", tz="UTC")-3600
    nn.fp1 <- dim(fp1)[1]
    fp1$UTC[1:10]

    rm("fp0")
    
  ## read event data ####  
    # preprocessed by CanWat 
    # They have increasing numbers 
    # UTC is at the beginning of the time intervall 
    cwo <- fread(file.path(path.d.cwo,fname.cwo))
    # header.cwo <- c("UTC", "EvNo", "dt", "C_3D", "dC", "PF", "Pintercept", "Pthroughf", "EV", "ET", "Drainage", "Dintercept", "Dthroughf", "Rn", "sH", "rLE_3D", "rH_3D")
    events <- as.numeric(names(table(cwo$EvNo)))[-1]
    nn.ev <- length(events)

    ## output tables ####
    if(!re.start){
      datedum <- POSIXct(length = nn.ev, tz="UTC")
      evlog <- data.table(EvNo = integer(nn.ev)
                          , evStart = datedum,    evEnd = datedum
                          , evStart.ec = datedum, evEnd.ec = datedum
                          , data.flag = single(nn.ev)     # part of data that are suitable for footprint calc.
                          , FFP.flag = integer(nn.ev)     # 0 if no error, see flag_err in calc_footprint_FFP_climatology.R
                          , fr.max = double(nn.ev)        # maximal footprint per grid cell
                          , fp.in.ext = integer(nn.ev)     # part that is covered by the given domain 
                          , fp.in.dom = integer(nn.ev)     # part that is covered by the given domain 
      )
    }
    
    
        
# PREPROCESSING ####
    
    ## event data ####
    # sum(!is.finite(cwo$UTC))
    cwo$UTC <- cwo$UTC + mean(diff(cwo$UTC, na.rm=T) )  # set the time to the end of the interval
    # aggregate()
    cwo[, UTC2:= ceiling_date(UTC, "30 mins")]
    cwo2 <- cwo[, .( max(EvNo, na.rm=T), mean(dt, na.rm=T), mean(C_3D, na.rm=T), mean(dC, na.rm=T)
             , sum(PF, na.rm=T), sum(Pintercept, na.rm=T), sum(Pthroughf, na.rm=T)
             , sum(EV, na.rm=T), sum(ET, na.rm=T), sum(Drainage, na.rm=T), sum(Dintercept, na.rm=T), sum(Dthroughf, na.rm=T)
             , sum(Rn, na.rm=T), sum(sH, na.rm=T), mean(rLE_3D, na.rm=T), mean(rH_3D)) , by=UTC2]
    names(cwo2) <- header.cwo
    UTC.r <- cwo2[, range(UTC)]
    journal <- c(journal, paste0("# Time intervall ") 
                 , paste0("Start: ", UTC.r[1]), paste0("End: ", UTC.r[2])
    )
    

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # CALCULATIONS  #####################################################
    
    i.ev <- 1
    # for (i.ev in event1:nn.ev) {  # ~~> event loop 1 ####
    for (i.ev in event1:event1) {  # ~~> event loop 1 ####
        (Ev.No <- events[i.ev])
      
      ## select rain events ####
      UTC.r.ev <- cwo[EvNo == Ev.No, range(UTC)]; ev.start <- UTC.r.ev[1]; ev.end <- UTC.r.ev[2]
      print(paste0("event ",  Ev.No, ", from " , ev.start, " to ", ev.end))
      ev.start.ec <- ev.start-minute(ev.start)*60 + (minute(ev.start)%/%30)*30*60
      ev.end.ec   <- max(ev.end - minute(ev.end)*60   + (minute(ev.end)%/%30)*30*60, ev.start.ec+1800)
      evlog[i.ev, ":=" (EvNo=Ev.No, evStart = ev.start, evEnd = ev.end
                        , evStart.ec = ev.start.ec, evEnd.ec = ev.end.ec)] 
      isel.ev <- which((fp1$UTC>=ev.start.ec) & (fp1$UTC <= ev.end.ec))
      if (length(isel.ev) < 1){
        print(paste0("===>  no EC data available"))
        journal <- c(journal, paste0("event ",  Ev.No, ", from " , ev.start, " to ", ev.end, ", no EC data available"))
        evlog[i.ev, data.flag:=0]
        next()
      }
      Tstamp.ev <- paste0( strftime(ev.start.ec, format("%Y-%m%d_%H%M"), tz="UTC"),"-", strftime(ev.end.ec, format("%m%d_%H%M"), tz="UTC"),"-UTC")
      fp <- fp1[isel.ev,]
      fp <- fp[!is.na(wind_speed) & !is.na(L) & !is.na(v_var) & !is.na(ustar) & (ustar > 0.1) & !is.na(wind_dir) & (z_m/L > -15.5), .(wind_speed, wind_dir, ustar, L, v_var)]
      (nn <- dim(fp)[1])
      evlog[i.ev, data.flag := nn/length(isel.ev)]; evlog$data.flag[i.ev]
      if (nn==0) {
        cat("NO valid data for event ",  Ev.No, ", from " , strftime(ev.start), " to ", strftime(ev.end),"\n")
        journal <- c(journal, paste0("NO valid data for event ",  Ev.No, ", from " , ev.start, " to ", ev.end ))
        next
      }
      
      ## FFP ####
      umean    <- fp$wind_speed  # m/s, Mean wind speed at zm
      MOL      <- fp$L           # m,   Obukhov length [m]
      sigmav   <- sqrt(fp$v_var) # m/s, standard deviation of lateral velocity fluctuations
      ustar    <- fp$ustar       # m/s, friction velocity [ms-1]
      wind_dir <- fp$wind_dir    # °
      hbl      <- 0.3*ustar/(2*(7.2921*10^-5)*sin(51*pi/180)) # m, Rossby and Montgomery (1935)
      hbl[hbl < 200]  <- 200
      hbl[hbl > 2000] <- 2000
      
      FFP <- calc_footprint_FFP_climatology(zm=z_m, z0=z_0, umean=umean, h=hbl
                      , ol=MOL, sigmav=sigmav, ustar=ustar, wind_dir=wind_dir
                      , domain=c(-ext,ext,-ext,ext), dx=mdiscr, dy=mdiscr, r=fpc
                      , smooth_data=1, crop = NULL)

      if(is.null(FFP)) {
        evlog[i.ev, ":=" (FFP.flag = 999)]
        next
      }

      xyf_2d <- data.table(x_2d=as.numeric(FFP$x_2d), y_2d=as.numeric(FFP$y_2d), fclim_2d=as.numeric(FFP$fclim_2d))

      f0 <- FFP$fclim_2d;       dimf0 <- dim(f0); range(f0)
      f0 <- f0/sum(f0, na.rm=T)  # normalize the footprint
      f0[is.na(f0)] <- 0; f0[f0 < 0] <- 0
      
      range(f0)
      
      ## crop the field to the canwat static driver ####
      # ffp.domain=c(-ext,ext,-ext,ext)   to   cw.domain=c(-600,190,-530,600)
      # xr.mask.ref = c(-604, 196) ; yr.mask.ref = c(-536, 604) # see section "Coordinates"
      nix.cw <- diff(xr.mask.ref)/mdiscr
      niy.cw <- diff(yr.mask.ref)/mdiscr
      (ix1 <- round(xr.mask.ref[1]/mdiscr) - round((-ext)/mdiscr) +1)
      (iy1 <- round(yr.mask.ref[1]/mdiscr+mdiscr/2) - round((-ext)/mdiscr) +1)
      (iy1 <- ceiling( ((yr.mask.ref[1]) - (-ext))/mdiscr)+1)
      (ix2 <- ix1+nix.cw-1)
      (iy2 <- iy1+niy.cw-1)

      f1 <- apply(f0[,iy1:iy2], 2, function(x, ii1=ix1, ii2=ix2){x[ix1:ix2]} )
      dim(f1);       range(f1)
      if (min(f1) < 0) stop("!!!! footprint < 0 !!!!")
      

# OUTPUT #################################################
      
    ## Data ####
      fname.base <- paste0("FFPc-of-event_", formatC(Ev.No, width = 3,flag="0" ),"_", Tstamp.ev)
      ### original climatology ####
      fwrite( xyf_2d, file.path(path.r.d0, paste0(fname.base, "_ot.csv")))            # as table
      fwrite( as.data.table(FFP$fclim_2d), file.path(path.r.d0, paste0(fname.base, "_fclim_2d.csv"))) # as matrix
      if (new.start) {               # coordinates of the original climatology
        fwrite( as.data.table(FFP$x_2d), file.path(path.r.d0, paste0(fname.base, "_x_2d.csv"))) 
        fwrite( as.data.table(FFP$y_2d), file.path(path.r.d0, paste0(fname.base, "_y_2d.csv"))) 
      }
      # contourlines
      fp.dom <- 0
      if (!is.null(FFP$xr[2])) {
        n.r <- sum(!is.na(FFP$r))   
        i <- 1
        if (n.r > 1) for (i in 1:n.r) {
          # check whether the data are in the cw domain
          cxy <- data.table(xr=as.numeric(FFP$xr[[i]]), yr=as.numeric(FFP$yr[[i]]))
          if( dim(cxy[(xr < xr.mask.ref[1]) | (xr > xr.mask.ref[2]) | 
                     (yr < yr.mask.ref[1]) | (yr > yr.mask.ref[2]), ])[1] == 0 ){
            fp.dom <- FFP$r[i]
          } 
          fwrite(cxy
            , file.path(path.r.dc
                      , paste0(fname.base, "_r"
                              , formatC(FFP$r[[i]]*100, width = 3,flag="0" ) 
                              , ".csv"))
          )
        }
      }
      
      ## log-file ####
      evlog[i.ev, ":=" (FFP.flag = FFP$flag_err
                        , fr.max = ifelse(any(!is.na(FFP$fr)), max(FFP$fr, na.rm = T), 0) 
                        , fp.in.ext = max(FFP$r, na.rm=T)*100
                        , fp.in.dom = fp.dom*100
      )]
      
    ### scaled and croped field ####      
      fwrite( as.data.table(t(f1)), file.path(path.r.d, paste0(fname.base, ".csv")), append=T) 
      

      new.start <- F
    } # <~~~ event loop ####

# } # <~~ i.file loop ###
#   

# FINAL OUTPUT ####
    # strftime(UTC.r[2], format("%Y%m%d"), tz="GMT")
    Tstamp.interv <- paste0(strftime(UTC.r[1], format("%Y%m%d")),"-",strftime(UTC.r[2]-1, format("%Y%m%d")))
    
    ## log file ####
    fwrite(evlog, file.path(path.r.m, paste0("EC_2D-footprints_", Tstamp.interv,"_calc", Tstamp ,"_log.csv" )))      
  
    ## journal ####
    journal <- c(journal, "# Footprint Mask"
                 , paste0("**Method**: Klijun et al. 2015, > calc_footprint_FFP_climatology.R <")
                 , paste0("   spatial grid width: ", mdiscr)
                 , paste0("   xy extend of the grid: ", -ext,", ", ext)
                 , paste0("**Description**: parameterisation of the simulations of the backward Lagrangian stochastic particle dispersion model LPDM-B (Kljun et al., 2002)")
    )
    journal <- c(journal, "# Output"
                 , paste0("## numerical output ")
                 , paste0("**Foodprints are saved as csv** in ")
                 , paste0( path.r.m )
                 , paste0("the original climatology is saved in ..\\FFP_results")
                 , paste0("   - as table with 'ot' before the file extension:   x_2d, y_2d, fclim_2d")
                 , paste0("   - as matrix with 'fclim_2d' before the file extension")
                 , paste0("   - cordinates for the matrixes are saved only ones with '_x_2d', '_y_2d' before the file extension")
                 , paste0("The files have 1 header line, then follows an array of ", 2*ext/mdiscr , "x", 2*ext/mdiscr, "grid cells")
                 , paste0("contour lines are saved in '..\\FFP_contours'")
                 , paste0("  for ", paste0(seq(10,90,10), collapse = "% "  ), "%" )
                 , paste0("  if available ")
                 , "\n"
                 , paste0("A scaled and croped climatology is saved as matrix in '..\\mask_data'")    
                 , paste0("The files have 1 header line, then follows an array of "
                          ,  diff(c(-604, 196))/mdiscr , "x", diff(c(-536, 604))/mdiscr )
                 , paste0("lower left corner and upper right corner is")
                 , paste0("   xr.mask.UTM <- c(398644, 399444)     # xref.UTM + c(-604, 196)")
                 , paste0("   yr.mask.UTM <- c(5646108, 5647248)   # yref.UTM + c(-536, 604)")          
                 , paste0("   reference is the tower (ASTW) ICOS Site DE-tha, Tharandt")
                 , paste0("")
                 , "\n"
                 , paste0("**Plots are saved as png**: ", path.r.p )
                 , paste0("**Interactive plots are saved as html**: ", path.r.pi )
    )
    write(journal, file.path(path.r.m, paste0("EC_2D-footprints_", Tstamp.interv,"_calc", Tstamp ,"_journal.md" )))
    
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # TIDY UP ###################################################
# # par(op)
# par(mfcol=c(1,1), fig=c(0,1,0,1), oma = c(2, 2, 2, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1) # outer(title), inner(labels) margin, c(unten, links, oben, rechts)
# layout(matrix(c(1,1,1,1),ncol=1));  plot.new()
# #  unlink
# #  for (i in dev.list()) dev.off(i) 

    