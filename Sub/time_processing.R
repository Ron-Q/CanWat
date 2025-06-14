#+
# NAME*: time_processing.R
# PURPOSE*: functions for time processing
  # Note CanWat marks a time interval with the beginning of this interval internally and in the output
  # you need to adjust your input by the variables in Section #* Date description ####
  # namTime <- "time"                 #  , name of the date column
  # Tformat <- "%Y-%m-%dT%H:%M:%SZ"   #  , format of the date column , e.g. "%Y-%m-%d  %H:%M:%S"
  # ToffUTC <- -3600                  # s, time offset to UTC (for CET it is -3600)
  # Tsme    <- 0                      # -, the given time is at the <s>tart 0, <m>id 0.5, or <e>nd 1 the interval
  # Tsoff   <- 540                    # s, offset from the interval-<s>tart of the given time 
  # # and in Section # EVENT SELECTION ###################################################
  # EvNo.namTime <- "UTC"                  #  , name of the date column
  # EvNo.Tformat <- "%Y-%m-%dT%H:%M:%SZ"   #  , format of the date column , e.g. "%Y-%m-%d  %H:%M:%S"
  # EvNo.ToffUTC <- 0                      # s, time offset to UTC (for CET it is -3600)
  # EvNo.Tsme    <- 0                      # -, the given time is at the <s>tart 0, <m>id 0.5, or <e>nd 1 the interval
  # EvNo.Tsoff   <- 0                      # s, offset from the interval-<s>tart of the given time    
  # EvNo.dt_met  <- dt_met
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"time_processing.R"))
  # round.timestep(timeest, tsl = 1800, toff = 0, boundary = "floor")
# REVISION HISTORY*:
  # 2023-08-06 : R. Queck, see R_lib\s_Time_and_Date\round.timestep.R
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
round.timestep <- function(
    timeest = Sys.time() # POSIXct, estimated time(s) to fit
  , tsl = 1800           # s, time step length
  , toff = 0             # s, offset to the next round time (e.g. our)
  , boundary = "floor"   # or "ceiling" # direction to of the next timestep 
                           ){
  # PURPOSE*: snaps the estimated time to the next timestep 
  # CALLED BY: CanWat.R
  # OUTPUT*: nearest timesteps to the estimated time(s)
  # REVISION HISTORY*:
  # 2023-08-06 : R. Queck, see R_lib\s_Time_and_Date\round.timestep.R

  hplus <- as.numeric(timeest) - as.numeric(trunc(timeest, "years"))
  hplus.f <- floor(hplus/tsl)
  start.end <- as.numeric(boundary == "ceiling")
  time.trunc <- trunc(timeest, "years") + (hplus.f+start.end)*tsl     
  return(time.trunc+toff)
}
