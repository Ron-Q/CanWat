#+
# NAME*:    CanWat_Start.R
# PURPOSE*: Starter of 'CanWat'
#   CanWat is a numerical model for the calculation of the in-canopy water balance based on 
#   gross rainfall, temperature, humidity, wind and a vegetation model
# RELEVANCY*: Water balance of forests
# PACKAGES:
  # install.packages("pacman")
  pacman::p_load(data.table, png, rasterImage, colorRamps)
# REFERENCE:
  #   Queck (2025) "CanWat - A Tool for Investigating Rainfall Interception with high spatial Resolution" paper in preparation", Poster at DACH 2025.
  #   Queck (2025) "Rainfall interception - Localization of water storage and evapo(transpi)ration in forests with CanWat" paper in preparation
#-
# INPUT BEGIN #######################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#* CLEAR the WORKSPACE? ####
  # if TRUE "T", every variable/device will be deleted/closed !
  # if FALSE "F", variables remain in your environment but may be overwritten or cause conflicts
  if (!exists("cw_multi")){
    if (T) {remove (list = ls())             ; print("######## Reset #########")}
    if (T) {for (i in dev.list()) dev.off(i) ; print("## all devices closed ##")}
  }    
  RunControlFine <- F # if TRUE there is a higher frequency of control outputs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
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
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#* name a case/incident ####
  # act <- "Case01_Tree01_1D_01L"  # example for a Big Leaf simulation
  # act <- "Case01_Tree01_1D_02L"
  # act <- "Case01_Tree01_1D_03L"
  # act <- "Case01_Tree01_1D_04L"
  # act <- "Case01_Tree01_1D_05L"
  # act <- "Case01_Tree01_1D_10L"
  # act <- "Case01_Tree01_2D_10L"
  # act <- "Case01_Tree01_3D_10L"  # example for a simulation on a grid of 1 m³
  # act <- "Case02_Tree01_1D_20L"
  # act <- "Case02_Tree01_3D_20L"
  # act <- "Case00_homogen_2D_01L"
   act <- "ASTW_intChute_2010_3D"   # ICOS site DE-THA, Trees around the interception chute near the measurement tower
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* Paths and subroutines #### 
  ## !!! necessary adaptations !!! ####
  # please add the name of your machine/computer and the main path of CanWat to the list of the following switch command
  # you may get the name of your computer by running the command: "Sys.info()["nodename"]"
  # An easy and save way to get the home path of CanWat is to navigate to the folder in your file commander/explorer and copy the path.
  # In windows you may run the command "gsub("\\\\", "/", (readClipboard()))" to change back slashs to slashs
  switch(Sys.info()["nodename"]
         , "MICROPC16"         = {path.CW <- "d:/b_Programming/Git/CanWat"}             # Ronald windows machine
         , "rq-ThinkPad-T460p" = {path.CW <- "/media/rq/Projects/b_Programming/Git/CanWat"}  # Ronald linux machine
         , "FMEPC178"          = {path.CW <- "d:/b_Programming/Git/CanWat"}             # IHM met windows machine
         , "FMELAP78"          = {path.CW <- "d:/b_Programming/Git/CanWat"}             # IHM met laptop
         , "COLDAIRSIMULATI"   = {path.CW <- "c:/Projects/b_Programming/Git/CanWat"}    # TUD virtual machine
  )

  ##~~ Possible customization for individualists ####
  # you can also change the following sub paths if you prefer another structure 
  path_Cases <- file.path(path.CW , "Cases")
  path_DriverStat <- file.path(path.CW , "Driver_static")
  path_DriverDyn <- file.path(path.CW , "Driver_dynamic")
  path_CWpara <- file.path(path.CW , "Parameters")
  path_output <- file.path(path.CW , "Output")
  path_sub <- file.path(path.CW , "Sub")

  
# INPUT END #########################################################  

  # RUN PROGRAM #######################################################
  CanWatStart <- T
  source(file.path(path_sub,"CanWat.R"))
  

