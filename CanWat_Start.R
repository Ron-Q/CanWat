#+
# NAME*:    CanWat_Start.R
# PURPOSE*: Starter of 'CanWat'
#   CanWat is a numerical model for the calculation of the in-canopy water balance based on 
#   gross rainfall, temperature, humidity, wind and a vegetation model
# RELEVANCY*: Water balance of forests
# PACKAGES:
  # install.packages("data.table")
  # install.packages("png")
  # install.packages("rasterImage")
  # install.packages("colorRamps")
  # install.packages("zeallot")
# REFERENCE:
#   Queck  (2023) paper in preparation
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
  act <- "Case00_homogen_2D_01L"
  # act <- "Case01_Tree01_1D_01L"
  # act <- "Case01_Tree01_1D_02L"
  # act <- "Case01_Tree01_1D_03L"
  # act <- "Case01_Tree01_1D_04L"
  # act <- "Case01_Tree01_1D_05L"
  # act <- "Case01_Tree01_1D_10L"
  # act <- "Case01_Tree01_2D_10L"
  # act <- "Case01_Tree01_3D_10L"
  # act <- "Case02_Tree01_1D_20L"
  # act <- "Case02_Tree01_3D_20L"
  
  # act <- "ASTW_intChute_2010_0D"
  # act <- "ASTW_intChute_2010_3D"
  # act <- "ASTW0_exmpl_07" 
  # act <- "StudyProject2023"
  # act <- "ASTW_intChute_2008-2010_0D"
  
  # act <- "ASTW_PAD2011-gutter-3D_2008-2010"    
  # act <- "ASTW_PAD2011-3D_2008-2010"           # 2025-01-22 tests no runs RQ
  # act <- "ASTW_PAD2011-2D_2008-2010"           # 2025-01-23 .. 02-13 RQ
  # act <- "ASTW_PAD2011-0Dgut_2008-2010"        # 2025-02-13 RQ
  
  # act <- "ASTW_mean_2010_0D"                   # 2025-01-17 RQ
  # act <- "ASTW_PAD2010_0D_dry"                 # 2025-01-28 RQ
  # act <- "ASTW_PAD2010_0D_2008-10AprSepDry"    # 2025-02-07 RQ   
  # act <- "ASTW_PAD2010_0D_2008-10AprSep"       # 2025-02-10 RQ
  
  #act <- "ASTW_PAD2010_0Dgut_2008-2010"        # 2025-02-26 RQ
  # act <- "ASTW_PAD2010_2Dguth_2008-2010"        # 2025-02-26 RQ
  # act <- "ASTW_PAD2010_2Dgut_2008-2010"        # 2025-02-26 RQ
  # act <- "ASTW_PAD2010_0D_2008-2010"           # 2025-02-25 RQ 
  # act <- "ASTW_PAD2010_2D_2008-2010"           # 2025-02-25   
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* Paths and subroutines #### 
  ## !!! necessary adaptations !!! ####
  # please add the name of your machine/computer and the main path of CanWat to the list of the following switch command
  # you may get the name of your computer by running the command: "Sys.info()["nodename"]"
  # An easy and save way to get the home path of CanWat is to navigate to the folder in your file commander/explorer and copy the path.
  # In windows you may run the command "gsub("\\\\", "/", (readClipboard()))" to change back slashs to slashs
  switch(Sys.info()["nodename"]
         , "MICROPC16"         = {path.CW <- "d:/b_Programming/0_R_lib/y_CanWat"}  # Ronald windows machine
         , "rq-ThinkPad-T460p" = {path.CW <- "/media/rq/Projects/b_Programming/0_R_lib/y_CanWat"}  # Ronald linux machine
         , "DESKTOP-2633AIU"   = {path.CW <- "C:/Users/HP/Desktop/Gitare/HSE Notes/Internship/Dr Queck/Doku/canwat-main"}  # Gitare 
         , "Hotzenplotz"       = {path.CW <- "..."}  # Uta
         , "FMEPC178"          = {path.CW <- "d:/b_Programming/0_R_lib/y_CanWat"}  # IHM met windows machine
         , "FMELAP78"          = {path.CW <- "d:/b_Programming/0_R_lib/y_CanWat"}       # IHM met laptop
         , "meinRechner"   = {path.CW <- "c:/Projects/b_Programming/0_R_lib/y_CanWat"}
         , "COLDAIRSIMULATI"   = {path.CW <- "c:/Projects/b_Programming/0_R_lib/y_CanWat"}    # TUD virtual machine
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
  
  # ito.stop <- 235
  
# RUN PROGRAM #######################################################
  CanWatStart <- T
  source(file.path(path_sub,"CanWat.R"))
  
# dev.off()

