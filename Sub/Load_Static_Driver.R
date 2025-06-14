#+
# NAME*: Load_Static_Driver.R
# PURPOSE*: load vegetation data for CanWat.
#           It has to be stored as one dimensional vector with highest frequency in z, then y and x. 
#           Thus z is the most inner loop and x is the outer loop.
#       PAD is in m²/m³, a scaling to the vertical grid size will be applied in the subroutines
#       ==> PAD*dz in m²/(m² voxel-height) plant-area per layer height ([dz] = m/voxel in z direction)
# RELEVANCY*: CanWat
# CALLING SEQ.: 
# INPUTS*:  path_DriverStat,fnam.SD, 
  # SD.dims :  
# OUTPUT*:  
  # PAD   :m²/m³ , [z,y,x], plant area density
  # PADc  :m²/m² , [y,x] cumulative plant area density
  # PAI   :m²/m² , scalar, mean plant area index
# PACKAGES: data.tables 
# REFERENCE: Queck R, Bernhofer C, Bienert A, Schlegel F (2016) The TurbEFA Field Experiment—Measuring the Influence of a Forest Clearing on the Turbulent Wind Field. Boundary-Layer Meteorol 160:397–423 
# REVISION HISTORY*:
#   2019-10-xx , RQ:
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (silent < 1) print("     -> load Static Driver")

  # read 3D-PAD voxel data as vector
  PAD_Voxel_vector <- fread(file.path(path_DriverStat, fnam.SD,sep=""))
  PAD.n <- max(which(!is.na(PAD_Voxel_vector)))
  PAD_Voxel_vector <- PAD_Voxel_vector[1:PAD.n]
  PAD_Voxel_vector[as.logical(is.na(PAD_Voxel_vector))] <- 0
  nxyz <- dim(PAD_Voxel_vector)
  if (nxyz[1] != prod(SD.dims)) stop("the given dimensions do not correspont to the static driver")
  
  # transform the vector to an array in correct order (z,y,x)
  PAD_Voxel_vector.names <- names(PAD_Voxel_vector)
  if (PAD_Voxel_vector.names[1] == "V1")  {
    PAD_all <- array(PAD_Voxel_vector$V1,dim=SD.dims)} else{
    PAD_all <- array(PAD_Voxel_vector$pad,dim=SD.dims)}

  # select data for model domain
  PAD0 <- PAD_all[iz1:iz2,iy1:iy2,ix1:ix2, drop=F]     
  # add an additional empty layer below the domain
  #c(nz0,ny0,nx0) %<-% dim(PAD0)
  dim.PAD <- dim(PAD0)
  nz0 <- dim.PAD[1]; ny0 <- dim.PAD[2]; nx0 <- dim.PAD[3]
  if (nz0+2 != nz) stop("error in z dimension")
  PAD0.t <- aperm(PAD0, c(3,2,1))
  PAD0.t1De <- c(rep(0, ny0*nx0), as.numeric(PAD0.t), rep(0, ny0*nx0))  # transposed and extended
  PAD0.te <- array(PAD0.t1De, dim=c(nx0,ny0,nz0+2))
  PAD0 <- aperm(PAD0.te, c(3,2,1))

  # adjust PAD after user function ####
  if (exists("PADfunction")) PAD <- PADfunction(PAD0) else PAD <- PAD0
  
  rm(PAD_Voxel_vector, PAD_all, PAD0, PAD0.t, PAD0.te, PAD0.t1De)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if (is.na(sum(PAD))) stop("There are missing values in the static driver")
  if (sum(PAD)==0) stop("There is no surface in the domain")
  
# PADc : cumulated PAD #####
  # add PAD in layers, 
  # start in the top layer with PADc = PAD
  # then the lowest layer contains the PAI
  PADc <- apply((PAD*dz), c(2,3), function(a){ rev(cumsum(rev(a)))})
  ihv <- apply(PAD, c(2,3), function(a){ if(sum(a)>0) max(which(a > 0)) else 0}) # highest layer with vegetation
  ihv.s    <- summary(as.numeric(ihv))         # vegetation statistic
  ihv.mean <- ihv.s[5]   # 3rd Qu. of vegetation height
  ihv.max  <- ihv.s[6]   # max of vegetation height
  if (!exists("hveg")) hveg <- (as.numeric(ihv.mean)-1)*dz
  
#* mean PAI ####
  PAI <- mean(PADc[1,,])

  if (!exists("PAI.S0")) PAI.S0 <- PAI
    

# stemflow ####
  LAD <- PAD 
  if (exists("sflow.s")) if (sflow.s){
    SAD <- PAD*SAI/PAI
    LAD <- PAD - SAD
  } 

#* calculate index vector with and without PAD ####
  iPAD <- which(PAD != 0)
  iPADeq0 <- which(PAD == 0)
  nPAD <- length(iPAD)

    
# plots
 # stop("handstop")  
 #  str(PAD)
  
  
  journal <- c(journal
    , paste("Dimensions given (xyz): ", SD.dims[3], ", ", SD.dims[2], ", ", SD.dims[1], "  ")
    , paste("selected range (xyz): ") # , ix1, ":", ix2, ", ", iy1, ":", iy2, ", ", iz1, ":", iz2, "  "  )
    , paste0("x: ", ix1," .. ", ix2," -> ", ix2-ix1+1 , " cells", "  ")
    , paste0("y: ", iy1," .. ", iy2," -> ", iy2-iy1+1 , " cells", "  ")
    , paste0("z: ", iz1," .. ", iz2," -> ", iz2-iz1+1 , " cells", "  ")
    , paste0("resolution: dx=", dx, ", dy=", dy, ", dz=", dz, "  ")
    , paste("PAI = ",round(PAI,2)," m²/m²"), paste("  ", "  ")
  )
  
