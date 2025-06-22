#+
# NAME*: storage_parameter.R
# PURPOSE*: canopy storage parameter 
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"plant_parameter_values.R",sep=""))
# INPUTS*:  PAD, 
  #  S0  : l/m² or mm/m², mean canopy storage capacity => CW_paramter
# OUTPUT*:  
  # S_3D: canopy storage capacity per voxel (Benetzungskapazitaet)
  # Dmin: minial drainage per column
  # bd :  drainage coefficient per column
  # Sc :  total canopy storage capacity per column
# PACKAGES: 
# REFERENCE: Moderow U (2004) Messung und Modellierung der Interzeption für den Fichtenbestand an der Ankerstation Tharandter Wald. Projektarbeit. TU Dresden, Tharandt.
# REVISION HISTORY*:
  # 2019-08-12 , RQ: Berechnung von p_3D und p_3D in P_dist_matrix.R verschoben
  # 2019-10-24 , RQ: S_3D scaled with PAD/PAI (relative plant area distribution), + several other changes
  # 2022-12-10 , RQ: changed to function type
  # 2025-03-12 , RQ: changed PAI to PAI.S0. 
  #         If storage capacity S0 is only valid for a certain area of the domain, 
  #         the calculation of the specific storage capacity S/PAI in mm/m² should
  #         should only take the PAI of this area (the PAI.S0) into account 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
storage_parameter <- function(S0, Dmin0, bd0, PAI.S0, PAD,dz){

  if (silent < 2) cat("---- calculate canopy storage parameter \n")

  # S, canopy storage capacity (Benetzungskapazitaet) ####
    # S ~ LAD
    # S = aa1 * LAD^aa2 + aa3  #
    # aa1 <- 0.71; aa2 <- 1;  aa3 <- 0
    # aa1 <- S0 = S_est/PAI  # mm/(m²-plant surface)
    # Distribution of the storage capacity
    S_3D <- S0/PAI.S0*PAD*dz         # mm/(m² voxel-height), mm per (m²-ground surface and voxel height)
    # S_mean <- mean(S_3D[iPAD ])
    # S_max <- max(S_3D[iPAD ])
    # S_min <- min(S_3D[iPAD ])
  
  # canopy storage capacity pro Column 
    # Sci <- array(NA, c(nx,ny))
    # for (i in 1:nx) for (j in 1:ny) Sci[i,j] <- sum(S_3D[i,j,]) 
  
  
  # minial Drainage ####
    # normalized minial Drainage
    # Dmin <- Dmin0/S0 * S_3D # after Rutter et al. (1975)
    Dmin <- Dmin0/S0
    
    # minial Drainage per column ####
    # Dmin <- array(NA, c(nx,ny, nz)) 
    # for (k in 1:nz) 
    #   Dmin[,,k] <- Dmin0/S0*rPAI   # normalized with S0 and horizontally distributed by rPAI  
  
  
  # drainage coefficient ####
    # normalized drainage coefficient
    bd <- bd0*S0 
  
  # drainage coefficient per column ####
    # bd <- array(NA, c(nx,ny,nz))
    # for (k in 1:nz) bd[,,k] <- bd0*S0
  
  
  # Smax, maximal canopy storage capacity (Saettigungsmenge) ####
  # Smax ~ LAD
  # bb1 <- 1.39
  # bb2 <- 1
  # bb3 <- 0
  # Smax_3D <- bb1*PAD^bb2 + bb3
    
  return(list(S_3D=S_3D, Dmin=Dmin, bd=bd))  
    
}
