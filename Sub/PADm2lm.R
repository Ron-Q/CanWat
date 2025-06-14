#+
# NAME: PAD2lm.R
# PURPOSE: calculates the mixing length for each voxel element
#    currently PADmax = 1, this is often exceeded!
#    if one takes smaller layer than 1m as a standard, the PADme value would be smaller, 
#    and this boundary would not be touched
# CALLED BY:
# SOURCE(file.path(path_sub,"PAD2lm.R",sep=""))
# INPUT:
  # PADme   : Plant area Density (spatial mean), extended over the whole domain
  # zzu     : # heights of the z levels
  # dz
# OUTPUT:
  # mixing length (Mischungsweglaenge)
# REFERENCE: Qu10: Queck R, Bernhofer C (2010) Constructing wind profiles 
#            in forests from limited measurements of wind and vegetation 
#            structure. Agricultural and Forest Meteorology, 150:S,724–735 
# REVISION HISTORY:
#   2014-10-01 (RQ): created 
#   2019-12-01 (RQ): xyz -> zyx
#   2021-03-23 (RQ): scaling of the PADme to 1m³ => PADme1m3
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PADm2lm  <- function(PADme,zzu, dz=dz, dy=dy, dx=dx, ES = 3){

  PAD2lm.fak <- 0.5
  
  nzu <- dim(PADme)[1]
  
  PADme1m3 <- PAD2lm.fak*PADme #/(dz*dy*dx)       # plant area in 1m³   # 20250212 
  
  # Local parameter 
  # ES <- 10 # 3                     # m², size of the vegetation cluster 
  dummy_number <- 0.000001           # dummy value (otherwise lpzi gets infinite large for PAD=0)
  PADmax <- 1 - dummy_number         # largest PAD (somewhat below 1) for this model
  PADme1m3[PADme1m3 > PADmax] <- PADmax
  PADme1m3[1,,] <- PADmax            # lower boundary
  
  # mean inter elemental spacing at height x (omnidirectional)
  # lpziq <- (ES / (PADme*dz + dummy_number) - ES)    # m², Qu10, Eq. 9
  lpziq <- (ES / (PADme1m3*1 + dummy_number) - ES)    # m², Qu10, Eq. 9
  # lpziq in the ground layer should be very small
  lpziqg <- lpziq[1,,]
  lpziqg[lpziqg > ((dz/2)^2)] <- (dz/2)^2
  lpziq[1,,] <- lpziqg
  
  # mixing length
  mixing_length <- array(NA,c(nzu,ny,nx))
  for(i in 1:nx) {
    for(j in 1:ny) {
      for(k in 1:nzu) {
        delta_y <- ((zzu[k]-zzu)^2 + lpziq[,j,i])     # restriction by the ground surface
        mixing_length[k,j,i] <- min(sqrt(delta_y), na.rm = TRUE) # Qu10, Eq. 9
        # for a special layer k the distance to obstacles in layer kk is calculated by
        # (zzu[k]-zzu[kk])^2 + lpziq[kk,j,i])
      }
    } 
  } 
  return(mixing_length)
}
