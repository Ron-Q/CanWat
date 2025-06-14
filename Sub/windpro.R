#+
# NAME*: windpro.R
# PURPOSE*: 
# derive generic wind profiles from PAD
# i.e. estimate a normalized windprofile for each xy-column
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"windpro.R",sep=""))
# INPUT:
  # PAD   : Plant area Density
  # zz     : Height vector (Hoehen Raster)
  # z0     : Roughness length (Rauigkeitslänge)
  # ZD <- z0 * 7  (nicht aktiv)     # Displacement height
  # MO <- 99999   (nicht aktiv)     # Monin Obukhov Length
  # hWS    : heights of wind speed measurements
  # imref  : index of reference height in measurements
# OUTPUT:
  # uz     : generic wind profiles
# VARIABLES:
  # lmix     : mixing length
  # PADm   : Plant area Density (spatial mean)
  # PADmc  : Plant area Density (spatial mean cumulative)
  # ihvm   : vegetation height from PADm
# SUBROUTINES:  PADm2lm.R
# REFERENCE: Qu10: Queck R, Bernhofer C (2010) Constructing wind profiles 
  #            in forests from limited measurements of wind and vegetation 
  #            structure. Agricultural and Forest Meteorology, 150:S,724–735 
# REVISION HISTORY:
  # 2014-10    , RQ: created 
  # 2019-12-01 , RQ: xyz -> zyx
  # 2021-01-10 , RQ: something improved but still not satisfying
  # 2022-12-10 , RQ: changed to function type
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
windpro <- function(PAD, zz, z0, hWS, imref, ztop, dz, dy, dx, nz, ny, nx, iyWS=1, ixWS=1){
  # parameters ####
  # PAD=PAD
  # hWS     : Messhoehe(n)
  # zz     : Height vector (Hoehen Raster)
  # z0     : Roughness length (Rauigkeitslänge)
  # ZD <- z0 * 7  (nicht aktiv)     # Displacement height
  # MO <- 99999   (nicht aktiv)     # Monin Obukhov Length
  # hvm: height_vegetation_mean = hvm # calculated in PADmean.R
  ES = 10   # PAD clustering
  fw = 10   # PAD filter width
  
  if (silent < 2) print("- calculate generic wind profiles - windpro.R")

# SUBROUTINES ####
  source(file.path(path_sub,"PADmean.R")) # moving spatial mean for PAD
  source(file.path(path_sub,"PADm2lm.R")) # calculates mixing length from PADm for every voxel

# INPUT ####  
  zWref <- hWS[imref]/100 # +dz      # + bottom layer
  # nztop <- ztop/dz                 # number of z levels up to the reference, assumption of horizontal homogeneity
  # nzu <- zWref/dz                  # number of z levels up to the highest wind measurement
  nztop <- ceiling(ztop/dz)          # number of z levels up to the reference, assumption of horizontal homogeneity
  nzu   <- ceiling((zWref+dz)/dz)    # number of z levels up to the highest wind measurement
  nzur <- max(nztop, nzu, nz)        # 20250212: nz added in case top wind measurements is below the static driver
  zzu <- seq(dz, nzur*dz, by = dz)-dz # heights of the z levels
  # zzu <- seq(0, nzur*dz, by = dz) # heights of the z levels

# Vegetation field and Mixing length ####  
  # PAD=PAD; fw = fw; nx=nx; ny=ny; nz=nz; dx=dx; dy=dy; dz=dz
  PADm  <- PADmean(PAD=PAD, fw = fw, nx=nx, ny=ny, nz=nz, dx=dx, dy=dy, dz=dz)       # spatial mean of PAD
  PADmc <- apply(PADm*dz, c(2,3), function(a){ rev(cumsum(rev(a)))})                 # Cumulative PADm
  
  if (wind.homo){
    ihvm  <- apply(PADmc, c(2,3), function(a){1}) # index of vegetation height
  } else {
    ihvm  <- apply(PADmc, c(2,3), function(a){ if(sum(a)>0) max(which(a > 0)) else 1}) # index of vegetation height
  }
  if ((nz-1)*dz < max(zzu)){              # lokal, das Vegetationsfeld vergrößern # 20250212 "nz" to "nz-1"
    PADme <- array(0, c(nzur,ny,nx))
    PADme[1:nz,,] <- PADm             # t(drop(PADme))
    PADmce <- array(0, c(nzur,ny,nx))
    PADmce[1:nz,,] <- PADmc
  } else {PADme <- PADm; PADmce <- PADmc}
  PADme[1,,] <- 99                  # bottom
  PADmce[1,,] <- 99                  # bottom
  
  if (wind.homo){
    lmix <- PADm2lm(PADme*0,zzu, dz=dz, dy=dy, dx=dx, ES = ES)
  } else {
    lmix <- PADm2lm(PADme,zzu, dz=dz, dy=dy, dx=dx, ES = ES)  # PADme=PADme; zzu=zzu; dz=dz; dy=dy; dx=dx; ES = ES
  }

# Profile parameter ####  
  # friction velocity and roughness length
  # if (!exists("z0"))  {z0 <- ihvm * 0.07}   # Roughness length 0.07
  # if (!exists("z0u")) {z0u <- ihvm * 0.03}  # in-canopy roughness length 0.03
  z0 <- ihvm * 0.07; 
  z0u <- ihvm * 0.01
  z0u[z0u > 0.3] <- 0.3                     
  lmx.min <- 1.2                             # minimum of the roughness corrected mixing length

# Calculation of generic wind profiles ####

  #* Stability ####
  if (!exists("psix")) {psix <- 0}           # stability function
  
  #* Friction Velocity #####
  # = u*/k # normalized friction velocity, is adjusted in Meteo_Wind_prof.R
  # cus: u* parameter, it scales the momentum absorption by the PAD and takes into account the different 
  # momentum absorption of different plant species and canopy structures
  cus <- 0.3                            # 0.8 m, u* PARAMETER (Qu10: between 0.95 and 2.5)
  if (wind.homo){
    usa <- exp(-0.5*cus*(PADmce*0+5))
  } else {
    usa <- exp(-0.5*cus*PADmce)           # friction velocity (Qu10 eq.6) ~ turbulent exchanges coefficient 
  }
  usa[1,,] <- 0                         # destruction of all eddies in the lowest layer
# tst <- usa; summary(tst); rasterImage2(z=t(tst[,,ixt]),z.cex=1)   # Friction velocity
  

  #* wind speed ####
  uz <- array(NA, dim = c(nzur, ny, nx))     
  #plot(c(0,2),c(0,ztop), type="n"); abline(v=c(0,lmx.min), col="gray")    # <<< Test plot
  ix <- 1; iy <- 1
  minb <- function(a, b=0.001){max(a,b)}
  for(ix in 1:nx) {
    for(iy in 1:ny) {
      # c1 <- ihvm[iy,ix]/2                                  # z0 parameter: half the vegetation height
      # c2 <- ihvm[iy,ix]/10                                 # z0 parameter
      # zr <- ((z0u[iy,ix]-z0[iy,ix])/(1+exp((zzu-c1)/c2))+z0[iy,ix]) # variable roughness length: Qu10, eq.10
      c1 <- ihvm[iy,ix]/4                                  # z0 parameter: height of the switch between z0u and z0
      c2 <- ihvm[iy,ix]/8                                 # z0 parameter: strength of the change
      (zr <- (z0u[iy,ix]-z0[iy,ix])/(1+exp((lmix[,iy,ix]-c1)/c2))+z0[iy,ix]) # variable roughness length: modified
      zr <- sapply(zr,minb)
      # lines(zr, zzu)
      lmx <- lmix[,iy,ix]/zr                                # l/z0, log - argument     
      lmx[lmx < lmx.min] <- lmx.min                       # no values lower 1, because uz ~ ln(1) = 0
      lmx[is.na(lmx)] <- lmx.min                          # no NA's
      # lines(lmx, zzu)
      uz[,iy,ix]  = (usa[,iy,ix]*(log(lmx)+psix)) # (5)   # Windprofil: Qu10 eq.3
      # assumption of horizontal homogeneity at ztop
      uz[,iy,ix]  = uz[,iy,ix]/uz[nztop,iy,ix]        
      #lines(uz[,iy,ix], zzu, col=2, lwd=2)
      # Sys.sleep(.1)
      # lines(uz[,iy,ix], zzu, col="lightgray", lwd=3)
      # lines(uz[,iy,ix], zzu, col="gray")
    }
  }
  uz[1,,] <- 0

  # normalize with reference height
  # um2utop.fak <- log ( (ztop-zd.ref) / z0 ) / log( (hWSref-zd.ref) / z0 ) # log profile not applicable
  uzref <- approx(zzu, uz[,iyWS[imref], ixWS[imref]], zWref)$y
  # uz[nzur,iyWS[imref], ixWS[imref]] = 1 
  um2utop.fak <- 1/uzref
  
  uz <- uz*um2utop.fak
  
  #rm(PADme, PADmce)
  return(list(uz=uz, usa=usa, zzu=zzu))
  
}
  


  
