#+
# NAME*: Drainage_distribution
# ==> prepared to be a function, but currently still a source chunk <==
#
# PURPOSE: partitioning of drainage into throughfall and re-interception
  # The drainage, which was calculated within Runge-Kutta-approach.R by Drain_funct.R, 
  # is distributed into a Dthroughf and Dintercept fields
  # this is analogous to throughfall calculation (Pthroughf, Pintercept)
  # however here we have a source in each layer
  # each voxel creates a individual drainage, which must be distributed on the voxel below. 
# CALLING: 
  # source(file.path(path_sub,"Drainage_distribution.R"))
  # Drainage_dist <- Drainage_distribution(Drainage, Dthroughf, Dintercept, nx, ny, nz)  # function call
# INPUT:
  # Drainage : Drainage
  # pTc, pIc : distribution matrices 
# OUTPUT:
  # Dthroughf :
  # Dintercept : 
# REFERENCE:
# REVISION HISTORY*:
#   2019-10-02 : RQ
#   2021-01-28 : RQ inefficiently use of indizes 
#       currently ix is the fastest, that should be changed again!!! 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Drainage_distribution <- function(Drainage, Dthroughf, Dintercept, nx, ny, nz){

  # reception of the last time step is set 0 anyway
  Dthroughf <- Dthroughf*0; Dintercept <- Dintercept*0 
  
  if (sum(Drainage) > 0) {
    # partitioning of the drainage from the last time-step into interception and throughfall
    iz <- 31; iy <- 1; ix <- 1  # test values
    numFehler <- 0
    for (ix in 1:nx){     # slowest index
      for (iy in 1:ny){
        for (iz in nz:1){ # these cells should be near together on the storage
          # print(paste0(ix," ",iy," ",iz))
          # print(Drainage[ix,iy,iz])
          Dthroughf[,iy,ix]  <- Dthroughf[,iy,ix] +  (pTc[,iz,iy,ix] * Drainage[iz,iy,ix])    
          Dintercept[,iy,ix] <- Dintercept[,iy,ix] + (pIc[,iz,iy,ix] * Drainage[iz,iy,ix])
          # numFehler <- numFehler +  ((pTc[,iy,ix,iz] * Drainage[iz,iy,ix])[1] + sum(pIc[,iy,ix,iz] * Drainage[iz,iy,ix])) - Drainage[iz,iy,ix]
          # dtf <- (pTc[ix,iy,,iz] * Drainage[ix,iy,iz])[1]
          # din <- sum(pIc[ix,iy,,iz] * Drainage[ix,iy,iz])
          # if ( (dtf + din) - Drainage[ix,iy,iz] > 0.000000001 ) {cat("error"); stop()}
        }
      }
    }
  }
#}