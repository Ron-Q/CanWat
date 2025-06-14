#+
# NAME*: PADmean
# PURPOSE: spatial mean of PAD
  # separated from Load_Static_Driver.R due to variable filter calculation
# INPUT: 
  # PAD    : Height vector (Hoehen Raster)
# OUTPUT:
  # PADm   : Plant area Density (spatial mean)
  # PADmc  : Plant area Density (spatial mean) cumulative
# REVISION HISTORY:
#  10/2014 (RQ): created 
#  01/2020 (RQ): new filter calculation 
#-

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PADmean <- function(
  PAD=PAD 
  , fw = 5  # width of the filter 5 m
  , asra = 1.1  # aspect ratio of the filter-widths
  , nx=nx, ny=ny, nz=nz
  , dx=dx, dy=dy, dz=dz
){
  
  PADmh <- array(NA, dim = c(nz, ny, nx))
  # spatial averaging (mean-filter) ####
  # horizontal
  fw2h <- floor(fw/(dx+dy))
  if (fw2h < 1) fw2h <- 1 
  for (i in 1:nx) {
    for (j in 1:ny) {
      for (k in 1:nz) {
        # i1 <- i; i2 <- i; j1 <- j; j2 <- j   # no change
        if (i > fw2h)     i1 <- i-fw2h  else  i1 <- 1
        if (i < nx-fw2h)  i2 <- i+fw2h  else  i2 <- nx
        if (j > fw2h)     j1 <- j-fw2h  else  j1 <- 1
        if (j < ny-fw2h)  j2 <- j+fw2h  else  j2 <- ny
        # cat("k: ",k,"j: ",j,j1,j2,"i: ",i,i1,i2, "\n")
        PADmh[k,j,i] <- mean(PAD[ k, j1:j2, i1:i2 ])
        
      }
    }
  }

  PADm <- PADmh
 
  # vertical
  fw2v <- floor(fw/(asra*dz*2))
  # if (fw2v < 1) fw2v <- 1
  for (i in 1:(nx)) {
    for (j in 1:(ny)) {
      for (k in 1:nz) {
        
        if (k > fw2v)     k1 <- k-fw2v  else  k1 <- 1
        if (k < nz-fw2v)  k2 <- k+fw2v  else  k2 <- nz
        PADm[k,j,i] <- mean(PADmh[ k1:k2, j, i ])

      }
    }
  }

  PADm <- PADm*sum(PAD)/sum(PADm)
  return(PADm)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# following functions are deleted
# PADcum: Cumulative PAD
# iHveg : index of vegetation height ####
# they are already calculated in "Load_Static_Driver.R
