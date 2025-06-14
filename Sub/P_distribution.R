#+
# NAME: P_distribution
# PURPOSE: 
  # precipitation distribution within the canopy into throughfall and interception
  # T(t) = pTc * PF, durchtropfender Anteil
  # I(t) = pIc * PF, interzipierter Anteil 
# RELEVANCY*: CanWat subroutine
# CALLING SEQ.: source(file.path(path_sub,"P_distribution.R"))
# INPUTS*: 
  # pTc, pIc  : distribution matrixes
  # PF        : gross precipitation
# OUTPUT*:
  # Pthroughf, Pintercept
# REFERENCE:
# REVISION HISTORY*:
#   2019-10-10 : RQ
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (silent < 2) print("- vertikal distribution function for precipitation and drainage")

  # durchtropfender Niederschlag  
  Pthroughf <- array(pTc[,nz,,],c(nz,ny,nx)) * PFi   # mm/s
  
  # interzipierter Niederschlag
  Pintercept <- array(pIc[,nz,,],c(nz,ny,nx)) * PFi   # mm/s

  # check
  # (Pthroughf[,1,1]   + sapply(as.data.table(t(drop(Pintercept[,1,]))), sum) )/PFi   # should be 1
  # plot(c(0,PFi), c(1,42), type="n")
  # for (i in 1:nx) for (j in 1:ny) lines(Pthroughf[i,j,],1:42)                       # should increase with height
  # plot(c(0,PFi), c(1,42), type="n")
  # for (i in 1:nx) for (j in 1:ny) lines(Pintercept[i,j,],1:42)
  
