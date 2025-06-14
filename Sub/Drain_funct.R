#+
# NAME*: Drainage
# PURPOSE: Drainage calculation after Rutter (1971)  => see also Rutter and Morton 1977
  # Basics:
  # D(t) = 0, fuer C(t) <= S
  # D(t) = exp(a + b * (C(t) - S)), für C(t) > S
  # D(t) = log(Dmin) * exp(b * (C(t) - S))
  # D <- Ss * Dmin/S_0 * exp( b*S_0 * (C-Ss)/Ss )   <<< scalable draining function
# CALLING: source(file.path(path_sub,"Drain_funct.R"))
# INPUT:
  # Dmin, mm/min miniale Drainage
  # Con : water content in the storage
  # Sat : saturation of the storage
  # D_matrix : Drainage of the storage
# Output:
  # D_matrix : mm/s, drainage of the storage
# EXAMPLE: 
  # Con <- Cmaxe; Sat <- S_3D[iCmaxe]; D_matrix <- 0
  # Drain_funct(Con, Sat, D_matrix)
  # Con <- 0.2*(1:20); Sat <- rep(0.2,20); D_matrix <- 0
  # (Con <- 0.2*(1:20))
  # plot(Con,Drain_funct(Con), log="")
# REFERENCE:
# REVISION HISTORY*:
  # 2019-10-02 , RQ:
  # 2019-10-14 , RQ: distribution moved to Drainage-distribution.R
  # 2019-10-24 , RQ: scale-able draining function
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Drain_funct <- function(Con=1, Sat=2.0, Dmin=0.002, bd=4.6, D_matrix=0){
  Dmax <- 10  # mm/s, maximal drainage from a cell (in a one layer model, this is the whole canopy)
  
  nCon <- length(Con); nSat <- length(Sat); nD_matrix <- length(D_matrix)
  if (nSat == 1) Sat <- rep(Sat,nCon)
  if (nD_matrix == 1) D_matrix <- rep(D_matrix,nCon)
  D_matrix <- D_matrix*0
  idrain <- which(Con > Sat)
  # idrain <- 1:length(Con)
  # the formula of Rutter gives the drainage rate in in mm/min, thus it must be divided by 60 s/min 
  if (length(idrain) > 0) D_matrix[idrain]  <- Sat[idrain] * Dmin * exp(bd * (Con[idrain] - Sat[idrain])/Sat[idrain] ) / 60  
  
  D_matrix[idrain][is.infinite(D_matrix[idrain]) | D_matrix[idrain] > Dmax] <- Dmax
  # if (length(idrain) > 0) D_matrix[idrain]  <- Dmin * exp(bd * (Con[idrain] - Sat[idrain]) ) / 60  
  
  return(D_matrix)
}

# round(range( (Con[idrain] - Sat[idrain])/Sat[idrain]),3)
# round(range( Sat[idrain] * Dmin),3)
# round(range( exp(bd * (Con[idrain] - Sat[idrain])/Sat[idrain] )  ),3)
# round(range( exp(bd * (Con[idrain] - Sat[idrain])/Sat[idrain] ) / 60  ),3)
# round(range( Sat[idrain] * Dmin * exp(bd * (Con[idrain] - Sat[idrain])/Sat[idrain] ) / 60  ),3)

## Test ####
# ntest <- 1              # number of layers
# ctest <- rep(2/ntest,ntest); sum(ctest)      # aktuelle Füllung,   ctest <- (1:ntest)/100
# stest <- rep(1/ntest,ntest); sum(stest)      # Speicherkapazität
# # bd*(ctest-stest)/stest
# l1 <-  Drain_funct(Con=ctest, Sat=stest, Dmin=Dmin, bd=4.6, D_matrix=0)
# sum(l1)
# 
# ntest <- 2; ctest <- rep(2/ntest,ntest); stest <- rep(1/ntest,ntest); sum(ctest); sum(stest)
# l2 <-  Drain_funct(Con=ctest, Sat=stest, Dmin=Dmin, bd=4.6, D_matrix=0)
# sum(l2)
# 
# # das schein noch nicht zu funktionieren
# Con=C_3D; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage



# plot(ctest, Drain_funct(ctest, stest,  rep(1,ntest)), ylim=c(0, 5))
# Dmin <- Dmin0; bd <- bd0
# lines(ctest, Drain_funct(ctest, stest, rep(1,ntest)))
# Dmin <- Dmin0/S_0; bd <- bd0*S_0

# old function, not scalable ?
# Drain_funct <- function(C_3D, S_3D, Drainage){
#   Drainage <- Drainage*0
#   idrain <- which(C_3D > S_3D)
#   # the formula of Rutter gives the drainage rate in in mm/min, thus it must be divided by 60 s/min 
#   if (length(idrain) > 0) Drainage[idrain]  <- Dmin * exp(bd * (C_3D[idrain] - S_3D[idrain])) / 60  
#   return(Drainage)
# }
