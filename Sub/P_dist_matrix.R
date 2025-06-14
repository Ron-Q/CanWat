#+
# NAME*:    P_dist_matrix.R
# PURPOSE*: Calculate the vertical distribution of the gross precipitation within the canopy, 
  # based on the PAD. It provides vectors with fractions of through-fallen and intercepted precipitation
  # , but also fractions of through-fallen and intercepted Drainage.
  # Method:
    # pT ~ LAI                    # 3D-array: the fraction that is NOT caught by surfaces in layer i
    # pTc[n] = pT[n] * PF         # 4D-array: cumulative products of the through-fall fractions
    # pTc[i] = pT[i] * pTc(i+1) 
    # pI = 1 - pT
    # pIc[n] = pI[n] * PF         # 4D-array: cumulative products of the intercepted fractions
    # pIc[i] = pI[i] * pIc[i] 
# Starting from each z-layer, fields for falling water and interception are calculated.
# Assumption: The degree of crown closure in a layer first increases rapidly with increasing LAI,
#             until almost everything is dense, but then more and more slowly 
#             a 100% interception of the P is never achieved or only at extrem high LAI
#             => exponential relationship
# RELEVANCY*: Interception
# CALLING SEQ.: source(file.path(path_sub,"Coefficients.R",sep=""))
# INPUTS*: PAD
  # nz  : index of domain top
# OUTPUT*: p_3D_co, q_3D_co => pTc, pIc    (p_3D => pT, q_3D => pI)
#         pTc, pIc : 4D-arrays [nz,nx,ny,nz], for each z-Layer one 3D-array
# REFERENCE:
# REVISION HISTORY*:
  # 2019-08-12 : RQ, isolated from "Coefficients.R" ++
  # 2022-12-10 , RQ: changed to function type
#  
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P_dist_matrix <- function(PAD, cc1, cc2, nz){

  pTc <- array(0,c(nz,nz,ny,nx))              # pTc[k,l,j,i] probability for throughfall of rain from point [l,j,i] in layer k
  pIc <- array(0,c(nz,nz,ny,nx))              # pIc[k,l,j,i] probability for intercepting of rain from point [l,j,i] in layer k
    
  # throughfall in layer i;  pT ~ PAD
  if (!exists("pTf")){
    # cc1 = 0.195 seams very small, it is a result from the optimization by Martin Kunath
    # cc2 = 0
    # a.test <- (1:20)/2
    # cc1 <- 0.195; cc2 <- 0
    # plot(a.test, exp(-(cc1*a.test + cc2))  , type="l", ylim=c(0,1), xlab="PAD", main="Throughfall Coefficient")
    # points(a.test, exp(-(cc1*a.test + cc2)) , pch=16)
    # points(a.test, exp(-(0.1*a.test)) , col="blue", pch=16)  # maximal troughfall
    # points(a.test, exp(-(1.4*a.test)) , col="red" , pch=16)  # minimal troughfall
    if ((cc1 < 0.1) | (cc1 > 1.4)) (stop("cc1 is out of range"))
    if ((cc2 < -0.05) | (cc2 > 0.1)) (stop("cc2 is out of range"))
    pT <- exp(-(cc1*PAD*dz + cc2))       # throughfall distribution
    # probability that water falls through a layer
    # plot(pT[1,1,],1:nz, type = "l"); for (xi in 1:nx) for (yi in 1:nx) lines(pT[xi,yi,],1:nz)
    # t(drop(pT))
  } else {
    pT <- pTf(PAD*dz, cc1, cc2)
  }

  
  # Interception in Layer i 
  pI = 1-pT                         # Interception Distribution     
  # plot(pI[1,1,],1:nz, type = "l"); for (xi in 1:nx) for (yi in 1:ny) lines(pI[xi,yi,],1:nz)
  itop <- nz # itop <- itop-1
  pTc[,itop,,] <- 1
  for (itop in nz:2){   # itop <- j     
    # for each z-Layer one 3D-array is calculated
    # topmost layer: NO vegetation, everything falls through pT[itop] = 1
    (pTc[itop,itop,,] <- 1)
    (pIc[itop,itop,,] <- 0)      
    # following layers: throughfall from the layer above times the probability 
    #         that the precipitation is catched or not within the actual layer
    i <- (itop-1) # i <- i-1
    for (i in (itop-1):1) {
      pTc[i,itop,,] <- pTc[i+1,itop,,] * pT[i,,] 
      pIc[i,itop,,] <- pTc[i+1,itop,,] * pI[i,,] 
    }
  }  
  pTc[1,1,,] <- 1 # Drainage from the lowest layer can not be 
  pIc[1,1,,] <- 0 # intercepted within the model domain
  
  return(list(pTc=pTc,  pIc=pIc))
  
  
  
  # fine ##############################################################
  
  # Test
  # df <- as.data.frame(pTc[1,1,,])
  # View(df)
  # der aus der untersten Schicht fallende Anteil sollte der "Throughfall-rate entsprechen
  # die Summe sollte dem NICHT durchfallenden Anteil entsprechen
  # pTc[,1,1,nz] + sapply(as.data.table(t(drop(pIc[,1,,nz]))), sum)
  
  # for (l in 1:41) cat(l, ", ", pTc[,1,1,l] + sapply(as.data.table(t(drop(pIc[,1,,l]))), sum), " \n" )
  # 
  # t(drop(pTc[,1,,1]))
}
