#    !!!! still in development !!!!
#+
# NAME*: Meteo_Wind_prof
# PURPOSE*: use the generic wind profiles and wind measurements 
#           to calculate wind profiles
#           currently only the reference wind speed is regarded
# CALLING:  source(file.path(path_sub,"Meteo_Wind_prof.R",sep=""))
# Input:
  # um     : measured wind speed
  # hWS     : measurement heights
  # uz     : generic wind profiles (calculated in "windpro.R")
  # Parameter ----
  # PAD   : Plant area Density (spatial mean)
  # lmix     : mixing length
# Output:
  # u_3D   : Windprofil mit gemessener Windgeschwindigkeit normiert
  # usm    : Friction velocity
# RESTRICTIONS:
  # only for dz >= 1m 
  # only for measurments at one profile
# REVISION HISTORY:
  #  10/2014    RQ : created 
  #  2019-10-24 RQ : variable heigths of the measurements
  #  2019-12-03 RQ : measurement profiles
  #  2021-01-10 RQ : something improved but still not satisfying
  #  2021-01-10 RQ : moved um2utop.fak to windpro()
  #-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # measurements
  um <- as.numeric(metWS[ito, ..WS_nam])
  h.um <- hWS/100    # measurement heights in m
  if (dim(uz)[1]*dz < max(round(hWS/100), na.rm=T) ) stop() # check reach the generic wind profiles up to the measurements  

  # friction velocity at reference height  
  usm.ref <- um[imref]*0.41/log((h.um[imref]-hveg*zd.fak)/(hveg*z0.fak))
  usm <- usa*usm.ref  # profile
  
  # Wind field
  uzm <- uz*um[imref]
  u_3D <- uzm[1:nz,, , drop = FALSE]
  u_3D[u_3D == 0] <- 0.001

  usm <- usm[1:nz,, , drop = FALSE]
  usm[usm == 0] <- 0.001

  
  
  #####################################################
  # multiple scaling was placed here
  # nm <- length(hWS)
  # zmu <- round(hWS/100) # +dz   # heights in m + bottom layer
  # izWS <- round(zmu/dz)
  # ymu <- iyWS*dy
  # xmu <- ixWS*dx
  # zmtop <- max(zmu)
  # zzu <- seq(dz, nzur*dz, by = dz) # heights of the z levels => already calc. in windpro.R
  # fu <- vector(mode = "numeric", length = length(zmu))
  # # PADmm <- fu
  # for (i in 1:nm) {  # over all measurements
  #   # fu[i] <- um[i]/(uz[izWS[i], iyWS[i], ixWS[i]]*um[imref]) # deviations of the single measurements
  #    fu[i] <- um[i]/(uz[izWS[i], iyWS[i], ixWS[i]]*um2utop.fak*um[imref]) # deviations of the single measurements
  #   # assumption: the error depends on PADm and height <=== das geht so nicht
  #   # PADmm[i] <- PADme[izWS[i], iyWS[i], ixWS[i]]
  # }
  # # PAD dependence of the error - very experimental !!!
  # # PADdum <- (1:10)*0.08
  # # plot(PADmm, fu, ylim=c(0,2));  lines(PADdum, exp(PADdum*6)/200+1 )
  # # cfak <- (exp(PADme*6)/200+1)
  
  
  
  # Test
  # # check
  # for (i in 1:nm) fu[i] <- um[i]/(uzm[izWS[i], iyWS[i], ixWS[i]])
  # plot(fu, zmu)
  # 
  # # height dependence of the error
  # fup <- approx(zmu, fu, zzu, method = "linear", rule = 2)$y
  # # plot(fup, zzu)
  # 
  # for (i in 1:nz) {
  #   usm[i,,] <- usa[i,,] * fup[i]
  #   uzm[i,,] <- uzm[i,,] * fup[i]
  # }
  
  # plot(um,zmu,pch=16, xlim=c(0,7), ylim=c(0,70))
  # points(u_3D,zzu, pch=16, col=2)
  # dev.set(5)
  # tst <- u_3D; summary(tst); rasterImage2(z=t(tst[,,3]),z.cex=1)
  # 
  # uzm2 <- uz
  # for (i in 1:42) uzm2[i,,] <- uz[i,,] * fup[i]
  # tst <- uzm2; summary(tst); rasterImage2(z=t(tst[,,3]),z.cex=1)
