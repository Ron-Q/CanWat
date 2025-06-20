#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+
# NAME*: Meteo_Rn.R
# PURPOSE*: Radiation transfer within canopy (approach: Beers law)
  #         for net radiation
  #         
# RELEVANCY*: CanWat
  # CALLING SEQ.:  source(file.path(path_sub,"Meteo_Rn.R",sep=""))
  # CALLED BY:  CanWat.r
  # INPUTS*:
  # Rndat : net radiation data 
  # PADc  : cumulative plant area index
  # zz    : heights
                      # extinct    : 0.5, Extinktionskoeffizient
# OUTPUT*:
  # Rn_3D
# EXAMPLE:
# REFERENCE: MU08: Monteith JL, Unsworth MH (2008) Principles of environmental physics. Academic Press 
# REVISION HISTORY*:
  # 01/2021 (RQ): created 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meteo_Rn <- function(Rndat, Rnheights, Rg_3D, Rgtop, Ts_3D, Tatop, DDtop, PAD, PADc, pRtrans, dz, nz, Rn_3D, padat){
# check input 
               Rnheights <- hRn/100 
               # cat(Rndat,   hRn/100       , Rgtop       , Tatop, DDtop                    , dz, nz)
               # round(range(Rg_3D)); round(range(Ts_3D))
               # round(range(pRtrans)); round(range(Rn_3D))
  # if (Rndat<0) Rndat=0
  Rndat <- as.numeric(metRn[ito,Rn_nam, with=F])
  Rnheights <- hRn/100
  izRn <-  Rnheights/dz
  
  # extinct <- 0.5 # Extinction coefficient  => Parameter
  alph <- 0.1  # Albedo, coniferous: 0.05 .. 0.15, deciduous 0.15 .. 0.2 (Oke 1987, p.12)
  epsi <- 0.97 # Emissivity of the vegetation (Oke 1987, p.12)
  
  # regard the soil and canopy heat flux
  Rndat <- Rndat*(1-fSHF2Rn)
  
  
  lRn <- length(Rndat)
  if (lRn > 1) Rntop <- Rndat[lRn] else Rntop <- Rndat
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rntop: radiation that is captured by the stand
  # But WHERE?
  
  if (bigleaf){

        
    #* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #* Strahlungsverteilung muss überprüft werden
    #* Aktuell besser nur Rechnungen mit 1 Layer machen
    #* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    # APPROACH 0) -----------------------------------------------------
    Rn_3D[1,,] <- 0
    Rn_3D[2,,] <- Rntop
    Rn_3D[3,,] <- 0
        
    # APPROACH A) -----------------------------------------------------
    # Rnext <- Rntop*pRtrans      # extinction after Beers law
    # Rn_3D[1,,] <- Rnext[1,,] # absorbed radiation in the lowest layer
    # for (ir in 2:(nz)){      # absorbed radiation in the single layers
    #   Rn_3D[ir,,] <- Rnext[ir,,]-Rnext[ir-1,,]
    # }

  } else {
    
    # APPROACH b) ---------------------------------------------------
    # was ist in einer bestimmten Schicht reingegangen? => kurzwelliger Einstrahlung
    # was ist in einer bestimmten Schicht rausgegangen? => langwellige Austrahlung
    #                                   hängt von der Gegenstrahlung ab
    # longwave radiation depends on the temperature of the canopy
    # Rli ~ sigma * sum(sign(direction) Ti^4)
    # man könnte das Temp-feld ^4 nehmen, dann könnte man für jeden Punkt in alle Richtungen die Indexe mit Richtung und Raumwinkel ablegen, 
    # und daraus die einfallende langwellige Einstrahlung berechnen.
    # man kann auch Annehmen, dass der Bestand und Boden gleichwarm ist, dh. darin kein effektiver Austausch stattfindet
    # nur nach oben => d.h. der Anteil der aus einer Schicht nach oben durchdringen und raus kann (Beersches Gesetz),
    # der ist in der bestimmten Schicht verloren gegangen.
    # ich brauche also den PADc von unten gesehen, und das aus jeder Schicht.
    # damit kann ich einmalig den Restanteil berechnen, nach oben entfleucht.
    # diese Restandteile stelle ich als Feld dar und multipliziere sie mit der ausgehenden Strahlung in der Schicht
    
    # Incoming Radiation shortwave ####
    # see Meteo_Rg.R
    # Rs.in <- Rg_3D
    # Rgtop: measured shortwave radiation
    # ~~~~ distribution of shortwave radiation ~~~~ 
    alphC <- alph #*1.3
    Rs.in <- array(dim=dim(Rg_3D))   # absorbed radiation
    Rs.rfl <- array(dim=dim(Rg_3D))  # reflected radiation
    ir <- 1
    for (ir in 1:(nz-1)){
      Rs.in[ir,,] <- (Rg_3D[ir+1,,]-Rg_3D[ir,,])*(1-alphC)
      Rs.rfl[ir,,] <- (Rg_3D[ir+1,,]-Rg_3D[ir,,])*(alphC)
    }
    Rs.in[nz,,]  <- (Rgtop-Rg_3D[nz,,])*(1-alphC)
    Rs.rfl[nz,,] <- (Rgtop-Rg_3D[nz,,])*(alphC)
    
    # Outgoing Radiation shortwave ####
    Rs.out <- Rs.rfl*pRtrans
    # this is certainly below the measured values
        

    # Incoming Radiation longwave (from the Sky) ####
    # as a function of temperature, vapour pressure and cloud cover 
    Tair <- Tatop+273.15 # K, temperature near the ground
    # DDtop
    # ~~~~ Emissivity - cloudless sky ~~~~
    # Rl.skyest <- -119 + 1.06 * 5.6698e-8 * Tair^4 # estimate for cloudless sky
    # MU08,p76
    a <- 0.1; c <- 0.3 # m^2/kg
    b <- 1.2 
    w <- 4.65*(DDtop*100)/(Tatop+273.15)    # in kg/m^2, precipitable water content of the atmosphere
    epsi.a <- 1 - (1+a*w)*exp(-sqrt(b+c*w)) # - , emissivity of air 
    # ~~~~ Shortwave Radiation of a Clear Sky  ~~~~ 
    # Rg0 <- Rgext(UTC, psi) 
    SolarC = 1366  # W/m^2 solar constant
    latitude <- 51.1 ;    longitude <- 13.7               # <===== INPUT
    phi <- latitude/180*pi ;  lambda  <- longitude/180*pi # 
    # psi, zenith angle ~~~~
    (doy <- as.numeric(strftime(metPP$UTC[ito], format = "%j")))# ; par(new=T); cex<-2
    (Stunde <- hour(metPP$UTC[ito]) + minute(metPP$UTC[ito])/60  - 1)  # <====== "-1" wegnehmen wenn auf UTC umgestellt wurde
    delta <- asin( 0.39785*sin( (278.97+0.9856*doy+1.9165*sin((356.6+0.9856*doy)/180*pi))/180*pi ) ) # solar declination (annual cycle)
    theta <- Stunde*pi/12 + lambda - pi     # hour angle of the sun (diurnal cycle) after local noon ("-pi" shifts origin from midnight to noon)
    psi <- acos( sin(phi)*sin(delta) +  cos(phi)*cos(delta)*cos(theta) )
    # potential radiation ~~~~
    amn <- padat/1013/cos(pmin(psi, 90/180*pi))   # air mass number, MU08p60 
    tauM <- 0.3 # optical thickness for molecular attenuation
    tauA <- 0.05 # 0.05 .. 0.5, aerosol optical thickness
    Sdir = SolarC *exp(-tauM*amn)*exp(-tauA*amn)                                   # direct solar radiation
    Sdiff = 0.3*SolarC * cos(psi) * (1-exp(-(tauM+tauA)*amn) ); Sdiff[psi>pi/2]<-0 # diffuse solar radiation
    Rg0 <- Sdir*cos(psi)+Sdiff      # potential Radiation
    if (Rg0 < Rgtop) Rg0 <- Rgtop
    
    # ~~~~ Cloud Fraction   ~~~~
    # cloud fraction after (DyPe95p31e3.13) ~~~~~
    ac <- 1;    bc <-  0       # humid regions
    #  ac <- 1.35; bc <- -0.35 # arid regions
    if (Rg0 != 0) clf <- 1-(ac*Rgtop/Rg0 + bc)
    # ~~~~ longwave radiation from cloudy sky  ~~~~
    # assume dTac = 11 K and Tair around 283 K
    epsi.ac <- (1-0.84*clf)*epsi.a + 0.84*clf # - , emissivity of a partly clouded sky  
    Rl.skyC <- epsi.ac * 5.6698e-8 * Tair^4  #  (MU08p78)

    # Outgoing Radiation longwave ####
    Rl.vegpot <- (epsi * 5.6698e-8 * (Ts_3D + 273.15)^4) # potential emission
    Rl.veg <- Rl.vegpot*(1-exp(-PAD*dz))                    # emission reduced by the available surfaces in that layer
    Rl.veg[1,,] <- Rl.vegpot[1,,]                        # emission from the lowest layer is not reduced by PAD
    # part of that radiation which could be transmitted through the canopy - from each voxel to the sky
    Rl.out  <- Rl.veg*pRtrans                          
    # that is probably not exact Rl.vegpot of this column, but a reasonable distribution of Rl.out from the canopy 

    
    # Net Radiation in the canopy ####
    Rl.net <- array(dim=dim(Rg_3D))
    Rs.net <- array(dim=dim(Rg_3D))
    # Rs.outc <- array(dim=dim(Rg_3D))
    for (i in 1:nx) {
      for (j in 1:ny) {
        Rl.net[,j,i] <- - Rl.out[,j,i]/sum(Rl.out[,j,i])*(mean(Rl.vegpot[,j,i])-Rl.skyC)  # longwave net radiation
        RsinS <- sum(Rs.in[,j,i])
        if (RsinS == 0) Rs.net[,j,i] <- 0 else {
          Rs.net[,j,i] <-   Rs.in[,j,i]/RsinS*Rgtop*(1-alph)   # shortwave absorbed (net radiation)
          # Rs.outc[,j,i] <- Rs.out[,j,i]/sum(Rs.out[,j,i])*Rgtop*(alph)  # shortwave reflected
        }
      }
    }   
    Rn_3D <- Rl.net+Rs.net
    
    # BALANCE over all layer and mean over the columns ####
    (Rnet3D <- sum(Rn_3D)/area_xy); Rntop
    # there is still a small deviation between Rn_top and , to adjust later
    Rn_3D <- Rn_3D*Rntop/Rnet3D
    
  }
  
  if (silent < 1) print(paste("Rntop: ",round(Rntop),"; Rn_3D:", round(sum(Rn_3D)/area_xy)))
  
  return(Rn_3D)
}




if(F){
  # ==> Check the balance with measurements
  (Rnet3D <- sum(Rn_3D)/area_xy)
  
  if (silent < 1) print(paste("Radiation",round(Rntop), round(Rnet3D)))
  
  dim.Rn <- dim(Rn_3D)
  plot(Rn_3D[,1,1], zz)
  for (i in 1:dim.Rn[1]){
    lines(Rn_3D[,1,i], zz, col=i)
  }
  
}
