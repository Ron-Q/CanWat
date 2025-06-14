#+
# NAME*: ETR_funct.R
# PURPOSE*:# calc resistance networks based on the relative filling of the canopy storage, sc 
          # apply Penman-Monteith
          # ETr =   ((s * (Rn - G)) + (rhoa * cp * vpd / rH)) 
          #        / (L * (s + gam * (1 + (rLE / rH))))
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"ETR_funct.R"))
# INPUTS*:
  # Rn_3D, DDs_3D, DD_3D, 
  # iPADeq0 : PAD == 0
  # rHt_3D, rHw_3D, rH_3D, rLEt_3D, rLEw_3D, rLE_3D  # resistances
# OUTPUT*:
  # L_3D, rhoa_3D, des_3D
  # ETR, ET_t, Ev_w
# SUBROUTINES: to call before ET.R
#        source(file.path(path_sub,"Humidity.R"))
#        source(file.path(path_sub,"ET_resistances.R"))
# REFERENCE:
# REVISION HISTORY*:
#   2019-10-02 : RQ 
#   2025-01-22 : RQ: assumption: ET is always lower than EV (if there is water on the leaves)
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ETR_funct <- function(C_3D) {
#* saturated part of storage capacity (sc) ####
  # additional assumption: sc0 equals the partly saturation of the laminar boundary layer
  sc0[iPAD] <- (C_3D[iPAD])/(Sp*S_3D[iPAD])
  sc0[iPADeq0] <- 0        
  sc0[sc0 > 1] <- 1              # 1 is complete saturation of the surface near air
  # sc0[(sc0 < sc.min) & (sc0 != 0)] <- sc.min 
  sc0[C_3D == 0] <- 0
  # sc.f <- 2 ; sc.min <- 0.02; sc0 <- (0:100)/100;  plot(sc0, (1-exp(-((1-sc.min)*sc0+sc.min)*sc.f))/(1-exp(-sc.f))  , ylim=c(0,1)); abline(a=0, b=1, h=c(0,1), v=c(0,1))
  # sc1 <- (1-exp(-sc0*sc.f))/(1-exp(-sc.f))
  sc1 <- (1-exp(-((1-sc.min)*sc0+sc.min)*sc.f))/(1-exp(-sc.f))
  sc1[C_3D==0] <- 0

#* Resistances modification by surface wetness ####
  #* Sensible heat flux  
    # rHw_3D <- rH_3D/sc       # resistances for the sensible heatflux are not affected by wetness
    # rHt_3D <- rH_3D/(1-sc)
  #* for Transpiration ET
  gLEt_3D <- (1-sc1)/rET_3D     # (1-sc1) Reduction of the transport cross-section to the area that is not wetted
  gLEt_3D[iPADeq0] <-  1/Rinf
  #* for Evaporation EV
  gLEw_3D <-  sc1/rw_3D
  gLEw_3D[iPADeq0] <-  1/Rinf
  
  #* Total resistances for evapotranspiration ETR
  # rw in parallel to rc and rb: rcf=1/rw+1/(rc+rb)
  # rLE_3D <- 1/( sc1/(rb_3D + rab) + (1-sc1)/(rc_3D + rb_3D + rab) ) # (17)
  # rLE_3D <- 1/( sc1/rw_3D + (1-sc1)/rET_3D )
  rLE_3D <- 1/( gLEw_3D + gLEt_3D )
  rLE_3D[iPADeq0] <-  Rinf
  
#* Evapotranspiration under consideration of leaf wetness in kg/(s*m²) = mm/s
  # ETR <- ( (des_3D * Rn_3D)  +  (rhoa_3D * cp * (DDs_3D - DD_3D) / rH_3D) ) / 
  #           (L_3D * (des_3D + (gam * (1 + (rLE_3D / rH_3D) ) ) )  )
  LE <- ( (des_3D * Rn_3D)  +  (rhoa_3D * cp * (DDs_3D - DD_3D) / rH_3D) ) / 
                  ( (des_3D + gam * rLE_3D / rH_3D ) )   
  ETR <- LE/L_3D 
  # if (any(ETR<0)) stop("ETR < 0")
  # i.e. condensation on the wet leaves ... like dew fall
  # this happens if Rn<0, then both of turbulent fluxes are also negative
  # as the radiation calculation is currently under development still
  # these cases ETR is set to 0
#  ETR[ETR < 0] <- 0 #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Tau zugelassen !!!!!!!!!!!!!!!!!!!!!
  ETR[iPADeq0] <- 0
  
  # Separation of transpiration and evaporation by approximation 
  ET <- gLEt_3D * rLE_3D * ETR # transpiration from stomata
  EV <- gLEw_3D * rLE_3D * ETR # Evaporation from the interception storage
  EV[is.na(EV)] <- 0
  ET[is.na(ET)] <- 0
  
  # print(paste("ETR_funct: ",round(sum(ETR, na.rm=T), 6), round(sum(EV, na.rm=T), 6)))
  if (F){
    print(paste("ETR_funct: ETR=",round(sum(ETR), 4)*dtc, "mm, EV=",round(sum(EV), 4)*dtc, "mm, ET=",round(sum(ET), 4)*dtc, "mm per time step"))
    C_3D.sum <- sum(C_3D, na.rm=T)
    if (C_3D.sum>0) print(paste("ETR_funct: EV is ", signif(sum(EV)/sum(C_3D)*100, 3), "% of the storage" ))
  }
  
  # 2025-01-21 correction of to small EV ####
  if(F){
    # assumption ET is always lower than EV # added on 2025-01-22
    i.ET.lt.EV <- which((ET > EV) & (C_3D>0))
    if (length(i.ET.lt.EV)>0){
      EV[i.ET.lt.EV] <- ETR[i.ET.lt.EV]/2  
      # this seems to be the same as setting sc.min <- 0.5, but is not, increasing sc.min reduces the resistance and increases ETR
    }
    ineg <- which((C_3D[iPAD]-EV[iPAD])<0)
    if (length(ineg)>0){
      EV[iPAD][ineg] <- C_3D[iPAD][ineg]
    }
    ET <- ETR-EV
  }
  
# OUTPUT ####
  if (exists("TestEV0")) if (TestEV0) EV <- EV*0
  sc1 <<- sc1
  rLE_3D <<- rLE_3D
  ETR <<- ETR
  ET <<- ET
  return(EV)
# Check ####  
 # f1 <- dt0/(nx*dx*ny*dy)
 # sum(ET*f1) ; sum(EV*f1)
 # sum(ETR*f1) ; sum(ET*f1)+sum(EV*f1)
}




# Test area #########################################################################
##  old version ###### 
# if(F){
#   sc[iPAD] <<- (C_3D[iPAD])/(Sp*S_3D[iPAD])
#   sc[iPADeq0] <<- 0        
#   sc[sc > 1] <<- 1              # 1 is complete saturation of the surface near air
#   sc[(sc < sc.min) & (sc != 0)] <<- sc.min        
#   sc[C_3D == 0] <<- 0
#   
#   
#   gLEt_3D <- (1-sc)/rET_3D     # (1-sc) Reduktion des Transportquerschnitts auf die Fläche, die nicht benetzt ist
#   gLEt_3D[iPADeq0] <- 1/Rinf
#   gLEw_3D <-  sc/rw_3D
#   gLEw_3D[iPADeq0] <- 1/Rinf
#   rLE_3D <- 1/( gLEw_3D + gLEt_3D )
#   rLE_3D[iPADeq0] <- Rinf            # ???????????
# 
# 
#   ETR <- ( (des_3D * Rn_3D) + (rhoa_3D * cp * (DDs_3D - DD_3D) / rH_3D) ) / 
#     (L_3D * (des_3D + (gam * (1 + (rLE_3D / rH_3D) ) ) )  )   #in kg/(s*m²) = mm/s
# 
#   length(is.na(ETR))
#   sUTC
#   sum(Rn_3D)/(nx*ny); Rndat
#   L_3D*sum(ETR, na.rm=T)/(nx*ny)
#   hist(L_3D*ETR) # in W/m²
#   
#   ETR[ETR < 0] <- 0
#   ETR[iPADeq0] <- 0
# 
#   # Aufteilen de Verdunstung entsprechend der Leitwerte
#   range(gLEt_3D * rLE_3D, na.rm=T)
#   range(gLEw_3D * rLE_3D, na.rm=T)
#   range(gLEt_3D * rLE_3D + gLEw_3D * rLE_3D, na.rm=T)
# 
#   ET <- gLEt_3D * rLE_3D * ETR # Verdunstung aus der Stomata
#   EV <- gLEw_3D * rLE_3D * ETR # Verdunstung aus dem Interzeptionsspeicher
#   round(sum((ETR-ET-EV)/ETR, na.rm=T), 4)
#   print(paste("ETR_funct: ETR=",round(sum(ETR, na.rm=T), 4)*dtc, "mm, EV=",round(sum(EV, na.rm=T), 4)*dtc, "mm per time step"))
#   print(paste("ETR_funct: EV is ", signif(sum(EV, na.rm=T)/sum(C_3D, na.rm=T)*100, 3), "% of the storage" ))
# 
#   # assumption ET is always lower than EV # added on 2025-01-22
#   i.ET.lt.EV <- which((ET > EV) & (C_3D>0))
#   length(which(C_3D[i.ET.lt.EV]>0))
#   length(which(gLEw_3D[i.ET.lt.EV] < gLEt_3D[i.ET.lt.EV]))
#   length(which((1/rw_3D[i.ET.lt.EV]) < (1/rET_3D[i.ET.lt.EV])))
#   length(which( ( sc[i.ET.lt.EV]/rw_3D[i.ET.lt.EV]) < ( (1-sc[i.ET.lt.EV])/rET_3D[i.ET.lt.EV])))
#   # der eigentlich höhere Leitwert feuchter Oberflächen wird durch den geringen befeuchteten Anteil in vielen Zellen unter den Leitwert für die Transpiration gebracht 
#   EVtest <- EV[i.ET.lt.EV]
#   ETtest <- ET[i.ET.lt.EV]
#   if (length(i.ET.lt.EV)>0){
#     EV[i.ET.lt.EV] <- ETR[i.ET.lt.EV]/2 # EV is at the least half of the ETR 
#   }
#   ineg <- which((C_3D[iPAD]-EV[iPAD])<0)
#   if (length(ineg)>0){
#     EV[iPAD][ineg] <- C_3D[iPAD][ineg]
#   }
#   ET <- ETR-EV
#   round(sum((ETR-ET-EV)/ETR, na.rm=T), 4)
#   sum(EVtest) ; sum(EV[i.ET.lt.EV])
#   sum(ETtest) ; sum(ET[i.ET.lt.EV])
#   sum(EVtest) + sum(ETtest) 
#   sum(EV[i.ET.lt.EV]) + sum(ET[i.ET.lt.EV])
# 
#   print(paste("ETR_funct: ETR=",round(sum(ETR, na.rm=T), 4)*dtc, "mm, EV=",round(sum(EV, na.rm=T), 4)*dtc, "mm per time step"))
#   print(paste("ETR_funct: EV is ", signif(sum(EV, na.rm=T)/sum(C_3D, na.rm=T)*100, 3), "% of the storage" ))
#   
# # check    
#   range(sc)
#   range(1/rET_3D[iPAD])*1000
#   range(gLEt_3D[iPAD])*1000
#   range(1/rw_3D[iPAD])*1000
#   range(gLEw_3D[iPAD])*1000
#   range(1/rLE_3D[iPAD])*1000
#   1/Rmax*1000
#   
#   
#   
# }
