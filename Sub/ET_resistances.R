#+
# NAME*: ET_resistances.R
# PURPOSE*: Berechnung der Verdunstungswiderstaende
# beim rab müsste vielleicht nochmal über eine Widerstandskaskade nachgedacht werden !!!!!!!!!!!!!!!!!!
# allerdings werden die Widerstände schon allein durch das Windprofil im Bestant immer größer,
# sodass dann vielleicht eine Dopplung vorliegt
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"ET_resistances.R"))
# INPUTS*:
  # iPAD : index for PAD != 0
  # eq.rb: type of quasi-laminar resistance equation (1 results in larger rb) 
  # # meteorolocial variables as 3D fields
  # uu = u_3d        # m/s, wind speed
  # , usm = USM       # m/s, friction velocity 
  # , DD = DD_3D       # hPa, water vapour pressure
  # , Ta = Ta_3D       # °C, temperature
  # , Rsd = Rg_3D     # W/m², Radiation short wave downwards
  dOmega= 0    # %   , soil moisture deficit
  # , p_air = 1012  # hPa , air pressure at station 
  # , PADdz = PAD*dz     # m²/m³, plant area density over the canopy segment dz
  # , LADdz = LAD*dz     # m²/m³, leaf area density over the canopy segment dz
  # , fSHF2Rn = 0.1 # if soil heat flux is not given, the part fSHF2Rn * Rn is assumed to go in the ground
  # # canopy resistance, weighting function parameters
  # , gca = 1000    # radiation     control of  stomata
  # , gcb = 34      # water vapour  control of  stomata
  # , gcc = 16.7    # temperature   control of  stomata
  # , gcd = 0.2     # soil moisture control of  stomata (gcd <- 0.0164 for ASTW)
  # , gcmax = 0.004 # maximal conductance
  dOmega.max = 22 # % , maximal soil moisture deficit (ASTW)

##* cw Parameter ####
  # cp <-  1005          # (J/kgK) : heat capacity of air , =>  source(paste0(rlib,"s_Meteorology/rho_cp.r"))
  # mue <- 0.62344       # (hPa/K) : Mv/Ma  = 18.061/28.97, relation of molecular weights =>  source(paste0(rlib,"s_Meteorology/humidity.r"))
  # Ra <-  287           # (J/kgK) : general gas constant
  # Rmax <- 50000        # s/m , maximal canopy resistance 
  # Rinf <- 1000000      # s/m , infinite resistance (for numerical reasons used as a dummy for areas without leafs, ETR is set 0 afterwards)  
  # eq.rb <- 1           # type of equation (please see ../Sub/ET_resistances.R)
  # Diffs <- 21.2*10^-6  # m^2/s, specific molecular diffusion coefficient
  # vk <- 13.3*10^-6     # m^2/s, kinematic viscosity of air
  # lw <-   0.01         # m   , (picea abies : 0.002) typical leaf width for laminar boundary layer resistance, regarding clumping
  # tu <- 0;    to <- 40 # °C, limits for temperature,  gcc between 16 and 19
  K4 <- 9.44             # g/kg, limit for the specific humidity deficit (Stewart, 1988, p.25)
  
# OUTPUT*:
  # rab_ 3D, rb_3D, rc_3D
  # rH_3D  <- rb_3D + rab         # Sensible heat flux
  # rET_3D <- rc_3D + rb_3D + rab # Transpiration
  # rw_3D  <- rb_3D + rab         # Evaporation
# REFERENCE:
  # Stewart JB (1988) Modelling surface conductance of pine forest. Agricultural and Forest Meteorology, 43:S,19–35 
# REVISION HISTORY*:
#   2019-10-02 : RQ
#   2025-03-03 : RQ, limit for the absolute specific humidity introduced, soil moisture added, preparation for function form
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aerodynamischer Widerstand im Bestand rab -----------------------------------------------------
  # Monteith Unsworth 2008, p.318, Monteith Unsworth 1990, p.118
  # u_3D gibt den Geschwindigkeitsgradienten zwischen der laminaren Grenzschicht (u=0) und dem Bestandesraum
  # usm gibt die Impulsaustauschgeschwindigkeit im Bestand
  # hohes u bei geringem us ergibt einen großen Widerstand gegen den Vertikaltransport
  rab <- u_3D/(usm*usm) # (1)  das ist der Austausch von der Oberfläche bis in die Bestandesluft
  # hier müsste vielleicht nochmal über eine Widerstandskaskade nachgedacht werden !!!!!!!!!!!!!!!!!!
  # usm[1,1,];  drop(usm)
  # t(drop(rab)) ; 
  # plot(rab[1,1,]/20); lines(u_3D[1,1,]); lines(usm[1,1,], col=2)
  # plot(rab[1,1,], ylim=c(0,500)); for (i in 1:length(rab[,1,1])) lines((rab[i,1,]), col=i)
  # ttt <- t(rab[2,,]); hist(ttt); rasterImage2(z=ttt)
  
# quasilaminarer Grenzschichtwiderstand rb ------------------------------------------------------
  # rb = v/Diffs*[(c/LAI^2) * (l*usm/v)]^(1/3) * (1/usm) # (1)
  # rl_3D <- (v*l/u_3D)^0.5 /(Diffs) # (2)
  # rb_3D <- rl_3D/( LAD * dz) # (3)
  if (eq.rb == 1){
    rb_3D[iPAD] = vk/Diffs*((100/((PAD[iPAD] * dz)^2)) * (lw*usm[iPAD]/vk))^(1/3) * (1/usm[iPAD])  # (1)
  } else {
    rb_3D[iPAD] <- (vk*lw/u_3D[iPAD])^0.5 / Diffs  / (PAD[iPAD] * dz)                           # (2) # (3)  
  }
  rb_3D[rb_3D == 0] <- 0.001
  # t(drop(rb_3D)) ; plot(rb_3D, ylim = c(0,200))
  # ttt <- t(rb_3D[2,,]); ttt[ttt>30] <- NA;  hist(ttt); rasterImage2(z=ttt)
  
# Canopy Resistance rc --------------------------------------------------------------------------
  # rs = 1/((1/rcf) - (1/rgrey))
  # rcf = 1/(gcmax * GR * Glf * GT * GBf)
  
  ## Weighting Functions ####
  ##* Radiation Control ####
  # GR is 1 for 1000 W/m²  
  # GR <- 2*Rg_3D[iPAD] / (Rg_3D[iPAD] + 1000) # (4)
  GR = Rg_3D[iPAD] * (1000 + gca)/(1000 * (gca + Rg_3D[iPAD]))
  # GRtop <- Rgtop * (1000 + gca)/(1000 * (gca + Rgtop))
  # GR <- rep(GRtop, nPAD)
  # plot(Rg_3D[iPAD], GR, ylim=c(0,1), xlim=c(0,1000)); points(Rg_3D[iPAD], GR1, col=2)
  
  ##* Water Vapour Control ####
  # Glf = 1 - gcb * drhoe
  K.4 <- K4/1000 *( (101300 /0.622) * 0.00217/(15+273.15))   # in kg/m³, limit for absolute humidity deficit
  drhoe = 0.217*((DDs_3D[iPAD] - DD_3D[iPAD])/(Ta_3D[iPAD] + 273.15))   # kg/m³, absolute humidity deficit
  # Glf <- 1 - ( gcb * drhoe ) # (5)
  Glf <- ifelse( (drhoe > K.4), 1 - gcb*K.4 ,  1 - ( gcb * drhoe) ) 
  # plot(0.217*((DDs_3D[iPAD] - DD_3D[iPAD])/(Ta_3D[iPAD] + 273.15)),Glf, ylim=c(0,1), xlim=c(0,0.025)); lines( (1:10)*0.0025 , 1 - ( gcb*(1:10)*0.0025)  )

  ##* Temperature Control ####
  # GT =       (t - 0) * (40 - t) ^ ((40 - gcc) / (gcc - 0)) # Flussneu:
  #     / ((gcc - 0) * (40 - gcc) ^ ((40 - gcc) / (gcc - 0)))
  # GT = (t - tu) * (to - t)^((to-gcc)/(gcc-tu)) / ( (gcc - tu) * (to - gcc)^((to-gcc)/(gcc-tu)) )
  gctexp <- ((to-gcc)/(gcc-tu))
  gctnom <- (gcc - tu) * ( (to - gcc)^gctexp )
  GT <- (Ta_3D[iPAD] - tu) * (to - Ta_3D[iPAD])^gctexp / gctnom 
  GT[is.na(GT) | GT<0] <- 0.
  # plot(Ta_3D[iPAD],GT, ylim=c(0,1), xlim=c(0,40)); lines( (1:40) ,  ((1:40) - tu) * ((to - (1:40))^gctexp) / gctnom  )
  
  ##* Soil Moisture control ####
  # GBf = 1, Soil moisture is currently not regarded due to the bigger input data demand.
  # It is 
  GBf <- 1 - exp(gcd*(dOmega - dOmega.max))  

  
  ##* Combination ####
  rc_3D[iPAD] <- (1/(LAD[iPAD]*dz * gcmax * GR * Glf * GT * GBf)) # (8)
  rc_3D[iPAD[is.infinite(rc_3D[iPAD])]] <- Rinf
  # range(LAD[iPAD]*dz*gcmax)
  # image(1/rc_3D[2,,])
  # rc_3D <- (1/(LAD*dz * gcmax * GR * Glf * GT * GBf)) # (8)
  # image(1/rc_3D[2,,])
  # str(GT)
  # image(GR[2,,])
  # image(LAD[2,,]*dz, col=topo.colors(100))
  # apply(drop(LAD*dz), 1,sum)
  # plot(1/rc_3D, ylim=c(0,gcmax)); abline(h=gcmax)
  # plot(rc_3D, ylim=c(0,2000))
  # apply(drop(1/rc_3D), 1,sum) # sum(1/rc_3D[2,1,])
   
  
# Oberflächenwiderstand rw (wet surface) --------------------------------------------------------
  # wird jetzt durch den quasilaminaren Grenzschichtwid. festgelegt 
  # sc anteilige Füllung des Speichers
  # rw <- rb_3D/(sc) 
  # rw[rw>Rmax] <- Rmax
  # t(drop(rw))

# total resistances ####
  # Sensible heat flux  
  rH_3D <- rb_3D + rab
  
  # Transpiration
  rET_3D <- rc_3D + rb_3D + rab   

  # Evaporation
  rw_3D <-  rb_3D + rab

  
