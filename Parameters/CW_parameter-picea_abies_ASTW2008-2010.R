#+
# NAME*:  CW_parameter-picea_abies_ASTW2008-2010.R
# PURPOSE*: PARAMETERS & CONSTANTS for the use in CanWat
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"CW_parameter.r"))
# INPUTS*: 
# OUTPUT*: PARAMETERS & CONSTANTS
# REVISION HISTORY*:
#    2019-10-17 RQ :
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definition of Parameters ####
cat("---- CALL: CW_parameter.r ---- \n")

#* Wind field ####
# the following parameters adjust the calculation of the initial wind field (see Queck R, Bernhofer C (2010))
ztop   <- 50    # m, level of horizontal homogeneity (assumption)
z0.fak <- 0.083  # = z0/H, relation between roughness length and the vegetation height --> adapted by SF [Queck 2004: Fraktionierung und zeitliche Differenzierung von Depositionsraten in Waldbestände]
z0u.fak <- 0.03 # = z0/H, relation between "roughness length" of the understory and the vegetation height
zd.fak <- 0.7   # = d/H, relation between displacement height and the vegetation height

#* Precipitation ####
evpause.min <- 6  # minimal time to divide events (just for counting the events)
evpause.min <- 6  # minimal time to divide events (just for counting the events)
# Free Troughfall Coefficient pT
# examples for pT are given by 
# 0.35 mm, Norway spruce, Grunicke et al. (2020),        0.40 mm, Norway spruce stand Lankreijer et al. (1999)
# 0.42 mm, old growth Douglas fir Pypker et al. (2005),  0.36 mm Link et al. (2004)
# in CanWat, throughfall is calculated according to the plant area (PAD) 
# within each grid cell by the following function
# cc1=0.21, cc2=0
# pTf <- function(p.area, cc1=0.21, cc2=0){ exp(-(cc1*p.area + cc2)) } # before 2025-03-15
cc1 <- 2.5
cc2 <- 15   # plant area with full closure
pTf <- function(p.area    # m²/m², plant area 
                , cc1=2.5 # -, degree of convexity, the higher the earlier the canopy closes
                , cc2=15  # m²/m², max plant area with gaps, i.e. denser canopies catch every rain drop"
                ){ ifelse(p.area<cc2, 1-(1-exp(-p.area*cc1/cc2))/(1-exp(-cc1)) , 0) }
# to get a feeling for the parameters, check out the following test code 
if(F){  # the example of cc1 = 2.5;  cc2 = 15 gives 40 % by PAD = 5 m²/m²
  p.area <- (0:40)/2; cc1.n <- (1:5)
  plot(p.area, pTf(p.area,cc1,cc2)*100, xlim=c(0,20), ylim=c(0,100), type="n"
       # , main=paste0("cc2=", cc2, "m²/m²")
       , xlab="plant area in m²/m²", ylab="Throughfall in %")
  abline(h=c(0:5)*20, v=c(0:4)*5, lty=3, col=c("gray")) #; abline(h=35, v=6.5, lty=3, col=c("green"))
  text(cc2, 10, expression(paste(cc[2])), srt=90, cex=1.5)
  for (i in seq_along(cc1.n)) {lines(p.area, pTf(p.area,cc1.n[i], cc2)*100, col=i, lwd=3, lty=c(3,5,1,2,4)[i])}
  legend("topright", title = expression(paste(cc[1])), legend = cc1.n, col=seq_along(cc1.n), lty=c(3,5,1,2,4), lwd=3)
}

#* Evaporation ####
cp <-  1005    # (J/kgK) : heat capacity of air , =>  source(paste0(rlib,"s_Meteorology/rho_cp.r"))
mue <- 0.62344 # (hPa/K) : Mv/Ma  = 18.061/28.97, relation of molecular weights =>  source(paste0(rlib,"s_Meteorology/humidity.r"))
Ra <-  287     # (J/kgK) : general gas constant

#* Radiation ####
extinct = 0.4       # Extinction coefficient, Bolstad and Gower ST (1990) "Estimation of leaf area index ..." Tree Physiology, 7:S,115–124 

#* Soil Heat Flux ####
fSHF2Rn = 0.1       # if soil heat flux is not given, the part fSHF2Rn * Rn is assumed to go in the ground

#* Resistance Parameterization ####
Rmax <- 50000          # s/m , maximal canopy resistance 
Rinf <- 1000000        # s/m , infinite resistance (for numerical reasons used as a dummy for areas without leafs, ETR is set 0 afterwards)  
##~ Boundary Layer resistance ####
eq.rb <- 1             # type of equation (please see ../Sub/ET_resistances.R)
Diffs <- 21.2*10^-6    # m^2/s, specific molecular diffusion coefficient
vk <- 13.3*10^-6        # m^2/s, kinematic viscosity of air
 lw <-   0.002           # m   , (picea abies : 0.002) typical leaf width for laminar boundary layer resistance
# lw <-   0.01           # m   , (picea abies : 0.002) typical leaf width for laminar boundary layer resistance, regarding clumping
##~ Canopy resistance (transpiration) ####
# parameter for weighting function after Stewart 1988
tu <- 0;    to <- 40   # °C, limits for temperature,  gcc between 16 and 19
# [gca] = W/m², [gcb] = K/hPa, [gcc] = K, [gcd] = % ,[gcmax] = m/s  # [gdc] is not used currently 
# gca <- 1000 ; gcb <- 34;  gcc <- 6   ; gcd <- 0.01357 ; gcmax <- 0.0085 # used by Martin
# gca <- 1046; gcb <- 11.82965; gcc <- 19.04589; gcd <- 0.01357; gcmax <- 0.00966 # Flussneu.mdb ASTWd
# gca <- 1000; gcb <- 34; gcc <- 16.7; gcd <- 0.0164; gcmax <- 0.0246 # Flussneu.mdb ASTW
# Flussneu.mdb ASTW justiert am Jahr 2010
if (!exists("gca")) gca <- 800
if (!exists("gcb")) gcb <- 34
if (!exists("gcc")) gcc <- 16.7
if (!exists("gcd")) gcd <- 0.0164
# if (!exists("gcmax")) gcmax <- 0.011/4.65                                               # cat("rcmin =", 1/gcmax)
# if (!exists("gcmax")) gcmax <- 0.01464/4.65  # 5% quantile of the rc that were calc from EC data (DE-Tha 2010) using the inverse PM Eq.
if (!exists("gcmax")) gcmax <- 0.027/4.65  # CanWat optimization using EC data (DE-Tha 2010) as reference.

#* Storage ####
#* parameter that regulate the resistance network, ie. divide the available energy between ET, EV and H 
Sp <- 0.99     # part of the saturation capacity C necessary to reach saturated water vapor pressure within the near surface air
if (!exists("sc.min")) sc.min <- 0.01 # part of storage from that evaporation occurs at the least, if there is water in the storage
               # be careful, increasing sc.min reduces the resistance and increases ETR
if (!exists("sc.f")) sc.f <- 12      # non-linearity of the drying process, i.e. of the saturation of the air near the surface, check with the following line
               # the higher sc.f the faster the saturation increases with the first drops on the surface 
     if(F){  # example
       sc.f <- 12 ; sc.min <- 0.01; sc0 <- (0:100)/100; sc.f.n <- c(0.1, 1,4,8,12)
       plot(sc0, (1-exp(-((1-sc.min)*sc0+sc.min)*sc.f.n[1]))/(1-exp(-sc.f.n[1])), xlim=c(0,1.2), ylim=c(0,1.1), type="l", lwd=3
            , ylab="evaporation layer saturation, sc", xlab=expression(paste("canopy storage saturation, ", sc[0])))
       lines(c(1,2), c(1,1), col=1, lwd=3, lty=1)
       abline(a=0, b=1, h=c(0,1), v=c(0,1), lty=3)
       for (i in seq_along(sc.f.n)[-1]) {lines(sc0, (1-exp(-((1-sc.min)*sc0+sc.min)*sc.f.n[i]))/(1-exp(-sc.f.n[i]))
                                           , col=i, lwd=3, lty=c(1,2,3,4,5)[i])}
       lines(c(sc0,2), (1-exp(-((1-sc.min)*c(sc0,2)+sc.min)*sc.f.n[5]))/(1-exp(-sc.f.n[5])), col=5, lwd=2, lty=5)
       legend("bottomright", title = expression(paste(sc[f])), legend = sc.f.n, col=seq_along(cc1.n), lty=c(1,2,3,4,5), lwd=3) #c(3,2,2,2,2))
     }
 
#* Plants ####  
LAI0 = 7       # m²/m² mean default value, overwritten by static driver

#* Drainage ####  
# D(t) = exp(a + b * (C(t) - S)), for C(t) > S
# S0 <- 3      # mm or l/(m²-ground area), estimated mean saturation capacity <- used here --> adapted by SF [Grunicke et al. 2020,period 2002-2010]
# S0 <- 2.8    # mm or l/(m²-ground area), estimated mean saturation capacity <- used here --> adapted by SF [Grunicke et al. 2020,period 2002-2010]
# S0 <- 2.92   # mm, Grunicke et al. (2020)
# S0 <- 3.1      # mm, MODEROW 2004, p25)
 S0 <- 3.5      # mm, Model Fit Queck 2025)
PAI.S0 <- 6.54 # 
Dmin0 <- 0.002*S0/1.05 # 1/min, minimal Drainage, scaled from the 0.002 mm/min given by Rutter et al. (1975 p369)
                       # using 1.05 mm, mean saturation capacity for their forest of Corsican Pine  
bd0 <- 3.7     # 3.7, drainage coefficient, Rutter 1975 p368 (seams insensitive,  4.6 in MKunath 2014)

#* Model Parameters ####
dt1 <- 600     # in sec, maximum step-size of the inner loop (kernel), if there occurs rain or drainage, used for die extension of output files
dts <- c(1,5,seq(10,200, by=5 ),300,600,900,1800,3600)  # possible time steps 3600 = 1800 + 2*600 + 300 + 4*60 + 2*20 + 10 + 5 + 5*1
fdtmax <- 4    # factor to reduce the calculated maximal time step (the larger fdtmax the smaller the time steps)
psize<- 0.5    # size of the plot symbols


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output of parameters ####
if (exists("journal")){
  journal <- c(journal, "  " )
  journal <- c(journal, "# Parameters ##################  ")
  journal <- c(journal, paste0("parameter file name: ", CWpara_name ) )
  journal <- c(journal, "## Precipitation  " )
  journal <- c(journal, paste0("Throughfall propability as function of PAD: pT <- exp(-(cc1*PAD*dz + cc2))"))
  journal <- c(journal, paste0("parameter: cc1 = ", cc1 , " -, cc2 = ", cc2))
  journal <- c(journal, "## Evaporation  " )
  journal <- c(journal, paste0("cp = ", cp , " J/kgK, heat capacity of air  "))
  journal <- c(journal, paste0("mue = ", mue, " hPa/K, Mv/Ma  = 18.061/28.97, relation of molecular weigths  "))
  journal <- c(journal, paste0("Ra = ",Ra, " J/kgK, general gas constant  "))
  journal <- c(journal, "  " )
  journal <- c(journal, "## Resistance Parameterisation  ")
  # journal <- c(journal, paste0("Rmax = ", Rmax, "s/m, canopy resistance for areas without leafs  "))
  journal <- c(journal, paste0("Diffs = ",Diffs, "m^2/s, molecular diffusivity coefficient  "))
  journal <- c(journal, paste0("vk = ",vk," m^2/s, kinematic viskosity of air  "))
  journal <- c(journal, paste0("lw = ", lw," m, typical leaf width (picea abies for laminar boundary layer resistance)  "))
  journal <- c(journal, "parameters for weighting function  after Stewart 1988  ")
  journal <- c(journal, paste0("gcmax =", gcmax, " s/m, maximal stomatal conductance  "))
  journal <- c(journal, paste0("tu = ", tu, ", to = ", to, "(-), limits for temperature weighting function  "))
  journal <- c(journal, paste0("gca = ", gca, ", gcb = ", gcb, ", gcc = ", gcc, ", gcd = ", gcd, "  "))
  journal <- c(journal, paste0("Sp = ", Sp," part of the saturation capacity necessary to saturate the near surface air completely "))
  journal <- c(journal, paste0("sc.f = ", sc.f, " non-linearity of the drying process, i.e. of the saturation of the air near the surface  "))
  journal <- c(journal, paste0("sc.min = ", sc.min, " part of storage from that evaporation occurs at the least, if there is water in the storage  "))
  journal <- c(journal, "  " )
  journal <- c(journal, "## Plants  ")  
  journal <- c(journal, paste0("LAI0 = ", LAI0, ", mean default value, overwritten by static driver  ") )
  journal <- c(journal, "  " )
  journal <- c(journal, "## Drainage  ")  
  journal <- c(journal, "D(t) = exp(a + b * (C(t) - S)), for C(t) > S  ")
  journal <- c(journal, paste0("S0 = ", S0, " mm, estimated mean saturation capacity  ") )
  journal <- c(journal, paste0("Dmin0 =", round(Dmin0,3), " mm/min, Drainage minimum  ") )
  journal <- c(journal, paste0("bd0 =", bd0, " Drainage coefficient  ") )
  journal <- c(journal, "  " )
  journal <- c(journal, "## Model Parameters  " )
  journal <- c(journal, paste0("dt0 = ", dt_met, " s, time step of input data  "))
  journal <- c(journal, paste0("dt1 = ", dt1, " s, maximum step-size of the inner loop (kernel), if there occurs rain or drainage, used for die extention of ouput files  "))
  journal <- c(journal, paste0("dts = (" , paste(dts, collapse = ","), ") s, possible time steps  ") )
  journal <- c(journal, paste0("fdtmax = ", fdtmax, " (-) factor to reduce the calculated maximal time step (the larger fdtmax the smaller the time steps)  "))
  # journal <- c(journal, paste0("psize = ", psize, "size of the plot symbols  "))
}
