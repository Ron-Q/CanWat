#+
# NAME*:  CW_parameter.R
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
print("---- CALL: CW_parameter.r ----")

#* Wind field ####
# the following parameters adjust the calculation of the initial wind field (see Queck R, Bernhofer C (2010))
ztop   <- 50    # m, level of horizontal homogeneity (assumption)
z0.fak <- 0.07  # = z0/H, relation between roughness length and the vegetation height
z0u.fak <- 0.03 # = z0/H, relation between "roughness length" of the understory and the vegetation height
zd.fak <- 0.7   # = d/H, relation between displacement height and the vegetation height

#* Precipitation ####
evpause.min <- 6  # minimal time to divide events (just for counting the events)
evpause.min <- 6  # minimal time to divide events (just for counting the events)
# Free Troughfall Coefficient pT
# examples for pT are given by 
# 0.35 mm, Norway spruce, Grunicke et al. (2020),        0.40 mm, Norway spruce stand Lankreijer et al. (1999)
# 0.42 mm, old growth Douglas fir Pypker et al. (2005),  0.36 mm Link et al. (2004)
# in CanWat, throughfall is calculated according to the plant area (PAD) within each grid cell
# pT <- exp(-(cc1*PAD*dz + cc2)), e.g. grid cell with PAD=1m²/m³ of 5m height (PAI=5) results in pt = 0.377
cc1 <- 0.195  # the higher cc1 the less throughfall
cc2 <- 0

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
Diffs <- 21.2*10^-6    # m^2/s, specific molecular diffusion coefficient
vk <- 13.3*10^-6        # m^2/s, kinematic viscosity of air
lw <-   0.002           # m   , (picea abies : 0.002) typical leaf width for laminar boundary layer resistance
# parameter for weighting function after Stewart 1988
tu <- 0;    to <- 40   # °C, limits for temperature,  gcc between 16 and 19
# [gca] = W/m², [gcb] = K/hPa, [gcc] = K, [gcd] = % ,[gcmax] = m/s  # [gdc] is not used currently 
# gca <- 1000 ; gcb <- 34;  gcc <- 6   ; gcd <- 0.01357 ; gcmax <- 0.0085 # used by Martin
gca <- 1046; gcb <- 11.82965; gcc <- 19.04589; gcd <- 0.01357; gcmax <- 0.00966 # Flussneu.mdb ASTWd
# gca <- 1000 ; gcb <- 34;  gcc <- 16.7; gcd <- 0.0164  ; gcmax <- 0.0246 # Flussneu.mdb ASTW
                                                   # cat("rcmin =", 1/gcmax)
sc.min <- 0.0  # part of storage from that evaporation occurs at the least, if there is water in the storage
               # be careful, increasing sc.min reduces the resistance and increases ETR
Sp <- 0.99     # part of the saturation capacity C necessary to reach saturated water vapor pressure within the near surface air

#* Plants ####  
LAI0 = 7       # m²/m² mean default value, overwritten by static driver

#* Drainage ####  
# D(t) = exp(a + b * (C(t) - S)), for C(t) > S
S0 <- 2.8      # mm or l/(m²-ground area), estimated mean saturation capacity <- used here --> adapted by SF [Grunicke et al. 2020,period 2002-2010]
# S0 <- 2.92    # mm, Grunicke et al. (2020)
# S0 <- 3.1     # mm, MODEROW 2004, p25)
Dmin0 <- 0.002*S0/1.05 # 1/min, miniale Drainage, scaled from the 0.002 mm/min given by Rutter et al. (1975 p369)
                       # using 1.05 mm, mean saturation capacity for their forest of Corsian Pine  
bd0 <- 3.7     # 3.7, drainage coefficient, Rutter 1975 p368 (seams insensitive,  4.6 in MKunath 2014)

#* Model Parameters ####
dt1 <- 600     # in sec, maximum step-size of the inner loop (kernel), if there occurs rain or drainage, used for die extention of ouput files
dts <- c(1,5,seq(10,200, by=5 ),300,600,900,1800,3600)  # possible time steps 3600 = 1800 + 2*600 + 300 + 4*60 + 2*20 + 10 + 5 + 5*1
fdtmax <- 4    # factor to reduce the calculated maximal time step (the larger fdtmax the smaller the time steps)
dtcm <- 3600   # moving average of the time step size
psize<- 0.5    # size of the plot symbols


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output of parameters ####
if (exists("journal")){
  journal <- c(journal, "  " )
  journal <- c(journal, "# Parameters ##################  ")
  journal <- c(journal, "  " )
  journal <- c(journal, "## Evaporation  " )
  journal <- c(journal, paste0("cp = ", cp , "J/kgK, heat capacity of air  "))
  journal <- c(journal, paste0("mue = ", mue, "hPa/K, Mv/Ma  = 18.061/28.97, relation of molecular weigths  "))
  journal <- c(journal, paste0("Ra = ",Ra, "J/kgK, general gas constant  "))
  journal <- c(journal, "  " )
  journal <- c(journal, "## Resistance Parameterisation  ")
  # journal <- c(journal, paste0("Rmax = ", Rmax, "s/m, canopy resistance for areas without leafs  "))
  journal <- c(journal, paste0("Diffs = ",Diffs, "m^2/s, molecular diffusivity coefficient  "))
  journal <- c(journal, paste0("vk = ",vk," m^2/s, kinematic viskosity of air  "))
  journal <- c(journal, paste0("lw = ", lw," m, typical leaf width (picea abies for laminar boundary layer resistance)  "))
  journal <- c(journal, "parameters for weighting function  after Stewart 1988  ")
  journal <- c(journal, paste0("gcmax =", gcmax, "s/m, maximal stomatal conductance  "))
  journal <- c(journal, paste0("tu = ", tu, ", to = ", to, "(-), limits for temperature weighting function  "))
  journal <- c(journal, paste0("gca = ", gca, ", gcb = ", gcb, ", gcc = ", gcc, ", gcd = ", gcd, "  "))
  journal <- c(journal, paste0("sc.min = ", sc.min, "part of storage from that evaporation occurs at the least, if there is water in the storage  "))
  journal <- c(journal, paste0("Sp = ", Sp,"part of the saturation capacity necessary to saturate the near surface air  "))
  journal <- c(journal, "  " )
  journal <- c(journal, "## Plants  ")  
  journal <- c(journal, paste0("LAI0 = ", LAI0, ", mean default value, overwritten by static driver  ") )
  journal <- c(journal, "  " )
  journal <- c(journal, "## Drainage  ")  
  journal <- c(journal, "D(t) = exp(a + b * (C(t) - S)), for C(t) > S  ")
  journal <- c(journal, paste0("S0 = ", S0, " mm, estimated mean saturation capacity  ") )
  journal <- c(journal, paste0("Dmin0 =", round(Dmin0,3), " mm/min, Drainage minimum  ") )
  journal <- c(journal, paste0("bd0 =", bd0, "Drainage coefficient  ") )
  journal <- c(journal, "  " )
  journal <- c(journal, "## Model Parameters  " )
  journal <- c(journal, paste0("dt0 = ", dt_met, " s, time step of input data  "))
  journal <- c(journal, paste0("dt1 = ", dt1, "s, maximum step-size of the inner loop (kernel), if there occurs rain or drainage, used for die extention of ouput files  "))
  journal <- c(journal, paste0("dts = (" , paste(dts, collapse = ","), ") s, possible time steps  ") )
  journal <- c(journal, paste0("fdtmax = ", fdtmax, "(-) factor to reduce the calculated maximal time step (the larger fdtmax the smaller the time steps)  "))
  journal <- c(journal, paste0("dtcm = ", dtcm,"s, moving average of the time step size  "))
  # journal <- c(journal, paste0("psize = ", psize, "size of the plot symbols  "))
}
