################################################################################################
# Prozedur: (21) Meteo_Pressure

# Purpose: Erstellung der Luftdruckdruckprofile

# source(file.path(path_sub,"Meteo_Pressure.R",sep=""))

# Input:
# Luftdruck

# Output:
# Luftdruck_3D

################################################################################################

Meteo_pressure <- function(padat, pafield, hi){
  
  # paprof <- approx(paheights, padat, hi, method = "linear", rule = 2)
  # # plot(padat, paheights); lines(paprof$y, paprof$x )
  # 
  # for (i in 1:length(hi)) pafield[,,i] <- paprof$y[i]
  pafield <- padat
  return(pafield)
}

