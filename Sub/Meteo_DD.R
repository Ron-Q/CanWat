################################################################################################
# Prozedur: (20) Meteo_DD

# Purpose: Erstellung der Dampfdruckprofile (lineare Regression)

# source(file.path(path_sub,"Meteo_DD.R",sep=""))

# Input:
# Dampfdruck

# Output:
# DDdat as 3D field

################################################################################################
Meteo_DD <- function(DDdat, DDheights, DDfield, zz){
  
  if (length(DDdat)==1) DDprof <- data.frame(y=rep(DDdat,length(zz))) else
    DDprof <- approx(DDheights, DDdat, zz, method = "linear", rule = 2)
  # plot(Tdat, Theights); lines(Tprof$y, Tprof$x )
  
  for (i in 1:length(zz)) DDfield[i,,] <- DDprof$y[i]
  
  return(DDfield)
}



