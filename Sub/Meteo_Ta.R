#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+
# NAME*: Meteo_Ta.R
# PURPOSE*: Estimation of temperature distribution within the canopy
# RELEVANCY*: CanWat
# CALLING SEQ.:  source(file.path(path_sub,"Meteo_Ta.R"))
# CALLED BY:  CanWat.R
# INPUTS*:
# Tadat : measured temperature 
# PADc  : cumulative plant area index
# zz    : heights of the layers
# OUTPUT*:
# Ta_3D
# EXAMPLE:
# REFERENCE:
# REVISION HISTORY*:
#  10/2014 (RQ): created 
#  01/2021 (RQ): basic revision 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     # Theights=hTa/100; Tfield=Ta_3D
Meteo_Ta <- function(Tadat, Theights, Tfield, zz){
  # Tadat<- as.numeric(metTa[ito,Ta_nam, with=F])
  # Theights <- hTa/100
  
  nz <- length(zz)
  if (length(Tadat)==1) Tprof <- data.frame(y=rep(Tadat,nz)) else
    Tprof <- approx(Theights, Tadat, zz, method = "linear", rule = 2)
    # plot(Tadat, Theights); lines(Tprof$y, Tprof$x )
  
  for (i in 1:nz) Tfield[i,,] <- Tprof$y[i]
  # Tfield[Tfield < 0] <- NA
  return(Tfield)
  
}

