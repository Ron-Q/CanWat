#+
# NAME*: Humidity.R
# PURPOSE*: provide humidity variables
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"Humidity.R"))
# INPUTS*:
  # Ta_3D, pa_3D, DDs_3D
  # cp, mue, Ra
# OUTPUT*:
  # L_3D, gam, rhoa_3D, des_3D
# REFERENCE:
# REVISION HISTORY*:
#   2019-10-02 : RQ
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# heat of vaporisation in J/kg (spezifische Verdampfungswaerme)
# L = (2.5008 - 0.002372 * T[°C])*10^6 (Liljequist, 2006)
# L_3D <- (2.5008 - 0.002372 * Ta_3D)*10^6 # J/kg
L_3D <- (2.5008 - 0.002372 * mean(Ta_3D))*10^6 # J/kg

# psychrometric constant in hPa/K (Psychrometerkonstante)
# gam = (p * cp)/(mue * L)
gam <- ((pa_3D * cp)/(mue * L_3D))   # in hPa * J/kgK / (J/kg) = hPa/K

# Luftdichte
# rhoa = p / (Ra * T)   p in Pa,  T in K
rhoa_3D <- pa_3D * 100 / (Ra * (Ta_3D+273.15)) #  Pa /( J/kgK * K) = kg Pa / J = kg /m^3  

# Anstieg der Saettigungsdampfdruckkurve
# s = es [hPa] * (4284 / (243.12 + T [°C])^2)  (Allg. Meteo Skript, (4))
des_3D <- DDs_3D * (4284 / (243.12 + Ta_3D)^2) # hPa/K

