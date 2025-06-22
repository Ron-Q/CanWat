#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+
# NAME*: Meteo_Rg.R
# PURPOSE*: Radiation transfer within canopy (approach: Beers law)
#           for shortwave radiation
#           ==> to do ==> need to by 3D
#           ==> to do ==> need to by 3D
# 
# RELEVANCY*: CanWat
# CALLING SEQ.:  source(file.path(path_sub,"Meteo_Rg.R",sep=""))
# CALLED BY:  CanWat.R
# INPUTS*:
  # Rgdat : Global radiation data 
  # PADc  : cumulative plant area index
  # ex    : 0.5, Extinktionskoeffizient
        # nz,ny,nx : number of grid points
        # zz    : heights
# OUTPUT*:
  # Rg_3D : W/m², short wave radiation at the top of the layer
# EXAMPLE:
# REFERENCE:
# REVISION HISTORY*:
  #  10/2014 (RQ): created 
  #  01/2021 (RQ): basic revision 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meteo_Rg <- function(Rgdat,  Rgheights, PADc, pRtrans, dz, Rg_3D){
  #Rgheights <- hRg/100
  itop <- which.max(Rgheights)
  Rgtop <- Rgdat[itop]
  
  izRg <- Rgheights/dz   # layer of the measurements
  
   
   
  #* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #* Strahlungsverteilung muss überprüft werden
  #* Aktuell nur Rechnungen mit 1 Layer machen
  #* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  # pRtrans.m <- apply(pRtrans, 1, function(a){ mean(a)})
  # rasterImage2(z=t(pRtrans[1,,]))
  # mean(pRtrans[1,,])
 if (bigleaf){
    Rg_3D[1,,] <- 0
    Rg_3D[2,,] <- Rgtop
    Rg_3D[3,,] <- Rgtop
  } else {
    Rg_3D[nz,,] <- Rgtop   # top layer is empty, all radiation goes trough
    
    Rg_3D[nz,,] <- Rgtop
    # Rg_3D <- Rgtop*pRtrans # estimates Rg at the bottom of the layers 
    # but, for consistency with the bigleaf model, Rg_3D is currently defined at the top of the layer  
    Rg_3D[-nz,,] <- Rgtop * pRtrans[-1,,]
    
    Rg_3D[Rgdat < 0] <- 0
    
  }
    
  # tst <- Rg_3D; summary(tst) 
  # dev.set(4); rasterImage2(z=t(tst[,,ixreft]),z.cex=1); abline(v=iyreft)
  # dev.set(5); rasterImage2(z=t(tst[,iyreft,]),z.cex=1); abline(v=ixreft)
  # dev.set(6); plot(tst[,iyreft,ixreft],1:33, xlim=c(0,3)); for (i in 1:nx)  for (j in 1:ny) lines(tst[,j,i],1:33); lines(tst[,iyreft,ixreft],1:33, col=2, lwd=3); abline(v=0, col=3)
  
  # if (silent < 1) print(paste("Rgtop: ",round(Rgtop), "; Rg_3D[nz,,]:", round(sum(Rg_3D[nz,,])/area_xy), "; Rg_3D[2,,]:", round(sum(Rg_3D[2,,])/area_xy), "; Rg_3D[1,,]:", round(sum(Rg_3D[1,,])/area_xy)))
  
  return(Rg_3D)
}

