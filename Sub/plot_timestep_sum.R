#+
# NAME*: plot_timestep_sum.R
# PURPOSE: 
# CALLING: source(file.path(path_sub,"plot_timestep_sum.R"))
# INPUT:
# Output:
# Drainage
# REFERENCE:
# REVISION HISTORY*:
#   2019-10-25 , RQ:
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_timestep_sum_0 <- function(ds = 4, ntu=ntu){

  ylimP = c(0.0025,0)   # limits of the precipitation plot
  fakstor <<- 2000           # factor between the y-scales of precipitation and storage plot
  
  dev.set(ds)
  fig.x1 <- 0.003;  fig.x2 <- 0.77
  fig.y1a <- 0.7;   fig.y2a <- 1
  fig.y1b <- 0;   fig.y2b <- 0.7
  
#  plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
  # switch(.Platform$OS.type
  #        , "windows" = {
  #          ylab1 = paste0("in 10 W/m\262 | \260C | hPa | m/s")
  #        }
  #        , "unix" = { 
  #          ylab1 = paste0("in 10 W/m² | °C | hPa | m/s") 
  #        }
  # )
  ylab1 = expression("in 10 W/m"^2*" | " * symbol("\260")*"C | hPa | m/s")
  if(plotlife){
    cex1 <- 1
  } else {
    cex1 <- 0.7
  }
  
  # meteo plot
  Rg.p.fak <- 0.1; Rn.p.fak <- 0.1; Ta.p.fak <- 1; DD.p.fak <- 1; WS.p.fak <- 1
  par(fig=c(fig.x1, fig.x2, fig.y1a, fig.y2a), mar = c(0,5,4,2)) 
  ylimM = range(metRg.top/10, metRn.top/10, metTa.top, metDD.top, metWS.top, na.rm = T)+c(-1,+1)
  if (ylimM[2]>50){ Ta.p.fak <- 2; DD.p.fak <- 2}
  WS.p.fak <- floor(ylimM[2]/(max(metWS.top,na.rm=T)) )
  if (WS.p.fak<10) WS.p.fak <- WS.p.fak%/%2 else WS.p.fak <- (WS.p.fak%/%10)*10
  plot(metRg$UTC, metRg.top*Rg.p.fak, type="l", xlim=(datr)-dt0, ylim=ylimM, col="yellow", lwd=3
       , xlab = "", ylab = ylab1, xaxs="i", yaxs="i", xaxt='n' ,cex = cex1, cex.axis=cex1, cex.lab=cex1 )
  abline(h=0)
  lines( metRn$UTC, metRn.top*Rn.p.fak, col="thistle2", lwd=3)
  points(metTa$UTC, metTa.top*Ta.p.fak, col="red", pch=16)
  points(metDD$UTC, metDD.top*DD.p.fak, col="lightsteelblue1", pch=16)
  lines( metWS$UTC, metWS.top*WS.p.fak, col=1, lwd=1)
  # legend("topright"   # legend position
  legend(x=mean(par()$usr[2]),y=par()$usr[4], xjust=1, yjust=0, xpd=NA   # legend position
                ,c(paste0("Net Radiation*", Rg.p.fak), paste0("short.in.Radiat.*",Rn.p.fak)
                   , paste0("Temperature*", Ta.p.fak), paste0("Water Vapour P.*", DD.p.fak)
                   , paste0("Wind*", WS.p.fak))
         #,       c("Net Radiation", "short.in.Radiat.","Temperature"    ,"Water Vapour P."  ,"Wind")
         ,       col=c( "thistle2"       , "yellow",    "red"           ,"lightsteelblue1"      ,1     )
         , pch=c(NA, NA,16, 16,NA), lty=c(1, 1,NA,NA,1), lwd=c(3,3,NA,NA,3), ncol = 3, cex = cex1)
  text(x=par()$usr[1],y=par()$usr[4], adj=c(0,-0.3), paste("CanWat version: ", CWversion, "\nCase:",act), cex=1, xpd=NA)
  
  # set up storage plot
  par(fig=c(fig.x1, fig.x2, fig.y1b, fig.y2b), new=TRUE, mar = c(5,5,4,2)) 
  ylimC<-c(0, max(ylimP)) # limits of the storage plot
  # barplot(metPP$PFi*dt0, col="lightblue1", border = "lightblue1", ylim = ylimP, axes=F, xaxs="i", yaxs="i"); axis(4); par(new=T);
  plot(metPP$UTC, rep(0.1,  ntu), type="n", xlim=(datr)-dt0, ylim=ylimC
       # , main=paste("Case: ",act) 
       , xlab = "UTC", ylab = "Fluxes in mm/s", xaxs="i", yaxs="i", cex = cex1, cex.axis=cex1, cex.lab=cex1 )
  Splot <- sum(S_3D)/(nx*ny)/fakstor;  abline(h=Splot, lwd=1, col="blue" )
  abline(h=ylimC[2]/2, lwd=2, col="lightgray" )
  stortics <- signif((1:5)/5*ylimC[2], 2)
  axis(4,at=c(ylimC[2]/2,Splot), labels=c("",""), las=0, col.axis = "blue", col.ticks = "blue", col.lab = "blue", cex = cex1, cex.axis=cex1, cex.lab=cex1 )
  axis(4,at=c(ylimC[2]/2,Splot), labels=c("dC=0","S"), line = -0.7, las=0, lwd=0, col.lab = "blue", cex = cex1, cex.axis=cex1, cex.lab=cex1 )
  axis(4,at=stortics, labels=stortics*fakstor, las=0, col.axis = "blue", col.ticks = 0, col.lab = 0, cex = cex1/2, cex.axis=cex1, cex.lab=cex1 )
  mtext("storage in mm", side = 4, line = 2, outer = FALSE, col="blue", adj=0.5, cex=cex1)
  # legend(x=mean(par()$usr[1:2]),y=par()$usr[4], xjust=0.5, yjust=0, xpd=NA
  legend(x=mean(par()$usr[2]),y=par()$usr[4], xjust=1, yjust=0, xpd=NA
         ,c("Interception","Throughfall","Storage","delta Storage","Drainage","Dthroughfall", "Evaporation*10", "Transpiration*10"  )
         ,       col=c("steelblue1","lightsteelblue1","blue",       1,              3,         "darkgreen",     "red" , "yellow4")
         , pch=c(NA,NA,3,16, 16,16,16,16), lty=c(1,1,rep(NA,6)), lwd=c(3,3,rep(NA,6)), ncol = 4, cex = cex1)
  # legend("top",c("Interception","Throughfall","Storage","delta Storage","Drainage","Dthroughfall", "Evaporation", "Evaporation*100", "ETR*100"  )
  #        ,       col=c("steelblue1","lightsteelblue1",4,       1,              3,         "darkgreen",     "red",            "thistle2", "yellow4")
  #        , pch=c(NA,NA,3,16, 16,16,16,16,16), lty=c(1,1,rep(NA,7)), lwd=c(3,3,rep(NA,7)), ncol = 9)
  
  if (exists("WFa.l")) with(WFa.l,{
    points(UTC+3600, C_3D/100, pch=3, col="steelblue1", cex=psize)    # blue:  Storage
    points(UTC+3600, dC+ylimC[2]/2, pch=20, col="gray", cex=psize)    # black: delta Storage
    points(UTC+3600, Drainage, pch=20, col="lightgreen", cex=psize)   # green: Drainage
    points(UTC+3600, Dthroughf, pch=20, col="darkgreen", cex=psize*2) # green: Dthroughfall
    points(UTC+3600, EV*20, pch=20, col="red", cex=psize)             # red:   Evaporation
  })

  return(ylimC)
}



# plot intensities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_timestep_sum <- function(WFa.itt  # Data
                              , ds=4   # device number
                              , DrOn   # Drainage is active
                              ){
  # # WFa.itt <- WFa[itt-1]
#  WBa.itt <- WFa.itt*600
  
  # lines(c(sUTC,sUTC), c(0 , 0.04), lwd=2, col="steelblue1" )  #dark blue: Throughfall
  # lines(c(sUTC,sUTC), c(0.04 , (0.08)), lwd=2, col="red" ) #  Throughfall
  # points(sUTC, C_3D/100, pch=3, col=4, cex=psize)                   # blue:  Storage
  # points(sUTC, dC+ylimC[2]/2, pch=16, col=1, cex=psize)             # black: delta Storage
  # points(sUTC, Drainage, pch=16, col=3, cex=psize)                  # green: Drainage
  # points(sUTC, Dthroughf, pch=16, col="darkgreen", cex=psize*2)     # green: Dthroughfall
  # points(sUTC, EV*20, pch=16, col="red", cex=psize*2)               # red:   Evaporation
  # points(sUTC, (ET+EV)*20, pch=16, col="yellow4", cex=psize*1.5)    # yellow4:   Evapotranspiration
  
  
  if ( "png" == names(dev.cur()) ) {ppsize <- psize*0.5} else {ppsize <- psize}
  dev.set(ds)
  with(WFa.itt,{
    lines(c(sUTC,sUTC), c(0 , Pintercept), lwd=2, col="steelblue" )  #dark blue: Throughfall
    lines(c(sUTC,sUTC), c(Pintercept , (Pthroughf+Pintercept)), lwd=2, col="lightsteelblue1" ) #  Throughfall
    points(sUTC, C_3D/fakstor, pch=3, col="blue", cex=ppsize)                   # blue:  Storage
    points(sUTC, dC+ylimC[2]/2, pch=16, col=1, cex=ppsize)             # black: delta Storage
    points(sUTC, Drainage, pch=16, col=3, cex=ppsize)                  # green: Drainage
    points(sUTC, Dthroughf, pch=16, col="darkgreen", cex=ppsize*2)     # green: Dthroughfall
    points(sUTC, EV*10, pch=16, col="red", cex=ppsize*2)               # red:   Evaporation
    points(sUTC, (ET)*10, pch=16, col="yellow4", cex=ppsize*1.7)    # yellow4:   Evapotranspiration
    if ((Drainage > 0) & !DrOn) { abline(v=sUTC, lty=2) } # Drainage has started
    if ((Drainage == 0) & DrOn) { abline(v=sUTC, lty=3) } # Drainage has ceased
    
  })
}


# print final sums in the plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_event_sum <- function(WB, ds=4){
  # switch(.Platform$OS.type
  #        , "windows" = {
  #          ylab1 = paste0("in 10 W/m\262 | \260C | hPa | m/s")
  #        }
  #        , "unix" = { 
  #          ylab1 = paste0("in 10 W/m² | °C | hPa | m/s") 
  #        }
  # )
  ylab1 = expression("in 10 W/m"^2*" | " * symbol("\260")*"C | hPa | m/s")
  if(plotlife){
    cex1 <- 1*1.1
  } else {
    cex1 <- 0.8*1.1
  }
  
  dev.set(ds)
  par(fig=c(0.8, 1, 0, 1), new=TRUE, mar = c(1,1,1,1)) 
  plot(1:1, type="n", xlim=c(0,10), ylim=c(0,10),ann=F, xaxt='n' , yaxt='n', bty="n" )
  tpx0 <- 0
  tpy0 <- 10; tpyd <- 0.5
  text(x=tpx0,y=tpy0-1*tpyd, adj=c(0,0), paste("Domain Characteristics"), cex=cex1*1.2, xpd=NA)
  text(x=tpx0,y=tpy0-1.75*tpyd, adj=c(0,0), paste("[nx,ny,nz] = [",nx0,",",ny0,",",nz0, "]", sep=""), cex=cex1/1.2, xpd=NA)
  text(x=tpx0,y=tpy0-2.5*tpyd, adj=c(0,0), paste("gridsize = [",dx,",",dy,",",dz, "]", sep=""), cex=cex1/1.2, xpd=NA)
  # switch(.Platform$OS.type
  #        , "windows" = {
  #          text(x=tpx0,y=tpy0-3.5*tpyd, adj=c(0,0), paste("PAI =",round(PAI,1), "m\262/m\262"), cex=cex1, xpd=NA)
  #          text(x=tpx0,y=tpy0-5.25*tpyd, adj=c(0,0), paste("mean values in mm = kg/m\\262"), cex=cex1, xpd=NA)
  #        }
  #        , "unix" = { 
  #          text(x=tpx0,y=tpy0-3.5*tpyd, adj=c(0,0), paste("PAI =",round(PAI,1), "m²/m²"), cex=cex1, xpd=NA)
  #          text(x=tpx0,y=tpy0-5.25*tpyd, adj=c(0,0), paste("mean values in mm = kg/m²"), cex=cex1, xpd=NA)
  #        }
  # )
  teq <- substitute(italic(PAD) == PADv ~ m^2/m^2, list(PADv = format(sum(PAD*dz/(nx*ny)), digits = 3)))
  text(x=tpx0,y=tpy0-3.5*tpyd, adj=c(0,0), teq, cex=cex1, xpd=NA)
  teq <- expression("mean values in mm = kg/m"^2)
  text(x=tpx0,y=tpy0-5.25*tpyd, adj=c(0,0), teq, cex=cex1, xpd=NA)
  
  text(x=tpx0,y=tpy0-4.5*tpyd, adj=c(0,0), paste("Balances are given as"), cex=cex1, xpd=NA)
  text(x=tpx0,y=tpy0-6.*tpyd, adj=c(0,0), paste("summed over the time interval"), cex=cex1, xpd=NA)
  
  with(WB,{
    ptime <- strftime(range(WB$UTC, na.rm = T ), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    text(x=tpx0,y=tpy0-6.75*tpyd, adj=c(0,0), paste("from: ", ptime[1] ), cex=cex1/1.2, xpd=NA)
    text(x=tpx0,y=tpy0-7.5*tpyd, adj=c(0,0),   paste("to:      ", ptime[2]), cex=cex1/1.2, xpd=NA)
    tpy1 <- 8.75 
    text(x=tpx0,y=tpy0-(tpy1+0)*tpyd, adj=c(0,0), paste("Water Balance"), cex=cex1*1.2, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+1)*tpyd, adj=c(0,0), paste("PF =",round(sum(WB$PF),1), "mm"), cex=cex1, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+2)*tpyd, adj=c(0,0), paste("Storage Change =",round(sum(WB$dC),1), "mm"), cex=cex1, xpd=NA)
    sptr <- sum(WB$Pthroughf);   sDtr <- sum(WB$Dthroughf)
    text(x=tpx0,y=tpy0-(tpy1+3)*tpyd, adj=c(0,0), paste("Pthroughf =",round(sptr,1), "mm"), cex=cex1, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+4)*tpyd, adj=c(0,0), paste("Dthroughf =",round(sDtr,1), "mm"), cex=cex1, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+5)*tpyd, adj=c(0,0), paste("Troughfall =",round(sptr+sDtr,1), "mm"), cex=cex1, xpd=NA)
    spEV <- sum(WB$EV); spET <- sum(WB$ET)
    text(x=tpx0,y=tpy0-(tpy1+6)*tpyd, adj=c(0,0), paste("EV =",round(spEV,1), "mm"), cex=cex1, xpd=NA)
    tpy1 <- 15 
    text(x=tpx0,y=tpy0-(tpy1+1)*tpyd, adj=c(0,0), paste("Energy Balance"), cex=cex1*1.2, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+2)*tpyd, adj=c(0,0), paste("Rn =",round(sum(WB$Rn),1), "mm"), cex=cex1, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+3)*tpyd, adj=c(0,0), paste("ETR =",round(spEV+spET,1), "mm"), cex=cex1, xpd=NA)
    text(x=tpx0,y=tpy0-(tpy1+4)*tpyd, adj=c(0,0), paste("H =",round(sum(WB$sH),1), "mm"), cex=cex1, xpd=NA)

  })
}







# plot budgets 
# with(WBa[itt],{
#   lines(c(sUTC,sUTC), c(0 , Pintercept), lwd=2, col="steelblue1" )   #dark blue: Throughfall
#   lines(c(sUTC,sUTC), c(Pintercept , (Pthroughf+Pintercept)), lwd=2, col="lightsteelblue1" )
#   points(sUTC, C_3D/10, pch=3, col=4, cex=psize)                          # blue:  Storage
#   points(sUTC, (dC+ylimC[2]/2), pch=16, col=1, cex=psize)               # black: delta Storage
#   points(sUTC, Drainage, pch=16, col="green", cex=psize*2)             # green: Drainage
#   points(sUTC, Dthroughf, pch=16, col="darkgreen", cex=psize*2)             # green: Drainage
#   points(sUTC, EV, pch=16, col=2, cex=psize)                           # red:   Evaporation
# })




# # plot budgets 
# # with(WBa[itt],{         
#   # cat(PF - (Pintercept + Pthroughf),"\n")
#   # cat(dC - (Pintercept + Dintercept - EV - Drainage),"\n")
#   #* Output graphic 
#   lines(c(sUTC,sUTC), c(ylimC[2] , ylimC[2]-fakstor*Pthroughf/dtc), lwd=2, col="dodgerblue" )   #dark blue: Throughfall
#   lines(c(sUTC,sUTC), c(ylimC[2]-fakstor*Pthroughf/dtc , ylimC[2]-fakstor*(Pthroughf+Pintercept)/dtc), lwd=2, col="steelblue1" )
#   points(sUTC, C_3D, pch=3, col=4, cex=psize)                          # blue:  Storage 
#   points(sUTC, dC+ylimC[2]/2, pch=16, col=1, cex=psize)               # black: delta Storage
#   # points(sUTC, Pintercept, pch=16, col=4, cex=psize)
#   # points(sUTC, Dintercept, pch=1, col="lightblue", cex=psize)
#   points(sUTC, Drainage, pch=16, col=3, cex=psize)                     # green: Drainage
#   points(sUTC, EV, pch=16, col=2, cex=psize)                           # red:   Evaporation
#  })
