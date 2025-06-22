#+
# NAME*: output_TS_ASTW.R
  # PURPOSE: define individual output/plots for a special case
  # e.g. save timeseries of selected areas  
# CALLING: source(file.path(path_sub,"<case>_output_TS.r")
# INPUT:
  # , Pthroughf, P_Intercept_3D, C_3D, ETR_3D, P_Drain_3D
# OUTPUT:
  #    
# REVISION HISTORY*:
  #   2019-11-22, RQ: 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# hcl.pals()

# complete fields onto HD ####
#  source(file.path(path_sub,"output_timestep.r"))

# YZ-CROSS SECTION ####
# Xcut => xc
if(F){
# if (itt==10)  stop("here sollte der output stehen")
  TSout <- sUTC
  if (itt == 1){  ## First Timestep Output ####
    figsize.uo <- 1000
    
    xc.x <- ceiling(nx/2)   # x-position of the cut
    xc.d <- 4               # width, half the number of layers for mean calculation 
    # e.g. if you set it to 2 then the average over 5 layers is build
    xc.d1 <- max(1, xc.x-xc.d) 
    xc.d2 <- min(nx, xc.x+xc.d)
    xc.nm <- xc.d2-xc.d1+1
    
    xc.PAD <- PAD[,,xc.d1]; main="PAD"
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.PAD <- xc.PAD+PAD[,,xc.i]}
      xc.PAD <- xc.PAD/xc.nm}
    x <- dy*(1:ncol(xc.PAD)); xc.xlim <- range(x)
    y <- dz*(1:nrow(xc.PAD)); xc.ylim <- range(y)
    zlimPAD <- range(as.numeric(xc.PAD))
    
    ### PAD ####
    # rasterImage2(x, y, z=t(xc.PAD), zlim = zlimPAD, cex.axis=1.5,z.cex=1.5
    #              , ncolors=256, palette = hcl.colors(100, "Greens 3", rev = T)
    #              , main=paste(main,", section along y at x=",xc.x, ", facing West")
    #              , xlab="North in m", ylab="Height in m"
    # )
    # contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
    #         add = TRUE, col = "forestgreen")   # brown is auch OK
    # grid(nx = ny, ny = nz, col = "darkgray") # Gitternetz
 
    # the limits in z needs to be fitted to the values of the event   
    # make a trial, during this the max Values in the plot domain 
    # will be calculated, print it and set the zlim
    zlimEV <- c(0,0.00023) #  range(as.numeric(xc.plot))
    zlimDr <- c(0,0.01) # range(as.numeric(xc.plot))
    zlimC <- c(0,1.5) #  range(as.numeric(xc.plot))
    zlimP <- c(0,0.01) #  range(as.numeric(xc.plot))
  }

  ## Each Timestep Output ########
  png(file.path(path_output_act_user, paste0(act, "_YZcut_atX",xc.x,"_",formatC(itt, width=3,flag="0"),".png")), width = figsize.uo, height = figsize.uo/16*9)
    plot.new()
    Titel <- paste0("yz-cross section, at x=",xc.x, ", facing West (mean over x-layer ", xc.d1, " to ", xc.d2,")")
    (main <- paste0(Titel, ", ", as.Date(metPP$UTC[nt%/%2]), "\n " ))
  
  ### Freilandniederschlag ####
    par(fig=c(0,1,0.6,1), new=TRUE)
    ylim = c(-0.001, max(metPP$PF)*2)
    # barplot(metPP$PF,ylim = ylim, xaxs="i", yaxs="i", col="lightblue",border = NA) # , names.arg = 1:28
    # ylim <- par()$usr[3:4]*2
    # par(new=TRUE)
    xlim <- range(metPP$UTC); xlim[1] <- xlim[1]-dt0/2; xlim[2] <- xlim[2]+dt0/2
    plot(metPP$UTC,metPP$PFi, type="n", xlim=xlim, ylim=ylim, xaxs="i", yaxs="i"
         , bty="n", axes=F, xlab="", ylab="", main=main) 
    axis.POSIXct(3, at=seq(min(metPP$UTC), max(metPP$UTC), by="hours"))
    abline(h = ylim[2])
    abline(v = sUTC , col = "red")    # Indikator fuer Zeitschritt
    cs.dC <- cumsum(WBa$dC)
    cs.throu <- cumsum(WBa$Pthroughf+WBa$Dthroughf)
    cs.EV <- cumsum(WBa$EV)
    segments(WFa$UTC,0,WFa$UTC, WFa$PF*600, col="lightblue")
    lines(WBa$UTC, cs.throu, pch=16, col=1 )
    lines(WBa$UTC, cs.dC, pch=16, col=4 )
    lines(WBa$UTC, cs.EV, pch=16, col=2 )
    legend("topright",c("Throughfall", "Storage", "Evaporation")
           , col=c(1,4,2), pch = c(NA,NA,NA), lty = c(1,1,1), lwd = c(2,2,2)
           , bty="n", ncol = 3)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Evaporation ####
    par(fig=c(0.7,1,0,0.7), new=TRUE)
    xc.plot <- EV[,,xc.d1]; main="Evaporation"
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+EV[,,xc.i]}
      xc.plot <- xc.plot/xc.nm}
    x <- dy*(1:ncol(xc.plot)); xc.xlim <- range(x)
    y <- dz*(1:nrow(xc.plot)); xc.ylim <- range(y)
    plot.new()
    image(x, y, z=t(xc.plot), zlim = zlimEV, cex.axis=1.5
          , col = hcl.colors(100, "reds 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
            add = TRUE, col = "lightgray",lwd=0.2)   # brown is auch OK
    
    
  ### Drainage ####
    par(fig=c(0.475,0.775,0,0.7), new=TRUE)
    xc.plot <- Drainage[,,xc.d1]; main="Drainage"
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+Drainage[,,xc.i]}
    xc.plot <- xc.plot/xc.nm}
    x <- dy*(1:ncol(xc.plot)); xc.xlim <- range(x)
    y <- dz*(1:nrow(xc.plot)); xc.ylim <- range(y)
    image(x, y, z=t(xc.plot), zlim = zlimDr, cex.axis=1.5
          , col = hcl.colors(100, "gray", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
            add = TRUE, col = "lightgray",lwd=0.2)   # brown is auch OK


  ### Storage #### 
    par(fig=c(0.225,0.525,0,0.7), new=TRUE)
    xc.plot <- C_3D[,,xc.d1]; main="Storage"
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+C_3D[,,xc.i]}
      xc.plot <- xc.plot/xc.nm}
    x <- dy*(1:ncol(xc.plot)); xc.xlim <- range(x)
    y <- dz*(1:nrow(xc.plot)); xc.ylim <- range(y)
    image(x, y, z=t(xc.plot), zlim = zlimC, cex.axis=1.5
          , col = hcl.colors(100, "Blues 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
            add = TRUE, col = "lightgray",lwd=0.2)   # brown is auch OK
    
    
  ### Retained Rain ####    
    par(fig=c(0.,0.3,0,0.7), new=TRUE)
    xc.plot <- Pintercept[,,xc.d1]; main="Retained Rain"
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+Pintercept[,,xc.i]}
      xc.plot <- xc.plot/xc.nm}
    x <- dy*(1:ncol(xc.plot)); xc.xlim <- range(x)
    y <- dz*(1:nrow(xc.plot)); xc.ylim <- range(y)
    image(x, y, z=t(xc.plot), zlim = zlimP, cex.axis=1.5
          , col = hcl.colors(100, "Blues 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
            add = TRUE, col = "lightgray",lwd=0.2)   # brown is auch OK
    #grid(nx = ny, ny = nz, col = "darkgray") # Gitternetz

  dev.off()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XY-TOPVIEW SUMS ####
# Sums over z: z-cut-sum zcs 
# sums from zcs.d1 up to zcs.d2
if(T){
  if (itt == 1){  ## First Timestep Output ####
    ## zcs initialisation ####
    # oma <- par()$oma
    # mar <- par()$mar 
    # omp <- oma*par("cra")[2]  # margin * line height in px
    # omp <- mar*par("cra")[2]  # margin height in px
    
    aspr <- ny/nx
    figsize.uo.zcs <- min(max(nx,ny),60)*20  # max number of pixel
    figsize.uo.zcs <- max(figsize.uo.zcs, 200)  # min number of pixel
    if (nx > ny) ppg <- figsize.uo.zcs/nx else # pixel per grid
                 ppg <- figsize.uo.zcs/ny
    figsize.uo.zcs.x <- round(ppg*nx)
    figsize.uo.zcs.y <- round(ppg*ny)*0.9
    txtheight <- par("cra")[2]
    figsize.uo.zcs.x <- round(ppg*nx)+6*txtheight
    figsize.uo.zcs.y <- round(ppg*ny)+6*txtheight
    
    zcs.z0 <- 2 # m, lower boundary
    zcs.d1 <- max(1, zcs.z0) 
    zcs.d2 <- nz
    zcs.nm <- zcs.d2-zcs.d1+1
    
    ## PAD ####
    # lower boundary for PAD
    zcs.d1PAD <- 2   # m <<<<<<< change appropriately<<<<<<<<<<<====
    # switch(.Platform$OS.type
    #        , "windows" = {main="PAD in m\262/m\262"}
    #        , "unix" = {main="PAD in m²/m²"}
    # )
    main=expression(paste("PAD in ", m^2/m^2))
    zcs.PAI <- PAD[zcs.d1PAD,,]    # initial value
    if (nz>1) for (zcs.i in (zcs.d1PAD+1):zcs.d2){zcs.PAI <- zcs.PAI+PAD[zcs.i,,]}
# the calculation above ist not realy necessary as PAI is already summarized in Load_Static_Driver.r,
# thus zcs.PAI == PADc[1,,]    # <=== to check
    if ( is.null(ncol(zcs.PAI)) ) x <- dx else x <- dx*(1:ncol(zcs.PAI)); zcs.xlim <- range(x)
    if ( is.null(nrow(zcs.PAI)) ) y <- dy else y <- dy*(1:nrow(zcs.PAI)); zcs.ylim <- range(y)
    zlimPAD <- c(0,max(zcs.PAI))
    # hist(as.numeric(PAD))
    # plot.new(); par(new=TRUE); plot(c(x,1,1,1,1,1,1,1,1), y)
    fwrite(as.data.table(zcs.PAI), file.path(path_output_act_user, paste0(act, "_PAI_", Tstamp,".csv")))
    png(file.path(path_output_act_user, paste0(act, "_PAI_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      rasterImage2(x, y, z=t(zcs.PAI), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimPAD  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(100, "Greens 3", rev = T)
                   , main=main, xlab="East in m", ylab="North in m"
      )
      points(ixref*dx,iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
      # grid(nx = nx, ny = ny, col = "darkgray") # Gitternetz
    dev.off()

    # contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
    #         add = TRUE, col = "forestgreen")   # brown is auch OK
    # grid(nx = nx, ny = ny, col = "darkgray") # Gitternetz
    # 
    # for (zcs.ii in 1:nz){
    #   png(file.path(path_output, "_usr_output_", Tstamp, "/", "ASTW0_exmpl_07_PAD_XYsection_at_",formatC(zcs.ii, width=3,flag="0"),".png"), width = figsize.uo.zcs/2, height = figsize.uo.zcs/2)
    #     rasterImage2(x, y, z=t(PAD[zcs.ii,,]), zlim = c(0,1.5), cex.axis=1.5,z.cex=1.5
    #                  , ncolors=256, palette = hcl.colors(100, "Terrain", rev = T)
    #                  , main=paste(main, "z = ", formatC(zcs.ii, width=3,flag="0"))
    #                  , xlab="East in m", ylab="North in m"
    #     )
    #     points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
    #     segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    #   dev.off()
    # }
    # 
    
    # create the result fields
    # zcs.EV <- EV[zcs.d1,,]; main="Evaporation"
    # if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.EV <- zcs.EV+EV[zcs.i,,]}
    zcs.EV <- EV*dtc
    zcs.ET <- ET*dtc
    zcs.Rn <- Rn_3D*dtc/L_3D
    zcs.H  <- sH_3D*dtc
    zcs.TF <- (Pthroughf[1,,]+Dthroughf[1,,])*dtc
    
  } else { ## Each Timestep Output ########
    ### zsc collection during the run ####
    zcs.EV <- zcs.EV + EV*dtc
    zcs.ET <- zcs.ET + ET*dtc
    zcs.Rn <- zcs.Rn + Rn_3D*dtc/L_3D
    zcs.H  <- zcs.H  + sH_3D*dtc
    zcs.TF <- zcs.TF + (Pthroughf[1,,]+Dthroughf[1,,])*dtc
    #if (!exists("zcs.H")) zcs.H  <- sH_3D*dtc else zcs.H  <- zcs.H  + sH_3D*dtc
  }

  
  
  ## Final Timestep Output ####  
  # zsc final plots => usr_output
  # if ( (ito == ntu) & (dt0r == 0) ){
  if ( ((ito == ntu) & (dt0r == 0)) | (!EvOn & EvOn.prev) ){
      # aspr <- ny/nx
    # figsize.uo.zcs <- min(max(nx,ny),60)*20  # max number of pixel
    # ppg <- figsize.uo.zcs/nx else # pixel per grid
    # zcs initialisation
    # oma <- par()$oma
    # mar <- par()$mar 
    # omp <- oma*par("cra")[2]  # margin * line height in px
    # omp <- mar*par("cra")[2]  # margin height in px
    
    # the png device has at the least 2 additional lines as margin
    txtheight <- par("cra")[2]
    figsize.uo.zcs.x <- round(ppg*nx)+6*txtheight
    figsize.uo.zcs.y <- round(ppg*ny)+6*txtheight
    
    
    Titel <- paste0("top view, sums over z-layer from ", zcs.d1, " to ", zcs.d2," (1 m to 36 m), \n Site: ICOS station Tharandter Wald")
    x <- dx*(1:ncol(zcs.PAI)); zcs.xlim <- range(x)
    y <- dy*(1:nrow(zcs.PAI)); zcs.ylim <- range(y)

    ### EV ####
    png(file.path(path_output_act_user, paste0(act, "_EVsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      zcs.plot <- zcs.EV[zcs.d1,,]; main="Evaporation from forest canopy (in mm)"
      if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.EV[zcs.i,,]}
      fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_EVsum", EvNo_str, "_", Tstamp,".csv")))
      zlimzcsEV <- c(0.0,max(as.numeric(zcs.plot), na.rm = T )*0.8)
      rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimzcsEV  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(100, "Blues 3", rev = T)   # Terrain, Spectral
                   , main=paste0(main,"\n",Titel)
                   , xlab="East in m", ylab="North in m"
      )
      contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
              add = TRUE, col = "forestgreen")   # brown is auch OK
      points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()

    ### ET ####
    png(file.path(path_output_act_user, paste0(act, "_ETsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      zcs.plot <- zcs.ET[zcs.d1,,]; main="Transpiration from forest canopy (in mm)"
      if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.ET[zcs.i,,]}
      fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_ETsum", EvNo_str, "_", Tstamp,".csv")))
      zlimzcsET <- c(0.0,max(as.numeric(zcs.plot), na.rm = T ))
      rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimzcsET  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(100, "Terrain", rev = T)
                   , main=paste0(main,"\n",Titel)
                   , xlab="East in m", ylab="North in m", xaxs="i", yaxs="i"
      )
      contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
              add = TRUE, col = "forestgreen")   # brown is auch OK
      points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()
    
    ### Rn ####
    png(file.path(path_output_act_user, paste0(act, "_Rnsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      zcs.plot <- zcs.Rn[zcs.d1,,]; main="Net Radiation input to forest canopy (in mm)"
      if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.Rn[zcs.i,,]}
      fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_Rnsum", EvNo_str, "_", Tstamp,".csv")))
      zlimzcsRn <- c(0.0,max(as.numeric(zcs.plot), na.rm = T ))
      rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimzcsRn  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(100, "Heat", rev = T)
                   , main=paste0(main,"\n",Titel)
                   , xlab="East in m", ylab="North in m"
      )
      contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
              add = TRUE, col = "forestgreen")   # brown is auch OK
      points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()

    ### ETR ####
    zcs.ETR <- zcs.EV+zcs.ET
    png(file.path(path_output_act_user, paste0(act, "_ETRsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      zcs.plot <- zcs.ETR[zcs.d1,,]; main="Evapotranspiration of forest canopy (in mm)"
      if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.ETR[zcs.i,,]}
      fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_ETRsum", EvNo_str, "_", Tstamp,".csv")))
      zlimzcsETR <- c(0.0,max(as.numeric(zcs.plot), na.rm = T ))
      rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimzcsETR  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(100, "Blues 3", rev = T) # hcl.colors(100, "Terrain", rev = T)
                   , main=paste0(main,"\n",Titel)
                   , xlab="East in m", ylab="North in m", xaxs="i", yaxs="i"
      )
      contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
              add = TRUE, col = "forestgreen")   # brown is auch OK
      points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()
    
    ### H ####
    # zcs.H <- zcs.Rn-zcs.ETR
    # zcs.H  <- zcs.H  + H_3D*dtc
    png(file.path(path_output_act_user, paste0(act, "_Hsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
      par(fig=c(0.,1,0,1))
      zcs.plot <- zcs.H[zcs.d1,,]; main="Sensible Heat from forest canopy (in mm)"
      if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.H[zcs.i,,]}
      fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_Hsum", EvNo_str, "_", Tstamp,".csv")))
      zlimzcsH <- c(-10,10) # range(as.numeric(zcs.plot), na.rm = T )
      rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimzcsH  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                   , ncolors=256, palette = hcl.colors(255, "Zissou 1", rev = F)     # RdYlBu, Spectral
                   , main=paste0(main,"\n",Titel)
                   , xlab="East in m", ylab="North in m", xaxs="i", yaxs="i"
      )
      contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
              add = TRUE, col = "forestgreen")   # brown is auch OK
      points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
      segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()

    ### Throughfall ####
    # zcs.TF <- zcs.TF + (Pthroughf[1,,]+Dthroughf[1,,])*dtc
    png(file.path(path_output_act_user, paste0(act, "_TFsum", EvNo_str, "_", Tstamp,".png")), width = figsize.uo.zcs.x, height = figsize.uo.zcs.y)
    par(fig=c(0.,1,0,1))
    zcs.plot <- zcs.TF; main=paste0("Throughfall below layer", zcs.d1," (in mm)")
    # if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.plot <- zcs.plot+zcs.H[zcs.i,,]}
    fwrite(as.data.table(zcs.plot), file.path(path_output_act_user, paste0(act, "_TFsum", EvNo_str, "_", Tstamp,".csv")))
    zlimzcsTF <- range(as.numeric(zcs.plot), na.rm = T )
    rasterImage2(x, y, z=t(zcs.plot), cex.axis=1.5,z.cex=1.5
                 , zlim = zlimzcsTF  #, xlim = c(0,nx)+0.5, ylim=c(0,ny)+0.5
                 , ncolors=256, palette = hcl.colors(255, "Zissou 1", rev = T)     # RdYlBu, Spectral
                 , main=paste0(main,"\n",Titel)
                 , xlab="East in m", ylab="North in m", xaxs="i", yaxs="i"
    )
    contour(x, y, t(zcs.PAI), levels = seq(zlimPAD[1], zlimPAD[2], by = 2),
            add = TRUE, col = "forestgreen")   # brown is auch OK
    points(ixref*dx, iyref*dy, pch=16, col=2, cex=3)
    segments(c(7,14)-2-x1.ref, c(-2,-10)-y1.ref, c(14,21)-2-x1.ref, c(-10,-3)-y1.ref, lwd=3  )  # interception chute
    dev.off()

      
    # create the result fields
    # zcs.EV <- EV[zcs.d1,,]; main="Evaporation"
    # if (nz>1) for (zcs.i in (zcs.d1+1):zcs.d2){zcs.EV <- zcs.EV+EV[zcs.i,,]}
    zcs.EV <- EV*dtc
    zcs.ET <- ET*dtc
    zcs.Rn <- Rn_3D*dtc/L_3D
    zcs.H  <- sH_3D*dtc
    zcs.TF <- (Pthroughf[1,,]+Dthroughf[1,,])*dtc
    
    
    
  } # end of if ( ((ito == ntu) & (dt0r == 0)) | (!EvOn & EvOn.prev) ){

  
} # end of # XY-TOPVIEW SUMS ####


# XY-CROSS SECTION ####
# Zcut => zc
if(F){
  

}













# das Folgende ist noch nicht angepasst !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# xyz => zxy

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Verdunstung in den einzelnen Messfeldern
# # Messfeld 1 ------------------------------------------------------------------------------------
# if(MF1 == 1) {
# if(!exists("EV_mf1")) { EV_mf1 <- rep(NA,it2)}
# if(!exists("ET_mf1")) {  ET_mf1 <- rep(NA,it2)}
# if(!exists("ETR_mf1")) {  ETR_mf1 <- rep(NA,it2)}
# xmf11 <- 167 ; xmf12 <- 187 ; ymf11 <- 41 ; ymf12 <- 61
# EV_mf1[tx] <- (sum(EV[((xmf11 - ix1) + 1):((xmf12 - ix1) + 1),((ymf11 - iy1) + 1):((ymf12 - iy1) + 1),]))/441
# ET_mf1[tx] <- (sum(ET[((xmf11 - ix1) + 1):((xmf12 - ix1) + 1),((ymf11 - iy1) + 1):((ymf12 - iy1) + 1),]))/441
# ETR_mf1[tx] <- (sum(ETR[((xmf11 - ix1) + 1):((xmf12 - ix1) + 1),((ymf11 - iy1) + 1):((ymf12 - iy1) + 1),]))/441
# }
# 
# # Messfeld 2 ------------------------------------------------------------------------------------
# if (MF2 == 1) {
# if(!exists("EV_mf2")) { EV_mf2 <- rep(NA,it2)}
# if(!exists("ET_mf2")) {ET_mf2 <- rep(NA,it2)}
# if(!exists("ETR_mf2")) {ETR_mf2 <- rep(NA,it2)}
# xmf21 <- 185 ; xmf22 <- 191 ; ymf21 <- 15 ; ymf22 <- 35
# EV_mf2[tx] <- (sum(EV[((xmf21 - ix1) + 1):((xmf22 - ix1) + 1),((ymf21 - iy1) + 1):((ymf22 - iy1) + 1),]))/441
# ET_mf2[tx] <- (sum(ET[((xmf21 - ix1) + 1):((xmf22 - ix1) + 1),((ymf21 - iy1) + 1):((ymf22 - iy1) + 1),]))/441
# ETR_mf2[tx] <- (sum(ETR[((xmf21 - ix1) + 1):((xmf22 - ix1) + 1),((ymf21 - iy1) + 1):((ymf22 - iy1) + 1),]))/441
# }
# 
# # Messfeld 3 ------------------------------------------------------------------------------------
# if (MF3 == 1) {
# if(!exists("EV_mf3")) {EV_mf3 <- rep(NA,it2)}
# if(!exists("ET_mf3")) {ET_mf3 <- rep(NA,it2)}
# if(!exists("ETR_mf3")) {ETR_mf3 <- rep(NA,it2)}
# 
# xmf31 <- 89 ; xmf32 <- 109 ; ymf31 <- 9 ; ymf32 <- 29
# EV_mf3[tx] <- (sum(EV[((xmf31 - ix1) + 1):((xmf32 - ix1) + 1),((ymf31 - iy1) + 1):((ymf32 - iy1) + 1),]))/441
# ET_mf3[tx] <- (sum(ET[((xmf31 - ix1) + 1):((xmf32 - ix1) + 1),((ymf31 - iy1) + 1):((ymf32 - iy1) + 1),]))/441
# ETR_mf3[tx] <- (sum(ETR[((xmf31 - ix1) + 1):((xmf32 - ix1) + 1),((ymf31 - iy1) + 1):((ymf32 - iy1) + 1),]))/441
# }
# 
