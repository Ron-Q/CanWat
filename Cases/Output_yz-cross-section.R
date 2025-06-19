#+
# NAME*: Output_yz-cross-section.R
  # PURPOSE: define individual output/plots for a special case
  # e.g. save timeseries of selected areas  
# CALLING: source(file.path(path_sub,"<case>_output_TS.r")
# INPUT:
  # , Pthroughf, P_Intercept_3D, C_3D, ETR_3D, P_Drain_3D
# OUTPUT:
  #    
# PACKAGES:
  # fields
# REVISION HISTORY*:
  #   2019-11-22, RQ: 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# hcl.pals()

mean_over_all <- F   # Mean over all x layers?

# complete fields onto HD ####
#  source(file.path(path_sub,"output_timestep.r"))


# YZ-CROSS SECTION ####
# Xcut => xc
# if ((itt == 1) & (SD.dims[2] == 1)) cat("Y dimension is 1, no YZ-cross-section is plotted \n")   
# if(T & (SD.dims[2] > 1)){

# stop("Output_yz-cross-section.R")

if(T){
  # if (itt==10)  stop("here sollte der output stehen")
  TSout <- sUTC
  if (itt == 1){
    figsize.uo <- 1000
    
    if (mean_over_all)  {
      xc.d1 <- 1
      xc.d2 <- nx
      xc.x <- ceiling(nx/2)
    } else {
      xc.x <- ceiling(nx/2)   # x-position of the cut
      xc.d <- 2               # width, half the number of layers for mean building 
      # e.g. if you set it to 2 then the average over 5 layers is build
      xc.d1 <- max(1, xc.x-xc.d) 
      xc.d2 <- min(nx, xc.x+xc.d)
    }
    xc.nm <- xc.d2-xc.d1+1
    
    xc.PAD <- PAD[-1,,xc.d1]; main="PAD"
    if (ny ==1) xc.PAD <- array(xc.PAD , dim =c(length(xc.PAD),1))
    if (nx>1) {for (xc.i in (xc.d1+1):xc.d2){xc.PAD <- xc.PAD+PAD[-1,,xc.i]}
               xc.PAD <- xc.PAD/xc.nm}
    #* plot PAD ####
    png(file.path(path_output_act, paste0(act,fnam.sd, "PAD.png")), width = figsize.uo, height = figsize.uo/16*9) 
      plot.new()
      par(fig=c(0,0.5,0,1))
      x <- dx*(1:nx-0.5)     ; xc.xlim <- range(x)+c(-dx/2,+dx/2)
      y <- dy*(1:ny-0.5)     ; xc.ylim <- range(y)+c(-dy/2,+dy/2)
      PAIf <- array(PADc[1,,], dim=c(ny, nx))
      if (ny == 1) {PAIf <- rbind(PAIf,PAIf); y <- (y-1):y}
      if (nx == 1) {PAIf <- cbind(PAIf,PAIf); x <- (x-1):x}
      zlimPAI <- range(as.numeric(PAIf))   #; xlim <- range(x); ylim <- range(y)
      if(diff(zlimPAI) == 0) zlimPAI <- zlimPAI[1]+c(-0.5,0.5)
      rasterImage2(x, y, z=t(PAIf), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimPAI, xlim=xc.xlim, ylim=xc.ylim
                   , ncolors=256, palette = hcl.colors(100, "Greens 3", rev = T)
                   , main=paste0("Static Driver: ", fnam.sd, ", PAI")
                   , ylab="North in m", xlab="East in m"
      )   # invokes extrapolation as x an y are smaller then xlim and ylim
      grid(nx = nx, ny = ny, col = "darkgray") # Gitternetz

      par(fig=c(0.5,1,0,1), new=TRUE)
      main <- paste0("PAD YZcut at X",xc.x)
      x <- dy*(1:ny-0.5)      ; xc.xlim <- range(x)+c(-dy/2,+dy/2)
      y <- dz*((2:nz)-1.5); xc.ylim <- range(y)+c(-dz/2,+dz/2)
      xc.PAD <- array(xc.PAD, dim=c(nz-1, ny))
      # if (nz == 1) {xc.PAD <- rbind(xc.PAD,xc.PAD); y <- 1:(2*y)/2}
      xc.PAD.p <- xc.PAD
      if (ny == 1) {xc.PAD.p <- cbind(xc.PAD,xc.PAD); x <- (x-1):x}
      zlimPAD <- range(as.numeric(xc.PAD.p))
      xlim <- range(x); ylim <- range(y)+c(-dz/2,+dz/2)
      rasterImage2(x, y, z=t(xc.PAD.p), cex.axis=1.5,z.cex=1.5
                   , zlim = zlimPAD, xlim=xc.xlim, ylim=xc.ylim
                   , ncolors=256, palette = hcl.colors(100, "Greens 3", rev = T)
                   , main=paste(main,", section along y at x=",xc.x, ", facing West")
                   , xlab="North in m", ylab="Height in m"
      ) 
      grid(nx = nx, ny = ny+1, col = "darkgray") # Gitternetz
    dev.off()
    
    # other plot options
    # rasterImage(... , xpd=T)
    # rasterImage(t(xc.PAD.p),0, 0, 1, 15) # verlangt z = 0..1
    # image(x, y, t(xc.PAD.p)) # 

    # prepare plots of the water balance ####
    # the limits in z needs to be fitted to the values of the event   
    # make a trial, during this the max Values in the plot domain 
    # will be calculated, print it and set the zlim
    # the fluxes are in mm/s/voxel
    EV.xSumMax <- 0
    Dr.xSumMax <- 0
    C.xSumMax <- 0
    P.xSumMax <- 0
    # print(paste( EV.xSumMax, Dr.xSumMax, C.xSumMax, P.xSumMax))
    
    zlimEV <- c(0,0.005) # /nz*10 
    zlimDr <- c(0,0.16) # /nz*10
    zlimC <- c(0,45) # /nz*10
    zlimP <- c(0,0.1) # /nz*10
    
    # zlimEV <- c(0,0.005) # /nz*10 
    # zlimDr <- c(0,0.16) # /nz*10
    # zlimC <- c(0,45) # /nz*10
    # zlimP <- c(0,0.1) # /nz*10
    
    pPAD <- which(xc.PAD > 0) # plot PAD at these grids
    PADbg <- xc.PAD*0
#    PADbg[1:255] <- 1:255
    PADbg[pPAD] <- 2
    cs <- rev(blue2green(255))
    cs[1] <- "#FFFFFD"
    # plot.new()
    # image(x, y, z=t(PADbg), zlim = c(1,255), cex.axis=1.5, col = cs)
    # rasterImage2(x, y, z=t(PADbg), zlim = c(1,255), cex.axis=1.5, ncolors=256, palette = cs)

  }

  png(file.path(path_output_act_user, paste0(act, "_YZcut_atX",xc.x,"_",formatC(itt, width=3,flag="0"),".png")), width = figsize.uo, height = figsize.uo/16*9) 
    plot.new()
    Titel <- paste0("yz-cross section, at x=",xc.x, ", facing West (mean over x-layer ", xc.d1, " to ", xc.d2,")")
    (main <- paste0(Titel, ", ", as.Date(metPP$UTC[1]), "\n " ))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Course ####
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

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # XZ-Crosscut ####
    x <- dy*(1:ny-0.5)  ; xc.xlim <- range(x)+c(-dy/2,+dy/2) #; xc.xlim <- range(x)+c(-dy,+0)
    y <- dz*((2:nz)-1.5); xc.ylim <- range(y)+c(-dz/2,+dz/2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #* Evaporation ####
  #* 
    # write(EV,file = "d:/b_Programming/0_R_lib/y_CanWat/Output/CaseTest.csv",append=T, sep=",")
    par(fig=c(0.7,1,0,0.7), new=TRUE)
    xc.plot <- EV[-1,,xc.d1]; main="Evaporation"
    if (ny ==1) xc.plot <- array(xc.plot , dim =c(length(xc.plot),1))
    if (nx>1) for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+EV[-1,,xc.i]}
    EV.xSumMax <- max(xc.plot,EV.xSumMax)
    if (zlimEV[2] < EV.xSumMax) print(paste(fnam.out, " zlimEV is to small"))
    xc.plot <- PADbg+xc.plot/zlimEV[2]*253 # scale between 0 and 253
    plot.new()
    image(x, y, z=t(xc.plot), zlim = c(0,255), cex.axis=1.5
          , col = cs # hcl.colors(255, "reds 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    # if (ny > 1) contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
    #         add = TRUE, col = "lightgray",lwd=0.1)   # brown is auch OK
    

  #* Drainage ####
    par(fig=c(0.475,0.775,0,0.7), new=TRUE)
    xc.plot <- Drainage[-1,,xc.d1]; main="Drainage"
    if (ny ==1) xc.plot <- array(xc.plot , dim =c(length(xc.plot),1))
    if (nx>1) for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+Drainage[-1,,xc.i]}
    Dr.xSumMax <- max(xc.plot,Dr.xSumMax)
    if (zlimDr[2] < Dr.xSumMax) print(paste(fnam.out, " zlimDr is to small"))
    xc.plot <- PADbg+xc.plot/zlimDr[2]*253 # scale between 0 and 253
    image(x, y, z=t(xc.plot), zlim = c(0,255), cex.axis=1.5
          , col = cs # hcl.colors(100, "gray", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    # if (ny > 1) contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
    #         add = TRUE, col = "lightgray",lwd=0.1)   # brown is auch OK


  #* Storage #### 
    par(fig=c(0.225,0.525,0,0.7), new=TRUE)
    xc.plot <- C_3D[-1,,xc.d1]; main="Storage"
    if (ny ==1) xc.plot <- array(xc.plot , dim =c(length(xc.plot),1))
    if (nx>1) for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+C_3D[-1,,xc.i]}
    C.xSumMax <- max(xc.plot,C.xSumMax)
    if (zlimC[2] < C.xSumMax) print(paste(fnam.out, " zlimC is to small"))
    xc.plot <- PADbg+xc.plot/zlimC[2]*253 # scale between 0 and 253
    image(x, y, z=t(xc.plot), zlim = c(0,255), cex.axis=1.5
          , col = cs # hcl.colors(100, "Blues 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    # if (ny > 1) contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
    #         add = TRUE, col = "lightgray",lwd=0.1)   # brown is auch OK
    
    
  #* Retained Rain ####    
    par(fig=c(0.,0.3,0,0.7), new=TRUE)
    xc.plot <- Pintercept[-1,,xc.d1]; main="Retained Rain"
    if (ny ==1) xc.plot <- array(xc.plot , dim =c(length(xc.plot),1))
    if (nx>1) for (xc.i in (xc.d1+1):xc.d2){xc.plot <- xc.plot+Pintercept[-1,,xc.i]}
    P.xSumMax <- max(xc.plot,P.xSumMax)
    if (zlimP[2] < P.xSumMax) print(paste(fnam.out, " zlimP is to small"))
    xc.plot <- PADbg+xc.plot/zlimP[2]*253 # scale between 0 and 253
    image(x, y, z=t(xc.plot), zlim = c(0,255), cex.axis=1.5 #,z.cex=1.5
          , col = cs # hcl.colors(100, "Blues 3", rev = T)
          , main=paste(main)
          , xlab="North in m", ylab="Height in m"
    )
    # if (ny > 1) contour(x, y, t(xc.PAD), levels = seq(zlimPAD[1], zlimPAD[2], by = 0.5),
    #                     add = TRUE, col = "lightgray",lwd=0.1)   # brown is auch OK
    #grid(nx = ny, ny = nz, col = "darkgray") # Gitternetz
        
  dev.off()
}



# is.null(dim(xc.plot))

