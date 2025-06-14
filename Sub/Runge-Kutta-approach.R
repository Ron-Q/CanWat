#+
# NAME*: Runge-Kutta-approach.R
# PURPOSE*: Runge-Kutta approach of the 4th order to solve
          # dC/dt  = I(t) - E(t) - D(t)
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"Runge-Kutta-approach.R",sep=""))
# INPUTS*:
    # dtc :  current time step 
    # Pintercept
    # Dintercept
    # C_3D
# OUTPUT*:
    # EV, Drainage, ETR
    # ETR, ET_t, Ev_w
# SUBROUTINES: 
    # source(file.path(path_sub,"ETR_funct.R"))
    # source(file.path(path_sub,"Drain_funct.R"))
# REFERENCE: https://de.wikipedia.org/wiki/Klassisches_Runge-Kutta-Verfahren, and rmutil::runge-kutta
# REVISION HISTORY*:
    # 2019-10-12 : RQ
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RK.test <- F

# f1 ################
EV <- ETR_funct(C_3D)
Drainage <- Drain_funct(Con=C_3D, Sat=S_3D, Dmin=Dmin, bd=bd, D_matrix=Drainage)    # Con=C_3D; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage
f1 <- dtc * (interc - EV - Drainage )
if (RK.test){
  Con=C_3D; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage
  cat("C_3D(sum, min, max)=", signif(sum(C_3D),3), signif(min(C_3D),3), signif(max(C_3D),3),"\n")
  cat("ranges: C_3D:", signif(range(C_3D),2),", interc:", signif(range(interc)*dtc,2), "EV:", signif(range(EV)*dtc,2), "Drainage:", signif(range(Drainage)*dtc,2), "f1:", signif(range(f1),2),"\n" )
  cat(signif( range((C_3D[iPAD]-S_3D[iPAD])) ,2),"| ", signif( range((C_3D[iPAD]-S_3D[iPAD])/S_3D[iPAD]) ,2),"| ", signif( range(  bd*((C_3D[iPAD]-S_3D[iPAD])/S_3D[iPAD])   ) ,2))
  # cat("sum(f1): ", sum(f1), "sum(EV)*dtc: ", sum(EV)*dtc, "sum(Drainage)*dtc: ", sum(Drainage)*dtc , "\n")
}
ineg <- which(C_3D < -f1)   # identical with (C_3D+f1) < 0, but maybe faster
if (length(ineg) > 0)  {
  Sneg <- (C_3D[ineg] + f1[ineg])/dtc                              # negative storage flux [:o], either EV or Drainage is to high
  iDred <- which((Drainage[ineg] + Sneg) > 0)                      # can I reduce the drainage to avoid the excessive emptying of the storage?
  if (length(iDred) > 0) {
    Drainage[ineg[iDred]] <- Drainage[ineg[iDred]] + Sneg[iDred]   # yes! - then reduce Drainage for these indexes
    EV[ineg[-iDred]] <- EV[ineg[-iDred]] + Drainage[ineg[-iDred]] + Sneg[-iDred] # and reduce evaporation with that part which cannot equalised by Drainage 
    Drainage[ineg[-iDred]] <- 0                                    # set drainage to 0 for all other cells 
  } else {
      EV[ineg] <- EV[ineg] + Drainage[ineg] + Sneg                 # No! - for all indexes, reduce evaporation with that part which cannot equalised by Drainage
      Drainage[ineg] <- 0                                          # and set drainage to 0
  }
  f1 <- dtc * (interc - EV - Drainage )
}
EVm <- EV/6; Drainm <- Drainage/6

# f2 ################
EV <- ETR_funct(C_3D+f1/2)
Drainage <- Drain_funct(Con=C_3D+f1/2, Sat=S_3D, Dmin=Dmin, bd=bd, D_matrix=Drainage)
f2 <- dtc * (interc - EV - Drainage )
if (RK.test){
  Con=C_3D+f1/2; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage
  cat("C_3D+f1/2(sum, min, max)=", signif(sum(C_3D+f1/2),3), signif(min(C_3D+f1/2),3), signif(max(C_3D+f1/2),3),"\n")
  cat("ranges: C_3D:", signif(range(C_3D),2),", interc:", signif(range(interc)*dtc,2), "EV:", signif(range(EV)*dtc,2), "Drainage:", signif(range(Drainage)*dtc,2), "f2:", signif(range(f2),2),"\n" )
  # cat("sum(f2): ", sum(f2), "sum(EV)*dtc: ", sum(EV)*dtc, "sum(Drainage)*dtc: ", sum(Drainage)*dtc , "\n")
}
ineg <- which(C_3D < -f2)
if (length(ineg) > 0)  {
  Sneg <- (C_3D[ineg] + f2[ineg])/dtc            
  iDred <- which((Drainage[ineg] + Sneg) > 0)     
  if (length(iDred) > 0) {
    Drainage[ineg[iDred]] <-  Drainage[ineg[iDred]] + Sneg[iDred]     
    EV[ineg[-iDred]] <- EV[ineg[-iDred]] + Drainage[ineg[-iDred]] + Sneg[-iDred]            
    Drainage[ineg[-iDred]] <- 0
  } else {
    EV[ineg] <- EV[ineg] + Drainage[ineg] + Sneg            
    Drainage[ineg] <- 0
  }
  f2 <- dtc * (interc - EV - Drainage )
}
EVm <- EVm + EV/3;     Drainm <- Drainm + Drainage/3

# f3 ################
EV <- ETR_funct(C_3D+f2/2)
Drainage <- Drain_funct(Con=C_3D+f2/2, Sat=S_3D, Dmin=Dmin, bd=bd, D_matrix=Drainage)
f3 <- dtc * (interc - EV - Drainage )
if (RK.test){
  Con=C_3D+f2/2; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage
  cat("C_3+f2/2(sum, min, max)=", signif(sum(C_3D+f2/2),3), signif(min(C_3D+f2/2),3), signif(max(C_3D+f2/2),3),"\n")
  cat("ranges: C_3D:", signif(range(C_3D),2),", interc:", signif(range(interc)*dtc,2), "EV:", signif(range(EV)*dtc,2), "Drainage:", signif(range(Drainage)*dtc,2), "f3:", signif(range(f3),2),"\n" )
  # cat("sum(f3): ",  sum(f3), "sum(EV)*dtc: ", sum(EV)*dtc, "sum(Drainage)*dtc: ", sum(Drainage)*dtc , "\n")
}  
ineg <- which(C_3D < -f3)
if (length(ineg) > 0)  {
  Sneg <- (C_3D[ineg] + f3[ineg])/dtc             # negative storage flux
  iDred <- which((Drainage[ineg] + Sneg) > 0)     
  if (length(iDred) > 0) {
    Drainage[ineg[iDred]] <- Drainage[ineg[iDred]] + Sneg[iDred]     # Drainage = Drainage + C_3D/dtc + f3/dtc, is equal to: Drainage = C_3D/dtc + interc - EV
    EV[ineg[-iDred]] <- EV[ineg[-iDred]] + Drainage[ineg[-iDred]] + Sneg[-iDred]   # is equal to: EV = C_3D/dtc + interc          
    Drainage[ineg[-iDred]] <- 0
  } else {
    EV[ineg] <- EV[ineg] + Drainage[ineg] + Sneg           
    Drainage[ineg] <- 0
  }
  f3 <- dtc * (interc - EV - Drainage )  #   (ineg <- which(C_3D < -f3))             # check for still negative storage flux
  f3[C_3D < -f3] <- -C_3D[C_3D < -f3]
}
EVm <- EVm + EV/3;    Drainm <- Drainm + Drainage/3

# f4 ################
EV <- ETR_funct(C_3D+f3)
Drainage <- Drain_funct(Con=C_3D+f3, Sat=S_3D, Dmin=Dmin, bd=bd, D_matrix=Drainage)
f4 <- dtc * (interc - EV - Drainage )
if (RK.test){
  Con=C_3D+f3; Sat=S_3D; Dmin=Dmin; bd=bd; D_matrix=Drainage
  cat("C_3D+f3(sum, min, max)=", signif(sum(C_3D+f3),3), signif(min(C_3D+f3),3), signif(max(C_3D+f3),3),"\n")
  cat("ranges: C_3D:", signif(range(C_3D),2),", interc:", signif(range(interc)*dtc,2), "EV:", signif(range(EV)*dtc,2), "Drainage:", signif(range(Drainage)*dtc,2), "f4:", signif(range(f4),2),"\n" )
}  
ineg <- which(C_3D < -f4)                                             # i.e. (C_3D + f1 < 0)
if (length(ineg) > 0)  {                                              # the storage is more then empty [:o]
  Sneg <- (C_3D[ineg] + f4[ineg])/dtc                                 # means negative storage 
  iDred <- which((Drainage[ineg] + Sneg) > 0)                         # is there enough drainage to reduce the imbalance?
  if (length(iDred) > 0) {                                            # yes!
    Drainage[ineg[iDred]] <- Drainage[ineg[iDred]] + Sneg[iDred]                  # then reduce Drainage for these indexes
    EV[ineg[-iDred]] <- EV[ineg[-iDred]] + Drainage[ineg[-iDred]] + Sneg[-iDred]  # and reduce evaporation with that part which cannot equalised by Drainage          
    Drainage[ineg[-iDred]] <- 0                                                   # and set drainage to 0
  } else {
    EV[ineg] <- EV[ineg] + Drainage[ineg] + Sneg           
    Drainage[ineg] <- 0
  }
  f4 <- dtc * (interc - EV - Drainage )
  f4[C_3D < -f4] <- -C_3D[C_3D < -f4]
}
EVm <- EVm+EV/6; Drainm <- Drainm + Drainage/6
  # cat("sum(f4): ",  sum(f4), ", sum(EV)*dtc: ", sum(EV)*dtc, ", sum(Drainage)*dtc: ", sum(Drainage)*dtc , "\n")

# estimated change ##########      
dC <- (f1+2*f2+2*f3+f4)/6         # <<<<<<<<< OUTPUT #### 
EV <- EVm                         # <<<<<<<<< OUTPUT #### 
Drainage <- Drainm  # is this the reason for the balance error? => seams not
# EV <-  interc - Drainage - dC/dtc # <<<<<<<<< OUTPUT #### 
# Drainage <-  interc - EV - dC/dtc # <<<<<<<<< OUTPUT #### 

        
# Test on Balance Error due to numeric?
# cat("sum(dC): ",  sum(dC), ", sum(EV)*dtc: ", sum(EV)*dtc, ", sum(Drainage)*dtc: ", sum(Drainage)*dtc 
#    , "Fehler:",  sum(dC - dtc * (interc - EV - Drainage )), "\n")
# BERR <- BERR + sum(dC - dtc * (interc - EV - Drainage )) # => it is for one day about  3.7e-16


# if (sum(dC) > 0) stop()

# Test on negative storages    # das sollte eigentlich nicht eintreten
ineg <- which(C_3D < -dC)      # identical with (C_3D+dC) < 0, but maybe faster 
if (length(ineg) > 0)  {
  Sneg <- (C_3D[ineg] + dC[ineg])/dtc             
  iDred <- which((Drainage[ineg] + Sneg) > 0)     
  if (length(iDred) > 0) {
    Drainage[ineg[iDred]] <- Drainage[ineg[iDred]] + Sneg[iDred]
    EV[ineg[-iDred]] <- EV[ineg[-iDred]] + Drainage[ineg[-iDred]] + Sneg[-iDred]
    Drainage[ineg[-iDred]] <- 0
  } else {
    EV[ineg] <- EV[ineg] + Drainage[ineg] + Sneg     # No! - for all indexes, reduce evaporation with that part which cannot equalised by Drainage
    Drainage[ineg] <- 0
  }
  dC <- dtc * (interc - EV - Drainage )
}
ineg <- which(C_3D < -dC)    # dieser Reste sind nun wirklich Rundungsfehler
dC[ineg] <- -C_3D[ineg]





# if (silent < 0){
#   cat("Pinterc=", sum(Pintercept*dtc), "\n")
#   cat("Dinterc=", sum(Dintercept*dtc), "\n")
#   cat("sum(dC) =", sum(dC) , "\n")
#   cat("sum(C_3D) =", sum(C_3D),", max(C_3D) =", max(C_3D), "\n")
#   # Und welchen Wert haben nun die EV und Drainage?
#   cat("sum(I-dC = E+D)", sum(Pintercept*dtc)-sum(dC), "\n")
#   cat("sum(E+D)", sum((EV+Drainage)*dtc), "\n")
#   cat("sum(EV)=", sum(EV*dtc), "\n")
#   cat("sum(Drainage)=", sum(Drainage*dtc), "\n")
# }
# if (iti < ni) {iti <- iti+1}  else break()

