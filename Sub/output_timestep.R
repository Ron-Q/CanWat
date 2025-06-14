#+
# NAME*:  output_timestep.R
# PURPOSE*: output fields and intersections 
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"output_timestep.R"))
# INPUTS*: called by CanWat::main, fields xcut.out, ycut.out, zcut.out
# OUTPUT*: files
# RESTRICTIONS:
# SUBROUTINES:
# REVISION HISTORY*:
#    2019-10-17 RQ :
#    2022-01-12 RQ : dC1 => dC , 
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(T){
  # Example for reloading the fields 
  # test <- array(as.numeric(fread(file.path(path_output_act_user, paste0(Pintercept_",Tstamp,"txt" ))))), c(nx,ny,nz))
  # sum(Pintercept-test)
  # range(Pintercept-test)
  # sum(Pintercept)
  # sum(test)
  if (layer.out == T) {
    if (exists("C_lmean"))  {
      if (addNrows > 0) C_lmean <- rbind(C_lmean,array(NA, c(addNrows,nz)))
      for (i in 1:nz) C_lmean[itt,i] <- mean(C_3D[i,,])
    } 
    if (exists("ETR_lmean")) {
      if (addNrows > 0) ETR_lmean <- rbind(ETR_lmean,array(NA, c(addNrows,nz)))
      for (i in 1:nz) ETR_lmean[itt,i] <- mean(ETR[i,,])
    }
    if (exists("ET_lmean"))  {
      if (addNrows > 0) ET_lmean <- rbind(ET_lmean,array(NA, c(addNrows,nz)))
      for (i in 1:nz) ET_lmean[itt,i] <- mean(ET[i,,])
    }
    if (exists("EV_lmean"))  {
      if (addNrows > 0) EV_lmean <- rbind(EV_lmean,array(NA, c(addNrows,nz)))
      for (i in 1:nz) EV_lmean[itt,i] <- mean(EV[i,,])
    }
  }
  
  if (field.out) {
    fwrite(as.list(dC), file.path(path_output_act_user, paste0("dC_",Tstamp,"txt" )))
    fwrite(as.list(Pintercept), file.path(path_output_act_user, paste0("Pintercept_",Tstamp,"txt" )))
    fwrite(as.list(Pthroughf), file.path(path_output_act_user, paste0("Pthroughf_",Tstamp,"txt" )))
    fwrite(as.list(EV), file.path(path_output_act_user, paste0("EV_",Tstamp,"txt" )))
    fwrite(as.list(ET), file.path(path_output_act_user, paste0("ET_",Tstamp,"txt" )))
    fwrite(as.list(Drainage), file.path(path_output_act_user, paste0("Drainage_",Tstamp,"txt" )))
    fwrite(as.list(Dintercept), file.path(path_output_act_user, paste0("Dintercept_",Tstamp,"txt" )))
    fwrite(as.list(Dthroughf), file.path(path_output_act_user, paste0("Dthroughf_",Tstamp,"txt" )))
  }
  
  if (!is.na(xcut.out)) {
    fwrite(as.list(dC[,,xcut.out]), file.path(path_output_act_user, paste0("dC_at_x", xcut.out , "_" ,Tstamp,".txt" )))
    fwrite(as.list(Pintercept[,,xcut.out]), file.path(path_output_act_user, paste0("Pintercept_at_x, ", xcut.out , "_",Tstamp,".txt" )))
    fwrite(as.list(Pthroughf[,,xcut.out]), file.path(path_output_act_user, paste0("Pthroughf_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,".txt" )))
    fwrite(as.list(EV[,,xcut.out]), file.path(path_output_act_user, paste0("EV_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(ET[,,xcut.out]), file.path(path_output_act_user, paste0("ET_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Drainage[,,xcut.out]), file.path(path_output_act_user, paste0("Drainage_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dintercept[,,xcut.out]), file.path(path_output_act_user, paste0("Dintercept_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dthroughf[,,xcut.out]), file.path(path_output_act_user, paste0("Dthroughf_at_x, ", xcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
  }
  
  if ( !is.na(ycut.out) ) {
    fwrite(as.list(dC[ ,ycut.out,]), file.path(path_output_act_user, paste0("dC_at_y", ycut.out , "_",Tstamp,"txt" )))
    fwrite(as.list(Pintercept[ ,ycut.out,]), file.path(path_output_act_user, paste0("Pintercept_at_y", ycut.out , "_",Tstamp,"txt" )))
    fwrite(as.list(Pthroughf[ ,ycut.out,]), file.path(path_output_act_user, paste0("Pthroughf_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(EV[ ,ycut.out,]), file.path(path_output_act_user, paste0("EV_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(ET[ ,ycut.out,]), file.path(path_output_act_user, paste0("ET_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Drainage[ ,ycut.out,]), file.path(path_output_act_user, paste0("Drainage_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dintercept[ ,ycut.out,]), file.path(path_output_act_user, paste0("Dintercept_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dthroughf[ ,ycut.out,]), file.path(path_output_act_user, paste0("Dthroughf_at_y", ycut.out , "_",Tstamp,"_",Tstamp,"txt" )))
  }

  if ( !is.na(zcut.out)) {
    fwrite(as.list(dC[ zcut.out,,]), file.path(path_output_act_user, paste0("dC_at_z", zcut.out , "_",Tstamp,"txt" )))
    fwrite(as.list(Pintercept[ zcut.out,,]), file.path(path_output_act_user, paste0("Pintercept_at_z", zcut.out , "_",Tstamp,"txt" )))
    fwrite(as.list(Pthroughf[ zcut.out,,]), file.path(path_output_act_user, paste0("Pthroughf_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(EV[ zcut.out,,]), file.path(path_output_act_user, paste0("EV_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(ET[ zcut.out,,]), file.path(path_output_act_user, paste0("ET_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Drainage[ zcut.out,,]), file.path(path_output_act_user, paste0("Drainage_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dintercept[ zcut.out,,]), file.path(path_output_act_user, paste0("Dintercept_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
    fwrite(as.list(Dthroughf[ zcut.out,,]), file.path(path_output_act_user, paste0("Dthroughf_at_z", zcut.out , "_",Tstamp,"_",Tstamp,"txt" )))
  }
  
}
