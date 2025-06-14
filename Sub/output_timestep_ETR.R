#+
# NAME*:  output_timestep.R
# PURPOSE*: output fields and intersections 
# RELEVANCY*: CanWat
# CALLING SEQ.: source(file.path(path_sub,"output_timestep.R"))
# INPUTS*: called by CanWat::main, fields xcut.out, ycut.out
# OUTPUT*: files
# RESTRICTIONS:
# SUBROUTINES:
# REVISION HISTORY*:
#    2019-10-17 RQ :
#-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* Paths and subroutines #### gsub("\\\\", "/", (readClipboard()))


# Example for reloading the fields 
# test <- array(as.numeric(fread(file.path(path_output, "Fields/ET_",Tstamp,"_",as.numeric(sdate), ".txt" ))), c(nx,ny,nz))
# sum(ET-test)
# range(ET-test)  # rounding error !!!
# sum(ET)
# sum(test)


#* Paths and subroutines #### gsub("\\\\", "/", (readClipboard()))
if (field.out) {
  fwrite(as.list(EV), file.path(path_output, "Fields/EV_",Tstamp,"_",as.numeric(sdate), ".txt" ))
  fwrite(as.list(ET), file.path(path_output, "Fields/ET_",Tstamp,"_",as.numeric(sdate), ".txt" ))
}

if ((!is.na(xcut.out))  ) {
  fwrite(as.list(EV[,,xcut.out]), file.path(path_output, "Fields/EV_at_x", xcut.out , "_",Tstamp,"_",Tstamp,"_",as.numeric(sdate), ".txt" ))
  fwrite(as.list(ET[,,xcut.out]), file.path(path_output, "Fields/ET_at_x", xcut.out , "_",Tstamp,"_",Tstamp,"_",as.numeric(sdate), ".txt" ))
}

if ( (!is.na(ycut.out)) ) {
  fwrite(as.list(EV[ ,ycut.out,]), file.path(path_output, "Fields/EV_at_x", ycut.out , "_",Tstamp,"_",Tstamp,"_",as.numeric(sdate), ".txt" ))
  fwrite(as.list(ET[ ,ycut.out,]), file.path(path_output, "Fields/ET_at_x", ycut.out , "_",Tstamp,"_",Tstamp,"_",as.numeric(sdate), ".txt" ))
}
