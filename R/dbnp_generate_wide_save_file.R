#' Generate special format data file for upload to dbNp Phenotyp Database
#' @export
dbnp_generate_wide_save_file <- function(x,id_var,treat_var,visit_var,time_var,visit_units="weeks",time_units="minutes",exclude_vars=NULL,file="",sep=",",dec=".",rename_id=TRUE) {

  visits <- sort(unique(x[,visit_var]))
  treatments <- sort(unique(x[,treat_var]))
  times <- sort(unique(x[,time_var]))
  ids <- sort(unique(x[,id_var]))
  
  # x <- x[order(x[,id_var],x[,treat_var],x[,visit_var],x[,time_var]),]
  
  ## make sure design variables are always excluded
  design_vars <- c(id_var,treat_var,visit_var,time_var)
  if(!is.character(exclude_vars)) {
    exclude_vars <- design_vars
  } else {
    exclude_vars <- unique(c(exclude_vars,design_vars))
  }
  print(design_vars)
  print(exclude_vars)
  ## get names of measurement variables
  measure_varnames <- names(x)[which(!(names(x) %in% exclude_vars))]
  print(measure_varnames)
  
  ## compute time names from visits and times
  visits_times <- sort(rep(visits,length(times)))
  time_names <- sapply(time_converter(visits_times,visit_units) + time_converter(times,time_units),time_create)

  ## combine visits and times into one set of times
  x[,"__INTERNAL__time_names"] <- sapply(time_converter(x[,visit_var],visit_units) + time_converter(x[,time_var],time_units),time_create)

  ## wide format number of columns: (visits_times) * treatments * measurements + subject_id
  ncol <- length(time_names) * length(measure_varnames) * length(treatments) + 1
  
  df <- as.data.frame(matrix(NA, nrow=1,ncol=ncol))
  
  ## debug: print treatments
  for(tr in 1:length(treatments)) {
    cat("  Treatment: ",tr," ",treatments[tr],"\n")
  }
  
  ## empty data.frame to collect restructured data
  out_df <- as.data.frame(matrix(nrow=0,ncol=ncol))
  for(id in ids) {
    
    ## temporary data.frame (colect data from a single subject)
    tmp_df <- df
    
    ## position in temp df
    pos <- 1
    tmp_df[,pos] <- id
    pos <- pos + 1

    ## treatments
    for(tr in 1:length(treatments)) {
      
      ## extract measurements for single id,treatment
      tmp_dat <- x[which(x[,id_var] == id & x[,treat_var] == treatments[tr]),]
      
      ## measurements
      for(m in 1:length(measure_varnames)) {
        for(vt in 1:length(time_names)) {
          if(length(idx <- which(tmp_dat[,"__INTERNAL__time_names"] == time_names[vt]))) {
            tmp_df[,pos] <- tmp_dat[idx,measure_varnames[m]]
          } else {
            tmp_df[,pos] <- NA
          }
          pos <- pos + 1
        }
      }
    }
    out_df <- rbind(out_df,tmp_df)
  }
  
  ## create headers
  varnames_hdr <- character()
  times_hdr <- character()
  treat_hdr <- character()
  for(tr in 1:length(treatments)) {
    for(m in 1:length(measure_varnames)) {
      varnames_hdr <- c(varnames_hdr,rep(measure_varnames[m],length(time_names)))
      times_hdr <- c(times_hdr,time_names)
    }
    ## treatment header (for debugging)
    treat_hdr <- c(treat_hdr,rep(treatments[tr],length(time_names)*length(measure_varnames)))
  }
  if(rename_id) {
    out <- list(vheader=c("Subject name",varnames_hdr),theader=c("",times_hdr),trheader=c("",treat_hdr),data=out_df)
  } else {
    out <- list(vheader=c(id_var,varnames_hdr),theader=c("",times_hdr),trheader=c("",treat_hdr),data=out_df)
  }
  
  if(nchar(file)>0) {
    write.table(rbind(out$vheader),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,dec=dec,quote = TRUE)
    write.table(rbind(out$theader),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,append = TRUE,dec=dec)
    write.table(rbind(out$data),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,append = TRUE,dec=dec)
    c("File saved as '",file,",\n")
  } else {
    warning("No filename")
  }
  invisible(out)
}

