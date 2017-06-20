#' Generate file specifically formatted for import into dbNp Phenotype Database
#' @export
dbnp_generate_wide_save_file <- function(
      x
      ,id_var
      ,treat_var
      ,visit_var
      ,time_var
      ,visit_units="weeks"
      ,time_units="minutes"
      ,include_vars=NULL
      ,exclude_vars=NULL
      ,file=""
      ,sep=","
      ,dec="."
      ,rename_id=TRUE
  ) {
  
  if(any(is.na(x[,id_var]))) {
    stop("Some IDs are missing or invalid. Please check your data")
  }
  
  if(any(is.na(x[,visit_var]))) {
    stop("Some visits are missing or invalid. Please check your data")
  }
  
  if(any(is.na(x[,treat_var]))) {
    stop("Some treatment values are missing or invalid. Please check your data")
  }
  
  if(any(is.na(x[,time_var]))) {
    stop("Some time values are missing or invalid. Please check your data")
  }
  
  ## validate uniqueness
  check_design <- paste0(x[,id_var],x[,visit_var],x[,time_var],x[,treat_var])
  if(any(table(check_design)>1)) {
    stop("Design mismatch between ID, visit, time and treatment. Please check your data")
  }
  
  visits <- sort(unique(x[,visit_var]))
  treatments <- sort(unique(x[,treat_var]))
  times <- sort(unique(x[,time_var]))
  ids <- sort(unique(x[,id_var]))
  
  ## make sure design variables are always excluded
  design_vars <- c(id_var,treat_var,visit_var,time_var)
  if(!is.character(exclude_vars)) {
    exclude_vars <- design_vars
  } else {
    exclude_vars <- unique(c(exclude_vars,design_vars))
  }

  ## get names of measurement variables
  measure_varnames <- names(x)[which(!(names(x) %in% exclude_vars))]
  
  ## filter out variables not in include_vars
  if(is.character(include_vars)) {
    measure_varnames <- measure_varnames[which(measure_varnames %in% include_vars)]
  }
  
  ## compute time names from visits and times
  visits_times <- sort(rep(visits,length(times)))
  time_names <- sapply(time_converter(visits_times,visit_units) + time_converter(times,time_units),time_create)

  ## combine visits and times into one set of times
  x[,"__INTERNAL__time_names"] <- sapply(time_converter(x[,visit_var],visit_units) + time_converter(x[,time_var],time_units),time_create)

  if(any(is.na(x[,"__INTERNAL__time_names"]))) {
    stop("Some time points are missing or invalid. Please check your data")
  }
  

  ## wide format number of columns: visits * times * treatments * measurements + subject_id
  ncol <- length(time_names) * length(measure_varnames) * length(treatments) + 1
  ## number of rows (1 row per subject)
  nrow <- length(ids)

  ## debug: print treatments
  for(tr in 1:length(treatments)) {
    cat("  Treatment: ",tr," ",treatments[tr],"\n")
  }

  ## initialize idx-vars
  ## time indicator
  vt <- 1
  ## treatment indicator
  m <- 1
  ## row indicator
  row <- 1
  
  ## empty data.frame to collect restructured data
  out_df <- as.data.frame(matrix(NA,nrow=nrow,ncol=ncol))
  
  ##------------------------------------------------------------------
  ## loop starts
  ##------------------------------------------------------------------
  for(id in ids) {
    
    ## position in temp df
    pos <- 1
    out_df[row,pos] <- id
    pos <- pos + 1

    ## treatments
    for(tr in 1:length(treatments)) {
      
      ## extract measurements for single id,treatment
      tmp_dat <- x[which(x[,id_var] == id & x[,treat_var] == treatments[tr]),]
      
      ## print out during execution
      cat("\rid: ",id,', treat: ',tr,sep="")
      
      ## measurements
      for(m in 1:length(measure_varnames)) {
        for(vt in 1:length(time_names)) {
          if(length(idx <- which(tmp_dat[,"__INTERNAL__time_names"] == time_names[vt]))) {
            out_df[row,pos] <- tmp_dat[idx,measure_varnames[m]]
          } 
          pos <- pos + 1
        }
      }
    }
    row <- row + 1
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
    write.table(rbind(out$vheader),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,dec=dec,quote = FALSE)
    write.table(rbind(out$theader),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,append = FALSE,dec=dec)
    write.table(rbind(out$data),file=file,sep=sep,col.names = FALSE,na="",row.names = FALSE,append = FALSE,dec=dec)
    c("File saved as '",file,",\n")
  } else {
    warning("No filename")
  }
  invisible(out)
}

