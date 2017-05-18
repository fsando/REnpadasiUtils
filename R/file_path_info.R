file_path_info <- function(x) {
  bnm <- basename(x)
  dnm <- dirname(x)
  pos <- max(gregexpr("\\.",bnm)[[1]])
  if(pos==-1) {
    return(list(name=bnm,ext="",dir=dnm))
  } else {
    ext <- substring(bnm,pos+1)
    fnm <- substr(bnm,1,pos-1)
    return(list(name=fnm,ext=ext,dir=dnm))
  }
}
