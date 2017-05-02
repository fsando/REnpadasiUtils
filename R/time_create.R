#' function to create time
time_create <- function(x) {
  ## extract weeks
  a <- ""
  if(x < 0) {
    a <- "-"
    x <- abs(x)
  }
  w <- x %/% 604800
  x <- x %%  604800
  d <- x %/% 86400
  x <- x %%  86400
  h <- x %/% 3600
  x <- x %%  3600
  m <- x %/% 60
  s <- x %%  60

  p <- 0
  ws <- ds <- hs <- ms <- ss <- ""
  if(w>0) {
    ws <- paste0(w,"w")
    p <- 1
  }
  if(d>0){
    ds <- paste0(d,"d")
    p <- 1
  }
  if(h>0) {
    hs <- paste0(h,"h")
    p <- 1
  }
  if(m>0) {
    ms <- paste0(m,"m")
    p <- 1
  }
  if((w>0 && d==0 && h==0 && m==0) || p==0) {
    ss <- paste0(s,"s")
  }
  outstr <- paste0(a,ws,ds,hs,ms,ss)
  outstr
}
