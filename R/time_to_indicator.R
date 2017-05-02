#' Internal function, Creates dbnp-import ready time indicater code from time
#' \code(time)
time_to_indicator <- function(x1) {
  xs <- time_converter(x1,"minutes")
  time_create(xs)
}