time_converter <- function(x,from="seconds",to="seconds") {
  x1=switch(from,
    seconds=x,
    minutes=x*60,
    hours  =x*3600,
    days   =x*86400,
    weeks  =x*604800
  )
  x2=switch(to,
    seconds=x1,
    minutes=x1/60,
    hours  =x1/3600,
    days   =x1/86400,
    weeks  =x1/604800
  )
  x2
}
