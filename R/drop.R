
dtq_add_drop <- function(x,  dtt = "DateTime", colname = "Discharge",
                         start = x[[dtt]][1], duration = 1L,
                         target = 0, rate_down = Inf, rate_up = rate_down,
                         units = dtt_units(x[[dtt]])) {
  chk_string(colname)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  if(!nrow(x)) return(x)
  check_dtt(start, length = TRUE, unique = TRUE, sorted = TRUE, 
            tz = dtt_tz(x[[dtt]]))
  if(!all(start %in% x[[dtt]])) 
    err("start must be in ", dtt, " column")
  
  length <- c(1L, 1L, length(start))
  check_vector(target, c(0, chk_max_dbl()), length = length)
  check_vector(rate_down, c(0, chk_max_dbl()), length = length)
  check_vector(rate_up, c(chk_tiny_dbl(), chk_max_dbl()), length = length)
  

  
}
