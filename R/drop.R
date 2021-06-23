
dtq_add_drop <- function(x,  dtt = "DateTime", colname = "Discharge",
                         start = x[[dtt]][1], duration = 1L,
                         target = 0, rate_down = Inf, rate_up = rate_down,
                         units = dtt_units(x[[dtt]])) {
  chk_string(colname)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  if(!nrow(x)) return(x)
  dtt_units(start)
  chk_sorted(start)
  chk_unique(start)
  dtt_tz(start, x[[dtt]])
  
  if(!all(start %in% x[[dtt]]))
    err("start must be in ", dtt, " column")
  
  length <- c(1L, 1L, length(start))
  
  chk_vector(target)
  check_dim(target)
  chk_gte(target)
  
  chk_vector(rate_down)
  check_dim(rate_down)
  chk_gte(rate_down)
  
  chk_vector(rate_up)
  check_dim(rate_up, values = c(.Machine$double.xmin, .Machine$double.xmax))
  
}
