pulse <- function(discharge, target, rate_down, rate_up) {
  n <- length(discharge)
  if(is.infinite(rate_down) && is.infinite(rate_up)) {
    discharge[2:(n-1L)] <- target
    return(discharge)
  }
  discharge[2:(n-1L)] <- NA_real_
  if(is.na(discharge[1]) || is.na(discharge[n])) return(discharge)
  
  down <- cumsum(rep(rate_down, n - 1L)) * -1
  up <- cumsum(rep(rate_up, n - 1L))
  
  to <- if(discharge[1] > target) down else up
  from <- if(discharge[n] > target) up else down
  from <- rev(from) * -1
  
  to <- c(0, to)
  from <- c(from, 0)
  
  to <- to + discharge[1]
  from <- from + discharge[n]
  
  if(discharge[1] > target) {
    to[to < target] <- target
  } else
    to[to > target] <- target
  
  if(discharge[n] > target) {
    from[from < target] <- target
  } else
    from[from > target] <- target
  
  switch <- to > from
  if(switch[1]) switch <- !switch
  switch <- which(switch)
  if(!length(switch)) {
    switch <- which(to == from)
    switch <- switch[!switch %in% c(1L, n)]
  }
  if(!length(switch)) {
    wrn("pulse rates are incompatible with start and end discharges")
    return(discharge)
  }
  switch <- switch[1]
  c(to[1:(switch-1L)], from[switch:n])
}

#' Pulse
#' 
#' Adds a pulse to a discharge or stage time series.
#' 
#' @inheritParams check_dtq
#' @param start A dtt object of the start time.
#' @param duration A positive integer of the duration.
#' @param target The target discharge.
#'
#' @export
dtq_pulse <- function(x, dtt = "DateTime", colname = "Discharge",
                      start = x[[dtt]][1], duration = 1L,
                      target = 0, rate_down = Inf, rate_up = rate_down,
                      units = dtt_units(x[[dtt]])) {
  check_string(colname)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, 
            units = units)

  check_pos_int(duration)
  check_noneg_dbl(target)
  check_pos_dbl(rate_down)
  check_pos_dbl(rate_up)
  
  if(!nrow(x)) return(x)
  check_dtt(start, length = 1L, nas = FALSE, tz = dtt_tz(x[[dtt]]))
  
  if(dtt_is_date(x[[dtt]])) {
    start <- dtt_date(start)
  } else
    start <- dtt_date_time(start, tz = dtt_tz(x[[dtt]]))
  
  start <- which(x[[dtt]] %in% start)
  if(!length(start)) err("start must be in ", dtt, " column")
  
  end <- start + duration + 1L
  if(end > nrow(x)) err("end must be in ", dtt, " column")
  
  x[[colname]][start:end] <- pulse(x[[colname]][start:end], target = target, 
                                   rate_down = rate_down, rate_up = rate_up)
  x
}
