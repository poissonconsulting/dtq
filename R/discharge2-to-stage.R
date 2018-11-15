#' Convert Discharge from Two Inputs to Stage
#' 
#' @inheritParams check_dtq
#' @inheritParams dtq_discharge_to_stage
#' @param n2 A count of the number of time units until the stage responds to the second discharge value.
#' @return The updated dtq data frame with a stage column.
#' @export
dtq_discharge2_to_stage <- function(
  x, r1d, dtt = "DateTime", colname = c("Discharge", "Discharge2"), 
  stage = "Stage", n = 0L, n2 = 0L, rate_down = Inf,
  rate_up = rate_down, units = dtt_units(x[[dtt]])) {
  
  check_vector(colname, "", unique = TRUE, length = 2L)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  check_string(stage)
  check_data(r1d, c(colname, stage), key = colname)
  
  n <- check_count(n, coerce = TRUE)
  n2 <- check_count(n2, coerce = TRUE)
  
  if(!is.infinite(rate_down)) .NotYetUsed("rate_down")
  if(!is.infinite(rate_up)) .NotYetUsed("rate_up")

  rate_down <- check_pos_dbl(rate_down, coerce = TRUE)
  rate_up <- check_pos_dbl(rate_up, coerce = TRUE)
  
  x[[stage]] <- rep(NA_real_, nrow(x))
  
  if(!nrow(x) || !nrow(r1d)) return(x)
  
  check_vector(r1d[[colname[1]]], c(0, chk_max_dbl()))
  check_vector(r1d[[colname[2]]], c(0, chk_max_dbl()))
  check_unique(r1d[colname])
  check_vector(r1d[[stage]], 1)
  
  if(!requireNamespace("interp", quietly = TRUE)) 
    err("package interp is required")

  xo <- dts_lag(x, dtt = dtt, colname = colname[1], n = n, units = units)[[colname[1]]]
  yo <- dts_lag(x, dtt = dtt, colname = colname[2], n = n2, units = units)[[colname[2]]]
  
  x[[stage]] <- interp::interp(
    r1d[[colname[1]]], y = r1d[[colname[2]]], z = r1d[[stage]], 
    xo = xo, yo = yo, output = "points")$z
  
  x
}
