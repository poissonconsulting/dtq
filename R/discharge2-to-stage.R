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
  
  chk_vector(colname)
  check_values(colname, "")
  chk_unique(colname)
  check_dim(colname, values = 2L)
  
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  
  chk_string(stage)
  check_data(r1d, key = colname)
  check_names(r1d, names = c(colname, stage))
  chk_whole_number(n)
  chk_gte(n)
  n <- as.integer(n)
  
  chk_whole_number(n2)
  chk_gte(n2)
  n2 <- as.integer(n2)

  if(!is.infinite(rate_down)) .NotYetUsed("rate_down")
  if(!is.infinite(rate_up)) .NotYetUsed("rate_up")

  chk_dbl(rate_down)
  chk_gt(rate_down)
  rate_down <- as.double(rate_down)
  
  chk_dbl(rate_up)
  chk_gt(rate_up)
  rate_down <- as.double(rate_up)
  
  x[[stage]] <- rep(NA_real_, nrow(x))
  
  if(!nrow(x) || !nrow(r1d)) return(x)
  
  chk_vector(r1d[[colname[1]]])
  chk_gte(r1d[[colname[1]]])

  chk_vector(r1d[[colname[2]]])
  chk_gte(r1d[[colname[2]]])
  
  chk_unique(r1d[colname])
  chk_vector(r1d[[stage]], 1)
  
  if(!requireNamespace("interp", quietly = TRUE)) 
    err("package interp is required")

  xo <- dts_lag(x, dtt = dtt, colname = colname[1], n = n, units = units)[[colname[1]]]
  yo <- dts_lag(x, dtt = dtt, colname = colname[2], n = n2, units = units)[[colname[2]]]
  
  x[[stage]] <- interp::interp(
    r1d[[colname[1]]], y = r1d[[colname[2]]], z = r1d[[stage]], 
    xo = xo, yo = yo, output = "points")$z
  
  x
}
