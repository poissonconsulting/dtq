#' Convert Discharge to Stage
#' 
#' @inheritParams check_dtq
#' @param r1d A data frame of the discharge and stage values for a transect 
#' from a 1-dimensional model.
#' @param stage A string of the name of the column with the stage values.
#' @param n A count of the number of time units until the stage responds.
#' @return The updated dtq data frame with a stage column.
#' @export
dtq_discharge_to_stage <- function(x, r1d, dtt = "DateTime", colname = "Discharge", 
                      stage = "Stage", n = 0L, rate_down = Inf,
                      rate_up = rate_down, units = dtt_units(x[[dtt]])) {
  chk_string(colname)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  chk_string(stage)
  check_data(r1d, key = colname)
  check_names(r1d, names = c(colname, stage))
  
  chk_whole_number(n)
  chk_gte(n)
  n <- as.integer(n)

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

  chk_vector(r1d[[colname]])
  chk_gte(r1d[[colname]])
  chk_vector(r1d[[stage]])
  
  xo <- dts_lag(x, dtt = dtt, colname = colname, n = n, units = units)[[colname]]

  x[[stage]] <- stats::approx(r1d[[colname]], r1d[[stage]], xo)$y
  
  x
}
