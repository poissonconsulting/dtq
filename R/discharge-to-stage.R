#' Convert Discharge to Stage
#' 
#' @inheritParams check_dtq
#' @param r1d A data frame of the discharge and stage values for a transect 
#' from a 1-dimensional model.
#' @param stage A string of the name of the column with the stage values.
#' @param delay A count of the number of time units until the stage responds.
#' @return The updated dtq data frame with a stage column.
#' @export
dtq_discharge_to_stage <- function(x, r1d, dtt = "DateTime", colname = "Discharge", 
                      stage = "Stage", delay = 0L, rate_down = Inf,
                      rate_up = rate_down, units = dtt_units(x[[dtt]])) {
  check_string(colname)
  check_dtq(x, dtt = dtt, colname = colname, 
            complete = TRUE, sorted = TRUE, unique = TRUE, units = units)
  check_string(stage)
  check_data(r1d, c(colname, stage), key = colname)
  
  delay <- check_count(delay, coerce = TRUE)
  
  if(!is.infinite(rate_down)) .NotYetUsed("rate_down")
  if(!is.infinite(rate_up)) .NotYetUsed("rate_up")

  rate_down <- check_pos_dbl(rate_down, coerce = TRUE)
  rate_up <- check_pos_dbl(rate_up, coerce = TRUE)
  
  x[[stage]] <- rep(NA_real_, nrow(x))
  
  if(!nrow(x) || !nrow(r1d)) return(x)

  check_vector(r1d[[colname]], c(0, chk_max_dbl()))
  check_vector(r1d[[stage]])
  
  xo <- dts_delay(x, dtt = dtt, colname = colname, delay = delay)[[colname]]

  x[[stage]] <- stats::approx(r1d[[colname]], r1d[[stage]], xo)$y
  
  x
}
