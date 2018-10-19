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
  
  check_count(delay)
  check_pos_dbl(rate_down)
  check_pos_dbl(rate_up)
  
  x[[stage]] <- rep(NA_real_, nrow(x))
  
  if(!nrow(x) || !nrow(r1d)) return(double(0))

  check_vector(r1d[[colname]], c(0, chk_max_dbl()), sorted = TRUE)
  check_vector(r1d[[stage]], 1, sorted = TRUE)
  
  x[[stage]] <- stats::approx(r1d[[colname]], r1d[[stage]], x[[colname]])$y
  if(delay > 0)
    x[[stage]] <- c(rep(NA_real_, delay), x[[stage]][-(1:delay)])
  if(!is.infinite(rate_down)) .NotYetUsed(rate_down)
  if(!is.infinite(rate_up)) .NotYetUsed(rate_up)
    
  x
}
