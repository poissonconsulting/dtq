#' Check DateTime Discharge Data Frame
#' 
#' A dts data frame is a data frame with a column of Date or POSIXct values
#' and at least one double column of Discharge or Stage data.
#'
#' @param x A dts data frame
#' @param dtt A string specifying the column with the Date or POSIXct values.
#' @param colname A character vector specifying the column(s) with the discharge and/or stage data.
#' @inheritParams dts::check_dts
#' @param rate_down A positive number of the maximum absolute rate when dropping the discharge.
#' @param rate_up A positive number of the maximum rate when increasing the discharge.
#' @return An invisible copy of x (if it doesn't throw an error).
#' @export
check_dtq <- function(x, dtt = "DateTime", colname = "Discharge", 
                      nrow = NA, nas = TRUE, floored = TRUE, 
                      sorted = FALSE, unique = FALSE, 
                      complete = FALSE, rate_down = Inf, rate_up = rate_down,
                      units = dttr::dtt_units(x[[dtt]]),
                      tz = dttr::dtt_tz(x[[dtt]]),
                      exclusive = FALSE, order = FALSE,
                      x_name = substitute(x), error = TRUE) {
  x_name <- chk_deparse(x_name)
  check_vector(colname, "", length = TRUE, unique = TRUE)
  
  check_dts(x, dtt = dtt, colname = colname, nrow = nrow,
            nas = nas, floored = floored, sorted = sorted,
            unique = unique, complete = complete, units = units,
            tz = tz, exclusive = exclusive, order = order,
            x_name = x_name, error = TRUE)
  
  check_pos_dbl(rate_down)
  check_pos_dbl(rate_up)

  if(!nrow(x)) return(invisible(x))
  
  for(col in colname) {
    check_vector(x[[colname]], c(0, chk_max_dbl(), NA),
                 x_name = paste0("column '", col, "' of ", x_name))
    check_vector(diff(x[[colname]]), c(rate_down * -1, rate_up, NA),
                 x_name = paste0("the differenced column '", col, "' of ", x_name),
                 error = error)
  }
  invisible(x)
}
