# added as hack for tests

dtt_disaggregate <- function(x, ...) {
  UseMethod("dtt_disaggregate")
}

dtt_disaggregate.Date <- function(x, ...) {
  units <- dtt_units(x)
  if(identical(units, "days"))
    err("Date vectors cannot be disaggregated below 'days'")
  end <- x[length(x)]
  
  if(identical(units, "months")) {
    end <- dtt_add_months(end)
    end <- dtt_subtract_days(end)
  } else {
    end <- dtt_add_years(end)
    end <- dtt_subtract_months(end)
  }
  x <- c(x, end)
  x <- dtt_floor(x)
  x <- dtt_complete(x)
  x
}

dtt_disaggregate.POSIXct <- function(x, ...) {
  units <- dtt_units(x)
  if(identical(units, "seconds"))
    err("POSIXct vectors cannot be disaggregated below 'seconds'")
  end <- x[length(x)]
  
  if(identical(units, "minutes")) {
    end <- dtt_add_minutes(end)
    end <- dtt_subtract_seconds(end)
  } else if(identical(units, "hours")) {
    end <- dtt_add_hours(end)
    end <- dtt_subtract_minutes(end)
  } else if(identical(units, "days")) {
    end <- dtt_add_days(end)
    end <- dtt_subtract_hours(end)
  } else if(identical(units, "months")) {
    end <- dtt_add_months(end)
    end <- dtt_subtract_days(end)
  } else {
    end <- dtt_add_years(end)
    end <- dtt_subtract_months(end)
  }
  x <- c(x, end)
  x <- dtt_complete(x)
  x
}