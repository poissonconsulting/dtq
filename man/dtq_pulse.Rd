% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pulse.R
\name{dtq_pulse}
\alias{dtq_pulse}
\title{Pulse}
\usage{
dtq_pulse(
  x,
  dtt = "DateTime",
  colname = "Discharge",
  start = x[[dtt]][1],
  duration = 1L,
  target = 0,
  rate_down = Inf,
  rate_up = rate_down,
  units = dtt_units(x[[dtt]])
)
}
\arguments{
\item{x}{A dts data frame}

\item{dtt}{A string specifying the column with the Date or POSIXct values.}

\item{colname}{A character vector specifying the column(s) with the discharge and/or stage data.}

\item{start}{A dtt object of the start time.}

\item{duration}{A positive integer of the duration.}

\item{target}{The target discharge.}

\item{rate_down}{A positive number of the maximum absolute rate when dropping the discharge.}

\item{rate_up}{A positive number of the maximum rate when increasing the discharge.}

\item{units}{A string of the units.}
}
\description{
Adds a pulse to a discharge or stage time series.
}
