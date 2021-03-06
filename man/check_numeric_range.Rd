% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quadrat_qc_functions.R
\name{check_numeric_range}
\alias{check_numeric_range}
\title{Check for invalid numeric range}
\usage{
check_numeric_range(df, column_name, min_val = -Inf, max_val = Inf,
  allow_min = T, allow_max = T)
}
\arguments{
\item{df}{Data frame of quadrat data}

\item{column_name}{Name of column in df to be checked}

\item{min_val}{Minimum allowed value; default = -Inf}

\item{max_val}{Maximum allowed value; default = Inf}

\item{allow_min}{T/F: T = indicated minimum value is an allowed value; F = minimum value is not an allowed value. default = T}

\item{allow_max}{T/F: T = indicated maximum value is an allowed value; F = maximum value is not an allowed value. default = T}
}
\value{
Rows from df containing values outside valid range, plus additional columns indicating error_type and error_value.
        If there are no errors, this will return an empty data frame.
}
\description{
Check a data frame of quadrat data for invalid numeric data. Specify the allowed range by min and max values,
  and whether the allowed range should include the min and max values.
}
\examples{
check_numeric_range(df, column_name = 'area', min_val = 0, max_val = 1, allow_min=F, allow_max=T)

}
