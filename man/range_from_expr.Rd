% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{range_from_expr}
\alias{range_from_expr}
\title{Get range from shorthand expression}
\usage{
range_from_expr(period_expr = "720h", anchor = NULL)
}
\arguments{
\item{period_expr}{The shorthand period expression}

\item{anchor}{The reference time to use}
}
\value{
The range constructed from this expression
}
\description{
Get a range back from a period expression (e.g. "24h") and an anchor timestamp
}
