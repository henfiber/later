% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{split_data_on_date}
\alias{split_data_on_date}
\title{Split and save dataset by date}
\usage{
split_data_on_date(DT, data_path = "data/cache", date_field,
  date_unit = c("hour", "day", "Month", "year", "week", "minute"))
}
\arguments{
\item{DT}{The data frame to split}

\item{data_path}{The data path to save the parts of the dataset}

\item{date_field}{The name of the date field to use for splitting}

\item{date_unit}{The date unit to use for rounding to complete intervals}
}
\value{
invisible(NULL)
}
\description{
Split dataset DT on subfolders defined by its date (Y/m/d/H format)
}
