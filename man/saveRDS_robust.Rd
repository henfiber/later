% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{saveRDS_robust}
\alias{saveRDS_robust}
\title{Robust saveRDS}
\usage{
saveRDS_robust(dt, fpath, backup_on_overwrite = TRUE, allowZero = FALSE,
  ...)
}
\arguments{
\item{dt}{The object to save}

\item{fpath}{The path to save the object}

\item{backup_on_overwrite}{Whether to keep a backup when overwriting an existing file}

\item{allowZero}{Whether to save zero length objects}

\item{...}{The dots are passed to saveRDS}
}
\value{
TRUE if successful, a negative value if it fails
}
\description{
A saveRDS with atomic write and backup support
}
