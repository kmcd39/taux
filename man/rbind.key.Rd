% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling-fcns.R
\name{rbind.key}
\alias{rbind.key}
\title{rbind.key}
\usage{
\method{rbind}{key}(x, ids_to = "id")
}
\arguments{
\item{x}{named list of data.frames to rbind}

\item{ids_to}{name of id column in new data.frame}
}
\description{
Row binds a named list of tables, using element names to build a new identifier
column.
}
