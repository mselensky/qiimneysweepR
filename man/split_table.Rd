% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_sample.R
\name{split_table}
\alias{split_table}
\title{Split ASV table}
\usage{
split_table(asv_table, metadata, metadata_column = ".", variable = ".")
}
\arguments{
\item{asv_table}{An OTU or ASV table.}

\item{metadata}{Metadata file.}

\item{metadata_column}{Column from metadata from which to filter.}

\item{variable}{Variable from metadata_column for which to filter.}
}
\description{
This function filters an OTU or ASV table based on a defined condition and returns a smaller table.
}
\examples{
split_table()
}
\keyword{filter}
\keyword{split}
