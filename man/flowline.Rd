% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowline.R
\name{flowline}
\alias{flowline}
\title{Flowline}
\usage{
flowline(flowline, dem)
}
\arguments{
\item{flowline}{sf object;}

\item{dem}{terra SpatRast object;}
}
\value{
a valid flowline sf object
}
\description{
Takes a newly drawn flowline and uses the dem to ensure
the flowline is digitized in the upstream direction.
}
