% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_get_z.R
\name{sf_get_z}
\alias{sf_get_z}
\title{SF Get Z}
\usage{
sf_get_z(points, dem)
}
\arguments{
\item{points}{sf object;}

\item{dem}{terra SpatRast object;}
}
\value{
point sf object with new field containing dem z
}
\description{
Adds a new attribute "Z" to the input points sf object based
on the input dem.
}
