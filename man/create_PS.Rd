% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterer.R
\name{create_PS}
\alias{create_PS}
\title{Create a Model of Polystyrene Bead}
\usage{
create_PS(medium = 1.34, lambda = 488, n = 1.605, d)
}
\arguments{
\item{medium}{The refractive index (RI) of the material surrounding the particle (Default = 1.34,
approximating the RI of normal saline solution)}

\item{lambda}{The wavelength of the incident light, in nm.  Default = 488.}

\item{n}{The refractive index of Polystyrene.  Default = 1.605.}

\item{d}{The  diameter of the polystyrene bead, in nm.}
}
\value{
An object of class \code{ \link[Rscattnlay]{Scatterer}}.
}
\description{
Create_PS creates a model of a polystyrene bead.
This is really just a convenience wrapper for \link{create_particle} with default
refractive index corresponding to polystyrene.  The user only needs to
specify the size of the bead.
**Please note that this function uses the diameter, NOT the radius of the particle!**
}
