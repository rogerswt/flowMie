% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterer.R
\name{create_particle}
\alias{create_particle}
\title{Create a Model of a (Multi-layer) Scattering Particle}
\usage{
create_particle(medium = 1.34, lambda = 488, n_angle = 361, n_layers, r, n)
}
\arguments{
\item{medium}{The refractive index (RI) of the material surrounding the particle (Default = 1.34,
approximating the RI of normal saline solution)}

\item{lambda}{The wavelength of the incident light, in nm.  Default = 488.}

\item{n_angle}{The number of angular steps to calculate Stokes coefficients, in degrees.  (Default = 361).
Note: more steps increases resolution at the expense of run time.  The default is
adequate for most situations.}

\item{n_layers}{The number of layers in a particle.  A typical model of an EV
will have two layers.  The inner layer is the lumen, and the outer layer is the
membrane.}

\item{r}{A numeric vector of radii of each layer, in nm.  Note that length(r) must equal
n_layers.}

\item{n}{A numeric vector of refractive indices of each layer.  Note that length(n) must equal
n_layers.}
}
\value{
An object of class \code{ \link[Rscattnlay]{Scatterer}}.
}
\description{
Create_particle creates a model of a spherical particle with \code{n_layers} layers, each
of which has a radius and a refractive index.  Typically, layers are described from inside
towards the outside (i.e. lumen first, then membrane).
}
