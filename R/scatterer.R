#
# scatterer.R
#
#
################################################################################
################################################################################
#                     Copyright Still Pond Cytomics LLC 2019.                 ##
#        All Rights Reserved. No part of this source code may be reproduced   ##
#            without Still Pond Cytomics' express written consent.            ##
################################################################################
################################################################################
#

#' @title Create a Model of a (Multi-layer) Scattering Particle
#' @param medium The refractive index (RI) of the material surrounding the particle (Default = 1.34,
#' approximating the RI of normal saline solution)
#' @param lambda The wavelength of the incident light, in nm.  Default = 488.
#' @param n_angle The number of angular steps to calculate scattering, in degrees.  (Default = 361).
#' Note: more steps increases resolution at the expense of run time.  The default is
#' adequate for most situations.
#' @param n_layers The number of layers in a particle.  A typical model of an EV
#' will have two layers.  The inner layer is the lumen, and the outer layer is the
#' membrane.
#' @param r A numeric vector of radii of each layer, in nm.  Note that length(r) must equal
#' n_layers.
#' @param n A numeric vector of refractive indices of each layer.  Note that length(n) must equal
#' n_layers.
#' @description Create_particle creates a model of a spherical particle with \code{n_layers} layers, each
#' of which has a radius and a refractive index.  Typically, layers are described from inside
#' towards the outside (i.e. lumen first, then membrane).
#' @return An object of class \code{ \link[Rscattnlay]{Scatterer}}.
#' @export
create_particle = function(medium = 1.34, lambda = 488, n_angle = 361, n_layers, r, n) {

  # sanity checks
  if (length(r) != n_layers) {
    stop("Radius vector doesn't match n_layers")
  }
  if (length(n) != n_layers) {
    stop("Index vector doesn't match n_layers")
  }

  Env <- Scatterer()   # create a scatterer object
  na(Env) <- medium     # set the ambient refractive index eg PBS

  lambda(Env) <- lambda   # wavelength

  nt(Env) <- n_angle       # angles to compute
  tf(Env) <- 360

  # make the particle
  S <- Env
  layer = list()
  for (i in 1:n_layers) {
    layer[[i]] <- Layer()
    r(layer[[i]]) <- r[i]
    m(layer[[i]]) <- as.complex(n[i])
    S <- S + layer[[i]]
  }

  S
}

#' @title Create a Model of an Extracellular Vesicle
#' @param medium The refractive index (RI) of the material surrounding the particle (Default = 1.34,
#' approximating the RI of normal saline solution)
#' @param lambda The wavelength of the incident light, in nm.  Default = 488.
#' @param n_core The refractive index of the EV lumen.  Default = 1.38.
#' @param n_membrane The refractive index of the EV membrane.  Default = 1.46.
#' @param thickness_membrane The thickness of the EV membrane, in nm.  Default = 5.0.
#' @param d The overall diameter (lumen + membrane) of the EV, in nm.
#' @description Create_EV creates a model of a 2-layer particle, suitable to describe an EV.
#' This is really just a convenience wrapper for \link{create_particle}, allowing the description of
#' a particle with total diameter `d`, surrounded by a membrane specified by its thickness.
#' The refractive indices of the core and membrane can be specified separately. **Please note
#' that this function uses the diameter, NOT the radius of the particle!**
#' @return An object of class \code{ \link[Rscattnlay]{Scatterer}}.
#' @export
create_EV <- function(medium = 1.34, lambda = 488, n_core = 1.38, n_membrane = 1.46, thickness_membrane = 5.0, d) {

  r_core = d / 2 - thickness_membrane
  P = create_particle(medium = medium, lambda = lambda, n_layers = 2, r = c(r_core, d / 2), n = c(n_core, n_membrane))

  P
}

#' @title Create a Model of Polystyrene Bead
#' @param medium The refractive index (RI) of the material surrounding the particle (Default = 1.34,
#' approximating the RI of normal saline solution)
#' @param lambda The wavelength of the incident light, in nm.  Default = 488.
#' @param n The refractive index of Polystyrene.  Default = 1.605.
#' @param d The  diameter of the polystyrene bead, in nm.
#' @description Create_PS creates a model of a polystyrene bead.
#' This is really just a convenience wrapper for \link{create_particle} with default
#' refractive index corresponding to polystyrene.  The user only needs to
#' specify the size of the bead.   **Please note
#' that this function uses the diameter, NOT the radius of the particle!**
#' @return An object of class \code{ \link[Rscattnlay]{Scatterer}}.
#' @export
create_PS = function(medium = 1.34, lambda = 488, n = 1.605, d) {
  P = create_particle(medium = medium, lambda = lambda, n_layers = 1, r = d / 2, n = n)

  P
}

#' @title Create a Model of Silica Bead
#' @param medium The refractive index (RI) of the material surrounding the particle (Default = 1.34,
#' approximating the RI of normal saline solution)
#' @param lambda The wavelength of the incident light, in nm.  Default = 488.
#' @param n The refractive index of Silica.  Default = 1.463.
#' @param d The  diameter of the silica bead, in nm.
#' @description Create_SI creates a model of a silica bead.
#' This is really just a convenience wrapper for \link{create_particle} with default
#' refractive index corresponding to silica  The user only needs to
#' specify the size of the bead.   **Please note
#' that this function uses the diameter, NOT the radius of the particle!**
#' @return An object of class \code{ \link[Rscattnlay]{Scatterer}}.
#' @export
create_SI = function(medium = 1.34, lambda = 488, n = 1.463, d) {
  P = create_particle(medium = medium, lambda = lambda, n_layers = 1, r = d / 2, n = n)

  P
}
