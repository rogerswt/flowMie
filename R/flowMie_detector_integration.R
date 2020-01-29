#
# flowMie_detector_integration.R
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
# Instrument characterization.
# Default is the CytoVas Canto SSC detector
# beta = 60    # detector acceptance half-angle
# psi_0 = 90   # polarization perpendicular to the plane of incidence
# pol = 1      # fully polarized light
# theta_0 = 90 # SSC detector

#' @import Rscattnlay
#' @importFrom methods as
#' @importFrom stats approx

#' @title Create a Model of a Flow Cytometer Scatter Detector
#' @param theta_0 Angle between the incident light source and the detector,in degrees.
#' Default = 90 (side scatter detector).
#' @param beta The detector acceptance half-angle, in degrees.  Default = 60.
#' @param psi_0 Polarizaton of the incident light in the plane of incidence (PoI), in degrees.
#' 90 degrees corresponds to perpendicular to the PoI.  0 degrees corresponds to
#' polarization in the PoI.  Default = 90.
#' @param pol Degree of (linear) polarization.  0 <= pol <= 1.0.  1.0 means fully polarized, 0.0 means
#' unpolarized.  Default = 1.0
#' @description Create_detector creates a model of a flow cytometer scatter detector.
#' It is used along with a model of a scattering particle to model the detection of
#' light scatter due to small particles such as extracellular vesicles (EVs).
#' @return An object of class 'detector', with the following elements:
#' \describe{
#'   \item{beta}{The detector half-angle of acceptance}
#'   \item{psi_0}{The angle of polarization of incident light}
#'   \item{pol}{The degree of polarization of incident light}
#'   \item{theta_0}{Angle between the incident light source and the detector}
#' }
#' @export
create_detector = function(theta_0 = 90, beta = 60, psi_0 = 90, pol = 1) {
  detector = list()
  detector$beta = beta
  detector$psi_0 = psi_0
  detector$pol = pol
  detector$theta_0 = theta_0

  class(detector) = "detector"
  detector
}


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
#' @param r A numberic vector of radii of each layer, in nm.  Note that length(r) must equal
#' n_layers.
#' @param n A numberic vector of refractive indices of each layer.  Note that length(n) must equal
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
#' @param r The overall radius (lumen + membrane) of the EV, in nm.
#' @description Create_EV creates a model of a 2-layer particle, suitable to describe an EV.
#' This is really just a convenience wrapper for \link{create_particle}, allowing the description of
#' a particle with total radius `r`, surrounded by a membrane specified by its thickness.
#' The refractive indices of the core and membrane can be specified separately.
#' @return An object of class \code{ \link[Rscattnlay]{Scatterer}}.
#' @export
create_EV <- function(medium = 1.34, lambda = 488, n_core = 1.38, n_membrane = 1.46, thickness_membrane = 5.0, r) {

  r_core = r - thickness_membrane

  P <- create_particle(medium = medium, lambda = lambda, n_layers = 2, r = c(r_core, r), n = c(n_core, n_membrane))

  P
}

# Given: A RScattnlay Scatterer specification 'S'
#        describes the medium's and the particle's refractive indices
#        and particle geometry (e.g. core + shell, just core, etc)
#        and incident radiation wavelength
# Given: detector half-angle of acceptance 'beta' (degrees)
# Given: Laser polarization angle psi_0 (degrees)
#          0   = in the plane of incidence
#         pi/2 = perpendicular to the plane of incidence
#         degree of polarization 'pol' (0 <= pol <= 1)
# Given: a coefficient 'gain' which expresses the relative conversion
#        of photons to SSC signal amplitude
#
# Do the numeric integration of detected scattering signal over the
# detector surface
#    dr   = delta_r (scaled such that detector radius = 1 unit)
#    dphi = delta_phi (azimuthal angle on detector face, in degrees)
calculate_ssc_particle = function(S, detector = create_detector(),
                                  gain = 1, dr = .01, dphi = 1) {

  beta = detector$beta
  psi_0 = detector$psi_0
  pol = detector$pol
  theta_0 = detector$theta_0

  # get the Stokes matrix elements S11, S12
  Q <- amplitudes(S)
  S1 = Q$S1
  S2 = Q$S2
  qthet = Q$Theta

  S11 = 0.5 * (abs(S2)^2 + abs(S1)^2)
  S12 = 0.5 * (abs(S2)^2 - abs(S1)^2)

  # calculate distance to detector, given R = 1 and beta
  l = 1 / tan(beta*pi/180)

  # calculate area of detector, for normalization at the end
  A = pi

  dphi  = dphi  * pi / 180   # convert to radians
  psi_0 = psi_0 * pi / 180
  ssc = 0
  for (r in seq(0, 1, by = dr)) {
    for (phi in seq(0, 2*pi - dphi, by = dphi)) {
      theta = theta_0 * pi / 180 + atan(r * cos(phi) / l)
      psi = psi_0  - atan(r * sin(phi) / l)
      # if (r == 1) {
      #   cat("r = ", r, "phi = ", phi, "theta = ", theta, " psi = ", psi, "\n")
      # }

      idx = which(abs(qthet - theta) == min(abs(qthet - theta)))[1]
      s11 = S11[idx]
      s12 = S12[idx]

      incr_ssc = (s11 + s12 * pol * cos(2*psi)) * r * dr * dphi
      # if (r == 1) {
      #   cat("idx = ", idx, "psi = ", psi, "incr_ssc = ", incr_ssc, "\n")
      # }
      ssc = ssc + incr_ssc
    }
  }

  # divide the detector area out of the result (area = pi)
  ssc = ssc / A

  # multiply by gain factor
  ssc = ssc * gain

  ssc
}



