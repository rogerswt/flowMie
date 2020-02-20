#
# detector.R
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
# alpha = 60    # detector acceptance half-angle
# psi_0 = 90   # polarization perpendicular to the plane of incidence
# pol = 1      # fully polarized light
# theta_0 = 90 # SSC detector

#' @import Rscattnlay
#' @import flowCore
#' @importFrom methods as is
#' @importFrom stats approx

#' @title Create a Model of a Flow Cytometer Scatter Detector
#' @param theta_0 Angle between the incident light source and the detector,in degrees.
#' Default = 90 (side scatter detector).
#' @param alpha The detector acceptance half-angle, in degrees.  Default = 60.
#' @param psi_0 Polarizaton of the incident light in the plane of incidence (PoI), in degrees.
#' 90 degrees corresponds to perpendicular to the PoI.  0 degrees corresponds to
#' polarization in the PoI.  Default = 90.
#' @param pol Degree of (linear) polarization.  0 <= pol <= 1.0.  1.0 means fully polarized, 0.0 means
#' unpolarized.  Default = 1.0
#' @param gain A relative gain factor, used to "calibrate" a detector
#' @param eta A function describing the radial detector efficiency
#' @param lut A lookup table, calculated during calibration, to 'invert' the Mie transform
#' such that, given a scattering signal, the particle diameter can be looked up.
#' @param eta_fac A factor used in some eta functions
#' @description Create_detector creates a model of a flow cytometer scatter detector.
#' It is used along with a model of a scattering particle to model the detection of
#' light scatter due to small particles such as extracellular vesicles (EVs).
#' @return An object of class 'detector', with the following elements:
#' \describe{
#'   \item{alpha}{The detector half-angle of acceptance}
#'   \item{psi_0}{The angle of polarization of incident light}
#'   \item{pol}{The degree of polarization of incident light}
#'   \item{theta_0}{Angle between the incident light source and the detector}
#' }
#' @export
create_detector = function(theta_0 = 90, alpha = 60, psi_0 = 90, pol = 1, gain = 1, lut = NULL, eta = eta_uniform, eta_fac = NA) {
  detector = list()
  detector$alpha = alpha
  detector$psi_0 = psi_0
  detector$pol = pol
  detector$theta_0 = theta_0
  detector$eta_fac = eta_fac
  detector$eta = match.fun(eta)
  detector$gain = gain
  detector$lut = lut

  class(detector) = "detector"
  detector
}

# eta functions represent the relative efficiency of the detector as a function
# of the angle alpha_prime from the axis of the detector, where alpha_prime
# ranges from zero (on the axis) to alpha (at the edge of the objective's entrance
# aperture).
#
# This is a uniform detector
#' @title Model Detector Radial Efficiency
#' @param alpha_prime The variable angle (in radians) over the detector (zero on the axis)
#' @param alpha The maximum acceptance angle (in radians) of the detector
#' @description \code{eta_uniform} models a uniform detector
#' @rdname eta_functions
#' @export
eta_uniform = function(alpha_prime = NA, alpha = NA, eta_fac = NA) {
  1.0
}

# this is a van der Pol detector
#' @title Model Detector Radial Efficiency
#' @param alpha_prime The variable angle (in radians) over the detector (zero on the axis)
#' @param alpha The maximum acceptance angle (in radians) of the detector
#' @description \code{eta_van_der_pol} models a detector whose efficiency drops
#' off sinusoidally from 1.0 on the axis to 0.0 at the maximum acceptance angle
#' @rdname eta_functions
#' @export
eta_van_der_pol = function(alpha_prime = NA, alpha = NA, eta_fac = NA) {
  eta = sin((pi / 2) * ((alpha_prime / alpha) + 1))

  eta
}

# this is a Modified van der Pol detector
#' @title Model Detector Radial Efficiency
#' @param alpha_prime The variable angle (in radians) over the detector (zero on the axis)
#' @param alpha The maximum acceptance angle (in radians) of the detector
#' @param fac The relative efficiency of the detector at its edge (0.0 < fac < 1.0)
#' @description \code{eta_mvdp} models a detector whose efficiency drops
#' off sinusoidally from 1.0 on the axis to 'fac' (0.0 < fac < 1.0) at the maximum acceptance angle
#' @rdname eta_functions
#' @export
eta_mvdp = function(alpha_prime = NA, alpha = NA, eta_fac = NA) {
  if (eta_fac < 0 | eta_fac > 1) {stop("eta_fac must be between 0.0 and 1.0")}
  fac2 = 2 * acos(eta_fac) / pi
  eta = sin((pi / 2) * ((fac2 * alpha_prime / alpha) + 1))

  eta
}
