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
# beta = 60    # detector acceptance half-angle
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
