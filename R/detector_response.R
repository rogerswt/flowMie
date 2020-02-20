#
# detector_response.R
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

#' @title Calculate Detector Response
#' @param particle A \code{ \link[Rscattnlay]{Scatterer}} particle description. This object
#' encapsulates the medium's and the particle's refractive indices, the particle's
#' geometry (e.g. core + shell, just core, etc), and the incident radiation wavelength.
#' Default = an EV with diameter 180 nm.
#' @param detector A detector object, created using \link{create_detector}.  This
#' object describes the geometry of the detector as well as the polarization state
#' of the incident light.
#' @param dr The numerical integration delta_radius across the detector, whose
#' diameter is, by definition, 1.0.  Integration limits are 0,1.  Default = 0.02,
#' accurate to about 2 percent.
#' @param dphi The numerical integration of azimuthal angle delta_phi across the detector.
#' Integration limits are 0,360.  Default = 10.0, accurate to about .06 percent.
#' @description This function performs the numerical integration of detected
#' scattering signal over the detector surface.  A significant constraint is that
#' the detector is assumed to be cylindrically symmetrical.
#'
#' We retrieve the scattering amplitudes from Rscattnlay, and calculate the
#' Stokes matrix elements S11 and S12, which are required to accurately handle
#' polarization effects.
#' @export
calculate_detector_response = function(particle = create_EV(d = 180), detector = create_detector(),
                                       dr = .02, dphi = 10.0) {

  if (is(detector) != "detector") {
    stop("Please create a valid detector using create_detector()")
  }

  if (is(particle) != "Scatterer") {
    stop("Please create a valid particle description using create_particle()")
  }
  alpha = detector$alpha
  psi_0 = detector$psi_0
  pol = detector$pol
  theta_0 = detector$theta_0
  eta = detector$eta
  eta_fac = detector$eta_fac
  gain = detector$gain

  # get the Stokes matrix elements S11, S12
  Q <- amplitudes(particle)
  S1 = Q$S1
  S2 = Q$S2
  qthet = Q$Theta

  S11 = 0.5 * (abs(S2)^2 + abs(S1)^2)
  S12 = 0.5 * (abs(S2)^2 - abs(S1)^2)

  # calculate distance to detector, given R = 1 and alpha
  l = 1 / tan(alpha*pi/180)

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

      # this area element has a relative efficiency given by eta(alpha)
      alpha_prime = atan2(r, l)
      eff = eta(alpha_prime, alpha * pi / 180, eta_fac = eta_fac)
      incr_ssc = eff * (s11 + s12 * pol * cos(2*psi)) * r * dr * dphi
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




