#
# mie_transform.R
#
################################################################################
################################################################################
#                     Copyright Still Pond Cytomics LLC 2019.                 ##
#        All Rights Reserved. No part of this source code may be reproduced   ##
#            without Still Pond Cytomics' express written consent.            ##
################################################################################
################################################################################
#


# # Transform SSC signal to a particle size representation
# # calibrate the 'gain' argument with 200nm ps beads @ SSC = 60000
# #
#
# # "train" the mie transform using SSC signal measured from a particle of known RI and size
# # Default particle is 200 nm polystyrene bead:
# # n = 1.605
# # r = 100
# # ssc_observed = 60000
# calibrate_ssc_conversion = function(n = 1.605, r = 100, ssc_observed = 60000) {
#   P <- create_particle(n_layers = 1, r = r, n = n)
#   gain = ssc_observed / ssc_raw
#
#   ssc_ev = vector('numeric')
#   rad = seq(10, 500, by = 5)
#   for (i in 1:length(rad)) {
#     P <- create_EV(r = rad[i])
#     ssc_ev[i]  = calculate_ssc_particle(P, gain = gain, dr = .01, dphi = 1)
#   }
#
#   lut = data.frame(SSC = ssc_ev, diameter = 2*rad)
#
#   lut
# }
#
# # use the calibrated SSC detector to append a calculated size parameter to
# # a flowFrame
# #
# append_size_parameter = function(ff, param = "SSC-H", lut = calibrate_ssc_conversion()) {
#   # extract the ssc parameter
#   ssc = exprs(ff)[, param]
#
#   # try to guess if it's been transformed.  Linearize it if so... KLUDGE
#   idx = which(colnames(ff) == param)
#   top = pData(parameters(ff))$maxRange[idx]
#   somebignumber = 1e5
#   if (!(top > somebignumber)) {
#     # we think a biexponential transform has been done.  undo it...  KLUDGE!!!
#     ssc = ibx(ssc)
#   }
#
#   # handle negative values
#   ssc[ssc < 0] = 0
#
#   # pass it through the lut
#   size = approx(lut, xout = ssc)$y
#
#   # tack this onto ff as a new calculated parameter
#   tmpmat = exprs(ff)
#   tmpmat = cbind(tmpmat, "Size (nm)" = size)
#   pdata = pData(parameters(ff))
#   pdata = rbind(pdata, list("Size (nm)", "<NA>", 262144, 0, 1000))
#   last.pname = paste("$P", nrow(pdata), sep = "")
#   rownames(pdata)[nrow(pdata)] = last.pname
#
#   ffs = flowFrame(tmpmat, parameters = as(pdata, "AnnotatedDataFrame"))
#
#   ffs
# }












