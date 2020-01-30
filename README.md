# flowMie
Mie Theory modeling of flow cytometer response to extracellular vesicles

## Installation

#### Install Rcpp
flowMie depends upon Rscattnlay, written by Jon Bramble.  Before installing 
Rscattnlay, please be sure you have installed Rcpp first.  You can get that
directly from [CRAN](https://cran.r-project.org/web/packages/Rcpp/index.html).

`devtools::install_cran(pkgs = "Rcpp")`

#### Install Rscattnlay
Once you have Rcpp installed, install Rscattnlay from my fork of Jon's package:

`devtools::install_github("rogerswt/RScattnlay")`

#### Install flowMie
Now you're ready:

`devtools::install_github("rogerswt/flowMie", build_vignettes = TRUE)`


