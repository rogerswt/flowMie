# flowMie
Mie Theory modeling of flow cytometer responses to extracellular vesicles

## Installation

#### Step 1: Install Rcpp
flowMie depends upon Rscattnlay, written by Jon Bramble.  Before installing 
Rscattnlay, please be sure you have installed Rcpp first.  You can get that
directly from [CRAN](https://cran.r-project.org/web/packages/Rcpp/index.html).

`devtools::install_cran(pkgs = "Rcpp")`

#### Step 2: Install Rscattnlay
Once you have Rcpp installed, install Rscattnlay from my fork of Jon's package:

`devtools::install_github("rogerswt/RScattnlay")`

#### Step 3: Install flowMie
Now you're ready:

`devtools::install_github("rogerswt/flowMie", build_vignettes = TRUE)`

#### Step 4: Please read the vignette and enjoy!

## Citation
flowMie is as yet unpublished.  Please acknowledge me if you use it for publication,
and drop me a note as well!

[Wade Rogers](mailto:wade.rogers@spcytomics.com)

[Still Pond Cytomics](https://spcytomics.com)
