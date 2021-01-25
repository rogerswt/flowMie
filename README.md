# flowMie
Mie Theory modeling of flow cytometer responses to extracellular vesicles

## Installation

#### Step 0: Install flowMie
`devtools::install_github("rogerswt/flowMie", build_vignettes = TRUE)`

In your list of packages you should now have

- Rcpp
- Rscattnlay
- flowMie

If they're all there you're good to go - skip to Step 4.  If not, Please proceed through
steps 1 - 3.

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
flowMie was presented as a [poster at the virtual CYTO_2020 meeting](FlowMie_CYTO2020.pdf).  
Please acknowledge me if you use it for publication,
and drop me a note as well!

[Wade Rogers](mailto:wade.rogers@spcytomics.com)

[Still Pond Cytomics](https://spcytomics.com)
