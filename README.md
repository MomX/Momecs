
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="https://raw.githubusercontent.com/googlei18n/noto-emoji/master/svg/emoji_u1f37c.svg?sanitize=true" width="40"> Momecs
===============================================================================================================================

*Part of [MomX](https://momx.github.io/MomX/)* [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/vbonhomme/Momecs.svg?branch=master)](https://travis-ci.org/vbonhomme/Momecs) [![CRAN status](https://www.r-pkg.org/badges/version/Momecs)](https://cran.r-project.org/package=Momecs)

The goal of Momecs is, for multivariate data, and particularly morphometric data, to easily subset and perform PCA, LDA, SVM, KMEANS, HCLUST and friends.

Sometimes reproducibility profits some buttons and clicks.

Installation
------------

<!--
You can install the released version of Momecs from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Momecs")
```
-->
You can try the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vbonhomme/Momecs")
```

Example
-------

``` r
library(Momecs)

# On toy datasets
Momecs()

# On a Coe object
olea %>% opoly(5, nb.pts=60) %>% Momecs()
```

More to come.
