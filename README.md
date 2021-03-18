
# PROJECT3PACKAGE

<!-- badges: start -->
[![R-CMD-check](https://github.com/seangrimm/PROJECT3PACKAGE/workflows/R-CMD-check/badge.svg)](https://github.com/seangrimm/PROJECT3PACKAGE/actions)
[![codecov](https://codecov.io/gh/seangrimm/PROJECT3PACKAGE/branch/master/graph/badge.svg?token=1GGO5ZRFTW)](https://codecov.io/gh/seangrimm/PROJECT3PACKAGE)
<!-- badges: end -->

The goal of `PROJECT3PACKAGE` is to demonstrate package building for Project 3 in STAT 302 at UW.

## Installation

You can install the released version of PROJECT3PACKAGE using the following lines:

``` r
# install.packages("devtools")
devtools::install_github("seangrimm/PROJECT3PACKAGE", build_vignette = TRUE, build_opts = c())
library(PROJECT3PACKAGE)
```

## Use

Demonstrations of this package's functions can be seen on the attached vignette. To view the vignette in the Help window, simply use the following line:

```r
help(package = "PROJECT3PACKAGE", help_type = "html")
```

Alternatively, you can view the vignette as a separate HTML file using the line:

```r
utils::browseVignettes(package = "PROJECT3PACKAGE")
```
