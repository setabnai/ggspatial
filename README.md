
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggspatial

<!-- badges: start -->

[![ggspatial on
CRAN](https://cranlogs.r-pkg.org/badges/ggspatial)](https://cran.r-project.org/package=ggspatial)
[![Coverage
Status](https://img.shields.io/codecov/c/github/paleolimbot/ggspatial/master.svg)](https://codecov.io/github/paleolimbot/ggspatial?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/paleolimbot/ggspatial/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/ggspatial/actions)
<!-- badges: end -->

Spatial data plus the power of the `ggplot2` framework means easier
mapping.

## Installation

The package is available on CRAN, and can be installed using
`install.packages("ggspatial")`. The development version can be
installed via **remotes**.

``` r
install.packages("ggspatial")
```

Or for the development version:

``` r
install.packages("remotes") # if remotes isn't installed
remotes::install_github("paleolimbot/ggspatial")
```

## Introduction

This package is a framework for interacting with spatial data using
**ggplot2** as a plotting backend. The package supports **sf** package
objects, **sp** package objects, and **raster** package objects, and
uses `geom_sf()` and `coord_sf()` to do most of the heavy lifting with
respect to coordinate transformation.

``` r
library(ggplot2)
library(ggspatial)
load_longlake_data()

ggplot() +
  # loads background map tiles from a tile source
  annotation_map_tile(zoomin = -1) +
  
  # annotation_spatial() layers don't train the scales, so data stays central
  annotation_spatial(longlake_roadsdf, size = 2, col = "black") +
  annotation_spatial(longlake_roadsdf, size = 1.6, col = "white") +

  # raster layers train scales and get projected automatically
  layer_spatial(longlake_depth_raster, aes(colour = after_stat(band1))) +
  # make no data values transparent
  scale_fill_viridis_c(na.value = NA) +
  
  # layer_spatial trains the scales
  layer_spatial(longlake_depthdf, aes(fill = DEPTH_M)) +
  
  # spatial-aware automagic scale bar
  annotation_scale(location = "tl") +

  # spatial-aware automagic north arrow
  annotation_north_arrow(location = "br", which_north = "true")
```

<img src="man/figures/README-fig-layer-spatial-sf-1.png" width="100%" />
