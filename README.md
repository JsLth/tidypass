
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidypass

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A tidy R interface to the
[Postpass](https://github.com/woodpeck/postpass-ops) service. Allows you
to use dplyr syntax to retrieve OpenStreetMap data using
[`{dbplyr}`](https://dbplyr.tidyverse.org/). Postpass is a nice
alternative to [Overpass Turbo](https://overpass-turbo.eu/) that is
faster and more versatile because it uses a Postgres database in the
background.

## Installation

You can install the development version of tidypass from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jslth/tidypass")
```

## Example

This is a basic example which extracts all fast food restaurants in
Karlsruhe along with their stated cuisine.

``` r
library(tidypass)
library(dplyr, warn.conflicts = FALSE)
library(sf, quietly = TRUE)
#> Linking to GEOS 3.13.0, GDAL 3.10.1, PROJ 9.5.1; sf_use_s2() is TRUE

# Boundaries of Karlsruhe
bbox <- st_as_sfc(st_bbox(c(
  xmin = 8.34,
  xmax = 8.46,
  ymin = 48.97,
  ymax = 49.03
)))

pp_tbl("point") |>
  filter(amenity == "fast_food" & way %&&% !!pg_bbox(bbox)) |>
  mutate(cuisine = tags %->% "cuisine") |>
  select(name, cuisine, way) |>
  collect()
#> Simple feature collection with 141 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 8.346221 ymin: 48.97115 xmax: 8.459447 ymax: 49.02864
#> Geodetic CRS:  WGS 84
#> # A tibble: 141 × 3
#>    name                       cuisine                             geometry
#>    <chr>                      <chr>                            <POINT [°]>
#>  1 Habibi                     oriental;falafel;kebab   (8.411658 49.00911)
#>  2 Shanghai Wok               asian                    (8.371491 49.01087)
#>  3 Pizza Pazza                burger;currys;pasta;piz…  (8.392619 48.9857)
#>  4 Kayas Döner                kebab                    (8.457469 48.99779)
#>  5 anatolia Pizza & Kebaphaus kebab                    (8.366429 49.01724)
#>  6 Monis Westbahnhof          german                    (8.36379 49.00237)
#>  7 Bamboo Canteen             asian                    (8.400894 48.99431)
#>  8 Abo's Pizza & Döner        kebab;pizza              (8.360729 48.98807)
#>  9 33 Mersin Tantuni          <NA>                     (8.453179 48.99663)
#> 10 Pizza King 60              <NA>                      (8.374086 48.9944)
#> # ℹ 131 more rows
```
