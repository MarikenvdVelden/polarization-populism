Prepare Data
================

- [Required Packages &
  Reproducibility](#required-packages--reproducibility)
- [Tidy & Save Data](#tidy--save-data)
- [Descriptives](#descriptives)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
#renv::snapshot()
```

## Tidy & Save Data

``` r
d <- read_sav(here("data/raw-private/kieskompas.sav")) 
source(here("src/data-processing/clean_data.R"))
save(d, file = here("data/intermediate/cleandata.Rdata"))
```

## Descriptives

<img src="../../report/figures/contvar-1.png" style="display: block; margin: auto;" />

<img src="../../report/figures/corrplot-1.png" style="display: block; margin: auto;" /><img src="../../report/figures/corrplot-2.png" style="display: block; margin: auto;" /><img src="../../report/figures/corrplot-3.png" style="display: block; margin: auto;" /><img src="../../report/figures/corrplot-4.png" style="display: block; margin: auto;" /><img src="../../report/figures/corrplot-5.png" style="display: block; margin: auto;" /><img src="../../report/figures/corrplot-6.png" style="display: block; margin: auto;" />
