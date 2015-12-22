# RNHANES
`RNHANES` is an R package for accessing and analyzing CDC [NHANES](http://www.cdc.gov/nchs/nhanes.htm) data

## Examples

```R

library(RNHANES)

# Download environmental phenols & parabens data from the 2011-2012 survey cycle
load_nhanes_data("EPH_G", "2011-2012")

# Download the same data, but this time include demographics data
dat <- load_nhanes_data("EPH_G", "2011-2012", demographics = TRUE)

# Compute 95th and 99th quantiles for urinary triclosan
nhanes_quantile(dat, column = "URXTRS", weights_column = "WTSA2YR", quantiles = c(0.95, 0.99))

```

## Install

The simplest way to install `RHNAHES` is to use the `devtools` package

```R
library(devtools)

install_github("silentspringinstitute/RNHANES")
```
