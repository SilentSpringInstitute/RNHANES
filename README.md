# RNHANES
`RNHANES` is an R package for accessing and analyzing CDC [NHANES](http://www.cdc.gov/nchs/nhanes.htm) data

[![Build Status](https://travis-ci.org/SilentSpringInstitute/RNHANES.svg?branch=master)](https://travis-ci.org/SilentSpringInstitute/RNHANES)
[![codecov.io](https://codecov.io/github/SilentSpringInstitute/RNHANES/coverage.svg?branch=master)](https://codecov.io/github/SilentSpringInstitute/RNHANES?branch=master)


## Examples

```R

library(RNHANES)

# Download environmental phenols & parabens data from the 2011-2012 survey cycle
dat <- nhanes_load_data("EPH", "2011-2012")

# Download the same data, but this time include demographics data (which includes sample weights)
dat <- nhanes_load_data("EPH", "2011-2012", demographics = TRUE)

# Find the sample size for urinary triclosan
nhanes_sample_size(dat,
                   column = "URXTRS",
                   comment_column = "URDTRSLC",
                   weights_column = "WTSA2YR")

# Compute 95th and 99th quantiles for urinary triclosan
nhanes_quantile(dat,
                column = "URXTRS",
                comment_column = "URDTRSLC",
                weights_column = "WTSA2YR",
                quantiles = c(0.95, 0.99))

# Compute the detection frequency of urinary triclosan
nhanes_detection_frequency(dat,
                           column = "URXTRS",
                           comment_column = "URDTRSLC",
                           weights_column = "WTSA2YR")

```

## Install

The simplest way to install `RHNAHES` is to use the `devtools` package

```R
library(devtools)

install_github("silentspringinstitute/RNHANES")
```
