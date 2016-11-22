# RNHANES
`RNHANES` is an R package for accessing and analyzing CDC [NHANES](http://www.cdc.gov/nchs/nhanes.htm) data that was developed by [Silent Spring Institute](http://silentspring.org).

[![CRAN Version](http://www.r-pkg.org/badges/version/RNHANES)](https://cran.r-project.org/web/packages/RNHANES/)
[![Build Status](https://travis-ci.org/SilentSpringInstitute/RNHANES.svg?branch=master)](https://travis-ci.org/SilentSpringInstitute/RNHANES)
[![codecov.io](https://codecov.io/github/SilentSpringInstitute/RNHANES/coverage.svg?branch=master)](https://codecov.io/github/SilentSpringInstitute/RNHANES?branch=master)

![Demo of RNHANES](http://i.imgur.com/TCYW4qR.gif)

## Features

- Download and search NHANES variable and data file lists
- Download and cache NHANES data files
- Compute sample sizes, detection frequencies, quantiles, and covariance matrices
- Plot weighted histograms

## Install

You can install the latest stable version through CRAN:
```R
install.packages("RNHANES")
```

Or you can install the latest development version from github:

```R
library(devtools)

install_github("silentspringinstitute/RNHANES")
```

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

# Compute the detection frequency of urinary triclosan
nhanes_detection_frequency(dat,
  column = "URXTRS",
  comment_column = "URDTRSLC",
  weights_column = "WTSA2YR")

# Compute 95th and 99th quantiles for urinary triclosan
nhanes_quantile(dat,
  column = "URXTRS",
  comment_column = "URDTRSLC",
  weights_column = "WTSA2YR",
  quantiles = c(0.95, 0.99))

# Plot a histogram of the urinary triclosan distribution
nhanes_hist(dat,
  column = "URXTRS",
  comment_column = "URDTRSLC",
  weights_column = "WTSA2YR")

# Build a survey design object for use with survey package
design <- nhanes_survey_design(dat, weights_column = "WTSA2YR")

```

