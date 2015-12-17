# RNHANES
`RNHANES` is an R package for accessing and analyzing CDC [NHANES](http://www.cdc.gov/nchs/nhanes.htm) data

## Examples

```R

library(RNHANES)

# Download environmental phenols & parabens data from the 2011-2012 survey cycle
load_nhanes_data("EPH_G", "2011-2012")

# Download the same data, but this time include demographics data
load_nhaneS_data("EPH_G", "2011-2012", demographics = TRUE)

# Download the data, and save it to a known location
load_nhanes_data("EPH_G", "2011-2012", demographics = TRUE, destination = "./NHANES_Data")
```

## Install

The simplest way to install `RHNAHES` is to use the `devtools` package

```R
library(devtools)

install_github("silentspringinstitute/RNHANES")
```
