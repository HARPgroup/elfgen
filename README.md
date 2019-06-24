# elfgen <img src="man/figures/logo.png" align="right" width="120" />

[![Travis Build Status](https://travis-ci.org/HARPgroup/elfgen.svg?branch=master)](https://travis-ci.org/HARPgroup/elfgen)
[![Coverage Status](https://codecov.io/gh/HARPgroup/elfgen/branch/master/graph/badge.svg)](https://codecov.io/gh/HARPgroup/elfgen)

# Overview
 
elfgen is a framework for generating Ecological Limit Function (ELF) models.

# Installation

``` r
# Install development version from Github:
# install.packages("devtools")
devtools::install_github("HARPgroup/elfgen")
```

# Usage
An introductory example of how elfgen works is supplied below. You start 
by either supplying a dataset with flow and richness data, or by supplying 
a HUC code of interest. When supplying a HUC code: `elfdata()` will 
retieve IchthyMaps data from USGS ScienceBase and automatically derive fish 
species richness at the NHDPlusV2 segment scale. Mean annual flow data is then
automatically retrieved for each NHDPlusV2 segment using an EPA JSON webservice.

A breakpoint in the flow-ecology relation is determined using a fixed user-defined
value, or identified using the functions `bkpt_pwit()` or `bkpt_ymax()`. The ELF
model is then generated and plotted using `elfgen()` with ELF model statistics
returned.


## Example
Load package and data.

``` r
library(elfgen)

# Retrieve dataset of interest
watershed.df <- elfdata('02080201')
```

``` r
# Alternatively, utilize a user-supplied dataset in the following format:
watershed.df <- data.frame(flow=c(28.257, 20.254, 22.825, ...), 
			   richness=c(2, 10, 12, ...),
			   watershed.code='02080201',
			   stringsAsFactors=FALSE) 
```


Identify breakpoint in flow-ecology relation using one of 3 methods.
``` r
# Fixed Method
breakpt <- 500

# Piecewise Iterative Method
breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = 0.95, "blo" = 200, "bhi" = 500)  
#> [1] "Breakpoint identified at 310.815"
		
# Ymax Method		
breakpt <- bkpt_ymax("watershed.df" = watershed.df)			   
#> [1] "Breakpoint identified at 142.989"
```

Plot flow-ecology relation and generate ELF model.	
``` r				   
elf <- elfgen("watershed.df" = watershed.df,
	      "quantile" = 0.95,
	      "breakpt" = breakpt,
	      "yaxis_thresh" = 53,
	      "xlabel" = "Mean Annual Flow (ft3/s)",
	      "ylabel" = "Fish Species Richness")
```

``` r
elf$plot
```

![](man/figures/README-example-1.png)<!-- -->

``` r
elf$stats
#>     watershed breakpt quantile    m    b rsquared rsquared_adj p n_total n_subset n_subset_upper
#> 1	02080201 142.989     0.95 2.34 9.19    0.806          0.8 0     861      705             35
```

## Richness Change

``` r
# Calculate absolute richness change
richness_change(elf$stats, "pctchg" = 10)
#> [1] 0.2465436

# Calculate percent richness change at a specific stream size
richness_change(elf$stats, "pctchg" = 10, "xval" = 500)
#> [1] 1.038858
```

``` r
# Generate plot of percent richness change for various percent flow reductions
elfchange(elf$stats, "yaxis_thresh" = 25)
```

![](man/figures/README-example-2.png)<!-- -->
