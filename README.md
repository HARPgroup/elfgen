# elfgen <img src="man/figures/logo.png" align="right" width="120" />

[![Travis Build Status](https://travis-ci.org/HARPgroup/elfgen.svg?branch=master)](https://travis-ci.org/HARPgroup/elfgen)
[![Coverage Status](https://codecov.io/gh/HARPgroup/elfgen/branch/master/graph/badge.svg)](https://codecov.io/gh/HARPgroup/elfgen)

## Overview
 
elfgen is a framework for generating Ecological Limit Function (ELF) models.

## Installation

``` r
library("devtools")
install_github('HARPgroup/elfgen')
library(elfgen)
```

## Usage
An introductory example of how elfgen works is supplied below. You start 
by either supplying a dataset with flow and richness data, or by supplying 
a HUC code of interest. When supplying a HUC code: `elfgen_getdata()` will 
retieve IchthyMaps data from USGS ScienceBase and automatically derive fish 
species richness at the NHDPlusV2 segment scale. Mean annual flow data is then
automatically retrieved for each NHDPlusV2 segment using an EPA JSON webservice.

A breakpoint in the flow-ecology relation is determined using a fixed user-defined
value, or identified using the functions `bkpt_pwit()` or `bkpt_ymax()`. The ELF
model is then generated and plotted using `elfgen()` with ELF model statistics
returned.

``` r
# Retrieve dataset of interest
watershed.df <- elfgen_getdata('02080201')

# Determine breakpoint in flow-ecology relation
breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = 0.95, "glo" = 50, "ghi" = 1000)  
					   
# Plot the flow-ecology relation and generate ELF model					   
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
#>	   m    b rsquared rsquared_adj p n_total n_subset n_subset_upper
#> 1	2.34 9.19    0.806          0.8 0     861      705             35
```

## Richness Change

``` r
elf_change(elf$stats, 10)
```
