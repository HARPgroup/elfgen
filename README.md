# elfgen <img src="man/figures/logo.png" align="right" width="120" />

## Overview

elfgen is a framwework for generating Ecological Limit Functions (ELFs).

## Installation

``` r
library("devtools")
install_github('HARPgroup/elfgen')
library(elfgen)
```

## Usage
``` r
library(elfgen)

# Retrieve dataset of interest
watershed.df <- elfgen_getdata('02080201')

# Determine breakpoint in flow-ecology relation
breakpt <- method_pwit("watershed.df" = watershed.df,
					   "quantile" = 0.95,
					   "glo" = 50,
					   "ghi" = 1000)  
					   
# Plot the flow-ecology relation and derived ELF model					   
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.95,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53,
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")
```

![](man/figures/README-example-1.png)<!-- -->
