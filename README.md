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
```
library(elfgen)

watershed.df <- elfgen_getdata('02080201')
breakpt <- method_pwit("watershed.df" = watershed.df,
					   "quantile" = 0.95,
					   "glo" = 50,
					   "ghi" = 1000)  
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.95,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53,
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")
```

![](man/figures/README-example-1.png)<!-- -->
