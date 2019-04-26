# elfgen <img src="man/figures/logo.png" align="right" width="120" />

## Overview
elfgen is a generator for Ecological Limit Function plots and equations. 
Examples and test datasets for generating Flow and Species Richness ELFs
for flow alteration analysis.

## Installation
```
library("devtools")
install_github('HARPgroup/elfgen')
library(elfgen)
```
# try it out
```
elfgen_cleandata(data=data.frame())

watershed.df <- elfgen_getdata('020700080403')
elfgen_testplot(watershed.df)
```
