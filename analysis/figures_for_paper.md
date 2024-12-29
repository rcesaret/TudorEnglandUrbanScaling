---
title: "Tudor England Urban Scaling"
subtitle: "Figures from the paper"
author: "Rudolf Cesaretti"
date: "2019-08-09"
output:
  html_document: 
    toc: true
    toc_float: true
    code_folding: show
    keep_md: true
    highlight: tango
    theme: sandstone
---

Please direct any questions and comments to Rudolf Cesaretti (rcesaret@asu.edu)

Analyses in this 'Data Replication' script were performed in R versions 3.4.3 (2017-11-30) and 3.6.0 (2019-04-26). R-Studio is ideal for viewing and manipulating this file. 

The subsections of this script, indicated by triple-pound-sign comment lines, specify which subsections they correspond to in the main text (e.g. "4.1 Data Quality") and in the online Appendix (e.g. "A1.1 Distributional Analysis")

The additional R packages required in each section are indicated at the top of each section. To download the requisite packages to your library, simply un-comment-out the function "install.packages()", and make sure you have a secure connection to the internet.

This file requires that the working-directory be set to a local folder that includes "main_dataset.csv", "add_taxpayers.csv", "WCS.csv", "resid_coords_geog.csv", and "TudorCounties.csv" which are included in the online supplementary materials

Commands to export figures (as .tif files) and tables (as .csv files) have also been commented-out

## Preliminaries




### Load Custom R Functions


``` r
#Read in custom R functions located in the WD$R directory folder

# Custom function R file names
functions <- c("ks_test_tidy.R", "histogram_density.R", 
               "plot_ks_ci.R", "gg_histogram_density.R", 
               "ggplot_ks_ci.R") 

invisible(lapply(functions, function(filename) {
  source(file.path("R", filename))
}))

rm(functions) # Remove the vector of function filenames from the environment
```


### Load Required Packages


``` r
# Load required packages
library(MASS)
library(NSM3)
library(gt)
library(dplyr)
library(ggplot2)
```


### Load Data


``` r
# Upload datasets from working directory
main_dataset <- read.csv(file = "data/raw/main_dataset.csv", header=TRUE, sep=",")

add_taxpayers <- read.csv(file = "data/raw/add_taxpayers.csv", header=TRUE, sep=",")
```




## 4.1 Data Quality and A1.1 Distributional Analysis


### Figure A1: Histograms of the Lay Subsidy data (n = 93)



<img src="figures/Fig_A1_Hists.png" width="2700" />



### Figure A2: Kolmogorov-Smirnov Test Plots with 95% C.I.s



<img src="figures/Fig_A2_KS.png" width="2400" />



### Figure A3: Zipfian Tail of Taxpayer Count Log-Log Rank-Size Plot






<img src="figures/Fig_A3_Zipf.png" width="1500" />


### Figure A4: Analysis of the Expanded 1524/5 Lay Subsidy Taxpayer Counts Dataset (expanded sample of towns with at least 50 taxpayers) 



<img src="figures/Fig_A4_HistKS.png" width="2550" />

