---
title: "Tudor England Urban Scaling"
subtitle: "Part 1 - Data Quality Checks"
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


#### Figure A1: Histograms of the Lay Subsidy data (n = 93)



<img src="figures/Fig_A1_Hists.png" width="2700" />



``` r
ks_test_tidy(data = main_dataset, columns = c("Log_Tax", "Log_Taxpayers"), 
             output = "gt", dist = "normal", alpha = 0.05, signif_digits = 3) %>% 
  tab_header(
    title = "Table 1. Asymptotic One-Sample Kolmogorov-Smirnov Normality Test for Log Tax and Log Taxpayers (n = 93)"
  )
```

```{=html}
<div id="hqnmlsdhwt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hqnmlsdhwt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hqnmlsdhwt thead, #hqnmlsdhwt tbody, #hqnmlsdhwt tfoot, #hqnmlsdhwt tr, #hqnmlsdhwt td, #hqnmlsdhwt th {
  border-style: none;
}

#hqnmlsdhwt p {
  margin: 0;
  padding: 0;
}

#hqnmlsdhwt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hqnmlsdhwt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hqnmlsdhwt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hqnmlsdhwt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hqnmlsdhwt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqnmlsdhwt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqnmlsdhwt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqnmlsdhwt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hqnmlsdhwt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hqnmlsdhwt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hqnmlsdhwt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hqnmlsdhwt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hqnmlsdhwt .gt_spanner_row {
  border-bottom-style: hidden;
}

#hqnmlsdhwt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hqnmlsdhwt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hqnmlsdhwt .gt_from_md > :first-child {
  margin-top: 0;
}

#hqnmlsdhwt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hqnmlsdhwt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hqnmlsdhwt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hqnmlsdhwt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hqnmlsdhwt .gt_row_group_first td {
  border-top-width: 2px;
}

#hqnmlsdhwt .gt_row_group_first th {
  border-top-width: 2px;
}

#hqnmlsdhwt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqnmlsdhwt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hqnmlsdhwt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hqnmlsdhwt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqnmlsdhwt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqnmlsdhwt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hqnmlsdhwt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hqnmlsdhwt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hqnmlsdhwt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqnmlsdhwt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqnmlsdhwt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqnmlsdhwt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqnmlsdhwt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqnmlsdhwt .gt_left {
  text-align: left;
}

#hqnmlsdhwt .gt_center {
  text-align: center;
}

#hqnmlsdhwt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hqnmlsdhwt .gt_font_normal {
  font-weight: normal;
}

#hqnmlsdhwt .gt_font_bold {
  font-weight: bold;
}

#hqnmlsdhwt .gt_font_italic {
  font-style: italic;
}

#hqnmlsdhwt .gt_super {
  font-size: 65%;
}

#hqnmlsdhwt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hqnmlsdhwt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hqnmlsdhwt .gt_indent_1 {
  text-indent: 5px;
}

#hqnmlsdhwt .gt_indent_2 {
  text-indent: 10px;
}

#hqnmlsdhwt .gt_indent_3 {
  text-indent: 15px;
}

#hqnmlsdhwt .gt_indent_4 {
  text-indent: 20px;
}

#hqnmlsdhwt .gt_indent_5 {
  text-indent: 25px;
}

#hqnmlsdhwt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hqnmlsdhwt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Table 1. Asymptotic One-Sample Kolmogorov-Smirnov Normality Test for Log Tax and Log Taxpayers (n = 93)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="var">Variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="n">Sample<br>Size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="dist">Distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="statistic">D Statistic</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="p.value">P-Value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="alpha">Alpha</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="alternative">Alternative<br>Hypothesis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="result">Result</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="var" class="gt_row gt_center">Log_Tax</td>
<td headers="n" class="gt_row gt_center">93</td>
<td headers="dist" class="gt_row gt_center">Gaussian</td>
<td headers="statistic" class="gt_row gt_center">0.080</td>
<td headers="p.value" class="gt_row gt_center">0.594</td>
<td headers="alpha" class="gt_row gt_center">0.05</td>
<td headers="alternative" class="gt_row gt_center">Two-sided</td>
<td headers="result" class="gt_row gt_center">Fail to Reject</td></tr>
    <tr><td headers="var" class="gt_row gt_center">Log_Taxpayers</td>
<td headers="n" class="gt_row gt_center">93</td>
<td headers="dist" class="gt_row gt_center">Gaussian</td>
<td headers="statistic" class="gt_row gt_center">0.137</td>
<td headers="p.value" class="gt_row gt_center">0.060</td>
<td headers="alpha" class="gt_row gt_center">0.05</td>
<td headers="alternative" class="gt_row gt_center">Two-sided</td>
<td headers="result" class="gt_row gt_center">Fail to Reject</td></tr>
  </tbody>
  
  
</table>
</div>
```



#### Figure A2: Kolmogorov-Smirnov Test Plots with 95% C.I.s



<img src="figures/Fig_A2_KS.png" width="2400" />



#### Figure A3: Zipfian Tail of Taxpayer Count Log-Log Rank-Size Plot






<img src="figures/Fig_A3_Zipf.png" width="1500" />


#### Figure A4: Analysis of the Expanded 1524/5 Lay Subsidy Taxpayer Counts Dataset (expanded sample of towns with at least 50 taxpayers) 



<img src="figures/Fig_A4_HistKS.png" width="2550" />


Kolmogorov-Smirnov test for Log Taxpayers (n = 134) using expanded sample of towns with at least 50 taxpayers 


``` r
ks_test_tidy(data = add_taxpayers, columns = "Log_Taxpayers", output = "gt", 
             dist = "normal", alpha = 0.05, signif_digits = 3) %>% 
  tab_header(
    title = "Table 2. Asymptotic One-Sample Kolmogorov-Smirnov Normality Test",
    subtitle = "For Log Taxpayers using the expanded sample of towns with at least 50 taxpayers (n = 134)"
  )
```

```{=html}
<div id="tvpoocmvuc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tvpoocmvuc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tvpoocmvuc thead, #tvpoocmvuc tbody, #tvpoocmvuc tfoot, #tvpoocmvuc tr, #tvpoocmvuc td, #tvpoocmvuc th {
  border-style: none;
}

#tvpoocmvuc p {
  margin: 0;
  padding: 0;
}

#tvpoocmvuc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tvpoocmvuc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tvpoocmvuc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tvpoocmvuc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tvpoocmvuc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tvpoocmvuc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tvpoocmvuc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tvpoocmvuc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tvpoocmvuc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tvpoocmvuc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tvpoocmvuc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tvpoocmvuc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tvpoocmvuc .gt_spanner_row {
  border-bottom-style: hidden;
}

#tvpoocmvuc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#tvpoocmvuc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tvpoocmvuc .gt_from_md > :first-child {
  margin-top: 0;
}

#tvpoocmvuc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tvpoocmvuc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tvpoocmvuc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#tvpoocmvuc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#tvpoocmvuc .gt_row_group_first td {
  border-top-width: 2px;
}

#tvpoocmvuc .gt_row_group_first th {
  border-top-width: 2px;
}

#tvpoocmvuc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tvpoocmvuc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tvpoocmvuc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tvpoocmvuc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tvpoocmvuc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tvpoocmvuc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tvpoocmvuc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tvpoocmvuc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tvpoocmvuc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tvpoocmvuc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tvpoocmvuc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tvpoocmvuc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tvpoocmvuc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tvpoocmvuc .gt_left {
  text-align: left;
}

#tvpoocmvuc .gt_center {
  text-align: center;
}

#tvpoocmvuc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tvpoocmvuc .gt_font_normal {
  font-weight: normal;
}

#tvpoocmvuc .gt_font_bold {
  font-weight: bold;
}

#tvpoocmvuc .gt_font_italic {
  font-style: italic;
}

#tvpoocmvuc .gt_super {
  font-size: 65%;
}

#tvpoocmvuc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tvpoocmvuc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tvpoocmvuc .gt_indent_1 {
  text-indent: 5px;
}

#tvpoocmvuc .gt_indent_2 {
  text-indent: 10px;
}

#tvpoocmvuc .gt_indent_3 {
  text-indent: 15px;
}

#tvpoocmvuc .gt_indent_4 {
  text-indent: 20px;
}

#tvpoocmvuc .gt_indent_5 {
  text-indent: 25px;
}

#tvpoocmvuc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#tvpoocmvuc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal" style>Table 2. Asymptotic One-Sample Kolmogorov-Smirnov Normality Test</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>For Log Taxpayers using the expanded sample of towns with at least 50 taxpayers (n = 134)</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="var">Variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="n">Sample<br>Size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="dist">Distribution</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="statistic">D Statistic</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="p.value">P-Value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="alpha">Alpha</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="alternative">Alternative<br>Hypothesis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="text-align: center; vertical-align: middle; font-weight: bold;" scope="col" id="result">Result</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="var" class="gt_row gt_center">Log_Taxpayers</td>
<td headers="n" class="gt_row gt_center">134</td>
<td headers="dist" class="gt_row gt_center">Gaussian</td>
<td headers="statistic" class="gt_row gt_center">0.063</td>
<td headers="p.value" class="gt_row gt_center">0.655</td>
<td headers="alpha" class="gt_row gt_center">0.05</td>
<td headers="alternative" class="gt_row gt_center">Two-sided</td>
<td headers="result" class="gt_row gt_center">Fail to Reject</td></tr>
  </tbody>
  
  
</table>
</div>
```



