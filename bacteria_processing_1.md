bacteria\_processing\_1
================
Ilya
1/28/2019

#### install and load required packages

    ## Removing package from '/Library/Frameworks/R.framework/Versions/3.4/Resources/library'
    ## (as 'lib' is unspecified)
    ## Removing package from '/Library/Frameworks/R.framework/Versions/3.4/Resources/library'
    ## (as 'lib' is unspecified)

    ## 
    ## Attaching package: 'rlang'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     :=

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/0d/qm_pqljx11s_ddc42g1_yscr0000gn/T//RtmpCE1SUw/downloaded_packages
    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/0d/qm_pqljx11s_ddc42g1_yscr0000gn/T//RtmpCE1SUw/downloaded_packages

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:glue':
    ## 
    ##     collapse

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## Attaching package: 'reshape2'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     dcast, melt

    ## corrplot 0.84 loaded

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

    ## CHNOSZ version 1.1.3 (2017-11-13)

    ## Please run data(thermo) to create the "thermo" object

    ## 
    ## Attaching package: 'CHNOSZ'

    ## The following objects are masked from 'package:Hmisc':
    ## 
    ##     mtitle, spearman

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

    ## Skipping install of 'taxizedb' from a github remote, the SHA1 (7ee9741a) has not changed since last install.
    ##   Use `force = TRUE` to force installation

    ## 
    ## Attaching package: 'taxizedb'

    ## The following objects are masked from 'package:taxize':
    ## 
    ##     children, classification, downstream

### 1. Get bacteria-caused dx in GIDEON

### 2. Match dx to pathogen spp. names

### 3. Match spp. names (from GIDEON zdx and GMPD) to traits (in GMPD & other)

### 4. Compile master list of bacteria spp & traits

### 5. Feature construction with bacterial traits

### 6. Data visualization: summary “state of knowledge” on bacteria causing disease in mammals or humans

### 7. Use traits to predict transmissibility and human disease outcomes

### 2. Match dx to pathogen spp. names

#### Include non-zoonotic pathogenic bacteria

#### get zoonotic and non-zoonotic pathogens from GMPD and assign taxonomic ID only for those that are species-level

select only bacteria using info in G\_taxonomy

``` r
source("R_name2taxid_GMPD.R")
```

    ## [1] TRUE
    ## [1] 2362

``` r
# source(paste0(wd,"/R_name2taxid_GMPD.R"))
# ../CODE/DATA_PROCESSING/
```

#### classify pathogens in GMPD to order, family, genus. Make G\_classified.Rdata and save

``` r
source("R_classify_bacteria_observed_GMPD.R")
```

    ##       user     system    elapsed 
    ## 0.69635000 0.02011667 0.71906667

#### load data on zoonotic bacteria

``` r
# source("R_human_zoonotic_non_zoonotic1.R")
```
