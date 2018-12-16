bacteria
================
Ilya
12/7/2018

#### To do:

Correct taxonomy in Barberan et al. 2017.

Add code reading in GIDEON data from other Rmd file.

Bacterial traits and human disease outcomes
===========================================

Strategy
--------

Integrate data on host species, pathogen species, pathogen traits, and human disease outcomes.

Apply generalized boosted models (GBM) to predict human disease outcomes based on bacterial traits.

Data sources
------------

1.  GIDEON dataset of ungulate hosts and zoonotic diseases
2.  Dataset relating mammals to pathogens (GMPD)
3.  Bacterial trait datasets (Brbic et al. 2016; Barberan et al. 2017)
4.  Human disease outcomes (GIDEON): case fatality rates, outbreak size, prevalence 5. Ungulate host ranges (IUCN)

Study design
------------

#### install and load required packages

    ## 
    ## Attaching package: 'dplyr'

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

### Data: bacterial traits

#### Bacterial traits: read in and save Brbic et al. 2016

Source: ProTraits: <http://protraits.irb.hr/data.html>. We are using version of data in which traits have been binarized. Read in data and save as p1.Rdata.

``` r
source("read_data_pathogen_traits1.R")
```

    ## [1] "dimensions"
    ## [1] 3046  427

#### Bacterial traits: correct taxonomy in Brbic et al. 2016 data

Output data to file (species.csv) to upload to NCBI website (<https://www.ncbi.nlm.nih.gov/Taxonomy/TaxIdentifier/tax_identifier.cgi>). After running taxonomy\_ncbi.R, go to NCBI website, upload species.csv, and choose option to save to file from website. Save file to working directory.

``` r
source("taxonomy_ncbi.R")
```

Read back in file outputted from NCBI website ("tax\_report.txt"). Merge tax\_report.txt with p1 (original Brbic et al. data), with new field "preferred.name"

``` r
source("taxonomy_ncbi_out.R")
```

    ## [1] "number with non-preferred species name"
    ## [1] 204

Also attempted, didn't work: Attempted with R package taxize fxn synonyms and "col" (Catalog of Life). Note: taxize does not include NCBI. This requires a lot of interaction for species with multiple matches.

``` r
# source("taxonomy1.R")
```

Also attempted, didn't work: Attempted with R package myTAI. This option requires interaction while code is looping through species, for species with more than one entry in NCBI.

``` r
# source("taxonomy_p1_myTAI.R")
```

#### Bacterial traits: Read in data in Barberan et al. 2017 and save as p2.Rdata

Source: <https://figshare.com/articles/International_Journal_of_Systematic_and_Evolutionary_Microbiology_IJSEM_phenotypic_database/4272392>

``` r
source("read_data_pathogen_traits2.R")
```

#### Bacterial traits: correct taxonomy in Barberan et al. 2017 data

#### find species in common between Brbic et al. (p1) and Barberan et al.

``` r
load("p1.Rdata")
load("p2.Rdata")
p2$Organism_name = paste(p2$Genus, p2$Species)

common = intersect(p1$Organism_name,
                   p2$Organism_name)
length(common)
```

    ## [1] 395
