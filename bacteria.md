Bacterial traits and human disease outcomes
================
Ilya
12/7/2018

#### To do:

Add code reading in GIDEON data from other Rmd file.

Strategy
--------

Integrate data on host species, pathogen species, pathogen traits, and human disease outcomes.

Apply generalized boosted models (GBM) to predict human disease outcomes based on bacterial traits.

Data sources
------------

1.  GIDEON dataset of ungulate hosts and zoonotic diseases
2.  Dataset relating mammals to pathogens (GMPD)
3.  Bacterial trait datasets (Brbic et al. 2016; Barberan et al. 2017)
4.  Dataset matching diseases to pathogens (to be collected)
5.  Human disease outcomes (GIDEON): case fatality rates, outbreak size, prevalence 5. Ungulate host ranges (IUCN)

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

### Data: Global Mammal Parasite Database

### Mammal pathogen data

#### read in GMPD and save

check out taxonomy dataset

``` r
source("GMPD.R")
```

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

#### Bacterial traits: determine correlations among variables in Brbic et al. 2016.

Note: commenting out corrplot for now because it throws an error.

``` r
# source("corrplot_bacteria.R")
```

#### Bacterial traits: Read in data in Barberan et al. 2017 and save as p2.Rdata

Source: <https://figshare.com/articles/International_Journal_of_Systematic_and_Evolutionary_Microbiology_IJSEM_phenotypic_database/4272392>

``` r
source("read_data_pathogen_traits2.R")
```

#### Bacterial traits: correct taxonomy in Barberan et al. 2017 data

output file as species2.csv. make field Organism\_name. Some species have multiple entries. There is no explanation about this (<https://figshare.com/articles/International_Journal_of_Systematic_and_Evolutionary_Microbiology_IJSEM_phenotypic_database/4272392>). Executive decision: filter data so that there are only records for species with one line of data, because these are data in which we have most confidence.
After running taxonomy\_ncbi\_2.R, go to NCBI website, upload species.csv, and choose option to save to file from website. Save file to working directory.

``` r
source("taxonomy_ncbi_2.R")
```

    ## [1] 205

Read back in file outputted from NCBI website ("tax\_report2.txt"). Merge tax\_report.txt with p2 (original Barberan et al. data), with new field "preferred.name"

``` r
source("taxonomy_ncbi_out2.R")
```

    ## [1] 7
    ## [1] "number with non-preferred species name"
    ## [1] 857
    ## [1] "check"
    ## [1] TRUE

#### Bacterial traits: determine overlap between species in GMPD and each trait database

``` r
source("GMPD_pathogen.R")
```

    ## [1] "species in common GMPD and bacterial traits in Brbic et al. 2016"
    ## [1] 141
    ##   [1] "Bacillus anthracis"                
    ##   [2] "Brucella abortus"                  
    ##   [3] "Ehrlichia ruminantium"             
    ##   [4] "Escherichia coli"                  
    ##   [5] "Leptospira interrogans"            
    ##   [6] "Anaplasma phagocytophilum"         
    ##   [7] "Brucella suis"                     
    ##   [8] "Erysipelothrix rhusiopathiae"      
    ##   [9] "Fusobacterium necrophorum"         
    ##  [10] "Klebsiella pneumoniae"             
    ##  [11] "Mycobacterium avium"               
    ##  [12] "Pasteurella multocida"             
    ##  [13] "Chlamydia psittaci"                
    ##  [14] "Mannheimia haemolytica"            
    ##  [15] "Mycobacterium tuberculosis"        
    ##  [16] "Anaplasma marginale"               
    ##  [17] "Brucella melitensis"               
    ##  [18] "Coxiella burnetii"                 
    ##  [19] "Enterococcus casseliflavus"        
    ##  [20] "Enterococcus faecalis"             
    ##  [21] "Enterococcus faecium"              
    ##  [22] "Enterococcus hirae"                
    ##  [23] "Enterococcus mundtii"              
    ##  [24] "Moraxella bovis"                   
    ##  [25] "Pantoea agglomerans"               
    ##  [26] "Proteus mirabilis"                 
    ##  [27] "Staphylococcus aureus"             
    ##  [28] "Staphylococcus epidermidis"        
    ##  [29] "Corynebacterium renale"            
    ##  [30] "Mycoplasma bovis"                  
    ##  [31] "Pseudomonas aeruginosa"            
    ##  [32] "Ehrlichia chaffeensis"             
    ##  [33] "Dichelobacter nodosus"             
    ##  [34] "Mycoplasma conjunctivae"           
    ##  [35] "Mycoplasma agalactiae"             
    ##  [36] "Mycoplasma mycoides"               
    ##  [37] "Francisella tularensis"            
    ##  [38] "Rickettsia helvetica"              
    ##  [39] "Yersinia enterocolitica"           
    ##  [40] "Lawsonia intracellularis"          
    ##  [41] "Rickettsia slovaca"                
    ##  [42] "Salmonella enterica"               
    ##  [43] "Yersinia pseudotuberculosis"       
    ##  [44] "Bacillus thuringiensis"            
    ##  [45] "Ehrlichia muris"                   
    ##  [46] "Listeria monocytogenes"            
    ##  [47] "Dermatophilus congolensis"         
    ##  [48] "Yersinia pestis"                   
    ##  [49] "Clostridium perfringens"           
    ##  [50] "Streptococcus dysgalactiae"        
    ##  [51] "Acinetobacter lwoffii"             
    ##  [52] "Bacteroides pyogenes"              
    ##  [53] "Bibersteinia trehalosi"            
    ##  [54] "Cupriavidus pauculus"              
    ##  [55] "Mycoplasma ovipneumoniae"          
    ##  [56] "Neisseria elongata"                
    ##  [57] "Pseudomonas fluorescens"           
    ##  [58] "Staphylococcus cohnii"             
    ##  [59] "Staphylococcus warneri"            
    ##  [60] "Staphylococcus xylosus"            
    ##  [61] "Streptococcus mitis"               
    ##  [62] "Streptococcus mutans"              
    ##  [63] "Streptococcus sanguinis"           
    ##  [64] "Streptococcus suis"                
    ##  [65] "Bartonella henselae"               
    ##  [66] "Leptospira borgpetersenii"         
    ##  [67] "Bordetella bronchiseptica"         
    ##  [68] "Corynebacterium pseudotuberculosis"
    ##  [69] "Gemella haemolysans"               
    ##  [70] "Gemella morbillorum"               
    ##  [71] "Moraxella catarrhalis"             
    ##  [72] "Nocardia asteroides"               
    ##  [73] "Photobacterium damselae"           
    ##  [74] "Pseudomonas alcaligenes"           
    ##  [75] "Streptococcus agalactiae"          
    ##  [76] "Streptococcus equi"                
    ##  [77] "Streptococcus uberis"              
    ##  [78] "Edwardsiella tarda"                
    ##  [79] "Streptococcus canis"               
    ##  [80] "Hafnia alvei"                      
    ##  [81] "Ehrlichia canis"                   
    ##  [82] "Rickettsia conorii"                
    ##  [83] "Bartonella vinsonii"               
    ##  [84] "Mycobacterium intracellulare"      
    ##  [85] "Neorickettsia risticii"            
    ##  [86] "Rickettsia rickettsii"             
    ##  [87] "Brucella pinnipedialis"            
    ##  [88] "Clostridioides difficile"          
    ##  [89] "Plesiomonas shigelloides"          
    ##  [90] "Vibrio alginolyticus"              
    ##  [91] "Vibrio cholerae"                   
    ##  [92] "Vibrio fluvialis"                  
    ##  [93] "Vibrio parahaemolyticus"           
    ##  [94] "Mycoplasma haemofelis"             
    ##  [95] "Aeromonas hydrophila"              
    ##  [96] "Agrobacterium tumefaciens"         
    ##  [97] "Burkholderia cepacia"              
    ##  [98] "Morganella morganii"               
    ##  [99] "Paenibacillus alvei"               
    ## [100] "Rahnella aquatilis"                
    ## [101] "Streptococcus porcinus"            
    ## [102] "Mycoplasma fermentans"             
    ## [103] "Acinetobacter baumannii"           
    ## [104] "Citrobacter koseri"                
    ## [105] "Enterobacter cloacae"              
    ## [106] "Listeria ivanovii"                 
    ## [107] "Providencia stuartii"              
    ## [108] "Pseudomonas putida"                
    ## [109] "Serratia liquefaciens"             
    ## [110] "Campylobacter jejuni"              
    ## [111] "Corynebacterium ulcerans"          
    ## [112] "Citrobacter freundii"              
    ## [113] "Serratia marcescens"               
    ## [114] "Bartonella clarridgeiae"           
    ## [115] "Clostridium botulinum"             
    ## [116] "Clostridium colicanis"             
    ## [117] "Brucella microti"                  
    ## [118] "Ochrobactrum anthropi"             
    ## [119] "Acinetobacter calcoaceticus"       
    ## [120] "Arcanobacterium haemolyticum"      
    ## [121] "Bacillus coagulans"                
    ## [122] "Brevibacillus brevis"              
    ## [123] "Lysinibacillus sphaericus"         
    ## [124] "Micrococcus luteus"                
    ## [125] "Neisseria flavescens"              
    ## [126] "Neisseria mucosa"                  
    ## [127] "Pseudomonas stutzeri"              
    ## [128] "Shewanella putrefaciens"           
    ## [129] "Stenotrophomonas maltophilia"      
    ## [130] "Treponema pallidum"                
    ## [131] "Bacillus cereus"                   
    ## [132] "Mycoplasma pneumoniae"             
    ## [133] "Shigella boydii"                   
    ## [134] "Shigella flexneri"                 
    ## [135] "Shigella sonnei"                   
    ## [136] "Streptococcus pneumoniae"          
    ## [137] "Achromobacter xylosoxidans"        
    ## [138] "Mycoplasma hominis"                
    ## [139] "Streptococcus pyogenes"            
    ## [140] "Bacillus mycoides"                 
    ## [141] "Borrelia recurrentis"              
    ## [1] "species in common GMPD and bacterial traits in Barberan et al. 2017"
    ## [1] 2
    ## [1] "Brucella pinnipedialis" "Brucella microti"

#### find species in common between Brbic et al. (p1) and Barberan et al.

``` r
source("common_p.R")
```

    ## [1] "number in common"
    ## [1] 362

### Data: match diseases to bacterial pathogens

#### Read in GIDEON data from scrape

Save as GIDEON.Rdata, including unique diseases associated with each mammal taxon

``` r
source("GIDEON_read.R")
```

#### GIDEON data: subset to include only ungulates

Subset ungulate GIDEON data to then match up diseases with pathogens. This way of doing the subset is wrong because it assumes ungulates must be in GMPD; there could be records in GIDEON for ungulate / disease for which associated pathogen has not been recorded in GMPD. Need to find dataset with corrected names for all ungulates.

``` r
source("GIDEON_subset.R")
```

    ## [1] 2256    2
    ## [1] 293422      3
    ## [1] 1091    3
    ## [1] 334   3
