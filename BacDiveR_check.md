BacDiveR\_check
================
Ilya
1/31/2019

#### install packages

``` r
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE,repos = "http://cran.us.r-project.org")    
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("data.table", "glue", "rlang","tibble", "tidyselect", "dplyr", "ggplot2", "tidyr"
                    )
#"rstan"
for (package in neededPackages){pkgTest(package)}
```

    ## 
    ## Attaching package: 'rlang'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     :=

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

``` r
D = readRDS("Data for Bacillus halotolerans.rds")
#find out field names
names(D)
```

    ## [1] "bacdive_id" "section"    "subsection" "field"      "value"

``` r
#look at unique fields
unique(D$field)
```

    ##   [1] "species_epithet"             "subspecies_epithet"         
    ##   [3] "is_type_strain"              "domain"                     
    ##   [5] "phylum"                      "class"                      
    ##   [7] "ordo"                        "family"                     
    ##   [9] "status_fam"                  "genus"                      
    ##  [11] "status_gen"                  "species"                    
    ##  [13] "status_spec"                 "full_scientific_name"       
    ##  [15] "ID_reference"                "designation"                
    ##  [17] "pnu_synonym"                 "medium_name1"               
    ##  [19] "medium_name2"                "medium_growth1"             
    ##  [21] "medium_growth2"              "media_link1"                
    ##  [23] "media_link2"                 "medium_composition1"        
    ##  [25] "medium_composition2"         "ID_reference1"              
    ##  [27] "ID_reference2"               "ability1"                   
    ##  [29] "ability2"                    "test_type1"                 
    ##  [31] "test_type2"                  "temp1"                      
    ##  [33] "temp2"                       "temperature_range1"         
    ##  [35] "temperature_range2"          "pathogenicity_human"        
    ##  [37] "pathogenicity_animal"        "pathogenicity_plant"        
    ##  [39] "biosafety_level"             "biosafety_level_comment"    
    ##  [41] "DB_sequence"                 "Sequence_accession_title"   
    ##  [43] "seq_acc_num"                 "sequence_length"            
    ##  [45] "NCBI_tax_ID"                 "NCBI_Gi"                    
    ##  [47] "text_mined"                  "history"                    
    ##  [49] "strain_number"               "straininfo_strainnumber1"   
    ##  [51] "straininfo_strainnumber2"    "straininfo_strainnumber3"   
    ##  [53] "straininfo_strainnumber4"    "URL1"                       
    ##  [55] "URL2"                        "URL3"                       
    ##  [57] "URL4"                        "ID_reference3"              
    ##  [59] "ID_reference4"               NA                           
    ##  [61] "compound_name"               "excreted"                   
    ##  [63] "DB_sequence1"                "DB_sequence2"               
    ##  [65] "DB_sequence3"                "DB_sequence4"               
    ##  [67] "DB_sequence5"                "DB_sequence6"               
    ##  [69] "DB_sequence7"                "DB_sequence8"               
    ##  [71] "DB_sequence9"                "DB_sequence10"              
    ##  [73] "DB_sequence11"               "DB_sequence12"              
    ##  [75] "DB_sequence13"               "DB_sequence14"              
    ##  [77] "DB_sequence15"               "DB_sequence16"              
    ##  [79] "DB_sequence17"               "DB_sequence18"              
    ##  [81] "DB_sequence19"               "DB_sequence20"              
    ##  [83] "DB_sequence21"               "DB_sequence22"              
    ##  [85] "DB_sequence23"               "DB_sequence24"              
    ##  [87] "DB_sequence25"               "DB_sequence26"              
    ##  [89] "DB_sequence27"               "DB_sequence28"              
    ##  [91] "DB_sequence29"               "DB_sequence30"              
    ##  [93] "DB_sequence31"               "DB_sequence32"              
    ##  [95] "DB_sequence33"               "DB_sequence34"              
    ##  [97] "DB_sequence35"               "DB_sequence36"              
    ##  [99] "DB_sequence37"               "DB_sequence38"              
    ## [101] "DB_sequence39"               "DB_sequence40"              
    ## [103] "DB_sequence41"               "DB_sequence42"              
    ## [105] "DB_sequence43"               "DB_sequence44"              
    ## [107] "DB_sequence45"               "DB_sequence46"              
    ## [109] "DB_sequence47"               "DB_sequence48"              
    ## [111] "DB_sequence49"               "DB_sequence50"              
    ## [113] "DB_sequence51"               "DB_sequence52"              
    ## [115] "DB_sequence53"               "DB_sequence54"              
    ## [117] "DB_sequence55"               "DB_sequence56"              
    ## [119] "DB_sequence57"               "DB_sequence58"              
    ## [121] "DB_sequence59"               "DB_sequence60"              
    ## [123] "DB_sequence61"               "DB_sequence62"              
    ## [125] "DB_sequence63"               "DB_sequence64"              
    ## [127] "DB_sequence65"               "DB_sequence66"              
    ## [129] "DB_sequence67"               "DB_sequence68"              
    ## [131] "DB_sequence69"               "DB_sequence70"              
    ## [133] "DB_sequence71"               "DB_sequence72"              
    ## [135] "DB_sequence73"               "DB_sequence74"              
    ## [137] "DB_sequence75"               "DB_sequence76"              
    ## [139] "DB_sequence77"               "DB_sequence78"              
    ## [141] "DB_sequence79"               "DB_sequence80"              
    ## [143] "DB_sequence81"               "DB_sequence82"              
    ## [145] "DB_sequence83"               "DB_sequence84"              
    ## [147] "Sequence_accession_title1"   "Sequence_accession_title2"  
    ## [149] "Sequence_accession_title3"   "Sequence_accession_title4"  
    ## [151] "Sequence_accession_title5"   "Sequence_accession_title6"  
    ## [153] "Sequence_accession_title7"   "Sequence_accession_title8"  
    ## [155] "Sequence_accession_title9"   "Sequence_accession_title10" 
    ## [157] "Sequence_accession_title11"  "Sequence_accession_title12" 
    ## [159] "Sequence_accession_title13"  "Sequence_accession_title14" 
    ## [161] "Sequence_accession_title15"  "Sequence_accession_title16" 
    ## [163] "Sequence_accession_title17"  "Sequence_accession_title18" 
    ## [165] "Sequence_accession_title19"  "Sequence_accession_title20" 
    ## [167] "Sequence_accession_title21"  "Sequence_accession_title22" 
    ## [169] "Sequence_accession_title23"  "Sequence_accession_title24" 
    ## [171] "Sequence_accession_title25"  "Sequence_accession_title26" 
    ## [173] "Sequence_accession_title27"  "Sequence_accession_title28" 
    ## [175] "Sequence_accession_title29"  "Sequence_accession_title30" 
    ## [177] "Sequence_accession_title31"  "Sequence_accession_title32" 
    ## [179] "Sequence_accession_title33"  "Sequence_accession_title34" 
    ## [181] "Sequence_accession_title35"  "Sequence_accession_title36" 
    ## [183] "Sequence_accession_title37"  "Sequence_accession_title38" 
    ## [185] "Sequence_accession_title39"  "Sequence_accession_title40" 
    ## [187] "Sequence_accession_title41"  "Sequence_accession_title42" 
    ## [189] "Sequence_accession_title43"  "Sequence_accession_title44" 
    ## [191] "Sequence_accession_title45"  "Sequence_accession_title46" 
    ## [193] "Sequence_accession_title47"  "Sequence_accession_title48" 
    ## [195] "Sequence_accession_title49"  "Sequence_accession_title50" 
    ## [197] "Sequence_accession_title51"  "Sequence_accession_title52" 
    ## [199] "Sequence_accession_title53"  "Sequence_accession_title54" 
    ## [201] "Sequence_accession_title55"  "Sequence_accession_title56" 
    ## [203] "Sequence_accession_title57"  "Sequence_accession_title58" 
    ## [205] "Sequence_accession_title59"  "Sequence_accession_title60" 
    ## [207] "Sequence_accession_title61"  "Sequence_accession_title62" 
    ## [209] "Sequence_accession_title63"  "Sequence_accession_title64" 
    ## [211] "Sequence_accession_title65"  "Sequence_accession_title66" 
    ## [213] "Sequence_accession_title67"  "Sequence_accession_title68" 
    ## [215] "Sequence_accession_title69"  "Sequence_accession_title70" 
    ## [217] "Sequence_accession_title71"  "Sequence_accession_title72" 
    ## [219] "Sequence_accession_title73"  "Sequence_accession_title74" 
    ## [221] "Sequence_accession_title75"  "Sequence_accession_title76" 
    ## [223] "Sequence_accession_title77"  "Sequence_accession_title78" 
    ## [225] "Sequence_accession_title79"  "Sequence_accession_title80" 
    ## [227] "Sequence_accession_title81"  "Sequence_accession_title82" 
    ## [229] "Sequence_accession_title83"  "Sequence_accession_title84" 
    ## [231] "seq_acc_num1"                "seq_acc_num2"               
    ## [233] "seq_acc_num3"                "seq_acc_num4"               
    ## [235] "seq_acc_num5"                "seq_acc_num6"               
    ## [237] "seq_acc_num7"                "seq_acc_num8"               
    ## [239] "seq_acc_num9"                "seq_acc_num10"              
    ## [241] "seq_acc_num11"               "seq_acc_num12"              
    ## [243] "seq_acc_num13"               "seq_acc_num14"              
    ## [245] "seq_acc_num15"               "seq_acc_num16"              
    ## [247] "seq_acc_num17"               "seq_acc_num18"              
    ## [249] "seq_acc_num19"               "seq_acc_num20"              
    ## [251] "seq_acc_num21"               "seq_acc_num22"              
    ## [253] "seq_acc_num23"               "seq_acc_num24"              
    ## [255] "seq_acc_num25"               "seq_acc_num26"              
    ## [257] "seq_acc_num27"               "seq_acc_num28"              
    ## [259] "seq_acc_num29"               "seq_acc_num30"              
    ## [261] "seq_acc_num31"               "seq_acc_num32"              
    ## [263] "seq_acc_num33"               "seq_acc_num34"              
    ## [265] "seq_acc_num35"               "seq_acc_num36"              
    ## [267] "seq_acc_num37"               "seq_acc_num38"              
    ## [269] "seq_acc_num39"               "seq_acc_num40"              
    ## [271] "seq_acc_num41"               "seq_acc_num42"              
    ## [273] "seq_acc_num43"               "seq_acc_num44"              
    ## [275] "seq_acc_num45"               "seq_acc_num46"              
    ## [277] "seq_acc_num47"               "seq_acc_num48"              
    ## [279] "seq_acc_num49"               "seq_acc_num50"              
    ## [281] "seq_acc_num51"               "seq_acc_num52"              
    ## [283] "seq_acc_num53"               "seq_acc_num54"              
    ## [285] "seq_acc_num55"               "seq_acc_num56"              
    ## [287] "seq_acc_num57"               "seq_acc_num58"              
    ## [289] "seq_acc_num59"               "seq_acc_num60"              
    ## [291] "seq_acc_num61"               "seq_acc_num62"              
    ## [293] "seq_acc_num63"               "seq_acc_num64"              
    ## [295] "seq_acc_num65"               "seq_acc_num66"              
    ## [297] "seq_acc_num67"               "seq_acc_num68"              
    ## [299] "seq_acc_num69"               "seq_acc_num70"              
    ## [301] "seq_acc_num71"               "seq_acc_num72"              
    ## [303] "seq_acc_num73"               "seq_acc_num74"              
    ## [305] "seq_acc_num75"               "seq_acc_num76"              
    ## [307] "seq_acc_num77"               "seq_acc_num78"              
    ## [309] "seq_acc_num79"               "seq_acc_num80"              
    ## [311] "seq_acc_num81"               "seq_acc_num82"              
    ## [313] "seq_acc_num83"               "seq_acc_num84"              
    ## [315] "sequence_length1"            "sequence_length2"           
    ## [317] "sequence_length3"            "sequence_length4"           
    ## [319] "sequence_length5"            "sequence_length6"           
    ## [321] "sequence_length7"            "sequence_length8"           
    ## [323] "sequence_length9"            "sequence_length10"          
    ## [325] "sequence_length11"           "sequence_length12"          
    ## [327] "sequence_length13"           "sequence_length14"          
    ## [329] "sequence_length15"           "sequence_length16"          
    ## [331] "sequence_length17"           "sequence_length18"          
    ## [333] "sequence_length19"           "sequence_length20"          
    ## [335] "sequence_length21"           "sequence_length22"          
    ## [337] "sequence_length23"           "sequence_length24"          
    ## [339] "sequence_length25"           "sequence_length26"          
    ## [341] "sequence_length27"           "sequence_length28"          
    ## [343] "sequence_length29"           "sequence_length30"          
    ## [345] "sequence_length31"           "sequence_length32"          
    ## [347] "sequence_length33"           "sequence_length34"          
    ## [349] "sequence_length35"           "sequence_length36"          
    ## [351] "sequence_length37"           "sequence_length38"          
    ## [353] "sequence_length39"           "sequence_length40"          
    ## [355] "sequence_length41"           "sequence_length42"          
    ## [357] "sequence_length43"           "sequence_length44"          
    ## [359] "sequence_length45"           "sequence_length46"          
    ## [361] "sequence_length47"           "sequence_length48"          
    ## [363] "sequence_length49"           "sequence_length50"          
    ## [365] "sequence_length51"           "sequence_length52"          
    ## [367] "sequence_length53"           "sequence_length54"          
    ## [369] "sequence_length55"           "sequence_length56"          
    ## [371] "sequence_length57"           "sequence_length58"          
    ## [373] "sequence_length59"           "sequence_length60"          
    ## [375] "sequence_length61"           "sequence_length62"          
    ## [377] "sequence_length63"           "sequence_length64"          
    ## [379] "sequence_length65"           "sequence_length66"          
    ## [381] "sequence_length67"           "sequence_length68"          
    ## [383] "sequence_length69"           "sequence_length70"          
    ## [385] "sequence_length71"           "sequence_length72"          
    ## [387] "sequence_length73"           "sequence_length74"          
    ## [389] "sequence_length75"           "sequence_length76"          
    ## [391] "sequence_length77"           "sequence_length78"          
    ## [393] "sequence_length79"           "sequence_length80"          
    ## [395] "sequence_length81"           "sequence_length82"          
    ## [397] "sequence_length83"           "sequence_length84"          
    ## [399] "NCBI_tax_ID1"                "NCBI_tax_ID2"               
    ## [401] "NCBI_tax_ID3"                "NCBI_tax_ID4"               
    ## [403] "NCBI_tax_ID5"                "NCBI_tax_ID6"               
    ## [405] "NCBI_tax_ID7"                "NCBI_tax_ID8"               
    ## [407] "NCBI_tax_ID9"                "NCBI_tax_ID10"              
    ## [409] "NCBI_tax_ID11"               "NCBI_tax_ID12"              
    ## [411] "NCBI_tax_ID13"               "NCBI_tax_ID14"              
    ## [413] "NCBI_tax_ID15"               "NCBI_tax_ID16"              
    ## [415] "NCBI_tax_ID17"               "NCBI_tax_ID18"              
    ## [417] "NCBI_tax_ID19"               "NCBI_tax_ID20"              
    ## [419] "NCBI_tax_ID21"               "NCBI_tax_ID22"              
    ## [421] "NCBI_tax_ID23"               "NCBI_tax_ID24"              
    ## [423] "NCBI_tax_ID25"               "NCBI_tax_ID26"              
    ## [425] "NCBI_tax_ID27"               "NCBI_tax_ID28"              
    ## [427] "NCBI_tax_ID29"               "NCBI_tax_ID30"              
    ## [429] "NCBI_tax_ID31"               "NCBI_tax_ID32"              
    ## [431] "NCBI_tax_ID33"               "NCBI_tax_ID34"              
    ## [433] "NCBI_tax_ID35"               "NCBI_tax_ID36"              
    ## [435] "NCBI_tax_ID37"               "NCBI_tax_ID38"              
    ## [437] "NCBI_tax_ID39"               "NCBI_tax_ID40"              
    ## [439] "NCBI_tax_ID41"               "NCBI_tax_ID42"              
    ## [441] "NCBI_tax_ID43"               "NCBI_tax_ID44"              
    ## [443] "NCBI_tax_ID45"               "NCBI_tax_ID46"              
    ## [445] "NCBI_tax_ID47"               "NCBI_tax_ID48"              
    ## [447] "NCBI_tax_ID49"               "NCBI_tax_ID50"              
    ## [449] "NCBI_tax_ID51"               "NCBI_tax_ID52"              
    ## [451] "NCBI_tax_ID53"               "NCBI_tax_ID54"              
    ## [453] "NCBI_tax_ID55"               "NCBI_tax_ID56"              
    ## [455] "NCBI_tax_ID57"               "NCBI_tax_ID58"              
    ## [457] "NCBI_tax_ID59"               "NCBI_tax_ID60"              
    ## [459] "NCBI_tax_ID61"               "NCBI_tax_ID62"              
    ## [461] "NCBI_tax_ID63"               "NCBI_tax_ID64"              
    ## [463] "NCBI_tax_ID65"               "NCBI_tax_ID66"              
    ## [465] "NCBI_tax_ID67"               "NCBI_tax_ID68"              
    ## [467] "NCBI_tax_ID69"               "NCBI_tax_ID70"              
    ## [469] "NCBI_tax_ID71"               "NCBI_tax_ID72"              
    ## [471] "NCBI_tax_ID73"               "NCBI_tax_ID74"              
    ## [473] "NCBI_tax_ID75"               "NCBI_tax_ID76"              
    ## [475] "NCBI_tax_ID77"               "NCBI_tax_ID78"              
    ## [477] "NCBI_tax_ID79"               "NCBI_tax_ID80"              
    ## [479] "NCBI_tax_ID81"               "NCBI_tax_ID82"              
    ## [481] "NCBI_tax_ID83"               "NCBI_tax_ID84"              
    ## [483] "NCBI_Gi1"                    "NCBI_Gi2"                   
    ## [485] "NCBI_Gi3"                    "NCBI_Gi4"                   
    ## [487] "NCBI_Gi5"                    "NCBI_Gi6"                   
    ## [489] "NCBI_Gi7"                    "NCBI_Gi8"                   
    ## [491] "NCBI_Gi9"                    "NCBI_Gi10"                  
    ## [493] "NCBI_Gi11"                   "NCBI_Gi12"                  
    ## [495] "NCBI_Gi13"                   "NCBI_Gi14"                  
    ## [497] "NCBI_Gi15"                   "NCBI_Gi16"                  
    ## [499] "NCBI_Gi17"                   "NCBI_Gi18"                  
    ## [501] "NCBI_Gi19"                   "NCBI_Gi20"                  
    ## [503] "NCBI_Gi21"                   "NCBI_Gi22"                  
    ## [505] "NCBI_Gi23"                   "NCBI_Gi24"                  
    ## [507] "NCBI_Gi25"                   "NCBI_Gi26"                  
    ## [509] "NCBI_Gi27"                   "NCBI_Gi28"                  
    ## [511] "NCBI_Gi29"                   "NCBI_Gi30"                  
    ## [513] "NCBI_Gi31"                   "NCBI_Gi32"                  
    ## [515] "NCBI_Gi33"                   "NCBI_Gi34"                  
    ## [517] "NCBI_Gi35"                   "NCBI_Gi36"                  
    ## [519] "NCBI_Gi37"                   "NCBI_Gi38"                  
    ## [521] "NCBI_Gi39"                   "NCBI_Gi40"                  
    ## [523] "NCBI_Gi41"                   "NCBI_Gi42"                  
    ## [525] "NCBI_Gi43"                   "NCBI_Gi44"                  
    ## [527] "NCBI_Gi45"                   "NCBI_Gi46"                  
    ## [529] "NCBI_Gi47"                   "NCBI_Gi48"                  
    ## [531] "NCBI_Gi49"                   "NCBI_Gi50"                  
    ## [533] "NCBI_Gi51"                   "NCBI_Gi52"                  
    ## [535] "NCBI_Gi53"                   "NCBI_Gi54"                  
    ## [537] "NCBI_Gi55"                   "NCBI_Gi56"                  
    ## [539] "NCBI_Gi57"                   "NCBI_Gi58"                  
    ## [541] "NCBI_Gi59"                   "NCBI_Gi60"                  
    ## [543] "NCBI_Gi61"                   "NCBI_Gi62"                  
    ## [545] "NCBI_Gi63"                   "NCBI_Gi64"                  
    ## [547] "NCBI_Gi65"                   "NCBI_Gi66"                  
    ## [549] "NCBI_Gi67"                   "NCBI_Gi68"                  
    ## [551] "NCBI_Gi69"                   "NCBI_Gi70"                  
    ## [553] "NCBI_Gi71"                   "NCBI_Gi72"                  
    ## [555] "NCBI_Gi73"                   "NCBI_Gi74"                  
    ## [557] "NCBI_Gi75"                   "NCBI_Gi76"                  
    ## [559] "NCBI_Gi77"                   "NCBI_Gi78"                  
    ## [561] "NCBI_Gi79"                   "NCBI_Gi80"                  
    ## [563] "NCBI_Gi81"                   "NCBI_Gi82"                  
    ## [565] "NCBI_Gi83"                   "NCBI_Gi84"                  
    ## [567] "ID_reference5"               "ID_reference6"              
    ## [569] "ID_reference7"               "ID_reference8"              
    ## [571] "ID_reference9"               "ID_reference10"             
    ## [573] "ID_reference11"              "ID_reference12"             
    ## [575] "ID_reference13"              "ID_reference14"             
    ## [577] "ID_reference15"              "ID_reference16"             
    ## [579] "ID_reference17"              "ID_reference18"             
    ## [581] "ID_reference19"              "ID_reference20"             
    ## [583] "ID_reference21"              "ID_reference22"             
    ## [585] "ID_reference23"              "ID_reference24"             
    ## [587] "ID_reference25"              "ID_reference26"             
    ## [589] "ID_reference27"              "ID_reference28"             
    ## [591] "ID_reference29"              "ID_reference30"             
    ## [593] "ID_reference31"              "ID_reference32"             
    ## [595] "ID_reference33"              "ID_reference34"             
    ## [597] "ID_reference35"              "ID_reference36"             
    ## [599] "ID_reference37"              "ID_reference38"             
    ## [601] "ID_reference39"              "ID_reference40"             
    ## [603] "ID_reference41"              "ID_reference42"             
    ## [605] "ID_reference43"              "ID_reference44"             
    ## [607] "ID_reference45"              "ID_reference46"             
    ## [609] "ID_reference47"              "ID_reference48"             
    ## [611] "ID_reference49"              "ID_reference50"             
    ## [613] "ID_reference51"              "ID_reference52"             
    ## [615] "ID_reference53"              "ID_reference54"             
    ## [617] "ID_reference55"              "ID_reference56"             
    ## [619] "ID_reference57"              "ID_reference58"             
    ## [621] "ID_reference59"              "ID_reference60"             
    ## [623] "ID_reference61"              "ID_reference62"             
    ## [625] "ID_reference63"              "ID_reference64"             
    ## [627] "ID_reference65"              "ID_reference66"             
    ## [629] "ID_reference67"              "ID_reference68"             
    ## [631] "ID_reference69"              "ID_reference70"             
    ## [633] "ID_reference71"              "ID_reference72"             
    ## [635] "ID_reference73"              "ID_reference74"             
    ## [637] "ID_reference75"              "ID_reference76"             
    ## [639] "ID_reference77"              "ID_reference78"             
    ## [641] "ID_reference79"              "ID_reference80"             
    ## [643] "ID_reference81"              "ID_reference82"             
    ## [645] "ID_reference83"              "ID_reference84"             
    ## [647] "text_mined1"                 "text_mined2"                
    ## [649] "text_mined3"                 "text_mined4"                
    ## [651] "text_mined5"                 "text_mined6"                
    ## [653] "text_mined7"                 "text_mined8"                
    ## [655] "text_mined9"                 "text_mined10"               
    ## [657] "text_mined11"                "text_mined12"               
    ## [659] "text_mined13"                "text_mined14"               
    ## [661] "text_mined15"                "text_mined16"               
    ## [663] "text_mined17"                "text_mined18"               
    ## [665] "text_mined19"                "text_mined20"               
    ## [667] "text_mined21"                "text_mined22"               
    ## [669] "text_mined23"                "text_mined24"               
    ## [671] "text_mined25"                "text_mined26"               
    ## [673] "text_mined27"                "text_mined28"               
    ## [675] "text_mined29"                "text_mined30"               
    ## [677] "text_mined31"                "text_mined32"               
    ## [679] "text_mined33"                "text_mined34"               
    ## [681] "text_mined35"                "text_mined36"               
    ## [683] "text_mined37"                "text_mined38"               
    ## [685] "text_mined39"                "text_mined40"               
    ## [687] "text_mined41"                "text_mined42"               
    ## [689] "text_mined43"                "text_mined44"               
    ## [691] "text_mined45"                "text_mined46"               
    ## [693] "text_mined47"                "text_mined48"               
    ## [695] "text_mined49"                "text_mined50"               
    ## [697] "text_mined51"                "text_mined52"               
    ## [699] "text_mined53"                "text_mined54"               
    ## [701] "text_mined55"                "text_mined56"               
    ## [703] "text_mined57"                "text_mined58"               
    ## [705] "text_mined59"                "text_mined60"               
    ## [707] "text_mined61"                "text_mined62"               
    ## [709] "text_mined63"                "text_mined64"               
    ## [711] "text_mined65"                "text_mined66"               
    ## [713] "text_mined67"                "text_mined68"               
    ## [715] "text_mined69"                "text_mined70"               
    ## [717] "text_mined71"                "text_mined72"               
    ## [719] "text_mined73"                "text_mined74"               
    ## [721] "text_mined75"                "text_mined76"               
    ## [723] "text_mined77"                "text_mined78"               
    ## [725] "text_mined79"                "text_mined80"               
    ## [727] "text_mined81"                "text_mined82"               
    ## [729] "text_mined83"                "text_mined84"               
    ## [731] "GC_content1"                 "GC_content2"                
    ## [733] "GC_method1"                  "GC_method2"                 
    ## [735] "straininfo_strainnumber5"    "straininfo_strainnumber6"   
    ## [737] "straininfo_strainnumber7"    "URL5"                       
    ## [739] "URL6"                        "URL7"                       
    ## [741] "medium_name"                 "medium_growth"              
    ## [743] "media_link"                  "medium_composition"         
    ## [745] "ability"                     "test_type"                  
    ## [747] "temp"                        "temperature_range"          
    ## [749] "sample_type"                 "sample_date"                
    ## [751] "geo_loc_name"                "country"                    
    ## [753] "continent"                   "latitude"                   
    ## [755] "longitude"                   "enrichment_cult_name"       
    ## [757] "enrichment_cult_composition" "enrichment_cult_duration"   
    ## [759] "enrichment_cult_temp"        "isolation_date"             
    ## [761] "procedure_origin"            "straininfo_strainnumber"    
    ## [763] "URL"

``` r
#look at unique sections
unique(D$section)
```

    ## [1] "taxonomy_name"                        
    ## [2] "culture_growth_condition"             
    ## [3] "application_interaction"              
    ## [4] "molecular_biology"                    
    ## [5] "strain_availability"                  
    ## [6] "references"                           
    ## [7] "morphology_physiology"                
    ## [8] "environment_sampling_isolation_source"

``` r
#get just taxonomy name
test = subset(D, section == "taxonomy_name")
#look at unique subsections
unique(test$subsection)
```

    ## [1] "strains_tax_PNU"      "strains"              "strains_synonyms_PNU"

``` r
#get just strains
test_strains = subset(test, subsection == "strains")
#see what fields are for strain
unique(test_strains$field)
```

    ##  [1] "domain"               "phylum"               "class"               
    ##  [4] "ordo"                 "family"               "genus"               
    ##  [7] "species"              "species_epithet"      "subspecies_epithet"  
    ## [10] "full_scientific_name" "designation"          "is_type_strain"      
    ## [13] "ID_reference"

``` r
#check is_type_strain
test_strain_type = subset(test_strains, field == "is_type_strain")
table(test_strain_type$bacdive_id,test_strain_type$value)
```

    ##         
    ##          FALSE TRUE
    ##   100619     0    0
    ##   100620     0    0
    ##   100621     0    0
    ##   11406      1    0
    ##   11407      0    1
    ##   11408      1    0
    ##   135999     1    0
    ##   138625     1    0
    ##   139422     1    0

#### find out what values should be there for one ID

``` r
id_1 = subset(D, bacdive_id == "100619")
unique(id_1$value)
```

    ##  [1] "laterosporus"                                                                                                                                                                                                                                               
    ##  [2] NA                                                                                                                                                                                                                                                           
    ##  [3] "Bacteria"                                                                                                                                                                                                                                                   
    ##  [4] "Firmicutes"                                                                                                                                                                                                                                                 
    ##  [5] "Bacilli"                                                                                                                                                                                                                                                    
    ##  [6] "Paenibacillaceae"                                                                                                                                                                                                                                           
    ##  [7] "Brevibacillus"                                                                                                                                                                                                                                              
    ##  [8] "gen. nov. (VP) "                                                                                                                                                                                                                                            
    ##  [9] "Brevibacillus laterosporus"                                                                                                                                                                                                                                 
    ## [10] "comb. nov. (VP)"                                                                                                                                                                                                                                            
    ## [11] "Brevibacillus laterosporus (Laubach 1916) Shida et al. 1996"                                                                                                                                                                                                
    ## [12] "20215"                                                                                                                                                                                                                                                      
    ## [13] "STI09656(IMET); 31/73"                                                                                                                                                                                                                                      
    ## [14] "20216"                                                                                                                                                                                                                                                      
    ## [15] "Bacillus laterosporus"                                                                                                                                                                                                                                      
    ## [16] "D.Gleim, M.Kracht, N.Weiss et. al.: Prokaryotic Nomenclature Up-to-date - compilation of all names of Bacteria and Archaea, validly published according to the Bacteriological Code since 1. Jan. 1980, and validly published nomenclatural changes since. "
    ## [17] "Curators of the HKI: Collection Description Leibniz-Institut für Naturstoff-Forschung und Infektionsbiologie e. V. Hans-Knöll-Institut (HKI). "

### try spread and dcast

``` r
#spread
D <- D[,c("bacdive_id",
          "field", 
          "value")]
# D$value[is.na(D$value)] <- -9999#this gets duplicate error, as does replacing with blank

D_uni=distinct(D)#make sure rows are unique
dim(D_uni)
```

    ## [1] 1120    3

``` r
#remove NA values
D_uni = D_uni[!is.na(D_uni$value),]
dim(D_uni)
```

    ## [1] 1009    3

``` r
D_uni = subset(D_uni, field != "ID_reference")
D_uni = subset(D_uni, field != "ID_reference1")
D_uni = subset(D_uni, field != "ID_reference2")

# Spread_1 = spread(D_uni, key = c(bacdive_id, field), value= value)
#this makes columns equal to ID and row equal to field name
# Spread_1 = spread(D_uni, key = bacdive_id, value = value)
Spread_2 = spread(D_uni, key = field, value = value)

write.csv(Spread_2,file = "bacdive_1_species_wide.csv")
#this also works
library(data.table)
# D_cast = dcast(setDT(D), bacdive_id ~ field, value.var = "value")
D_cast = dcast(setDT(D_uni), bacdive_id ~ field, value.var = "value")
```
