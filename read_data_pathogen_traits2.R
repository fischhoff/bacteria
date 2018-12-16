path = "Barberan_et_al/"
#read table

ijsem<-read.delim(paste0(path,"IJSEM_pheno_db_v1.0.txt"), sep="\t", header=T, check.names=F, fill=T,
                  na.strings=c("NA", "", "Not indicated", " Not indicated","not indicated", "Not Indicated", "n/a", "N/A", "Na", "Not given", "not given","Not given for yeasts", "not indicated, available in the online version", "Not indicated for yeasts", "Not Stated", "Not described for yeasts", "Not determined", "Not determined for yeasts"))

#simplify column names
colnames(ijsem)<-c("Habitat", "Year", "DOI", "rRNA16S", "GC", "Oxygen",
                   "Length", "Width", "Motility", "Spore", "MetabAssays", "Genus", "Species", "Strain", "pH_optimum", "pH_range", "Temp_optimum", "Temp_range", "Salt_optimum", "Salt_range", "Pigment", "Shape", "Aggregation", "FirstPage", "CultureCollection", "CarbonSubstrate", "Genome", "Gram", "Subhabitat", "Biolog")

#clean Habitat column
levels(ijsem$Habitat)[levels(ijsem$Habitat)=="freshwater (river, lake, pond)"]<-"freshwater"
levels(ijsem$Habitat)[levels(ijsem$Habitat)=="freshwater sediment (river, lake, pond"]<-"freshwater sediment"

#clean Oxygen column
levels(ijsem$Oxygen)[levels(ijsem$Oxygen)=="aerobic"]<-"obligate aerobe"
levels(ijsem$Oxygen)[levels(ijsem$Oxygen)=="anerobic"]<-"obligate anerobe"
levels(ijsem$Oxygen)[levels(ijsem$Oxygen)=="microerophile"]<-"microaerophile"

#clean pH_optimum column
ijsem$pH_optimum<-as.character(ijsem$pH_optimum)
#this step splits the range values and takes the mean value
#values that are not numeric are transformed to NAs
ijsem$pH_optimum<-suppressWarnings(sapply(ijsem$pH_optimum, simplify=T, function(x){mean(as.numeric(unlist(strsplit(x, split="-", fixed=T))))}))
#remove pH values <0 and >10
ijsem$pH_optimum[ijsem$pH_optimum<0 | ijsem$pH_optimum>10]<-NA

#clean Temp_optimum column
ijsem$Temp_optimum<-as.character(ijsem$Temp_optimum)
#this step splits the range values and takes the mean value
#values that are not numeric are transformed to NAs
ijsem$Temp_optimum<-suppressWarnings(sapply(ijsem$Temp_optimum, simplify=T, function(x){mean(as.numeric(unlist(strsplit(x, split="-", fixed=T))))}))

#clean Salt_optimum column
ijsem$Salt_optimum<-as.character(ijsem$Salt_optimum)
#this step splits the range values and takes the mean value
#values that are not numeric are transformed to NAs
ijsem$Salt_optimum<-suppressWarnings(sapply(ijsem$Salt_optimum, simplify=T, function(x){mean(as.numeric(unlist(strsplit(x, split="-", fixed=T))))}))
#there are some formatting issues that should be solved

p2 = ijsem
save(p2, file = "p2.Rdata")
