path = "GIDEON/"
#columns with mammalian host, disease, country
GIDEON = read.csv(paste0(path, "GIDEON scrape Feb2018.csv"))
dim(GIDEON)
tmp = data.frame(Spp="Abrothrix olivaceus",
                 Zoonosis = "Trypanosomiasis...American",
                 Country = "Chile")
names(GIDEON) = c("Spp","Zoonosis", "Country")
GIDEON = rbind(GIDEON, tmp)
#not using country for now
GIDEON= GIDEON[,c("Spp", "Zoonosis")]
GIDEON = unique(GIDEON)
# Cervus_elaphus_inds = which(GIDEON$Spp == "Cervus elaphus") 
# Cervus_elaphus_example = GIDEON[446,]
# Cervus_elaphus_example
save(GIDEON, file = "GIDEON.Rdata")
