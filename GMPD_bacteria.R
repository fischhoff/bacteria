path = "GMPD/"
gmpd_taxonomy = read.csv(paste0(path, "GMPD_parasite_taxonomy_2016-02-06.csv"))
names(gmpd_taxonomy)
# summary(GMPD_parasite_taxonomy_2016_02_06)
gmpd_taxonomy_bacteria = subset(gmpd_taxonomy, ParType == "Bacteria")
dim(gmpd_taxonomy_bacteria)
save(gmpd_taxonomy_bacteria,
     file = "gmpd_taxonomy_bacteria.Rdata")