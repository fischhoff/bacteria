#read in parasite traits
path = "GMPD/"
gmpd_traits = read.csv(paste0(path,"GMPD_parasite_traits_2016-12-01.csv"))

load("gmpd_taxonomy_bacteria.Rdata")
dim(gmpd_taxonomy_bacteria)[1]
dim(gmpd_traits)
intersect(names(gmpd_traits),names(gmpd_taxonomy_bacteria))
gmpd = merge(gmpd_traits, gmpd_taxonomy_bacteria)
save(gmpd,file = "gmpd.Rdata")