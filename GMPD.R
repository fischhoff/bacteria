path = "GMPD/"
GMPD_main_2017_02_06 = read.csv(paste0(path,"GMPD_main_2017-02-06.csv"))
# summary(GMPD_main_2017_02_06)
dim(GMPD_main_2017_02_06)
GMPD <- GMPD_main_2017_02_06
save(GMPD, file = "GMPD.Rdata" )
#compare two data files
# GMPD_main_zeroPrevIncluded_2016_12_01 = read.csv(paste0(path, "GMPD_main_zeroPrevIncluded_2016-12-01.csv"))
# dim(GMPD_main_zeroPrevIncluded_2016_12_01)
# summary(GMPD_main_zeroPrevIncluded_2016_12_01)

GMPD_parasite_taxonomy_2016_02_06 = read.csv(paste0(path, "GMPD_parasite_taxonomy_2016-02-06.csv"))
# summary(GMPD_parasite_taxonomy_2016_02_06)

intersect(names(GMPD), names(GMPD_parasite_taxonomy_2016_02_06))
