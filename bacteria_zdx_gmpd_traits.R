#load data on hosts, zdx, parasites
load("df_parasite.Rdata")
df = df_parasite
names(df)
#names(df)[names(df)=="parasiteGMPD"]=
load("gmpd.Rdata")#load data on gmpd traits
cols = names(gmpd)
keep = setdiff(cols, c("ParasiteReportedName", "StrainorSubsps"))
gmpd = gmpd[,keep]
names(gmpd)[names(gmpd)=="ParasiteCorrectedName"]="parasiteGMPD"
dim(gmpd)
gmpd = unique(gmpd)
dim(gmpd)
#Note this will include only those pathogens that are in GMPD
dim(df_parasite)
#trim white space in case that is reason for duplicated rows
df_parasite$parasiteGMPD=trimws(df_parasite$parasiteGMPD)
gmpd$parasiteGMPD=trimws(gmpd$parasiteGMPD)
df_parasite$row = seq(1,dim(df_parasite)[1])
intersect(names(df_parasite),names(gmpd))
df_parasite_gmpd = merge(df_parasite, gmpd)
#check why records getting multiplied
test = subset(df_parasite_gmpd, row == 319)
df_parasite_gmpd=unique(df_parasite_gmpd)
dim(df_parasite_gmpd)
save(df_parasite_gmpd,
     file = "df_parasite_gmpd.Rdata")