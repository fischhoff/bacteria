
wd = getwd()
setwd(wd)
print(file.exists("../DATA/GMPD_main_2017-02-06.csv"))#two dots and slash to go up two levels
G = read.csv("../DATA/GMPD_main_2017-02-06.csv")

#don't need parasite taxonomy because going to use NCBI
# G_taxonomy = read.csv("../DATA/GMPD_parasite_taxonomy_2016-02-06.csv")

G_taxonomy = read.csv("../DATA/GMPD_parasite_taxonomy_2016-02-06.csv")
#subset G_taxonomy to require binomial name
G_taxonomy = subset(G_taxonomy, HasBinomialName=="yes")
G_traits = read.csv("../DATA/GMPD_parasite_traits_2016-12-01.csv")
dim(G)
# G = merge(G, G_traits)
dim(G)#this is smaller, so G_traits is not completely filled in, need to use method  other than merge

true_false = G$ParasiteCorrectedName %in% G_traits$ParasiteCorrectedName
inds_true = which(true_false == TRUE)
inds_not = which(true_false == FALSE)
G = G[,c("HostCorrectedName",
         "HostOrder",
         "ParasiteCorrectedName")]
G_not = G[inds_not,]
G_to_merge =G[inds_true,]
G_to_merge = merge(G_to_merge, G_traits)

G_not$close=NA
G_not$nonclose=NA
G_not$vector=NA
G_not$intermediate=NA
G_not$ParasiteTraitsCitation=NA

G_combined = rbind(G_to_merge, G_not)
dim(G_combined)
G_taxonomy = G_taxonomy[,c("ParType",
                           "ParasiteCorrectedName")]

out = NULL
up = unique(G_combined$ParasiteCorrectedName)
for (a in 1:length(up)){#for each ParasiteCorrectedName
  tmp = subset(G_combined, ParasiteCorrectedName == up[a])
  tmp_tax = subset(G_taxonomy, ParasiteCorrectedName == up[a])
  dim_tax = dim(tmp_tax)[1]
  if (dim_tax>0){
    tmp$ParType = tmp_tax$ParType[1]
    out = rbind(out, tmp)
  }
}
G_combined = out
dim(G_combined)
df= G_combined

#get only bacteria
df = subset(df, ParType == "Bacteria")

df = rename(df, pathogen = ParasiteCorrectedName)
df = subset(df, pathogen !="")

dim = dim(df)[1]
for (a in 1:dim){
  id = name2taxid(df$pathogen[a], out_type = "summary")
  taxid = id$tax_id[1]
  df$tax_id[a] = taxid
}
G = df
print(dim(G)[1])
save(G, file = "../DATA/PROCESSED/G.Rdata")