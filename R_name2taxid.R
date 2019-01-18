
load("df_all.Rdata")
#Start the clock
ptm<-proc.time()
out =NULL#initialize output
df = subset(df_all, pathogen !="")
df_no_parasite = subset(df_all, pathogen =="")

dim = dim(df)[1]
print(dim)
a = 1
for (a in 1:dim){
  id = name2taxid(df$pathogen[a], out_type = "summary")
  taxid = id$tax_id[1]
  df$tax_id[a] = taxid
}  
df_no_parasite$tax_id = NA
df_all = rbind(df, df_no_parasite)
save(df_all, file = "df_all.Rdata")