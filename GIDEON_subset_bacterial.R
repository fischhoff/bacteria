#load matchup of disease to bacterial species
# load("df_all.Rdata")
load("df_all.Rdata")
print("rows including all types of diseases")
print(dim(df_all)[1])
#read in bacterial diseases from df_all
df = read.csv("GIDEON_bacterium_dx - 20181219.csv")
print("number of bacterial diseases  in df_all")
print(dim(df)[1])
#subset df_all to include only those zoonoses that are bacterial
df_all$disease = trimws(as.character(df_all$disease))
df$bacterial_disease =as.character(df$bacterial_disease)

df_0 = subset(df_all, Label == 0)
df_all = subset(df_all, disease %in% df$bacterial_disease)
print("rows in df_all -- only bacterial diseases")
print(dim(df_all)[1])

df_all = rbind(df_0, df_all)
print("rows in df_all -- including mammals with Label = 0")
print(dim(df_all)[1])
save(df_all, file = "df_all.Rdata")
