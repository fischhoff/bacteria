load("human_bacteria.Rdata")
load("df_all.Rdata")
#rename fields to be concordant
keep = names(df_all)
keep = setdiff(keep, "Zoonosis")
df_all = df_all[,keep]
# names(df_all)[names(df_all)=="Zoonosis"]="disease"
names(human_bacteria)[names(human_bacteria)=="bacterial_disease"]="disease"
names(df_all)[names(df_all)=="parasiteGMPD"]="pathogen"

df = human_bacteria
df$Spp="Homo sapiens"
df$Order = "Primates"
df$Label = 1
df$Citation = ""
df$ParasiteCorrectedNameGMPD= ""
df$Note = ""
df$multiple.countries=NA
df$row = ""

df_all$vector = ""
df_all = rbind(df_all, df)
print("rows in df_all")
print(dim(df_all)[1])
save(df_all, file = "df_all.Rdata")