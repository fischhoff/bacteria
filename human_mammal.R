load("human_bacteria.Rdata")
load("df_all.Rdata")
#rename fields to be concordant
names(df_all)[names(df_all)=="Zoonosis"]="disease"
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
save(df_all, file = "df_all.Rdata")