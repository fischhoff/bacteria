#read in data on non-carnivore, non-primate mammals. 
df1 = read.csv("animal-dx-parasites_20190103.csv")
names(df1)
keep.col = c("Spp",
             "Order",
             "Zoonosis",
             "Label",
             "parasiteGMPD",
             "Citation",
             "ParasiteCorrectedNameGMPD",
             "Note",
             "multiple.countries")

df1$ParasiteCorrectedNameGMPD =""
df1 = df1[,keep.col]

test = subset(df1, parasiteGMPD != "")
dim(test)
print(dim(df1))

#read in data on carnivores 
df2 = read.csv("carnivore-zdx-parasites.csv")
print(dim(df2))
names(df2)[names(df2)=="ParasiteGMPD"]="parasiteGMPD"
df2$Citation = ""
df2$Note = ""
df2$multiple.countries = ""
df2$ParasiteCorrectedNameGMPD =""
# setdiff(names(df1),names(df2))
# setdiff(names(df2),names(df1))
df = rbind(df1,df2)
#in primate data, ParasiteCorrectedNameGMPD has parasite for some rows for which parasiteGMPD is empty. 
#for these rows, copy ParasiteCorrectedNameGMPD into parasiteGMPD. 
df3 = read.csv("prim-zdx-parasites.xls - prim-zdx-parasites.csv")
print(dim(df3))
names(df3)[names(df3)=="ParasiteGMPD"]="parasiteGMPD"
inds_empty = which(df3$parasiteGMPD=="")
df3$parasiteGMPD=as.character(df3$parasiteGMPD)
df3$ParasiteCorrectedNameGMPD=as.character(df3$ParasiteCorrectedNameGMPD)
df3$parasiteGMPD[inds_empty]=df3$ParasiteCorrectedNameGMPD[inds_empty]
names(df3)[names(df3)=="Zoonoses"]="Zoonosis"
df3$multiple.countries = ""
setdiff(names(df),names(df3))
setdiff(names(df3),names(df))
df3 = df3[,keep.col]
#put together all three
df = rbind(df, df3)
dim(df)
# df_host_zdx_parasite= df
# save(df_host_zdx_parasite,
#      file = "df_host_zdx_parasite.Rdata")
df_parasite = subset(df, parasiteGMPD != "")
save(df_parasite, file = "df_parasite.Rdata")

df_no_parasite = subset(df, parasiteGMPD == "")
save(df_no_parasite, file = "df_no_parasite.Rdata")
out = df_parasite$parasiteGMPD
write.csv(out, file = "parasiteGMPD.csv", row.names=FALSE)
# write.csv(out, file = "parasiteGMPD.csv")
df$row = seq(1,dim(df)[1])

df_all = df
save(df_all, file = "df_all.Rdata")