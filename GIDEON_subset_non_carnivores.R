load("GIDEON.Rdata")
#rename column to match  GMPD
names(GIDEON)[names(GIDEON)=="Spp"]="binomial"

#version from Bucknell has more fields
df = read.csv("msw3-all.csv")
dim(df)
#remove records with no species
df = subset(df, Species != "")
dim(df)
df$binomial = paste(df$Genus,df$Species)


#Also looked at GBIF version, which seems to be from same source but has fewer fields (and requires fixing column names)
#https://www.gbif.org/dataset/672aca30-f1b5-43d3-8a2b-c1606125fa1b
# path = "msw3/"
# df = fread(paste0(path,"msw3-dwc.txt"))

print("number of species / zoonosis pairs in GIDEON including all orders")
print(dim(GIDEON))

#get the rows of mammals that have not been associated with a disease
df_no = subset(df, !(binomial %in% GIDEON$binomial ))
df_no = subset(df_no, Order != "CARNIVORA")
print("number of non-carnivore mammal species without records in GIDEON")
print(dim(df_no)[1])
df_no = df_no[,c("binomial", "Order")]
df_no$Zoonosis = ""
df_no$Label = 0
df_no$parasiteGMPD = ""

#get the rows of mammals that have been associated with a disease
df_dx = subset(df, binomial %in% GIDEON$binomial)
GIDEON = merge(df_dx, GIDEON, by = "binomial")

GIDEON = GIDEON[,c("Order", 
                   "binomial",
                   "Zoonosis")]
print("number of species / zoonosis pairs in GIDEON (all orders) after merge with mammals of the world checklist")
print(dim(GIDEON))
GIDEON = unique(GIDEON)

print("number of *unique* species / zoonosis pairs in GIDEON (all orders) after merge with mammals of the world checklist")
print(dim(GIDEON))
print(dim(GIDEON))

GIDEON = subset(GIDEON, Order != "CARNIVORA")

save(GIDEON, file = "GIDEON.Rdata")
print("number of species / zoonosis records in GIDEON for non-carnivores")
print(dim(GIDEON)[1])

GIDEON$Label = 1
GIDEON$parasiteGMPD = ""

df_out = rbind(df_no, GIDEON)
print("records of non-carnivore mammals, with zdx and including one row for each mammal w/o a recorded zdx")
print(dim(df_out)[1])

print("check that size of animal-dx-parasites matches number GIDEON records plus number of other mammals")
print((dim(df_no)[1]+dim(GIDEON)[1]) == dim(df_out)[1])
write.csv(df_out, file  = "animal-dx-parasites.csv", 
          row.names = FALSE)
#change binomial to Spp
#Order 
#Zoonosis
#Add column Label (1), because all of these have zdx 
#Add parasiteGMPD, empty for now