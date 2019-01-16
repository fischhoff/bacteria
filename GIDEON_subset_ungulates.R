load("GIDEON.Rdata")
#rename column to match  GMPD
names(GIDEON)[names(GIDEON)=="Spp"]="binomial"

#version from Bucknell has more fields
df = read.csv("msw3-all.csv")
df$binomial = paste(df$Genus,df$Species)

#Also looked at GBIF version, which seems to be from same source but has fewer fields (and requires fixing column names)
#https://www.gbif.org/dataset/672aca30-f1b5-43d3-8a2b-c1606125fa1b
# path = "msw3/"
# df = fread(paste0(path,"msw3-dwc.txt"))

print("number of species / zoonosis pairs in GIDEON including all orders")
print(dim(GIDEON))
GIDEON = merge(df, GIDEON, by = "binomial")

GIDEON = GIDEON[,c("Order", 
                   "binomial",
                   "Zoonosis")]
print("number of species / zoonosis pairs in GIDEON (all orders) after merge with mammals of the world checklist")
print(dim(GIDEON))
GIDEON = unique(GIDEON)

print("number of *unique* species / zoonosis pairs in GIDEON (all orders) after merge with mammals of the world checklist")
print(dim(GIDEON))
print(dim(GIDEON))

GIDEON = subset(GIDEON, Order %in% c("ARTIODACTYLA", "PERISSODACTYLA"))
# GIDEON = subset(GIDEON, Order %in% c("Artiodactyla", "Perissodactyla"))

save(GIDEON, file = "GIDEON.Rdata")
print("number of species / zoonosis records in GIDEON for ungulates")
print(dim(GIDEON))
