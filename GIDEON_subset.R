load("GIDEON.Rdata")
#rename column to match  GMPD
names(GIDEON)[names(GIDEON)=="Spp"]="HostCorrectedName"
load("GMPD.Rdata")
print(dim(GIDEON))
GIDEON = merge(GMPD, GIDEON, by = "HostCorrectedName")

GIDEON = GIDEON[,c("HostOrder", 
                   "HostCorrectedName",
                   "Zoonosis")]
print(dim(GIDEON))
GIDEON = unique(GIDEON)

print(dim(GIDEON))

GIDEON = subset(GIDEON, HostOrder %in% c("Artiodactyla", "Perissodactyla"))

# print("GIDEON dimensions after subsetting by relevant ungulate orders")                                                                                    "Perissodactyla"))
print(dim(GIDEON))
save(GIDEON, file = "GIDEON.Rdata")
dim(GIDEON)