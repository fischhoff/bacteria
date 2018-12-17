load("GMPD.Rdata")
load("p1.Rdata")
load("p2.Rdata")
#check how many in common with p1
names(p1)[names(p1)=="preferred.name"]="ParasiteCorrectedName"
common1 = intersect(GMPD$ParasiteCorrectedName,
       p1$ParasiteCorrectedName)

print("species in common GMPD and bacterial traits in Brbic et al. 2016")
print(length(common1))
print(common1)

#check how many in common with p1
names(p2)[names(p2)=="preferred.name"]="ParasiteCorrectedName"
common2 = intersect(GMPD$ParasiteCorrectedName,
                   p2$ParasiteCorrectedName)

print("species in common GMPD and bacterial traits in Barberan et al. 2017")
print(length(common2))
print(common2)

intersect(common1, common2)