# df = read.csv("bacteria_species.csv")
load("bacteria_species.Rdata")

#Start the clock
ptm<-proc.time()
out =NULL#initialize output
df = bacteria_species
dim = dim(df)[1]
# for (a in 1:10){
out_all = NULL#initialize data frame for all data
for (a in 1:dim){
  #Sys.sleep(0.15)
  tmp = classification(df$childtaxa_id[a], db = "ncbi")
  tmp = tmp[[1]]
  nrows = dim(tmp)[1]
  if (nrows>0){#if at least one row
    # tmp_species = subset(tmp, rank == "species")
    # tmp$species = tmp_species$name 
    tmp_order = subset(tmp, rank == "order")
    dim_o = dim(tmp_order)[1]
    tmp$order = ""
    if(dim_o>0){
      tmp$order = tmp_order$name 
    }
    out = rbind(out,tmp)
    
  }
  out_all = rbind(out_all, tmp)
}

bacteria_taxonomy = out_all
save(bacteria_taxonomy, file = "bacteria_taxonomy.Rdata")
#Stop the clock
print((proc.time()-ptm)/60)
bacteria_species_out = out
save(bacteria_species_out, file = "bacteria_species_out.Rdata")