#http://www.chnosz.net/manual/taxonomy.html
#To do: check for species at each level
path= "taxdmp/"
nodes = getnodes(path)
#nodes = getnodes(paste0(path,"names.dmp"))
nodes[1,]

names = getnames(path)
#merge nodes and names
all = merge(nodes, names)

#find the id of Bacteria
bac_id = which(all$name == "Bacteria" &
                 all$type == "scientific name")

#get indices of children of bacteria superkingdom
inds_bac_children = which (all$parent == all$id[bac_id]
                           &
                           all$type == "scientific name" &
                             all$rank == "phylum")

names_phylum = c(all$name[inds_bac_children])
ids_phylum = c(all$id[inds_bac_children])

phylum_tmp = all[inds_bac_children,]
names(phylum_tmp)[names(phylum_tmp)=="id"]="phylum_id"
names(phylum_tmp)[names(phylum_tmp)=="name"]="phylum_name"
phylum_tmp = phylum_tmp[,c("phylum_id",
                           "phylum_name",
                           "type")]
a = 1
out_class = NULL
for (a in 1:length(phylum_tmp$phylum_name)){#for each phylum
  inds_phylum = which(all$name == phylum_tmp$phylum_name[a] &
                                 all$type == "scientific name")
  
  inds_phylum_children = which(all$parent == all$id[inds_phylum]
                      &
                        all$type == "scientific name" 
                       &
                         all$rank == "class" )
  
  class_tmp = all[inds_phylum_children,]
  dim = dim(class_tmp)[1]
  if (dim > 0){#there is at least one class record
    names(class_tmp)[names(class_tmp)=="id"]="class_id"
    names(class_tmp)[names(class_tmp)=="name"]="class_name"
    class_tmp$phylum_id = phylum_tmp$phylum_id[a]
    class_tmp$phylum_name = phylum_tmp$phylum_name[a]
    class_tmp$order_id = ""
    class_tmp$order_name = ""
    #assign order to be unassigned
    # inds_order = which(class_tmp$rank == "order")
    out_class = rbind(out_class, class_tmp)
  }
  #get the order rows
  inds_phylum_children = which(all$parent == all$id[inds_phylum]
                               &
                                 all$type == "scientific name" 
                               &
                                 all$rank == "order")
  order_tmp = all[inds_phylum_children,]
  dim = dim(order_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(order_tmp)[names(order_tmp)=="id"]="order_id"
    names(order_tmp)[names(order_tmp)=="name"]="order_name"
    order_tmp$phylum_id = phylum_tmp$phylum_id[a]
    order_tmp$phylum_name = phylum_tmp$phylum_name[a]
    order_tmp$class_id = ""#this is blank
    order_tmp$class_name = ""
    out_class = rbind(out_class, order_tmp)
  }#end if at least one order record  
}

#get the classes
class =subset(out_class, class_name !="")

#add the orders from out_class
b = 1
out_order = NULL
all$name = as.character(all$name)
class$class_name=as.character(class$class_name)
for (b in 1:length(class$class_name)){#each class
  inds_class = which(all$name == class$class_name[b] &
                        all$type == "scientific name")
  inds_class_children = which(all$parent == all$id[inds_class]
                               &
                                 all$type == "scientific name" 
                               &
                                 all$rank == "order" )
  order_tmp = all[inds_class_children,]
  dim = dim(order_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(order_tmp)[names(order_tmp)=="id"]="order_id"
    names(order_tmp)[names(order_tmp)=="name"]="order_name"
    
    order_tmp$class_id = class$class_id[b]#
    order_tmp$class_name = class$class_name[b]
    order_tmp$phylum_id = class$phylum_id[b]
    order_tmp$phylum_name = class$phylum_name[b]
    order_tmp$genus_id = ""#
    order_tmp$genus_name = ""
    
    out_order = rbind(out_order, order_tmp)
  }#end if at least one order record  
  else {#there are no records under class 
    print("check")
  }#end else
  
  #now get ones that go straight to genus
  inds_class_children = which(all$parent == all$id[inds_class]
                              &
                                all$type == "scientific name"
                              & all$rank == "genus") 
  genus_tmp = all[inds_class_children,]
  dim = dim(genus_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(genus_tmp)[names(genus_tmp)=="id"]="genus_id"
    names(genus_tmp)[names(genus_tmp)=="name"]="genus_name"
    
    genus_tmp$class_id = class$class_id[b]#
    genus_tmp$class_name = class$class_name[b]
    genus_tmp$phylum_id = class$phylum_id[b]
    genus_tmp$phylum_name = class$phylum_name[b]
    
    genus_tmp$order_id = ""#
    genus_tmp$order_name = ""
    
    out_order = rbind(out_order, genus_tmp)
  }#end if at least one order record  
}#end class

#get the orders from out_class
orders_from_class = subset(out_class, order_name != "")
orders_from_class$genus_id = ""
orders_from_class$genus_name = ""
#get the orders
order =subset(out_order, order_name !="")
order = rbind(order, orders_from_class)

source("ncbi_taxonomy_read_order_family_genus.R")#output is out_genus

source("ncbi_taxonomy_read_family_genus_species.R")
# ncbi_taxonomy = all
# save(ncbi_taxonomy, file = "ncbi_taxonomy.Rdata")