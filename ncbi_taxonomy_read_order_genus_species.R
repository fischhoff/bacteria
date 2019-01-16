
#add the orders from out_class
b = 1
out_genus = NULL
all$name = as.character(all$name)
order$order_name=as.character(order$order_name)
for (b in 1:length(order$order_name)){#each class
  inds_order = which(all$name == order$order_name[b] &
                       all$type == "scientific name")
  inds_order_children = which(all$parent == all$id[inds_order]
                              &
                                all$type == "scientific name" 
                              &
                                all$rank == "genus" )
  genus_tmp = all[inds_order_children,]
  dim = dim(genus_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(genus_tmp)[names(genus_tmp)=="id"]="order_id"
    names(genus_tmp)[names(genus_tmp)=="name"]="order_name"
    
    genus_tmp$class_id = order$class_id[b]#
    genus_tmp$class_name = order$class_name[b]
    genus_tmp$phylum_id = order$phylum_id[b]
    genus_tmp$phylum_name = order$phylum_name[b]
    genus_tmp$species_id = ""#
    genus_tmp$species_name = ""
    
    out_genus = rbind(out_genus, genus_tmp)
  }#end if at least one order record  
  else {#there are no records under class 
    print("check")
  }#end else
  
  #now get ones that go straight to genus
  inds_order_children = which(all$parent == all$id[inds_class]
                              &
                                all$type == "scientific name"
                              & all$rank == "species") 
  species_tmp = all[inds_order_children,]
  dim = dim(species_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(species_tmp)[names(species_tmp)=="id"]="species_id"
    names(species_tmp)[names(species_tmp)=="name"]="species_name"
    
    species_tmp$class_id = order$class_id[b]#
    species_tmp$class_name = order$class_name[b]
    species_tmp$phylum_id = order$phylum_id[b]
    species_tmp$phylum_name = order$phylum_name[b]
    
    genus_tmp$genus_id = ""#
    genus_tmp$genus_name = ""
    
    out_genus = rbind(out_genus, species_tmp)
  }#end if at least one order record  
}#end class
