
#add the orders from out_class
b = 1
out_family = NULL
all$name = as.character(all$name)
order$order_name=as.character(order$order_name)
for (b in 1:length(order$order_name)){#each class
  print(b)
  inds_order = which(all$name == order$order_name[b] &
                       all$type == "scientific name")
  inds_order_children = which(all$parent == all$id[inds_order]
                              &
                                all$type == "scientific name" 
                              &
                                all$rank == "genus" )
  genus_tmp = all[inds_order_children,]
  dim = dim(genus_tmp)[1]
  # print(dim)
  # print(b)
  if (dim > 0){#there is at least one order record
    names(genus_tmp)[names(genus_tmp)=="id"]="species_id"
    names(genus_tmp)[names(genus_tmp)=="name"]="species_name"
    
    genus_tmp$class_id = order$class_id[b]#
    genus_tmp$class_name = order$class_name[b]
    genus_tmp$order_id = order$order_id[b]#
    genus_tmp$order_name = order$order_name[b]
    
    genus_tmp$phylum_id = order$phylum_id[b]
    genus_tmp$phylum_name = order$phylum_name[b]
    genus_tmp$family_id = ""#
    genus_tmp$family_name = ""
    
    out_family = rbind(out_family, genus_tmp)
  }#end if at least one order record  
  else {#there are no records under class 
    # print("check")
    # print(b)
  }#end else
  
  #now get ones that go straight to family
  inds_order_children = which(all$parent == all$id[inds_order]
                              &
                                all$type == "scientific name"
                              & all$rank == "family") 
  family_tmp = all[inds_order_children,]
  dim = dim(family_tmp)[1]
  if (dim > 0){#there is at least one order record
    names(family_tmp)[names(family_tmp)=="id"]="family_id"
    names(family_tmp)[names(family_tmp)=="name"]="family_name"
    
    family_tmp$class_id = order$class_id[b]#
    family_tmp$class_name = order$class_name[b]
    family_tmp$phylum_id = order$phylum_id[b]
    family_tmp$phylum_name = order$phylum_name[b]
    family_tmp$order_id = order$order_id[b]#
    family_tmp$order_name = order$order_name[b]
    
    family_tmp$species_id = ""#
    family_tmp$species_name = ""
    
    out_family = rbind(out_family, family_tmp)
  }#end if at least one order record  
  
  inds_order_children = which(all$parent == all$id[inds_order]
                              &
                                all$type == "scientific name" 
                              &
                                all$rank == "species" )
  species_tmp = all[inds_order_children,]
  dim = dim(species_tmp)[1]
  if (dim > 0){#there is at least one species record
    names(species_tmp)[names(species_tmp)=="id"]="species_id"
    names(species_tmp)[names(species_tmp)=="name"]="species_name"
    
    species_tmp$class_id = order$class_id[b]#
    species_tmp$class_name = order$class_name[b]
    species_tmp$phylum_id = order$phylum_id[b]
    species_tmp$phylum_name = order$phylum_name[b]
    species_tmp$order_id = order$order_id[b]#
    species_tmp$order_name = order$order_name[b]
    
    species_tmp$family_id = ""#
    species_tmp$family_name = ""
    
    out_family = rbind(out_family, species_tmp)
  }#end if at least one order record  
  
  
  #print(length(inds_order_children))
  
  
}#end class
