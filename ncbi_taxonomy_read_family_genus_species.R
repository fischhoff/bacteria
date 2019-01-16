
#get the genera from out_order; not sure how to handle
genus_from_order = subset(out_order, genus_name != "")
#get the orders
family =subset(out_family, family_name !="")
#put together genus_from_order and family

b = 1
out_species = NULL
all$name = as.character(all$name)
family$family_name=as.character(family$family_name)
ranks = NULL
for (b in 1:length(family$family_name)){#each family
  inds_family = which(all$name == family$family_name[b] &
                       all$type == "scientific name")
  inds_family_children = which(all$parent == all$id[inds_family]
                               &
                                 all$type == "scientific name")
  ranks = print(unique(all[inds_family_children,"rank"]))
    # inds_family_children = which(all$parent == all$id[inds_family]
    #                           &
    #                             all$type == "scientific name"
    #                           &
    #                             all$rank == "genus" )
  # genus_tmp = all[inds_order_children,]
  # dim = dim(genus_tmp)[1]
  # print(dim)
  # print(b)
  # if (dim > 0){#there is at least one order record
  #   names(genus_tmp)[names(genus_tmp)=="id"]="genus_id"
  #   names(genus_tmp)[names(genus_tmp)=="name"]="genus_name"
  #   
  #   genus_tmp$class_id = order$class_id[b]#
  #   genus_tmp$class_name = order$class_name[b]
  #   genus_tmp$phylum_id = order$phylum_id[b]
  #   genus_tmp$phylum_name = order$phylum_name[b]
  #   genus_tmp$family_id = ""#
  #   genus_tmp$family_name = ""
  #   
  #   out_genus = rbind(out_genus, genus_tmp)
  # }#end if at least one order record  
  # else {#there are no records under class 
  #   print("check")
  #   print(b)
  # }#end else
  # 
  # #now get ones that go straight to family
  # inds_order_children = which(all$parent == all$id[inds_order]
  #                             &
  #                               all$type == "scientific name"
  #                             & all$rank == "family") 
  # family_tmp = all[inds_order_children,]
  # dim = dim(family_tmp)[1]
  # if (dim > 0){#there is at least one order record
  #   names(family_tmp)[names(family_tmp)=="id"]="family_id"
  #   names(family_tmp)[names(family_tmp)=="name"]="family_name"
  #   
  #   family_tmp$class_id = order$class_id[b]#
  #   family_tmp$class_name = order$class_name[b]
  #   family_tmp$phylum_id = order$phylum_id[b]
  #   family_tmp$phylum_name = order$phylum_name[b]
  #   
  #   # family_tmp$genus_id = ""#
  #   # family_tmp$genus_name = ""
  # 
  #   family_tmp$species_id = ""#
  #   family_tmp$species_name = ""
  #   
  #   out_genus = rbind(out_genus, family_tmp)
  # }#end if at least one order record  
}#end family
