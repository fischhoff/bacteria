
load("human_bacteria.Rdata")

df = human_bacteria
df$vector = as.character(df$vector)
df$vector = tolower(df$vector)
df$vector = trimws(df$vector)
inds0 = which(df$vector == 0)
df$vector[inds0]=""
out = NULL

for (z in 1:dim(df)[1]){
  if (df$vector[z] == ""){
    tmp = df[z,]
    out = rbind(out, tmp)#add this row without any changes
  }
  if (df$vector[z] != ""){#if there is an entry
    #split by comma
    species.split <- strsplit(df$vector[z], ",")
    #get number of species
    nspecies = length(species.split[[1]])
    for (a in 1:nspecies){#for each species
      species = species.split[[1]][a]
      tmp = df[z,]
      tmp$vector = species
      out = rbind(out, tmp)
    }#end a'th species
  }#end if statement re: speciesDescription not empty
}#end row of df

human_bacteria = out
human_bacteria$vector =  trimws(human_bacteria$vector)
save(human_bacteria, file = "human_bacteria.Rdata")

inds_fly = which(human_bacteria$vector %in% c("fly",
                                              "black fly",
                                              "deer fly",
                                              "sandfly"))
human_bacteria$vector[inds_fly]="other flies"
human_dx_vector = human_bacteria[,c("bacterial_disease", "vector")]
human_dx_vector$vector[human_dx_vector$vector==""]="none"
human_dx_vector = unique(human_dx_vector)

plot <- ggplot(data = human_dx_vector, aes(x = vector))+
  geom_histogram(stat = "count")+
  ylab("count of diseases")

plot1 = plot

ggsave(plot, filename = "vector_count.jpg")

human_dx_vector_sum <- human_dx_vector %>% 
  group_by(bacterial_disease) %>%
  summarise(n = n())
plot <- ggplot(data = human_dx_vector_sum, aes(x = n))+
  geom_histogram()+
  ylab("count of diseases")+
  xlab("number of vectors per disease")

plot2 = plot

ggsave(plot, filename = "vector_count_by_disease.jpg")

# mites = subset(human_dx_vector, vector == "mite")
# for (a in 1:dim(df)[1]){
#   tmp = df[a,]
#   #flea
#   test_split = strsplit(tmp$vector,"flea")
#   tmp$flea = 0
#   n = length(test_split[[1]])
#   if (n >1){
#     tmp$flea = 1
#   }
#   
#   test_split = strsplit(tmp$vector,"mosquito")
#   tmp$mosquito = 0
#   n = length(test_split[[1]])
#   if (n >1){
#     tmp$mosquito = 1
#   }
#   
#   out = rbind(out, tmp)
# }