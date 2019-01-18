load("df_all.Rdata")
df = df_all
names(df)
#size= 14
#get only records with pathogen
df = subset(df, pathogen !="")

inds_empty=which(df$vector == "")
df$vector[inds_empty]="none"
inds_human = which(df$Spp == "Homo sapiens")
df$Order = as.character(df$Order)
df$Order[inds_human]="Human"

human = subset(df, Order == "Human")
human = human[,c("pathogen", "vector")]
non_human = subset(df, Order !="Human")
non_human = non_human[,c("Spp","Order","pathogen")]
non_human = merge(non_human, human)

df = non_human#includes human
df = df[,c("Spp", "vector",
           "Order")]
df = unique(df)
plot <- ggplot(df,
               aes(Order))+
  geom_bar(aes(fill = vector))+
  ylab("count of host-vector pairs")+
  xlab("host order")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5))
  # theme(legend.position="none")

plot
ggsave(filename = "host_vector_stacked.jpg",
       plot = plot)
# df_all = df
# save(df_all, file = "df_all.Rdata")