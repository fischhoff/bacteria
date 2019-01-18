load("df_all.Rdata")

df = df_all

# test = subset(df, Order == "")
df = subset(df, order_pathogen != "")

# inds_human = which(df$Spp == "Homo sapiens")
# df$Order = as.character(df$Order)
# df$Order[inds_human]="Human"

# unique(df$Label)#check that all are 1
# dim(df)[1]

df = df[,c("Spp",
           "disease",
           "pathogen",
           "order_pathogen",
           "genus_pathogen")]
df = unique(df)
dim(df)[1]
size = 8
plot <- ggplot(df,
               aes(order_pathogen))+
  geom_bar(aes(fill = genus_pathogen))+
  ylab("count of host-pathogen pairs")+
  xlab("pathogen order")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   vjust = 1, size =size))+
  theme(legend.position="none")

plot
ggsave(filename = "graph_pathogen_order_genus.jpg",
       plot = plot, height = 12, width = 12, units = "in")
