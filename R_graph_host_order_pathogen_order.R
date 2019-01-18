load("df_all.Rdata")

df = df_all

df = subset(df, order_pathogen != "")

inds_human = which(df$Spp == "Homo sapiens")
df$Order = as.character(df$Order)
df$Order[inds_human]="Human"

# unique(df$Label)#check that all are 1
# dim(df)[1]

df = df[,c("Spp",
           "disease",
           "pathogen",
           "order_pathogen",
           "Order")]
df = unique(df)
dim(df)[1]
size = 8
plot <- ggplot(df,
               aes(Order))+
  geom_bar(aes(fill = order_pathogen))+
  ylab("count of host-pathogen pairs")+
  xlab("host order")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   vjust =1, size =size))+
  theme(legend.position="bottom")

plot
ggsave(filename = "graph_host_order_pathogen_order.jpg",
       plot = plot, height = 12, width = 12, units = "in")
