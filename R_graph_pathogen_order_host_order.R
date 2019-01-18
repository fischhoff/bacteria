load("df_all.Rdata")

df = df_all

# test = subset(df, Order == "")
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
size = 14
plot <- ggplot(df,
               aes(order_pathogen))+
  geom_bar(aes(fill = Order))+
  ylab("count of host-pathogen pairs")+
  xlab("pathogen order")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size =size))+
  theme(legend.position="bottom")

plot
ggsave(filename = "graph_pathogen_order_host_order.jpg",
       plot = plot, height = 8, width = 12, units = "in")
