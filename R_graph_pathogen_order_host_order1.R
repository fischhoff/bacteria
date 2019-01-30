load("../DATA/PROCESSED/bacteria_pathogenic_mammals.Rdata")

df = bacteria_pathogenic_mammals

# test = subset(df, Order == "")
df = subset(df, order_pathogen != "")

inds_human = which(df$host_species == "Homo sapiens")
df$host_order = as.character(df$host_order)
df$host_order[inds_human]="Human"

df = df[,c("host_species",
           "disease",
           "pathogen",
           "order_pathogen",
           "host_order")]
df = unique(df)
dim(df)[1]
size = 14
plot <- ggplot(df,
               aes(order_pathogen))+
  geom_bar(aes(fill = host_order))+
  ylab("count of host-pathogen pairs")+
  xlab("pathogen order")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size =size))+
  theme(legend.position="bottom")

ggsave(filename = "../PLOTS/graph_pathogen_order_host_order_non_zoonotic.jpg",
       plot = plot, height = 8, width = 12, units = "in")
