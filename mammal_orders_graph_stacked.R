#load matchup of disease to bacterial species and GMPD traits
# load("df_parasite.Rdata")
load("df_all.Rdata")

df = df_all

# test = subset(df, Order == "")
df = subset(df, pathogen != "")

inds_human = which(df$Spp == "Homo sapiens")
df$Order = as.character(df$Order)
df$Order[inds_human]="Human"

unique(df$Label)#check that all are 1
dim(df)[1]

df = df[,c("Spp",
           "disease",
           "pathogen",
           "Order")]
df = unique(df)
dim(df)[1]
size = 14
plot <- ggplot(df,
               aes(Order))+
  geom_bar(aes(fill = pathogen))+
  ylab("count of host-pathogen pairs")+
  xlab("host order")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size =size))+
  theme(legend.position="none")

plot
ggsave(filename = "host_order_stacked.jpg",
       plot = plot)
