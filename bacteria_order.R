
#load matchup of disease to bacterial species and GMPD traits
load("df_parasite_gmpd.Rdata")
dim(df)[1]
df = df_parasite_gmpd
df = df[,c("ParOrder",           "Spp",
           "Zoonosis",
           "parasiteGMPD")]
df = unique(df)
dim(df)[1]
size = 14
plot <- ggplot(df,
                aes(x = ParOrder))+
  geom_histogram(stat = "count")+
  ylab("count of host-pathogen pairs")+
  xlab("bacteria order")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =size))

plot
ggsave(filename = "bacteria_order.jpg",
      plot = plot)