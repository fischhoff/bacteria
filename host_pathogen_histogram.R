
load("df_all.Rdata")
df_all$pathogen = trimws(df_all$pathogen)
df_all = subset(df_all, pathogen !="")
df_all = subset(df_all, Spp != "Homo sapiens")
df_sum <- df_all %>% 
  group_by(Spp, Order) %>%
  summarise(
    count_pathogen = n()
  )

plot <- ggplot(df_sum, aes(x = count_pathogen))+
  geom_histogram(stat = "count")+
  xlab("bacteria spp. associated with each host spp.")+
  ylab("number of host species")+
  # facet_wrap(.~Order, scales= "free")
  facet_wrap(.~Order)
plot
ggsave(plot = plot, filename = "host_pathogen_histogram.jpg")