
load("df_all.Rdata")
df_all$pathogen = trimws(df_all$pathogen)
df_all = subset(df_all, pathogen !="")
df_sum <- df_all %>% 
  group_by(pathogen) %>%
  summarise(
    count_hosts = n()
  )

plot <- ggplot(df_sum, aes(x = count_hosts))+
  geom_histogram(stat = "count")+
  xlab("host spp. associated with each bacteria spp.")+
  ylab("number of bacteria species")
plot
ggsave(plot = plot, filename = "bacteria_host_species_hist_graph.jpg")