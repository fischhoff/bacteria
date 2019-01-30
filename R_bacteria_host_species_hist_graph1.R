
load("../DATA/PROCESSED/bacteria_pathogenic_mammals.Rdata")

df = bacteria_pathogenic_mammals

df = df[, c("pathogen",
            "host_species",
            "zoonotic_category")]

df_test = subset(df, pathogen == "Borrelia hermsii")
dim(df)
df = unique(df)
dim(df)
df$pathogen = trimws(df$pathogen)
df_sum <- df %>% 
  group_by(pathogen, zoonotic_category) %>%
  summarise(
    count_hosts = n()
  )

test =subset(df_sum, zoonotic_category == "human only" & count_hosts > 1)

plot <- ggplot(df_sum, aes(x = count_hosts, group = zoonotic_category,
                           fill = zoonotic_category))+
  geom_histogram(position = "dodge")+
  # geom_histogram(stat = "count")+
  xlab("host spp. associated with each bacteria spp.")+
  ylab("number of bacteria species")
plot
ggsave(plot = plot, filename = "../PLOTS/bacteria_host_species_hist_graph_zoonotic_category.jpg")