load("df_parasite_gmpd.Rdata")
df = df_parasite_gmpd
dim(df)[1]
df = df[,c("close",
           "nonclose",
           "vector",
           "intermediate",
           "Spp",
           "Zoonosis",
           "parasiteGMPD")]
# names = names(df)
# for (a in 1:length(names)){
#   df[,names[a]]=trimws(as.character(df[,names[a]]))
# }
df = distinct(df)
dim(df)[1]
v.names = "var."

inds = which(names(df) %in% c("close",
                              "nonclose",
                              "vector",
                              "intermediate"))
names(df)[inds]=paste0(v.names,names(df)[inds])
#make id_time to use in reshape as idvar
df$id_time_case = paste(df$Spp, df$Zoonosis, df$parasiteGMPD)
df_r = reshape(df,
               idvar = "id_time_case",
               varying = names(df)[inds],
               v.names = "var",
               direction = "long")
test = subset(df, Spp == "Myotis myotis")
name_list = names(df)[inds]
out_name = rep("", dim(df_r)[1])
for (a in 1:length(name_list)){
  #find the index in df_r$time matching a; these will be indexes to rename to name_list[a]
  inds = which(df_r$time == a)   
  out_name[inds]=name_list[a]
}
df_r$var_name = out_name


plot <- ggplot(df_r, aes(x = var))+
  geom_histogram(binwidth = 0.5)+
  # geom_smooth()+
  facet_wrap(.~var_name,
             scales = "fixed")
plot

ggsave(file = "bacteria_traits_gmpd_graph.jpg", plot = plot)
