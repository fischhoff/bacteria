#load matchup of disease to bacterial species and GMPD traits
# load("df_parasite.Rdata")
load("df_all.Rdata")

df = df_all
unique(df$Label)#check that all are 1
dim(df)[1]

inds_human = which(df$Spp == "Homo sapiens")
df$Order = as.character(df$Order)
df$Order[inds_human]="Human"
df = df[,c("Spp",
           "disease",
           "pathogen",
           "Order",
           "Label")]
df = unique(df)
dim(df)[1]
size = 14
df$Label = factor(df$Label)
plot <- ggplot(df, alpha = 0.2,
       aes(x = Order, group = Label, fill = Label))+
  geom_bar(stat="count", position='dodge')+
   theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                    vjust = 0.5, size =size))+
  guides(fill=guide_legend(title="Pathogen host"))

# plot <- ggplot(df,
#                aes(x = Order, fill=Label))+
#   geom_bar(stat="identity", color = "black", size = 0.2, 
#            position=position_dodge()) +
#     # 
#     # 
#     # geom_histogram(stat = "count")+
#   ylab("count of host-pathogen pairs")+
#   xlab("host order")+
# 
plot
ggsave(filename = "host_order.jpg",
       plot = plot)
