# Figure S6
require(data.table)
remap <- fread("C:/Users/crow/Desktop/plot_new/p3_figure_table/mean_type_terrene_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]
mean_pre <- mean(remap$prediction_mean)
remap_lat <- remap[,mean_type:=mean(prediction_mean),by =c("map_join.y")]
remap_lat <- remap[,median_type:=median(prediction_mean),by =c("map_join.y")]
remap_lat <- remap_lat[,q1_type:=quantile(prediction_mean,0.25),by =c("map_join.y")]
remap_lat <- remap_lat[,q3_type:=quantile(prediction_mean,0.75),by =c("map_join.y")]
remap_lat <- unique(remap_lat[,c(2,4:7)])
setorder(remap_lat, map_join.y)

require(ggplot2)
ps4_3 <- ggplot()+
  geom_line(data = remap_lat,aes(x = map_join.y, y = mean_type),color = "#2266ac")+
  geom_line(data = remap_lat,aes(x = map_join.y, y = q1_type),color = "grey50")+
  geom_line(data = remap_lat,aes(x = map_join.y, y = q3_type),color = "grey50")+
  geom_hline(yintercept = mean_pre,color = "red")+
  annotate("text", x = 75 , y = mean_pre-0.005,
           label = paste0("Mean: ",mean_pre|>round(3)),colour="red")+
  scale_x_continuous(n.breaks = 10)+
  theme_bw()+
  labs(x="Latitude",
       y="Relative richness",
       title = "Terrene")
ps4_3

require(data.table)
remap <- fread("E:/desktopcopy//plot_new/p3_figure_table/mean_type_marine_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]
mean_pre <- mean(remap$prediction_mean)
remap_lat <- remap[,mean_type:=mean(prediction_mean),by =c("map_join.y")]
remap_lat <- remap[,median_type:=median(prediction_mean),by =c("map_join.y")]
remap_lat <- remap_lat[,q1_type:=quantile(prediction_mean,0.25),by =c("map_join.y")]
remap_lat <- remap_lat[,q3_type:=quantile(prediction_mean,0.75),by =c("map_join.y")]
remap_lat <- unique(remap_lat[,c(2,4:7)])
setorder(remap_lat, map_join.y)

ps4_4 <- ggplot()+
  geom_line(data = remap_lat,aes(x = map_join.y, y = mean_type),color = "#2266ac")+
  geom_line(data = remap_lat,aes(x = map_join.y, y = q1_type),color = "grey50")+
  geom_line(data = remap_lat,aes(x = map_join.y, y = q3_type),color = "grey50")+
  geom_hline(yintercept = mean_pre,color = "red")+
  annotate("text", x = 30 , y = mean_pre-0.005,
           label = paste0("Mean:",mean_pre|>round(3)),colour="red")+
  scale_x_continuous(n.breaks = 10)+
  theme_bw()+
  labs(x="Latitude",
       y="Relative richness",
       title = "Marine")
ps4_4

require(patchwork)
pa <- ps4_3/ps4_4
pa

#ggsave(pa,filename = "C:/Users/crow/Desktop/plot_new/p3_figure_table/ps3_4.pdf",width = 10, height = 5.27, dpi = 300)
