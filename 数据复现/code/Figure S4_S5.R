# Figure S4 


require(data.table)
remap <- fread("C:/Users/crow/Desktop/plot_new/p3_figure_table/mean_type_marine_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]
quantile(remap$prediction_mean,0.999)
# remap$prediction_mean[remap$prediction_mean > quantile(remap$prediction_mean,0.999)] <- quantile(remap$prediction_mean,0.999)

remap$prediction_mean[remap$prediction_mean > 0.12] <- 0.12

ps3_2 <- ggplot(data=remap,aes(x = map_join.x, y = map_join.y, fill = prediction_mean))+
  geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "white",color = "grey50") +
  geom_tile()+
  scale_fill_distiller(palette = "RdYlBu",direction = -1)+
  scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     position = "right")+
  coord_equal()+
  labs(title = "Marine",
       fill = 'Relative richness')+
  theme_bw(base_size = 10)+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = "top" ,
        legend.box = "horizontal",
        legend.title.position = "top",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 10,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10))
ps3_2

ggsave(ps3_2,filename = "C:/Users/crow/Desktop/plot_new/p3_figure_table/ps3_2.pdf",width = 10, height = 5.27, dpi = 300)



# Figure S5

require(data.table)
remap <- fread("E:/desktopcopy/plot_new/new/mean_type_terra_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]

require(rgdal)
require(sf)
require(terra)
require(ggplot2)
# read shapefile
wmap <- readOGR(dsn="E:/random_forest/ne_110m_land.shp", layer="ne_110m_land")

# convert to dataframe
wmap_df <- fortify(wmap)

ps3_3_1 <- ggplot(data=remap_cv,aes(x = map_join.x, y = map_join.y, fill = prediction_cv))+
  geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "white",color = "grey50") +
  geom_tile()+
  scale_fill_distiller(palette = "RdYlBu",direction = -1)+
  scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(limits = c(-56,81),expand = expansion(mult = c(0, 0)),
                     position = "right")+
  coord_equal()+
  labs(title = "Terrene",
       fill = 'Coefficient of variation')+
  theme_bw(base_size = 10)+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = c(0.1,0.25) ,
        # legend.box = "horizontal",
        # legend.title.position = "top",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 10,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10))

require(data.table)
remap <- fread("E:/desktopcopy//plot_new/p3_figure_table/mean_type_marine_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]


require(ggplot2)
ps3_3_2 <- ggplot(data=remap_cv,aes(x = map_join.x, y = map_join.y, fill = prediction_cv))+
  geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "white",color = "grey50") +
  geom_tile()+
  scale_fill_distiller(palette = "RdYlBu",direction = -1)+
  scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     position = "right")+
  coord_equal()+
  labs(title = "Marine",
       fill = 'Coefficient of variation')+
  theme_bw(base_size = 10)+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = c(0.1,0.25) ,
        # legend.box = "horizontal",
        # legend.title.position = "top",
        plot.margin = margin(t = 10,  # 顶部边缘距离
                             r = 10,  # 右边边缘距离
                             b = 10,  # 底部边缘距离
                             l = 10))
ps3_3_2
