# R version 4.1.2 (2021-11-01)
# Platform: x86_64-pc-linux-gnu (64-bit)
options(warn=-1)
# ../data/
#   ../results/
# package loading
library(data.table)
library(ggpointdensity)
library(ggplot2)


## Figure 3a 3b
remap <- fread("../data/mean_type_terrene_final_10_cv_0.05.csv",integer64 = "numeric")
remap_cv <- remap[,c(11,12,15)]
remap <- remap[,c(11,12,14)]

remap$prediction_mean[remap$prediction_mean > quantile(remap$prediction_mean,0.999)] <- quantile(remap$prediction_mean,0.999)


# require(rgdal)
# require(sf)
# require(terra)
# require(ggplot2)
# read shapefile
# wmap <- readOGR(dsn="../data/ne_110m_land.shp", layer="ne_110m_land")

# convert to dataframe
# wmap_df <- fortify(wmap)

wmap_df <- fread("../data/wmap_df.csv")

p3_1 <- ggplot(data=remap,aes(x = map_join.x, y = map_join.y, fill = prediction_mean))+
  geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "white",color = "grey50",linewidth = 0.1) +
  geom_tile()+
  scale_fill_distiller(palette = "RdYlBu",direction = -1)+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),n.breaks = 7,)+
  scale_y_continuous(limits = c(-56,81),expand = expansion(mult = c(0, 0)),
                     position = "left")+
  coord_equal()+
  labs(fill = 'Relative richness')+
  xlab(label = "Lontitude")+
  ylab(label = "Latitude")+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(),
    legend.title = element_text(hjust = 0.5),
    legend.position = c(0.1,0.25),
    legend.box.background = element_blank(),
    legend.key.size= unit(5, 'pt'),
    legend.key.width = unit(10,"pt"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title.position = "top",
    plot.tag = element_text(size = 10,face = "bold")
  )


p3_2 <- ggplot(remap,aes(x = map_join.y,y = prediction_mean))+
  geom_bin2d(bins = 100)+
  coord_flip()+
  scale_fill_distiller(palette = "RdYlBu",direction = -1)+
  scale_y_continuous(limits = c(0,0.2),expand = expansion(mult = c(0, 0)))+
  labs(y = 'Relative richness')+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(axis.title.y = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = "none" ,
        plot.tag = element_text(size = 10,face = "bold")
  )


# Supplementary Fig. 6 
# remap_lat <- remap[,median_type:=median(prediction_mean),by =c("map_join.y")]
# remap_lat <- remap_lat[,q1_type:=quantile(prediction_mean,0.25),by =c("map_join.y")]
# remap_lat <- remap_lat[,q3_type:=quantile(prediction_mean,0.75),by =c("map_join.y")]
# remap_lat <- unique(remap_lat[,c(2,4:6)])
# setorder(remap_lat, map_join.y)

# require(ggplot2)
# ps6 <- ggplot()+
#   geom_line(data = remap_lat,aes(x = map_join.y, y = mean_type),color = "#2266ac")+
#   geom_line(data = remap_lat,aes(x = map_join.y, y = q1_type),color = "grey50")+
#   geom_line(data = remap_lat,aes(x = map_join.y, y = q3_type),color = "grey50")+
#   geom_hline(yintercept = mean_pre,color = "red")+
#   annotate("text", x = 75 , y = mean_pre-0.005,
#            label = paste0("Mean: ",mean_pre|>round(3)),colour="red")+
#   scale_x_continuous(n.breaks = 10)+
#   theme_bw()+
#   labs(x="Latitude",
#        y="Relative richness",
#        title = "Terrene")
# ps6


## Supplementary Dataset 5
## method: spearman, alternative: two side(default)

# require(data.table)
# table_1 <- fread("~/mean_type_terra_final_10_cv_0.05.csv")
# table_1 <- table_1[,c(14,11,12)]
# colnames(table_1)[2:3] <- c("x","y")
# table_2 <- fread("~/mapjoin_terra_type.csv")
# table_all <- table_1[table_2,on =c("x","y")]
# rm(table_1,table_2);gc()
# table_all <- as.data.frame(table_all)
# head(table_all)
# 
# var_name<-c()
# cor_r<-c()
# pvalue<-c()
# # spearman
# for (r in 2:64){
#   g2=colnames(table_all)[r]
#   c_r=cor(as.numeric(table_all[,1]),as.numeric(table_all[,r]),method="spearman")
#   p=cor.test(as.numeric(table_all[,1]),as.numeric(table_all[,r]),method ="spearman")[[3]]
#   var_name=c(var_name,g2)
#   cor_r=c(cor_r,c_r)
#   pvalue=c(pvalue,p)
#   print(r)
# }
# data_cor<-data.frame(var_name,cor_r,pvalue)
# data_cor <- data.table(data_cor)
# data_cor <- fread("../data/dat_cor.csv")

## Figure 3c 3d

library(lessR)
library(treemapify)

test <- fread("../data/terrene_type_importance.csv")
test <- setorder(test, IncNodePurity)
test$var_name[-c(52,59,61)] <- test$var_name[-c(52,59,61)] |>stringr::str_extract("(?<=converted).*")
test$var_name[c(52,59,61)] <- c("latitude","longitude","ai")
test$var_class <- factor(test$var_class,levels = c("Climatic variables",
                                                   "Soil properties",
                                                   "Human activities","Others"))
test <- test[,"group_total":=sum(`IncNodePurity`),by = c("var_class")]
test$group_per <- test$group_total/sum(test$`IncNodePurity`)

test1 <- test[,c(4,6)]|>unique()
test2 <- test1$group_per
names(test2) <- test1$var_class
test1$label <- paste0(test1$group_per|>round(3)*100,"%")

test$label <- paste(test$var_class," (",test$group_per|>round(3)*100,"%",")",sep = "")
p3_3<- ggplot(test, aes(area = IncNodePurity, fill = var_class,label = var_name,
                    subgroup = label)) +
  geom_treemap(alpha = 0.8) +
  geom_treemap_subgroup_border(colour = "white",size = 5) +
  geom_treemap_subgroup_text(place = "top",size = 4,
                             alpha = 1, colour = "black", reflow = T) +
  
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 3,
                    min.size = 1,
                    grow = FALSE,
                    reflow = T) +
  scale_fill_manual(values = c("#42a5f5","#ffa726","#d95b70","grey60"))+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(legend.position = "none",
        plot.tag = element_text(size = 10,face = "bold"))

test_top30 <- test[order(test$IncNodePurity, decreasing = TRUE), ] 
test_top30 <- test_top30[test_top30$var_class!= "Others",]
test_top30 <- test_top30[c(1:30),]

annotation <- readxl::read_xlsx("../data/Supplementary Dataset 5.xlsx")
annotation <- as.data.table(annotation[,c(1:3)])

colnames(annotation)[2] <- "var_name"
test_top30 <- annotation[test_top30,on =c("var_name"),]
test_top30$`Variable names` <- paste("[",test_top30$var_name,"] ",test_top30$`Variable names`,sep = "")



p3_4 = ggplot(test_top30, 
              aes(x = reorder(`Variable names`,IncNodePurity),
                  y = IncNodePurity,fill = var_class,color = var_class)) +
  geom_segment(aes(x = reorder(`Variable names`,IncNodePurity), y = 0, yend = IncNodePurity))+ 
  geom_point(size = 3/.pt, pch = 21,color = "white")+ 
  scale_fill_manual(values = c("#42a5f5","#ffa726","#d95b70","grey60"))+
  scale_color_manual(values = c("#42a5f5","#ffa726","#d95b70","grey60"))+
  coord_flip()+
  scale_x_discrete(position ="top")+
  scale_y_reverse(expand = c(0,0),limits = c(0.81,0))+
  labs(y = "Importance (IncNodePurity)")+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )

require(patchwork)
p3_all_1 <- (p3_1+p3_2)+
  plot_layout(heights = c(1,1),widths = c(5,1),nrow=1)+
  plot_annotation(tag_levels = c("a"))&
  theme(plot.tag = element_text(size = 10,face = "bold"))

ggsave(p3_all_1,file="../results/Figure_3_ab.pdf",width = 180,height = 60,units = "mm")
ggsave(p3_all_1,file="../results/Figure_3_ab.tif",width = 180,height = 60,units = "mm", dpi = 600)


p3_all_2 <- p3_3+p3_4+
  plot_annotation(tag_levels = list(c("c","d")))&
  theme(plot.tag = element_text(size = 10,face = "bold"))
p3_all_2

ggsave(p3_all_2,file="../results/Figure_3_cd.pdf",width = 180,height = 60,units = "mm")
ggsave(p3_all_2,file="../results/Figure_3_cd.tif",width = 180,height = 60,units = "mm", dpi = 600)


## Supplementary Fig. 5
# ps5 <- ggplot(data=remap_cv,aes(x = map_join.x, y = map_join.y, fill = prediction_cv))+
#   geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "white",color = "grey50") +
#   geom_tile()+
#   scale_fill_distiller(palette = "RdYlBu",direction = -1)+
#   scale_x_continuous(expand = expansion(mult = c(0, 0)))+
#   scale_y_continuous(limits = c(-56,81),expand = expansion(mult = c(0, 0)),
#                      position = "right")+
#   coord_equal()+
#   labs(title = "Terrene",
#        fill = 'Coefficient of variation')+
#   theme_bw(base_size = 10)+
#   theme(axis.title = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor =  element_blank(),
#         plot.margin = margin(t = 10, 
#                              r = 10,  
#                              b = 10,  
#                              l = 10))
# ps5

rm(list = ls())
gc()




