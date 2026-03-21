
# R version 4.3.3 (2024-02-29 ucrt) -- "Angel Food Cake"
# Platform: x86_64-w64-mingw32/x64 (64-bit)
options(warn=-1)

require(data.table)
ssp126 <- fread("../data//p4_2_SSP126.csv")
ssp245 <- fread("../data//p4_2_SSP245.csv")
ssp370 <- fread("../data//p4_2_SSP370.csv")
ssp585 <- fread("../data//p4_2_SSP585.csv")
current_data <- fread("../data/current_data.csv")

### SSP126

ssp126$'id' <- paste0(ssp126$x|>round(5),"@",ssp126$y|>round(5))
FALSE %in% c(current_data$id %in% ssp126$id)
FALSE %in% c(ssp126$id %in% current_data$id)

ssp126 <- ssp126[current_data,on =c("x","y")]
ssp126 <- ssp126[complete.cases(ssp126),]
(sum(ssp126$average)-sum(ssp126$current_pred))/sum(ssp126$current_pred)
ssp126 <- ssp126[,"lat_mean":=(sum(average)-sum(current_pred))/sum(current_pred),by =c("y")]
# -0.01487126

require(ggplot2)
require(ggpubr)
require(scales)
p_126 <- ggplot(ssp126,aes(y = lat_mean,x = y))+
  geom_line(color = "#2266ac",linewidth = 0.2)+
  geom_hline(yintercept = (sum(ssp126$average)-sum(ssp126$current_pred))/sum(ssp126$current_pred),color = "red",linetype='dotted',linewidth = 0.2)+
  geom_hline(yintercept = 0,color = "grey", linetype='dotted',linewidth = 0.2)+
  annotate("text", x = -80 , y = -0.12,
           label = paste0("Overall \nchange: \n",round((sum(ssp126$average)-sum(ssp126$current_pred))/sum(ssp126$current_pred),6)*100,"%"),
           colour="red",size=3/.pt)+
  # geom_point(color = "#2266ac",size = 0.1)+
  scale_y_continuous(labels = label_percent(),limits = c(-0.2,0.2))+
  coord_flip()+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 6))+
  xlab("Latitude")+
  ylab("Relative change")+
  labs(title = "SSP126")
p_126

p126 <- ggplot(data=ssp126,aes(x = x, y = y, fill = respone_chage))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdBu",direction = -1,breaks =c(-0.4,-0.2,0,0.2,0.4))+
  # labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = "top" ,legend.box = "horizontal",
        # legend.title = element_text(#face = "italic",family = "Times",
        #                             #size = 18,
        #                             hjust = 0.5,
        #                             colour = "black"),
        # legend.title.position = "top"
        # legend.direction = "horizontal",
        # legend.position = "top"
  )+
  coord_fixed()+
  labs(x="Latitude",
       y="Relative change",
       title = "SSP126")
p126

#ggsave(p126,filename = "../results/Figure_s11_ssp-126.png",width = 9.06, height = 5.64, dpi = 300)
#ggsave(p126,filename = "../results/Figure_s11_ssp-126.pdf",width = 9.06, height = 5.64, dpi = 300)
# ssp245 

ssp245$'id' <- paste0(ssp245$x|>round(5),"@",ssp245$y|>round(5))
FALSE %in% c(current_data$id %in% ssp245$id)
FALSE %in% c(ssp245$id %in% current_data$id)

ssp245 <- ssp245[current_data,on =c("x","y")]
ssp245 <- ssp245[complete.cases(ssp245),]
(sum(ssp245$average)-sum(ssp245$current_pred))/sum(ssp245$current_pred)
ssp245 <- ssp245[,"lat_mean":=(sum(average)-sum(current_pred))/sum(current_pred),by =c("y")]

require(ggplot2)
require(ggpubr)
require(scales)
p_245 <- ggplot(ssp245,aes(y = lat_mean,x = y))+
  geom_line(color = "#2266ac",linewidth = 0.2)+
  geom_hline(yintercept = (sum(ssp245$average)-sum(ssp245$current_pred))/sum(ssp245$current_pred),color = "red",linetype='dotted',linewidth = 0.2)+
  geom_hline(yintercept = 0,color = "grey", linetype='dotted',linewidth = 0.2)+
  annotate("text", x = -80 , y = -0.12,
           label = paste0("Overall \n change: \n",round((sum(ssp245$average)-sum(ssp245$current_pred))/sum(ssp245$current_pred),6)*100,"%"),
           colour="red",size=3/.pt)+
  # geom_point(color = "#2266ac",size = 0.1)+
  scale_y_continuous(labels = label_percent(),limits = c(-0.2,0.2))+
  coord_flip()+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 6))+
  xlab("Latitude")+
  ylab("Relative change")+
  labs(title = "SSP245")
p_245

p245 <- ggplot(data=ssp245,aes(x = x, y = y, fill = respone_chage))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdBu",direction = -1,breaks =c(-0.4,-0.2,0,0.2,0.4))+
  # labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = "top" ,legend.box = "horizontal",
        # legend.title = element_text(#face = "italic",family = "Times",
        #                             #size = 18,
        #                             hjust = 0.5,
        #                             colour = "black"),
        # legend.title.position = "top"
        # legend.direction = "horizontal",
        # legend.position = "top"
  )+
  coord_fixed()+
  labs(x="Latitude",
       y="Relative change",
       title = "SSP245")
p245
#151 94 453 282 906 564

#ggsave(p245,filename = "../results/Figure_s11_ssp-245.png",width = 9.06, height = 5.64, dpi = 300)
#ggsave(p245,filename = "../results/Figure_s11_ssp-245.pdf",width = 9.06, height = 5.64, dpi = 300)

#ssp370

ssp370$'id' <- paste0(ssp370$x|>round(5),"@",ssp370$y|>round(5))
FALSE %in% c(current_data$id %in% ssp370$id)
FALSE %in% c(ssp370$id %in% current_data$id)


ssp370 <- ssp370[current_data,on =c("x","y")]
ssp370 <- ssp370[complete.cases(ssp370),]
(sum(ssp370$average)-sum(ssp370$current_pred))/sum(ssp370$current_pred)
ssp370 <- ssp370[,"lat_mean":=(sum(average)-sum(current_pred))/sum(current_pred),by =c("y")]

require(ggplot2)
require(ggpubr)
require(scales)
p_370 <- ggplot(ssp370,aes(y = lat_mean,x = y))+
  geom_line(color = "#2266ac",linewidth = 0.2)+
  geom_hline(yintercept = (sum(ssp370$average)-sum(ssp370$current_pred))/sum(ssp370$current_pred),color = "red",linetype='dotted',linewidth = 0.2)+
  geom_hline(yintercept = 0,color = "grey", linetype='dotted',linewidth = 0.2)+
  annotate("text", x = -80 , y = -0.12,
           label = paste0("Overall \n change: \n",round((sum(ssp370$average)-sum(ssp370$current_pred))/sum(ssp370$current_pred),6)*100,"%"),
           colour="red",size=3/.pt)+
  scale_y_continuous(labels = label_percent(),limits = c(-0.2,0.2))+
  coord_flip()+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 6))+
  xlab("Latitude")+
  ylab("Relative change")+
  labs(title = "SSP370")
p_370

p370 <- ggplot(data=ssp370,aes(x = x, y = y, fill = respone_chage))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdBu",direction = -1,breaks =c(-0.4,-0.2,0,0.2,0.4))+
  # labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = "top" ,legend.box = "horizontal",
        # legend.title = element_text(#face = "italic",family = "Times",
        #                             #size = 18,
        #                             hjust = 0.5,
        #                             colour = "black"),
        # legend.title.position = "top"
        # legend.direction = "horizontal",
        # legend.position = "top"
  )+
  coord_fixed()+
  labs(x="Latitude",
       y="Relative change",
       title = "SSP370")
p370
#151 94 453 282 906 564

#ggsave(p370,filename = "../results/Figure_s11_ssp-370.png",width = 9.06, height = 5.64, dpi = 300)
#ggsave(p370,filename = "../results/Figure_s11_ssp-370.pdf",width = 9.06, height = 5.64, dpi = 300)


#ssp585 

ssp585$'id' <- paste0(ssp585$x|>round(5),"@",ssp585$y|>round(5))
FALSE %in% c(current_data$id %in% ssp585$id)
FALSE %in% c(ssp585$id %in% current_data$id)


ssp585 <- ssp585[current_data,on =c("x","y")]
ssp585 <- ssp585[complete.cases(ssp585),]
(sum(ssp585$average)-sum(ssp585$current_pred))/sum(ssp585$current_pred)
ssp585 <- ssp585[,"lat_mean":=(sum(average)-sum(current_pred))/sum(current_pred),by =c("y")]

require(ggplot2)
require(ggpubr)
require(scales)
p_585 <- ggplot(ssp585,aes(y = lat_mean,x = y))+
  geom_line(color = "#2266ac",linewidth = 0.2)+
  geom_hline(yintercept = (sum(ssp585$average)-sum(ssp585$current_pred))/sum(ssp585$current_pred),color = "red",linetype='dotted',linewidth = 0.2)+
  geom_hline(yintercept = 0,color = "grey", linetype='dotted',linewidth = 0.2)+
  annotate("text", x = -80 , y = -0.12,
           label = paste0("Overall \n change: \n",round((sum(ssp585$average)-sum(ssp585$current_pred))/sum(ssp585$current_pred),6)*100,"%"),
           colour="red",size=3/.pt)+
  scale_y_continuous(labels = label_percent(),limits = c(-0.2,0.2))+
  coord_flip()+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 6))+
  xlab("Latitude")+
  ylab("Relative change")+
  labs(title = "SSP585")
p_585

p585 <- ggplot(data=ssp585,aes(x = x, y = y, fill = respone_chage))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdBu",direction = -1,breaks =c(-0.4,-0.2,0,0.2,0.4))+
  # labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        # legend.position = "top" ,legend.box = "horizontal",
        # legend.title = element_text(#face = "italic",family = "Times",
        #                             #size = 18,
        #                             hjust = 0.5,
        #                             colour = "black"),
        # legend.title.position = "top"
        # legend.direction = "horizontal",
        # legend.position = "top"
  )+
  coord_fixed()+
  labs(x="Latitude",
       y="Relative change",
       title = "SSP585")
p585
#151 94 453 282 906 564

#ggsave(p585,filename = "../results/Figure_s11_ssp-585.png",width = 9.06, height = 5.64, dpi = 300)
#ggsave(p585,filename = "../results/Figure_s11_ssp-585.pdf",width = 9.06, height = 5.64, dpi = 300)

require(patchwork)
ps11 <- p126+p245+p370+p585+plot_layout(nrow = 2,widths = c(1, 1))
# ggsave(ps11,file="../results/Figure_s11.tif",width = 180,height = 120,units = "mm",dpi = 600)

total <- ssp126[,c(1,2,4,20)]
total <- total[ssp245[,c(4,18)],on = c("id")]
total <- total[ssp370[,c(4,17)],on = c("id")]
total <- total[ssp585[,c(4,18)],on = c("id")]
rm(ssp126,ssp245,ssp370,ssp585);gc()
colnames(total)[4:7] <- paste0("change_",c(126,245,370,585))



total <- current_data[total,on =c("x","y")]
total <- total[complete.cases(total),]
total[,c(3:6)] <- c()
total$'level' <- apply(total[,c(3:6)],1,function(x){
  test = unlist(x)
  return(test[test>0]|>length())
})

# require(rgdal)
# require(sf)
# require(terra)
# # read shapefile
# wmap <- readOGR(dsn="E:/random_forest/ne_110m_land.shp", layer="ne_110m_land")
# 
# # convert to dataframe
# wmap_df <- fortify(wmap)
# 
# # create a blank ggplot theme
# theme_opts <-list(theme(panel.grid.minor = element_blank(),
#                         panel.grid.major = element_blank(),
#                         panel.background = element_blank(),
#                         plot.background = element_rect(fill="white"),
#                         panel.border = element_blank(),
#                         axis.line = element_blank(),
#                         axis.text.x = element_blank(),
#                         axis.text.y = element_blank(),
#                         axis.ticks = element_blank(),
#                         axis.title.x = element_blank(),
#                         axis.title.y = element_blank(),
#                         plot.title = element_text(size=22,hjust = .5)))
# fwrite(wmap_df,"C:/Users/crow/Desktop/wmap_df.csv")
wmap_df <- fread("../data/wmap_df.csv")

require(ggplot2)
require(ggplot2)
p4_1 <- ggplot(data=total,aes(x = x, y = y, fill = factor(level)))+
  geom_polygon(data = wmap_df, aes(long,lat, group=group),fill = "grey90",color = "white",linewidth = 0.1) +
  geom_tile()+
  scale_fill_brewer(palette = "RdBu",direction = -1)+
  # labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  labs(fill ="No. of scenarios that\nfollow increasing trend")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  coord_equal()+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = c(0.1,0.35),
        legend.box = "vertical",
        legend.title = element_text(hjust = 0.5,angle = 90,
                                    colour = "black",size = 4),
        legend.title.position = "left",
        legend.key.size= unit(5, 'pt'),
        legend.key.height = unit(7,"pt"),
        plot.margin =  ggplot2::margin(t = 0,  
                                       r = 15/.pt,  
                                       b = 0, 
                                       l = 0)
  )

require(patchwork)
p4_2 <- p_126+p_245+p_370+p_585+
  plot_layout(axes = "collect",axis_titles = "collect",nrow = 1)
p4_2


total <- fread("../data/p5_total_1.csv")
total <- total[,c(1:4,54,5:53)]

add_continent <- fread("../data/final.csv")
add_continent <- add_continent[,c(2,3,7)]

FALSE %in% c(total$x %in% add_continent$x)
total$x[!total$x %in% add_continent$x] <- total$x[!total$x %in% add_continent$x]|>round(5)
add_continent$x[!add_continent$x %in% total$x] <- add_continent$x[!add_continent$x %in% total$x]|>round(5)
FALSE %in% c(total$x %in% add_continent$x)
FALSE %in% c(total$y %in% add_continent$y)
total$y[!total$y %in% add_continent$y]<- total$y[!total$y %in% add_continent$y]|>round(5)
add_continent$y[!add_continent$y %in% total$y] <- add_continent$y[!add_continent$y %in% total$y]|>round(5)
FALSE %in% c(total$y %in% add_continent$y)
total <- add_continent[total,on =c("x","y")]
total <- total[,-6]
total$CONTINENT[total$CONTINENT == "Australia"] <- "Oceania"

total$CONTINENT|>unique()
colnames(total)[3] <- "Continent"

result <- data.table(name = colnames(total)[6:54])
result$'scenario' <- result$name|>stringr::str_extract("ssp\\w*(?=_)")

result$'All continent' <- total[, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'North America' <- total[total$Continent == "North America",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'Asia' <- total[total$Continent == "Asia",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'Europe' <- total[total$Continent == "Europe",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'Africa' <- total[total$Continent == "Africa",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'Oceania' <- total[total$Continent == "Oceania",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'South America' <- total[total$Continent == "South America",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()

result$'Antarctica' <- total[total$Continent == "Antarctica",][, lapply(.SD, function(x){mean(sum(x-current_pred)/sum(current_pred))}), .SDcols = c(result$name)] |> unlist()|>as.vector()


result <- melt(result, id=colnames(result)[1:2],
               variable.name = "Continent",
               value.name = "Relative change")
result$Continent <- factor(result$Continent,
                           levels = c(
                             "All continent","North America","South America","Asia",
                             "Oceania","Antarctica","Europe","Africa"
                           ))
result$scenario <- paste0("SSP",result$scenario|>stringr::str_extract("(?<=ssp).*"))


require(ggplot2)
require(ggpubr)
require(scales)

n_fun <- function(x){
  return(data.frame(y = 0.075, label = paste0("n = ",length(x))))
}
p4_3 <- ggplot(result,aes(x = scenario, y = `Relative change`))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1,size = 0.5/.pt)+
  geom_boxplot(outlier.shape = NA,size = 0.5/.pt)+
  geom_point(aes(color=scenario),
             pch=21,stroke=0.3,size = 0.5/.pt,
             position = position_jitter())+
  geom_hline(yintercept = 0,color = "black", linetype='dotted',linewidth = 0.1)+
  scale_y_continuous(labels = label_percent(),limits = c(-0.08,0.08),breaks = c(-0.08,-0.04,0,0.04,0.08))+
  stat_summary(fun.data = n_fun, geom = "text",size = 3/.pt,)+
  scale_color_brewer(palette = "YlOrRd")+
  facet_wrap(Continent~.,nrow = 2)+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x=element_text(hjust =-0.03,size = 6),
        legend.position = "none")

p4_3


total <- fread("../data/p5_4_total_1.csv")
add_continent <- fread("../data/final.csv")
add_continent <- add_continent[,c(2,3,7)]

FALSE %in% c(total$x %in% add_continent$x)
total$x[!total$x %in% add_continent$x] <- total$x[!total$x %in% add_continent$x]|>round(5)
add_continent$x[!add_continent$x %in% total$x] <- add_continent$x[!add_continent$x %in% total$x]|>round(5)
FALSE %in% c(total$x %in% add_continent$x)
FALSE %in% c(total$y %in% add_continent$y)
total$y[!total$y %in% add_continent$y]<- total$y[!total$y %in% add_continent$y]|>round(5)
add_continent$y[!add_continent$y %in% total$y] <- add_continent$y[!add_continent$y %in% total$y]|>round(5)
FALSE %in% c(total$y %in% add_continent$y)
total <- add_continent[total,on =c("x","y")]
total <- total[,-9]
colnames(total)[3] <- "Continent"
total <- total[Continent != "",]
total$Continent[total$Continent == "Australia"] <- "Oceania"




result <- total[,.(
  'ssp126_up'=length(change_126[change_126>0])/length(change_126),
  'ssp126_down'=length(change_126[change_126<0])/length(change_126),
  'ssp245_up'=length(change_245[change_245>0])/length(change_245),
  'ssp245_down'=length(change_245[change_245<0])/length(change_245),
  'ssp370_up'=length(change_370[change_370>0])/length(change_370),
  'ssp370_down'=length(change_370[change_370<0])/length(change_370),
  'ssp585_up'=length(change_585[change_585>0])/length(change_585),
  'ssp585_down'=length(change_585[change_585<0])/length(change_585)
),by = c("Continent")]
# result <- result[-c(1,4),]

result <- rbind(result,list("All continent",0,0,0,0,0,0,0,0))
result$ssp126_up[8] <- length(total$change_126[total$change_126>0])/length(total$change_126)
result$ssp126_down[8] <- length(total$change_126[total$change_126<0])/length(total$change_126)
result$ssp245_up[8] <- length(total$change_245[total$change_245>0])/length(total$change_245)
result$ssp245_down[8] <- length(total$change_245[total$change_245<0])/length(total$change_245)
result$ssp370_up[8] <- length(total$change_370[total$change_370>0])/length(total$change_370)
result$ssp370_down[8] <- length(total$change_370[total$change_370<0])/length(total$change_370)
result$ssp585_up[8] <- length(total$change_585[total$change_585>0])/length(total$change_585)
result$ssp585_down[8] <- length(total$change_585[total$change_585<0])/length(total$change_585)


result <- melt(result,
               id=colnames(result)[1],
               variable.name = "scenarios trend",
               value.name = "Relative change(%)")

result$'scenrios' <- result$`scenarios trend`|>stringr::str_extract("(?<=ssp).*(?=_)")
result$'scenrios' <- paste0("SSP",result$'scenrios')
result$'trend' <- result$`scenarios trend`|>stringr::str_extract("(?<=_).*")

result$trend <- factor(result$trend,levels = c("up","down"))
result$Continent <- factor(result$Continent,
                           levels = c(
                             "All continent","North America","South America","Asia",
                             "Oceania","Antarctica","Europe","Africa"
                           ))

require(ggplot2)
require(ggplot2)
p4_4 <- ggplot(result, aes(
  x = Continent, 
  y = `Relative change(%)`,  
  fill = trend))+
  geom_bar(stat = "identity",position = "fill")+
  geom_hline(yintercept = 0.5,color = "white", linetype="dotted")+
  facet_wrap(scenrios~.,nrow = 1)+
  scale_fill_manual(values = c("#d95b70","#509cc8"))+
  scale_y_continuous(labels = label_percent(),expand = expansion(mult = c(0, 0)))+
  theme_bw(base_size = 6,base_line_size = 0.5, base_rect_size = 0.5)+
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x=element_text(hjust =-0.03,size = 6),
        legend.position = "none")+
  labs(y = "Percent of trend")
p4_4



require(patchwork)
p4_all<- p4_1+p4_2+p4_3+p4_4+plot_layout(nrow = 2,widths = c(1, 1))+plot_annotation(tag_levels =list(c("a","b","","","","c","d")))&
  theme(plot.tag = element_text(size = 10,face = "bold"))
p4_all
# 
ggsave(p4_all,file="../results/Figure_4.pdf",width = 180,height = 120,units = "mm")
ggsave(p4_all,file="../results/Figure_4.tif",width = 180,height = 120,units = "mm",dpi = 600)
