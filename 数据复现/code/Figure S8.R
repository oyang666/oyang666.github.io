### Multivariate environmental similarity surface

http://www.idata8.com/rpackage/dismo/00Index.html

```{r}
require(data.table)
require(raster)
require(dismo)
set.seed(1,"L'Ecuyer-CMRG")
historial_clim <-  dir('Z:/future_clim_part/clim_1970_2000/wc2.1_5m_bio/',full.names = TRUE)|>raster::stack()
history_dataset.name <- fread("glm_start_table.csv",integer64 = "numeric")|>colnames()
history_dataset <- Data[,c(2,11:20,3:10)]
colnames(history_dataset) <- history_dataset.name[20:38]

suppressWarnings(ms_report <- mess(historial_clim, history_dataset, full=TRUE))
plot(ms_report$mess)
# ms_report <- mess(historial_clim, history_dataset, full=TRUE)
result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
# fwrite(result,"./GLM/MESS_table.csv")
```

```{r}
require(data.table)
dat_mess <- fread("./GLM/MESS_table.csv")
```

```{r}
require(RColorBrewer)
require(ggplot2)
p <- ggplot(data=dat_mess,aes(x = x, y = y, fill = mess))+
  geom_tile()+
  # scale_fill_gradient2(#limits = c(-110, 55),
  #                      #breaks = c(-100,-50, 0, 50), 
  #                      midpoint = 0,
  #                      low = "#b42032", 
  #                      # mid = "white", 
  #                      high = "#2266ac"
  #                      )+
  # RdYlBu
  scale_fill_fermenter(palette = "RdYlBu",direction = 1,breaks =c(-100,-50,0,50))+
  labs(fill = 'MESS value')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
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
        legend.title = element_text(angle = 90,hjust=0.5),
        legend.title.position = "left"
  )+
  coord_fixed()
p

# ggsave(p,filename = "C:/Users/crow/Desktop/mess.png",width = 8, height = 3.82, dpi = 300)
```