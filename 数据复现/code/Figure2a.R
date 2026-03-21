# figure2a LDG
options(warn=-1)
library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
library(ggpmisc)

dat3 <- fread("../data/p2-1-1.csv")
dat3_1 <- fread("../data/p2-1-2.csv")
f2 <- y ~ x
f2 <- y ~ x

dat3$y = abs(dat3$y)
p_terra <-  ggscatter(dat3, x = "y", y = "re_type",
                      color = "convertedbio_1",
                      size = 1/.pt)+
  stat_correlation(
    mapping = use_label(c("R","P","method")),
    method = "pearson",alternative = "two.sided",
    small.r = F,small.p = T,size=6/.pt,r.digits = 3,
    label.y = 0.85)+
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label..)),
               parse = TRUE,size = 6/.pt,
               label.y = 0.95) +
  scale_color_fermenter(palette = "RdYlBu",direction = -1,
                        breaks = c(-20,-10,0,10,20))+
  labs(x="Absolute latitude",
       y="Relative richness",
       title = "Terra",
       color ="Annual mean temperature")+
  theme_bw(base_size = 6, base_family='sans')+
  theme(legend.title = element_text(angle = 90,hjust=0.5),
        panel.grid = element_blank(),
        legend.title.position = "left",
        legend.key.size =unit(6,'pt'))


dat3_1$y = abs(dat3_1$y)
p_marine <- ggscatter(dat3_1, x = "y", y = "re_type",
                      color = "convertedthetao_mean_Layer",
                      size = 1/.pt)+
  stat_correlation(
    mapping = use_label(c("R","P","method")),
    method = "pearson",alternative = "two.sided",
    small.r = F,small.p = T,size=6/.pt,r.digits = 3,
    label.y = 0.85)+
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label..)),
               parse = TRUE,size = 6/.pt,
               label.y = 0.95) +
  scale_color_fermenter(palette = "RdYlBu",direction = -1,
                        breaks = c(-20,-10,0,10,20))+
  labs(x="Absolute latitude",
       y="Relative richness",
       title = "Marine",
       color ="Annual mean temperature")+
  theme_bw(base_size = 6, base_family='sans')+
  theme(legend.title = element_text(angle = 90,hjust=0.5),
        panel.grid = element_blank(),
        legend.title.position = "left",
        legend.key.size =unit(6,'pt')
  )


p2a <- cowplot::plot_grid(p_terra,NULL,p_marine,
                         ncol = 1,rel_heights = c(1,0,1),
                         align = "vh")
                         
ggsave(p2a,file="../results/Figure_2a.png",width = 90,height = 90,units = "mm",dpi = 300)

