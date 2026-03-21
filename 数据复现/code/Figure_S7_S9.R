# # Calculate the error between models

# require(data.table)
# current_data <- fread("./current_data.csv")
# ssp126 <- fread("rf_ssp126_1.csv")
# ssp245 <- fread("rf_ssp245_1.csv")
# ssp370 <- fread("rf_ssp370_1.csv")
# ssp585 <- fread("rf_ssp585_1.csv")
# 
# ssp126$'id' <- paste0(ssp126$x|>round(5),"@",ssp126$y|>round(5))
# FALSE %in% c(current_data$id %in% ssp126$id)
# FALSE %in% c(ssp126$id %in% current_data$id)
# 
# ssp126 <- ssp126[current_data,on =c("id")]
# ssp126 <- ssp126[complete.cases(ssp126),]
# ssp126 <- ssp126[ssp126$mess>0,]
# (sum(ssp126$average)-sum(ssp126$current_pred))/sum(ssp126$current_pred)
# 
# pred_ssp126 <- data.table(
#   ssp = rep("ssp126",13),
#   model = colnames(ssp126)[3:15],
#   overall_change = rep(0,13),
#   origin_oc = rep(-0.01487126,13)
# )
# 
# pred_ssp126$overall_change <- lapply(pred_ssp126$model,function(x){
#   cols <- x
#   return((sum(ssp126[,..cols]) - sum(ssp126$current_pred))/sum(ssp126$current_pred))
# })|>unlist()
# 
# ssp245$'id' <- paste0(ssp245$x|>round(5),"@",ssp245$y|>round(5))
# # FALSE %in% c(current_data$id %in% ssp126$id)
# # FALSE %in% c(ssp126$id %in% current_data$id)
# 
# ssp245 <- ssp245[current_data,on =c("id")]
# ssp245 <- ssp245[complete.cases(ssp245),]
# ssp245 <- ssp245[ssp245$mess>0,]
# (sum(ssp245$average)-sum(ssp245$current_pred))/sum(ssp245$current_pred)
# colnames(ssp245)
# 
# pred_ssp245 <- data.table(
#   ssp = rep("ssp245",12),
#   model = colnames(ssp245)[2:13],
#   overall_change = rep(0,12),
#   origin_oc = rep(-0.02470307,12)
# )
# 
# pred_ssp245$overall_change <- lapply(pred_ssp245$model,function(x){
#   cols <- x
#   return((sum(ssp245[,..cols]) - sum(ssp245$current_pred))/sum(ssp245$current_pred))
# })|>unlist()
# 
# ssp370$'id' <- paste0(ssp370$x|>round(5),"@",ssp370$y|>round(5))
# # FALSE %in% c(current_data$id %in% ssp126$id)
# # FALSE %in% c(ssp126$id %in% current_data$id)
# 
# ssp370 <- ssp370[current_data,on =c("id")]
# ssp370 <- ssp370[complete.cases(ssp370),]
# ssp370 <- ssp370[ssp370$mess>0,]
# (sum(ssp370$average)-sum(ssp370$current_pred))/sum(ssp370$current_pred)
# colnames(ssp370)
# 
# pred_ssp370 <- data.table(
#   ssp = rep("ssp370",11),
#   model = colnames(ssp370)[2:12],
#   overall_change = rep(0,11),
#   origin_oc = rep(-0.02905826,11)
# )
# 
# pred_ssp370$overall_change <- lapply(pred_ssp370$model,function(x){
#   cols <- x
#   return((sum(ssp370[,..cols]) - sum(ssp370$current_pred))/sum(ssp370$current_pred))
# })|>unlist() 
# 
# ssp585$'id' <- paste0(ssp585$x|>round(5),"@",ssp585$y|>round(5))
# # FALSE %in% c(current_data$id %in% ssp126$id)
# # FALSE %in% c(ssp126$id %in% current_data$id)
# 
# ssp585 <- ssp585[current_data,on =c("id")]
# ssp585 <- ssp585[complete.cases(ssp585),]
# ssp585 <- ssp585[ssp585$mess>0,]
# (sum(ssp585$average)-sum(ssp585$current_pred))/sum(ssp585$current_pred)
# colnames(ssp585)
# 
# pred_ssp585 <- data.table(
#   ssp = rep("ssp585",12),
#   model = colnames(ssp585)[2:13],
#   overall_change = rep(0,12),
#   origin_oc = rep(-0.03257376,12)
# )
# 
# pred_ssp585$overall_change <- lapply(pred_ssp585$model,function(x){
#   cols <- x
#   return((sum(ssp585[,..cols]) - sum(ssp585$current_pred))/sum(ssp585$current_pred))
# })|>unlist() 
# 
# # Standard error calculation: sd(x)/sqrt(length(x))
# print("ssp1226: -0.01487126")
# (pred_ssp126$overall_change |> sd())/sqrt(length(pred_ssp126$overall_change))
# print("ssp245: -0.02470307")
# (pred_ssp245$overall_change |> sd())/sqrt(length(pred_ssp245$overall_change))
# print("ssp370: -0.02905826")
# (pred_ssp370$overall_change |> sd())/sqrt(length(pred_ssp370$overall_change))
# print("ssp585: -0.03257376")
# (pred_ssp585$overall_change |> sd())/sqrt(length(pred_ssp585$overall_change))
# 
# pred_repeat <- dplyr::bind_rows(pred_ssp126,pred_ssp245,pred_ssp370,pred_ssp585)

# fwrite(pred_repeat,"./pred_repeat.csv")

# Supplementary Fig. 9
require(data.table)
require(ggplot2)
require(ggpubr)
require(patchwork)
pred_repeat <- fread("../data/pred_repeat.csv")
my_comparisons <- list( c("ssp126", "ssp245"), c("ssp126", "ssp370"), c("ssp126", "ssp585") )

n_fun <- function(x){
  return(data.frame(y = median(x)+0.025, label = paste0("n = ",length(x))))
}
p0 <- ggboxplot(pred_repeat,x = "ssp", y = "overall_change",
                color = "ssp",add =c("jitter","mean_se"))+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif",method = "wilcox.test")+ 
  stat_summary(fun.data = n_fun, geom = "text",size = 3,)+
  scale_color_manual(values = c("#20854e","#0072b5","#e18727","#bc3c29"))+
  scale_y_continuous(limits = c(-0.042,0.042),breaks = c(-0.04,-0.02,0,0.02,0.04))+
  coord_flip()+
  labs(color  = "Relative change of all GCMs")+
  theme_bw()+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y =element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())
p0

pred_error <- fread("../data/pred_repeat.csv")
# 标准误计算 sd(x)/sqrt(length(x))
pred_error <- pred_error[,"se":=sd(overall_change)/sqrt(length(overall_change)),by = ssp]




p1 <- ggplot(pred_error,aes(color = ssp))+
  geom_point(aes(x = ssp, y = origin_oc),size = 5,shape = 18)+
  geom_errorbar(aes(x = ssp, ymin = origin_oc-se, ymax = origin_oc+se),width =0.2,size = 1)+
  scale_color_manual(values = c("#20854e","#0072b5","#e18727","#bc3c29"))+
  scale_y_continuous(limits = c(-0.042,0.042),breaks = c(-0.04,-0.02,0,0.02,0.04))+
  coord_flip()+
  labs(y = "Relative change",color  = "Change with standard error")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank(),
        panel.grid = element_blank())
p1

require(patchwork)
pe1 <- (p0/p1)+patchwork::plot_layout(widths = c(1,1))
#http://127.0.0.1:36569/graphics/plot_zoom_png?width=1371&height=392
ggsave(pe1,file = "../results/Figure_s9.tif",width=8,height=6,dpi=600)

# Supplementary Fig. 7 
# code : nf_model_repeat.mhtml
require(data.table)
test <- data.table(
  model = c("All","Climatic variables","Soil properties","Human activities","Others"),
  
  performance = c(0.5195756,
                  0.5014994,
                  0.3957725,
                  0.4761556,
                  0.4785352)
)

test$model <- factor(test$model,levels = rev(c("All","Climatic variables","Soil properties","Human activities","Others")))
p <-  ggplot(test,aes(x=model, y=performance, fill = model)) +
  geom_bar(stat="identity", alpha=.6, width=.5,color = "black")+
  geom_text(aes(label = performance|>round(3),fill = NULL),
            hjust = 1.2,size= 4,color = "black")+
  scale_fill_manual(values = c("grey60","#d95b70","#ffa726","#42a5f5","#f68060"))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  labs(y = "Performance (R-squared)")+
  theme_classic()+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p
ggsave(p,file = "../results/Figure_s7.tif",width=8,height=4,dpi = 600)



