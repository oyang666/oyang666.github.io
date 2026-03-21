# figure1c barplot

require(data.table)
require(ggplot2)
require(patchwork)
require(readxl)  #新加代码
options(warn=-1)
#../data/
#../results/
file_path <- "/home/oyang/文档/文献文件/数据复现/sourcedata/Figure 1.xlsx"
test <- as.data.table(read_excel(file_path, sheet = "Figure1c"))
# 替换的路径代码 test <- fread("../data/nif_genome_information.csv",integer64 = "numeric")
test <- test[,'total_count':=length(unique(GCF)),by = c("phylum")]
test <- test[main == "HDK",]
test <- test[,'nif_count':=length(unique(GCF)),by = c("phylum")]
test <- test[,'evid_count':=length(unique(GCF[evid == "Yes"])),by = c("phylum")]
print(names(test))

test$'no_evid_count' <- test$nif_count - test$evid_count
test <- unique(test[,c(11,22:25)],by =c("phylum"))
test$'nif_percent' <- test$nif_count/test$total_count
test$'no_evid_percent' <- test$no_evid_count/test$total_count
test$'evid_percent' <- test$evid_count/test$total_count


test0 <- melt(test[-22,c(1,3,6)],id.vars = "phylum",
              variable.name="measure", value.name="value")
test0$'type' <- ifelse(test0$measure == "nif_count","abs","per")
test0$'label' <- ifelse(test0$type == "abs", test0$value, paste0(as.character(test0$value|>round(4)*100),'%'))
# test0$label <- paste0("total = ",test0$label)

test1 <- melt(test[-22,-c(2:3,6)],id.vars = "phylum",
              variable.name="measure", value.name="value")
test1$'type' <- ifelse(test1$measure %in% c("no_evid_count","evid_count"),"abs","per")
test1$'level' <- ifelse(test1$measure %in% c("no_evid_count","no_evid_percent"),"no_evid","evid")
test1$level <- factor(test1$level,levels = c("no_evid","evid"))
# test1$'label' <- ifelse(test1$type == "abs",test1$value,
#                        paste0(as.character(test1$value|>round(4)*100),'%'))
test1$phylum <- factor(test1$phylum,
                       levels = rev(c("Pseudomonadota","Bacillota",
                                      "Thermodesulfobacteriota","Cyanobacteriota",
                                      "Euryarchaeota","Bacteroidota",
                                      "Campylobacterota","Chlorobiota",
                                      "Actinomycetota","Verrucomicrobiota",
                                      "Spirochaetota","Nitrospirota",
                                      "Chloroflexota","Deferribacterota",
                                      "Planctomycetota","Chrysiogenota",
                                      "Kiritimatiellota","Candidatus Thermoplasmatota",
                                      "Aquificota","Acidobacteriota","Fusobacteriota")))


p3_1 <- ggplot(test1[test1$type == "abs",], aes(
  x = phylum, 
  y = ifelse(type == "abs", value, value*650),
  fill = level))+
  geom_bar(stat = 'identity',position = "stack")+
  geom_text(data = test0[test0$type== "abs",],aes(label = label,fill = NULL),
            hjust = -0.3,size= 3,color = "black")+
  coord_flip()+
  facet_grid(.~type)+
  scale_fill_manual(values = c("#74b1df","#7594a9"))+
  scale_y_continuous(                                         
    labels = abs,                                             
    expand = expansion(mult = c(0, 0.1)))+
  theme_classic()+
  theme(axis.title = element_blank(),
        panel.grid=element_blank(),
        strip.background.x = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"),
        strip.placement = "outside",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "italic",
                                   hjust = .55),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.05),
        plot.margin = margin(t = 10,  
                             r = 10,  
                             b = 10,  
                             l = 0)
  )


p3_2 <- ggplot(test1[test1$type != "abs",], aes(
  y = phylum, 
  x = ifelse(type == "abs", value, value),
  fill = level))+
  geom_bar(stat = 'identity',position = "stack")+
  geom_text(data = test0[test0$type!= "abs",],aes(label = label,fill = NULL),
            hjust = 1.3,size= 3,color = "black")+
  scale_fill_manual(values = c("#74b1df","#7594a9"))+
  scale_y_discrete(position = "right")+
  scale_x_reverse(                                         
    labels = abs,
    expand = expansion(mult = c(0.1, 0)))+
  facet_grid(.~type)+
  theme_classic()+
  theme(axis.title = element_blank(),
        panel.grid=element_blank(),
        strip.background.x = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"),
        strip.placement = "outside",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 10,  
                             r = 0,  
                             b = 10,  
                             l = 10)
  )

# require(patchwork) ../results/
p <- p3_2|p3_1
p
#p<-cowplot::plot_grid(p3_2, NULL, p3_1, rel_widths = c(1, -0.2, 1), align = "hv",
#                          nrow = 1)
# 11;5.8

png("../results/Figure_1c.png", width = 11, height = 5.8, units = "in", res = 600)
print(p) 
dev.off() 
