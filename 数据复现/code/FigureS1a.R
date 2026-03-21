
library(data.table)
library(ggplot2)
library(ggsankey)
table_all <- fread("../data/sample_summary.csv")
nrow(table_all)
test <- table_all
test2 <-test[,.(env_00 = unlist(strsplit(envir_1, split = '\\|'))),by = sample_name]
test <- test[test2,on = c("sample_name")]
test$'env_type' <- ifelse(grepl("\\|",test$envir_1),"overlap","parallel")
test$'env_01' <- test$env_00|>stringr::str_extract("\\w*")
test$'env_02' <- test$env_00|>stringr::str_extract("(?<=;)\\w*")
test$env_02[grepl("sea|ocean|marine",test$env_02)] <- "sea"
test$env_02[grepl("brine",test$env_02)] <- "others"
test$env_02[is.na(test$env_02)] <- "others"
test <- test[,c(1,164,163,165:166,2:162)]
test$env_02[test$env_02 == "others"] <- paste0(test$env_01[test$env_02 == "others"],".others")
rm(test2,table_all);gc()

test$'abd_lati' <- ifelse(abs(test$lat)>=60,"high latitude",ifelse(abs(test$lat)>=30,"medium latitude","low latitude"))

# shurb
test2 <- test[!grepl("other",env_02),]
# test2 <- test2[env_01!="plant",]
test2$env_02[test2$env_02 == "sea"] <- "marine"

test2 <- test2[env_02 %in% c("sediment","marine","river","lake",
                             "waste","estuary","groundwater","reservoir","ice",
                             "field","forest","farm","agricultural",
                             "peatland","paddy","desert","tundra","shrub"),]

dat2 <- test2[,c(4,167,5)] |>
  make_long(env_01,abd_lati,env_02)

dat2$node <- factor(dat2$node,levels = c(
  "aquatic","soil",
  "low latitude","medium latitude","high latitude",
  "sediment","marine","river","lake",
  "waste","estuary","groundwater","reservoir","ice",
  "field","forest","farm","agricultural",
  "peatland","paddy","desert","tundra","shrub"
))

p1_1 <- ggplot(dat2, aes(x = x, 
                         next_x = next_x, 
                         node = node, 
                         next_node = next_node,
                         fill = factor(node),
                         label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  # geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  geom_sankey_label(aes(label = node), size = 3,color = 1, fill = "white",position = position_nudge(x = 0.3)) +
  scale_fill_manual(values = c(
    "#81afd2","#e7d185",
    "#ffa726","#d4e157","#b7cdef",
    c(rep("#81afd2",9)),
    rep("#e7d185",9)
  ))+
  theme_sankey(base_size = 16) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5)) 
p1_1 

ggsave(p1_1,filename = "Figure S1a.pdf",width = 6, height = 8, dpi = 300)

