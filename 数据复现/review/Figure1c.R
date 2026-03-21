# 加载必要的包
library(data.table)
library(ggplot2)
library(patchwork)
library(readxl)

options(warn = -1)

# 读取数据
file_path <- "/home/oyang/文档/文献文件/数据复现/sourcedata/Figure 1.xlsx"
dt_raw <- as.data.table(read_excel(file_path, sheet = "Figure1c"))

# 查看数据结构和 measure 有哪些值
print(names(dt_raw))
print(unique(dt_raw$measure))

# ---------- 构建 test0（用于总标签：nif_count 和 nif_percent）----------
# 假设 measure 中包含 "nif_count" 和 "nif_percent"
test0 <- dt_raw[measure %in% c("nif_count", "nif_percent")]
# 添加 type 和 label 列
test0[, type := ifelse(measure == "nif_count", "abs", "per")]
test0[, label := ifelse(type == "abs", 
                        as.character(value), 
                        paste0(as.character(round(value, 4) * 100), "%"))]

# ---------- 构建 test1（用于堆叠条形图：no_evid_count, evid_count, no_evid_percent, evid_percent）----------
test1 <- dt_raw[measure %in% c("no_evid_count", "evid_count", "no_evid_percent", "evid_percent")]
test1[, type := ifelse(measure %in% c("no_evid_count", "evid_count"), "abs", "per")]
test1[, level := ifelse(measure %in% c("no_evid_count", "no_evid_percent"), "no_evid", "evid")]
test1[, level := factor(level, levels = c("no_evid", "evid"))]
phylum_levels <- c("Pseudomonadota", "Bacillota",
                   "Thermodesulfobacteriota", "Cyanobacteriota",
                   "Euryarchaeota", "Bacteroidota",
                   "Campylobacterota", "Chlorobiota",
                   "Actinomycetota", "Verrucomicrobiota",
                   "Spirochaetota", "Nitrospirota",
                   "Chloroflexota", "Deferribacterota",
                   "Planctomycetota", "Chrysiogenota",
                   "Kiritimatiellota", "Candidatus Thermoplasmatota",
                   "Aquificota", "Acidobacteriota", "Fusobacteriota")
# 反转顺序（因为 coord_flip 后希望从上到下按列表顺序）
test0[, phylum := factor(phylum, levels = rev(phylum_levels))]
test1[, phylum := factor(phylum, levels = rev(phylum_levels))]

# ---------- 绘图 ----------
p3_1 <- ggplot(test1[type == "abs", ], aes(x = phylum, y = value, fill = level)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = test0[type == "abs", ], aes(label = label, fill = NULL),
            hjust = -0.5, size = 3, color = "black") +
  coord_flip() +facet_grid(. ~ type, 
                           labeller = labeller(type = c(abs = "Nitrogen-fixing genomes", 
                                                        per = "Percentage"))) +
  scale_fill_manual(name = "yes",values = c("#74b1df", "#7594a9")) +
  scale_y_continuous(labels = abs, expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background.x = element_rect(color = "white", fill = "white", size = 1.5, linetype = "solid"),
        strip.placement = "outside",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "italic", hjust = 0.55),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.05),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0))

p3_2 <- ggplot(test1[type != "abs", ], aes(y = phylum, x = value, fill = level)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = test0[type != "abs", ], aes(label = label, fill = NULL),
            hjust = 1.3, size = 3, color = "black") +
  scale_fill_manual(name = "No",values = c("#74b1df", "#7594a9")) +
  scale_y_discrete(position = "right") +
  scale_x_reverse(labels = abs, expand = expansion(mult = c(0.1, 0))) +
  facet_grid(. ~ type, 
             labeller = labeller(type = c(abs = "Nitrogen-fixing genomes", 
                                          per = "Percentage"))) +
  theme_classic() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background.x = element_rect(color = "white", fill = "white", size = 1.5, linetype = "solid"),
        strip.placement = "outside",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 0, b = 10, l = 10))

p <- p3_2 | p3_1

# 保存图片
png("/home/oyang/文档/文献文件/数据复现/review/Figure_1c.png", width = 11, height = 5.8, units = "in", res = 600)
print(p)
dev.off()