library(readxl)
library(data.table)
library(UpSetR)


# 读 Figure1a_main 这张表；若文件只有一个 sheet，也可以不写 sheet=
# 视你的文件结构而定，如果前两行是 "Figure1a_main" 和 "Figure1a_annotation" 就跳过
file <- "/home/oyang/文档/文献文件/数据复现/sourcedata/Figure 1.xlsx"
x <- read_excel(file, skip = 1,sheet = "Figure1c")
dt <- as.data.table(x)

# 看一下列名，确认有这 些：
# superkingdom, phylum, class, order, family, genus, main,
# evid, `anfG or vnfG`, nifB, nifD, nifE, nifH, nifK, nifN, sets, genes
print(names(dt))

# 选出 UpSet 用到的列（main + 各 nif/anf/vnf 基因）
dat2_1_0 <- dt[, .(main,`anfG or vnfG`, nifB, nifD, nifE, nifH, nifK, nifN)]

# 确保是 0/1（有些可能本来就是 numeric/0/1，这一步是保险）
gene_cols <- c("anfG or vnfG", "nifB","nifD","nifE","nifH","nifK","nifN")
dat2_1_0[, (gene_cols) := lapply(.SD, function(x) as.integer(x > 0)), .SDcols = gene_cols]

# 对应原代码这句：
# dat2_1_0$main[dat2_1_0$main == "HDK" & dat2_1_0$`anfG or vnfG`>0] <- "HDGK"
dat2_1_0[main == "HDK" & `anfG or vnfG` > 0, main := "HDGK"]

# 生成 set.metadata 需要的统计（每个基因出现的基因组数）
dat2_1_meta <- colSums(dat2_1_0[, ..gene_cols])
dat2_1_meta <- data.frame(
  sets  = names(dat2_1_meta),
  Genes = as.integer(dat2_1_meta)
)

# 按照原脚本的配色和参数调用 UpSetR
p2_1 <- upset(dat2_1_0,
              sets = rev(c("nifH","nifD","nifK","nifE","nifN","nifB","anfG or vnfG")),
              order.by = c("degree", "freq"),
              decreasing = c(TRUE, TRUE),
              keep.order = TRUE,
              sets.x.label = "Genomes with any nif gene",
              query.legend = "top", 
              point.size = 3,
              line.size = 1,
              mb.ratio = c(0.7,0.3),
              mainbar.y.label = "Genomes",
              sets.bar.color = rev(c("#ffa726",'#42a5f5','#42a5f5',
                                     'grey50','grey50','grey50',"#26c6d5")),
              queries = list(
                list(
                  query = elements,
                  params = list("main","HDK"),
                  color = "#42a5f5",
                  active = TRUE,
                  query.name = "Completed Nitrogenase (nifHDK)"
                ),
                list(
                  query = elements,
                  params = list("main","H"),
                  color = "#ffa726",
                  active = TRUE,
                  query.name = "Incompleted Nitrogenase (nifH only)"
                ),
                list(
                  query = elements,
                  params = list("main","HDGK"),
                  color = "#26c6d5",
                  active = TRUE,
                  query.name = "Incompleted Nitrogenase (anf or vnf)"
                )
              ),
              set.metadata = list(
                data = dat2_1_meta, 
                plots = list(
                  list(
                    type = "hist", 
                    column = "Genes", 
                    colors = rev(c("#ffa726",'#42a5f5','#42a5f5',
                                   'grey50','grey50','grey50',"#26c6d5")),
                    assign = 20
                  ), 
                  list(
                    type = "matrix_rows", 
                    column = "sets", 
                    colors = c(
                      nifH = "#ffa726", 
                      nifD = '#42a5f5',
                      nifK = '#42a5f5',
                      nifE = 'grey50',
                      nifN = 'grey50',
                      nifB = 'grey50',
                      `anfG or vnfG` = "#26c6d5"
                    ),
                    alpha = 0.5
                  )
                )
              )
)

png("Figure_1a_reproduce_from_xlsx.png", width = 10, height = 5.27, units = "in", res = 600)
print(p2_1)
dev.off()
