library(readxl)
library(data.table)

x <- read_excel("/home/oyang/文档/文献文件/数据复现/sourcedata/Figure 1.xlsx",sheet = "Figure1a",skip = 1)
dt <- as.data.table(x)

print(names(dt))

dt <- dt[, .(GCF, main, `anfG or vnfG`, nifB, nifD, nifE, nifH, nifK, nifN)]


long <- melt(
  dt,
  id.vars = c("GCF", "main"),
  measure.vars = c("anfG or vnfG", "nifB", "nifD", "nifE", "nifH", "nifK", "nifN"),
  variable.name = "sign",
  value.name   = "present"
)

nf_fixed <- long[present == 1]
nf_fixed[, present := NULL]
nf_fixed[, count := 1L]


head(nf_fixed)

fwrite(nf_fixed, "nf_fixed_from_excel.csv")

options(warn=-1)
require(data.table)
require(UpSetR)
# nf_fixed <- fread("../data/nf_fixed.csv")
nf_fixed <- fread("nf_fixed_from_excel.csv")
nf_fixed$'count' <- rep(1,nrow(nf_fixed))
dat2_1 <- dcast(nf_fixed,GCF~sign,fun.aggregate = sum,value.var = c("count"))

test <- unique(nf_fixed[, .(GCF, main)], by = "GCF")
dat2_1 <- dat2_1|>as.data.frame()
dat2_1_0 <- dat2_1
dat2_1_0[,c(2:8)][dat2_1_0[,c(2:8)]>0] <- 1
dat2_1_0 <- test[dat2_1_0,on = c("GCF")]
dat2_1_0$main[dat2_1_0$main == "HDK" & dat2_1_0$`anfG or vnfG`>0] <- "HDGK"
dat2_1_meta <- colSums(dat2_1[,2:8])|>data.frame()
colnames(dat2_1_meta)[1] <- "Genes" 
dat2_1_meta$'sets' <- rownames(dat2_1_meta)
dat2_1_meta <- dat2_1_meta[,c(2,1)]


p2_1 <- upset(dat2_1_0,
              sets = rev(c("nifH","nifD","nifK","nifE","nifN","nifB","anfG or vnfG")),
              #nsets = 7,
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
                  active = T,
                  query.name = "Completed Nitrogenase (nifHDK)"
                ),
                list(
                  query = elements,
                  params = list("main","H"),
                  color = "#ffa726",
                  active = T,
                  query.name = "Incompleted Nitrogenase (nifH only)"
                ),
                list(
                  query = elements,
                  params = list("main","HDGK"),
                  color = "#26c6d5",
                  active = T,
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
png("/home/oyang/文档/文献文件/数据复现/review/Figure_1a.png", width = 10, height = 5.27, units = "in", res = 600)


tryCatch(
  {
    print(p2_1)   # 可能在这里报错
  },
  error = function(e) {
    message("UpSetR 在 PNG 设备上绘图时出现已知 grid 错误，但图像很可能已经生成：")
    message(e$message)
  }
)

dev.off()

