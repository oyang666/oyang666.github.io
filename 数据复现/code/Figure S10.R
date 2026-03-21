

load("rffuncs_future_terra.rdata")
xyplot(rfFuncs$results$Rsquared~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$RMSE~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$MAE~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$RMSESD~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)

# 返回最优Rsquared的特征数量
rfFuncs$bestSubset
optvar <- rfFuncs$optVariables
optvar
max(rfFuncs$results$Rsquared)

test <- rfFuncs$results[,c(1:5)]|>data.table()
test <- melt(test, id.vars = "Variables", measure.vars = colnames(test)[-1])
test$variable <- factor(test$variable,levels = c("Rsquared","RMSE","MAE","RMSESD"))
p0 <- ggplot(test,aes(x = Variables, y = value))+
  geom_point(color = "#4DBBD5B2")+
  geom_line(color = "#4DBBD5B2")+
  facet_wrap(variable~.,nrow = 2,scale="free_y")+
  theme_bw(base_size = 10)
p0