
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
result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
# fwrite(result,"./GLM/MESS_table.csv")
# 
# require(data.table)
dat_mess <- fread("./GLM/MESS_table.csv")

## Supplementary Fig. 8 
## Multivariate environmental similarity surface 
require(RColorBrewer)
require(ggplot2)
ps7 <- ggplot(data=dat_mess,aes(x = x, y = y, fill = mess))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdYlBu",direction = 1,breaks =c(-100,-50,0,50))+
  labs(fill = 'MESS value')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.title = element_text(angle = 90,hjust=0.5),
        legend.title.position = "left"
  )+
  coord_fixed()
ps7

# ggsave(ps7,filename = "mess.png",width = 8, height = 3.82, dpi = 300)
# 
## random forest model for climate

history_dataset <- fread("glm_start_table.csv",integer64 = "numeric")
history_dataset$'nif_re_type' <- history_dataset$nif_type/history_dataset$mapped_16106_16s_type|>round(7)
history_dataset <- history_dataset[complete.cases(history_dataset),]
history_dataset[,c(6:8)] <- c()
history_dataset <- history_dataset[!history_dataset$nif_re_type %in% boxplot.stats(history_dataset$nif_re_type)$out,]
history_dataset <- history_dataset[,'mean_nif_re_type':=mean(nif_re_type),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(15,16,37,17:35)],by = c("y","x"))
history_dataset <- history_dataset[,c(1:4,15:22,5:14)]
history_dataset[,c(1:2)] <- c()
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_extract("(?<=wc2.1_5m_).*")
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_replace("_","")

# history_dataset$mean_nif_re_type <- 100*history_dataset$mean_nif_re_type
colnames(history_dataset)[2:10] <- paste0("bio0",1:9)
Data <- history_dataset
names(Data)[1] <- "Response"
hist(Data$Response)

Sys.time()

require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)
require(rsample)
num_cores <- 5
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
set.seed(123,"L'Ecuyer-CMRG")
inTrain <- initial_split(Data,prop = 2/3,strata ="Response")
data_train <- training(inTrain)
data_test <- testing(inTrain)
trainx <- data_train[,-1]
trainy <- data_train$Response
set.seed(123,"L'Ecuyer-CMRG")
control<-rfeControl(functions = rfFuncs,method = "cv",number = 10,allowParallel=TRUE)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:19),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
Sys.time()
# save(rfFuncs,file = "rffuncs_future_terra.rdata")

load("rffuncs_future_terra.rdata")
xyplot(rfFuncs$results$Rsquared~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$RMSE~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$MAE~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)
xyplot(rfFuncs$results$RMSESD~rfFuncs$results$Variables,type = c("p",'l'),auto.key = TRUE)

rfFuncs$bestSubset
optvar <- rfFuncs$optVariables
optvar
max(rfFuncs$results$Rsquared)
postResample(predict(rfFuncs, data_test[,-1]), data_test$Response)
hist(predict(rfFuncs, data_test[,-1]))
# rm(rfFuncs)

test <- rfFuncs$results[,c(1:5)]|>data.table()
test <- melt(test, id.vars = "Variables", measure.vars = colnames(test)[-1])
test$variable <- factor(test$variable,levels = c("Rsquared","RMSE","MAE","RMSESD"))
p0 <- ggplot(test,aes(x = Variables, y = value))+
  geom_point(color = "#4DBBD5B2")+
  geom_line(color = "#4DBBD5B2")+
  facet_wrap(variable~.,nrow = 2,scale="free_y")+
  theme_bw(base_size = 10)
p0
#ggsave(p0,filename = "C:/Users/crow/Desktop/p0.png",width = 8, height = 6, dpi = 300)

cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response
# fwrite(data_test,"~/assistant/data_test.csv")
# fwrite(data_train,"~/assistant/data_train.csv")

require(ggplot2)
require(caret)
require(randomForest)
require(future.apply)
require(parallel)
require(doParallel)
require(foreach)
# 300,500,700,1000,1300,1600,1900,2200,2500,3000
cl <- makeCluster(8) # 这里使用4个核心，你可以根据需要调整
registerDoParallel(cl)
for (ntree in c(300,500,700,1000,1300,1600,1900,2200,2500,3000)) {


  Sys.time()
  control <- trainControl(method="cv", number=10,allowParallel = T)
  tunegrid <- expand.grid(.mtry = 1:12)
  set.seed(1,"L'Ecuyer-CMRG")
  custom <- train(Response~.,
                  data=data_train, method="rf",
                  # metric="rmse",
                  metric = "Rsquared",importance=T,
                  tuneGrid=tunegrid,
                  ntree = ntree,
                  trControl=control)

  save(custom,file = paste0('E:/random_forest/grid/future_ntree_',ntree,".rdata"))
  Sys.time()
}
stopCluster(cl)

final_grid <- data.table()


load("E:/random_forest/grid/future_ntree_300.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(300,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("E:/random_forest/grid/future_ntree_500.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("E:/random_forest/grid/future_ntree_700.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(700,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("E:/random_forest/grid/future_ntree_1000.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("E:/random_forest/grid/future_ntree_1300.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1300,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)


load("E:/random_forest/grid/future_ntree_1600.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1600,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)


load("E:/random_forest/grid/future_ntree_1900.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1900,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)


load("E:/random_forest/grid/future_ntree_2200.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2200,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)


load("E:/random_forest/grid/future_ntree_2500.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)


load("E:/random_forest/grid/future_ntree_3000.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(3000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

final_grid$ntree <- as.character(final_grid$ntree)

final_grid$ntree[which.max(final_grid$Rsquared)]
final_grid$mtry[which.max(final_grid$Rsquared)]
final_grid$ntree[which.min(final_grid$RMSE)]
final_grid$mtry[which.min(final_grid$RMSE)]


# Supplementary Fig. 3
require(ggplot2)

p01 <- ggplot(final_grid,aes(x = mtry, y = Rsquared))+
  geom_point(aes(color=(ntree)))+
  geom_line(aes(color=ntree))+
  scale_color_manual(values = c("#E64B35B2", "#4DBBD5B2" ,"#00A087B2", "#3C5488B2", "#F39B7FB2",
                                "#7E6148B2","#d4e157","#ffa726","#66bb6a","#42a5f5","red"))+
  scale_x_continuous(breaks = seq(1,56))+
  theme_bw(base_size = 10)+
  theme(legend.position = "none")

p01

p02 <- ggplot(final_grid,aes(x = mtry, y = RMSE))+
  geom_point(aes(color=(ntree)))+
  geom_line(aes(color=ntree))+
  scale_color_manual(values = c("#E64B35B2", "#4DBBD5B2" ,"#00A087B2", "#3C5488B2", "#F39B7FB2",
                                "#7E6148B2","#d4e157","#ffa726","#66bb6a","#42a5f5","red"))+
  scale_x_continuous(breaks = seq(1:56))+
  theme_bw(base_size = 10)

p02


require(randomForest)
load("E:/random_forest/grid/future_ntree_3000.rdata")
custom$finalModel

predictRF <- predict(custom$finalModel,newdata = data_test)
rsq <- function(x, y) summary(lm(y~x))$r.squared
R2(data_train$Response, custom[["finalModel"]][["predicted"]])
rsq(data_test$Response, predictRF)
R2(data_test$Response, predictRF)

# predict
data_train$Predict <- predict(custom$finalModel,newdata = data_train[,-1])
cor.test(data_train$Response,data_train$Predict)
yardstick::rmse(data_train,truth=Response,estimate=Predict)

library(scales)
library(ggpmisc)
p03 <- ggplot(data_train,aes(Response,Predict))+
  geom_point(aes(Response,Predict),color="grey80",size=2)+
  geom_smooth(aes(Response,Predict),method = "lm",
              fill="lightblue",color="black",linewidth=0.8)+
  stat_correlation(mapping = use_label(c("R","P","n","method")),
                   small.r = T,small.p = T,size=4,r.digits = 3)+
  scale_x_continuous(n.breaks = 7,labels = label_percent())+
  scale_y_continuous(n.breaks = 5,labels = label_percent())+
  labs(x="Observed relative richness in train dataset",
       y="Predicted relative richness in train dataset"
       #,title = "Cross-validation on the abundance-climate model"
  )+
  theme_bw(base_size = 10)
p03

# predict
data_test$Predict <- predict(custom$finalModel,newdata = data_test[,-1])
cor.test(data_test$Response,data_test$Predict)
yardstick::rmse(data_test,truth=Response,estimate=Predict)

p04 <- ggplot(data_test,aes(Response,Predict))+
  geom_point(aes(Response,Predict),color="grey80",size=2)+
  geom_smooth(aes(Response,Predict),method = "lm",
              fill="lightblue",color="black",linewidth=0.8)+
  stat_correlation(mapping = use_label(c("R","P","n","method")),
                   small.r = T,small.p = T,size=4,r.digits = 3)+
  scale_x_continuous(n.breaks = 7,labels = label_percent())+
  scale_y_continuous(n.breaks = 5,labels = label_percent())+
  labs(x="Observed relative richness in test dataset",
       y="Predicted relative richness in test dataset"
       #,title = "Cross-validation on the abundance-climate model"
  )+
  theme_bw(base_size = 10)
p04

require(patchwork)
ps8 <- (p0|(p01+p02))/(p03|p04)
ps8

# final_model
fit3 <- custom$finalModel
set.seed(1,"L'Ecuyer-CMRG")
current_data <-  dir('Z:/future_clim_part/clim_1970_2000/wc2.1_5m_bio/',full.names = TRUE)|>raster::stack()
current_data <- current_data|>terra::as.data.frame(xy =TRUE)
current_data <- current_data[complete.cases(current_data),]
colnames(current_data)[3:21] <- colnames(current_data)[3:21]|>stringr::str_extract("(?<=wc2.1_5m_).*")|>stringr::str_replace("_","")
colnames(current_data)[c(3,14:21)] <- paste0("bio0",1:9)
current_data$'current_pred' <- predict(fit3,newdata=current_data)
current_data <- current_data[,c(1:2,22)]
current_data$'id' <- paste0(current_data$x,'@',current_data$y)
current_data <- data.table(current_data)
current_data$'id' <- paste0(current_data$x|>round(5),"@",current_data$y|>round(5))
dat_mess$'id' <- paste0(dat_mess$x|>round(5),"@",dat_mess$y|>round(5))
FALSE %in% c(dat_mess$id %in% current_data$id)
FALSE %in% c(current_data$id %in% dat_mess$id)
current_data <- dat_mess[current_data[,c(3,4)],on =c("id")]
current_data <- current_data[complete.cases(current_data),]
current_data <- current_data[current_data$mess>0,]

# fwrite(current_data,"~/current_data.csv,row.names =  F)
