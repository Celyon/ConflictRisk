##Step1##
##train BRT models using one-year sample under strategy b##
##Author:Fangyu Ding##
##Time:20200716##
##Run Time: approximately 18 hours##
rm(list=ls());
library(caret);
library(ggplot2);
library(nnet);
library(e1071);
library(ROCR)
library(RColorBrewer)
library(MLmetrics) #PRAUC
require(ggthemes)
require(coin)
require(plotrix)
library(dismo)
library(gbm)

years <- c(2000:2015)

timestart<-Sys.time();

filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\BackgroundFactors_revisedversion.csv",sep='')
dataBF <- read.csv(filename,header=T,encoding="utf-8")
names(dataBF) <- c("nrow","ncol","Elevation","NighttimeLight","UrbanAccessibility","EthnicDiversity","NDH","Precipitation","scPDSI","Temperature","NDVI","Mask")
temp_data <- dataBF[,6]
dataBF$EthnicDiversity <- as.factor(temp_data)
dataBF$Mask[dataBF$Mask == 255] <- 0

filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Raster_allconflict_Singleyear.csv",sep='')
##load conflict dataset from 2000 to 2015 years
dataRisk <- read.csv(filename,header=F,encoding="utf-8")
names(dataRisk) <- c("nrow","ncol","Conflict2000","Conflict2001","Conflict2002","Conflict2003","Conflict2004","Conflict2005","Conflict2006","Conflict2007","Conflict2008","Conflict2009","Conflict2010","Conflict2011","Conflict2012","Conflict2013","Conflict2014","Conflict2015")
dataRisk$Conflict2000[dataRisk$Conflict2000 == 9999] <- 0
dataRisk$Conflict2001[dataRisk$Conflict2001 == 9999] <- 0
dataRisk$Conflict2002[dataRisk$Conflict2002 == 9999] <- 0
dataRisk$Conflict2003[dataRisk$Conflict2003 == 9999] <- 0
dataRisk$Conflict2004[dataRisk$Conflict2004 == 9999] <- 0
dataRisk$Conflict2005[dataRisk$Conflict2005 == 9999] <- 0
dataRisk$Conflict2006[dataRisk$Conflict2006 == 9999] <- 0
dataRisk$Conflict2007[dataRisk$Conflict2007 == 9999] <- 0
dataRisk$Conflict2008[dataRisk$Conflict2008 == 9999] <- 0
dataRisk$Conflict2009[dataRisk$Conflict2009 == 9999] <- 0
dataRisk$Conflict2010[dataRisk$Conflict2010 == 9999] <- 0
dataRisk$Conflict2011[dataRisk$Conflict2011 == 9999] <- 0
dataRisk$Conflict2012[dataRisk$Conflict2012 == 9999] <- 0
dataRisk$Conflict2013[dataRisk$Conflict2013 == 9999] <- 0
dataRisk$Conflict2014[dataRisk$Conflict2014 == 9999] <- 0
dataRisk$Conflict2015[dataRisk$Conflict2015 == 9999] <- 0

summary(dataRisk)

p <- 1
for(p in 1:length(years)){
  tempyear = years[p]
  filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Precipitation\\12MonthsSD\\",years[p],".csv",sep='')
  ##load SPI12 dataset
  dataPrecipitation <- read.csv(filename,header=T,encoding="utf-8")
  names(dataPrecipitation) <- c("nrow","ncol","SPI12")
  
  filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Temperature\\12MonthsSD\\",years[p],".csv",sep='')
  ##load STI12 dataset
  dataTemperature <- read.csv(filename,header=T,encoding="utf-8")
  names(dataTemperature) <- c("nrow","ncol","STI12")
  
  Alldata <- merge(dataPrecipitation,dataTemperature, by= c("nrow","ncol"))
  
  Alldata <- merge(Alldata,dataBF, by= c("nrow","ncol"))
  
  Alldata <- merge(Alldata,dataRisk, by= c("nrow","ncol"))
  
  Alldata<- subset(Alldata,Elevation < 10000)
  Alldata<- subset(Alldata,NighttimeLight < 100)
  Alldata<- subset(Alldata,UrbanAccessibility > -10)
  Alldata<- subset(Alldata,Temperature > -100)
  Alldata<- subset(Alldata,NDVI > -1000)
  
  Alldata<- subset(Alldata,SPI12 > -100)
  Alldata<- subset(Alldata,STI12 > -100)
  
  if(p == 1){
    Alldata$Risk <- Alldata$Conflict2000
  }  else if(p == 2){
    Alldata$Risk <- Alldata$Conflict2001
  } else if(p == 3){
    Alldata$Risk <- Alldata$Conflict2002
  } else if(p == 4){
    Alldata$Risk <- Alldata$Conflict2003
  } else if(p == 5){
    Alldata$Risk <- Alldata$Conflict2004
  } else if(p == 6){
    Alldata$Risk <- Alldata$Conflict2005
  } else if(p == 7){
    Alldata$Risk <- Alldata$Conflict2006
  } else if(p == 8){
    Alldata$Risk <- Alldata$Conflict2007
  } else if(p == 9){
    Alldata$Risk <- Alldata$Conflict2008
  } else if(p == 10){
    Alldata$Risk <- Alldata$Conflict2009
  } else if(p == 11){
    Alldata$Risk <- Alldata$Conflict2010
  } else if(p == 12){
    Alldata$Risk <- Alldata$Conflict2011
  } else if(p == 13){
    Alldata$Risk <- Alldata$Conflict2012
  } else if(p == 14){
    Alldata$Risk <- Alldata$Conflict2013
  } else if(p == 15){
    Alldata$Risk <- Alldata$Conflict2014
  } else if(p == 16){
    Alldata$Risk <- Alldata$Conflict2015
  } 
  Alldata <- subset( Alldata, select = -Conflict2015 )
  Alldata <- subset( Alldata, select = -Conflict2014 )
  Alldata <- subset( Alldata, select = -Conflict2013 )
  Alldata <- subset( Alldata, select = -Conflict2012 )
  Alldata <- subset( Alldata, select = -Conflict2011 )
  Alldata <- subset( Alldata, select = -Conflict2010 )
  Alldata <- subset( Alldata, select = -Conflict2009 )
  Alldata <- subset( Alldata, select = -Conflict2008 )
  Alldata <- subset( Alldata, select = -Conflict2007 )
  Alldata <- subset( Alldata, select = -Conflict2006 )
  Alldata <- subset( Alldata, select = -Conflict2005 )
  Alldata <- subset( Alldata, select = -Conflict2004 )
  Alldata <- subset( Alldata, select = -Conflict2003 )
  Alldata <- subset( Alldata, select = -Conflict2002 )
  Alldata <- subset( Alldata, select = -Conflict2001 )
  Alldata <- subset( Alldata, select = -Conflict2000 )
  
  Alldata <- subset( Alldata, select = -Elevation )
  Alldata <- subset( Alldata, select = -NighttimeLight )
  Alldata <- subset( Alldata, select = -UrbanAccessibility )
  Alldata <- subset( Alldata, select = -EthnicDiversity )
  Alldata <- subset( Alldata, select = -NDH )
  Alldata <- subset( Alldata, select = -scPDSI )
  Alldata <- subset( Alldata, select = -NDVI )
  
  Accuracy_data <- c()
  
  copy_Alldata <- Alldata[,1:2]
  temp_Alldata <- Alldata[,3:8]
  Alldata <- temp_Alldata
  i = 1
  for(i in 1 : 20 ){
    set.seed(i)
    
    
    Conflict_data_all<- subset(Alldata,Risk == 1 & Mask == 1)
    noConflict_data_all<- subset(Alldata,Risk != 1 & Mask == 1)
    
    rows_Conflict <- nrow(Conflict_data_all)
    rows_noConflict <- nrow(noConflict_data_all)
    proportion <- rows_Conflict/rows_noConflict
    
    samples_noConflict_size <- round(rows_noConflict*proportion);
    selectIndex <- sample(rows_noConflict, size=samples_noConflict_size);
    samples_noConflict_data <- noConflict_data_all[selectIndex, ]
    
    Conflict_data=rbind(Conflict_data_all,samples_noConflict_data)
    summary(Conflict_data)
    
    
    train.flag <- createDataPartition(y=Conflict_data$Risk,p=1,list=FALSE)
    training <- Conflict_data[train.flag,]
    
    
    filename=paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\",years[p],"_",i,"_StrategyB_12M_samples.csv",sep='')
    write.csv(training, file=filename,row.names = FALSE,col.names = FALSE,quote = F)
    
    tempdata <- training
    
    ####Collinearity test####
    tempdata <- subset(tempdata, select = -Mask)
    library(car)
    lm1 <- lm(Risk ~ ., data = tempdata)
    
    summary(lm1)
    vif(lm1, digits = 3)
    ####Collinearity test end####
    
    BRT_model <- gbm.step(data=training, gbm.x = 1:4, gbm.y = 6, tree.complexity = 4,learning.rate = 0.01, bag.fraction = 0.75, step.size = 10, cv.folds = 10, max.trees = 10000)
    
    pred <- predict.gbm(BRT_model, training, n.trees=BRT_model$n.trees, "response")
    pred <- data.frame(pred)
    names(pred) <- c("risk")
    ggplot(pred, aes(x=risk)) + geom_bar(stat="bin")
    
    ###PR-AUC
    PR_AUC<-PRAUC(y_pred = pred, y_true = training$Risk)
    ###F1 score
    F1_pred <- ifelse(pred < 0.5, 0, 1)
    F1_score1 <- F1_Score(y_pred = F1_pred, y_true =training$Risk, positive = "1")
    
    temp_auc_training <- BRT_model$cv.statistics$discrimination.mean
    
    temp_accuracy_data <- c()
    temp_accuracy_data <- cbind(temp_auc_training,PR_AUC,F1_score1)
    Accuracy_data <- rbind(Accuracy_data,temp_accuracy_data)
    
    modelname<-paste("E:\\Conflict\\Model\\",years[p],"_",i,"_BRT_StrategyB_12M.Rdata",sep="")
    
    save(BRT_model, file = modelname)
    
    pred <-  predict.gbm(BRT_model, Alldata, n.trees=BRT_model$n.trees, "response")
    
    copy_Alldata$Risk <- pred
    simulate_filename <- paste("E:\\Conflict\\Tables\\Simulation\\StrategyB_12M_ConflictRisk_simulated_",years[p],"_",i,".csv",sep="")
    write.csv(copy_Alldata, file=simulate_filename,row.names=F)
  }
  filename<-paste("E:\\Conflict\\Tables\\Accuracy\\StrategyB_12M_accuracy_",years[p],".csv",sep="")
  write.csv(Accuracy_data, file=filename)
}

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
###End###



##Step2##
##Time-cross validation method##
##Author:Fangyu Ding##
##Time:20200716##
##Evaluate the performance of BRT models at time scale##
##Run Time: approximately 2 hours##
rm(list=ls());
library(ROCR)
library(dismo)
library(gbm)
library(MLmetrics) #PRAUC
timestart<-Sys.time();
for(ModelYear in 2000:2015){
  Accuracy_data <- c()
  for(SampleYear in 2000:2015){
    for(Modelorder in 1:20){
      modelname<-paste("E:\\Conflict\\Model\\",ModelYear,"_",Modelorder,"_BRT_StrategyB_12M.Rdata",sep="")
      load(modelname)
      
      filename=paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\",SampleYear,"_",Modelorder,"_StrategyB_12M_samples.csv",sep='')
      ##load background dataset
      tempsamples <- read.csv(filename,header=T,encoding="utf-8")
      
      ##Calculate ROC-AUC##
      probs <- predict.gbm(BRT_model, tempsamples, n.trees=BRT_model$n.trees, "response")
      isPositiveClass <- tempsamples[,6] == 1
      pred <- prediction(probs, isPositiveClass)
      perf_training <- performance(pred, 'tpr', 'fpr')
      auc_training <- performance(pred,"auc")
      temp_auc <- as.integer((attributes(auc_training)$y.value[[1]]*1000))/1000
      
      ###PR-AUC
      PR_AUC<-PRAUC(y_pred = probs, y_true = tempsamples$Risk)
      ###F1 score
      F1_pred <- ifelse(probs < 0.5, 0, 1)
      F1_score1 <- F1_Score(y_pred = F1_pred, y_true =tempsamples$Risk, positive = "1")
      
      temp_accuracy_data <- c()
      temp_accuracy_data <- c(ModelYear,SampleYear,Modelorder,temp_auc,PR_AUC,F1_score1)
      Accuracy_data <- rbind(Accuracy_data,temp_accuracy_data)
    }
  }
  save_file<-paste("E:\\Conflict\\Tables\\Performace\\StrategyB_12M_accuracy_",ModelYear,".csv",sep="")
  write.csv(Accuracy_data, file = save_file)
}
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)

##Merge the time-cross validation result##
filename=paste("E:\\Conflict\\Tables\\Performace\\StrategyB_12M_accuracy_",2000,".csv",sep="")
##load background dataset
data <- read.csv(filename,header=T,encoding="utf-8")
for(ModelYear in 2001:2015){
  filename=paste("E:\\Conflict\\Tables\\Performace\\StrategyB_12M_accuracy_",ModelYear,".csv",sep="")
  
  tempdata <- read.csv(filename,header=T,encoding="utf-8")
  data <- rbind(data, tempdata)
}
data <- subset(data, select = -X)
write.csv(data, file = "E:\\Conflict\\Tables\\Performace\\StrategyB_12M_accuracy.csv")

##Calculate 95%CI ##
data <- read.csv(file = "E:\\Conflict\\Tables\\Performace\\StrategyB_12M_accuracy.csv",header=T,encoding="utf-8")
Accuracy_data <- c()
for(ModelYear in 2000:2015){
  for(SampleYear in 2000:2015){
    tempdata<- subset(data,V1 == ModelYear & V2 == SampleYear)
    
    ##ROC-AUC
    temp_auc <- tempdata[,5]
    sd_auc <- sd(temp_auc)
    auc<- t.test(temp_auc)
    auc_95CIlow <- auc$conf.int[1]
    auc_95CItop <- auc$conf.int[2]
    auc_mean <- auc$estimate
    
    ##PR-AUC
    temp_prauc <- tempdata[,6]
    sd_prauc <- sd(temp_prauc)
    prauc<- t.test(temp_prauc)
    prauc_95CIlow <- prauc$conf.int[1]
    prauc_95CItop <- prauc$conf.int[2]
    prauc_mean <- prauc$estimate
    
    ##F1-score
    temp_f1score1 <- tempdata[,7]
    sd_f1score1 <- sd(temp_f1score1)
    f1score1<- t.test(temp_f1score1)
    f1score1_95CIlow <- f1score1$conf.int[1]
    f1score1_95CItop <- f1score1$conf.int[2]
    f1score1_mean <- f1score1$estimate
    
    temp_accuracy_data <- c()
    temp_accuracy_data <-
      c(ModelYear,SampleYear, auc_mean, sd_auc,auc_95CIlow,auc_95CItop,prauc_mean,sd_prauc,prauc_95CIlow,prauc_95CItop,f1score1_mean,sd_f1score1,f1score1_95CIlow,f1score1_95CItop)
    
    Accuracy_data <- rbind(Accuracy_data,temp_accuracy_data)
  }
}
Accuracy_data <- data.frame(Accuracy_data)
names(Accuracy_data) <- c("ModelYear","SampleYear","Mean-ROCAUC","Sd-ROCAUC","95CIlow-ROCAUC","95CItop-ROCAUC","Mean-PRAUC","Sd-PRAUC","95CIlow-PRAUC","95CItop-PRAUC","Mean-F1score","Sd-F1score","95CIlow-F1score","95CItop-F1score")
###For strategy b, Generate the data source of Supplementary Fig. 3 and Supplementary Table 1###
write.csv(Accuracy_data, file="E:\\Conflict\\Tables\\Performace\\Performance_Timecorss_StrategyB_12M.csv",row.names = FALSE,col.names = FALSE,quote = F)
###End###


##Step3##
##train BRT models using the merged all samples (2000-2015) under strategy b##
##Author:Fangyu Ding##
##Time:20200716##
##Run Time: approximately 22 hours##
##Increase sample size and avoid the model skew to the single-year sample##
rm(list=ls());
library(caret);
library(ggplot2);
library(nnet);
library(e1071);
library(ROCR)
library(RColorBrewer)
library(MLmetrics)
require(ggthemes)
require(coin)
require(plotrix)
library(dismo)
library(gbm)

timestart<-Sys.time();
for(order in 1:20){
  filename=paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\2000_",order,"_StrategyB_12M_samples.csv",sep="")
  ##load background dataset
  data <- read.csv(filename,header=T,encoding="utf-8")
  for(ModelYear in 2001:2015){
    filename=paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\",ModelYear,"_",order,"_StrategyB_12M_samples.csv",sep="")
    tempdata <- read.csv(filename,header=T,encoding="utf-8")
    data <- rbind(data, tempdata)
  }
  filename=paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyB_12M_AllSamples_",order,".csv",sep="")
  write.csv(data, file = filename,row.names=F)
}


i = 1
Accuracy_data <- c()
for(i in 1 : 20 ){
  set.seed(i)
  
  filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyB_12M_AllSamples_",i,".csv",sep="")
  Alldata <- read.csv(filename,header=T,encoding="utf-8")  
  
  
  train.flag <- createDataPartition(y=Alldata$Risk,p=1,list=FALSE)
  training <- Alldata[train.flag,]
  
  BRT_model <- gbm.step(data=training, gbm.x = 1:4, gbm.y = 6, tree.complexity = 4,learning.rate = 0.01, bag.fraction = 0.75, step.size = 10, cv.folds = 10, max.trees = 10000)
  
  pred <- predict.gbm(BRT_model, training, n.trees=BRT_model$n.trees, "response")
  pred <- data.frame(pred)
  names(pred) <- c("risk")
  ggplot(pred, aes(x=risk)) + geom_bar(stat="bin")
  
  temp_auc_training <- BRT_model$cv.statistics$discrimination.mean
  
  
  PR_AUC<-PRAUC(y_pred = pred, y_true = training$Risk)
  
  F1_pred <- ifelse(pred < 0.5, 0, 1)
  F1_score1 <- F1_Score(y_pred = F1_pred, y_true =training$Risk, positive = "1")
  
  temp_accuracy_data <- c()
  temp_accuracy_data <- cbind(temp_auc_training,PR_AUC,F1_score1)
  Accuracy_data <- rbind(Accuracy_data,temp_accuracy_data)
  
  modelname<-paste("E:\\Conflict\\Model\\BRT_StrategyB_12M_AllSample_",i,".Rdata",sep="")
  
  save(BRT_model, file = modelname)
}
filename<-paste("E:\\Conflict\\Tables\\Accuracy\\StrategyB_12M_AllSample_accuracy.csv",sep="")
write.csv(Accuracy_data, file=filename)

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
###End###


##Step4##
##Simulated the risk of armed conflicts at global scale using the BRT models trained on all samples (2000-2015) under strategy b ##
##Author:Fangyu Ding##
##Time:20200716##
##Run Time: approximately 10 hours##
rm(list=ls());
library(caret);
library(ggplot2);
library(nnet);
library(e1071);
library(ROCR)
library(RColorBrewer)
library(MLmetrics) #PRAUC
require(ggthemes)
require(coin)
require(plotrix)
library(dismo)
library(gbm)

years <- c(2000:2015)

timestart<-Sys.time();

filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\BackgroundFactors_revisedversion.csv",sep='')
dataBF <- read.csv(filename,header=T,encoding="utf-8")
names(dataBF) <- c("nrow","ncol","Elevation","NighttimeLight","UrbanAccessibility","EthnicDiversity","NDH","Precipitation","scPDSI","Temperature","NDVI","Mask")
temp_data <- dataBF[,6]
dataBF$EthnicDiversity <- as.factor(temp_data)
dataBF$Mask[dataBF$Mask == 255] <- 0

filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Raster_allconflict_Singleyear.csv",sep='')
##load conflict dataset from 2000 to 2015 years
dataRisk <- read.csv(filename,header=F,encoding="utf-8")
names(dataRisk) <- c("nrow","ncol","Conflict2000","Conflict2001","Conflict2002","Conflict2003","Conflict2004","Conflict2005","Conflict2006","Conflict2007","Conflict2008","Conflict2009","Conflict2010","Conflict2011","Conflict2012","Conflict2013","Conflict2014","Conflict2015")
dataRisk$Conflict2000[dataRisk$Conflict2000 == 9999] <- 0
dataRisk$Conflict2001[dataRisk$Conflict2001 == 9999] <- 0
dataRisk$Conflict2002[dataRisk$Conflict2002 == 9999] <- 0
dataRisk$Conflict2003[dataRisk$Conflict2003 == 9999] <- 0
dataRisk$Conflict2004[dataRisk$Conflict2004 == 9999] <- 0
dataRisk$Conflict2005[dataRisk$Conflict2005 == 9999] <- 0
dataRisk$Conflict2006[dataRisk$Conflict2006 == 9999] <- 0
dataRisk$Conflict2007[dataRisk$Conflict2007 == 9999] <- 0
dataRisk$Conflict2008[dataRisk$Conflict2008 == 9999] <- 0
dataRisk$Conflict2009[dataRisk$Conflict2009 == 9999] <- 0
dataRisk$Conflict2010[dataRisk$Conflict2010 == 9999] <- 0
dataRisk$Conflict2011[dataRisk$Conflict2011 == 9999] <- 0
dataRisk$Conflict2012[dataRisk$Conflict2012 == 9999] <- 0
dataRisk$Conflict2013[dataRisk$Conflict2013 == 9999] <- 0
dataRisk$Conflict2014[dataRisk$Conflict2014 == 9999] <- 0
dataRisk$Conflict2015[dataRisk$Conflict2015 == 9999] <- 0

summary(dataRisk)

p <- 1
for(p in 1:length(years)){
  tempyear = years[p]
  filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Precipitation\\12MonthsSD\\",years[p],".csv",sep='')
  ##load SPI12 dataset
  dataPrecipitation <- read.csv(filename,header=T,encoding="utf-8")
  names(dataPrecipitation) <- c("nrow","ncol","SPI12")
  
  filename=paste("E:\\Conflict\\Tables\\RevisedVersion\\Temperature\\12MonthsSD\\",years[p],".csv",sep='')
  ##load STI12 dataset
  dataTemperature <- read.csv(filename,header=T,encoding="utf-8")
  names(dataTemperature) <- c("nrow","ncol","STI12")
  
  Alldata <- merge(dataPrecipitation,dataTemperature, by= c("nrow","ncol"))
  
  Alldata <- merge(Alldata,dataBF, by= c("nrow","ncol"))
  
  Alldata <- merge(Alldata,dataRisk, by= c("nrow","ncol"))
  
  Alldata<- subset(Alldata,Elevation < 10000)
  Alldata<- subset(Alldata,NighttimeLight < 100)
  Alldata<- subset(Alldata,UrbanAccessibility > -10)
  Alldata<- subset(Alldata,Temperature > -100)
  Alldata<- subset(Alldata,NDVI > -1000)
  
  Alldata<- subset(Alldata,SPI12 > -100)
  Alldata<- subset(Alldata,STI12 > -100)
  
  if(p == 1){
    Alldata$Risk <- Alldata$Conflict2000
  }  else if(p == 2){
    Alldata$Risk <- Alldata$Conflict2001
  } else if(p == 3){
    Alldata$Risk <- Alldata$Conflict2002
  } else if(p == 4){
    Alldata$Risk <- Alldata$Conflict2003
  } else if(p == 5){
    Alldata$Risk <- Alldata$Conflict2004
  } else if(p == 6){
    Alldata$Risk <- Alldata$Conflict2005
  } else if(p == 7){
    Alldata$Risk <- Alldata$Conflict2006
  } else if(p == 8){
    Alldata$Risk <- Alldata$Conflict2007
  } else if(p == 9){
    Alldata$Risk <- Alldata$Conflict2008
  } else if(p == 10){
    Alldata$Risk <- Alldata$Conflict2009
  } else if(p == 11){
    Alldata$Risk <- Alldata$Conflict2010
  } else if(p == 12){
    Alldata$Risk <- Alldata$Conflict2011
  } else if(p == 13){
    Alldata$Risk <- Alldata$Conflict2012
  } else if(p == 14){
    Alldata$Risk <- Alldata$Conflict2013
  } else if(p == 15){
    Alldata$Risk <- Alldata$Conflict2014
  } else if(p == 16){
    Alldata$Risk <- Alldata$Conflict2015
  } 
  Alldata <- subset( Alldata, select = -Conflict2015 )
  Alldata <- subset( Alldata, select = -Conflict2014 )
  Alldata <- subset( Alldata, select = -Conflict2013 )
  Alldata <- subset( Alldata, select = -Conflict2012 )
  Alldata <- subset( Alldata, select = -Conflict2011 )
  Alldata <- subset( Alldata, select = -Conflict2010 )
  Alldata <- subset( Alldata, select = -Conflict2009 )
  Alldata <- subset( Alldata, select = -Conflict2008 )
  Alldata <- subset( Alldata, select = -Conflict2007 )
  Alldata <- subset( Alldata, select = -Conflict2006 )
  Alldata <- subset( Alldata, select = -Conflict2005 )
  Alldata <- subset( Alldata, select = -Conflict2004 )
  Alldata <- subset( Alldata, select = -Conflict2003 )
  Alldata <- subset( Alldata, select = -Conflict2002 )
  Alldata <- subset( Alldata, select = -Conflict2001 )
  Alldata <- subset( Alldata, select = -Conflict2000 )
  
  Alldata <- subset( Alldata, select = -Elevation )
  Alldata <- subset( Alldata, select = -NighttimeLight )
  Alldata <- subset( Alldata, select = -UrbanAccessibility )
  Alldata <- subset( Alldata, select = -EthnicDiversity )
  Alldata <- subset( Alldata, select = -NDH )
  Alldata <- subset( Alldata, select = -scPDSI )
  Alldata <- subset( Alldata, select = -NDVI )
  
  Accuracy_data <- c()
  
  copy_Alldata <- Alldata[,1:2]
  temp_Alldata <- Alldata[,3:8]
  Alldata <- temp_Alldata
  i = 1
  for(i in 1 : 20 ){
    modelname <-
      paste("E:\\Conflict\\Model\\BRT_StrategyB_12M_AllSample_",
            i,
            ".Rdata",
            sep = "")
    load(modelname)
    
    pred <-  predict.gbm(BRT_model, Alldata, n.trees=BRT_model$n.trees, "response")
    
    copy_Alldata$Risk <- pred
    simulate_filename <- paste("E:\\Conflict\\Tables\\Simulation\\All_StrategyB_12M_ConflictRisk_simulated_",years[p],"_",i,".csv",sep="")
    write.csv(copy_Alldata, file=simulate_filename,row.names=F)
  }
}
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
###End###
