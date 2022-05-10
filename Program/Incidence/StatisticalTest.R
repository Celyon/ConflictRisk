##Statistical test##
##Data sources of Tables S2-S9 ##
##Author:Fangyu Ding##
##Time:20200808##

##Strategy a+ All Samples##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyA_24M_AllSamples_",i,".csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-24M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-24M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-24M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)

##Strategy a All Samples##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyA_AllSamples_",i,".csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-12M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-12M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-12M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)


##Strategy b All Samples##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyB_AllSamples_",i,".csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-12M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-12M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-12M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)


##Strategy b+ All Samples##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\StrategyB_24M_AllSamples_",i,".csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-24M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-24M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-24M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)


##Strategy a+ one-year sample##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\2000_",i,"_StrategyA_24M_samples.csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-24M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-24M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-24M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)

##Strategy a one-year sample##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\2000_",i,"_StrategyA_samples.csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-12M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-12M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyA-OneYear-12M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)


##Strategy b one-year sample##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\2000_",i,"_StrategyB_samples.csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-12M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-12M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-12M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)


##Strategy b+ one-year sample##
i = 1
set.seed(i)

filename <- paste("E:\\Conflict\\Tables\\Samples-Revisedversion\\2000_",i,"_StrategyB_24M_samples.csv",sep="")
Alldata <- read.csv(filename,header=T,encoding="utf-8")  
tempdata <- subset(Alldata, select = -Mask)

library(car)
lm1 <- lm(Risk ~ ., data = tempdata)
temp<-summary(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-24M-LM-",i,".csv",sep="")
write.csv(temp$coefficients, file=filename)

vif(lm1)
filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-24M-Multicollinearity-",i,".csv",sep="")
write.csv(vif(lm1), file=filename)

tempdata <- subset(tempdata, select = -Risk)  
res <- cor(tempdata)
round(res, 3)

filename <- paste("E:\\Conflict\\Tables\\StatisticalTest\\StrategyB-OneYear-24M-Correlation-",i,".csv",sep="")
write.csv(round(res, 3), file=filename)




