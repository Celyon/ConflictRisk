##Estimate the mean and standard deviation of performance##
##Data sources of Figure 1, Supplementary Figures 2-3 and Supplementary Tables 1-3##
##Author:Fangyu Ding##
##Time:20200803##

Alldata_A_12M <- read.csv(file = "E:\\Conflict\\Tables\\Performace\\Performance_Timecorss_StrategyA_12M.csv", header = T, encoding = "utf-8")
Alldata_A_24M <- read.csv(file = "E:\\Conflict\\Tables\\Performace\\Performance_Timecorss_StrategyA_24M.csv", header = T, encoding = "utf-8")

Alldata_B_12M <- read.csv(file = "E:\\Conflict\\Tables\\Performace\\Performance_Timecorss_StrategyB_12M.csv", header = T, encoding = "utf-8")
Alldata_B_24M <- read.csv(file = "E:\\Conflict\\Tables\\Performace\\Performance_Timecorss_StrategyB_24M.csv", header = T, encoding = "utf-8")

wilcox.test(Alldata_A_12M$Mean.AUC,Alldata_A_24M$Mean.AUC, alternative = 'two.sided')
wilcox.test(Alldata_A_12M$Mean.AUC,Alldata_B_12M$Mean.AUC, alternative = 'two.sided')
wilcox.test(Alldata_A_12M$Mean.AUC,Alldata_B_24M$Mean.AUC, alternative = 'two.sided')
wilcox.test(Alldata_A_24M$Mean.AUC,Alldata_B_12M$Mean.AUC, alternative = 'two.sided')
wilcox.test(Alldata_A_24M$Mean.AUC,Alldata_B_24M$Mean.AUC, alternative = 'two.sided')
wilcox.test(Alldata_B_12M$Mean.AUC,Alldata_B_24M$Mean.AUC, alternative = 'two.sided')

sd(Alldata_A_12M$Mean.AUC)
mean(Alldata_A_12M$Mean.AUC)
max(Alldata_A_12M$Mean.AUC)
min(Alldata_A_12M$Mean.AUC)

sd(Alldata_A_12M$Mean.PRAUC)
mean(Alldata_A_12M$Mean.PRAUC)
max(Alldata_A_12M$Mean.PRAUC)
min(Alldata_A_12M$Mean.PRAUC)

sd(Alldata_A_12M$Mean.F1score1)
mean(Alldata_A_12M$Mean.F1score1)
max(Alldata_A_12M$Mean.F1score1)
min(Alldata_A_12M$Mean.F1score1)


sd(Alldata_A_24M$Mean.AUC)
mean(Alldata_A_24M$Mean.AUC)
max(Alldata_A_24M$Mean.AUC)
min(Alldata_A_24M$Mean.AUC)

sd(Alldata_A_24M$Mean.PRAUC)
mean(Alldata_A_24M$Mean.PRAUC)
max(Alldata_A_24M$Mean.PRAUC)
min(Alldata_A_24M$Mean.PRAUC)

sd(Alldata_A_24M$Mean.F1score1)
mean(Alldata_A_24M$Mean.F1score1)
max(Alldata_A_24M$Mean.F1score1)
min(Alldata_A_24M$Mean.F1score1)


Alldata_A_12M <- read.csv(file = "E:\\Conflict\\Tables\\Accuracy\\StrategyA_12M_AllSample_accuracy.csv", header = T, encoding = "utf-8")
Alldata_A_24M <- read.csv(file = "E:\\Conflict\\Tables\\Accuracy\\StrategyA_24M_AllSample_accuracy.csv", header = T, encoding = "utf-8")

Alldata_B_12M <- read.csv(file = "E:\\Conflict\\Tables\\Accuracy\\StrategyB_12M_AllSample_accuracy.csv", header = T, encoding = "utf-8")
Alldata_B_24M <- read.csv(file = "E:\\Conflict\\Tables\\Accuracy\\StrategyB_24M_AllSample_accuracy.csv", header = T, encoding = "utf-8")

wilcox.test(Alldata_A_12M$temp_auc_training,Alldata_A_24M$temp_auc_training, alternative = 'two.sided')
wilcox.test(Alldata_A_12M$temp_auc_training,Alldata_B_12M$temp_auc_training, alternative = 'two.sided')
wilcox.test(Alldata_A_12M$temp_auc_training,Alldata_B_24M$temp_auc_training, alternative = 'two.sided')
wilcox.test(Alldata_A_24M$temp_auc_training,Alldata_B_12M$temp_auc_training, alternative = 'two.sided')
wilcox.test(Alldata_A_24M$temp_auc_training,Alldata_B_24M$temp_auc_training, alternative = 'two.sided')
wilcox.test(Alldata_B_12M$temp_auc_training,Alldata_B_24M$temp_auc_training, alternative = 'two.sided')


sd(Alldata_A_12M$temp_auc_training)
mean(Alldata_A_12M$temp_auc_training)

sd(Alldata_A_24M$temp_auc_training)
mean(Alldata_A_24M$temp_auc_training)

sd(Alldata_B_12M$temp_auc_training)
mean(Alldata_B_12M$temp_auc_training)

sd(Alldata_B_24M$temp_auc_training)
mean(Alldata_B_24M$temp_auc_training)

sd(Alldata_A_12M$PR_AUC)
mean(Alldata_A_12M$PR_AUC)

sd(Alldata_A_24M$PR_AUC)
mean(Alldata_A_24M$PR_AUC)

sd(Alldata_B_12M$PR_AUC)
mean(Alldata_B_12M$PR_AUC)

sd(Alldata_B_24M$PR_AUC)
mean(Alldata_B_24M$PR_AUC)


sd(Alldata_A_12M$F1_score1)
mean(Alldata_A_12M$F1_score1)

sd(Alldata_A_24M$F1_score1)
mean(Alldata_A_24M$F1_score1)

sd(Alldata_B_12M$F1_score1)
mean(Alldata_B_12M$F1_score1)

sd(Alldata_B_24M$F1_score1)
mean(Alldata_B_24M$F1_score1)