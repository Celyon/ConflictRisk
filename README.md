1.Description

Replication materials for Q Ge#, M Hao#, F Ding*, D Jiang*, J Scheffran, D Helman, T Ide. (2022). Modelling armed conflict risk under climate change with machine learning and time-series data.

The materials in this repository allow users to reproduce the data analysis appearing in the paper.

If you have questions or suggestions, please contact Fangyu Ding at dingfy@igsnrr.ac.cn or dingfy.17b@igsnrr.ac.cn



2.Acknoledgemnet

We acknowledge to authors of R packages used in this study. 



3.System requirements

Operating systems: Win 7/8/10.

Software: R (Version X64 3.3/3.4/3.5) and Python (Version X64 3.6)

Memory capacity: 16G/32G/64G

Note: There must be an E disk, and the remaining capacity of the disk must be higher than 450G. It is recommended to build an operating environment on a workstation or server.



4.Installation

Firstlly, we need to install some packages in R platform:

install.packages("caret")

install.packages("ggplot2")

install.packages("nnet")

install.packages("e1071")

install.packages("ROCR")

install.packages("RColorBrewer")

install.packages("MLmetrics")

install.packages("ggthemes")

install.packages("coin")

install.packages("plotrix")

install.packages("dismo")

install.packages("gbm")

install.packages("car")



5.Dataset

The authors declare that all data supporting the findings were obtained from open data. 



6.Examples

Running the code (Steps 1,2,3 and 4 section) in "StrategyA-12M-modelling.R", "StrategyA-24M-modelling.R", "StrategyB-12M-modelling.R" and "StrategyB-24M-modelling.R". 

Running the code in "StatisticalTest.R" to make a statistical analysis (collinearity test and significance analysis).

Running the code in "Performance-Estimated.R" to analyze the performance of the ensemble models.



