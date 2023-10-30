library(readxl)
setwd("/home/claudia/Documents/Knowledge-based Systems/Project/Project_2/")
Project2 <- read_excel("Project2.xlsx")
data <- Project2
# remove empty data
#data <- Project2[colMeans(Project2=="?")<=0.2]
# The features in the data sets are id, p1-p762 and Host
# Divide data into species #### I am not sure that we need to split into training/testing


Species <- split(data, data$Host)
View(Species)
data_avian <- data$Host == "Avian"
data_human <- data$Host == "Human"
View(data_human)

# split data into train and test
sample_human <- sample(c(TRUE, FALSE), nrow(Species$Human), replace=TRUE, prob=c(0.7,0.3))
sample_avian <- sample(c(TRUE, FALSE), nrow(Species$Avian), replace=TRUE, prob=c(0.7,0.3))

train_human  <- Species$Human[sample_human, ]
test_human   <- Species$Human[!sample_human, ]
train_avian  <- Species$Avian[sample_avian, ]
test_avian   <- Species$Avian[!sample_avian, ]

Test <- rbind(test_human, test_avian)
Train <- rbind(train_human, train_avian)
View(Test)



# MCFS
library(rmcfs)
#needs to be a dataframe input
data_train<- as.data.frame(Train)
result <- mcfs(Host ~ ., data_train, projections = 1500, projectionSize = 0.1, splits = 5, splitSetSize = 500, cutoffPermutations = 6, threadsNumber = 8)
# plot
plot(result, type = "distances")
# get the most significant results
Sig_result <- result$RI[1:result$cutoff_value,]
View(Sig_result)
# 20 most significant
Sig_result[1:20,]
# interdependancy plot
gid <- build.idgraph(result, size = 20)
plot.idgraph(gid, label_dist = 0.3)

# ROSETTA
set.seed(1)
# install and load the library
install.packages("devtools")
library(devtools)
install_github("komorowskilab/R.ROSETTA")
library(R.ROSETTA)

#Create input table for Rosetta from significant results in rmcfs
table_significant<- result$data
#or 
#table_significant<- cbind(data[,names(data) %in% Sig_result$attribute], data$Host)

#Check how many features has our data (-1 decision class)
dim(table_significant)[2]-1

# number of classes per decision class (hosts)
table(table_significant$decision) #it is balanced

# run rosetta with PB1 data
#johnson
datajohnsonhuman <- rosetta(table_significant, roc = TRUE, clroc = "Human", discrete = T)
rules_h <- datajohnsonhuman$main
qual_h <- datajohnsonhuman$quality
#genetic
datagenetic <- rosetta(table_significant, roc = TRUE, clroc = "Human", discrete = T, reducer= "Genetic")
rules_h_genetic <- datagenetic$main
qual_h_genetic <- datagenetic$quality

#Print top 12 rules
rlsAut_h <- viewRules(rules_h)
print(rlsAut_h[1:12,])
rlsAut_h_g <- viewRules(rules_h_genetic[rules_h_genetic$decision=="Human",])
print(rlsAut_h_g[1:12,])
rlsAut_h_all <- viewRules(rules_h_genetic)
print(rlsAut_h_all[1:12,])

# significant rules in the model
tabS_h <- table(rules_h[rules_h$pValue < 0.05,]$decision)
# fraction of significant rules, in [%]
tabS_h[1]/length(rules_h$decision=="Human")*100
tabS_h[2]/length(rules_h$decision=="Avian")*100

# ROC curve
plotMeanROC(datajohnsonhuman)
plotMeanROC(datagenetic)
# analyze the rule-based model
gf_h <- getFeatures(rules_h, filter = T, filterType = "pvalue", thr = 0.05)
features_human <- gf_h$features$Human
features_avian <- gf_h$features$Avian

#set operators from "base" library
?union
?intersect
?setdiff

# recalculate the model
recalculate_datarules <- recalculateRules(table_significant, rules_h, discrete = TRUE)
topRules_h <-  viewRules(recalculate_datarules[recalculate_datarules$decision=="Human",])
topRuleInd_h<- which(recalculate_datarules$pValue == topRules_h[1,]$pValue)
# find the most significant rule for Human
#topRules_h <-  viewRules(recalculate_datarules[recalculate_datarules$decision=="Human",])
#topRuleInd_h<- rownames(topRules[1,])
#topRuleInd_h <- rlsAut_h[,rlsAut_h$pValue==rlsAut_h$pValue[1]][1]

# set new labels for plots using parameter label=c() FROM HERE DOESN'T WORK
plotRule(table_significant, recalculate_datarules, type="heatmap", discrete=TRUE, ind= topRuleInd_h)
plotRule(table_significant, recalculate_datarules, type="boxplot", discrete=TRUE, ind= topRuleInd_h)

#Evaluation of Rosetta model with 10-fold cross validation
#Johnson
decision<- Test$Host
pred_class<- predictClass(Test, rules_h, discrete=TRUE, normalize = TRUE, normalizeMethod="rss", validate = TRUE, decision)
pred_out<- pred_class$out
pred_acc<- pred_class$accuracy
#Genetic
pred_class_G<- predictClass(Test, rules_h_genetic, discrete=TRUE, normalize = TRUE, normalizeMethod="rss", validate = TRUE, decision)
pred_out_g<- pred_class_G$out
pred_acc_g<- pred_class_G$accuracy

#Visualization of results
plot_ros_G<- visunet(datagenetic$main)
