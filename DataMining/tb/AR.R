library(rminer)
library(randomForest)
library(rpart.plot)
library(caTools)
library(caret)
library(mltools)
library(ade4)
library(data.table)
library(nnet)
library(arules)

#Math
#base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-mat.csv", header = TRUE, sep = ";")
#Portugues
base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-por.csv", header = TRUE, sep = ";")

#------------------------DATA UNDERSTANDING------------------------------------

baseAR=base

#Remove age
baseAR$age=NULL

#Criacao de base binaria
baseAR$G3[base$G3 < 10] = "fail"
baseAR$G3[base$G3 >= 10] = "pass"
baseAR$G2[base$G2 < 10] = "fail"
baseAR$G2[base$G2 >= 10] = "pass"
baseAR$G1[base$G1 < 10] = "fail"
baseAR$G1[base$G1 >= 10] = "pass"

baseAR = as.data.frame(sapply(colnames(baseAR),function(name){ paste(name,baseAR[,name],sep="_")}))

#save and load
write.csv(baseAR,"C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/baseAR_mat.csv", row.names = FALSE)

baseAR=read.transactions("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/baseAR_mat.csv", header = TRUE, sep = ",",rm.duplicates = TRUE)


#write.csv(baseAR,"C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/baseAR_Port.csv", row.names = FALSE)

#baseAR=read.transactions("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/baseAR_Port.csv", header = TRUE, sep = ",",rm.duplicates = TRUE)

#Apriori algorithm
rules1=apriori(baseAR,parameter = list(minlen=2, maxlen=4, supp=0.1, conf=0.6), 
                appearance = list(rhs=c("G3_fail"), default="lhs"),)
rules_sort = sort(rules1, by="support")[1:10]
inspect(rules_sort)
rules_sort = sort(rules1, by="confidence")[1:10]
inspect(rules_sort)
rules_sort = sort(rules1, by="lift")[1:10]
inspect(rules_sort)
















