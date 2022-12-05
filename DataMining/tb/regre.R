library(rminer)
library(randomForest)
library(rpart.plot)
library(caTools)
library(caret)
library(mltools)
library(ade4)
library(data.table)
library(nnet)
library(dplyr)
library(e1071)
#Math
base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-mat.csv", header = TRUE, sep = ";")

#Portugues
#base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-por.csv", header = TRUE, sep = ";")


barplot(prop.table(table(base$G3))) #barplot G3 grades

#------------------------DATA UNDERSTANDING-------------------------------------
print(base)
is.na(base) #ver missing values
sapply(base, function(x) sum(is.na(x))) #ver missing values resumido

#ver se todos os valores estão dentro do esperado
summary(base)

attributes = c("age","absences","G1","G2")

#ver outliers
boxplot(base$age, col = "blue") #idade 1 outlier (remover 22anos)
boxplot(base$absences, col = "blue") #nao se retirar
boxplot(base$G1, col = "blue") # 0outlier
boxplot(base$G2, col = "blue")# 1outlier (nao remover)

hist(base$G3, breaks = 20, xlab = "G3", main="") #ver distribuicao

#--------Situação onde se remove todos os outliers-------------------
for (a in attributes) {  
  
  outliers = boxplot(base[,a], plot=TRUE)$out
  
  while (length(outliers)!= 0){
    
    base[which(base[,a] %in% outliers),]
    
    base <- base[-which(base[,a] %in% outliers),]
    outliers = boxplot(base[,a], plot=FALSE)$out
  }
  
}

#Tornar atributos binarios em 0 e 1
base$school [base$school  == 'GP'] = 0
base$school [base$school  == 'MS'] = 1

base$sex[base$sex == 'F'] = 0
base$sex[base$sex == 'M'] = 1


base$address [base$address == 'U'] = 0
base$address [base$address  == 'R'] = 1


base$famsize [base$famsize == 'LE3'] = 0
base$famsize [base$famsize  == 'GT3'] = 1


base$Pstatus  [base$Pstatus == 'T'] = 0
base$Pstatus  [base$Pstatus  == 'A'] = 1


base$schoolsup [base$schoolsup == 'no'] = 0
base$schoolsup  [base$schoolsup  == 'yes'] = 1


base$famsup [base$famsup == 'no'] = 0
base$famsup  [base$famsup  == 'yes'] = 1


base$paid [base$paid == 'no'] = 0
base$paid  [base$paid  == 'yes'] = 1


base$activities [base$activities == 'no'] = 0
base$activities  [base$activities  == 'yes'] = 1


base$nursery [base$nursery == 'no'] = 0
base$nursery  [base$nursery  == 'yes'] = 1


base$higher [base$higher == 'no'] = 0
base$higher  [base$higher  == 'yes'] = 1


base$internet [base$internet == 'no'] = 0
base$internet  [base$internet  == 'yes'] = 1


base$romantic [base$romantic == 'no'] = 0
base$romantic  [base$romantic  == 'yes'] = 1



#base$school=as.numeric(base$school)
#base$address=as.numeric(base$address)
#base$famsize=as.numeric(base$famsize)
#base$Pstatus=as.numeric(base$Pstatus)
#base$schoolsup=as.numeric(base$schoolsup)
#base$activities=as.numeric(base$activities)
#base$sex=as.numeric(base$sex)
#base$famsup=as.numeric(base$famsup)
#base$nursery=as.numeric(base$nursery)
#base$higher=as.numeric(base$higher)
#base$internet=as.numeric(base$internet)
#base$romantic=as.numeric(base$romantic)
#base$paid=as.numeric(base$paid)

#---------------------------##AS-numeric##--------------------

cols.num <- c("school","sex","address","famsize","Pstatus","schoolsup",
              "famsup","paid","activities","nursery","higher","internet","romantic")
base[cols.num] <- sapply(base[cols.num],as.numeric)


#one-hot encoding
ohe_feats = c("Mjob","Fjob","reason","guardian")
dummies = dummyVars(~ Mjob + Fjob + reason + guardian , data = base)
df_all_ohe =  as.data.frame(predict(dummies, newdata = base))
df_all_combined = cbind(base[,-c(which(colnames(base) %in% ohe_feats))],df_all_ohe)
base = as.data.frame(df_all_combined)




#standardization------------------------------
min_max <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}

standardization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

base$age=standardization(base$age)
base$Fedu=standardization(base$Fedu)
base$Medu=standardization(base$Medu)
base$traveltime=standardization(base$traveltime)
base$studytime=standardization(base$studytime)
base$failures=standardization(base$failures)
base$famrel=standardization(base$famrel)
base$freetime=standardization(base$freetime)
base$goout=standardization(base$goout)
base$Dalc=standardization(base$Dalc)
base$Walc=standardization(base$Walc)
base$health=standardization(base$health)
base$absences=standardization(base$absences)


#-----------------------DATA PREPARATION----------------------------------------
#retirar outliers
base= base[base[, "age"] <22,]
summary(base$age) #verificando o maximo
#base2= base2[base2[, "age"] <22,]
#base_binary= base_binary[base_binary[, "age"] <22,]


#---------------------
### exploration of some rminer classification models: 
#--http://math.furman.edu/~dcs/courses/math47/R/library/randomForest/html/varImpPlot.html---
#Ver a importancia de cada atributo
set.seed(4543)
#data(base)
mtcars.rf <- randomForest(G3 ~ ., data=base, ntree=2000, keep.forest=FALSE,importance=TRUE)
varImpPlot(mtcars.rf)


#Retirar as coluna de G1 e G2 que sao muito preditivas
#base$G2 = NULL

#base$G1= NULL


set.seed(4543)
#(base)
mtcars.rf <- randomForest(G3 ~ ., data=base, ntree=2000, keep.forest=FALSE,importance=TRUE)
varImpPlot(mtcars.rf)

#-------------rearrange-data-------------

base=base %>% relocate(G3, .after = last_col())


#models=c("randomForest","dt","svm")


#-------------Random-Forest-------------
g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")

# mining for randomForest, external 10-fold, 20 Runs (=60 fitted models)
M1=mining(G3~.,base,model="randomForest",method=c("kfold",10,123),Runs=10) #cortez2008using
m=mmetric(M1,metric=c("RMSE","MAE"))  
summary(m)

#-------------Decision-Tree-------------
g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")

# mining for randomForest, external 10-fold, 20 Runs (=60 fitted models)
M1=mining(G3~.,base,model="dt",method=c("kfold",10,123),Runs=10) #cortez2008using
m=mmetric(M1,metric=c("RMSE","MAE"))  
summary(m)

#-------------KNN-------------

g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")

M1=mining(G3~.,base,model="knn",method=c("kfold",10,123),Runs=10) #cortez2008using
m=mmetric(M1,metric=c("RMSE","MAE"))  
summary(m)





#-------------SVM-------------


g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")


M1=mining(G3~.,base,model="svm",method=c("kfold",10,123),Runs=10) #cortez2008using
m=mmetric(M1,metric=c("RMSE","MAE"))  
summary(m)



#-------------Naive--------------
g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")


M1=mining(G3~.,base,model="naive",method=c("kfold",10,123),Runs=10) #cortez2008using
m=mmetric(M1,metric=c("RMSE","MAE"))  
summary(m)




#-------------Random-Forest-------------backup_code____

g3=which(names(base)=="G3")
cat("output class:",class(base[,g3]),"\n")

# mining for randomForest, external 10-fold, 10 Runs (=60 fitted models)
M1=mining(G3~.,base,model="randomForest",method=c("kfold",10,123),Runs=10) #cortez2008using
#m=mmetric(M1,metric=c("RMSE","MAE"))  
m=mmetric(M1,metric="RMSE")
summary(m)

plot(base$G3,M1$object$cv.fit,ylim=c(0,20),xlim=c(0,20),xlab="G3",ylab="Predicted G3" )

abline(a=0,b=1, col="red")
#rmse(base$G3,M1$object$cv.fit)









