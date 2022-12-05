library(rminer)
library(randomForest)
library(rpart.plot)
library(caTools)
library(caret)
library(mltools)
library(ade4)
library(data.table)
library(nnet)
#Math
base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-mat.csv", header = TRUE, sep = ";")

#Portugues
#base=read.csv("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-por.csv", header = TRUE, sep = ";")
#------------------------DATA UNDERSTANDING-------------------------------------
print(base)
is.na(base) #ver missing values
sapply(base, function(x) sum(is.na(x))) #ver missing values resumido

#ver se todos os valores estÃ£o dentro do esperado
summary(base)


attributes = c("age","absences","G1","G2")


#ver outliers
boxplot(base$age, col = "blue") #idade 1 outlier (remover 22anos)
boxplot(base$absences, col = "blue") #nao se retirar
boxplot(base$G1, col = "blue") # 0outlier
boxplot(base$G2, col = "blue")# 1outlier (nao remover)

hist(base$G3, col = "blue", breaks = 20) #ver distribuicao



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

#one-hot encoding
ohe_feats = c("Mjob","Fjob","reason","guardian")
dummies = dummyVars(~ Mjob + Fjob + reason + guardian , data = base)
df_all_ohe =  as.data.frame(predict(dummies, newdata = base))
df_all_combined = cbind(base[,-c(which(colnames(base) %in% ohe_feats))],df_all_ohe)
base = as.data.frame(df_all_combined)


#---------------------------##AS-numeric##--------------------

cols.num <- c("school","sex","address","famsize","Pstatus","schoolsup",
              "famsup","paid","activities","nursery","higher","internet","romantic")
base[cols.num] <- sapply(base[cols.num],as.numeric)




#-----------------------DATA PREPARATION----------------------------------------
#retirar outliers

base= base[base[, "age"] <22,]
summary(base$age) #verificando o maximo



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



#---------Definir-as-restantes-bases-de-dados-------------
base2=base

#Criacao de base de dados com 5 valores

base2$G3=cut(base$G3,c(-1,9,11,13,15,21),c(5,4,3,2,1))
base2$G2=cut(base$G2,c(-1,9,11,13,15,21),c(5,4,3,2,1))
base2$G1=cut(base$G1,c(-1,9,11,13,15,21),c(5,4,3,2,1))



#Retirar as coluna de G1 e G2 que sao muito preditivas

#base2$G2 = NULL
#base2$G1= NULL


set.seed(4543)
#(base)
mtcars.rf <- randomForest(G3 ~ ., data=base, ntree=2000, keep.forest=FALSE,importance=TRUE)
varImpPlot(mtcars.rf)


barplot(prop.table(table(base2$G3)))
#-------------divide-data-------------
dataset_divide=sample.split(base2,SplitRatio=0.7)
df_train=base2[dataset_divide==TRUE, ]
df_test=base2[dataset_divide==FALSE, ]
Y=factor(df_test$G3)

#-------------naive-------------


s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
       convex=0,metric="AUC",method=c("kfold",10,12345))


M5=fit(G3~.,df_train,model="naive", task="class",search = s)
P5=predict(M5,df_test)
print(confusionMatrix(P5, Y))
cat("predicted ACC:",round(mmetric(Y,P5,metric="ALL"),1),"\n")


#-------------KNN-------------


s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
       convex=0,metric="AUC",method=c("kfold",10,12345))


M5=fit(G3~.,df_train,model="knn", task="class",search = s)
P5=predict(M5,df_test)
print(confusionMatrix(P5, Y))
cat("predicted ACC:",round(mmetric(Y,P5,metric="ALL"),1),"\n")

#-------------DT-------------


s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
       convex=0,metric="AUC",method=c("kfold",10,12345))


M5=fit(G3~.,df_train,model="dt", task="class",search = s)
P5=predict(M5,df_test)
print(confusionMatrix(P5, Y))
cat("predicted ACC:",round(mmetric(Y,P5,metric="ALL"),1),"\n")



#-------------SVM-------------


s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
       convex=0,metric="AUC",method=c("kfold",10,12345))


M5=fit(G3~.,df_train,model="svm", task="class",search = s)
P5=predict(M5,df_test)

cat("predicted ACC:",round(mmetric(Y,P5,metric="ACC"),1),"\n")



#-------------Random-Forest-------------
  
  s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
         convex=0,metric="AUC",method=c("kfold",10,12345))
  
  
  
  M5=fit(G3~.,df_train,model="dt", task="class",search = s)
  P5=predict(M5,df_test)
  
  cat("predicted ACC:",round(mmetric(Y,P5,metric="ACC"),1),"\n")
  
  print(confusionMatrix(P5, Y))
  
  print(M5@mpar)




# correr tudo num ciclo
models=c("randomForest","naive","dt","knn","svm")

s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
       convex=0,metric="AUC",method=c("kfold",10,12345))

for(m in models)
{ cat("model:",m,"\n")

  M=fit(G3~.,df_train,model=m,task="class",search = s)
  P=predict(M,df_test)
  #print(confusionMatrix(P, Y))
  print(mmetric(Y,P,metric="ACC"))
}

