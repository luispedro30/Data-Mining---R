library('fpc')
library('factoextra')
library(dplyr)
library('magrittr')
library(tidyverse)
library(broom)
library(caret)
library(dbscan)


set.seed(1234)

#Math
base=read.csv("student/student-mat.csv", header = TRUE, sep = ";")
base= base[base[, "age"] <22,]
#base$G3=as.numeric(cut(base$G3,c(-1,10,12,14,16,21),c(5,4,3,2,1)))
#base$G2=as.numeric(cut(base$G2,c(-1,10,12,14,16,21),c(5,4,3,2,1)))
#base$G1=as.numeric(cut(base$G1,c(-1,10,12,14,16,21),c(5,4,3,2,1)))


base$G3[base$G3 < 10] = 0
base$G3[base$G3 >= 10] = 1
base$G2[base$G2 < 10] = 0
base$G2[base$G2 >= 10] = 1
base$G1[base$G1 < 10] = 0
base$G1[base$G1 >= 10] = 1

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

ohe_feats = c("Mjob","Fjob","reason","guardian")
dummies = dummyVars(~ Mjob + Fjob + reason + guardian , data = base)
df_all_ohe =  as.data.frame(predict(dummies, newdata = base))
df_all_combined = cbind(base[,-c(which(colnames(base) %in% ohe_feats))],df_all_ohe)
base = as.data.frame(df_all_combined)

cols.num <- c("school","sex","address","famsize","Pstatus","schoolsup",
              "famsup","paid","activities","nursery","higher","internet","romantic")
base[cols.num] <- sapply(base[cols.num],as.numeric)

view(base)
base.features = base
base.features$G3= NULL
view(base.features)


#----------Pre-Processing-----------------
#missing values and standardization

numdata = sapply(base.features, is.numeric) #seperate numeric variables
mydata = base.features[ , numdata] #keep only numeric variables


standardization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#create a normalization function

#for_clust_data  = as.data.frame(lapply(mydata, standardization)) #normalize data , keep only 6 variables
#for_clust_data  = as.data.frame(lapply(mydata[ ,c(1,5,6,13,14,15)], standardization)) 
for_clust_data  = as.data.frame(lapply(mydata[ ,c(3,26,28)], standardization))

km1 = kmeans(for_clust_data, 2, nstart=100)
km1$cluster#vector of clusters
km1$size

table(base$G3,km1$cluster) #comparation
plot(base[c("G1","G2")],col=km1$cluster)
plot(base[c("G1","G2")],col=base$G3)

p1 = fviz_cluster(km1, geom = "point", data = for_clust_data) + ggtitle("K-means with 2 clusters")
table(base$G3,km1$cluster) #comparation
plot(p1)


kNNdistplot(for_clust_data, k=4) #optimal value of eps with minimal of 4 points
abline(h=0.2, col="red")

db = dbscan(for_clust_data, 0.4, 4)
p2 = fviz_cluster(db, geom = "point", data = for_clust_data) + ggtitle("DBSCAN")
table(base$G3,db$cluster) #comparation
plot(p2)

d=dist(for_clust_data, method = "euclidean") # distance matrix
c1=cluster.stats(d,km1$cluster)
c2=cluster.stats(d,db$cluster)

#internal evaluation
# a higher Dunn index indicates better clustering
cat("c1 DUNN:",c1$dunn,"\n")
cat("c2 DUNN:",c2$dunn,"\n")


library(fossil)
library("jaccard")
# a higher rand and Jaccard index indicates better clustering
rand.index(base$G3,km1$cluster)
rand.index(base$G3,db$cluster)
jaccard(base$G3,km1$cluster)
jaccard(base$G3,db$cluster)







