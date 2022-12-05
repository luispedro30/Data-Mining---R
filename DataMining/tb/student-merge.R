d1=read.table("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-mat.csv",sep=";",header=TRUE)
d2=read.table("C:/Users/andre/Desktop/OneDrive - Universidade do Minho/DataMining/tb/student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students
