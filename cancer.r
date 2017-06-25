setwd("C:/Users/Divya Sharma/Desktop/myprojects/cancer")
install.packages('class')
library(class)
install.packages('gmodels')
library(gmodels)
bcd<-read.csv("breast cancer data.csv",stringsAsFactors = FALSE)
str(bcd)
bcd<-bcd[-1]
table(bcd$diagnosis)
bcd$diagnosis<-factor(bcd$diagnosis,levels=c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(bcd$diagnosis))*100,digits=1)
summary(bcd[c("radius_mean","area_mean","smoothness_mean")])

normalize<-function(x){ return((x-min(x))/(max(x)-min(x)))}
normalize(c(1,2,3,4,5))
bcd_n<-as.data.frame(lapply(bcd[2:31],normalize))
summary(bcd_n$area_mean)

bcd_train<-bcd_n[1:469, ]
bcd_test<-bcd_n[470:569, ]
bcd_train_labels<-bcd[1:469,1]
bcd_test_labels<-bcd[470:569,1]

bcd_test_pred<-knn(train=bcd_train,test=bcd_test,cl=bcd_train_labels,k=21)

CrossTable(x=bcd_test_labels,y=bcd_test_pred,prop.chisq = FALSE)

bcd_z<-as.data.frame(scale(bcd[-1]))
summary(bcd_z$area_mean)

bcd_train<-bcd_z[1:469,]
bcd_test<-bcd_z[470:569,]
bcd_train_labels<-bcd[1:469,1]
bcd_test_labels<-bcd[470:569,1]
bcd_test_pred<-knn(train=bcd_train,test=bcd_test,cl=bcd_train_labels,k=21)
CrossTable(x=bcd_test_labels,y=bcd_test_pred,prop.chisq = FALSE)