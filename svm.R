##Performance of SVMlight, LibSVM and LibLinear
#- compare the performance of the three SVM implementations using the linear SVM to see if the performance is the same
#- if not, why is it not? Explain the differences.
#- compare SVMlight and LibSVM with the Gaussian kernel for the SVM
#- is the performance the same with SVMlight and LibSVM this time?
library(e1071)
library(MASS)
library(klaR)
library(LiblineaR)
train_data <- read.csv("C:\\Users\\46693\\Desktop\\books\\2\\machine learning\\adult.csv",header = F,sep=',',na.strings = " ?")#读取文件，无标题
test_data<-read.csv("C:\\Users\\46693\\Desktop\\books\\2\\machine learning\\adult.test.csv",header=F,sep=',',na.strings = " ?")
name<-c('age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours',
        'native','income')
names(train_data)<-name
names(test_data)<-name

##train_data
#drop "?" 
train_data[!complete.cases(train_data),]
train_data<-na.omit(train_data)
#sex  Male 1 ;Female 0 
typeof(train_data)
a <- sub(" Male","1",train_data$sex)
b <- sub(" Female","0",a)
train_data$sex <- b
train_data$sex <- as.numeric(train_data$sex)
#one hot encording
for(unique_value in unique(train_data$workclass)){
  train_data[paste("workclass", unique_value, sep = ".")] <- ifelse(train_data$workclass == unique_value, 1, 0)
}
for(unique_value in unique(train_data$`marital-status`)){
  train_data[paste("marital.status", unique_value, sep = ".")] <- ifelse(train_data$`marital-status` == unique_value, 1, 0)
}
for(unique_value in unique(train_data$occupation)){
  train_data[paste("occupation", unique_value, sep = ".")] <- ifelse(train_data$occupation == unique_value, 1, 0)
}
for(unique_value in unique(train_data$relationship)){
  train_data[paste("relationtship", unique_value, sep = ".")] <- ifelse(train_data$relationship == unique_value, 1, 0)
}
for(unique_value in unique(train_data$race)){
  train_data[paste("race", unique_value, sep = ".")] <- ifelse(train_data$race == unique_value, 1, 0)
}
for(unique_value in unique(train_data$native)){
  train_data[paste("native", unique_value, sep = ".")] <- ifelse(train_data$native == unique_value, 1, 0)
}

train_data<-train_data[,-which(names(train_data)%in%c('workclass','education','marital-status','occupation','relationship','race','native'))]

##test_data
#drop "?" col
test_data[!complete.cases(test_data),]
test_data<-na.omit(test_data)
#sex  Male 1 ;Female 0 
a <- sub(" Male","1",test_data$sex)
b <- sub(" Female","0",a)
test_data$sex <- b
test_data$sex <- as.numeric(test_data$sex)
#one hot encording
for(unique_value in unique(test_data$workclass)){
  test_data[paste("workclass", unique_value, sep = ".")] <- ifelse(test_data$workclass == unique_value, 1, 0)
}
for(unique_value in unique(test_data$`marital-status`)){
  test_data[paste("marital.status", unique_value, sep = ".")] <- ifelse(test_data$`marital-status` == unique_value, 1, 0)
}
for(unique_value in unique(test_data$occupation)){
  test_data[paste("occupation", unique_value, sep = ".")] <- ifelse(test_data$occupation == unique_value, 1, 0)
}
for(unique_value in unique(test_data$relationship)){
  test_data[paste("relationtship", unique_value, sep = ".")] <- ifelse(test_data$relationship == unique_value, 1, 0)
}
for(unique_value in unique(test_data$race)){
  test_data[paste("race", unique_value, sep = ".")] <- ifelse(test_data$race == unique_value, 1, 0)
}
for(unique_value in unique(test_data$native)){
  test_data[paste("native", unique_value, sep = ".")] <- ifelse(test_data$native == unique_value, 1, 0)
}
#add column
test_data<-test_data[,-which(names(test_data)%in%c('workclass','education','marital-status','occupation','relationship','race','native'))]
test_data$native..Holand.Netherlands<-c(rep(0,dim(test_data)[1]))##添加一列
#normalized
train_data[,c(1:3,5:7)] <- scale(train_data[,c(1:3,5:7)])
test_data[,c(1:3,5:7)] <- scale(test_data[,c(1:3,5:7)])
#select x y
colnames(train_data)<-make.names(colnames(train_data))
colnames(test_data)<-make.names(colnames(test_data))
trainx<-train_data[,-which(names(train_data)%in%c('income'))]
testx<-test_data[,-which(names(test_data)%in%c('income'))]

##libsvm
model1<-svm(income~., train_data,kernel='linear')
pred1<-predict(model1,testx)
table_1<-table(pred=pred1,true=test_data$income)
accuracy_1<-sum(diag(table_1))/sum(table_1)
print(accuracy_1)

##svmlight
model2<-svmlight(income~.,data = train_data,svm.options="-c 10")
pred2<-predict(model2,testx)
table_2<-table(pred=pred2$class,true=test_data$income)
table_2
accuracy_2<-sum(diag(table_2))/sum(table_2)
print(accuracy_2)

##Liblinear
model3<-LiblineaR(trainx,train_data$income,type=5,cost=1,epsilon=0.01)
pred3<-predict(model3,testx)
table_3<-table(pred3$predictions,test_data$income)
table_3
accuracy_3<-sum(diag(table_3))/sum(table_3)
print(accuracy_3)

#### the Gaussian kernel
##Libsvm
gmodel1<-svm(trainx,train_data$income,kernel ='radial')
pred11<-predict(gmodel1,testx)
table_11<-table(pred=pred11,true=test_data$income)
table_11
accuracy_11<-sum(diag(table_11))/sum(table_11)
print(accuracy_11)

## svmlight
gmodel2<-svmlight(income~.,data = train_data,svm.options ="-c 1 -t 2 -g 0.1")
pred22<-predict(gmodel2,testx)
table_22<-table(pred=pred22$class,true=test_data$income)
table_22
accuracy_22<-sum(diag(table_22))/sum(table_22)
print(accuracy_22)

