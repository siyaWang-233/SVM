library(e1071)
library(MASS)
library(klaR)
library(LiblineaR)
library(KRLS)
train_data <- read.csv("C:\\Users\\46693\\Desktop\\books\\2\\machine learning\\adult.csv",header = F,sep=',',na.strings = " ?")
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
test_data$native..Holand.Netherlands<-c(rep(0,dim(test_data)[1]))##Ìí¼ÓÒ»ÁÐ
#normalized
train_data[,c(1:3,5:7)] <- scale(train_data[,c(1:3,5:7)])
test_data[,c(1:3,5:7)] <- scale(test_data[,c(1:3,5:7)])

#select x y
colnames(train_data)<-make.names(colnames(train_data))
colnames(test_data)<-make.names(colnames(test_data))
trainx<-train_data[,-which(names(train_data)%in%c('income'))]
testx<-test_data[,-which(names(test_data)%in%c('income'))]

#### the Gaussian kernel
##Libsvm
time1 <- proc.time()
gmodel1<-svm(trainx,train_data$income,kernel ='radial')
pred11<-predict(gmodel1,testx)
table_11<-table(pred=pred11,true=test_data$income)
table_11
accuracy_11<-sum(diag(table_11))/sum(table_11)
print(accuracy_11)
proc.time() - time1

## svmlight
time2 <- proc.time()
gmodel2<-svmlight(income~.,data = train_data,svm.options ="-c 1 -t 2 -g 0.1")
pred22<-predict(gmodel2,testx)
table_22<-table(pred=pred22$class,true=test_data$income)
table_22
accuracy_22<-sum(diag(table_22))/sum(table_22)
print(accuracy_22)
proc.time() - time2

###Use random Fourier features proposed to approximate the Guassian kernel with LibLinear
#Random Fourier Features
time3 <- proc.time()
D <- 100                                   
d <- 87                                    
W <- matrix(rnorm(d*D,mean=0,sd= 1/2),d,D) 
b <- runif(D,min=0,max=2*pi)               
B <- matrix(rep(b,times=nrow(trainx)),nrow =D,ncol=nrow(trainx),byrow =F)
B1 <- matrix(rep(b,times=nrow(testx)),nrow =D,ncol=nrow(testx),byrow =F)
Z <- sqrt(2/D)*cos(t(W)%*%t(trainx)+B)
Z1 <- sqrt(2/D)*cos(t(W)%*%t(testx)+B1)
##Liblinear
model3<-LiblineaR(t(Z),train_data$income)
pred3<-predict(model3,t(Z1))
table_3<-table(pred3$predictions,test_data$income)
table_3
accuracy_3<-sum(diag(table_3))/sum(table_3)
print(accuracy_3)
proc.time() - time3

###Compare the accuracy and time of different D
zip1<-c(2,10,100,1000,2000)
t1 = Sys.time()  
for (D in zip1) {
  d <- 87
  W <- matrix(rnorm(D*d,mean=0,sd=1/2),d,D)
  b <- runif(D,min=0,max=2*pi)
  B <- matrix(rep(b,times=nrow(trainx)),nrow =D,ncol=nrow(trainx),byrow =F)
  B1 <- matrix(rep(b,times=nrow(testx)),nrow =D,ncol=nrow(testx),byrow =F)
  #trainx<-as.matrix(trainx)
  Z <- sqrt(2/D)*cos(t(W)%*%t(trainx)+B)
  Z1 <- sqrt(2/D)*cos(t(W)%*%t(testx)+B1)
  model3<-LiblineaR(t(Z),train_data$income,type=5)
  pred3<-predict(model3,t(Z1))
  table_3<-table(pred3$predictions,test_data$income)
  accuracy_3<-sum(diag(table_3))/sum(table_3)
  print(accuracy_3)
  print(difftime(Sys.time(), t1, units = 'sec')) 
}
  
#############
#Compare Gaussin kernel K and K1 = z(x)Tz(y)
exampleX<-as.matrix(trainx[1:5000,])
k<-gausskernel(X = exampleX, sigma = 1)
D_seq<-seq(from=2, to=1000, by=100)
for (D in D_seq) {
  d<-87
  W <- matrix(rnorm(d*D,mean=0,sd= 1),d,D)
  b <- runif(D,min=0,max=2*pi)
  B <- matrix(rep(b,times=nrow(trainx)),nrow =D,ncol=nrow(trainx),byrow =F)
  trainx<-as.matrix(trainx)
  Z <- sqrt(2/D)*cos(t(W)%*%t(trainx)+B)
  #Extract part data
  Z_eg<-as.matrix(Z[,1:5000])
  k1<-t(Z_eg)%*%Z_eg
  Error<- mean((k-k1)^2)
  print(Error)
  }

