# while getting confusion matrix i have written the accuracy as comment

bank <- read.csv("demo.csv") # reading the bank data csv file
bank$D <- factor(bank$D)
str(bank)
bank=subset(bank,select=-c(`NO`,`YR`))

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=bank$D,p=0.7,list=FALSE)

trainingWOY <- bank[intrain,-1]
validationWOY <- bank[-intrain,-1]

YofTraining <- bank[intrain,1]
YofValidation <- bank[-intrain,1]


library(class)

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=1)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )


knn3.pred=knn(trainingWOY,validationWOY,YofTraining,k=3)
tbl_3 <- table(knn3.pred , YofValidation)
confusionMatrix( tbl_3 )

knn5.pred=knn(trainingWOY,validationWOY,YofTraining,k=5)
tbl_5 <- table(knn5.pred , YofValidation)
confusionMatrix( tbl_5 )

knn7.pred=knn(trainingWOY,validationWOY,YofTraining,k=7)
tbl_7 <- table(knn7.pred , YofValidation)
confusionMatrix( tbl_7 )

knn7.pred=knn(trainingWOY,validationWOY,YofTraining,k=9)
tbl_7 <- table(knn7.pred , YofValidation)
confusionMatrix( tbl_7 )

###################### CANCER data set  ######################

bcan <- read.csv("demo.csv") # rwading the cancer data
str(bcan)
bcan <- subset(bcan,select=-c(`Code`))



library(caret)
set.seed(2018)
intrain<-createDataPartition(y=bcan$Class,p=0.7,list=FALSE)

trainingWOY <- bcan[intrain,-10]
validationWOY <- bcan[-intrain,-10]

YofTraining <- bcan[intrain,10]
YofValidation <- bcan[-intrain,10]



library(class)

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=1)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=3)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=5)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 ) # max 98.56

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=7)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=9)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )

#############doing above excersise with logistic , LDA , QDA , SVM{linear,radial,polynomial}

#doing SVM
bcan <- read.csv("demo.csv") # cancer dataset
str(bcan)
bcan <- subset(bcan,select=-c(`Code`))

set.seed(2018)

intrain<-createDataPartition(y=bcan$Class,p=0.7,list=FALSE)

trainingWOY <- bcan[intrain,]
validationWOY <- bcan[-intrain,]
str(trainingWOY)



library(e1071)
fit.svm <- svm(Class~., type="C",data=trainingWOY, kernel="linear")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf)  #97.61


library(e1071)
fit.svm <- svm(Class~., type="C",data=trainingWOY, kernel="radial")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf)  #98.09

library(e1071)
fit.svm <- svm(Class~., type="C",data=trainingWOY, kernel="polynomial")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) # 97.13


#doing discriminant LDA

bcan <- read.csv("/home/himanshu/Documents/CDAC/theory/trainer/ML/sanjay sane/Cases/Wisconsin/BreastCancer.csv")
str(bcan)
bcan <- subset(bcan,select=-c(`Code`))

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=bcan$Class , p=0.7,list=FALSE)

training   <- bcan[ intrain , ]
validation <- bcan[-intrain , ]


fit.lda <- lda(Class ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)

confusionMatrix(pred.lda$class,validation$Class) #96.17

#QDA

bcan <- read.csv("demo.csv") # reading cancer dataset
str(bcan)
bcan <- subset(bcan,select=-c(`Code`))

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=bcan$Class , p=0.7,list=FALSE)

training   <- bcan[ intrain , ]
validation <- bcan[-intrain , ]

fit.qda <- qda(Class ~ . , data = training)

pred.qda <- predict(fit.qda , newdata = validation)

confusionMatrix(pred.qda$class,validation$Class) #94.74

#doing via logistic regression


bcan <- read.csv("demo.csv")
str(bcan)
bcan <- subset(bcan,select=-c(`Code`))

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=bcan$Class , p=0.7,list=FALSE)

training   <- bcan[ intrain , ]
validation <- bcan[-intrain , ]

bcan[complete.cases(bcan), ]


mylogit <- glm(Class ~ . ,
               data =training, family=binomial(link="logit"),
               na.action=na.pass)


pred_Cust <- predict(mylogit,newdata=validation,type="response")
#complete the above case with accuracy only

######On glass data set

library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type,p=0.7,list=FALSE)

trainingWOY <- Glass[intrain,]
validationWOY <- Glass[-intrain,]
str(trainingWOY)



library(e1071)
fit.svm <- svm(Type~., type="C",data=trainingWOY, kernel="linear")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf)  # 59.02


#polynomial

library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type,p=0.7,list=FALSE)

trainingWOY <- Glass[intrain,]
validationWOY <- Glass[-intrain,]
str(trainingWOY)

library(e1071)
fit.svm <- svm(Type~., type="C",data=trainingWOY, kernel="polynomial")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf)  #52.46



# for radial


library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type,p=0.7,list=FALSE)

trainingWOY <- Glass[intrain,]
validationWOY <- Glass[-intrain,]
str(trainingWOY)

library(e1071)
fit.svm <- svm(Type~., type="C",data=trainingWOY, kernel="radial")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) #63.93

#Tuning of glass dadta



library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type,p=0.7,list=FALSE)

trainingWOY <- Glass[intrain,]
validationWOY <- Glass[-intrain,]
str(trainingWOY)

library(e1071)
fit.svm <- svm(Type~., type="C",data=trainingWOY, kernel="radial")
svm.pred <- predict(fit.svm, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 


tune.out <- tune(svm,Type~.,data = trainingWOY, kernel="radial",
                 ranges=list(gamma=c(0.001,0.002,0.005,0.007,0.008,0.01,0.1,1,2,3,4),
                             cost=c(0.001,0.002,0.005,0.007,0.008,0.01,0.1,1,2,3,4)))

summary(tune.out)

tune.out$best.model

svm.pred <- predict(tune.out$best.model, newdata=validationWOY)
svm.perf <- table(svm.pred, validationWOY$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf)  # this this time we got the improvement but its not always



