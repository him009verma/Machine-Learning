#applying the neural n/w in bank ruptcy data , nnet and neural network
#no need to convert the D into factor here
bank <- read.csv("demo.csv")
bank <- subset(bank,select=-c(1,3))
#bank$D <- factor(bank$D)
library(caret) # createdatapartition
set.seed(2018)
intrain<-createDataPartition(y=bank$D, p=0.7,list=FALSE)
training   <- bank[ intrain , ]
validation <- bank[-intrain , ]
allcolumns <- colnames(bank)
predcolumns <- allcolumns[!allcolumns%in%"D"]
predvars <- paste(predcolumns, collapse = "+")
predformula <- as.formula(paste("D ~ ",predvars))

model.nueral <- neuralnet(formula = predformula, data = training, linear.output = F, hidden = c(4,2))
plot(model.nueral)

pred.neural <- compute(model.nueral,validation[,-1])

#typeof(pred.neural$net.result)
#class(pred.neural$net.result)
dim((pred.neural$net.result))
predicted <- ifelse(pred.neural$net.result[,1]>0.5,1,0)
#str(predicted)- atill its not vector
predicted <- factor(predicted,levels = c(1,0))
validation$D <- factor(validation$D,levels = c(1,0))
str(validation)
confusionMatrix(predicted,validation$D)


############################################################ Using nnet



bank <- read.csv("demo.csv")
bank <- subset(bank,select=-c(1,3))
bank$D <- factor(bank$D)
library(caret) # createdatapartition
library(nnet)
set.seed(2018)
intrain<-createDataPartition(y=bank$D, p=0.7,list=FALSE)

training   <- bank[ intrain , ]
validation <- bank[-intrain , ]


fit.nn <- nnet(D ~ . , data = training ,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"), 
                  levels = c(1,0))

confusionMatrix(pred.nn, factor(validation$D,levels=c(1,0)))



#using h20.deeplearning #AMINST data set

library(h2o)

h2o.init()
train.hex <- as.h2o(training)
valid.hex <- as.h2o(validation)
model.dl <- h2o.deeplearning(y=1,x=2:785,training_frame=train.hex)
pred.dl <- h2o.predict(model.dl,newdata=valid.hex)
pred.df <- as.data.frame(pred.dl)
pred.df$predict <- factor(pred.df$predict)
confusionMatrix(pred.df$predict,valiation$label)

h2o.shutdown()


#using classification tree using Bankruptcy dataset

bank <- read.csv("demo.csv")
bank <- subset(bank,select=-c(1,3))
bank$D <- factor(bank$D)
library(caret) #  for createdatapartition function
set.seed(2018)
intrain<-createDataPartition(y=bank$D, p=0.7,list=FALSE)
training   <- bank[ intrain , ]
validation <- bank[-intrain , ]
library(rpart)
library(rpart.plot)


fitRPart <- rpart(D ~ ., method="class", data=training)

rpart.plot(fitRPart,type=4, extra=1 , main = "Classification of D")   

pred.rpart <- predict(fitRPart , newdata=validation[,-1],
                      type=c("class"))

tblMowers <- table( pred.rpart , validation$D)

confusionMatrix(tblMowers)
plotcp(fitRPart)

class(fitRPart)
optim_CP <- fitRPart$cptable[which.min(fitRPart$cptable[,"xerror"]),"CP"] # one way for getting complexity parameter(CP)

#fitRPart.pruned<- prune(fitRPart, cp= optim_CP ) 
fitRPart.pruned<- prune(fitRPart, cp= 0.08510638298 ) 

pred.rpart.pruned <- predict(fitRPart.pruned , newdata=validation[,-1],
                      type=c("class"))
#creating confusion matrix on pruned tree for checking if the performance has increased

tblMowers.pruned <- table( pred.rpart.pruned , validation$D)

confusionMatrix(tblMowers.pruned)

str(pred.rpart.pruned)









