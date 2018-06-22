#1)Glass from mlbech 


#using randomforest

library(mlbench)
data("Glass")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]


library(randomForest)
model.RF <- randomForest(Type ~ . , data = training ,
                         na.action=na.roughfix, importance=TRUE)

model.RF
importance(model.RF,type=2) #check which is more influentail more the value more influential mg and al OOB 20.92

#using c forest

library(mlbench)
data("Glass")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]


library(party)
model.RF <- cforest(Type ~ ., data = training, 
                    control = cforest_unbiased(ntree = 50))

model.RF

pred.RF <- predict(model.RF, newdata = validation[,-10])

postResample(pred.RF , validation$Type)

#tuning

library(mlbench)
data("Glass")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

myGrid <- data.frame(mtry=seq(1,6))
tuned.model <- train(Type~.,data = training, 
                     method="rf",tuneGrid=myGrid)
plot(tuned.model)

pred.RF <- predict(tuned.model, newdata = validation[-10],
                   type = "raw")

postResample(pred = pred.RF, obs = validation$Type)






#applying xgboost

library(mlbench)
data("Glass")
str(Glass)
library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

training$Type <- as.integer(training$Type) -1
validation$Type <- as.integer(validation$Type) -1
str(training)
library(xgboost)
training.matrix <- as.matrix(training[,-10])
#dim(training.matrix)
validation.matrix <- as.matrix(validation[,-10])
model.xgb <- xgboost(data=training.matrix,label =training$Type ,nrounds = 9,
                     num_class=6, objective="multi:softmax")

pred.xgb <- predict(model.xgb,newdata=validation.matrix)

confusionMatrix(as.factor(pred.xgb),as.factor(validation$Type))



########tuning the above model
#not preffered
myGrid=data.frame(nrounds=seq(20,50,by = 5),
                  lambda=seq(0.1,0.7,by = 0.1),
                  alpha=seq(0.1,0.7,by = 0.1),
                  eta=seq(0.1,0.7,by = 0.1))
model.xgb.tuned <- train(x = training.matrix,y = training$Type,
                   method = "xgbLinear",tuneGrid = myGrid)

pred.xgb <- predict(model.xgb,newdata=validation.matrix)
postResample(pred.xgb,validation$Type)




####################### stacking



library(caret)
data("Sacramento")
str(Sacramento)

set.seed(2018)
trainIndex <- createDataPartition(y=Sacramento$price,
                                  p=0.7,list=F)
Sacramento <- Sacramento[,-2]

training <- Sacramento[trainIndex, ]
validation <- Sacramento[-trainIndex, ]

#creating lm
fitLM <- lm(price~. , data = training)
pred.LM <- predict(fitLM, newdata = training)

#creating tree using rpart
library(rpart)
fitRegTree <- rpart(price~. , data = training, method = "anova")
pred.RegTree <- predict(fitRegTree, newdata = training)

trndf <- data.frame(price=training$price, 
                    pred.LM, pred.RegTree)
library(randomForest)
fitRF <- randomForest(price~., data = trndf)

#processing on validation set

fitLM2 <- lm(price~. , data = validation)
pred.LM <- predict(fitLM2, newdata = validation)

fitRegTree <- rpart(price~. , data = validation, method = "anova")
pred.RegTree <- predict(fitRegTree, newdata = validation)


valdf <- data.frame(pred.LM, pred.RegTree)
pred.RF <- predict(fitRF, newdata = valdf)

postResample(pred = pred.RF, obs = validation$price)


### Alone Regression Tree
fitRegTree2 <- rpart(price~. , data = training, method = "anova")
pred.RegTree <- predict(fitRegTree2, newdata = validation)
postResample(pred = pred.RegTree, obs = validation$price)

## Alone Random Forest
fitRF <- randomForest(price~., data = training)
pred.RanFor <- predict(fitRF, newdata = validation)
postResample(pred = pred.RanFor, obs = validation$price)

####Alone Linear Regression
pred.LM2 <- predict(fitLM, newdata = validation)
postResample(pred = pred.LM2, obs = validation$price)










