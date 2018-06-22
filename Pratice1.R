can <- read.csv("demo.csv") # csv file path

library(caret)
set.seed(2018)
intrain <- createDataPartition(y=can$Class,
                               p=0.7,list = FALSE)
training <- can[intrain,   ]
validation <- can[-intrain,]
validation$Class <- factor(validation$Class,levels = c('recurrence-events','no-recurrence-events'))
library(e1071)
classifier <- naiveBayes(training[,2:10], training[,11]) 

PredY <- predict(classifier, newdata=validation[,-11], 
                 type="class")

PredYProb <- predict(classifier, newdata=validation[,-11],
                     type="raw")

PredY <- factor(PredY,levels = c("recurrence-events","no-recurrence-events"))

tbl <- table(PredY, validation[,11],
             dnn=list('predicted','actual'))

confusionMatrix(tbl)


##### bank data set example

bank <- read.csv("demo.csv",sep=';')

library(caret)
set.seed(2018)
intrain <- createDataPartition(y=can$Class,
                               p=0.7,list = FALSE)
training <- can[intrain,   ]
validation <- can[-intrain,]
validation$Class <- factor(validation$Class,levels = c('recurrence-events','no-recurrence-events'))
library(e1071)
classifier <- naiveBayes(training[,2:10], training[,11]) 

PredY <- predict(classifier, newdata=validation[,-11], 
                 type="class")

PredYProb <- predict(classifier, newdata=validation[,-11],
                     type="raw")

PredY <- factor(PredY,levels = c("recurrence-events","no-recurrence-events"))

tbl <- table(PredY, validation[,11],
             dnn=list('predicted','actual'))

confusionMatrix(tbl)



