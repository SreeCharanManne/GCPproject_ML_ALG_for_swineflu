library(caret)
library(rpart)
library(rpart.plot)
dataset <- read.csv("Swineflu.csv")

validation_index <- createDataPartition(dataset$Chill, p=0.80, list=FALSE)

validation <- dataset[-validation_index,]

dataset <- dataset[validation_index,]

percentage <- prop.table(table(dataset$Chill)) * 100
cbind(freq=table(dataset$Chill), percentage=percentage)

summary(dataset)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# DT
set.seed(7)
fit.dt <- train(Chill~., data=dataset, method="rpart", metric=metric, trControl=control)

print(fit.dt)
fit.dt1 <- rpart(Chill~., data=dataset)

rpart.plot(fit.dt1)
predictions <- predict(fit.dt, validation)
x<-confusionMatrix(predictions, validation$Chill)
recall <- x$overall['Accuracy']

fileConn<-file("decisiontree_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)