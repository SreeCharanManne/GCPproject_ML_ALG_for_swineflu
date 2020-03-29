library(caret)
dataset <- read.csv("Swineflu2.csv")
dataset$Swineflu = as.factor(dataset$Swineflu)

validation_index <- createDataPartition(dataset$Swineflu, p=0.80, list=FALSE)

validation <- dataset[-validation_index,]

dataset <- dataset[validation_index,]

percentage <- prop.table(table(dataset$Swineflu)) * 100
cbind(freq=table(dataset$Swineflu), percentage=percentage)

summary(dataset)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# Random Forest
set.seed(7)
fit.rf <- train(Swineflu~., data=dataset, method="rf", metric=metric, trControl=control)

print(fit.rf)
predictions <- predict(fit.rf, validation)
x<-confusionMatrix(predictions, validation$Swineflu)
recall <- x$overall['Accuracy']
print(recall)

fileConn<-file("Randomforest_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)