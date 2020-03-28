library(caret)
dataset <- read.csv("Swineflu.csv")

validation_index <- createDataPartition(dataset$Chill, p=0.80, list=FALSE)

validation <- dataset[-validation_index,]

dataset <- dataset[validation_index,]

percentage <- prop.table(table(dataset$Chill)) * 100
cbind(freq=table(dataset$Chill), percentage=percentage)

summary(dataset)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# Random Forest
set.seed(7)
fit.rf <- train(Chill~., data=dataset, method="rf", metric=metric, trControl=control)

print(fit.rf)
predictions <- predict(fit.rf, validation)
x<-confusionMatrix(predictions, validation$Chill)
recall <- x$overall['Accuracy']

fileConn<-file("Randomforest_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)