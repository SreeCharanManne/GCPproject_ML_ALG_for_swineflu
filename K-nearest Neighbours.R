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
# kNN
set.seed(7)

fit.knn <- train(Swineflu~., data=dataset, method="knn", metric=metric, trControl=control)

print(fit.knn)
predictions <- predict(fit.knn, validation)
x<-confusionMatrix(predictions, validation$Swineflu)
recall <- x$overall['Accuracy']
print(recall)

fileConn<-file("K-nearest Neighbors_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)