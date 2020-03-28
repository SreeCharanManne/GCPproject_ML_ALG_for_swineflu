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
# kNN
set.seed(7)
fit.knn <- train(Chill~., data=dataset, method="knn", metric=metric, trControl=control)

print(fit.knn)
predictions <- predict(fit.knn, validation)
x<-confusionMatrix(predictions, validation$Chill)
recall <- x$overall['Accuracy']

fileConn<-file("K-nearest Neighbors_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)