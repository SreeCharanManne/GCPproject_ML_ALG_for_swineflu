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
#nb
set.seed(7)
fit.nb <- train(Chill~., data=dataset, method="nb", metric=metric, trControl=control)

print(fit.nb)
predictions <- predict(fit.nb, validation)
x<-confusionMatrix(predictions, validation$Chill)
recall <- x$overall['Accuracy']

fileConn<-file("naivebaiyes_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)