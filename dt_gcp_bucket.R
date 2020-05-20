library(cloudml)
library(readr)
library(caret)

data_dir <- gs_data_dir_local("gs://cmuni1")
dataset <- read_csv(file.path(data_dir, "Swineflu2.csv"))
dataset$Swineflu = as.factor(dataset$Swineflu)

validation_index <- createDataPartition(dataset$Swineflu, p=0.80, list=FALSE)

validation <- dataset[-validation_index,]

dataset <- dataset[validation_index,]

percentage <- prop.table(table(dataset$Swineflu)) * 100
cbind(freq=table(dataset$Swineflu), percentage=percentage)

summary(dataset)

control <- trainControl(method="cv", number=7)
metric <- "Accuracy"
# DT
set.seed(7)
fit.dt <- train(Swineflu~., data=dataset, method="rpart", metric=metric, trControl=control)


predictions <- predict(fit.dt, validation)
x<-confusionMatrix(predictions, validation$Swineflu)
recall <- x$overall['Accuracy']
print(recall)

fileConn<-file("decisiontree_accuracy_for_swineflu.txt")
writeLines("Accuracy of naivebaiyes : ", fileConn)
o=toString(recall)
writeLines(c(o), fileConn)
close(fileConn)
