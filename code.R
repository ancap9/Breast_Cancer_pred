

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
colnames(data)<- c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean",
                   "compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimension_mean",
                   "radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se",
                   "concave points_se","symmetry_se","fractal_dimension_se","radius_worst","texture_worst","perimeter_worst",
                   "area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst","symmetry_worst",
                   "fractal_dimension_worst")




class(data)
dim(data)


ggplot(data, aes(x=diagnosis))+geom_bar(fill="red",alpha=0.5)+labs(title="Distribution")



correlationMatrix <- cor(data[,3:ncol(data)])


corrplot(correlationMatrix)
# find features that are highly corrected (corr >0.90)
high_correlation <- findCorrelation(correlationMatrix, cutoff=0.9)


# print indexes of highly correlated attributes
print(high_correlation)

# Remove correlated variables

data_no_cor <- data %>% select(-high_correlation)


#final data set

data <- cbind(diagnosis= data$diagnosis, data_no_cor)


# split data in train and test set
set.seed(1, sample.kind="Rounding")

data_index <- createDataPartition(data$diagnosis, times=1, p=0.7, list = FALSE)
train_set <- data[data_index, ]
test_set <- data[-data_index, ]

fitControl <- trainControl(method="cv", number = 15, classProbs = TRUE,summaryFunction = twoClassSummary)



#logistic regression


logr_model<- train(diagnosis ~ . ,data= train_set, method= "glm",
                   preProcess=c("scale", "center"),metric="ROC", trControl= fitControl)

logr_pred<- predict(logr_model, test_set)

logr_confmatrix<- confusionMatrix(logr_pred, test_set$diagnosis, positive = "M")

logr_confmatrix

plot(varImp(logr_model), top=10, main="Top variables - Log Regr")



#random forest

rf_model<- train(diagnosis ~ . ,data= train_set, method= "rf",
                 preProcess=c("scale", "center"),metric="ROC", trControl= fitControl)

rf_pred<- predict(rf_model, test_set)

rf_confmatrix<- confusionMatrix(rf_pred, test_set$diagnosis, positive = "M")

rf_confmatrix


plot(varImp(rf_model), top=10, main="Top variables - Log Regr")



#knn

knn_model<- train(diagnosis ~ . ,data= train_set, method= "knn",
                 preProcess=c("scale", "center"),metric="ROC", trControl= fitControl, tuneGrid = data.frame(k = seq(9, 71, 2)))

knn_pred<- predict(knn_model, test_set)

knn_confmatrix<- confusionMatrix(knn_pred, test_set$diagnosis, positive = "M")

knn_confmatrix


#qda

qda_model<- train(diagnosis ~ . ,data= train_set, method= "qda",
                  preProcess=c("scale", "center"))

qda_pred<- predict(qda_model, test_set)

qda_confmatrix<- confusionMatrix(qda_pred, test_set$diagnosis, positive = "M")

qda_confmatrix


#summary


models<- list(Log_regr= logr_model, Random_Forest= rf_model, KNN= knn_model,QDA=qda_model)

confusionmatrix<- list(Log_regr= logr_confmatrix, Random_Forest= rf_confmatrix,
                       KNN= knn_confmatrix,QDA=qda_confmatrix)

confusionmatrix_results <- sapply(confusionmatrix, function(x) x$byClass)
confusionmatrix_results %>% knitr::kable()

confusionmatrix_accuracy <- list(confusionmatrix$Log_regr$overall["Accuracy"],
                                 confusionmatrix$Random_Forest$overall["Accuracy"],
                                 confusionmatrix$KNN$overall["Accuracy"],
                                 confusionmatrix$QDA$overall["Accuracy"])
confusionmatrix_accuracy %>% knitr::kable()

