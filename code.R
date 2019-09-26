

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")


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