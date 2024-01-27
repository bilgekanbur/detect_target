#Bilge Sena KANBUR
#200709008
#R Project 



#we implement needed library and packages
library(caret)#for knn
library(tidyverse)
library(ggplot2)#for graphics
library(class)
library(naivebayes)#for naive bayes classifier
library(e1071)#for naive bayes
library(rpart)#for decision tree
library(randomForest)#for random forest
library(modelr)
library(plotly)#for graphics
library(rpart.plot)#for graphic decision tree
library(partykit)
library(pROC)#for proc graphic visuliation
library(kernlab)#for kstar algorithm
library(data.table)#for merge the dataframe


#we import dataset for using in project
#in my computer recording file path of dataset
#YOU CHANGE THİS PATH
data <- read.csv("C:\\Users\\bilge\\OneDrive\\Belgeler\\r\\hearth.csv")
#we are looking top 5 data in data set
head(data)
#we examine some knowledge about dataset
summary(data)
#we look how many null data in dataset
sum(is.na(data))
names(data)# for column names

nrow(data)#for how many row in dataset
ncol(data)#for how many column in dataset

dim(data)#we look total row and column number
#we are drawing some graphic about dataset
barPlot <- ggplot(data=data,
                   mapping= aes(x=age, 
                                color=target)) +
  geom_bar() +
  scale_color_brewer(palette = 'Accent') +
  theme_classic() +
  labs(title='Bar Graphic Of Age', 
       x='Age',
       y='Level')

ggplotly(barPlot)

plot(density(data$age), main = "Density Of Age", xlab("Age"), ylab("Density"))

zerocounter=0
onecounter=1
for (i in data$target){
  if (i ==0){
    zerocounter=zerocounter+1
  }
  else{
    onecounter=onecounter+1
  }
}
x <- c(zerocounter,onecounter)
degerler <- c(0,1)
pie(x,degerler, main = "Distruituon Of Target")



plot_ly(
  data = data, x=~chol, 
  y =~trestbps, z=~fbs,
  color=~target,
  type="scatter3d",
  mode="markers"
)
layout(scene= list(xaxis=list(title='chol'),
                   yaxis=list(title='trestbps'),
                   zaxis=list(title='fbs')))


cizim <- ggplot(
  data=data, mapping = aes(x=slope, 
                           y=oldpeak,
                           fill=target)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess") +
  scale_fill_viridis_c() +
  theme_classic() +
  labs(title='ST Graphic', x='Slope', y='Oldpeak')

ggplotly(cizim)


hist(data$chol, main="Graphic Of Chol")
cp <- table(data$cp)
barplot(cp, main = "Barplot Of Cp")

# ggplot2
ggplot(data, aes(x = restecg, y = thalach)) +
  geom_point()



#We were asked to add empty data and instead of doing it manually I did it with the function.
#I made a random distribution with the set.seed method and deleted the data from the columns I specified.
set.seed(123)
missingValues<- data
missingValues[sample(1:nrow(data),3),"exang"]<-NA
missingValues[sample(1:nrow(data),1),"slope"]<-NA
missingValues[sample(1:nrow(data),2),"chol"]<-NA
missingValues[sample(1:nrow(data),1),"ca"]<-NA
missingValues[sample(1:nrow(data),1),"trestbps"]<-NA
missingValues[sample(1:nrow(data),2),"restecg"]<-NA

#I see that a total of 10 data are missing.
sum(is.na(missingValues))

#Now I fill the empty data here with the average value. With the ifelse method I use here,
#if there is missing data in my column, it uses the first condition, otherwise it uses the second condition.
missingValues$age<-ifelse(is.na(missingValues$age),
                               mean(missingValues$age, 
                                    na.rm = TRUE),
                               missingValues$age)

missingValues$sex<-ifelse(is.na(missingValues$sex),
                               mean(missingValues$sex, 
                                    na.rm = TRUE),
                               missingValues$sex)

missingValues$cp<-ifelse(is.na(missingValues$cp),
                              mean(missingValues$cp, 
                                   na.rm = TRUE),
                              missingValues$cp)

missingValues$trestbps<-ifelse(is.na(missingValues$trestbps),
                                    mean(missingValues$trestbps, 
                                         na.rm = TRUE),
                                    missingValues$trestbps)

missingValues$chol<-ifelse(is.na(missingValues$chol),
                                mean(missingValues$chol, 
                                     na.rm = TRUE),
                                missingValues$chol)

missingValues$fbs<-ifelse(is.na(missingValues$fbs),
                               mean(missingValues$fbs, 
                                    na.rm = TRUE),
                               missingValues$fbs)

sum(is.na(missingValues))#7 empty data in dataset


missingValues$restecg<-ifelse(is.na(missingValues$restecg),
                                   mean(missingValues$restecg, 
                                        na.rm = TRUE),
                                   missingValues$restecg)

missingValues$thalach<-ifelse(is.na(missingValues$thalach),
                                   mean(missingValues$thalach, 
                                        na.rm = TRUE),
                                   missingValues$thalach)


missingValues$exang<-ifelse(is.na(missingValues$exang),
                                 mean(missingValues$exang, 
                                      na.rm = TRUE),
                                 missingValues$exang)

missingValues$oldpeak<-ifelse(is.na(missingValues$oldpeak),
                                   mean(missingValues$oldpeak, 
                                        na.rm = TRUE),
                                   missingValues$oldpeak)

missingValues$slope<-ifelse(is.na(missingValues$slope),
                                 mean(missingValues$slope, 
                                      na.rm = TRUE),
                                 missingValues$slope)

missingValues$ca<-ifelse(is.na(missingValues$ca),
                              mean(missingValues$ca, 
                                   na.rm = TRUE),
                              missingValues$ca)

missingValues$thal<-ifelse(is.na(missingValues$thal),
                                mean(missingValues$thal, 
                                     na.rm = TRUE),
                                missingValues$thal)

missingValues$target<-ifelse(is.na(missingValues$target),
                                  mean(missingValues$target, 
                                       na.rm = TRUE),
                                  missingValues$target)



sum(is.na(missingValues))#no empty data in dataset

data<- missingValues #we assigned missingValues to data

#The reason I do this is to distribute my target column homogeneously. so I'm giving it away.
set.seed(123)
data <- data[sample(nrow(data)),]
head(data)

#I proceed to adding a new column.
#The first column I added is BNP. The main factors affecting this value are high blood pressure and high age.
#Its value is higher in women, but it can also be seen in men. A high value affects the risk of heart attack.
#Its normal value is between 0-100.
#If she/he is having a heart attack, BNP is high.

new_column <- function(data){

    data$BNP <- ifelse(data$target==1 &( data$age >= 50 & data$trestbps >=130) ,
                       runif(48, min=100, max=200),#I say generate 48 different values between 0-100 with the runif method.,
                       runif(53, min=0 , max=100)
    )
  
  return(data)
}
#I apply the function to my dataset and a new column is created.
data<-new_column(data)
head(data)

#HFS is heart-fitness score
#If the person has severe pain in the chest while doing sports and the heartbeat is very fast,
#the hfs value is 1, otherwise it is 0.

new_column_two <- function(data) {
  data$HFS <- ifelse(data$thalac >= (220 - data$age) & data$cp >= 2 ,
                     runif(nrow(data), 1, 1),
                     runif(nrow(data), 0, 0))
  return(data)
}
data<-new_column_two(data)
head(data)

#Since I will separate the data, I put the target column last.
data<-data[,c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","BNP","HFS","target" )]
head(data)

#I convert them to integer values into get more accurate values 
#and to avoid problems when I enter new data.
data <- as.data.frame(lapply(data, as.integer))
head(data)

boxplot(data$age, main="Boxplot of outlier data in age" ) #no outlier value
boxplot(data$sex , main="Boxplot of outlier data in sex") #no outlier value
boxplot(data$cp ,  main="Boxplot of outlier data in cp") #no outlier value
#-------------------------------------------
#Here I found my starting and ending values with $stats and my outliers with $out.
#Then I took the max value in $stats and assigned it to a variable.
boxplot(data$trestbps,main="Boxplot of outlier data in trestbps") #present outlier value
outliers1 <- boxplot.stats(data$trestbps)
outliers_out1<- outliers1$out
max_deger1<-max(outliers1$stats, na.rm = TRUE)
outliers1
max_deger1
#--------------------------------------------
boxplot(data$chol,main="Boxplot of outlier data in chol") #present outlier value
outliers2 <- boxplot.stats(data$chol)
outliers_out2<- outliers2$out
max_deger2<-max(outliers2$stats, na.rm = TRUE)
outliers2
max_deger2
#---------------------------------------------
boxplot(data$fbs,main="Boxplot of outlier data in fbs") #no outlier value
boxplot(data$restecg,main="Boxplot of outlier data in restecg") #no outlier value
#---------------------------------------------
boxplot(data$thalach,main="Boxplot of outlier data in thalac") #present outlier value
outliers3 <- boxplot.stats(data$thalach)
outliers_out3<- outliers3$out
max_deger3<-max(outliers3$stats, na.rm = TRUE)
outliers3
max_deger3

#---------------------------------------------
boxplot(data$exang,main="Boxplot of outlier data in exang") #no outlier value

#---------------------------------------------
boxplot(data$oldpeak,main="Boxplot of outlier data in oldpeak") #present outlier value
outliers4 <- boxplot.stats(data$oldpeak)
outliers_out4<- outliers4$out
max_deger4<-max(boxplot.stats(data$oldpeak)$stats, na.rm = TRUE)
outliers4
max_deger4

#----------------------------------------------
boxplot(data$slope,main="Boxplot of outlier data in slope") #no outlier value
#----------------------------------------------
boxplot(data$ca, main="Boxplot of outlier data in ca") #present outlier value
outliers5 <- boxplot.stats(data$ca)
outliers_out5<- outliers5$out
max_deger5<-max(outliers5$stats, na.rm = TRUE)
outliers5
max_deger5
#----------------------------------------------
boxplot(data$thal,main="Boxplot of outlier data in thal") #present outlier value
outliers6 <- boxplot.stats(data$thal)
outliers_out6<- outliers6$out
max_deger6<-max(outliers6$stats, na.rm = TRUE)
outliers6
outliers_out6
max_deger6
#----------------------------------------------
boxplot(data$target,main="Boxplot of outlier data in target") #no outlier value
#----------------------------------------------
boxplot(data$BNP,main="Boxplot of outlier data in BNP")#present outlier values
outliers7 <- boxplot.stats(data$BNP)
outliers_out7<- outliers7$out
max_deger7<-max(outliers7$stats, na.rm = TRUE)
outliers7
outliers_out7
max_deger7
#-----------------------------------------------------
boxplot(data$HFS,main="Boxplot of outlier data in HFS")#no outlier value


#Actually, I compared two vectors here. corresponding columns and vectors of outliers.
#If there is a match, replace it with the contrary max value, otherwise it will stop its own value.
data$thal <- ifelse(!is.na(match(data$thal, outliers_out6)), max_deger6, data$thal)
boxplot(data$thal,main="Boxplot of nonoutlier data in thal")

data$ca <- ifelse(!is.na(match(data$ca, outliers_out5)), max_deger5, data$ca)
boxplot(data$ca,main="Boxplot of nonoutlier data ca")

data$oldpeak<- ifelse(!is.na(match(data$oldpeak, outliers_out4)), max_deger4, data$oldpeak)
boxplot(data$oldpeak,main="Boxplot of nonoutlier data in oldpeak")

data$thalach<- ifelse(!is.na(match(data$thalach, outliers_out3)), max_deger3, data$thalach)
boxplot(data$thalach,main="Boxplot of nonoutlier data in thalach")

data$chol<- ifelse(!is.na(match(data$chol, outliers_out2)), max_deger2, data$chol)
boxplot(data$chol, main="Boxplot of nonoutlier data in chol")

data$trestbps<- ifelse(!is.na(match(data$trestbps, outliers_out1)), max_deger1, data$trestbps)
boxplot(data$trestbps,main="Boxplot of nonoutlier data in trestbps")

data$BNP<- ifelse(!is.na(match(data$BNP, outliers_out7)), max_deger7, data$BNP)
boxplot(data$BNP,main="Boxplot of nonoutlier data in BNP")

#I used mi-max normalization here. I scaled my data between 0-1.
normalized_data <- function (x) {
  return(((x-min(x)))/ ((max(x)-min(x))))
}

#-------------------------------------------------------------------------------------------------------------,
#I divided my data into 70% training and 30% testing. I did not separate it separately for validation.
#Since I will use my data as normalized data and non-normalized data.
#I first separated it and then normalized what I separated.
normal_split_data <- createDataPartition(data$target,
                                         p=0.7,
                                         list = FALSE)
train_data <- data [normal_split_data,]
normalized_train_data<- lapply(train_data[,1:16], normalized_data)
normalized_train_data<-data.frame(normalized_train_data)
test_data <- data[-normal_split_data,]
normalized_test_data<- lapply(test_data[,1:16], normalized_data)
normalized_test_data<-data.frame(normalized_test_data)


head(train_data)
head(normalized_train_data)
head(test_data)
head(normalized_test_data)


#---------------------------------------------------------------------------------------------------------------
#I will use this function to create and view the confusion matrix and other metrics.
features_func<- function(confusion_matrix){
  
  #                     Predicted
  #|                   0    |    1    |
  #  Actual    0  |    TN   |    FP   |
  #            1  |    FN   |    TP   |
  
  
  TP<- confusion_matrix[2, 2]
  TN<- confusion_matrix[1, 1]
  FP<- confusion_matrix[1, 2]
  FN<- confusion_matrix[2, 1]
  
  
  
  accuracy<-(TP+TN)/(TP+TN+FP+FN)
  
  sensitivity<- TP/ (TP+ FN)
  
  specificity<- TN/ (TN+ FP)
  
  precision<- TP/ (TP+ FP)
  recall<- sensitivity # Recall is the same as Sensitivity
  f1_score<- 2 * (precision* recall) / (precision+ recall)
  
  data_frame<- data.frame(Features=c("Accuracy:","Sensitivity:","Specificity:","F1 Score:"),
                          Values=c(accuracy*100,sensitivity*100,specificity*100,f1_score*100))
  
  return(data_frame)
  
}
#---------------------------------------------------------------------------------------------------------------
#knn classifier without normalization

  set.seed(123)
  
  k_values <- seq(1, 29, by = 2)
  accuracy_value <- numeric(length(k_values))
 
#D1 find best k value
  for (k in k_values) {

    predictions <- knn(train = train_data[, 1:15],
                       test = test_data[, 1:15],
                       cl = train_data$target,
                       k = k)
  

    accuracy_value[which(k_values == k)] <- sum(predictions == test_data$target) / length(test_data$target)
  

  accuracy_df <- data.frame(k = k_values, accuracy = accuracy_value)
  }

  print(accuracy_df)

  library(ggplot2)
  ggplot(accuracy_df, aes(x = k, y = accuracy)) +
    geom_line() +
    geom_point() +
    labs(title = "KNN MODEL ACCURACY",
         x = "k values",
         y = "Accuracy")
  

  best_k <- k_values[which.max(accuracy_value)]
  print(paste("Best k value:", best_k))
  train_data$target <- as.factor(train_data$target)

  ctrl <- trainControl(method = "cv", number = 10)
  
#D1 used cross validation for validation , divided 10 equal part
  nonnormalized_knn_model <- train(target ~ ., data = train_data, 
                     method = "knn", 
                     trControl = ctrl, 
                     tuneGrid = data.frame(k = best_k))
  

  print(nonnormalized_knn_model)
  

  predictions <- predict(nonnormalized_knn_model, newdata = test_data)
  test_data$target <- as.factor(test_data$target)
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_knn<-features_func(confusion_matrix)
  print(result_nonnormalized_knn)
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------
#knn classifier with normalization 
  set.seed(123)
  

  k_values <- seq(1, 29, by = 2)
  accuracy_value <- numeric(length(k_values))
  
#D1 find best k value for model
  for (k in k_values) {

    predictions <- knn(train = normalized_train_data[, 1:15],
                       test = normalized_test_data[, 1:15],
                       cl = normalized_train_data$target,
                       k = k)
 
    accuracy_value[which(k_values == k)] <- sum(predictions == normalized_test_data$target) / length(normalized_test_data$target)
    
    
  
    accuracy_df <- data.frame(k = k_values, accuracy = accuracy_value)
  }

  print(accuracy_df)
 
  library(ggplot2)
  ggplot(accuracy_df, aes(x = k, y = accuracy)) +
    geom_line() +
    geom_point() +
    labs(title = "KNN MODEL ACCURACY",
         x = "k values",
         y = "Accuracy")
  

  best_k <- k_values[which.max(accuracy_value)]
  print(paste("Best k value:", best_k))
  normalized_train_data$target <- as.factor(normalized_train_data$target)

  ctrl <- trainControl(method = "cv", number = 10)
  
  
  normalized_knn_model <- train(target ~ ., data = normalized_train_data, 
                     method = "knn", 
                     trControl = ctrl, 
                     tuneGrid = data.frame(k = best_k))
  
  
  print(normalized_knn_model)
  

  predictions <- predict(normalized_knn_model, newdata = normalized_test_data)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)

  
 result_normalized_knn<-features_func(confusion_matrix)
 print(result_normalized_knn)
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#naive bayes model without normalization
 
  train_data$target <- as.factor(train_data$target)

  ctrl <- trainControl(method = "cv", number = 10)#for cross validation
  
# D1 don't use naiveBayes function because D1 did cross validation and naiveBayes function do not give permission cross validation method
# it give error, so D1 use train function and D1 write method parameters
  nonnormalized_nb_model <- train(target ~ ., data = train_data, method = "naive_bayes", trControl = ctrl)
  

  print(nonnormalized_nb_model)
  
  predictions <- predict(nonnormalized_nb_model, newdata = test_data)
  test_data$target <- as.factor(test_data$target)
  
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_nb<-features_func(confusion_matrix)
  print(result_nonnormalized_nb)

#-----------------------------------------------------------------------------------------------------------
#naive bayes classifier with normalization
  normalized_train_data$target <- as.factor(normalized_train_data$target)
# D1 did factor to normalized_train_data because the model is factor and also it wants the dataset is factor
  
  ctrl <- trainControl(method = "cv", number = 10)
  
  normalized_nb_model <- train(target ~ ., data = normalized_train_data, method = "naive_bayes", trControl = ctrl)

  print(normalized_nb_model)
  
  predictions <- predict(normalized_nb_model, newdata = normalized_test_data)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)
  
  result_normalized_nb<-features_func(confusion_matrix)
  print(result_normalized_nb)

#-----------------------------------------------------------------------------------------------------------
#decision tree classifier without normalization
  ctrl <- trainControl(method = "cv", number = 10)
  train_data$target <- as.factor(train_data$target)
  
  nonnormalized_tree_model <- train(target ~ ., data = train_data,trControl = ctrl, method="rpart")

  print(nonnormalized_tree_model)
  
  summary(nonnormalized_tree_model)
  
  rpart.plot(nonnormalized_tree_model$finalModel)#for D1 want to a tree model
  
  predictions <- predict(nonnormalized_tree_model, newdata = test_data)
  test_data$target <- as.factor(test_data$target)
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_dt<-features_func(confusion_matrix)
  print(result_nonnormalized_dt)
#------------------------------------------------------------------------------------------------------------------------------
#decision tree classifier with normalization
  
  ctrl <- trainControl(method = "cv", number = 10)
  normalized_train_data$target <- as.factor(normalized_train_data$target)
  
 
  normalized_decision_tree_model <- train(target ~ ., data = normalized_train_data, method = "rpart", trControl = ctrl)
  
  print(normalized_decision_tree_model)

  summary(normalized_decision_tree_model)
  
  rpart.plot(normalized_decision_tree_model$finalModel)
  
  predictions <- predict(normalized_decision_tree_model, newdata = normalized_test_data)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)
  
  result_normalized_dt<-features_func(confusion_matrix)
  print(result_normalized_dt)

#------------------------------------------------------------------------------------------------------------------------------
#random forest model without normalization
  ctrl <- trainControl(method = "cv", number = 10)
  train_data$target <- as.factor(train_data$target)
#D1 use cross validation
  nonnormalized_rf_model <- train(target ~ ., data = train_data, method = "rf", trControl = ctrl)
  
  print(nonnormalized_rf_model)
  
  summary(nonnormalized_rf_model)
  
  varImpPlot(nonnormalized_rf_model$finalModel)#visualition for classifier
  
  predictions <- predict(nonnormalized_rf_model, newdata = test_data)
  test_data$target <- as.factor(test_data$target)
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_rf<-features_func(confusion_matrix)
  print(result_nonnormalized_rf)

#------------------------------------------------------------------------------------------------------------------------------
#random forest classifier with normalization
  ctrl <- trainControl(method = "cv", number = 10)
  normalized_train_data$target <- as.factor(normalized_train_data$target)
  
  normalized_rf_model <- train(target ~ ., data = normalized_train_data, method = "rf", trControl = ctrl)
  
  print(normalized_rf_model)

  summary(normalized_rf_model)#for model performance
  
  varImpPlot(normalized_rf_model$finalModel)#D1 see calculating gini index for each features
  
  predictions <- predict(normalized_rf_model, newdata = normalized_test_data)
  print(predictions)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)
  
  result_normalized_rf<-features_func(confusion_matrix)
  print(result_normalized_rf)

#------------------------------------------------------------------------------------------------------------------------------
# Stochastic Gradient Descent (SGD) without normalization

  ctrl <- trainControl(method = "cv", number = 10)
  train_data$target <- as.factor(train_data$target)
  
  nonnormalized_sgd_model <- train(target ~ ., data = train_data, method = "svmLinear", trControl = ctrl)
  
  print(nonnormalized_sgd_model)
  
  summary(nonnormalized_sgd_model)
  
  predictions <- predict(nonnormalized_sgd_model, newdata = test_data)
  test_data$target <- as.factor(test_data$target)
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_sgd<-features_func(confusion_matrix)
  print(result_nonnormalized_sgd)
  
  nonnormalized_roc_curve <- roc(test_data$target, as.numeric(predictions))
  plot(nonnormalized_roc_curve, main = "Nonnormalized ROC Curve", col = "red", lwd = 2)

#----------------------------------------------------------------------------------------------------------------------------
# Stochastic Gradient Descent (SGD) with normalization

  ctrl <- trainControl(method = "cv", number = 10)
  normalized_train_data$target <- as.factor(normalized_train_data$target)

  normalized_sgd_model <- train(target ~ ., data = normalized_train_data, method = "svmLinear", trControl = ctrl)
  
  print(normalized_sgd_model)
  
  summary(normalized_sgd_model)
  
  predictions <- predict(normalized_sgd_model, newdata = normalized_test_data)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)
  
  result_normalized_sgd <- features_func(confusion_matrix)
  print(result_normalized_sgd)
  
  roc_curve <- roc(normalized_test_data$target, as.numeric(predictions))
  plot(roc_curve, main = "Normalized ROC Curve", col = "blue", lwd = 2)
  
#----------------------------------------------------------------------------------------------------------------------------
#kstar algorithm without normalization

  ctrl <- trainControl(method = "cv", number = 10)
  train_data$target <- as.factor(train_data$target)
  
  nonnormalized_kstar_model <- ksvm(target ~ ., data = train_data,trControl=ctrl)
  print(nonnormalized_kstar_model)
  
  predictions <- predict(nonnormalized_kstar_model, newdata =test_data)
  test_data$target <- as.factor(test_data$target)
  
  
  confusion_matrix<-table(predictions,test_data$target)
  print(confusion_matrix)
  
  result_nonnormalized_kstar <- features_func(confusion_matrix)
  print(result_nonnormalized_kstar)

#-------------------------------------------------------------------------------------------------------
#kstar algorithm with normalization
  ctrl <- trainControl(method = "cv", number = 10)
  normalized_train_data$target <- as.factor(normalized_train_data$target)
  normalized_kstar_model <- ksvm(target ~ ., data = normalized_train_data,trControl=ctrl)
  print(normalized_kstar_model)
  
  predictions <- predict(normalized_kstar_model, newdata =normalized_test_data)
  normalized_test_data$target <- as.factor(normalized_test_data$target)
  
  
  confusion_matrix<-table(predictions,normalized_test_data$target)
  print(confusion_matrix)
  
  result_normalized_kstar<-features_func(confusion_matrix)
  print(result_normalized_kstar)

#---------------------------------------------------------------------------------------------------------
#D1 take the prediction values from user and D1 did it as function because D1 call everything and when D1 make predictions,
#my data can be dataframe, D1 convert to this values to datafame
#the target is null value because, the model predict it,
#if D1 don't write it, when the model predict, D1 take the error, the target column is present my test and train data
#the values can be numeric, D1 check this.
prediction_value<- function(){
  for (i in length(ncol(data)-1)){
  age<- as.numeric(readline(prompt = "Please enter your age:"))
  sex<- as.numeric(readline(prompt = "Please enter your sex(0:Female,1:Male):"))
  cp<- as.numeric(readline (prompt = "Please enter your  chest pain type(0-1-2-3):"))
  trestbps<- as.numeric(readline(prompt = "Please enter  resting blood pressure:"))
  chol<- as.numeric(readline(prompt = "Please enter your  serum cholestoral in mg/dl:"))
  fbs<- as.numeric(readline(prompt = "Please enter your fbs( fasting blood sugar > 120 mg/dl) (1 = true; 0 = false):"))
  restecg<- as.numeric(readline (prompt = "Please enter your  resting electrocardiographic results:"))
  thalach<- as.numeric(readline(prompt = "Please enter your maximum heart rate achieved:"))
  exang<- as.numeric(readline(prompt = "Please enter your  exercise induced angina (1 = yes; 0 = no):"))
  oldpeak<- as.numeric(readline(prompt = "Please enter your ST depression induced by exercise relative to rest:"))
  slope<-as.numeric(readline(prompt = "Please enter your the slope of the peak exercise ST segment:"))
  ca<- as.numeric(readline(prompt = "Please enter your = number of major vessels (0-3) colored by flourosopy:"))
  thal<- as.numeric(readline(prompt = "Please enter your thal(normal; 6 = fixed defect; 7 = reversable defect):"))
  BNP<- as.numeric(readline(prompt = "Please enter your BNP:"))
  HFS<-as.numeric(readline(prompt = "Please enter your HFS(0 or 1):"))

}
new_data_point<- data.frame("age"=age, "sex" =sex, "cp"= cp, "trestbps"=trestbps,"chol"=chol,"fbs"=fbs,
                            "restecg"=restecg,"thalach"=thalach, "exang"=exang,"oldpeak"=oldpeak,"slope"=slope,
                            "ca"=ca,"thal"=thal,"BNP"=BNP,"HFS"=HFS,"target"=NA)


return(new_data_point)
}

#-----------------------------------------------------------------------------------------------------------------------------------

  
#D1 merged my all models with these other features. 
merged_dt <- rbindlist(list( "Normalized Kstar"=result_normalized_kstar , "Nonnormalized Kstar"= result_nonnormalized_kstar,
                             "Normalized SGD"=result_normalized_sgd, "Nonnormalized SGD"=result_nonnormalized_sgd,
                             "Normalized Random Forest"=result_normalized_rf , "Nonnormalized Random Forest"= result_nonnormalized_rf,
                             "Normalized Decision Tree"=result_normalized_dt, "Nonnormalized Decision Tree"=result_nonnormalized_dt,
                             "Normalized Naive Bayes"=result_normalized_nb , "Nonnormalized Naive Bayes"= result_nonnormalized_nb,
                             "Normalized KNN"=result_normalized_knn, "Nonnormalized KNN"=result_nonnormalized_knn), idcol = "Source")
print(merged_dt)
#I made a dataframe where the accuracy values of all models are shown.
k_values <- seq(1, nrow(merged_dt), by = 4) 
merge_df <- data.frame(Model = character(), Values = numeric()) 

for (k in k_values) {
  selected_rows <- merged_dt[k, ]
  merge_df <- rbind(merge_df, selected_rows)  
}

print(merge_df)

#Among these models, I found the model with best accuracy and printed it with accuracy.
max_row <- merge_df[which.max(as.numeric(gsub("Accuracy: ", "", merge_df$Values))), ]

print("The highest model and this accuracy:")
print(max_row)

#I wrote and recorded my best model. 
#Then I made a prediction by entering a row of data from my dataset. It gave the target value correctly.
best_model <- nonnormalized_rf_model
saveRDS(best_model, file = "best_model.rds")
loaded_model <- readRDS(file = "best_model.rds")
rf_predictions<- predict(loaded_model, new_data_point)
print(rf_predictions)
new_data_point[] <- 0#to clear the data set





