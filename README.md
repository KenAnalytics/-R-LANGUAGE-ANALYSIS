# Machine Learning Using R 
Classification Outcomes of Breast Cancer Data Using K Nearest Neighbor (Machine Learning)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(janitor)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(ggmosaic)
library(lubridate)
install.packages("class")
install.packages("gmodels")

#Load the dataset
read_csv(breast_Cancer)
str(breast_Cancer)

#Remove the id column so it wont affect our prediction Analysis 
breast_Cancer <- breast_Cancer[-1]
View(breast_Cancer)

#the context of the diagnosis variable 
table(breast_Cancer$diagnosis)

#Label the context of the daignosis features
breast_Cancer$diagnosis = factor(breast_Cancer$diagnosis, levels = c("B", "M"),
                                 labels = c("Benign", "Malignant"))

#Check the percentage/proportion 
round(prop.table(table(breast_Cancer$diagnosis)) * 100, digits = 1)

#summarize three numeric features 
summary(breast_Cancer[c("radius_mean", "area_mean", "smoothness_mean")])

#Create a normalization function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#test the normalization function - result should be identical
 normalize(c(1, 2, 3, 4, 5))
 normalize(c(10, 20, 30, 40, 50))

#Normalize the breast cancer data 
 breast_Cancer_n <- as.data.frame(lapply(breast_Cancer[2:31], normalize))

#Verification of your normalization 
 summary(breast_Cancer_n$area_mean)

#Create training and test data 
 breast_Cancer_train <- breast_Cancer_n[1:469, ]
 breast_Cancer_test <- breast_Cancer_n[470:569, ]

#Create labels for  training and test data 
 breast_Cancer_train_labels <- breast_Cancer$diagnosis[1:469]
 breast_Cancer_test_labels  <- breast_Cancer$diagnosis[470:569]
 
#load the "class" library 
 library(class)
#reclasssify test case 
breast_Cancer_test_pred = knn(train = breast_Cancer_train, test = breast_Cancer_test,
                              cl = breast_Cancer_train_labels, k = )

#rows and length 
nrow(breast_Cancer_train)
length(breast_Cancer_train_labels)


#Evaluating model performance 
library(gmodels)
library(class)
breast_Cancer_z <- as.data.frame(scale(breast_Cancer[-1]))
summary(breast_Cancer_z$area_mean)

#create cross tabulation of predicted vs actual 
CrossTable(x = breast_Cancer_test_labels, y = breast_Cancer_test_pred,

Total Observations in Table:  100 

 
                          | breast_Cancer_test_pred 
breast_Cancer_test_labels |    Benign | Malignant | Row Total | 
--------------------------|-----------|-----------|-----------|
                   Benign |        61 |         0 |        61 | 
                          |     1.000 |     0.000 |     0.610 | 
                          |     0.968 |     0.000 |           | 
                          |     0.610 |     0.000 |           | 
--------------------------|-----------|-----------|-----------|
                Malignant |         2 |        37 |        39 | 
                          |     0.051 |     0.949 |     0.390 | 
                          |     0.032 |     1.000 |           | 
                          |     0.020 |     0.370 |           | 
--------------------------|-----------|-----------|-----------|
             Column Total |        63 |        37 |       100 | 
                          |     0.630 |     0.370 |           | 
--------------------------|-----------|-----------|-----------|


Total observations: 100

Correct predictions: 98 (98% accuracy)

Misclassifications: 2 (malignant predicted as benign)

Class-wise:

Benign: All 61 benign cases were correctly classified (100% recall)

Malignant: 37 out of 39 malignant cases were correctly classified (94.9% recall)

Precision: Benign = 96.8%, Malignant = 100%

👉 In summary:
The model predicts benign perfectly and predicts malignant almost perfectly, with just 2 malignant cases mistakenly labeled benign. Overall, the model is highly accurate (98%) and reliable.




            
