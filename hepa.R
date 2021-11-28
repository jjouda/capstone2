---------------------------------------------------------------
# Hepatitis C virus - Blood based Detection Project
# Capstone project 2
# HarvardX Data Science Professional Certificate: PH125.9x
# author: "jamal jouda"
# date: "11/26/2021"
---------------------------------------------------------------

# install required packages and libraries
if(!require(tidyverse)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("readr", repos = "http://cran.us.r-project.org")  

# load required packages and libraries
library(tidyverse)
library(readr)
library(corrplot)
library(scales)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# download the datast file from kaggle website from link: https://www.kaggle.com/amritpal333/hepatitis-c-virus-blood-biomarkers
# or from github https://github.com/jjouda/capstone2/
# then extract the archive to working directory then rename the csv to hepa.csv
# then read the dataset file and save it in hepa (hepatitis)
hepa <- read_csv("~/hepa.csv")

# show the first 6 rows in the dataset
head(hepa)

# show the structures of dataset
str(hepa)

------------------------------------------------- 
# Dataset Preparing
--------------------------------------------------
# drop serial number column (..1)
hepa = select(hepa, -1)

# number of rows and columns after deleting serial no column
dim(hepa)

# find the sum of na values in the dataset
sum(is.na(hepa))

# find the count of na values for each column
na_values <- hepa %>% summarise_all(funs(sum(is.na(.))))
na_values <- gather(na_values, key = "variables", value = "na_count")
na_values

# plot the count of na values for each column
ggplot(na_values, aes(x = reorder(variables,na_count), y = na_count)) +  
    geom_bar(stat = "identity", fill = "yellowgreen") +  
    ggtitle("Count of na values per coulmn") +  
    labs(x="coulumn", y="na_count") +  
    theme(plot.title = element_text(hjust = 0.5))
	
# delete all na rows in the dataset
hepa <- drop_na(hepa)

# ensure that dataset has no na values
sum(is.na(hepa))

# number of rows and columns after deleting na values
dim(hepa)

# count the numbers of rows in each Category
hepa %>% group_by(Category) %>% summarise(count = n())

# plot rows count in each Category
ggplot(hepa, aes(Category)) + geom_bar(fill = "yellowgreen")

# replace "0=Blood Donor" Category by 0 
hepa$Category <- str_replace_all(hepa$Category, setNames(c("0"), c("0=Blood Donor")))

# replace other Categories by 1 
hepa$Category <- str_replace_all(hepa$Category, setNames(c("1","1","1","1"),  
  c("0s=suspect Blood Donor","1=Hepatitis","2=Fibrosis","3=Cirrhosis")))

# factorized Category column
hepa$Category <- as.factor(hepa$Category)

# count the numbers of rows in each Category
hepa %>% group_by(Category) %>% summarise(count = n())
  
# replace m (male) with 0 and f (female) with 1 in sex column and factorized them
hepa$Sex <- str_replace_all(hepa$Sex, setNames(c("0"), c("m")))
hepa$Sex <- str_replace_all(hepa$Sex, setNames(c("1"), c("f")))

# factorized Sex column
hepa$Sex <- as.factor(hepa$Sex)

--------------------------------------------------------
# Data Analysis
--------------------------------------------------------

# plot correlation matrix for dataset features using corrplot package
hepa_temp = select(hepa, -1, -3)
hepa_cor <- cor(hepa_temp)
corrplot(hepa_cor, method="color",addCoef.col = "black", tl.col="black", number.cex = 0.7)

# plot the distribution of Category (count and percentage)
hepa %>% group_by(Category) %>% summarise(count = n()) %>% mutate(prop =  
round(count/sum(count),2)) %>% ungroup() %>% ggplot(aes(x = Category, y =  
count)) + geom_bar(stat = "identity", fill = "yellowgreen") + 
    labs(x = "Category",y = "Count", title = "distribution of Category") + 
    geom_label(aes(label=percent(prop))) + 
    scale_x_discrete(labels = c("Blood donor","Hepatits")) +  
    theme(plot.title = element_text(hjust = 0.5))
  
# plot the distribution of Age
ggplot(hepa, aes(x = Age)) + geom_histogram(bins = 30, color = "black", fill = "yellowgreen") +  
    ggtitle("distribution of Age") +  
    theme(plot.title = element_text(hjust = 0.5))
	
# plot the distribution of age by Category
ggplot(hepa, aes(x = Age, fill = Category)) + geom_histogram(color= "black", alpha = 0.4) +  
    scale_fill_discrete(labels = c("Blood donor", "Hepatitis")) +  
    ggtitle("distribution of Age by Category") +  
    theme(plot.title = element_text(hjust = 0.5))

# plot the distribution of Sex
hepa %>% group_by(Sex) %>% summarise(count = n()) %>% 
    mutate(prop = round(count/sum(count),2)) %>% 
    ungroup() %>% ggplot(aes(x = Sex, y = count)) +  
    geom_bar(stat = "identity", fill = "yellowgreen") +  
    labs(x = "Sex",y = "Count", title = "distribution of Sex") +  
    geom_label(aes(label=percent(prop))) + scale_x_discrete(labels = c("Male","Female")) +  
    theme(plot.title = element_text(hjust = 0.5))
	
# plot the distribution of Sex by Category
hepa %>% group_by(Sex, Category) %>% summarise(count = n()) %>% 
    mutate(prop = round(count/sum(count),2)) %>% ungroup() %>% 
    ggplot(aes(x = Sex, y = count, fill = Category)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Sex",y = "Count", title = "distribution of Sex by Category") +  
geom_label(aes(label=percent(prop)), show.legend = FALSE) +  
scale_x_discrete(labels = c("Male","Female")) +  
scale_fill_discrete(labels = c("Blood donor", "Hepatits")) +  
theme(plot.title = element_text(hjust = 0.5))

# plot the density distribution of laboratory blood test features 
plot_blood_features <- function(feature){
    i <-1 
    plots <- list()
	for(column in feature){ 
		col <- sym(column) 
        plots[[i]]<-hepa %>% ggplot(aes(x=!!col, fill = Category)) + 
            geom_density(alpha = 0.3) + ggtitle(paste("distribution of", col, "by Category")) + 
            scale_fill_discrete(labels = c("Blood donor", "Hepatits")) + theme_stata()
        i<-i+1
    }
    plot_grid(plotlist = plots,ncol=2)
}
plot_blood_features(c("ALB","ALP"))
plot_blood_features(c("ALT","AST"))
plot_blood_features(c("BIL","CHE"))
plot_blood_features(c("CHOL","CREA"))
plot_blood_features(c("GGT","PROT"))

# boxplot for dataset features
lab_values <- select(hepa, -1, -2, -3)
ggplot(stack(lab_values), aes(x = ind, y = values)) +  
geom_boxplot() + scale_y_log10() + labs(x="Variables", y= "Values")

------------------------------------------------------------------
# Modeling Approach
------------------------------------------------------------------

# split the dataset into training and testing using caret package
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = hepa$Category, times = 1, p = 0.2, list = FALSE)
training <- hepa[-test_index,]
testing <- hepa[test_index,] 
 
# find number of columns and rows in training dataset  
dim(training)  

# find number of columns and rows in testing dataset  
dim(testing)

-------------------------------------------------
# Logistic Regression Model
-------------------------------------------------

# build logestic regression model usin training dataset
glm_model <- glm(Category ~ ., family = "binomial", data=training)
summary(glm_model)

# predict Category on testing using glm_model
preds_glm <- predict(glm_model, newdata = testing, type = "response")
y_preds <- ifelse(preds_glm > 0.5, 1, 0) %>% factor(levels=c(1, 0))

# find accuracy of linear regression model using mean function
mean(y_preds == testing$Category)

# Confusion Matrix for logistic regression model
glm_cm <- confusionMatrix(y_preds , testing$Category)
glm_cm

# summary table for evaluation metrics for logistic regression model
glm_results <- data.frame(TP=glm_cm$table[1,1], FP=glm_cm$table[1,2],  
TN=glm_cm$table[2,2], FN=glm_cm$table[2,1],  
Accuracy=round(glm_cm$overall["Accuracy"]*100,2),  
Sensitivity=round(glm_cm$byClass["Sensitivity"]*100, 2),  
Specificity=round(glm_cm$byClass["Specificity"]*100, 2),  
Balanced_Accuracy=round(glm_cm$byClass["Balanced Accuracy"]*100, 2),  
row.names = "Logistic Regression Model")  
glm_results %>% knitr::kable(align = "cccccccc")

-----------------------------------------------------
# K-Nearest Neighbors (KNN) Model
-----------------------------------------------------

# use train function to find optimal K using 10-fold cross validation
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number = 10, repeats=3, p = .9)
train_knn <- train(Category ~ ., method = "knn", data = training,  
tuneGrid = data.frame(k = seq(1, 5, 0.2)), trControl = control)

# plot accuracy for each k value and select optimal k value
ggplot(train_knn, highlight = TRUE) 
 
# print optimal k value  
train_knn$bestTune

# build final knn model with optimal k = 4.8
knn_model <- knn3(Category ~ ., data = training, k = 4.8)
  
# predict Category in testing using final knn model
preds_knn <- predict(knn_model, newdata = testing, type = "class")
  
# Confusion Matrix for KNN model  
knn_cm <- confusionMatrix(preds_knn , testing$Category)
knn_cm

# summary table for evaluation metrics for knn model
knn_results <- data.frame(TP=knn_cm$table[1,1], FP=knn_cm$table[1,2],  
TN=knn_cm$table[2,2], FN=knn_cm$table[2,1],  
Accuracy=round(knn_cm$overall["Accuracy"]*100,2),  
Sensitivity=round(knn_cm$byClass["Sensitivity"]*100, 2),  
Specificity=round(knn_cm$byClass["Specificity"]*100, 2),  
Balanced_Accuracy=round(knn_cm$byClass["Balanced Accuracy"]*100, 2),  
row.names = "K-Nearest Neighbors (KNN) model")  
knn_results %>% knitr::kable(align = "cccccccc")

---------------------------------------------
# Decision Tree Model  
---------------------------------------------

# use train function to find optimal cp using 10-fold cross validation
set.seed(1, sample.kind = "Rounding")
control = trainControl(method="repeatedcv", number=10, repeats=5, p = .9)
train_dt <- train(Category ~ ., method = "rpart",  
tuneGrid = data.frame(cp = seq(0, 0.1, 0.005)), trControl = control, data = training)
  
# plot accuracy for each cp value and select optimal cp value  
ggplot(train_dt, highlight = TRUE)
  
# print optimal cp value  
train_dt$bestTune

# build final decision tree model with optimal cp = 0.075
dt_model <- rpart(Category~., cp = 0.075, data = training)

# predict Category in testing using final decision tree model
preds_dt <- predict(dt_model, newdata = testing, type = "class") 
 
# Confusion Matrix for decision tree model  
dt_cm <- confusionMatrix(preds_dt , testing$Category)
dt_cm

# plot decision tree
rpart.plot(dt_model)

# summary table for evaluation metrics for decision tree model  
dt_results <- data.frame(TP=dt_cm$table[1,1], FP=dt_cm$table[1,2],  
TN=dt_cm$table[2,2], FN=dt_cm$table[2,1],  
Accuracy=round(dt_cm$overall["Accuracy"]*100,2),  
Sensitivity=round(dt_cm$byClass["Sensitivity"]*100, 2),  
Specificity=round(dt_cm$byClass["Specificity"]*100, 2),  
Balanced_Accuracy=round(dt_cm$byClass["Balanced Accuracy"]*100, 2),  
row.names = "Decision Tree model")  
dt_results %>% knitr::kable(align = "cccccccc")

--------------------------------------------
# Random Forest Model
--------------------------------------------

# use train function to find optimal mtry using 10-fold cross validation
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number = 10, p = .9)
train_rf <- train(Category ~ . ,method = "rf",  
tuneGrid = data.frame(mtry = 1:12), data = training, trControl = control)

# plot accuracy for each mtry value and select optimal mtry value  
ggplot(train_rf, highlight = TRUE)  

# print optimal mtry value
train_rf$bestTune

# build final rf model with optimal mtry = 2
rf_model <- randomForest(Category ~., data = training, mtry = 2, importance = TRUE)

# list importance variable
varImp(train_rf)  

# plot importance variable
ggplot(varImp(train_rf))

# predict Category in testing using final random forest model
preds_rf <- predict(rf_model, newdata = testing, type = "class")  

# Confusion Matrix for random forest model  
rf_cm <- confusionMatrix(preds_rf , testing$Category)
rf_cm

# summary table for evaluation metrics for decision tree model  
rf_results <- data.frame(TP=rf_cm$table[1,1], FP=rf_cm$table[1,2],  
TN=rf_cm$table[2,2], FN=rf_cm$table[2,1],  
Accuracy=round(rf_cm$overall["Accuracy"]*100,2),  
Sensitivity=round(rf_cm$byClass["Sensitivity"]*100, 2),  
Specificity=round(rf_cm$byClass["Specificity"]*100, 2),  
Balanced_Accuracy=round(rf_cm$byClass["Balanced Accuracy"]*100, 2),  
row.names = "Random Forest model")  
rf_results %>% knitr::kable(align = "cccccccc")

--------------------------------------------
# Ensemble Method
--------------------------------------------

# combine glm, knn, dt, rf ML methods
set.seed(1, sample.kind = "Rounding")
ensemble <- cbind(glm = preds_glm == 1, rf = preds_rf == 1, knn = preds_knn == 1, dt = preds_dt == 1)
preds_en <- ifelse(rowMeans(ensemble) > 0.5, 1, 0) %>% factor(levels=c(0, 1))
mean(preds_en == testing$Category)
  
# Confusion Matrix for ensemble model  
en_cm <- confusionMatrix(preds_en, testing$Category)
en_cm  

# summary table for evaluation metrics for ensemble model  
en_results <- data.frame(TP=en_cm$table[1,1], FP=en_cm$table[1,2],  
TN=en_cm$table[2,2], FN=en_cm$table[2,1],  
Accuracy=round(en_cm$overall["Accuracy"]*100,2),  
Sensitivity=round(en_cm$byClass["Sensitivity"]*100, 2),  
Specificity=round(en_cm$byClass["Specificity"]*100, 2),  
Balanced_Accuracy=round(en_cm$byClass["Balanced Accuracy"]*100, 2),  
row.names = "Ensemble model")  
en_results %>% knitr::kable(align = "cccccccc")

--------------------------------------
# Results
--------------------------------------

# final results for all 5 machine learning methods
all_results <- rbind("linear regression" = glm_results,  
"k-nearest neighbors"=  knn_results, "decision tree"= dt_results,  
"random forest"= rf_results, ensemble = en_results)  
all_results %>% knitr::kable(align = "lcccccccc")

