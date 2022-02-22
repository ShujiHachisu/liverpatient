################################################################################
# Create training set, validation set (final hold-out test set)
################################################################################


# Note: this process could take a couple of minutes

# install packages if needed

# setup libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# load libraries

library(tidyverse)
library(caret)
library(data.table)
library(corrplot)

# download data, create dataframe, name columns, create factors, and omit rows with blank values

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", dl)

# read csv file and store in liverPatient

liverPatient <- read.csv(file = dl)

# set column names

colnames(liverPatient) <- c("Age", "Gender", "Total_Bilirubin", "Direct_Bilirubin",
                      "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase",
                      "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio", "Dataset")

# covert DataSet columns so that healthy patients are zero and sick patients are 1.
# Convert gendar into factor and drop rows with empty fields

liverPatient <-
  liverPatient %>%
  mutate(Dataset = factor(Dataset - 1),
         Gender = factor(Gender)) %>%
  na.omit()


# create both training set and validation set 
# Validation set will be 10% of data set

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = liverPatient$Dataset, times = 1, p = 0.1, list = FALSE)

training_set <- liverPatient[-test_index,]
test_set <- liverPatient[test_index,]

# remove unnecessary variable and data sets
rm(dl, test_index, liverPatient)

################################################################################
# Explore and analyse data set
################################################################################


# show first 6 rows of the training set to get insight into the table and data structure
head(training_set)
# show data structure, column names and number of records
str(training_set)
# show summary statistics of training set
summary(training_set)
# training set data set has 519 records and 11 fields

# show first 6 rows of the test set to get insight into the table and data structure
head(test_set)
# show data structure, column names and number of records
str(test_set)
# show summary statistics of test set
summary(test_set)
# Test set has 59 records and 11 fields.
# This will be used at the final stage to evaluated the model created using the training data set

######################### age plots ############################################

# plot a histogram by age for training set colored by Dataset
training_set %>%
  ggplot(aes(Age, fill = Dataset)) +
  geom_histogram(binwidth = 20, color = "black") +
  labs(title = "Age vs Number of people", y = "Number of people") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))


# plot a normalized histogram by age for training set colored by Dataset
training_set %>%
  ggplot(aes(Age, fill = Dataset)) +
  geom_histogram(binwidth = 20, color = "black", position = "fill") +
  labs(title = "Age vs proportion of people", y = "Proportion of people") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))




######################### gender plots ##########################################

# plot a histogram by gender for training set with proportion of Dataset shown
# by color
training_set %>% 
  group_by(Dataset, Gender) %>%
  summarize(count = n()) %>%
  ggplot(aes(Gender, count, fill = Dataset)) + 
  geom_col() +
  labs(title = "Gender vs Number of people", y = "Number of people") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# plot a normalized histogram by gender for training set with proportion of
# Dataset shown by color
training_set %>% 
  group_by(Dataset, Gender) %>%
  summarize(count = n()) %>%
  ggplot(aes(Gender, count, fill = Dataset)) + 
  geom_col(position = "fill") +
  labs(title = "Gender vs proportion of people", y = "Proportion of people") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Dataset plots ########################################

# plot a histogram by Dataset for training set colored by gender
training_set %>% 
  group_by(Dataset, Gender) %>%
  summarize(count = n()) %>%
  ggplot(aes(Dataset, count, fill = Gender)) +
  geom_col() +
  labs(title = "Liver diseased vs Number of people",
       y = "Number of people",
       x = "liver diasesed (no disease = 0, diseased = 1)")

# plot a normalized histogram by Dataset for training set colored by gender
training_set %>% 
  group_by(Dataset, Gender) %>%
  summarize(count = n()) %>%
  ggplot(aes(Dataset, count, fill = Gender)) +
  geom_col(position = "fill") +
  labs(title = "Liver diseased vs proportion of people",
       y = "Proportion of people",
       x = "liver diasesed (no disease = 0, diseased = 1)")



######################### Total Bilirubin plots ##########################################

# Plot a histogram by Total Bilirubin colored by Dataset
training_set %>%
  ggplot(aes(Total_Bilirubin, fill = Dataset)) +
  geom_histogram(binwidth = 3, color = "black") +
  labs(title = "Total Bilirubin vs Number of people",
       y = "Number of people",
       x = "Total Bilirubin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Total Bilirubin colored by Dataset
training_set %>%
  ggplot(aes(Total_Bilirubin, fill = Dataset)) +
  geom_histogram(binwidth = 3, color = "black", position = "fill") +
  labs(title = "Total Bilirubin vs proportion of people",
       y = "Proportion of people",
       x = "Total Bilirubin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Direct Bilirubin plots ###############################

# Plot a histogram by Direct Bilirubin colored by Dataset
training_set %>%
  ggplot(aes(Direct_Bilirubin, fill = Dataset)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Direct Bilirubin vs Number of people",
       y = "Number of people",
       x = "Direct Bilirubin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Direct Bilirubin colored by Dataset
training_set %>%
  ggplot(aes(Direct_Bilirubin, fill = Dataset)) +
  geom_histogram(binwidth = 1, color = "black", position = "fill") +
  labs(title = "Direct Bilirubin vs proportion of people",
       y = "Proportion of people",
       x = "Direct Bilirubin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Alkaline Phosphotase plots ###########################

# Plot a histogram by Alkaline Phosphotase colored by Dataset
training_set %>%
  ggplot(aes(Alkaline_Phosphotase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(title = "Alkaline Phosphotase vs Number of people",
       y = "Number of people",
       x = "Alkaline Phosphotase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Alkaline Phosphotase colored by Dataset
training_set %>%
  ggplot(aes(Alkaline_Phosphotase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black", position = "fill") +
  labs(title = "Alkaline Phosphotase vs Proportion of people",
       y = "Proportion of people",
       x = "Alkaline Phosphotase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Alamine Aminotransferase plots ########################

# Plot a histogram by Alamine Aminotransferase colored by Dataset
training_set %>%
  ggplot(aes(Alamine_Aminotransferase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(title = "Alamine Aminotransferase vs Number of people",
       y = "Number of people",
       x = "Alamine Aminotransferase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Alamine Aminotransferase colored by Dataset
training_set %>%
  ggplot(aes(Alamine_Aminotransferase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black", position = "fill") +
  labs(title = "Alamine Aminotransferase vs Proportion of people",
       y = "Proportion of people",
       x = "Alamine Aminotransferase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))




######################### Aspartate Aminotransferase plots #####################

# Plot a histogram by Aspartate Aminotransferase colored by Dataset
training_set %>%
  ggplot(aes(Aspartate_Aminotransferase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(title = "Aspartate Aminotransferase vs Number of people",
       y = "Number of people",
       x = "Aspartate Aminotransferase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Aspartate Aminotransferase colored by Dataset
training_set %>%
  ggplot(aes(Aspartate_Aminotransferase , fill = Dataset)) +
  geom_histogram(binwidth = 100, color = "black", position = "fill") +
  labs(title = "Aspartate Aminotransferase vs Proportion of people",
       y = "Proportion of people",
       x = "Aspartate Aminotransferase") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Total Protein plots ##################################
 
# Plot a histogram by Total Proteins colored by Dataset
training_set %>%
  ggplot(aes(Total_Protiens, fill = Dataset)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Total Proteins vs Number of people",
       y = "Number of people",
       x = "Total Proteins") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Total Protiens colored by Dataset
training_set %>%
  ggplot(aes(Total_Protiens, fill = Dataset)) +
  geom_histogram(binwidth = 0.5, color = "black", position = "fill") +
  labs(title = "Total Proteins vs Proportion of people",
       y = "Proportion of people",
       x = "Total Proteins") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Albumin plots ########################################

# Plot a histogram by Albumin colored by Dataset
training_set %>%
  ggplot(aes(Albumin, fill = Dataset)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Albumin vs Number of people",
       y = "Number of people",
       x = "Albumin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Albumin colored by Dataset
training_set %>%
  ggplot(aes(Albumin, fill = Dataset)) +
  geom_histogram(binwidth = 0.5, color = "black", position = "fill") +
  labs(title = "Albumin vs Proportion of people",
       y = "Proportion of people",
       x = "Albumin") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



######################### Albumin and Globulin Ratio plots #####################

# Plot a histogram by Albumin and Globulin Ratio colored by Dataset
training_set %>%
  ggplot(aes(Albumin_and_Globulin_Ratio, fill = Dataset)) +
  geom_histogram(binwidth = 0.2, color = "black") +
  labs(title = "Albumin and Globulin Ratio vs Number of people",
       y = "Number of people",
       x = "Albumin and Globulin Ratio") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))

# Plot a normalized histogram by Albumin and Globulin Ratio colored by Dataset
training_set %>%
  ggplot(aes(Albumin_and_Globulin_Ratio, fill = Dataset)) +
  geom_histogram(binwidth = 0.2, color = "black", position = "fill") +
  labs(title = "Albumin and Globulin Ratio vs Proportion of people",
       y = "Proportion of people",
       x = "Albumin and Globulin Ratio") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))



###################  Principal Component Analysis (PCA)  #######################

# convert test and training sets from data frames to matrices
test_set_matrix <-
  test_set[,1:10] %>%
  mutate(Gender = Gender == "Male") %>%
  as.matrix()

training_set_matrix <-
  training_set[,1:10] %>%
  mutate(Gender = Gender == "Male") %>%
  as.matrix()

# calculate correlation between fields in the training set
mydata_cor <- cor(training_set_matrix)

# plot correlation 
corrplot(cor(mydata_cor))

# plot correlation as heat map
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = mydata_cor, col = palette, symm = TRUE)

# carry out PCA on training set
pca <- prcomp(training_set_matrix)

# show summary of PCA to gain insight on the PCs that account for most of the variance 
summary(pca)

# plot PC1 vs PC2 and color point by whehter teh patient is sick or not
data.frame(pca$x[,1:2], Sick=training_set$Dataset) %>%
  ggplot(aes(PC1,PC2, fill = Sick))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1) +
  labs(title = "PC1 vs PC2") +
  scale_fill_discrete(name = "Disease status",
                      labels = c("no disease", "liver disease"))
  



################################################################################
# Create ML model that predicts whether a patient is sick or not
################################################################################

# create a function that returns the performance of a model in a data frame

model_performance <-
  function (model, name_of_model) {
    # measure and record performance of model
    cm <- confusionMatrix(predict(model, test_set), test_set$Dataset, positive = '1')
    # create a data frame with overall precision, sensitivity, specificity and accuracy
    data_frame(Method = name_of_model,
               Precision = cm$byClass["Precision"],
               sensitivity = cm$byClass["Sensitivity"],
               Specificity = cm$byClass["Specificity"],
               Balanced_Accuracy = cm$byClass["Balanced Accuracy"]) 
  }


#################### Generalized Linear Model (GLM) ############################

# create a linear model
train_glm <- 
  train(Dataset ~ .,
        method = "glm",
        data = training_set)

# assess performance of results and store in a data frame
results <- model_performance(train_glm, "linear model")

# print out all the results for all the models explored above
results %>% knitr::kable()


# create a linear model with the 5 most significant PCs
train_glm_pca <- 
  train(Dataset ~ .,
        method = "glm",
        data = training_set,
        preProcess = "pca",
        trControl = trainControl(preProcOptions = list(pcaComp = 5)))

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_glm_pca, "linear model with 5 PCs"))

# print out all the results for all the models explored above
results %>% knitr::kable()



####################### Boosted Logistic Regression (LogitBoost) #############################

# create a LogitBoost model   
train_LogitBoost <- 
  train(Dataset ~ .,
        method = "LogitBoost",
        data = training_set)

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_LogitBoost, "LogitBoost"))

# print out all the results for all the models explored above
results %>% knitr::kable()


# create a KNN model with the 5 most significant PCs
train_LogitBoost_pca <- 
  train(Dataset ~ .,
        method = "LogitBoost",
        data = training_set,
        preProcess = "pca",
        trControl = trainControl(preProcOptions = list(pcaComp = 5)))

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_LogitBoost_pca, "LogitBoost with 5 PCs"))

# print out all the results for all the models explored above
results %>% knitr::kable()



########################### Random Forest ######################################

# create a random forest model  
train_Rborist <- 
  train(Dataset ~ .,
        method = "Rborist",
        tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
        data = training_set)

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_Rborist, "random forest"))

# print out all the results for all the models explored above
results %>% knitr::kable()


# create a random forest with the 5 most significant PCs
train_Rborist_pca <- 
  train(Dataset ~ .,
        method = "Rborist",
        tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
        data = training_set,
        preProcess = "pca",
        trControl = trainControl(preProcOptions = list(pcaComp = 5)))

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_Rborist_pca, "random forest wiht with 5 PCs"))

# print out all the results for all the models explored above
results %>% knitr::kable()




####################### k-nearest neighbors (KNN) #############################

# create a KNN model   
train_knn <- 
  train(Dataset ~ .,
        method = "knn",
        data = training_set)

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_knn, "KNN"))

# print out all the results for all the models explored above
results %>% knitr::kable()


# create a KNN model with the 5 most significant PCs
train_knn_pca <- 
  train(Dataset ~ .,
        method = "knn",
        data = training_set,
        preProcess = "pca",
        trControl = trainControl(preProcOptions = list(pcaComp = 5)))

# assess performance of results and add store as a new row in results
results <- bind_rows(results, model_performance(train_knn_pca, "KNN with 5 PCs"))

# print out all the results for all the models explored above
results %>% knitr::kable()

#explore performance of model using 1-10 PCs
knn_results <- sapply(seq(1,10), function(pc) {
  train_knn_pca <- 
    train(Dataset ~ .,
          method = "knn",
          data = training_set,
          preProcess = "pca",
          trControl = trainControl(preProcOptions = list(pcaComp = pc)))
  cm <- confusionMatrix(predict(train_knn_pca, test_set), test_set$Dataset, positive = '1')
  data_frame(PC = pc,
             Precision = cm$byClass["Precision"],
             sensitivity = cm$byClass["Sensitivity"],
             Specificity = cm$byClass["Specificity"],
             Balanced_Accuracy = cm$byClass["Balanced Accuracy"]) 
})

# print out the performance of models using different number of PCs
knn_results

