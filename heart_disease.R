############################################################
#
##### Analysis of clinical trial data
#
#
## Load required packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)

# import dataset
data <- read.csv("C:/Users/User/Documents/cw_clinical_trials (1).csv")

# Investigating the structure of the dataset
str(data)
summary(data)

### Exploratory Data Analysis
#
## Age Distribution
#
# From the data summary, the age range is 29-76 with the mean age of 54

# Create a histogram with a density curve
ggplot(data, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", 
                 color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Histogram of age distribution with Density Curve",
       x = "Age",
       y = "Density") +
  theme_minimal()

############################################################

## Gender nd heart disease
# Determining the distribution of heart disease (num) among the male and female
#
# Convert "num" and "sex" to factor variables
data$sex <- as.factor(data$sex)
data$num <- as.factor(data$num)

# Create a bar chart using ggplot2
ggplot(data, aes(x = sex, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c('Male', 'Female')) +
  labs(title = "Frequency of heart disease status by Sex",
       x = "Sex",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red")) 

# Perform Chi-Square test to test if the observed relationship between sex 
# and heart disease status is statistically significant
chisq.test(data$sex, data$num)

# The result shows that X-squared = 3.3885 and p-value = 0.06565

#########################################################

## Convert all categorical variables to factor variables
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)

## Relationship between chest pain type and heart disease status
#
# Create a bar chart using ggplot2
ggplot(data, aes(x = cp, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("type 1", "type 2", "type 3", "type 4")) +
  labs(title = "Chest Pain type and heart disease status",
       x = "Chest Pain",
       y = "Frequency") +
  scale_fill_manual(values = c("navyblue", "red")) 

# Perform Chi-Square test to test if the observed relationship between 
# chest pain type and heart disease status is statistically significant
chisq.test(data$cp, data$num)

# The result shows that X-squared = 49.715 and p-value = 9.187e-11

#########################################################

## Relationship between Fasting blood sugar and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = fbs, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("< 120 mg/dl", "> 120 mg/dl" )) +
  labs(title = "Fasting blood sugar and heart disease status",
       x = "Fasting blood sugar",
       y = "Frequency") +
  scale_fill_manual(values = c("steelblue", "red"))

# Perform Chi-Square test to test if the observed relationship between 
# fasting blood sugar and heart disease status is statistically significant
chisq.test(data$fbs, data$num)

# The result shows that X-squared = 0.00065523 and p-value = 0.9796

#########################################################

## Relationship between Resting ECG and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = restecg, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("Normal", "Having ST-T wave abnormality", 
                              "Ventricular hypertrophy")) +
  labs(title = "Resting ECG and heart disease status",
       x = "Resting ECG",
       y = "Frequency") +
  scale_fill_manual(values = c("darkblue", "darkred"))

# Perform Chi-Square test to test if the observed relationship between 
# resting ECG and heart disease status is statistically significant
chisq.test(data$restecg, data$num)

# The result shows that X-squared = 4.6833 and p-value = 0.09617

#########################################################

## Relationship between Slope of the peak exercise and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = slope, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("Upsloping", "Flat","Downsloping")) +
  labs(title = "Slope of Peak Exercise and heart disease status",
       x = "Slope",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"))

# Perform Chi-Square test to test if the observed relationship between 
# slope and heart disease status is statistically significant
chisq.test(data$slope, data$num)

# The result shows that X-squared = 18.217 and p-value = 0.0001107

##############################################################

## Relationship between exercise induced angina and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = exang, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Exercise Induced Angina and Heart Disease Status",
       x = "Exercise Induced Angina",
       y = "Frequency") +
  scale_fill_manual(values = c("navyblue", "red"))

# Perform Chi-Square test to test if the observed relationship between 
# Exercise Induced Angina and heart disease status is statistically significant
chisq.test(data$exang, data$num)

# The result shows that X-squared = 18.058, and p-value = 2.143e-05

##############################################################

## Relationship between no of major vessels and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = ca, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Number of major vessels and Heart Disease Status",
       x = "Major Vessels",
       y = "Frequency") +
  scale_fill_manual(values = c("navyblue", "red"))

# Perform Chi-Square test to test if the observed relationship between number 
# of major vessels and heart disease status is statistically significant
chisq.test(data$ca, data$num)

# The result shows that X-squared = 21.555 and p-value = 8.073e-05

##############################################################

## Relationship between thalassemia and heart disease status
#
# Create a bar chart of the relationship
ggplot(data, aes(x = thal, fill = num)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(labels = c("Normal", "Fixed defect", "Reversible defect")) +
  labs(title = "Thalassemia and Heart Disease Status",
       x = "Thal Status",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"))

# Perform Chi-Square test to test if the observed relationship between 
# thalassemia and heart disease status is statistically significant
chisq.test(data$thal, data$num)

# The result shows that X-squared = 37.649 and p-value = 6.679e-09

##############################################################

## Relationship between age and heart disease status
#
# Create a boxplot for the relationship between age and num
ggplot(data, aes(x = num, y = age)) +
  geom_boxplot(fill = "thistle", color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Boxplot of Age by Heart disease Status",
       x = "Heart Disease",
       y = "Age")

# Perform a t-test to determine if the observed relationship between 
# age of patient and heart disease status is statistically significant
t_test_result <- t.test(age ~ num, data = data)

# Display the results
print(t_test_result)

# Result shows t = -1.9828, p-value = 0.04976, mean in group 0 = 52.61905,
# mean in group 1 = 56.05263

###############################################################

## Relationship between Cholesterol level and heart disease status
#
# Create a boxplot for the relationship between chol and num
ggplot(data, aes(x = num, y = chol)) +
  geom_boxplot(fill = "thistle", color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Boxplot of serum Cholesterol by Heart disease Status",
       x = "Heart Disease",
       y = "Cholesterol (mg/dl)")

# Perform a t-test to determine if the observed relationship between the  
# cholesterol level and heart disease status is statistically significant
t_test_result <- t.test(chol ~ num, data = data)

# Display the results
print(t_test_result)

# Result shows t = 0.023222, p-value = 0.9815, mean in group 0 = 249.8413,
# mean in group 1 = 249.5789

#######################################################

## Relationship between resting blood pressure and heart disease status
#
# Create a boxplot for the relationship between chol and num
ggplot(data, aes(x = num, y = trestbps)) +
  geom_boxplot(fill = "thistle", color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Boxplot of resting blood pressure by Heart disease Status",
       x = "Heart Disease",
       y = "Resting BP")

# Perform a t-test to determine if the observed relationship between resting 
# blood pressure and heart disease status is statistically significant
t_test_result <- t.test(trestbps ~ num, data = data)

# Display the results
print(t_test_result)

# Result shows t = -1.5559, p-value = 0.1229, mean in group 0 = 128.1905,
# mean in group 1 = 133.4211

#################################################

## Relationship between ST Depression and heart disease status
#
# Create a boxplot for the relationship between oldpeak and num
ggplot(data, aes(x = num, y = oldpeak)) +
  geom_boxplot(fill = "thistle", color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Boxplot of ST Depression (oldpeak) by Heart disease Status",
       x = "Heart Disease",
       y = "oldpeak")

# Perform a t-test to determine if the observed relationship between 
# ST Depression and heart disease status is statistically significant
t_test_result <- t.test(oldpeak ~ num, data = data)

# Display the results
print(t_test_result)

# Result shows t = -4.7862, p-value = 6.465e-06, mean in group 0 = 0.6619048,
# mean in group 1 = 1.6228070

########################################################

## Perform a logistic regression model on the data
#
# create the logistic regression model
model <- glm(num ~., family = binomial, data = data)

# obtain the summary of the logistic regression
summary(model)

# determine the accuracy of the model by training and testing the model
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]  

#fit logistic regression model
train_model <- glm(num~., family = binomial, data = train)

# use the trained model to obtain predictions probablity on test data
test$pred_prob <- predict(train_model,test,type="response")

# transform those probabilities into successes and failures (1’s and 0’s),
# and we save them under the variable “model_pred”.
test <- test  %>% mutate(model_pred = 1*(pred_prob > .53) + 0)

# compare the newly created columns “model_pred” and “num” 
# this is done to calculate the accuracy of our model.
test <- test %>% mutate(accurate = 1*(model_pred == num))
sum(test$accurate, na.rm = TRUE)/nrow(test)

# model accuracy = 0.8108108

#obtain the p-values of each symptom
p_values <- coef(summary(model))[,4]

# create a dataframe for the p values of the symptoms
p_df <- as.data.frame(p_values)

# convert the rownames(i.e. the symptoms) to the first column
p_df <- tibble::rownames_to_column(p_df, "Variables")

# delete the first row of the dataframe... it contains intercept of the model
p_df <- p_df[-1,]

# identify the significant variables. These are variables with 
# p-value less than 0.05
sig_variables <- filter(p_df, p_values < 0.05)
# arrange the variables from the most significant to the least significant
sig_variables <- arrange(sig_variables, desc(p_values))

# plot a bar chart of the significant symptoms
# Reorder the "Symptoms" variable based on descending "p_values" 
# so as to plot bar chart of the symptoms in order of significance
sig_variables$Variables <- reorder(sig_variables$Variables, -sig_variables$p_values)

# Create the plot with the reordered data
plt <- ggplot(sig_variables) +
  geom_col(aes(x = log(p_values), y = Variables), fill = "blue", width = 0.9) +
  scale_y_discrete(position = "right") +
  labs(y = NULL)
plt
# Add annotations
plt <- plt +
  labs(
    title = "Significant Variables",
    subtitle = "Variables that are significant predictors of heart disease"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 8
    )
  )
plt