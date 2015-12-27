---
title: "Practical Machine Learning - Course Project"
author: "KSSam"
date: "December 25, 2015"
output: html_document
---
### Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset). 

### Data Processing
### Loading data
```{r}
# Check the current working directory.
getwd()

# Assign the chosen working directory.
setwd("C:\\Users\\kssam\\Desktop\\KDC\\M8_Practical_Machine_Learning\\Course Project")
getwd() # cross check new assign working directory.

# loaded package needed in this project.
library(caret)
library(randomForest)

# Loading a dataset
training_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("#DIV/0!"," ", "", "NA", "NAs", "NULL"))

testing_data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("#DIV/0!"," ", "", "NA", "NAs", "NULL"))

# Retrieve or set the dimension of an object
dim(training_data) # [1] 19622   160
dim(testing_data)  # [1]    20   160

# Look at the structure. 
#str(training_data) # For details, kindly refer to appendix 1.
#str(testing_data)  # For details, kindly refer to appendix 1.

# Return the columns Names for Information
# names(training_data) 

# Let's look at the number of records in "classe" variable in the training set.
table(training_data$classe)
```

Initial dataset:
The training data set contains **`r ncol(training_data)`** variables and **`r nrow(training_data)`** obs.
The testing data set contains **`r ncol(testing_data)`** variables and  **`r nrow(testing_data)`** obs.

### Perform Preprocessing.
```{r}
# Remove columns with all missing values.
training_data <- training_data[,colSums(is.na(training_data)) == 0]
testing_data  <- testing_data[,colSums(is.na(testing_data)) == 0]

# Remove irrelevant variables from dataset: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). We can delete these variables.
training_data1 <- training_data[,-c(1:7)]
testing_data1  <- testing_data[,-c(1:7)]

# and have a look at our new datasets:
dim(training_data1) # [1] 19622    53
dim(testing_data1)  # [1] 20 53

# Return the columns Names for Information
names(training_data1) 

# look at the sample data.
# head(training_data1,5) # remove the '#' to run the head function.
# head(testing_data1,5)  # remove the '#' to run the head function.
```
After cleaning: 
The training data set contains **`r ncol(training_data1)`** variables and **`r nrow(training_data1)`** obs.
The testing data set contains **`r ncol(testing_data1)`** variables and  **`r nrow(testing_data1)`** obs.

### Cross-validation:
***Approach:***
1. Use the training set.

2. Split it into training/test sets.

3. Build a model on training set.

4. Evaluate on the test set.

5. Repeat and average the estimated errors.

Now let's partitioning the dataset into a 60% training and 40% probing dataset.
```{r}
#set seeding.
set.seed(112233)
# get training and testing sets
inTrain <- createDataPartition(y=training_data1$classe, p=0.6, list=FALSE)
subTraining <- training_data1[inTrain, ] 
subTesting  <- training_data1[-inTrain, ]
```

### Model Fitting : Random Forest.
Now we will build a model on training set.
To estimate the generalization error, we apply out-of-bag (OOB) into trainControl.
```{r}
trCtrl <- trainControl(method = "oob")
Model_rf <- train(classe ~ ., data=subTraining, method="rf", 
                  trControl = trCtrl, tuneGrid = data.frame(.mtry = 2))
Model_rf$finalModel
```
The compute estimate of error rate of OBB is ***0.76%***.

### Prediction and Confusion Matrix.
```{r}
predictions <- predict(Model_rf, subTraining)
print(confusionMatrix(predictions, subTraining$classe))
```


### Model Validation.
Now, let's evaluate the model on the test set.
```{r}
Validation <- predict(Model_rf, subTesting)
print(confusionMatrix(Validation, subTesting$classe))
Validation_cmat <- (confusionMatrix(Validation, subTesting$classe))
```
The The model accuracy are at **`r Validation_cmat$overall[1]`** and estimate the error from the validation is ***0.84%*** (1 - out of sample from accuracy). The error rate from the validation model is higher than training model and this is expected. Nonetheless, the different is very small and this show the accuracy in prediction.


### Final Step: Applying the model to predict the output from the test data.
```{r}
# Predicting:
final_prediction <- predict(Model_rf, testing_data1)
final_prediction
```


### Prediction Submission.
Below are the output files generate according to instructions for submission.
```{r}
answers <- as.vector(final_prediction)

pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}

pml_write_files(answers)
```

### Appendix 1: Look at the structure. 
```{r, echo=FALSE}
str(training_data)
str(testing_data)
```

