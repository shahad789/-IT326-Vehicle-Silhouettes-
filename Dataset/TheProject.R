dataset<-read.csv('vehicle.csv')

##Encoding categorical data
dataset$class = factor(dataset$class,levels = c("bus","car", "van"), labels = c(1, 2, 3))

##Dealing with  missing value
dataset$compactness[is.na(dataset$compactness)]<- (median(dataset$compactness, na.rm =TRUE))
dataset$circularity[is.na(dataset$circularity)]<- (median(dataset$circularity, na.rm =TRUE))
dataset$distance_circularity[is.na(dataset$distance_circularity)]<- (median(dataset$distance_circularity, na.rm =TRUE))
dataset$radius_ratio[is.na(dataset$radius_ratio)]<- (median(dataset$radius_ratio, na.rm =TRUE))
dataset$pr.axis_aspect_ratio[is.na(dataset$pr.axis_aspect_ratio)]<- (median(dataset$pr.axis_aspect_ratio, na.rm =TRUE))
dataset$max.length_aspect_ratio[is.na(dataset$max.length_aspect_ratio)]<- (median(dataset$max.length_aspect_ratio, na.rm =TRUE))
dataset$scatter_ratio[is.na(dataset$scatter_ratio)]<- (median(dataset$scatter_ratio, na.rm =TRUE))
dataset$elongatedness[is.na(dataset$elongatedness)]<- (median(dataset$elongatedness, na.rm =TRUE))
dataset$pr.axis_rectangularity[is.na(dataset$pr.axis_rectangularity)]<- (median(dataset$pr.axis_rectangularity, na.rm =TRUE))
dataset$max.length_rectangularity[is.na(dataset$max.length_rectangularity)]<- (median(dataset$max.length_rectangularity, na.rm =TRUE))
dataset$scaled_variance[is.na(dataset$scaled_variance)]<- (median(dataset$scaled_variance, na.rm =TRUE))
dataset$scaled_variance.1[is.na(dataset$scaled_variance.1)]<- (median(dataset$scaled_variance.1, na.rm =TRUE))
dataset$scaled_radius_of_gyration[is.na(dataset$scaled_radius_of_gyration)]<- (median(dataset$scaled_radius_of_gyration, na.rm =TRUE))
dataset$scaled_radius_of_gyration.1[is.na(dataset$scaled_radius_of_gyration.1)]<- (median(dataset$scaled_radius_of_gyration.1, na.rm =TRUE))
dataset$skewness_about[is.na(dataset$skewness_about)]<- (median(dataset$skewness_about, na.rm =TRUE))
dataset$skewness_about.1[is.na(dataset$skewness_about.1)]<- (median(dataset$skewness_about.1, na.rm =TRUE))
dataset$skewness_about.2[is.na(dataset$skewness_about.2)]<- (median(dataset$skewness_about.2, na.rm =TRUE))
dataset$hollows_ratio[is.na(dataset$hollows_ratio)]<- (median(dataset$hollows_ratio,na.rm=TRUE))


## outliers analysis


##radius_ratio founded outliers 3

quartiles <- quantile(dataset$radius_ratio, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$radius_ratio)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$radius_ratio > Lower & dataset$radius_ratio < Upper)
dim(data_no_outlier)


##pr.axis_aspect_ratio outliers 8

quartiles <- quantile(dataset$pr.axis_aspect_ratio, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$pr.axis_aspect_ratio)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$pr.axis_aspect_ratio > Lower & dataset$pr.axis_aspect_ratio < Upper)
dim(data_no_outlier)


##max.length_aspect_ratio outliers is 5

quartiles <- quantile(dataset$max.length_aspect_ratio, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$max.length_aspect_ratio)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$max.length_aspect_ratio > Lower & dataset$max.length_aspect_ratio < Upper)
dim(data_no_outlier)



##scaled_variance outliers is 1

quartiles <- quantile(dataset$scaled_variance, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$scaled_variance)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$scaled_variance > Lower & dataset$scaled_variance < Upper)
dim(data_no_outlier)



##scaled_variance.1 outliers is 2

quartiles <- quantile(dataset$scaled_variance.1, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$scaled_variance.1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$scaled_variance.1 > Lower & dataset$scaled_variance.1 < Upper)
dim(data_no_outlier)



##scaled_radius_of_gyration.1 is outliers is 10

quartiles <- quantile(dataset$scaled_radius_of_gyration.1, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$scaled_variance.1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$scaled_radius_of_gyration.1 > Lower & dataset$scaled_radius_of_gyration.1 < Upper)
dim(data_no_outlier)


##skewness_about is outliers is 3

quartiles <- quantile(dataset$skewness_about, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$skewness_about)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$skewness_about > Lower & dataset$skewness_about < Upper)
dim(data_no_outlier)


##skewness_about.1 is outliers is 3

quartiles <- quantile(dataset$skewness_about.1, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(dataset$skewness_about.1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(dataset, dataset$skewness_about.1 > Lower & dataset$skewness_about.1 < Upper)
dim(data_no_outlier)

##feature selection
cor(dataset[, unlist(lapply(dataset, is.numeric))]) 
dataset<- subset(dataset, select = -pr.axis_rectangularity)

##normalization
normalize <- function(x) {return((x-min(x))/ (max(x)-min(x)))}
dataset$scaled_variance.1 <-normalize(dataset$scaled_variance.1)

#-------------balanced---------------------------------

install.packages("ggplot2")
library(ggplot2)


View(dataset)
View(iris)

install.packages("ROSE")
library(ROSE)
install.packages("caret")
library(caret)
dataset <- downSample(x = dataset[, -which(names(dataset) == "class")], y =dataset$class)

#ggplot(dataset, aes(x = class)) + geom_bar(fill = "skyblue", color = "black") + labs(title = "Class Distribution Plot", x = "class", y = "Count")
#"the data imbalanced"


#----------------------------End Balanced---------------------------------------

##------------------------------hard part------------------------------------

#------------------packages problem
remove.packages("caret")
remove.packages("infotheo")
remove.packages("MLmetrics")


packageVersion("caret") #false
packageVersion("infotheo") #true
installed.packages()
R.version$version.string
chooseCRANmirror()
install.packages("caret", repos = "https://cloud.r-project.org/")
remove.packages("caret")

#------------------packages problem




#--------------------the start of tree code-------------------------
# Install and load the required packages (if not already installed)
install.packages("caret")
library(caret)

install.packages("infotheo")
library(infotheo)

install.packages("MLmetrics")
library(MLmetrics)

install.packages("rpart")#additional
library(rpart)



# Install and load the required packages (if not already installed)
#install.packages("caret")
#install.packages("rpart")
#library(caret)
#library(rpart)



# Load your dataset into 'dataset' (replace 'your_dataset.csv' with the actual file path)
#dataset <- read.csv("your_dataset.csv")

# Define the target variable and predictor variables
target_variable <- "class"
predictor_variables <- setdiff(names(dataset), target_variable)

# Define the splits for cross-validation (3, 5, and 10)
splits <- c(3, 5, 10)

# Create a list to store the results for different splits and metrics
results <- list()

# Loop over different split values
for (split in splits) {
  # Create a list to store the results for this split
  split_results <- list()
  
  # Perform cross-validation and tree building for each metric (information gain, gain ratio, gini index)
  metrics <- c("informationGain", "gainRatio", "Gini")
  for (metric in metrics) {
    # Define the train control parameters for cross-validation
    ctrl <- trainControl(
      method = "cv",
      number = split,
      summaryFunction = multiClassSummary,
      classProbs = TRUE,
      verboseIter = TRUE
    )
    
    # Fit the decision tree using rpart
    model <- train(
      as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+"))),
      data = dataset,
      method = "rpart",
      trControl = ctrl,
      metric = metric
    )
    
    # Print the tree
    cat("Split:", split, "Metric:", metric, "\n")
    print(model$finalModel)
    
    split_results[[metric]] <- model
  }
  
  results[[paste("Split", split)]] <- split_results
}

#------------------------------------------------------------------------

#-----------------------------the lap1-------------------------------------------
# Load the 'party' package

install.packages('party')
library(party)

# Set a fixed random seed for reproducibility
set.seed(123)

# Define the number of splits
splits <- c(3, 5, 10)

# Define the target variable and predictor variables
target_variable <- "class"
predictor_variables <- setdiff(names(your_data_frame), target_variable)

# Create a list to store the results
results <- list()

# Loop through different splits
for (split in splits) {
  # Create a list to store the trees for each metric
  split_results <- list()
  
  # Loop through different metrics (information gain, gain ratio, Gini)
  metrics <- c("info", "gain", "gini")
  for (metric in metrics) {
    # Define the formula
    formula <- as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+")))
    
    # Define the control parameters
    ctrl <- ctree_control(minsplit = 2, minbucket = 1, mincriterion = 0, maxdepth = 30, teststat = metric)
    
    # Build the decision tree using ctree
    tree <- ctree(formula, data = your_data_frame, control = ctrl)
    
    # Store the tree in the results list
    split_results[[metric]] <- tree
    
    # Print the rules
    cat("Split:", split, "Metric:", metric, "\n")
    print(tree)
    
    # Plot the tree
    plot(tree)
  }
  
  # Store the results for this split
  results[[paste("Split", split)]] <- split_results
}

# Print the results
print(results)


#------------------------------------------------------------------------

#-----------------------------the lap2(the good one)-------------------------------------------
# Load the Vehicle Silhouettes dataset
# Replace 'vehicle_dataset.csv' with the actual file path or URL to your dataset
dataset <- read.csv('vehicle.csv', header = TRUE)
dataset <- read.csv('vehicle.csv')

# Check the structure of the dataset
str(dataset)

# Set a fixed random seed for reproducibility
set.seed(1234)

# Split the dataset into training and testing subsets
ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.7, 0.3))
trainData <- dataset[ind == 1, ]
testData <- dataset[ind == 2, ]

# Install and load the 'party' package
install.packages('party')
library(party)

# Define the formula for classification
myFormula <- class ~ compactness + circularity + distance_circularity + radius_ratio +
  pr.axis_aspect_ratio + max.length_aspect_ratio + scatter_ratio + elongatedness + max.length_rectangularity + scaled_variance + scaled_variance.1 +
  pr.axis_rectangularity + scaled_radius_of_gyration + scaled_radius_of_gyration.1 + skewness_about + skewness_about.1 +
  skewness_about.2 + hollows_ratio

# Build the decision tree using the 'ctree' function
dataset_ctree <- ctree(myFormula, data = trainData)

# Check the prediction on the training data
table(predict(dataset_ctree), trainData$class)

# Print the decision tree
print(dataset_ctree)

# Plot the decision tree
plot(dataset_ctree, type = "simple")

# Predict on the test data
testPred <- predict(dataset_ctree, newdata = testData)
table(testPred, testData$class)

# Calculate the accuracy of the model
accuracy <- sum(diag(table(testPred, testData$class))) / sum(table(testPred, testData$class)) * 100
accuracy

#-----------------------------the lap2(the good one)-------------------------------------------

#-----------------------------the lap(cart)-------------------------------------------
dataset <- read.csv('vehicle.csv')


# Set a fixed random seed for reproducibility
set.seed(234)

# Randomly sample row indices for the training set
train_indices <- sample(1:nrow(dataset), 200)

# Create the training set by selecting rows based on the sampled indices
Vehicle.train <- dataset[train_indices, ]

# Create the testing set by excluding rows from the training set
Vehicle.test <- dataset[-train_indices, ]

# Extract the target variable for the testing set
class.test <- dataset$class[-train_indices]



# Fit a classification tree to your Vehicle Silhouettes dataset
fit.tree <- rpart(class ~ ., data = Vehicle.train, method = "class", cp = 0.008)


# Print the fitted tree
fit.tree


#Visualize the Classification Tree
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(fit.tree)


# Checking the order of variable importance
fit.tree$variable.importance


#Predict Using the Classification Tree
pred.tree <- predict(fit.tree, Vehicle.test, type = "class")

# Evaluate the performance of the classification tree
confusion_matrix <- table(pred.tree, Vehicle.test$class)

# Print or inspect the confusion matrix
print(confusion_matrix)


##table(pred.tree, dataset$class) WRONG

#plotcp(fit.tree)
printcp(fit.tree)

# Explicitly request the lowest cp value
fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]


#Applying the pruned tree, the tree with the lowest cp value
bestcp <-fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(fit.tree, cp = bestcp)
rpart.plot(pruned.tree)


#Predict using the pruned tree on your test data
pred.prune <- predict(pruned.tree, Vehicle.test, type = "class")

# Evaluate the performance of the pruned tree
confusion_matrix <- table(pred.prune, Vehicle.test$class)

# Print or inspect the confusion matrix
print(confusion_matrix)


#-----------------------------the lap(cart)-------------------------------------------




#-----------------------------the lap-------------------------------------------
# Load the 'party' package
library(party)

# Set a fixed random seed for reproducibility
set.seed(123)

# Define the number of splits
splits <- c(3, 5, 10)

# Define the target variable and predictor variables
target_variable <- "class"
predictor_variables <- setdiff(names(your_data_frame), target_variable)

# Create a list to store the results
results <- list()

# Loop through different splits
for (split in splits) {
  # Create a list to store the trees for each metric
  split_results <- list()
  
  # Loop through different metrics (information gain, gain ratio, Gini)
  metrics <- c("info", "gain", "gini")
  for (metric in metrics) {
    # Define the formula
    formula <- as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+")))
    
    # Define the control parameters
    ctrl <- ctree_control(minsplit = 2, minbucket = 1, mincriterion = 0, maxdepth = 30, teststat = metric)
    
    # Build the decision tree using ctree
    tree <- ctree(formula, data = your_data_frame, control = ctrl)
    
    # Store the tree in the results list
    split_results[[metric]] <- tree
    
    # Print the rules
    cat("Split:", split, "Metric:", metric, "\n")
    print(tree)
    
    # Plot the tree
    plot(tree)
  }
  
  # Store the results for this split
  results[[paste("Split", split)]] <- split_results
}

# Print the results
print(results)


#------------------------------------------------------------------------


# Load your dataset into 'dataset' (replace 'your_dataset.csv' with the actual file path)
##dataset <- read.csv("your_dataset.csv")

# Define the target variable and predictor variables
target_variable <- "class"
predictor_variables <- setdiff(names(dataset), target_variable)

# Define the splits for cross-validation (3, 5, and 10)
splits <- c(3, 5, 10)

# Create a list to store the results for different splits
results <- list()

# Loop over different split values
for (split in splits) {
  # Define the train control parameters for cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = split,
    summaryFunction = multiClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE
  )
  
  # Create a list to store the results for this split
  split_results <- list()
  
  # Perform cross-validation and tree building for each metric (information gain, gain ratio, gini index)
  metrics <- c("informationGain", "gainRatio", "Gini")
  for (metric in metrics) {
    model <- train(
      as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+"))),
      data = dataset,
      method = "rpart",  # You can choose a different method if needed
      trControl = ctrl,
      metric = metric
    )
    
    split_results[[metric]] <- model$results$Accuracy
  }
  
  results[[paste("Split", split)]] <- split_results
}

# Print the results
for (split_result in results) {
  for (metric_result in split_result) {
    cat("Split:", names(split_result), "Metric:", metric_result, "\n")
  }
}

##-----------------------------------------------------------------------------



# Load your dataset into 'dataset' (replace 'your_dataset.csv' with the actual file path)
##dataset <- read.csv("your_dataset.csv")

# Define the target variable and predictor variables
target_variable <- "class"
predictor_variables <- setdiff(names(dataset), target_variable)

# Define the splits for cross-validation (3, 5, and 10)
splits <- c(3, 5, 10)

# Create a list to store the results for different splits
results <- list()

# Loop over different split values
for (split in splits) {
  # Define the train control parameters for cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = split,
    summaryFunction = multiClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE
  )
  
  # Create a list to store the results for this split
  split_results <- list()
  
  # Perform cross-validation and tree building for each metric (information gain, gain ratio, gini index)
  metrics <- c("informationGain", "gainRatio", "Gini")
  for (metric in metrics) {
    model <- train(
      as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+"))),
      data = dataset,
      method = "rpart",  # You can choose a different method if needed
      trControl = ctrl,
      metric = metric
    )
    
    split_results[[metric]] <- model$results$Accuracy
  }
  
  results[[paste("Split", split)]] <- split_results
}

# Print the results
for (split_result in results) {
  for (metric_result in split_result) {
    cat("Split:", names(split_result), "Metric:", metric_result, "\n")
  }
}

##-----------------------------------------------------------------------------






install.packages("DMwR2")
library(DMwR2)


# Replace "your_data" and "class" with the actual names in your dataset
dataset_balanced <- SMOTE(class ~ ., dataset, perc.over = 200, k = 5, perc.under = 200)

# Access the balanced data
balanced_data <- dataset_balanced$balanced

R.version.string

install.packages("ROSE")
library(ROSE)

# Replace "your_data" and "class" with the actual names in your dataset

# List of class labels
class_labels <- unique(dataset$class)

# Specify the number of samples to select for each class (adjust as needed)
num_samples_per_class <- 3

# Initialize an empty data frame for the balanced dataset
balanced_data <- data.frame()

# Loop through each class
for (label in class_labels) {
  # Subset the data for the current class
  class_data <- subset(dataset, class == label)
  
  # Randomly select a subset of instances
  sampled_instances <- class_data[sample(1:nrow(class_data), num_samples_per_class), ]
  
  # Append the selected instances to the balanced dataset
  balanced_data <- rbind(balanced_data, sampled_instances)
}

# Shuffle the order of rows in the balanced dataset
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

# Check the class distribution in the balanced dataset
table(balanced_data$class)
table(class)




with(class,{
  print(table(1))
  print(table(2))
  print(table(3))
}
)

class<- na.omit(class)
