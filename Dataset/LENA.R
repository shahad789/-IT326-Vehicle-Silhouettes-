
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

















############################################################################



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
fit.tree <- rpart(Class ~ ., data = Vehicle.train, method = "class", cp = 0.008)


# Print the fitted tree(flag)
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
confusion_matrix <- table(pred.tree, Vehicle.test$Class)

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
confusion_matrix <- table(pred.prune, Vehicle.test$Class)

# Print or inspect the confusion matrix
print(confusion_matrix)

#performance metrics
library(caret)
confusion_matrix_result <- confusionMatrix(pred.prune, Vehicle.test$Class)
confusion_matrix_result