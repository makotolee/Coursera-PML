# Clean up
rm(list = ls(all = TRUE))

library(caret)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

#Setting the working directory
#setwd('/media/nicolo/Data/Coursera/PracticalMachineLearning/Data/PracticalMachineLearning')
#setwd("C:\\Users\\leemc\\Desktop\\MyProCert - DataScience\\Module 8 - Practical Machine Learning\\Project")
setwd("C:\\Users\\Brandon\\Desktop\\mdec")

training_set <- read.csv(file="pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))
testing_set <- read.csv(file="pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))



within_train_set <- createDataPartition(training_set$classe, p=0.6, list=FALSE)
my_training_set <- training_set[within_train_set, ]
my_cross_validation_set <- training_set[-within_train_set, ]
dim(my_training_set); dim(my_cross_validation_set)


near_zero_Var_set <- nearZeroVar(my_training_set, saveMetrics=TRUE)
my_training_set <- my_training_set[,near_zero_Var_set$nzv==FALSE]

near_zero_Var_set <- nearZeroVar(my_cross_validation_set,saveMetrics=TRUE)
my_cross_validation_set <- my_cross_validation_set[,near_zero_Var_set$nzv==FALSE]

my_training_set <- my_training_set[c(-1)]

temp_my_training_set <- my_training_set 
for(i in 1:length(my_training_set )) {
    if( sum( is.na( my_training_set[, i] ) ) /nrow(my_training_set) >= .7) {
        for(j in 1:length(temp_my_training_set)) {
            if( length( grep(names(my_training_set[i]), names(temp_my_training_set)[j]) ) == 1)  {
                temp_my_training_set <- temp_my_training_set[ , -j]
            }
        }
    }
}

# Set back to the original variable name
my_training_set <- temp_my_training_set
rm(temp_my_training_set)


all_col_names <- colnames(my_training_set)
col_names_no_classe <- colnames(my_training_set[, -58])  # remove the classe column
my_cross_validation_set <- my_cross_validation_set[all_col_names]         # allow only variables in my_cross_validation_set that are also in my_training_set
testing_set <- testing_set[col_names_no_classe]             # allow only variables in testing that are also in my_training_set

#dim(my_cross_validation_set)
#dim(testing_set)


for (i in 1:length(testing_set) ) {
    for(j in 1:length(my_training_set)) {
        if( length( grep(names(my_training_set[i]), names(testing_set)[j]) ) == 1)  {
            class(testing_set[j]) <- class(my_training_set[i])
        }
    }
}

# To get the same class between testing_set and my_training_set
testing_set <- rbind(my_training_set[2, -58] , testing_set)
testing_set <- testing_set[-1,]


set.seed(7878)
model_fit <- rpart(classe ~ ., data=my_training_set, method="class")
fancyRpartPlot(model_fit)

pred_my_training_set <- predict(model_fit, my_training_set, type = "class")
conf_matrix_training <- confusionMatrix(pred_my_training_set, my_training_set$classe) 
conf_matrix_training
plot(conf_matrix$table, col = conf_matrix$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(conf_matrix$overall['Accuracy'], 4)))



pred_my_cross_validation_set <- predict(model_fit, my_cross_validation_set, type = "class")
confusionMatrix(pred_my_cross_validation_set, my_cross_validation_set$classe)

pred_testing_set <- predict(model_fit, testing_set, type = "class")
pred_testing_set


# Write the results to a text file for submission
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(pred_testing_set)