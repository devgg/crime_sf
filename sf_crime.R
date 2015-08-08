library("ggplot2")

setwd("C:/Users/Florian/projects/machine_learning/kaggle/sf_crime")
source("format_records.R")
source("neural_network.R")
# 
# 
# # train <- read.csv(file = "data/train.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE,
# #                   colClasses=c('time', 'factor', 'factor', 'factor', 'factor', 'factor', 'character', 'numeric', 'numeric'))
# # write.csv(train[sample(nrow(train)),], file = "data/train_shuffle.csv", row.names=FALSE)
# 
# categories <- as.factor(read.csv(file = "data/categories.csv", header = F, sep = ",", quote = "\"", dec = ".", fill = FALSE,
#                   colClasses=c('factor'))[, 1])
# 
# 
# 
# setClass('time')
# setAs("character", "time", function(from) as.POSIXlt(from))
# 
# train <- read.csv(file = "data/train_shuffle.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE, #nrows = 500, 
#                   colClasses=c('time', 'factor', 'factor', 'factor', 'factor', 'factor', 'character', 'numeric', 'numeric'))
# test <- read.csv(file = "data/test.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE, #nrows = 1000, 
#                   colClasses=c('numeric', 'time', 'factor', 'factor', 'character', 'numeric', 'numeric'))
# 
# train_formated <- format_records(train, categories)
# test_formated <- format_records(test, categories)
# 
# 
# #train_formated <- format_records(train[1:(nrow(train) / 2),], categories)
# #test_formated <- format_records(train[((nrow(train) / 2) + 1):nrow(train),], categories)
# 
# neural_network <- create_neural_network(c(ncol(train_formated$data), 20, length(levels(train$Category))), colnames(train_formated$data), levels(train$Category))

#forward_data <- forward_propagation(neural_network, train_formated$data)
#cost_init <- calculate_cost(test_formated$y, forward_data$h)

#neural_network = gradient_descent(neural_network, train_formated, 100, 0.1, 0.5)

#neural_network2 <- create_neural_network(c(ncol(train_formated$data), length(levels(train$Category))), colnames(train_formated$data), levels(train$Category))

alpha = 0.15
momentum = 0.6

v_old <- list();
v_new <- list();
for (j in 1:10) {
  forward_data <- forward_propagation(neural_network2, train_formated$data)
  print(sprintf("%d: %f", j, calculate_cost(train_formated$y, forward_data$h)))
  gradients <- back_propagation(neural_network2, forward_data, train_formated$y)
  
  for (i in 1: length(neural_network2)) {
    v_new[[i]] = alpha * gradients[[i]]
    neural_network2[[i]] = neural_network2[[i]] - (v_new[[i]] + (if(j == 1) 0 else momentum * v_old[[i]]))
    v_old[[i]] = v_new[[i]]
  }
}



#forward_data <- forward_propagation(neural_network, train_formated$data)
#cost <- calculate_cost(test_formated$y, forward_data$h)


#prediction <- forward_data$h

test_prediction <- forward_propagation(neural_network2, test_formated$data)$h

options(scipen=100)
write.table(cbind(test$Id, map_categories(categories, test_prediction)), file = "data/submission.csv", quote=F, row.names=F, col.names = c("Id", levels(categories)), sep=",")
options(scipen=0)

