library(caret)
source("R/read_mnist.R")
if (!exists("mnist")) mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p = 0.8, list = FALSE)
## get the quadrants
row_column <- expand.grid(row = 1:28, col = 1:28) 
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
## binarize the values. Above 200 is ink, below is no ink
x <- x > 200 
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), 
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 
##save data
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1], x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1], x_2 = x[-index_train,2])

mnist_127 <- list(
  train = data.frame(y = factor(y[index_train]),
                     x_1 = x[index_train,1],
                     x_2 = x[index_train,2]),
  test = data.frame(y = factor(y[-index_train]),
                    x_1 = x[-index_train,1],
                    x_2 = x[-index_train,2]))

save(mnist_127, file = "data/mnist_127.rda", compress = "xz", version = 2)
