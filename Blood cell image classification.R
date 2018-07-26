install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
#install.packages("mxnet")
#image_dir <- "C:/Nithi/Github/MyProgramming/images"
library(EBImage)
library(keras)
install.packages("keras")
#Read images
image_dir <- "C:/Nithi/Github/MyProgramming/TEST"
bloodcell <- c('E1.jpeg','E2.jpeg','E3.jpeg','E4.jpeg','E5.jpeg',
               'L1.jpeg','L2.jpeg','L3.jpeg','L4.jpeg','L5.jpeg',
               'M1.jpeg','M2.jpeg','M3.jpeg','M4.jpeg','M5.jpeg',
               'N1.jpeg','N2.jpeg','N3.jpeg','N4.jpeg','N5.jpeg')
train <- list()
for (i in 1:20) {train[[i]] <- readImage(bloodcell[i])}
bloodcell2 <- c('E6.jpeg','L6.jpeg','M6.jpeg','N6.jpeg')
test <- list()
for (j in 1:4) {test[[j]] <- readImage(bloodcell2[j])} 
print(train[[3]])
display(train[[3]])

#Resize
str(train)
for (i in 1:20) {train[[i]] <- resize(train[[i]],100,100)}
for (j in 1:4) {test[[j]] <- resize(test[[j]],100,100)}

#combain everthing 
train <-combine(train)
x <-tile(train,5)
display(x,title=bc)

test <-combine(test)
y <-tile(test,4)
display(y,title=bc1)

str(train)
  
#Reorder dimension
train <- aperm(train,c(4,1,2,3))
test <- aperm(test,c(4,1,2,3))

#Respond
trainy <- c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
testy <- c(0,1,2,3)

#Encodeing 
library(keras)
library(tensorflow)
trainlabels <- to_categorical(trainy)
testlabels <-to_categorical(testy)
trainlabels

#model
model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(100, 100, 3)) %>%
  layer_conv_2d(filters = 32,
      kernel_size = c(3,3),
      activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate=0.25) %>%
  layer_dense(units = 4, activation = 'softmax') %>%
  
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_sgd(lr = 0.01,
                                    decay = 1e-6,
                                    momentum = 0.9,
                                    nesterov = T),
          metrics = c('accuracy'))
summary(model)  

#fit model 
history <- model %>%
  fit(train,
      trainlabels,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.4,
      validation_data = list(test, testlabels))
plot(history)  

#Evaluation & Prediction of the training data 
model %>% evaluate(train, trainlabels)
pred <- model %>% predict_classes(train)
table(Predicted = pred, Actual = trainy)

prob <- model %>% predict_proba(train)
cbind(prob, Predicted_class = pred, Actual = trainy)
  
  
  
  
  
  