#intalltion
install.packages(keras)
library(keras)
install_keras()
#Get the Data from CSV file
data <- read.csv(file.choose(),header = T)
summary(data)
data <- as.matrix(data)
dimnames(data) <- NULL

#Normalize
data[,1:21] <-normalize(data[,1:21])
data[,22] <- as.numeric(data[,22]) - 1

#Data partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind == 1,1:21]
test <- data[ind == 2,1:21]
training_target <- data[ind==1,22]
test_target <-data[ind==2,22]

#One Hot Encoder
train_labels <- to_categorical(training_target)
test_labels <- to_categorical(test_target)

#Building the Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 50,activation = 'relu',input_shape = c(21)) %>%
  layer_dense(units=8,activation = 'relu') %>%
  layer_dense(units=3,activation = 'softmax')
summary(model)

#Compile
model %>% 
  compile(loss = 'categorical_crossentropy',
          optimizer='adam',
          metrics = 'accuracy')

#Fit the model
history <- model %>% fit(training,train_labels,
                         epoch=200,
                         batch_size=32,
                         validation_split = 0.2)
#Plotting
plot(history)

#Evaluation
model<-model %>% evaluate(test,test_labels)

#Prediction and confusion matrix using test data
prob<- model %>% 
  predict_proba(test)

pred <- model %>% 
  predict_classes(test)
table <- table(Predicted = pred,Actual = test_target)

#binding
cbind(prob,pred,test_target)
