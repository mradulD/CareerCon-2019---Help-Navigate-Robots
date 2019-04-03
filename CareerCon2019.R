require("readxl")
library(readxl)

x_test_final <- read.csv("X_test.csv")
xsample <- read.csv("X_train.csv")
y_train <- read.csv("y_train.csv")

library(dplyr)

xsample = xsample %>% left_join(y_train,by = "series_id")

#library(caTools)

# set.seed(420)
# split <- sample.split(xsample, SplitRatio = 0.7)
# 
# trainset <- subset(xsample, split == "TRUE")
# trainset <- trainset[, -c(1,2,3,14)]
# trainset_scale <- scale(trainset[-11])
# 
# testset <- subset(xsample, split == "FALSE")
# testset <- testset[, -c(1,2,3,14)]
# testset_scale <- scale(testset[-11])
# 
# install.packages("caret")
# library(caret)
# confusionMatrix(table(testset$surface, testset$PredSurface))

#K-NN
library(class)

#training data scaling
xsample <- xsample[,-c(1,2,3,14)]
xsample_scale <- scale(xsample[-11])

#test data scaling
x_test_final_scale <- x_test_final[,-c(1,2,3)]
x_test_final_scale <- scale(x_test_final_scale)

#Predict
x_test_final$surface <- knn(train = xsample_scale,
                           test = x_test_final_scale,
                           cl = xsample$surface,
                           k=5
                           )

result <- x_test_final %>% select(series_id, surface)

##############################################################################################################
#MOST IMPORTANT -  took me an hourto figure out how to do this
#To take mode of the result for each series id and replace 128 entries of same seriesid with one.
i = 0  
j = 1

list2<-lapply(1:3816, function(x) matrix(NA, nrow=138, ncol=2))

for(var in list2){
list2[[j]] =   result %>%
  filter(series_id == i)
  i=i+1
  j= j+1
}

head(list2)



i=0
series_id = c(0:3815)

df <- data.frame(series_id)
df$surface <- NA

for (k in 1:3816) {
  ta <- list2[[k]]
  surface <- table(ta$surface)
  df$surface[df$series_id == i]<- names(surface)[surface == max(surface)]
  i= i+1
}

summary(df)

write.csv(df, file = "K_NN_Nofeature.csv")








#SVM
install.packages("e1071")
library(e1071)

#Fit a model. The function syntax is very similar to lm function

model_svm <- svm( x_train_original$surface ~ x_train_original$orientation_X , x_train_original)

#Use the predictions on the data

pred <- predict(model_svm, train)


#Plot the predictions and the plot to see our model fit

points(train$x, pred, col = "blue", pch=4)