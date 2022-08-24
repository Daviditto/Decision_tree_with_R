library(ISLR)
library(ggplot2)
head(College)

# data EDA
ggplot(College, aes(Grad.Rate, Room.Board)) +geom_point(aes(color=Private))
ggplot(College, aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color='black', bins=50)
ggplot(College, aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color='black', bins=50)


class(College$Grad.Rate)
College[College$Grad.Rate>100,]$Grad.Rate <- 100
any(College[College$Grad.Rate>100,])

#split the dataset
library(caTools)
split <- sample.split(College, SplitRatio = 0.7)
train <- subset(College, split=T)
test <- subset(College, split=F)

# build the decision tree model 
library(rpart)

# plot the tree
tree <-rpart(Private~., method='class', data=College)
printcp(tree)
plot(tree, uniform = T)
text(tree, use.n=T, all=T)

library(rpart.plot)
rpart.plot(tree) 
prp(tree)

pred <- predict(tree, test)
head(pred)
pred <- data.frame(pred) 
head(pred)

pred.final <- predict(tree, test, type='class')
pred.final2 <-ifelse(pred$Yes>0.5, 'Yes', 'No')
head(pred.final2)
head(pred.final)

table(test$Private, pred.final2)
table(test$Private, pred.final)

# build the random forest model 
library(randomForest)

forest <- randomForest(Private ~., data=College, importance=TRUE)
forest$confusion
forest$importance

pred.forest <- predict(forest, test)
table(test$Private, pred.forest) # perfect!!!!




