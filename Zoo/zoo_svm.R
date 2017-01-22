

library(ggplot2)
library(dplyr)
install.packages('caret')
library(caret)
library(gridExtra)
install.packages('seriation')
library(seriation)



class.data = read.csv('class.csv' , header = T , sep = ',')
raw.data = read.csv('zoo.csv' , header = T , sep = ',')

head(class.data)

colnames(class.data) <-  tolower(colnames(class.data))


zoo.data <- merge(x = raw.data, y = class.data, by = "class_type", all.x = TRUE)

fix(zoo.data)
fix(class.data)

colnames(class.data)


colnames(class.data) <- c("class_type", 
                          "no_of_animal_species_in_class", 
                          "class_name", 
                          "animal_name"
)
colnames(class.data)
class.data





class.data <-  select(class.data , class_type, class_name)


class.data






raw.data$class_type <- as.factor(raw.data$class_type)


zoo.data <- merge(x = raw.data, y = class.data, by = "class_type", all.x = TRUE)


head(zoo.data, 3)

rm(raw.data, class.data)


# starting plotting here ... above was to merge two datasets using class_type to see to classify animals and their features


# plotting animals and features



plotHist <- function(df, i) {
  data <- data.frame(x=df[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + 
    stat_count() + 
    xlab(colnames(df)[i]) + 
    ggtitle(colnames(df)[i]) +
    theme_bw() + 
    facet_wrap(~df[[19]]) +
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}




lets.plot <- function(df, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(df=df, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

convert.to.numeric <- function(df, lst){
  for (i in lst){
    df[[i]] <- as.numeric(df[[i]])
  }
  return(df)
}


int.list <- names(zoo.data)[which(sapply(zoo.data, is.integer))]
zoo.data <- convert.to.numeric(zoo.data, int.list)


rm(int.list)

fix(zoo.data)

for(i in seq(3,18,1)) {
  lets.plot(zoo.data, fun=plotHist, ii=i, ncol=1)
}


# running svm


# creating training datasets 70% and 30% testing datasets

intraining <- createDataPartition(y=zoo.data$class_type, p=0.7, list=F)

dim(intraining)

train.batch <- zoo.data[intraining,]
test.batch <- zoo.data[-intraining,]


cat("----- Training batch ------")
table(train.batch$class_type)



cat("----- Testing batch -----")
table(test.batch$class_type)



library(e1071)

train.batch
train.batch <-  select(train.batch , -animal_name , -class_name)


train.x <- select(train.batch , -class_type)


train.x
train.y <- train.batch$class_type

train.y

set.seed(501)



svm.model <-  svm(class_type~., data=train.batch , kernel = 'polynomial')


summary(svm.model)




test.x <- select(test.batch,
                 -class_type,
                 -animal_name,
                 -class_name)

test.y <- test.batch$class_type


test.pred <- predict(svm.model, test.x)

confusionMatrix(test.pred, test.y)
