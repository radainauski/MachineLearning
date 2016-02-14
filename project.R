setwd("F:/MachineLearning"); library(caret); library(kernlab); 
library(randomForest); set.seed(123)

#acquire data

workingAddr <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
quizAddr <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

working <- read.csv(url(workingAddr), na.strings=c("NA","#DIV/0!",""), stringsAsFactors=FALSE)
quiz <- read.csv(url(quizAddr), na.strings=c("NA","#DIV/0!",""), stringsAsFactors=FALSE)

working$classe <- as.factor(working$classe)

inTrain <- createDataPartition(y=working$classe, p=0.6, list=FALSE)

training <- working[inTrain, ]; testing <- working[-inTrain, ]

dim(training); dim(testing)

# Remove NZVs

nzvcols <- nearZeroVar(training)
training <- training[,-nzvcols]

# remove any column with over 500 NA values

training <- training[!colSums(is.na(training)) > 500]

# remove row.nums and other irrelevant data

training <- training[,-c(1:6)]


# try random forest without preprocessing

modFit <- train(classe ~ ., method="rf", data=training, ntree=100,do.trace=T)


modFit

predTesting <- predict(modFit, testing); testing$predRight <- predTesting==testing$classe

confusionMatrix(predTesting, testing$classe)

predQuiz <- predict(modFit, quiz)

predQuiz
