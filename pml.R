setwd("/home/kailex/Doc/Coursera/PractML/proj")

# Get data
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="train.csv", method="wget")
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="test.csv", method="wget")

# Load data
data <- read.csv("train.csv", na.strings="NA", header=T)
set.seed(1111)
library(caret)
data <- data[sample(nrow(data)),]
inTrain <- createDataPartition(data$X, p = 0.7)[[1]]
train <- data[inTrain,]
valid <- data[-inTrain,]
test <- read.csv("test.csv", na.strings="NA", header=T)

# Detect columns with more then 10% of NA's
idx2remove <- function(df){
  idx <- c()
  for (i in 1:dim(df)[2]){
    if (sum(is.na(df[,i]))/dim(df)[1] > 0.1) idx <- c(idx, i)
  }
  return(idx)
}
# Useless columns
col.to.remove <- unique(c(idx2remove(train), idx2remove(valid), idx2remove(test)))

trainC <- train$classe
train <- train[,-c(col.to.remove)]

validC <- valid$classe
valid <- valid[,-c(col.to.remove)]

testC <- test$problem_id
test <- test[,-c(col.to.remove)]

# Remove non-numeric variables
train <- train[,sapply(train, is.numeric)]
valid <- valid[,sapply(valid, is.numeric)]
test <- test[,sapply(test, is.numeric)]

# Detect highly correlated columns
num.idx <- which(sapply(train, class) == "numeric")
descrCorr <- abs(cor(train[,num.idx]))
highCorr <- findCorrelation(descrCorr, 0.9)
highCorrCol <- dimnames(descrCorr[, highCorr])[[2]]
# Remove highly correlated columns
train <- train[, !names(train) %in% c(highCorrCol, "X", "cvtd_timestamp", "raw_timestamp_part_1",	"raw_timestamp_part_2",	"num_window", "classe")]
valid <-valid[, !names(valid) %in% c(highCorrCol, "X", "cvtd_timestamp", "raw_timestamp_part_1",  "raw_timestamp_part_2",	"num_window", "classe")]
test <- test[, !names(test) %in% c(highCorrCol, "X", "cvtd_timestamp", "raw_timestamp_part_1",  "raw_timestamp_part_2",	"num_window", "problem_id")]
# Correct types
train <- sapply(train, as.numeric)
valid <- sapply(valid, as.numeric)
test <- as.matrix(sapply(test, as.numeric))

# train <- scale(train)
# valid <- scale(valid)
# test <- scale(test)

# Ensure that there are no NA's
sum(is.na(train))
sum(is.na(valid))
sum(is.na(test))

# Get common properties
sapply(train, class)
str(train)
#sum(sapply(test,class) != sapply(train,class))

# Create RF model
library(randomForest)
fitRF <- randomForest(x=train, y=trainC, importance=T, ntree=317)
predRF <- predict(fitRF, valid)
confusionMatrix(predRF, validC)
par(mfrow=c(1,1))
varImpPlot(fitRF, n.var=5, type=1, main="Fig 1. Variable Importance")

par(mfrow=c(2,2))
valid <- data.frame(valid)
plot(valid$yaw_belt, valid$roll_belt,
     xlab="Yaw belt", 
     ylab="Roll belt", 
     main="Fig. 2",col=as.numeric(validC), pch=19, cex=.5)

plot(valid$yaw_belt,valid$pitch_belt,
     xlab="Yaw belt", 
     ylab="Pitch belt", 
     main="Fig. 3",col=as.numeric(validC), pch=19, cex=.5)

plot(valid$roll_belt,valid$pitch_belt,
     xlab="Roll belt", 
     ylab="Pitch belt", 
     main="Fig. 4",col=as.numeric(validC), pch=19, cex=.5)
plot.new()
legend("center", legend=levels(validC), cex=2, text.col=seq_along(levels(validC)))


# 20 test cases
predRFT <- predict(fitRF, test)
answer <- as.character(predRF)
pml_write_files <- function(x){
  n <- length(x)
  for(i in 1:n){
    filename <- paste0("problem_id_",i,".txt")
    write.table(x[i],file <- filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answer)




