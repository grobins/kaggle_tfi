
require(neuralnet)




setwd('c:/gr/tfi')

import <- read.table('data//train.csv', header=T, sep=',')

import$Open.Date <- as.Date(import$Open.Date, '%m/%d/%Y')
import$age <- as.numeric(Sys.Date() - import$Open.Date)
import$City  <- as.numeric(import$City)
import$City.Group  <- as.numeric(import$City.Group)
import$Type  <- as.numeric(import$Type)

train <- import[, c(43, 3:42, 44)]
train$revenue <- train$revenue / train$age / 10000


fm <- as.formula(paste('revenue ~', 'City + City.Group + Type +', gsub(' ', '', paste('P', 1:37, collapse = '+')),'+age'))

norm.fun = function(x){ 
  (x - min(x))/(max(x) - min(x)) 
}



train.norm <- apply(train, 2, norm.fun)
train.norm <- as.data.frame(cbind(revenue = train[, 1], train.norm))

nn <- neuralnet(fm, 
                data = train, 
                hidden = 30, 
                linear.output = F, 
                threshold = 0.01,
                stepmax = 1e6,
                lifesign = "full",
                learningrate = .1,
                algorithm='backprop')



test <- read.table('data//test.csv', header=T, sep=',')
test$Open.Date <- as.Date(test$Open.Date, '%m/%d/%Y')
test$age <- as.numeric(Sys.Date() - test$Open.Date)
test$City  <- as.numeric(test$City)
test$City.Group  <- as.numeric(test$City.Group)
test$Type  <- as.numeric(test$Type)
test <- test[, -2]

test.norm <- apply(test, 2, norm.fun)

res <- compute(nn, test.norm[, -1])$net.result

submit <- res * 10000 * test[, 'age']


submit <- data.frame(id = test[, 'Id'], Prediction = submit)

write.table(submit, 'data/submit_201504081547.csv', row.names=F, sep=',')















