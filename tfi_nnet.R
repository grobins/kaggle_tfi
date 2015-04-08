
require(nnet)

#import function from Github
require(RCurl)



setwd('c:/gr/tfi')

import <- read.table('data//train.csv', header=T, sep=',')

import$Open.Date <- as.Date(import$Open.Date, '%m/%d/%Y')

import$age <- as.numeric(Sys.Date() - import$Open.Date)

import$City  <- as.numeric(import$City)
import$City.Group  <- as.numeric(import$City.Group)
import$Type  <- as.numeric(import$Type)

train <- import[, c(43, 3:42, 44)]

#fm <- as.formula(paste('revenue ~', gsub(' ', '', paste('P', 1:37, collapse = '+')),'+age'))

#nn <- nnet(fm, data = train, size = 5)

nn <- nnet(revenue ~ ., train, size=10)

test <- read.table('data//test.csv', header=T, sep=',')
test$Open.Date <- as.Date(test$Open.Date, '%m/%d/%Y')
test$age <- as.numeric(Sys.Date() - test$Open.Date)
test$City  <- as.numeric(test$City)
test$City.Group  <- as.numeric(test$City.Group)
test$Type  <- as.numeric(test$Type)
test <- test[, -2]
predict(nn, test[1:3,])
