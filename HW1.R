#HW1
rm(list=ls())
library(abind)
library(readr)
library(glmnet)
# import the 1st cleaned CSV data file into R
# returns a data frame with a list of variables 
setwd("~/Desktop")

## you need to change the directory below
df.y <- read.csv("100_Portfolios_10x10_Daily.CSV")
# number of rows
nr.y = nrow(df.y)
# number of columns
nc.y = ncol(df.y)

# now define data matrix Y (1259 by 100) for prediction
# need to convert a data frame to a numeric matrix
Y = apply(as.matrix.noquote(df.y[2:nr.y, 2:nc.y]), 2, as.numeric)
# the sample size (number of observations or data points)
n = nrow(Y)

# import the 2nd cleaned CSV data file into R
# returns a data frame with a list of variables 

## you need to change the directory below
df.x = read.csv("F-F_Research_Data_5_Factors_2x3_daily.csv")
# number of rows
nr.x = nrow(df.x)
# number of columns
nc.x = ncol(df.x)
# now define data matrix X (1259 by 6) for prediction
# need to convert a data frame to a numeric matrix
X.all = apply(as.matrix.noquote(df.x[2:nr.x, 2:nc.x]), 2, as.numeric)

# preprocess the data
# consider the 10th portfolio return as the Y variable
y.id = 10
y = Y[,y.id]
# consider the excess return of portfolio by subtracting risk-free 
# interest rate (the RF column of X data matrix)
y = as.matrix(y - X.all[,6], n,1)
# convert y into categorical data
y = 1+(y>0)+(y>0.6)

# consider only the 5 factors as the predictors (X variables)
X = X.all[,1:5]
# the dimensionality (number of predictor or X variables)
p = ncol(X)

################################################################################
# divide the data into training and testing
n.train=1008
n.test=n-n.train

X.train=X[1:n.train,]
y.train=y[1:n.train]

X.test = X[(n.train+1):n,]
y.test=y[(n.train+1):n]
################################################################################

multi.mod.reg<-cv.glmnet(X.train, y.train,family="multinomial")
plot(multi.mod.reg)
coef(multi.mod.reg)

yhat.train.reg<-predict(multi.mod.reg,newx=X.train, s="lambda.min",type = "class")
1-mean(y.train==yhat.train.reg)
#Prediction error on training: 0.2281746

yhat.test.reg<-predict(multi.mod.reg,newx=X.test, s="lambda.min",type = "class")
1-mean(y.test==yhat.test.reg)
#Prediction error on test: 0.250996
