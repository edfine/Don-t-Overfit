# My Attempt at Kaggle Don't Overfit Competition
# Produceded by Edward Fine of A Fine Point Consulting
# edward.fine@gmail.com
# 
# Thanks to the numerous helpful formun contributors.  
#

library(glmnet)
library(caTools) #requireed for AUC calc

#### In This section we prepare the raw data for manipulation ####
setwd("/Users/edwardfine/Documents/rcode/overfit")
# For larger data sets we would have to establish a DB
mydata <- read.csv("overfitting.csv", header=TRUE)

# This is a handy function to change the character string nems into integers.
colNames2varIndex <- function(strNames){
  as.integer(sub("var_", "", strNames))
}

#######################################
# Here we identify the data into labelled blocks so we do not 
#  need to use colons and indices when we train.  
trainset = mydata[mydata$train == 1,]
testset = mydata[mydata$train == 0,]

# Factors are our y values that we are trying to predict
# There are three sets of variables.
factor_train_practice = trainset[,3]
factor_train_leaderboard = trainset[,4]
factor_train_evaluate = trainset[,5]

factor_test_practice = testset[,3]
factor_test_leaderboard = testset[,4]
factor_test_evaluate = testset[,5]

# Features are the possibly predictive x values
feature_train = as.matrix(trainset[,6:205])
feature_test = as.matrix(testset[,6:205])


# Next we will attempt to identify relevant varriables
# Ockam provided variables on the leaderboard set with inside information
# Thus our variable fit problem should reproduce ockams variables.  
                                
# Setting lambda near zero but just off the lower bound allows us 
# to set most variables to zero as in Lasso, but with better stability
# for eaually important variables.
glmFitForVarSelectionCV <- cv.glmnet(x = feature_train,
                                y = factor_train_evaluate,
                                family = "binomial",
                                alpha = 0.8,
                                dfmax = 110, 
                                nfolds = 5,
                                type.measure="auc")

# Now pull out the coefficients
glwCV <- coef(glmFitForVarSelectionCV)
glwCV.sorted <- sort(glwCV, index.return = TRUE)
# Here I brazenly assume that there are 110 useful features.  
# This assumption proved most challenging in the test phase.


varI <- colNames2varIndex(names(glwCV.sorted)) + 5
varI <- varI[1:110]
Final_var  <- mat.or.vec(1,200)
Final_var[varI] <- 1
#write.csv(Final_var, file="var_Fine.txt", row.names = FALSE)

vars <- varI[1:110]
Final_var  <- mat.or.vec(1,200)
Final_var[varI] <- 1


glmFitSol <- glmnet(as.matrix(mydata[1:250, varI]),
                    mydata[1:250, 5],
                    family = "multinomial",
                    alpha = 0.5,
                    lambda = 0.02)
Final_predict <- predict(glmFitSol, as.matrix(mydata[251:20000, varI]))[, 2, ]

#submit_file = cbind(testID,Final_predict)
write.csv(Final_predict, file="AUC_Fine.txt", row.names = FALSE)
