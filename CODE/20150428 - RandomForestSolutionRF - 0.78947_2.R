rm(list = ls())
load(file = "CombinedAgesRestored.dat", verbose = T)

set.seed(15)
train <- sample(x = train.id, 
                size = length(train.id)*0.65,
                replace = F)
test <- train.id[-train]

y <- as.factor(Combined$Survived)
# names(Combined)
x <- Combined[,c(2:24)]
x <- model.matrix(Survived~.-MSurnameLW-GivenNames,x)[,-1] #colnames(x) 

# Really Random Forest: m<p

#Initial forest of 500 trees
Nt <- 500

library(doParallel)
library(randomForest)
registerDoParallel(5,cores = 5)
err <- unlist(foreach (i = 1:200) %dopar% {
  set.seed(513)
  library(randomForest)
  rf.Surv <- randomForest(x[train,], y[train],
                          mtry = i, importance = T, ntree = Nt)
  rf.Surv$err.rate[Nt,1]
})
m = which.min(err)
set.seed(513)
rf.Surv <- randomForest(x[train,], y[train],
           mtry = m, importance = T, ntree = Nt)
rf.Surv
varImpPlot(rf.Surv)
# According to error rate plot 
matplot(rf.Surv$err.rate, type = "l", lty = 1)
# growing more trees is useless - training (OOB) error rate
# dropped to its minimum at the 1st 100 trees:
# > which.min(rf.Surv$err.rate[,1])
# [1] 408
# Minimum error rate of victims:
# > which.min(rf.Surv$err.rate[,2])
# [1] 95
# Minimum error rate of survivors:
# > which.min(rf.Surv$err.rate[,3])
# [1] 49
# Minimum error rate of survivors and victims combined:
# > which.min(rf.Surv$err.rate[,2]+rf.Surv$err.rate[,3])
# [1] 408
#What about test errors?
pred.mat <- predict(rf.Surv, newdata = x[test,],
                    predict.all = T)$individual
true.test <- as.character(Combined$Survived[test])
test.err.rate <- matrix(0, nrow = Nt, ncol = 3)
colnames(test.err.rate) <- colnames(rf.Surv$err.rate)
No <- length(test)
for (i in 1:Nt){
  # Making predictions from i first trees
  pred.test <- sapply(1:No, function(j){
    ifelse(((sum(pred.mat[j,1:i]=="1")/i)>0.65),
           yes = "1",no = "0")
  })
  CM <- table(pred.test, true.test)
  test.err.rate[i,]<-c(
    (CM[1,2]+CM[2,1])/No, # Overall error rate
    (CM[1,2]/(CM[1,2]+CM[1,1])), # Wrong deaths 
    (CM[2,1]/(CM[2,2]+CM[2,1]))) # Wrong survivals
}
matplot(test.err.rate[1:Nt,], type = "l", lty = 1)
which.min(test.err.rate[,1])
# > which.min(test.err.rate[,1])
# [1] 154
# > which.min(test.err.rate[,2])
# [1] 35
# > which.min(test.err.rate[,3])
# [1] 236
# > which.min(test.err.rate[,3]+test.err.rate[,2])
# [1] 236
# Thus, sort of conclusions are:
# 1. Use "rule of elbow" to choose number of trees
# 2. Bagged forest here learns too quickly to succeed

#Refitting on the complete training data
# to choose Nt
Nt <- 1000
set.seed(513)
rf.Surv <- randomForest(x[train.id,], y[train.id],
                        mtry = m, importance = T, ntree = Nt)

matplot(rf.Surv$err.rate, type = "l", lty = 1)



submit <- Combined[-train.id, 1:2]

#rf.pred=predict(rf.Surv, x[-train.id,],type ="response")
pred.mat <- predict(rf.Surv, newdata = x[-train.id,],
                    predict.all = T)$individual
No <- N - length(train.id)
rf.pred <- sapply(1:No, function(j){
    ifelse(((sum(pred.mat[j,1:Nt]=="1")/Nt)>0.65),
           yes = "1",no = "0")
  })


submit[,2] <- rf.pred
row.names(submit) <- NULL
View(submit)
write.csv(submit, "RFsubRF0.65.csv", row.names = F, quote = F)
