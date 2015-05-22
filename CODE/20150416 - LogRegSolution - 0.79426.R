load(file = "CombinedAgesRestored.dat", verbose = T)

# Some exploration of the decision boundary shape
x <- t(sapply(1:length(Combined), function(i){
  c(names(Combined)[i],
    class(Combined[,i]))}))

Combined$Survived <- as.factor(Combined$Survived)
attach(Combined)
table(Survived[train.id], Pclass[train.id])
table(Survived[train.id], Title[train.id])
table(Survived[train.id], Sex[train.id])
table(Survived[train.id], hasAnyNick[train.id])
table(Survived[train.id], hasFamily[train.id])

detach(Combined)


#save(Combined, train.id, N, InsertAfter, 
#     InsertAfterName, file = "CombinedAfterLR.dat")

set.seed(15)
train <- sample(x = train.id, 
                size = length(train.id)*0.8,
                replace = F)
test <- train.id[-train] # bad style, 
# works only for 
# consequtive numbers
# in train.id!!!


# Preparing features for glmnet
y <- Combined$Survived
# names(Combined)
x <- Combined[,c(2:8,10,11,13,15,16,18,19,21,22,24)]
x <- model.matrix(Survived~.-MSurnameLW-GivenNames,x) #colnames(x) 

library(glmnet)
# LASSO mix = 1 
# Ridge mix = 0
# Mixed: mix = (0,1)
mix <- .08 #.18 #.05
mixed.mod=glmnet(x[train,],y[train],alpha=mix,family="binomial")
plot(mixed.mod)

set.seed(15)
cv.out=cv.glmnet(x[train,],y[train],alpha=mix,family="binomial")
plot(cv.out)
bestlam=cv.out$lambda.min
mixed.pred=predict(mixed.mod,s=bestlam ,newx=x[test,],alpha=mix,family="binomial")
mixed.pred01 <- ifelse(mixed.pred>0, 1, 0)
table(mixed.pred01, Combined$Survived[test])
# mixed.pred01   0   1
#             0 112  15
#             1   8  44
# > 23/179
# [1] 0.1284916

#Final mix
# Refitting the model on the complete dataset
mixed.mod=glmnet(x[train.id,],y[train.id],
          alpha=mix,family="binomial") # , lambda = 2*bestlam
plot(mixed.mod)

set.seed(15)
cv.out=cv.glmnet(x[train.id,],y[train.id],alpha=mix,family="binomial")
plot(cv.out)
bestlam=cv.out$lambda.min

mixed.pred=predict(mixed.mod,s=bestlam,
                   newx=x[train.id,],alpha=mix,family="binomial")
mixed.pred01 <- ifelse(mixed.pred>0, 1, 0)
table(mixed.pred01, Combined$Survived[train.id])
# mixed.pred01   0   1
#            0 521  62
#            1  28 280
# > 90/891
# [1] 0.1010101 - training error

mixed.pred=predict(mixed.mod,s=bestlam,
          newx=x[-train.id,],alpha=mix,family="binomial")
mixed.pred01 <- ifelse(mixed.pred>0, 1, 0)
table(mixed.pred01, Combined$Survived[-train.id])

# Preparing submission
submit <- Combined[-train.id, 1:2]
submit[,2] <- mixed.pred01
row.names(submit) <- NULL
View(submit)
write.csv(submit, "logreg_f1.csv", row.names = F, quote = F)

