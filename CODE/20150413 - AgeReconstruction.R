rm(list = ls())
load(file = "CombinedNamesParsed.dat", verbose = T)

# Why Age is important
attach(Combined)
#plot(Age[train.id], Survived[train.id]) # useless plot
a <- table(Age[train.id], Survived[train.id])
a <- data.frame(a)
#View(a)
len <- dim(a)[1]/2
b <- data.frame(Age = a[1:len,1], Died = a[1:len, 3], Survived = a[(len+1):(2*len),3])
#View(b) # first column is a factor! - class(b[,1])
# sum(b[,2:3]) - checking that the total number of passengers is correct
b[,1]<-as.numeric(as.character(b[,1])) #now numeriс - class(b[,1])
hw <- c(80, 3, 5, 8) #running average halfwidths

Srate <- sapply(1:4, function(j){
  sapply(1:80, function(i){
    bot<- i-hw[j]; top <- i+hw[j]
    id<- (b[,1]>bot)&(b[,1]<top)
    n <- sum(id)
    sr <- sum(b[id,3])/sum(b[id,2])
    return(log(sr))
  })  
})

matplot(1:80, Srate, type = "l", 
        lwd= c(3, 2, 2, 2), xlab = "Age, years",
        ylab = "Survival log-odds",
        main = "Survival log-odds running average",
        xaxp = c(0,80,8))
legend(x = 40, y = 1,        legend = c("average odds",
                                          "+/-3 years averaged",
                                          "+/-5 years averaged",
                                          "+/-8 years averaged"),
       lwd= c(3, 2, 2, 2), lty = 1:4, col = 1:4)
# Looks like a 6-7-th order poly 
detach(Combined)
rm(a, b, Srate, hw, len)

#Use a separate data frame
AgeData <- Combined[, !(names(Combined) %in% 
            c("PassengerId", "Survived", "AgeStatus"))] 
  # x <- t(sapply(1:21, function(i){
  #                   c(names(AgeData)[i], 
  #                     class(AgeData[,i]))}))

AgeData$hasAnyNick <- as.factor(AgeData$hasAnyNick)
AgeData$hasFamily <- as.factor(AgeData$hasFamily)
AgeData$NumCab <- as.factor(AgeData$NumCab)
AgeData$TickNum <- as.factor(AgeData$TickNum)

# x <- t(sapply(1:21, function(i){
#                   c(names(AgeData)[i], 
#                     class(AgeData[,i]))}))

full <- !(is.na(AgeData$Age))
full <- which(full == T)
AgeData$Age[-full] <- 0

# a bit of exploratory data analysis
y <- sapply(1:21, function(i){
        plot(AgeData[full,i], AgeData$Age[full], main = "Age exploration", 
             xlab = names(AgeData)[i], ylab = "Age")})
# observations from the plots
# 1. Embraked looks not associated with the response. 
#    Q option might be a little.
# 2. NumCab of 3 or 4 generally correspond to a higher 
#    age people
# 3. Deck F and G - smaller age?
# 4. Most of the children had a low pass fare.
# 5. Same holds for fares, but class separation 
#    is less visible
# 6. TickFreq is better viewed in factors: plot(as.factor(AgeData[train,"TickFreq"]), AgeData$Age[train])
#    Single tickets tipically belong to older people
# 7. TickNum reveals sort of qualitative behavior - possibly additional feature is needed
# 8. TickPrefix, hasFamily, Sex does not seem to matter
# 9. High SibSp and Parch values seem to be related to age
# 10. Presence of a nick name seem to be relevant
# 11. Name derivatives except for Title look useless
# 12. Pclass on the average inversely proportional to age

library(glmnet)
set.seed(15)
train <- sample(x = 1:length(full), 
                size = length(full)*0.8,
                replace = F)
test <- -train
train <- full[train]
test <- full[-train] 

# Preparing features for glmnet
x <- AgeData
x <- model.matrix(sqrt(Age)~.+I(SibSp^3)-
                    MSurnameLW-GivenNames,x) #colnames(x)
y <- sqrt(AgeData$Age)
# names(AgeData)

#elasticnet 
mix <- 0.11
mod <- glmnet(x[train,],y[train],alpha=mix,family="gaussian")
plot(mod)

set.seed(15)
cv=cv.glmnet(x[train,],y[train],alpha=mix,family="gaussian")
plot(cv)
bestlam=cv$lambda.min

pred=predict(mod,s=bestlam ,newx=x[test,],alpha=mix,family="gaussian")
plot(AgeData$Age[test], pred^2)
MSE <- (sum((AgeData$Age[test] - pred^2)^2))/length(test)
pred=predict(mod,s=bestlam ,newx=x[train,],alpha=mix,family="gaussian")
MSEtr <- (sum((AgeData$Age[train] - pred^2)^2))/length(train)
plot(AgeData$Age[train], pred^2)

full.mod <- glmnet(x[full,],y[full],alpha=mix,family="gaussian")
AgeP <- predict(full.mod, s=bestlam, newx = x[-full,])
AgeP <- (AgeP^2)
plot(AgeP)
# sum(AgeP>80)
# sum(AgeP<15)
# Amount of kids seem to be underestimated
# > sum(AgeP<15)/sum(AgeP>=15)
# [1] 0.06910569
# > sum(AgeData$Age[full]<15)/sum(AgeData$Age[full]>=15)
# [1] 0.1163287

Combined$Age[-full]<-AgeP
# sum(is.na(Combined$Age)) # it's zero!

save(Combined, train.id, N, InsertAfter, InsertAfterName, file = "CombinedAgesRestored.dat")
rm(list = ls())

