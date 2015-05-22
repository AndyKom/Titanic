rm(list = ls())
load(file = "CombinedAfterLR.dat", verbose = T)

# Preparing features
Combined <- Combined[,-24] #removing Cabin - NAs
for(i in c(1,2,9,16,25))
  Combined[,i] <- as.factor(Combined[,i]) #names(Combined)[c(1,9,16,25)]
#Making FamilySize integer
Combined$FamilySize <- Combined$SibSp+Combined$Parch+1L

NnC <- t(sapply(1:length(Combined), # Name'n'Class
                function(i){
                  c(names(Combined)[i],class(Combined[,i]))}))
toScale <- ifelse((NnC[,2]=="numeric"), T, F)

#Splitting
set.seed(15)
train <- sample(x = train.id, 
                size = length(train.id)*0.65,
                replace = F)
test <- train.id[-train]

#Fitting
library(doParallel)
registerDoParallel(4,cores = 4)


#Listing formulas to choose from
Forms <- c(
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW+Parch+FamilySize+hasFamily+TickNumGroup+Deck",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW+Parch+FamilySize+hasFamily+TickNumGroup",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW+Parch+FamilySize+hasFamily",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW+Parch+FamilySize",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW+Parch",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp+SurnameLW",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare+SibSp",
  "Survived~Pclass+Title+Sex+Age+PassFare+Fare",
  "Survived~Pclass+Title+Sex+Age+PassFare",
  "Survived~Pclass+Title+Sex+Age",
  "Survived~Pclass+Title+Sex"
  )

# Radial kernel
tune.rad.list <- foreach(i = 1:length(Forms)) %dopar% {
  library(e1071)
  set.seed(513)  
  tune(svm, formula(Forms[i]), 
       data = Combined[train.id,],
       ranges = list(cost = sqrt(10)^(1:9),
                     gamma = sqrt(10)^(-6:2)),
       scale = c(F,F,F,T,T,T,F,F,F,F,F,F,F,F,F), 
       kernel = "radial")
}
set.seed(513)  
a <-   tune(svm, formula("Survived~Pclass+Title+Sex+Age+PassFare+Fare+SurnameLW"), 
            data = Combined[train.id,],
            ranges = list(cost = sqrt(10)^(1:9),
                          gamma = sqrt(10)^(-6:2)),
            scale = c(F,F,F,T,T,T,F,F,F,F,F,F,F,F,F), 
            kernel = "radial")


submit <- Combined[-train.id, 1:2]
submit[,2] <- predict(a$best.model, Combined[-train.id,])
row.names(submit) <- NULL
View(submit)
write.csv(submit, "SVMradsub1.csv", row.names = F, quote = F)
