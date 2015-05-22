load(file = "CombinedAgesRestored.dat", verbose = T)

#Exploring Simple Features
attach(Combined)
  table(Survived[train.id], Pclass[train.id])
  table(Survived[train.id], Sex[train.id])
  table(Survived[train.id], AgeStatus[train.id])
  table(Survived[train.id], SibSp[train.id])
  table(Survived[train.id], Parch[train.id])
  table(Survived[train.id], FamilySize[train.id])
  
  table(Survived[train.id], Title[train.id])
  table(Survived[train.id], hasAnyNick[train.id])
  table(Survived[train.id], hasFamily[train.id])

  table(Survived[train.id], TickFreq[train.id])
  table(Survived[train.id], TickNumGroup[train.id])

#Plotting Fares
a <- table(Fare[train.id], Survived[train.id])
a <- data.frame(a)
#View(a)
len <- dim(a)[1]/2
b <- data.frame(Fare = a[1:len,1], Died = a[1:len, 3], Survived = a[(len+1):(2*len),3])
#View(b) # first column is a factor! - class(b[,1])
# sum(b[,2:3]) - checking that the total number of passengers is correct
b[,1]<-as.numeric(as.character(b[,1])) #now numeriс - class(b[,1])
hw <- c(512, 10, 20, 300) #running average halfwidths

Srate <- sapply(1:4, function(j){
  sapply(1:len, function(i){
    bot<- b[i,1]-hw[j]; top <- b[i,1]+hw[j]
    id<- (b[,1]>bot)&(b[,1]<top)
    n <- sum(id)
    sr <- sum(b[id,3])/sum(b[id,2])
    return(log(sr))
  })  
})

matplot(b[,1], Srate, type = "l", 
        lwd= c(3, 2, 2, 2), xlab = "Ticket price, units",
        ylab = "Survival log-odds",
        main = "Survival log-odds running average",
        xaxp = c(0,512,8))
legend(x = 256, y = 2,        legend = c("average odds",
                                         "+/-10 units averaged",
                                         "+/-20 units averaged",
                                         "+/-300 units averaged"),
       lwd= c(3, 2, 2, 2), lty = 1:4, col = 1:4)

#Plotting PassFares
a <- table(PassFare[train.id], Survived[train.id])
a <- data.frame(a)
#View(a)
len <- dim(a)[1]/2
b <- data.frame(Fare = a[1:len,1], Died = a[1:len, 3], Survived = a[(len+1):(2*len),3])
#View(b) # first column is a factor! - class(b[,1])
# sum(b[,2:3]) - checking that the total number of passengers is correct
b[,1]<-as.numeric(as.character(b[,1])) #now numeriс - class(b[,1])
hw <- c(128, 10, 20, 60) #running average halfwidths

Srate <- sapply(1:4, function(j){
  sapply(1:len, function(i){
    bot<- b[i,1]-hw[j]; top <- b[i,1]+hw[j]
    id<- (b[,1]>bot)&(b[,1]<top)
    n <- sum(id)
    sr <- sum(b[id,3])/sum(b[id,2])
    return(log(sr))
  })  
})

matplot(b[,1], Srate, type = "l", 
        lwd= c(3, 2, 2, 2), xlab = "Passenger fare, units",
        ylab = "Survival log-odds",
        main = "Survival log-odds running average",
        xaxp = c(0,128,8))
legend(x = -2, y = 1.4,        legend = c("average odds",
                                         "+/-10 units averaged",
                                         "+/-20 units averaged",
                                         "+/-60 units averaged"),
       lwd= c(3, 2, 2, 2), lty = 1:4, col = 1:4)

  table(Survived[train.id], Deck[train.id])
  table(Survived[train.id], NumCab[train.id])
  table(Survived[train.id], Embarked[train.id])



detach(Combined)
