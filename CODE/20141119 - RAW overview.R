rm(list = ls())
#Raw data overview
Train = read.csv("d:/~Data Science/EPAM KAGGLE TITANIC/DATA/train.csv", na.string = "")
Test = read.csv("d:/~Data Science/EPAM KAGGLE TITANIC/DATA/test.csv", na.string = "")

# Counting NAs
sum(is.na(Train)) # Total NAs in training data
sum(is.na(Test))  # Total NAs in test data

test.na = rep(0, 11)
train.na = rep(0, 12)
for (i in 1:11) {
  test.na[i] = sum(is.na(Test[[i]]))
  train.na[i] = sum(is.na(Train[[i]]))
}
train.na[12] = sum(is.na(Train[[12]]))
names(test.na) = names(Test)
names(train.na) = names(Train)

train.na
test.na

#Ratio of embarked 
test.emb = rep(0, 3)
train.emb = rep(0, 3)
emb.port = c("C","Q","S")
for (i in 1:3) {
  test.emb[i] = sum(Test$Embarked==emb.port[i])/length(Test$Embarked)
  train.emb[i] = sum(na.omit(Train$Embarked)==emb.port[i])/length(Train$Embarked)
}

train.emb
test.emb

#Ratio of Males and females 
test.sex = rep(0, 2)
train.sex = rep(0, 2)
sex = c("male","female")
for (i in 1:2) {
  test.sex[i] = sum(Test$Sex==sex[i])/length(Test$Sex)
  train.sex[i] = sum(Train$Sex==sex[i])/length(Train$Sex)
}

train.sex
test.sex

#Ratio by Class 
test.class = rep(0, 3)
train.class = rep(0, 3)
pass.class = c(1,2,3)
for (i in 1:3) {
  test.class[i] = sum(Test$Pclass==pass.class[i])/length(Test$Pclass)
  train.class[i] = sum(Train$Pclass==pass.class[i])/length(Train$Pclass)
}

train.class
test.class

# Comparing the Age data
hist(Train$Age, freq = T, breaks = 20, xlab = "Age in years", ylab = "Number of passengers", main = "Training data set passengers' age distribution")
hist(Test$Age, freq = T, breaks = 20, xlab = "Age in years", ylab = "Number of passengers", main = "Test data set passengers' age distribution")

#  comparison of Parch and SibSp
SibSp.tab = cbind(table(Train$SibSp)/length(Train$PassengerId), 
                  table(Test$SibSp)/length(Test$PassengerId))
matplot(as.numeric(names(SibSp.tab[,1])), log(SibSp.tab), pch=16,
        xlab = "Number of Siblings and Spouses", 
        ylab = "Log of passengers share" )
legend(5, -1, legend = c("Train", "Test"), pch = 16, col = 1:2)

Parch.tab = cbind(table(Train$Parch)/length(Train$PassengerId), 
                  table(Test$Parch)/length(Test$PassengerId))
Parch.tab[8,1]=0
matplot(as.numeric(names(Parch.tab[,1])), log(Parch.tab), pch=16,
        xlab = "Number of Parents and Children", 
        ylab = "Log of passengers share" )
legend(5, -1, legend = c("Train", "Test"), pch = 16, col = 1:2)

#####
#Each ticket has a unique fare (except for Ticket 7534 in Train data):
#####
Tick.table = table(Train$Ticket)
print(length(Tick.table))
DCcount = 0
for (i in 1:length(Tick.table)){
  idx = Train$Ticket == names(Tick.table)[i]
  DiffFare = sum(Train$Fare[idx]!=Train$Fare[idx][1]) #amount of tickets 
  #with fares different from that of the first one with the same number
  if (DiffFare>0) {
    print(c(i, DiffFare, "Different fare"))
    print(Train[idx,])
  }
  DiffCabin = sum(Train$Cabin[idx]!=Train$Cabin[idx][1]) #amount of tickets 
  #with cabins different from that of the first one with the same number
  if (is.na(DiffCabin)) {   
  } else
    if (DiffCabin>0) {
      print(c(i, DiffCabin, "Different cabin"))
      print(Train[idx,])
      DCcount = DCcount + 1
      print(DCcount)
    }
}

Tick.table = table(Test$Ticket)
print(length(Tick.table))
DCcount = 0
for (i in 1:length(Tick.table)){
  idx = Test$Ticket == names(Tick.table)[i]
  DiffFare = sum(Test$Fare[idx]!=Test$Fare[idx][1])
  if (is.na(DiffFare)) {
    print(c(i, DiffFare))
    print(Test[idx,])
  } else
    if (DiffFare>0) {
      print(c(i, DiffFare))
      print(Test[idx,])
    }
  DiffCabin = sum(Test$Cabin[idx]!=Test$Cabin[idx][1]) #amount of tickets 
  #with cabins different from that of the first one with the same number
  if (is.na(DiffCabin)) {   
  } else
    if (DiffCabin>0) {
      print(c(i, DiffCabin, "Different cabin"))
      print(Test[idx,])
      DCcount = DCcount + 1
      print(DCcount)
    }
}

#####
# Significance of fares
#####
Train.Fares = table(Train$Fare)
length(Train.Fares)
Test.Fares = table(Test$Fare)
length(Test.Fares)

Train.Class = factor(Train$Pclass, labels = c("I", "II", "III"))
Test.Class = factor(Test$Pclass, labels = c("I", "II", "III"))
plot(Train.Class, log(Train$Fare+1), col=4:2, 
     xlab = "Passenger class",
     ylab = "Log of passenger fare",
     main = "Training data set fares")
plot(Test.Class, log(Test$Fare+1), col=4:2, 
     xlab = "Passenger class",
     ylab = "Log of passenger fare",
     main = "Test data set fares")

#####
# Estimating the significance of Cabin data
#####
CabClass = rep(0, 4)
CabClass[4] = sum(is.na(Train$Cabin))/dim(Train)[1] # Cabin data is often empty
for (i in 1:3){
  CabClass[i]=sum(is.na(Train$Cabin[Train$Pclass==i]))/sum(Train$Pclass==i)
}
names(CabClass)=c("I", "II", "III", "All")
barplot(CabClass, col = (4:1), 
        ylab = "No cabin specified share", ylim = c(0,1),
        xlab = "Passenger class",
        main = "Training data set")
grid(nx=0, ny=5, col="cyan")

CabClass = rep(0, 4)
CabClass[4] = sum(is.na(Test$Cabin))/dim(Test)[1] # Cabin data is often empty
for (i in 1:3){
  CabClass[i]=sum(is.na(Test$Cabin[Test$Pclass==i]))/sum(Test$Pclass==i)
}
names(CabClass)=c("I", "II", "III", "All")
barplot(CabClass, col = (4:1), 
        ylab = "No cabin specified share", ylim = c(0,1),
        xlab = "Passenger class",
        main = "Test data set")
grid(nx=0, ny=5, col="cyan")