# Reading data
{ #loading raw data
  rm(list = ls())
  Train = read.csv("d:/~Data Science/EPAM KAGGLE TITANIC/DATA/train.csv", na.string = "")
  Test = read.csv("d:/~Data Science/EPAM KAGGLE TITANIC/DATA/test.csv", na.string = "")
  Test$Survived <- rep(0, dim(Test)[1])
  train.id <- 1:dim(Train)[1]
  Combined <- rbind(Train, Test)
  rm(Train, Test)
  N <- length(Combined[,1])
} #loading raw data

#Useful routines
{
  InsertAfter <- function(DF, Predictor, ColName = NULL, pos = NULL, Name = "Predictor"){
    if (!(is.null(pos)&is.null(ColName))){
      if (is.null(pos)) {
        pos <- which(names(DF)==ColName)
      }
      DF <- data.frame(DF[,1:pos], Predictor, 
                       DF[,(pos+1):length(DF)])
      names(DF)[pos+1] <- Name    
    }
    return(DF)
  }
  
  InsertAfterName <- function(DF, ColName, Predictor, Name = "Predictor"){
    pos <- which(names(DF)==ColName)
    last <- names(DF)[length(DF)]
    DF <- data.frame(DF[,1:pos], Predictor,
                     DF[,(pos+1):length(DF)])
    names(DF)[pos+1] <- Name
    names(DF)[length(DF)] <- last
    return(DF)
  }
}

#Feature engineering
  {#Simple Features
    # Family Size
    Combined <- InsertAfterName(Combined, "Parch", 
                                Combined$SibSp +
                                Combined$Parch + 1, "FamilySize")
    
    # hasFamily indicator
    Combined <- InsertAfterName(Combined, "FamilySize", 
                                as.numeric(Combined$FamilySize>1),
                                "hasFamily")
    
    # AgeStatus
    AgeStatus <- sapply(Combined$Age, function(a){
      if (is.na(a)) return("Unknown")
      else{
        if ((a%%1 == 0)|((a%/%1 == 0)&(a%%1 != 0.5))) 
          return("Exact")
        else return ("Approx")
      }
    })
    Combined <- InsertAfterName(Combined, "Age", 
                                AgeStatus, "AgeStatus")
    
    # Update approximate Ages
    Combined$Age[AgeStatus=="Approx"]<-
      Combined$Age[AgeStatus=="Approx"]%/%1

    # Team Size (aka TickFrequency)
    Combined <- InsertAfterName(Combined, "Ticket", 
                  as.vector(table(Combined$Ticket)[Combined$Ticket]),
                  "TickFreq")
    
    # Fighting a single NA in Fare
    {#summary(Combined$Fare)
      idx <- is.na(Combined$Fare)
      #Combined[idx,] 
      # Lonely 3rd-class passenger embarked in Southampton 
      Combined$Fare[idx] <- median(na.omit(
        Combined$Fare[
          (Combined$Pclass==3)
          &(Combined$hasFamily==0)
          &(Combined$TickFreq==1)
          &(Combined$Embarked =="S")]
      ))
      rm(idx)
    }
    
    #Fare per capita (aka Passenger Fare)
    # A finer alternative to passenger class
    Combined <- InsertAfterName(Combined, "Fare", 
                  Combined$Fare/Combined$TickFreq, "PassFare")
    {
      pid <- (Combined$PassFare>1)
      plot(as.factor(Combined$Pclass[pid]), 
           log(Combined$Fare[pid]), col =(2:4),
           main = "log(Fare) vs. Pclass")
      plot(as.factor(Combined$Pclass[pid]), 
           log(Combined$PassFare[pid]), col =(2:4),
           main = "log(PassFare) vs. Pclass")
      rm(pid)
    } #explanatory plots


    }#Simple Features  

    # Parsing Tickets
    {
      # Ticket values almost always end with a number 
      # and might start with some sort of prefix
      Tickets <- strsplit(as.character(Combined$Ticket), split = " ")
      TickPrefix <- sapply(Tickets, function(x){
        xLen <- length(x)
        if (xLen==1) {
          if (!is.na(as.numeric(x))) return("No prefix")
          else return(x)
        }
        else return(paste0(x[1:xLen-1],collapse = ""))
      })
      # Pruning prefixes making 10+ out 50+
      # SOTON means Southampton
      # plot(log(sort(table(TickPrefix)))) - note x-coord
      { 
        TickPrefix[TickPrefix %in% 
                     c("A./5.", "A.5.", "A/5",
                       "A/5.", "A/S")] <- "A5"
        
        TickPrefix[TickPrefix %in%
                     c("A/4", "A/4.", "A4.",
                       "AQ/4", "A. 2.", "AQ/3.")] <- "A234"
        
        TickPrefix[TickPrefix %in%
                     c("C.A.", "C.A./SOTON",
                       "CA", "CA.")] <- "CA"
        
        TickPrefix[TickPrefix %in%
                     c("F.C.", "F.C.C.", "Fa")] <- "Fall"
        
        TickPrefix[TickPrefix %in%
                     c("S.O.C.", "SO/C")] <- "SOC"
        
        TickPrefix[TickPrefix %in%
                     c("S.O.P.", "S.O./P.P.", 
                       "S.P.", "S.W./PP", "SW/PP",
                       "P/PP", "PP", "LP")] <- "AnyPP"
        
        TickPrefix[TickPrefix %in%
                     c("SC", "SC/A.3", "SC/A4",
                       "S.C./A.4.", "SC/AH",
                       "SC/AH Basle", "SCO/W")] <- "SCother"
        
        TickPrefix[TickPrefix %in%
                     c("SC/Paris", "SC/PARIS",
                       "S.C./PARIS")] <- "SCParis"
        
        TickPrefix[TickPrefix %in%
                     c("SOTON/O.Q.", "SOTON/O2",
                       "SOTON/OQ", "STON/O 2.",
                       "STON/O2.", "STON/OQ.")] <- "SOTON"
        
        TickPrefix[TickPrefix %in%
                     c("W./C.", "W/C")] <- "WC"
        
        TickPrefix[TickPrefix %in%
                     c("WE/P", "W.E.P.")] <- "WEP"
      } # plot(log(sort(table(TickPrefix)))) - check the result

      TickNum <- sapply(Tickets, function(x){
        val <- as.numeric(x[length(x)])
        if (is.na(val)) return(0)
        else return(val)
      })
      Combined <- InsertAfterName(Combined, "Ticket",
                                  TickNum, "TickNum")
      Combined <- InsertAfterName(Combined, "Ticket",
                                  TickPrefix, "TickPrefix")

      # Inserting TickNumGroup
      x <- hist((Combined$TickNum), 500)
      which(x$counts>0)
      edges <- x$breaks[c(19,33,58,100)]
      TickNumGroup <- sapply(1:N, function(i){
        res <- min(which(edges>Combined$TickNum[i]))
        if (is.infinite(res)) res<-5
        return(res)
      })
      
      Combined <- InsertAfterName(Combined, "TickNum", 
                    as.factor(TickNumGroup), "TickNumGroup")
      rm(TickNum, TickPrefix, Tickets, TickNumGroup, x, edges)
      
      
      } #TickPrefix, TickNum and TickNumGroup added to Combined
    
    
    # TickNumFrequency - just to have a look
#    Combined <- InsertAfterName(Combined, "TickFreq", 
#      table(Combined$TickNum)[as.character(Combined$TickNum)], "TickNumFreq")
    # sum(Combined$TickFreq-Combined$TickNumFreq) - the features 
    # are almost the same, but their difference:
    # Combined[(Combined$TickFreq!=Combined$TickNumFreq),] 
    # shows some coarseness of AnyPP prefix (there's a pair of survivors
    # with S.W./PP ticket prefixes)

    # Fighting NAs in Embarked
    {
      Combined$Embarked[is.na(Combined$Embarked)] <- names(
        which.max(table(Combined$Embarked[
          (Combined$TickPrefix == "No prefix")&
          (abs(as.numeric(as.character(Combined$TickNum))
          -113572)<=300)]))) # "S"
    #where 113572 is the number of the ticket 
    #with unknown port of embarkation
    }    


  # Fighting NAs in Cabins
  {
    CabNum <- sapply(Combined$Cabin, function(i){
      if (is.na(i)) return(c("0", "X")) #No cabin, Deck X (Unknown)
      else {
        prs <- unlist(strsplit(as.character(i), split=" "))      
        prs1Len <- nchar(prs[1])
        prsLen <- length(prs)
        if (prs1Len == 1){
          if (prsLen == 1)
            return(c("1", prs[1]))
          else
            return(c(prsLen-1, prs[1]))
        }
        else return(c(prsLen, substr(prs[1],1,1)))
      }
    })  
    CabNum[2,(CabNum[2,]=="T")] <- "C"  
    
    Combined <- InsertAfterName(Combined, "Cabin", 
      factor(CabNum[2,]), "Deck")
    Combined$Deck[(Combined$Pclass == 1)&
                    (Combined$Deck== "X")] <- "C"
    Combined <- InsertAfterName(Combined, "Deck", 
      as.numeric(CabNum[1,]), "NumCab")
    rm(CabNum)
  } #Counting cabins per ticket and cabin decks

  # Removing processed and thus useless features Cabin and Ticket
  Combined <- Combined[,!(names(Combined) %in% 
                            c("Ticket", "Cabin"))]

save(Combined, train.id, N, InsertAfter, InsertAfterName, file = "CombinedDataPrep.dat")
rm(list = ls())
