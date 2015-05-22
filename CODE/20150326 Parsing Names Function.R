rm(list = ls())
load(file = "CombinedDataPrep.dat", verbose = T)

#Working out Titles (after Trevor Stephens
# with some improvements) and Surnames, fixed = T
    Names <- sapply(as.character(Combined$Name),
                    strsplit, split = ", ")
    names(Names)<-NULL
Surname <- sapply(Names, "[", 1) #raw last names (i.e. longer than 1 word)
    
    Names <- sapply(Names, "[", 2)
    Names <- sapply(Names, strsplit, split =". ", fixed = T)
    names(Names)<-NULL
    LL <- sapply(Names, length) #one of the names has 
    id <- (LL==3L) #meaningless dot inside - table(LL)
    Names[id][[1]][2] <- paste(Names[id][[1]][2],
                           Names[id][[1]][3],sep=" ")

Title <- sapply(Names, "[", 1) #raw title
    {
      Title[Title=="Ms"] <- "Miss" # because Combined[Title=="Ms",] 
      # ladies were alone on board and had no maiden names provided in brackets
      Title[Title=="Mme"] <- "Mrs" 
      Title[Title=="Mlle"] <- "Miss"
      
      Title[Title %in% c("Capt", "Col", "Major")] <- "Military" # Military titles
      
      Title[Title %in% c("Lady", "Sir", "the Countess", "Jonkheer", "Dona", "Don")] <- "Noble"
    } #Pruning Titles
    
    Names <- sapply(Names, "[", 2)
    Names2 <- sapply(Names, strsplit,
                     split ="[()]", fixed = F)
    names(Names2) <- NULL


#Processing Given and Maiden names    
    rGNames <- rep("", N) # raw given names
    rMNames <- rGNames    # raw maiden names

    for (i in 1:N) { 
      L2 <- length(Names2[[i]])
      if (L2==1) # no parentheses found - both Names coincide
        {rGNames[i] <- Names[i]
         rMNames[i] <- "-empty-"}
      else # parentheses found
        { # and Names[[i]] is split into 2 or more parts
          # LL <- sapply(Names2, length)
          # table(LL)
          #    1    2    3    4 
          # 1088  205   15    1 
          # Names2[LL==4]
          # Names2[LL==3]
          # only the 1st 2 items seem to actually matter
          rMNames[i] <- if (L2<4) 
                          Names2[[i]][2]
                        else 
                          paste(Names2[[i]][2], Names2[[i]][4])
          L21 <- nchar(Names2[[i]][1])
          rGNames[i] <- if (L21==0) #Given name is missing
                          "-empty-"
                        else 
                          Names2[[i]][1]
        }
  
    }
    rm(L2, L21, LL, Names2, i, id, Names)

# It's time to clean the Given 
# and Maiden names from nicknames
# in quotes ""
Parsed <- data.frame(Combined$Name, Surname, Title, rGNames, rMNames)
View(Parsed)
aLen <- sapply(Parsed$rMNames, function(x){
  length(which(
    strsplit(as.character(x), "")[[1]]=="\"")
  )>0})
View(Parsed[aLen,])
bLen <- sapply(Parsed$rGNames, function(x){
  length(which(
    strsplit(as.character(x), "")[[1]]=="\"")
  )>0})
View(Parsed[bLen,])

ExtractNicks <- function(aName){
  qPos <- which(strsplit(aName, "")[[1]]=="\"")
  qPL <- length(qPos)
  if (qPL==0) return(c(gdata::trim(aName), "-empty-"))
  else 
    if ((qPL==2)&&((qPos[2]-qPos[1])>1)) {
      # quotes really delimit the nickname
      Nick <- substr(aName, qPos[1]+1, qPos[2]-1)
      Name <- gdata::trim(sub(substr(aName, qPos[1], qPos[2]), "", aName))
      Name <- sub("  ", " ", Name)
      Name <- if (Name=="") "-empty-" else Name
      return(c(Name, Nick))
    }
    else {
      # either a quote is missing OR
      # quotes are incorrectly positioned
      Name <- gdata::trim(sub("\"\"", "\"", aName))
      bPos <- which(strsplit(Name, "")[[1]]==" ")
      bPL <- length(bPos)
      if (bPL==0){
        #no blanks - everything is a nick
        Nick <- sub("\"", "", Name)
        return(c("-empty-", Nick))
      }
      else {
        # separating blanks exist!
        qPos <- qPos[1]
        if (qPos<bPos[1]) {
          # First word is a nick
          Nick <- sub("\"", "", substr(Name, 1, bPos[1]-1))
          Name <- substr(Name, bPos[1]+1, nchar(Name))
        }
        else{
          if (qPos>bPos[bPL]) {
            # Last word is a nick
            Nick <- sub("\"", "", substr(Name, bPos[bPL]+1, nchar(Name)))
            Name <- substr(Name, 1, bPos[bPL]-1)
          }
          else {
            idx <- which(abs(bPos-qPos)==1)
            if (qPos>bPos[idx]) {
              Nick <- substr(Name, qPos+1, bPos[idx+1]-1)
              Name <- paste(substr(Name, 1, bPos[idx]-1),
                      substr(Name, bPos[idx+1], nchar(Name)),
                      sep="")
            }
            else{
              Nick <- substr(Name, bPos[idx-1]+1, qPos-1)
              Name <- paste(substr(Name, 1, bPos[idx-1]-1),
              substr(Name, bPos[idx], nchar(Name)),
              sep="")
            }
          }
        }
        return(c(Name, Nick))
      }
    }

}

    X <- sapply(rGNames, ExtractNicks)
    GName <- X[1,]
    GNick <- X[2,]

    X <- sapply(rMNames, ExtractNicks)
    MName <- X[1,]
    MNick <- X[2,]

    Parsed <- data.frame(Combined$Name, Surname, Title, 
                         GName, GNick, MName, MNick)

    
    #Splitting surnames names
    SNsplit <- sapply(as.character(Surname), strsplit, split = " ")
    #... and extracting their last word
    SNlast <- sapply(SNsplit, function(x){x[length(x)]})
    # Counting frequencies
    LWSfreq = table(SNlast)
    Sfreq = table(Surname)
    PassSurFreq <- Sfreq[Surname]
    PassLWSFreq <- LWSfreq[SNlast]
    sum(PassLWSFreq-PassSurFreq)
    Surname[PassLWSFreq!=PassSurFreq]
    PassLWSFreq[PassLWSFreq!=PassSurFreq]
    PassSurFreq[PassLWSFreq!=PassSurFreq]
    # To find out that truncation to the last word is not essential for
    # passengers with families
    Combined[PassLWSFreq!=PassSurFreq,]
    
    Parsed <- InsertAfterName(Parsed, ColName = "Surname", 
      Predictor = toupper(SNlast), Name = "SurnameLW")
    View(Parsed[Surname!=SNlast,])

    # Now we repeat the same procedure for the Maiden Names?
    MSurLW <- sapply(1:N, function(i){
      if ((Combined$Sex[i]=="male")|
          (Parsed$Title[i] == "Miss"))
            {res <- Parsed$SurnameLW[i]}
      else { # use last word of the Maiden Name
        x <- unlist(strsplit(MName[i], split = " "))
        res <- x[length(x)]
      }
      return(as.character(res))
    })
    Parsed <- InsertAfterName(Parsed, ColName = "MName", 
      Predictor = toupper(MSurLW), Name = "MSurLW")
    
    hasAnyNick <- as.numeric(!((GNick == "-empty-")&(MNick == "-empty-")))
    Parsed <- InsertAfterName(Parsed, ColName = "MSurLW", 
                              Predictor = hasAnyNick, Name = "hasAnyNick")
    
    View(Parsed)
    
#     # First Given Name extraction
#     GFName <- sapply(1:N, function(i){
#       x <- unlist(strsplit(GName[i], split = " "))
#       return(as.character(x[1]))
#     })
#     Parsed <- InsertAfterName(Parsed, ColName = "GName", 
#       Predictor = GFName, Name = "GFName")
#     View(Parsed)
#     
#     GNfreq <- table(GName)
#     GFNfreq <- table(GFName)
#     PassGNfreq <- GNfreq[GName]
#     PassGFNfreq <- GFNfreq[GFName]
#     sum(PassGNfreq[(Combined$SibSp>0)]-PassGFNfreq[(Combined$SibSp>0)])

    # Putting everything useful to the Combined
    Combined <- InsertAfterName(Combined, ColName = "Name", 
                  Predictor = Title, Name = "Title")
    Combined <- InsertAfterName(Combined, ColName = "Title", 
                  Predictor = GName, Name = "GivenNames")
    Combined <- InsertAfterName(Combined, ColName = "GivenNames", 
                  Predictor = toupper(SNlast), Name = "SurnameLW")
    Combined <- InsertAfterName(Combined, ColName = "SurnameLW", 
                  Predictor = toupper(MSurLW), Name = "MSurnameLW")
    Combined <- InsertAfterName(Combined, ColName = "MSurnameLW", 
                  Predictor = hasAnyNick, Name = "hasAnyNick")
    
    rm(Parsed, MName, MNick, GName, GFName, GNick, MSurLW, 
       Surname, LWSfreq, SNlast, SNsplit, Sfreq, Title, 
       hasAnyNick, rGNames, rMNames)
    rm(aLen, bLen, PassLWSFreq, PassSurFreq)
    Combined <- Combined[,-4] # Removing Name
    View(Combined)

    # Further name derivatives pruning
    #Revisiting features
    attach(Combined)
    Combined$Pclass <- as.factor(Pclass)
    
    tmp <- as.character(GivenNames)
    tmp[(Title %in% c("Master", "Miss"))|
          (SibSp==0)] <- "-ignored-"
    # Further unique FirstGivenNames are ignored too
    tmp[tmp %in% names(table(tmp)[
      table(tmp)<2])] <- "-ignored-"
    Combined$GivenNames <- as.factor(tmp)
    # > length(table(tmp))
    # [1] 97
    # > length(table(GivenNames))
    # [1] 954
    
    tmp <- as.character(SurnameLW)
    tmp[(hasFamily==0)] <- "-ignored-"
    Combined$SurnameLW <- as.factor(tmp)
    # > length(table(SurnameLW))
    # [1] 874
    # > length(table(Combined$SurnameLW))
    # [1] 210

    tmp <- as.character(MSurnameLW)
    tmp[(hasFamily==0)] <- "-ignored-"
    Combined$MSurnameLW <- as.factor(tmp)
    # > length(table(MSurnameLW))
    # [1] 968
    # > length(table(Combined$MSurnameLW))
    # [1] 325

    detach(Combined); attach(Combined)
    
    # Next, Surnames unique in joint set of 
    # SurnameLW and MSurnameLW are ignored too
    tmp<-
      c(as.character(SurnameLW),as.character(MSurnameLW))
    SurnameLW[SurnameLW %in% 
                names(table(tmp)[table(tmp)<2])] <- "-ignored-"
    MSurnameLW[MSurnameLW %in% 
                 names(table(tmp)[table(tmp)<2])] <- "-ignored-"
    Combined$SurnameLW <- 
      as.factor(as.character(SurnameLW))
    Combined$MSurnameLW <- 
      as.factor(as.character(MSurnameLW))
    # > length(table(SurnameLW))
    # [1] 210
    # > length(table(Combined$SurnameLW))
    # [1] 198
    # > length(table(MSurnameLW))
    # [1] 325
    # > length(table(Combined$MSurnameLW))
    # [1] 206
    detach(Combined)

save(Combined, train.id, N, InsertAfter, InsertAfterName, file = "CombinedNamesParsed.dat")
rm(list = ls())

