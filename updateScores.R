# script for updating the general standing of the TDF 2023
# 
# winner <- c("A","B","C")
# # winner <- c("F","A","B")
# pronos <- c("B","C","A")
# 
# computeScore(winner,pronos)

computeScore <- function(winner,pronos)
{
  # base score
  score <- 0
  # check if any of teh winner is in the three pronos
  condition <- pronos %in% winner
  # if true -> add score
  
  condAdd <- TRUE
  
  if(sum(condition))
  {
    for(k in 1 : sum(condition) )
    {# for each of the pronos in the winner
      # position in the pronos vector
      pronosPos <-which(pronos == pronos[condition][k])
      # position in the winner position
      winnerPos <- which(winner == pronos[condition][k])
      if(pronosPos == winnerPos)
      {# if the pronos is well placed
        condAdd <- FALSE 
        if(pronosPos == 1 )
        {score <- score + 20}
          else if(pronosPos == 2)
          {score <- score + 15}
          else if(pronosPos == 3)
          {score <- score + 10}
      }
      else{# if not well placed
        if(winnerPos == 1 )
        {score <- score + 7.5}
        else if(winnerPos == 2)
        {score <- score + 5}
        else if(winnerPos == 3)
        {score <- score + 2.5}
        # if all three are in wrong order
      }
    }
    if(sum(condition) == 3 & condAdd == TRUE){score <- score+10}
    
  }
  return(score)
}


################################################################################
##########################  UPDATE STAGE POINTS ################################
# Etappe 1 
N = 6

# gets standings
standings <- read.csv("data/standings.csv")
# get evolStanding
evolStanding <- read.csv("data/evolStandings.csv")


# get data from the stage
nameFile <- paste0("data/etappe",N,".csv")
data <- read.csv(nameFile)

# get winner
winner <- data$Vainqueur[1:3]

# ppoints for the stage
players <- names(data)[2:7]
# for each player
for (i in players)
{
  # compute score for each player
   score <- computeScore(winner, data[1:3,i])
   data[4,i] <- score
  # update general standing
   standings[standings$Nom == i,"Points"] <- standings[standings$Nom == i,"Points"] + score
 # update evolution standing
   evolStanding[evolStanding$etappe == N,i] <- evolStanding[evolStanding$etappe == (N-1) ,i] + score
}
data
standings
evolStanding
# update csv
write.csv(data,nameFile,row.names = FALSE)
write.csv(standings,"data/standings.csv",row.names = FALSE)
write.csv(evolStanding,"data/evolStandings.csv",row.names = FALSE)


