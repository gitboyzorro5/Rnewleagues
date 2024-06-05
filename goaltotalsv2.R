# Goal totals V2
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#Create the home and awag team matrix
mls_goaltotalsv2 <- tapply(MLS$TG, MLS[c("HomeTeam", "AwayTeam")],mean)
arg_goaltotalsv2 <- tapply(ARG$TG, ARG[c("HomeTeam", "AwayTeam")],mean)
#Create the row and column sums
#MLS
mls_hgtotals <- rowSums(mls_goaltotalsv2, na.rm = T)
mls_agtotals <- colSums(mls_goaltotalsv2, na.rm = T)
#ARG
arg_hgtotals <- rowSums(arg_goaltotalsv2, na.rm = T)
arg_agtotals <- colSums(arg_goaltotalsv2, na.rm = T)

#Bind hgtotal and agtotal
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_hgtotals,mls_agtotals)
arg_goaltotalsv2 <- cbind(arg_goaltotalsv2,arg_hgtotals,arg_agtotals)

#add total goals
mls_totalgoals <- mls_hgtotals + mls_agtotals
arg_totalgoals <- arg_hgtotals + arg_agtotals

#bind total goals column
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_totalgoals)
arg_goaltotalsv2 <- cbind(arg_goaltotalsv2,arg_totalgoals)

#Get teams in each division as a vector
mls_teams <- sort(unique(MLS$HomeTeam))
arg_teams <- sort(unique(ARG$HomeTeam))

#initialize home and awag game vectors
#MLS
mls_home_games <- c()
mls_away_games <-c()
#ARG
arg_home_games <- c()
arg_away_games <-c()

#Get number of home and away games played
#MLS
for (i_mls in 1:length(mls_teams))
{

mls_home_games[i_mls] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls],])
mls_away_games[i_mls]  <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls],])

}
#ARG
for (i_arg in 1:length(arg_teams))
{

  arg_home_games[i_arg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg],])
  arg_away_games[i_arg]  <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg],])

}


#Add home games and away games to get total games played
mls_games_played <- mls_home_games + mls_away_games
arg_games_played <- arg_home_games + arg_away_games

#Bind total games plaged

mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_games_played)
arg_goaltotalsv2 <- cbind(arg_goaltotalsv2,arg_games_played)

#Calculate averaye total goals
mls_avg_totalgoals <- round((mls_totalgoals/ mls_games_played), digits = 4)
arg_avg_totalgoals <- round((arg_totalgoals/ arg_games_played), digits = 4)

#Remove NA values
mls_goaltotalsv2[is.na(mls_goaltotalsv2)] <- ""
arg_goaltotalsv2[is.na(arg_goaltotalsv2)] <- ""
#Bind average total goals

mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_avg_totalgoals)
arg_goaltotalsv2 <- cbind(arg_goaltotalsv2,arg_avg_totalgoals)

View(arg_goaltotalsv2)



