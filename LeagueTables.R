library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')

#league tables
#P-played W-wins D-draw L-loss F-goals for A-goals against GD-goal diff PTS-points
#MLS
mls_teams
mls_games_played
#hwins and away wins
mls_home_wins <- c()
mls_away_wins <- c()
mls_home_draws <- c()
mls_away_draws <- c()
mls_home_loss <- c()
mls_away_loss <- c()



for (i_mls_wins in 1:length(mls_teams))
{

  mls_home_wins[i_mls_wins] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_wins] & MLS$FTR == "H",])
  mls_away_wins[i_mls_wins] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_wins] & MLS$FTR == "A",])
  mls_home_draws[i_mls_wins] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_wins] & MLS$FTR == "D",])
  mls_away_draws[i_mls_wins] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_wins] & MLS$FTR == "D",])
  mls_home_loss[i_mls_wins] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_wins] & MLS$FTR == "A",])
  mls_away_loss[i_mls_wins] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_wins] & MLS$FTR == "H",])

}

mls_total_wins <- mls_home_wins + mls_away_wins
mls_total_draws <- mls_home_draws + mls_away_draws
mls_total_loss <- mls_home_loss + mls_away_loss

mls_league_table <- cbind(mls_teams,mls_games_played,mls_total_wins,mls_total_draws,mls_total_loss)
mls_GS <- mls_scoring$TGS
mls_GC <-mls_conceding$TGC
mls_GD <- mls_scoring$TGS - mls_conceding$TGC
mls_PTS <- (mls_total_wins*3) + (mls_total_draws*1)
mls_league_table <- cbind(mls_league_table,mls_GS,mls_GC,mls_GD,mls_PTS)
mls_league_table <- as.data.frame(mls_league_table)
#rename the columns
names(mls_league_table)[names(mls_league_table) == "mls_teams"] <- "Team"
names(mls_league_table)[names(mls_league_table) == "mls_games_played"] <- "P"
names(mls_league_table)[names(mls_league_table) == "mls_total_wins"] <- "W"
names(mls_league_table)[names(mls_league_table) == "mls_total_draws"] <- "D"
names(mls_league_table)[names(mls_league_table) == "mls_total_loss"] <- "L"
names(mls_league_table)[names(mls_league_table) == "mls_GS"] <- "F"
names(mls_league_table)[names(mls_league_table) == "mls_GC"] <- "A"
points_mls <- mls_league_table[order(as.numeric(mls_league_table$mls_PTS), decreasing = TRUE),]
points_mls$mls_rank <- 1:length(mls_teams)
row.names(points_mls) <- points_mls$mls_rank
#create final_mls_hf_against with team ranks in brackets
for(mls_rowhrank in 1:nrow(mls_form_team_against_h)) {
  for(mls_colhrank in 1:ncol(mls_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!mls_form_team_against_h[mls_rowhrank,mls_colhrank]=="",mls_form_team_against_h[mls_rowhrank,mls_colhrank] <- paste(mls_form_team_against_h[mls_rowhrank,mls_colhrank],"(",points_mls$mls_rank[points_mls$Team ==mls_form_team_against_h[mls_rowhrank,mls_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#ARG
arg_teams
arg_games_played
#hwins and away wins
arg_home_wins <- c()
arg_away_wins <- c()
arg_home_draws <- c()
arg_away_draws <- c()
arg_home_loss <- c()
arg_away_loss <- c()



for (i_arg_wins in 1:length(arg_teams))
{

  arg_home_wins[i_arg_wins] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_wins] & ARG$FTR == "H",])
  arg_away_wins[i_arg_wins] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_wins] & ARG$FTR == "A",])
  arg_home_draws[i_arg_wins] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_wins] & ARG$FTR == "D",])
  arg_away_draws[i_arg_wins] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_wins] & ARG$FTR == "D",])
  arg_home_loss[i_arg_wins] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_wins] & ARG$FTR == "A",])
  arg_away_loss[i_arg_wins] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_wins] & ARG$FTR == "H",])

}

arg_total_wins <- arg_home_wins + arg_away_wins
arg_total_draws <- arg_home_draws + arg_away_draws
arg_total_loss <- arg_home_loss + arg_away_loss

arg_league_table <- cbind(arg_teams,arg_games_played,arg_total_wins,arg_total_draws,arg_total_loss)
arg_GS <- arg_scoring$TGS
arg_GC <-arg_conceding$TGC
arg_GD <- arg_scoring$TGS - arg_conceding$TGC
arg_PTS <- (arg_total_wins*3) + (arg_total_draws*1)
arg_league_table <- cbind(arg_league_table,arg_GS,arg_GC,arg_GD,arg_PTS)
arg_league_table <- as.data.frame(arg_league_table)
#rename the columns
names(arg_league_table)[names(arg_league_table) == "arg_teams"] <- "Team"
names(arg_league_table)[names(arg_league_table) == "arg_games_played"] <- "P"
names(arg_league_table)[names(arg_league_table) == "arg_total_wins"] <- "W"
names(arg_league_table)[names(arg_league_table) == "arg_total_draws"] <- "D"
names(arg_league_table)[names(arg_league_table) == "arg_total_loss"] <- "L"
names(arg_league_table)[names(arg_league_table) == "arg_GS"] <- "F"
names(arg_league_table)[names(arg_league_table) == "arg_GC"] <- "A"
points_arg <- arg_league_table[order(as.numeric(arg_league_table$arg_PTS), decreasing = TRUE),]
points_arg$arg_rank <- 1:length(arg_teams)
row.names(points_arg) <- points_arg$arg_rank
#create final_arg_hf_against with team ranks in brackets
for(arg_rowhrank in 1:nrow(arg_form_team_against_h)) {
  for(arg_colhrank in 1:ncol(arg_form_team_against_h)) {

    # print(my_matrix[row, col])

    ifelse(!arg_form_team_against_h[arg_rowhrank,arg_colhrank]=="",arg_form_team_against_h[arg_rowhrank,arg_colhrank] <- paste(arg_form_team_against_h[arg_rowhrank,arg_colhrank],"(",points_arg$arg_rank[points_arg$Team ==arg_form_team_against_h[arg_rowhrank,arg_colhrank]],")",sep = ""),next)
    #print(my_matrix[row, col])


  }
}

#D2
# d2_teams
# d2_games_played
# #hwins and away wins
# d2_home_wins <- c()
# d2_away_wins <- c()
# d2_home_draws <- c()
# d2_away_draws <- c()
# d2_home_loss <- c()
# d2_away_loss <- c()
#
#
#
# for (i_d2_wins in 1:length(d2_teams))
# {
#
#   d2_home_wins[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "H",])
#   d2_away_wins[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "A",])
#   d2_home_draws[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "D",])
#   d2_away_draws[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "D",])
#   d2_home_loss[i_d2_wins] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_wins] & D2$FTR == "A",])
#   d2_away_loss[i_d2_wins] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_wins] & D2$FTR == "H",])
#
# }
#
# d2_total_wins <- d2_home_wins + d2_away_wins
# d2_total_draws <- d2_home_draws + d2_away_draws
# d2_total_loss <- d2_home_loss + d2_away_loss
#
# d2_league_table <- cbind(d2_teams,d2_games_played,d2_total_wins,d2_total_draws,d2_total_loss)
# d2_GS <- d2_scoring$TGS
# d2_GC <-d2_conceding$TGC
# d2_GD <- d2_scoring$TGS - d2_conceding$TGC
# d2_PTS <- (d2_total_wins*3) + (d2_total_draws*1)
# d2_league_table <- cbind(d2_league_table,d2_GS,d2_GC,d2_GD,d2_PTS)
# d2_league_table <- as.data.frame(d2_league_table)
# #rename the columns
# names(d2_league_table)[names(d2_league_table) == "d2_teams"] <- "Team"
# names(d2_league_table)[names(d2_league_table) == "d2_games_played"] <- "P"
# names(d2_league_table)[names(d2_league_table) == "d2_total_wins"] <- "W"
# names(d2_league_table)[names(d2_league_table) == "d2_total_draws"] <- "D"
# names(d2_league_table)[names(d2_league_table) == "d2_total_loss"] <- "L"
# names(d2_league_table)[names(d2_league_table) == "d2_GS"] <- "F"
# names(d2_league_table)[names(d2_league_table) == "d2_GC"] <- "A"
# points_d2 <- d2_league_table[order(as.numeric(d2_league_table$d2_PTS), decreasing = TRUE),]
# points_d2$d2_rank <- 1:length(d2_teams)
# row.names(points_d2) <- points_d2$d2_rank
# #create final_d2_hf_against with team ranks in brackets
# for(d2_rowhrank in 1:nrow(d2_form_team_against_h)) {
#   for(d2_colhrank in 1:ncol(d2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!d2_form_team_against_h[d2_rowhrank,d2_colhrank]=="",d2_form_team_against_h[d2_rowhrank,d2_colhrank] <- paste(d2_form_team_against_h[d2_rowhrank,d2_colhrank],"(",points_d2$d2_rank[points_d2$Team ==d2_form_team_against_h[d2_rowhrank,d2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #E0
# e0_teams
# e0_games_played
# #hwins and away wins
# e0_home_wins <- c()
# e0_away_wins <- c()
# e0_home_draws <- c()
# e0_away_draws <- c()
# e0_home_loss <- c()
# e0_away_loss <- c()
#
#
#
# for (i_e0_wins in 1:length(e0_teams))
# {
#
#   e0_home_wins[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "H",])
#   e0_away_wins[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "A",])
#   e0_home_draws[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "D",])
#   e0_away_draws[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "D",])
#   e0_home_loss[i_e0_wins] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_wins] & E0$FTR == "A",])
#   e0_away_loss[i_e0_wins] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_wins] & E0$FTR == "H",])
#
# }
#
# e0_total_wins <- e0_home_wins + e0_away_wins
# e0_total_draws <- e0_home_draws + e0_away_draws
# e0_total_loss <- e0_home_loss + e0_away_loss
#
# e0_league_table <- cbind(e0_teams,e0_games_played,e0_total_wins,e0_total_draws,e0_total_loss)
# e0_GS <- e0_scoring$TGS
# e0_GC <-e0_conceding$TGC
# e0_GD <- e0_scoring$TGS - e0_conceding$TGC
# e0_PTS <- (e0_total_wins*3) + (e0_total_draws*1)
# e0_league_table <- cbind(e0_league_table,e0_GS,e0_GC,e0_GD,e0_PTS)
# e0_league_table <- as.data.frame(e0_league_table)
# #rename the columns
# names(e0_league_table)[names(e0_league_table) == "e0_teams"] <- "Team"
# names(e0_league_table)[names(e0_league_table) == "e0_games_played"] <- "P"
# names(e0_league_table)[names(e0_league_table) == "e0_total_wins"] <- "W"
# names(e0_league_table)[names(e0_league_table) == "e0_total_draws"] <- "D"
# names(e0_league_table)[names(e0_league_table) == "e0_total_loss"] <- "L"
# names(e0_league_table)[names(e0_league_table) == "e0_GS"] <- "F"
# names(e0_league_table)[names(e0_league_table) == "e0_GC"] <- "A"
# points_e0 <- e0_league_table[order(as.numeric(e0_league_table$e0_PTS), decreasing = TRUE),]
# points_e0$e0_rank <- 1:length(e0_teams)
# row.names(points_e0) <- points_e0$e0_rank
# #create final_e0_hf_against with team ranks in brackets
# for(e0_rowhrank in 1:nrow(e0_form_team_against_h)) {
#   for(e0_colhrank in 1:ncol(e0_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!e0_form_team_against_h[e0_rowhrank,e0_colhrank]=="",e0_form_team_against_h[e0_rowhrank,e0_colhrank] <- paste(e0_form_team_against_h[e0_rowhrank,e0_colhrank],"(",points_e0$e0_rank[points_e0$Team ==e0_form_team_against_h[e0_rowhrank,e0_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #E1
# e1_teams
# e1_games_played
# #hwins and away wins
# e1_home_wins <- c()
# e1_away_wins <- c()
# e1_home_draws <- c()
# e1_away_draws <- c()
# e1_home_loss <- c()
# e1_away_loss <- c()
#
#
#
# for (i_e1_wins in 1:length(e1_teams))
# {
#
#   e1_home_wins[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "H",])
#   e1_away_wins[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "A",])
#   e1_home_draws[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "D",])
#   e1_away_draws[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "D",])
#   e1_home_loss[i_e1_wins] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_wins] & E1$FTR == "A",])
#   e1_away_loss[i_e1_wins] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_wins] & E1$FTR == "H",])
#
# }
#
# e1_total_wins <- e1_home_wins + e1_away_wins
# e1_total_draws <- e1_home_draws + e1_away_draws
# e1_total_loss <- e1_home_loss + e1_away_loss
#
# e1_league_table <- cbind(e1_teams,e1_games_played,e1_total_wins,e1_total_draws,e1_total_loss)
# e1_GS <- e1_scoring$TGS
# e1_GC <-e1_conceding$TGC
# e1_GD <- e1_scoring$TGS - e1_conceding$TGC
# e1_PTS <- (e1_total_wins*3) + (e1_total_draws*1)
# e1_league_table <- cbind(e1_league_table,e1_GS,e1_GC,e1_GD,e1_PTS)
# e1_league_table <- as.data.frame(e1_league_table)
# #rename the columns
# names(e1_league_table)[names(e1_league_table) == "e1_teams"] <- "Team"
# names(e1_league_table)[names(e1_league_table) == "e1_games_played"] <- "P"
# names(e1_league_table)[names(e1_league_table) == "e1_total_wins"] <- "W"
# names(e1_league_table)[names(e1_league_table) == "e1_total_draws"] <- "D"
# names(e1_league_table)[names(e1_league_table) == "e1_total_loss"] <- "L"
# names(e1_league_table)[names(e1_league_table) == "e1_GS"] <- "F"
# names(e1_league_table)[names(e1_league_table) == "e1_GC"] <- "A"
# points_e1 <- e1_league_table[order(as.numeric(e1_league_table$e1_PTS), decreasing = TRUE),]
# points_e1$e1_rank <- 1:length(e1_teams)
# row.names(points_e1) <- points_e1$e1_rank
# #create final_e1_hf_against with team ranks in brackets
# for(e1_rowhrank in 1:nrow(e1_form_team_against_h)) {
#   for(e1_colhrank in 1:ncol(e1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!e1_form_team_against_h[e1_rowhrank,e1_colhrank]=="",e1_form_team_against_h[e1_rowhrank,e1_colhrank] <- paste(e1_form_team_against_h[e1_rowhrank,e1_colhrank],"(",points_e1$e1_rank[points_e1$Team ==e1_form_team_against_h[e1_rowhrank,e1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# e2_teams
# e2_games_played
# #hwins and away wins
# e2_home_wins <- c()
# e2_away_wins <- c()
# e2_home_draws <- c()
# e2_away_draws <- c()
# e2_home_loss <- c()
# e2_away_loss <- c()
#
#
#
# for (i_e2_wins in 1:length(e2_teams))
# {
#
#   e2_home_wins[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "H",])
#   e2_away_wins[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "A",])
#   e2_home_draws[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "D",])
#   e2_away_draws[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "D",])
#   e2_home_loss[i_e2_wins] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_wins] & E2$FTR == "A",])
#   e2_away_loss[i_e2_wins] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_wins] & E2$FTR == "H",])
#
# }
#
# e2_total_wins <- e2_home_wins + e2_away_wins
# e2_total_draws <- e2_home_draws + e2_away_draws
# e2_total_loss <- e2_home_loss + e2_away_loss
#
# e2_league_table <- cbind(e2_teams,e2_games_played,e2_total_wins,e2_total_draws,e2_total_loss)
# e2_GS <- e2_scoring$TGS
# e2_GC <-e2_conceding$TGC
# e2_GD <- e2_scoring$TGS - e2_conceding$TGC
# e2_PTS <- (e2_total_wins*3) + (e2_total_draws*1)
# e2_league_table <- cbind(e2_league_table,e2_GS,e2_GC,e2_GD,e2_PTS)
# e2_league_table <- as.data.frame(e2_league_table)
# #rename the columns
# names(e2_league_table)[names(e2_league_table) == "e2_teams"] <- "Team"
# names(e2_league_table)[names(e2_league_table) == "e2_games_played"] <- "P"
# names(e2_league_table)[names(e2_league_table) == "e2_total_wins"] <- "W"
# names(e2_league_table)[names(e2_league_table) == "e2_total_draws"] <- "D"
# names(e2_league_table)[names(e2_league_table) == "e2_total_loss"] <- "L"
# names(e2_league_table)[names(e2_league_table) == "e2_GS"] <- "F"
# names(e2_league_table)[names(e2_league_table) == "e2_GC"] <- "A"
# points_e2 <- e2_league_table[order(as.numeric(e2_league_table$e2_PTS), decreasing = TRUE),]
# points_e2$e2_rank <- 1:length(e2_teams)
# row.names(points_e2) <- points_e2$e2_rank
# #create final_e2_hf_against with team ranks in brackets
# for(e2_rowhrank in 1:nrow(e2_form_team_against_h)) {
#   for(e2_colhrank in 1:ncol(e2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!e2_form_team_against_h[e2_rowhrank,e2_colhrank]=="",e2_form_team_against_h[e2_rowhrank,e2_colhrank] <- paste(e2_form_team_against_h[e2_rowhrank,e2_colhrank],"(",points_e2$e2_rank[points_e2$Team ==e2_form_team_against_h[e2_rowhrank,e2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #E3
# e3_teams
# e3_games_played
# #hwins and away wins
# e3_home_wins <- c()
# e3_away_wins <- c()
# e3_home_draws <- c()
# e3_away_draws <- c()
# e3_home_loss <- c()
# e3_away_loss <- c()
#
#
#
# for (i_e3_wins in 1:length(e3_teams))
# {
#
#   e3_home_wins[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "H",])
#   e3_away_wins[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "A",])
#   e3_home_draws[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "D",])
#   e3_away_draws[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "D",])
#   e3_home_loss[i_e3_wins] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_wins] & E3$FTR == "A",])
#   e3_away_loss[i_e3_wins] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_wins] & E3$FTR == "H",])
#
# }
#
# e3_total_wins <- e3_home_wins + e3_away_wins
# e3_total_draws <- e3_home_draws + e3_away_draws
# e3_total_loss <- e3_home_loss + e3_away_loss
#
# e3_league_table <- cbind(e3_teams,e3_games_played,e3_total_wins,e3_total_draws,e3_total_loss)
# e3_GS <- e3_scoring$TGS
# e3_GC <-e3_conceding$TGC
# e3_GD <- e3_scoring$TGS - e3_conceding$TGC
# e3_PTS <- (e3_total_wins*3) + (e3_total_draws*1)
# e3_league_table <- cbind(e3_league_table,e3_GS,e3_GC,e3_GD,e3_PTS)
# e3_league_table <- as.data.frame(e3_league_table)
# #rename the columns
# names(e3_league_table)[names(e3_league_table) == "e3_teams"] <- "Team"
# names(e3_league_table)[names(e3_league_table) == "e3_games_played"] <- "P"
# names(e3_league_table)[names(e3_league_table) == "e3_total_wins"] <- "W"
# names(e3_league_table)[names(e3_league_table) == "e3_total_draws"] <- "D"
# names(e3_league_table)[names(e3_league_table) == "e3_total_loss"] <- "L"
# names(e3_league_table)[names(e3_league_table) == "e3_GS"] <- "F"
# names(e3_league_table)[names(e3_league_table) == "e3_GC"] <- "A"
# points_e3 <- e3_league_table[order(as.numeric(e3_league_table$e3_PTS), decreasing = TRUE),]
# points_e3$e3_rank <- 1:length(e3_teams)
# row.names(points_e3) <- points_e3$e3_rank
# #create final_e3_hf_against with team ranks in brackets
# for(e3_rowhrank in 1:nrow(e3_form_team_against_h)) {
#   for(e3_colhrank in 1:ncol(e3_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!e3_form_team_against_h[e3_rowhrank,e3_colhrank]=="",e3_form_team_against_h[e3_rowhrank,e3_colhrank] <- paste(e3_form_team_against_h[e3_rowhrank,e3_colhrank],"(",points_e3$e3_rank[points_e3$Team ==e3_form_team_against_h[e3_rowhrank,e3_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #EC
# ec_teams
# ec_games_played
# #hwins and away wins
# ec_home_wins <- c()
# ec_away_wins <- c()
# ec_home_draws <- c()
# ec_away_draws <- c()
# ec_home_loss <- c()
# ec_away_loss <- c()
#
#
#
# for (i_ec_wins in 1:length(ec_teams))
# {
#
#   ec_home_wins[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "H",])
#   ec_away_wins[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "A",])
#   ec_home_draws[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "D",])
#   ec_away_draws[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "D",])
#   ec_home_loss[i_ec_wins] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_wins] & EC$FTR == "A",])
#   ec_away_loss[i_ec_wins] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_wins] & EC$FTR == "H",])
#
# }
#
# ec_total_wins <- ec_home_wins + ec_away_wins
# ec_total_draws <- ec_home_draws + ec_away_draws
# ec_total_loss <- ec_home_loss + ec_away_loss
#
# ec_league_table <- cbind(ec_teams,ec_games_played,ec_total_wins,ec_total_draws,ec_total_loss)
# ec_GS <- ec_scoring$TGS
# ec_GC <-ec_conceding$TGC
# ec_GD <- ec_scoring$TGS - ec_conceding$TGC
# ec_PTS <- (ec_total_wins*3) + (ec_total_draws*1)
# ec_league_table <- cbind(ec_league_table,ec_GS,ec_GC,ec_GD,ec_PTS)
# ec_league_table <- as.data.frame(ec_league_table)
# #rename the columns
# names(ec_league_table)[names(ec_league_table) == "ec_teams"] <- "Team"
# names(ec_league_table)[names(ec_league_table) == "ec_games_played"] <- "P"
# names(ec_league_table)[names(ec_league_table) == "ec_total_wins"] <- "W"
# names(ec_league_table)[names(ec_league_table) == "ec_total_draws"] <- "D"
# names(ec_league_table)[names(ec_league_table) == "ec_total_loss"] <- "L"
# names(ec_league_table)[names(ec_league_table) == "ec_GS"] <- "F"
# names(ec_league_table)[names(ec_league_table) == "ec_GC"] <- "A"
# points_ec <- ec_league_table[order(as.numeric(ec_league_table$ec_PTS), decreasing = TRUE),]
# points_ec$ec_rank <- 1:length(ec_teams)
# row.names(points_ec) <- points_ec$ec_rank
# #create final_ec_hf_against with team ranks in brackets
# for(ec_rowhrank in 1:nrow(ec_form_team_against_h)) {
#   for(ec_colhrank in 1:ncol(ec_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!ec_form_team_against_h[ec_rowhrank,ec_colhrank]=="",ec_form_team_against_h[ec_rowhrank,ec_colhrank] <- paste(ec_form_team_against_h[ec_rowhrank,ec_colhrank],"(",points_ec$ec_rank[points_ec$Team ==ec_form_team_against_h[ec_rowhrank,ec_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #F1
# f1_teams
# f1_games_played
# #hwins and away wins
# f1_home_wins <- c()
# f1_away_wins <- c()
# f1_home_draws <- c()
# f1_away_draws <- c()
# f1_home_loss <- c()
# f1_away_loss <- c()
#
#
#
# for (i_f1_wins in 1:length(f1_teams))
# {
#
#   f1_home_wins[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "H",])
#   f1_away_wins[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "A",])
#   f1_home_draws[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "D",])
#   f1_away_draws[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "D",])
#   f1_home_loss[i_f1_wins] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_wins] & F1$FTR == "A",])
#   f1_away_loss[i_f1_wins] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_wins] & F1$FTR == "H",])
#
# }
#
# f1_total_wins <- f1_home_wins + f1_away_wins
# f1_total_draws <- f1_home_draws + f1_away_draws
# f1_total_loss <- f1_home_loss + f1_away_loss
#
# f1_league_table <- cbind(f1_teams,f1_games_played,f1_total_wins,f1_total_draws,f1_total_loss)
# f1_GS <- f1_scoring$TGS
# f1_GC <-f1_conceding$TGC
# f1_GD <- f1_scoring$TGS - f1_conceding$TGC
# f1_PTS <- (f1_total_wins*3) + (f1_total_draws*1)
# f1_league_table <- cbind(f1_league_table,f1_GS,f1_GC,f1_GD,f1_PTS)
# f1_league_table <- as.data.frame(f1_league_table)
# #rename the columns
# names(f1_league_table)[names(f1_league_table) == "f1_teams"] <- "Team"
# names(f1_league_table)[names(f1_league_table) == "f1_games_played"] <- "P"
# names(f1_league_table)[names(f1_league_table) == "f1_total_wins"] <- "W"
# names(f1_league_table)[names(f1_league_table) == "f1_total_draws"] <- "D"
# names(f1_league_table)[names(f1_league_table) == "f1_total_loss"] <- "L"
# names(f1_league_table)[names(f1_league_table) == "f1_GS"] <- "F"
# names(f1_league_table)[names(f1_league_table) == "f1_GC"] <- "A"
# points_f1 <- f1_league_table[order(as.numeric(f1_league_table$f1_PTS), decreasing = TRUE),]
# points_f1$f1_rank <- 1:length(f1_teams)
# row.names(points_f1) <- points_f1$f1_rank
# #create final_f1_hf_against with team ranks in brackets
# for(f1_rowhrank in 1:nrow(f1_form_team_against_h)) {
#   for(f1_colhrank in 1:ncol(f1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!f1_form_team_against_h[f1_rowhrank,f1_colhrank]=="",f1_form_team_against_h[f1_rowhrank,f1_colhrank] <- paste(f1_form_team_against_h[f1_rowhrank,f1_colhrank],"(",points_f1$f1_rank[points_f1$Team ==f1_form_team_against_h[f1_rowhrank,f1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #F2
# f2_teams
# f2_games_played
# #hwins and away wins
# f2_home_wins <- c()
# f2_away_wins <- c()
# f2_home_draws <- c()
# f2_away_draws <- c()
# f2_home_loss <- c()
# f2_away_loss <- c()
#
#
#
# for (i_f2_wins in 1:length(f2_teams))
# {
#
#   f2_home_wins[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "H",])
#   f2_away_wins[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "A",])
#   f2_home_draws[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "D",])
#   f2_away_draws[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "D",])
#   f2_home_loss[i_f2_wins] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_wins] & F2$FTR == "A",])
#   f2_away_loss[i_f2_wins] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_wins] & F2$FTR == "H",])
#
# }
#
# f2_total_wins <- f2_home_wins + f2_away_wins
# f2_total_draws <- f2_home_draws + f2_away_draws
# f2_total_loss <- f2_home_loss + f2_away_loss
#
# f2_league_table <- cbind(f2_teams,f2_games_played,f2_total_wins,f2_total_draws,f2_total_loss)
# f2_GS <- f2_scoring$TGS
# f2_GC <-f2_conceding$TGC
# f2_GD <- f2_scoring$TGS - f2_conceding$TGC
# f2_PTS <- (f2_total_wins*3) + (f2_total_draws*1)
# f2_league_table <- cbind(f2_league_table,f2_GS,f2_GC,f2_GD,f2_PTS)
# f2_league_table <- as.data.frame(f2_league_table)
# #rename the columns
# names(f2_league_table)[names(f2_league_table) == "f2_teams"] <- "Team"
# names(f2_league_table)[names(f2_league_table) == "f2_games_played"] <- "P"
# names(f2_league_table)[names(f2_league_table) == "f2_total_wins"] <- "W"
# names(f2_league_table)[names(f2_league_table) == "f2_total_draws"] <- "D"
# names(f2_league_table)[names(f2_league_table) == "f2_total_loss"] <- "L"
# names(f2_league_table)[names(f2_league_table) == "f2_GS"] <- "F"
# names(f2_league_table)[names(f2_league_table) == "f2_GC"] <- "A"
# points_f2 <- f2_league_table[order(as.numeric(f2_league_table$f2_PTS), decreasing = TRUE),]
# points_f2$f2_rank <- 1:length(f2_teams)
# row.names(points_f2) <- points_f2$f2_rank
# #create final_f2_hf_against with team ranks in brackets
# for(f2_rowhrank in 1:nrow(f2_form_team_against_h)) {
#   for(f2_colhrank in 1:ncol(f2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!f2_form_team_against_h[f2_rowhrank,f2_colhrank]=="",f2_form_team_against_h[f2_rowhrank,f2_colhrank] <- paste(f2_form_team_against_h[f2_rowhrank,f2_colhrank],"(",points_f2$f2_rank[points_f2$Team ==f2_form_team_against_h[f2_rowhrank,f2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #G1
# g1_teams
# g1_games_played
# #hwins and away wins
# g1_home_wins <- c()
# g1_away_wins <- c()
# g1_home_draws <- c()
# g1_away_draws <- c()
# g1_home_loss <- c()
# g1_away_loss <- c()
#
#
#
# for (i_g1_wins in 1:length(g1_teams))
# {
#
#   g1_home_wins[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "H",])
#   g1_away_wins[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "A",])
#   g1_home_draws[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "D",])
#   g1_away_draws[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "D",])
#   g1_home_loss[i_g1_wins] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_wins] & G1$FTR == "A",])
#   g1_away_loss[i_g1_wins] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_wins] & G1$FTR == "H",])
#
# }
#
# g1_total_wins <- g1_home_wins + g1_away_wins
# g1_total_draws <- g1_home_draws + g1_away_draws
# g1_total_loss <- g1_home_loss + g1_away_loss
#
# g1_league_table <- cbind(g1_teams,g1_games_played,g1_total_wins,g1_total_draws,g1_total_loss)
# g1_GS <- g1_scoring$TGS
# g1_GC <-g1_conceding$TGC
# g1_GD <- g1_scoring$TGS - g1_conceding$TGC
# g1_PTS <- (g1_total_wins*3) + (g1_total_draws*1)
# g1_league_table <- cbind(g1_league_table,g1_GS,g1_GC,g1_GD,g1_PTS)
# g1_league_table <- as.data.frame(g1_league_table)
# #rename the columns
# names(g1_league_table)[names(g1_league_table) == "g1_teams"] <- "Team"
# names(g1_league_table)[names(g1_league_table) == "g1_games_played"] <- "P"
# names(g1_league_table)[names(g1_league_table) == "g1_total_wins"] <- "W"
# names(g1_league_table)[names(g1_league_table) == "g1_total_draws"] <- "D"
# names(g1_league_table)[names(g1_league_table) == "g1_total_loss"] <- "L"
# names(g1_league_table)[names(g1_league_table) == "g1_GS"] <- "F"
# names(g1_league_table)[names(g1_league_table) == "g1_GC"] <- "A"
# points_g1 <- g1_league_table[order(as.numeric(g1_league_table$g1_PTS), decreasing = TRUE),]
# points_g1$g1_rank <- 1:length(g1_teams)
# row.names(points_g1) <- points_g1$g1_rank
# #create final_g1_hf_against with team ranks in brackets
# for(g1_rowhrank in 1:nrow(g1_form_team_against_h)) {
#   for(g1_colhrank in 1:ncol(g1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!g1_form_team_against_h[g1_rowhrank,g1_colhrank]=="",g1_form_team_against_h[g1_rowhrank,g1_colhrank] <- paste(g1_form_team_against_h[g1_rowhrank,g1_colhrank],"(",points_g1$g1_rank[points_g1$Team ==g1_form_team_against_h[g1_rowhrank,g1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #I1
# i1_teams
# i1_games_played
# #hwins and away wins
# i1_home_wins <- c()
# i1_away_wins <- c()
# i1_home_draws <- c()
# i1_away_draws <- c()
# i1_home_loss <- c()
# i1_away_loss <- c()
#
#
#
# for (i_i1_wins in 1:length(i1_teams))
# {
#
#   i1_home_wins[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "H",])
#   i1_away_wins[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "A",])
#   i1_home_draws[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "D",])
#   i1_away_draws[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "D",])
#   i1_home_loss[i_i1_wins] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_wins] & I1$FTR == "A",])
#   i1_away_loss[i_i1_wins] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_wins] & I1$FTR == "H",])
#
# }
#
# i1_total_wins <- i1_home_wins + i1_away_wins
# i1_total_draws <- i1_home_draws + i1_away_draws
# i1_total_loss <- i1_home_loss + i1_away_loss
#
# i1_league_table <- cbind(i1_teams,i1_games_played,i1_total_wins,i1_total_draws,i1_total_loss)
# i1_GS <- i1_scoring$TGS
# i1_GC <-i1_conceding$TGC
# i1_GD <- i1_scoring$TGS - i1_conceding$TGC
# i1_PTS <- (i1_total_wins*3) + (i1_total_draws*1)
# i1_league_table <- cbind(i1_league_table,i1_GS,i1_GC,i1_GD,i1_PTS)
# i1_league_table <- as.data.frame(i1_league_table)
# #rename the columns
# names(i1_league_table)[names(i1_league_table) == "i1_teams"] <- "Team"
# names(i1_league_table)[names(i1_league_table) == "i1_games_played"] <- "P"
# names(i1_league_table)[names(i1_league_table) == "i1_total_wins"] <- "W"
# names(i1_league_table)[names(i1_league_table) == "i1_total_draws"] <- "D"
# names(i1_league_table)[names(i1_league_table) == "i1_total_loss"] <- "L"
# names(i1_league_table)[names(i1_league_table) == "i1_GS"] <- "F"
# names(i1_league_table)[names(i1_league_table) == "i1_GC"] <- "A"
# points_i1 <- i1_league_table[order(as.numeric(i1_league_table$i1_PTS), decreasing = TRUE),]
# points_i1$i1_rank <- 1:length(i1_teams)
# row.names(points_i1) <- points_i1$i1_rank
# #create final_i1_hf_against with team ranks in brackets
# for(i1_rowhrank in 1:nrow(i1_form_team_against_h)) {
#   for(i1_colhrank in 1:ncol(i1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!i1_form_team_against_h[i1_rowhrank,i1_colhrank]=="",i1_form_team_against_h[i1_rowhrank,i1_colhrank] <- paste(i1_form_team_against_h[i1_rowhrank,i1_colhrank],"(",points_i1$i1_rank[points_i1$Team ==i1_form_team_against_h[i1_rowhrank,i1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #I2
# i2_teams
# i2_games_played
# #hwins and away wins
# i2_home_wins <- c()
# i2_away_wins <- c()
# i2_home_draws <- c()
# i2_away_draws <- c()
# i2_home_loss <- c()
# i2_away_loss <- c()
#
#
#
# for (i_i2_wins in 1:length(i2_teams))
# {
#
#   i2_home_wins[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "H",])
#   i2_away_wins[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "A",])
#   i2_home_draws[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "D",])
#   i2_away_draws[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "D",])
#   i2_home_loss[i_i2_wins] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_wins] & I2$FTR == "A",])
#   i2_away_loss[i_i2_wins] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_wins] & I2$FTR == "H",])
#
# }
#
# i2_total_wins <- i2_home_wins + i2_away_wins
# i2_total_draws <- i2_home_draws + i2_away_draws
# i2_total_loss <- i2_home_loss + i2_away_loss
#
# i2_league_table <- cbind(i2_teams,i2_games_played,i2_total_wins,i2_total_draws,i2_total_loss)
# i2_GS <- i2_scoring$TGS
# i2_GC <-i2_conceding$TGC
# i2_GD <- i2_scoring$TGS - i2_conceding$TGC
# i2_PTS <- (i2_total_wins*3) + (i2_total_draws*1)
# i2_league_table <- cbind(i2_league_table,i2_GS,i2_GC,i2_GD,i2_PTS)
# i2_league_table <- as.data.frame(i2_league_table)
# #rename the columns
# names(i2_league_table)[names(i2_league_table) == "i2_teams"] <- "Team"
# names(i2_league_table)[names(i2_league_table) == "i2_games_played"] <- "P"
# names(i2_league_table)[names(i2_league_table) == "i2_total_wins"] <- "W"
# names(i2_league_table)[names(i2_league_table) == "i2_total_draws"] <- "D"
# names(i2_league_table)[names(i2_league_table) == "i2_total_loss"] <- "L"
# names(i2_league_table)[names(i2_league_table) == "i2_GS"] <- "F"
# names(i2_league_table)[names(i2_league_table) == "i2_GC"] <- "A"
# points_i2 <- i2_league_table[order(as.numeric(i2_league_table$i2_PTS), decreasing = TRUE),]
# points_i2$i2_rank <- 1:length(i2_teams)
# row.names(points_i2) <- points_i2$i2_rank
# #create final_i2_hf_against with team ranks in brackets
# for(i2_rowhrank in 1:nrow(i2_form_team_against_h)) {
#   for(i2_colhrank in 1:ncol(i2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!i2_form_team_against_h[i2_rowhrank,i2_colhrank]=="",i2_form_team_against_h[i2_rowhrank,i2_colhrank] <- paste(i2_form_team_against_h[i2_rowhrank,i2_colhrank],"(",points_i2$i2_rank[points_i2$Team ==i2_form_team_against_h[i2_rowhrank,i2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #N1
# n1_teams
# n1_games_played
# #hwins and away wins
# n1_home_wins <- c()
# n1_away_wins <- c()
# n1_home_draws <- c()
# n1_away_draws <- c()
# n1_home_loss <- c()
# n1_away_loss <- c()
#
#
#
# for (i_n1_wins in 1:length(n1_teams))
# {
#
#   n1_home_wins[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "H",])
#   n1_away_wins[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "A",])
#   n1_home_draws[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "D",])
#   n1_away_draws[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "D",])
#   n1_home_loss[i_n1_wins] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_wins] & N1$FTR == "A",])
#   n1_away_loss[i_n1_wins] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_wins] & N1$FTR == "H",])
#
# }
#
# n1_total_wins <- n1_home_wins + n1_away_wins
# n1_total_draws <- n1_home_draws + n1_away_draws
# n1_total_loss <- n1_home_loss + n1_away_loss
#
# n1_league_table <- cbind(n1_teams,n1_games_played,n1_total_wins,n1_total_draws,n1_total_loss)
# n1_GS <- n1_scoring$TGS
# n1_GC <-n1_conceding$TGC
# n1_GD <- n1_scoring$TGS - n1_conceding$TGC
# n1_PTS <- (n1_total_wins*3) + (n1_total_draws*1)
# n1_league_table <- cbind(n1_league_table,n1_GS,n1_GC,n1_GD,n1_PTS)
# n1_league_table <- as.data.frame(n1_league_table)
# #rename the columns
# names(n1_league_table)[names(n1_league_table) == "n1_teams"] <- "Team"
# names(n1_league_table)[names(n1_league_table) == "n1_games_played"] <- "P"
# names(n1_league_table)[names(n1_league_table) == "n1_total_wins"] <- "W"
# names(n1_league_table)[names(n1_league_table) == "n1_total_draws"] <- "D"
# names(n1_league_table)[names(n1_league_table) == "n1_total_loss"] <- "L"
# names(n1_league_table)[names(n1_league_table) == "n1_GS"] <- "F"
# names(n1_league_table)[names(n1_league_table) == "n1_GC"] <- "A"
# points_n1 <- n1_league_table[order(as.numeric(n1_league_table$n1_PTS), decreasing = TRUE),]
# points_n1$n1_rank <- 1:length(n1_teams)
# row.names(points_n1) <- points_n1$n1_rank
# #create final_n1_hf_against with team ranks in brackets
# for(n1_rowhrank in 1:nrow(n1_form_team_against_h)) {
#   for(n1_colhrank in 1:ncol(n1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!n1_form_team_against_h[n1_rowhrank,n1_colhrank]=="",n1_form_team_against_h[n1_rowhrank,n1_colhrank] <- paste(n1_form_team_against_h[n1_rowhrank,n1_colhrank],"(",points_n1$n1_rank[points_n1$Team ==n1_form_team_against_h[n1_rowhrank,n1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #P1
# p1_teams
# p1_games_played
# #hwins and away wins
# p1_home_wins <- c()
# p1_away_wins <- c()
# p1_home_draws <- c()
# p1_away_draws <- c()
# p1_home_loss <- c()
# p1_away_loss <- c()
#
#
#
# for (i_p1_wins in 1:length(p1_teams))
# {
#
#   p1_home_wins[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "H",])
#   p1_away_wins[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "A",])
#   p1_home_draws[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "D",])
#   p1_away_draws[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "D",])
#   p1_home_loss[i_p1_wins] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_wins] & P1$FTR == "A",])
#   p1_away_loss[i_p1_wins] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_wins] & P1$FTR == "H",])
#
# }
#
# p1_total_wins <- p1_home_wins + p1_away_wins
# p1_total_draws <- p1_home_draws + p1_away_draws
# p1_total_loss <- p1_home_loss + p1_away_loss
#
# p1_league_table <- cbind(p1_teams,p1_games_played,p1_total_wins,p1_total_draws,p1_total_loss)
# p1_GS <- p1_scoring$TGS
# p1_GC <-p1_conceding$TGC
# p1_GD <- p1_scoring$TGS - p1_conceding$TGC
# p1_PTS <- (p1_total_wins*3) + (p1_total_draws*1)
# p1_league_table <- cbind(p1_league_table,p1_GS,p1_GC,p1_GD,p1_PTS)
# p1_league_table <- as.data.frame(p1_league_table)
# #rename the columns
# names(p1_league_table)[names(p1_league_table) == "p1_teams"] <- "Team"
# names(p1_league_table)[names(p1_league_table) == "p1_games_played"] <- "P"
# names(p1_league_table)[names(p1_league_table) == "p1_total_wins"] <- "W"
# names(p1_league_table)[names(p1_league_table) == "p1_total_draws"] <- "D"
# names(p1_league_table)[names(p1_league_table) == "p1_total_loss"] <- "L"
# names(p1_league_table)[names(p1_league_table) == "p1_GS"] <- "F"
# names(p1_league_table)[names(p1_league_table) == "p1_GC"] <- "A"
# points_p1 <- p1_league_table[order(as.numeric(p1_league_table$p1_PTS), decreasing = TRUE),]
# points_p1$p1_rank <- 1:length(p1_teams)
# row.names(points_p1) <- points_p1$p1_rank
# #create final_p1_hf_against with team ranks in brackets
# for(p1_rowhrank in 1:nrow(p1_form_team_against_h)) {
#   for(p1_colhrank in 1:ncol(p1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!p1_form_team_against_h[p1_rowhrank,p1_colhrank]=="",p1_form_team_against_h[p1_rowhrank,p1_colhrank] <- paste(p1_form_team_against_h[p1_rowhrank,p1_colhrank],"(",points_p1$p1_rank[points_p1$Team ==p1_form_team_against_h[p1_rowhrank,p1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #SC0
# sc0_teams
# sc0_games_played
# #hwins and away wins
# sc0_home_wins <- c()
# sc0_away_wins <- c()
# sc0_home_draws <- c()
# sc0_away_draws <- c()
# sc0_home_loss <- c()
# sc0_away_loss <- c()
#
#
#
# for (i_sc0_wins in 1:length(sc0_teams))
# {
#
#   sc0_home_wins[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "H",])
#   sc0_away_wins[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "A",])
#   sc0_home_draws[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "D",])
#   sc0_away_draws[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "D",])
#   sc0_home_loss[i_sc0_wins] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "A",])
#   sc0_away_loss[i_sc0_wins] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_wins] & SC0$FTR == "H",])
#
# }
#
# sc0_total_wins <- sc0_home_wins + sc0_away_wins
# sc0_total_draws <- sc0_home_draws + sc0_away_draws
# sc0_total_loss <- sc0_home_loss + sc0_away_loss
#
# sc0_league_table <- cbind(sc0_teams,sc0_games_played,sc0_total_wins,sc0_total_draws,sc0_total_loss)
# sc0_GS <- sc0_scoring$TGS
# sc0_GC <-sc0_conceding$TGC
# sc0_GD <- sc0_scoring$TGS - sc0_conceding$TGC
# sc0_PTS <- (sc0_total_wins*3) + (sc0_total_draws*1)
# sc0_league_table <- cbind(sc0_league_table,sc0_GS,sc0_GC,sc0_GD,sc0_PTS)
# sc0_league_table <- as.data.frame(sc0_league_table)
# #rename the columns
# names(sc0_league_table)[names(sc0_league_table) == "sc0_teams"] <- "Team"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_games_played"] <- "P"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_total_wins"] <- "W"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_total_draws"] <- "D"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_total_loss"] <- "L"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_GS"] <- "F"
# names(sc0_league_table)[names(sc0_league_table) == "sc0_GC"] <- "A"
# points_sc0 <- sc0_league_table[order(as.numeric(sc0_league_table$sc0_PTS), decreasing = TRUE),]
# points_sc0$sc0_rank <- 1:length(sc0_teams)
# row.names(points_sc0) <- points_sc0$sc0_rank
# #create final_sc0_hf_against with team ranks in brackets
# for(sc0_rowhrank in 1:nrow(sc0_form_team_against_h)) {
#   for(sc0_colhrank in 1:ncol(sc0_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sc0_form_team_against_h[sc0_rowhrank,sc0_colhrank]=="",sc0_form_team_against_h[sc0_rowhrank,sc0_colhrank] <- paste(sc0_form_team_against_h[sc0_rowhrank,sc0_colhrank],"(",points_sc0$sc0_rank[points_sc0$Team ==sc0_form_team_against_h[sc0_rowhrank,sc0_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #SC1
# sc1_teams
# sc1_games_played
# #hwins and away wins
# sc1_home_wins <- c()
# sc1_away_wins <- c()
# sc1_home_draws <- c()
# sc1_away_draws <- c()
# sc1_home_loss <- c()
# sc1_away_loss <- c()
#
#
#
# for (i_sc1_wins in 1:length(sc1_teams))
# {
#
#   sc1_home_wins[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "H",])
#   sc1_away_wins[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "A",])
#   sc1_home_draws[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "D",])
#   sc1_away_draws[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "D",])
#   sc1_home_loss[i_sc1_wins] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "A",])
#   sc1_away_loss[i_sc1_wins] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_wins] & SC1$FTR == "H",])
#
# }
#
# sc1_total_wins <- sc1_home_wins + sc1_away_wins
# sc1_total_draws <- sc1_home_draws + sc1_away_draws
# sc1_total_loss <- sc1_home_loss + sc1_away_loss
#
# sc1_league_table <- cbind(sc1_teams,sc1_games_played,sc1_total_wins,sc1_total_draws,sc1_total_loss)
# sc1_GS <- sc1_scoring$TGS
# sc1_GC <-sc1_conceding$TGC
# sc1_GD <- sc1_scoring$TGS - sc1_conceding$TGC
# sc1_PTS <- (sc1_total_wins*3) + (sc1_total_draws*1)
# sc1_league_table <- cbind(sc1_league_table,sc1_GS,sc1_GC,sc1_GD,sc1_PTS)
# sc1_league_table <- as.data.frame(sc1_league_table)
# #rename the columns
# names(sc1_league_table)[names(sc1_league_table) == "sc1_teams"] <- "Team"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_games_played"] <- "P"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_total_wins"] <- "W"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_total_draws"] <- "D"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_total_loss"] <- "L"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_GS"] <- "F"
# names(sc1_league_table)[names(sc1_league_table) == "sc1_GC"] <- "A"
# points_sc1 <- sc1_league_table[order(as.numeric(sc1_league_table$sc1_PTS), decreasing = TRUE),]
# points_sc1$sc1_rank <- 1:length(sc1_teams)
# row.names(points_sc1) <- points_sc1$sc1_rank
# #create final_sc1_hf_against with team ranks in brackets
# for(sc1_rowhrank in 1:nrow(sc1_form_team_against_h)) {
#   for(sc1_colhrank in 1:ncol(sc1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sc1_form_team_against_h[sc1_rowhrank,sc1_colhrank]=="",sc1_form_team_against_h[sc1_rowhrank,sc1_colhrank] <- paste(sc1_form_team_against_h[sc1_rowhrank,sc1_colhrank],"(",points_sc1$sc1_rank[points_sc1$Team ==sc1_form_team_against_h[sc1_rowhrank,sc1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #SC2
# sc2_teams
# sc2_games_played
# #hwins and away wins
# sc2_home_wins <- c()
# sc2_away_wins <- c()
# sc2_home_draws <- c()
# sc2_away_draws <- c()
# sc2_home_loss <- c()
# sc2_away_loss <- c()
#
#
#
# for (i_sc2_wins in 1:length(sc2_teams))
# {
#
#   sc2_home_wins[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "H",])
#   sc2_away_wins[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "A",])
#   sc2_home_draws[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "D",])
#   sc2_away_draws[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "D",])
#   sc2_home_loss[i_sc2_wins] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "A",])
#   sc2_away_loss[i_sc2_wins] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_wins] & SC2$FTR == "H",])
#
# }
#
# sc2_total_wins <- sc2_home_wins + sc2_away_wins
# sc2_total_draws <- sc2_home_draws + sc2_away_draws
# sc2_total_loss <- sc2_home_loss + sc2_away_loss
#
# sc2_league_table <- cbind(sc2_teams,sc2_games_played,sc2_total_wins,sc2_total_draws,sc2_total_loss)
# sc2_GS <- sc2_scoring$TGS
# sc2_GC <-sc2_conceding$TGC
# sc2_GD <- sc2_scoring$TGS - sc2_conceding$TGC
# sc2_PTS <- (sc2_total_wins*3) + (sc2_total_draws*1)
# sc2_league_table <- cbind(sc2_league_table,sc2_GS,sc2_GC,sc2_GD,sc2_PTS)
# sc2_league_table <- as.data.frame(sc2_league_table)
# #rename the columns
# names(sc2_league_table)[names(sc2_league_table) == "sc2_teams"] <- "Team"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_games_played"] <- "P"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_total_wins"] <- "W"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_total_draws"] <- "D"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_total_loss"] <- "L"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_GS"] <- "F"
# names(sc2_league_table)[names(sc2_league_table) == "sc2_GC"] <- "A"
# points_sc2 <- sc2_league_table[order(as.numeric(sc2_league_table$sc2_PTS), decreasing = TRUE),]
# points_sc2$sc2_rank <- 1:length(sc2_teams)
# row.names(points_sc2) <- points_sc2$sc2_rank
# #create final_sc2_hf_against with team ranks in brackets
# for(sc2_rowhrank in 1:nrow(sc2_form_team_against_h)) {
#   for(sc2_colhrank in 1:ncol(sc2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sc2_form_team_against_h[sc2_rowhrank,sc2_colhrank]=="",sc2_form_team_against_h[sc2_rowhrank,sc2_colhrank] <- paste(sc2_form_team_against_h[sc2_rowhrank,sc2_colhrank],"(",points_sc2$sc2_rank[points_sc2$Team ==sc2_form_team_against_h[sc2_rowhrank,sc2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #SC3
# sc3_teams
# sc3_games_played
# #hwins and away wins
# sc3_home_wins <- c()
# sc3_away_wins <- c()
# sc3_home_draws <- c()
# sc3_away_draws <- c()
# sc3_home_loss <- c()
# sc3_away_loss <- c()
#
#
#
# for (i_sc3_wins in 1:length(sc3_teams))
# {
#
#   sc3_home_wins[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "H",])
#   sc3_away_wins[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "A",])
#   sc3_home_draws[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "D",])
#   sc3_away_draws[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "D",])
#   sc3_home_loss[i_sc3_wins] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "A",])
#   sc3_away_loss[i_sc3_wins] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_wins] & SC3$FTR == "H",])
#
# }
#
# sc3_total_wins <- sc3_home_wins + sc3_away_wins
# sc3_total_draws <- sc3_home_draws + sc3_away_draws
# sc3_total_loss <- sc3_home_loss + sc3_away_loss
#
# sc3_league_table <- cbind(sc3_teams,sc3_games_played,sc3_total_wins,sc3_total_draws,sc3_total_loss)
# sc3_GS <- sc3_scoring$TGS
# sc3_GC <-sc3_conceding$TGC
# sc3_GD <- sc3_scoring$TGS - sc3_conceding$TGC
# sc3_PTS <- (sc3_total_wins*3) + (sc3_total_draws*1)
# sc3_league_table <- cbind(sc3_league_table,sc3_GS,sc3_GC,sc3_GD,sc3_PTS)
# sc3_league_table <- as.data.frame(sc3_league_table)
# #rename the columns
# names(sc3_league_table)[names(sc3_league_table) == "sc3_teams"] <- "Team"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_games_played"] <- "P"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_total_wins"] <- "W"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_total_draws"] <- "D"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_total_loss"] <- "L"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_GS"] <- "F"
# names(sc3_league_table)[names(sc3_league_table) == "sc3_GC"] <- "A"
# points_sc3 <- sc3_league_table[order(as.numeric(sc3_league_table$sc3_PTS), decreasing = TRUE),]
# points_sc3$sc3_rank <- 1:length(sc3_teams)
# row.names(points_sc3) <- points_sc3$sc3_rank
# #create final_sc3_hf_against with team ranks in brackets
# for(sc3_rowhrank in 1:nrow(sc3_form_team_against_h)) {
#   for(sc3_colhrank in 1:ncol(sc3_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sc3_form_team_against_h[sc3_rowhrank,sc3_colhrank]=="",sc3_form_team_against_h[sc3_rowhrank,sc3_colhrank] <- paste(sc3_form_team_against_h[sc3_rowhrank,sc3_colhrank],"(",points_sc3$sc3_rank[points_sc3$Team ==sc3_form_team_against_h[sc3_rowhrank,sc3_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #SP1
# sp1_teams
# sp1_games_played
# #hwins and away wins
# sp1_home_wins <- c()
# sp1_away_wins <- c()
# sp1_home_draws <- c()
# sp1_away_draws <- c()
# sp1_home_loss <- c()
# sp1_away_loss <- c()
#
#
#
# for (i_sp1_wins in 1:length(sp1_teams))
# {
#
#   sp1_home_wins[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "H",])
#   sp1_away_wins[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "A",])
#   sp1_home_draws[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "D",])
#   sp1_away_draws[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "D",])
#   sp1_home_loss[i_sp1_wins] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "A",])
#   sp1_away_loss[i_sp1_wins] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_wins] & SP1$FTR == "H",])
#
# }
#
# sp1_total_wins <- sp1_home_wins + sp1_away_wins
# sp1_total_draws <- sp1_home_draws + sp1_away_draws
# sp1_total_loss <- sp1_home_loss + sp1_away_loss
#
# sp1_league_table <- cbind(sp1_teams,sp1_games_played,sp1_total_wins,sp1_total_draws,sp1_total_loss)
# sp1_GS <- sp1_scoring$TGS
# sp1_GC <-sp1_conceding$TGC
# sp1_GD <- sp1_scoring$TGS - sp1_conceding$TGC
# sp1_PTS <- (sp1_total_wins*3) + (sp1_total_draws*1)
# sp1_league_table <- cbind(sp1_league_table,sp1_GS,sp1_GC,sp1_GD,sp1_PTS)
# sp1_league_table <- as.data.frame(sp1_league_table)
# #rename the columns
# names(sp1_league_table)[names(sp1_league_table) == "sp1_teams"] <- "Team"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_games_played"] <- "P"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_total_wins"] <- "W"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_total_draws"] <- "D"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_total_loss"] <- "L"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_GS"] <- "F"
# names(sp1_league_table)[names(sp1_league_table) == "sp1_GC"] <- "A"
# points_sp1 <- sp1_league_table[order(as.numeric(sp1_league_table$sp1_PTS), decreasing = TRUE),]
# points_sp1$sp1_rank <- 1:length(sp1_teams)
# row.names(points_sp1) <- points_sp1$sp1_rank
# #create final_sp1_hf_against with team ranks in brackets
# for(sp1_rowhrank in 1:nrow(sp1_form_team_against_h)) {
#   for(sp1_colhrank in 1:ncol(sp1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sp1_form_team_against_h[sp1_rowhrank,sp1_colhrank]=="",sp1_form_team_against_h[sp1_rowhrank,sp1_colhrank] <- paste(sp1_form_team_against_h[sp1_rowhrank,sp1_colhrank],"(",points_sp1$sp1_rank[points_sp1$Team ==sp1_form_team_against_h[sp1_rowhrank,sp1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
# #SP2
# sp2_teams
# sp2_games_played
# #hwins and away wins
# sp2_home_wins <- c()
# sp2_away_wins <- c()
# sp2_home_draws <- c()
# sp2_away_draws <- c()
# sp2_home_loss <- c()
# sp2_away_loss <- c()
#
#
#
# for (i_sp2_wins in 1:length(sp2_teams))
# {
#
#   sp2_home_wins[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "H",])
#   sp2_away_wins[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "A",])
#   sp2_home_draws[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "D",])
#   sp2_away_draws[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "D",])
#   sp2_home_loss[i_sp2_wins] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "A",])
#   sp2_away_loss[i_sp2_wins] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_wins] & SP2$FTR == "H",])
#
# }
#
# sp2_total_wins <- sp2_home_wins + sp2_away_wins
# sp2_total_draws <- sp2_home_draws + sp2_away_draws
# sp2_total_loss <- sp2_home_loss + sp2_away_loss
#
# sp2_league_table <- cbind(sp2_teams,sp2_games_played,sp2_total_wins,sp2_total_draws,sp2_total_loss)
# sp2_GS <- sp2_scoring$TGS
# sp2_GC <-sp2_conceding$TGC
# sp2_GD <- sp2_scoring$TGS - sp2_conceding$TGC
# sp2_PTS <- (sp2_total_wins*3) + (sp2_total_draws*1)
# sp2_league_table <- cbind(sp2_league_table,sp2_GS,sp2_GC,sp2_GD,sp2_PTS)
# sp2_league_table <- as.data.frame(sp2_league_table)
# #rename the columns
# names(sp2_league_table)[names(sp2_league_table) == "sp2_teams"] <- "Team"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_games_played"] <- "P"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_total_wins"] <- "W"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_total_draws"] <- "D"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_total_loss"] <- "L"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_GS"] <- "F"
# names(sp2_league_table)[names(sp2_league_table) == "sp2_GC"] <- "A"
# points_sp2 <- sp2_league_table[order(as.numeric(sp2_league_table$sp2_PTS), decreasing = TRUE),]
# points_sp2$sp2_rank <- 1:length(sp2_teams)
# row.names(points_sp2) <- points_sp2$sp2_rank
# #create final_sp2_hf_against with team ranks in brackets
# for(sp2_rowhrank in 1:nrow(sp2_form_team_against_h)) {
#   for(sp2_colhrank in 1:ncol(sp2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!sp2_form_team_against_h[sp2_rowhrank,sp2_colhrank]=="",sp2_form_team_against_h[sp2_rowhrank,sp2_colhrank] <- paste(sp2_form_team_against_h[sp2_rowhrank,sp2_colhrank],"(",points_sp2$sp2_rank[points_sp2$Team ==sp2_form_team_against_h[sp2_rowhrank,sp2_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }
#
# #T1
# t1_teams
# t1_games_played
# #hwins and away wins
# t1_home_wins <- c()
# t1_away_wins <- c()
# t1_home_draws <- c()
# t1_away_draws <- c()
# t1_home_loss <- c()
# t1_away_loss <- c()
#
#
#
# for (i_t1_wins in 1:length(t1_teams))
# {
#
#   t1_home_wins[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "H",])
#   t1_away_wins[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "A",])
#   t1_home_draws[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "D",])
#   t1_away_draws[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "D",])
#   t1_home_loss[i_t1_wins] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_wins] & T1$FTR == "A",])
#   t1_away_loss[i_t1_wins] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_wins] & T1$FTR == "H",])
#
# }
#
# t1_total_wins <- t1_home_wins + t1_away_wins
# t1_total_draws <- t1_home_draws + t1_away_draws
# t1_total_loss <- t1_home_loss + t1_away_loss
#
# t1_league_table <- cbind(t1_teams,t1_games_played,t1_total_wins,t1_total_draws,t1_total_loss)
# t1_GS <- t1_scoring$TGS
# t1_GC <-t1_conceding$TGC
# t1_GD <- t1_scoring$TGS - t1_conceding$TGC
# t1_PTS <- (t1_total_wins*3) + (t1_total_draws*1)
# t1_league_table <- cbind(t1_league_table,t1_GS,t1_GC,t1_GD,t1_PTS)
# t1_league_table <- as.data.frame(t1_league_table)
# #rename the columns
# names(t1_league_table)[names(t1_league_table) == "t1_teams"] <- "Team"
# names(t1_league_table)[names(t1_league_table) == "t1_games_played"] <- "P"
# names(t1_league_table)[names(t1_league_table) == "t1_total_wins"] <- "W"
# names(t1_league_table)[names(t1_league_table) == "t1_total_draws"] <- "D"
# names(t1_league_table)[names(t1_league_table) == "t1_total_loss"] <- "L"
# names(t1_league_table)[names(t1_league_table) == "t1_GS"] <- "F"
# names(t1_league_table)[names(t1_league_table) == "t1_GC"] <- "A"
# points_t1 <- t1_league_table[order(as.numeric(t1_league_table$t1_PTS), decreasing = TRUE),]
# points_t1$t1_rank <- 1:length(t1_teams)
# row.names(points_t1) <- points_t1$t1_rank
# #create final_t1_hf_against with team ranks in brackets
# for(t1_rowhrank in 1:nrow(t1_form_team_against_h)) {
#   for(t1_colhrank in 1:ncol(t1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#
#     ifelse(!t1_form_team_against_h[t1_rowhrank,t1_colhrank]=="",t1_form_team_against_h[t1_rowhrank,t1_colhrank] <- paste(t1_form_team_against_h[t1_rowhrank,t1_colhrank],"(",points_t1$t1_rank[points_t1$Team ==t1_form_team_against_h[t1_rowhrank,t1_colhrank]],")",sep = ""),next)
#     #print(my_matrix[row, col])
#
#
#   }
# }

# #write out the data to excel
# write.xlsx(points_mls,'LeagueTable.xlsx',sheetName = "MLS")
# write.xlsx(points_arg,'LeagueTable.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(points_d2,'LeagueTable.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(points_e0,'LeagueTable.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(points_e1,'LeagueTable.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(points_e2,'LeagueTable.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(points_e3,'LeagueTable.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(points_ec,'LeagueTable.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(points_f1,'LeagueTable.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(points_f2,'LeagueTable.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(points_g1,'LeagueTable.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(points_i1,'LeagueTable.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(points_i2,'LeagueTable.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(points_n1,'LeagueTable.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(points_p1,'LeagueTable.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(points_sc0,'LeagueTable.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(points_sc1,'LeagueTable.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(points_sc2,'LeagueTable.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(points_sc3,'LeagueTable.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(points_sp1,'LeagueTable.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(points_sp2,'LeagueTable.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(points_t1,'LeagueTable.xlsx',sheetName = "T1", append = TRUE)

