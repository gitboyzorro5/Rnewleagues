library('plyr')
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
options(java.parameters = "-Xmx4g")
unlink('Divisions/Simulations.xlsx')
#simulation of league tables
##################################################################################################################
###################################################################################################################
#MLS
MLS_sim <- MLS
MLS_sim$matchid <- paste(MLS_sim$HomeTeam,MLS_sim$AwayTeam,sep = "-")
MLS_fixtures$matchid <- paste(MLS_fixtures$HomeTeam_mls,MLS_fixtures$AwayTeam_mls,sep = "-")
MLS_fixtures$mls_FTR <- sapply(MLS_fixtures$mls_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

MLS_fixtures$mls_gamestatus <- ifelse(MLS_fixtures$matchid %in% MLS_sim$matchid,"played","notplayed")

mls_home_wins_sim <- c()
mls_away_wins_sim <- c()
mls_home_draws_sim <- c()
mls_away_draws_sim <- c()
mls_home_loss_sim <- c()
mls_away_loss_sim <- c()



for (i_mls_wins_sim in 1:length(mls_teams))
{

  mls_home_wins_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "H" & MLS_fixtures$mls_gamestatus =="notplayed",])
  mls_away_wins_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "A" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_home_draws_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "D" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_draws_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "D" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_home_loss_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "A" & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_loss_sim[i_mls_wins_sim] <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_wins_sim] & MLS_fixtures$mls_FTR == "H" & MLS_fixtures$mls_gamestatus == "notplayed", ])

}

mls_total_wins_sim <- mls_home_wins_sim + mls_away_wins_sim
mls_total_draws_sim <- mls_home_draws_sim + mls_away_draws_sim
mls_total_loss_sim <- mls_home_loss_sim + mls_away_loss_sim

mls_home_games_sim <- c()
mls_away_games_sim <-c()

for (i_mls_sim in 1:length(mls_teams))
{

  mls_home_games_sim[i_mls_sim] <- nrow(MLS_fixtures[MLS_fixtures$HomeTeam_mls == mls_teams[i_mls_sim] & MLS_fixtures$mls_gamestatus == "notplayed",])
  mls_away_games_sim[i_mls_sim]  <- nrow(MLS_fixtures[MLS_fixtures$AwayTeam_mls == mls_teams[i_mls_sim] & MLS_fixtures$mls_gamestatus == "notplayed",])

}

mls_games_played_sim <- mls_home_games_sim + mls_away_games_sim

mls_league_table_sim <- cbind(mls_teams,mls_games_played_sim,mls_total_wins_sim,mls_total_draws_sim,mls_total_loss_sim)
mls_PTS_sim <- (mls_total_wins_sim*3) + (mls_total_draws_sim*1)
mls_league_table_sim <- cbind(mls_league_table_sim,mls_PTS_sim)

mls_games_played_simfinal <- mls_games_played + mls_games_played_sim
mls_total_wins_simfinal <- mls_total_wins + mls_total_wins_sim
mls_total_draws_simfinal <- mls_total_draws + mls_total_draws_sim
mls_total_loss_simfinal <- mls_total_loss + mls_total_loss_sim
mls_PTS_simfinal <- mls_PTS + mls_PTS_sim

mls_league_table_simfinal <- cbind(mls_teams,mls_games_played_simfinal,mls_total_wins_simfinal,mls_total_draws_simfinal,mls_total_loss_simfinal,mls_PTS_simfinal)
mls_league_table_simfinal <- as.data.frame(mls_league_table_simfinal)
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_teams"] <- "Team_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_games_played_simfinal"] <- "P_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_wins_simfinal"] <- "W_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_draws_simfinal"] <- "D_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_total_loss_simfinal"] <- "L_f"
names(mls_league_table_simfinal)[names(mls_league_table_simfinal) == "mls_PTS_simfinal"] <- "PTS_f"
points_mls_sim <-  mls_league_table_simfinal[order(as.numeric(mls_league_table_simfinal$PTS_f), decreasing = TRUE),]

MLS_notplayed <- MLS_fixtures[MLS_fixtures$mls_gamestatus == "notplayed",]

write.xlsx(points_mls,'Divisions/Simulations.xlsx', sheetName = "MLS_Table",append = TRUE)
write.xlsx(mls_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "MLS_sim",append = TRUE)
write.xlsx(points_mls_sim,'Divisions/Simulations.xlsx', sheetName = "MLS_simfinal",append = TRUE)
#write.xlsx(MLS_notplayed,'Divisions/Simulations.xlsx', sheetName = "MLS_notplayed",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################
#ARG
ARG_sim <- ARG
ARG_sim$matchid <- paste(ARG_sim$HomeTeam,ARG_sim$AwayTeam,sep = "-")
ARG_fixtures$matchid <- paste(ARG_fixtures$HomeTeam_arg,ARG_fixtures$AwayTeam_arg,sep = "-")
ARG_fixtures$arg_FTR <- sapply(ARG_fixtures$arg_pscore,switch,
                             '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
                             '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
                             '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')

ARG_fixtures$arg_gamestatus <- ifelse(ARG_fixtures$matchid %in% ARG_sim$matchid,"played","notplayed")

arg_home_wins_sim <- c()
arg_away_wins_sim <- c()
arg_home_draws_sim <- c()
arg_away_draws_sim <- c()
arg_home_loss_sim <- c()
arg_away_loss_sim <- c()



for (i_arg_wins_sim in 1:length(arg_teams))
{

  arg_home_wins_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "H" & ARG_fixtures$arg_gamestatus =="notplayed",])
  arg_away_wins_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "A" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_home_draws_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "D" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_draws_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "D" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_home_loss_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "A" & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_loss_sim[i_arg_wins_sim] <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_wins_sim] & ARG_fixtures$arg_FTR == "H" & ARG_fixtures$arg_gamestatus == "notplayed", ])

}

arg_total_wins_sim <- arg_home_wins_sim + arg_away_wins_sim
arg_total_draws_sim <- arg_home_draws_sim + arg_away_draws_sim
arg_total_loss_sim <- arg_home_loss_sim + arg_away_loss_sim

arg_home_games_sim <- c()
arg_away_games_sim <-c()

for (i_arg_sim in 1:length(arg_teams))
{

  arg_home_games_sim[i_arg_sim] <- nrow(ARG_fixtures[ARG_fixtures$HomeTeam_arg == arg_teams[i_arg_sim] & ARG_fixtures$arg_gamestatus == "notplayed",])
  arg_away_games_sim[i_arg_sim]  <- nrow(ARG_fixtures[ARG_fixtures$AwayTeam_arg == arg_teams[i_arg_sim] & ARG_fixtures$arg_gamestatus == "notplayed",])

}

arg_games_played_sim <- arg_home_games_sim + arg_away_games_sim

arg_league_table_sim <- cbind(arg_teams,arg_games_played_sim,arg_total_wins_sim,arg_total_draws_sim,arg_total_loss_sim)
arg_PTS_sim <- (arg_total_wins_sim*3) + (arg_total_draws_sim*1)
arg_league_table_sim <- cbind(arg_league_table_sim,arg_PTS_sim)

arg_games_played_simfinal <- arg_games_played + arg_games_played_sim
arg_total_wins_simfinal <- arg_total_wins + arg_total_wins_sim
arg_total_draws_simfinal <- arg_total_draws + arg_total_draws_sim
arg_total_loss_simfinal <- arg_total_loss + arg_total_loss_sim
arg_PTS_simfinal <- arg_PTS + arg_PTS_sim

arg_league_table_simfinal <- cbind(arg_teams,arg_games_played_simfinal,arg_total_wins_simfinal,arg_total_draws_simfinal,arg_total_loss_simfinal,arg_PTS_simfinal)
arg_league_table_simfinal <- as.data.frame(arg_league_table_simfinal)
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_teams"] <- "Team_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_games_played_simfinal"] <- "P_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_wins_simfinal"] <- "W_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_draws_simfinal"] <- "D_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_total_loss_simfinal"] <- "L_f"
names(arg_league_table_simfinal)[names(arg_league_table_simfinal) == "arg_PTS_simfinal"] <- "PTS_f"
points_arg_sim <-  arg_league_table_simfinal[order(as.numeric(arg_league_table_simfinal$PTS_f), decreasing = TRUE),]

ARG_notplayed <- ARG_fixtures[ARG_fixtures$arg_gamestatus == "notplayed",]

write.xlsx(points_arg,'Divisions/Simulations.xlsx', sheetName = "ARG_table",append = TRUE)
write.xlsx(arg_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "ARG_sim",append = TRUE)
write.xlsx(points_arg_sim,'Divisions/Simulations.xlsx', sheetName = "ARG_simfinal",append = TRUE)
#write.xlsx(ARG_notplayed,'Divisions/Simulations.xlsx', sheetName = "ARG_notplayed",append = TRUE)
############################################################################################################################################################
############################################################################################################################################################
#D2
# D2_sim <- D2
# D2_sim$matchid <- paste(D2_sim$HomeTeam,D2_sim$AwayTeam,sep = "-")
# D2_fixtures$matchid <- paste(D2_fixtures$HomeTeam_d2,D2_fixtures$AwayTeam_d2,sep = "-")
# D2_fixtures$d2_FTR <- sapply(D2_fixtures$d2_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# D2_fixtures$d2_gamestatus <- ifelse(D2_fixtures$matchid %in% D2_sim$matchid,"played","notplayed")
#
# d2_home_wins_sim <- c()
# d2_away_wins_sim <- c()
# d2_home_draws_sim <- c()
# d2_away_draws_sim <- c()
# d2_home_loss_sim <- c()
# d2_away_loss_sim <- c()
#
#
#
# for (i_d2_wins_sim in 1:length(d2_teams))
# {
#
#   d2_home_wins_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$HomeTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "H" & D2_fixtures$d2_gamestatus =="notplayed",])
#   d2_away_wins_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$AwayTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "A" & D2_fixtures$d2_gamestatus == "notplayed",])
#   d2_home_draws_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$HomeTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "D" & D2_fixtures$d2_gamestatus == "notplayed",])
#   d2_away_draws_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$AwayTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "D" & D2_fixtures$d2_gamestatus == "notplayed",])
#   d2_home_loss_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$HomeTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "A" & D2_fixtures$d2_gamestatus == "notplayed",])
#   d2_away_loss_sim[i_d2_wins_sim] <- nrow(D2_fixtures[D2_fixtures$AwayTeam_d2 == d2_teams[i_d2_wins_sim] & D2_fixtures$d2_FTR == "H" & D2_fixtures$d2_gamestatus == "notplayed", ])
#
# }
#
# d2_total_wins_sim <- d2_home_wins_sim + d2_away_wins_sim
# d2_total_draws_sim <- d2_home_draws_sim + d2_away_draws_sim
# d2_total_loss_sim <- d2_home_loss_sim + d2_away_loss_sim
#
# d2_home_games_sim <- c()
# d2_away_games_sim <-c()
#
# for (i_d2_sim in 1:length(d2_teams))
# {
#
#   d2_home_games_sim[i_d2_sim] <- nrow(D2_fixtures[D2_fixtures$HomeTeam_d2 == d2_teams[i_d2_sim] & D2_fixtures$d2_gamestatus == "notplayed",])
#   d2_away_games_sim[i_d2_sim]  <- nrow(D2_fixtures[D2_fixtures$AwayTeam_d2 == d2_teams[i_d2_sim] & D2_fixtures$d2_gamestatus == "notplayed",])
#
# }
#
# d2_games_played_sim <- d2_home_games_sim + d2_away_games_sim
#
# d2_league_table_sim <- cbind(d2_teams,d2_games_played_sim,d2_total_wins_sim,d2_total_draws_sim,d2_total_loss_sim)
# d2_PTS_sim <- (d2_total_wins_sim*3) + (d2_total_draws_sim*1)
# d2_league_table_sim <- cbind(d2_league_table_sim,d2_PTS_sim)
#
# d2_games_played_simfinal <- d2_games_played + d2_games_played_sim
# d2_total_wins_simfinal <- d2_total_wins + d2_total_wins_sim
# d2_total_draws_simfinal <- d2_total_draws + d2_total_draws_sim
# d2_total_loss_simfinal <- d2_total_loss + d2_total_loss_sim
# d2_PTS_simfinal <- d2_PTS + d2_PTS_sim
#
# d2_league_table_simfinal <- cbind(d2_teams,d2_games_played_simfinal,d2_total_wins_simfinal,d2_total_draws_simfinal,d2_total_loss_simfinal,d2_PTS_simfinal)
# d2_league_table_simfinal <- as.data.frame(d2_league_table_simfinal)
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_teams"] <- "Team_f"
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_games_played_simfinal"] <- "P_f"
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_total_wins_simfinal"] <- "W_f"
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_total_draws_simfinal"] <- "D_f"
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_total_loss_simfinal"] <- "L_f"
# names(d2_league_table_simfinal)[names(d2_league_table_simfinal) == "d2_PTS_simfinal"] <- "PTS_f"
# points_d2_sim <-  d2_league_table_simfinal[order(as.numeric(d2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# D2_notplayed <- D2_fixtures[D2_fixtures$d2_gamestatus == "notplayed",]
#
#
# write.xlsx(points_d2,'Divisions/Simulations.xlsx', sheetName = "D2_table",append = TRUE)
# write.xlsx(d2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "D2_sim",append = TRUE)
# write.xlsx(points_d2_sim,'Divisions/Simulations.xlsx', sheetName = "D2_simfinal",append = TRUE)
# #write.xlsx(D2_notplayed,'Divisions/Simulations.xlsx', sheetName = "D2_notplayed",append = TRUE)
# ############################################################################################################################
# ###########################################################################################################################
# #E0
# E0_sim <- E0
# E0_sim$matchid <- paste(E0_sim$HomeTeam,E0_sim$AwayTeam,sep = "-")
# E0_fixtures$matchid <- paste(E0_fixtures$HomeTeam_e0,E0_fixtures$AwayTeam_e0,sep = "-")
# E0_fixtures$e0_FTR <- sapply(E0_fixtures$e0_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# E0_fixtures$e0_gamestatus <- ifelse(E0_fixtures$matchid %in% E0_sim$matchid,"played","notplayed")
#
# e0_home_wins_sim <- c()
# e0_away_wins_sim <- c()
# e0_home_draws_sim <- c()
# e0_away_draws_sim <- c()
# e0_home_loss_sim <- c()
# e0_away_loss_sim <- c()
#
#
#
# for (i_e0_wins_sim in 1:length(e0_teams))
# {
#
#   e0_home_wins_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$HomeTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "H" & E0_fixtures$e0_gamestatus =="notplayed",])
#   e0_away_wins_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$AwayTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "A" & E0_fixtures$e0_gamestatus == "notplayed",])
#   e0_home_draws_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$HomeTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "D" & E0_fixtures$e0_gamestatus == "notplayed",])
#   e0_away_draws_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$AwayTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "D" & E0_fixtures$e0_gamestatus == "notplayed",])
#   e0_home_loss_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$HomeTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "A" & E0_fixtures$e0_gamestatus == "notplayed",])
#   e0_away_loss_sim[i_e0_wins_sim] <- nrow(E0_fixtures[E0_fixtures$AwayTeam_e0 == e0_teams[i_e0_wins_sim] & E0_fixtures$e0_FTR == "H" & E0_fixtures$e0_gamestatus == "notplayed", ])
#
# }
#
# e0_total_wins_sim <- e0_home_wins_sim + e0_away_wins_sim
# e0_total_draws_sim <- e0_home_draws_sim + e0_away_draws_sim
# e0_total_loss_sim <- e0_home_loss_sim + e0_away_loss_sim
#
# e0_home_games_sim <- c()
# e0_away_games_sim <-c()
#
# for (i_e0_sim in 1:length(e0_teams))
# {
#
#   e0_home_games_sim[i_e0_sim] <- nrow(E0_fixtures[E0_fixtures$HomeTeam_e0 == e0_teams[i_e0_sim] & E0_fixtures$e0_gamestatus == "notplayed",])
#   e0_away_games_sim[i_e0_sim]  <- nrow(E0_fixtures[E0_fixtures$AwayTeam_e0 == e0_teams[i_e0_sim] & E0_fixtures$e0_gamestatus == "notplayed",])
#
# }
#
# e0_games_played_sim <- e0_home_games_sim + e0_away_games_sim
#
# e0_league_table_sim <- cbind(e0_teams,e0_games_played_sim,e0_total_wins_sim,e0_total_draws_sim,e0_total_loss_sim)
# e0_PTS_sim <- (e0_total_wins_sim*3) + (e0_total_draws_sim*1)
# e0_league_table_sim <- cbind(e0_league_table_sim,e0_PTS_sim)
#
# e0_games_played_simfinal <- e0_games_played + e0_games_played_sim
# e0_total_wins_simfinal <- e0_total_wins + e0_total_wins_sim
# e0_total_draws_simfinal <- e0_total_draws + e0_total_draws_sim
# e0_total_loss_simfinal <- e0_total_loss + e0_total_loss_sim
# e0_PTS_simfinal <- e0_PTS + e0_PTS_sim
#
# e0_league_table_simfinal <- cbind(e0_teams,e0_games_played_simfinal,e0_total_wins_simfinal,e0_total_draws_simfinal,e0_total_loss_simfinal,e0_PTS_simfinal)
# e0_league_table_simfinal <- as.data.frame(e0_league_table_simfinal)
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_teams"] <- "Team_f"
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_games_played_simfinal"] <- "P_f"
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_total_wins_simfinal"] <- "W_f"
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_total_draws_simfinal"] <- "D_f"
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_total_loss_simfinal"] <- "L_f"
# names(e0_league_table_simfinal)[names(e0_league_table_simfinal) == "e0_PTS_simfinal"] <- "PTS_f"
# points_e0_sim <-  e0_league_table_simfinal[order(as.numeric(e0_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# E0_notplayed <- E0_fixtures[E0_fixtures$e0_gamestatus == "notplayed",]
#
# write.xlsx(points_e0,'Divisions/Simulations.xlsx', sheetName = "E0_table",append = TRUE)
# write.xlsx(e0_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "E0_sim",append = TRUE)
# write.xlsx(points_e0_sim,'Divisions/Simulations.xlsx', sheetName = "E0_simfinal",append = TRUE)
# #write.xlsx(E0_notplayed,'Divisions/Simulations.xlsx', sheetName = "E0_notplayed",append = TRUE)
# #########################################################################################################################################
# #########################################################################################################################################
# ###################################################################################################################
# #E1
# E1_sim <- E1
# E1_sim$matchid <- paste(E1_sim$HomeTeam,E1_sim$AwayTeam,sep = "-")
# E1_fixtures$matchid <- paste(E1_fixtures$HomeTeam_e1,E1_fixtures$AwayTeam_e1,sep = "-")
# E1_fixtures$e1_FTR <- sapply(E1_fixtures$e1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# E1_fixtures$e1_gamestatus <- ifelse(E1_fixtures$matchid %in% E1_sim$matchid,"played","notplayed")
#
# e1_home_wins_sim <- c()
# e1_away_wins_sim <- c()
# e1_home_draws_sim <- c()
# e1_away_draws_sim <- c()
# e1_home_loss_sim <- c()
# e1_away_loss_sim <- c()
#
#
#
# for (i_e1_wins_sim in 1:length(e1_teams))
# {
#
#   e1_home_wins_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$HomeTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "H" & E1_fixtures$e1_gamestatus =="notplayed",])
#   e1_away_wins_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$AwayTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "A" & E1_fixtures$e1_gamestatus == "notplayed",])
#   e1_home_draws_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$HomeTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "D" & E1_fixtures$e1_gamestatus == "notplayed",])
#   e1_away_draws_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$AwayTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "D" & E1_fixtures$e1_gamestatus == "notplayed",])
#   e1_home_loss_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$HomeTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "A" & E1_fixtures$e1_gamestatus == "notplayed",])
#   e1_away_loss_sim[i_e1_wins_sim] <- nrow(E1_fixtures[E1_fixtures$AwayTeam_e1 == e1_teams[i_e1_wins_sim] & E1_fixtures$e1_FTR == "H" & E1_fixtures$e1_gamestatus == "notplayed", ])
#
# }
#
# e1_total_wins_sim <- e1_home_wins_sim + e1_away_wins_sim
# e1_total_draws_sim <- e1_home_draws_sim + e1_away_draws_sim
# e1_total_loss_sim <- e1_home_loss_sim + e1_away_loss_sim
#
# e1_home_games_sim <- c()
# e1_away_games_sim <-c()
#
# for (i_e1_sim in 1:length(e1_teams))
# {
#
#   e1_home_games_sim[i_e1_sim] <- nrow(E1_fixtures[E1_fixtures$HomeTeam_e1 == e1_teams[i_e1_sim] & E1_fixtures$e1_gamestatus == "notplayed",])
#   e1_away_games_sim[i_e1_sim]  <- nrow(E1_fixtures[E1_fixtures$AwayTeam_e1 == e1_teams[i_e1_sim] & E1_fixtures$e1_gamestatus == "notplayed",])
#
# }
#
# e1_games_played_sim <- e1_home_games_sim + e1_away_games_sim
#
# e1_league_table_sim <- cbind(e1_teams,e1_games_played_sim,e1_total_wins_sim,e1_total_draws_sim,e1_total_loss_sim)
# e1_PTS_sim <- (e1_total_wins_sim*3) + (e1_total_draws_sim*1)
# e1_league_table_sim <- cbind(e1_league_table_sim,e1_PTS_sim)
#
# e1_games_played_simfinal <- e1_games_played + e1_games_played_sim
# e1_total_wins_simfinal <- e1_total_wins + e1_total_wins_sim
# e1_total_draws_simfinal <- e1_total_draws + e1_total_draws_sim
# e1_total_loss_simfinal <- e1_total_loss + e1_total_loss_sim
# e1_PTS_simfinal <- e1_PTS + e1_PTS_sim
#
# e1_league_table_simfinal <- cbind(e1_teams,e1_games_played_simfinal,e1_total_wins_simfinal,e1_total_draws_simfinal,e1_total_loss_simfinal,e1_PTS_simfinal)
# e1_league_table_simfinal <- as.data.frame(e1_league_table_simfinal)
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_teams"] <- "Team_f"
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_games_played_simfinal"] <- "P_f"
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_total_wins_simfinal"] <- "W_f"
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_total_draws_simfinal"] <- "D_f"
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_total_loss_simfinal"] <- "L_f"
# names(e1_league_table_simfinal)[names(e1_league_table_simfinal) == "e1_PTS_simfinal"] <- "PTS_f"
# points_e1_sim <-  e1_league_table_simfinal[order(as.numeric(e1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# E1_notplayed <- E1_fixtures[E1_fixtures$e1_gamestatus == "notplayed",]
#
#
# write.xlsx(points_e1,'Divisions/Simulations.xlsx', sheetName = "E1_table",append = TRUE)
# write.xlsx(e1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "E1_sim",append = TRUE)
# write.xlsx(points_e1_sim,'Divisions/Simulations.xlsx', sheetName = "E1_simfinal",append = TRUE)
# #write.xlsx(E1_notplayed,'Divisions/Simulations.xlsx', sheetName = "E1_notplayed",append = TRUE)
# ###############################################################################################################################
# ###############################################################################################################################
# #E2
# E2_sim <- E2
# E2_sim$matchid <- paste(E2_sim$HomeTeam,E2_sim$AwayTeam,sep = "-")
# E2_fixtures$matchid <- paste(E2_fixtures$HomeTeam_e2,E2_fixtures$AwayTeam_e2,sep = "-")
# E2_fixtures$e2_FTR <- sapply(E2_fixtures$e2_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# E2_fixtures$e2_gamestatus <- ifelse(E2_fixtures$matchid %in% E2_sim$matchid,"played","notplayed")
#
# e2_home_wins_sim <- c()
# e2_away_wins_sim <- c()
# e2_home_draws_sim <- c()
# e2_away_draws_sim <- c()
# e2_home_loss_sim <- c()
# e2_away_loss_sim <- c()
#
#
#
# for (i_e2_wins_sim in 1:length(e2_teams))
# {
#
#   e2_home_wins_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$HomeTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "H" & E2_fixtures$e2_gamestatus =="notplayed",])
#   e2_away_wins_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$AwayTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "A" & E2_fixtures$e2_gamestatus == "notplayed",])
#   e2_home_draws_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$HomeTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "D" & E2_fixtures$e2_gamestatus == "notplayed",])
#   e2_away_draws_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$AwayTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "D" & E2_fixtures$e2_gamestatus == "notplayed",])
#   e2_home_loss_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$HomeTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "A" & E2_fixtures$e2_gamestatus == "notplayed",])
#   e2_away_loss_sim[i_e2_wins_sim] <- nrow(E2_fixtures[E2_fixtures$AwayTeam_e2 == e2_teams[i_e2_wins_sim] & E2_fixtures$e2_FTR == "H" & E2_fixtures$e2_gamestatus == "notplayed", ])
#
# }
#
# e2_total_wins_sim <- e2_home_wins_sim + e2_away_wins_sim
# e2_total_draws_sim <- e2_home_draws_sim + e2_away_draws_sim
# e2_total_loss_sim <- e2_home_loss_sim + e2_away_loss_sim
#
# e2_home_games_sim <- c()
# e2_away_games_sim <-c()
#
# for (i_e2_sim in 1:length(e2_teams))
# {
#
#   e2_home_games_sim[i_e2_sim] <- nrow(E2_fixtures[E2_fixtures$HomeTeam_e2 == e2_teams[i_e2_sim] & E2_fixtures$e2_gamestatus == "notplayed",])
#   e2_away_games_sim[i_e2_sim]  <- nrow(E2_fixtures[E2_fixtures$AwayTeam_e2 == e2_teams[i_e2_sim] & E2_fixtures$e2_gamestatus == "notplayed",])
#
# }
#
# e2_games_played_sim <- e2_home_games_sim + e2_away_games_sim
#
# e2_league_table_sim <- cbind(e2_teams,e2_games_played_sim,e2_total_wins_sim,e2_total_draws_sim,e2_total_loss_sim)
# e2_PTS_sim <- (e2_total_wins_sim*3) + (e2_total_draws_sim*1)
# e2_league_table_sim <- cbind(e2_league_table_sim,e2_PTS_sim)
#
# e2_games_played_simfinal <- e2_games_played + e2_games_played_sim
# e2_total_wins_simfinal <- e2_total_wins + e2_total_wins_sim
# e2_total_draws_simfinal <- e2_total_draws + e2_total_draws_sim
# e2_total_loss_simfinal <- e2_total_loss + e2_total_loss_sim
# e2_PTS_simfinal <- e2_PTS + e2_PTS_sim
#
# e2_league_table_simfinal <- cbind(e2_teams,e2_games_played_simfinal,e2_total_wins_simfinal,e2_total_draws_simfinal,e2_total_loss_simfinal,e2_PTS_simfinal)
# e2_league_table_simfinal <- as.data.frame(e2_league_table_simfinal)
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_teams"] <- "Team_f"
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_games_played_simfinal"] <- "P_f"
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_total_wins_simfinal"] <- "W_f"
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_total_draws_simfinal"] <- "D_f"
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_total_loss_simfinal"] <- "L_f"
# names(e2_league_table_simfinal)[names(e2_league_table_simfinal) == "e2_PTS_simfinal"] <- "PTS_f"
# points_e2_sim <-  e2_league_table_simfinal[order(as.numeric(e2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# E2_notplayed <- E2_fixtures[E2_fixtures$e2_gamestatus == "notplayed",]
#
# write.xlsx(points_e2,'Divisions/Simulations.xlsx', sheetName = "E2_table",append = TRUE)
# write.xlsx(e2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "E2_sim",append = TRUE)
# write.xlsx(points_e2_sim,'Divisions/Simulations.xlsx', sheetName = "E2_simfinal",append = TRUE)
# #write.xlsx(E2_notplayed,'Divisions/Simulations.xlsx', sheetName = "E2_notplayed",append = TRUE)
#
# ###################################################################################################################################
# ###################################################################################################################
# #E3
# E3_sim <- E3
# E3_sim$matchid <- paste(E3_sim$HomeTeam,E3_sim$AwayTeam,sep = "-")
# E3_fixtures$matchid <- paste(E3_fixtures$HomeTeam_e3,E3_fixtures$AwayTeam_e3,sep = "-")
# E3_fixtures$e3_FTR <- sapply(E3_fixtures$e3_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# E3_fixtures$e3_gamestatus <- ifelse(E3_fixtures$matchid %in% E3_sim$matchid,"played","notplayed")
#
# e3_home_wins_sim <- c()
# e3_away_wins_sim <- c()
# e3_home_draws_sim <- c()
# e3_away_draws_sim <- c()
# e3_home_loss_sim <- c()
# e3_away_loss_sim <- c()
#
#
#
# for (i_e3_wins_sim in 1:length(e3_teams))
# {
#
#   e3_home_wins_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$HomeTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "H" & E3_fixtures$e3_gamestatus =="notplayed",])
#   e3_away_wins_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$AwayTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "A" & E3_fixtures$e3_gamestatus == "notplayed",])
#   e3_home_draws_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$HomeTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "D" & E3_fixtures$e3_gamestatus == "notplayed",])
#   e3_away_draws_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$AwayTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "D" & E3_fixtures$e3_gamestatus == "notplayed",])
#   e3_home_loss_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$HomeTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "A" & E3_fixtures$e3_gamestatus == "notplayed",])
#   e3_away_loss_sim[i_e3_wins_sim] <- nrow(E3_fixtures[E3_fixtures$AwayTeam_e3 == e3_teams[i_e3_wins_sim] & E3_fixtures$e3_FTR == "H" & E3_fixtures$e3_gamestatus == "notplayed", ])
#
# }
#
# e3_total_wins_sim <- e3_home_wins_sim + e3_away_wins_sim
# e3_total_draws_sim <- e3_home_draws_sim + e3_away_draws_sim
# e3_total_loss_sim <- e3_home_loss_sim + e3_away_loss_sim
#
# e3_home_games_sim <- c()
# e3_away_games_sim <-c()
#
# for (i_e3_sim in 1:length(e3_teams))
# {
#
#   e3_home_games_sim[i_e3_sim] <- nrow(E3_fixtures[E3_fixtures$HomeTeam_e3 == e3_teams[i_e3_sim] & E3_fixtures$e3_gamestatus == "notplayed",])
#   e3_away_games_sim[i_e3_sim]  <- nrow(E3_fixtures[E3_fixtures$AwayTeam_e3 == e3_teams[i_e3_sim] & E3_fixtures$e3_gamestatus == "notplayed",])
#
# }
#
# e3_games_played_sim <- e3_home_games_sim + e3_away_games_sim
#
# e3_league_table_sim <- cbind(e3_teams,e3_games_played_sim,e3_total_wins_sim,e3_total_draws_sim,e3_total_loss_sim)
# e3_PTS_sim <- (e3_total_wins_sim*3) + (e3_total_draws_sim*1)
# e3_league_table_sim <- cbind(e3_league_table_sim,e3_PTS_sim)
#
# e3_games_played_simfinal <- e3_games_played + e3_games_played_sim
# e3_total_wins_simfinal <- e3_total_wins + e3_total_wins_sim
# e3_total_draws_simfinal <- e3_total_draws + e3_total_draws_sim
# e3_total_loss_simfinal <- e3_total_loss + e3_total_loss_sim
# e3_PTS_simfinal <- e3_PTS + e3_PTS_sim
#
# e3_league_table_simfinal <- cbind(e3_teams,e3_games_played_simfinal,e3_total_wins_simfinal,e3_total_draws_simfinal,e3_total_loss_simfinal,e3_PTS_simfinal)
# e3_league_table_simfinal <- as.data.frame(e3_league_table_simfinal)
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_teams"] <- "Team_f"
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_games_played_simfinal"] <- "P_f"
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_total_wins_simfinal"] <- "W_f"
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_total_draws_simfinal"] <- "D_f"
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_total_loss_simfinal"] <- "L_f"
# names(e3_league_table_simfinal)[names(e3_league_table_simfinal) == "e3_PTS_simfinal"] <- "PTS_f"
# points_e3_sim <-  e3_league_table_simfinal[order(as.numeric(e3_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# E3_notplayed <- E3_fixtures[E3_fixtures$e3_gamestatus == "notplayed",]
#
# write.xlsx(points_e3,'Divisions/Simulations.xlsx', sheetName = "E3_table",append = TRUE)
# write.xlsx(e3_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "E3_sim",append = TRUE)
# write.xlsx(points_e3_sim,'Divisions/Simulations.xlsx', sheetName = "E3_simfinal",append = TRUE)
# #write.xlsx(E3_notplayed,'Divisions/Simulations.xlsx', sheetName = "E3_notplayed",append = TRUE)
# ###############################################################################################################################
# ###############################################################################################################################
# #EC
# EC_sim <- EC
# EC_sim$matchid <- paste(EC_sim$HomeTeam,EC_sim$AwayTeam,sep = "-")
# EC_fixtures$matchid <- paste(EC_fixtures$HomeTeam_ec,EC_fixtures$AwayTeam_ec,sep = "-")
# EC_fixtures$ec_FTR <- sapply(EC_fixtures$ec_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# EC_fixtures$ec_gamestatus <- ifelse(EC_fixtures$matchid %in% EC_sim$matchid,"played","notplayed")
#
# ec_home_wins_sim <- c()
# ec_away_wins_sim <- c()
# ec_home_draws_sim <- c()
# ec_away_draws_sim <- c()
# ec_home_loss_sim <- c()
# ec_away_loss_sim <- c()
#
#
#
# for (i_ec_wins_sim in 1:length(ec_teams))
# {
#
#   ec_home_wins_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$HomeTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "H" & EC_fixtures$ec_gamestatus =="notplayed",])
#   ec_away_wins_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$AwayTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "A" & EC_fixtures$ec_gamestatus == "notplayed",])
#   ec_home_draws_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$HomeTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "D" & EC_fixtures$ec_gamestatus == "notplayed",])
#   ec_away_draws_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$AwayTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "D" & EC_fixtures$ec_gamestatus == "notplayed",])
#   ec_home_loss_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$HomeTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "A" & EC_fixtures$ec_gamestatus == "notplayed",])
#   ec_away_loss_sim[i_ec_wins_sim] <- nrow(EC_fixtures[EC_fixtures$AwayTeam_ec == ec_teams[i_ec_wins_sim] & EC_fixtures$ec_FTR == "H" & EC_fixtures$ec_gamestatus == "notplayed", ])
#
# }
#
# ec_total_wins_sim <- ec_home_wins_sim + ec_away_wins_sim
# ec_total_draws_sim <- ec_home_draws_sim + ec_away_draws_sim
# ec_total_loss_sim <- ec_home_loss_sim + ec_away_loss_sim
#
# ec_home_games_sim <- c()
# ec_away_games_sim <-c()
#
# for (i_ec_sim in 1:length(ec_teams))
# {
#
#   ec_home_games_sim[i_ec_sim] <- nrow(EC_fixtures[EC_fixtures$HomeTeam_ec == ec_teams[i_ec_sim] & EC_fixtures$ec_gamestatus == "notplayed",])
#   ec_away_games_sim[i_ec_sim]  <- nrow(EC_fixtures[EC_fixtures$AwayTeam_ec == ec_teams[i_ec_sim] & EC_fixtures$ec_gamestatus == "notplayed",])
#
# }
#
# ec_games_played_sim <- ec_home_games_sim + ec_away_games_sim
#
# ec_league_table_sim <- cbind(ec_teams,ec_games_played_sim,ec_total_wins_sim,ec_total_draws_sim,ec_total_loss_sim)
# ec_PTS_sim <- (ec_total_wins_sim*3) + (ec_total_draws_sim*1)
# ec_league_table_sim <- cbind(ec_league_table_sim,ec_PTS_sim)
#
# ec_games_played_simfinal <- ec_games_played + ec_games_played_sim
# ec_total_wins_simfinal <- ec_total_wins + ec_total_wins_sim
# ec_total_draws_simfinal <- ec_total_draws + ec_total_draws_sim
# ec_total_loss_simfinal <- ec_total_loss + ec_total_loss_sim
# ec_PTS_simfinal <- ec_PTS + ec_PTS_sim
#
# ec_league_table_simfinal <- cbind(ec_teams,ec_games_played_simfinal,ec_total_wins_simfinal,ec_total_draws_simfinal,ec_total_loss_simfinal,ec_PTS_simfinal)
# ec_league_table_simfinal <- as.data.frame(ec_league_table_simfinal)
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_teams"] <- "Team_f"
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_games_played_simfinal"] <- "P_f"
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_total_wins_simfinal"] <- "W_f"
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_total_draws_simfinal"] <- "D_f"
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_total_loss_simfinal"] <- "L_f"
# names(ec_league_table_simfinal)[names(ec_league_table_simfinal) == "ec_PTS_simfinal"] <- "PTS_f"
# points_ec_sim <-  ec_league_table_simfinal[order(as.numeric(ec_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# EC_notplayed <- EC_fixtures[EC_fixtures$ec_gamestatus == "notplayed",]
#
# write.xlsx(points_ec,'Divisions/Simulations.xlsx', sheetName = "EC_table",append = TRUE)
# write.xlsx(ec_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "EC_sim",append = TRUE)
# write.xlsx(points_ec_sim,'Divisions/Simulations.xlsx', sheetName = "EC_simfinal",append = TRUE)
# #write.xlsx(EC_notplayed,'Divisions/Simulations.xlsx', sheetName = "EC_notplayed",append = TRUE)
# ##############################################################################################################################
# ##############################################################################################################################
# #F1
# F1_sim <- F1
# F1_sim$matchid <- paste(F1_sim$HomeTeam,F1_sim$AwayTeam,sep = "-")
# F1_fixtures$matchid <- paste(F1_fixtures$HomeTeam_f1,F1_fixtures$AwayTeam_f1,sep = "-")
# F1_fixtures$f1_FTR <- sapply(F1_fixtures$f1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# F1_fixtures$f1_gamestatus <- ifelse(F1_fixtures$matchid %in% F1_sim$matchid,"played","notplayed")
#
# f1_home_wins_sim <- c()
# f1_away_wins_sim <- c()
# f1_home_draws_sim <- c()
# f1_away_draws_sim <- c()
# f1_home_loss_sim <- c()
# f1_away_loss_sim <- c()
#
#
#
# for (i_f1_wins_sim in 1:length(f1_teams))
# {
#
#   f1_home_wins_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$HomeTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "H" & F1_fixtures$f1_gamestatus =="notplayed",])
#   f1_away_wins_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$AwayTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "A" & F1_fixtures$f1_gamestatus == "notplayed",])
#   f1_home_draws_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$HomeTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "D" & F1_fixtures$f1_gamestatus == "notplayed",])
#   f1_away_draws_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$AwayTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "D" & F1_fixtures$f1_gamestatus == "notplayed",])
#   f1_home_loss_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$HomeTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "A" & F1_fixtures$f1_gamestatus == "notplayed",])
#   f1_away_loss_sim[i_f1_wins_sim] <- nrow(F1_fixtures[F1_fixtures$AwayTeam_f1 == f1_teams[i_f1_wins_sim] & F1_fixtures$f1_FTR == "H" & F1_fixtures$f1_gamestatus == "notplayed", ])
#
# }
#
# f1_total_wins_sim <- f1_home_wins_sim + f1_away_wins_sim
# f1_total_draws_sim <- f1_home_draws_sim + f1_away_draws_sim
# f1_total_loss_sim <- f1_home_loss_sim + f1_away_loss_sim
#
# f1_home_games_sim <- c()
# f1_away_games_sim <-c()
#
# for (i_f1_sim in 1:length(f1_teams))
# {
#
#   f1_home_games_sim[i_f1_sim] <- nrow(F1_fixtures[F1_fixtures$HomeTeam_f1 == f1_teams[i_f1_sim] & F1_fixtures$f1_gamestatus == "notplayed",])
#   f1_away_games_sim[i_f1_sim]  <- nrow(F1_fixtures[F1_fixtures$AwayTeam_f1 == f1_teams[i_f1_sim] & F1_fixtures$f1_gamestatus == "notplayed",])
#
# }
#
# f1_games_played_sim <- f1_home_games_sim + f1_away_games_sim
#
# f1_league_table_sim <- cbind(f1_teams,f1_games_played_sim,f1_total_wins_sim,f1_total_draws_sim,f1_total_loss_sim)
# f1_PTS_sim <- (f1_total_wins_sim*3) + (f1_total_draws_sim*1)
# f1_league_table_sim <- cbind(f1_league_table_sim,f1_PTS_sim)
#
# f1_games_played_simfinal <- f1_games_played + f1_games_played_sim
# f1_total_wins_simfinal <- f1_total_wins + f1_total_wins_sim
# f1_total_draws_simfinal <- f1_total_draws + f1_total_draws_sim
# f1_total_loss_simfinal <- f1_total_loss + f1_total_loss_sim
# f1_PTS_simfinal <- f1_PTS + f1_PTS_sim
#
# f1_league_table_simfinal <- cbind(f1_teams,f1_games_played_simfinal,f1_total_wins_simfinal,f1_total_draws_simfinal,f1_total_loss_simfinal,f1_PTS_simfinal)
# f1_league_table_simfinal <- as.data.frame(f1_league_table_simfinal)
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_teams"] <- "Team_f"
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_games_played_simfinal"] <- "P_f"
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_total_wins_simfinal"] <- "W_f"
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_total_draws_simfinal"] <- "D_f"
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_total_loss_simfinal"] <- "L_f"
# names(f1_league_table_simfinal)[names(f1_league_table_simfinal) == "f1_PTS_simfinal"] <- "PTS_f"
# points_f1_sim <-  f1_league_table_simfinal[order(as.numeric(f1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# F1_notplayed <- F1_fixtures[F1_fixtures$f1_gamestatus == "notplayed",]
#
# write.xlsx(points_f1,'Divisions/Simulations.xlsx', sheetName = "F1_table",append = TRUE)
# write.xlsx(f1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "F1_sim",append = TRUE)
# write.xlsx(points_f1_sim,'Divisions/Simulations.xlsx', sheetName = "F1_simfinal",append = TRUE)
# #write.xlsx(F1_notplayed,'Divisions/Simulations.xlsx', sheetName = "F1_notplayed",append = TRUE)
# #################################################################################################################################
# #################################################################################################################################
# #F2
# F2_sim <- F2
# F2_sim$matchid <- paste(F2_sim$HomeTeam,F2_sim$AwayTeam,sep = "-")
# F2_fixtures$matchid <- paste(F2_fixtures$HomeTeam_f2,F2_fixtures$AwayTeam_f2,sep = "-")
# F2_fixtures$f2_FTR <- sapply(F2_fixtures$f2_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# F2_fixtures$f2_gamestatus <- ifelse(F2_fixtures$matchid %in% F2_sim$matchid,"played","notplayed")
#
# f2_home_wins_sim <- c()
# f2_away_wins_sim <- c()
# f2_home_draws_sim <- c()
# f2_away_draws_sim <- c()
# f2_home_loss_sim <- c()
# f2_away_loss_sim <- c()
#
#
#
# for (i_f2_wins_sim in 1:length(f2_teams))
# {
#
#   f2_home_wins_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$HomeTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "H" & F2_fixtures$f2_gamestatus =="notplayed",])
#   f2_away_wins_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$AwayTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "A" & F2_fixtures$f2_gamestatus == "notplayed",])
#   f2_home_draws_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$HomeTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "D" & F2_fixtures$f2_gamestatus == "notplayed",])
#   f2_away_draws_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$AwayTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "D" & F2_fixtures$f2_gamestatus == "notplayed",])
#   f2_home_loss_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$HomeTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "A" & F2_fixtures$f2_gamestatus == "notplayed",])
#   f2_away_loss_sim[i_f2_wins_sim] <- nrow(F2_fixtures[F2_fixtures$AwayTeam_f2 == f2_teams[i_f2_wins_sim] & F2_fixtures$f2_FTR == "H" & F2_fixtures$f2_gamestatus == "notplayed", ])
#
# }
#
# f2_total_wins_sim <- f2_home_wins_sim + f2_away_wins_sim
# f2_total_draws_sim <- f2_home_draws_sim + f2_away_draws_sim
# f2_total_loss_sim <- f2_home_loss_sim + f2_away_loss_sim
#
# f2_home_games_sim <- c()
# f2_away_games_sim <-c()
#
# for (i_f2_sim in 1:length(f2_teams))
# {
#
#   f2_home_games_sim[i_f2_sim] <- nrow(F2_fixtures[F2_fixtures$HomeTeam_f2 == f2_teams[i_f2_sim] & F2_fixtures$f2_gamestatus == "notplayed",])
#   f2_away_games_sim[i_f2_sim]  <- nrow(F2_fixtures[F2_fixtures$AwayTeam_f2 == f2_teams[i_f2_sim] & F2_fixtures$f2_gamestatus == "notplayed",])
#
# }
#
# f2_games_played_sim <- f2_home_games_sim + f2_away_games_sim
#
# f2_league_table_sim <- cbind(f2_teams,f2_games_played_sim,f2_total_wins_sim,f2_total_draws_sim,f2_total_loss_sim)
# f2_PTS_sim <- (f2_total_wins_sim*3) + (f2_total_draws_sim*1)
# f2_league_table_sim <- cbind(f2_league_table_sim,f2_PTS_sim)
#
# f2_games_played_simfinal <- f2_games_played + f2_games_played_sim
# f2_total_wins_simfinal <- f2_total_wins + f2_total_wins_sim
# f2_total_draws_simfinal <- f2_total_draws + f2_total_draws_sim
# f2_total_loss_simfinal <- f2_total_loss + f2_total_loss_sim
# f2_PTS_simfinal <- f2_PTS + f2_PTS_sim
#
# f2_league_table_simfinal <- cbind(f2_teams,f2_games_played_simfinal,f2_total_wins_simfinal,f2_total_draws_simfinal,f2_total_loss_simfinal,f2_PTS_simfinal)
# f2_league_table_simfinal <- as.data.frame(f2_league_table_simfinal)
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_teams"] <- "Team_f"
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_games_played_simfinal"] <- "P_f"
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_total_wins_simfinal"] <- "W_f"
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_total_draws_simfinal"] <- "D_f"
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_total_loss_simfinal"] <- "L_f"
# names(f2_league_table_simfinal)[names(f2_league_table_simfinal) == "f2_PTS_simfinal"] <- "PTS_f"
# points_f2_sim <-  f2_league_table_simfinal[order(as.numeric(f2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# F2_notplayed <- F2_fixtures[F2_fixtures$f2_gamestatus == "notplayed",]
#
# write.xlsx(points_f2,'Divisions/Simulations.xlsx', sheetName = "F2_table",append = TRUE)
# write.xlsx(f2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "F2_sim",append = TRUE)
# write.xlsx(points_f2_sim,'Divisions/Simulations.xlsx', sheetName = "F2_simfinal",append = TRUE)
# #write.xlsx(F2_notplayed,'Divisions/Simulations.xlsx', sheetName = "F2_notplayed",append = TRUE)
# ####################################################################################################################################
# ###################################################################################################################
# #G1
# G1_sim <- G1
# G1_sim$matchid <- paste(G1_sim$HomeTeam,G1_sim$AwayTeam,sep = "-")
# G1_fixtures$matchid <- paste(G1_fixtures$HomeTeam_g1,G1_fixtures$AwayTeam_g1,sep = "-")
# G1_fixtures$g1_FTR <- sapply(G1_fixtures$g1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# G1_fixtures$g1_gamestatus <- ifelse(G1_fixtures$matchid %in% G1_sim$matchid,"played","notplayed")
#
# g1_home_wins_sim <- c()
# g1_away_wins_sim <- c()
# g1_home_draws_sim <- c()
# g1_away_draws_sim <- c()
# g1_home_loss_sim <- c()
# g1_away_loss_sim <- c()
#
#
#
# for (i_g1_wins_sim in 1:length(g1_teams))
# {
#
#   g1_home_wins_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$HomeTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "H" & G1_fixtures$g1_gamestatus =="notplayed",])
#   g1_away_wins_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$AwayTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "A" & G1_fixtures$g1_gamestatus == "notplayed",])
#   g1_home_draws_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$HomeTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "D" & G1_fixtures$g1_gamestatus == "notplayed",])
#   g1_away_draws_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$AwayTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "D" & G1_fixtures$g1_gamestatus == "notplayed",])
#   g1_home_loss_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$HomeTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "A" & G1_fixtures$g1_gamestatus == "notplayed",])
#   g1_away_loss_sim[i_g1_wins_sim] <- nrow(G1_fixtures[G1_fixtures$AwayTeam_g1 == g1_teams[i_g1_wins_sim] & G1_fixtures$g1_FTR == "H" & G1_fixtures$g1_gamestatus == "notplayed", ])
#
# }
#
# g1_total_wins_sim <- g1_home_wins_sim + g1_away_wins_sim
# g1_total_draws_sim <- g1_home_draws_sim + g1_away_draws_sim
# g1_total_loss_sim <- g1_home_loss_sim + g1_away_loss_sim
#
# g1_home_games_sim <- c()
# g1_away_games_sim <-c()
#
# for (i_g1_sim in 1:length(g1_teams))
# {
#
#   g1_home_games_sim[i_g1_sim] <- nrow(G1_fixtures[G1_fixtures$HomeTeam_g1 == g1_teams[i_g1_sim] & G1_fixtures$g1_gamestatus == "notplayed",])
#   g1_away_games_sim[i_g1_sim]  <- nrow(G1_fixtures[G1_fixtures$AwayTeam_g1 == g1_teams[i_g1_sim] & G1_fixtures$g1_gamestatus == "notplayed",])
#
# }
#
# g1_games_played_sim <- g1_home_games_sim + g1_away_games_sim
#
# g1_league_table_sim <- cbind(g1_teams,g1_games_played_sim,g1_total_wins_sim,g1_total_draws_sim,g1_total_loss_sim)
# g1_PTS_sim <- (g1_total_wins_sim*3) + (g1_total_draws_sim*1)
# g1_league_table_sim <- cbind(g1_league_table_sim,g1_PTS_sim)
#
# g1_games_played_simfinal <- g1_games_played + g1_games_played_sim
# g1_total_wins_simfinal <- g1_total_wins + g1_total_wins_sim
# g1_total_draws_simfinal <- g1_total_draws + g1_total_draws_sim
# g1_total_loss_simfinal <- g1_total_loss + g1_total_loss_sim
# g1_PTS_simfinal <- g1_PTS + g1_PTS_sim
#
# g1_league_table_simfinal <- cbind(g1_teams,g1_games_played_simfinal,g1_total_wins_simfinal,g1_total_draws_simfinal,g1_total_loss_simfinal,g1_PTS_simfinal)
# g1_league_table_simfinal <- as.data.frame(g1_league_table_simfinal)
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_teams"] <- "Team_f"
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_games_played_simfinal"] <- "P_f"
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_total_wins_simfinal"] <- "W_f"
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_total_draws_simfinal"] <- "D_f"
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_total_loss_simfinal"] <- "L_f"
# names(g1_league_table_simfinal)[names(g1_league_table_simfinal) == "g1_PTS_simfinal"] <- "PTS_f"
# points_g1_sim <-  g1_league_table_simfinal[order(as.numeric(g1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# G1_notplayed <- G1_fixtures[G1_fixtures$g1_gamestatus == "notplayed",]
#
#
# write.xlsx(points_g1,'Divisions/Simulations.xlsx', sheetName = "G1_Table",append = TRUE)
# write.xlsx(g1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "G1_sim",append = TRUE)
# write.xlsx(points_g1_sim,'Divisions/Simulations.xlsx', sheetName = "G1_simfinal",append = TRUE)
# #write.xlsx(G1_notplayed,'Divisions/Simulations.xlsx', sheetName = "G1_notplayed",append = TRUE)
# ############################################################################################################################################################
# ############################################################################################################################################################
# #I1
# I1_sim <- I1
# I1_sim$matchid <- paste(I1_sim$HomeTeam,I1_sim$AwayTeam,sep = "-")
# I1_fixtures$matchid <- paste(I1_fixtures$HomeTeam_i1,I1_fixtures$AwayTeam_i1,sep = "-")
# I1_fixtures$i1_FTR <- sapply(I1_fixtures$i1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# I1_fixtures$i1_gamestatus <- ifelse(I1_fixtures$matchid %in% I1_sim$matchid,"played","notplayed")
#
# i1_home_wins_sim <- c()
# i1_away_wins_sim <- c()
# i1_home_draws_sim <- c()
# i1_away_draws_sim <- c()
# i1_home_loss_sim <- c()
# i1_away_loss_sim <- c()
#
#
#
# for (i_i1_wins_sim in 1:length(i1_teams))
# {
#
#   i1_home_wins_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$HomeTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "H" & I1_fixtures$i1_gamestatus =="notplayed",])
#   i1_away_wins_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$AwayTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "A" & I1_fixtures$i1_gamestatus == "notplayed",])
#   i1_home_draws_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$HomeTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "D" & I1_fixtures$i1_gamestatus == "notplayed",])
#   i1_away_draws_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$AwayTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "D" & I1_fixtures$i1_gamestatus == "notplayed",])
#   i1_home_loss_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$HomeTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "A" & I1_fixtures$i1_gamestatus == "notplayed",])
#   i1_away_loss_sim[i_i1_wins_sim] <- nrow(I1_fixtures[I1_fixtures$AwayTeam_i1 == i1_teams[i_i1_wins_sim] & I1_fixtures$i1_FTR == "H" & I1_fixtures$i1_gamestatus == "notplayed", ])
#
# }
#
# i1_total_wins_sim <- i1_home_wins_sim + i1_away_wins_sim
# i1_total_draws_sim <- i1_home_draws_sim + i1_away_draws_sim
# i1_total_loss_sim <- i1_home_loss_sim + i1_away_loss_sim
#
# i1_home_games_sim <- c()
# i1_away_games_sim <-c()
#
# for (i_i1_sim in 1:length(i1_teams))
# {
#
#   i1_home_games_sim[i_i1_sim] <- nrow(I1_fixtures[I1_fixtures$HomeTeam_i1 == i1_teams[i_i1_sim] & I1_fixtures$i1_gamestatus == "notplayed",])
#   i1_away_games_sim[i_i1_sim]  <- nrow(I1_fixtures[I1_fixtures$AwayTeam_i1 == i1_teams[i_i1_sim] & I1_fixtures$i1_gamestatus == "notplayed",])
#
# }
#
# i1_games_played_sim <- i1_home_games_sim + i1_away_games_sim
#
# i1_league_table_sim <- cbind(i1_teams,i1_games_played_sim,i1_total_wins_sim,i1_total_draws_sim,i1_total_loss_sim)
# i1_PTS_sim <- (i1_total_wins_sim*3) + (i1_total_draws_sim*1)
# i1_league_table_sim <- cbind(i1_league_table_sim,i1_PTS_sim)
#
# i1_games_played_simfinal <- i1_games_played + i1_games_played_sim
# i1_total_wins_simfinal <- i1_total_wins + i1_total_wins_sim
# i1_total_draws_simfinal <- i1_total_draws + i1_total_draws_sim
# i1_total_loss_simfinal <- i1_total_loss + i1_total_loss_sim
# i1_PTS_simfinal <- i1_PTS + i1_PTS_sim
#
# i1_league_table_simfinal <- cbind(i1_teams,i1_games_played_simfinal,i1_total_wins_simfinal,i1_total_draws_simfinal,i1_total_loss_simfinal,i1_PTS_simfinal)
# i1_league_table_simfinal <- as.data.frame(i1_league_table_simfinal)
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_teams"] <- "Team_f"
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_games_played_simfinal"] <- "P_f"
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_total_wins_simfinal"] <- "W_f"
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_total_draws_simfinal"] <- "D_f"
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_total_loss_simfinal"] <- "L_f"
# names(i1_league_table_simfinal)[names(i1_league_table_simfinal) == "i1_PTS_simfinal"] <- "PTS_f"
# points_i1_sim <-  i1_league_table_simfinal[order(as.numeric(i1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# I1_notplayed <- I1_fixtures[I1_fixtures$i1_gamestatus == "notplayed",]
#
#
# write.xlsx(points_i1,'Divisions/Simulations.xlsx', sheetName = "I1_table",append = TRUE)
# write.xlsx(i1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "I1_sim",append = TRUE)
# write.xlsx(points_i1_sim,'Divisions/Simulations.xlsx', sheetName = "I1_simfinal",append = TRUE)
# #write.xlsx(I1_notplayed,'Divisions/Simulations.xlsx', sheetName = "I1_notplayed",append = TRUE)
# ##################################################################################################################################
# ##################################################################################################################################
# #I2
# I2_sim <- I2
# I2_sim$matchid <- paste(I2_sim$HomeTeam,I2_sim$AwayTeam,sep = "-")
# I2_fixtures$matchid <- paste(I2_fixtures$HomeTeam_i2,I2_fixtures$AwayTeam_i2,sep = "-")
# I2_fixtures$i2_FTR <- sapply(I2_fixtures$i2_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# I2_fixtures$i2_gamestatus <- ifelse(I2_fixtures$matchid %in% I2_sim$matchid,"played","notplayed")
#
# i2_home_wins_sim <- c()
# i2_away_wins_sim <- c()
# i2_home_draws_sim <- c()
# i2_away_draws_sim <- c()
# i2_home_loss_sim <- c()
# i2_away_loss_sim <- c()
#
#
#
# for (i_i2_wins_sim in 1:length(i2_teams))
# {
#
#   i2_home_wins_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$HomeTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "H" & I2_fixtures$i2_gamestatus =="notplayed",])
#   i2_away_wins_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$AwayTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "A" & I2_fixtures$i2_gamestatus == "notplayed",])
#   i2_home_draws_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$HomeTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "D" & I2_fixtures$i2_gamestatus == "notplayed",])
#   i2_away_draws_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$AwayTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "D" & I2_fixtures$i2_gamestatus == "notplayed",])
#   i2_home_loss_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$HomeTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "A" & I2_fixtures$i2_gamestatus == "notplayed",])
#   i2_away_loss_sim[i_i2_wins_sim] <- nrow(I2_fixtures[I2_fixtures$AwayTeam_i2 == i2_teams[i_i2_wins_sim] & I2_fixtures$i2_FTR == "H" & I2_fixtures$i2_gamestatus == "notplayed", ])
#
# }
#
# i2_total_wins_sim <- i2_home_wins_sim + i2_away_wins_sim
# i2_total_draws_sim <- i2_home_draws_sim + i2_away_draws_sim
# i2_total_loss_sim <- i2_home_loss_sim + i2_away_loss_sim
#
# i2_home_games_sim <- c()
# i2_away_games_sim <-c()
#
# for (i_i2_sim in 1:length(i2_teams))
# {
#
#   i2_home_games_sim[i_i2_sim] <- nrow(I2_fixtures[I2_fixtures$HomeTeam_i2 == i2_teams[i_i2_sim] & I2_fixtures$i2_gamestatus == "notplayed",])
#   i2_away_games_sim[i_i2_sim]  <- nrow(I2_fixtures[I2_fixtures$AwayTeam_i2 == i2_teams[i_i2_sim] & I2_fixtures$i2_gamestatus == "notplayed",])
#
# }
#
# i2_games_played_sim <- i2_home_games_sim + i2_away_games_sim
#
# i2_league_table_sim <- cbind(i2_teams,i2_games_played_sim,i2_total_wins_sim,i2_total_draws_sim,i2_total_loss_sim)
# i2_PTS_sim <- (i2_total_wins_sim*3) + (i2_total_draws_sim*1)
# i2_league_table_sim <- cbind(i2_league_table_sim,i2_PTS_sim)
#
# i2_games_played_simfinal <- i2_games_played + i2_games_played_sim
# i2_total_wins_simfinal <- i2_total_wins + i2_total_wins_sim
# i2_total_draws_simfinal <- i2_total_draws + i2_total_draws_sim
# i2_total_loss_simfinal <- i2_total_loss + i2_total_loss_sim
# i2_PTS_simfinal <- i2_PTS + i2_PTS_sim
#
# i2_league_table_simfinal <- cbind(i2_teams,i2_games_played_simfinal,i2_total_wins_simfinal,i2_total_draws_simfinal,i2_total_loss_simfinal,i2_PTS_simfinal)
# i2_league_table_simfinal <- as.data.frame(i2_league_table_simfinal)
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_teams"] <- "Team_f"
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_games_played_simfinal"] <- "P_f"
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_total_wins_simfinal"] <- "W_f"
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_total_draws_simfinal"] <- "D_f"
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_total_loss_simfinal"] <- "L_f"
# names(i2_league_table_simfinal)[names(i2_league_table_simfinal) == "i2_PTS_simfinal"] <- "PTS_f"
# points_i2_sim <-  i2_league_table_simfinal[order(as.numeric(i2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# I2_notplayed <- I2_fixtures[I2_fixtures$i2_gamestatus == "notplayed",]
#
# write.xlsx(points_i2,'Divisions/Simulations.xlsx', sheetName = "I2_table",append = TRUE)
# write.xlsx(i2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "I2_sim",append = TRUE)
# write.xlsx(points_i2_sim,'Divisions/Simulations.xlsx', sheetName = "I2_simfinal",append = TRUE)
# #write.xlsx(I2_notplayed,'Divisions/Simulations.xlsx', sheetName = "I2_simfinal",append = TRUE)
# #########################################################################################################################################
# #########################################################################################################################################
# #N1
# N1_sim <- N1
# N1_sim$matchid <- paste(N1_sim$HomeTeam,N1_sim$AwayTeam,sep = "-")
# N1_fixtures$matchid <- paste(N1_fixtures$HomeTeam_n1,N1_fixtures$AwayTeam_n1,sep = "-")
# N1_fixtures$n1_FTR <- sapply(N1_fixtures$n1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# N1_fixtures$n1_gamestatus <- ifelse(N1_fixtures$matchid %in% N1_sim$matchid,"played","notplayed")
#
# n1_home_wins_sim <- c()
# n1_away_wins_sim <- c()
# n1_home_draws_sim <- c()
# n1_away_draws_sim <- c()
# n1_home_loss_sim <- c()
# n1_away_loss_sim <- c()
#
#
#
# for (i_n1_wins_sim in 1:length(n1_teams))
# {
#
#   n1_home_wins_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$HomeTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "H" & N1_fixtures$n1_gamestatus =="notplayed",])
#   n1_away_wins_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$AwayTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "A" & N1_fixtures$n1_gamestatus == "notplayed",])
#   n1_home_draws_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$HomeTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "D" & N1_fixtures$n1_gamestatus == "notplayed",])
#   n1_away_draws_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$AwayTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "D" & N1_fixtures$n1_gamestatus == "notplayed",])
#   n1_home_loss_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$HomeTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "A" & N1_fixtures$n1_gamestatus == "notplayed",])
#   n1_away_loss_sim[i_n1_wins_sim] <- nrow(N1_fixtures[N1_fixtures$AwayTeam_n1 == n1_teams[i_n1_wins_sim] & N1_fixtures$n1_FTR == "H" & N1_fixtures$n1_gamestatus == "notplayed", ])
#
# }
#
# n1_total_wins_sim <- n1_home_wins_sim + n1_away_wins_sim
# n1_total_draws_sim <- n1_home_draws_sim + n1_away_draws_sim
# n1_total_loss_sim <- n1_home_loss_sim + n1_away_loss_sim
#
# n1_home_games_sim <- c()
# n1_away_games_sim <-c()
#
# for (i_n1_sim in 1:length(n1_teams))
# {
#
#   n1_home_games_sim[i_n1_sim] <- nrow(N1_fixtures[N1_fixtures$HomeTeam_n1 == n1_teams[i_n1_sim] & N1_fixtures$n1_gamestatus == "notplayed",])
#   n1_away_games_sim[i_n1_sim]  <- nrow(N1_fixtures[N1_fixtures$AwayTeam_n1 == n1_teams[i_n1_sim] & N1_fixtures$n1_gamestatus == "notplayed",])
#
# }
#
# n1_games_played_sim <- n1_home_games_sim + n1_away_games_sim
#
# n1_league_table_sim <- cbind(n1_teams,n1_games_played_sim,n1_total_wins_sim,n1_total_draws_sim,n1_total_loss_sim)
# n1_PTS_sim <- (n1_total_wins_sim*3) + (n1_total_draws_sim*1)
# n1_league_table_sim <- cbind(n1_league_table_sim,n1_PTS_sim)
#
# n1_games_played_simfinal <- n1_games_played + n1_games_played_sim
# n1_total_wins_simfinal <- n1_total_wins + n1_total_wins_sim
# n1_total_draws_simfinal <- n1_total_draws + n1_total_draws_sim
# n1_total_loss_simfinal <- n1_total_loss + n1_total_loss_sim
# n1_PTS_simfinal <- n1_PTS + n1_PTS_sim
#
# n1_league_table_simfinal <- cbind(n1_teams,n1_games_played_simfinal,n1_total_wins_simfinal,n1_total_draws_simfinal,n1_total_loss_simfinal,n1_PTS_simfinal)
# n1_league_table_simfinal <- as.data.frame(n1_league_table_simfinal)
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_teams"] <- "Team_f"
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_games_played_simfinal"] <- "P_f"
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_total_wins_simfinal"] <- "W_f"
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_total_draws_simfinal"] <- "D_f"
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_total_loss_simfinal"] <- "L_f"
# names(n1_league_table_simfinal)[names(n1_league_table_simfinal) == "n1_PTS_simfinal"] <- "PTS_f"
# points_n1_sim <-  n1_league_table_simfinal[order(as.numeric(n1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# N1_notplayed <- N1_fixtures[N1_fixtures$n1_gamestatus == "notplayed",]
#
# write.xlsx(points_n1,'Divisions/Simulations.xlsx', sheetName = "N1_table",append = TRUE)
# write.xlsx(n1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "N1_sim",append = TRUE)
# write.xlsx(points_n1_sim,'Divisions/Simulations.xlsx', sheetName = "N1_simfinal",append = TRUE)
# #write.xlsx(N1_notplayed,'Divisions/Simulations.xlsx', sheetName = "N1_notplayed",append = TRUE)
# ###################################################################################################################################
# ###################################################################################################################################
# #P1
# P1_sim <- P1
# P1_sim$matchid <- paste(P1_sim$HomeTeam,P1_sim$AwayTeam,sep = "-")
# P1_fixtures$matchid <- paste(P1_fixtures$HomeTeam_p1,P1_fixtures$AwayTeam_p1,sep = "-")
# P1_fixtures$p1_FTR <- sapply(P1_fixtures$p1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# P1_fixtures$p1_gamestatus <- ifelse(P1_fixtures$matchid %in% P1_sim$matchid,"played","notplayed")
#
# p1_home_wins_sim <- c()
# p1_away_wins_sim <- c()
# p1_home_draws_sim <- c()
# p1_away_draws_sim <- c()
# p1_home_loss_sim <- c()
# p1_away_loss_sim <- c()
#
#
#
# for (i_p1_wins_sim in 1:length(p1_teams))
# {
#
#   p1_home_wins_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$HomeTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "H" & P1_fixtures$p1_gamestatus =="notplayed",])
#   p1_away_wins_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$AwayTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "A" & P1_fixtures$p1_gamestatus == "notplayed",])
#   p1_home_draws_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$HomeTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "D" & P1_fixtures$p1_gamestatus == "notplayed",])
#   p1_away_draws_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$AwayTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "D" & P1_fixtures$p1_gamestatus == "notplayed",])
#   p1_home_loss_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$HomeTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "A" & P1_fixtures$p1_gamestatus == "notplayed",])
#   p1_away_loss_sim[i_p1_wins_sim] <- nrow(P1_fixtures[P1_fixtures$AwayTeam_p1 == p1_teams[i_p1_wins_sim] & P1_fixtures$p1_FTR == "H" & P1_fixtures$p1_gamestatus == "notplayed", ])
#
# }
#
# p1_total_wins_sim <- p1_home_wins_sim + p1_away_wins_sim
# p1_total_draws_sim <- p1_home_draws_sim + p1_away_draws_sim
# p1_total_loss_sim <- p1_home_loss_sim + p1_away_loss_sim
#
# p1_home_games_sim <- c()
# p1_away_games_sim <-c()
#
# for (i_p1_sim in 1:length(p1_teams))
# {
#
#   p1_home_games_sim[i_p1_sim] <- nrow(P1_fixtures[P1_fixtures$HomeTeam_p1 == p1_teams[i_p1_sim] & P1_fixtures$p1_gamestatus == "notplayed",])
#   p1_away_games_sim[i_p1_sim]  <- nrow(P1_fixtures[P1_fixtures$AwayTeam_p1 == p1_teams[i_p1_sim] & P1_fixtures$p1_gamestatus == "notplayed",])
#
# }
#
# p1_games_played_sim <- p1_home_games_sim + p1_away_games_sim
#
# p1_league_table_sim <- cbind(p1_teams,p1_games_played_sim,p1_total_wins_sim,p1_total_draws_sim,p1_total_loss_sim)
# p1_PTS_sim <- (p1_total_wins_sim*3) + (p1_total_draws_sim*1)
# p1_league_table_sim <- cbind(p1_league_table_sim,p1_PTS_sim)
#
# p1_games_played_simfinal <- p1_games_played + p1_games_played_sim
# p1_total_wins_simfinal <- p1_total_wins + p1_total_wins_sim
# p1_total_draws_simfinal <- p1_total_draws + p1_total_draws_sim
# p1_total_loss_simfinal <- p1_total_loss + p1_total_loss_sim
# p1_PTS_simfinal <- p1_PTS + p1_PTS_sim
#
# p1_league_table_simfinal <- cbind(p1_teams,p1_games_played_simfinal,p1_total_wins_simfinal,p1_total_draws_simfinal,p1_total_loss_simfinal,p1_PTS_simfinal)
# p1_league_table_simfinal <- as.data.frame(p1_league_table_simfinal)
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_teams"] <- "Team_f"
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_games_played_simfinal"] <- "P_f"
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_total_wins_simfinal"] <- "W_f"
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_total_draws_simfinal"] <- "D_f"
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_total_loss_simfinal"] <- "L_f"
# names(p1_league_table_simfinal)[names(p1_league_table_simfinal) == "p1_PTS_simfinal"] <- "PTS_f"
# points_p1_sim <-  p1_league_table_simfinal[order(as.numeric(p1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# P1_notplayed <- P1_fixtures[P1_fixtures$p1_gamestatus == "notplayed",]
#
#
# write.xlsx(points_p1,'Divisions/Simulations.xlsx', sheetName = "P1_table",append = TRUE)
# write.xlsx(p1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "P1_sim",append = TRUE)
# write.xlsx(points_p1_sim,'Divisions/Simulations.xlsx', sheetName = "P1_simfinal",append = TRUE)
# #write.xlsx(P1_notplayed,'Divisions/Simulations.xlsx', sheetName = "P1_notplayed",append = TRUE)
# ##########################################################################################################################################
# ###################################################################################################################
# #SP1
# SP1_sim <- SP1
# SP1_sim$matchid <- paste(SP1_sim$HomeTeam,SP1_sim$AwayTeam,sep = "-")
# SP1_fixtures$matchid <- paste(SP1_fixtures$HomeTeam_sp1,SP1_fixtures$AwayTeam_sp1,sep = "-")
# SP1_fixtures$sp1_FTR <- sapply(SP1_fixtures$sp1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SP1_fixtures$sp1_gamestatus <- ifelse(SP1_fixtures$matchid %in% SP1_sim$matchid,"played","notplayed")
#
# sp1_home_wins_sim <- c()
# sp1_away_wins_sim <- c()
# sp1_home_draws_sim <- c()
# sp1_away_draws_sim <- c()
# sp1_home_loss_sim <- c()
# sp1_away_loss_sim <- c()
#
#
#
# for (i_sp1_wins_sim in 1:length(sp1_teams))
# {
#
#   sp1_home_wins_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$HomeTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "H" & SP1_fixtures$sp1_gamestatus =="notplayed",])
#   sp1_away_wins_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$AwayTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "A" & SP1_fixtures$sp1_gamestatus == "notplayed",])
#   sp1_home_draws_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$HomeTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "D" & SP1_fixtures$sp1_gamestatus == "notplayed",])
#   sp1_away_draws_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$AwayTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "D" & SP1_fixtures$sp1_gamestatus == "notplayed",])
#   sp1_home_loss_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$HomeTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "A" & SP1_fixtures$sp1_gamestatus == "notplayed",])
#   sp1_away_loss_sim[i_sp1_wins_sim] <- nrow(SP1_fixtures[SP1_fixtures$AwayTeam_sp1 == sp1_teams[i_sp1_wins_sim] & SP1_fixtures$sp1_FTR == "H" & SP1_fixtures$sp1_gamestatus == "notplayed", ])
#
# }
#
# sp1_total_wins_sim <- sp1_home_wins_sim + sp1_away_wins_sim
# sp1_total_draws_sim <- sp1_home_draws_sim + sp1_away_draws_sim
# sp1_total_loss_sim <- sp1_home_loss_sim + sp1_away_loss_sim
#
# sp1_home_games_sim <- c()
# sp1_away_games_sim <-c()
#
# for (i_sp1_sim in 1:length(sp1_teams))
# {
#
#   sp1_home_games_sim[i_sp1_sim] <- nrow(SP1_fixtures[SP1_fixtures$HomeTeam_sp1 == sp1_teams[i_sp1_sim] & SP1_fixtures$sp1_gamestatus == "notplayed",])
#   sp1_away_games_sim[i_sp1_sim]  <- nrow(SP1_fixtures[SP1_fixtures$AwayTeam_sp1 == sp1_teams[i_sp1_sim] & SP1_fixtures$sp1_gamestatus == "notplayed",])
#
# }
#
# sp1_games_played_sim <- sp1_home_games_sim + sp1_away_games_sim
#
# sp1_league_table_sim <- cbind(sp1_teams,sp1_games_played_sim,sp1_total_wins_sim,sp1_total_draws_sim,sp1_total_loss_sim)
# sp1_PTS_sim <- (sp1_total_wins_sim*3) + (sp1_total_draws_sim*1)
# sp1_league_table_sim <- cbind(sp1_league_table_sim,sp1_PTS_sim)
#
# sp1_games_played_simfinal <- sp1_games_played + sp1_games_played_sim
# sp1_total_wins_simfinal <- sp1_total_wins + sp1_total_wins_sim
# sp1_total_draws_simfinal <- sp1_total_draws + sp1_total_draws_sim
# sp1_total_loss_simfinal <- sp1_total_loss + sp1_total_loss_sim
# sp1_PTS_simfinal <- sp1_PTS + sp1_PTS_sim
#
# sp1_league_table_simfinal <- cbind(sp1_teams,sp1_games_played_simfinal,sp1_total_wins_simfinal,sp1_total_draws_simfinal,sp1_total_loss_simfinal,sp1_PTS_simfinal)
# sp1_league_table_simfinal <- as.data.frame(sp1_league_table_simfinal)
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_teams"] <- "Team_f"
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_games_played_simfinal"] <- "P_f"
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_total_wins_simfinal"] <- "W_f"
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_total_draws_simfinal"] <- "D_f"
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_total_loss_simfinal"] <- "L_f"
# names(sp1_league_table_simfinal)[names(sp1_league_table_simfinal) == "sp1_PTS_simfinal"] <- "PTS_f"
# points_sp1_sim <-  sp1_league_table_simfinal[order(as.numeric(sp1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SP1_notplayed <- SP1_fixtures[SP1_fixtures$sp1_gamestatus == "notplayed",]
#
#
# write.xlsx(points_sp1,'Divisions/Simulations.xlsx', sheetName = "SP1_table",append = TRUE)
# write.xlsx(sp1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SP1_sim",append = TRUE)
# write.xlsx(points_sp1_sim,'Divisions/Simulations.xlsx', sheetName = "SP1_simfinal",append = TRUE)
# #write.xlsx(SP1_notplayed,'Divisions/Simulations.xlsx', sheetName = "SP1_notplayed",append = TRUE)
# #####################################################################################################################################
# ###################################################################################################################
# #SP2
# SP2_sim <- SP2
# SP2_sim$matchid <- paste(SP2_sim$HomeTeam,SP2_sim$AwayTeam,sep = "-")
# SP2_fixtures$matchid <- paste(SP2_fixtures$HomeTeam_sp2,SP2_fixtures$AwayTeam_sp2,sep = "-")
# SP2_fixtures$sp2_FTR <- sapply(SP2_fixtures$sp2_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SP2_fixtures$sp2_gamestatus <- ifelse(SP2_fixtures$matchid %in% SP2_sim$matchid,"played","notplayed")
#
# sp2_home_wins_sim <- c()
# sp2_away_wins_sim <- c()
# sp2_home_draws_sim <- c()
# sp2_away_draws_sim <- c()
# sp2_home_loss_sim <- c()
# sp2_away_loss_sim <- c()
#
#
#
# for (i_sp2_wins_sim in 1:length(sp2_teams))
# {
#
#   sp2_home_wins_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$HomeTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "H" & SP2_fixtures$sp2_gamestatus =="notplayed",])
#   sp2_away_wins_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$AwayTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "A" & SP2_fixtures$sp2_gamestatus == "notplayed",])
#   sp2_home_draws_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$HomeTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "D" & SP2_fixtures$sp2_gamestatus == "notplayed",])
#   sp2_away_draws_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$AwayTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "D" & SP2_fixtures$sp2_gamestatus == "notplayed",])
#   sp2_home_loss_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$HomeTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "A" & SP2_fixtures$sp2_gamestatus == "notplayed",])
#   sp2_away_loss_sim[i_sp2_wins_sim] <- nrow(SP2_fixtures[SP2_fixtures$AwayTeam_sp2 == sp2_teams[i_sp2_wins_sim] & SP2_fixtures$sp2_FTR == "H" & SP2_fixtures$sp2_gamestatus == "notplayed", ])
#
# }
#
# sp2_total_wins_sim <- sp2_home_wins_sim + sp2_away_wins_sim
# sp2_total_draws_sim <- sp2_home_draws_sim + sp2_away_draws_sim
# sp2_total_loss_sim <- sp2_home_loss_sim + sp2_away_loss_sim
#
# sp2_home_games_sim <- c()
# sp2_away_games_sim <-c()
#
# for (i_sp2_sim in 1:length(sp2_teams))
# {
#
#   sp2_home_games_sim[i_sp2_sim] <- nrow(SP2_fixtures[SP2_fixtures$HomeTeam_sp2 == sp2_teams[i_sp2_sim] & SP2_fixtures$sp2_gamestatus == "notplayed",])
#   sp2_away_games_sim[i_sp2_sim]  <- nrow(SP2_fixtures[SP2_fixtures$AwayTeam_sp2 == sp2_teams[i_sp2_sim] & SP2_fixtures$sp2_gamestatus == "notplayed",])
#
# }
#
# sp2_games_played_sim <- sp2_home_games_sim + sp2_away_games_sim
#
# sp2_league_table_sim <- cbind(sp2_teams,sp2_games_played_sim,sp2_total_wins_sim,sp2_total_draws_sim,sp2_total_loss_sim)
# sp2_PTS_sim <- (sp2_total_wins_sim*3) + (sp2_total_draws_sim*1)
# sp2_league_table_sim <- cbind(sp2_league_table_sim,sp2_PTS_sim)
#
# sp2_games_played_simfinal <- sp2_games_played + sp2_games_played_sim
# sp2_total_wins_simfinal <- sp2_total_wins + sp2_total_wins_sim
# sp2_total_draws_simfinal <- sp2_total_draws + sp2_total_draws_sim
# sp2_total_loss_simfinal <- sp2_total_loss + sp2_total_loss_sim
# sp2_PTS_simfinal <- sp2_PTS + sp2_PTS_sim
#
# sp2_league_table_simfinal <- cbind(sp2_teams,sp2_games_played_simfinal,sp2_total_wins_simfinal,sp2_total_draws_simfinal,sp2_total_loss_simfinal,sp2_PTS_simfinal)
# sp2_league_table_simfinal <- as.data.frame(sp2_league_table_simfinal)
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_teams"] <- "Team_f"
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_games_played_simfinal"] <- "P_f"
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_total_wins_simfinal"] <- "W_f"
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_total_draws_simfinal"] <- "D_f"
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_total_loss_simfinal"] <- "L_f"
# names(sp2_league_table_simfinal)[names(sp2_league_table_simfinal) == "sp2_PTS_simfinal"] <- "PTS_f"
# points_sp2_sim <-  sp2_league_table_simfinal[order(as.numeric(sp2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SP2_notplayed <- SP2_fixtures[SP2_fixtures$sp2_gamestatus == "notplayed",]
#
# write.xlsx(points_sp2,'Divisions/Simulations.xlsx', sheetName = "SP2_table",append = TRUE)
# write.xlsx(sp2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SP2_sim",append = TRUE)
# write.xlsx(points_sp2_sim,'Divisions/Simulations.xlsx', sheetName = "SP2_simfinal",append = TRUE)
# #write.xlsx(SP2_notplayed,'Divisions/Simulations.xlsx', sheetName = "SP1_simfinal",append = TRUE)
# ##########################################################################################################################################
# ##########################################################################################################################################
# #SC0
# SC0_sim <- SC0
# SC0_sim$matchid <- paste(SC0_sim$HomeTeam,SC0_sim$AwayTeam,sep = "-")
# SC0_fixtures$matchid <- paste(SC0_fixtures$HomeTeam_sc0,SC0_fixtures$AwayTeam_sc0,sep = "-")
# SC0_fixtures$sc0_FTR <- sapply(SC0_fixtures$sc0_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SC0_fixtures$sc0_gamestatus <- ifelse(SC0_fixtures$matchid %in% SC0_sim$matchid,"played","notplayed")
#
# sc0_home_wins_sim <- c()
# sc0_away_wins_sim <- c()
# sc0_home_draws_sim <- c()
# sc0_away_draws_sim <- c()
# sc0_home_loss_sim <- c()
# sc0_away_loss_sim <- c()
#
#
#
# for (i_sc0_wins_sim in 1:length(sc0_teams))
# {
#
#   sc0_home_wins_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$HomeTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "H" & SC0_fixtures$sc0_gamestatus =="notplayed",])
#   sc0_away_wins_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$AwayTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "A" & SC0_fixtures$sc0_gamestatus == "notplayed",])
#   sc0_home_draws_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$HomeTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "D" & SC0_fixtures$sc0_gamestatus == "notplayed",])
#   sc0_away_draws_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$AwayTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "D" & SC0_fixtures$sc0_gamestatus == "notplayed",])
#   sc0_home_loss_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$HomeTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "A" & SC0_fixtures$sc0_gamestatus == "notplayed",])
#   sc0_away_loss_sim[i_sc0_wins_sim] <- nrow(SC0_fixtures[SC0_fixtures$AwayTeam_sc0 == sc0_teams[i_sc0_wins_sim] & SC0_fixtures$sc0_FTR == "H" & SC0_fixtures$sc0_gamestatus == "notplayed", ])
#
# }
#
# sc0_total_wins_sim <- sc0_home_wins_sim + sc0_away_wins_sim
# sc0_total_draws_sim <- sc0_home_draws_sim + sc0_away_draws_sim
# sc0_total_loss_sim <- sc0_home_loss_sim + sc0_away_loss_sim
#
# sc0_home_games_sim <- c()
# sc0_away_games_sim <-c()
#
# for (i_sc0_sim in 1:length(sc0_teams))
# {
#
#   sc0_home_games_sim[i_sc0_sim] <- nrow(SC0_fixtures[SC0_fixtures$HomeTeam_sc0 == sc0_teams[i_sc0_sim] & SC0_fixtures$sc0_gamestatus == "notplayed",])
#   sc0_away_games_sim[i_sc0_sim]  <- nrow(SC0_fixtures[SC0_fixtures$AwayTeam_sc0 == sc0_teams[i_sc0_sim] & SC0_fixtures$sc0_gamestatus == "notplayed",])
#
# }
#
# sc0_games_played_sim <- sc0_home_games_sim + sc0_away_games_sim
#
# sc0_league_table_sim <- cbind(sc0_teams,sc0_games_played_sim,sc0_total_wins_sim,sc0_total_draws_sim,sc0_total_loss_sim)
# sc0_PTS_sim <- (sc0_total_wins_sim*3) + (sc0_total_draws_sim*1)
# sc0_league_table_sim <- cbind(sc0_league_table_sim,sc0_PTS_sim)
#
# sc0_games_played_simfinal <- sc0_games_played + sc0_games_played_sim
# sc0_total_wins_simfinal <- sc0_total_wins + sc0_total_wins_sim
# sc0_total_draws_simfinal <- sc0_total_draws + sc0_total_draws_sim
# sc0_total_loss_simfinal <- sc0_total_loss + sc0_total_loss_sim
# sc0_PTS_simfinal <- sc0_PTS + sc0_PTS_sim
#
# sc0_league_table_simfinal <- cbind(sc0_teams,sc0_games_played_simfinal,sc0_total_wins_simfinal,sc0_total_draws_simfinal,sc0_total_loss_simfinal,sc0_PTS_simfinal)
# sc0_league_table_simfinal <- as.data.frame(sc0_league_table_simfinal)
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_teams"] <- "Team_f"
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_games_played_simfinal"] <- "P_f"
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_total_wins_simfinal"] <- "W_f"
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_total_draws_simfinal"] <- "D_f"
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_total_loss_simfinal"] <- "L_f"
# names(sc0_league_table_simfinal)[names(sc0_league_table_simfinal) == "sc0_PTS_simfinal"] <- "PTS_f"
# points_sc0_sim <-  sc0_league_table_simfinal[order(as.numeric(sc0_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SC0_notplayed <- SC0_fixtures[SC0_fixtures$sc0_gamestatus == "notplayed",]
#
# write.xlsx(points_sc0,'Divisions/Simulations.xlsx', sheetName = "SC0_table",append = TRUE)
# write.xlsx(sc0_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SC0_sim",append = TRUE)
# write.xlsx(points_sc0_sim,'Divisions/Simulations.xlsx', sheetName = "SC0_simfinal",append = TRUE)
# #write.xlsx(SC0_notplayed,'Divisions/Simulations.xlsx', sheetName = "SC0_notplayed",append = TRUE)
# ######################################################################################################################################
# ######################################################################################################################################
# #SC1
# SC1_sim <- SC1
# SC1_sim$matchid <- paste(SC1_sim$HomeTeam,SC1_sim$AwayTeam,sep = "-")
# SC1_fixtures$matchid <- paste(SC1_fixtures$HomeTeam_sc1,SC1_fixtures$AwayTeam_sc1,sep = "-")
# SC1_fixtures$sc1_FTR <- sapply(SC1_fixtures$sc1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SC1_fixtures$sc1_gamestatus <- ifelse(SC1_fixtures$matchid %in% SC1_sim$matchid,"played","notplayed")
#
# sc1_home_wins_sim <- c()
# sc1_away_wins_sim <- c()
# sc1_home_draws_sim <- c()
# sc1_away_draws_sim <- c()
# sc1_home_loss_sim <- c()
# sc1_away_loss_sim <- c()
#
#
#
# for (i_sc1_wins_sim in 1:length(sc1_teams))
# {
#
#   sc1_home_wins_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$HomeTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "H" & SC1_fixtures$sc1_gamestatus =="notplayed",])
#   sc1_away_wins_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$AwayTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "A" & SC1_fixtures$sc1_gamestatus == "notplayed",])
#   sc1_home_draws_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$HomeTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "D" & SC1_fixtures$sc1_gamestatus == "notplayed",])
#   sc1_away_draws_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$AwayTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "D" & SC1_fixtures$sc1_gamestatus == "notplayed",])
#   sc1_home_loss_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$HomeTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "A" & SC1_fixtures$sc1_gamestatus == "notplayed",])
#   sc1_away_loss_sim[i_sc1_wins_sim] <- nrow(SC1_fixtures[SC1_fixtures$AwayTeam_sc1 == sc1_teams[i_sc1_wins_sim] & SC1_fixtures$sc1_FTR == "H" & SC1_fixtures$sc1_gamestatus == "notplayed", ])
#
# }
#
# sc1_total_wins_sim <- sc1_home_wins_sim + sc1_away_wins_sim
# sc1_total_draws_sim <- sc1_home_draws_sim + sc1_away_draws_sim
# sc1_total_loss_sim <- sc1_home_loss_sim + sc1_away_loss_sim
#
# sc1_home_games_sim <- c()
# sc1_away_games_sim <-c()
#
# for (i_sc1_sim in 1:length(sc1_teams))
# {
#
#   sc1_home_games_sim[i_sc1_sim] <- nrow(SC1_fixtures[SC1_fixtures$HomeTeam_sc1 == sc1_teams[i_sc1_sim] & SC1_fixtures$sc1_gamestatus == "notplayed",])
#   sc1_away_games_sim[i_sc1_sim]  <- nrow(SC1_fixtures[SC1_fixtures$AwayTeam_sc1 == sc1_teams[i_sc1_sim] & SC1_fixtures$sc1_gamestatus == "notplayed",])
#
# }
#
# sc1_games_played_sim <- sc1_home_games_sim + sc1_away_games_sim
#
# sc1_league_table_sim <- cbind(sc1_teams,sc1_games_played_sim,sc1_total_wins_sim,sc1_total_draws_sim,sc1_total_loss_sim)
# sc1_PTS_sim <- (sc1_total_wins_sim*3) + (sc1_total_draws_sim*1)
# sc1_league_table_sim <- cbind(sc1_league_table_sim,sc1_PTS_sim)
#
# sc1_games_played_simfinal <- sc1_games_played + sc1_games_played_sim
# sc1_total_wins_simfinal <- sc1_total_wins + sc1_total_wins_sim
# sc1_total_draws_simfinal <- sc1_total_draws + sc1_total_draws_sim
# sc1_total_loss_simfinal <- sc1_total_loss + sc1_total_loss_sim
# sc1_PTS_simfinal <- sc1_PTS + sc1_PTS_sim
#
# sc1_league_table_simfinal <- cbind(sc1_teams,sc1_games_played_simfinal,sc1_total_wins_simfinal,sc1_total_draws_simfinal,sc1_total_loss_simfinal,sc1_PTS_simfinal)
# sc1_league_table_simfinal <- as.data.frame(sc1_league_table_simfinal)
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_teams"] <- "Team_f"
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_games_played_simfinal"] <- "P_f"
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_total_wins_simfinal"] <- "W_f"
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_total_draws_simfinal"] <- "D_f"
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_total_loss_simfinal"] <- "L_f"
# names(sc1_league_table_simfinal)[names(sc1_league_table_simfinal) == "sc1_PTS_simfinal"] <- "PTS_f"
# points_sc1_sim <-  sc1_league_table_simfinal[order(as.numeric(sc1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SC1_notplayed <- SC1_fixtures[SC1_fixtures$sc1_gamestatus == "notplayed",]
#
# write.xlsx(points_sc1,'Divisions/Simulations.xlsx', sheetName = "SC1_table",append = TRUE)
# write.xlsx(sc1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SC1_sim",append = TRUE)
# write.xlsx(points_sc1_sim,'Divisions/Simulations.xlsx', sheetName = "SC1_simfinal",append = TRUE)
# #write.xlsx(SC1_notplayed,'Divisions/Simulations.xlsx', sheetName = "SC1_notplayed",append = TRUE)
# ###################################################################################################################################
# ###################################################################################################################
# #SC2
# SC2_sim <- SC2
# SC2_sim$matchid <- paste(SC2_sim$HomeTeam,SC2_sim$AwayTeam,sep = "-")
# SC2_fixtures$matchid <- paste(SC2_fixtures$HomeTeam_sc2,SC2_fixtures$AwayTeam_sc2,sep = "-")
# SC2_fixtures$sc2_FTR <- sapply(SC2_fixtures$sc2_pscore,switch,
#                                '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                                '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                                '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SC2_fixtures$sc2_gamestatus <- ifelse(SC2_fixtures$matchid %in% SC2_sim$matchid,"played","notplayed")
#
# sc2_home_wins_sim <- c()
# sc2_away_wins_sim <- c()
# sc2_home_draws_sim <- c()
# sc2_away_draws_sim <- c()
# sc2_home_loss_sim <- c()
# sc2_away_loss_sim <- c()
#
#
#
# for (i_sc2_wins_sim in 1:length(sc2_teams))
# {
#
#   sc2_home_wins_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$HomeTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "H" & SC2_fixtures$sc2_gamestatus =="notplayed",])
#   sc2_away_wins_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$AwayTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "A" & SC2_fixtures$sc2_gamestatus == "notplayed",])
#   sc2_home_draws_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$HomeTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "D" & SC2_fixtures$sc2_gamestatus == "notplayed",])
#   sc2_away_draws_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$AwayTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "D" & SC2_fixtures$sc2_gamestatus == "notplayed",])
#   sc2_home_loss_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$HomeTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "A" & SC2_fixtures$sc2_gamestatus == "notplayed",])
#   sc2_away_loss_sim[i_sc2_wins_sim] <- nrow(SC2_fixtures[SC2_fixtures$AwayTeam_sc2 == sc2_teams[i_sc2_wins_sim] & SC2_fixtures$sc2_FTR == "H" & SC2_fixtures$sc2_gamestatus == "notplayed", ])
#
# }
#
# sc2_total_wins_sim <- sc2_home_wins_sim + sc2_away_wins_sim
# sc2_total_draws_sim <- sc2_home_draws_sim + sc2_away_draws_sim
# sc2_total_loss_sim <- sc2_home_loss_sim + sc2_away_loss_sim
#
# sc2_home_games_sim <- c()
# sc2_away_games_sim <-c()
#
# for (i_sc2_sim in 1:length(sc2_teams))
# {
#
#   sc2_home_games_sim[i_sc2_sim] <- nrow(SC2_fixtures[SC2_fixtures$HomeTeam_sc2 == sc2_teams[i_sc2_sim] & SC2_fixtures$sc2_gamestatus == "notplayed",])
#   sc2_away_games_sim[i_sc2_sim]  <- nrow(SC2_fixtures[SC2_fixtures$AwayTeam_sc2 == sc2_teams[i_sc2_sim] & SC2_fixtures$sc2_gamestatus == "notplayed",])
#
# }
#
# sc2_games_played_sim <- sc2_home_games_sim + sc2_away_games_sim
#
# sc2_league_table_sim <- cbind(sc2_teams,sc2_games_played_sim,sc2_total_wins_sim,sc2_total_draws_sim,sc2_total_loss_sim)
# sc2_PTS_sim <- (sc2_total_wins_sim*3) + (sc2_total_draws_sim*1)
# sc2_league_table_sim <- cbind(sc2_league_table_sim,sc2_PTS_sim)
#
# sc2_games_played_simfinal <- sc2_games_played + sc2_games_played_sim
# sc2_total_wins_simfinal <- sc2_total_wins + sc2_total_wins_sim
# sc2_total_draws_simfinal <- sc2_total_draws + sc2_total_draws_sim
# sc2_total_loss_simfinal <- sc2_total_loss + sc2_total_loss_sim
# sc2_PTS_simfinal <- sc2_PTS + sc2_PTS_sim
#
# sc2_league_table_simfinal <- cbind(sc2_teams,sc2_games_played_simfinal,sc2_total_wins_simfinal,sc2_total_draws_simfinal,sc2_total_loss_simfinal,sc2_PTS_simfinal)
# sc2_league_table_simfinal <- as.data.frame(sc2_league_table_simfinal)
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_teams"] <- "Team_f"
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_games_played_simfinal"] <- "P_f"
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_total_wins_simfinal"] <- "W_f"
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_total_draws_simfinal"] <- "D_f"
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_total_loss_simfinal"] <- "L_f"
# names(sc2_league_table_simfinal)[names(sc2_league_table_simfinal) == "sc2_PTS_simfinal"] <- "PTS_f"
# points_sc2_sim <-  sc2_league_table_simfinal[order(as.numeric(sc2_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SC2_notplayed <- SC2_fixtures[SC2_fixtures$sc2_gamestatus == "notplayed",]
#
# write.xlsx(points_sc2,'Divisions/Simulations.xlsx', sheetName = "SC2_table",append = TRUE)
# write.xlsx(sc2_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SC2_sim",append = TRUE)
# write.xlsx(points_sc2_sim,'Divisions/Simulations.xlsx', sheetName = "SC2_simfinal",append = TRUE)
# #write.xlsx(SC2_notplayed,'Divisions/Simulations.xlsx', sheetName = "SC2_notplayed",append = TRUE)
# ###################################################################################################################################
# ###################################################################################################################
# #SC3
# SC3_sim <- SC3
# SC3_sim$matchid <- paste(SC3_sim$HomeTeam,SC3_sim$AwayTeam,sep = "-")
# SC3_fixtures$matchid <- paste(SC3_fixtures$HomeTeam_sc3,SC3_fixtures$AwayTeam_sc3,sep = "-")
# SC3_fixtures$sc3_FTR <- sapply(SC3_fixtures$sc3_pscore,switch,
#                                '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                                '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                                '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# SC3_fixtures$sc3_gamestatus <- ifelse(SC3_fixtures$matchid %in% SC3_sim$matchid,"played","notplayed")
#
# sc3_home_wins_sim <- c()
# sc3_away_wins_sim <- c()
# sc3_home_draws_sim <- c()
# sc3_away_draws_sim <- c()
# sc3_home_loss_sim <- c()
# sc3_away_loss_sim <- c()
#
#
#
# for (i_sc3_wins_sim in 1:length(sc3_teams))
# {
#
#   sc3_home_wins_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$HomeTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "H" & SC3_fixtures$sc3_gamestatus =="notplayed",])
#   sc3_away_wins_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$AwayTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "A" & SC3_fixtures$sc3_gamestatus == "notplayed",])
#   sc3_home_draws_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$HomeTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "D" & SC3_fixtures$sc3_gamestatus == "notplayed",])
#   sc3_away_draws_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$AwayTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "D" & SC3_fixtures$sc3_gamestatus == "notplayed",])
#   sc3_home_loss_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$HomeTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "A" & SC3_fixtures$sc3_gamestatus == "notplayed",])
#   sc3_away_loss_sim[i_sc3_wins_sim] <- nrow(SC3_fixtures[SC3_fixtures$AwayTeam_sc3 == sc3_teams[i_sc3_wins_sim] & SC3_fixtures$sc3_FTR == "H" & SC3_fixtures$sc3_gamestatus == "notplayed", ])
#
# }
#
# sc3_total_wins_sim <- sc3_home_wins_sim + sc3_away_wins_sim
# sc3_total_draws_sim <- sc3_home_draws_sim + sc3_away_draws_sim
# sc3_total_loss_sim <- sc3_home_loss_sim + sc3_away_loss_sim
#
# sc3_home_games_sim <- c()
# sc3_away_games_sim <-c()
#
# for (i_sc3_sim in 1:length(sc3_teams))
# {
#
#   sc3_home_games_sim[i_sc3_sim] <- nrow(SC3_fixtures[SC3_fixtures$HomeTeam_sc3 == sc3_teams[i_sc3_sim] & SC3_fixtures$sc3_gamestatus == "notplayed",])
#   sc3_away_games_sim[i_sc3_sim]  <- nrow(SC3_fixtures[SC3_fixtures$AwayTeam_sc3 == sc3_teams[i_sc3_sim] & SC3_fixtures$sc3_gamestatus == "notplayed",])
#
# }
#
# sc3_games_played_sim <- sc3_home_games_sim + sc3_away_games_sim
#
# sc3_league_table_sim <- cbind(sc3_teams,sc3_games_played_sim,sc3_total_wins_sim,sc3_total_draws_sim,sc3_total_loss_sim)
# sc3_PTS_sim <- (sc3_total_wins_sim*3) + (sc3_total_draws_sim*1)
# sc3_league_table_sim <- cbind(sc3_league_table_sim,sc3_PTS_sim)
#
# sc3_games_played_simfinal <- sc3_games_played + sc3_games_played_sim
# sc3_total_wins_simfinal <- sc3_total_wins + sc3_total_wins_sim
# sc3_total_draws_simfinal <- sc3_total_draws + sc3_total_draws_sim
# sc3_total_loss_simfinal <- sc3_total_loss + sc3_total_loss_sim
# sc3_PTS_simfinal <- sc3_PTS + sc3_PTS_sim
#
# sc3_league_table_simfinal <- cbind(sc3_teams,sc3_games_played_simfinal,sc3_total_wins_simfinal,sc3_total_draws_simfinal,sc3_total_loss_simfinal,sc3_PTS_simfinal)
# sc3_league_table_simfinal <- as.data.frame(sc3_league_table_simfinal)
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_teams"] <- "Team_f"
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_games_played_simfinal"] <- "P_f"
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_total_wins_simfinal"] <- "W_f"
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_total_draws_simfinal"] <- "D_f"
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_total_loss_simfinal"] <- "L_f"
# names(sc3_league_table_simfinal)[names(sc3_league_table_simfinal) == "sc3_PTS_simfinal"] <- "PTS_f"
# points_sc3_sim <-  sc3_league_table_simfinal[order(as.numeric(sc3_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# SC3_notplayed <- SC3_fixtures[SC3_fixtures$sc3_gamestatus == "notplayed",]
#
# write.xlsx(points_sc3,'Divisions/Simulations.xlsx', sheetName = "SC3_table",append = TRUE)
# write.xlsx(sc3_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "SC3_sim",append = TRUE)
# write.xlsx(points_sc3_sim,'Divisions/Simulations.xlsx', sheetName = "SC3_simfinal",append = TRUE)
# #write.xlsx(SC3_notplayed,'Divisions/Simulations.xlsx', sheetName = "SC3_notplayed",append = TRUE)
# ###################################################################################################################################
# ###################################################################################################################
# #T1
# T1_sim <- T1
# T1_sim$matchid <- paste(T1_sim$HomeTeam,T1_sim$AwayTeam,sep = "-")
# T1_fixtures$matchid <- paste(T1_fixtures$HomeTeam_t1,T1_fixtures$AwayTeam_t1,sep = "-")
# T1_fixtures$t1_FTR <- sapply(T1_fixtures$t1_pscore,switch,
#                              '1-0' = 'H','2-0'='H','2-1'= 'H','3-0'= 'H','3-1'= 'H','3-2'= 'H','4-0'= 'H','4-1'= 'H','4-2'= 'H','4-3'= 'H','5-0'= 'H','5-1'= 'H','5-2'= 'H','5-3'= 'H','5-4'= 'H','6-0'= 'H','6-1'= 'H','6-2'= 'H','6-3'= 'H','6-4'= 'H','6-5'= 'H','7-0'= 'H','7-2'= 'H','9-0'= 'H',
#                              '0-0' = 'D','1-1' = 'D','2-2' = 'D','3-3' = 'D','4-4' = 'D','5-5' = 'D',
#                              '0-1'= 'A','0-2' = 'A','1-2'= 'A','0-3'= 'A','1-3'= 'A','2-3'= 'A','0-4'= 'A','1-4'= 'A','2-4'= 'A','3-4'= 'A','0-5'= 'A','1-5'= 'A','2-5'= 'A','3-5'= 'A','4-5'= 'A','0-6'= 'A','1-6'= 'A','2-6'= 'A','3-6'= 'A','4-6'= 'A','3-8'= 'A','5-6'= 'A')
#
# T1_fixtures$t1_gamestatus <- ifelse(T1_fixtures$matchid %in% T1_sim$matchid,"played","notplayed")
#
# t1_home_wins_sim <- c()
# t1_away_wins_sim <- c()
# t1_home_draws_sim <- c()
# t1_away_draws_sim <- c()
# t1_home_loss_sim <- c()
# t1_away_loss_sim <- c()
#
#
#
# for (i_t1_wins_sim in 1:length(t1_teams))
# {
#
#   t1_home_wins_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$HomeTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "H" & T1_fixtures$t1_gamestatus =="notplayed",])
#   t1_away_wins_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$AwayTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "A" & T1_fixtures$t1_gamestatus == "notplayed",])
#   t1_home_draws_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$HomeTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "D" & T1_fixtures$t1_gamestatus == "notplayed",])
#   t1_away_draws_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$AwayTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "D" & T1_fixtures$t1_gamestatus == "notplayed",])
#   t1_home_loss_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$HomeTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "A" & T1_fixtures$t1_gamestatus == "notplayed",])
#   t1_away_loss_sim[i_t1_wins_sim] <- nrow(T1_fixtures[T1_fixtures$AwayTeam_t1 == t1_teams[i_t1_wins_sim] & T1_fixtures$t1_FTR == "H" & T1_fixtures$t1_gamestatus == "notplayed", ])
#
# }
#
# t1_total_wins_sim <- t1_home_wins_sim + t1_away_wins_sim
# t1_total_draws_sim <- t1_home_draws_sim + t1_away_draws_sim
# t1_total_loss_sim <- t1_home_loss_sim + t1_away_loss_sim
#
# t1_home_games_sim <- c()
# t1_away_games_sim <-c()
#
# for (i_t1_sim in 1:length(t1_teams))
# {
#
#   t1_home_games_sim[i_t1_sim] <- nrow(T1_fixtures[T1_fixtures$HomeTeam_t1 == t1_teams[i_t1_sim] & T1_fixtures$t1_gamestatus == "notplayed",])
#   t1_away_games_sim[i_t1_sim]  <- nrow(T1_fixtures[T1_fixtures$AwayTeam_t1 == t1_teams[i_t1_sim] & T1_fixtures$t1_gamestatus == "notplayed",])
#
# }
#
# t1_games_played_sim <- t1_home_games_sim + t1_away_games_sim
#
# t1_league_table_sim <- cbind(t1_teams,t1_games_played_sim,t1_total_wins_sim,t1_total_draws_sim,t1_total_loss_sim)
# t1_PTS_sim <- (t1_total_wins_sim*3) + (t1_total_draws_sim*1)
# t1_league_table_sim <- cbind(t1_league_table_sim,t1_PTS_sim)
#
# t1_games_played_simfinal <- t1_games_played + t1_games_played_sim
# t1_total_wins_simfinal <- t1_total_wins + t1_total_wins_sim
# t1_total_draws_simfinal <- t1_total_draws + t1_total_draws_sim
# t1_total_loss_simfinal <- t1_total_loss + t1_total_loss_sim
# t1_PTS_simfinal <- t1_PTS + t1_PTS_sim
#
# t1_league_table_simfinal <- cbind(t1_teams,t1_games_played_simfinal,t1_total_wins_simfinal,t1_total_draws_simfinal,t1_total_loss_simfinal,t1_PTS_simfinal)
# t1_league_table_simfinal <- as.data.frame(t1_league_table_simfinal)
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_teams"] <- "Team_f"
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_games_played_simfinal"] <- "P_f"
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_total_wins_simfinal"] <- "W_f"
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_total_draws_simfinal"] <- "D_f"
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_total_loss_simfinal"] <- "L_f"
# names(t1_league_table_simfinal)[names(t1_league_table_simfinal) == "t1_PTS_simfinal"] <- "PTS_f"
# points_t1_sim <-  t1_league_table_simfinal[order(as.numeric(t1_league_table_simfinal$PTS_f), decreasing = TRUE),]
#
# T1_notplayed <- T1_fixtures[T1_fixtures$t1_gamestatus == "notplayed",]
#
# write.xlsx(points_t1,'Divisions/Simulations.xlsx', sheetName = "T1_table",append = TRUE)
# write.xlsx(t1_league_table_sim,'Divisions/Simulations.xlsx', sheetName = "T1_sim",append = TRUE)
# write.xlsx(points_t1_sim,'Divisions/Simulations.xlsx', sheetName = "T1_simfinal",append = TRUE)
# #write.xlsx(T1_notplayed,'Divisions/Simulations.xlsx', sheetName = "T1_notplayed",append = TRUE)
# ######################################################################################################################################
#
#
# library('xlsx')
# write.xlsx(E0_notplayed,"Divisions/Notplayed.xlsx", sheetName = "E0")
# write.xlsx(ARG_notplayed,"Divisions/Notplayed.xlsx", sheetName = "ARG", append = TRUE)
























