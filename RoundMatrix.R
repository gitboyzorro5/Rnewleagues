Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')

unlink('Divisions/Roundmatrix.xlsx')
####################################################################################################
####################################################################################################
#mls
#hwins and away wins
mls_home_wins_rnds <- c()
mls_away_wins_rnds <- c()
mls_home_draws_rnds <- c()
mls_away_draws_rnds <- c()
mls_home_loss_rnds <- c()
mls_away_loss_rnds <- c()

#mls_krounds is total rounds as per current season
mls_krounds <- tail(unique(MLS_rounds$mls_matchday),1)
mls_roundmatrix <- data.frame(matrix(nrow = length(mls_teams),ncol = mls_krounds))

for(i_mls_krounds in 1:mls_krounds)
{

  for (i_mls_wins_rnds in 1:length(mls_teams))
  {

    mls_home_wins_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$HomeTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "H" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_wins_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$AwayTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "A" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_home_draws_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$HomeTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "D" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_draws_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$AwayTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "D" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_home_loss_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$HomeTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "A" & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_loss_rnds[i_mls_wins_rnds] <- nrow(MLS_rounds[MLS_rounds$AwayTeam == mls_teams[i_mls_wins_rnds] & MLS_rounds$FTR == "H" & MLS_rounds$mls_matchday <= i_mls_krounds,])

  }

  mls_total_wins_rnds <- mls_home_wins_rnds + mls_away_wins_rnds
  mls_total_draws_rnds <- mls_home_draws_rnds + mls_away_draws_rnds
  mls_total_loss_rnds <- mls_home_loss_rnds + mls_away_loss_rnds


  mls_home_games_rnds <- c()
  mls_away_games_rnds <-c()

  for (i_mls_rnds in 1:length(mls_teams))
  {

    mls_home_games_rnds[i_mls_rnds] <- nrow(MLS_rounds[MLS_rounds$HomeTeam == mls_teams[i_mls_rnds] & MLS_rounds$mls_matchday <= i_mls_krounds,])
    mls_away_games_rnds[i_mls_rnds]  <- nrow(MLS_rounds[MLS_rounds$AwayTeam == mls_teams[i_mls_rnds] & MLS_rounds$mls_matchday <= i_mls_krounds,])

  }

  mls_games_played_rnds <- mls_home_games_rnds + mls_away_games_rnds

  mls_league_table_rnds <- cbind(mls_teams,mls_games_played_rnds,mls_total_wins_rnds,mls_total_draws_rnds,mls_total_loss_rnds)

  # mls_GS <- mls_scoring$TGS
  # mls_GC <-mls_conceding$TGC
  # mls_GD <- mls_scoring$TGS - mls_conceding$TGC

  mls_PTS_rnds <- (mls_total_wins_rnds*3) + (mls_total_draws_rnds*1)
  mls_league_table_rnds <- cbind(mls_league_table_rnds,mls_PTS_rnds)
  mls_league_table_rnds <- as.data.frame(mls_league_table_rnds)
  #rename the columns
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_teams"] <- "Team"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_games_played_rnds"] <- "P"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_wins_rnds"] <- "W"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_draws_rnds"] <- "D"
  names(mls_league_table_rnds)[names(mls_league_table_rnds) == "mls_total_loss_rnds"] <- "L"
  # names(mls_league_table)[names(mls_league_table) == "mls_GS"] <- "F"
  # names(mls_league_table)[names(mls_league_table) == "mls_GC"] <- "A"
  points_mls_rnds <- mls_league_table_rnds[order(as.numeric(mls_league_table_rnds$mls_PTS_rnds), decreasing = TRUE),]
  points_mls_rnds$mls_rank_rnds <- 1:length(mls_teams)
  row.names(points_mls_rnds) <- points_mls_rnds$mls_rank


  points_mls_rnds <- points_mls_rnds[order(as.character(points_mls_rnds$Team)),]


  mls_roundmatrix[,i_mls_krounds] <- as.data.frame(points_mls_rnds$mls_rank_rnds)


}

mls_roundmatrix <- cbind(mls_teams,mls_roundmatrix)
write.xlsx(mls_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "mls",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#arg
#hwins and away wins
arg_home_wins_rnds <- c()
arg_away_wins_rnds <- c()
arg_home_draws_rnds <- c()
arg_away_draws_rnds <- c()
arg_home_loss_rnds <- c()
arg_away_loss_rnds <- c()

#arg_krounds is total rounds as per current season
arg_krounds <- tail(unique(ARG_rounds$arg_matchday),1)
arg_roundmatrix <- data.frame(matrix(nrow = length(arg_teams),ncol = arg_krounds))

for(i_arg_krounds in 1:arg_krounds)
{

  for (i_arg_wins_rnds in 1:length(arg_teams))
  {

    arg_home_wins_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$HomeTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "H" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_wins_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$AwayTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "A" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_home_draws_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$HomeTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "D" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_draws_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$AwayTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "D" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_home_loss_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$HomeTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "A" & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_loss_rnds[i_arg_wins_rnds] <- nrow(ARG_rounds[ARG_rounds$AwayTeam == arg_teams[i_arg_wins_rnds] & ARG_rounds$FTR == "H" & ARG_rounds$arg_matchday <= i_arg_krounds,])

  }

  arg_total_wins_rnds <- arg_home_wins_rnds + arg_away_wins_rnds
  arg_total_draws_rnds <- arg_home_draws_rnds + arg_away_draws_rnds
  arg_total_loss_rnds <- arg_home_loss_rnds + arg_away_loss_rnds


  arg_home_games_rnds <- c()
  arg_away_games_rnds <-c()

  for (i_arg_rnds in 1:length(arg_teams))
  {

    arg_home_games_rnds[i_arg_rnds] <- nrow(ARG_rounds[ARG_rounds$HomeTeam == arg_teams[i_arg_rnds] & ARG_rounds$arg_matchday <= i_arg_krounds,])
    arg_away_games_rnds[i_arg_rnds]  <- nrow(ARG_rounds[ARG_rounds$AwayTeam == arg_teams[i_arg_rnds] & ARG_rounds$arg_matchday <= i_arg_krounds,])

  }

  arg_games_played_rnds <- arg_home_games_rnds + arg_away_games_rnds

  arg_league_table_rnds <- cbind(arg_teams,arg_games_played_rnds,arg_total_wins_rnds,arg_total_draws_rnds,arg_total_loss_rnds)

  # arg_GS <- arg_scoring$TGS
  # arg_GC <-arg_conceding$TGC
  # arg_GD <- arg_scoring$TGS - arg_conceding$TGC

  arg_PTS_rnds <- (arg_total_wins_rnds*3) + (arg_total_draws_rnds*1)
  arg_league_table_rnds <- cbind(arg_league_table_rnds,arg_PTS_rnds)
  arg_league_table_rnds <- as.data.frame(arg_league_table_rnds)
  #rename the columns
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_teams"] <- "Team"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_games_played_rnds"] <- "P"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_wins_rnds"] <- "W"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_draws_rnds"] <- "D"
  names(arg_league_table_rnds)[names(arg_league_table_rnds) == "arg_total_loss_rnds"] <- "L"
  # names(arg_league_table)[names(arg_league_table) == "arg_GS"] <- "F"
  # names(arg_league_table)[names(arg_league_table) == "arg_GC"] <- "A"
  points_arg_rnds <- arg_league_table_rnds[order(as.numeric(arg_league_table_rnds$arg_PTS_rnds), decreasing = TRUE),]
  points_arg_rnds$arg_rank_rnds <- 1:length(arg_teams)
  row.names(points_arg_rnds) <- points_arg_rnds$arg_rank


  points_arg_rnds <- points_arg_rnds[order(as.character(points_arg_rnds$Team)),]


  arg_roundmatrix[,i_arg_krounds] <- as.data.frame(points_arg_rnds$arg_rank_rnds)


}

arg_roundmatrix <- cbind(arg_teams,arg_roundmatrix)
write.xlsx(arg_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "arg",append = TRUE)
#############################################################################################################################################
####################################################################################################
####################################################################################################
#d2
#hwins and away wins
# d2_home_wins_rnds <- c()
# d2_away_wins_rnds <- c()
# d2_home_draws_rnds <- c()
# d2_away_draws_rnds <- c()
# d2_home_loss_rnds <- c()
# d2_away_loss_rnds <- c()
#
# #d2_krounds is total rounds as per current season
# d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
# d2_roundmatrix <- data.frame(matrix(nrow = length(d2_teams),ncol = d2_krounds))
#
# for(i_d2_krounds in 1:d2_krounds)
# {
#
#   for (i_d2_wins_rnds in 1:length(d2_teams))
#   {
#
#     d2_home_wins_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$HomeTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "H" & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_away_wins_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$AwayTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "A" & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_home_draws_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$HomeTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "D" & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_away_draws_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$AwayTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "D" & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_home_loss_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$HomeTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "A" & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_away_loss_rnds[i_d2_wins_rnds] <- nrow(D2_rounds[D2_rounds$AwayTeam == d2_teams[i_d2_wins_rnds] & D2_rounds$FTR == "H" & D2_rounds$d2_matchday <= i_d2_krounds,])
#
#   }
#
#   d2_total_wins_rnds <- d2_home_wins_rnds + d2_away_wins_rnds
#   d2_total_draws_rnds <- d2_home_draws_rnds + d2_away_draws_rnds
#   d2_total_loss_rnds <- d2_home_loss_rnds + d2_away_loss_rnds
#
#
#   d2_home_games_rnds <- c()
#   d2_away_games_rnds <-c()
#
#   for (i_d2_rnds in 1:length(d2_teams))
#   {
#
#     d2_home_games_rnds[i_d2_rnds] <- nrow(D2_rounds[D2_rounds$HomeTeam == d2_teams[i_d2_rnds] & D2_rounds$d2_matchday <= i_d2_krounds,])
#     d2_away_games_rnds[i_d2_rnds]  <- nrow(D2_rounds[D2_rounds$AwayTeam == d2_teams[i_d2_rnds] & D2_rounds$d2_matchday <= i_d2_krounds,])
#
#   }
#
#   d2_games_played_rnds <- d2_home_games_rnds + d2_away_games_rnds
#
#   d2_league_table_rnds <- cbind(d2_teams,d2_games_played_rnds,d2_total_wins_rnds,d2_total_draws_rnds,d2_total_loss_rnds)
#
#   # d2_GS <- d2_scoring$TGS
#   # d2_GC <-d2_conceding$TGC
#   # d2_GD <- d2_scoring$TGS - d2_conceding$TGC
#
#   d2_PTS_rnds <- (d2_total_wins_rnds*3) + (d2_total_draws_rnds*1)
#   d2_league_table_rnds <- cbind(d2_league_table_rnds,d2_PTS_rnds)
#   d2_league_table_rnds <- as.data.frame(d2_league_table_rnds)
#   #rename the columns
#   names(d2_league_table_rnds)[names(d2_league_table_rnds) == "d2_teams"] <- "Team"
#   names(d2_league_table_rnds)[names(d2_league_table_rnds) == "d2_games_played_rnds"] <- "P"
#   names(d2_league_table_rnds)[names(d2_league_table_rnds) == "d2_total_wins_rnds"] <- "W"
#   names(d2_league_table_rnds)[names(d2_league_table_rnds) == "d2_total_draws_rnds"] <- "D"
#   names(d2_league_table_rnds)[names(d2_league_table_rnds) == "d2_total_loss_rnds"] <- "L"
#   # names(d2_league_table)[names(d2_league_table) == "d2_GS"] <- "F"
#   # names(d2_league_table)[names(d2_league_table) == "d2_GC"] <- "A"
#   points_d2_rnds <- d2_league_table_rnds[order(as.numeric(d2_league_table_rnds$d2_PTS_rnds), decreasing = TRUE),]
#   points_d2_rnds$d2_rank_rnds <- 1:length(d2_teams)
#   row.names(points_d2_rnds) <- points_d2_rnds$d2_rank
#
#
#   points_d2_rnds <- points_d2_rnds[order(as.character(points_d2_rnds$Team)),]
#
#
#   d2_roundmatrix[,i_d2_krounds] <- as.data.frame(points_d2_rnds$d2_rank_rnds)
#
#
# }
#
# d2_roundmatrix <- cbind(d2_teams,d2_roundmatrix)
# write.xlsx(d2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "d2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #e0
# #hwins and away wins
# e0_home_wins_rnds <- c()
# e0_away_wins_rnds <- c()
# e0_home_draws_rnds <- c()
# e0_away_draws_rnds <- c()
# e0_home_loss_rnds <- c()
# e0_away_loss_rnds <- c()
#
# #e0_krounds is total rounds as per current season
# e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
# e0_roundmatrix <- data.frame(matrix(nrow = length(e0_teams),ncol = e0_krounds))
#
# for(i_e0_krounds in 1:e0_krounds)
# {
#
#   for (i_e0_wins_rnds in 1:length(e0_teams))
#   {
#
#     e0_home_wins_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$HomeTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "H" & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_away_wins_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$AwayTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "A" & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_home_draws_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$HomeTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "D" & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_away_draws_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$AwayTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "D" & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_home_loss_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$HomeTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "A" & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_away_loss_rnds[i_e0_wins_rnds] <- nrow(E0_rounds[E0_rounds$AwayTeam == e0_teams[i_e0_wins_rnds] & E0_rounds$FTR == "H" & E0_rounds$e0_matchday <= i_e0_krounds,])
#
#   }
#
#   e0_total_wins_rnds <- e0_home_wins_rnds + e0_away_wins_rnds
#   e0_total_draws_rnds <- e0_home_draws_rnds + e0_away_draws_rnds
#   e0_total_loss_rnds <- e0_home_loss_rnds + e0_away_loss_rnds
#
#
#   e0_home_games_rnds <- c()
#   e0_away_games_rnds <-c()
#
#   for (i_e0_rnds in 1:length(e0_teams))
#   {
#
#     e0_home_games_rnds[i_e0_rnds] <- nrow(E0_rounds[E0_rounds$HomeTeam == e0_teams[i_e0_rnds] & E0_rounds$e0_matchday <= i_e0_krounds,])
#     e0_away_games_rnds[i_e0_rnds]  <- nrow(E0_rounds[E0_rounds$AwayTeam == e0_teams[i_e0_rnds] & E0_rounds$e0_matchday <= i_e0_krounds,])
#
#   }
#
#   e0_games_played_rnds <- e0_home_games_rnds + e0_away_games_rnds
#
#   e0_league_table_rnds <- cbind(e0_teams,e0_games_played_rnds,e0_total_wins_rnds,e0_total_draws_rnds,e0_total_loss_rnds)
#
#   # e0_GS <- e0_scoring$TGS
#   # e0_GC <-e0_conceding$TGC
#   # e0_GD <- e0_scoring$TGS - e0_conceding$TGC
#
#   e0_PTS_rnds <- (e0_total_wins_rnds*3) + (e0_total_draws_rnds*1)
#   e0_league_table_rnds <- cbind(e0_league_table_rnds,e0_PTS_rnds)
#   e0_league_table_rnds <- as.data.frame(e0_league_table_rnds)
#   #rename the columns
#   names(e0_league_table_rnds)[names(e0_league_table_rnds) == "e0_teams"] <- "Team"
#   names(e0_league_table_rnds)[names(e0_league_table_rnds) == "e0_games_played_rnds"] <- "P"
#   names(e0_league_table_rnds)[names(e0_league_table_rnds) == "e0_total_wins_rnds"] <- "W"
#   names(e0_league_table_rnds)[names(e0_league_table_rnds) == "e0_total_draws_rnds"] <- "D"
#   names(e0_league_table_rnds)[names(e0_league_table_rnds) == "e0_total_loss_rnds"] <- "L"
#   # names(e0_league_table)[names(e0_league_table) == "e0_GS"] <- "F"
#   # names(e0_league_table)[names(e0_league_table) == "e0_GC"] <- "A"
#   points_e0_rnds <- e0_league_table_rnds[order(as.numeric(e0_league_table_rnds$e0_PTS_rnds), decreasing = TRUE),]
#   points_e0_rnds$e0_rank_rnds <- 1:length(e0_teams)
#   row.names(points_e0_rnds) <- points_e0_rnds$e0_rank
#
#
#   points_e0_rnds <- points_e0_rnds[order(as.character(points_e0_rnds$Team)),]
#
#
#   e0_roundmatrix[,i_e0_krounds] <- as.data.frame(points_e0_rnds$e0_rank_rnds)
#
#
# }
#
# e0_roundmatrix <- cbind(e0_teams,e0_roundmatrix)
# write.xlsx(e0_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "e0",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #e1
# #hwins and away wins
# e1_home_wins_rnds <- c()
# e1_away_wins_rnds <- c()
# e1_home_draws_rnds <- c()
# e1_away_draws_rnds <- c()
# e1_home_loss_rnds <- c()
# e1_away_loss_rnds <- c()
#
# #e1_krounds is total rounds as per current season
# e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
# e1_roundmatrix <- data.frame(matrix(nrow = length(e1_teams),ncol = e1_krounds))
#
# for(i_e1_krounds in 1:e1_krounds)
# {
#
#   for (i_e1_wins_rnds in 1:length(e1_teams))
#   {
#
#     e1_home_wins_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$HomeTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "H" & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_away_wins_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$AwayTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "A" & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_home_draws_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$HomeTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "D" & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_away_draws_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$AwayTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "D" & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_home_loss_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$HomeTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "A" & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_away_loss_rnds[i_e1_wins_rnds] <- nrow(E1_rounds[E1_rounds$AwayTeam == e1_teams[i_e1_wins_rnds] & E1_rounds$FTR == "H" & E1_rounds$e1_matchday <= i_e1_krounds,])
#
#   }
#
#   e1_total_wins_rnds <- e1_home_wins_rnds + e1_away_wins_rnds
#   e1_total_draws_rnds <- e1_home_draws_rnds + e1_away_draws_rnds
#   e1_total_loss_rnds <- e1_home_loss_rnds + e1_away_loss_rnds
#
#
#   e1_home_games_rnds <- c()
#   e1_away_games_rnds <-c()
#
#   for (i_e1_rnds in 1:length(e1_teams))
#   {
#
#     e1_home_games_rnds[i_e1_rnds] <- nrow(E1_rounds[E1_rounds$HomeTeam == e1_teams[i_e1_rnds] & E1_rounds$e1_matchday <= i_e1_krounds,])
#     e1_away_games_rnds[i_e1_rnds]  <- nrow(E1_rounds[E1_rounds$AwayTeam == e1_teams[i_e1_rnds] & E1_rounds$e1_matchday <= i_e1_krounds,])
#
#   }
#
#   e1_games_played_rnds <- e1_home_games_rnds + e1_away_games_rnds
#
#   e1_league_table_rnds <- cbind(e1_teams,e1_games_played_rnds,e1_total_wins_rnds,e1_total_draws_rnds,e1_total_loss_rnds)
#
#   # e1_GS <- e1_scoring$TGS
#   # e1_GC <-e1_conceding$TGC
#   # e1_GD <- e1_scoring$TGS - e1_conceding$TGC
#
#   e1_PTS_rnds <- (e1_total_wins_rnds*3) + (e1_total_draws_rnds*1)
#   e1_league_table_rnds <- cbind(e1_league_table_rnds,e1_PTS_rnds)
#   e1_league_table_rnds <- as.data.frame(e1_league_table_rnds)
#   #rename the columns
#   names(e1_league_table_rnds)[names(e1_league_table_rnds) == "e1_teams"] <- "Team"
#   names(e1_league_table_rnds)[names(e1_league_table_rnds) == "e1_games_played_rnds"] <- "P"
#   names(e1_league_table_rnds)[names(e1_league_table_rnds) == "e1_total_wins_rnds"] <- "W"
#   names(e1_league_table_rnds)[names(e1_league_table_rnds) == "e1_total_draws_rnds"] <- "D"
#   names(e1_league_table_rnds)[names(e1_league_table_rnds) == "e1_total_loss_rnds"] <- "L"
#   # names(e1_league_table)[names(e1_league_table) == "e1_GS"] <- "F"
#   # names(e1_league_table)[names(e1_league_table) == "e1_GC"] <- "A"
#   points_e1_rnds <- e1_league_table_rnds[order(as.numeric(e1_league_table_rnds$e1_PTS_rnds), decreasing = TRUE),]
#   points_e1_rnds$e1_rank_rnds <- 1:length(e1_teams)
#   row.names(points_e1_rnds) <- points_e1_rnds$e1_rank
#
#
#   points_e1_rnds <- points_e1_rnds[order(as.character(points_e1_rnds$Team)),]
#
#
#   e1_roundmatrix[,i_e1_krounds] <- as.data.frame(points_e1_rnds$e1_rank_rnds)
#
#
# }
#
# e1_roundmatrix <- cbind(e1_teams,e1_roundmatrix)
# write.xlsx(e1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "e1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #e2
# #hwins and away wins
# e2_home_wins_rnds <- c()
# e2_away_wins_rnds <- c()
# e2_home_draws_rnds <- c()
# e2_away_draws_rnds <- c()
# e2_home_loss_rnds <- c()
# e2_away_loss_rnds <- c()
#
# #e2_krounds is total rounds as per current season
# e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
# e2_roundmatrix <- data.frame(matrix(nrow = length(e2_teams),ncol = e2_krounds))
#
# for(i_e2_krounds in 1:e2_krounds)
# {
#
#   for (i_e2_wins_rnds in 1:length(e2_teams))
#   {
#
#     e2_home_wins_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$HomeTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "H" & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_away_wins_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$AwayTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "A" & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_home_draws_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$HomeTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "D" & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_away_draws_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$AwayTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "D" & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_home_loss_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$HomeTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "A" & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_away_loss_rnds[i_e2_wins_rnds] <- nrow(E2_rounds[E2_rounds$AwayTeam == e2_teams[i_e2_wins_rnds] & E2_rounds$FTR == "H" & E2_rounds$e2_matchday <= i_e2_krounds,])
#
#   }
#
#   e2_total_wins_rnds <- e2_home_wins_rnds + e2_away_wins_rnds
#   e2_total_draws_rnds <- e2_home_draws_rnds + e2_away_draws_rnds
#   e2_total_loss_rnds <- e2_home_loss_rnds + e2_away_loss_rnds
#
#
#   e2_home_games_rnds <- c()
#   e2_away_games_rnds <-c()
#
#   for (i_e2_rnds in 1:length(e2_teams))
#   {
#
#     e2_home_games_rnds[i_e2_rnds] <- nrow(E2_rounds[E2_rounds$HomeTeam == e2_teams[i_e2_rnds] & E2_rounds$e2_matchday <= i_e2_krounds,])
#     e2_away_games_rnds[i_e2_rnds]  <- nrow(E2_rounds[E2_rounds$AwayTeam == e2_teams[i_e2_rnds] & E2_rounds$e2_matchday <= i_e2_krounds,])
#
#   }
#
#   e2_games_played_rnds <- e2_home_games_rnds + e2_away_games_rnds
#
#   e2_league_table_rnds <- cbind(e2_teams,e2_games_played_rnds,e2_total_wins_rnds,e2_total_draws_rnds,e2_total_loss_rnds)
#
#   # e2_GS <- e2_scoring$TGS
#   # e2_GC <-e2_conceding$TGC
#   # e2_GD <- e2_scoring$TGS - e2_conceding$TGC
#
#   e2_PTS_rnds <- (e2_total_wins_rnds*3) + (e2_total_draws_rnds*1)
#   e2_league_table_rnds <- cbind(e2_league_table_rnds,e2_PTS_rnds)
#   e2_league_table_rnds <- as.data.frame(e2_league_table_rnds)
#   #rename the columns
#   names(e2_league_table_rnds)[names(e2_league_table_rnds) == "e2_teams"] <- "Team"
#   names(e2_league_table_rnds)[names(e2_league_table_rnds) == "e2_games_played_rnds"] <- "P"
#   names(e2_league_table_rnds)[names(e2_league_table_rnds) == "e2_total_wins_rnds"] <- "W"
#   names(e2_league_table_rnds)[names(e2_league_table_rnds) == "e2_total_draws_rnds"] <- "D"
#   names(e2_league_table_rnds)[names(e2_league_table_rnds) == "e2_total_loss_rnds"] <- "L"
#   # names(e2_league_table)[names(e2_league_table) == "e2_GS"] <- "F"
#   # names(e2_league_table)[names(e2_league_table) == "e2_GC"] <- "A"
#   points_e2_rnds <- e2_league_table_rnds[order(as.numeric(e2_league_table_rnds$e2_PTS_rnds), decreasing = TRUE),]
#   points_e2_rnds$e2_rank_rnds <- 1:length(e2_teams)
#   row.names(points_e2_rnds) <- points_e2_rnds$e2_rank
#
#
#   points_e2_rnds <- points_e2_rnds[order(as.character(points_e2_rnds$Team)),]
#
#
#   e2_roundmatrix[,i_e2_krounds] <- as.data.frame(points_e2_rnds$e2_rank_rnds)
#
#
# }
#
# e2_roundmatrix <- cbind(e2_teams,e2_roundmatrix)
# write.xlsx(e2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "e2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #e3
# #hwins and away wins
# e3_home_wins_rnds <- c()
# e3_away_wins_rnds <- c()
# e3_home_draws_rnds <- c()
# e3_away_draws_rnds <- c()
# e3_home_loss_rnds <- c()
# e3_away_loss_rnds <- c()
#
# #e3_krounds is total rounds as per current season
# e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
# e3_roundmatrix <- data.frame(matrix(nrow = length(e3_teams),ncol = e3_krounds))
#
# for(i_e3_krounds in 1:e3_krounds)
# {
#
#   for (i_e3_wins_rnds in 1:length(e3_teams))
#   {
#
#     e3_home_wins_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$HomeTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "H" & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_away_wins_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$AwayTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "A" & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_home_draws_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$HomeTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "D" & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_away_draws_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$AwayTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "D" & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_home_loss_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$HomeTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "A" & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_away_loss_rnds[i_e3_wins_rnds] <- nrow(E3_rounds[E3_rounds$AwayTeam == e3_teams[i_e3_wins_rnds] & E3_rounds$FTR == "H" & E3_rounds$e3_matchday <= i_e3_krounds,])
#
#   }
#
#   e3_total_wins_rnds <- e3_home_wins_rnds + e3_away_wins_rnds
#   e3_total_draws_rnds <- e3_home_draws_rnds + e3_away_draws_rnds
#   e3_total_loss_rnds <- e3_home_loss_rnds + e3_away_loss_rnds
#
#
#   e3_home_games_rnds <- c()
#   e3_away_games_rnds <-c()
#
#   for (i_e3_rnds in 1:length(e3_teams))
#   {
#
#     e3_home_games_rnds[i_e3_rnds] <- nrow(E3_rounds[E3_rounds$HomeTeam == e3_teams[i_e3_rnds] & E3_rounds$e3_matchday <= i_e3_krounds,])
#     e3_away_games_rnds[i_e3_rnds]  <- nrow(E3_rounds[E3_rounds$AwayTeam == e3_teams[i_e3_rnds] & E3_rounds$e3_matchday <= i_e3_krounds,])
#
#   }
#
#   e3_games_played_rnds <- e3_home_games_rnds + e3_away_games_rnds
#
#   e3_league_table_rnds <- cbind(e3_teams,e3_games_played_rnds,e3_total_wins_rnds,e3_total_draws_rnds,e3_total_loss_rnds)
#
#   # e3_GS <- e3_scoring$TGS
#   # e3_GC <-e3_conceding$TGC
#   # e3_GD <- e3_scoring$TGS - e3_conceding$TGC
#
#   e3_PTS_rnds <- (e3_total_wins_rnds*3) + (e3_total_draws_rnds*1)
#   e3_league_table_rnds <- cbind(e3_league_table_rnds,e3_PTS_rnds)
#   e3_league_table_rnds <- as.data.frame(e3_league_table_rnds)
#   #rename the columns
#   names(e3_league_table_rnds)[names(e3_league_table_rnds) == "e3_teams"] <- "Team"
#   names(e3_league_table_rnds)[names(e3_league_table_rnds) == "e3_games_played_rnds"] <- "P"
#   names(e3_league_table_rnds)[names(e3_league_table_rnds) == "e3_total_wins_rnds"] <- "W"
#   names(e3_league_table_rnds)[names(e3_league_table_rnds) == "e3_total_draws_rnds"] <- "D"
#   names(e3_league_table_rnds)[names(e3_league_table_rnds) == "e3_total_loss_rnds"] <- "L"
#   # names(e3_league_table)[names(e3_league_table) == "e3_GS"] <- "F"
#   # names(e3_league_table)[names(e3_league_table) == "e3_GC"] <- "A"
#   points_e3_rnds <- e3_league_table_rnds[order(as.numeric(e3_league_table_rnds$e3_PTS_rnds), decreasing = TRUE),]
#   points_e3_rnds$e3_rank_rnds <- 1:length(e3_teams)
#   row.names(points_e3_rnds) <- points_e3_rnds$e3_rank
#
#
#   points_e3_rnds <- points_e3_rnds[order(as.character(points_e3_rnds$Team)),]
#
#
#   e3_roundmatrix[,i_e3_krounds] <- as.data.frame(points_e3_rnds$e3_rank_rnds)
#
#
# }
#
# e3_roundmatrix <- cbind(e3_teams,e3_roundmatrix)
# write.xlsx(e3_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "e3",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #ec
# #hwins and away wins
# ec_home_wins_rnds <- c()
# ec_away_wins_rnds <- c()
# ec_home_draws_rnds <- c()
# ec_away_draws_rnds <- c()
# ec_home_loss_rnds <- c()
# ec_away_loss_rnds <- c()
#
# #ec_krounds is total rounds as per current season
# ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
# ec_roundmatrix <- data.frame(matrix(nrow = length(ec_teams),ncol = ec_krounds))
#
# for(i_ec_krounds in 1:ec_krounds)
# {
#
#   for (i_ec_wins_rnds in 1:length(ec_teams))
#   {
#
#     ec_home_wins_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$HomeTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "H" & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_away_wins_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$AwayTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "A" & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_home_draws_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$HomeTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "D" & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_away_draws_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$AwayTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "D" & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_home_loss_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$HomeTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "A" & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_away_loss_rnds[i_ec_wins_rnds] <- nrow(EC_rounds[EC_rounds$AwayTeam == ec_teams[i_ec_wins_rnds] & EC_rounds$FTR == "H" & EC_rounds$ec_matchday <= i_ec_krounds,])
#
#   }
#
#   ec_total_wins_rnds <- ec_home_wins_rnds + ec_away_wins_rnds
#   ec_total_draws_rnds <- ec_home_draws_rnds + ec_away_draws_rnds
#   ec_total_loss_rnds <- ec_home_loss_rnds + ec_away_loss_rnds
#
#
#   ec_home_games_rnds <- c()
#   ec_away_games_rnds <-c()
#
#   for (i_ec_rnds in 1:length(ec_teams))
#   {
#
#     ec_home_games_rnds[i_ec_rnds] <- nrow(EC_rounds[EC_rounds$HomeTeam == ec_teams[i_ec_rnds] & EC_rounds$ec_matchday <= i_ec_krounds,])
#     ec_away_games_rnds[i_ec_rnds]  <- nrow(EC_rounds[EC_rounds$AwayTeam == ec_teams[i_ec_rnds] & EC_rounds$ec_matchday <= i_ec_krounds,])
#
#   }
#
#   ec_games_played_rnds <- ec_home_games_rnds + ec_away_games_rnds
#
#   ec_league_table_rnds <- cbind(ec_teams,ec_games_played_rnds,ec_total_wins_rnds,ec_total_draws_rnds,ec_total_loss_rnds)
#
#   # ec_GS <- ec_scoring$TGS
#   # ec_GC <-ec_conceding$TGC
#   # ec_GD <- ec_scoring$TGS - ec_conceding$TGC
#
#   ec_PTS_rnds <- (ec_total_wins_rnds*3) + (ec_total_draws_rnds*1)
#   ec_league_table_rnds <- cbind(ec_league_table_rnds,ec_PTS_rnds)
#   ec_league_table_rnds <- as.data.frame(ec_league_table_rnds)
#   #rename the columns
#   names(ec_league_table_rnds)[names(ec_league_table_rnds) == "ec_teams"] <- "Team"
#   names(ec_league_table_rnds)[names(ec_league_table_rnds) == "ec_games_played_rnds"] <- "P"
#   names(ec_league_table_rnds)[names(ec_league_table_rnds) == "ec_total_wins_rnds"] <- "W"
#   names(ec_league_table_rnds)[names(ec_league_table_rnds) == "ec_total_draws_rnds"] <- "D"
#   names(ec_league_table_rnds)[names(ec_league_table_rnds) == "ec_total_loss_rnds"] <- "L"
#   # names(ec_league_table)[names(ec_league_table) == "ec_GS"] <- "F"
#   # names(ec_league_table)[names(ec_league_table) == "ec_GC"] <- "A"
#   points_ec_rnds <- ec_league_table_rnds[order(as.numeric(ec_league_table_rnds$ec_PTS_rnds), decreasing = TRUE),]
#   points_ec_rnds$ec_rank_rnds <- 1:length(ec_teams)
#   row.names(points_ec_rnds) <- points_ec_rnds$ec_rank
#
#
#   points_ec_rnds <- points_ec_rnds[order(as.character(points_ec_rnds$Team)),]
#
#
#   ec_roundmatrix[,i_ec_krounds] <- as.data.frame(points_ec_rnds$ec_rank_rnds)
#
#
# }
#
# ec_roundmatrix <- cbind(ec_teams,ec_roundmatrix)
# write.xlsx(ec_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "ec",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #f1
# #hwins and away wins
# f1_home_wins_rnds <- c()
# f1_away_wins_rnds <- c()
# f1_home_draws_rnds <- c()
# f1_away_draws_rnds <- c()
# f1_home_loss_rnds <- c()
# f1_away_loss_rnds <- c()
#
# #f1_krounds is total rounds as per current season
# f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
# f1_roundmatrix <- data.frame(matrix(nrow = length(f1_teams),ncol = f1_krounds))
#
# for(i_f1_krounds in 1:f1_krounds)
# {
#
#   for (i_f1_wins_rnds in 1:length(f1_teams))
#   {
#
#     f1_home_wins_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$HomeTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "H" & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_away_wins_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$AwayTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "A" & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_home_draws_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$HomeTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "D" & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_away_draws_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$AwayTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "D" & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_home_loss_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$HomeTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "A" & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_away_loss_rnds[i_f1_wins_rnds] <- nrow(F1_rounds[F1_rounds$AwayTeam == f1_teams[i_f1_wins_rnds] & F1_rounds$FTR == "H" & F1_rounds$f1_matchday <= i_f1_krounds,])
#
#   }
#
#   f1_total_wins_rnds <- f1_home_wins_rnds + f1_away_wins_rnds
#   f1_total_draws_rnds <- f1_home_draws_rnds + f1_away_draws_rnds
#   f1_total_loss_rnds <- f1_home_loss_rnds + f1_away_loss_rnds
#
#
#   f1_home_games_rnds <- c()
#   f1_away_games_rnds <-c()
#
#   for (i_f1_rnds in 1:length(f1_teams))
#   {
#
#     f1_home_games_rnds[i_f1_rnds] <- nrow(F1_rounds[F1_rounds$HomeTeam == f1_teams[i_f1_rnds] & F1_rounds$f1_matchday <= i_f1_krounds,])
#     f1_away_games_rnds[i_f1_rnds]  <- nrow(F1_rounds[F1_rounds$AwayTeam == f1_teams[i_f1_rnds] & F1_rounds$f1_matchday <= i_f1_krounds,])
#
#   }
#
#   f1_games_played_rnds <- f1_home_games_rnds + f1_away_games_rnds
#
#   f1_league_table_rnds <- cbind(f1_teams,f1_games_played_rnds,f1_total_wins_rnds,f1_total_draws_rnds,f1_total_loss_rnds)
#
#   # f1_GS <- f1_scoring$TGS
#   # f1_GC <-f1_conceding$TGC
#   # f1_GD <- f1_scoring$TGS - f1_conceding$TGC
#
#   f1_PTS_rnds <- (f1_total_wins_rnds*3) + (f1_total_draws_rnds*1)
#   f1_league_table_rnds <- cbind(f1_league_table_rnds,f1_PTS_rnds)
#   f1_league_table_rnds <- as.data.frame(f1_league_table_rnds)
#   #rename the columns
#   names(f1_league_table_rnds)[names(f1_league_table_rnds) == "f1_teams"] <- "Team"
#   names(f1_league_table_rnds)[names(f1_league_table_rnds) == "f1_games_played_rnds"] <- "P"
#   names(f1_league_table_rnds)[names(f1_league_table_rnds) == "f1_total_wins_rnds"] <- "W"
#   names(f1_league_table_rnds)[names(f1_league_table_rnds) == "f1_total_draws_rnds"] <- "D"
#   names(f1_league_table_rnds)[names(f1_league_table_rnds) == "f1_total_loss_rnds"] <- "L"
#   # names(f1_league_table)[names(f1_league_table) == "f1_GS"] <- "F"
#   # names(f1_league_table)[names(f1_league_table) == "f1_GC"] <- "A"
#   points_f1_rnds <- f1_league_table_rnds[order(as.numeric(f1_league_table_rnds$f1_PTS_rnds), decreasing = TRUE),]
#   points_f1_rnds$f1_rank_rnds <- 1:length(f1_teams)
#   row.names(points_f1_rnds) <- points_f1_rnds$f1_rank
#
#
#   points_f1_rnds <- points_f1_rnds[order(as.character(points_f1_rnds$Team)),]
#
#
#   f1_roundmatrix[,i_f1_krounds] <- as.data.frame(points_f1_rnds$f1_rank_rnds)
#
#
# }
#
# f1_roundmatrix <- cbind(f1_teams,f1_roundmatrix)
# write.xlsx(f1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "f1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #f2
# #hwins and away wins
# f2_home_wins_rnds <- c()
# f2_away_wins_rnds <- c()
# f2_home_draws_rnds <- c()
# f2_away_draws_rnds <- c()
# f2_home_loss_rnds <- c()
# f2_away_loss_rnds <- c()
#
# #f2_krounds is total rounds as per current season
# f2_krounds <- tail(unique(F2_rounds$f2_matchday),1)
# f2_roundmatrix <- data.frame(matrix(nrow = length(f2_teams),ncol = f2_krounds))
#
# for(i_f2_krounds in 1:f2_krounds)
# {
#
#   for (i_f2_wins_rnds in 1:length(f2_teams))
#   {
#
#     f2_home_wins_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$HomeTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "H" & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_away_wins_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$AwayTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "A" & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_home_draws_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$HomeTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "D" & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_away_draws_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$AwayTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "D" & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_home_loss_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$HomeTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "A" & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_away_loss_rnds[i_f2_wins_rnds] <- nrow(F2_rounds[F2_rounds$AwayTeam == f2_teams[i_f2_wins_rnds] & F2_rounds$FTR == "H" & F2_rounds$f2_matchday <= i_f2_krounds,])
#
#   }
#
#   f2_total_wins_rnds <- f2_home_wins_rnds + f2_away_wins_rnds
#   f2_total_draws_rnds <- f2_home_draws_rnds + f2_away_draws_rnds
#   f2_total_loss_rnds <- f2_home_loss_rnds + f2_away_loss_rnds
#
#
#   f2_home_games_rnds <- c()
#   f2_away_games_rnds <-c()
#
#   for (i_f2_rnds in 1:length(f2_teams))
#   {
#
#     f2_home_games_rnds[i_f2_rnds] <- nrow(F2_rounds[F2_rounds$HomeTeam == f2_teams[i_f2_rnds] & F2_rounds$f2_matchday <= i_f2_krounds,])
#     f2_away_games_rnds[i_f2_rnds]  <- nrow(F2_rounds[F2_rounds$AwayTeam == f2_teams[i_f2_rnds] & F2_rounds$f2_matchday <= i_f2_krounds,])
#
#   }
#
#   f2_games_played_rnds <- f2_home_games_rnds + f2_away_games_rnds
#
#   f2_league_table_rnds <- cbind(f2_teams,f2_games_played_rnds,f2_total_wins_rnds,f2_total_draws_rnds,f2_total_loss_rnds)
#
#   # f2_GS <- f2_scoring$TGS
#   # f2_GC <-f2_conceding$TGC
#   # f2_GD <- f2_scoring$TGS - f2_conceding$TGC
#
#   f2_PTS_rnds <- (f2_total_wins_rnds*3) + (f2_total_draws_rnds*1)
#   f2_league_table_rnds <- cbind(f2_league_table_rnds,f2_PTS_rnds)
#   f2_league_table_rnds <- as.data.frame(f2_league_table_rnds)
#   #rename the columns
#   names(f2_league_table_rnds)[names(f2_league_table_rnds) == "f2_teams"] <- "Team"
#   names(f2_league_table_rnds)[names(f2_league_table_rnds) == "f2_games_played_rnds"] <- "P"
#   names(f2_league_table_rnds)[names(f2_league_table_rnds) == "f2_total_wins_rnds"] <- "W"
#   names(f2_league_table_rnds)[names(f2_league_table_rnds) == "f2_total_draws_rnds"] <- "D"
#   names(f2_league_table_rnds)[names(f2_league_table_rnds) == "f2_total_loss_rnds"] <- "L"
#   # names(f2_league_table)[names(f2_league_table) == "f2_GS"] <- "F"
#   # names(f2_league_table)[names(f2_league_table) == "f2_GC"] <- "A"
#   points_f2_rnds <- f2_league_table_rnds[order(as.numeric(f2_league_table_rnds$f2_PTS_rnds), decreasing = TRUE),]
#   points_f2_rnds$f2_rank_rnds <- 1:length(f2_teams)
#   row.names(points_f2_rnds) <- points_f2_rnds$f2_rank
#
#
#   points_f2_rnds <- points_f2_rnds[order(as.character(points_f2_rnds$Team)),]
#
#
#   f2_roundmatrix[,i_f2_krounds] <- as.data.frame(points_f2_rnds$f2_rank_rnds)
#
#
# }
#
# f2_roundmatrix <- cbind(f2_teams,f2_roundmatrix)
# write.xlsx(f2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "f2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #g1
# #hwins and away wins
# g1_home_wins_rnds <- c()
# g1_away_wins_rnds <- c()
# g1_home_draws_rnds <- c()
# g1_away_draws_rnds <- c()
# g1_home_loss_rnds <- c()
# g1_away_loss_rnds <- c()
#
# #g1_krounds is total rounds as per current season
# g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
# g1_roundmatrix <- data.frame(matrix(nrow = length(g1_teams),ncol = g1_krounds))
#
# for(i_g1_krounds in 1:g1_krounds)
# {
#
#   for (i_g1_wins_rnds in 1:length(g1_teams))
#   {
#
#     g1_home_wins_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$HomeTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "H" & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_away_wins_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$AwayTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "A" & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_home_draws_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$HomeTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "D" & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_away_draws_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$AwayTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "D" & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_home_loss_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$HomeTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "A" & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_away_loss_rnds[i_g1_wins_rnds] <- nrow(G1_rounds[G1_rounds$AwayTeam == g1_teams[i_g1_wins_rnds] & G1_rounds$FTR == "H" & G1_rounds$g1_matchday <= i_g1_krounds,])
#
#   }
#
#   g1_total_wins_rnds <- g1_home_wins_rnds + g1_away_wins_rnds
#   g1_total_draws_rnds <- g1_home_draws_rnds + g1_away_draws_rnds
#   g1_total_loss_rnds <- g1_home_loss_rnds + g1_away_loss_rnds
#
#
#   g1_home_games_rnds <- c()
#   g1_away_games_rnds <-c()
#
#   for (i_g1_rnds in 1:length(g1_teams))
#   {
#
#     g1_home_games_rnds[i_g1_rnds] <- nrow(G1_rounds[G1_rounds$HomeTeam == g1_teams[i_g1_rnds] & G1_rounds$g1_matchday <= i_g1_krounds,])
#     g1_away_games_rnds[i_g1_rnds]  <- nrow(G1_rounds[G1_rounds$AwayTeam == g1_teams[i_g1_rnds] & G1_rounds$g1_matchday <= i_g1_krounds,])
#
#   }
#
#   g1_games_played_rnds <- g1_home_games_rnds + g1_away_games_rnds
#
#   g1_league_table_rnds <- cbind(g1_teams,g1_games_played_rnds,g1_total_wins_rnds,g1_total_draws_rnds,g1_total_loss_rnds)
#
#   # g1_GS <- g1_scoring$TGS
#   # g1_GC <-g1_conceding$TGC
#   # g1_GD <- g1_scoring$TGS - g1_conceding$TGC
#
#   g1_PTS_rnds <- (g1_total_wins_rnds*3) + (g1_total_draws_rnds*1)
#   g1_league_table_rnds <- cbind(g1_league_table_rnds,g1_PTS_rnds)
#   g1_league_table_rnds <- as.data.frame(g1_league_table_rnds)
#   #rename the columns
#   names(g1_league_table_rnds)[names(g1_league_table_rnds) == "g1_teams"] <- "Team"
#   names(g1_league_table_rnds)[names(g1_league_table_rnds) == "g1_games_played_rnds"] <- "P"
#   names(g1_league_table_rnds)[names(g1_league_table_rnds) == "g1_total_wins_rnds"] <- "W"
#   names(g1_league_table_rnds)[names(g1_league_table_rnds) == "g1_total_draws_rnds"] <- "D"
#   names(g1_league_table_rnds)[names(g1_league_table_rnds) == "g1_total_loss_rnds"] <- "L"
#   # names(g1_league_table)[names(g1_league_table) == "g1_GS"] <- "F"
#   # names(g1_league_table)[names(g1_league_table) == "g1_GC"] <- "A"
#   points_g1_rnds <- g1_league_table_rnds[order(as.numeric(g1_league_table_rnds$g1_PTS_rnds), decreasing = TRUE),]
#   points_g1_rnds$g1_rank_rnds <- 1:length(g1_teams)
#   row.names(points_g1_rnds) <- points_g1_rnds$g1_rank
#
#
#   points_g1_rnds <- points_g1_rnds[order(as.character(points_g1_rnds$Team)),]
#
#
#   g1_roundmatrix[,i_g1_krounds] <- as.data.frame(points_g1_rnds$g1_rank_rnds)
#
#
# }
#
# g1_roundmatrix <- cbind(g1_teams,g1_roundmatrix)
# write.xlsx(g1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "g1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #i1
# #hwins and away wins
# i1_home_wins_rnds <- c()
# i1_away_wins_rnds <- c()
# i1_home_draws_rnds <- c()
# i1_away_draws_rnds <- c()
# i1_home_loss_rnds <- c()
# i1_away_loss_rnds <- c()
#
# #i1_krounds is total rounds as per current season
# i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
# i1_roundmatrix <- data.frame(matrix(nrow = length(i1_teams),ncol = i1_krounds))
#
# for(i_i1_krounds in 1:i1_krounds)
# {
#
#   for (i_i1_wins_rnds in 1:length(i1_teams))
#   {
#
#     i1_home_wins_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$HomeTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "H" & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_away_wins_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$AwayTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "A" & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_home_draws_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$HomeTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "D" & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_away_draws_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$AwayTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "D" & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_home_loss_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$HomeTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "A" & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_away_loss_rnds[i_i1_wins_rnds] <- nrow(I1_rounds[I1_rounds$AwayTeam == i1_teams[i_i1_wins_rnds] & I1_rounds$FTR == "H" & I1_rounds$i1_matchday <= i_i1_krounds,])
#
#   }
#
#   i1_total_wins_rnds <- i1_home_wins_rnds + i1_away_wins_rnds
#   i1_total_draws_rnds <- i1_home_draws_rnds + i1_away_draws_rnds
#   i1_total_loss_rnds <- i1_home_loss_rnds + i1_away_loss_rnds
#
#
#   i1_home_games_rnds <- c()
#   i1_away_games_rnds <-c()
#
#   for (i_i1_rnds in 1:length(i1_teams))
#   {
#
#     i1_home_games_rnds[i_i1_rnds] <- nrow(I1_rounds[I1_rounds$HomeTeam == i1_teams[i_i1_rnds] & I1_rounds$i1_matchday <= i_i1_krounds,])
#     i1_away_games_rnds[i_i1_rnds]  <- nrow(I1_rounds[I1_rounds$AwayTeam == i1_teams[i_i1_rnds] & I1_rounds$i1_matchday <= i_i1_krounds,])
#
#   }
#
#   i1_games_played_rnds <- i1_home_games_rnds + i1_away_games_rnds
#
#   i1_league_table_rnds <- cbind(i1_teams,i1_games_played_rnds,i1_total_wins_rnds,i1_total_draws_rnds,i1_total_loss_rnds)
#
#   # i1_GS <- i1_scoring$TGS
#   # i1_GC <-i1_conceding$TGC
#   # i1_GD <- i1_scoring$TGS - i1_conceding$TGC
#
#   i1_PTS_rnds <- (i1_total_wins_rnds*3) + (i1_total_draws_rnds*1)
#   i1_league_table_rnds <- cbind(i1_league_table_rnds,i1_PTS_rnds)
#   i1_league_table_rnds <- as.data.frame(i1_league_table_rnds)
#   #rename the columns
#   names(i1_league_table_rnds)[names(i1_league_table_rnds) == "i1_teams"] <- "Team"
#   names(i1_league_table_rnds)[names(i1_league_table_rnds) == "i1_games_played_rnds"] <- "P"
#   names(i1_league_table_rnds)[names(i1_league_table_rnds) == "i1_total_wins_rnds"] <- "W"
#   names(i1_league_table_rnds)[names(i1_league_table_rnds) == "i1_total_draws_rnds"] <- "D"
#   names(i1_league_table_rnds)[names(i1_league_table_rnds) == "i1_total_loss_rnds"] <- "L"
#   # names(i1_league_table)[names(i1_league_table) == "i1_GS"] <- "F"
#   # names(i1_league_table)[names(i1_league_table) == "i1_GC"] <- "A"
#   points_i1_rnds <- i1_league_table_rnds[order(as.numeric(i1_league_table_rnds$i1_PTS_rnds), decreasing = TRUE),]
#   points_i1_rnds$i1_rank_rnds <- 1:length(i1_teams)
#   row.names(points_i1_rnds) <- points_i1_rnds$i1_rank
#
#
#   points_i1_rnds <- points_i1_rnds[order(as.character(points_i1_rnds$Team)),]
#
#
#   i1_roundmatrix[,i_i1_krounds] <- as.data.frame(points_i1_rnds$i1_rank_rnds)
#
#
# }
#
# i1_roundmatrix <- cbind(i1_teams,i1_roundmatrix)
# write.xlsx(i1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "i1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #i2
# #hwins and away wins
# i2_home_wins_rnds <- c()
# i2_away_wins_rnds <- c()
# i2_home_draws_rnds <- c()
# i2_away_draws_rnds <- c()
# i2_home_loss_rnds <- c()
# i2_away_loss_rnds <- c()
#
# #i2_krounds is total rounds as per current season
# i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
# i2_roundmatrix <- data.frame(matrix(nrow = length(i2_teams),ncol = i2_krounds))
#
# for(i_i2_krounds in 1:i2_krounds)
# {
#
#   for (i_i2_wins_rnds in 1:length(i2_teams))
#   {
#
#     i2_home_wins_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$HomeTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "H" & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_away_wins_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$AwayTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "A" & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_home_draws_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$HomeTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "D" & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_away_draws_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$AwayTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "D" & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_home_loss_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$HomeTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "A" & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_away_loss_rnds[i_i2_wins_rnds] <- nrow(I2_rounds[I2_rounds$AwayTeam == i2_teams[i_i2_wins_rnds] & I2_rounds$FTR == "H" & I2_rounds$i2_matchday <= i_i2_krounds,])
#
#   }
#
#   i2_total_wins_rnds <- i2_home_wins_rnds + i2_away_wins_rnds
#   i2_total_draws_rnds <- i2_home_draws_rnds + i2_away_draws_rnds
#   i2_total_loss_rnds <- i2_home_loss_rnds + i2_away_loss_rnds
#
#
#   i2_home_games_rnds <- c()
#   i2_away_games_rnds <-c()
#
#   for (i_i2_rnds in 1:length(i2_teams))
#   {
#
#     i2_home_games_rnds[i_i2_rnds] <- nrow(I2_rounds[I2_rounds$HomeTeam == i2_teams[i_i2_rnds] & I2_rounds$i2_matchday <= i_i2_krounds,])
#     i2_away_games_rnds[i_i2_rnds]  <- nrow(I2_rounds[I2_rounds$AwayTeam == i2_teams[i_i2_rnds] & I2_rounds$i2_matchday <= i_i2_krounds,])
#
#   }
#
#   i2_games_played_rnds <- i2_home_games_rnds + i2_away_games_rnds
#
#   i2_league_table_rnds <- cbind(i2_teams,i2_games_played_rnds,i2_total_wins_rnds,i2_total_draws_rnds,i2_total_loss_rnds)
#
#   # i2_GS <- i2_scoring$TGS
#   # i2_GC <-i2_conceding$TGC
#   # i2_GD <- i2_scoring$TGS - i2_conceding$TGC
#
#   i2_PTS_rnds <- (i2_total_wins_rnds*3) + (i2_total_draws_rnds*1)
#   i2_league_table_rnds <- cbind(i2_league_table_rnds,i2_PTS_rnds)
#   i2_league_table_rnds <- as.data.frame(i2_league_table_rnds)
#   #rename the columns
#   names(i2_league_table_rnds)[names(i2_league_table_rnds) == "i2_teams"] <- "Team"
#   names(i2_league_table_rnds)[names(i2_league_table_rnds) == "i2_games_played_rnds"] <- "P"
#   names(i2_league_table_rnds)[names(i2_league_table_rnds) == "i2_total_wins_rnds"] <- "W"
#   names(i2_league_table_rnds)[names(i2_league_table_rnds) == "i2_total_draws_rnds"] <- "D"
#   names(i2_league_table_rnds)[names(i2_league_table_rnds) == "i2_total_loss_rnds"] <- "L"
#   # names(i2_league_table)[names(i2_league_table) == "i2_GS"] <- "F"
#   # names(i2_league_table)[names(i2_league_table) == "i2_GC"] <- "A"
#   points_i2_rnds <- i2_league_table_rnds[order(as.numeric(i2_league_table_rnds$i2_PTS_rnds), decreasing = TRUE),]
#   points_i2_rnds$i2_rank_rnds <- 1:length(i2_teams)
#   row.names(points_i2_rnds) <- points_i2_rnds$i2_rank
#
#
#   points_i2_rnds <- points_i2_rnds[order(as.character(points_i2_rnds$Team)),]
#
#
#   i2_roundmatrix[,i_i2_krounds] <- as.data.frame(points_i2_rnds$i2_rank_rnds)
#
#
# }
#
# i2_roundmatrix <- cbind(i2_teams,i2_roundmatrix)
# write.xlsx(i2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "i2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #n1
# #hwins and away wins
# n1_home_wins_rnds <- c()
# n1_away_wins_rnds <- c()
# n1_home_draws_rnds <- c()
# n1_away_draws_rnds <- c()
# n1_home_loss_rnds <- c()
# n1_away_loss_rnds <- c()
#
# #n1_krounds is total rounds as per current season
# n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
# n1_roundmatrix <- data.frame(matrix(nrow = length(n1_teams),ncol = n1_krounds))
#
# for(i_n1_krounds in 1:n1_krounds)
# {
#
#   for (i_n1_wins_rnds in 1:length(n1_teams))
#   {
#
#     n1_home_wins_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$HomeTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "H" & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_away_wins_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$AwayTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "A" & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_home_draws_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$HomeTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "D" & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_away_draws_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$AwayTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "D" & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_home_loss_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$HomeTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "A" & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_away_loss_rnds[i_n1_wins_rnds] <- nrow(N1_rounds[N1_rounds$AwayTeam == n1_teams[i_n1_wins_rnds] & N1_rounds$FTR == "H" & N1_rounds$n1_matchday <= i_n1_krounds,])
#
#   }
#
#   n1_total_wins_rnds <- n1_home_wins_rnds + n1_away_wins_rnds
#   n1_total_draws_rnds <- n1_home_draws_rnds + n1_away_draws_rnds
#   n1_total_loss_rnds <- n1_home_loss_rnds + n1_away_loss_rnds
#
#
#   n1_home_games_rnds <- c()
#   n1_away_games_rnds <-c()
#
#   for (i_n1_rnds in 1:length(n1_teams))
#   {
#
#     n1_home_games_rnds[i_n1_rnds] <- nrow(N1_rounds[N1_rounds$HomeTeam == n1_teams[i_n1_rnds] & N1_rounds$n1_matchday <= i_n1_krounds,])
#     n1_away_games_rnds[i_n1_rnds]  <- nrow(N1_rounds[N1_rounds$AwayTeam == n1_teams[i_n1_rnds] & N1_rounds$n1_matchday <= i_n1_krounds,])
#
#   }
#
#   n1_games_played_rnds <- n1_home_games_rnds + n1_away_games_rnds
#
#   n1_league_table_rnds <- cbind(n1_teams,n1_games_played_rnds,n1_total_wins_rnds,n1_total_draws_rnds,n1_total_loss_rnds)
#
#   # n1_GS <- n1_scoring$TGS
#   # n1_GC <-n1_conceding$TGC
#   # n1_GD <- n1_scoring$TGS - n1_conceding$TGC
#
#   n1_PTS_rnds <- (n1_total_wins_rnds*3) + (n1_total_draws_rnds*1)
#   n1_league_table_rnds <- cbind(n1_league_table_rnds,n1_PTS_rnds)
#   n1_league_table_rnds <- as.data.frame(n1_league_table_rnds)
#   #rename the columns
#   names(n1_league_table_rnds)[names(n1_league_table_rnds) == "n1_teams"] <- "Team"
#   names(n1_league_table_rnds)[names(n1_league_table_rnds) == "n1_games_played_rnds"] <- "P"
#   names(n1_league_table_rnds)[names(n1_league_table_rnds) == "n1_total_wins_rnds"] <- "W"
#   names(n1_league_table_rnds)[names(n1_league_table_rnds) == "n1_total_draws_rnds"] <- "D"
#   names(n1_league_table_rnds)[names(n1_league_table_rnds) == "n1_total_loss_rnds"] <- "L"
#   # names(n1_league_table)[names(n1_league_table) == "n1_GS"] <- "F"
#   # names(n1_league_table)[names(n1_league_table) == "n1_GC"] <- "A"
#   points_n1_rnds <- n1_league_table_rnds[order(as.numeric(n1_league_table_rnds$n1_PTS_rnds), decreasing = TRUE),]
#   points_n1_rnds$n1_rank_rnds <- 1:length(n1_teams)
#   row.names(points_n1_rnds) <- points_n1_rnds$n1_rank
#
#
#   points_n1_rnds <- points_n1_rnds[order(as.character(points_n1_rnds$Team)),]
#
#
#   n1_roundmatrix[,i_n1_krounds] <- as.data.frame(points_n1_rnds$n1_rank_rnds)
#
#
# }
#
# n1_roundmatrix <- cbind(n1_teams,n1_roundmatrix)
# write.xlsx(n1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "n1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #p1
# #hwins and away wins
# p1_home_wins_rnds <- c()
# p1_away_wins_rnds <- c()
# p1_home_draws_rnds <- c()
# p1_away_draws_rnds <- c()
# p1_home_loss_rnds <- c()
# p1_away_loss_rnds <- c()
#
# #p1_krounds is total rounds as per current season
# p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
# p1_roundmatrix <- data.frame(matrix(nrow = length(p1_teams),ncol = p1_krounds))
#
# for(i_p1_krounds in 1:p1_krounds)
# {
#
#   for (i_p1_wins_rnds in 1:length(p1_teams))
#   {
#
#     p1_home_wins_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$HomeTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "H" & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_away_wins_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$AwayTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "A" & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_home_draws_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$HomeTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "D" & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_away_draws_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$AwayTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "D" & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_home_loss_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$HomeTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "A" & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_away_loss_rnds[i_p1_wins_rnds] <- nrow(P1_rounds[P1_rounds$AwayTeam == p1_teams[i_p1_wins_rnds] & P1_rounds$FTR == "H" & P1_rounds$p1_matchday <= i_p1_krounds,])
#
#   }
#
#   p1_total_wins_rnds <- p1_home_wins_rnds + p1_away_wins_rnds
#   p1_total_draws_rnds <- p1_home_draws_rnds + p1_away_draws_rnds
#   p1_total_loss_rnds <- p1_home_loss_rnds + p1_away_loss_rnds
#
#
#   p1_home_games_rnds <- c()
#   p1_away_games_rnds <-c()
#
#   for (i_p1_rnds in 1:length(p1_teams))
#   {
#
#     p1_home_games_rnds[i_p1_rnds] <- nrow(P1_rounds[P1_rounds$HomeTeam == p1_teams[i_p1_rnds] & P1_rounds$p1_matchday <= i_p1_krounds,])
#     p1_away_games_rnds[i_p1_rnds]  <- nrow(P1_rounds[P1_rounds$AwayTeam == p1_teams[i_p1_rnds] & P1_rounds$p1_matchday <= i_p1_krounds,])
#
#   }
#
#   p1_games_played_rnds <- p1_home_games_rnds + p1_away_games_rnds
#
#   p1_league_table_rnds <- cbind(p1_teams,p1_games_played_rnds,p1_total_wins_rnds,p1_total_draws_rnds,p1_total_loss_rnds)
#
#   # p1_GS <- p1_scoring$TGS
#   # p1_GC <-p1_conceding$TGC
#   # p1_GD <- p1_scoring$TGS - p1_conceding$TGC
#
#   p1_PTS_rnds <- (p1_total_wins_rnds*3) + (p1_total_draws_rnds*1)
#   p1_league_table_rnds <- cbind(p1_league_table_rnds,p1_PTS_rnds)
#   p1_league_table_rnds <- as.data.frame(p1_league_table_rnds)
#   #rename the columns
#   names(p1_league_table_rnds)[names(p1_league_table_rnds) == "p1_teams"] <- "Team"
#   names(p1_league_table_rnds)[names(p1_league_table_rnds) == "p1_games_played_rnds"] <- "P"
#   names(p1_league_table_rnds)[names(p1_league_table_rnds) == "p1_total_wins_rnds"] <- "W"
#   names(p1_league_table_rnds)[names(p1_league_table_rnds) == "p1_total_draws_rnds"] <- "D"
#   names(p1_league_table_rnds)[names(p1_league_table_rnds) == "p1_total_loss_rnds"] <- "L"
#   # names(p1_league_table)[names(p1_league_table) == "p1_GS"] <- "F"
#   # names(p1_league_table)[names(p1_league_table) == "p1_GC"] <- "A"
#   points_p1_rnds <- p1_league_table_rnds[order(as.numeric(p1_league_table_rnds$p1_PTS_rnds), decreasing = TRUE),]
#   points_p1_rnds$p1_rank_rnds <- 1:length(p1_teams)
#   row.names(points_p1_rnds) <- points_p1_rnds$p1_rank
#
#
#   points_p1_rnds <- points_p1_rnds[order(as.character(points_p1_rnds$Team)),]
#
#
#   p1_roundmatrix[,i_p1_krounds] <- as.data.frame(points_p1_rnds$p1_rank_rnds)
#
#
# }
#
# p1_roundmatrix <- cbind(p1_teams,p1_roundmatrix)
# write.xlsx(p1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "p1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sp1
# #hwins and away wins
# sp1_home_wins_rnds <- c()
# sp1_away_wins_rnds <- c()
# sp1_home_draws_rnds <- c()
# sp1_away_draws_rnds <- c()
# sp1_home_loss_rnds <- c()
# sp1_away_loss_rnds <- c()
#
# #sp1_krounds is total rounds as per current season
# sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
# sp1_roundmatrix <- data.frame(matrix(nrow = length(sp1_teams),ncol = sp1_krounds))
#
# for(i_sp1_krounds in 1:sp1_krounds)
# {
#
#   for (i_sp1_wins_rnds in 1:length(sp1_teams))
#   {
#
#     sp1_home_wins_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$HomeTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "H" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_away_wins_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$AwayTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "A" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_home_draws_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$HomeTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "D" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_away_draws_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$AwayTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "D" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_home_loss_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$HomeTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "A" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_away_loss_rnds[i_sp1_wins_rnds] <- nrow(SP1_rounds[SP1_rounds$AwayTeam == sp1_teams[i_sp1_wins_rnds] & SP1_rounds$FTR == "H" & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#
#   }
#
#   sp1_total_wins_rnds <- sp1_home_wins_rnds + sp1_away_wins_rnds
#   sp1_total_draws_rnds <- sp1_home_draws_rnds + sp1_away_draws_rnds
#   sp1_total_loss_rnds <- sp1_home_loss_rnds + sp1_away_loss_rnds
#
#
#   sp1_home_games_rnds <- c()
#   sp1_away_games_rnds <-c()
#
#   for (i_sp1_rnds in 1:length(sp1_teams))
#   {
#
#     sp1_home_games_rnds[i_sp1_rnds] <- nrow(SP1_rounds[SP1_rounds$HomeTeam == sp1_teams[i_sp1_rnds] & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#     sp1_away_games_rnds[i_sp1_rnds]  <- nrow(SP1_rounds[SP1_rounds$AwayTeam == sp1_teams[i_sp1_rnds] & SP1_rounds$sp1_matchday <= i_sp1_krounds,])
#
#   }
#
#   sp1_games_played_rnds <- sp1_home_games_rnds + sp1_away_games_rnds
#
#   sp1_league_table_rnds <- cbind(sp1_teams,sp1_games_played_rnds,sp1_total_wins_rnds,sp1_total_draws_rnds,sp1_total_loss_rnds)
#
#   # sp1_GS <- sp1_scoring$TGS
#   # sp1_GC <-sp1_conceding$TGC
#   # sp1_GD <- sp1_scoring$TGS - sp1_conceding$TGC
#
#   sp1_PTS_rnds <- (sp1_total_wins_rnds*3) + (sp1_total_draws_rnds*1)
#   sp1_league_table_rnds <- cbind(sp1_league_table_rnds,sp1_PTS_rnds)
#   sp1_league_table_rnds <- as.data.frame(sp1_league_table_rnds)
#   #rename the columns
#   names(sp1_league_table_rnds)[names(sp1_league_table_rnds) == "sp1_teams"] <- "Team"
#   names(sp1_league_table_rnds)[names(sp1_league_table_rnds) == "sp1_games_played_rnds"] <- "P"
#   names(sp1_league_table_rnds)[names(sp1_league_table_rnds) == "sp1_total_wins_rnds"] <- "W"
#   names(sp1_league_table_rnds)[names(sp1_league_table_rnds) == "sp1_total_draws_rnds"] <- "D"
#   names(sp1_league_table_rnds)[names(sp1_league_table_rnds) == "sp1_total_loss_rnds"] <- "L"
#   # names(sp1_league_table)[names(sp1_league_table) == "sp1_GS"] <- "F"
#   # names(sp1_league_table)[names(sp1_league_table) == "sp1_GC"] <- "A"
#   points_sp1_rnds <- sp1_league_table_rnds[order(as.numeric(sp1_league_table_rnds$sp1_PTS_rnds), decreasing = TRUE),]
#   points_sp1_rnds$sp1_rank_rnds <- 1:length(sp1_teams)
#   row.names(points_sp1_rnds) <- points_sp1_rnds$sp1_rank
#
#
#   points_sp1_rnds <- points_sp1_rnds[order(as.character(points_sp1_rnds$Team)),]
#
#
#   sp1_roundmatrix[,i_sp1_krounds] <- as.data.frame(points_sp1_rnds$sp1_rank_rnds)
#
#
# }
#
# sp1_roundmatrix <- cbind(sp1_teams,sp1_roundmatrix)
# write.xlsx(sp1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sp1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sp2
# #hwins and away wins
# sp2_home_wins_rnds <- c()
# sp2_away_wins_rnds <- c()
# sp2_home_draws_rnds <- c()
# sp2_away_draws_rnds <- c()
# sp2_home_loss_rnds <- c()
# sp2_away_loss_rnds <- c()
#
# #sp2_krounds is total rounds as per current season
# sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
# sp2_roundmatrix <- data.frame(matrix(nrow = length(sp2_teams),ncol = sp2_krounds))
#
# for(i_sp2_krounds in 1:sp2_krounds)
# {
#
#   for (i_sp2_wins_rnds in 1:length(sp2_teams))
#   {
#
#     sp2_home_wins_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "H" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_away_wins_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "A" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_home_draws_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "D" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_away_draws_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "D" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_home_loss_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "A" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_away_loss_rnds[i_sp2_wins_rnds] <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_wins_rnds] & SP2_rounds$FTR == "H" & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#
#   }
#
#   sp2_total_wins_rnds <- sp2_home_wins_rnds + sp2_away_wins_rnds
#   sp2_total_draws_rnds <- sp2_home_draws_rnds + sp2_away_draws_rnds
#   sp2_total_loss_rnds <- sp2_home_loss_rnds + sp2_away_loss_rnds
#
#
#   sp2_home_games_rnds <- c()
#   sp2_away_games_rnds <-c()
#
#   for (i_sp2_rnds in 1:length(sp2_teams))
#   {
#
#     sp2_home_games_rnds[i_sp2_rnds] <- nrow(SP2_rounds[SP2_rounds$HomeTeam == sp2_teams[i_sp2_rnds] & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#     sp2_away_games_rnds[i_sp2_rnds]  <- nrow(SP2_rounds[SP2_rounds$AwayTeam == sp2_teams[i_sp2_rnds] & SP2_rounds$sp2_matchday <= i_sp2_krounds,])
#
#   }
#
#   sp2_games_played_rnds <- sp2_home_games_rnds + sp2_away_games_rnds
#
#   sp2_league_table_rnds <- cbind(sp2_teams,sp2_games_played_rnds,sp2_total_wins_rnds,sp2_total_draws_rnds,sp2_total_loss_rnds)
#
#   # sp2_GS <- sp2_scoring$TGS
#   # sp2_GC <-sp2_conceding$TGC
#   # sp2_GD <- sp2_scoring$TGS - sp2_conceding$TGC
#
#   sp2_PTS_rnds <- (sp2_total_wins_rnds*3) + (sp2_total_draws_rnds*1)
#   sp2_league_table_rnds <- cbind(sp2_league_table_rnds,sp2_PTS_rnds)
#   sp2_league_table_rnds <- as.data.frame(sp2_league_table_rnds)
#   #rename the columns
#   names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_teams"] <- "Team"
#   names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_games_played_rnds"] <- "P"
#   names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_wins_rnds"] <- "W"
#   names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_draws_rnds"] <- "D"
#   names(sp2_league_table_rnds)[names(sp2_league_table_rnds) == "sp2_total_loss_rnds"] <- "L"
#   # names(sp2_league_table)[names(sp2_league_table) == "sp2_GS"] <- "F"
#   # names(sp2_league_table)[names(sp2_league_table) == "sp2_GC"] <- "A"
#   points_sp2_rnds <- sp2_league_table_rnds[order(as.numeric(sp2_league_table_rnds$sp2_PTS_rnds), decreasing = TRUE),]
#   points_sp2_rnds$sp2_rank_rnds <- 1:length(sp2_teams)
#   row.names(points_sp2_rnds) <- points_sp2_rnds$sp2_rank
#
#
#   points_sp2_rnds <- points_sp2_rnds[order(as.character(points_sp2_rnds$Team)),]
#
#
#   sp2_roundmatrix[,i_sp2_krounds] <- as.data.frame(points_sp2_rnds$sp2_rank_rnds)
#
#
# }
#
# sp2_roundmatrix <- cbind(sp2_teams,sp2_roundmatrix)
# write.xlsx(sp2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sp2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sc0
# #hwins and away wins
# sc0_home_wins_rnds <- c()
# sc0_away_wins_rnds <- c()
# sc0_home_draws_rnds <- c()
# sc0_away_draws_rnds <- c()
# sc0_home_loss_rnds <- c()
# sc0_away_loss_rnds <- c()
#
# #sc0_krounds is total rounds as per current season
# sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
# sc0_roundmatrix <- data.frame(matrix(nrow = length(sc0_teams),ncol = sc0_krounds))
#
# for(i_sc0_krounds in 1:sc0_krounds)
# {
#
#   for (i_sc0_wins_rnds in 1:length(sc0_teams))
#   {
#
#     sc0_home_wins_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "H" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_away_wins_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "A" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_home_draws_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "D" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_away_draws_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "D" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_home_loss_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "A" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_away_loss_rnds[i_sc0_wins_rnds] <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_wins_rnds] & SC0_rounds$FTR == "H" & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#
#   }
#
#   sc0_total_wins_rnds <- sc0_home_wins_rnds + sc0_away_wins_rnds
#   sc0_total_draws_rnds <- sc0_home_draws_rnds + sc0_away_draws_rnds
#   sc0_total_loss_rnds <- sc0_home_loss_rnds + sc0_away_loss_rnds
#
#
#   sc0_home_games_rnds <- c()
#   sc0_away_games_rnds <-c()
#
#   for (i_sc0_rnds in 1:length(sc0_teams))
#   {
#
#     sc0_home_games_rnds[i_sc0_rnds] <- nrow(SC0_rounds[SC0_rounds$HomeTeam == sc0_teams[i_sc0_rnds] & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#     sc0_away_games_rnds[i_sc0_rnds]  <- nrow(SC0_rounds[SC0_rounds$AwayTeam == sc0_teams[i_sc0_rnds] & SC0_rounds$sc0_matchday <= i_sc0_krounds,])
#
#   }
#
#   sc0_games_played_rnds <- sc0_home_games_rnds + sc0_away_games_rnds
#
#   sc0_league_table_rnds <- cbind(sc0_teams,sc0_games_played_rnds,sc0_total_wins_rnds,sc0_total_draws_rnds,sc0_total_loss_rnds)
#
#   # sc0_GS <- sc0_scoring$TGS
#   # sc0_GC <-sc0_conceding$TGC
#   # sc0_GD <- sc0_scoring$TGS - sc0_conceding$TGC
#
#   sc0_PTS_rnds <- (sc0_total_wins_rnds*3) + (sc0_total_draws_rnds*1)
#   sc0_league_table_rnds <- cbind(sc0_league_table_rnds,sc0_PTS_rnds)
#   sc0_league_table_rnds <- as.data.frame(sc0_league_table_rnds)
#   #rename the columns
#   names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_teams"] <- "Team"
#   names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_games_played_rnds"] <- "P"
#   names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_wins_rnds"] <- "W"
#   names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_draws_rnds"] <- "D"
#   names(sc0_league_table_rnds)[names(sc0_league_table_rnds) == "sc0_total_loss_rnds"] <- "L"
#   # names(sc0_league_table)[names(sc0_league_table) == "sc0_GS"] <- "F"
#   # names(sc0_league_table)[names(sc0_league_table) == "sc0_GC"] <- "A"
#   points_sc0_rnds <- sc0_league_table_rnds[order(as.numeric(sc0_league_table_rnds$sc0_PTS_rnds), decreasing = TRUE),]
#   points_sc0_rnds$sc0_rank_rnds <- 1:length(sc0_teams)
#   row.names(points_sc0_rnds) <- points_sc0_rnds$sc0_rank
#
#
#   points_sc0_rnds <- points_sc0_rnds[order(as.character(points_sc0_rnds$Team)),]
#
#
#   sc0_roundmatrix[,i_sc0_krounds] <- as.data.frame(points_sc0_rnds$sc0_rank_rnds)
#
#
# }
#
# sc0_roundmatrix <- cbind(sc0_teams,sc0_roundmatrix)
# write.xlsx(sc0_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sc0",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sc1
# #hwins and away wins
# sc1_home_wins_rnds <- c()
# sc1_away_wins_rnds <- c()
# sc1_home_draws_rnds <- c()
# sc1_away_draws_rnds <- c()
# sc1_home_loss_rnds <- c()
# sc1_away_loss_rnds <- c()
#
# #sc1_krounds is total rounds as per current season
# sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
# sc1_roundmatrix <- data.frame(matrix(nrow = length(sc1_teams),ncol = sc1_krounds))
#
# for(i_sc1_krounds in 1:sc1_krounds)
# {
#
#   for (i_sc1_wins_rnds in 1:length(sc1_teams))
#   {
#
#     sc1_home_wins_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "H" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_away_wins_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "A" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_home_draws_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "D" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_away_draws_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "D" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_home_loss_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "A" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_away_loss_rnds[i_sc1_wins_rnds] <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_wins_rnds] & SC1_rounds$FTR == "H" & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#
#   }
#
#   sc1_total_wins_rnds <- sc1_home_wins_rnds + sc1_away_wins_rnds
#   sc1_total_draws_rnds <- sc1_home_draws_rnds + sc1_away_draws_rnds
#   sc1_total_loss_rnds <- sc1_home_loss_rnds + sc1_away_loss_rnds
#
#
#   sc1_home_games_rnds <- c()
#   sc1_away_games_rnds <-c()
#
#   for (i_sc1_rnds in 1:length(sc1_teams))
#   {
#
#     sc1_home_games_rnds[i_sc1_rnds] <- nrow(SC1_rounds[SC1_rounds$HomeTeam == sc1_teams[i_sc1_rnds] & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#     sc1_away_games_rnds[i_sc1_rnds]  <- nrow(SC1_rounds[SC1_rounds$AwayTeam == sc1_teams[i_sc1_rnds] & SC1_rounds$sc1_matchday <= i_sc1_krounds,])
#
#   }
#
#   sc1_games_played_rnds <- sc1_home_games_rnds + sc1_away_games_rnds
#
#   sc1_league_table_rnds <- cbind(sc1_teams,sc1_games_played_rnds,sc1_total_wins_rnds,sc1_total_draws_rnds,sc1_total_loss_rnds)
#
#   # sc1_GS <- sc1_scoring$TGS
#   # sc1_GC <-sc1_conceding$TGC
#   # sc1_GD <- sc1_scoring$TGS - sc1_conceding$TGC
#
#   sc1_PTS_rnds <- (sc1_total_wins_rnds*3) + (sc1_total_draws_rnds*1)
#   sc1_league_table_rnds <- cbind(sc1_league_table_rnds,sc1_PTS_rnds)
#   sc1_league_table_rnds <- as.data.frame(sc1_league_table_rnds)
#   #rename the columns
#   names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_teams"] <- "Team"
#   names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_games_played_rnds"] <- "P"
#   names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_wins_rnds"] <- "W"
#   names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_draws_rnds"] <- "D"
#   names(sc1_league_table_rnds)[names(sc1_league_table_rnds) == "sc1_total_loss_rnds"] <- "L"
#   # names(sc1_league_table)[names(sc1_league_table) == "sc1_GS"] <- "F"
#   # names(sc1_league_table)[names(sc1_league_table) == "sc1_GC"] <- "A"
#   points_sc1_rnds <- sc1_league_table_rnds[order(as.numeric(sc1_league_table_rnds$sc1_PTS_rnds), decreasing = TRUE),]
#   points_sc1_rnds$sc1_rank_rnds <- 1:length(sc1_teams)
#   row.names(points_sc1_rnds) <- points_sc1_rnds$sc1_rank
#
#
#   points_sc1_rnds <- points_sc1_rnds[order(as.character(points_sc1_rnds$Team)),]
#
#
#   sc1_roundmatrix[,i_sc1_krounds] <- as.data.frame(points_sc1_rnds$sc1_rank_rnds)
#
#
# }
#
# sc1_roundmatrix <- cbind(sc1_teams,sc1_roundmatrix)
# write.xlsx(sc1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sc1",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sc2
# #hwins and away wins
# sc2_home_wins_rnds <- c()
# sc2_away_wins_rnds <- c()
# sc2_home_draws_rnds <- c()
# sc2_away_draws_rnds <- c()
# sc2_home_loss_rnds <- c()
# sc2_away_loss_rnds <- c()
#
# #sc2_krounds is total rounds as per current season
# sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
# sc2_roundmatrix <- data.frame(matrix(nrow = length(sc2_teams),ncol = sc2_krounds))
#
# for(i_sc2_krounds in 1:sc2_krounds)
# {
#
#   for (i_sc2_wins_rnds in 1:length(sc2_teams))
#   {
#
#     sc2_home_wins_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "H" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_away_wins_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "A" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_home_draws_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "D" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_away_draws_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "D" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_home_loss_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "A" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_away_loss_rnds[i_sc2_wins_rnds] <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_wins_rnds] & SC2_rounds$FTR == "H" & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#
#   }
#
#   sc2_total_wins_rnds <- sc2_home_wins_rnds + sc2_away_wins_rnds
#   sc2_total_draws_rnds <- sc2_home_draws_rnds + sc2_away_draws_rnds
#   sc2_total_loss_rnds <- sc2_home_loss_rnds + sc2_away_loss_rnds
#
#
#   sc2_home_games_rnds <- c()
#   sc2_away_games_rnds <-c()
#
#   for (i_sc2_rnds in 1:length(sc2_teams))
#   {
#
#     sc2_home_games_rnds[i_sc2_rnds] <- nrow(SC2_rounds[SC2_rounds$HomeTeam == sc2_teams[i_sc2_rnds] & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#     sc2_away_games_rnds[i_sc2_rnds]  <- nrow(SC2_rounds[SC2_rounds$AwayTeam == sc2_teams[i_sc2_rnds] & SC2_rounds$sc2_matchday <= i_sc2_krounds,])
#
#   }
#
#   sc2_games_played_rnds <- sc2_home_games_rnds + sc2_away_games_rnds
#
#   sc2_league_table_rnds <- cbind(sc2_teams,sc2_games_played_rnds,sc2_total_wins_rnds,sc2_total_draws_rnds,sc2_total_loss_rnds)
#
#   # sc2_GS <- sc2_scoring$TGS
#   # sc2_GC <-sc2_conceding$TGC
#   # sc2_GD <- sc2_scoring$TGS - sc2_conceding$TGC
#
#   sc2_PTS_rnds <- (sc2_total_wins_rnds*3) + (sc2_total_draws_rnds*1)
#   sc2_league_table_rnds <- cbind(sc2_league_table_rnds,sc2_PTS_rnds)
#   sc2_league_table_rnds <- as.data.frame(sc2_league_table_rnds)
#   #rename the columns
#   names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_teams"] <- "Team"
#   names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_games_played_rnds"] <- "P"
#   names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_wins_rnds"] <- "W"
#   names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_draws_rnds"] <- "D"
#   names(sc2_league_table_rnds)[names(sc2_league_table_rnds) == "sc2_total_loss_rnds"] <- "L"
#   # names(sc2_league_table)[names(sc2_league_table) == "sc2_GS"] <- "F"
#   # names(sc2_league_table)[names(sc2_league_table) == "sc2_GC"] <- "A"
#   points_sc2_rnds <- sc2_league_table_rnds[order(as.numeric(sc2_league_table_rnds$sc2_PTS_rnds), decreasing = TRUE),]
#   points_sc2_rnds$sc2_rank_rnds <- 1:length(sc2_teams)
#   row.names(points_sc2_rnds) <- points_sc2_rnds$sc2_rank
#
#
#   points_sc2_rnds <- points_sc2_rnds[order(as.character(points_sc2_rnds$Team)),]
#
#
#   sc2_roundmatrix[,i_sc2_krounds] <- as.data.frame(points_sc2_rnds$sc2_rank_rnds)
#
#
# }
#
# sc2_roundmatrix <- cbind(sc2_teams,sc2_roundmatrix)
# write.xlsx(sc2_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sc2",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #sc3
# #hwins and away wins
# sc3_home_wins_rnds <- c()
# sc3_away_wins_rnds <- c()
# sc3_home_draws_rnds <- c()
# sc3_away_draws_rnds <- c()
# sc3_home_loss_rnds <- c()
# sc3_away_loss_rnds <- c()
#
# #sc3_krounds is total rounds as per current season
# sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
# sc3_roundmatrix <- data.frame(matrix(nrow = length(sc3_teams),ncol = sc3_krounds))
#
# for(i_sc3_krounds in 1:sc3_krounds)
# {
#
#   for (i_sc3_wins_rnds in 1:length(sc3_teams))
#   {
#
#     sc3_home_wins_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "H" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_away_wins_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "A" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_home_draws_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "D" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_away_draws_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "D" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_home_loss_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "A" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_away_loss_rnds[i_sc3_wins_rnds] <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_wins_rnds] & SC3_rounds$FTR == "H" & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#
#   }
#
#   sc3_total_wins_rnds <- sc3_home_wins_rnds + sc3_away_wins_rnds
#   sc3_total_draws_rnds <- sc3_home_draws_rnds + sc3_away_draws_rnds
#   sc3_total_loss_rnds <- sc3_home_loss_rnds + sc3_away_loss_rnds
#
#
#   sc3_home_games_rnds <- c()
#   sc3_away_games_rnds <-c()
#
#   for (i_sc3_rnds in 1:length(sc3_teams))
#   {
#
#     sc3_home_games_rnds[i_sc3_rnds] <- nrow(SC3_rounds[SC3_rounds$HomeTeam == sc3_teams[i_sc3_rnds] & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#     sc3_away_games_rnds[i_sc3_rnds]  <- nrow(SC3_rounds[SC3_rounds$AwayTeam == sc3_teams[i_sc3_rnds] & SC3_rounds$sc3_matchday <= i_sc3_krounds,])
#
#   }
#
#   sc3_games_played_rnds <- sc3_home_games_rnds + sc3_away_games_rnds
#
#   sc3_league_table_rnds <- cbind(sc3_teams,sc3_games_played_rnds,sc3_total_wins_rnds,sc3_total_draws_rnds,sc3_total_loss_rnds)
#
#   # sc3_GS <- sc3_scoring$TGS
#   # sc3_GC <-sc3_conceding$TGC
#   # sc3_GD <- sc3_scoring$TGS - sc3_conceding$TGC
#
#   sc3_PTS_rnds <- (sc3_total_wins_rnds*3) + (sc3_total_draws_rnds*1)
#   sc3_league_table_rnds <- cbind(sc3_league_table_rnds,sc3_PTS_rnds)
#   sc3_league_table_rnds <- as.data.frame(sc3_league_table_rnds)
#   #rename the columns
#   names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_teams"] <- "Team"
#   names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_games_played_rnds"] <- "P"
#   names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_wins_rnds"] <- "W"
#   names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_draws_rnds"] <- "D"
#   names(sc3_league_table_rnds)[names(sc3_league_table_rnds) == "sc3_total_loss_rnds"] <- "L"
#   # names(sc3_league_table)[names(sc3_league_table) == "sc3_GS"] <- "F"
#   # names(sc3_league_table)[names(sc3_league_table) == "sc3_GC"] <- "A"
#   points_sc3_rnds <- sc3_league_table_rnds[order(as.numeric(sc3_league_table_rnds$sc3_PTS_rnds), decreasing = TRUE),]
#   points_sc3_rnds$sc3_rank_rnds <- 1:length(sc3_teams)
#   row.names(points_sc3_rnds) <- points_sc3_rnds$sc3_rank
#
#
#   points_sc3_rnds <- points_sc3_rnds[order(as.character(points_sc3_rnds$Team)),]
#
#
#   sc3_roundmatrix[,i_sc3_krounds] <- as.data.frame(points_sc3_rnds$sc3_rank_rnds)
#
#
# }
#
# sc3_roundmatrix <- cbind(sc3_teams,sc3_roundmatrix)
# write.xlsx(sc3_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "sc3",append = TRUE)
# #############################################################################################################################################
# ####################################################################################################
# ####################################################################################################
# #t1
# #hwins and away wins
# t1_home_wins_rnds <- c()
# t1_away_wins_rnds <- c()
# t1_home_draws_rnds <- c()
# t1_away_draws_rnds <- c()
# t1_home_loss_rnds <- c()
# t1_away_loss_rnds <- c()
#
# #t1_krounds is total rounds as per current season
# t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
# t1_roundmatrix <- data.frame(matrix(nrow = length(t1_teams),ncol = t1_krounds))
#
# for(i_t1_krounds in 1:t1_krounds)
# {
#
#   for (i_t1_wins_rnds in 1:length(t1_teams))
#   {
#
#     t1_home_wins_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "H" & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_away_wins_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "A" & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_home_draws_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "D" & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_away_draws_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "D" & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_home_loss_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "A" & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_away_loss_rnds[i_t1_wins_rnds] <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_wins_rnds] & T1_rounds$FTR == "H" & T1_rounds$t1_matchday <= i_t1_krounds,])
#
#   }
#
#   t1_total_wins_rnds <- t1_home_wins_rnds + t1_away_wins_rnds
#   t1_total_draws_rnds <- t1_home_draws_rnds + t1_away_draws_rnds
#   t1_total_loss_rnds <- t1_home_loss_rnds + t1_away_loss_rnds
#
#
#   t1_home_games_rnds <- c()
#   t1_away_games_rnds <-c()
#
#   for (i_t1_rnds in 1:length(t1_teams))
#   {
#
#     t1_home_games_rnds[i_t1_rnds] <- nrow(T1_rounds[T1_rounds$HomeTeam == t1_teams[i_t1_rnds] & T1_rounds$t1_matchday <= i_t1_krounds,])
#     t1_away_games_rnds[i_t1_rnds]  <- nrow(T1_rounds[T1_rounds$AwayTeam == t1_teams[i_t1_rnds] & T1_rounds$t1_matchday <= i_t1_krounds,])
#
#   }
#
#   t1_games_played_rnds <- t1_home_games_rnds + t1_away_games_rnds
#
#   t1_league_table_rnds <- cbind(t1_teams,t1_games_played_rnds,t1_total_wins_rnds,t1_total_draws_rnds,t1_total_loss_rnds)
#
#   # t1_GS <- t1_scoring$TGS
#   # t1_GC <-t1_conceding$TGC
#   # t1_GD <- t1_scoring$TGS - t1_conceding$TGC
#
#   t1_PTS_rnds <- (t1_total_wins_rnds*3) + (t1_total_draws_rnds*1)
#   t1_league_table_rnds <- cbind(t1_league_table_rnds,t1_PTS_rnds)
#   t1_league_table_rnds <- as.data.frame(t1_league_table_rnds)
#   #rename the columns
#   names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_teams"] <- "Team"
#   names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_games_played_rnds"] <- "P"
#   names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_wins_rnds"] <- "W"
#   names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_draws_rnds"] <- "D"
#   names(t1_league_table_rnds)[names(t1_league_table_rnds) == "t1_total_loss_rnds"] <- "L"
#   # names(t1_league_table)[names(t1_league_table) == "t1_GS"] <- "F"
#   # names(t1_league_table)[names(t1_league_table) == "t1_GC"] <- "A"
#   points_t1_rnds <- t1_league_table_rnds[order(as.numeric(t1_league_table_rnds$t1_PTS_rnds), decreasing = TRUE),]
#   points_t1_rnds$t1_rank_rnds <- 1:length(t1_teams)
#   row.names(points_t1_rnds) <- points_t1_rnds$t1_rank
#
#
#   points_t1_rnds <- points_t1_rnds[order(as.character(points_t1_rnds$Team)),]
#
#
#   t1_roundmatrix[,i_t1_krounds] <- as.data.frame(points_t1_rnds$t1_rank_rnds)
#
#
# }
#
# t1_roundmatrix <- cbind(t1_teams,t1_roundmatrix)
#write.xlsx(t1_roundmatrix,'Divisions/Roundmatrix.xlsx',sheetName = "t1",append = TRUE)
#############################################################################################################################################

































