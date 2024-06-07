#Create Home and Away Form
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
################################################################
#MLS
mls_un05_home <- c()
mls_un05_away <- c()
mls_ov05_home <- c()
mls_ov05_away <- c()

mls_un15_home <- c()
mls_un15_away <- c()
mls_ov15_home <- c()
mls_ov15_away <- c()

mls_un25_home <- c()
mls_un25_away <- c()
mls_ov25_home <- c()
mls_ov25_away <- c()

mls_un35_home <- c()
mls_un35_away <- c()
mls_ov35_home <- c()
mls_ov35_away <- c()

mls_un45_home <- c()
mls_un45_away <- c()
mls_ov45_home <- c()
mls_ov45_away <- c()

mls_un55_home <- c()
mls_un55_away <- c()
mls_ov55_home <- c()
mls_ov55_away <- c()

for (i_mls_tg in 1:length(mls_teams))
{

    mls_un05_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG == 0,])
    mls_un05_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG == 0,])

    mls_ov05_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG > 0,])
    mls_ov05_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG > 0,])

    mls_un15_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG <= 1,])
    mls_un15_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG <= 1,])

    mls_ov15_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG >= 2,])
    mls_ov15_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG >= 2,])

    mls_un25_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG <= 2,])
    mls_un25_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG <= 2,])

    mls_ov25_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG >=3,])
    mls_ov25_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG >=3,])

    mls_un35_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG <= 3,])
    mls_un35_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG <= 3,])

    mls_ov35_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG >= 4,])
    mls_ov35_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG >= 4,])

    mls_un45_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG <= 4,])
    mls_un45_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG <= 4,])

    mls_ov45_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG >= 5,])
    mls_ov45_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG >= 5,])

    mls_un55_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG <= 5,])
    mls_un55_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG <= 5,])

    mls_ov55_home[i_mls_tg] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls_tg] & MLS$TG >= 6,])
    mls_ov55_away[i_mls_tg] <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls_tg] & MLS$TG >= 6,])


}

mls_un05 <- mls_un05_home + mls_un05_away
mls_ov05 <- mls_ov05_home + mls_ov05_away

mls_un15 <- mls_un15_home + mls_un15_away
mls_ov15 <- mls_ov15_home + mls_ov15_away

mls_un25 <- mls_un25_home + mls_un25_away
mls_ov25 <- mls_ov25_home + mls_ov25_away

mls_un35 <- mls_un35_home + mls_un35_away
mls_ov35 <- mls_ov35_home + mls_ov35_away

mls_un45 <- mls_un45_home + mls_un45_away
mls_ov45 <- mls_ov45_home + mls_ov45_away

mls_un55 <- mls_un55_home + mls_un55_away
mls_ov55 <- mls_ov55_home + mls_ov55_away

mls_ovundata <- cbind(mls_teams,mls_un05,mls_ov05,mls_un15,mls_ov15,mls_un25,mls_ov25,mls_un35,mls_ov35,mls_un45,mls_ov45,mls_un55,mls_ov55)

###############################################################################################################################
################################################################
#ARG
arg_un05_home <- c()
arg_un05_away <- c()
arg_ov05_home <- c()
arg_ov05_away <- c()

arg_un15_home <- c()
arg_un15_away <- c()
arg_ov15_home <- c()
arg_ov15_away <- c()

arg_un25_home <- c()
arg_un25_away <- c()
arg_ov25_home <- c()
arg_ov25_away <- c()

arg_un35_home <- c()
arg_un35_away <- c()
arg_ov35_home <- c()
arg_ov35_away <- c()

arg_un45_home <- c()
arg_un45_away <- c()
arg_ov45_home <- c()
arg_ov45_away <- c()

arg_un55_home <- c()
arg_un55_away <- c()
arg_ov55_home <- c()
arg_ov55_away <- c()

for (i_arg_tg in 1:length(arg_teams))
{

  arg_un05_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG == 0,])
  arg_un05_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG == 0,])

  arg_ov05_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG > 0,])
  arg_ov05_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG > 0,])

  arg_un15_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG <= 1,])
  arg_un15_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG <= 1,])

  arg_ov15_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG >= 2,])
  arg_ov15_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG >= 2,])

  arg_un25_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG <= 2,])
  arg_un25_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG <= 2,])

  arg_ov25_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG >=3,])
  arg_ov25_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG >=3,])

  arg_un35_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG <= 3,])
  arg_un35_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG <= 3,])

  arg_ov35_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG >= 4,])
  arg_ov35_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG >= 4,])

  arg_un45_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG <= 4,])
  arg_un45_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG <= 4,])

  arg_ov45_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG >= 5,])
  arg_ov45_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG >= 5,])

  arg_un55_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG <= 5,])
  arg_un55_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG <= 5,])

  arg_ov55_home[i_arg_tg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg_tg] & ARG$TG >= 6,])
  arg_ov55_away[i_arg_tg] <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg_tg] & ARG$TG >= 6,])


}

arg_un05 <- arg_un05_home + arg_un05_away
arg_ov05 <- arg_ov05_home + arg_ov05_away

arg_un15 <- arg_un15_home + arg_un15_away
arg_ov15 <- arg_ov15_home + arg_ov15_away

arg_un25 <- arg_un25_home + arg_un25_away
arg_ov25 <- arg_ov25_home + arg_ov25_away

arg_un35 <- arg_un35_home + arg_un35_away
arg_ov35 <- arg_ov35_home + arg_ov35_away

arg_un45 <- arg_un45_home + arg_un45_away
arg_ov45 <- arg_ov45_home + arg_ov45_away

arg_un55 <- arg_un55_home + arg_un55_away
arg_ov55 <- arg_ov55_home + arg_ov55_away

arg_ovundata <- cbind(arg_teams,arg_un05,arg_ov05,arg_un15,arg_ov15,arg_un25,arg_ov25,arg_un35,arg_ov35,arg_un45,arg_ov45,arg_un55,arg_ov55)

###############################################################################################################################
################################################################
#D2
# d2_un05_home <- c()
# d2_un05_away <- c()
# d2_ov05_home <- c()
# d2_ov05_away <- c()
#
# d2_un15_home <- c()
# d2_un15_away <- c()
# d2_ov15_home <- c()
# d2_ov15_away <- c()
#
# d2_un25_home <- c()
# d2_un25_away <- c()
# d2_ov25_home <- c()
# d2_ov25_away <- c()
#
# d2_un35_home <- c()
# d2_un35_away <- c()
# d2_ov35_home <- c()
# d2_ov35_away <- c()
#
# d2_un45_home <- c()
# d2_un45_away <- c()
# d2_ov45_home <- c()
# d2_ov45_away <- c()
#
# d2_un55_home <- c()
# d2_un55_away <- c()
# d2_ov55_home <- c()
# d2_ov55_away <- c()
#
# for (i_d2_tg in 1:length(d2_teams))
# {
#
#   d2_un05_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG == 0,])
#   d2_un05_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG == 0,])
#
#   d2_ov05_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG > 0,])
#   d2_ov05_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG > 0,])
#
#   d2_un15_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG <= 1,])
#   d2_un15_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG <= 1,])
#
#   d2_ov15_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG >= 2,])
#   d2_ov15_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG >= 2,])
#
#   d2_un25_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG <= 2,])
#   d2_un25_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG <= 2,])
#
#   d2_ov25_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG >=3,])
#   d2_ov25_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG >=3,])
#
#   d2_un35_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG <= 3,])
#   d2_un35_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG <= 3,])
#
#   d2_ov35_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG >= 4,])
#   d2_ov35_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG >= 4,])
#
#   d2_un45_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG <= 4,])
#   d2_un45_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG <= 4,])
#
#   d2_ov45_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG >= 5,])
#   d2_ov45_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG >= 5,])
#
#   d2_un55_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG <= 5,])
#   d2_un55_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG <= 5,])
#
#   d2_ov55_home[i_d2_tg] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2_tg] & D2$TG >= 6,])
#   d2_ov55_away[i_d2_tg] <- nrow(D2[D2$AwayTeam == d2_teams[i_d2_tg] & D2$TG >= 6,])
#
#
# }
#
# d2_un05 <- d2_un05_home + d2_un05_away
# d2_ov05 <- d2_ov05_home + d2_ov05_away
#
# d2_un15 <- d2_un15_home + d2_un15_away
# d2_ov15 <- d2_ov15_home + d2_ov15_away
#
# d2_un25 <- d2_un25_home + d2_un25_away
# d2_ov25 <- d2_ov25_home + d2_ov25_away
#
# d2_un35 <- d2_un35_home + d2_un35_away
# d2_ov35 <- d2_ov35_home + d2_ov35_away
#
# d2_un45 <- d2_un45_home + d2_un45_away
# d2_ov45 <- d2_ov45_home + d2_ov45_away
#
# d2_un55 <- d2_un55_home + d2_un55_away
# d2_ov55 <- d2_ov55_home + d2_ov55_away
#
# d2_ovundata <- cbind(d2_teams,d2_un05,d2_ov05,d2_un15,d2_ov15,d2_un25,d2_ov25,d2_un35,d2_ov35,d2_un45,d2_ov45,d2_un55,d2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #E0
# e0_un05_home <- c()
# e0_un05_away <- c()
# e0_ov05_home <- c()
# e0_ov05_away <- c()
#
# e0_un15_home <- c()
# e0_un15_away <- c()
# e0_ov15_home <- c()
# e0_ov15_away <- c()
#
# e0_un25_home <- c()
# e0_un25_away <- c()
# e0_ov25_home <- c()
# e0_ov25_away <- c()
#
# e0_un35_home <- c()
# e0_un35_away <- c()
# e0_ov35_home <- c()
# e0_ov35_away <- c()
#
# e0_un45_home <- c()
# e0_un45_away <- c()
# e0_ov45_home <- c()
# e0_ov45_away <- c()
#
# e0_un55_home <- c()
# e0_un55_away <- c()
# e0_ov55_home <- c()
# e0_ov55_away <- c()
#
# for (i_e0_tg in 1:length(e0_teams))
# {
#
#   e0_un05_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG == 0,])
#   e0_un05_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG == 0,])
#
#   e0_ov05_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG > 0,])
#   e0_ov05_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG > 0,])
#
#   e0_un15_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG <= 1,])
#   e0_un15_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG <= 1,])
#
#   e0_ov15_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG >= 2,])
#   e0_ov15_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG >= 2,])
#
#   e0_un25_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG <= 2,])
#   e0_un25_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG <= 2,])
#
#   e0_ov25_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG >=3,])
#   e0_ov25_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG >=3,])
#
#   e0_un35_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG <= 3,])
#   e0_un35_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG <= 3,])
#
#   e0_ov35_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG >= 4,])
#   e0_ov35_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG >= 4,])
#
#   e0_un45_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG <= 4,])
#   e0_un45_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG <= 4,])
#
#   e0_ov45_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG >= 5,])
#   e0_ov45_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG >= 5,])
#
#   e0_un55_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG <= 5,])
#   e0_un55_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG <= 5,])
#
#   e0_ov55_home[i_e0_tg] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0_tg] & E0$TG >= 6,])
#   e0_ov55_away[i_e0_tg] <- nrow(E0[E0$AwayTeam == e0_teams[i_e0_tg] & E0$TG >= 6,])
#
#
# }
#
# e0_un05 <- e0_un05_home + e0_un05_away
# e0_ov05 <- e0_ov05_home + e0_ov05_away
#
# e0_un15 <- e0_un15_home + e0_un15_away
# e0_ov15 <- e0_ov15_home + e0_ov15_away
#
# e0_un25 <- e0_un25_home + e0_un25_away
# e0_ov25 <- e0_ov25_home + e0_ov25_away
#
# e0_un35 <- e0_un35_home + e0_un35_away
# e0_ov35 <- e0_ov35_home + e0_ov35_away
#
# e0_un45 <- e0_un45_home + e0_un45_away
# e0_ov45 <- e0_ov45_home + e0_ov45_away
#
# e0_un55 <- e0_un55_home + e0_un55_away
# e0_ov55 <- e0_ov55_home + e0_ov55_away
#
# e0_ovundata <- cbind(e0_teams,e0_un05,e0_ov05,e0_un15,e0_ov15,e0_un25,e0_ov25,e0_un35,e0_ov35,e0_un45,e0_ov45,e0_un55,e0_ov55)
#
# ###############################################################################################################################
# ################################################################
# #E1
# e1_un05_home <- c()
# e1_un05_away <- c()
# e1_ov05_home <- c()
# e1_ov05_away <- c()
#
# e1_un15_home <- c()
# e1_un15_away <- c()
# e1_ov15_home <- c()
# e1_ov15_away <- c()
#
# e1_un25_home <- c()
# e1_un25_away <- c()
# e1_ov25_home <- c()
# e1_ov25_away <- c()
#
# e1_un35_home <- c()
# e1_un35_away <- c()
# e1_ov35_home <- c()
# e1_ov35_away <- c()
#
# e1_un45_home <- c()
# e1_un45_away <- c()
# e1_ov45_home <- c()
# e1_ov45_away <- c()
#
# e1_un55_home <- c()
# e1_un55_away <- c()
# e1_ov55_home <- c()
# e1_ov55_away <- c()
#
# for (i_e1_tg in 1:length(e1_teams))
# {
#
#   e1_un05_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG == 0,])
#   e1_un05_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG == 0,])
#
#   e1_ov05_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG > 0,])
#   e1_ov05_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG > 0,])
#
#   e1_un15_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG <= 1,])
#   e1_un15_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG <= 1,])
#
#   e1_ov15_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG >= 2,])
#   e1_ov15_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG >= 2,])
#
#   e1_un25_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG <= 2,])
#   e1_un25_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG <= 2,])
#
#   e1_ov25_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG >=3,])
#   e1_ov25_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG >=3,])
#
#   e1_un35_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG <= 3,])
#   e1_un35_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG <= 3,])
#
#   e1_ov35_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG >= 4,])
#   e1_ov35_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG >= 4,])
#
#   e1_un45_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG <= 4,])
#   e1_un45_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG <= 4,])
#
#   e1_ov45_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG >= 5,])
#   e1_ov45_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG >= 5,])
#
#   e1_un55_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG <= 5,])
#   e1_un55_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG <= 5,])
#
#   e1_ov55_home[i_e1_tg] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1_tg] & E1$TG >= 6,])
#   e1_ov55_away[i_e1_tg] <- nrow(E1[E1$AwayTeam == e1_teams[i_e1_tg] & E1$TG >= 6,])
#
#
# }
#
# e1_un05 <- e1_un05_home + e1_un05_away
# e1_ov05 <- e1_ov05_home + e1_ov05_away
#
# e1_un15 <- e1_un15_home + e1_un15_away
# e1_ov15 <- e1_ov15_home + e1_ov15_away
#
# e1_un25 <- e1_un25_home + e1_un25_away
# e1_ov25 <- e1_ov25_home + e1_ov25_away
#
# e1_un35 <- e1_un35_home + e1_un35_away
# e1_ov35 <- e1_ov35_home + e1_ov35_away
#
# e1_un45 <- e1_un45_home + e1_un45_away
# e1_ov45 <- e1_ov45_home + e1_ov45_away
#
# e1_un55 <- e1_un55_home + e1_un55_away
# e1_ov55 <- e1_ov55_home + e1_ov55_away
#
# e1_ovundata <- cbind(e1_teams,e1_un05,e1_ov05,e1_un15,e1_ov15,e1_un25,e1_ov25,e1_un35,e1_ov35,e1_un45,e1_ov45,e1_un55,e1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #E2
# e2_un05_home <- c()
# e2_un05_away <- c()
# e2_ov05_home <- c()
# e2_ov05_away <- c()
#
# e2_un15_home <- c()
# e2_un15_away <- c()
# e2_ov15_home <- c()
# e2_ov15_away <- c()
#
# e2_un25_home <- c()
# e2_un25_away <- c()
# e2_ov25_home <- c()
# e2_ov25_away <- c()
#
# e2_un35_home <- c()
# e2_un35_away <- c()
# e2_ov35_home <- c()
# e2_ov35_away <- c()
#
# e2_un45_home <- c()
# e2_un45_away <- c()
# e2_ov45_home <- c()
# e2_ov45_away <- c()
#
# e2_un55_home <- c()
# e2_un55_away <- c()
# e2_ov55_home <- c()
# e2_ov55_away <- c()
#
# for (i_e2_tg in 1:length(e2_teams))
# {
#
#   e2_un05_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG == 0,])
#   e2_un05_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG == 0,])
#
#   e2_ov05_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG > 0,])
#   e2_ov05_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG > 0,])
#
#   e2_un15_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG <= 1,])
#   e2_un15_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG <= 1,])
#
#   e2_ov15_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG >= 2,])
#   e2_ov15_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG >= 2,])
#
#   e2_un25_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG <= 2,])
#   e2_un25_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG <= 2,])
#
#   e2_ov25_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG >=3,])
#   e2_ov25_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG >=3,])
#
#   e2_un35_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG <= 3,])
#   e2_un35_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG <= 3,])
#
#   e2_ov35_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG >= 4,])
#   e2_ov35_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG >= 4,])
#
#   e2_un45_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG <= 4,])
#   e2_un45_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG <= 4,])
#
#   e2_ov45_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG >= 5,])
#   e2_ov45_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG >= 5,])
#
#   e2_un55_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG <= 5,])
#   e2_un55_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG <= 5,])
#
#   e2_ov55_home[i_e2_tg] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2_tg] & E2$TG >= 6,])
#   e2_ov55_away[i_e2_tg] <- nrow(E2[E2$AwayTeam == e2_teams[i_e2_tg] & E2$TG >= 6,])
#
#
# }
#
# e2_un05 <- e2_un05_home + e2_un05_away
# e2_ov05 <- e2_ov05_home + e2_ov05_away
#
# e2_un15 <- e2_un15_home + e2_un15_away
# e2_ov15 <- e2_ov15_home + e2_ov15_away
#
# e2_un25 <- e2_un25_home + e2_un25_away
# e2_ov25 <- e2_ov25_home + e2_ov25_away
#
# e2_un35 <- e2_un35_home + e2_un35_away
# e2_ov35 <- e2_ov35_home + e2_ov35_away
#
# e2_un45 <- e2_un45_home + e2_un45_away
# e2_ov45 <- e2_ov45_home + e2_ov45_away
#
# e2_un55 <- e2_un55_home + e2_un55_away
# e2_ov55 <- e2_ov55_home + e2_ov55_away
#
# e2_ovundata <- cbind(e2_teams,e2_un05,e2_ov05,e2_un15,e2_ov15,e2_un25,e2_ov25,e2_un35,e2_ov35,e2_un45,e2_ov45,e2_un55,e2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #E3
# e3_un05_home <- c()
# e3_un05_away <- c()
# e3_ov05_home <- c()
# e3_ov05_away <- c()
#
# e3_un15_home <- c()
# e3_un15_away <- c()
# e3_ov15_home <- c()
# e3_ov15_away <- c()
#
# e3_un25_home <- c()
# e3_un25_away <- c()
# e3_ov25_home <- c()
# e3_ov25_away <- c()
#
# e3_un35_home <- c()
# e3_un35_away <- c()
# e3_ov35_home <- c()
# e3_ov35_away <- c()
#
# e3_un45_home <- c()
# e3_un45_away <- c()
# e3_ov45_home <- c()
# e3_ov45_away <- c()
#
# e3_un55_home <- c()
# e3_un55_away <- c()
# e3_ov55_home <- c()
# e3_ov55_away <- c()
#
# for (i_e3_tg in 1:length(e3_teams))
# {
#
#   e3_un05_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG == 0,])
#   e3_un05_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG == 0,])
#
#   e3_ov05_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG > 0,])
#   e3_ov05_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG > 0,])
#
#   e3_un15_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG <= 1,])
#   e3_un15_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG <= 1,])
#
#   e3_ov15_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG >= 2,])
#   e3_ov15_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG >= 2,])
#
#   e3_un25_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG <= 2,])
#   e3_un25_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG <= 2,])
#
#   e3_ov25_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG >=3,])
#   e3_ov25_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG >=3,])
#
#   e3_un35_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG <= 3,])
#   e3_un35_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG <= 3,])
#
#   e3_ov35_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG >= 4,])
#   e3_ov35_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG >= 4,])
#
#   e3_un45_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG <= 4,])
#   e3_un45_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG <= 4,])
#
#   e3_ov45_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG >= 5,])
#   e3_ov45_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG >= 5,])
#
#   e3_un55_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG <= 5,])
#   e3_un55_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG <= 5,])
#
#   e3_ov55_home[i_e3_tg] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3_tg] & E3$TG >= 6,])
#   e3_ov55_away[i_e3_tg] <- nrow(E3[E3$AwayTeam == e3_teams[i_e3_tg] & E3$TG >= 6,])
#
#
# }
#
# e3_un05 <- e3_un05_home + e3_un05_away
# e3_ov05 <- e3_ov05_home + e3_ov05_away
#
# e3_un15 <- e3_un15_home + e3_un15_away
# e3_ov15 <- e3_ov15_home + e3_ov15_away
#
# e3_un25 <- e3_un25_home + e3_un25_away
# e3_ov25 <- e3_ov25_home + e3_ov25_away
#
# e3_un35 <- e3_un35_home + e3_un35_away
# e3_ov35 <- e3_ov35_home + e3_ov35_away
#
# e3_un45 <- e3_un45_home + e3_un45_away
# e3_ov45 <- e3_ov45_home + e3_ov45_away
#
# e3_un55 <- e3_un55_home + e3_un55_away
# e3_ov55 <- e3_ov55_home + e3_ov55_away
#
# e3_ovundata <- cbind(e3_teams,e3_un05,e3_ov05,e3_un15,e3_ov15,e3_un25,e3_ov25,e3_un35,e3_ov35,e3_un45,e3_ov45,e3_un55,e3_ov55)
#
# ###############################################################################################################################
# ################################################################
# #EC
# ec_un05_home <- c()
# ec_un05_away <- c()
# ec_ov05_home <- c()
# ec_ov05_away <- c()
#
# ec_un15_home <- c()
# ec_un15_away <- c()
# ec_ov15_home <- c()
# ec_ov15_away <- c()
#
# ec_un25_home <- c()
# ec_un25_away <- c()
# ec_ov25_home <- c()
# ec_ov25_away <- c()
#
# ec_un35_home <- c()
# ec_un35_away <- c()
# ec_ov35_home <- c()
# ec_ov35_away <- c()
#
# ec_un45_home <- c()
# ec_un45_away <- c()
# ec_ov45_home <- c()
# ec_ov45_away <- c()
#
# ec_un55_home <- c()
# ec_un55_away <- c()
# ec_ov55_home <- c()
# ec_ov55_away <- c()
#
# for (i_ec_tg in 1:length(ec_teams))
# {
#
#   ec_un05_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG == 0,])
#   ec_un05_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG == 0,])
#
#   ec_ov05_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG > 0,])
#   ec_ov05_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG > 0,])
#
#   ec_un15_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG <= 1,])
#   ec_un15_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG <= 1,])
#
#   ec_ov15_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG >= 2,])
#   ec_ov15_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG >= 2,])
#
#   ec_un25_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG <= 2,])
#   ec_un25_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG <= 2,])
#
#   ec_ov25_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG >=3,])
#   ec_ov25_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG >=3,])
#
#   ec_un35_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG <= 3,])
#   ec_un35_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG <= 3,])
#
#   ec_ov35_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG >= 4,])
#   ec_ov35_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG >= 4,])
#
#   ec_un45_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG <= 4,])
#   ec_un45_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG <= 4,])
#
#   ec_ov45_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG >= 5,])
#   ec_ov45_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG >= 5,])
#
#   ec_un55_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG <= 5,])
#   ec_un55_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG <= 5,])
#
#   ec_ov55_home[i_ec_tg] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec_tg] & EC$TG >= 6,])
#   ec_ov55_away[i_ec_tg] <- nrow(EC[EC$AwayTeam == ec_teams[i_ec_tg] & EC$TG >= 6,])
#
#
# }
#
# ec_un05 <- ec_un05_home + ec_un05_away
# ec_ov05 <- ec_ov05_home + ec_ov05_away
#
# ec_un15 <- ec_un15_home + ec_un15_away
# ec_ov15 <- ec_ov15_home + ec_ov15_away
#
# ec_un25 <- ec_un25_home + ec_un25_away
# ec_ov25 <- ec_ov25_home + ec_ov25_away
#
# ec_un35 <- ec_un35_home + ec_un35_away
# ec_ov35 <- ec_ov35_home + ec_ov35_away
#
# ec_un45 <- ec_un45_home + ec_un45_away
# ec_ov45 <- ec_ov45_home + ec_ov45_away
#
# ec_un55 <- ec_un55_home + ec_un55_away
# ec_ov55 <- ec_ov55_home + ec_ov55_away
#
# ec_ovundata <- cbind(ec_teams,ec_un05,ec_ov05,ec_un15,ec_ov15,ec_un25,ec_ov25,ec_un35,ec_ov35,ec_un45,ec_ov45,ec_un55,ec_ov55)
#
# ###############################################################################################################################
# ################################################################
# #F1
# f1_un05_home <- c()
# f1_un05_away <- c()
# f1_ov05_home <- c()
# f1_ov05_away <- c()
#
# f1_un15_home <- c()
# f1_un15_away <- c()
# f1_ov15_home <- c()
# f1_ov15_away <- c()
#
# f1_un25_home <- c()
# f1_un25_away <- c()
# f1_ov25_home <- c()
# f1_ov25_away <- c()
#
# f1_un35_home <- c()
# f1_un35_away <- c()
# f1_ov35_home <- c()
# f1_ov35_away <- c()
#
# f1_un45_home <- c()
# f1_un45_away <- c()
# f1_ov45_home <- c()
# f1_ov45_away <- c()
#
# f1_un55_home <- c()
# f1_un55_away <- c()
# f1_ov55_home <- c()
# f1_ov55_away <- c()
#
# for (i_f1_tg in 1:length(f1_teams))
# {
#
#   f1_un05_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG == 0,])
#   f1_un05_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG == 0,])
#
#   f1_ov05_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG > 0,])
#   f1_ov05_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG > 0,])
#
#   f1_un15_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG <= 1,])
#   f1_un15_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG <= 1,])
#
#   f1_ov15_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG >= 2,])
#   f1_ov15_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG >= 2,])
#
#   f1_un25_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG <= 2,])
#   f1_un25_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG <= 2,])
#
#   f1_ov25_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG >=3,])
#   f1_ov25_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG >=3,])
#
#   f1_un35_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG <= 3,])
#   f1_un35_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG <= 3,])
#
#   f1_ov35_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG >= 4,])
#   f1_ov35_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG >= 4,])
#
#   f1_un45_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG <= 4,])
#   f1_un45_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG <= 4,])
#
#   f1_ov45_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG >= 5,])
#   f1_ov45_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG >= 5,])
#
#   f1_un55_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG <= 5,])
#   f1_un55_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG <= 5,])
#
#   f1_ov55_home[i_f1_tg] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1_tg] & F1$TG >= 6,])
#   f1_ov55_away[i_f1_tg] <- nrow(F1[F1$AwayTeam == f1_teams[i_f1_tg] & F1$TG >= 6,])
#
#
# }
#
# f1_un05 <- f1_un05_home + f1_un05_away
# f1_ov05 <- f1_ov05_home + f1_ov05_away
#
# f1_un15 <- f1_un15_home + f1_un15_away
# f1_ov15 <- f1_ov15_home + f1_ov15_away
#
# f1_un25 <- f1_un25_home + f1_un25_away
# f1_ov25 <- f1_ov25_home + f1_ov25_away
#
# f1_un35 <- f1_un35_home + f1_un35_away
# f1_ov35 <- f1_ov35_home + f1_ov35_away
#
# f1_un45 <- f1_un45_home + f1_un45_away
# f1_ov45 <- f1_ov45_home + f1_ov45_away
#
# f1_un55 <- f1_un55_home + f1_un55_away
# f1_ov55 <- f1_ov55_home + f1_ov55_away
#
# f1_ovundata <- cbind(f1_teams,f1_un05,f1_ov05,f1_un15,f1_ov15,f1_un25,f1_ov25,f1_un35,f1_ov35,f1_un45,f1_ov45,f1_un55,f1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #F2
# f2_un05_home <- c()
# f2_un05_away <- c()
# f2_ov05_home <- c()
# f2_ov05_away <- c()
#
# f2_un15_home <- c()
# f2_un15_away <- c()
# f2_ov15_home <- c()
# f2_ov15_away <- c()
#
# f2_un25_home <- c()
# f2_un25_away <- c()
# f2_ov25_home <- c()
# f2_ov25_away <- c()
#
# f2_un35_home <- c()
# f2_un35_away <- c()
# f2_ov35_home <- c()
# f2_ov35_away <- c()
#
# f2_un45_home <- c()
# f2_un45_away <- c()
# f2_ov45_home <- c()
# f2_ov45_away <- c()
#
# f2_un55_home <- c()
# f2_un55_away <- c()
# f2_ov55_home <- c()
# f2_ov55_away <- c()
#
# for (i_f2_tg in 1:length(f2_teams))
# {
#
#   f2_un05_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG == 0,])
#   f2_un05_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG == 0,])
#
#   f2_ov05_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG > 0,])
#   f2_ov05_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG > 0,])
#
#   f2_un15_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG <= 1,])
#   f2_un15_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG <= 1,])
#
#   f2_ov15_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG >= 2,])
#   f2_ov15_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG >= 2,])
#
#   f2_un25_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG <= 2,])
#   f2_un25_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG <= 2,])
#
#   f2_ov25_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG >=3,])
#   f2_ov25_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG >=3,])
#
#   f2_un35_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG <= 3,])
#   f2_un35_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG <= 3,])
#
#   f2_ov35_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG >= 4,])
#   f2_ov35_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG >= 4,])
#
#   f2_un45_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG <= 4,])
#   f2_un45_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG <= 4,])
#
#   f2_ov45_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG >= 5,])
#   f2_ov45_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG >= 5,])
#
#   f2_un55_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG <= 5,])
#   f2_un55_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG <= 5,])
#
#   f2_ov55_home[i_f2_tg] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2_tg] & F2$TG >= 6,])
#   f2_ov55_away[i_f2_tg] <- nrow(F2[F2$AwayTeam == f2_teams[i_f2_tg] & F2$TG >= 6,])
#
#
# }
#
# f2_un05 <- f2_un05_home + f2_un05_away
# f2_ov05 <- f2_ov05_home + f2_ov05_away
#
# f2_un15 <- f2_un15_home + f2_un15_away
# f2_ov15 <- f2_ov15_home + f2_ov15_away
#
# f2_un25 <- f2_un25_home + f2_un25_away
# f2_ov25 <- f2_ov25_home + f2_ov25_away
#
# f2_un35 <- f2_un35_home + f2_un35_away
# f2_ov35 <- f2_ov35_home + f2_ov35_away
#
# f2_un45 <- f2_un45_home + f2_un45_away
# f2_ov45 <- f2_ov45_home + f2_ov45_away
#
# f2_un55 <- f2_un55_home + f2_un55_away
# f2_ov55 <- f2_ov55_home + f2_ov55_away
#
# f2_ovundata <- cbind(f2_teams,f2_un05,f2_ov05,f2_un15,f2_ov15,f2_un25,f2_ov25,f2_un35,f2_ov35,f2_un45,f2_ov45,f2_un55,f2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #G1
# g1_un05_home <- c()
# g1_un05_away <- c()
# g1_ov05_home <- c()
# g1_ov05_away <- c()
#
# g1_un15_home <- c()
# g1_un15_away <- c()
# g1_ov15_home <- c()
# g1_ov15_away <- c()
#
# g1_un25_home <- c()
# g1_un25_away <- c()
# g1_ov25_home <- c()
# g1_ov25_away <- c()
#
# g1_un35_home <- c()
# g1_un35_away <- c()
# g1_ov35_home <- c()
# g1_ov35_away <- c()
#
# g1_un45_home <- c()
# g1_un45_away <- c()
# g1_ov45_home <- c()
# g1_ov45_away <- c()
#
# g1_un55_home <- c()
# g1_un55_away <- c()
# g1_ov55_home <- c()
# g1_ov55_away <- c()
#
# for (i_g1_tg in 1:length(g1_teams))
# {
#
#   g1_un05_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG == 0,])
#   g1_un05_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG == 0,])
#
#   g1_ov05_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG > 0,])
#   g1_ov05_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG > 0,])
#
#   g1_un15_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG <= 1,])
#   g1_un15_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG <= 1,])
#
#   g1_ov15_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG >= 2,])
#   g1_ov15_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG >= 2,])
#
#   g1_un25_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG <= 2,])
#   g1_un25_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG <= 2,])
#
#   g1_ov25_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG >=3,])
#   g1_ov25_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG >=3,])
#
#   g1_un35_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG <= 3,])
#   g1_un35_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG <= 3,])
#
#   g1_ov35_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG >= 4,])
#   g1_ov35_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG >= 4,])
#
#   g1_un45_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG <= 4,])
#   g1_un45_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG <= 4,])
#
#   g1_ov45_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG >= 5,])
#   g1_ov45_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG >= 5,])
#
#   g1_un55_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG <= 5,])
#   g1_un55_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG <= 5,])
#
#   g1_ov55_home[i_g1_tg] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1_tg] & G1$TG >= 6,])
#   g1_ov55_away[i_g1_tg] <- nrow(G1[G1$AwayTeam == g1_teams[i_g1_tg] & G1$TG >= 6,])
#
#
# }
#
# g1_un05 <- g1_un05_home + g1_un05_away
# g1_ov05 <- g1_ov05_home + g1_ov05_away
#
# g1_un15 <- g1_un15_home + g1_un15_away
# g1_ov15 <- g1_ov15_home + g1_ov15_away
#
# g1_un25 <- g1_un25_home + g1_un25_away
# g1_ov25 <- g1_ov25_home + g1_ov25_away
#
# g1_un35 <- g1_un35_home + g1_un35_away
# g1_ov35 <- g1_ov35_home + g1_ov35_away
#
# g1_un45 <- g1_un45_home + g1_un45_away
# g1_ov45 <- g1_ov45_home + g1_ov45_away
#
# g1_un55 <- g1_un55_home + g1_un55_away
# g1_ov55 <- g1_ov55_home + g1_ov55_away
#
# g1_ovundata <- cbind(g1_teams,g1_un05,g1_ov05,g1_un15,g1_ov15,g1_un25,g1_ov25,g1_un35,g1_ov35,g1_un45,g1_ov45,g1_un55,g1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #I1
# i1_un05_home <- c()
# i1_un05_away <- c()
# i1_ov05_home <- c()
# i1_ov05_away <- c()
#
# i1_un15_home <- c()
# i1_un15_away <- c()
# i1_ov15_home <- c()
# i1_ov15_away <- c()
#
# i1_un25_home <- c()
# i1_un25_away <- c()
# i1_ov25_home <- c()
# i1_ov25_away <- c()
#
# i1_un35_home <- c()
# i1_un35_away <- c()
# i1_ov35_home <- c()
# i1_ov35_away <- c()
#
# i1_un45_home <- c()
# i1_un45_away <- c()
# i1_ov45_home <- c()
# i1_ov45_away <- c()
#
# i1_un55_home <- c()
# i1_un55_away <- c()
# i1_ov55_home <- c()
# i1_ov55_away <- c()
#
# for (i_i1_tg in 1:length(i1_teams))
# {
#
#   i1_un05_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG == 0,])
#   i1_un05_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG == 0,])
#
#   i1_ov05_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG > 0,])
#   i1_ov05_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG > 0,])
#
#   i1_un15_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG <= 1,])
#   i1_un15_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG <= 1,])
#
#   i1_ov15_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG >= 2,])
#   i1_ov15_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG >= 2,])
#
#   i1_un25_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG <= 2,])
#   i1_un25_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG <= 2,])
#
#   i1_ov25_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG >=3,])
#   i1_ov25_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG >=3,])
#
#   i1_un35_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG <= 3,])
#   i1_un35_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG <= 3,])
#
#   i1_ov35_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG >= 4,])
#   i1_ov35_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG >= 4,])
#
#   i1_un45_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG <= 4,])
#   i1_un45_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG <= 4,])
#
#   i1_ov45_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG >= 5,])
#   i1_ov45_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG >= 5,])
#
#   i1_un55_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG <= 5,])
#   i1_un55_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG <= 5,])
#
#   i1_ov55_home[i_i1_tg] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1_tg] & I1$TG >= 6,])
#   i1_ov55_away[i_i1_tg] <- nrow(I1[I1$AwayTeam == i1_teams[i_i1_tg] & I1$TG >= 6,])
#
#
# }
#
# i1_un05 <- i1_un05_home + i1_un05_away
# i1_ov05 <- i1_ov05_home + i1_ov05_away
#
# i1_un15 <- i1_un15_home + i1_un15_away
# i1_ov15 <- i1_ov15_home + i1_ov15_away
#
# i1_un25 <- i1_un25_home + i1_un25_away
# i1_ov25 <- i1_ov25_home + i1_ov25_away
#
# i1_un35 <- i1_un35_home + i1_un35_away
# i1_ov35 <- i1_ov35_home + i1_ov35_away
#
# i1_un45 <- i1_un45_home + i1_un45_away
# i1_ov45 <- i1_ov45_home + i1_ov45_away
#
# i1_un55 <- i1_un55_home + i1_un55_away
# i1_ov55 <- i1_ov55_home + i1_ov55_away
#
# i1_ovundata <- cbind(i1_teams,i1_un05,i1_ov05,i1_un15,i1_ov15,i1_un25,i1_ov25,i1_un35,i1_ov35,i1_un45,i1_ov45,i1_un55,i1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #I2
# i2_un05_home <- c()
# i2_un05_away <- c()
# i2_ov05_home <- c()
# i2_ov05_away <- c()
#
# i2_un15_home <- c()
# i2_un15_away <- c()
# i2_ov15_home <- c()
# i2_ov15_away <- c()
#
# i2_un25_home <- c()
# i2_un25_away <- c()
# i2_ov25_home <- c()
# i2_ov25_away <- c()
#
# i2_un35_home <- c()
# i2_un35_away <- c()
# i2_ov35_home <- c()
# i2_ov35_away <- c()
#
# i2_un45_home <- c()
# i2_un45_away <- c()
# i2_ov45_home <- c()
# i2_ov45_away <- c()
#
# i2_un55_home <- c()
# i2_un55_away <- c()
# i2_ov55_home <- c()
# i2_ov55_away <- c()
#
# for (i_i2_tg in 1:length(i2_teams))
# {
#
#   i2_un05_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG == 0,])
#   i2_un05_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG == 0,])
#
#   i2_ov05_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG > 0,])
#   i2_ov05_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG > 0,])
#
#   i2_un15_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG <= 1,])
#   i2_un15_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG <= 1,])
#
#   i2_ov15_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG >= 2,])
#   i2_ov15_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG >= 2,])
#
#   i2_un25_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG <= 2,])
#   i2_un25_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG <= 2,])
#
#   i2_ov25_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG >=3,])
#   i2_ov25_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG >=3,])
#
#   i2_un35_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG <= 3,])
#   i2_un35_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG <= 3,])
#
#   i2_ov35_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG >= 4,])
#   i2_ov35_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG >= 4,])
#
#   i2_un45_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG <= 4,])
#   i2_un45_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG <= 4,])
#
#   i2_ov45_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG >= 5,])
#   i2_ov45_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG >= 5,])
#
#   i2_un55_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG <= 5,])
#   i2_un55_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG <= 5,])
#
#   i2_ov55_home[i_i2_tg] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2_tg] & I2$TG >= 6,])
#   i2_ov55_away[i_i2_tg] <- nrow(I2[I2$AwayTeam == i2_teams[i_i2_tg] & I2$TG >= 6,])
#
#
# }
#
# i2_un05 <- i2_un05_home + i2_un05_away
# i2_ov05 <- i2_ov05_home + i2_ov05_away
#
# i2_un15 <- i2_un15_home + i2_un15_away
# i2_ov15 <- i2_ov15_home + i2_ov15_away
#
# i2_un25 <- i2_un25_home + i2_un25_away
# i2_ov25 <- i2_ov25_home + i2_ov25_away
#
# i2_un35 <- i2_un35_home + i2_un35_away
# i2_ov35 <- i2_ov35_home + i2_ov35_away
#
# i2_un45 <- i2_un45_home + i2_un45_away
# i2_ov45 <- i2_ov45_home + i2_ov45_away
#
# i2_un55 <- i2_un55_home + i2_un55_away
# i2_ov55 <- i2_ov55_home + i2_ov55_away
#
# i2_ovundata <- cbind(i2_teams,i2_un05,i2_ov05,i2_un15,i2_ov15,i2_un25,i2_ov25,i2_un35,i2_ov35,i2_un45,i2_ov45,i2_un55,i2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #N1
# n1_un05_home <- c()
# n1_un05_away <- c()
# n1_ov05_home <- c()
# n1_ov05_away <- c()
#
# n1_un15_home <- c()
# n1_un15_away <- c()
# n1_ov15_home <- c()
# n1_ov15_away <- c()
#
# n1_un25_home <- c()
# n1_un25_away <- c()
# n1_ov25_home <- c()
# n1_ov25_away <- c()
#
# n1_un35_home <- c()
# n1_un35_away <- c()
# n1_ov35_home <- c()
# n1_ov35_away <- c()
#
# n1_un45_home <- c()
# n1_un45_away <- c()
# n1_ov45_home <- c()
# n1_ov45_away <- c()
#
# n1_un55_home <- c()
# n1_un55_away <- c()
# n1_ov55_home <- c()
# n1_ov55_away <- c()
#
# for (i_n1_tg in 1:length(n1_teams))
# {
#
#   n1_un05_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG == 0,])
#   n1_un05_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG == 0,])
#
#   n1_ov05_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG > 0,])
#   n1_ov05_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG > 0,])
#
#   n1_un15_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG <= 1,])
#   n1_un15_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG <= 1,])
#
#   n1_ov15_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG >= 2,])
#   n1_ov15_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG >= 2,])
#
#   n1_un25_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG <= 2,])
#   n1_un25_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG <= 2,])
#
#   n1_ov25_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG >=3,])
#   n1_ov25_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG >=3,])
#
#   n1_un35_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG <= 3,])
#   n1_un35_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG <= 3,])
#
#   n1_ov35_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG >= 4,])
#   n1_ov35_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG >= 4,])
#
#   n1_un45_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG <= 4,])
#   n1_un45_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG <= 4,])
#
#   n1_ov45_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG >= 5,])
#   n1_ov45_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG >= 5,])
#
#   n1_un55_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG <= 5,])
#   n1_un55_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG <= 5,])
#
#   n1_ov55_home[i_n1_tg] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1_tg] & N1$TG >= 6,])
#   n1_ov55_away[i_n1_tg] <- nrow(N1[N1$AwayTeam == n1_teams[i_n1_tg] & N1$TG >= 6,])
#
#
# }
#
# n1_un05 <- n1_un05_home + n1_un05_away
# n1_ov05 <- n1_ov05_home + n1_ov05_away
#
# n1_un15 <- n1_un15_home + n1_un15_away
# n1_ov15 <- n1_ov15_home + n1_ov15_away
#
# n1_un25 <- n1_un25_home + n1_un25_away
# n1_ov25 <- n1_ov25_home + n1_ov25_away
#
# n1_un35 <- n1_un35_home + n1_un35_away
# n1_ov35 <- n1_ov35_home + n1_ov35_away
#
# n1_un45 <- n1_un45_home + n1_un45_away
# n1_ov45 <- n1_ov45_home + n1_ov45_away
#
# n1_un55 <- n1_un55_home + n1_un55_away
# n1_ov55 <- n1_ov55_home + n1_ov55_away
#
# n1_ovundata <- cbind(n1_teams,n1_un05,n1_ov05,n1_un15,n1_ov15,n1_un25,n1_ov25,n1_un35,n1_ov35,n1_un45,n1_ov45,n1_un55,n1_ov55)
#
# ###############################################################################################################################
#
# ################################################################
# #P1
# p1_un05_home <- c()
# p1_un05_away <- c()
# p1_ov05_home <- c()
# p1_ov05_away <- c()
#
# p1_un15_home <- c()
# p1_un15_away <- c()
# p1_ov15_home <- c()
# p1_ov15_away <- c()
#
# p1_un25_home <- c()
# p1_un25_away <- c()
# p1_ov25_home <- c()
# p1_ov25_away <- c()
#
# p1_un35_home <- c()
# p1_un35_away <- c()
# p1_ov35_home <- c()
# p1_ov35_away <- c()
#
# p1_un45_home <- c()
# p1_un45_away <- c()
# p1_ov45_home <- c()
# p1_ov45_away <- c()
#
# p1_un55_home <- c()
# p1_un55_away <- c()
# p1_ov55_home <- c()
# p1_ov55_away <- c()
#
# for (i_p1_tg in 1:length(p1_teams))
# {
#
#   p1_un05_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG == 0,])
#   p1_un05_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG == 0,])
#
#   p1_ov05_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG > 0,])
#   p1_ov05_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG > 0,])
#
#   p1_un15_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG <= 1,])
#   p1_un15_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG <= 1,])
#
#   p1_ov15_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG >= 2,])
#   p1_ov15_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG >= 2,])
#
#   p1_un25_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG <= 2,])
#   p1_un25_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG <= 2,])
#
#   p1_ov25_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG >=3,])
#   p1_ov25_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG >=3,])
#
#   p1_un35_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG <= 3,])
#   p1_un35_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG <= 3,])
#
#   p1_ov35_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG >= 4,])
#   p1_ov35_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG >= 4,])
#
#   p1_un45_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG <= 4,])
#   p1_un45_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG <= 4,])
#
#   p1_ov45_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG >= 5,])
#   p1_ov45_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG >= 5,])
#
#   p1_un55_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG <= 5,])
#   p1_un55_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG <= 5,])
#
#   p1_ov55_home[i_p1_tg] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1_tg] & P1$TG >= 6,])
#   p1_ov55_away[i_p1_tg] <- nrow(P1[P1$AwayTeam == p1_teams[i_p1_tg] & P1$TG >= 6,])
#
#
# }
#
# p1_un05 <- p1_un05_home + p1_un05_away
# p1_ov05 <- p1_ov05_home + p1_ov05_away
#
# p1_un15 <- p1_un15_home + p1_un15_away
# p1_ov15 <- p1_ov15_home + p1_ov15_away
#
# p1_un25 <- p1_un25_home + p1_un25_away
# p1_ov25 <- p1_ov25_home + p1_ov25_away
#
# p1_un35 <- p1_un35_home + p1_un35_away
# p1_ov35 <- p1_ov35_home + p1_ov35_away
#
# p1_un45 <- p1_un45_home + p1_un45_away
# p1_ov45 <- p1_ov45_home + p1_ov45_away
#
# p1_un55 <- p1_un55_home + p1_un55_away
# p1_ov55 <- p1_ov55_home + p1_ov55_away
#
# p1_ovundata <- cbind(p1_teams,p1_un05,p1_ov05,p1_un15,p1_ov15,p1_un25,p1_ov25,p1_un35,p1_ov35,p1_un45,p1_ov45,p1_un55,p1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SC0
# sc0_un05_home <- c()
# sc0_un05_away <- c()
# sc0_ov05_home <- c()
# sc0_ov05_away <- c()
#
# sc0_un15_home <- c()
# sc0_un15_away <- c()
# sc0_ov15_home <- c()
# sc0_ov15_away <- c()
#
# sc0_un25_home <- c()
# sc0_un25_away <- c()
# sc0_ov25_home <- c()
# sc0_ov25_away <- c()
#
# sc0_un35_home <- c()
# sc0_un35_away <- c()
# sc0_ov35_home <- c()
# sc0_ov35_away <- c()
#
# sc0_un45_home <- c()
# sc0_un45_away <- c()
# sc0_ov45_home <- c()
# sc0_ov45_away <- c()
#
# sc0_un55_home <- c()
# sc0_un55_away <- c()
# sc0_ov55_home <- c()
# sc0_ov55_away <- c()
#
# for (i_sc0_tg in 1:length(sc0_teams))
# {
#
#   sc0_un05_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG == 0,])
#   sc0_un05_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG == 0,])
#
#   sc0_ov05_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG > 0,])
#   sc0_ov05_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG > 0,])
#
#   sc0_un15_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 1,])
#   sc0_un15_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 1,])
#
#   sc0_ov15_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 2,])
#   sc0_ov15_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 2,])
#
#   sc0_un25_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 2,])
#   sc0_un25_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 2,])
#
#   sc0_ov25_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG >=3,])
#   sc0_ov25_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG >=3,])
#
#   sc0_un35_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 3,])
#   sc0_un35_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 3,])
#
#   sc0_ov35_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 4,])
#   sc0_ov35_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 4,])
#
#   sc0_un45_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 4,])
#   sc0_un45_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 4,])
#
#   sc0_ov45_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 5,])
#   sc0_ov45_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 5,])
#
#   sc0_un55_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 5,])
#   sc0_un55_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG <= 5,])
#
#   sc0_ov55_home[i_sc0_tg] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 6,])
#   sc0_ov55_away[i_sc0_tg] <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0_tg] & SC0$TG >= 6,])
#
#
# }
#
# sc0_un05 <- sc0_un05_home + sc0_un05_away
# sc0_ov05 <- sc0_ov05_home + sc0_ov05_away
#
# sc0_un15 <- sc0_un15_home + sc0_un15_away
# sc0_ov15 <- sc0_ov15_home + sc0_ov15_away
#
# sc0_un25 <- sc0_un25_home + sc0_un25_away
# sc0_ov25 <- sc0_ov25_home + sc0_ov25_away
#
# sc0_un35 <- sc0_un35_home + sc0_un35_away
# sc0_ov35 <- sc0_ov35_home + sc0_ov35_away
#
# sc0_un45 <- sc0_un45_home + sc0_un45_away
# sc0_ov45 <- sc0_ov45_home + sc0_ov45_away
#
# sc0_un55 <- sc0_un55_home + sc0_un55_away
# sc0_ov55 <- sc0_ov55_home + sc0_ov55_away
#
# sc0_ovundata <- cbind(sc0_teams,sc0_un05,sc0_ov05,sc0_un15,sc0_ov15,sc0_un25,sc0_ov25,sc0_un35,sc0_ov35,sc0_un45,sc0_ov45,sc0_un55,sc0_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SC1
# sc1_un05_home <- c()
# sc1_un05_away <- c()
# sc1_ov05_home <- c()
# sc1_ov05_away <- c()
#
# sc1_un15_home <- c()
# sc1_un15_away <- c()
# sc1_ov15_home <- c()
# sc1_ov15_away <- c()
#
# sc1_un25_home <- c()
# sc1_un25_away <- c()
# sc1_ov25_home <- c()
# sc1_ov25_away <- c()
#
# sc1_un35_home <- c()
# sc1_un35_away <- c()
# sc1_ov35_home <- c()
# sc1_ov35_away <- c()
#
# sc1_un45_home <- c()
# sc1_un45_away <- c()
# sc1_ov45_home <- c()
# sc1_ov45_away <- c()
#
# sc1_un55_home <- c()
# sc1_un55_away <- c()
# sc1_ov55_home <- c()
# sc1_ov55_away <- c()
#
# for (i_sc1_tg in 1:length(sc1_teams))
# {
#
#   sc1_un05_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG == 0,])
#   sc1_un05_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG == 0,])
#
#   sc1_ov05_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG > 0,])
#   sc1_ov05_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG > 0,])
#
#   sc1_un15_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 1,])
#   sc1_un15_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 1,])
#
#   sc1_ov15_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 2,])
#   sc1_ov15_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 2,])
#
#   sc1_un25_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 2,])
#   sc1_un25_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 2,])
#
#   sc1_ov25_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG >=3,])
#   sc1_ov25_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG >=3,])
#
#   sc1_un35_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 3,])
#   sc1_un35_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 3,])
#
#   sc1_ov35_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 4,])
#   sc1_ov35_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 4,])
#
#   sc1_un45_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 4,])
#   sc1_un45_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 4,])
#
#   sc1_ov45_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 5,])
#   sc1_ov45_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 5,])
#
#   sc1_un55_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 5,])
#   sc1_un55_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG <= 5,])
#
#   sc1_ov55_home[i_sc1_tg] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 6,])
#   sc1_ov55_away[i_sc1_tg] <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1_tg] & SC1$TG >= 6,])
#
#
# }
#
# sc1_un05 <- sc1_un05_home + sc1_un05_away
# sc1_ov05 <- sc1_ov05_home + sc1_ov05_away
#
# sc1_un15 <- sc1_un15_home + sc1_un15_away
# sc1_ov15 <- sc1_ov15_home + sc1_ov15_away
#
# sc1_un25 <- sc1_un25_home + sc1_un25_away
# sc1_ov25 <- sc1_ov25_home + sc1_ov25_away
#
# sc1_un35 <- sc1_un35_home + sc1_un35_away
# sc1_ov35 <- sc1_ov35_home + sc1_ov35_away
#
# sc1_un45 <- sc1_un45_home + sc1_un45_away
# sc1_ov45 <- sc1_ov45_home + sc1_ov45_away
#
# sc1_un55 <- sc1_un55_home + sc1_un55_away
# sc1_ov55 <- sc1_ov55_home + sc1_ov55_away
#
# sc1_ovundata <- cbind(sc1_teams,sc1_un05,sc1_ov05,sc1_un15,sc1_ov15,sc1_un25,sc1_ov25,sc1_un35,sc1_ov35,sc1_un45,sc1_ov45,sc1_un55,sc1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SC2
# sc2_un05_home <- c()
# sc2_un05_away <- c()
# sc2_ov05_home <- c()
# sc2_ov05_away <- c()
#
# sc2_un15_home <- c()
# sc2_un15_away <- c()
# sc2_ov15_home <- c()
# sc2_ov15_away <- c()
#
# sc2_un25_home <- c()
# sc2_un25_away <- c()
# sc2_ov25_home <- c()
# sc2_ov25_away <- c()
#
# sc2_un35_home <- c()
# sc2_un35_away <- c()
# sc2_ov35_home <- c()
# sc2_ov35_away <- c()
#
# sc2_un45_home <- c()
# sc2_un45_away <- c()
# sc2_ov45_home <- c()
# sc2_ov45_away <- c()
#
# sc2_un55_home <- c()
# sc2_un55_away <- c()
# sc2_ov55_home <- c()
# sc2_ov55_away <- c()
#
# for (i_sc2_tg in 1:length(sc2_teams))
# {
#
#   sc2_un05_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG == 0,])
#   sc2_un05_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG == 0,])
#
#   sc2_ov05_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG > 0,])
#   sc2_ov05_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG > 0,])
#
#   sc2_un15_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 1,])
#   sc2_un15_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 1,])
#
#   sc2_ov15_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 2,])
#   sc2_ov15_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 2,])
#
#   sc2_un25_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 2,])
#   sc2_un25_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 2,])
#
#   sc2_ov25_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG >=3,])
#   sc2_ov25_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG >=3,])
#
#   sc2_un35_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 3,])
#   sc2_un35_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 3,])
#
#   sc2_ov35_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 4,])
#   sc2_ov35_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 4,])
#
#   sc2_un45_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 4,])
#   sc2_un45_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 4,])
#
#   sc2_ov45_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 5,])
#   sc2_ov45_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 5,])
#
#   sc2_un55_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 5,])
#   sc2_un55_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG <= 5,])
#
#   sc2_ov55_home[i_sc2_tg] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 6,])
#   sc2_ov55_away[i_sc2_tg] <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2_tg] & SC2$TG >= 6,])
#
#
# }
#
# sc2_un05 <- sc2_un05_home + sc2_un05_away
# sc2_ov05 <- sc2_ov05_home + sc2_ov05_away
#
# sc2_un15 <- sc2_un15_home + sc2_un15_away
# sc2_ov15 <- sc2_ov15_home + sc2_ov15_away
#
# sc2_un25 <- sc2_un25_home + sc2_un25_away
# sc2_ov25 <- sc2_ov25_home + sc2_ov25_away
#
# sc2_un35 <- sc2_un35_home + sc2_un35_away
# sc2_ov35 <- sc2_ov35_home + sc2_ov35_away
#
# sc2_un45 <- sc2_un45_home + sc2_un45_away
# sc2_ov45 <- sc2_ov45_home + sc2_ov45_away
#
# sc2_un55 <- sc2_un55_home + sc2_un55_away
# sc2_ov55 <- sc2_ov55_home + sc2_ov55_away
#
# sc2_ovundata <- cbind(sc2_teams,sc2_un05,sc2_ov05,sc2_un15,sc2_ov15,sc2_un25,sc2_ov25,sc2_un35,sc2_ov35,sc2_un45,sc2_ov45,sc2_un55,sc2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SC3
# sc3_un05_home <- c()
# sc3_un05_away <- c()
# sc3_ov05_home <- c()
# sc3_ov05_away <- c()
#
# sc3_un15_home <- c()
# sc3_un15_away <- c()
# sc3_ov15_home <- c()
# sc3_ov15_away <- c()
#
# sc3_un25_home <- c()
# sc3_un25_away <- c()
# sc3_ov25_home <- c()
# sc3_ov25_away <- c()
#
# sc3_un35_home <- c()
# sc3_un35_away <- c()
# sc3_ov35_home <- c()
# sc3_ov35_away <- c()
#
# sc3_un45_home <- c()
# sc3_un45_away <- c()
# sc3_ov45_home <- c()
# sc3_ov45_away <- c()
#
# sc3_un55_home <- c()
# sc3_un55_away <- c()
# sc3_ov55_home <- c()
# sc3_ov55_away <- c()
#
# for (i_sc3_tg in 1:length(sc3_teams))
# {
#
#   sc3_un05_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG == 0,])
#   sc3_un05_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG == 0,])
#
#   sc3_ov05_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG > 0,])
#   sc3_ov05_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG > 0,])
#
#   sc3_un15_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 1,])
#   sc3_un15_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 1,])
#
#   sc3_ov15_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 2,])
#   sc3_ov15_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 2,])
#
#   sc3_un25_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 2,])
#   sc3_un25_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 2,])
#
#   sc3_ov25_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG >=3,])
#   sc3_ov25_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG >=3,])
#
#   sc3_un35_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 3,])
#   sc3_un35_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 3,])
#
#   sc3_ov35_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 4,])
#   sc3_ov35_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 4,])
#
#   sc3_un45_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 4,])
#   sc3_un45_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 4,])
#
#   sc3_ov45_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 5,])
#   sc3_ov45_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 5,])
#
#   sc3_un55_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 5,])
#   sc3_un55_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG <= 5,])
#
#   sc3_ov55_home[i_sc3_tg] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 6,])
#   sc3_ov55_away[i_sc3_tg] <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3_tg] & SC3$TG >= 6,])
#
#
# }
#
# sc3_un05 <- sc3_un05_home + sc3_un05_away
# sc3_ov05 <- sc3_ov05_home + sc3_ov05_away
#
# sc3_un15 <- sc3_un15_home + sc3_un15_away
# sc3_ov15 <- sc3_ov15_home + sc3_ov15_away
#
# sc3_un25 <- sc3_un25_home + sc3_un25_away
# sc3_ov25 <- sc3_ov25_home + sc3_ov25_away
#
# sc3_un35 <- sc3_un35_home + sc3_un35_away
# sc3_ov35 <- sc3_ov35_home + sc3_ov35_away
#
# sc3_un45 <- sc3_un45_home + sc3_un45_away
# sc3_ov45 <- sc3_ov45_home + sc3_ov45_away
#
# sc3_un55 <- sc3_un55_home + sc3_un55_away
# sc3_ov55 <- sc3_ov55_home + sc3_ov55_away
#
# sc3_ovundata <- cbind(sc3_teams,sc3_un05,sc3_ov05,sc3_un15,sc3_ov15,sc3_un25,sc3_ov25,sc3_un35,sc3_ov35,sc3_un45,sc3_ov45,sc3_un55,sc3_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SP1
# sp1_un05_home <- c()
# sp1_un05_away <- c()
# sp1_ov05_home <- c()
# sp1_ov05_away <- c()
#
# sp1_un15_home <- c()
# sp1_un15_away <- c()
# sp1_ov15_home <- c()
# sp1_ov15_away <- c()
#
# sp1_un25_home <- c()
# sp1_un25_away <- c()
# sp1_ov25_home <- c()
# sp1_ov25_away <- c()
#
# sp1_un35_home <- c()
# sp1_un35_away <- c()
# sp1_ov35_home <- c()
# sp1_ov35_away <- c()
#
# sp1_un45_home <- c()
# sp1_un45_away <- c()
# sp1_ov45_home <- c()
# sp1_ov45_away <- c()
#
# sp1_un55_home <- c()
# sp1_un55_away <- c()
# sp1_ov55_home <- c()
# sp1_ov55_away <- c()
#
# for (i_sp1_tg in 1:length(sp1_teams))
# {
#
#   sp1_un05_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG == 0,])
#   sp1_un05_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG == 0,])
#
#   sp1_ov05_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG > 0,])
#   sp1_ov05_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG > 0,])
#
#   sp1_un15_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 1,])
#   sp1_un15_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 1,])
#
#   sp1_ov15_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 2,])
#   sp1_ov15_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 2,])
#
#   sp1_un25_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 2,])
#   sp1_un25_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 2,])
#
#   sp1_ov25_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG >=3,])
#   sp1_ov25_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG >=3,])
#
#   sp1_un35_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 3,])
#   sp1_un35_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 3,])
#
#   sp1_ov35_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 4,])
#   sp1_ov35_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 4,])
#
#   sp1_un45_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 4,])
#   sp1_un45_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 4,])
#
#   sp1_ov45_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 5,])
#   sp1_ov45_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 5,])
#
#   sp1_un55_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 5,])
#   sp1_un55_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG <= 5,])
#
#   sp1_ov55_home[i_sp1_tg] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 6,])
#   sp1_ov55_away[i_sp1_tg] <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1_tg] & SP1$TG >= 6,])
#
#
# }
#
# sp1_un05 <- sp1_un05_home + sp1_un05_away
# sp1_ov05 <- sp1_ov05_home + sp1_ov05_away
#
# sp1_un15 <- sp1_un15_home + sp1_un15_away
# sp1_ov15 <- sp1_ov15_home + sp1_ov15_away
#
# sp1_un25 <- sp1_un25_home + sp1_un25_away
# sp1_ov25 <- sp1_ov25_home + sp1_ov25_away
#
# sp1_un35 <- sp1_un35_home + sp1_un35_away
# sp1_ov35 <- sp1_ov35_home + sp1_ov35_away
#
# sp1_un45 <- sp1_un45_home + sp1_un45_away
# sp1_ov45 <- sp1_ov45_home + sp1_ov45_away
#
# sp1_un55 <- sp1_un55_home + sp1_un55_away
# sp1_ov55 <- sp1_ov55_home + sp1_ov55_away
#
# sp1_ovundata <- cbind(sp1_teams,sp1_un05,sp1_ov05,sp1_un15,sp1_ov15,sp1_un25,sp1_ov25,sp1_un35,sp1_ov35,sp1_un45,sp1_ov45,sp1_un55,sp1_ov55)
#
# ###############################################################################################################################
# ################################################################
# #SP2
# sp2_un05_home <- c()
# sp2_un05_away <- c()
# sp2_ov05_home <- c()
# sp2_ov05_away <- c()
#
# sp2_un15_home <- c()
# sp2_un15_away <- c()
# sp2_ov15_home <- c()
# sp2_ov15_away <- c()
#
# sp2_un25_home <- c()
# sp2_un25_away <- c()
# sp2_ov25_home <- c()
# sp2_ov25_away <- c()
#
# sp2_un35_home <- c()
# sp2_un35_away <- c()
# sp2_ov35_home <- c()
# sp2_ov35_away <- c()
#
# sp2_un45_home <- c()
# sp2_un45_away <- c()
# sp2_ov45_home <- c()
# sp2_ov45_away <- c()
#
# sp2_un55_home <- c()
# sp2_un55_away <- c()
# sp2_ov55_home <- c()
# sp2_ov55_away <- c()
#
# for (i_sp2_tg in 1:length(sp2_teams))
# {
#
#   sp2_un05_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG == 0,])
#   sp2_un05_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG == 0,])
#
#   sp2_ov05_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG > 0,])
#   sp2_ov05_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG > 0,])
#
#   sp2_un15_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 1,])
#   sp2_un15_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 1,])
#
#   sp2_ov15_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 2,])
#   sp2_ov15_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 2,])
#
#   sp2_un25_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 2,])
#   sp2_un25_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 2,])
#
#   sp2_ov25_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG >=3,])
#   sp2_ov25_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG >=3,])
#
#   sp2_un35_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 3,])
#   sp2_un35_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 3,])
#
#   sp2_ov35_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 4,])
#   sp2_ov35_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 4,])
#
#   sp2_un45_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 4,])
#   sp2_un45_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 4,])
#
#   sp2_ov45_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 5,])
#   sp2_ov45_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 5,])
#
#   sp2_un55_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 5,])
#   sp2_un55_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG <= 5,])
#
#   sp2_ov55_home[i_sp2_tg] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 6,])
#   sp2_ov55_away[i_sp2_tg] <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2_tg] & SP2$TG >= 6,])
#
#
# }
#
# sp2_un05 <- sp2_un05_home + sp2_un05_away
# sp2_ov05 <- sp2_ov05_home + sp2_ov05_away
#
# sp2_un15 <- sp2_un15_home + sp2_un15_away
# sp2_ov15 <- sp2_ov15_home + sp2_ov15_away
#
# sp2_un25 <- sp2_un25_home + sp2_un25_away
# sp2_ov25 <- sp2_ov25_home + sp2_ov25_away
#
# sp2_un35 <- sp2_un35_home + sp2_un35_away
# sp2_ov35 <- sp2_ov35_home + sp2_ov35_away
#
# sp2_un45 <- sp2_un45_home + sp2_un45_away
# sp2_ov45 <- sp2_ov45_home + sp2_ov45_away
#
# sp2_un55 <- sp2_un55_home + sp2_un55_away
# sp2_ov55 <- sp2_ov55_home + sp2_ov55_away
#
# sp2_ovundata <- cbind(sp2_teams,sp2_un05,sp2_ov05,sp2_un15,sp2_ov15,sp2_un25,sp2_ov25,sp2_un35,sp2_ov35,sp2_un45,sp2_ov45,sp2_un55,sp2_ov55)
#
# ###############################################################################################################################
# ################################################################
# #T1
# t1_un05_home <- c()
# t1_un05_away <- c()
# t1_ov05_home <- c()
# t1_ov05_away <- c()
#
# t1_un15_home <- c()
# t1_un15_away <- c()
# t1_ov15_home <- c()
# t1_ov15_away <- c()
#
# t1_un25_home <- c()
# t1_un25_away <- c()
# t1_ov25_home <- c()
# t1_ov25_away <- c()
#
# t1_un35_home <- c()
# t1_un35_away <- c()
# t1_ov35_home <- c()
# t1_ov35_away <- c()
#
# t1_un45_home <- c()
# t1_un45_away <- c()
# t1_ov45_home <- c()
# t1_ov45_away <- c()
#
# t1_un55_home <- c()
# t1_un55_away <- c()
# t1_ov55_home <- c()
# t1_ov55_away <- c()
#
# for (i_t1_tg in 1:length(t1_teams))
# {
#
#   t1_un05_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG == 0,])
#   t1_un05_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG == 0,])
#
#   t1_ov05_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG > 0,])
#   t1_ov05_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG > 0,])
#
#   t1_un15_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG <= 1,])
#   t1_un15_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG <= 1,])
#
#   t1_ov15_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG >= 2,])
#   t1_ov15_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG >= 2,])
#
#   t1_un25_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG <= 2,])
#   t1_un25_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG <= 2,])
#
#   t1_ov25_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG >=3,])
#   t1_ov25_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG >=3,])
#
#   t1_un35_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG <= 3,])
#   t1_un35_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG <= 3,])
#
#   t1_ov35_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG >= 4,])
#   t1_ov35_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG >= 4,])
#
#   t1_un45_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG <= 4,])
#   t1_un45_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG <= 4,])
#
#   t1_ov45_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG >= 5,])
#   t1_ov45_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG >= 5,])
#
#   t1_un55_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG <= 5,])
#   t1_un55_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG <= 5,])
#
#   t1_ov55_home[i_t1_tg] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1_tg] & T1$TG >= 6,])
#   t1_ov55_away[i_t1_tg] <- nrow(T1[T1$AwayTeam == t1_teams[i_t1_tg] & T1$TG >= 6,])
#
#
# }
#
# t1_un05 <- t1_un05_home + t1_un05_away
# t1_ov05 <- t1_ov05_home + t1_ov05_away
#
# t1_un15 <- t1_un15_home + t1_un15_away
# t1_ov15 <- t1_ov15_home + t1_ov15_away
#
# t1_un25 <- t1_un25_home + t1_un25_away
# t1_ov25 <- t1_ov25_home + t1_ov25_away
#
# t1_un35 <- t1_un35_home + t1_un35_away
# t1_ov35 <- t1_ov35_home + t1_ov35_away
#
# t1_un45 <- t1_un45_home + t1_un45_away
# t1_ov45 <- t1_ov45_home + t1_ov45_away
#
# t1_un55 <- t1_un55_home + t1_un55_away
# t1_ov55 <- t1_ov55_home + t1_ov55_away
#
# t1_ovundata <- cbind(t1_teams,t1_un05,t1_ov05,t1_un15,t1_ov15,t1_un25,t1_ov25,t1_un35,t1_ov35,t1_un45,t1_ov45,t1_un55,t1_ov55)

###############################################################################################################################
























