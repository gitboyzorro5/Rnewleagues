library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
mls_GP <- nrow(MLS)
arg_GP <- nrow(ARG)
# d2_GP <- nrow(D2)
# e0_GP <- nrow(E0)
# e1_GP <- nrow(E1)
# e2_GP <- nrow(E2)
# e3_GP <- nrow(E3)
# ec_GP <- nrow(EC)
# f1_GP <- nrow(F1)
# f2_GP <- nrow(F2)
# g1_GP <- nrow(G1)
# i1_GP <- nrow(I1)
# i2_GP <- nrow(I2)
# n1_GP <- nrow(N1)
# p1_GP <- nrow(P1)
# sc0_GP <- nrow(SC0)
# sc1_GP <- nrow(SC1)
# sc2_GP <- nrow(SC2)
# sc3_GP <- nrow(SC3)
# sp1_GP <- nrow(SP1)
# sp2_GP <- nrow(SP2)
# t1_GP <- nrow(T1)

#Calculate total home goals for each division
mls_T_HF <- sum(mls_home_fouls$x)
arg_T_HF <- sum(arg_home_fouls$x)
# d2_T_HF <- sum(d2_home_fouls$x)
# e0_T_HF <- sum(e0_home_fouls$x)
# e1_T_HF <- sum(e1_home_fouls$x)
# e2_T_HF <- sum(e2_home_fouls$x)
# e3_T_HF <- sum(e3_home_fouls$x)
# ec_T_HF <- sum(ec_home_fouls$x)
# f1_T_HF <- sum(f1_home_fouls$x)
# f2_T_HF <- sum(f2_home_fouls$x)
# g1_T_HF <- sum(g1_home_fouls$x)
# i1_T_HF <- sum(i1_home_fouls$x)
# i2_T_HF <- sum(i2_home_fouls$x)
# n1_T_HF <- sum(n1_home_fouls$x)
# p1_T_HF <- sum(p1_home_fouls$x)
# sc0_T_HF <- sum(sc0_home_fouls$x)
# sc1_T_HF <- sum(sc1_home_fouls$x)
# sc2_T_HF <- sum(sc2_home_fouls$x)
# sc3_T_HF <- sum(sc3_home_fouls$x)
# sp1_T_HF <- sum(sp1_home_fouls$x)
# sp2_T_HF <- sum(sp2_home_fouls$x)
# t1_T_HF <- sum(t1_home_fouls$x)
#calculate average home goal

mls_avg_HF <- round(mls_T_HF /mls_GP, digits = 4)
arg_avg_HF <- round(arg_T_HF /arg_GP, digits = 4)
# d2_avg_HF <- round(d2_T_HF /d2_GP, digits = 4)
# e0_avg_HF <- round(e0_T_HF /e0_GP, digits = 4)
# e1_avg_HF <- round(e1_T_HF /e1_GP, digits = 4)
# e2_avg_HF <- round(e2_T_HF /e2_GP, digits = 4)
# e3_avg_HF <- round(e3_T_HF /e3_GP, digits = 4)
# ec_avg_HF <- round(ec_T_HF /ec_GP, digits = 4)
# f1_avg_HF <- round(f1_T_HF /f1_GP, digits = 4)
# f2_avg_HF <- round(f2_T_HF /f2_GP, digits = 4)
# g1_avg_HF <- round(g1_T_HF /g1_GP, digits = 4)
# i1_avg_HF <- round(i1_T_HF /i1_GP, digits = 4)
# i2_avg_HF <- round(i2_T_HF /i2_GP, digits = 4)
# n1_avg_HF <- round(n1_T_HF /n1_GP, digits = 4)
# p1_avg_HF <- round(p1_T_HF /p1_GP, digits = 4)
# sc0_avg_HF <- round(sc0_T_HF /sc0_GP, digits = 4)
# sc1_avg_HF <- round(sc1_T_HF /sc1_GP, digits = 4)
# sc2_avg_HF <- round(sc2_T_HF /sc2_GP, digits = 4)
# sc3_avg_HF <- round(sc3_T_HF /sc3_GP, digits = 4)
# sp1_avg_HF <- round(sp1_T_HF /sp1_GP, digits = 4)
# sp2_avg_HF <- round(sp2_T_HF /sp2_GP, digits = 4)
# t1_avg_HF <- round(t1_T_HF /t1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AF <- sum(mls_away_fouls$x)
arg_T_AF <- sum(arg_away_fouls$x)
# d2_T_AF <- sum(d2_away_fouls$x)
# e0_T_AF <- sum(e0_away_fouls$x)
# e1_T_AF <- sum(e1_away_fouls$x)
# e2_T_AF <- sum(e2_away_fouls$x)
# e3_T_AF <- sum(e3_away_fouls$x)
# ec_T_AF <- sum(ec_away_fouls$x)
# f1_T_AF <- sum(f1_away_fouls$x)
# f2_T_AF <- sum(f2_away_fouls$x)
# g1_T_AF <- sum(g1_away_fouls$x)
# i1_T_AF <- sum(i1_away_fouls$x)
# i2_T_AF <- sum(i2_away_fouls$x)
# n1_T_AF <- sum(n1_away_fouls$x)
# p1_T_AF <- sum(p1_away_fouls$x)
# sc0_T_AF <- sum(sc0_away_fouls$x)
# sc1_T_AF <- sum(sc1_away_fouls$x)
# sc2_T_AF <- sum(sc2_away_fouls$x)
# sc3_T_AF <- sum(sc3_away_fouls$x)
# sp1_T_AF <- sum(sp1_away_fouls$x)
# sp2_T_AF <- sum(sp2_away_fouls$x)
# t1_T_AF <- sum(t1_away_fouls$x)
#calculate average away goal

mls_avg_AF <- round(mls_T_AF /mls_GP, digits = 4)
arg_avg_AF <- round(arg_T_AF /arg_GP, digits = 4)
# d2_avg_AF <- round(d2_T_AF /d2_GP, digits = 4)
# e0_avg_AF <- round(e0_T_AF /e0_GP, digits = 4)
# e1_avg_AF <- round(e1_T_AF /e1_GP, digits = 4)
# e2_avg_AF <- round(e2_T_AF /e2_GP, digits = 4)
# e3_avg_AF <- round(e3_T_AF /e3_GP, digits = 4)
# ec_avg_AF <- round(ec_T_AF /ec_GP, digits = 4)
# f1_avg_AF <- round(f1_T_AF /f1_GP, digits = 4)
# f2_avg_AF <- round(f2_T_AF /f2_GP, digits = 4)
# g1_avg_AF <- round(g1_T_AF /g1_GP, digits = 4)
# i1_avg_AF <- round(i1_T_AF /i1_GP, digits = 4)
# i2_avg_AF <- round(i2_T_AF /i2_GP, digits = 4)
# n1_avg_AF <- round(n1_T_AF /n1_GP, digits = 4)
# p1_avg_AF <- round(p1_T_AF /p1_GP, digits = 4)
# sc0_avg_AF <- round(sc0_T_AF /sc0_GP, digits = 4)
# sc1_avg_AF <- round(sc1_T_AF /sc1_GP, digits = 4)
# sc2_avg_AF <- round(sc2_T_AF /sc2_GP, digits = 4)
# sc3_avg_AF <- round(sc3_T_AF /sc3_GP, digits = 4)
# sp1_avg_AF <- round(sp1_T_AF /sp1_GP, digits = 4)
# sp2_avg_AF <- round(sp2_T_AF /sp2_GP, digits = 4)
# t1_avg_AF <- round(t1_T_AF /t1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength

mls_home_fas <- round(((mls_home_fouls$x/mls_home_games))/mls_avg_HF, digits = 4)
arg_home_fas <- round(((arg_home_fouls$x/arg_home_games))/arg_avg_HF, digits = 4)
# d2_home_fas <- round(((d2_home_fouls$x/d2_home_games))/d2_avg_HF, digits = 4)
# e0_home_fas <- round(((e0_home_fouls$x/e0_home_games))/e0_avg_HF, digits = 4)
# e1_home_fas <- round(((e1_home_fouls$x/e1_home_games))/e1_avg_HF, digits = 4)
# e2_home_fas <- round(((e2_home_fouls$x/e2_home_games))/e2_avg_HF, digits = 4)
# e3_home_fas <- round(((e3_home_fouls$x/e3_home_games))/e3_avg_HF, digits = 4)
# ec_home_fas <- round(((ec_home_fouls$x/ec_home_games))/ec_avg_HF, digits = 4)
# f1_home_fas <- round(((f1_home_fouls$x/f1_home_games))/f1_avg_HF, digits = 4)
# f2_home_fas <- round(((f2_home_fouls$x/f2_home_games))/f2_avg_HF, digits = 4)
# g1_home_fas <- round(((g1_home_fouls$x/g1_home_games))/g1_avg_HF, digits = 4)
# i1_home_fas <- round(((i1_home_fouls$x/i1_home_games))/i1_avg_HF, digits = 4)
# i2_home_fas <- round(((i2_home_fouls$x/i2_home_games))/i2_avg_HF, digits = 4)
# n1_home_fas <- round(((n1_home_fouls$x/n1_home_games))/n1_avg_HF, digits = 4)
# p1_home_fas <- round(((p1_home_fouls$x/p1_home_games))/p1_avg_HF, digits = 4)
# sc0_home_fas <- round(((sc0_home_fouls$x/sc0_home_games))/sc0_avg_HF, digits = 4)
# sc1_home_fas <- round(((sc1_home_fouls$x/sc1_home_games))/sc1_avg_HF, digits = 4)
# sc2_home_fas <- round(((sc2_home_fouls$x/sc2_home_games))/sc2_avg_HF, digits = 4)
# sc3_home_fas <- round(((sc3_home_fouls$x/sc3_home_games))/sc3_avg_HF, digits = 4)
# sp1_home_fas <- round(((sp1_home_fouls$x/sp1_home_games))/sp1_avg_HF, digits = 4)
# sp2_home_fas <- round(((sp2_home_fouls$x/sp2_home_games))/sp2_avg_HF, digits = 4)
# t1_home_fas <- round(((t1_home_fouls$x/t1_home_games))/t1_avg_HF, digits = 4)
#calculate away attack strength
mls_away_fas <- round(((mls_away_fouls$x/mls_away_games))/mls_avg_AF, digits = 4)
arg_away_fas <- round(((arg_away_fouls$x/arg_away_games))/arg_avg_AF, digits = 4)
# d2_away_fas <- round(((d2_away_fouls$x/d2_away_games))/d2_avg_AF, digits = 4)
# e0_away_fas <- round(((e0_away_fouls$x/e0_away_games))/e0_avg_AF, digits = 4)
# e1_away_fas <- round(((e1_away_fouls$x/e1_away_games))/e1_avg_AF, digits = 4)
# e2_away_fas <- round(((e2_away_fouls$x/e2_away_games))/e2_avg_AF, digits = 4)
# e3_away_fas <- round(((e3_away_fouls$x/e3_away_games))/e3_avg_AF, digits = 4)
# ec_away_fas <- round(((ec_away_fouls$x/ec_away_games))/ec_avg_AF, digits = 4)
# f1_away_fas <- round(((f1_away_fouls$x/f1_away_games))/f1_avg_AF, digits = 4)
# f2_away_fas <- round(((f2_away_fouls$x/f2_away_games))/f2_avg_AF, digits = 4)
# g1_away_fas <- round(((g1_away_fouls$x/g1_away_games))/g1_avg_AF, digits = 4)
# i1_away_fas <- round(((i1_away_fouls$x/i1_away_games))/i1_avg_AF, digits = 4)
# i2_away_fas <- round(((i2_away_fouls$x/i2_away_games))/i2_avg_AF, digits = 4)
# n1_away_fas <- round(((n1_away_fouls$x/n1_away_games))/n1_avg_AF, digits = 4)
# p1_away_fas <- round(((p1_away_fouls$x/p1_away_games))/p1_avg_AF, digits = 4)
# sc0_away_fas <- round(((sc0_away_fouls$x/sc0_away_games))/sc0_avg_AF, digits = 4)
# sc1_away_fas <- round(((sc1_away_fouls$x/sc1_away_games))/sc1_avg_AF, digits = 4)
# sc2_away_fas <- round(((sc2_away_fouls$x/sc2_away_games))/sc2_avg_AF, digits = 4)
# sc3_away_fas <- round(((sc3_away_fouls$x/sc3_away_games))/sc3_avg_AF, digits = 4)
# sp1_away_fas <- round(((sp1_away_fouls$x/sp1_away_games))/sp1_avg_AF, digits = 4)
# sp2_away_fas <- round(((sp2_away_fouls$x/sp2_away_games))/sp2_avg_AF, digits = 4)
# t1_away_fas <- round(((t1_away_fouls$x/t1_away_games))/t1_avg_AF, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HFC <- round(mls_T_AF /mls_GP, digits = 4)
arg_avg_HFC <- round(arg_T_AF /arg_GP, digits = 4)
# d2_avg_HFC <- round(d2_T_AF /d2_GP, digits = 4)
# e0_avg_HFC <- round(e0_T_AF /e0_GP, digits = 4)
# e1_avg_HFC <- round(e1_T_AF /e1_GP, digits = 4)
# e2_avg_HFC <- round(e2_T_AF /e2_GP, digits = 4)
# e3_avg_HFC <- round(e3_T_AF /e3_GP, digits = 4)
# ec_avg_HFC <- round(ec_T_AF /ec_GP, digits = 4)
# f1_avg_HFC <- round(f1_T_AF /f1_GP, digits = 4)
# f2_avg_HFC <- round(f2_T_AF /f2_GP, digits = 4)
# g1_avg_HFC <- round(g1_T_AF /g1_GP, digits = 4)
# i1_avg_HFC <- round(i1_T_AF /i1_GP, digits = 4)
# i2_avg_HFC <- round(i2_T_AF /i2_GP, digits = 4)
# n1_avg_HFC <- round(n1_T_AF /n1_GP, digits = 4)
# p1_avg_HFC <- round(p1_T_AF /p1_GP, digits = 4)
# sc0_avg_HFC <- round(sc0_T_AF /sc0_GP, digits = 4)
# sc1_avg_HFC <- round(sc1_T_AF /sc1_GP, digits = 4)
# sc2_avg_HFC <- round(sc2_T_AF /sc2_GP, digits = 4)
# sc3_avg_HFC <- round(sc3_T_AF /sc3_GP, digits = 4)
# sp1_avg_HFC <- round(sp1_T_AF /sp1_GP, digits = 4)
# sp2_avg_HFC <- round(sp2_T_AF /sp2_GP, digits = 4)
# t1_avg_HFC <- round(t1_T_AF /t1_GP, digits = 4)
#avg away concede
mls_avg_AFC <- round(mls_T_HF /mls_GP, digits = 4)
arg_avg_AFC <- round(arg_T_HF /arg_GP, digits = 4)
# d2_avg_AFC <- round(d2_T_HF /d2_GP, digits = 4)
# e0_avg_AFC <- round(e0_T_HF /e0_GP, digits = 4)
# e1_avg_AFC <- round(e1_T_HF /e1_GP, digits = 4)
# e2_avg_AFC <- round(e2_T_HF /e2_GP, digits = 4)
# e3_avg_AFC <- round(e3_T_HF /e3_GP, digits = 4)
# ec_avg_AFC <- round(ec_T_HF /ec_GP, digits = 4)
# f1_avg_AFC <- round(f1_T_HF /f1_GP, digits = 4)
# f2_avg_AFC <- round(f2_T_HF /f2_GP, digits = 4)
# g1_avg_AFC <- round(g1_T_HF /g1_GP, digits = 4)
# i1_avg_AFC <- round(i1_T_HF /i1_GP, digits = 4)
# i2_avg_AFC <- round(i2_T_HF /i2_GP, digits = 4)
# n1_avg_AFC <- round(n1_T_HF /n1_GP, digits = 4)
# p1_avg_AFC <- round(p1_T_HF /p1_GP, digits = 4)
# sc0_avg_AFC <- round(sc0_T_HF /sc0_GP, digits = 4)
# sc1_avg_AFC <- round(sc1_T_HF /sc1_GP, digits = 4)
# sc2_avg_AFC <- round(sc2_T_HF /sc2_GP, digits = 4)
# sc3_avg_AFC <- round(sc3_T_HF /sc3_GP, digits = 4)
# sp1_avg_AFC <- round(sp1_T_HF /sp1_GP, digits = 4)
# sp2_avg_AFC <- round(sp2_T_HF /sp2_GP, digits = 4)
# t1_avg_AFC <- round(t1_T_HF /t1_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
mls_home_fcc <- aggregate(MLS$AF, by = list(MLS$HomeTeam), FUN = sum)
mls_away_fcc <- aggregate(MLS$HF, by = list(MLS$AwayTeam), FUN = sum)
arg_home_fcc <- aggregate(ARG$AF, by = list(ARG$HomeTeam), FUN = sum)
arg_away_fcc <- aggregate(ARG$HF, by = list(ARG$AwayTeam), FUN = sum)
# d2_home_fcc <- aggregate(D2$AF, by = list(D2$HomeTeam), FUN = sum)
# d2_away_fcc <- aggregate(D2$HF, by = list(D2$AwayTeam), FUN = sum)
# e0_home_fcc <- aggregate(E0$AF, by = list(E0$HomeTeam), FUN = sum)
# e0_away_fcc <- aggregate(E0$HF, by = list(E0$AwayTeam), FUN = sum)
# e1_home_fcc <- aggregate(E1$AF, by = list(E1$HomeTeam), FUN = sum)
# e1_away_fcc <- aggregate(E1$HF, by = list(E1$AwayTeam), FUN = sum)
# e2_home_fcc <- aggregate(E2$AF, by = list(E2$HomeTeam), FUN = sum)
# e2_away_fcc <- aggregate(E2$HF, by = list(E2$AwayTeam), FUN = sum)
# e3_home_fcc <- aggregate(E3$AF, by = list(E3$HomeTeam), FUN = sum)
# e3_away_fcc <- aggregate(E3$HF, by = list(E3$AwayTeam), FUN = sum)
# ec_home_fcc <- aggregate(EC$AF, by = list(EC$HomeTeam), FUN = sum)
# ec_away_fcc <- aggregate(EC$HF, by = list(EC$AwayTeam), FUN = sum)
# f1_home_fcc <- aggregate(F1$AF, by = list(F1$HomeTeam), FUN = sum)
# f1_away_fcc <- aggregate(F1$HF, by = list(F1$AwayTeam), FUN = sum)
# f2_home_fcc <- aggregate(F2$AF, by = list(F2$HomeTeam), FUN = sum)
# f2_away_fcc <- aggregate(F2$HF, by = list(F2$AwayTeam), FUN = sum)
# g1_home_fcc <- aggregate(G1$AF, by = list(G1$HomeTeam), FUN = sum)
# g1_away_fcc <- aggregate(G1$HF, by = list(G1$AwayTeam), FUN = sum)
# i1_home_fcc <- aggregate(I1$AF, by = list(I1$HomeTeam), FUN = sum)
# i1_away_fcc <- aggregate(I1$HF, by = list(I1$AwayTeam), FUN = sum)
# i2_home_fcc <- aggregate(I2$AF, by = list(I2$HomeTeam), FUN = sum)
# i2_away_fcc <- aggregate(I2$HF, by = list(I2$AwayTeam), FUN = sum)
# n1_home_fcc <- aggregate(N1$AF, by = list(N1$HomeTeam), FUN = sum)
# n1_away_fcc <- aggregate(N1$HF, by = list(N1$AwayTeam), FUN = sum)
# p1_home_fcc <- aggregate(P1$AF, by = list(P1$HomeTeam), FUN = sum)
# p1_away_fcc <- aggregate(P1$HF, by = list(P1$AwayTeam), FUN = sum)
# sc0_home_fcc <- aggregate(SC0$AF, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_fcc <- aggregate(SC0$HF, by = list(SC0$AwayTeam), FUN = sum)
# sc1_home_fcc <- aggregate(SC1$AF, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_fcc <- aggregate(SC1$HF, by = list(SC1$AwayTeam), FUN = sum)
# sc2_home_fcc <- aggregate(SC2$AF, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_fcc <- aggregate(SC2$HF, by = list(SC2$AwayTeam), FUN = sum)
# sc3_home_fcc <- aggregate(SC3$AF, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_fcc <- aggregate(SC3$HF, by = list(SC3$AwayTeam), FUN = sum)
# sp1_home_fcc <- aggregate(SP1$AF, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_fcc <- aggregate(SP1$HF, by = list(SP1$AwayTeam), FUN = sum)
# sp2_home_fcc <- aggregate(SP2$AF, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_fcc <- aggregate(SP2$HF, by = list(SP2$AwayTeam), FUN = sum)
# t1_home_fcc <- aggregate(T1$AF, by = list(T1$HomeTeam), FUN = sum)
# t1_away_fcc <- aggregate(T1$HF, by = list(T1$AwayTeam), FUN = sum)
#home defense strength
mls_home_fds <- round(((mls_home_fcc$x/mls_home_games))/mls_avg_HFC, digits = 4)
arg_home_fds <- round(((arg_home_fcc$x/arg_home_games))/arg_avg_HFC, digits = 4)
# d2_home_fds <- round(((d2_home_fcc$x/d2_home_games))/d2_avg_HFC, digits = 4)
# e0_home_fds <- round(((e0_home_fcc$x/e0_home_games))/e0_avg_HFC, digits = 4)
# e1_home_fds <- round(((e1_home_fcc$x/e1_home_games))/e1_avg_HFC, digits = 4)
# e2_home_fds <- round(((e2_home_fcc$x/e2_home_games))/e2_avg_HFC, digits = 4)
# e3_home_fds <- round(((e3_home_fcc$x/e3_home_games))/e3_avg_HFC, digits = 4)
# ec_home_fds <- round(((ec_home_fcc$x/ec_home_games))/ec_avg_HFC, digits = 4)
# f1_home_fds <- round(((f1_home_fcc$x/f1_home_games))/f1_avg_HFC, digits = 4)
# f2_home_fds <- round(((f2_home_fcc$x/f2_home_games))/f2_avg_HFC, digits = 4)
# g1_home_fds <- round(((g1_home_fcc$x/g1_home_games))/g1_avg_HFC, digits = 4)
# i1_home_fds <- round(((i1_home_fcc$x/i1_home_games))/i1_avg_HFC, digits = 4)
# i2_home_fds <- round(((i2_home_fcc$x/i2_home_games))/i2_avg_HFC, digits = 4)
# n1_home_fds <- round(((n1_home_fcc$x/n1_home_games))/n1_avg_HFC, digits = 4)
# p1_home_fds <- round(((p1_home_fcc$x/p1_home_games))/p1_avg_HFC, digits = 4)
# sc0_home_fds <- round(((sc0_home_fcc$x/sc0_home_games))/sc0_avg_HFC, digits = 4)
# sc1_home_fds <- round(((sc1_home_fcc$x/sc1_home_games))/sc1_avg_HFC, digits = 4)
# sc2_home_fds <- round(((sc2_home_fcc$x/sc2_home_games))/sc2_avg_HFC, digits = 4)
# sc3_home_fds <- round(((sc3_home_fcc$x/sc3_home_games))/sc3_avg_HFC, digits = 4)
# sp1_home_fds <- round(((sp1_home_fcc$x/sp1_home_games))/sp1_avg_HFC, digits = 4)
# sp2_home_fds <- round(((sp2_home_fcc$x/sp2_home_games))/sp2_avg_HFC, digits = 4)
# t1_home_fds <- round(((t1_home_fcc$x/t1_home_games))/t1_avg_HFC, digits = 4)
#away defense strength
mls_away_fds <- round(((mls_away_fcc$x/mls_away_games))/mls_avg_AFC, digits = 4)
arg_away_fds <- round(((arg_away_fcc$x/arg_away_games))/arg_avg_AFC, digits = 4)
# d2_away_fds <- round(((d2_away_fcc$x/d2_away_games))/d2_avg_AFC, digits = 4)
# e0_away_fds <- round(((e0_away_fcc$x/e0_away_games))/e0_avg_AFC, digits = 4)
# e1_away_fds <- round(((e1_away_fcc$x/e1_away_games))/e1_avg_AFC, digits = 4)
# e2_away_fds <- round(((e2_away_fcc$x/e2_away_games))/e2_avg_AFC, digits = 4)
# e3_away_fds <- round(((e3_away_fcc$x/e3_away_games))/e3_avg_AFC, digits = 4)
# ec_away_fds <- round(((ec_away_fcc$x/ec_away_games))/ec_avg_AFC, digits = 4)
# f1_away_fds <- round(((f1_away_fcc$x/f1_away_games))/f1_avg_AFC, digits = 4)
# f2_away_fds <- round(((f2_away_fcc$x/f2_away_games))/f2_avg_AFC, digits = 4)
# g1_away_fds <- round(((g1_away_fcc$x/g1_away_games))/g1_avg_AFC, digits = 4)
# i1_away_fds <- round(((i1_away_fcc$x/i1_away_games))/i1_avg_AFC, digits = 4)
# i2_away_fds <- round(((i2_away_fcc$x/i2_away_games))/i2_avg_AFC, digits = 4)
# n1_away_fds <- round(((n1_away_fcc$x/n1_away_games))/n1_avg_AFC, digits = 4)
# p1_away_fds <- round(((p1_away_fcc$x/p1_away_games))/p1_avg_AFC, digits = 4)
# sc0_away_fds <- round(((sc0_away_fcc$x/sc0_away_games))/sc0_avg_AFC, digits = 4)
# sc1_away_fds <- round(((sc1_away_fcc$x/sc1_away_games))/sc1_avg_AFC, digits = 4)
# sc2_away_fds <- round(((sc2_away_fcc$x/sc2_away_games))/sc2_avg_AFC, digits = 4)
# sc3_away_fds <- round(((sc3_away_fcc$x/sc3_away_games))/sc3_avg_AFC, digits = 4)
# sp1_away_fds <- round(((sp1_away_fcc$x/sp1_away_games))/sp1_avg_AFC, digits = 4)
# sp2_away_fds <- round(((sp2_away_fcc$x/sp2_away_games))/sp2_avg_AFC, digits = 4)
# t1_away_fds <- round(((t1_away_fcc$x/t1_away_games))/t1_avg_AFC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_fo <- cbind(mls_division,mls_teams,mls_avg_HF,mls_home_fas,mls_home_fds)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson_fo <- cbind(arg_division,arg_teams,arg_avg_HF,arg_home_fas,arg_home_fds)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_home_poisson_fo <- cbind(d2_division,d2_teams,d2_avg_HF,d2_home_fas,d2_home_fds)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_home_poisson_fo <- cbind(e0_division,e0_teams,e0_avg_HF,e0_home_fas,e0_home_fds)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_home_poisson_fo <- cbind(e1_division,e1_teams,e1_avg_HF,e1_home_fas,e1_home_fds)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_home_poisson_fo <- cbind(e2_division,e2_teams,e2_avg_HF,e2_home_fas,e2_home_fds)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_home_poisson_fo <- cbind(e3_division,e3_teams,e3_avg_HF,e3_home_fas,e3_home_fds)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_home_poisson_fo <- cbind(ec_division,ec_teams,ec_avg_HF,ec_home_fas,ec_home_fds)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_home_poisson_fo <- cbind(f1_division,f1_teams,f1_avg_HF,f1_home_fas,f1_home_fds)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_home_poisson_fo <- cbind(f2_division,f2_teams,f2_avg_HF,f2_home_fas,f2_home_fds)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_home_poisson_fo <- cbind(g1_division,g1_teams,g1_avg_HF,g1_home_fas,g1_home_fds)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_home_poisson_fo <- cbind(i1_division,i1_teams,i1_avg_HF,i1_home_fas,i1_home_fds)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_home_poisson_fo <- cbind(i2_division,i2_teams,i2_avg_HF,i2_home_fas,i2_home_fds)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_home_poisson_fo <- cbind(n1_division,n1_teams,n1_avg_HF,n1_home_fas,n1_home_fds)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_home_poisson_fo <- cbind(p1_division,p1_teams,p1_avg_HF,p1_home_fas,p1_home_fds)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_home_poisson_fo <- cbind(sc0_division,sc0_teams,sc0_avg_HF,sc0_home_fas,sc0_home_fds)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_home_poisson_fo <- cbind(sc1_division,sc1_teams,sc1_avg_HF,sc1_home_fas,sc1_home_fds)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_home_poisson_fo <- cbind(sc2_division,sc2_teams,sc2_avg_HF,sc2_home_fas,sc2_home_fds)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_home_poisson_fo <- cbind(sc3_division,sc3_teams,sc3_avg_HF,sc3_home_fas,sc3_home_fds)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_home_poisson_fo <- cbind(sp1_division,sp1_teams,sp1_avg_HF,sp1_home_fas,sp1_home_fds)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_home_poisson_fo <- cbind(sp2_division,sp2_teams,sp2_avg_HF,sp2_home_fas,sp2_home_fds)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_home_poisson_fo <- cbind(t1_division,t1_teams,t1_avg_HF,t1_home_fas,t1_home_fds)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_fo <- cbind(mls_division,mls_teams,mls_avg_AF,mls_away_fas,mls_away_fds)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson_fo <- cbind(arg_division,arg_teams,arg_avg_AF,arg_away_fas,arg_away_fds)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_away_poisson_fo <- cbind(d2_division,d2_teams,d2_avg_AF,d2_away_fas,d2_away_fds)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_away_poisson_fo <- cbind(e0_division,e0_teams,e0_avg_AF,e0_away_fas,e0_away_fds)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_away_poisson_fo <- cbind(e1_division,e1_teams,e1_avg_AF,e1_away_fas,e1_away_fds)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_away_poisson_fo <- cbind(e2_division,e2_teams,e2_avg_AF,e2_away_fas,e2_away_fds)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_away_poisson_fo <- cbind(e3_division,e3_teams,e3_avg_AF,e3_away_fas,e3_away_fds)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_away_poisson_fo <- cbind(ec_division,ec_teams,ec_avg_AF,ec_away_fas,ec_away_fds)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_away_poisson_fo <- cbind(f1_division,f1_teams,f1_avg_AF,f1_away_fas,f1_away_fds)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_away_poisson_fo <- cbind(f2_division,f2_teams,f2_avg_AF,f2_away_fas,f2_away_fds)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_away_poisson_fo <- cbind(g1_division,g1_teams,g1_avg_AF,g1_away_fas,g1_away_fds)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_away_poisson_fo <- cbind(i1_division,i1_teams,i1_avg_AF,i1_away_fas,i1_away_fds)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_away_poisson_fo <- cbind(i2_division,i2_teams,i2_avg_AF,i2_away_fas,i2_away_fds)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_away_poisson_fo <- cbind(n1_division,n1_teams,n1_avg_AF,n1_away_fas,n1_away_fds)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_away_poisson_fo <- cbind(p1_division,p1_teams,p1_avg_AF,p1_away_fas,p1_away_fds)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_away_poisson_fo <- cbind(sc0_division,sc0_teams,sc0_avg_AF,sc0_away_fas,sc0_away_fds)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_away_poisson_fo <- cbind(sc1_division,sc1_teams,sc1_avg_AF,sc1_away_fas,sc1_away_fds)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_away_poisson_fo <- cbind(sc2_division,sc2_teams,sc2_avg_AF,sc2_away_fas,sc2_away_fds)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_away_poisson_fo <- cbind(sc3_division,sc3_teams,sc3_avg_AF,sc3_away_fas,sc3_away_fds)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_away_poisson_fo <- cbind(sp1_division,sp1_teams,sp1_avg_AF,sp1_away_fas,sp1_away_fds)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_away_poisson_fo <- cbind(sp2_division,sp2_teams,sp2_avg_AF,sp2_away_fas,sp2_away_fds)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_away_poisson_fo <- cbind(t1_division,t1_teams,t1_avg_AF,t1_away_fas,t1_away_fds)
#create home and away csv
home_poisson_fo <- rbind(mls_home_poisson_fo,arg_home_poisson_fo)
away_poisson_fo <- rbind(mls_away_poisson_fo,arg_away_poisson_fo)
# #delete current
# unlink("R_home.csv")
# unlink("R_away.csv")
# #write another one
# write.csv(home_poisson,'R_home.csv')
# write.csv(away_poisson,'R_away.csv')
#MLS
HomeTeam_mls_fo <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_fo <- rep(mls_teams, length(mls_teams))
MLS_fixtures_fo <- cbind(HomeTeam_mls_fo,AwayTeam_mls_fo)
MLS_fixtures_fo <- as.data.frame(MLS_fixtures_fo)
MLS_fixtures_fo <- MLS_fixtures_fo[!MLS_fixtures_fo$HomeTeam_mls_fo == MLS_fixtures_fo$AwayTeam_mls_fo,]
rownames(MLS_fixtures_fo) <- NULL
MLS_fixtures_fo$Div <- "MLS"
MLS_fixtures_fo <- MLS_fixtures_fo[,c(3,1,2)]

MLS_fixtures_fo$avg_HF_mls <- mls_avg_HF

MLS_fixtures_fo$mls_homefas <- rep(mls_home_fas,each = length(mls_teams)-1)

mls_awayfds_lookup <- cbind(mls_teams,mls_away_fds)

mls_awayfds_lookup <- as.data.frame(mls_awayfds_lookup)

colnames(mls_awayfds_lookup) <- c("AwayTeam_mls_fo","mls_awayfds")


require('RH2')
MLS_fixtures_fo$mls_awayfds <- sqldf("SELECT mls_awayfds_lookup.mls_awayfds FROM mls_awayfds_lookup INNER JOIN MLS_fixtures_fo ON mls_awayfds_lookup.AwayTeam_mls_fo = MLS_fixtures_fo.AwayTeam_mls_fo")

MLS_fixtures_fo$avg_AF_mls <- mls_avg_AF

mls_awayfas_lookup <- cbind(mls_teams,mls_away_fas)

mls_awayfas_lookup <- as.data.frame(mls_awayfas_lookup)

colnames(mls_awayfas_lookup) <- c("AwayTeam_mls_fo","mls_awayfas")

MLS_fixtures_fo$mls_awayfas <- sqldf("SELECT mls_awayfas_lookup.mls_awayfas FROM mls_awayfas_lookup INNER JOIN MLS_fixtures_fo ON mls_awayfas_lookup.AwayTeam_mls_fo = MLS_fixtures_fo.AwayTeam_mls_fo")

MLS_fixtures_fo$mls_homefds <- rep(mls_home_fds,each = length(mls_teams)-1)

MLS_fixtures_fo$mls_awayfds <- as.numeric(unlist(MLS_fixtures_fo$mls_awayfds))
#xGH
MLS_fixtures_fo$mls_xHF <- MLS_fixtures_fo$avg_HF_mls * MLS_fixtures_fo$mls_homefas * MLS_fixtures_fo$mls_awayfds
#xGA

MLS_fixtures_fo$mls_awayfas <- as.numeric(unlist(MLS_fixtures_fo$mls_awayfas))

MLS_fixtures_fo$mls_xAF <- MLS_fixtures_fo$avg_AF_mls * MLS_fixtures_fo$mls_awayfas * MLS_fixtures_fo$mls_homefds

MLS_fixtures_fo$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(0,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(1,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(2,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(3,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(4,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_fo$mls_xHF) * stats::dpois(5,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
MLS_fixtures_fo$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_fo$mls_xHF) * stats::dpois(6,MLS_fixtures_fo$mls_xAF), digits = 4)
#Home win
MLS_fixtures_fo$mls_H <- (
  MLS_fixtures_fo$mls_1_0 + MLS_fixtures_fo$mls_2_0 + MLS_fixtures_fo$mls_2_1 + MLS_fixtures_fo$mls_3_0 + MLS_fixtures_fo$mls_3_1 +
    MLS_fixtures_fo$mls_3_2 + MLS_fixtures_fo$mls_4_0 + MLS_fixtures_fo$mls_4_1 + MLS_fixtures_fo$mls_4_2 + MLS_fixtures_fo$mls_4_3 +
    MLS_fixtures_fo$mls_5_0 + MLS_fixtures_fo$mls_5_1 + MLS_fixtures_fo$mls_5_2 + MLS_fixtures_fo$mls_5_3 + MLS_fixtures_fo$mls_5_4 +
    MLS_fixtures_fo$mls_6_0 + MLS_fixtures_fo$mls_6_1 + MLS_fixtures_fo$mls_6_2 + MLS_fixtures_fo$mls_6_3 + MLS_fixtures_fo$mls_6_4 +
    MLS_fixtures_fo$mls_6_5
)

MLS_fixtures_fo$mls_H <- percent(MLS_fixtures_fo$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_fo$mls_D <- (

  MLS_fixtures_fo$mls_0_0 + MLS_fixtures_fo$mls_1_1 + MLS_fixtures_fo$mls_2_2 + MLS_fixtures_fo$mls_3_3 + MLS_fixtures_fo$mls_4_4 +
    MLS_fixtures_fo$mls_5_5 + MLS_fixtures_fo$mls_6_6
)

MLS_fixtures_fo$mls_D <- percent(MLS_fixtures_fo$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_fo$mls_A <- (
  MLS_fixtures_fo$mls_0_1 + MLS_fixtures_fo$mls_0_2 + MLS_fixtures_fo$mls_1_2 + MLS_fixtures_fo$mls_0_3 + MLS_fixtures_fo$mls_1_3 +
    MLS_fixtures_fo$mls_2_3 + MLS_fixtures_fo$mls_0_4 + MLS_fixtures_fo$mls_1_4 + MLS_fixtures_fo$mls_2_4 + MLS_fixtures_fo$mls_3_4 +
    MLS_fixtures_fo$mls_0_5 + MLS_fixtures_fo$mls_1_5 + MLS_fixtures_fo$mls_2_5 + MLS_fixtures_fo$mls_3_5 + MLS_fixtures_fo$mls_4_5 +
    MLS_fixtures_fo$mls_0_6 + MLS_fixtures_fo$mls_1_6 + MLS_fixtures_fo$mls_2_6 + MLS_fixtures_fo$mls_3_6 + MLS_fixtures_fo$mls_4_6 +
    MLS_fixtures_fo$mls_5_6
)

MLS_fixtures_fo$mls_A <- percent(MLS_fixtures_fo$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_fo$mls_ov25 <- (
  MLS_fixtures_fo$mls_2_1 + MLS_fixtures_fo$mls_1_2 + MLS_fixtures_fo$mls_2_2 + MLS_fixtures_fo$mls_3_0 + MLS_fixtures_fo$mls_3_1 +
    MLS_fixtures_fo$mls_3_2 + MLS_fixtures_fo$mls_0_3 + MLS_fixtures_fo$mls_1_3 + MLS_fixtures_fo$mls_2_3 + MLS_fixtures_fo$mls_3_3 +
    MLS_fixtures_fo$mls_4_0 + MLS_fixtures_fo$mls_4_1 + MLS_fixtures_fo$mls_4_2 + MLS_fixtures_fo$mls_4_3 + MLS_fixtures_fo$mls_0_4 +
    MLS_fixtures_fo$mls_1_4 + MLS_fixtures_fo$mls_2_4 + MLS_fixtures_fo$mls_3_4 + MLS_fixtures_fo$mls_4_4 + MLS_fixtures_fo$mls_5_0 +
    MLS_fixtures_fo$mls_5_1 + MLS_fixtures_fo$mls_5_2 + MLS_fixtures_fo$mls_5_3 + MLS_fixtures_fo$mls_5_4 + MLS_fixtures_fo$mls_0_5 +
    MLS_fixtures_fo$mls_1_5 + MLS_fixtures_fo$mls_2_5 + MLS_fixtures_fo$mls_3_5 + MLS_fixtures_fo$mls_4_5 + MLS_fixtures_fo$mls_5_5 +
    MLS_fixtures_fo$mls_6_0 + MLS_fixtures_fo$mls_6_1 + MLS_fixtures_fo$mls_6_2 + MLS_fixtures_fo$mls_6_3 + MLS_fixtures_fo$mls_6_4 +
    MLS_fixtures_fo$mls_6_5 + MLS_fixtures_fo$mls_0_6 + MLS_fixtures_fo$mls_1_6 + MLS_fixtures_fo$mls_2_6 + MLS_fixtures_fo$mls_3_6 +
    MLS_fixtures_fo$mls_4_6 + MLS_fixtures_fo$mls_5_6 + MLS_fixtures_fo$mls_6_6
)
#un25
MLS_fixtures_fo$mls_un25 <- (
  MLS_fixtures_fo$mls_0_0 + MLS_fixtures_fo$mls_1_0 + MLS_fixtures_fo$mls_0_1 + MLS_fixtures_fo$mls_1_1 + MLS_fixtures_fo$mls_2_0 + MLS_fixtures_fo$mls_0_2
)
#odds
MLS_fixtures_fo$mls_ov25_odds <- round((1/MLS_fixtures_fo$mls_ov25),digits = 2)
MLS_fixtures_fo$mls_un25_odds <- round((1/MLS_fixtures_fo$mls_un25),digits = 2)

MLS_fixtures_fo$mls_ov25_odds
MLS_fixtures_fo$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_fo$mls_ov25 <- percent(MLS_fixtures_fo$mls_ov25, accuracy = 0.1)

MLS_fixtures_fo$mls_un25 <- percent(MLS_fixtures_fo$mls_un25, accuracy = 0.1)
MLS_fixtures_fo$mls_psfore <- paste(round(MLS_fixtures_fo$mls_xHF,digits = 0),round(MLS_fixtures_fo$mls_xAF,digits = 0),sep = "-")
#write out

#write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
##################################################################################################################
#ARG
HomeTeam_arg_fo <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg_fo <- rep(arg_teams, length(arg_teams))
ARG_fixtures_fo <- cbind(HomeTeam_arg_fo,AwayTeam_arg_fo)
ARG_fixtures_fo <- as.data.frame(ARG_fixtures_fo)
ARG_fixtures_fo <- ARG_fixtures_fo[!ARG_fixtures_fo$HomeTeam_arg_fo == ARG_fixtures_fo$AwayTeam_arg_fo,]
rownames(ARG_fixtures_fo) <- NULL
ARG_fixtures_fo$Div <- "ARG"
ARG_fixtures_fo <- ARG_fixtures_fo[,c(3,1,2)]

ARG_fixtures_fo$avg_HF_arg <- arg_avg_HF

ARG_fixtures_fo$arg_homefas <- rep(arg_home_fas,each = length(arg_teams)-1)

arg_awayfds_lookup <- cbind(arg_teams,arg_away_fds)

arg_awayfds_lookup <- as.data.frame(arg_awayfds_lookup)

colnames(arg_awayfds_lookup) <- c("AwayTeam_arg_fo","arg_awayfds")


require('RH2')
ARG_fixtures_fo$arg_awayfds <- sqldf("SELECT arg_awayfds_lookup.arg_awayfds FROM arg_awayfds_lookup INNER JOIN ARG_fixtures_fo ON arg_awayfds_lookup.AwayTeam_arg_fo = ARG_fixtures_fo.AwayTeam_arg_fo")

ARG_fixtures_fo$avg_AF_arg <- arg_avg_AF

arg_awayfas_lookup <- cbind(arg_teams,arg_away_fas)

arg_awayfas_lookup <- as.data.frame(arg_awayfas_lookup)

colnames(arg_awayfas_lookup) <- c("AwayTeam_arg_fo","arg_awayfas")

ARG_fixtures_fo$arg_awayfas <- sqldf("SELECT arg_awayfas_lookup.arg_awayfas FROM arg_awayfas_lookup INNER JOIN ARG_fixtures_fo ON arg_awayfas_lookup.AwayTeam_arg_fo = ARG_fixtures_fo.AwayTeam_arg_fo")

ARG_fixtures_fo$arg_homefds <- rep(arg_home_fds,each = length(arg_teams)-1)

ARG_fixtures_fo$arg_awayfds <- as.numeric(unlist(ARG_fixtures_fo$arg_awayfds))
#xGH
ARG_fixtures_fo$arg_xHF <- ARG_fixtures_fo$avg_HF_arg * ARG_fixtures_fo$arg_homefas * ARG_fixtures_fo$arg_awayfds
#xGA

ARG_fixtures_fo$arg_awayfas <- as.numeric(unlist(ARG_fixtures_fo$arg_awayfas))

ARG_fixtures_fo$arg_xAF <- ARG_fixtures_fo$avg_AF_arg * ARG_fixtures_fo$arg_awayfas * ARG_fixtures_fo$arg_homefds

ARG_fixtures_fo$arg_0_0 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_0 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_1 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_1 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_0 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_2 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_2 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_1 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_2 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_3 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_0 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_1 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_2 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_3 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_3 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_3 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_4 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_0 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_1 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_2 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_3 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_4 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_4 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_4 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_4 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_5 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_0 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_1 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_2 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_3 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_4 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_5 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_5 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_5 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_5 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_5 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_6 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_0 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(0,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_1 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(1,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_2 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(2,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_3 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(3,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_4 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(4,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_6_5 <- round(stats::dpois(6,ARG_fixtures_fo$arg_xHF) * stats::dpois(5,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_0_6 <- round(stats::dpois(0,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_1_6 <- round(stats::dpois(1,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_2_6 <- round(stats::dpois(2,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_3_6 <- round(stats::dpois(3,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_4_6 <- round(stats::dpois(4,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
ARG_fixtures_fo$arg_5_6 <- round(stats::dpois(5,ARG_fixtures_fo$arg_xHF) * stats::dpois(6,ARG_fixtures_fo$arg_xAF), digits = 4)
#Home win
ARG_fixtures_fo$arg_H <- (
  ARG_fixtures_fo$arg_1_0 + ARG_fixtures_fo$arg_2_0 + ARG_fixtures_fo$arg_2_1 + ARG_fixtures_fo$arg_3_0 + ARG_fixtures_fo$arg_3_1 +
    ARG_fixtures_fo$arg_3_2 + ARG_fixtures_fo$arg_4_0 + ARG_fixtures_fo$arg_4_1 + ARG_fixtures_fo$arg_4_2 + ARG_fixtures_fo$arg_4_3 +
    ARG_fixtures_fo$arg_5_0 + ARG_fixtures_fo$arg_5_1 + ARG_fixtures_fo$arg_5_2 + ARG_fixtures_fo$arg_5_3 + ARG_fixtures_fo$arg_5_4 +
    ARG_fixtures_fo$arg_6_0 + ARG_fixtures_fo$arg_6_1 + ARG_fixtures_fo$arg_6_2 + ARG_fixtures_fo$arg_6_3 + ARG_fixtures_fo$arg_6_4 +
    ARG_fixtures_fo$arg_6_5
)

ARG_fixtures_fo$arg_H <- percent(ARG_fixtures_fo$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures_fo$arg_D <- (

  ARG_fixtures_fo$arg_0_0 + ARG_fixtures_fo$arg_1_1 + ARG_fixtures_fo$arg_2_2 + ARG_fixtures_fo$arg_3_3 + ARG_fixtures_fo$arg_4_4 +
    ARG_fixtures_fo$arg_5_5 + ARG_fixtures_fo$arg_6_6
)

ARG_fixtures_fo$arg_D <- percent(ARG_fixtures_fo$arg_D, accuracy = 0.1)

#Away

ARG_fixtures_fo$arg_A <- (
  ARG_fixtures_fo$arg_0_1 + ARG_fixtures_fo$arg_0_2 + ARG_fixtures_fo$arg_1_2 + ARG_fixtures_fo$arg_0_3 + ARG_fixtures_fo$arg_1_3 +
    ARG_fixtures_fo$arg_2_3 + ARG_fixtures_fo$arg_0_4 + ARG_fixtures_fo$arg_1_4 + ARG_fixtures_fo$arg_2_4 + ARG_fixtures_fo$arg_3_4 +
    ARG_fixtures_fo$arg_0_5 + ARG_fixtures_fo$arg_1_5 + ARG_fixtures_fo$arg_2_5 + ARG_fixtures_fo$arg_3_5 + ARG_fixtures_fo$arg_4_5 +
    ARG_fixtures_fo$arg_0_6 + ARG_fixtures_fo$arg_1_6 + ARG_fixtures_fo$arg_2_6 + ARG_fixtures_fo$arg_3_6 + ARG_fixtures_fo$arg_4_6 +
    ARG_fixtures_fo$arg_5_6
)

ARG_fixtures_fo$arg_A <- percent(ARG_fixtures_fo$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures_fo$arg_ov25 <- (
  ARG_fixtures_fo$arg_2_1 + ARG_fixtures_fo$arg_1_2 + ARG_fixtures_fo$arg_2_2 + ARG_fixtures_fo$arg_3_0 + ARG_fixtures_fo$arg_3_1 +
    ARG_fixtures_fo$arg_3_2 + ARG_fixtures_fo$arg_0_3 + ARG_fixtures_fo$arg_1_3 + ARG_fixtures_fo$arg_2_3 + ARG_fixtures_fo$arg_3_3 +
    ARG_fixtures_fo$arg_4_0 + ARG_fixtures_fo$arg_4_1 + ARG_fixtures_fo$arg_4_2 + ARG_fixtures_fo$arg_4_3 + ARG_fixtures_fo$arg_0_4 +
    ARG_fixtures_fo$arg_1_4 + ARG_fixtures_fo$arg_2_4 + ARG_fixtures_fo$arg_3_4 + ARG_fixtures_fo$arg_4_4 + ARG_fixtures_fo$arg_5_0 +
    ARG_fixtures_fo$arg_5_1 + ARG_fixtures_fo$arg_5_2 + ARG_fixtures_fo$arg_5_3 + ARG_fixtures_fo$arg_5_4 + ARG_fixtures_fo$arg_0_5 +
    ARG_fixtures_fo$arg_1_5 + ARG_fixtures_fo$arg_2_5 + ARG_fixtures_fo$arg_3_5 + ARG_fixtures_fo$arg_4_5 + ARG_fixtures_fo$arg_5_5 +
    ARG_fixtures_fo$arg_6_0 + ARG_fixtures_fo$arg_6_1 + ARG_fixtures_fo$arg_6_2 + ARG_fixtures_fo$arg_6_3 + ARG_fixtures_fo$arg_6_4 +
    ARG_fixtures_fo$arg_6_5 + ARG_fixtures_fo$arg_0_6 + ARG_fixtures_fo$arg_1_6 + ARG_fixtures_fo$arg_2_6 + ARG_fixtures_fo$arg_3_6 +
    ARG_fixtures_fo$arg_4_6 + ARG_fixtures_fo$arg_5_6 + ARG_fixtures_fo$arg_6_6
)
#un25
ARG_fixtures_fo$arg_un25 <- (
  ARG_fixtures_fo$arg_0_0 + ARG_fixtures_fo$arg_1_0 + ARG_fixtures_fo$arg_0_1 + ARG_fixtures_fo$arg_1_1 + ARG_fixtures_fo$arg_2_0 + ARG_fixtures_fo$arg_0_2
)
#odds
ARG_fixtures_fo$arg_ov25_odds <- round((1/ARG_fixtures_fo$arg_ov25),digits = 2)
ARG_fixtures_fo$arg_un25_odds <- round((1/ARG_fixtures_fo$arg_un25),digits = 2)

ARG_fixtures_fo$arg_ov25_odds
ARG_fixtures_fo$arg_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
ARG_fixtures_fo$arg_ov25 <- percent(ARG_fixtures_fo$arg_ov25, accuracy = 0.1)

ARG_fixtures_fo$arg_un25 <- percent(ARG_fixtures_fo$arg_un25, accuracy = 0.1)
ARG_fixtures_fo$arg_psfore <- paste(round(ARG_fixtures_fo$arg_xHF,digits = 0),round(ARG_fixtures_fo$arg_xAF,digits = 0),sep = "-")
#write out
#write.xlsx(ARG_fixtures,'Divisions/ARG.xlsx',sheetName = "ARG", append = TRUE)
#################################################################################################################
#D2
# HomeTeam_d2_fo <- rep(d2_teams, each = length(d2_teams))
# AwayTeam_d2_fo <- rep(d2_teams, length(d2_teams))
# D2_fixtures_fo <- cbind(HomeTeam_d2_fo,AwayTeam_d2_fo)
# D2_fixtures_fo <- as.data.frame(D2_fixtures_fo)
# D2_fixtures_fo <- D2_fixtures_fo[!D2_fixtures_fo$HomeTeam_d2_fo == D2_fixtures_fo$AwayTeam_d2_fo,]
# rownames(D2_fixtures_fo) <- NULL
# D2_fixtures_fo$Div <- "D2"
# D2_fixtures_fo <- D2_fixtures_fo[,c(3,1,2)]
#
# D2_fixtures_fo$avg_HF_d2 <- d2_avg_HF
#
# D2_fixtures_fo$d2_homefas <- rep(d2_home_fas,each = length(d2_teams)-1)
#
# d2_awayfds_lookup <- cbind(d2_teams,d2_away_fds)
#
# d2_awayfds_lookup <- as.data.frame(d2_awayfds_lookup)
#
# colnames(d2_awayfds_lookup) <- c("AwayTeam_d2_fo","d2_awayfds")
#
#
# require('RH2')
# D2_fixtures_fo$d2_awayfds <- sqldf("SELECT d2_awayfds_lookup.d2_awayfds FROM d2_awayfds_lookup INNER JOIN D2_fixtures_fo ON d2_awayfds_lookup.AwayTeam_d2_fo = D2_fixtures_fo.AwayTeam_d2_fo")
#
# D2_fixtures_fo$avg_AF_d2 <- d2_avg_AF
#
# d2_awayfas_lookup <- cbind(d2_teams,d2_away_fas)
#
# d2_awayfas_lookup <- as.data.frame(d2_awayfas_lookup)
#
# colnames(d2_awayfas_lookup) <- c("AwayTeam_d2_fo","d2_awayfas")
#
# D2_fixtures_fo$d2_awayfas <- sqldf("SELECT d2_awayfas_lookup.d2_awayfas FROM d2_awayfas_lookup INNER JOIN D2_fixtures_fo ON d2_awayfas_lookup.AwayTeam_d2_fo = D2_fixtures_fo.AwayTeam_d2_fo")
#
# D2_fixtures_fo$d2_homefds <- rep(d2_home_fds,each = length(d2_teams)-1)
#
# D2_fixtures_fo$d2_awayfds <- as.numeric(unlist(D2_fixtures_fo$d2_awayfds))
# #xGH
# D2_fixtures_fo$d2_xHF <- D2_fixtures_fo$avg_HF_d2 * D2_fixtures_fo$d2_homefas * D2_fixtures_fo$d2_awayfds
# #xGA
#
# D2_fixtures_fo$d2_awayfas <- as.numeric(unlist(D2_fixtures_fo$d2_awayfas))
#
# D2_fixtures_fo$d2_xAF <- D2_fixtures_fo$avg_AF_d2 * D2_fixtures_fo$d2_awayfas * D2_fixtures_fo$d2_homefds
#
# D2_fixtures_fo$d2_0_0 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_0 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_1 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_1 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_0 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_2 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_2 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_1 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_2 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_3 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_0 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_1 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_2 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_3 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_3 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_3 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_4 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_0 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_1 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_2 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_3 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_4 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_4 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_4 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_4 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_5 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_0 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_1 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_2 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_3 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_4 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_5 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_5 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_5 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_5 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_5 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_6 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_0 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(0,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_1 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(1,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_2 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(2,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_3 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(3,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_4 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(4,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_6_5 <- round(stats::dpois(6,D2_fixtures_fo$d2_xHF) * stats::dpois(5,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_0_6 <- round(stats::dpois(0,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_1_6 <- round(stats::dpois(1,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_2_6 <- round(stats::dpois(2,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_3_6 <- round(stats::dpois(3,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_4_6 <- round(stats::dpois(4,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# D2_fixtures_fo$d2_5_6 <- round(stats::dpois(5,D2_fixtures_fo$d2_xHF) * stats::dpois(6,D2_fixtures_fo$d2_xAF), digits = 4)
# #Home win
# D2_fixtures_fo$d2_H <- (
#   D2_fixtures_fo$d2_1_0 + D2_fixtures_fo$d2_2_0 + D2_fixtures_fo$d2_2_1 + D2_fixtures_fo$d2_3_0 + D2_fixtures_fo$d2_3_1 +
#     D2_fixtures_fo$d2_3_2 + D2_fixtures_fo$d2_4_0 + D2_fixtures_fo$d2_4_1 + D2_fixtures_fo$d2_4_2 + D2_fixtures_fo$d2_4_3 +
#     D2_fixtures_fo$d2_5_0 + D2_fixtures_fo$d2_5_1 + D2_fixtures_fo$d2_5_2 + D2_fixtures_fo$d2_5_3 + D2_fixtures_fo$d2_5_4 +
#     D2_fixtures_fo$d2_6_0 + D2_fixtures_fo$d2_6_1 + D2_fixtures_fo$d2_6_2 + D2_fixtures_fo$d2_6_3 + D2_fixtures_fo$d2_6_4 +
#     D2_fixtures_fo$d2_6_5
# )
#
# D2_fixtures_fo$d2_H <- percent(D2_fixtures_fo$d2_H, accuracy = 0.1)
#
# #Draw
# D2_fixtures_fo$d2_D <- (
#
#   D2_fixtures_fo$d2_0_0 + D2_fixtures_fo$d2_1_1 + D2_fixtures_fo$d2_2_2 + D2_fixtures_fo$d2_3_3 + D2_fixtures_fo$d2_4_4 +
#     D2_fixtures_fo$d2_5_5 + D2_fixtures_fo$d2_6_6
# )
#
# D2_fixtures_fo$d2_D <- percent(D2_fixtures_fo$d2_D, accuracy = 0.1)
#
# #Away
#
# D2_fixtures_fo$d2_A <- (
#   D2_fixtures_fo$d2_0_1 + D2_fixtures_fo$d2_0_2 + D2_fixtures_fo$d2_1_2 + D2_fixtures_fo$d2_0_3 + D2_fixtures_fo$d2_1_3 +
#     D2_fixtures_fo$d2_2_3 + D2_fixtures_fo$d2_0_4 + D2_fixtures_fo$d2_1_4 + D2_fixtures_fo$d2_2_4 + D2_fixtures_fo$d2_3_4 +
#     D2_fixtures_fo$d2_0_5 + D2_fixtures_fo$d2_1_5 + D2_fixtures_fo$d2_2_5 + D2_fixtures_fo$d2_3_5 + D2_fixtures_fo$d2_4_5 +
#     D2_fixtures_fo$d2_0_6 + D2_fixtures_fo$d2_1_6 + D2_fixtures_fo$d2_2_6 + D2_fixtures_fo$d2_3_6 + D2_fixtures_fo$d2_4_6 +
#     D2_fixtures_fo$d2_5_6
# )
#
# D2_fixtures_fo$d2_A <- percent(D2_fixtures_fo$d2_A, accuracy = 0.1)
#
# #ov25
# D2_fixtures_fo$d2_ov25 <- (
#   D2_fixtures_fo$d2_2_1 + D2_fixtures_fo$d2_1_2 + D2_fixtures_fo$d2_2_2 + D2_fixtures_fo$d2_3_0 + D2_fixtures_fo$d2_3_1 +
#     D2_fixtures_fo$d2_3_2 + D2_fixtures_fo$d2_0_3 + D2_fixtures_fo$d2_1_3 + D2_fixtures_fo$d2_2_3 + D2_fixtures_fo$d2_3_3 +
#     D2_fixtures_fo$d2_4_0 + D2_fixtures_fo$d2_4_1 + D2_fixtures_fo$d2_4_2 + D2_fixtures_fo$d2_4_3 + D2_fixtures_fo$d2_0_4 +
#     D2_fixtures_fo$d2_1_4 + D2_fixtures_fo$d2_2_4 + D2_fixtures_fo$d2_3_4 + D2_fixtures_fo$d2_4_4 + D2_fixtures_fo$d2_5_0 +
#     D2_fixtures_fo$d2_5_1 + D2_fixtures_fo$d2_5_2 + D2_fixtures_fo$d2_5_3 + D2_fixtures_fo$d2_5_4 + D2_fixtures_fo$d2_0_5 +
#     D2_fixtures_fo$d2_1_5 + D2_fixtures_fo$d2_2_5 + D2_fixtures_fo$d2_3_5 + D2_fixtures_fo$d2_4_5 + D2_fixtures_fo$d2_5_5 +
#     D2_fixtures_fo$d2_6_0 + D2_fixtures_fo$d2_6_1 + D2_fixtures_fo$d2_6_2 + D2_fixtures_fo$d2_6_3 + D2_fixtures_fo$d2_6_4 +
#     D2_fixtures_fo$d2_6_5 + D2_fixtures_fo$d2_0_6 + D2_fixtures_fo$d2_1_6 + D2_fixtures_fo$d2_2_6 + D2_fixtures_fo$d2_3_6 +
#     D2_fixtures_fo$d2_4_6 + D2_fixtures_fo$d2_5_6 + D2_fixtures_fo$d2_6_6
# )
# #un25
# D2_fixtures_fo$d2_un25 <- (
#   D2_fixtures_fo$d2_0_0 + D2_fixtures_fo$d2_1_0 + D2_fixtures_fo$d2_0_1 + D2_fixtures_fo$d2_1_1 + D2_fixtures_fo$d2_2_0 + D2_fixtures_fo$d2_0_2
# )
# #odds
# D2_fixtures_fo$d2_ov25_odds <- round((1/D2_fixtures_fo$d2_ov25),digits = 2)
# D2_fixtures_fo$d2_un25_odds <- round((1/D2_fixtures_fo$d2_un25),digits = 2)
#
# D2_fixtures_fo$d2_ov25_odds
# D2_fixtures_fo$d2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# D2_fixtures_fo$d2_ov25 <- percent(D2_fixtures_fo$d2_ov25, accuracy = 0.1)
#
# D2_fixtures_fo$d2_un25 <- percent(D2_fixtures_fo$d2_un25, accuracy = 0.1)
# D2_fixtures_fo$d2_psfore <- paste(round(D2_fixtures_fo$d2_xHF,digits = 0),round(D2_fixtures_fo$d2_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(D2_fixtures,'Divisions/D2.xlsx',sheetName = "D2", append = TRUE)
# #################################################################################################################
# #E0
# HomeTeam_e0_fo <- rep(e0_teams, each = length(e0_teams))
# AwayTeam_e0_fo <- rep(e0_teams, length(e0_teams))
# E0_fixtures_fo <- cbind(HomeTeam_e0_fo,AwayTeam_e0_fo)
# E0_fixtures_fo <- as.data.frame(E0_fixtures_fo)
# E0_fixtures_fo <- E0_fixtures_fo[!E0_fixtures_fo$HomeTeam_e0_fo == E0_fixtures_fo$AwayTeam_e0_fo,]
# rownames(E0_fixtures_fo) <- NULL
# E0_fixtures_fo$Div <- "E0"
# E0_fixtures_fo <- E0_fixtures_fo[,c(3,1,2)]
#
# E0_fixtures_fo$avg_HF_e0 <- e0_avg_HF
#
# E0_fixtures_fo$e0_homefas <- rep(e0_home_fas,each = length(e0_teams)-1)
#
# e0_awayfds_lookup <- cbind(e0_teams,e0_away_fds)
#
# e0_awayfds_lookup <- as.data.frame(e0_awayfds_lookup)
#
# colnames(e0_awayfds_lookup) <- c("AwayTeam_e0_fo","e0_awayfds")
#
#
# require('RH2')
# E0_fixtures_fo$e0_awayfds <- sqldf("SELECT e0_awayfds_lookup.e0_awayfds FROM e0_awayfds_lookup INNER JOIN E0_fixtures_fo ON e0_awayfds_lookup.AwayTeam_e0_fo = E0_fixtures_fo.AwayTeam_e0_fo")
#
# E0_fixtures_fo$avg_AF_e0 <- e0_avg_AF
#
# e0_awayfas_lookup <- cbind(e0_teams,e0_away_fas)
#
# e0_awayfas_lookup <- as.data.frame(e0_awayfas_lookup)
#
# colnames(e0_awayfas_lookup) <- c("AwayTeam_e0_fo","e0_awayfas")
#
# E0_fixtures_fo$e0_awayfas <- sqldf("SELECT e0_awayfas_lookup.e0_awayfas FROM e0_awayfas_lookup INNER JOIN E0_fixtures_fo ON e0_awayfas_lookup.AwayTeam_e0_fo = E0_fixtures_fo.AwayTeam_e0_fo")
#
# E0_fixtures_fo$e0_homefds <- rep(e0_home_fds,each = length(e0_teams)-1)
#
# E0_fixtures_fo$e0_awayfds <- as.numeric(unlist(E0_fixtures_fo$e0_awayfds))
# #xGH
# E0_fixtures_fo$e0_xHF <- E0_fixtures_fo$avg_HF_e0 * E0_fixtures_fo$e0_homefas * E0_fixtures_fo$e0_awayfds
# #xGA
#
# E0_fixtures_fo$e0_awayfas <- as.numeric(unlist(E0_fixtures_fo$e0_awayfas))
#
# E0_fixtures_fo$e0_xAF <- E0_fixtures_fo$avg_AF_e0 * E0_fixtures_fo$e0_awayfas * E0_fixtures_fo$e0_homefds
#
# E0_fixtures_fo$e0_0_0 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_0 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_1 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_1 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_0 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_2 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_2 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_1 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_2 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_3 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_0 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_1 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_2 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_3 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_3 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_3 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_4 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_0 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_1 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_2 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_3 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_4 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_4 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_4 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_4 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_5 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_0 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_1 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_2 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_3 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_4 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_5 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_5 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_5 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_5 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_5 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_6 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_0 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(0,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_1 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(1,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_2 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(2,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_3 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(3,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_4 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(4,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_6_5 <- round(stats::dpois(6,E0_fixtures_fo$e0_xHF) * stats::dpois(5,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_0_6 <- round(stats::dpois(0,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_1_6 <- round(stats::dpois(1,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_2_6 <- round(stats::dpois(2,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_3_6 <- round(stats::dpois(3,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_4_6 <- round(stats::dpois(4,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# E0_fixtures_fo$e0_5_6 <- round(stats::dpois(5,E0_fixtures_fo$e0_xHF) * stats::dpois(6,E0_fixtures_fo$e0_xAF), digits = 4)
# #Home win
# E0_fixtures_fo$e0_H <- (
#   E0_fixtures_fo$e0_1_0 + E0_fixtures_fo$e0_2_0 + E0_fixtures_fo$e0_2_1 + E0_fixtures_fo$e0_3_0 + E0_fixtures_fo$e0_3_1 +
#     E0_fixtures_fo$e0_3_2 + E0_fixtures_fo$e0_4_0 + E0_fixtures_fo$e0_4_1 + E0_fixtures_fo$e0_4_2 + E0_fixtures_fo$e0_4_3 +
#     E0_fixtures_fo$e0_5_0 + E0_fixtures_fo$e0_5_1 + E0_fixtures_fo$e0_5_2 + E0_fixtures_fo$e0_5_3 + E0_fixtures_fo$e0_5_4 +
#     E0_fixtures_fo$e0_6_0 + E0_fixtures_fo$e0_6_1 + E0_fixtures_fo$e0_6_2 + E0_fixtures_fo$e0_6_3 + E0_fixtures_fo$e0_6_4 +
#     E0_fixtures_fo$e0_6_5
# )
#
# E0_fixtures_fo$e0_H <- percent(E0_fixtures_fo$e0_H, accuracy = 0.1)
#
# #Draw
# E0_fixtures_fo$e0_D <- (
#
#   E0_fixtures_fo$e0_0_0 + E0_fixtures_fo$e0_1_1 + E0_fixtures_fo$e0_2_2 + E0_fixtures_fo$e0_3_3 + E0_fixtures_fo$e0_4_4 +
#     E0_fixtures_fo$e0_5_5 + E0_fixtures_fo$e0_6_6
# )
#
# E0_fixtures_fo$e0_D <- percent(E0_fixtures_fo$e0_D, accuracy = 0.1)
#
# #Away
#
# E0_fixtures_fo$e0_A <- (
#   E0_fixtures_fo$e0_0_1 + E0_fixtures_fo$e0_0_2 + E0_fixtures_fo$e0_1_2 + E0_fixtures_fo$e0_0_3 + E0_fixtures_fo$e0_1_3 +
#     E0_fixtures_fo$e0_2_3 + E0_fixtures_fo$e0_0_4 + E0_fixtures_fo$e0_1_4 + E0_fixtures_fo$e0_2_4 + E0_fixtures_fo$e0_3_4 +
#     E0_fixtures_fo$e0_0_5 + E0_fixtures_fo$e0_1_5 + E0_fixtures_fo$e0_2_5 + E0_fixtures_fo$e0_3_5 + E0_fixtures_fo$e0_4_5 +
#     E0_fixtures_fo$e0_0_6 + E0_fixtures_fo$e0_1_6 + E0_fixtures_fo$e0_2_6 + E0_fixtures_fo$e0_3_6 + E0_fixtures_fo$e0_4_6 +
#     E0_fixtures_fo$e0_5_6
# )
#
# E0_fixtures_fo$e0_A <- percent(E0_fixtures_fo$e0_A, accuracy = 0.1)
#
# #ov25
# E0_fixtures_fo$e0_ov25 <- (
#   E0_fixtures_fo$e0_2_1 + E0_fixtures_fo$e0_1_2 + E0_fixtures_fo$e0_2_2 + E0_fixtures_fo$e0_3_0 + E0_fixtures_fo$e0_3_1 +
#     E0_fixtures_fo$e0_3_2 + E0_fixtures_fo$e0_0_3 + E0_fixtures_fo$e0_1_3 + E0_fixtures_fo$e0_2_3 + E0_fixtures_fo$e0_3_3 +
#     E0_fixtures_fo$e0_4_0 + E0_fixtures_fo$e0_4_1 + E0_fixtures_fo$e0_4_2 + E0_fixtures_fo$e0_4_3 + E0_fixtures_fo$e0_0_4 +
#     E0_fixtures_fo$e0_1_4 + E0_fixtures_fo$e0_2_4 + E0_fixtures_fo$e0_3_4 + E0_fixtures_fo$e0_4_4 + E0_fixtures_fo$e0_5_0 +
#     E0_fixtures_fo$e0_5_1 + E0_fixtures_fo$e0_5_2 + E0_fixtures_fo$e0_5_3 + E0_fixtures_fo$e0_5_4 + E0_fixtures_fo$e0_0_5 +
#     E0_fixtures_fo$e0_1_5 + E0_fixtures_fo$e0_2_5 + E0_fixtures_fo$e0_3_5 + E0_fixtures_fo$e0_4_5 + E0_fixtures_fo$e0_5_5 +
#     E0_fixtures_fo$e0_6_0 + E0_fixtures_fo$e0_6_1 + E0_fixtures_fo$e0_6_2 + E0_fixtures_fo$e0_6_3 + E0_fixtures_fo$e0_6_4 +
#     E0_fixtures_fo$e0_6_5 + E0_fixtures_fo$e0_0_6 + E0_fixtures_fo$e0_1_6 + E0_fixtures_fo$e0_2_6 + E0_fixtures_fo$e0_3_6 +
#     E0_fixtures_fo$e0_4_6 + E0_fixtures_fo$e0_5_6 + E0_fixtures_fo$e0_6_6
# )
# #un25
# E0_fixtures_fo$e0_un25 <- (
#   E0_fixtures_fo$e0_0_0 + E0_fixtures_fo$e0_1_0 + E0_fixtures_fo$e0_0_1 + E0_fixtures_fo$e0_1_1 + E0_fixtures_fo$e0_2_0 + E0_fixtures_fo$e0_0_2
# )
# #odds
# E0_fixtures_fo$e0_ov25_odds <- round((1/E0_fixtures_fo$e0_ov25),digits = 2)
# E0_fixtures_fo$e0_un25_odds <- round((1/E0_fixtures_fo$e0_un25),digits = 2)
#
# E0_fixtures_fo$e0_ov25_odds
# E0_fixtures_fo$e0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E0_fixtures_fo$e0_ov25 <- percent(E0_fixtures_fo$e0_ov25, accuracy = 0.1)
#
# E0_fixtures_fo$e0_un25 <- percent(E0_fixtures_fo$e0_un25, accuracy = 0.1)
# E0_fixtures_fo$e0_psfore <- paste(round(E0_fixtures_fo$e0_xHF,digits = 0),round(E0_fixtures_fo$e0_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #E1
# HomeTeam_e1_fo <- rep(e1_teams, each = length(e1_teams))
# AwayTeam_e1_fo <- rep(e1_teams, length(e1_teams))
# E1_fixtures_fo <- cbind(HomeTeam_e1_fo,AwayTeam_e1_fo)
# E1_fixtures_fo <- as.data.frame(E1_fixtures_fo)
# E1_fixtures_fo <- E1_fixtures_fo[!E1_fixtures_fo$HomeTeam_e1_fo == E1_fixtures_fo$AwayTeam_e1_fo,]
# rownames(E1_fixtures_fo) <- NULL
# E1_fixtures_fo$Div <- "E1"
# E1_fixtures_fo <- E1_fixtures_fo[,c(3,1,2)]
#
# E1_fixtures_fo$avg_HF_e1 <- e1_avg_HF
#
# E1_fixtures_fo$e1_homefas <- rep(e1_home_fas,each = length(e1_teams)-1)
#
# e1_awayfds_lookup <- cbind(e1_teams,e1_away_fds)
#
# e1_awayfds_lookup <- as.data.frame(e1_awayfds_lookup)
#
# colnames(e1_awayfds_lookup) <- c("AwayTeam_e1_fo","e1_awayfds")
#
#
# require('RH2')
# E1_fixtures_fo$e1_awayfds <- sqldf("SELECT e1_awayfds_lookup.e1_awayfds FROM e1_awayfds_lookup INNER JOIN E1_fixtures_fo ON e1_awayfds_lookup.AwayTeam_e1_fo = E1_fixtures_fo.AwayTeam_e1_fo")
#
# E1_fixtures_fo$avg_AF_e1 <- e1_avg_AF
#
# e1_awayfas_lookup <- cbind(e1_teams,e1_away_fas)
#
# e1_awayfas_lookup <- as.data.frame(e1_awayfas_lookup)
#
# colnames(e1_awayfas_lookup) <- c("AwayTeam_e1_fo","e1_awayfas")
#
# E1_fixtures_fo$e1_awayfas <- sqldf("SELECT e1_awayfas_lookup.e1_awayfas FROM e1_awayfas_lookup INNER JOIN E1_fixtures_fo ON e1_awayfas_lookup.AwayTeam_e1_fo = E1_fixtures_fo.AwayTeam_e1_fo")
#
# E1_fixtures_fo$e1_homefds <- rep(e1_home_fds,each = length(e1_teams)-1)
#
# E1_fixtures_fo$e1_awayfds <- as.numeric(unlist(E1_fixtures_fo$e1_awayfds))
# #xGH
# E1_fixtures_fo$e1_xHF <- E1_fixtures_fo$avg_HF_e1 * E1_fixtures_fo$e1_homefas * E1_fixtures_fo$e1_awayfds
# #xGA
#
# E1_fixtures_fo$e1_awayfas <- as.numeric(unlist(E1_fixtures_fo$e1_awayfas))
#
# E1_fixtures_fo$e1_xAF <- E1_fixtures_fo$avg_AF_e1 * E1_fixtures_fo$e1_awayfas * E1_fixtures_fo$e1_homefds
#
# E1_fixtures_fo$e1_0_0 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_0 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_1 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_1 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_0 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_2 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_2 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_1 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_2 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_3 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_0 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_1 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_2 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_3 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_3 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_3 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_4 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_0 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_1 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_2 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_3 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_4 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_4 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_4 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_4 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_5 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_0 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_1 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_2 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_3 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_4 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_5 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_5 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_5 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_5 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_5 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_6 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_0 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(0,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_1 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(1,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_2 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(2,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_3 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(3,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_4 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(4,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_6_5 <- round(stats::dpois(6,E1_fixtures_fo$e1_xHF) * stats::dpois(5,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_0_6 <- round(stats::dpois(0,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_1_6 <- round(stats::dpois(1,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_2_6 <- round(stats::dpois(2,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_3_6 <- round(stats::dpois(3,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_4_6 <- round(stats::dpois(4,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# E1_fixtures_fo$e1_5_6 <- round(stats::dpois(5,E1_fixtures_fo$e1_xHF) * stats::dpois(6,E1_fixtures_fo$e1_xAF), digits = 4)
# #Home win
# E1_fixtures_fo$e1_H <- (
#   E1_fixtures_fo$e1_1_0 + E1_fixtures_fo$e1_2_0 + E1_fixtures_fo$e1_2_1 + E1_fixtures_fo$e1_3_0 + E1_fixtures_fo$e1_3_1 +
#     E1_fixtures_fo$e1_3_2 + E1_fixtures_fo$e1_4_0 + E1_fixtures_fo$e1_4_1 + E1_fixtures_fo$e1_4_2 + E1_fixtures_fo$e1_4_3 +
#     E1_fixtures_fo$e1_5_0 + E1_fixtures_fo$e1_5_1 + E1_fixtures_fo$e1_5_2 + E1_fixtures_fo$e1_5_3 + E1_fixtures_fo$e1_5_4 +
#     E1_fixtures_fo$e1_6_0 + E1_fixtures_fo$e1_6_1 + E1_fixtures_fo$e1_6_2 + E1_fixtures_fo$e1_6_3 + E1_fixtures_fo$e1_6_4 +
#     E1_fixtures_fo$e1_6_5
# )
#
# E1_fixtures_fo$e1_H <- percent(E1_fixtures_fo$e1_H, accuracy = 0.1)
#
# #Draw
# E1_fixtures_fo$e1_D <- (
#
#   E1_fixtures_fo$e1_0_0 + E1_fixtures_fo$e1_1_1 + E1_fixtures_fo$e1_2_2 + E1_fixtures_fo$e1_3_3 + E1_fixtures_fo$e1_4_4 +
#     E1_fixtures_fo$e1_5_5 + E1_fixtures_fo$e1_6_6
# )
#
# E1_fixtures_fo$e1_D <- percent(E1_fixtures_fo$e1_D, accuracy = 0.1)
#
# #Away
#
# E1_fixtures_fo$e1_A <- (
#   E1_fixtures_fo$e1_0_1 + E1_fixtures_fo$e1_0_2 + E1_fixtures_fo$e1_1_2 + E1_fixtures_fo$e1_0_3 + E1_fixtures_fo$e1_1_3 +
#     E1_fixtures_fo$e1_2_3 + E1_fixtures_fo$e1_0_4 + E1_fixtures_fo$e1_1_4 + E1_fixtures_fo$e1_2_4 + E1_fixtures_fo$e1_3_4 +
#     E1_fixtures_fo$e1_0_5 + E1_fixtures_fo$e1_1_5 + E1_fixtures_fo$e1_2_5 + E1_fixtures_fo$e1_3_5 + E1_fixtures_fo$e1_4_5 +
#     E1_fixtures_fo$e1_0_6 + E1_fixtures_fo$e1_1_6 + E1_fixtures_fo$e1_2_6 + E1_fixtures_fo$e1_3_6 + E1_fixtures_fo$e1_4_6 +
#     E1_fixtures_fo$e1_5_6
# )
#
# E1_fixtures_fo$e1_A <- percent(E1_fixtures_fo$e1_A, accuracy = 0.1)
#
# #ov25
# E1_fixtures_fo$e1_ov25 <- (
#   E1_fixtures_fo$e1_2_1 + E1_fixtures_fo$e1_1_2 + E1_fixtures_fo$e1_2_2 + E1_fixtures_fo$e1_3_0 + E1_fixtures_fo$e1_3_1 +
#     E1_fixtures_fo$e1_3_2 + E1_fixtures_fo$e1_0_3 + E1_fixtures_fo$e1_1_3 + E1_fixtures_fo$e1_2_3 + E1_fixtures_fo$e1_3_3 +
#     E1_fixtures_fo$e1_4_0 + E1_fixtures_fo$e1_4_1 + E1_fixtures_fo$e1_4_2 + E1_fixtures_fo$e1_4_3 + E1_fixtures_fo$e1_0_4 +
#     E1_fixtures_fo$e1_1_4 + E1_fixtures_fo$e1_2_4 + E1_fixtures_fo$e1_3_4 + E1_fixtures_fo$e1_4_4 + E1_fixtures_fo$e1_5_0 +
#     E1_fixtures_fo$e1_5_1 + E1_fixtures_fo$e1_5_2 + E1_fixtures_fo$e1_5_3 + E1_fixtures_fo$e1_5_4 + E1_fixtures_fo$e1_0_5 +
#     E1_fixtures_fo$e1_1_5 + E1_fixtures_fo$e1_2_5 + E1_fixtures_fo$e1_3_5 + E1_fixtures_fo$e1_4_5 + E1_fixtures_fo$e1_5_5 +
#     E1_fixtures_fo$e1_6_0 + E1_fixtures_fo$e1_6_1 + E1_fixtures_fo$e1_6_2 + E1_fixtures_fo$e1_6_3 + E1_fixtures_fo$e1_6_4 +
#     E1_fixtures_fo$e1_6_5 + E1_fixtures_fo$e1_0_6 + E1_fixtures_fo$e1_1_6 + E1_fixtures_fo$e1_2_6 + E1_fixtures_fo$e1_3_6 +
#     E1_fixtures_fo$e1_4_6 + E1_fixtures_fo$e1_5_6 + E1_fixtures_fo$e1_6_6
# )
# #un25
# E1_fixtures_fo$e1_un25 <- (
#   E1_fixtures_fo$e1_0_0 + E1_fixtures_fo$e1_1_0 + E1_fixtures_fo$e1_0_1 + E1_fixtures_fo$e1_1_1 + E1_fixtures_fo$e1_2_0 + E1_fixtures_fo$e1_0_2
# )
# #odds
# E1_fixtures_fo$e1_ov25_odds <- round((1/E1_fixtures_fo$e1_ov25),digits = 2)
# E1_fixtures_fo$e1_un25_odds <- round((1/E1_fixtures_fo$e1_un25),digits = 2)
#
# E1_fixtures_fo$e1_ov25_odds
# E1_fixtures_fo$e1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E1_fixtures_fo$e1_ov25 <- percent(E1_fixtures_fo$e1_ov25, accuracy = 0.1)
#
# E1_fixtures_fo$e1_un25 <- percent(E1_fixtures_fo$e1_un25, accuracy = 0.1)
# E1_fixtures_fo$e1_psfore <- paste(round(E1_fixtures_fo$e1_xHF,digits = 0),round(E1_fixtures_fo$e1_xAF,digits = 0),sep = "-")
#
# #write out
# #write.xlsx(E1_fixtures,'Divisions/E1.xlsx',sheetName = "E1", append = TRUE)
# #################################################################################################################
# #E2
# HomeTeam_e2_fo <- rep(e2_teams, each = length(e2_teams))
# AwayTeam_e2_fo <- rep(e2_teams, length(e2_teams))
# E2_fixtures_fo <- cbind(HomeTeam_e2_fo,AwayTeam_e2_fo)
# E2_fixtures_fo <- as.data.frame(E2_fixtures_fo)
# E2_fixtures_fo <- E2_fixtures_fo[!E2_fixtures_fo$HomeTeam_e2_fo == E2_fixtures_fo$AwayTeam_e2_fo,]
# rownames(E2_fixtures_fo) <- NULL
# E2_fixtures_fo$Div <- "E2"
# E2_fixtures_fo <- E2_fixtures_fo[,c(3,1,2)]
#
# E2_fixtures_fo$avg_HF_e2 <- e2_avg_HF
#
# E2_fixtures_fo$e2_homefas <- rep(e2_home_fas,each = length(e2_teams)-1)
#
# e2_awayfds_lookup <- cbind(e2_teams,e2_away_fds)
#
# e2_awayfds_lookup <- as.data.frame(e2_awayfds_lookup)
#
# colnames(e2_awayfds_lookup) <- c("AwayTeam_e2_fo","e2_awayfds")
#
#
# require('RH2')
# E2_fixtures_fo$e2_awayfds <- sqldf("SELECT e2_awayfds_lookup.e2_awayfds FROM e2_awayfds_lookup INNER JOIN E2_fixtures_fo ON e2_awayfds_lookup.AwayTeam_e2_fo = E2_fixtures_fo.AwayTeam_e2_fo")
#
# E2_fixtures_fo$avg_AF_e2 <- e2_avg_AF
#
# e2_awayfas_lookup <- cbind(e2_teams,e2_away_fas)
#
# e2_awayfas_lookup <- as.data.frame(e2_awayfas_lookup)
#
# colnames(e2_awayfas_lookup) <- c("AwayTeam_e2_fo","e2_awayfas")
#
# E2_fixtures_fo$e2_awayfas <- sqldf("SELECT e2_awayfas_lookup.e2_awayfas FROM e2_awayfas_lookup INNER JOIN E2_fixtures_fo ON e2_awayfas_lookup.AwayTeam_e2_fo = E2_fixtures_fo.AwayTeam_e2_fo")
#
# E2_fixtures_fo$e2_homefds <- rep(e2_home_fds,each = length(e2_teams)-1)
#
# E2_fixtures_fo$e2_awayfds <- as.numeric(unlist(E2_fixtures_fo$e2_awayfds))
# #xGH
# E2_fixtures_fo$e2_xHF <- E2_fixtures_fo$avg_HF_e2 * E2_fixtures_fo$e2_homefas * E2_fixtures_fo$e2_awayfds
# #xGA
#
# E2_fixtures_fo$e2_awayfas <- as.numeric(unlist(E2_fixtures_fo$e2_awayfas))
#
# E2_fixtures_fo$e2_xAF <- E2_fixtures_fo$avg_AF_e2 * E2_fixtures_fo$e2_awayfas * E2_fixtures_fo$e2_homefds
#
# E2_fixtures_fo$e2_0_0 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_0 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_1 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_1 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_0 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_2 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_2 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_1 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_2 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_3 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_0 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_1 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_2 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_3 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_3 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_3 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_4 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_0 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_1 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_2 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_3 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_4 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_4 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_4 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_4 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_5 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_0 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_1 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_2 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_3 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_4 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_5 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_5 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_5 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_5 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_5 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_6 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_0 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(0,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_1 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(1,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_2 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(2,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_3 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(3,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_4 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(4,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_6_5 <- round(stats::dpois(6,E2_fixtures_fo$e2_xHF) * stats::dpois(5,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_0_6 <- round(stats::dpois(0,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_1_6 <- round(stats::dpois(1,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_2_6 <- round(stats::dpois(2,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_3_6 <- round(stats::dpois(3,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_4_6 <- round(stats::dpois(4,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# E2_fixtures_fo$e2_5_6 <- round(stats::dpois(5,E2_fixtures_fo$e2_xHF) * stats::dpois(6,E2_fixtures_fo$e2_xAF), digits = 4)
# #Home win
# E2_fixtures_fo$e2_H <- (
#   E2_fixtures_fo$e2_1_0 + E2_fixtures_fo$e2_2_0 + E2_fixtures_fo$e2_2_1 + E2_fixtures_fo$e2_3_0 + E2_fixtures_fo$e2_3_1 +
#     E2_fixtures_fo$e2_3_2 + E2_fixtures_fo$e2_4_0 + E2_fixtures_fo$e2_4_1 + E2_fixtures_fo$e2_4_2 + E2_fixtures_fo$e2_4_3 +
#     E2_fixtures_fo$e2_5_0 + E2_fixtures_fo$e2_5_1 + E2_fixtures_fo$e2_5_2 + E2_fixtures_fo$e2_5_3 + E2_fixtures_fo$e2_5_4 +
#     E2_fixtures_fo$e2_6_0 + E2_fixtures_fo$e2_6_1 + E2_fixtures_fo$e2_6_2 + E2_fixtures_fo$e2_6_3 + E2_fixtures_fo$e2_6_4 +
#     E2_fixtures_fo$e2_6_5
# )
#
# E2_fixtures_fo$e2_H <- percent(E2_fixtures_fo$e2_H, accuracy = 0.1)
#
# #Draw
# E2_fixtures_fo$e2_D <- (
#
#   E2_fixtures_fo$e2_0_0 + E2_fixtures_fo$e2_1_1 + E2_fixtures_fo$e2_2_2 + E2_fixtures_fo$e2_3_3 + E2_fixtures_fo$e2_4_4 +
#     E2_fixtures_fo$e2_5_5 + E2_fixtures_fo$e2_6_6
# )
#
# E2_fixtures_fo$e2_D <- percent(E2_fixtures_fo$e2_D, accuracy = 0.1)
#
# #Away
#
# E2_fixtures_fo$e2_A <- (
#   E2_fixtures_fo$e2_0_1 + E2_fixtures_fo$e2_0_2 + E2_fixtures_fo$e2_1_2 + E2_fixtures_fo$e2_0_3 + E2_fixtures_fo$e2_1_3 +
#     E2_fixtures_fo$e2_2_3 + E2_fixtures_fo$e2_0_4 + E2_fixtures_fo$e2_1_4 + E2_fixtures_fo$e2_2_4 + E2_fixtures_fo$e2_3_4 +
#     E2_fixtures_fo$e2_0_5 + E2_fixtures_fo$e2_1_5 + E2_fixtures_fo$e2_2_5 + E2_fixtures_fo$e2_3_5 + E2_fixtures_fo$e2_4_5 +
#     E2_fixtures_fo$e2_0_6 + E2_fixtures_fo$e2_1_6 + E2_fixtures_fo$e2_2_6 + E2_fixtures_fo$e2_3_6 + E2_fixtures_fo$e2_4_6 +
#     E2_fixtures_fo$e2_5_6
# )
#
# E2_fixtures_fo$e2_A <- percent(E2_fixtures_fo$e2_A, accuracy = 0.1)
#
# #ov25
# E2_fixtures_fo$e2_ov25 <- (
#   E2_fixtures_fo$e2_2_1 + E2_fixtures_fo$e2_1_2 + E2_fixtures_fo$e2_2_2 + E2_fixtures_fo$e2_3_0 + E2_fixtures_fo$e2_3_1 +
#     E2_fixtures_fo$e2_3_2 + E2_fixtures_fo$e2_0_3 + E2_fixtures_fo$e2_1_3 + E2_fixtures_fo$e2_2_3 + E2_fixtures_fo$e2_3_3 +
#     E2_fixtures_fo$e2_4_0 + E2_fixtures_fo$e2_4_1 + E2_fixtures_fo$e2_4_2 + E2_fixtures_fo$e2_4_3 + E2_fixtures_fo$e2_0_4 +
#     E2_fixtures_fo$e2_1_4 + E2_fixtures_fo$e2_2_4 + E2_fixtures_fo$e2_3_4 + E2_fixtures_fo$e2_4_4 + E2_fixtures_fo$e2_5_0 +
#     E2_fixtures_fo$e2_5_1 + E2_fixtures_fo$e2_5_2 + E2_fixtures_fo$e2_5_3 + E2_fixtures_fo$e2_5_4 + E2_fixtures_fo$e2_0_5 +
#     E2_fixtures_fo$e2_1_5 + E2_fixtures_fo$e2_2_5 + E2_fixtures_fo$e2_3_5 + E2_fixtures_fo$e2_4_5 + E2_fixtures_fo$e2_5_5 +
#     E2_fixtures_fo$e2_6_0 + E2_fixtures_fo$e2_6_1 + E2_fixtures_fo$e2_6_2 + E2_fixtures_fo$e2_6_3 + E2_fixtures_fo$e2_6_4 +
#     E2_fixtures_fo$e2_6_5 + E2_fixtures_fo$e2_0_6 + E2_fixtures_fo$e2_1_6 + E2_fixtures_fo$e2_2_6 + E2_fixtures_fo$e2_3_6 +
#     E2_fixtures_fo$e2_4_6 + E2_fixtures_fo$e2_5_6 + E2_fixtures_fo$e2_6_6
# )
# #un25
# E2_fixtures_fo$e2_un25 <- (
#   E2_fixtures_fo$e2_0_0 + E2_fixtures_fo$e2_1_0 + E2_fixtures_fo$e2_0_1 + E2_fixtures_fo$e2_1_1 + E2_fixtures_fo$e2_2_0 + E2_fixtures_fo$e2_0_2
# )
# #odds
# E2_fixtures_fo$e2_ov25_odds <- round((1/E2_fixtures_fo$e2_ov25),digits = 2)
# E2_fixtures_fo$e2_un25_odds <- round((1/E2_fixtures_fo$e2_un25),digits = 2)
#
# E2_fixtures_fo$e2_ov25_odds
# E2_fixtures_fo$e2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E2_fixtures_fo$e2_ov25 <- percent(E2_fixtures_fo$e2_ov25, accuracy = 0.1)
#
# E2_fixtures_fo$e2_un25 <- percent(E2_fixtures_fo$e2_un25, accuracy = 0.1)
# E2_fixtures_fo$e2_psfore <- paste(round(E2_fixtures_fo$e2_xHF,digits = 0),round(E2_fixtures_fo$e2_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(E2_fixtures,'Divisions/E2.xlsx',sheetName = "E2", append = TRUE)
# #################################################################################################################
# #E3
# HomeTeam_e3_fo <- rep(e3_teams, each = length(e3_teams))
# AwayTeam_e3_fo <- rep(e3_teams, length(e3_teams))
# E3_fixtures_fo <- cbind(HomeTeam_e3_fo,AwayTeam_e3_fo)
# E3_fixtures_fo <- as.data.frame(E3_fixtures_fo)
# E3_fixtures_fo <- E3_fixtures_fo[!E3_fixtures_fo$HomeTeam_e3_fo == E3_fixtures_fo$AwayTeam_e3_fo,]
# rownames(E3_fixtures_fo) <- NULL
# E3_fixtures_fo$Div <- "E3"
# E3_fixtures_fo <- E3_fixtures_fo[,c(3,1,2)]
#
# E3_fixtures_fo$avg_HF_e3 <- e3_avg_HF
#
# E3_fixtures_fo$e3_homefas <- rep(e3_home_fas,each = length(e3_teams)-1)
#
# e3_awayfds_lookup <- cbind(e3_teams,e3_away_fds)
#
# e3_awayfds_lookup <- as.data.frame(e3_awayfds_lookup)
#
# colnames(e3_awayfds_lookup) <- c("AwayTeam_e3_fo","e3_awayfds")
#
#
# require('RH2')
# E3_fixtures_fo$e3_awayfds <- sqldf("SELECT e3_awayfds_lookup.e3_awayfds FROM e3_awayfds_lookup INNER JOIN E3_fixtures_fo ON e3_awayfds_lookup.AwayTeam_e3_fo = E3_fixtures_fo.AwayTeam_e3_fo")
#
# E3_fixtures_fo$avg_AF_e3 <- e3_avg_AF
#
# e3_awayfas_lookup <- cbind(e3_teams,e3_away_fas)
#
# e3_awayfas_lookup <- as.data.frame(e3_awayfas_lookup)
#
# colnames(e3_awayfas_lookup) <- c("AwayTeam_e3_fo","e3_awayfas")
#
# E3_fixtures_fo$e3_awayfas <- sqldf("SELECT e3_awayfas_lookup.e3_awayfas FROM e3_awayfas_lookup INNER JOIN E3_fixtures_fo ON e3_awayfas_lookup.AwayTeam_e3_fo = E3_fixtures_fo.AwayTeam_e3_fo")
#
# E3_fixtures_fo$e3_homefds <- rep(e3_home_fds,each = length(e3_teams)-1)
#
# E3_fixtures_fo$e3_awayfds <- as.numeric(unlist(E3_fixtures_fo$e3_awayfds))
# #xGH
# E3_fixtures_fo$e3_xHF <- E3_fixtures_fo$avg_HF_e3 * E3_fixtures_fo$e3_homefas * E3_fixtures_fo$e3_awayfds
# #xGA
#
# E3_fixtures_fo$e3_awayfas <- as.numeric(unlist(E3_fixtures_fo$e3_awayfas))
#
# E3_fixtures_fo$e3_xAF <- E3_fixtures_fo$avg_AF_e3 * E3_fixtures_fo$e3_awayfas * E3_fixtures_fo$e3_homefds
#
# E3_fixtures_fo$e3_0_0 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_0 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_1 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_1 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_0 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_2 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_2 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_1 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_2 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_3 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_0 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_1 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_2 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_3 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_3 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_3 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_4 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_0 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_1 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_2 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_3 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_4 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_4 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_4 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_4 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_5 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_0 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_1 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_2 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_3 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_4 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_5 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_5 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_5 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_5 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_5 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_6 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_0 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(0,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_1 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(1,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_2 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(2,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_3 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(3,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_4 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(4,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_6_5 <- round(stats::dpois(6,E3_fixtures_fo$e3_xHF) * stats::dpois(5,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_0_6 <- round(stats::dpois(0,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_1_6 <- round(stats::dpois(1,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_2_6 <- round(stats::dpois(2,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_3_6 <- round(stats::dpois(3,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_4_6 <- round(stats::dpois(4,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# E3_fixtures_fo$e3_5_6 <- round(stats::dpois(5,E3_fixtures_fo$e3_xHF) * stats::dpois(6,E3_fixtures_fo$e3_xAF), digits = 4)
# #Home win
# E3_fixtures_fo$e3_H <- (
#   E3_fixtures_fo$e3_1_0 + E3_fixtures_fo$e3_2_0 + E3_fixtures_fo$e3_2_1 + E3_fixtures_fo$e3_3_0 + E3_fixtures_fo$e3_3_1 +
#     E3_fixtures_fo$e3_3_2 + E3_fixtures_fo$e3_4_0 + E3_fixtures_fo$e3_4_1 + E3_fixtures_fo$e3_4_2 + E3_fixtures_fo$e3_4_3 +
#     E3_fixtures_fo$e3_5_0 + E3_fixtures_fo$e3_5_1 + E3_fixtures_fo$e3_5_2 + E3_fixtures_fo$e3_5_3 + E3_fixtures_fo$e3_5_4 +
#     E3_fixtures_fo$e3_6_0 + E3_fixtures_fo$e3_6_1 + E3_fixtures_fo$e3_6_2 + E3_fixtures_fo$e3_6_3 + E3_fixtures_fo$e3_6_4 +
#     E3_fixtures_fo$e3_6_5
# )
#
# E3_fixtures_fo$e3_H <- percent(E3_fixtures_fo$e3_H, accuracy = 0.1)
#
# #Draw
# E3_fixtures_fo$e3_D <- (
#
#   E3_fixtures_fo$e3_0_0 + E3_fixtures_fo$e3_1_1 + E3_fixtures_fo$e3_2_2 + E3_fixtures_fo$e3_3_3 + E3_fixtures_fo$e3_4_4 +
#     E3_fixtures_fo$e3_5_5 + E3_fixtures_fo$e3_6_6
# )
#
# E3_fixtures_fo$e3_D <- percent(E3_fixtures_fo$e3_D, accuracy = 0.1)
#
# #Away
#
# E3_fixtures_fo$e3_A <- (
#   E3_fixtures_fo$e3_0_1 + E3_fixtures_fo$e3_0_2 + E3_fixtures_fo$e3_1_2 + E3_fixtures_fo$e3_0_3 + E3_fixtures_fo$e3_1_3 +
#     E3_fixtures_fo$e3_2_3 + E3_fixtures_fo$e3_0_4 + E3_fixtures_fo$e3_1_4 + E3_fixtures_fo$e3_2_4 + E3_fixtures_fo$e3_3_4 +
#     E3_fixtures_fo$e3_0_5 + E3_fixtures_fo$e3_1_5 + E3_fixtures_fo$e3_2_5 + E3_fixtures_fo$e3_3_5 + E3_fixtures_fo$e3_4_5 +
#     E3_fixtures_fo$e3_0_6 + E3_fixtures_fo$e3_1_6 + E3_fixtures_fo$e3_2_6 + E3_fixtures_fo$e3_3_6 + E3_fixtures_fo$e3_4_6 +
#     E3_fixtures_fo$e3_5_6
# )
#
# E3_fixtures_fo$e3_A <- percent(E3_fixtures_fo$e3_A, accuracy = 0.1)
#
# #ov25
# E3_fixtures_fo$e3_ov25 <- (
#   E3_fixtures_fo$e3_2_1 + E3_fixtures_fo$e3_1_2 + E3_fixtures_fo$e3_2_2 + E3_fixtures_fo$e3_3_0 + E3_fixtures_fo$e3_3_1 +
#     E3_fixtures_fo$e3_3_2 + E3_fixtures_fo$e3_0_3 + E3_fixtures_fo$e3_1_3 + E3_fixtures_fo$e3_2_3 + E3_fixtures_fo$e3_3_3 +
#     E3_fixtures_fo$e3_4_0 + E3_fixtures_fo$e3_4_1 + E3_fixtures_fo$e3_4_2 + E3_fixtures_fo$e3_4_3 + E3_fixtures_fo$e3_0_4 +
#     E3_fixtures_fo$e3_1_4 + E3_fixtures_fo$e3_2_4 + E3_fixtures_fo$e3_3_4 + E3_fixtures_fo$e3_4_4 + E3_fixtures_fo$e3_5_0 +
#     E3_fixtures_fo$e3_5_1 + E3_fixtures_fo$e3_5_2 + E3_fixtures_fo$e3_5_3 + E3_fixtures_fo$e3_5_4 + E3_fixtures_fo$e3_0_5 +
#     E3_fixtures_fo$e3_1_5 + E3_fixtures_fo$e3_2_5 + E3_fixtures_fo$e3_3_5 + E3_fixtures_fo$e3_4_5 + E3_fixtures_fo$e3_5_5 +
#     E3_fixtures_fo$e3_6_0 + E3_fixtures_fo$e3_6_1 + E3_fixtures_fo$e3_6_2 + E3_fixtures_fo$e3_6_3 + E3_fixtures_fo$e3_6_4 +
#     E3_fixtures_fo$e3_6_5 + E3_fixtures_fo$e3_0_6 + E3_fixtures_fo$e3_1_6 + E3_fixtures_fo$e3_2_6 + E3_fixtures_fo$e3_3_6 +
#     E3_fixtures_fo$e3_4_6 + E3_fixtures_fo$e3_5_6 + E3_fixtures_fo$e3_6_6
# )
# #un25
# E3_fixtures_fo$e3_un25 <- (
#   E3_fixtures_fo$e3_0_0 + E3_fixtures_fo$e3_1_0 + E3_fixtures_fo$e3_0_1 + E3_fixtures_fo$e3_1_1 + E3_fixtures_fo$e3_2_0 + E3_fixtures_fo$e3_0_2
# )
# #odds
# E3_fixtures_fo$e3_ov25_odds <- round((1/E3_fixtures_fo$e3_ov25),digits = 2)
# E3_fixtures_fo$e3_un25_odds <- round((1/E3_fixtures_fo$e3_un25),digits = 2)
#
# E3_fixtures_fo$e3_ov25_odds
# E3_fixtures_fo$e3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E3_fixtures_fo$e3_ov25 <- percent(E3_fixtures_fo$e3_ov25, accuracy = 0.1)
#
# E3_fixtures_fo$e3_un25 <- percent(E3_fixtures_fo$e3_un25, accuracy = 0.1)
# E3_fixtures_fo$e3_psfore <- paste(round(E3_fixtures_fo$e3_xHF,digits = 0),round(E3_fixtures_fo$e3_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(E3_fixtures,'Divisions/E3.xlsx',sheetName = "E3", append = TRUE)
# #################################################################################################################
# #EC
# HomeTeam_ec_fo <- rep(ec_teams, each = length(ec_teams))
# AwayTeam_ec_fo <- rep(ec_teams, length(ec_teams))
# EC_fixtures_fo <- cbind(HomeTeam_ec_fo,AwayTeam_ec_fo)
# EC_fixtures_fo <- as.data.frame(EC_fixtures_fo)
# EC_fixtures_fo <- EC_fixtures_fo[!EC_fixtures_fo$HomeTeam_ec_fo == EC_fixtures_fo$AwayTeam_ec_fo,]
# rownames(EC_fixtures_fo) <- NULL
# EC_fixtures_fo$Div <- "EC"
# EC_fixtures_fo <- EC_fixtures_fo[,c(3,1,2)]
#
# EC_fixtures_fo$avg_HF_ec <- ec_avg_HF
#
# EC_fixtures_fo$ec_homefas <- rep(ec_home_fas,each = length(ec_teams)-1)
#
# ec_awayfds_lookup <- cbind(ec_teams,ec_away_fds)
#
# ec_awayfds_lookup <- as.data.frame(ec_awayfds_lookup)
#
# colnames(ec_awayfds_lookup) <- c("AwayTeam_ec_fo","ec_awayfds")
#
#
# require('RH2')
# EC_fixtures_fo$ec_awayfds <- sqldf("SELECT ec_awayfds_lookup.ec_awayfds FROM ec_awayfds_lookup INNER JOIN EC_fixtures_fo ON ec_awayfds_lookup.AwayTeam_ec_fo = EC_fixtures_fo.AwayTeam_ec_fo")
#
# EC_fixtures_fo$avg_AF_ec <- ec_avg_AF
#
# ec_awayfas_lookup <- cbind(ec_teams,ec_away_fas)
#
# ec_awayfas_lookup <- as.data.frame(ec_awayfas_lookup)
#
# colnames(ec_awayfas_lookup) <- c("AwayTeam_ec_fo","ec_awayfas")
#
# EC_fixtures_fo$ec_awayfas <- sqldf("SELECT ec_awayfas_lookup.ec_awayfas FROM ec_awayfas_lookup INNER JOIN EC_fixtures_fo ON ec_awayfas_lookup.AwayTeam_ec_fo = EC_fixtures_fo.AwayTeam_ec_fo")
#
# EC_fixtures_fo$ec_homefds <- rep(ec_home_fds,each = length(ec_teams)-1)
#
# EC_fixtures_fo$ec_awayfds <- as.numeric(unlist(EC_fixtures_fo$ec_awayfds))
# #xGH
# EC_fixtures_fo$ec_xHF <- EC_fixtures_fo$avg_HF_ec * EC_fixtures_fo$ec_homefas * EC_fixtures_fo$ec_awayfds
# #xGA
#
# EC_fixtures_fo$ec_awayfas <- as.numeric(unlist(EC_fixtures_fo$ec_awayfas))
#
# EC_fixtures_fo$ec_xAF <- EC_fixtures_fo$avg_AF_ec * EC_fixtures_fo$ec_awayfas * EC_fixtures_fo$ec_homefds
#
# EC_fixtures_fo$ec_0_0 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_0 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_1 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_1 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_0 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_2 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_2 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_1 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_2 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_3 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_0 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_1 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_2 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_3 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_3 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_3 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_4 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_0 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_1 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_2 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_3 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_4 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_4 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_4 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_4 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_5 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_0 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_1 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_2 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_3 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_4 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_5 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_5 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_5 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_5 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_5 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_6 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_0 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(0,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_1 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(1,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_2 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(2,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_3 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(3,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_4 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(4,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_6_5 <- round(stats::dpois(6,EC_fixtures_fo$ec_xHF) * stats::dpois(5,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_0_6 <- round(stats::dpois(0,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_1_6 <- round(stats::dpois(1,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_2_6 <- round(stats::dpois(2,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_3_6 <- round(stats::dpois(3,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_4_6 <- round(stats::dpois(4,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# EC_fixtures_fo$ec_5_6 <- round(stats::dpois(5,EC_fixtures_fo$ec_xHF) * stats::dpois(6,EC_fixtures_fo$ec_xAF), digits = 4)
# #Home win
# EC_fixtures_fo$ec_H <- (
#   EC_fixtures_fo$ec_1_0 + EC_fixtures_fo$ec_2_0 + EC_fixtures_fo$ec_2_1 + EC_fixtures_fo$ec_3_0 + EC_fixtures_fo$ec_3_1 +
#     EC_fixtures_fo$ec_3_2 + EC_fixtures_fo$ec_4_0 + EC_fixtures_fo$ec_4_1 + EC_fixtures_fo$ec_4_2 + EC_fixtures_fo$ec_4_3 +
#     EC_fixtures_fo$ec_5_0 + EC_fixtures_fo$ec_5_1 + EC_fixtures_fo$ec_5_2 + EC_fixtures_fo$ec_5_3 + EC_fixtures_fo$ec_5_4 +
#     EC_fixtures_fo$ec_6_0 + EC_fixtures_fo$ec_6_1 + EC_fixtures_fo$ec_6_2 + EC_fixtures_fo$ec_6_3 + EC_fixtures_fo$ec_6_4 +
#     EC_fixtures_fo$ec_6_5
# )
#
# EC_fixtures_fo$ec_H <- percent(EC_fixtures_fo$ec_H, accuracy = 0.1)
#
# #Draw
# EC_fixtures_fo$ec_D <- (
#
#   EC_fixtures_fo$ec_0_0 + EC_fixtures_fo$ec_1_1 + EC_fixtures_fo$ec_2_2 + EC_fixtures_fo$ec_3_3 + EC_fixtures_fo$ec_4_4 +
#     EC_fixtures_fo$ec_5_5 + EC_fixtures_fo$ec_6_6
# )
#
# EC_fixtures_fo$ec_D <- percent(EC_fixtures_fo$ec_D, accuracy = 0.1)
#
# #Away
#
# EC_fixtures_fo$ec_A <- (
#   EC_fixtures_fo$ec_0_1 + EC_fixtures_fo$ec_0_2 + EC_fixtures_fo$ec_1_2 + EC_fixtures_fo$ec_0_3 + EC_fixtures_fo$ec_1_3 +
#     EC_fixtures_fo$ec_2_3 + EC_fixtures_fo$ec_0_4 + EC_fixtures_fo$ec_1_4 + EC_fixtures_fo$ec_2_4 + EC_fixtures_fo$ec_3_4 +
#     EC_fixtures_fo$ec_0_5 + EC_fixtures_fo$ec_1_5 + EC_fixtures_fo$ec_2_5 + EC_fixtures_fo$ec_3_5 + EC_fixtures_fo$ec_4_5 +
#     EC_fixtures_fo$ec_0_6 + EC_fixtures_fo$ec_1_6 + EC_fixtures_fo$ec_2_6 + EC_fixtures_fo$ec_3_6 + EC_fixtures_fo$ec_4_6 +
#     EC_fixtures_fo$ec_5_6
# )
#
# EC_fixtures_fo$ec_A <- percent(EC_fixtures_fo$ec_A, accuracy = 0.1)
#
# #ov25
# EC_fixtures_fo$ec_ov25 <- (
#   EC_fixtures_fo$ec_2_1 + EC_fixtures_fo$ec_1_2 + EC_fixtures_fo$ec_2_2 + EC_fixtures_fo$ec_3_0 + EC_fixtures_fo$ec_3_1 +
#     EC_fixtures_fo$ec_3_2 + EC_fixtures_fo$ec_0_3 + EC_fixtures_fo$ec_1_3 + EC_fixtures_fo$ec_2_3 + EC_fixtures_fo$ec_3_3 +
#     EC_fixtures_fo$ec_4_0 + EC_fixtures_fo$ec_4_1 + EC_fixtures_fo$ec_4_2 + EC_fixtures_fo$ec_4_3 + EC_fixtures_fo$ec_0_4 +
#     EC_fixtures_fo$ec_1_4 + EC_fixtures_fo$ec_2_4 + EC_fixtures_fo$ec_3_4 + EC_fixtures_fo$ec_4_4 + EC_fixtures_fo$ec_5_0 +
#     EC_fixtures_fo$ec_5_1 + EC_fixtures_fo$ec_5_2 + EC_fixtures_fo$ec_5_3 + EC_fixtures_fo$ec_5_4 + EC_fixtures_fo$ec_0_5 +
#     EC_fixtures_fo$ec_1_5 + EC_fixtures_fo$ec_2_5 + EC_fixtures_fo$ec_3_5 + EC_fixtures_fo$ec_4_5 + EC_fixtures_fo$ec_5_5 +
#     EC_fixtures_fo$ec_6_0 + EC_fixtures_fo$ec_6_1 + EC_fixtures_fo$ec_6_2 + EC_fixtures_fo$ec_6_3 + EC_fixtures_fo$ec_6_4 +
#     EC_fixtures_fo$ec_6_5 + EC_fixtures_fo$ec_0_6 + EC_fixtures_fo$ec_1_6 + EC_fixtures_fo$ec_2_6 + EC_fixtures_fo$ec_3_6 +
#     EC_fixtures_fo$ec_4_6 + EC_fixtures_fo$ec_5_6 + EC_fixtures_fo$ec_6_6
# )
# #un25
# EC_fixtures_fo$ec_un25 <- (
#   EC_fixtures_fo$ec_0_0 + EC_fixtures_fo$ec_1_0 + EC_fixtures_fo$ec_0_1 + EC_fixtures_fo$ec_1_1 + EC_fixtures_fo$ec_2_0 + EC_fixtures_fo$ec_0_2
# )
# #odds
# EC_fixtures_fo$ec_ov25_odds <- round((1/EC_fixtures_fo$ec_ov25),digits = 2)
# EC_fixtures_fo$ec_un25_odds <- round((1/EC_fixtures_fo$ec_un25),digits = 2)
#
# EC_fixtures_fo$ec_ov25_odds
# EC_fixtures_fo$ec_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# EC_fixtures_fo$ec_ov25 <- percent(EC_fixtures_fo$ec_ov25, accuracy = 0.1)
#
# EC_fixtures_fo$ec_un25 <- percent(EC_fixtures_fo$ec_un25, accuracy = 0.1)
# EC_fixtures_fo$ec_psfore <- paste(round(EC_fixtures_fo$ec_xHF,digits = 0),round(EC_fixtures_fo$ec_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(EC_fixtures,'Divisions/EC.xlsx',sheetName = "EC", append = TRUE)
# #################################################################################################################
# #F1
# HomeTeam_f1_fo <- rep(f1_teams, each = length(f1_teams))
# AwayTeam_f1_fo <- rep(f1_teams, length(f1_teams))
# F1_fixtures_fo <- cbind(HomeTeam_f1_fo,AwayTeam_f1_fo)
# F1_fixtures_fo <- as.data.frame(F1_fixtures_fo)
# F1_fixtures_fo <- F1_fixtures_fo[!F1_fixtures_fo$HomeTeam_f1_fo == F1_fixtures_fo$AwayTeam_f1_fo,]
# rownames(F1_fixtures_fo) <- NULL
# F1_fixtures_fo$Div <- "F1"
# F1_fixtures_fo <- F1_fixtures_fo[,c(3,1,2)]
#
# F1_fixtures_fo$avg_HF_f1 <- f1_avg_HF
#
# F1_fixtures_fo$f1_homefas <- rep(f1_home_fas,each = length(f1_teams)-1)
#
# f1_awayfds_lookup <- cbind(f1_teams,f1_away_fds)
#
# f1_awayfds_lookup <- as.data.frame(f1_awayfds_lookup)
#
# colnames(f1_awayfds_lookup) <- c("AwayTeam_f1_fo","f1_awayfds")
#
#
# require('RH2')
# F1_fixtures_fo$f1_awayfds <- sqldf("SELECT f1_awayfds_lookup.f1_awayfds FROM f1_awayfds_lookup INNER JOIN F1_fixtures_fo ON f1_awayfds_lookup.AwayTeam_f1_fo = F1_fixtures_fo.AwayTeam_f1_fo")
#
# F1_fixtures_fo$avg_AF_f1 <- f1_avg_AF
#
# f1_awayfas_lookup <- cbind(f1_teams,f1_away_fas)
#
# f1_awayfas_lookup <- as.data.frame(f1_awayfas_lookup)
#
# colnames(f1_awayfas_lookup) <- c("AwayTeam_f1_fo","f1_awayfas")
#
# F1_fixtures_fo$f1_awayfas <- sqldf("SELECT f1_awayfas_lookup.f1_awayfas FROM f1_awayfas_lookup INNER JOIN F1_fixtures_fo ON f1_awayfas_lookup.AwayTeam_f1_fo = F1_fixtures_fo.AwayTeam_f1_fo")
#
# F1_fixtures_fo$f1_homefds <- rep(f1_home_fds,each = length(f1_teams)-1)
#
# F1_fixtures_fo$f1_awayfds <- as.numeric(unlist(F1_fixtures_fo$f1_awayfds))
# #xGH
# F1_fixtures_fo$f1_xHF <- F1_fixtures_fo$avg_HF_f1 * F1_fixtures_fo$f1_homefas * F1_fixtures_fo$f1_awayfds
# #xGA
#
# F1_fixtures_fo$f1_awayfas <- as.numeric(unlist(F1_fixtures_fo$f1_awayfas))
#
# F1_fixtures_fo$f1_xAF <- F1_fixtures_fo$avg_AF_f1 * F1_fixtures_fo$f1_awayfas * F1_fixtures_fo$f1_homefds
#
# F1_fixtures_fo$f1_0_0 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_0 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_1 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_1 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_0 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_2 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_2 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_1 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_2 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_3 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_0 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_1 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_2 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_3 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_3 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_3 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_4 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_0 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_1 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_2 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_3 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_4 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_4 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_4 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_4 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_5 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_0 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_1 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_2 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_3 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_4 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_5 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_5 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_5 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_5 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_5 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_6 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_0 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(0,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_1 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(1,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_2 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(2,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_3 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(3,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_4 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(4,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_6_5 <- round(stats::dpois(6,F1_fixtures_fo$f1_xHF) * stats::dpois(5,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_0_6 <- round(stats::dpois(0,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_1_6 <- round(stats::dpois(1,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_2_6 <- round(stats::dpois(2,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_3_6 <- round(stats::dpois(3,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_4_6 <- round(stats::dpois(4,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# F1_fixtures_fo$f1_5_6 <- round(stats::dpois(5,F1_fixtures_fo$f1_xHF) * stats::dpois(6,F1_fixtures_fo$f1_xAF), digits = 4)
# #Home win
# F1_fixtures_fo$f1_H <- (
#   F1_fixtures_fo$f1_1_0 + F1_fixtures_fo$f1_2_0 + F1_fixtures_fo$f1_2_1 + F1_fixtures_fo$f1_3_0 + F1_fixtures_fo$f1_3_1 +
#     F1_fixtures_fo$f1_3_2 + F1_fixtures_fo$f1_4_0 + F1_fixtures_fo$f1_4_1 + F1_fixtures_fo$f1_4_2 + F1_fixtures_fo$f1_4_3 +
#     F1_fixtures_fo$f1_5_0 + F1_fixtures_fo$f1_5_1 + F1_fixtures_fo$f1_5_2 + F1_fixtures_fo$f1_5_3 + F1_fixtures_fo$f1_5_4 +
#     F1_fixtures_fo$f1_6_0 + F1_fixtures_fo$f1_6_1 + F1_fixtures_fo$f1_6_2 + F1_fixtures_fo$f1_6_3 + F1_fixtures_fo$f1_6_4 +
#     F1_fixtures_fo$f1_6_5
# )
#
# F1_fixtures_fo$f1_H <- percent(F1_fixtures_fo$f1_H, accuracy = 0.1)
#
# #Draw
# F1_fixtures_fo$f1_D <- (
#
#   F1_fixtures_fo$f1_0_0 + F1_fixtures_fo$f1_1_1 + F1_fixtures_fo$f1_2_2 + F1_fixtures_fo$f1_3_3 + F1_fixtures_fo$f1_4_4 +
#     F1_fixtures_fo$f1_5_5 + F1_fixtures_fo$f1_6_6
# )
#
# F1_fixtures_fo$f1_D <- percent(F1_fixtures_fo$f1_D, accuracy = 0.1)
#
# #Away
#
# F1_fixtures_fo$f1_A <- (
#   F1_fixtures_fo$f1_0_1 + F1_fixtures_fo$f1_0_2 + F1_fixtures_fo$f1_1_2 + F1_fixtures_fo$f1_0_3 + F1_fixtures_fo$f1_1_3 +
#     F1_fixtures_fo$f1_2_3 + F1_fixtures_fo$f1_0_4 + F1_fixtures_fo$f1_1_4 + F1_fixtures_fo$f1_2_4 + F1_fixtures_fo$f1_3_4 +
#     F1_fixtures_fo$f1_0_5 + F1_fixtures_fo$f1_1_5 + F1_fixtures_fo$f1_2_5 + F1_fixtures_fo$f1_3_5 + F1_fixtures_fo$f1_4_5 +
#     F1_fixtures_fo$f1_0_6 + F1_fixtures_fo$f1_1_6 + F1_fixtures_fo$f1_2_6 + F1_fixtures_fo$f1_3_6 + F1_fixtures_fo$f1_4_6 +
#     F1_fixtures_fo$f1_5_6
# )
#
# F1_fixtures_fo$f1_A <- percent(F1_fixtures_fo$f1_A, accuracy = 0.1)
#
# #ov25
# F1_fixtures_fo$f1_ov25 <- (
#   F1_fixtures_fo$f1_2_1 + F1_fixtures_fo$f1_1_2 + F1_fixtures_fo$f1_2_2 + F1_fixtures_fo$f1_3_0 + F1_fixtures_fo$f1_3_1 +
#     F1_fixtures_fo$f1_3_2 + F1_fixtures_fo$f1_0_3 + F1_fixtures_fo$f1_1_3 + F1_fixtures_fo$f1_2_3 + F1_fixtures_fo$f1_3_3 +
#     F1_fixtures_fo$f1_4_0 + F1_fixtures_fo$f1_4_1 + F1_fixtures_fo$f1_4_2 + F1_fixtures_fo$f1_4_3 + F1_fixtures_fo$f1_0_4 +
#     F1_fixtures_fo$f1_1_4 + F1_fixtures_fo$f1_2_4 + F1_fixtures_fo$f1_3_4 + F1_fixtures_fo$f1_4_4 + F1_fixtures_fo$f1_5_0 +
#     F1_fixtures_fo$f1_5_1 + F1_fixtures_fo$f1_5_2 + F1_fixtures_fo$f1_5_3 + F1_fixtures_fo$f1_5_4 + F1_fixtures_fo$f1_0_5 +
#     F1_fixtures_fo$f1_1_5 + F1_fixtures_fo$f1_2_5 + F1_fixtures_fo$f1_3_5 + F1_fixtures_fo$f1_4_5 + F1_fixtures_fo$f1_5_5 +
#     F1_fixtures_fo$f1_6_0 + F1_fixtures_fo$f1_6_1 + F1_fixtures_fo$f1_6_2 + F1_fixtures_fo$f1_6_3 + F1_fixtures_fo$f1_6_4 +
#     F1_fixtures_fo$f1_6_5 + F1_fixtures_fo$f1_0_6 + F1_fixtures_fo$f1_1_6 + F1_fixtures_fo$f1_2_6 + F1_fixtures_fo$f1_3_6 +
#     F1_fixtures_fo$f1_4_6 + F1_fixtures_fo$f1_5_6 + F1_fixtures_fo$f1_6_6
# )
# #un25
# F1_fixtures_fo$f1_un25 <- (
#   F1_fixtures_fo$f1_0_0 + F1_fixtures_fo$f1_1_0 + F1_fixtures_fo$f1_0_1 + F1_fixtures_fo$f1_1_1 + F1_fixtures_fo$f1_2_0 + F1_fixtures_fo$f1_0_2
# )
# #odds
# F1_fixtures_fo$f1_ov25_odds <- round((1/F1_fixtures_fo$f1_ov25),digits = 2)
# F1_fixtures_fo$f1_un25_odds <- round((1/F1_fixtures_fo$f1_un25),digits = 2)
#
# F1_fixtures_fo$f1_ov25_odds
# F1_fixtures_fo$f1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F1_fixtures_fo$f1_ov25 <- percent(F1_fixtures_fo$f1_ov25, accuracy = 0.1)
#
# F1_fixtures_fo$f1_un25 <- percent(F1_fixtures_fo$f1_un25, accuracy = 0.1)
# F1_fixtures_fo$f1_psfore <- paste(round(F1_fixtures_fo$f1_xHF,digits = 0),round(F1_fixtures_fo$f1_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #F2
# HomeTeam_f2_fo <- rep(f2_teams, each = length(f2_teams))
# AwayTeam_f2_fo <- rep(f2_teams, length(f2_teams))
# F2_fixtures_fo <- cbind(HomeTeam_f2_fo,AwayTeam_f2_fo)
# F2_fixtures_fo <- as.data.frame(F2_fixtures_fo)
# F2_fixtures_fo <- F2_fixtures_fo[!F2_fixtures_fo$HomeTeam_f2_fo == F2_fixtures_fo$AwayTeam_f2_fo,]
# rownames(F2_fixtures_fo) <- NULL
# F2_fixtures_fo$Div <- "F2"
# F2_fixtures_fo <- F2_fixtures_fo[,c(3,1,2)]
#
# F2_fixtures_fo$avg_HF_f2 <- f2_avg_HF
#
# F2_fixtures_fo$f2_homefas <- rep(f2_home_fas,each = length(f2_teams)-1)
#
# f2_awayfds_lookup <- cbind(f2_teams,f2_away_fds)
#
# f2_awayfds_lookup <- as.data.frame(f2_awayfds_lookup)
#
# colnames(f2_awayfds_lookup) <- c("AwayTeam_f2_fo","f2_awayfds")
#
#
# require('RH2')
# F2_fixtures_fo$f2_awayfds <- sqldf("SELECT f2_awayfds_lookup.f2_awayfds FROM f2_awayfds_lookup INNER JOIN F2_fixtures_fo ON f2_awayfds_lookup.AwayTeam_f2_fo = F2_fixtures_fo.AwayTeam_f2_fo")
#
# F2_fixtures_fo$avg_AF_f2 <- f2_avg_AF
#
# f2_awayfas_lookup <- cbind(f2_teams,f2_away_fas)
#
# f2_awayfas_lookup <- as.data.frame(f2_awayfas_lookup)
#
# colnames(f2_awayfas_lookup) <- c("AwayTeam_f2_fo","f2_awayfas")
#
# F2_fixtures_fo$f2_awayfas <- sqldf("SELECT f2_awayfas_lookup.f2_awayfas FROM f2_awayfas_lookup INNER JOIN F2_fixtures_fo ON f2_awayfas_lookup.AwayTeam_f2_fo = F2_fixtures_fo.AwayTeam_f2_fo")
#
# F2_fixtures_fo$f2_homefds <- rep(f2_home_fds,each = length(f2_teams)-1)
#
# F2_fixtures_fo$f2_awayfds <- as.numeric(unlist(F2_fixtures_fo$f2_awayfds))
# #xGH
# F2_fixtures_fo$f2_xHF <- F2_fixtures_fo$avg_HF_f2 * F2_fixtures_fo$f2_homefas * F2_fixtures_fo$f2_awayfds
# #xGA
#
# F2_fixtures_fo$f2_awayfas <- as.numeric(unlist(F2_fixtures_fo$f2_awayfas))
#
# F2_fixtures_fo$f2_xAF <- F2_fixtures_fo$avg_AF_f2 * F2_fixtures_fo$f2_awayfas * F2_fixtures_fo$f2_homefds
#
# F2_fixtures_fo$f2_0_0 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_0 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_1 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_1 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_0 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_2 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_2 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_1 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_2 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_3 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_0 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_1 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_2 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_3 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_3 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_3 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_4 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_0 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_1 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_2 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_3 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_4 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_4 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_4 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_4 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_5 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_0 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_1 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_2 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_3 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_4 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_5 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_5 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_5 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_5 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_5 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_6 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_0 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(0,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_1 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(1,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_2 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(2,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_3 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(3,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_4 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(4,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_6_5 <- round(stats::dpois(6,F2_fixtures_fo$f2_xHF) * stats::dpois(5,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_0_6 <- round(stats::dpois(0,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_1_6 <- round(stats::dpois(1,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_2_6 <- round(stats::dpois(2,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_3_6 <- round(stats::dpois(3,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_4_6 <- round(stats::dpois(4,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# F2_fixtures_fo$f2_5_6 <- round(stats::dpois(5,F2_fixtures_fo$f2_xHF) * stats::dpois(6,F2_fixtures_fo$f2_xAF), digits = 4)
# #Home win
# F2_fixtures_fo$f2_H <- (
#   F2_fixtures_fo$f2_1_0 + F2_fixtures_fo$f2_2_0 + F2_fixtures_fo$f2_2_1 + F2_fixtures_fo$f2_3_0 + F2_fixtures_fo$f2_3_1 +
#     F2_fixtures_fo$f2_3_2 + F2_fixtures_fo$f2_4_0 + F2_fixtures_fo$f2_4_1 + F2_fixtures_fo$f2_4_2 + F2_fixtures_fo$f2_4_3 +
#     F2_fixtures_fo$f2_5_0 + F2_fixtures_fo$f2_5_1 + F2_fixtures_fo$f2_5_2 + F2_fixtures_fo$f2_5_3 + F2_fixtures_fo$f2_5_4 +
#     F2_fixtures_fo$f2_6_0 + F2_fixtures_fo$f2_6_1 + F2_fixtures_fo$f2_6_2 + F2_fixtures_fo$f2_6_3 + F2_fixtures_fo$f2_6_4 +
#     F2_fixtures_fo$f2_6_5
# )
#
# F2_fixtures_fo$f2_H <- percent(F2_fixtures_fo$f2_H, accuracy = 0.1)
#
# #Draw
# F2_fixtures_fo$f2_D <- (
#
#   F2_fixtures_fo$f2_0_0 + F2_fixtures_fo$f2_1_1 + F2_fixtures_fo$f2_2_2 + F2_fixtures_fo$f2_3_3 + F2_fixtures_fo$f2_4_4 +
#     F2_fixtures_fo$f2_5_5 + F2_fixtures_fo$f2_6_6
# )
#
# F2_fixtures_fo$f2_D <- percent(F2_fixtures_fo$f2_D, accuracy = 0.1)
#
# #Away
#
# F2_fixtures_fo$f2_A <- (
#   F2_fixtures_fo$f2_0_1 + F2_fixtures_fo$f2_0_2 + F2_fixtures_fo$f2_1_2 + F2_fixtures_fo$f2_0_3 + F2_fixtures_fo$f2_1_3 +
#     F2_fixtures_fo$f2_2_3 + F2_fixtures_fo$f2_0_4 + F2_fixtures_fo$f2_1_4 + F2_fixtures_fo$f2_2_4 + F2_fixtures_fo$f2_3_4 +
#     F2_fixtures_fo$f2_0_5 + F2_fixtures_fo$f2_1_5 + F2_fixtures_fo$f2_2_5 + F2_fixtures_fo$f2_3_5 + F2_fixtures_fo$f2_4_5 +
#     F2_fixtures_fo$f2_0_6 + F2_fixtures_fo$f2_1_6 + F2_fixtures_fo$f2_2_6 + F2_fixtures_fo$f2_3_6 + F2_fixtures_fo$f2_4_6 +
#     F2_fixtures_fo$f2_5_6
# )
#
# F2_fixtures_fo$f2_A <- percent(F2_fixtures_fo$f2_A, accuracy = 0.1)
#
# #ov25
# F2_fixtures_fo$f2_ov25 <- (
#   F2_fixtures_fo$f2_2_1 + F2_fixtures_fo$f2_1_2 + F2_fixtures_fo$f2_2_2 + F2_fixtures_fo$f2_3_0 + F2_fixtures_fo$f2_3_1 +
#     F2_fixtures_fo$f2_3_2 + F2_fixtures_fo$f2_0_3 + F2_fixtures_fo$f2_1_3 + F2_fixtures_fo$f2_2_3 + F2_fixtures_fo$f2_3_3 +
#     F2_fixtures_fo$f2_4_0 + F2_fixtures_fo$f2_4_1 + F2_fixtures_fo$f2_4_2 + F2_fixtures_fo$f2_4_3 + F2_fixtures_fo$f2_0_4 +
#     F2_fixtures_fo$f2_1_4 + F2_fixtures_fo$f2_2_4 + F2_fixtures_fo$f2_3_4 + F2_fixtures_fo$f2_4_4 + F2_fixtures_fo$f2_5_0 +
#     F2_fixtures_fo$f2_5_1 + F2_fixtures_fo$f2_5_2 + F2_fixtures_fo$f2_5_3 + F2_fixtures_fo$f2_5_4 + F2_fixtures_fo$f2_0_5 +
#     F2_fixtures_fo$f2_1_5 + F2_fixtures_fo$f2_2_5 + F2_fixtures_fo$f2_3_5 + F2_fixtures_fo$f2_4_5 + F2_fixtures_fo$f2_5_5 +
#     F2_fixtures_fo$f2_6_0 + F2_fixtures_fo$f2_6_1 + F2_fixtures_fo$f2_6_2 + F2_fixtures_fo$f2_6_3 + F2_fixtures_fo$f2_6_4 +
#     F2_fixtures_fo$f2_6_5 + F2_fixtures_fo$f2_0_6 + F2_fixtures_fo$f2_1_6 + F2_fixtures_fo$f2_2_6 + F2_fixtures_fo$f2_3_6 +
#     F2_fixtures_fo$f2_4_6 + F2_fixtures_fo$f2_5_6 + F2_fixtures_fo$f2_6_6
# )
# #un25
# F2_fixtures_fo$f2_un25 <- (
#   F2_fixtures_fo$f2_0_0 + F2_fixtures_fo$f2_1_0 + F2_fixtures_fo$f2_0_1 + F2_fixtures_fo$f2_1_1 + F2_fixtures_fo$f2_2_0 + F2_fixtures_fo$f2_0_2
# )
# #odds
# F2_fixtures_fo$f2_ov25_odds <- round((1/F2_fixtures_fo$f2_ov25),digits = 2)
# F2_fixtures_fo$f2_un25_odds <- round((1/F2_fixtures_fo$f2_un25),digits = 2)
#
# F2_fixtures_fo$f2_ov25_odds
# F2_fixtures_fo$f2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F2_fixtures_fo$f2_ov25 <- percent(F2_fixtures_fo$f2_ov25, accuracy = 0.1)
#
# F2_fixtures_fo$f2_un25 <- percent(F2_fixtures_fo$f2_un25, accuracy = 0.1)
# F2_fixtures_fo$f2_psfore <- paste(round(F2_fixtures_fo$f2_xHF,digits = 0),round(F2_fixtures_fo$f2_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #G1
# HomeTeam_g1_fo <- rep(g1_teams, each = length(g1_teams))
# AwayTeam_g1_fo <- rep(g1_teams, length(g1_teams))
# G1_fixtures_fo <- cbind(HomeTeam_g1_fo,AwayTeam_g1_fo)
# G1_fixtures_fo <- as.data.frame(G1_fixtures_fo)
# G1_fixtures_fo <- G1_fixtures_fo[!G1_fixtures_fo$HomeTeam_g1_fo == G1_fixtures_fo$AwayTeam_g1_fo,]
# rownames(G1_fixtures_fo) <- NULL
# G1_fixtures_fo$Div <- "G1"
# G1_fixtures_fo <- G1_fixtures_fo[,c(3,1,2)]
#
# G1_fixtures_fo$avg_HF_g1 <- g1_avg_HF
#
# G1_fixtures_fo$g1_homefas <- rep(g1_home_fas,each = length(g1_teams)-1)
#
# g1_awayfds_lookup <- cbind(g1_teams,g1_away_fds)
#
# g1_awayfds_lookup <- as.data.frame(g1_awayfds_lookup)
#
# colnames(g1_awayfds_lookup) <- c("AwayTeam_g1_fo","g1_awayfds")
#
#
# require('RH2')
# G1_fixtures_fo$g1_awayfds <- sqldf("SELECT g1_awayfds_lookup.g1_awayfds FROM g1_awayfds_lookup INNER JOIN G1_fixtures_fo ON g1_awayfds_lookup.AwayTeam_g1_fo = G1_fixtures_fo.AwayTeam_g1_fo")
#
# G1_fixtures_fo$avg_AF_g1 <- g1_avg_AF
#
# g1_awayfas_lookup <- cbind(g1_teams,g1_away_fas)
#
# g1_awayfas_lookup <- as.data.frame(g1_awayfas_lookup)
#
# colnames(g1_awayfas_lookup) <- c("AwayTeam_g1_fo","g1_awayfas")
#
# G1_fixtures_fo$g1_awayfas <- sqldf("SELECT g1_awayfas_lookup.g1_awayfas FROM g1_awayfas_lookup INNER JOIN G1_fixtures_fo ON g1_awayfas_lookup.AwayTeam_g1_fo = G1_fixtures_fo.AwayTeam_g1_fo")
#
# G1_fixtures_fo$g1_homefds <- rep(g1_home_fds,each = length(g1_teams)-1)
#
# G1_fixtures_fo$g1_awayfds <- as.numeric(unlist(G1_fixtures_fo$g1_awayfds))
# #xGH
# G1_fixtures_fo$g1_xHF <- G1_fixtures_fo$avg_HF_g1 * G1_fixtures_fo$g1_homefas * G1_fixtures_fo$g1_awayfds
# #xGA
#
# G1_fixtures_fo$g1_awayfas <- as.numeric(unlist(G1_fixtures_fo$g1_awayfas))
#
# G1_fixtures_fo$g1_xAF <- G1_fixtures_fo$avg_AF_g1 * G1_fixtures_fo$g1_awayfas * G1_fixtures_fo$g1_homefds
#
# G1_fixtures_fo$g1_0_0 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_0 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_1 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_1 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_0 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_2 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_2 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_1 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_2 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_3 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_0 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_1 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_2 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_3 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_3 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_3 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_4 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_0 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_1 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_2 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_3 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_4 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_4 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_4 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_4 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_5 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_0 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_1 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_2 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_3 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_4 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_5 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_5 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_5 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_5 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_5 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_6 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_0 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(0,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_1 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(1,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_2 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(2,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_3 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(3,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_4 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(4,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_6_5 <- round(stats::dpois(6,G1_fixtures_fo$g1_xHF) * stats::dpois(5,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_0_6 <- round(stats::dpois(0,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_1_6 <- round(stats::dpois(1,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_2_6 <- round(stats::dpois(2,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_3_6 <- round(stats::dpois(3,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_4_6 <- round(stats::dpois(4,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# G1_fixtures_fo$g1_5_6 <- round(stats::dpois(5,G1_fixtures_fo$g1_xHF) * stats::dpois(6,G1_fixtures_fo$g1_xAF), digits = 4)
# #Home win
# G1_fixtures_fo$g1_H <- (
#   G1_fixtures_fo$g1_1_0 + G1_fixtures_fo$g1_2_0 + G1_fixtures_fo$g1_2_1 + G1_fixtures_fo$g1_3_0 + G1_fixtures_fo$g1_3_1 +
#     G1_fixtures_fo$g1_3_2 + G1_fixtures_fo$g1_4_0 + G1_fixtures_fo$g1_4_1 + G1_fixtures_fo$g1_4_2 + G1_fixtures_fo$g1_4_3 +
#     G1_fixtures_fo$g1_5_0 + G1_fixtures_fo$g1_5_1 + G1_fixtures_fo$g1_5_2 + G1_fixtures_fo$g1_5_3 + G1_fixtures_fo$g1_5_4 +
#     G1_fixtures_fo$g1_6_0 + G1_fixtures_fo$g1_6_1 + G1_fixtures_fo$g1_6_2 + G1_fixtures_fo$g1_6_3 + G1_fixtures_fo$g1_6_4 +
#     G1_fixtures_fo$g1_6_5
# )
#
# G1_fixtures_fo$g1_H <- percent(G1_fixtures_fo$g1_H, accuracy = 0.1)
#
# #Draw
# G1_fixtures_fo$g1_D <- (
#
#   G1_fixtures_fo$g1_0_0 + G1_fixtures_fo$g1_1_1 + G1_fixtures_fo$g1_2_2 + G1_fixtures_fo$g1_3_3 + G1_fixtures_fo$g1_4_4 +
#     G1_fixtures_fo$g1_5_5 + G1_fixtures_fo$g1_6_6
# )
#
# G1_fixtures_fo$g1_D <- percent(G1_fixtures_fo$g1_D, accuracy = 0.1)
#
# #Away
#
# G1_fixtures_fo$g1_A <- (
#   G1_fixtures_fo$g1_0_1 + G1_fixtures_fo$g1_0_2 + G1_fixtures_fo$g1_1_2 + G1_fixtures_fo$g1_0_3 + G1_fixtures_fo$g1_1_3 +
#     G1_fixtures_fo$g1_2_3 + G1_fixtures_fo$g1_0_4 + G1_fixtures_fo$g1_1_4 + G1_fixtures_fo$g1_2_4 + G1_fixtures_fo$g1_3_4 +
#     G1_fixtures_fo$g1_0_5 + G1_fixtures_fo$g1_1_5 + G1_fixtures_fo$g1_2_5 + G1_fixtures_fo$g1_3_5 + G1_fixtures_fo$g1_4_5 +
#     G1_fixtures_fo$g1_0_6 + G1_fixtures_fo$g1_1_6 + G1_fixtures_fo$g1_2_6 + G1_fixtures_fo$g1_3_6 + G1_fixtures_fo$g1_4_6 +
#     G1_fixtures_fo$g1_5_6
# )
#
# G1_fixtures_fo$g1_A <- percent(G1_fixtures_fo$g1_A, accuracy = 0.1)
#
# #ov25
# G1_fixtures_fo$g1_ov25 <- (
#   G1_fixtures_fo$g1_2_1 + G1_fixtures_fo$g1_1_2 + G1_fixtures_fo$g1_2_2 + G1_fixtures_fo$g1_3_0 + G1_fixtures_fo$g1_3_1 +
#     G1_fixtures_fo$g1_3_2 + G1_fixtures_fo$g1_0_3 + G1_fixtures_fo$g1_1_3 + G1_fixtures_fo$g1_2_3 + G1_fixtures_fo$g1_3_3 +
#     G1_fixtures_fo$g1_4_0 + G1_fixtures_fo$g1_4_1 + G1_fixtures_fo$g1_4_2 + G1_fixtures_fo$g1_4_3 + G1_fixtures_fo$g1_0_4 +
#     G1_fixtures_fo$g1_1_4 + G1_fixtures_fo$g1_2_4 + G1_fixtures_fo$g1_3_4 + G1_fixtures_fo$g1_4_4 + G1_fixtures_fo$g1_5_0 +
#     G1_fixtures_fo$g1_5_1 + G1_fixtures_fo$g1_5_2 + G1_fixtures_fo$g1_5_3 + G1_fixtures_fo$g1_5_4 + G1_fixtures_fo$g1_0_5 +
#     G1_fixtures_fo$g1_1_5 + G1_fixtures_fo$g1_2_5 + G1_fixtures_fo$g1_3_5 + G1_fixtures_fo$g1_4_5 + G1_fixtures_fo$g1_5_5 +
#     G1_fixtures_fo$g1_6_0 + G1_fixtures_fo$g1_6_1 + G1_fixtures_fo$g1_6_2 + G1_fixtures_fo$g1_6_3 + G1_fixtures_fo$g1_6_4 +
#     G1_fixtures_fo$g1_6_5 + G1_fixtures_fo$g1_0_6 + G1_fixtures_fo$g1_1_6 + G1_fixtures_fo$g1_2_6 + G1_fixtures_fo$g1_3_6 +
#     G1_fixtures_fo$g1_4_6 + G1_fixtures_fo$g1_5_6 + G1_fixtures_fo$g1_6_6
# )
# #un25
# G1_fixtures_fo$g1_un25 <- (
#   G1_fixtures_fo$g1_0_0 + G1_fixtures_fo$g1_1_0 + G1_fixtures_fo$g1_0_1 + G1_fixtures_fo$g1_1_1 + G1_fixtures_fo$g1_2_0 + G1_fixtures_fo$g1_0_2
# )
# #odds
# G1_fixtures_fo$g1_ov25_odds <- round((1/G1_fixtures_fo$g1_ov25),digits = 2)
# G1_fixtures_fo$g1_un25_odds <- round((1/G1_fixtures_fo$g1_un25),digits = 2)
#
# G1_fixtures_fo$g1_ov25_odds
# G1_fixtures_fo$g1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# G1_fixtures_fo$g1_ov25 <- percent(G1_fixtures_fo$g1_ov25, accuracy = 0.1)
#
# G1_fixtures_fo$g1_un25 <- percent(G1_fixtures_fo$g1_un25, accuracy = 0.1)
# G1_fixtures_fo$g1_psfore <- paste(round(G1_fixtures_fo$g1_xHF,digits = 0),round(G1_fixtures_fo$g1_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #I1
# HomeTeam_i1_fo <- rep(i1_teams, each = length(i1_teams))
# AwayTeam_i1_fo <- rep(i1_teams, length(i1_teams))
# I1_fixtures_fo <- cbind(HomeTeam_i1_fo,AwayTeam_i1_fo)
# I1_fixtures_fo <- as.data.frame(I1_fixtures_fo)
# I1_fixtures_fo <- I1_fixtures_fo[!I1_fixtures_fo$HomeTeam_i1_fo == I1_fixtures_fo$AwayTeam_i1_fo,]
# rownames(I1_fixtures_fo) <- NULL
# I1_fixtures_fo$Div <- "I1"
# I1_fixtures_fo <- I1_fixtures_fo[,c(3,1,2)]
#
# I1_fixtures_fo$avg_HF_i1 <- i1_avg_HF
#
# I1_fixtures_fo$i1_homefas <- rep(i1_home_fas,each = length(i1_teams)-1)
#
# i1_awayfds_lookup <- cbind(i1_teams,i1_away_fds)
#
# i1_awayfds_lookup <- as.data.frame(i1_awayfds_lookup)
#
# colnames(i1_awayfds_lookup) <- c("AwayTeam_i1_fo","i1_awayfds")
#
#
# require('RH2')
# I1_fixtures_fo$i1_awayfds <- sqldf("SELECT i1_awayfds_lookup.i1_awayfds FROM i1_awayfds_lookup INNER JOIN I1_fixtures_fo ON i1_awayfds_lookup.AwayTeam_i1_fo = I1_fixtures_fo.AwayTeam_i1_fo")
#
# I1_fixtures_fo$avg_AF_i1 <- i1_avg_AF
#
# i1_awayfas_lookup <- cbind(i1_teams,i1_away_fas)
#
# i1_awayfas_lookup <- as.data.frame(i1_awayfas_lookup)
#
# colnames(i1_awayfas_lookup) <- c("AwayTeam_i1_fo","i1_awayfas")
#
# I1_fixtures_fo$i1_awayfas <- sqldf("SELECT i1_awayfas_lookup.i1_awayfas FROM i1_awayfas_lookup INNER JOIN I1_fixtures_fo ON i1_awayfas_lookup.AwayTeam_i1_fo = I1_fixtures_fo.AwayTeam_i1_fo")
#
# I1_fixtures_fo$i1_homefds <- rep(i1_home_fds,each = length(i1_teams)-1)
#
# I1_fixtures_fo$i1_awayfds <- as.numeric(unlist(I1_fixtures_fo$i1_awayfds))
# #xGH
# I1_fixtures_fo$i1_xHF <- I1_fixtures_fo$avg_HF_i1 * I1_fixtures_fo$i1_homefas * I1_fixtures_fo$i1_awayfds
# #xGA
#
# I1_fixtures_fo$i1_awayfas <- as.numeric(unlist(I1_fixtures_fo$i1_awayfas))
#
# I1_fixtures_fo$i1_xAF <- I1_fixtures_fo$avg_AF_i1 * I1_fixtures_fo$i1_awayfas * I1_fixtures_fo$i1_homefds
#
# I1_fixtures_fo$i1_0_0 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_0 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_1 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_1 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_0 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_2 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_2 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_1 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_2 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_3 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_0 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_1 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_2 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_3 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_3 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_3 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_4 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_0 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_1 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_2 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_3 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_4 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_4 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_4 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_4 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_5 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_0 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_1 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_2 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_3 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_4 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_5 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_5 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_5 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_5 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_5 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_6 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_0 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(0,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_1 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(1,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_2 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(2,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_3 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(3,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_4 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(4,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_6_5 <- round(stats::dpois(6,I1_fixtures_fo$i1_xHF) * stats::dpois(5,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_0_6 <- round(stats::dpois(0,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_1_6 <- round(stats::dpois(1,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_2_6 <- round(stats::dpois(2,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_3_6 <- round(stats::dpois(3,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_4_6 <- round(stats::dpois(4,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# I1_fixtures_fo$i1_5_6 <- round(stats::dpois(5,I1_fixtures_fo$i1_xHF) * stats::dpois(6,I1_fixtures_fo$i1_xAF), digits = 4)
# #Home win
# I1_fixtures_fo$i1_H <- (
#   I1_fixtures_fo$i1_1_0 + I1_fixtures_fo$i1_2_0 + I1_fixtures_fo$i1_2_1 + I1_fixtures_fo$i1_3_0 + I1_fixtures_fo$i1_3_1 +
#     I1_fixtures_fo$i1_3_2 + I1_fixtures_fo$i1_4_0 + I1_fixtures_fo$i1_4_1 + I1_fixtures_fo$i1_4_2 + I1_fixtures_fo$i1_4_3 +
#     I1_fixtures_fo$i1_5_0 + I1_fixtures_fo$i1_5_1 + I1_fixtures_fo$i1_5_2 + I1_fixtures_fo$i1_5_3 + I1_fixtures_fo$i1_5_4 +
#     I1_fixtures_fo$i1_6_0 + I1_fixtures_fo$i1_6_1 + I1_fixtures_fo$i1_6_2 + I1_fixtures_fo$i1_6_3 + I1_fixtures_fo$i1_6_4 +
#     I1_fixtures_fo$i1_6_5
# )
#
# I1_fixtures_fo$i1_H <- percent(I1_fixtures_fo$i1_H, accuracy = 0.1)
#
# #Draw
# I1_fixtures_fo$i1_D <- (
#
#   I1_fixtures_fo$i1_0_0 + I1_fixtures_fo$i1_1_1 + I1_fixtures_fo$i1_2_2 + I1_fixtures_fo$i1_3_3 + I1_fixtures_fo$i1_4_4 +
#     I1_fixtures_fo$i1_5_5 + I1_fixtures_fo$i1_6_6
# )
#
# I1_fixtures_fo$i1_D <- percent(I1_fixtures_fo$i1_D, accuracy = 0.1)
#
# #Away
#
# I1_fixtures_fo$i1_A <- (
#   I1_fixtures_fo$i1_0_1 + I1_fixtures_fo$i1_0_2 + I1_fixtures_fo$i1_1_2 + I1_fixtures_fo$i1_0_3 + I1_fixtures_fo$i1_1_3 +
#     I1_fixtures_fo$i1_2_3 + I1_fixtures_fo$i1_0_4 + I1_fixtures_fo$i1_1_4 + I1_fixtures_fo$i1_2_4 + I1_fixtures_fo$i1_3_4 +
#     I1_fixtures_fo$i1_0_5 + I1_fixtures_fo$i1_1_5 + I1_fixtures_fo$i1_2_5 + I1_fixtures_fo$i1_3_5 + I1_fixtures_fo$i1_4_5 +
#     I1_fixtures_fo$i1_0_6 + I1_fixtures_fo$i1_1_6 + I1_fixtures_fo$i1_2_6 + I1_fixtures_fo$i1_3_6 + I1_fixtures_fo$i1_4_6 +
#     I1_fixtures_fo$i1_5_6
# )
#
# I1_fixtures_fo$i1_A <- percent(I1_fixtures_fo$i1_A, accuracy = 0.1)
#
# #ov25
# I1_fixtures_fo$i1_ov25 <- (
#   I1_fixtures_fo$i1_2_1 + I1_fixtures_fo$i1_1_2 + I1_fixtures_fo$i1_2_2 + I1_fixtures_fo$i1_3_0 + I1_fixtures_fo$i1_3_1 +
#     I1_fixtures_fo$i1_3_2 + I1_fixtures_fo$i1_0_3 + I1_fixtures_fo$i1_1_3 + I1_fixtures_fo$i1_2_3 + I1_fixtures_fo$i1_3_3 +
#     I1_fixtures_fo$i1_4_0 + I1_fixtures_fo$i1_4_1 + I1_fixtures_fo$i1_4_2 + I1_fixtures_fo$i1_4_3 + I1_fixtures_fo$i1_0_4 +
#     I1_fixtures_fo$i1_1_4 + I1_fixtures_fo$i1_2_4 + I1_fixtures_fo$i1_3_4 + I1_fixtures_fo$i1_4_4 + I1_fixtures_fo$i1_5_0 +
#     I1_fixtures_fo$i1_5_1 + I1_fixtures_fo$i1_5_2 + I1_fixtures_fo$i1_5_3 + I1_fixtures_fo$i1_5_4 + I1_fixtures_fo$i1_0_5 +
#     I1_fixtures_fo$i1_1_5 + I1_fixtures_fo$i1_2_5 + I1_fixtures_fo$i1_3_5 + I1_fixtures_fo$i1_4_5 + I1_fixtures_fo$i1_5_5 +
#     I1_fixtures_fo$i1_6_0 + I1_fixtures_fo$i1_6_1 + I1_fixtures_fo$i1_6_2 + I1_fixtures_fo$i1_6_3 + I1_fixtures_fo$i1_6_4 +
#     I1_fixtures_fo$i1_6_5 + I1_fixtures_fo$i1_0_6 + I1_fixtures_fo$i1_1_6 + I1_fixtures_fo$i1_2_6 + I1_fixtures_fo$i1_3_6 +
#     I1_fixtures_fo$i1_4_6 + I1_fixtures_fo$i1_5_6 + I1_fixtures_fo$i1_6_6
# )
# #un25
# I1_fixtures_fo$i1_un25 <- (
#   I1_fixtures_fo$i1_0_0 + I1_fixtures_fo$i1_1_0 + I1_fixtures_fo$i1_0_1 + I1_fixtures_fo$i1_1_1 + I1_fixtures_fo$i1_2_0 + I1_fixtures_fo$i1_0_2
# )
# #odds
# I1_fixtures_fo$i1_ov25_odds <- round((1/I1_fixtures_fo$i1_ov25),digits = 2)
# I1_fixtures_fo$i1_un25_odds <- round((1/I1_fixtures_fo$i1_un25),digits = 2)
#
# I1_fixtures_fo$i1_ov25_odds
# I1_fixtures_fo$i1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I1_fixtures_fo$i1_ov25 <- percent(I1_fixtures_fo$i1_ov25, accuracy = 0.1)
#
# I1_fixtures_fo$i1_un25 <- percent(I1_fixtures_fo$i1_un25, accuracy = 0.1)
# I1_fixtures_fo$i1_psfore <- paste(round(I1_fixtures_fo$i1_xHF,digits = 0),round(I1_fixtures_fo$i1_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(I1_fixtures,'Divisions/I1.xlsx',sheetName = "I1", append = TRUE)
# ##################
# #I2
# HomeTeam_i2_fo <- rep(i2_teams, each = length(i2_teams))
# AwayTeam_i2_fo <- rep(i2_teams, length(i2_teams))
# I2_fixtures_fo <- cbind(HomeTeam_i2_fo,AwayTeam_i2_fo)
# I2_fixtures_fo <- as.data.frame(I2_fixtures_fo)
# I2_fixtures_fo <- I2_fixtures_fo[!I2_fixtures_fo$HomeTeam_i2_fo == I2_fixtures_fo$AwayTeam_i2_fo,]
# rownames(I2_fixtures_fo) <- NULL
# I2_fixtures_fo$Div <- "I2"
# I2_fixtures_fo <- I2_fixtures_fo[,c(3,1,2)]
#
# I2_fixtures_fo$avg_HF_i2 <- i2_avg_HF
#
# I2_fixtures_fo$i2_homefas <- rep(i2_home_fas,each = length(i2_teams)-1)
#
# i2_awayfds_lookup <- cbind(i2_teams,i2_away_fds)
#
# i2_awayfds_lookup <- as.data.frame(i2_awayfds_lookup)
#
# colnames(i2_awayfds_lookup) <- c("AwayTeam_i2_fo","i2_awayfds")
#
#
# require('RH2')
# I2_fixtures_fo$i2_awayfds <- sqldf("SELECT i2_awayfds_lookup.i2_awayfds FROM i2_awayfds_lookup INNER JOIN I2_fixtures_fo ON i2_awayfds_lookup.AwayTeam_i2_fo = I2_fixtures_fo.AwayTeam_i2_fo")
#
# I2_fixtures_fo$avg_AF_i2 <- i2_avg_AF
#
# i2_awayfas_lookup <- cbind(i2_teams,i2_away_fas)
#
# i2_awayfas_lookup <- as.data.frame(i2_awayfas_lookup)
#
# colnames(i2_awayfas_lookup) <- c("AwayTeam_i2_fo","i2_awayfas")
#
# I2_fixtures_fo$i2_awayfas <- sqldf("SELECT i2_awayfas_lookup.i2_awayfas FROM i2_awayfas_lookup INNER JOIN I2_fixtures_fo ON i2_awayfas_lookup.AwayTeam_i2_fo = I2_fixtures_fo.AwayTeam_i2_fo")
#
# I2_fixtures_fo$i2_homefds <- rep(i2_home_fds,each = length(i2_teams)-1)
#
# I2_fixtures_fo$i2_awayfds <- as.numeric(unlist(I2_fixtures_fo$i2_awayfds))
# #xGH
# I2_fixtures_fo$i2_xHF <- I2_fixtures_fo$avg_HF_i2 * I2_fixtures_fo$i2_homefas * I2_fixtures_fo$i2_awayfds
# #xGA
#
# I2_fixtures_fo$i2_awayfas <- as.numeric(unlist(I2_fixtures_fo$i2_awayfas))
#
# I2_fixtures_fo$i2_xAF <- I2_fixtures_fo$avg_AF_i2 * I2_fixtures_fo$i2_awayfas * I2_fixtures_fo$i2_homefds
#
# I2_fixtures_fo$i2_0_0 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_0 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_1 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_1 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_0 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_2 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_2 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_1 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_2 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_3 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_0 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_1 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_2 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_3 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_3 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_3 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_4 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_0 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_1 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_2 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_3 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_4 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_4 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_4 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_4 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_5 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_0 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_1 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_2 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_3 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_4 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_5 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_5 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_5 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_5 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_5 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_6 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_0 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(0,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_1 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(1,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_2 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(2,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_3 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(3,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_4 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(4,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_6_5 <- round(stats::dpois(6,I2_fixtures_fo$i2_xHF) * stats::dpois(5,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_0_6 <- round(stats::dpois(0,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_1_6 <- round(stats::dpois(1,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_2_6 <- round(stats::dpois(2,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_3_6 <- round(stats::dpois(3,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_4_6 <- round(stats::dpois(4,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# I2_fixtures_fo$i2_5_6 <- round(stats::dpois(5,I2_fixtures_fo$i2_xHF) * stats::dpois(6,I2_fixtures_fo$i2_xAF), digits = 4)
# #Home win
# I2_fixtures_fo$i2_H <- (
#   I2_fixtures_fo$i2_1_0 + I2_fixtures_fo$i2_2_0 + I2_fixtures_fo$i2_2_1 + I2_fixtures_fo$i2_3_0 + I2_fixtures_fo$i2_3_1 +
#     I2_fixtures_fo$i2_3_2 + I2_fixtures_fo$i2_4_0 + I2_fixtures_fo$i2_4_1 + I2_fixtures_fo$i2_4_2 + I2_fixtures_fo$i2_4_3 +
#     I2_fixtures_fo$i2_5_0 + I2_fixtures_fo$i2_5_1 + I2_fixtures_fo$i2_5_2 + I2_fixtures_fo$i2_5_3 + I2_fixtures_fo$i2_5_4 +
#     I2_fixtures_fo$i2_6_0 + I2_fixtures_fo$i2_6_1 + I2_fixtures_fo$i2_6_2 + I2_fixtures_fo$i2_6_3 + I2_fixtures_fo$i2_6_4 +
#     I2_fixtures_fo$i2_6_5
# )
#
# I2_fixtures_fo$i2_H <- percent(I2_fixtures_fo$i2_H, accuracy = 0.1)
#
# #Draw
# I2_fixtures_fo$i2_D <- (
#
#   I2_fixtures_fo$i2_0_0 + I2_fixtures_fo$i2_1_1 + I2_fixtures_fo$i2_2_2 + I2_fixtures_fo$i2_3_3 + I2_fixtures_fo$i2_4_4 +
#     I2_fixtures_fo$i2_5_5 + I2_fixtures_fo$i2_6_6
# )
#
# I2_fixtures_fo$i2_D <- percent(I2_fixtures_fo$i2_D, accuracy = 0.1)
#
# #Away
#
# I2_fixtures_fo$i2_A <- (
#   I2_fixtures_fo$i2_0_1 + I2_fixtures_fo$i2_0_2 + I2_fixtures_fo$i2_1_2 + I2_fixtures_fo$i2_0_3 + I2_fixtures_fo$i2_1_3 +
#     I2_fixtures_fo$i2_2_3 + I2_fixtures_fo$i2_0_4 + I2_fixtures_fo$i2_1_4 + I2_fixtures_fo$i2_2_4 + I2_fixtures_fo$i2_3_4 +
#     I2_fixtures_fo$i2_0_5 + I2_fixtures_fo$i2_1_5 + I2_fixtures_fo$i2_2_5 + I2_fixtures_fo$i2_3_5 + I2_fixtures_fo$i2_4_5 +
#     I2_fixtures_fo$i2_0_6 + I2_fixtures_fo$i2_1_6 + I2_fixtures_fo$i2_2_6 + I2_fixtures_fo$i2_3_6 + I2_fixtures_fo$i2_4_6 +
#     I2_fixtures_fo$i2_5_6
# )
#
# I2_fixtures_fo$i2_A <- percent(I2_fixtures_fo$i2_A, accuracy = 0.1)
#
# #ov25
# I2_fixtures_fo$i2_ov25 <- (
#   I2_fixtures_fo$i2_2_1 + I2_fixtures_fo$i2_1_2 + I2_fixtures_fo$i2_2_2 + I2_fixtures_fo$i2_3_0 + I2_fixtures_fo$i2_3_1 +
#     I2_fixtures_fo$i2_3_2 + I2_fixtures_fo$i2_0_3 + I2_fixtures_fo$i2_1_3 + I2_fixtures_fo$i2_2_3 + I2_fixtures_fo$i2_3_3 +
#     I2_fixtures_fo$i2_4_0 + I2_fixtures_fo$i2_4_1 + I2_fixtures_fo$i2_4_2 + I2_fixtures_fo$i2_4_3 + I2_fixtures_fo$i2_0_4 +
#     I2_fixtures_fo$i2_1_4 + I2_fixtures_fo$i2_2_4 + I2_fixtures_fo$i2_3_4 + I2_fixtures_fo$i2_4_4 + I2_fixtures_fo$i2_5_0 +
#     I2_fixtures_fo$i2_5_1 + I2_fixtures_fo$i2_5_2 + I2_fixtures_fo$i2_5_3 + I2_fixtures_fo$i2_5_4 + I2_fixtures_fo$i2_0_5 +
#     I2_fixtures_fo$i2_1_5 + I2_fixtures_fo$i2_2_5 + I2_fixtures_fo$i2_3_5 + I2_fixtures_fo$i2_4_5 + I2_fixtures_fo$i2_5_5 +
#     I2_fixtures_fo$i2_6_0 + I2_fixtures_fo$i2_6_1 + I2_fixtures_fo$i2_6_2 + I2_fixtures_fo$i2_6_3 + I2_fixtures_fo$i2_6_4 +
#     I2_fixtures_fo$i2_6_5 + I2_fixtures_fo$i2_0_6 + I2_fixtures_fo$i2_1_6 + I2_fixtures_fo$i2_2_6 + I2_fixtures_fo$i2_3_6 +
#     I2_fixtures_fo$i2_4_6 + I2_fixtures_fo$i2_5_6 + I2_fixtures_fo$i2_6_6
# )
# #un25
# I2_fixtures_fo$i2_un25 <- (
#   I2_fixtures_fo$i2_0_0 + I2_fixtures_fo$i2_1_0 + I2_fixtures_fo$i2_0_1 + I2_fixtures_fo$i2_1_1 + I2_fixtures_fo$i2_2_0 + I2_fixtures_fo$i2_0_2
# )
# #odds
# I2_fixtures_fo$i2_ov25_odds <- round((1/I2_fixtures_fo$i2_ov25),digits = 2)
# I2_fixtures_fo$i2_un25_odds <- round((1/I2_fixtures_fo$i2_un25),digits = 2)
#
# I2_fixtures_fo$i2_ov25_odds
# I2_fixtures_fo$i2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I2_fixtures_fo$i2_ov25 <- percent(I2_fixtures_fo$i2_ov25, accuracy = 0.1)
#
# I2_fixtures_fo$i2_un25 <- percent(I2_fixtures_fo$i2_un25, accuracy = 0.1)
# I2_fixtures_fo$i2_psfore <- paste(round(I2_fixtures_fo$i2_xHF,digits = 0),round(I2_fixtures_fo$i2_xAF,digits = 0),sep = "-")
#
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################################################################################################################
# #N1
# HomeTeam_n1_fo <- rep(n1_teams, each = length(n1_teams))
# AwayTeam_n1_fo <- rep(n1_teams, length(n1_teams))
# N1_fixtures_fo <- cbind(HomeTeam_n1_fo,AwayTeam_n1_fo)
# N1_fixtures_fo <- as.data.frame(N1_fixtures_fo)
# N1_fixtures_fo <- N1_fixtures_fo[!N1_fixtures_fo$HomeTeam_n1_fo == N1_fixtures_fo$AwayTeam_n1_fo,]
# rownames(N1_fixtures_fo) <- NULL
# N1_fixtures_fo$Div <- "N1"
# N1_fixtures_fo <- N1_fixtures_fo[,c(3,1,2)]
#
# N1_fixtures_fo$avg_HF_n1 <- n1_avg_HF
#
# N1_fixtures_fo$n1_homefas <- rep(n1_home_fas,each = length(n1_teams)-1)
#
# n1_awayfds_lookup <- cbind(n1_teams,n1_away_fds)
#
# n1_awayfds_lookup <- as.data.frame(n1_awayfds_lookup)
#
# colnames(n1_awayfds_lookup) <- c("AwayTeam_n1_fo","n1_awayfds")
#
#
# require('RH2')
# N1_fixtures_fo$n1_awayfds <- sqldf("SELECT n1_awayfds_lookup.n1_awayfds FROM n1_awayfds_lookup INNER JOIN N1_fixtures_fo ON n1_awayfds_lookup.AwayTeam_n1_fo = N1_fixtures_fo.AwayTeam_n1_fo")
#
# N1_fixtures_fo$avg_AF_n1 <- n1_avg_AF
#
# n1_awayfas_lookup <- cbind(n1_teams,n1_away_fas)
#
# n1_awayfas_lookup <- as.data.frame(n1_awayfas_lookup)
#
# colnames(n1_awayfas_lookup) <- c("AwayTeam_n1_fo","n1_awayfas")
#
# N1_fixtures_fo$n1_awayfas <- sqldf("SELECT n1_awayfas_lookup.n1_awayfas FROM n1_awayfas_lookup INNER JOIN N1_fixtures_fo ON n1_awayfas_lookup.AwayTeam_n1_fo = N1_fixtures_fo.AwayTeam_n1_fo")
#
# N1_fixtures_fo$n1_homefds <- rep(n1_home_fds,each = length(n1_teams)-1)
#
# N1_fixtures_fo$n1_awayfds <- as.numeric(unlist(N1_fixtures_fo$n1_awayfds))
# #xGH
# N1_fixtures_fo$n1_xHF <- N1_fixtures_fo$avg_HF_n1 * N1_fixtures_fo$n1_homefas * N1_fixtures_fo$n1_awayfds
# #xGA
#
# N1_fixtures_fo$n1_awayfas <- as.numeric(unlist(N1_fixtures_fo$n1_awayfas))
#
# N1_fixtures_fo$n1_xAF <- N1_fixtures_fo$avg_AF_n1 * N1_fixtures_fo$n1_awayfas * N1_fixtures_fo$n1_homefds
#
# N1_fixtures_fo$n1_0_0 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_0 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_1 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_1 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_0 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_2 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_2 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_1 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_2 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_3 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_0 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_1 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_2 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_3 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_3 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_3 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_4 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_0 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_1 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_2 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_3 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_4 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_4 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_4 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_4 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_5 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_0 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_1 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_2 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_3 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_4 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_5 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_5 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_5 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_5 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_5 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_6 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_0 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(0,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_1 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(1,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_2 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(2,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_3 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(3,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_4 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(4,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_6_5 <- round(stats::dpois(6,N1_fixtures_fo$n1_xHF) * stats::dpois(5,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_0_6 <- round(stats::dpois(0,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_1_6 <- round(stats::dpois(1,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_2_6 <- round(stats::dpois(2,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_3_6 <- round(stats::dpois(3,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_4_6 <- round(stats::dpois(4,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# N1_fixtures_fo$n1_5_6 <- round(stats::dpois(5,N1_fixtures_fo$n1_xHF) * stats::dpois(6,N1_fixtures_fo$n1_xAF), digits = 4)
# #Home win
# N1_fixtures_fo$n1_H <- (
#   N1_fixtures_fo$n1_1_0 + N1_fixtures_fo$n1_2_0 + N1_fixtures_fo$n1_2_1 + N1_fixtures_fo$n1_3_0 + N1_fixtures_fo$n1_3_1 +
#     N1_fixtures_fo$n1_3_2 + N1_fixtures_fo$n1_4_0 + N1_fixtures_fo$n1_4_1 + N1_fixtures_fo$n1_4_2 + N1_fixtures_fo$n1_4_3 +
#     N1_fixtures_fo$n1_5_0 + N1_fixtures_fo$n1_5_1 + N1_fixtures_fo$n1_5_2 + N1_fixtures_fo$n1_5_3 + N1_fixtures_fo$n1_5_4 +
#     N1_fixtures_fo$n1_6_0 + N1_fixtures_fo$n1_6_1 + N1_fixtures_fo$n1_6_2 + N1_fixtures_fo$n1_6_3 + N1_fixtures_fo$n1_6_4 +
#     N1_fixtures_fo$n1_6_5
# )
#
# N1_fixtures_fo$n1_H <- percent(N1_fixtures_fo$n1_H, accuracy = 0.1)
#
# #Draw
# N1_fixtures_fo$n1_D <- (
#
#   N1_fixtures_fo$n1_0_0 + N1_fixtures_fo$n1_1_1 + N1_fixtures_fo$n1_2_2 + N1_fixtures_fo$n1_3_3 + N1_fixtures_fo$n1_4_4 +
#     N1_fixtures_fo$n1_5_5 + N1_fixtures_fo$n1_6_6
# )
#
# N1_fixtures_fo$n1_D <- percent(N1_fixtures_fo$n1_D, accuracy = 0.1)
#
# #Away
#
# N1_fixtures_fo$n1_A <- (
#   N1_fixtures_fo$n1_0_1 + N1_fixtures_fo$n1_0_2 + N1_fixtures_fo$n1_1_2 + N1_fixtures_fo$n1_0_3 + N1_fixtures_fo$n1_1_3 +
#     N1_fixtures_fo$n1_2_3 + N1_fixtures_fo$n1_0_4 + N1_fixtures_fo$n1_1_4 + N1_fixtures_fo$n1_2_4 + N1_fixtures_fo$n1_3_4 +
#     N1_fixtures_fo$n1_0_5 + N1_fixtures_fo$n1_1_5 + N1_fixtures_fo$n1_2_5 + N1_fixtures_fo$n1_3_5 + N1_fixtures_fo$n1_4_5 +
#     N1_fixtures_fo$n1_0_6 + N1_fixtures_fo$n1_1_6 + N1_fixtures_fo$n1_2_6 + N1_fixtures_fo$n1_3_6 + N1_fixtures_fo$n1_4_6 +
#     N1_fixtures_fo$n1_5_6
# )
#
# N1_fixtures_fo$n1_A <- percent(N1_fixtures_fo$n1_A, accuracy = 0.1)
#
# #ov25
# N1_fixtures_fo$n1_ov25 <- (
#   N1_fixtures_fo$n1_2_1 + N1_fixtures_fo$n1_1_2 + N1_fixtures_fo$n1_2_2 + N1_fixtures_fo$n1_3_0 + N1_fixtures_fo$n1_3_1 +
#     N1_fixtures_fo$n1_3_2 + N1_fixtures_fo$n1_0_3 + N1_fixtures_fo$n1_1_3 + N1_fixtures_fo$n1_2_3 + N1_fixtures_fo$n1_3_3 +
#     N1_fixtures_fo$n1_4_0 + N1_fixtures_fo$n1_4_1 + N1_fixtures_fo$n1_4_2 + N1_fixtures_fo$n1_4_3 + N1_fixtures_fo$n1_0_4 +
#     N1_fixtures_fo$n1_1_4 + N1_fixtures_fo$n1_2_4 + N1_fixtures_fo$n1_3_4 + N1_fixtures_fo$n1_4_4 + N1_fixtures_fo$n1_5_0 +
#     N1_fixtures_fo$n1_5_1 + N1_fixtures_fo$n1_5_2 + N1_fixtures_fo$n1_5_3 + N1_fixtures_fo$n1_5_4 + N1_fixtures_fo$n1_0_5 +
#     N1_fixtures_fo$n1_1_5 + N1_fixtures_fo$n1_2_5 + N1_fixtures_fo$n1_3_5 + N1_fixtures_fo$n1_4_5 + N1_fixtures_fo$n1_5_5 +
#     N1_fixtures_fo$n1_6_0 + N1_fixtures_fo$n1_6_1 + N1_fixtures_fo$n1_6_2 + N1_fixtures_fo$n1_6_3 + N1_fixtures_fo$n1_6_4 +
#     N1_fixtures_fo$n1_6_5 + N1_fixtures_fo$n1_0_6 + N1_fixtures_fo$n1_1_6 + N1_fixtures_fo$n1_2_6 + N1_fixtures_fo$n1_3_6 +
#     N1_fixtures_fo$n1_4_6 + N1_fixtures_fo$n1_5_6 + N1_fixtures_fo$n1_6_6
# )
# #un25
# N1_fixtures_fo$n1_un25 <- (
#   N1_fixtures_fo$n1_0_0 + N1_fixtures_fo$n1_1_0 + N1_fixtures_fo$n1_0_1 + N1_fixtures_fo$n1_1_1 + N1_fixtures_fo$n1_2_0 + N1_fixtures_fo$n1_0_2
# )
# #odds
# N1_fixtures_fo$n1_ov25_odds <- round((1/N1_fixtures_fo$n1_ov25),digits = 2)
# N1_fixtures_fo$n1_un25_odds <- round((1/N1_fixtures_fo$n1_un25),digits = 2)
#
# N1_fixtures_fo$n1_ov25_odds
# N1_fixtures_fo$n1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# N1_fixtures_fo$n1_ov25 <- percent(N1_fixtures_fo$n1_ov25, accuracy = 0.1)
#
# N1_fixtures_fo$n1_un25 <- percent(N1_fixtures_fo$n1_un25, accuracy = 0.1)
# N1_fixtures_fo$n1_psfore <- paste(round(N1_fixtures_fo$n1_xHF,digits = 0),round(N1_fixtures_fo$n1_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(N1_fixtures,'Divisions/N1.xlsx',sheetName = "N1", append = TRUE)
# #################################################################################################################
# #P1
# HomeTeam_p1_fo <- rep(p1_teams, each = length(p1_teams))
# AwayTeam_p1_fo <- rep(p1_teams, length(p1_teams))
# P1_fixtures_fo <- cbind(HomeTeam_p1_fo,AwayTeam_p1_fo)
# P1_fixtures_fo <- as.data.frame(P1_fixtures_fo)
# P1_fixtures_fo <- P1_fixtures_fo[!P1_fixtures_fo$HomeTeam_p1_fo == P1_fixtures_fo$AwayTeam_p1_fo,]
# rownames(P1_fixtures_fo) <- NULL
# P1_fixtures_fo$Div <- "P1"
# P1_fixtures_fo <- P1_fixtures_fo[,c(3,1,2)]
#
# P1_fixtures_fo$avg_HF_p1 <- p1_avg_HF
#
# P1_fixtures_fo$p1_homefas <- rep(p1_home_fas,each = length(p1_teams)-1)
#
# p1_awayfds_lookup <- cbind(p1_teams,p1_away_fds)
#
# p1_awayfds_lookup <- as.data.frame(p1_awayfds_lookup)
#
# colnames(p1_awayfds_lookup) <- c("AwayTeam_p1_fo","p1_awayfds")
#
#
# require('RH2')
# P1_fixtures_fo$p1_awayfds <- sqldf("SELECT p1_awayfds_lookup.p1_awayfds FROM p1_awayfds_lookup INNER JOIN P1_fixtures_fo ON p1_awayfds_lookup.AwayTeam_p1_fo = P1_fixtures_fo.AwayTeam_p1_fo")
#
# P1_fixtures_fo$avg_AF_p1 <- p1_avg_AF
#
# p1_awayfas_lookup <- cbind(p1_teams,p1_away_fas)
#
# p1_awayfas_lookup <- as.data.frame(p1_awayfas_lookup)
#
# colnames(p1_awayfas_lookup) <- c("AwayTeam_p1_fo","p1_awayfas")
#
# P1_fixtures_fo$p1_awayfas <- sqldf("SELECT p1_awayfas_lookup.p1_awayfas FROM p1_awayfas_lookup INNER JOIN P1_fixtures_fo ON p1_awayfas_lookup.AwayTeam_p1_fo = P1_fixtures_fo.AwayTeam_p1_fo")
#
# P1_fixtures_fo$p1_homefds <- rep(p1_home_fds,each = length(p1_teams)-1)
#
# P1_fixtures_fo$p1_awayfds <- as.numeric(unlist(P1_fixtures_fo$p1_awayfds))
# #xGH
# P1_fixtures_fo$p1_xHF <- P1_fixtures_fo$avg_HF_p1 * P1_fixtures_fo$p1_homefas * P1_fixtures_fo$p1_awayfds
# #xGA
#
# P1_fixtures_fo$p1_awayfas <- as.numeric(unlist(P1_fixtures_fo$p1_awayfas))
#
# P1_fixtures_fo$p1_xAF <- P1_fixtures_fo$avg_AF_p1 * P1_fixtures_fo$p1_awayfas * P1_fixtures_fo$p1_homefds
#
# P1_fixtures_fo$p1_0_0 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_0 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_1 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_1 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_0 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_2 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_2 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_1 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_2 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_3 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_0 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_1 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_2 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_3 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_3 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_3 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_4 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_0 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_1 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_2 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_3 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_4 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_4 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_4 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_4 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_5 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_0 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_1 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_2 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_3 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_4 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_5 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_5 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_5 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_5 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_5 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_6 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_0 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(0,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_1 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(1,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_2 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(2,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_3 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(3,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_4 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(4,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_6_5 <- round(stats::dpois(6,P1_fixtures_fo$p1_xHF) * stats::dpois(5,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_0_6 <- round(stats::dpois(0,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_1_6 <- round(stats::dpois(1,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_2_6 <- round(stats::dpois(2,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_3_6 <- round(stats::dpois(3,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_4_6 <- round(stats::dpois(4,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# P1_fixtures_fo$p1_5_6 <- round(stats::dpois(5,P1_fixtures_fo$p1_xHF) * stats::dpois(6,P1_fixtures_fo$p1_xAF), digits = 4)
# #Home win
# P1_fixtures_fo$p1_H <- (
#   P1_fixtures_fo$p1_1_0 + P1_fixtures_fo$p1_2_0 + P1_fixtures_fo$p1_2_1 + P1_fixtures_fo$p1_3_0 + P1_fixtures_fo$p1_3_1 +
#     P1_fixtures_fo$p1_3_2 + P1_fixtures_fo$p1_4_0 + P1_fixtures_fo$p1_4_1 + P1_fixtures_fo$p1_4_2 + P1_fixtures_fo$p1_4_3 +
#     P1_fixtures_fo$p1_5_0 + P1_fixtures_fo$p1_5_1 + P1_fixtures_fo$p1_5_2 + P1_fixtures_fo$p1_5_3 + P1_fixtures_fo$p1_5_4 +
#     P1_fixtures_fo$p1_6_0 + P1_fixtures_fo$p1_6_1 + P1_fixtures_fo$p1_6_2 + P1_fixtures_fo$p1_6_3 + P1_fixtures_fo$p1_6_4 +
#     P1_fixtures_fo$p1_6_5
# )
#
# P1_fixtures_fo$p1_H <- percent(P1_fixtures_fo$p1_H, accuracy = 0.1)
#
# #Draw
# P1_fixtures_fo$p1_D <- (
#
#   P1_fixtures_fo$p1_0_0 + P1_fixtures_fo$p1_1_1 + P1_fixtures_fo$p1_2_2 + P1_fixtures_fo$p1_3_3 + P1_fixtures_fo$p1_4_4 +
#     P1_fixtures_fo$p1_5_5 + P1_fixtures_fo$p1_6_6
# )
#
# P1_fixtures_fo$p1_D <- percent(P1_fixtures_fo$p1_D, accuracy = 0.1)
#
# #Away
#
# P1_fixtures_fo$p1_A <- (
#   P1_fixtures_fo$p1_0_1 + P1_fixtures_fo$p1_0_2 + P1_fixtures_fo$p1_1_2 + P1_fixtures_fo$p1_0_3 + P1_fixtures_fo$p1_1_3 +
#     P1_fixtures_fo$p1_2_3 + P1_fixtures_fo$p1_0_4 + P1_fixtures_fo$p1_1_4 + P1_fixtures_fo$p1_2_4 + P1_fixtures_fo$p1_3_4 +
#     P1_fixtures_fo$p1_0_5 + P1_fixtures_fo$p1_1_5 + P1_fixtures_fo$p1_2_5 + P1_fixtures_fo$p1_3_5 + P1_fixtures_fo$p1_4_5 +
#     P1_fixtures_fo$p1_0_6 + P1_fixtures_fo$p1_1_6 + P1_fixtures_fo$p1_2_6 + P1_fixtures_fo$p1_3_6 + P1_fixtures_fo$p1_4_6 +
#     P1_fixtures_fo$p1_5_6
# )
#
# P1_fixtures_fo$p1_A <- percent(P1_fixtures_fo$p1_A, accuracy = 0.1)
#
# #ov25
# P1_fixtures_fo$p1_ov25 <- (
#   P1_fixtures_fo$p1_2_1 + P1_fixtures_fo$p1_1_2 + P1_fixtures_fo$p1_2_2 + P1_fixtures_fo$p1_3_0 + P1_fixtures_fo$p1_3_1 +
#     P1_fixtures_fo$p1_3_2 + P1_fixtures_fo$p1_0_3 + P1_fixtures_fo$p1_1_3 + P1_fixtures_fo$p1_2_3 + P1_fixtures_fo$p1_3_3 +
#     P1_fixtures_fo$p1_4_0 + P1_fixtures_fo$p1_4_1 + P1_fixtures_fo$p1_4_2 + P1_fixtures_fo$p1_4_3 + P1_fixtures_fo$p1_0_4 +
#     P1_fixtures_fo$p1_1_4 + P1_fixtures_fo$p1_2_4 + P1_fixtures_fo$p1_3_4 + P1_fixtures_fo$p1_4_4 + P1_fixtures_fo$p1_5_0 +
#     P1_fixtures_fo$p1_5_1 + P1_fixtures_fo$p1_5_2 + P1_fixtures_fo$p1_5_3 + P1_fixtures_fo$p1_5_4 + P1_fixtures_fo$p1_0_5 +
#     P1_fixtures_fo$p1_1_5 + P1_fixtures_fo$p1_2_5 + P1_fixtures_fo$p1_3_5 + P1_fixtures_fo$p1_4_5 + P1_fixtures_fo$p1_5_5 +
#     P1_fixtures_fo$p1_6_0 + P1_fixtures_fo$p1_6_1 + P1_fixtures_fo$p1_6_2 + P1_fixtures_fo$p1_6_3 + P1_fixtures_fo$p1_6_4 +
#     P1_fixtures_fo$p1_6_5 + P1_fixtures_fo$p1_0_6 + P1_fixtures_fo$p1_1_6 + P1_fixtures_fo$p1_2_6 + P1_fixtures_fo$p1_3_6 +
#     P1_fixtures_fo$p1_4_6 + P1_fixtures_fo$p1_5_6 + P1_fixtures_fo$p1_6_6
# )
# #un25
# P1_fixtures_fo$p1_un25 <- (
#   P1_fixtures_fo$p1_0_0 + P1_fixtures_fo$p1_1_0 + P1_fixtures_fo$p1_0_1 + P1_fixtures_fo$p1_1_1 + P1_fixtures_fo$p1_2_0 + P1_fixtures_fo$p1_0_2
# )
# #odds
# P1_fixtures_fo$p1_ov25_odds <- round((1/P1_fixtures_fo$p1_ov25),digits = 2)
# P1_fixtures_fo$p1_un25_odds <- round((1/P1_fixtures_fo$p1_un25),digits = 2)
#
# P1_fixtures_fo$p1_ov25_odds
# P1_fixtures_fo$p1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# P1_fixtures_fo$p1_ov25 <- percent(P1_fixtures_fo$p1_ov25, accuracy = 0.1)
#
# P1_fixtures_fo$p1_un25 <- percent(P1_fixtures_fo$p1_un25, accuracy = 0.1)
# P1_fixtures_fo$p1_psfore <- paste(round(P1_fixtures_fo$p1_xHF,digits = 0),round(P1_fixtures_fo$p1_xAF,digits = 0),sep = "-")
# #write.xlsx(P1_fixtures,'Divisions/P1.xlsx',sheetName = "P1", append = TRUE)
# #################################################################################################################
# #SC0
# HomeTeam_sc0_fo <- rep(sc0_teams, each = length(sc0_teams))
# AwayTeam_sc0_fo <- rep(sc0_teams, length(sc0_teams))
# SC0_fixtures_fo <- cbind(HomeTeam_sc0_fo,AwayTeam_sc0_fo)
# SC0_fixtures_fo <- as.data.frame(SC0_fixtures_fo)
# SC0_fixtures_fo <- SC0_fixtures_fo[!SC0_fixtures_fo$HomeTeam_sc0_fo == SC0_fixtures_fo$AwayTeam_sc0_fo,]
# rownames(SC0_fixtures_fo) <- NULL
# SC0_fixtures_fo$Div <- "SC0"
# SC0_fixtures_fo <- SC0_fixtures_fo[,c(3,1,2)]
#
# SC0_fixtures_fo$avg_HF_sc0 <- sc0_avg_HF
#
# SC0_fixtures_fo$sc0_homefas <- rep(sc0_home_fas,each = length(sc0_teams)-1)
#
# sc0_awayfds_lookup <- cbind(sc0_teams,sc0_away_fds)
#
# sc0_awayfds_lookup <- as.data.frame(sc0_awayfds_lookup)
#
# colnames(sc0_awayfds_lookup) <- c("AwayTeam_sc0_fo","sc0_awayfds")
#
#
# require('RH2')
# SC0_fixtures_fo$sc0_awayfds <- sqldf("SELECT sc0_awayfds_lookup.sc0_awayfds FROM sc0_awayfds_lookup INNER JOIN SC0_fixtures_fo ON sc0_awayfds_lookup.AwayTeam_sc0_fo = SC0_fixtures_fo.AwayTeam_sc0_fo")
#
# SC0_fixtures_fo$avg_AF_sc0 <- sc0_avg_AF
#
# sc0_awayfas_lookup <- cbind(sc0_teams,sc0_away_fas)
#
# sc0_awayfas_lookup <- as.data.frame(sc0_awayfas_lookup)
#
# colnames(sc0_awayfas_lookup) <- c("AwayTeam_sc0_fo","sc0_awayfas")
#
# SC0_fixtures_fo$sc0_awayfas <- sqldf("SELECT sc0_awayfas_lookup.sc0_awayfas FROM sc0_awayfas_lookup INNER JOIN SC0_fixtures_fo ON sc0_awayfas_lookup.AwayTeam_sc0_fo = SC0_fixtures_fo.AwayTeam_sc0_fo")
#
# SC0_fixtures_fo$sc0_homefds <- rep(sc0_home_fds,each = length(sc0_teams)-1)
#
# SC0_fixtures_fo$sc0_awayfds <- as.numeric(unlist(SC0_fixtures_fo$sc0_awayfds))
# #xGH
# SC0_fixtures_fo$sc0_xHF <- SC0_fixtures_fo$avg_HF_sc0 * SC0_fixtures_fo$sc0_homefas * SC0_fixtures_fo$sc0_awayfds
# #xGA
#
# SC0_fixtures_fo$sc0_awayfas <- as.numeric(unlist(SC0_fixtures_fo$sc0_awayfas))
#
# SC0_fixtures_fo$sc0_xAF <- SC0_fixtures_fo$avg_AF_sc0 * SC0_fixtures_fo$sc0_awayfas * SC0_fixtures_fo$sc0_homefds
#
# SC0_fixtures_fo$sc0_0_0 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_0 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_1 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_1 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_0 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_2 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_2 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_1 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_2 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_3 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_0 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_1 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_2 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_3 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_3 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_3 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_4 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_0 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_1 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_2 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_3 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_4 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_4 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_4 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_4 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_5 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_0 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_1 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_2 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_3 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_4 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_5 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_5 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_5 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_5 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_5 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_6 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_0 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(0,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_1 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(1,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_2 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(2,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_3 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(3,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_4 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(4,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_6_5 <- round(stats::dpois(6,SC0_fixtures_fo$sc0_xHF) * stats::dpois(5,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_0_6 <- round(stats::dpois(0,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_1_6 <- round(stats::dpois(1,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_2_6 <- round(stats::dpois(2,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_3_6 <- round(stats::dpois(3,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_4_6 <- round(stats::dpois(4,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# SC0_fixtures_fo$sc0_5_6 <- round(stats::dpois(5,SC0_fixtures_fo$sc0_xHF) * stats::dpois(6,SC0_fixtures_fo$sc0_xAF), digits = 4)
# #Home win
# SC0_fixtures_fo$sc0_H <- (
#   SC0_fixtures_fo$sc0_1_0 + SC0_fixtures_fo$sc0_2_0 + SC0_fixtures_fo$sc0_2_1 + SC0_fixtures_fo$sc0_3_0 + SC0_fixtures_fo$sc0_3_1 +
#     SC0_fixtures_fo$sc0_3_2 + SC0_fixtures_fo$sc0_4_0 + SC0_fixtures_fo$sc0_4_1 + SC0_fixtures_fo$sc0_4_2 + SC0_fixtures_fo$sc0_4_3 +
#     SC0_fixtures_fo$sc0_5_0 + SC0_fixtures_fo$sc0_5_1 + SC0_fixtures_fo$sc0_5_2 + SC0_fixtures_fo$sc0_5_3 + SC0_fixtures_fo$sc0_5_4 +
#     SC0_fixtures_fo$sc0_6_0 + SC0_fixtures_fo$sc0_6_1 + SC0_fixtures_fo$sc0_6_2 + SC0_fixtures_fo$sc0_6_3 + SC0_fixtures_fo$sc0_6_4 +
#     SC0_fixtures_fo$sc0_6_5
# )
#
# SC0_fixtures_fo$sc0_H <- percent(SC0_fixtures_fo$sc0_H, accuracy = 0.1)
#
# #Draw
# SC0_fixtures_fo$sc0_D <- (
#
#   SC0_fixtures_fo$sc0_0_0 + SC0_fixtures_fo$sc0_1_1 + SC0_fixtures_fo$sc0_2_2 + SC0_fixtures_fo$sc0_3_3 + SC0_fixtures_fo$sc0_4_4 +
#     SC0_fixtures_fo$sc0_5_5 + SC0_fixtures_fo$sc0_6_6
# )
#
# SC0_fixtures_fo$sc0_D <- percent(SC0_fixtures_fo$sc0_D, accuracy = 0.1)
#
# #Away
#
# SC0_fixtures_fo$sc0_A <- (
#   SC0_fixtures_fo$sc0_0_1 + SC0_fixtures_fo$sc0_0_2 + SC0_fixtures_fo$sc0_1_2 + SC0_fixtures_fo$sc0_0_3 + SC0_fixtures_fo$sc0_1_3 +
#     SC0_fixtures_fo$sc0_2_3 + SC0_fixtures_fo$sc0_0_4 + SC0_fixtures_fo$sc0_1_4 + SC0_fixtures_fo$sc0_2_4 + SC0_fixtures_fo$sc0_3_4 +
#     SC0_fixtures_fo$sc0_0_5 + SC0_fixtures_fo$sc0_1_5 + SC0_fixtures_fo$sc0_2_5 + SC0_fixtures_fo$sc0_3_5 + SC0_fixtures_fo$sc0_4_5 +
#     SC0_fixtures_fo$sc0_0_6 + SC0_fixtures_fo$sc0_1_6 + SC0_fixtures_fo$sc0_2_6 + SC0_fixtures_fo$sc0_3_6 + SC0_fixtures_fo$sc0_4_6 +
#     SC0_fixtures_fo$sc0_5_6
# )
#
# SC0_fixtures_fo$sc0_A <- percent(SC0_fixtures_fo$sc0_A, accuracy = 0.1)
#
# #ov25
# SC0_fixtures_fo$sc0_ov25 <- (
#   SC0_fixtures_fo$sc0_2_1 + SC0_fixtures_fo$sc0_1_2 + SC0_fixtures_fo$sc0_2_2 + SC0_fixtures_fo$sc0_3_0 + SC0_fixtures_fo$sc0_3_1 +
#     SC0_fixtures_fo$sc0_3_2 + SC0_fixtures_fo$sc0_0_3 + SC0_fixtures_fo$sc0_1_3 + SC0_fixtures_fo$sc0_2_3 + SC0_fixtures_fo$sc0_3_3 +
#     SC0_fixtures_fo$sc0_4_0 + SC0_fixtures_fo$sc0_4_1 + SC0_fixtures_fo$sc0_4_2 + SC0_fixtures_fo$sc0_4_3 + SC0_fixtures_fo$sc0_0_4 +
#     SC0_fixtures_fo$sc0_1_4 + SC0_fixtures_fo$sc0_2_4 + SC0_fixtures_fo$sc0_3_4 + SC0_fixtures_fo$sc0_4_4 + SC0_fixtures_fo$sc0_5_0 +
#     SC0_fixtures_fo$sc0_5_1 + SC0_fixtures_fo$sc0_5_2 + SC0_fixtures_fo$sc0_5_3 + SC0_fixtures_fo$sc0_5_4 + SC0_fixtures_fo$sc0_0_5 +
#     SC0_fixtures_fo$sc0_1_5 + SC0_fixtures_fo$sc0_2_5 + SC0_fixtures_fo$sc0_3_5 + SC0_fixtures_fo$sc0_4_5 + SC0_fixtures_fo$sc0_5_5 +
#     SC0_fixtures_fo$sc0_6_0 + SC0_fixtures_fo$sc0_6_1 + SC0_fixtures_fo$sc0_6_2 + SC0_fixtures_fo$sc0_6_3 + SC0_fixtures_fo$sc0_6_4 +
#     SC0_fixtures_fo$sc0_6_5 + SC0_fixtures_fo$sc0_0_6 + SC0_fixtures_fo$sc0_1_6 + SC0_fixtures_fo$sc0_2_6 + SC0_fixtures_fo$sc0_3_6 +
#     SC0_fixtures_fo$sc0_4_6 + SC0_fixtures_fo$sc0_5_6 + SC0_fixtures_fo$sc0_6_6
# )
# #un25
# SC0_fixtures_fo$sc0_un25 <- (
#   SC0_fixtures_fo$sc0_0_0 + SC0_fixtures_fo$sc0_1_0 + SC0_fixtures_fo$sc0_0_1 + SC0_fixtures_fo$sc0_1_1 + SC0_fixtures_fo$sc0_2_0 + SC0_fixtures_fo$sc0_0_2
# )
# #odds
# SC0_fixtures_fo$sc0_ov25_odds <- round((1/SC0_fixtures_fo$sc0_ov25),digits = 2)
# SC0_fixtures_fo$sc0_un25_odds <- round((1/SC0_fixtures_fo$sc0_un25),digits = 2)
#
# SC0_fixtures_fo$sc0_ov25_odds
# SC0_fixtures_fo$sc0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC0_fixtures_fo$sc0_ov25 <- percent(SC0_fixtures_fo$sc0_ov25, accuracy = 0.1)
#
# SC0_fixtures_fo$sc0_un25 <- percent(SC0_fixtures_fo$sc0_un25, accuracy = 0.1)
# SC0_fixtures_fo$sc0_psfore <- paste(round(SC0_fixtures_fo$sc0_xHF,digits = 0),round(SC0_fixtures_fo$sc0_xAF,digits = 0),sep = "-")
# #write.xlsx(SC0_fixtures,'Divisions/SC0.xlsx',sheetName = "SC0", append = TRUE)
# #################################################################################################################
# #SC1
# HomeTeam_sc1_fo <- rep(sc1_teams, each = length(sc1_teams))
# AwayTeam_sc1_fo <- rep(sc1_teams, length(sc1_teams))
# SC1_fixtures_fo <- cbind(HomeTeam_sc1_fo,AwayTeam_sc1_fo)
# SC1_fixtures_fo <- as.data.frame(SC1_fixtures_fo)
# SC1_fixtures_fo <- SC1_fixtures_fo[!SC1_fixtures_fo$HomeTeam_sc1_fo == SC1_fixtures_fo$AwayTeam_sc1_fo,]
# rownames(SC1_fixtures_fo) <- NULL
# SC1_fixtures_fo$Div <- "SC1"
# SC1_fixtures_fo <- SC1_fixtures_fo[,c(3,1,2)]
#
# SC1_fixtures_fo$avg_HF_sc1 <- sc1_avg_HF
#
# SC1_fixtures_fo$sc1_homefas <- rep(sc1_home_fas,each = length(sc1_teams)-1)
#
# sc1_awayfds_lookup <- cbind(sc1_teams,sc1_away_fds)
#
# sc1_awayfds_lookup <- as.data.frame(sc1_awayfds_lookup)
#
# colnames(sc1_awayfds_lookup) <- c("AwayTeam_sc1_fo","sc1_awayfds")
#
#
# require('RH2')
# SC1_fixtures_fo$sc1_awayfds <- sqldf("SELECT sc1_awayfds_lookup.sc1_awayfds FROM sc1_awayfds_lookup INNER JOIN SC1_fixtures_fo ON sc1_awayfds_lookup.AwayTeam_sc1_fo = SC1_fixtures_fo.AwayTeam_sc1_fo")
#
# SC1_fixtures_fo$avg_AF_sc1 <- sc1_avg_AF
#
# sc1_awayfas_lookup <- cbind(sc1_teams,sc1_away_fas)
#
# sc1_awayfas_lookup <- as.data.frame(sc1_awayfas_lookup)
#
# colnames(sc1_awayfas_lookup) <- c("AwayTeam_sc1_fo","sc1_awayfas")
#
# SC1_fixtures_fo$sc1_awayfas <- sqldf("SELECT sc1_awayfas_lookup.sc1_awayfas FROM sc1_awayfas_lookup INNER JOIN SC1_fixtures_fo ON sc1_awayfas_lookup.AwayTeam_sc1_fo = SC1_fixtures_fo.AwayTeam_sc1_fo")
#
# SC1_fixtures_fo$sc1_homefds <- rep(sc1_home_fds,each = length(sc1_teams)-1)
#
# SC1_fixtures_fo$sc1_awayfds <- as.numeric(unlist(SC1_fixtures_fo$sc1_awayfds))
# #xGH
# SC1_fixtures_fo$sc1_xHF <- SC1_fixtures_fo$avg_HF_sc1 * SC1_fixtures_fo$sc1_homefas * SC1_fixtures_fo$sc1_awayfds
# #xGA
#
# SC1_fixtures_fo$sc1_awayfas <- as.numeric(unlist(SC1_fixtures_fo$sc1_awayfas))
#
# SC1_fixtures_fo$sc1_xAF <- SC1_fixtures_fo$avg_AF_sc1 * SC1_fixtures_fo$sc1_awayfas * SC1_fixtures_fo$sc1_homefds
#
# SC1_fixtures_fo$sc1_0_0 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_0 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_1 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_1 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_0 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_2 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_2 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_1 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_2 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_3 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_0 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_1 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_2 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_3 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_3 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_3 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_4 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_0 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_1 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_2 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_3 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_4 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_4 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_4 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_4 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_5 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_0 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_1 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_2 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_3 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_4 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_5 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_5 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_5 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_5 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_5 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_6 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_0 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(0,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_1 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(1,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_2 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(2,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_3 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(3,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_4 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(4,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_6_5 <- round(stats::dpois(6,SC1_fixtures_fo$sc1_xHF) * stats::dpois(5,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_0_6 <- round(stats::dpois(0,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_1_6 <- round(stats::dpois(1,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_2_6 <- round(stats::dpois(2,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_3_6 <- round(stats::dpois(3,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_4_6 <- round(stats::dpois(4,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# SC1_fixtures_fo$sc1_5_6 <- round(stats::dpois(5,SC1_fixtures_fo$sc1_xHF) * stats::dpois(6,SC1_fixtures_fo$sc1_xAF), digits = 4)
# #Home win
# SC1_fixtures_fo$sc1_H <- (
#   SC1_fixtures_fo$sc1_1_0 + SC1_fixtures_fo$sc1_2_0 + SC1_fixtures_fo$sc1_2_1 + SC1_fixtures_fo$sc1_3_0 + SC1_fixtures_fo$sc1_3_1 +
#     SC1_fixtures_fo$sc1_3_2 + SC1_fixtures_fo$sc1_4_0 + SC1_fixtures_fo$sc1_4_1 + SC1_fixtures_fo$sc1_4_2 + SC1_fixtures_fo$sc1_4_3 +
#     SC1_fixtures_fo$sc1_5_0 + SC1_fixtures_fo$sc1_5_1 + SC1_fixtures_fo$sc1_5_2 + SC1_fixtures_fo$sc1_5_3 + SC1_fixtures_fo$sc1_5_4 +
#     SC1_fixtures_fo$sc1_6_0 + SC1_fixtures_fo$sc1_6_1 + SC1_fixtures_fo$sc1_6_2 + SC1_fixtures_fo$sc1_6_3 + SC1_fixtures_fo$sc1_6_4 +
#     SC1_fixtures_fo$sc1_6_5
# )
#
# SC1_fixtures_fo$sc1_H <- percent(SC1_fixtures_fo$sc1_H, accuracy = 0.1)
#
# #Draw
# SC1_fixtures_fo$sc1_D <- (
#
#   SC1_fixtures_fo$sc1_0_0 + SC1_fixtures_fo$sc1_1_1 + SC1_fixtures_fo$sc1_2_2 + SC1_fixtures_fo$sc1_3_3 + SC1_fixtures_fo$sc1_4_4 +
#     SC1_fixtures_fo$sc1_5_5 + SC1_fixtures_fo$sc1_6_6
# )
#
# SC1_fixtures_fo$sc1_D <- percent(SC1_fixtures_fo$sc1_D, accuracy = 0.1)
#
# #Away
#
# SC1_fixtures_fo$sc1_A <- (
#   SC1_fixtures_fo$sc1_0_1 + SC1_fixtures_fo$sc1_0_2 + SC1_fixtures_fo$sc1_1_2 + SC1_fixtures_fo$sc1_0_3 + SC1_fixtures_fo$sc1_1_3 +
#     SC1_fixtures_fo$sc1_2_3 + SC1_fixtures_fo$sc1_0_4 + SC1_fixtures_fo$sc1_1_4 + SC1_fixtures_fo$sc1_2_4 + SC1_fixtures_fo$sc1_3_4 +
#     SC1_fixtures_fo$sc1_0_5 + SC1_fixtures_fo$sc1_1_5 + SC1_fixtures_fo$sc1_2_5 + SC1_fixtures_fo$sc1_3_5 + SC1_fixtures_fo$sc1_4_5 +
#     SC1_fixtures_fo$sc1_0_6 + SC1_fixtures_fo$sc1_1_6 + SC1_fixtures_fo$sc1_2_6 + SC1_fixtures_fo$sc1_3_6 + SC1_fixtures_fo$sc1_4_6 +
#     SC1_fixtures_fo$sc1_5_6
# )
#
# SC1_fixtures_fo$sc1_A <- percent(SC1_fixtures_fo$sc1_A, accuracy = 0.1)
#
# #ov25
# SC1_fixtures_fo$sc1_ov25 <- (
#   SC1_fixtures_fo$sc1_2_1 + SC1_fixtures_fo$sc1_1_2 + SC1_fixtures_fo$sc1_2_2 + SC1_fixtures_fo$sc1_3_0 + SC1_fixtures_fo$sc1_3_1 +
#     SC1_fixtures_fo$sc1_3_2 + SC1_fixtures_fo$sc1_0_3 + SC1_fixtures_fo$sc1_1_3 + SC1_fixtures_fo$sc1_2_3 + SC1_fixtures_fo$sc1_3_3 +
#     SC1_fixtures_fo$sc1_4_0 + SC1_fixtures_fo$sc1_4_1 + SC1_fixtures_fo$sc1_4_2 + SC1_fixtures_fo$sc1_4_3 + SC1_fixtures_fo$sc1_0_4 +
#     SC1_fixtures_fo$sc1_1_4 + SC1_fixtures_fo$sc1_2_4 + SC1_fixtures_fo$sc1_3_4 + SC1_fixtures_fo$sc1_4_4 + SC1_fixtures_fo$sc1_5_0 +
#     SC1_fixtures_fo$sc1_5_1 + SC1_fixtures_fo$sc1_5_2 + SC1_fixtures_fo$sc1_5_3 + SC1_fixtures_fo$sc1_5_4 + SC1_fixtures_fo$sc1_0_5 +
#     SC1_fixtures_fo$sc1_1_5 + SC1_fixtures_fo$sc1_2_5 + SC1_fixtures_fo$sc1_3_5 + SC1_fixtures_fo$sc1_4_5 + SC1_fixtures_fo$sc1_5_5 +
#     SC1_fixtures_fo$sc1_6_0 + SC1_fixtures_fo$sc1_6_1 + SC1_fixtures_fo$sc1_6_2 + SC1_fixtures_fo$sc1_6_3 + SC1_fixtures_fo$sc1_6_4 +
#     SC1_fixtures_fo$sc1_6_5 + SC1_fixtures_fo$sc1_0_6 + SC1_fixtures_fo$sc1_1_6 + SC1_fixtures_fo$sc1_2_6 + SC1_fixtures_fo$sc1_3_6 +
#     SC1_fixtures_fo$sc1_4_6 + SC1_fixtures_fo$sc1_5_6 + SC1_fixtures_fo$sc1_6_6
# )
# #un25
# SC1_fixtures_fo$sc1_un25 <- (
#   SC1_fixtures_fo$sc1_0_0 + SC1_fixtures_fo$sc1_1_0 + SC1_fixtures_fo$sc1_0_1 + SC1_fixtures_fo$sc1_1_1 + SC1_fixtures_fo$sc1_2_0 + SC1_fixtures_fo$sc1_0_2
# )
# #odds
# SC1_fixtures_fo$sc1_ov25_odds <- round((1/SC1_fixtures_fo$sc1_ov25),digits = 2)
# SC1_fixtures_fo$sc1_un25_odds <- round((1/SC1_fixtures_fo$sc1_un25),digits = 2)
#
# SC1_fixtures_fo$sc1_ov25_odds
# SC1_fixtures_fo$sc1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC1_fixtures_fo$sc1_ov25 <- percent(SC1_fixtures_fo$sc1_ov25, accuracy = 0.1)
#
# SC1_fixtures_fo$sc1_un25 <- percent(SC1_fixtures_fo$sc1_un25, accuracy = 0.1)
# SC1_fixtures_fo$sc1_psfore <- paste(round(SC1_fixtures_fo$sc1_xHF,digits = 0),round(SC1_fixtures_fo$sc1_xAF,digits = 0),sep = "-")
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# ##########################################################################################################################################
# #SC2
# HomeTeam_sc2_fo <- rep(sc2_teams, each = length(sc2_teams))
# AwayTeam_sc2_fo <- rep(sc2_teams, length(sc2_teams))
# SC2_fixtures_fo <- cbind(HomeTeam_sc2_fo,AwayTeam_sc2_fo)
# SC2_fixtures_fo <- as.data.frame(SC2_fixtures_fo)
# SC2_fixtures_fo <- SC2_fixtures_fo[!SC2_fixtures_fo$HomeTeam_sc2_fo == SC2_fixtures_fo$AwayTeam_sc2_fo,]
# rownames(SC2_fixtures_fo) <- NULL
# SC2_fixtures_fo$Div <- "SC2"
# SC2_fixtures_fo <- SC2_fixtures_fo[,c(3,1,2)]
#
# SC2_fixtures_fo$avg_HF_sc2 <- sc2_avg_HF
#
# SC2_fixtures_fo$sc2_homefas <- rep(sc2_home_fas,each = length(sc2_teams)-1)
#
# sc2_awayfds_lookup <- cbind(sc2_teams,sc2_away_fds)
#
# sc2_awayfds_lookup <- as.data.frame(sc2_awayfds_lookup)
#
# colnames(sc2_awayfds_lookup) <- c("AwayTeam_sc2_fo","sc2_awayfds")
#
#
# require('RH2')
# SC2_fixtures_fo$sc2_awayfds <- sqldf("SELECT sc2_awayfds_lookup.sc2_awayfds FROM sc2_awayfds_lookup INNER JOIN SC2_fixtures_fo ON sc2_awayfds_lookup.AwayTeam_sc2_fo = SC2_fixtures_fo.AwayTeam_sc2_fo")
#
# SC2_fixtures_fo$avg_AF_sc2 <- sc2_avg_AF
#
# sc2_awayfas_lookup <- cbind(sc2_teams,sc2_away_fas)
#
# sc2_awayfas_lookup <- as.data.frame(sc2_awayfas_lookup)
#
# colnames(sc2_awayfas_lookup) <- c("AwayTeam_sc2_fo","sc2_awayfas")
#
# SC2_fixtures_fo$sc2_awayfas <- sqldf("SELECT sc2_awayfas_lookup.sc2_awayfas FROM sc2_awayfas_lookup INNER JOIN SC2_fixtures_fo ON sc2_awayfas_lookup.AwayTeam_sc2_fo = SC2_fixtures_fo.AwayTeam_sc2_fo")
#
# SC2_fixtures_fo$sc2_homefds <- rep(sc2_home_fds,each = length(sc2_teams)-1)
#
# SC2_fixtures_fo$sc2_awayfds <- as.numeric(unlist(SC2_fixtures_fo$sc2_awayfds))
# #xGH
# SC2_fixtures_fo$sc2_xHF <- SC2_fixtures_fo$avg_HF_sc2 * SC2_fixtures_fo$sc2_homefas * SC2_fixtures_fo$sc2_awayfds
# #xGA
#
# SC2_fixtures_fo$sc2_awayfas <- as.numeric(unlist(SC2_fixtures_fo$sc2_awayfas))
#
# SC2_fixtures_fo$sc2_xAF <- SC2_fixtures_fo$avg_AF_sc2 * SC2_fixtures_fo$sc2_awayfas * SC2_fixtures_fo$sc2_homefds
#
# SC2_fixtures_fo$sc2_0_0 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_0 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_1 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_1 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_0 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_2 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_2 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_1 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_2 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_3 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_0 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_1 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_2 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_3 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_3 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_3 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_4 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_0 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_1 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_2 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_3 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_4 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_4 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_4 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_4 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_5 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_0 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_1 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_2 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_3 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_4 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_5 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_5 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_5 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_5 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_5 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_6 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_0 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(0,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_1 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(1,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_2 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(2,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_3 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(3,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_4 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(4,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_6_5 <- round(stats::dpois(6,SC2_fixtures_fo$sc2_xHF) * stats::dpois(5,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_0_6 <- round(stats::dpois(0,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_1_6 <- round(stats::dpois(1,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_2_6 <- round(stats::dpois(2,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_3_6 <- round(stats::dpois(3,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_4_6 <- round(stats::dpois(4,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# SC2_fixtures_fo$sc2_5_6 <- round(stats::dpois(5,SC2_fixtures_fo$sc2_xHF) * stats::dpois(6,SC2_fixtures_fo$sc2_xAF), digits = 4)
# #Home win
# SC2_fixtures_fo$sc2_H <- (
#   SC2_fixtures_fo$sc2_1_0 + SC2_fixtures_fo$sc2_2_0 + SC2_fixtures_fo$sc2_2_1 + SC2_fixtures_fo$sc2_3_0 + SC2_fixtures_fo$sc2_3_1 +
#     SC2_fixtures_fo$sc2_3_2 + SC2_fixtures_fo$sc2_4_0 + SC2_fixtures_fo$sc2_4_1 + SC2_fixtures_fo$sc2_4_2 + SC2_fixtures_fo$sc2_4_3 +
#     SC2_fixtures_fo$sc2_5_0 + SC2_fixtures_fo$sc2_5_1 + SC2_fixtures_fo$sc2_5_2 + SC2_fixtures_fo$sc2_5_3 + SC2_fixtures_fo$sc2_5_4 +
#     SC2_fixtures_fo$sc2_6_0 + SC2_fixtures_fo$sc2_6_1 + SC2_fixtures_fo$sc2_6_2 + SC2_fixtures_fo$sc2_6_3 + SC2_fixtures_fo$sc2_6_4 +
#     SC2_fixtures_fo$sc2_6_5
# )
#
# SC2_fixtures_fo$sc2_H <- percent(SC2_fixtures_fo$sc2_H, accuracy = 0.1)
#
# #Draw
# SC2_fixtures_fo$sc2_D <- (
#
#   SC2_fixtures_fo$sc2_0_0 + SC2_fixtures_fo$sc2_1_1 + SC2_fixtures_fo$sc2_2_2 + SC2_fixtures_fo$sc2_3_3 + SC2_fixtures_fo$sc2_4_4 +
#     SC2_fixtures_fo$sc2_5_5 + SC2_fixtures_fo$sc2_6_6
# )
#
# SC2_fixtures_fo$sc2_D <- percent(SC2_fixtures_fo$sc2_D, accuracy = 0.1)
#
# #Away
#
# SC2_fixtures_fo$sc2_A <- (
#   SC2_fixtures_fo$sc2_0_1 + SC2_fixtures_fo$sc2_0_2 + SC2_fixtures_fo$sc2_1_2 + SC2_fixtures_fo$sc2_0_3 + SC2_fixtures_fo$sc2_1_3 +
#     SC2_fixtures_fo$sc2_2_3 + SC2_fixtures_fo$sc2_0_4 + SC2_fixtures_fo$sc2_1_4 + SC2_fixtures_fo$sc2_2_4 + SC2_fixtures_fo$sc2_3_4 +
#     SC2_fixtures_fo$sc2_0_5 + SC2_fixtures_fo$sc2_1_5 + SC2_fixtures_fo$sc2_2_5 + SC2_fixtures_fo$sc2_3_5 + SC2_fixtures_fo$sc2_4_5 +
#     SC2_fixtures_fo$sc2_0_6 + SC2_fixtures_fo$sc2_1_6 + SC2_fixtures_fo$sc2_2_6 + SC2_fixtures_fo$sc2_3_6 + SC2_fixtures_fo$sc2_4_6 +
#     SC2_fixtures_fo$sc2_5_6
# )
#
# SC2_fixtures_fo$sc2_A <- percent(SC2_fixtures_fo$sc2_A, accuracy = 0.1)
#
# #ov25
# SC2_fixtures_fo$sc2_ov25 <- (
#   SC2_fixtures_fo$sc2_2_1 + SC2_fixtures_fo$sc2_1_2 + SC2_fixtures_fo$sc2_2_2 + SC2_fixtures_fo$sc2_3_0 + SC2_fixtures_fo$sc2_3_1 +
#     SC2_fixtures_fo$sc2_3_2 + SC2_fixtures_fo$sc2_0_3 + SC2_fixtures_fo$sc2_1_3 + SC2_fixtures_fo$sc2_2_3 + SC2_fixtures_fo$sc2_3_3 +
#     SC2_fixtures_fo$sc2_4_0 + SC2_fixtures_fo$sc2_4_1 + SC2_fixtures_fo$sc2_4_2 + SC2_fixtures_fo$sc2_4_3 + SC2_fixtures_fo$sc2_0_4 +
#     SC2_fixtures_fo$sc2_1_4 + SC2_fixtures_fo$sc2_2_4 + SC2_fixtures_fo$sc2_3_4 + SC2_fixtures_fo$sc2_4_4 + SC2_fixtures_fo$sc2_5_0 +
#     SC2_fixtures_fo$sc2_5_1 + SC2_fixtures_fo$sc2_5_2 + SC2_fixtures_fo$sc2_5_3 + SC2_fixtures_fo$sc2_5_4 + SC2_fixtures_fo$sc2_0_5 +
#     SC2_fixtures_fo$sc2_1_5 + SC2_fixtures_fo$sc2_2_5 + SC2_fixtures_fo$sc2_3_5 + SC2_fixtures_fo$sc2_4_5 + SC2_fixtures_fo$sc2_5_5 +
#     SC2_fixtures_fo$sc2_6_0 + SC2_fixtures_fo$sc2_6_1 + SC2_fixtures_fo$sc2_6_2 + SC2_fixtures_fo$sc2_6_3 + SC2_fixtures_fo$sc2_6_4 +
#     SC2_fixtures_fo$sc2_6_5 + SC2_fixtures_fo$sc2_0_6 + SC2_fixtures_fo$sc2_1_6 + SC2_fixtures_fo$sc2_2_6 + SC2_fixtures_fo$sc2_3_6 +
#     SC2_fixtures_fo$sc2_4_6 + SC2_fixtures_fo$sc2_5_6 + SC2_fixtures_fo$sc2_6_6
# )
# #un25
# SC2_fixtures_fo$sc2_un25 <- (
#   SC2_fixtures_fo$sc2_0_0 + SC2_fixtures_fo$sc2_1_0 + SC2_fixtures_fo$sc2_0_1 + SC2_fixtures_fo$sc2_1_1 + SC2_fixtures_fo$sc2_2_0 + SC2_fixtures_fo$sc2_0_2
# )
# #odds
# SC2_fixtures_fo$sc2_ov25_odds <- round((1/SC2_fixtures_fo$sc2_ov25),digits = 2)
# SC2_fixtures_fo$sc2_un25_odds <- round((1/SC2_fixtures_fo$sc2_un25),digits = 2)
#
# SC2_fixtures_fo$sc2_ov25_odds
# SC2_fixtures_fo$sc2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC2_fixtures_fo$sc2_ov25 <- percent(SC2_fixtures_fo$sc2_ov25, accuracy = 0.1)
#
# SC2_fixtures_fo$sc2_un25 <- percent(SC2_fixtures_fo$sc2_un25, accuracy = 0.1)
# SC2_fixtures_fo$sc2_psfore <- paste(round(SC2_fixtures_fo$sc2_xHF,digits = 0),round(SC2_fixtures_fo$sc2_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC2_fixtures,'Divisions/SC2.xlsx',sheetName = "SC2", append = TRUE)
# ################################################################################################################################################################################################################################
# #SC3
# HomeTeam_sc3_fo <- rep(sc3_teams, each = length(sc3_teams))
# AwayTeam_sc3_fo <- rep(sc3_teams, length(sc3_teams))
# SC3_fixtures_fo <- cbind(HomeTeam_sc3_fo,AwayTeam_sc3_fo)
# SC3_fixtures_fo <- as.data.frame(SC3_fixtures_fo)
# SC3_fixtures_fo <- SC3_fixtures_fo[!SC3_fixtures_fo$HomeTeam_sc3_fo == SC3_fixtures_fo$AwayTeam_sc3_fo,]
# rownames(SC3_fixtures_fo) <- NULL
# SC3_fixtures_fo$Div <- "SC3"
# SC3_fixtures_fo <- SC3_fixtures_fo[,c(3,1,2)]
#
# SC3_fixtures_fo$avg_HF_sc3 <- sc3_avg_HF
#
# SC3_fixtures_fo$sc3_homefas <- rep(sc3_home_fas,each = length(sc3_teams)-1)
#
# sc3_awayfds_lookup <- cbind(sc3_teams,sc3_away_fds)
#
# sc3_awayfds_lookup <- as.data.frame(sc3_awayfds_lookup)
#
# colnames(sc3_awayfds_lookup) <- c("AwayTeam_sc3_fo","sc3_awayfds")
#
#
# require('RH2')
# SC3_fixtures_fo$sc3_awayfds <- sqldf("SELECT sc3_awayfds_lookup.sc3_awayfds FROM sc3_awayfds_lookup INNER JOIN SC3_fixtures_fo ON sc3_awayfds_lookup.AwayTeam_sc3_fo = SC3_fixtures_fo.AwayTeam_sc3_fo")
#
# SC3_fixtures_fo$avg_AF_sc3 <- sc3_avg_AF
#
# sc3_awayfas_lookup <- cbind(sc3_teams,sc3_away_fas)
#
# sc3_awayfas_lookup <- as.data.frame(sc3_awayfas_lookup)
#
# colnames(sc3_awayfas_lookup) <- c("AwayTeam_sc3_fo","sc3_awayfas")
#
# SC3_fixtures_fo$sc3_awayfas <- sqldf("SELECT sc3_awayfas_lookup.sc3_awayfas FROM sc3_awayfas_lookup INNER JOIN SC3_fixtures_fo ON sc3_awayfas_lookup.AwayTeam_sc3_fo = SC3_fixtures_fo.AwayTeam_sc3_fo")
#
# SC3_fixtures_fo$sc3_homefds <- rep(sc3_home_fds,each = length(sc3_teams)-1)
#
# SC3_fixtures_fo$sc3_awayfds <- as.numeric(unlist(SC3_fixtures_fo$sc3_awayfds))
# #xGH
# SC3_fixtures_fo$sc3_xHF <- SC3_fixtures_fo$avg_HF_sc3 * SC3_fixtures_fo$sc3_homefas * SC3_fixtures_fo$sc3_awayfds
# #xGA
#
# SC3_fixtures_fo$sc3_awayfas <- as.numeric(unlist(SC3_fixtures_fo$sc3_awayfas))
#
# SC3_fixtures_fo$sc3_xAF <- SC3_fixtures_fo$avg_AF_sc3 * SC3_fixtures_fo$sc3_awayfas * SC3_fixtures_fo$sc3_homefds
#
# SC3_fixtures_fo$sc3_0_0 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_0 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_1 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_1 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_0 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_2 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_2 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_1 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_2 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_3 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_0 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_1 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_2 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_3 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_3 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_3 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_4 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_0 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_1 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_2 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_3 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_4 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_4 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_4 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_4 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_5 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_0 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_1 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_2 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_3 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_4 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_5 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_5 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_5 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_5 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_5 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_6 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_0 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(0,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_1 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(1,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_2 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(2,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_3 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(3,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_4 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(4,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_6_5 <- round(stats::dpois(6,SC3_fixtures_fo$sc3_xHF) * stats::dpois(5,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_0_6 <- round(stats::dpois(0,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_1_6 <- round(stats::dpois(1,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_2_6 <- round(stats::dpois(2,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_3_6 <- round(stats::dpois(3,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_4_6 <- round(stats::dpois(4,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# SC3_fixtures_fo$sc3_5_6 <- round(stats::dpois(5,SC3_fixtures_fo$sc3_xHF) * stats::dpois(6,SC3_fixtures_fo$sc3_xAF), digits = 4)
# #Home win
# SC3_fixtures_fo$sc3_H <- (
#   SC3_fixtures_fo$sc3_1_0 + SC3_fixtures_fo$sc3_2_0 + SC3_fixtures_fo$sc3_2_1 + SC3_fixtures_fo$sc3_3_0 + SC3_fixtures_fo$sc3_3_1 +
#     SC3_fixtures_fo$sc3_3_2 + SC3_fixtures_fo$sc3_4_0 + SC3_fixtures_fo$sc3_4_1 + SC3_fixtures_fo$sc3_4_2 + SC3_fixtures_fo$sc3_4_3 +
#     SC3_fixtures_fo$sc3_5_0 + SC3_fixtures_fo$sc3_5_1 + SC3_fixtures_fo$sc3_5_2 + SC3_fixtures_fo$sc3_5_3 + SC3_fixtures_fo$sc3_5_4 +
#     SC3_fixtures_fo$sc3_6_0 + SC3_fixtures_fo$sc3_6_1 + SC3_fixtures_fo$sc3_6_2 + SC3_fixtures_fo$sc3_6_3 + SC3_fixtures_fo$sc3_6_4 +
#     SC3_fixtures_fo$sc3_6_5
# )
#
# SC3_fixtures_fo$sc3_H <- percent(SC3_fixtures_fo$sc3_H, accuracy = 0.1)
#
# #Draw
# SC3_fixtures_fo$sc3_D <- (
#
#   SC3_fixtures_fo$sc3_0_0 + SC3_fixtures_fo$sc3_1_1 + SC3_fixtures_fo$sc3_2_2 + SC3_fixtures_fo$sc3_3_3 + SC3_fixtures_fo$sc3_4_4 +
#     SC3_fixtures_fo$sc3_5_5 + SC3_fixtures_fo$sc3_6_6
# )
#
# SC3_fixtures_fo$sc3_D <- percent(SC3_fixtures_fo$sc3_D, accuracy = 0.1)
#
# #Away
#
# SC3_fixtures_fo$sc3_A <- (
#   SC3_fixtures_fo$sc3_0_1 + SC3_fixtures_fo$sc3_0_2 + SC3_fixtures_fo$sc3_1_2 + SC3_fixtures_fo$sc3_0_3 + SC3_fixtures_fo$sc3_1_3 +
#     SC3_fixtures_fo$sc3_2_3 + SC3_fixtures_fo$sc3_0_4 + SC3_fixtures_fo$sc3_1_4 + SC3_fixtures_fo$sc3_2_4 + SC3_fixtures_fo$sc3_3_4 +
#     SC3_fixtures_fo$sc3_0_5 + SC3_fixtures_fo$sc3_1_5 + SC3_fixtures_fo$sc3_2_5 + SC3_fixtures_fo$sc3_3_5 + SC3_fixtures_fo$sc3_4_5 +
#     SC3_fixtures_fo$sc3_0_6 + SC3_fixtures_fo$sc3_1_6 + SC3_fixtures_fo$sc3_2_6 + SC3_fixtures_fo$sc3_3_6 + SC3_fixtures_fo$sc3_4_6 +
#     SC3_fixtures_fo$sc3_5_6
# )
#
# SC3_fixtures_fo$sc3_A <- percent(SC3_fixtures_fo$sc3_A, accuracy = 0.1)
#
# #ov25
# SC3_fixtures_fo$sc3_ov25 <- (
#   SC3_fixtures_fo$sc3_2_1 + SC3_fixtures_fo$sc3_1_2 + SC3_fixtures_fo$sc3_2_2 + SC3_fixtures_fo$sc3_3_0 + SC3_fixtures_fo$sc3_3_1 +
#     SC3_fixtures_fo$sc3_3_2 + SC3_fixtures_fo$sc3_0_3 + SC3_fixtures_fo$sc3_1_3 + SC3_fixtures_fo$sc3_2_3 + SC3_fixtures_fo$sc3_3_3 +
#     SC3_fixtures_fo$sc3_4_0 + SC3_fixtures_fo$sc3_4_1 + SC3_fixtures_fo$sc3_4_2 + SC3_fixtures_fo$sc3_4_3 + SC3_fixtures_fo$sc3_0_4 +
#     SC3_fixtures_fo$sc3_1_4 + SC3_fixtures_fo$sc3_2_4 + SC3_fixtures_fo$sc3_3_4 + SC3_fixtures_fo$sc3_4_4 + SC3_fixtures_fo$sc3_5_0 +
#     SC3_fixtures_fo$sc3_5_1 + SC3_fixtures_fo$sc3_5_2 + SC3_fixtures_fo$sc3_5_3 + SC3_fixtures_fo$sc3_5_4 + SC3_fixtures_fo$sc3_0_5 +
#     SC3_fixtures_fo$sc3_1_5 + SC3_fixtures_fo$sc3_2_5 + SC3_fixtures_fo$sc3_3_5 + SC3_fixtures_fo$sc3_4_5 + SC3_fixtures_fo$sc3_5_5 +
#     SC3_fixtures_fo$sc3_6_0 + SC3_fixtures_fo$sc3_6_1 + SC3_fixtures_fo$sc3_6_2 + SC3_fixtures_fo$sc3_6_3 + SC3_fixtures_fo$sc3_6_4 +
#     SC3_fixtures_fo$sc3_6_5 + SC3_fixtures_fo$sc3_0_6 + SC3_fixtures_fo$sc3_1_6 + SC3_fixtures_fo$sc3_2_6 + SC3_fixtures_fo$sc3_3_6 +
#     SC3_fixtures_fo$sc3_4_6 + SC3_fixtures_fo$sc3_5_6 + SC3_fixtures_fo$sc3_6_6
# )
# #un25
# SC3_fixtures_fo$sc3_un25 <- (
#   SC3_fixtures_fo$sc3_0_0 + SC3_fixtures_fo$sc3_1_0 + SC3_fixtures_fo$sc3_0_1 + SC3_fixtures_fo$sc3_1_1 + SC3_fixtures_fo$sc3_2_0 + SC3_fixtures_fo$sc3_0_2
# )
# #odds
# SC3_fixtures_fo$sc3_ov25_odds <- round((1/SC3_fixtures_fo$sc3_ov25),digits = 2)
# SC3_fixtures_fo$sc3_un25_odds <- round((1/SC3_fixtures_fo$sc3_un25),digits = 2)
#
# SC3_fixtures_fo$sc3_ov25_odds
# SC3_fixtures_fo$sc3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC3_fixtures_fo$sc3_ov25 <- percent(SC3_fixtures_fo$sc3_ov25, accuracy = 0.1)
#
# SC3_fixtures_fo$sc3_un25 <- percent(SC3_fixtures_fo$sc3_un25, accuracy = 0.1)
# SC3_fixtures_fo$sc3_psfore <- paste(round(SC3_fixtures_fo$sc3_xHF,digits = 0),round(SC3_fixtures_fo$sc3_xAF,digits = 0),sep = "-")
# #write.xlsx(SC3_fixtures,'Divisions/SC3.xlsx',sheetName = "SC3", append = TRUE)
# #################################################################################################################
# ################################################################################################################
# #################################################################################################################
# #SP1
# HomeTeam_sp1_fo <- rep(sp1_teams, each = length(sp1_teams))
# AwayTeam_sp1_fo <- rep(sp1_teams, length(sp1_teams))
# SP1_fixtures_fo <- cbind(HomeTeam_sp1_fo,AwayTeam_sp1_fo)
# SP1_fixtures_fo <- as.data.frame(SP1_fixtures_fo)
# SP1_fixtures_fo <- SP1_fixtures_fo[!SP1_fixtures_fo$HomeTeam_sp1_fo == SP1_fixtures_fo$AwayTeam_sp1_fo,]
# rownames(SP1_fixtures_fo) <- NULL
# SP1_fixtures_fo$Div <- "SP1"
# SP1_fixtures_fo <- SP1_fixtures_fo[,c(3,1,2)]
#
# SP1_fixtures_fo$avg_HF_sp1 <- sp1_avg_HF
#
# SP1_fixtures_fo$sp1_homefas <- rep(sp1_home_fas,each = length(sp1_teams)-1)
#
# sp1_awayfds_lookup <- cbind(sp1_teams,sp1_away_fds)
#
# sp1_awayfds_lookup <- as.data.frame(sp1_awayfds_lookup)
#
# colnames(sp1_awayfds_lookup) <- c("AwayTeam_sp1_fo","sp1_awayfds")
#
#
# require('RH2')
# SP1_fixtures_fo$sp1_awayfds <- sqldf("SELECT sp1_awayfds_lookup.sp1_awayfds FROM sp1_awayfds_lookup INNER JOIN SP1_fixtures_fo ON sp1_awayfds_lookup.AwayTeam_sp1_fo = SP1_fixtures_fo.AwayTeam_sp1_fo")
#
# SP1_fixtures_fo$avg_AF_sp1 <- sp1_avg_AF
#
# sp1_awayfas_lookup <- cbind(sp1_teams,sp1_away_fas)
#
# sp1_awayfas_lookup <- as.data.frame(sp1_awayfas_lookup)
#
# colnames(sp1_awayfas_lookup) <- c("AwayTeam_sp1_fo","sp1_awayfas")
#
# SP1_fixtures_fo$sp1_awayfas <- sqldf("SELECT sp1_awayfas_lookup.sp1_awayfas FROM sp1_awayfas_lookup INNER JOIN SP1_fixtures_fo ON sp1_awayfas_lookup.AwayTeam_sp1_fo = SP1_fixtures_fo.AwayTeam_sp1_fo")
#
# SP1_fixtures_fo$sp1_homefds <- rep(sp1_home_fds,each = length(sp1_teams)-1)
#
# SP1_fixtures_fo$sp1_awayfds <- as.numeric(unlist(SP1_fixtures_fo$sp1_awayfds))
# #xGH
# SP1_fixtures_fo$sp1_xHF <- SP1_fixtures_fo$avg_HF_sp1 * SP1_fixtures_fo$sp1_homefas * SP1_fixtures_fo$sp1_awayfds
# #xGA
#
# SP1_fixtures_fo$sp1_awayfas <- as.numeric(unlist(SP1_fixtures_fo$sp1_awayfas))
#
# SP1_fixtures_fo$sp1_xAF <- SP1_fixtures_fo$avg_AF_sp1 * SP1_fixtures_fo$sp1_awayfas * SP1_fixtures_fo$sp1_homefds
#
# SP1_fixtures_fo$sp1_0_0 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_0 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_1 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_1 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_0 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_2 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_2 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_1 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_2 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_3 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_0 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_1 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_2 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_3 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_3 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_3 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_4 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_0 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_1 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_2 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_3 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_4 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_4 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_4 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_4 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_5 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_0 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_1 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_2 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_3 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_4 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_5 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_5 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_5 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_5 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_5 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_6 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_0 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(0,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_1 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(1,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_2 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(2,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_3 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(3,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_4 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(4,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_6_5 <- round(stats::dpois(6,SP1_fixtures_fo$sp1_xHF) * stats::dpois(5,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_0_6 <- round(stats::dpois(0,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_1_6 <- round(stats::dpois(1,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_2_6 <- round(stats::dpois(2,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_3_6 <- round(stats::dpois(3,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_4_6 <- round(stats::dpois(4,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# SP1_fixtures_fo$sp1_5_6 <- round(stats::dpois(5,SP1_fixtures_fo$sp1_xHF) * stats::dpois(6,SP1_fixtures_fo$sp1_xAF), digits = 4)
# #Home win
# SP1_fixtures_fo$sp1_H <- (
#   SP1_fixtures_fo$sp1_1_0 + SP1_fixtures_fo$sp1_2_0 + SP1_fixtures_fo$sp1_2_1 + SP1_fixtures_fo$sp1_3_0 + SP1_fixtures_fo$sp1_3_1 +
#     SP1_fixtures_fo$sp1_3_2 + SP1_fixtures_fo$sp1_4_0 + SP1_fixtures_fo$sp1_4_1 + SP1_fixtures_fo$sp1_4_2 + SP1_fixtures_fo$sp1_4_3 +
#     SP1_fixtures_fo$sp1_5_0 + SP1_fixtures_fo$sp1_5_1 + SP1_fixtures_fo$sp1_5_2 + SP1_fixtures_fo$sp1_5_3 + SP1_fixtures_fo$sp1_5_4 +
#     SP1_fixtures_fo$sp1_6_0 + SP1_fixtures_fo$sp1_6_1 + SP1_fixtures_fo$sp1_6_2 + SP1_fixtures_fo$sp1_6_3 + SP1_fixtures_fo$sp1_6_4 +
#     SP1_fixtures_fo$sp1_6_5
# )
#
# SP1_fixtures_fo$sp1_H <- percent(SP1_fixtures_fo$sp1_H, accuracy = 0.1)
#
# #Draw
# SP1_fixtures_fo$sp1_D <- (
#
#   SP1_fixtures_fo$sp1_0_0 + SP1_fixtures_fo$sp1_1_1 + SP1_fixtures_fo$sp1_2_2 + SP1_fixtures_fo$sp1_3_3 + SP1_fixtures_fo$sp1_4_4 +
#     SP1_fixtures_fo$sp1_5_5 + SP1_fixtures_fo$sp1_6_6
# )
#
# SP1_fixtures_fo$sp1_D <- percent(SP1_fixtures_fo$sp1_D, accuracy = 0.1)
#
# #Away
#
# SP1_fixtures_fo$sp1_A <- (
#   SP1_fixtures_fo$sp1_0_1 + SP1_fixtures_fo$sp1_0_2 + SP1_fixtures_fo$sp1_1_2 + SP1_fixtures_fo$sp1_0_3 + SP1_fixtures_fo$sp1_1_3 +
#     SP1_fixtures_fo$sp1_2_3 + SP1_fixtures_fo$sp1_0_4 + SP1_fixtures_fo$sp1_1_4 + SP1_fixtures_fo$sp1_2_4 + SP1_fixtures_fo$sp1_3_4 +
#     SP1_fixtures_fo$sp1_0_5 + SP1_fixtures_fo$sp1_1_5 + SP1_fixtures_fo$sp1_2_5 + SP1_fixtures_fo$sp1_3_5 + SP1_fixtures_fo$sp1_4_5 +
#     SP1_fixtures_fo$sp1_0_6 + SP1_fixtures_fo$sp1_1_6 + SP1_fixtures_fo$sp1_2_6 + SP1_fixtures_fo$sp1_3_6 + SP1_fixtures_fo$sp1_4_6 +
#     SP1_fixtures_fo$sp1_5_6
# )
#
# SP1_fixtures_fo$sp1_A <- percent(SP1_fixtures_fo$sp1_A, accuracy = 0.1)
#
# #ov25
# SP1_fixtures_fo$sp1_ov25 <- (
#   SP1_fixtures_fo$sp1_2_1 + SP1_fixtures_fo$sp1_1_2 + SP1_fixtures_fo$sp1_2_2 + SP1_fixtures_fo$sp1_3_0 + SP1_fixtures_fo$sp1_3_1 +
#     SP1_fixtures_fo$sp1_3_2 + SP1_fixtures_fo$sp1_0_3 + SP1_fixtures_fo$sp1_1_3 + SP1_fixtures_fo$sp1_2_3 + SP1_fixtures_fo$sp1_3_3 +
#     SP1_fixtures_fo$sp1_4_0 + SP1_fixtures_fo$sp1_4_1 + SP1_fixtures_fo$sp1_4_2 + SP1_fixtures_fo$sp1_4_3 + SP1_fixtures_fo$sp1_0_4 +
#     SP1_fixtures_fo$sp1_1_4 + SP1_fixtures_fo$sp1_2_4 + SP1_fixtures_fo$sp1_3_4 + SP1_fixtures_fo$sp1_4_4 + SP1_fixtures_fo$sp1_5_0 +
#     SP1_fixtures_fo$sp1_5_1 + SP1_fixtures_fo$sp1_5_2 + SP1_fixtures_fo$sp1_5_3 + SP1_fixtures_fo$sp1_5_4 + SP1_fixtures_fo$sp1_0_5 +
#     SP1_fixtures_fo$sp1_1_5 + SP1_fixtures_fo$sp1_2_5 + SP1_fixtures_fo$sp1_3_5 + SP1_fixtures_fo$sp1_4_5 + SP1_fixtures_fo$sp1_5_5 +
#     SP1_fixtures_fo$sp1_6_0 + SP1_fixtures_fo$sp1_6_1 + SP1_fixtures_fo$sp1_6_2 + SP1_fixtures_fo$sp1_6_3 + SP1_fixtures_fo$sp1_6_4 +
#     SP1_fixtures_fo$sp1_6_5 + SP1_fixtures_fo$sp1_0_6 + SP1_fixtures_fo$sp1_1_6 + SP1_fixtures_fo$sp1_2_6 + SP1_fixtures_fo$sp1_3_6 +
#     SP1_fixtures_fo$sp1_4_6 + SP1_fixtures_fo$sp1_5_6 + SP1_fixtures_fo$sp1_6_6
# )
# #un25
# SP1_fixtures_fo$sp1_un25 <- (
#   SP1_fixtures_fo$sp1_0_0 + SP1_fixtures_fo$sp1_1_0 + SP1_fixtures_fo$sp1_0_1 + SP1_fixtures_fo$sp1_1_1 + SP1_fixtures_fo$sp1_2_0 + SP1_fixtures_fo$sp1_0_2
# )
# #odds
# SP1_fixtures_fo$sp1_ov25_odds <- round((1/SP1_fixtures_fo$sp1_ov25),digits = 2)
# SP1_fixtures_fo$sp1_un25_odds <- round((1/SP1_fixtures_fo$sp1_un25),digits = 2)
#
# SP1_fixtures_fo$sp1_ov25_odds
# SP1_fixtures_fo$sp1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP1_fixtures_fo$sp1_ov25 <- percent(SP1_fixtures_fo$sp1_ov25, accuracy = 0.1)
#
# SP1_fixtures_fo$sp1_un25 <- percent(SP1_fixtures_fo$sp1_un25, accuracy = 0.1)
# SP1_fixtures_fo$sp1_psfore <- paste(round(SP1_fixtures_fo$sp1_xHF,digits = 0),round(SP1_fixtures_fo$sp1_xAF,digits = 0),sep = "-")
# #write.xlsx(SP1_fixtures,'Divisions/SP1.xlsx',sheetName = "SP1", append = TRUE)
# #################################################################################################################
# #SP2
# HomeTeam_sp2_fo <- rep(sp2_teams, each = length(sp2_teams))
# AwayTeam_sp2_fo <- rep(sp2_teams, length(sp2_teams))
# SP2_fixtures_fo <- cbind(HomeTeam_sp2_fo,AwayTeam_sp2_fo)
# SP2_fixtures_fo <- as.data.frame(SP2_fixtures_fo)
# SP2_fixtures_fo <- SP2_fixtures_fo[!SP2_fixtures_fo$HomeTeam_sp2_fo == SP2_fixtures_fo$AwayTeam_sp2_fo,]
# rownames(SP2_fixtures_fo) <- NULL
# SP2_fixtures_fo$Div <- "SP2"
# SP2_fixtures_fo <- SP2_fixtures_fo[,c(3,1,2)]
#
# SP2_fixtures_fo$avg_HF_sp2 <- sp2_avg_HF
#
# SP2_fixtures_fo$sp2_homefas <- rep(sp2_home_fas,each = length(sp2_teams)-1)
#
# sp2_awayfds_lookup <- cbind(sp2_teams,sp2_away_fds)
#
# sp2_awayfds_lookup <- as.data.frame(sp2_awayfds_lookup)
#
# colnames(sp2_awayfds_lookup) <- c("AwayTeam_sp2_fo","sp2_awayfds")
#
#
# require('RH2')
# SP2_fixtures_fo$sp2_awayfds <- sqldf("SELECT sp2_awayfds_lookup.sp2_awayfds FROM sp2_awayfds_lookup INNER JOIN SP2_fixtures_fo ON sp2_awayfds_lookup.AwayTeam_sp2_fo = SP2_fixtures_fo.AwayTeam_sp2_fo")
#
# SP2_fixtures_fo$avg_AF_sp2 <- sp2_avg_AF
#
# sp2_awayfas_lookup <- cbind(sp2_teams,sp2_away_fas)
#
# sp2_awayfas_lookup <- as.data.frame(sp2_awayfas_lookup)
#
# colnames(sp2_awayfas_lookup) <- c("AwayTeam_sp2_fo","sp2_awayfas")
#
# SP2_fixtures_fo$sp2_awayfas <- sqldf("SELECT sp2_awayfas_lookup.sp2_awayfas FROM sp2_awayfas_lookup INNER JOIN SP2_fixtures_fo ON sp2_awayfas_lookup.AwayTeam_sp2_fo = SP2_fixtures_fo.AwayTeam_sp2_fo")
#
# SP2_fixtures_fo$sp2_homefds <- rep(sp2_home_fds,each = length(sp2_teams)-1)
#
# SP2_fixtures_fo$sp2_awayfds <- as.numeric(unlist(SP2_fixtures_fo$sp2_awayfds))
# #xGH
# SP2_fixtures_fo$sp2_xHF <- SP2_fixtures_fo$avg_HF_sp2 * SP2_fixtures_fo$sp2_homefas * SP2_fixtures_fo$sp2_awayfds
# #xGA
#
# SP2_fixtures_fo$sp2_awayfas <- as.numeric(unlist(SP2_fixtures_fo$sp2_awayfas))
#
# SP2_fixtures_fo$sp2_xAF <- SP2_fixtures_fo$avg_AF_sp2 * SP2_fixtures_fo$sp2_awayfas * SP2_fixtures_fo$sp2_homefds
#
# SP2_fixtures_fo$sp2_0_0 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_0 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_1 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_1 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_0 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_2 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_2 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_1 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_2 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_3 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_0 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_1 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_2 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_3 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_3 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_3 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_4 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_0 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_1 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_2 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_3 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_4 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_4 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_4 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_4 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_5 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_0 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_1 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_2 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_3 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_4 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_5 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_5 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_5 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_5 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_5 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_6 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_0 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(0,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_1 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(1,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_2 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(2,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_3 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(3,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_4 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(4,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_6_5 <- round(stats::dpois(6,SP2_fixtures_fo$sp2_xHF) * stats::dpois(5,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_0_6 <- round(stats::dpois(0,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_1_6 <- round(stats::dpois(1,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_2_6 <- round(stats::dpois(2,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_3_6 <- round(stats::dpois(3,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_4_6 <- round(stats::dpois(4,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# SP2_fixtures_fo$sp2_5_6 <- round(stats::dpois(5,SP2_fixtures_fo$sp2_xHF) * stats::dpois(6,SP2_fixtures_fo$sp2_xAF), digits = 4)
# #Home win
# SP2_fixtures_fo$sp2_H <- (
#   SP2_fixtures_fo$sp2_1_0 + SP2_fixtures_fo$sp2_2_0 + SP2_fixtures_fo$sp2_2_1 + SP2_fixtures_fo$sp2_3_0 + SP2_fixtures_fo$sp2_3_1 +
#     SP2_fixtures_fo$sp2_3_2 + SP2_fixtures_fo$sp2_4_0 + SP2_fixtures_fo$sp2_4_1 + SP2_fixtures_fo$sp2_4_2 + SP2_fixtures_fo$sp2_4_3 +
#     SP2_fixtures_fo$sp2_5_0 + SP2_fixtures_fo$sp2_5_1 + SP2_fixtures_fo$sp2_5_2 + SP2_fixtures_fo$sp2_5_3 + SP2_fixtures_fo$sp2_5_4 +
#     SP2_fixtures_fo$sp2_6_0 + SP2_fixtures_fo$sp2_6_1 + SP2_fixtures_fo$sp2_6_2 + SP2_fixtures_fo$sp2_6_3 + SP2_fixtures_fo$sp2_6_4 +
#     SP2_fixtures_fo$sp2_6_5
# )
#
# SP2_fixtures_fo$sp2_H <- percent(SP2_fixtures_fo$sp2_H, accuracy = 0.1)
#
# #Draw
# SP2_fixtures_fo$sp2_D <- (
#
#   SP2_fixtures_fo$sp2_0_0 + SP2_fixtures_fo$sp2_1_1 + SP2_fixtures_fo$sp2_2_2 + SP2_fixtures_fo$sp2_3_3 + SP2_fixtures_fo$sp2_4_4 +
#     SP2_fixtures_fo$sp2_5_5 + SP2_fixtures_fo$sp2_6_6
# )
#
# SP2_fixtures_fo$sp2_D <- percent(SP2_fixtures_fo$sp2_D, accuracy = 0.1)
#
# #Away
#
# SP2_fixtures_fo$sp2_A <- (
#   SP2_fixtures_fo$sp2_0_1 + SP2_fixtures_fo$sp2_0_2 + SP2_fixtures_fo$sp2_1_2 + SP2_fixtures_fo$sp2_0_3 + SP2_fixtures_fo$sp2_1_3 +
#     SP2_fixtures_fo$sp2_2_3 + SP2_fixtures_fo$sp2_0_4 + SP2_fixtures_fo$sp2_1_4 + SP2_fixtures_fo$sp2_2_4 + SP2_fixtures_fo$sp2_3_4 +
#     SP2_fixtures_fo$sp2_0_5 + SP2_fixtures_fo$sp2_1_5 + SP2_fixtures_fo$sp2_2_5 + SP2_fixtures_fo$sp2_3_5 + SP2_fixtures_fo$sp2_4_5 +
#     SP2_fixtures_fo$sp2_0_6 + SP2_fixtures_fo$sp2_1_6 + SP2_fixtures_fo$sp2_2_6 + SP2_fixtures_fo$sp2_3_6 + SP2_fixtures_fo$sp2_4_6 +
#     SP2_fixtures_fo$sp2_5_6
# )
#
# SP2_fixtures_fo$sp2_A <- percent(SP2_fixtures_fo$sp2_A, accuracy = 0.1)
#
# #ov25
# SP2_fixtures_fo$sp2_ov25 <- (
#   SP2_fixtures_fo$sp2_2_1 + SP2_fixtures_fo$sp2_1_2 + SP2_fixtures_fo$sp2_2_2 + SP2_fixtures_fo$sp2_3_0 + SP2_fixtures_fo$sp2_3_1 +
#     SP2_fixtures_fo$sp2_3_2 + SP2_fixtures_fo$sp2_0_3 + SP2_fixtures_fo$sp2_1_3 + SP2_fixtures_fo$sp2_2_3 + SP2_fixtures_fo$sp2_3_3 +
#     SP2_fixtures_fo$sp2_4_0 + SP2_fixtures_fo$sp2_4_1 + SP2_fixtures_fo$sp2_4_2 + SP2_fixtures_fo$sp2_4_3 + SP2_fixtures_fo$sp2_0_4 +
#     SP2_fixtures_fo$sp2_1_4 + SP2_fixtures_fo$sp2_2_4 + SP2_fixtures_fo$sp2_3_4 + SP2_fixtures_fo$sp2_4_4 + SP2_fixtures_fo$sp2_5_0 +
#     SP2_fixtures_fo$sp2_5_1 + SP2_fixtures_fo$sp2_5_2 + SP2_fixtures_fo$sp2_5_3 + SP2_fixtures_fo$sp2_5_4 + SP2_fixtures_fo$sp2_0_5 +
#     SP2_fixtures_fo$sp2_1_5 + SP2_fixtures_fo$sp2_2_5 + SP2_fixtures_fo$sp2_3_5 + SP2_fixtures_fo$sp2_4_5 + SP2_fixtures_fo$sp2_5_5 +
#     SP2_fixtures_fo$sp2_6_0 + SP2_fixtures_fo$sp2_6_1 + SP2_fixtures_fo$sp2_6_2 + SP2_fixtures_fo$sp2_6_3 + SP2_fixtures_fo$sp2_6_4 +
#     SP2_fixtures_fo$sp2_6_5 + SP2_fixtures_fo$sp2_0_6 + SP2_fixtures_fo$sp2_1_6 + SP2_fixtures_fo$sp2_2_6 + SP2_fixtures_fo$sp2_3_6 +
#     SP2_fixtures_fo$sp2_4_6 + SP2_fixtures_fo$sp2_5_6 + SP2_fixtures_fo$sp2_6_6
# )
# #un25
# SP2_fixtures_fo$sp2_un25 <- (
#   SP2_fixtures_fo$sp2_0_0 + SP2_fixtures_fo$sp2_1_0 + SP2_fixtures_fo$sp2_0_1 + SP2_fixtures_fo$sp2_1_1 + SP2_fixtures_fo$sp2_2_0 + SP2_fixtures_fo$sp2_0_2
# )
# #odds
# SP2_fixtures_fo$sp2_ov25_odds <- round((1/SP2_fixtures_fo$sp2_ov25),digits = 2)
# SP2_fixtures_fo$sp2_un25_odds <- round((1/SP2_fixtures_fo$sp2_un25),digits = 2)
#
# SP2_fixtures_fo$sp2_ov25_odds
# SP2_fixtures_fo$sp2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP2_fixtures_fo$sp2_ov25 <- percent(SP2_fixtures_fo$sp2_ov25, accuracy = 0.1)
#
# SP2_fixtures_fo$sp2_un25 <- percent(SP2_fixtures_fo$sp2_un25, accuracy = 0.1)
# SP2_fixtures_fo$sp2_psfore <- paste(round(SP2_fixtures_fo$sp2_xHF,digits = 0),round(SP2_fixtures_fo$sp2_xAF,digits = 0),sep = "-")
# #write out
# #write.xlsx(SP2_fixtures,'Divisions/SP2.xlsx',sheetName = "SP2", append = TRUE)
# #################################################################################################################
# #T1
# HomeTeam_t1_fo <- rep(t1_teams, each = length(t1_teams))
# AwayTeam_t1_fo <- rep(t1_teams, length(t1_teams))
# T1_fixtures_fo <- cbind(HomeTeam_t1_fo,AwayTeam_t1_fo)
# T1_fixtures_fo <- as.data.frame(T1_fixtures_fo)
# T1_fixtures_fo <- T1_fixtures_fo[!T1_fixtures_fo$HomeTeam_t1_fo == T1_fixtures_fo$AwayTeam_t1_fo,]
# rownames(T1_fixtures_fo) <- NULL
# T1_fixtures_fo$Div <- "T1"
# T1_fixtures_fo <- T1_fixtures_fo[,c(3,1,2)]
#
# T1_fixtures_fo$avg_HF_t1 <- t1_avg_HF
#
# T1_fixtures_fo$t1_homefas <- rep(t1_home_fas,each = length(t1_teams)-1)
#
# t1_awayfds_lookup <- cbind(t1_teams,t1_away_fds)
#
# t1_awayfds_lookup <- as.data.frame(t1_awayfds_lookup)
#
# colnames(t1_awayfds_lookup) <- c("AwayTeam_t1_fo","t1_awayfds")
#
#
# require('RH2')
# T1_fixtures_fo$t1_awayfds <- sqldf("SELECT t1_awayfds_lookup.t1_awayfds FROM t1_awayfds_lookup INNER JOIN T1_fixtures_fo ON t1_awayfds_lookup.AwayTeam_t1_fo = T1_fixtures_fo.AwayTeam_t1_fo")
#
# T1_fixtures_fo$avg_AF_t1 <- t1_avg_AF
#
# t1_awayfas_lookup <- cbind(t1_teams,t1_away_fas)
#
# t1_awayfas_lookup <- as.data.frame(t1_awayfas_lookup)
#
# colnames(t1_awayfas_lookup) <- c("AwayTeam_t1_fo","t1_awayfas")
#
# T1_fixtures_fo$t1_awayfas <- sqldf("SELECT t1_awayfas_lookup.t1_awayfas FROM t1_awayfas_lookup INNER JOIN T1_fixtures_fo ON t1_awayfas_lookup.AwayTeam_t1_fo = T1_fixtures_fo.AwayTeam_t1_fo")
#
# T1_fixtures_fo$t1_homefds <- rep(t1_home_fds,each = length(t1_teams)-1)
#
# T1_fixtures_fo$t1_awayfds <- as.numeric(unlist(T1_fixtures_fo$t1_awayfds))
# #xGH
# T1_fixtures_fo$t1_xHF <- T1_fixtures_fo$avg_HF_t1 * T1_fixtures_fo$t1_homefas * T1_fixtures_fo$t1_awayfds
# #xGA
#
# T1_fixtures_fo$t1_awayfas <- as.numeric(unlist(T1_fixtures_fo$t1_awayfas))
#
# T1_fixtures_fo$t1_xAF <- T1_fixtures_fo$avg_AF_t1 * T1_fixtures_fo$t1_awayfas * T1_fixtures_fo$t1_homefds
#
# T1_fixtures_fo$t1_0_0 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_0 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_1 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_1 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_0 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_2 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_2 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_1 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_2 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_3 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_0 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_1 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_2 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_3 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_3 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_3 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_4 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_0 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_1 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_2 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_3 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_4 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_4 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_4 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_4 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_5 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_0 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_1 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_2 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_3 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_4 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_5 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_5 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_5 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_5 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_5 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_6 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_0 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(0,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_1 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(1,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_2 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(2,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_3 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(3,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_4 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(4,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_6_5 <- round(stats::dpois(6,T1_fixtures_fo$t1_xHF) * stats::dpois(5,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_0_6 <- round(stats::dpois(0,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_1_6 <- round(stats::dpois(1,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_2_6 <- round(stats::dpois(2,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_3_6 <- round(stats::dpois(3,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_4_6 <- round(stats::dpois(4,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# T1_fixtures_fo$t1_5_6 <- round(stats::dpois(5,T1_fixtures_fo$t1_xHF) * stats::dpois(6,T1_fixtures_fo$t1_xAF), digits = 4)
# #Home win
# T1_fixtures_fo$t1_H <- (
#   T1_fixtures_fo$t1_1_0 + T1_fixtures_fo$t1_2_0 + T1_fixtures_fo$t1_2_1 + T1_fixtures_fo$t1_3_0 + T1_fixtures_fo$t1_3_1 +
#     T1_fixtures_fo$t1_3_2 + T1_fixtures_fo$t1_4_0 + T1_fixtures_fo$t1_4_1 + T1_fixtures_fo$t1_4_2 + T1_fixtures_fo$t1_4_3 +
#     T1_fixtures_fo$t1_5_0 + T1_fixtures_fo$t1_5_1 + T1_fixtures_fo$t1_5_2 + T1_fixtures_fo$t1_5_3 + T1_fixtures_fo$t1_5_4 +
#     T1_fixtures_fo$t1_6_0 + T1_fixtures_fo$t1_6_1 + T1_fixtures_fo$t1_6_2 + T1_fixtures_fo$t1_6_3 + T1_fixtures_fo$t1_6_4 +
#     T1_fixtures_fo$t1_6_5
# )
#
# T1_fixtures_fo$t1_H <- percent(T1_fixtures_fo$t1_H, accuracy = 0.1)
#
# #Draw
# T1_fixtures_fo$t1_D <- (
#
#   T1_fixtures_fo$t1_0_0 + T1_fixtures_fo$t1_1_1 + T1_fixtures_fo$t1_2_2 + T1_fixtures_fo$t1_3_3 + T1_fixtures_fo$t1_4_4 +
#     T1_fixtures_fo$t1_5_5 + T1_fixtures_fo$t1_6_6
# )
#
# T1_fixtures_fo$t1_D <- percent(T1_fixtures_fo$t1_D, accuracy = 0.1)
#
# #Away
#
# T1_fixtures_fo$t1_A <- (
#   T1_fixtures_fo$t1_0_1 + T1_fixtures_fo$t1_0_2 + T1_fixtures_fo$t1_1_2 + T1_fixtures_fo$t1_0_3 + T1_fixtures_fo$t1_1_3 +
#     T1_fixtures_fo$t1_2_3 + T1_fixtures_fo$t1_0_4 + T1_fixtures_fo$t1_1_4 + T1_fixtures_fo$t1_2_4 + T1_fixtures_fo$t1_3_4 +
#     T1_fixtures_fo$t1_0_5 + T1_fixtures_fo$t1_1_5 + T1_fixtures_fo$t1_2_5 + T1_fixtures_fo$t1_3_5 + T1_fixtures_fo$t1_4_5 +
#     T1_fixtures_fo$t1_0_6 + T1_fixtures_fo$t1_1_6 + T1_fixtures_fo$t1_2_6 + T1_fixtures_fo$t1_3_6 + T1_fixtures_fo$t1_4_6 +
#     T1_fixtures_fo$t1_5_6
# )
#
# T1_fixtures_fo$t1_A <- percent(T1_fixtures_fo$t1_A, accuracy = 0.1)
#
# #ov25
# T1_fixtures_fo$t1_ov25 <- (
#   T1_fixtures_fo$t1_2_1 + T1_fixtures_fo$t1_1_2 + T1_fixtures_fo$t1_2_2 + T1_fixtures_fo$t1_3_0 + T1_fixtures_fo$t1_3_1 +
#     T1_fixtures_fo$t1_3_2 + T1_fixtures_fo$t1_0_3 + T1_fixtures_fo$t1_1_3 + T1_fixtures_fo$t1_2_3 + T1_fixtures_fo$t1_3_3 +
#     T1_fixtures_fo$t1_4_0 + T1_fixtures_fo$t1_4_1 + T1_fixtures_fo$t1_4_2 + T1_fixtures_fo$t1_4_3 + T1_fixtures_fo$t1_0_4 +
#     T1_fixtures_fo$t1_1_4 + T1_fixtures_fo$t1_2_4 + T1_fixtures_fo$t1_3_4 + T1_fixtures_fo$t1_4_4 + T1_fixtures_fo$t1_5_0 +
#     T1_fixtures_fo$t1_5_1 + T1_fixtures_fo$t1_5_2 + T1_fixtures_fo$t1_5_3 + T1_fixtures_fo$t1_5_4 + T1_fixtures_fo$t1_0_5 +
#     T1_fixtures_fo$t1_1_5 + T1_fixtures_fo$t1_2_5 + T1_fixtures_fo$t1_3_5 + T1_fixtures_fo$t1_4_5 + T1_fixtures_fo$t1_5_5 +
#     T1_fixtures_fo$t1_6_0 + T1_fixtures_fo$t1_6_1 + T1_fixtures_fo$t1_6_2 + T1_fixtures_fo$t1_6_3 + T1_fixtures_fo$t1_6_4 +
#     T1_fixtures_fo$t1_6_5 + T1_fixtures_fo$t1_0_6 + T1_fixtures_fo$t1_1_6 + T1_fixtures_fo$t1_2_6 + T1_fixtures_fo$t1_3_6 +
#     T1_fixtures_fo$t1_4_6 + T1_fixtures_fo$t1_5_6 + T1_fixtures_fo$t1_6_6
# )
# #un25
# T1_fixtures_fo$t1_un25 <- (
#   T1_fixtures_fo$t1_0_0 + T1_fixtures_fo$t1_1_0 + T1_fixtures_fo$t1_0_1 + T1_fixtures_fo$t1_1_1 + T1_fixtures_fo$t1_2_0 + T1_fixtures_fo$t1_0_2
# )
# #odds
# T1_fixtures_fo$t1_ov25_odds <- round((1/T1_fixtures_fo$t1_ov25),digits = 2)
# T1_fixtures_fo$t1_un25_odds <- round((1/T1_fixtures_fo$t1_un25),digits = 2)
#
# T1_fixtures_fo$t1_ov25_odds
# T1_fixtures_fo$t1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# T1_fixtures_fo$t1_ov25 <- percent(T1_fixtures_fo$t1_ov25, accuracy = 0.1)
#
# T1_fixtures_fo$t1_un25 <- percent(T1_fixtures_fo$t1_un25, accuracy = 0.1)
# T1_fixtures_fo$t1_psfore <- paste(round(T1_fixtures_fo$t1_xHF,digits = 0),round(T1_fixtures_fo$t1_xAF,digits = 0),sep = "-")
#

















