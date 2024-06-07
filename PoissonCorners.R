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

#Calculate total home corners for each division
mls_home_corners <- aggregate(MLS$HCO, by = list(MLS$HomeTeam), FUN = sum)
mls_away_corners <- aggregate(MLS$ACO, by = list(MLS$AwayTeam), FUN = sum)
arg_home_corners <- aggregate(ARG$HCO, by = list(ARG$HomeTeam), FUN = sum)
arg_away_corners <- aggregate(ARG$ACO, by = list(ARG$AwayTeam), FUN = sum)
# d2_home_corners <- aggregate(D2$HCO, by = list(D2$HomeTeam), FUN = sum)
# d2_away_corners <- aggregate(D2$ACO, by = list(D2$AwayTeam), FUN = sum)
# e0_home_corners <- aggregate(E0$HCO, by = list(E0$HomeTeam), FUN = sum)
# e0_away_corners <- aggregate(E0$ACO, by = list(E0$AwayTeam), FUN = sum)
# e1_home_corners <- aggregate(E1$HCO, by = list(E1$HomeTeam), FUN = sum)
# e1_away_corners <- aggregate(E1$ACO, by = list(E1$AwayTeam), FUN = sum)
# e2_home_corners <- aggregate(E2$HCO, by = list(E2$HomeTeam), FUN = sum)
# e2_away_corners <- aggregate(E2$ACO, by = list(E2$AwayTeam), FUN = sum)
# e3_home_corners <- aggregate(E3$HCO, by = list(E3$HomeTeam), FUN = sum)
# e3_away_corners <- aggregate(E3$ACO, by = list(E3$AwayTeam), FUN = sum)
# ec_home_corners <- aggregate(EC$HCO, by = list(EC$HomeTeam), FUN = sum)
# ec_away_corners <- aggregate(EC$ACO, by = list(EC$AwayTeam), FUN = sum)
# f1_home_corners <- aggregate(F1$HCO, by = list(F1$HomeTeam), FUN = sum)
# f1_away_corners <- aggregate(F1$ACO, by = list(F1$AwayTeam), FUN = sum)
# f2_home_corners <- aggregate(F2$HCO, by = list(F2$HomeTeam), FUN = sum)
# f2_away_corners <- aggregate(F2$ACO, by = list(F2$AwayTeam), FUN = sum)
# g1_home_corners <- aggregate(G1$HCO, by = list(G1$HomeTeam), FUN = sum)
# g1_away_corners <- aggregate(G1$ACO, by = list(G1$AwayTeam), FUN = sum)
# i1_home_corners <- aggregate(I1$HCO, by = list(I1$HomeTeam), FUN = sum)
# i1_away_corners <- aggregate(I1$ACO, by = list(I1$AwayTeam), FUN = sum)
# i2_home_corners <- aggregate(I2$HCO, by = list(I2$HomeTeam), FUN = sum)
# i2_away_corners <- aggregate(I2$ACO, by = list(I2$AwayTeam), FUN = sum)
# n1_home_corners <- aggregate(N1$HCO, by = list(N1$HomeTeam), FUN = sum)
# n1_away_corners <- aggregate(N1$ACO, by = list(N1$AwayTeam), FUN = sum)
# p1_home_corners <- aggregate(P1$HCO, by = list(P1$HomeTeam), FUN = sum)
# p1_away_corners <- aggregate(P1$ACO, by = list(P1$AwayTeam), FUN = sum)
# sc0_home_corners <- aggregate(SC0$HCO, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_corners <- aggregate(SC0$ACO, by = list(SC0$AwayTeam), FUN = sum)
# sc1_home_corners <- aggregate(SC1$HCO, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_corners <- aggregate(SC1$ACO, by = list(SC1$AwayTeam), FUN = sum)
# sc2_home_corners <- aggregate(SC2$HCO, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_corners <- aggregate(SC2$ACO, by = list(SC2$AwayTeam), FUN = sum)
# sc3_home_corners <- aggregate(SC3$HCO, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_corners <- aggregate(SC3$ACO, by = list(SC3$AwayTeam), FUN = sum)
# sp1_home_corners <- aggregate(SP1$HCO, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_corners <- aggregate(SP1$ACO, by = list(SP1$AwayTeam), FUN = sum)
# sp2_home_corners <- aggregate(SP2$HCO, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_corners <- aggregate(SP2$ACO, by = list(SP2$AwayTeam), FUN = sum)
# t1_home_corners <- aggregate(T1$HCO, by = list(T1$HomeTeam), FUN = sum)
# t1_away_corners <- aggregate(T1$ACO, by = list(T1$AwayTeam), FUN = sum)
###############################################################################
mls_T_HCO <- sum(mls_home_corners$x)
arg_T_HCO <- sum(arg_home_corners$x)
# d2_T_HCO <- sum(d2_home_corners$x)
# e0_T_HCO <- sum(e0_home_corners$x)
# e1_T_HCO <- sum(e1_home_corners$x)
# e2_T_HCO <- sum(e2_home_corners$x)
# e3_T_HCO <- sum(e3_home_corners$x)
# ec_T_HCO <- sum(ec_home_corners$x)
# f1_T_HCO <- sum(f1_home_corners$x)
# f2_T_HCO <- sum(f2_home_corners$x)
# g1_T_HCO <- sum(g1_home_corners$x)
# i1_T_HCO <- sum(i1_home_corners$x)
# i2_T_HCO <- sum(i2_home_corners$x)
# n1_T_HCO <- sum(n1_home_corners$x)
# p1_T_HCO <- sum(p1_home_corners$x)
# sc0_T_HCO <- sum(sc0_home_corners$x)
# sc1_T_HCO <- sum(sc1_home_corners$x)
# sc2_T_HCO <- sum(sc2_home_corners$x)
# sc3_T_HCO <- sum(sc3_home_corners$x)
# sp1_T_HCO <- sum(sp1_home_corners$x)
# sp2_T_HCO <- sum(sp2_home_corners$x)
# t1_T_HCO <- sum(t1_home_corners$x)
#calculate average home corners

mls_avg_HCO <- round(mls_T_HCO /mls_GP, digits = 4)
arg_avg_HCO <- round(arg_T_HCO /arg_GP, digits = 4)
# d2_avg_HCO <- round(d2_T_HCO /d2_GP, digits = 4)
# e0_avg_HCO <- round(e0_T_HCO /e0_GP, digits = 4)
# e1_avg_HCO <- round(e1_T_HCO /e1_GP, digits = 4)
# e2_avg_HCO <- round(e2_T_HCO /e2_GP, digits = 4)
# e3_avg_HCO <- round(e3_T_HCO /e3_GP, digits = 4)
# ec_avg_HCO <- round(ec_T_HCO /ec_GP, digits = 4)
# f1_avg_HCO <- round(f1_T_HCO /f1_GP, digits = 4)
# f2_avg_HCO <- round(f2_T_HCO /f2_GP, digits = 4)
# g1_avg_HCO <- round(g1_T_HCO /g1_GP, digits = 4)
# i1_avg_HCO <- round(i1_T_HCO /i1_GP, digits = 4)
# i2_avg_HCO <- round(i2_T_HCO /i2_GP, digits = 4)
# n1_avg_HCO <- round(n1_T_HCO /n1_GP, digits = 4)
# p1_avg_HCO <- round(p1_T_HCO /p1_GP, digits = 4)
# sc0_avg_HCO <- round(sc0_T_HCO /sc0_GP, digits = 4)
# sc1_avg_HCO <- round(sc1_T_HCO /sc1_GP, digits = 4)
# sc2_avg_HCO <- round(sc2_T_HCO /sc2_GP, digits = 4)
# sc3_avg_HCO <- round(sc3_T_HCO /sc3_GP, digits = 4)
# sp1_avg_HCO <- round(sp1_T_HCO /sp1_GP, digits = 4)
# sp2_avg_HCO <- round(sp2_T_HCO /sp2_GP, digits = 4)
# t1_avg_HCO <- round(t1_T_HCO /t1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_ACO <- sum(mls_away_corners$x)
arg_T_ACO <- sum(arg_away_corners$x)
# d2_T_ACO <- sum(d2_away_corners$x)
# e0_T_ACO <- sum(e0_away_corners$x)
# e1_T_ACO <- sum(e1_away_corners$x)
# e2_T_ACO <- sum(e2_away_corners$x)
# e3_T_ACO <- sum(e3_away_corners$x)
# ec_T_ACO <- sum(ec_away_corners$x)
# f1_T_ACO <- sum(f1_away_corners$x)
# f2_T_ACO <- sum(f2_away_corners$x)
# g1_T_ACO <- sum(g1_away_corners$x)
# i1_T_ACO <- sum(i1_away_corners$x)
# i2_T_ACO <- sum(i2_away_corners$x)
# n1_T_ACO <- sum(n1_away_corners$x)
# p1_T_ACO <- sum(p1_away_corners$x)
# sc0_T_ACO <- sum(sc0_away_corners$x)
# sc1_T_ACO <- sum(sc1_away_corners$x)
# sc2_T_ACO <- sum(sc2_away_corners$x)
# sc3_T_ACO <- sum(sc3_away_corners$x)
# sp1_T_ACO <- sum(sp1_away_corners$x)
# sp2_T_ACO <- sum(sp2_away_corners$x)
# t1_T_ACO <- sum(t1_away_corners$x)
#calculate average away goal

mls_avg_ACO <- round(mls_T_ACO /mls_GP, digits = 4)
arg_avg_ACO <- round(arg_T_ACO /arg_GP, digits = 4)
# d2_avg_ACO <- round(d2_T_ACO /d2_GP, digits = 4)
# e0_avg_ACO <- round(e0_T_ACO /e0_GP, digits = 4)
# e1_avg_ACO <- round(e1_T_ACO /e1_GP, digits = 4)
# e2_avg_ACO <- round(e2_T_ACO /e2_GP, digits = 4)
# e3_avg_ACO <- round(e3_T_ACO /e3_GP, digits = 4)
# ec_avg_ACO <- round(ec_T_ACO /ec_GP, digits = 4)
# f1_avg_ACO <- round(f1_T_ACO /f1_GP, digits = 4)
# f2_avg_ACO <- round(f2_T_ACO /f2_GP, digits = 4)
# g1_avg_ACO <- round(g1_T_ACO /g1_GP, digits = 4)
# i1_avg_ACO <- round(i1_T_ACO /i1_GP, digits = 4)
# i2_avg_ACO <- round(i2_T_ACO /i2_GP, digits = 4)
# n1_avg_ACO <- round(n1_T_ACO /n1_GP, digits = 4)
# p1_avg_ACO <- round(p1_T_ACO /p1_GP, digits = 4)
# sc0_avg_ACO <- round(sc0_T_ACO /sc0_GP, digits = 4)
# sc1_avg_ACO <- round(sc1_T_ACO /sc1_GP, digits = 4)
# sc2_avg_ACO <- round(sc2_T_ACO /sc2_GP, digits = 4)
# sc3_avg_ACO <- round(sc3_T_ACO /sc3_GP, digits = 4)
# sp1_avg_ACO <- round(sp1_T_ACO /sp1_GP, digits = 4)
# sp2_avg_ACO <- round(sp2_T_ACO /sp2_GP, digits = 4)
# t1_avg_ACO <- round(t1_T_ACO /t1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength

mls_home_coas <- round(((mls_home_corners$x/mls_home_games))/mls_avg_HCO, digits = 4)
arg_home_coas <- round(((arg_home_corners$x/arg_home_games))/arg_avg_HCO, digits = 4)
# d2_home_coas <- round(((d2_home_corners$x/d2_home_games))/d2_avg_HCO, digits = 4)
# e0_home_coas <- round(((e0_home_corners$x/e0_home_games))/e0_avg_HCO, digits = 4)
# e1_home_coas <- round(((e1_home_corners$x/e1_home_games))/e1_avg_HCO, digits = 4)
# e2_home_coas <- round(((e2_home_corners$x/e2_home_games))/e2_avg_HCO, digits = 4)
# e3_home_coas <- round(((e3_home_corners$x/e3_home_games))/e3_avg_HCO, digits = 4)
# ec_home_coas <- round(((ec_home_corners$x/ec_home_games))/ec_avg_HCO, digits = 4)
# f1_home_coas <- round(((f1_home_corners$x/f1_home_games))/f1_avg_HCO, digits = 4)
# f2_home_coas <- round(((f2_home_corners$x/f2_home_games))/f2_avg_HCO, digits = 4)
# g1_home_coas <- round(((g1_home_corners$x/g1_home_games))/g1_avg_HCO, digits = 4)
# i1_home_coas <- round(((i1_home_corners$x/i1_home_games))/i1_avg_HCO, digits = 4)
# i2_home_coas <- round(((i2_home_corners$x/i2_home_games))/i2_avg_HCO, digits = 4)
# n1_home_coas <- round(((n1_home_corners$x/n1_home_games))/n1_avg_HCO, digits = 4)
# p1_home_coas <- round(((p1_home_corners$x/p1_home_games))/p1_avg_HCO, digits = 4)
# sc0_home_coas <- round(((sc0_home_corners$x/sc0_home_games))/sc0_avg_HCO, digits = 4)
# sc1_home_coas <- round(((sc1_home_corners$x/sc1_home_games))/sc1_avg_HCO, digits = 4)
# sc2_home_coas <- round(((sc2_home_corners$x/sc2_home_games))/sc2_avg_HCO, digits = 4)
# sc3_home_coas <- round(((sc3_home_corners$x/sc3_home_games))/sc3_avg_HCO, digits = 4)
# sp1_home_coas <- round(((sp1_home_corners$x/sp1_home_games))/sp1_avg_HCO, digits = 4)
# sp2_home_coas <- round(((sp2_home_corners$x/sp2_home_games))/sp2_avg_HCO, digits = 4)
# t1_home_coas <- round(((t1_home_corners$x/t1_home_games))/t1_avg_HCO, digits = 4)
#calculate away attack strength
mls_away_coas <- round(((mls_away_corners$x/mls_away_games))/mls_avg_ACO, digits = 4)
arg_away_coas <- round(((arg_away_corners$x/arg_away_games))/arg_avg_ACO, digits = 4)
# d2_away_coas <- round(((d2_away_corners$x/d2_away_games))/d2_avg_ACO, digits = 4)
# e0_away_coas <- round(((e0_away_corners$x/e0_away_games))/e0_avg_ACO, digits = 4)
# e1_away_coas <- round(((e1_away_corners$x/e1_away_games))/e1_avg_ACO, digits = 4)
# e2_away_coas <- round(((e2_away_corners$x/e2_away_games))/e2_avg_ACO, digits = 4)
# e3_away_coas <- round(((e3_away_corners$x/e3_away_games))/e3_avg_ACO, digits = 4)
# ec_away_coas <- round(((ec_away_corners$x/ec_away_games))/ec_avg_ACO, digits = 4)
# f1_away_coas <- round(((f1_away_corners$x/f1_away_games))/f1_avg_ACO, digits = 4)
# f2_away_coas <- round(((f2_away_corners$x/f2_away_games))/f2_avg_ACO, digits = 4)
# g1_away_coas <- round(((g1_away_corners$x/g1_away_games))/g1_avg_ACO, digits = 4)
# i1_away_coas <- round(((i1_away_corners$x/i1_away_games))/i1_avg_ACO, digits = 4)
# i2_away_coas <- round(((i2_away_corners$x/i2_away_games))/i2_avg_ACO, digits = 4)
# n1_away_coas <- round(((n1_away_corners$x/n1_away_games))/n1_avg_ACO, digits = 4)
# p1_away_coas <- round(((p1_away_corners$x/p1_away_games))/p1_avg_ACO, digits = 4)
# sc0_away_coas <- round(((sc0_away_corners$x/sc0_away_games))/sc0_avg_ACO, digits = 4)
# sc1_away_coas <- round(((sc1_away_corners$x/sc1_away_games))/sc1_avg_ACO, digits = 4)
# sc2_away_coas <- round(((sc2_away_corners$x/sc2_away_games))/sc2_avg_ACO, digits = 4)
# sc3_away_coas <- round(((sc3_away_corners$x/sc3_away_games))/sc3_avg_ACO, digits = 4)
# sp1_away_coas <- round(((sp1_away_corners$x/sp1_away_games))/sp1_avg_ACO, digits = 4)
# sp2_away_coas <- round(((sp2_away_corners$x/sp2_away_games))/sp2_avg_ACO, digits = 4)
# t1_away_coas <- round(((t1_away_corners$x/t1_away_games))/t1_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HCOC <- round(mls_T_ACO /mls_GP, digits = 4)
arg_avg_HCOC <- round(arg_T_ACO /arg_GP, digits = 4)
# d2_avg_HCOC <- round(d2_T_ACO /d2_GP, digits = 4)
# e0_avg_HCOC <- round(e0_T_ACO /e0_GP, digits = 4)
# e1_avg_HCOC <- round(e1_T_ACO /e1_GP, digits = 4)
# e2_avg_HCOC <- round(e2_T_ACO /e2_GP, digits = 4)
# e3_avg_HCOC <- round(e3_T_ACO /e3_GP, digits = 4)
# ec_avg_HCOC <- round(ec_T_ACO /ec_GP, digits = 4)
# f1_avg_HCOC <- round(f1_T_ACO /f1_GP, digits = 4)
# f2_avg_HCOC <- round(f2_T_ACO /f2_GP, digits = 4)
# g1_avg_HCOC <- round(g1_T_ACO /g1_GP, digits = 4)
# i1_avg_HCOC <- round(i1_T_ACO /i1_GP, digits = 4)
# i2_avg_HCOC <- round(i2_T_ACO /i2_GP, digits = 4)
# n1_avg_HCOC <- round(n1_T_ACO /n1_GP, digits = 4)
# p1_avg_HCOC <- round(p1_T_ACO /p1_GP, digits = 4)
# sc0_avg_HCOC <- round(sc0_T_ACO /sc0_GP, digits = 4)
# sc1_avg_HCOC <- round(sc1_T_ACO /sc1_GP, digits = 4)
# sc2_avg_HCOC <- round(sc2_T_ACO /sc2_GP, digits = 4)
# sc3_avg_HCOC <- round(sc3_T_ACO /sc3_GP, digits = 4)
# sp1_avg_HCOC <- round(sp1_T_ACO /sp1_GP, digits = 4)
# sp2_avg_HCOC <- round(sp2_T_ACO /sp2_GP, digits = 4)
# t1_avg_HCOC <- round(t1_T_ACO /t1_GP, digits = 4)
#avg away concede
mls_avg_ACOC <- round(mls_T_HCO /mls_GP, digits = 4)
arg_avg_ACOC <- round(arg_T_HCO /arg_GP, digits = 4)
# d2_avg_ACOC <- round(d2_T_HCO /d2_GP, digits = 4)
# e0_avg_ACOC <- round(e0_T_HCO /e0_GP, digits = 4)
# e1_avg_ACOC <- round(e1_T_HCO /e1_GP, digits = 4)
# e2_avg_ACOC <- round(e2_T_HCO /e2_GP, digits = 4)
# e3_avg_ACOC <- round(e3_T_HCO /e3_GP, digits = 4)
# ec_avg_ACOC <- round(ec_T_HCO /ec_GP, digits = 4)
# f1_avg_ACOC <- round(f1_T_HCO /f1_GP, digits = 4)
# f2_avg_ACOC <- round(f2_T_HCO /f2_GP, digits = 4)
# g1_avg_ACOC <- round(g1_T_HCO /g1_GP, digits = 4)
# i1_avg_ACOC <- round(i1_T_HCO /i1_GP, digits = 4)
# i2_avg_ACOC <- round(i2_T_HCO /i2_GP, digits = 4)
# n1_avg_ACOC <- round(n1_T_HCO /n1_GP, digits = 4)
# p1_avg_ACOC <- round(p1_T_HCO /p1_GP, digits = 4)
# sc0_avg_ACOC <- round(sc0_T_HCO /sc0_GP, digits = 4)
# sc1_avg_ACOC <- round(sc1_T_HCO /sc1_GP, digits = 4)
# sc2_avg_ACOC <- round(sc2_T_HCO /sc2_GP, digits = 4)
# sc3_avg_ACOC <- round(sc3_T_HCO /sc3_GP, digits = 4)
# sp1_avg_ACOC <- round(sp1_T_HCO /sp1_GP, digits = 4)
# sp2_avg_ACOC <- round(sp2_T_HCO /sp2_GP, digits = 4)
# t1_avg_ACOC <- round(t1_T_HCO /t1_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
mls_home_coc <- aggregate(MLS$ACO, by = list(MLS$HomeTeam), FUN = sum)
mls_away_coc <- aggregate(MLS$HCO, by = list(MLS$AwayTeam), FUN = sum)
arg_home_coc <- aggregate(ARG$ACO, by = list(ARG$HomeTeam), FUN = sum)
arg_away_coc <- aggregate(ARG$HCO, by = list(ARG$AwayTeam), FUN = sum)
# d2_home_coc <- aggregate(D2$ACO, by = list(D2$HomeTeam), FUN = sum)
# d2_away_coc <- aggregate(D2$HCO, by = list(D2$AwayTeam), FUN = sum)
# e0_home_coc <- aggregate(E0$ACO, by = list(E0$HomeTeam), FUN = sum)
# e0_away_coc <- aggregate(E0$HCO, by = list(E0$AwayTeam), FUN = sum)
# e1_home_coc <- aggregate(E1$ACO, by = list(E1$HomeTeam), FUN = sum)
# e1_away_coc <- aggregate(E1$HCO, by = list(E1$AwayTeam), FUN = sum)
# e2_home_coc <- aggregate(E2$ACO, by = list(E2$HomeTeam), FUN = sum)
# e2_away_coc <- aggregate(E2$HCO, by = list(E2$AwayTeam), FUN = sum)
# e3_home_coc <- aggregate(E3$ACO, by = list(E3$HomeTeam), FUN = sum)
# e3_away_coc <- aggregate(E3$HCO, by = list(E3$AwayTeam), FUN = sum)
# ec_home_coc <- aggregate(EC$ACO, by = list(EC$HomeTeam), FUN = sum)
# ec_away_coc <- aggregate(EC$HCO, by = list(EC$AwayTeam), FUN = sum)
# f1_home_coc <- aggregate(F1$ACO, by = list(F1$HomeTeam), FUN = sum)
# f1_away_coc <- aggregate(F1$HCO, by = list(F1$AwayTeam), FUN = sum)
# f2_home_coc <- aggregate(F2$ACO, by = list(F2$HomeTeam), FUN = sum)
# f2_away_coc <- aggregate(F2$HCO, by = list(F2$AwayTeam), FUN = sum)
# g1_home_coc <- aggregate(G1$ACO, by = list(G1$HomeTeam), FUN = sum)
# g1_away_coc <- aggregate(G1$HCO, by = list(G1$AwayTeam), FUN = sum)
# i1_home_coc <- aggregate(I1$ACO, by = list(I1$HomeTeam), FUN = sum)
# i1_away_coc <- aggregate(I1$HCO, by = list(I1$AwayTeam), FUN = sum)
# i2_home_coc <- aggregate(I2$ACO, by = list(I2$HomeTeam), FUN = sum)
# i2_away_coc <- aggregate(I2$HCO, by = list(I2$AwayTeam), FUN = sum)
# n1_home_coc <- aggregate(N1$ACO, by = list(N1$HomeTeam), FUN = sum)
# n1_away_coc <- aggregate(N1$HCO, by = list(N1$AwayTeam), FUN = sum)
# p1_home_coc <- aggregate(P1$ACO, by = list(P1$HomeTeam), FUN = sum)
# p1_away_coc <- aggregate(P1$HCO, by = list(P1$AwayTeam), FUN = sum)
# sc0_home_coc <- aggregate(SC0$ACO, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_coc <- aggregate(SC0$HCO, by = list(SC0$AwayTeam), FUN = sum)
# sc1_home_coc <- aggregate(SC1$ACO, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_coc <- aggregate(SC1$HCO, by = list(SC1$AwayTeam), FUN = sum)
# sc2_home_coc <- aggregate(SC2$ACO, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_coc <- aggregate(SC2$HCO, by = list(SC2$AwayTeam), FUN = sum)
# sc3_home_coc <- aggregate(SC3$ACO, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_coc <- aggregate(SC3$HCO, by = list(SC3$AwayTeam), FUN = sum)
# sp1_home_coc <- aggregate(SP1$ACO, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_coc <- aggregate(SP1$HCO, by = list(SP1$AwayTeam), FUN = sum)
# sp2_home_coc <- aggregate(SP2$ACO, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_coc <- aggregate(SP2$HCO, by = list(SP2$AwayTeam), FUN = sum)
# t1_home_coc <- aggregate(T1$ACO, by = list(T1$HomeTeam), FUN = sum)
# t1_away_coc <- aggregate(T1$HCO, by = list(T1$AwayTeam), FUN = sum)
#home defense strength
mls_home_cods <- round(((mls_home_coc$x/mls_home_games))/mls_avg_HCOC, digits = 4)
arg_home_cods <- round(((arg_home_coc$x/arg_home_games))/arg_avg_HCOC, digits = 4)
# d2_home_cods <- round(((d2_home_coc$x/d2_home_games))/d2_avg_HCOC, digits = 4)
# e0_home_cods <- round(((e0_home_coc$x/e0_home_games))/e0_avg_HCOC, digits = 4)
# e1_home_cods <- round(((e1_home_coc$x/e1_home_games))/e1_avg_HCOC, digits = 4)
# e2_home_cods <- round(((e2_home_coc$x/e2_home_games))/e2_avg_HCOC, digits = 4)
# e3_home_cods <- round(((e3_home_coc$x/e3_home_games))/e3_avg_HCOC, digits = 4)
# ec_home_cods <- round(((ec_home_coc$x/ec_home_games))/ec_avg_HCOC, digits = 4)
# f1_home_cods <- round(((f1_home_coc$x/f1_home_games))/f1_avg_HCOC, digits = 4)
# f2_home_cods <- round(((f2_home_coc$x/f2_home_games))/f2_avg_HCOC, digits = 4)
# g1_home_cods <- round(((g1_home_coc$x/g1_home_games))/g1_avg_HCOC, digits = 4)
# i1_home_cods <- round(((i1_home_coc$x/i1_home_games))/i1_avg_HCOC, digits = 4)
# i2_home_cods <- round(((i2_home_coc$x/i2_home_games))/i2_avg_HCOC, digits = 4)
# n1_home_cods <- round(((n1_home_coc$x/n1_home_games))/n1_avg_HCOC, digits = 4)
# p1_home_cods <- round(((p1_home_coc$x/p1_home_games))/p1_avg_HCOC, digits = 4)
# sc0_home_cods <- round(((sc0_home_coc$x/sc0_home_games))/sc0_avg_HCOC, digits = 4)
# sc1_home_cods <- round(((sc1_home_coc$x/sc1_home_games))/sc1_avg_HCOC, digits = 4)
# sc2_home_cods <- round(((sc2_home_coc$x/sc2_home_games))/sc2_avg_HCOC, digits = 4)
# sc3_home_cods <- round(((sc3_home_coc$x/sc3_home_games))/sc3_avg_HCOC, digits = 4)
# sp1_home_cods <- round(((sp1_home_coc$x/sp1_home_games))/sp1_avg_HCOC, digits = 4)
# sp2_home_cods <- round(((sp2_home_coc$x/sp2_home_games))/sp2_avg_HCOC, digits = 4)
# t1_home_cods <- round(((t1_home_coc$x/t1_home_games))/t1_avg_HCOC, digits = 4)
#away defense strength
mls_away_cods <- round(((mls_away_coc$x/mls_away_games))/mls_avg_ACOC, digits = 4)
arg_away_cods <- round(((arg_away_coc$x/arg_away_games))/arg_avg_ACOC, digits = 4)
# d2_away_cods <- round(((d2_away_coc$x/d2_away_games))/d2_avg_ACOC, digits = 4)
# e0_away_cods <- round(((e0_away_coc$x/e0_away_games))/e0_avg_ACOC, digits = 4)
# e1_away_cods <- round(((e1_away_coc$x/e1_away_games))/e1_avg_ACOC, digits = 4)
# e2_away_cods <- round(((e2_away_coc$x/e2_away_games))/e2_avg_ACOC, digits = 4)
# e3_away_cods <- round(((e3_away_coc$x/e3_away_games))/e3_avg_ACOC, digits = 4)
# ec_away_cods <- round(((ec_away_coc$x/ec_away_games))/ec_avg_ACOC, digits = 4)
# f1_away_cods <- round(((f1_away_coc$x/f1_away_games))/f1_avg_ACOC, digits = 4)
# f2_away_cods <- round(((f2_away_coc$x/f2_away_games))/f2_avg_ACOC, digits = 4)
# g1_away_cods <- round(((g1_away_coc$x/g1_away_games))/g1_avg_ACOC, digits = 4)
# i1_away_cods <- round(((i1_away_coc$x/i1_away_games))/i1_avg_ACOC, digits = 4)
# i2_away_cods <- round(((i2_away_coc$x/i2_away_games))/i2_avg_ACOC, digits = 4)
# n1_away_cods <- round(((n1_away_coc$x/n1_away_games))/n1_avg_ACOC, digits = 4)
# p1_away_cods <- round(((p1_away_coc$x/p1_away_games))/p1_avg_ACOC, digits = 4)
# sc0_away_cods <- round(((sc0_away_coc$x/sc0_away_games))/sc0_avg_ACOC, digits = 4)
# sc1_away_cods <- round(((sc1_away_coc$x/sc1_away_games))/sc1_avg_ACOC, digits = 4)
# sc2_away_cods <- round(((sc2_away_coc$x/sc2_away_games))/sc2_avg_ACOC, digits = 4)
# sc3_away_cods <- round(((sc3_away_coc$x/sc3_away_games))/sc3_avg_ACOC, digits = 4)
# sp1_away_cods <- round(((sp1_away_coc$x/sp1_away_games))/sp1_avg_ACOC, digits = 4)
# sp2_away_cods <- round(((sp2_away_coc$x/sp2_away_games))/sp2_avg_ACOC, digits = 4)
# t1_away_cods <- round(((t1_away_coc$x/t1_away_games))/t1_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_corners <- cbind(mls_division,mls_teams,mls_avg_HCO,mls_home_coas,mls_home_cods)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson_corners <- cbind(arg_division,arg_teams,arg_avg_HCO,arg_home_coas,arg_home_cods)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_home_poisson_corners <- cbind(d2_division,d2_teams,d2_avg_HCO,d2_home_coas,d2_home_cods)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_home_poisson_corners <- cbind(e0_division,e0_teams,e0_avg_HCO,e0_home_coas,e0_home_cods)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_home_poisson_corners <- cbind(e1_division,e1_teams,e1_avg_HCO,e1_home_coas,e1_home_cods)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_home_poisson_corners <- cbind(e2_division,e2_teams,e2_avg_HCO,e2_home_coas,e2_home_cods)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_home_poisson_corners <- cbind(e3_division,e3_teams,e3_avg_HCO,e3_home_coas,e3_home_cods)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_home_poisson_corners <- cbind(ec_division,ec_teams,ec_avg_HCO,ec_home_coas,ec_home_cods)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_home_poisson_corners <- cbind(f1_division,f1_teams,f1_avg_HCO,f1_home_coas,f1_home_cods)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_home_poisson_corners <- cbind(f2_division,f2_teams,f2_avg_HCO,f2_home_coas,f2_home_cods)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_home_poisson_corners <- cbind(g1_division,g1_teams,g1_avg_HCO,g1_home_coas,g1_home_cods)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_home_poisson_corners <- cbind(i1_division,i1_teams,i1_avg_HCO,i1_home_coas,i1_home_cods)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_home_poisson_corners <- cbind(i2_division,i2_teams,i2_avg_HCO,i2_home_coas,i2_home_cods)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_home_poisson_corners <- cbind(n1_division,n1_teams,n1_avg_HCO,n1_home_coas,n1_home_cods)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_home_poisson_corners <- cbind(p1_division,p1_teams,p1_avg_HCO,p1_home_coas,p1_home_cods)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_home_poisson_corners <- cbind(sc0_division,sc0_teams,sc0_avg_HCO,sc0_home_coas,sc0_home_cods)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_home_poisson_corners <- cbind(sc1_division,sc1_teams,sc1_avg_HCO,sc1_home_coas,sc1_home_cods)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_home_poisson_corners <- cbind(sc2_division,sc2_teams,sc2_avg_HCO,sc2_home_coas,sc2_home_cods)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_home_poisson_corners <- cbind(sc3_division,sc3_teams,sc3_avg_HCO,sc3_home_coas,sc3_home_cods)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_home_poisson_corners <- cbind(sp1_division,sp1_teams,sp1_avg_HCO,sp1_home_coas,sp1_home_cods)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_home_poisson_corners <- cbind(sp2_division,sp2_teams,sp2_avg_HCO,sp2_home_coas,sp2_home_cods)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_home_poisson_corners <- cbind(t1_division,t1_teams,t1_avg_HCO,t1_home_coas,t1_home_cods)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_corners <- cbind(mls_division,mls_teams,mls_avg_ACO,mls_away_coas,mls_away_cods)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson_corners <- cbind(arg_division,arg_teams,arg_avg_ACO,arg_away_coas,arg_away_cods)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_away_poisson_corners <- cbind(d2_division,d2_teams,d2_avg_ACO,d2_away_coas,d2_away_cods)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_away_poisson_corners <- cbind(e0_division,e0_teams,e0_avg_ACO,e0_away_coas,e0_away_cods)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_away_poisson_corners <- cbind(e1_division,e1_teams,e1_avg_ACO,e1_away_coas,e1_away_cods)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_away_poisson_corners <- cbind(e2_division,e2_teams,e2_avg_ACO,e2_away_coas,e2_away_cods)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_away_poisson_corners <- cbind(e3_division,e3_teams,e3_avg_ACO,e3_away_coas,e3_away_cods)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_away_poisson_corners <- cbind(ec_division,ec_teams,ec_avg_ACO,ec_away_coas,ec_away_cods)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_away_poisson_corners <- cbind(f1_division,f1_teams,f1_avg_ACO,f1_away_coas,f1_away_cods)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_away_poisson_corners <- cbind(f2_division,f2_teams,f2_avg_ACO,f2_away_coas,f2_away_cods)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_away_poisson_corners <- cbind(g1_division,g1_teams,g1_avg_ACO,g1_away_coas,g1_away_cods)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_away_poisson_corners <- cbind(i1_division,i1_teams,i1_avg_ACO,i1_away_coas,i1_away_cods)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_away_poisson_corners <- cbind(i2_division,i2_teams,i2_avg_ACO,i2_away_coas,i2_away_cods)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_away_poisson_corners <- cbind(n1_division,n1_teams,n1_avg_ACO,n1_away_coas,n1_away_cods)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_away_poisson_corners <- cbind(p1_division,p1_teams,p1_avg_ACO,p1_away_coas,p1_away_cods)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_away_poisson_corners <- cbind(sc0_division,sc0_teams,sc0_avg_ACO,sc0_away_coas,sc0_away_cods)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_away_poisson_corners <- cbind(sc1_division,sc1_teams,sc1_avg_ACO,sc1_away_coas,sc1_away_cods)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_away_poisson_corners <- cbind(sc2_division,sc2_teams,sc2_avg_ACO,sc2_away_coas,sc2_away_cods)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_away_poisson_corners <- cbind(sc3_division,sc3_teams,sc3_avg_ACO,sc3_away_coas,sc3_away_cods)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_away_poisson_corners <- cbind(sp1_division,sp1_teams,sp1_avg_ACO,sp1_away_coas,sp1_away_cods)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_away_poisson_corners <- cbind(sp2_division,sp2_teams,sp2_avg_ACO,sp2_away_coas,sp2_away_cods)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_away_poisson_corners <- cbind(t1_division,t1_teams,t1_avg_ACO,t1_away_coas,t1_away_cods)
#create home and away csv
home_poisson_corners <- rbind(mls_home_poisson_corners,arg_home_poisson_corners)
away_poisson_corners <- rbind(mls_away_poisson_corners,arg_away_poisson_corners)
# #delete current
# unlink("R_home.csv")
# unlink("R_away.csv")
# #write another one
# write.csv(home_poisson,'R_home.csv')
# write.csv(away_poisson,'R_away.csv')
#MLS
HomeTeam_mls_co <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_co <- rep(mls_teams, length(mls_teams))
MLS_fixtures_co <- cbind(HomeTeam_mls_co,AwayTeam_mls_co)
MLS_fixtures_co <- as.data.frame(MLS_fixtures_co)
MLS_fixtures_co <- MLS_fixtures_co[!MLS_fixtures_co$HomeTeam_mls_co == MLS_fixtures_co$AwayTeam_mls_co,]
rownames(MLS_fixtures_co) <- NULL
MLS_fixtures_co$Div <- "MLS"
MLS_fixtures_co <- MLS_fixtures_co[,c(3,1,2)]

MLS_fixtures_co$avg_HCO_mls <- mls_avg_HCO

MLS_fixtures_co$mls_homecoas <- rep(mls_home_coas,each = length(mls_teams)-1)

mls_awaycods_lookup <- cbind(mls_teams,mls_away_cods)

mls_awaycods_lookup <- as.data.frame(mls_awaycods_lookup)

colnames(mls_awaycods_lookup) <- c("AwayTeam_mls_co","mls_awaycods")


require('RH2')
MLS_fixtures_co$mls_awaycods <- sqldf("SELECT mls_awaycods_lookup.mls_awaycods FROM mls_awaycods_lookup INNER JOIN MLS_fixtures_co ON mls_awaycods_lookup.AwayTeam_mls_co = MLS_fixtures_co.AwayTeam_mls_co")

MLS_fixtures_co$avg_ACO_mls <- mls_avg_ACO

mls_awaycoas_lookup <- cbind(mls_teams,mls_away_coas)

mls_awaycoas_lookup <- as.data.frame(mls_awaycoas_lookup)

colnames(mls_awaycoas_lookup) <- c("AwayTeam_mls_co","mls_awaycoas")

MLS_fixtures_co$mls_awaycoas <- sqldf("SELECT mls_awaycoas_lookup.mls_awaycoas FROM mls_awaycoas_lookup INNER JOIN MLS_fixtures_co ON mls_awaycoas_lookup.AwayTeam_mls_co = MLS_fixtures_co.AwayTeam_mls_co")

MLS_fixtures_co$mls_homecods <- rep(mls_home_cods,each = length(mls_teams)-1)

MLS_fixtures_co$mls_awaycods <- as.numeric(unlist(MLS_fixtures_co$mls_awaycods))
#xGH
MLS_fixtures_co$mls_xHCOC <- MLS_fixtures_co$avg_HCO_mls * MLS_fixtures_co$mls_homecoas * MLS_fixtures_co$mls_awaycods
#xGA

MLS_fixtures_co$mls_awaycoas <- as.numeric(unlist(MLS_fixtures_co$mls_awaycoas))

MLS_fixtures_co$mls_xACOC <- MLS_fixtures_co$avg_ACO_mls * MLS_fixtures_co$mls_awaycoas * MLS_fixtures_co$mls_homecods

MLS_fixtures_co$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(0,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(1,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(2,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(3,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(4,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_co$mls_xHCOC) * stats::dpois(5,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
MLS_fixtures_co$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_co$mls_xHCOC) * stats::dpois(6,MLS_fixtures_co$mls_xACOC), digits = 4)
#Home win
MLS_fixtures_co$mls_H <- (
  MLS_fixtures_co$mls_1_0 + MLS_fixtures_co$mls_2_0 + MLS_fixtures_co$mls_2_1 + MLS_fixtures_co$mls_3_0 + MLS_fixtures_co$mls_3_1 +
    MLS_fixtures_co$mls_3_2 + MLS_fixtures_co$mls_4_0 + MLS_fixtures_co$mls_4_1 + MLS_fixtures_co$mls_4_2 + MLS_fixtures_co$mls_4_3 +
    MLS_fixtures_co$mls_5_0 + MLS_fixtures_co$mls_5_1 + MLS_fixtures_co$mls_5_2 + MLS_fixtures_co$mls_5_3 + MLS_fixtures_co$mls_5_4 +
    MLS_fixtures_co$mls_6_0 + MLS_fixtures_co$mls_6_1 + MLS_fixtures_co$mls_6_2 + MLS_fixtures_co$mls_6_3 + MLS_fixtures_co$mls_6_4 +
    MLS_fixtures_co$mls_6_5
)

MLS_fixtures_co$mls_H <- percent(MLS_fixtures_co$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_co$mls_D <- (

  MLS_fixtures_co$mls_0_0 + MLS_fixtures_co$mls_1_1 + MLS_fixtures_co$mls_2_2 + MLS_fixtures_co$mls_3_3 + MLS_fixtures_co$mls_4_4 +
    MLS_fixtures_co$mls_5_5 + MLS_fixtures_co$mls_6_6
)

MLS_fixtures_co$mls_D <- percent(MLS_fixtures_co$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_co$mls_A <- (
  MLS_fixtures_co$mls_0_1 + MLS_fixtures_co$mls_0_2 + MLS_fixtures_co$mls_1_2 + MLS_fixtures_co$mls_0_3 + MLS_fixtures_co$mls_1_3 +
    MLS_fixtures_co$mls_2_3 + MLS_fixtures_co$mls_0_4 + MLS_fixtures_co$mls_1_4 + MLS_fixtures_co$mls_2_4 + MLS_fixtures_co$mls_3_4 +
    MLS_fixtures_co$mls_0_5 + MLS_fixtures_co$mls_1_5 + MLS_fixtures_co$mls_2_5 + MLS_fixtures_co$mls_3_5 + MLS_fixtures_co$mls_4_5 +
    MLS_fixtures_co$mls_0_6 + MLS_fixtures_co$mls_1_6 + MLS_fixtures_co$mls_2_6 + MLS_fixtures_co$mls_3_6 + MLS_fixtures_co$mls_4_6 +
    MLS_fixtures_co$mls_5_6
)

MLS_fixtures_co$mls_A <- percent(MLS_fixtures_co$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_co$mls_ov25 <- (
  MLS_fixtures_co$mls_2_1 + MLS_fixtures_co$mls_1_2 + MLS_fixtures_co$mls_2_2 + MLS_fixtures_co$mls_3_0 + MLS_fixtures_co$mls_3_1 +
    MLS_fixtures_co$mls_3_2 + MLS_fixtures_co$mls_0_3 + MLS_fixtures_co$mls_1_3 + MLS_fixtures_co$mls_2_3 + MLS_fixtures_co$mls_3_3 +
    MLS_fixtures_co$mls_4_0 + MLS_fixtures_co$mls_4_1 + MLS_fixtures_co$mls_4_2 + MLS_fixtures_co$mls_4_3 + MLS_fixtures_co$mls_0_4 +
    MLS_fixtures_co$mls_1_4 + MLS_fixtures_co$mls_2_4 + MLS_fixtures_co$mls_3_4 + MLS_fixtures_co$mls_4_4 + MLS_fixtures_co$mls_5_0 +
    MLS_fixtures_co$mls_5_1 + MLS_fixtures_co$mls_5_2 + MLS_fixtures_co$mls_5_3 + MLS_fixtures_co$mls_5_4 + MLS_fixtures_co$mls_0_5 +
    MLS_fixtures_co$mls_1_5 + MLS_fixtures_co$mls_2_5 + MLS_fixtures_co$mls_3_5 + MLS_fixtures_co$mls_4_5 + MLS_fixtures_co$mls_5_5 +
    MLS_fixtures_co$mls_6_0 + MLS_fixtures_co$mls_6_1 + MLS_fixtures_co$mls_6_2 + MLS_fixtures_co$mls_6_3 + MLS_fixtures_co$mls_6_4 +
    MLS_fixtures_co$mls_6_5 + MLS_fixtures_co$mls_0_6 + MLS_fixtures_co$mls_1_6 + MLS_fixtures_co$mls_2_6 + MLS_fixtures_co$mls_3_6 +
    MLS_fixtures_co$mls_4_6 + MLS_fixtures_co$mls_5_6 + MLS_fixtures_co$mls_6_6
)
#un25
MLS_fixtures_co$mls_un25 <- (
  MLS_fixtures_co$mls_0_0 + MLS_fixtures_co$mls_1_0 + MLS_fixtures_co$mls_0_1 + MLS_fixtures_co$mls_1_1 + MLS_fixtures_co$mls_2_0 + MLS_fixtures_co$mls_0_2
)
#odds
MLS_fixtures_co$mls_ov25_odds <- round((1/MLS_fixtures_co$mls_ov25),digits = 2)
MLS_fixtures_co$mls_un25_odds <- round((1/MLS_fixtures_co$mls_un25),digits = 2)

MLS_fixtures_co$mls_ov25_odds
MLS_fixtures_co$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_co$mls_ov25 <- percent(MLS_fixtures_co$mls_ov25, accuracy = 0.1)

MLS_fixtures_co$mls_un25 <- percent(MLS_fixtures_co$mls_un25, accuracy = 0.1)
MLS_fixtures_co$mls_pscore <- paste(round(MLS_fixtures_co$mls_xHCOC,digits = 0),round(MLS_fixtures_co$mls_xACOC,digits = 0),sep = "-")
#write out
#write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
##################################################################################################################
#ARG
HomeTeam_arg_co <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg_co <- rep(arg_teams, length(arg_teams))
ARG_fixtures_co <- cbind(HomeTeam_arg_co,AwayTeam_arg_co)
ARG_fixtures_co <- as.data.frame(ARG_fixtures_co)
ARG_fixtures_co <- ARG_fixtures_co[!ARG_fixtures_co$HomeTeam_arg_co == ARG_fixtures_co$AwayTeam_arg_co,]
rownames(ARG_fixtures_co) <- NULL
ARG_fixtures_co$Div <- "ARG"
ARG_fixtures_co <- ARG_fixtures_co[,c(3,1,2)]

ARG_fixtures_co$avg_HCO_arg <- arg_avg_HCO

ARG_fixtures_co$arg_homecoas <- rep(arg_home_coas,each = length(arg_teams)-1)

arg_awaycods_lookup <- cbind(arg_teams,arg_away_cods)

arg_awaycods_lookup <- as.data.frame(arg_awaycods_lookup)

colnames(arg_awaycods_lookup) <- c("AwayTeam_arg_co","arg_awaycods")


require('RH2')
ARG_fixtures_co$arg_awaycods <- sqldf("SELECT arg_awaycods_lookup.arg_awaycods FROM arg_awaycods_lookup INNER JOIN ARG_fixtures_co ON arg_awaycods_lookup.AwayTeam_arg_co = ARG_fixtures_co.AwayTeam_arg_co")

ARG_fixtures_co$avg_ACO_arg <- arg_avg_ACO

arg_awaycoas_lookup <- cbind(arg_teams,arg_away_coas)

arg_awaycoas_lookup <- as.data.frame(arg_awaycoas_lookup)

colnames(arg_awaycoas_lookup) <- c("AwayTeam_arg_co","arg_awaycoas")

ARG_fixtures_co$arg_awaycoas <- sqldf("SELECT arg_awaycoas_lookup.arg_awaycoas FROM arg_awaycoas_lookup INNER JOIN ARG_fixtures_co ON arg_awaycoas_lookup.AwayTeam_arg_co = ARG_fixtures_co.AwayTeam_arg_co")

ARG_fixtures_co$arg_homecods <- rep(arg_home_cods,each = length(arg_teams)-1)

ARG_fixtures_co$arg_awaycods <- as.numeric(unlist(ARG_fixtures_co$arg_awaycods))
#xGH
ARG_fixtures_co$arg_xHCOC <- ARG_fixtures_co$avg_HCO_arg * ARG_fixtures_co$arg_homecoas * ARG_fixtures_co$arg_awaycods
#xGA

ARG_fixtures_co$arg_awaycoas <- as.numeric(unlist(ARG_fixtures_co$arg_awaycoas))

ARG_fixtures_co$arg_xACOC <- ARG_fixtures_co$avg_ACO_arg * ARG_fixtures_co$arg_awaycoas * ARG_fixtures_co$arg_homecods

ARG_fixtures_co$arg_0_0 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_0 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_1 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_1 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_0 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_2 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_2 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_1 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_2 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_3 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_0 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_1 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_2 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_3 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_3 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_3 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_4 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_0 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_1 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_2 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_3 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_4 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_4 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_4 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_4 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_5 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_0 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_1 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_2 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_3 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_4 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_5 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_5 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_5 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_5 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_5 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_6 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_0 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(0,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_1 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(1,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_2 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(2,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_3 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(3,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_4 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(4,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_6_5 <- round(stats::dpois(6,ARG_fixtures_co$arg_xHCOC) * stats::dpois(5,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_0_6 <- round(stats::dpois(0,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_1_6 <- round(stats::dpois(1,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_2_6 <- round(stats::dpois(2,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_3_6 <- round(stats::dpois(3,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_4_6 <- round(stats::dpois(4,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
ARG_fixtures_co$arg_5_6 <- round(stats::dpois(5,ARG_fixtures_co$arg_xHCOC) * stats::dpois(6,ARG_fixtures_co$arg_xACOC), digits = 4)
#Home win
ARG_fixtures_co$arg_H <- (
  ARG_fixtures_co$arg_1_0 + ARG_fixtures_co$arg_2_0 + ARG_fixtures_co$arg_2_1 + ARG_fixtures_co$arg_3_0 + ARG_fixtures_co$arg_3_1 +
    ARG_fixtures_co$arg_3_2 + ARG_fixtures_co$arg_4_0 + ARG_fixtures_co$arg_4_1 + ARG_fixtures_co$arg_4_2 + ARG_fixtures_co$arg_4_3 +
    ARG_fixtures_co$arg_5_0 + ARG_fixtures_co$arg_5_1 + ARG_fixtures_co$arg_5_2 + ARG_fixtures_co$arg_5_3 + ARG_fixtures_co$arg_5_4 +
    ARG_fixtures_co$arg_6_0 + ARG_fixtures_co$arg_6_1 + ARG_fixtures_co$arg_6_2 + ARG_fixtures_co$arg_6_3 + ARG_fixtures_co$arg_6_4 +
    ARG_fixtures_co$arg_6_5
)

ARG_fixtures_co$arg_H <- percent(ARG_fixtures_co$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures_co$arg_D <- (

  ARG_fixtures_co$arg_0_0 + ARG_fixtures_co$arg_1_1 + ARG_fixtures_co$arg_2_2 + ARG_fixtures_co$arg_3_3 + ARG_fixtures_co$arg_4_4 +
    ARG_fixtures_co$arg_5_5 + ARG_fixtures_co$arg_6_6
)

ARG_fixtures_co$arg_D <- percent(ARG_fixtures_co$arg_D, accuracy = 0.1)

#Away

ARG_fixtures_co$arg_A <- (
  ARG_fixtures_co$arg_0_1 + ARG_fixtures_co$arg_0_2 + ARG_fixtures_co$arg_1_2 + ARG_fixtures_co$arg_0_3 + ARG_fixtures_co$arg_1_3 +
    ARG_fixtures_co$arg_2_3 + ARG_fixtures_co$arg_0_4 + ARG_fixtures_co$arg_1_4 + ARG_fixtures_co$arg_2_4 + ARG_fixtures_co$arg_3_4 +
    ARG_fixtures_co$arg_0_5 + ARG_fixtures_co$arg_1_5 + ARG_fixtures_co$arg_2_5 + ARG_fixtures_co$arg_3_5 + ARG_fixtures_co$arg_4_5 +
    ARG_fixtures_co$arg_0_6 + ARG_fixtures_co$arg_1_6 + ARG_fixtures_co$arg_2_6 + ARG_fixtures_co$arg_3_6 + ARG_fixtures_co$arg_4_6 +
    ARG_fixtures_co$arg_5_6
)

ARG_fixtures_co$arg_A <- percent(ARG_fixtures_co$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures_co$arg_ov25 <- (
  ARG_fixtures_co$arg_2_1 + ARG_fixtures_co$arg_1_2 + ARG_fixtures_co$arg_2_2 + ARG_fixtures_co$arg_3_0 + ARG_fixtures_co$arg_3_1 +
    ARG_fixtures_co$arg_3_2 + ARG_fixtures_co$arg_0_3 + ARG_fixtures_co$arg_1_3 + ARG_fixtures_co$arg_2_3 + ARG_fixtures_co$arg_3_3 +
    ARG_fixtures_co$arg_4_0 + ARG_fixtures_co$arg_4_1 + ARG_fixtures_co$arg_4_2 + ARG_fixtures_co$arg_4_3 + ARG_fixtures_co$arg_0_4 +
    ARG_fixtures_co$arg_1_4 + ARG_fixtures_co$arg_2_4 + ARG_fixtures_co$arg_3_4 + ARG_fixtures_co$arg_4_4 + ARG_fixtures_co$arg_5_0 +
    ARG_fixtures_co$arg_5_1 + ARG_fixtures_co$arg_5_2 + ARG_fixtures_co$arg_5_3 + ARG_fixtures_co$arg_5_4 + ARG_fixtures_co$arg_0_5 +
    ARG_fixtures_co$arg_1_5 + ARG_fixtures_co$arg_2_5 + ARG_fixtures_co$arg_3_5 + ARG_fixtures_co$arg_4_5 + ARG_fixtures_co$arg_5_5 +
    ARG_fixtures_co$arg_6_0 + ARG_fixtures_co$arg_6_1 + ARG_fixtures_co$arg_6_2 + ARG_fixtures_co$arg_6_3 + ARG_fixtures_co$arg_6_4 +
    ARG_fixtures_co$arg_6_5 + ARG_fixtures_co$arg_0_6 + ARG_fixtures_co$arg_1_6 + ARG_fixtures_co$arg_2_6 + ARG_fixtures_co$arg_3_6 +
    ARG_fixtures_co$arg_4_6 + ARG_fixtures_co$arg_5_6 + ARG_fixtures_co$arg_6_6
)
#un25
ARG_fixtures_co$arg_un25 <- (
  ARG_fixtures_co$arg_0_0 + ARG_fixtures_co$arg_1_0 + ARG_fixtures_co$arg_0_1 + ARG_fixtures_co$arg_1_1 + ARG_fixtures_co$arg_2_0 + ARG_fixtures_co$arg_0_2
)
#odds
ARG_fixtures_co$arg_ov25_odds <- round((1/ARG_fixtures_co$arg_ov25),digits = 2)
ARG_fixtures_co$arg_un25_odds <- round((1/ARG_fixtures_co$arg_un25),digits = 2)

ARG_fixtures_co$arg_ov25_odds
ARG_fixtures_co$arg_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
ARG_fixtures_co$arg_ov25 <- percent(ARG_fixtures_co$arg_ov25, accuracy = 0.1)

ARG_fixtures_co$arg_un25 <- percent(ARG_fixtures_co$arg_un25, accuracy = 0.1)
ARG_fixtures_co$arg_pscore <- paste(round(ARG_fixtures_co$arg_xHCOC,digits = 0),round(ARG_fixtures_co$arg_xACOC,digits = 0),sep = "-")
#write out
#write.xlsx(ARG_fixtures,'Divisions/ARG.xlsx',sheetName = "ARG", append = TRUE)
#################################################################################################################
#D2
# HomeTeam_d2_co <- rep(d2_teams, each = length(d2_teams))
# AwayTeam_d2_co <- rep(d2_teams, length(d2_teams))
# D2_fixtures_co <- cbind(HomeTeam_d2_co,AwayTeam_d2_co)
# D2_fixtures_co <- as.data.frame(D2_fixtures_co)
# D2_fixtures_co <- D2_fixtures_co[!D2_fixtures_co$HomeTeam_d2_co == D2_fixtures_co$AwayTeam_d2_co,]
# rownames(D2_fixtures_co) <- NULL
# D2_fixtures_co$Div <- "D2"
# D2_fixtures_co <- D2_fixtures_co[,c(3,1,2)]
#
# D2_fixtures_co$avg_HCO_d2 <- d2_avg_HCO
#
# D2_fixtures_co$d2_homecoas <- rep(d2_home_coas,each = length(d2_teams)-1)
#
# d2_awaycods_lookup <- cbind(d2_teams,d2_away_cods)
#
# d2_awaycods_lookup <- as.data.frame(d2_awaycods_lookup)
#
# colnames(d2_awaycods_lookup) <- c("AwayTeam_d2_co","d2_awaycods")
#
#
# require('RH2')
# D2_fixtures_co$d2_awaycods <- sqldf("SELECT d2_awaycods_lookup.d2_awaycods FROM d2_awaycods_lookup INNER JOIN D2_fixtures_co ON d2_awaycods_lookup.AwayTeam_d2_co = D2_fixtures_co.AwayTeam_d2_co")
#
# D2_fixtures_co$avg_ACO_d2 <- d2_avg_ACO
#
# d2_awaycoas_lookup <- cbind(d2_teams,d2_away_coas)
#
# d2_awaycoas_lookup <- as.data.frame(d2_awaycoas_lookup)
#
# colnames(d2_awaycoas_lookup) <- c("AwayTeam_d2_co","d2_awaycoas")
#
# D2_fixtures_co$d2_awaycoas <- sqldf("SELECT d2_awaycoas_lookup.d2_awaycoas FROM d2_awaycoas_lookup INNER JOIN D2_fixtures_co ON d2_awaycoas_lookup.AwayTeam_d2_co = D2_fixtures_co.AwayTeam_d2_co")
#
# D2_fixtures_co$d2_homecods <- rep(d2_home_cods,each = length(d2_teams)-1)
#
# D2_fixtures_co$d2_awaycods <- as.numeric(unlist(D2_fixtures_co$d2_awaycods))
# #xGH
# D2_fixtures_co$d2_xHCOC <- D2_fixtures_co$avg_HCO_d2 * D2_fixtures_co$d2_homecoas * D2_fixtures_co$d2_awaycods
# #xGA
#
# D2_fixtures_co$d2_awaycoas <- as.numeric(unlist(D2_fixtures_co$d2_awaycoas))
#
# D2_fixtures_co$d2_xACOC <- D2_fixtures_co$avg_ACO_d2 * D2_fixtures_co$d2_awaycoas * D2_fixtures_co$d2_homecods
#
# D2_fixtures_co$d2_0_0 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_0 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_1 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_1 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_0 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_2 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_2 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_1 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_2 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_3 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_0 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_1 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_2 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_3 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_3 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_3 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_4 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_0 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_1 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_2 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_3 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_4 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_4 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_4 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_4 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_5 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_0 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_1 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_2 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_3 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_4 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_5 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_5 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_5 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_5 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_5 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_6 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_0 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(0,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_1 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(1,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_2 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(2,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_3 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(3,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_4 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(4,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_6_5 <- round(stats::dpois(6,D2_fixtures_co$d2_xHCOC) * stats::dpois(5,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_0_6 <- round(stats::dpois(0,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_1_6 <- round(stats::dpois(1,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_2_6 <- round(stats::dpois(2,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_3_6 <- round(stats::dpois(3,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_4_6 <- round(stats::dpois(4,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# D2_fixtures_co$d2_5_6 <- round(stats::dpois(5,D2_fixtures_co$d2_xHCOC) * stats::dpois(6,D2_fixtures_co$d2_xACOC), digits = 4)
# #Home win
# D2_fixtures_co$d2_H <- (
#   D2_fixtures_co$d2_1_0 + D2_fixtures_co$d2_2_0 + D2_fixtures_co$d2_2_1 + D2_fixtures_co$d2_3_0 + D2_fixtures_co$d2_3_1 +
#     D2_fixtures_co$d2_3_2 + D2_fixtures_co$d2_4_0 + D2_fixtures_co$d2_4_1 + D2_fixtures_co$d2_4_2 + D2_fixtures_co$d2_4_3 +
#     D2_fixtures_co$d2_5_0 + D2_fixtures_co$d2_5_1 + D2_fixtures_co$d2_5_2 + D2_fixtures_co$d2_5_3 + D2_fixtures_co$d2_5_4 +
#     D2_fixtures_co$d2_6_0 + D2_fixtures_co$d2_6_1 + D2_fixtures_co$d2_6_2 + D2_fixtures_co$d2_6_3 + D2_fixtures_co$d2_6_4 +
#     D2_fixtures_co$d2_6_5
# )
#
# D2_fixtures_co$d2_H <- percent(D2_fixtures_co$d2_H, accuracy = 0.1)
#
# #Draw
# D2_fixtures_co$d2_D <- (
#
#   D2_fixtures_co$d2_0_0 + D2_fixtures_co$d2_1_1 + D2_fixtures_co$d2_2_2 + D2_fixtures_co$d2_3_3 + D2_fixtures_co$d2_4_4 +
#     D2_fixtures_co$d2_5_5 + D2_fixtures_co$d2_6_6
# )
#
# D2_fixtures_co$d2_D <- percent(D2_fixtures_co$d2_D, accuracy = 0.1)
#
# #Away
#
# D2_fixtures_co$d2_A <- (
#   D2_fixtures_co$d2_0_1 + D2_fixtures_co$d2_0_2 + D2_fixtures_co$d2_1_2 + D2_fixtures_co$d2_0_3 + D2_fixtures_co$d2_1_3 +
#     D2_fixtures_co$d2_2_3 + D2_fixtures_co$d2_0_4 + D2_fixtures_co$d2_1_4 + D2_fixtures_co$d2_2_4 + D2_fixtures_co$d2_3_4 +
#     D2_fixtures_co$d2_0_5 + D2_fixtures_co$d2_1_5 + D2_fixtures_co$d2_2_5 + D2_fixtures_co$d2_3_5 + D2_fixtures_co$d2_4_5 +
#     D2_fixtures_co$d2_0_6 + D2_fixtures_co$d2_1_6 + D2_fixtures_co$d2_2_6 + D2_fixtures_co$d2_3_6 + D2_fixtures_co$d2_4_6 +
#     D2_fixtures_co$d2_5_6
# )
#
# D2_fixtures_co$d2_A <- percent(D2_fixtures_co$d2_A, accuracy = 0.1)
#
# #ov25
# D2_fixtures_co$d2_ov25 <- (
#   D2_fixtures_co$d2_2_1 + D2_fixtures_co$d2_1_2 + D2_fixtures_co$d2_2_2 + D2_fixtures_co$d2_3_0 + D2_fixtures_co$d2_3_1 +
#     D2_fixtures_co$d2_3_2 + D2_fixtures_co$d2_0_3 + D2_fixtures_co$d2_1_3 + D2_fixtures_co$d2_2_3 + D2_fixtures_co$d2_3_3 +
#     D2_fixtures_co$d2_4_0 + D2_fixtures_co$d2_4_1 + D2_fixtures_co$d2_4_2 + D2_fixtures_co$d2_4_3 + D2_fixtures_co$d2_0_4 +
#     D2_fixtures_co$d2_1_4 + D2_fixtures_co$d2_2_4 + D2_fixtures_co$d2_3_4 + D2_fixtures_co$d2_4_4 + D2_fixtures_co$d2_5_0 +
#     D2_fixtures_co$d2_5_1 + D2_fixtures_co$d2_5_2 + D2_fixtures_co$d2_5_3 + D2_fixtures_co$d2_5_4 + D2_fixtures_co$d2_0_5 +
#     D2_fixtures_co$d2_1_5 + D2_fixtures_co$d2_2_5 + D2_fixtures_co$d2_3_5 + D2_fixtures_co$d2_4_5 + D2_fixtures_co$d2_5_5 +
#     D2_fixtures_co$d2_6_0 + D2_fixtures_co$d2_6_1 + D2_fixtures_co$d2_6_2 + D2_fixtures_co$d2_6_3 + D2_fixtures_co$d2_6_4 +
#     D2_fixtures_co$d2_6_5 + D2_fixtures_co$d2_0_6 + D2_fixtures_co$d2_1_6 + D2_fixtures_co$d2_2_6 + D2_fixtures_co$d2_3_6 +
#     D2_fixtures_co$d2_4_6 + D2_fixtures_co$d2_5_6 + D2_fixtures_co$d2_6_6
# )
# #un25
# D2_fixtures_co$d2_un25 <- (
#   D2_fixtures_co$d2_0_0 + D2_fixtures_co$d2_1_0 + D2_fixtures_co$d2_0_1 + D2_fixtures_co$d2_1_1 + D2_fixtures_co$d2_2_0 + D2_fixtures_co$d2_0_2
# )
# #odds
# D2_fixtures_co$d2_ov25_odds <- round((1/D2_fixtures_co$d2_ov25),digits = 2)
# D2_fixtures_co$d2_un25_odds <- round((1/D2_fixtures_co$d2_un25),digits = 2)
#
# D2_fixtures_co$d2_ov25_odds
# D2_fixtures_co$d2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# D2_fixtures_co$d2_ov25 <- percent(D2_fixtures_co$d2_ov25, accuracy = 0.1)
#
# D2_fixtures_co$d2_un25 <- percent(D2_fixtures_co$d2_un25, accuracy = 0.1)
# D2_fixtures_co$d2_pscore <- paste(round(D2_fixtures_co$d2_xHCOC,digits = 0),round(D2_fixtures_co$d2_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(D2_fixtures,'Divisions/D2.xlsx',sheetName = "D2", append = TRUE)
# #################################################################################################################
# #E0
# HomeTeam_e0_co <- rep(e0_teams, each = length(e0_teams))
# AwayTeam_e0_co <- rep(e0_teams, length(e0_teams))
# E0_fixtures_co <- cbind(HomeTeam_e0_co,AwayTeam_e0_co)
# E0_fixtures_co <- as.data.frame(E0_fixtures_co)
# E0_fixtures_co <- E0_fixtures_co[!E0_fixtures_co$HomeTeam_e0_co == E0_fixtures_co$AwayTeam_e0_co,]
# rownames(E0_fixtures_co) <- NULL
# E0_fixtures_co$Div <- "E0"
# E0_fixtures_co <- E0_fixtures_co[,c(3,1,2)]
#
# E0_fixtures_co$avg_HCO_e0 <- e0_avg_HCO
#
# E0_fixtures_co$e0_homecoas <- rep(e0_home_coas,each = length(e0_teams)-1)
#
# e0_awaycods_lookup <- cbind(e0_teams,e0_away_cods)
#
# e0_awaycods_lookup <- as.data.frame(e0_awaycods_lookup)
#
# colnames(e0_awaycods_lookup) <- c("AwayTeam_e0_co","e0_awaycods")
#
#
# require('RH2')
# E0_fixtures_co$e0_awaycods <- sqldf("SELECT e0_awaycods_lookup.e0_awaycods FROM e0_awaycods_lookup INNER JOIN E0_fixtures_co ON e0_awaycods_lookup.AwayTeam_e0_co = E0_fixtures_co.AwayTeam_e0_co")
#
# E0_fixtures_co$avg_ACO_e0 <- e0_avg_ACO
#
# e0_awaycoas_lookup <- cbind(e0_teams,e0_away_coas)
#
# e0_awaycoas_lookup <- as.data.frame(e0_awaycoas_lookup)
#
# colnames(e0_awaycoas_lookup) <- c("AwayTeam_e0_co","e0_awaycoas")
#
# E0_fixtures_co$e0_awaycoas <- sqldf("SELECT e0_awaycoas_lookup.e0_awaycoas FROM e0_awaycoas_lookup INNER JOIN E0_fixtures_co ON e0_awaycoas_lookup.AwayTeam_e0_co = E0_fixtures_co.AwayTeam_e0_co")
#
# E0_fixtures_co$e0_homecods <- rep(e0_home_cods,each = length(e0_teams)-1)
#
# E0_fixtures_co$e0_awaycods <- as.numeric(unlist(E0_fixtures_co$e0_awaycods))
# #xGH
# E0_fixtures_co$e0_xHCOC <- E0_fixtures_co$avg_HCO_e0 * E0_fixtures_co$e0_homecoas * E0_fixtures_co$e0_awaycods
# #xGA
#
# E0_fixtures_co$e0_awaycoas <- as.numeric(unlist(E0_fixtures_co$e0_awaycoas))
#
# E0_fixtures_co$e0_xACOC <- E0_fixtures_co$avg_ACO_e0 * E0_fixtures_co$e0_awaycoas * E0_fixtures_co$e0_homecods
#
# E0_fixtures_co$e0_0_0 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_0 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_1 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_1 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_0 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_2 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_2 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_1 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_2 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_3 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_0 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_1 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_2 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_3 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_3 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_3 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_4 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_0 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_1 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_2 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_3 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_4 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_4 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_4 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_4 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_5 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_0 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_1 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_2 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_3 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_4 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_5 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_5 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_5 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_5 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_5 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_6 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_0 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(0,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_1 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(1,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_2 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(2,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_3 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(3,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_4 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(4,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_6_5 <- round(stats::dpois(6,E0_fixtures_co$e0_xHCOC) * stats::dpois(5,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_0_6 <- round(stats::dpois(0,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_1_6 <- round(stats::dpois(1,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_2_6 <- round(stats::dpois(2,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_3_6 <- round(stats::dpois(3,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_4_6 <- round(stats::dpois(4,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# E0_fixtures_co$e0_5_6 <- round(stats::dpois(5,E0_fixtures_co$e0_xHCOC) * stats::dpois(6,E0_fixtures_co$e0_xACOC), digits = 4)
# #Home win
# E0_fixtures_co$e0_H <- (
#   E0_fixtures_co$e0_1_0 + E0_fixtures_co$e0_2_0 + E0_fixtures_co$e0_2_1 + E0_fixtures_co$e0_3_0 + E0_fixtures_co$e0_3_1 +
#     E0_fixtures_co$e0_3_2 + E0_fixtures_co$e0_4_0 + E0_fixtures_co$e0_4_1 + E0_fixtures_co$e0_4_2 + E0_fixtures_co$e0_4_3 +
#     E0_fixtures_co$e0_5_0 + E0_fixtures_co$e0_5_1 + E0_fixtures_co$e0_5_2 + E0_fixtures_co$e0_5_3 + E0_fixtures_co$e0_5_4 +
#     E0_fixtures_co$e0_6_0 + E0_fixtures_co$e0_6_1 + E0_fixtures_co$e0_6_2 + E0_fixtures_co$e0_6_3 + E0_fixtures_co$e0_6_4 +
#     E0_fixtures_co$e0_6_5
# )
#
# E0_fixtures_co$e0_H <- percent(E0_fixtures_co$e0_H, accuracy = 0.1)
#
# #Draw
# E0_fixtures_co$e0_D <- (
#
#   E0_fixtures_co$e0_0_0 + E0_fixtures_co$e0_1_1 + E0_fixtures_co$e0_2_2 + E0_fixtures_co$e0_3_3 + E0_fixtures_co$e0_4_4 +
#     E0_fixtures_co$e0_5_5 + E0_fixtures_co$e0_6_6
# )
#
# E0_fixtures_co$e0_D <- percent(E0_fixtures_co$e0_D, accuracy = 0.1)
#
# #Away
#
# E0_fixtures_co$e0_A <- (
#   E0_fixtures_co$e0_0_1 + E0_fixtures_co$e0_0_2 + E0_fixtures_co$e0_1_2 + E0_fixtures_co$e0_0_3 + E0_fixtures_co$e0_1_3 +
#     E0_fixtures_co$e0_2_3 + E0_fixtures_co$e0_0_4 + E0_fixtures_co$e0_1_4 + E0_fixtures_co$e0_2_4 + E0_fixtures_co$e0_3_4 +
#     E0_fixtures_co$e0_0_5 + E0_fixtures_co$e0_1_5 + E0_fixtures_co$e0_2_5 + E0_fixtures_co$e0_3_5 + E0_fixtures_co$e0_4_5 +
#     E0_fixtures_co$e0_0_6 + E0_fixtures_co$e0_1_6 + E0_fixtures_co$e0_2_6 + E0_fixtures_co$e0_3_6 + E0_fixtures_co$e0_4_6 +
#     E0_fixtures_co$e0_5_6
# )
#
# E0_fixtures_co$e0_A <- percent(E0_fixtures_co$e0_A, accuracy = 0.1)
#
# #ov25
# E0_fixtures_co$e0_ov25 <- (
#   E0_fixtures_co$e0_2_1 + E0_fixtures_co$e0_1_2 + E0_fixtures_co$e0_2_2 + E0_fixtures_co$e0_3_0 + E0_fixtures_co$e0_3_1 +
#     E0_fixtures_co$e0_3_2 + E0_fixtures_co$e0_0_3 + E0_fixtures_co$e0_1_3 + E0_fixtures_co$e0_2_3 + E0_fixtures_co$e0_3_3 +
#     E0_fixtures_co$e0_4_0 + E0_fixtures_co$e0_4_1 + E0_fixtures_co$e0_4_2 + E0_fixtures_co$e0_4_3 + E0_fixtures_co$e0_0_4 +
#     E0_fixtures_co$e0_1_4 + E0_fixtures_co$e0_2_4 + E0_fixtures_co$e0_3_4 + E0_fixtures_co$e0_4_4 + E0_fixtures_co$e0_5_0 +
#     E0_fixtures_co$e0_5_1 + E0_fixtures_co$e0_5_2 + E0_fixtures_co$e0_5_3 + E0_fixtures_co$e0_5_4 + E0_fixtures_co$e0_0_5 +
#     E0_fixtures_co$e0_1_5 + E0_fixtures_co$e0_2_5 + E0_fixtures_co$e0_3_5 + E0_fixtures_co$e0_4_5 + E0_fixtures_co$e0_5_5 +
#     E0_fixtures_co$e0_6_0 + E0_fixtures_co$e0_6_1 + E0_fixtures_co$e0_6_2 + E0_fixtures_co$e0_6_3 + E0_fixtures_co$e0_6_4 +
#     E0_fixtures_co$e0_6_5 + E0_fixtures_co$e0_0_6 + E0_fixtures_co$e0_1_6 + E0_fixtures_co$e0_2_6 + E0_fixtures_co$e0_3_6 +
#     E0_fixtures_co$e0_4_6 + E0_fixtures_co$e0_5_6 + E0_fixtures_co$e0_6_6
# )
# #un25
# E0_fixtures_co$e0_un25 <- (
#   E0_fixtures_co$e0_0_0 + E0_fixtures_co$e0_1_0 + E0_fixtures_co$e0_0_1 + E0_fixtures_co$e0_1_1 + E0_fixtures_co$e0_2_0 + E0_fixtures_co$e0_0_2
# )
# #odds
# E0_fixtures_co$e0_ov25_odds <- round((1/E0_fixtures_co$e0_ov25),digits = 2)
# E0_fixtures_co$e0_un25_odds <- round((1/E0_fixtures_co$e0_un25),digits = 2)
#
# E0_fixtures_co$e0_ov25_odds
# E0_fixtures_co$e0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E0_fixtures_co$e0_ov25 <- percent(E0_fixtures_co$e0_ov25, accuracy = 0.1)
#
# E0_fixtures_co$e0_un25 <- percent(E0_fixtures_co$e0_un25, accuracy = 0.1)
# E0_fixtures_co$e0_pscore <- paste(round(E0_fixtures_co$e0_xHCOC,digits = 0),round(E0_fixtures_co$e0_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #E1
# HomeTeam_e1_co <- rep(e1_teams, each = length(e1_teams))
# AwayTeam_e1_co <- rep(e1_teams, length(e1_teams))
# E1_fixtures_co <- cbind(HomeTeam_e1_co,AwayTeam_e1_co)
# E1_fixtures_co <- as.data.frame(E1_fixtures_co)
# E1_fixtures_co <- E1_fixtures_co[!E1_fixtures_co$HomeTeam_e1_co == E1_fixtures_co$AwayTeam_e1_co,]
# rownames(E1_fixtures_co) <- NULL
# E1_fixtures_co$Div <- "E1"
# E1_fixtures_co <- E1_fixtures_co[,c(3,1,2)]
#
# E1_fixtures_co$avg_HCO_e1 <- e1_avg_HCO
#
# E1_fixtures_co$e1_homecoas <- rep(e1_home_coas,each = length(e1_teams)-1)
#
# e1_awaycods_lookup <- cbind(e1_teams,e1_away_cods)
#
# e1_awaycods_lookup <- as.data.frame(e1_awaycods_lookup)
#
# colnames(e1_awaycods_lookup) <- c("AwayTeam_e1_co","e1_awaycods")
#
#
# require('RH2')
# E1_fixtures_co$e1_awaycods <- sqldf("SELECT e1_awaycods_lookup.e1_awaycods FROM e1_awaycods_lookup INNER JOIN E1_fixtures_co ON e1_awaycods_lookup.AwayTeam_e1_co = E1_fixtures_co.AwayTeam_e1_co")
#
# E1_fixtures_co$avg_ACO_e1 <- e1_avg_ACO
#
# e1_awaycoas_lookup <- cbind(e1_teams,e1_away_coas)
#
# e1_awaycoas_lookup <- as.data.frame(e1_awaycoas_lookup)
#
# colnames(e1_awaycoas_lookup) <- c("AwayTeam_e1_co","e1_awaycoas")
#
# E1_fixtures_co$e1_awaycoas <- sqldf("SELECT e1_awaycoas_lookup.e1_awaycoas FROM e1_awaycoas_lookup INNER JOIN E1_fixtures_co ON e1_awaycoas_lookup.AwayTeam_e1_co = E1_fixtures_co.AwayTeam_e1_co")
#
# E1_fixtures_co$e1_homecods <- rep(e1_home_cods,each = length(e1_teams)-1)
#
# E1_fixtures_co$e1_awaycods <- as.numeric(unlist(E1_fixtures_co$e1_awaycods))
# #xGH
# E1_fixtures_co$e1_xHCOC <- E1_fixtures_co$avg_HCO_e1 * E1_fixtures_co$e1_homecoas * E1_fixtures_co$e1_awaycods
# #xGA
#
# E1_fixtures_co$e1_awaycoas <- as.numeric(unlist(E1_fixtures_co$e1_awaycoas))
#
# E1_fixtures_co$e1_xACOC <- E1_fixtures_co$avg_ACO_e1 * E1_fixtures_co$e1_awaycoas * E1_fixtures_co$e1_homecods
#
# E1_fixtures_co$e1_0_0 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_0 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_1 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_1 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_0 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_2 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_2 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_1 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_2 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_3 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_0 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_1 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_2 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_3 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_3 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_3 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_4 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_0 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_1 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_2 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_3 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_4 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_4 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_4 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_4 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_5 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_0 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_1 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_2 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_3 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_4 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_5 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_5 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_5 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_5 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_5 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_6 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_0 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(0,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_1 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(1,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_2 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(2,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_3 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(3,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_4 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(4,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_6_5 <- round(stats::dpois(6,E1_fixtures_co$e1_xHCOC) * stats::dpois(5,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_0_6 <- round(stats::dpois(0,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_1_6 <- round(stats::dpois(1,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_2_6 <- round(stats::dpois(2,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_3_6 <- round(stats::dpois(3,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_4_6 <- round(stats::dpois(4,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# E1_fixtures_co$e1_5_6 <- round(stats::dpois(5,E1_fixtures_co$e1_xHCOC) * stats::dpois(6,E1_fixtures_co$e1_xACOC), digits = 4)
# #Home win
# E1_fixtures_co$e1_H <- (
#   E1_fixtures_co$e1_1_0 + E1_fixtures_co$e1_2_0 + E1_fixtures_co$e1_2_1 + E1_fixtures_co$e1_3_0 + E1_fixtures_co$e1_3_1 +
#     E1_fixtures_co$e1_3_2 + E1_fixtures_co$e1_4_0 + E1_fixtures_co$e1_4_1 + E1_fixtures_co$e1_4_2 + E1_fixtures_co$e1_4_3 +
#     E1_fixtures_co$e1_5_0 + E1_fixtures_co$e1_5_1 + E1_fixtures_co$e1_5_2 + E1_fixtures_co$e1_5_3 + E1_fixtures_co$e1_5_4 +
#     E1_fixtures_co$e1_6_0 + E1_fixtures_co$e1_6_1 + E1_fixtures_co$e1_6_2 + E1_fixtures_co$e1_6_3 + E1_fixtures_co$e1_6_4 +
#     E1_fixtures_co$e1_6_5
# )
#
# E1_fixtures_co$e1_H <- percent(E1_fixtures_co$e1_H, accuracy = 0.1)
#
# #Draw
# E1_fixtures_co$e1_D <- (
#
#   E1_fixtures_co$e1_0_0 + E1_fixtures_co$e1_1_1 + E1_fixtures_co$e1_2_2 + E1_fixtures_co$e1_3_3 + E1_fixtures_co$e1_4_4 +
#     E1_fixtures_co$e1_5_5 + E1_fixtures_co$e1_6_6
# )
#
# E1_fixtures_co$e1_D <- percent(E1_fixtures_co$e1_D, accuracy = 0.1)
#
# #Away
#
# E1_fixtures_co$e1_A <- (
#   E1_fixtures_co$e1_0_1 + E1_fixtures_co$e1_0_2 + E1_fixtures_co$e1_1_2 + E1_fixtures_co$e1_0_3 + E1_fixtures_co$e1_1_3 +
#     E1_fixtures_co$e1_2_3 + E1_fixtures_co$e1_0_4 + E1_fixtures_co$e1_1_4 + E1_fixtures_co$e1_2_4 + E1_fixtures_co$e1_3_4 +
#     E1_fixtures_co$e1_0_5 + E1_fixtures_co$e1_1_5 + E1_fixtures_co$e1_2_5 + E1_fixtures_co$e1_3_5 + E1_fixtures_co$e1_4_5 +
#     E1_fixtures_co$e1_0_6 + E1_fixtures_co$e1_1_6 + E1_fixtures_co$e1_2_6 + E1_fixtures_co$e1_3_6 + E1_fixtures_co$e1_4_6 +
#     E1_fixtures_co$e1_5_6
# )
#
# E1_fixtures_co$e1_A <- percent(E1_fixtures_co$e1_A, accuracy = 0.1)
#
# #ov25
# E1_fixtures_co$e1_ov25 <- (
#   E1_fixtures_co$e1_2_1 + E1_fixtures_co$e1_1_2 + E1_fixtures_co$e1_2_2 + E1_fixtures_co$e1_3_0 + E1_fixtures_co$e1_3_1 +
#     E1_fixtures_co$e1_3_2 + E1_fixtures_co$e1_0_3 + E1_fixtures_co$e1_1_3 + E1_fixtures_co$e1_2_3 + E1_fixtures_co$e1_3_3 +
#     E1_fixtures_co$e1_4_0 + E1_fixtures_co$e1_4_1 + E1_fixtures_co$e1_4_2 + E1_fixtures_co$e1_4_3 + E1_fixtures_co$e1_0_4 +
#     E1_fixtures_co$e1_1_4 + E1_fixtures_co$e1_2_4 + E1_fixtures_co$e1_3_4 + E1_fixtures_co$e1_4_4 + E1_fixtures_co$e1_5_0 +
#     E1_fixtures_co$e1_5_1 + E1_fixtures_co$e1_5_2 + E1_fixtures_co$e1_5_3 + E1_fixtures_co$e1_5_4 + E1_fixtures_co$e1_0_5 +
#     E1_fixtures_co$e1_1_5 + E1_fixtures_co$e1_2_5 + E1_fixtures_co$e1_3_5 + E1_fixtures_co$e1_4_5 + E1_fixtures_co$e1_5_5 +
#     E1_fixtures_co$e1_6_0 + E1_fixtures_co$e1_6_1 + E1_fixtures_co$e1_6_2 + E1_fixtures_co$e1_6_3 + E1_fixtures_co$e1_6_4 +
#     E1_fixtures_co$e1_6_5 + E1_fixtures_co$e1_0_6 + E1_fixtures_co$e1_1_6 + E1_fixtures_co$e1_2_6 + E1_fixtures_co$e1_3_6 +
#     E1_fixtures_co$e1_4_6 + E1_fixtures_co$e1_5_6 + E1_fixtures_co$e1_6_6
# )
# #un25
# E1_fixtures_co$e1_un25 <- (
#   E1_fixtures_co$e1_0_0 + E1_fixtures_co$e1_1_0 + E1_fixtures_co$e1_0_1 + E1_fixtures_co$e1_1_1 + E1_fixtures_co$e1_2_0 + E1_fixtures_co$e1_0_2
# )
# #odds
# E1_fixtures_co$e1_ov25_odds <- round((1/E1_fixtures_co$e1_ov25),digits = 2)
# E1_fixtures_co$e1_un25_odds <- round((1/E1_fixtures_co$e1_un25),digits = 2)
#
# E1_fixtures_co$e1_ov25_odds
# E1_fixtures_co$e1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E1_fixtures_co$e1_ov25 <- percent(E1_fixtures_co$e1_ov25, accuracy = 0.1)
#
# E1_fixtures_co$e1_un25 <- percent(E1_fixtures_co$e1_un25, accuracy = 0.1)
# E1_fixtures_co$e1_pscore <- paste(round(E1_fixtures_co$e1_xHCOC,digits = 0),round(E1_fixtures_co$e1_xACOC,digits = 0),sep = "-")
#
# #write out
# #write.xlsx(E1_fixtures,'Divisions/E1.xlsx',sheetName = "E1", append = TRUE)
# #################################################################################################################
# #E2
# HomeTeam_e2_co <- rep(e2_teams, each = length(e2_teams))
# AwayTeam_e2_co <- rep(e2_teams, length(e2_teams))
# E2_fixtures_co <- cbind(HomeTeam_e2_co,AwayTeam_e2_co)
# E2_fixtures_co <- as.data.frame(E2_fixtures_co)
# E2_fixtures_co <- E2_fixtures_co[!E2_fixtures_co$HomeTeam_e2_co == E2_fixtures_co$AwayTeam_e2_co,]
# rownames(E2_fixtures_co) <- NULL
# E2_fixtures_co$Div <- "E2"
# E2_fixtures_co <- E2_fixtures_co[,c(3,1,2)]
#
# E2_fixtures_co$avg_HCO_e2 <- e2_avg_HCO
#
# E2_fixtures_co$e2_homecoas <- rep(e2_home_coas,each = length(e2_teams)-1)
#
# e2_awaycods_lookup <- cbind(e2_teams,e2_away_cods)
#
# e2_awaycods_lookup <- as.data.frame(e2_awaycods_lookup)
#
# colnames(e2_awaycods_lookup) <- c("AwayTeam_e2_co","e2_awaycods")
#
#
# require('RH2')
# E2_fixtures_co$e2_awaycods <- sqldf("SELECT e2_awaycods_lookup.e2_awaycods FROM e2_awaycods_lookup INNER JOIN E2_fixtures_co ON e2_awaycods_lookup.AwayTeam_e2_co = E2_fixtures_co.AwayTeam_e2_co")
#
# E2_fixtures_co$avg_ACO_e2 <- e2_avg_ACO
#
# e2_awaycoas_lookup <- cbind(e2_teams,e2_away_coas)
#
# e2_awaycoas_lookup <- as.data.frame(e2_awaycoas_lookup)
#
# colnames(e2_awaycoas_lookup) <- c("AwayTeam_e2_co","e2_awaycoas")
#
# E2_fixtures_co$e2_awaycoas <- sqldf("SELECT e2_awaycoas_lookup.e2_awaycoas FROM e2_awaycoas_lookup INNER JOIN E2_fixtures_co ON e2_awaycoas_lookup.AwayTeam_e2_co = E2_fixtures_co.AwayTeam_e2_co")
#
# E2_fixtures_co$e2_homecods <- rep(e2_home_cods,each = length(e2_teams)-1)
#
# E2_fixtures_co$e2_awaycods <- as.numeric(unlist(E2_fixtures_co$e2_awaycods))
# #xGH
# E2_fixtures_co$e2_xHCOC <- E2_fixtures_co$avg_HCO_e2 * E2_fixtures_co$e2_homecoas * E2_fixtures_co$e2_awaycods
# #xGA
#
# E2_fixtures_co$e2_awaycoas <- as.numeric(unlist(E2_fixtures_co$e2_awaycoas))
#
# E2_fixtures_co$e2_xACOC <- E2_fixtures_co$avg_ACO_e2 * E2_fixtures_co$e2_awaycoas * E2_fixtures_co$e2_homecods
#
# E2_fixtures_co$e2_0_0 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_0 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_1 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_1 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_0 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_2 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_2 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_1 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_2 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_3 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_0 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_1 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_2 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_3 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_3 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_3 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_4 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_0 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_1 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_2 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_3 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_4 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_4 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_4 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_4 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_5 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_0 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_1 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_2 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_3 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_4 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_5 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_5 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_5 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_5 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_5 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_6 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_0 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(0,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_1 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(1,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_2 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(2,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_3 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(3,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_4 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(4,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_6_5 <- round(stats::dpois(6,E2_fixtures_co$e2_xHCOC) * stats::dpois(5,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_0_6 <- round(stats::dpois(0,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_1_6 <- round(stats::dpois(1,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_2_6 <- round(stats::dpois(2,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_3_6 <- round(stats::dpois(3,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_4_6 <- round(stats::dpois(4,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# E2_fixtures_co$e2_5_6 <- round(stats::dpois(5,E2_fixtures_co$e2_xHCOC) * stats::dpois(6,E2_fixtures_co$e2_xACOC), digits = 4)
# #Home win
# E2_fixtures_co$e2_H <- (
#   E2_fixtures_co$e2_1_0 + E2_fixtures_co$e2_2_0 + E2_fixtures_co$e2_2_1 + E2_fixtures_co$e2_3_0 + E2_fixtures_co$e2_3_1 +
#     E2_fixtures_co$e2_3_2 + E2_fixtures_co$e2_4_0 + E2_fixtures_co$e2_4_1 + E2_fixtures_co$e2_4_2 + E2_fixtures_co$e2_4_3 +
#     E2_fixtures_co$e2_5_0 + E2_fixtures_co$e2_5_1 + E2_fixtures_co$e2_5_2 + E2_fixtures_co$e2_5_3 + E2_fixtures_co$e2_5_4 +
#     E2_fixtures_co$e2_6_0 + E2_fixtures_co$e2_6_1 + E2_fixtures_co$e2_6_2 + E2_fixtures_co$e2_6_3 + E2_fixtures_co$e2_6_4 +
#     E2_fixtures_co$e2_6_5
# )
#
# E2_fixtures_co$e2_H <- percent(E2_fixtures_co$e2_H, accuracy = 0.1)
#
# #Draw
# E2_fixtures_co$e2_D <- (
#
#   E2_fixtures_co$e2_0_0 + E2_fixtures_co$e2_1_1 + E2_fixtures_co$e2_2_2 + E2_fixtures_co$e2_3_3 + E2_fixtures_co$e2_4_4 +
#     E2_fixtures_co$e2_5_5 + E2_fixtures_co$e2_6_6
# )
#
# E2_fixtures_co$e2_D <- percent(E2_fixtures_co$e2_D, accuracy = 0.1)
#
# #Away
#
# E2_fixtures_co$e2_A <- (
#   E2_fixtures_co$e2_0_1 + E2_fixtures_co$e2_0_2 + E2_fixtures_co$e2_1_2 + E2_fixtures_co$e2_0_3 + E2_fixtures_co$e2_1_3 +
#     E2_fixtures_co$e2_2_3 + E2_fixtures_co$e2_0_4 + E2_fixtures_co$e2_1_4 + E2_fixtures_co$e2_2_4 + E2_fixtures_co$e2_3_4 +
#     E2_fixtures_co$e2_0_5 + E2_fixtures_co$e2_1_5 + E2_fixtures_co$e2_2_5 + E2_fixtures_co$e2_3_5 + E2_fixtures_co$e2_4_5 +
#     E2_fixtures_co$e2_0_6 + E2_fixtures_co$e2_1_6 + E2_fixtures_co$e2_2_6 + E2_fixtures_co$e2_3_6 + E2_fixtures_co$e2_4_6 +
#     E2_fixtures_co$e2_5_6
# )
#
# E2_fixtures_co$e2_A <- percent(E2_fixtures_co$e2_A, accuracy = 0.1)
#
# #ov25
# E2_fixtures_co$e2_ov25 <- (
#   E2_fixtures_co$e2_2_1 + E2_fixtures_co$e2_1_2 + E2_fixtures_co$e2_2_2 + E2_fixtures_co$e2_3_0 + E2_fixtures_co$e2_3_1 +
#     E2_fixtures_co$e2_3_2 + E2_fixtures_co$e2_0_3 + E2_fixtures_co$e2_1_3 + E2_fixtures_co$e2_2_3 + E2_fixtures_co$e2_3_3 +
#     E2_fixtures_co$e2_4_0 + E2_fixtures_co$e2_4_1 + E2_fixtures_co$e2_4_2 + E2_fixtures_co$e2_4_3 + E2_fixtures_co$e2_0_4 +
#     E2_fixtures_co$e2_1_4 + E2_fixtures_co$e2_2_4 + E2_fixtures_co$e2_3_4 + E2_fixtures_co$e2_4_4 + E2_fixtures_co$e2_5_0 +
#     E2_fixtures_co$e2_5_1 + E2_fixtures_co$e2_5_2 + E2_fixtures_co$e2_5_3 + E2_fixtures_co$e2_5_4 + E2_fixtures_co$e2_0_5 +
#     E2_fixtures_co$e2_1_5 + E2_fixtures_co$e2_2_5 + E2_fixtures_co$e2_3_5 + E2_fixtures_co$e2_4_5 + E2_fixtures_co$e2_5_5 +
#     E2_fixtures_co$e2_6_0 + E2_fixtures_co$e2_6_1 + E2_fixtures_co$e2_6_2 + E2_fixtures_co$e2_6_3 + E2_fixtures_co$e2_6_4 +
#     E2_fixtures_co$e2_6_5 + E2_fixtures_co$e2_0_6 + E2_fixtures_co$e2_1_6 + E2_fixtures_co$e2_2_6 + E2_fixtures_co$e2_3_6 +
#     E2_fixtures_co$e2_4_6 + E2_fixtures_co$e2_5_6 + E2_fixtures_co$e2_6_6
# )
# #un25
# E2_fixtures_co$e2_un25 <- (
#   E2_fixtures_co$e2_0_0 + E2_fixtures_co$e2_1_0 + E2_fixtures_co$e2_0_1 + E2_fixtures_co$e2_1_1 + E2_fixtures_co$e2_2_0 + E2_fixtures_co$e2_0_2
# )
# #odds
# E2_fixtures_co$e2_ov25_odds <- round((1/E2_fixtures_co$e2_ov25),digits = 2)
# E2_fixtures_co$e2_un25_odds <- round((1/E2_fixtures_co$e2_un25),digits = 2)
#
# E2_fixtures_co$e2_ov25_odds
# E2_fixtures_co$e2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E2_fixtures_co$e2_ov25 <- percent(E2_fixtures_co$e2_ov25, accuracy = 0.1)
#
# E2_fixtures_co$e2_un25 <- percent(E2_fixtures_co$e2_un25, accuracy = 0.1)
# E2_fixtures_co$e2_pscore <- paste(round(E2_fixtures_co$e2_xHCOC,digits = 0),round(E2_fixtures_co$e2_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(E2_fixtures,'Divisions/E2.xlsx',sheetName = "E2", append = TRUE)
# #################################################################################################################
# #E3
# HomeTeam_e3_co <- rep(e3_teams, each = length(e3_teams))
# AwayTeam_e3_co <- rep(e3_teams, length(e3_teams))
# E3_fixtures_co <- cbind(HomeTeam_e3_co,AwayTeam_e3_co)
# E3_fixtures_co <- as.data.frame(E3_fixtures_co)
# E3_fixtures_co <- E3_fixtures_co[!E3_fixtures_co$HomeTeam_e3_co == E3_fixtures_co$AwayTeam_e3_co,]
# rownames(E3_fixtures_co) <- NULL
# E3_fixtures_co$Div <- "E3"
# E3_fixtures_co <- E3_fixtures_co[,c(3,1,2)]
#
# E3_fixtures_co$avg_HCO_e3 <- e3_avg_HCO
#
# E3_fixtures_co$e3_homecoas <- rep(e3_home_coas,each = length(e3_teams)-1)
#
# e3_awaycods_lookup <- cbind(e3_teams,e3_away_cods)
#
# e3_awaycods_lookup <- as.data.frame(e3_awaycods_lookup)
#
# colnames(e3_awaycods_lookup) <- c("AwayTeam_e3_co","e3_awaycods")
#
#
# require('RH2')
# E3_fixtures_co$e3_awaycods <- sqldf("SELECT e3_awaycods_lookup.e3_awaycods FROM e3_awaycods_lookup INNER JOIN E3_fixtures_co ON e3_awaycods_lookup.AwayTeam_e3_co = E3_fixtures_co.AwayTeam_e3_co")
#
# E3_fixtures_co$avg_ACO_e3 <- e3_avg_ACO
#
# e3_awaycoas_lookup <- cbind(e3_teams,e3_away_coas)
#
# e3_awaycoas_lookup <- as.data.frame(e3_awaycoas_lookup)
#
# colnames(e3_awaycoas_lookup) <- c("AwayTeam_e3_co","e3_awaycoas")
#
# E3_fixtures_co$e3_awaycoas <- sqldf("SELECT e3_awaycoas_lookup.e3_awaycoas FROM e3_awaycoas_lookup INNER JOIN E3_fixtures_co ON e3_awaycoas_lookup.AwayTeam_e3_co = E3_fixtures_co.AwayTeam_e3_co")
#
# E3_fixtures_co$e3_homecods <- rep(e3_home_cods,each = length(e3_teams)-1)
#
# E3_fixtures_co$e3_awaycods <- as.numeric(unlist(E3_fixtures_co$e3_awaycods))
# #xGH
# E3_fixtures_co$e3_xHCOC <- E3_fixtures_co$avg_HCO_e3 * E3_fixtures_co$e3_homecoas * E3_fixtures_co$e3_awaycods
# #xGA
#
# E3_fixtures_co$e3_awaycoas <- as.numeric(unlist(E3_fixtures_co$e3_awaycoas))
#
# E3_fixtures_co$e3_xACOC <- E3_fixtures_co$avg_ACO_e3 * E3_fixtures_co$e3_awaycoas * E3_fixtures_co$e3_homecods
#
# E3_fixtures_co$e3_0_0 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_0 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_1 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_1 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_0 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_2 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_2 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_1 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_2 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_3 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_0 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_1 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_2 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_3 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_3 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_3 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_4 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_0 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_1 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_2 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_3 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_4 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_4 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_4 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_4 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_5 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_0 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_1 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_2 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_3 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_4 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_5 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_5 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_5 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_5 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_5 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_6 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_0 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(0,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_1 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(1,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_2 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(2,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_3 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(3,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_4 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(4,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_6_5 <- round(stats::dpois(6,E3_fixtures_co$e3_xHCOC) * stats::dpois(5,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_0_6 <- round(stats::dpois(0,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_1_6 <- round(stats::dpois(1,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_2_6 <- round(stats::dpois(2,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_3_6 <- round(stats::dpois(3,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_4_6 <- round(stats::dpois(4,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# E3_fixtures_co$e3_5_6 <- round(stats::dpois(5,E3_fixtures_co$e3_xHCOC) * stats::dpois(6,E3_fixtures_co$e3_xACOC), digits = 4)
# #Home win
# E3_fixtures_co$e3_H <- (
#   E3_fixtures_co$e3_1_0 + E3_fixtures_co$e3_2_0 + E3_fixtures_co$e3_2_1 + E3_fixtures_co$e3_3_0 + E3_fixtures_co$e3_3_1 +
#     E3_fixtures_co$e3_3_2 + E3_fixtures_co$e3_4_0 + E3_fixtures_co$e3_4_1 + E3_fixtures_co$e3_4_2 + E3_fixtures_co$e3_4_3 +
#     E3_fixtures_co$e3_5_0 + E3_fixtures_co$e3_5_1 + E3_fixtures_co$e3_5_2 + E3_fixtures_co$e3_5_3 + E3_fixtures_co$e3_5_4 +
#     E3_fixtures_co$e3_6_0 + E3_fixtures_co$e3_6_1 + E3_fixtures_co$e3_6_2 + E3_fixtures_co$e3_6_3 + E3_fixtures_co$e3_6_4 +
#     E3_fixtures_co$e3_6_5
# )
#
# E3_fixtures_co$e3_H <- percent(E3_fixtures_co$e3_H, accuracy = 0.1)
#
# #Draw
# E3_fixtures_co$e3_D <- (
#
#   E3_fixtures_co$e3_0_0 + E3_fixtures_co$e3_1_1 + E3_fixtures_co$e3_2_2 + E3_fixtures_co$e3_3_3 + E3_fixtures_co$e3_4_4 +
#     E3_fixtures_co$e3_5_5 + E3_fixtures_co$e3_6_6
# )
#
# E3_fixtures_co$e3_D <- percent(E3_fixtures_co$e3_D, accuracy = 0.1)
#
# #Away
#
# E3_fixtures_co$e3_A <- (
#   E3_fixtures_co$e3_0_1 + E3_fixtures_co$e3_0_2 + E3_fixtures_co$e3_1_2 + E3_fixtures_co$e3_0_3 + E3_fixtures_co$e3_1_3 +
#     E3_fixtures_co$e3_2_3 + E3_fixtures_co$e3_0_4 + E3_fixtures_co$e3_1_4 + E3_fixtures_co$e3_2_4 + E3_fixtures_co$e3_3_4 +
#     E3_fixtures_co$e3_0_5 + E3_fixtures_co$e3_1_5 + E3_fixtures_co$e3_2_5 + E3_fixtures_co$e3_3_5 + E3_fixtures_co$e3_4_5 +
#     E3_fixtures_co$e3_0_6 + E3_fixtures_co$e3_1_6 + E3_fixtures_co$e3_2_6 + E3_fixtures_co$e3_3_6 + E3_fixtures_co$e3_4_6 +
#     E3_fixtures_co$e3_5_6
# )
#
# E3_fixtures_co$e3_A <- percent(E3_fixtures_co$e3_A, accuracy = 0.1)
#
# #ov25
# E3_fixtures_co$e3_ov25 <- (
#   E3_fixtures_co$e3_2_1 + E3_fixtures_co$e3_1_2 + E3_fixtures_co$e3_2_2 + E3_fixtures_co$e3_3_0 + E3_fixtures_co$e3_3_1 +
#     E3_fixtures_co$e3_3_2 + E3_fixtures_co$e3_0_3 + E3_fixtures_co$e3_1_3 + E3_fixtures_co$e3_2_3 + E3_fixtures_co$e3_3_3 +
#     E3_fixtures_co$e3_4_0 + E3_fixtures_co$e3_4_1 + E3_fixtures_co$e3_4_2 + E3_fixtures_co$e3_4_3 + E3_fixtures_co$e3_0_4 +
#     E3_fixtures_co$e3_1_4 + E3_fixtures_co$e3_2_4 + E3_fixtures_co$e3_3_4 + E3_fixtures_co$e3_4_4 + E3_fixtures_co$e3_5_0 +
#     E3_fixtures_co$e3_5_1 + E3_fixtures_co$e3_5_2 + E3_fixtures_co$e3_5_3 + E3_fixtures_co$e3_5_4 + E3_fixtures_co$e3_0_5 +
#     E3_fixtures_co$e3_1_5 + E3_fixtures_co$e3_2_5 + E3_fixtures_co$e3_3_5 + E3_fixtures_co$e3_4_5 + E3_fixtures_co$e3_5_5 +
#     E3_fixtures_co$e3_6_0 + E3_fixtures_co$e3_6_1 + E3_fixtures_co$e3_6_2 + E3_fixtures_co$e3_6_3 + E3_fixtures_co$e3_6_4 +
#     E3_fixtures_co$e3_6_5 + E3_fixtures_co$e3_0_6 + E3_fixtures_co$e3_1_6 + E3_fixtures_co$e3_2_6 + E3_fixtures_co$e3_3_6 +
#     E3_fixtures_co$e3_4_6 + E3_fixtures_co$e3_5_6 + E3_fixtures_co$e3_6_6
# )
# #un25
# E3_fixtures_co$e3_un25 <- (
#   E3_fixtures_co$e3_0_0 + E3_fixtures_co$e3_1_0 + E3_fixtures_co$e3_0_1 + E3_fixtures_co$e3_1_1 + E3_fixtures_co$e3_2_0 + E3_fixtures_co$e3_0_2
# )
# #odds
# E3_fixtures_co$e3_ov25_odds <- round((1/E3_fixtures_co$e3_ov25),digits = 2)
# E3_fixtures_co$e3_un25_odds <- round((1/E3_fixtures_co$e3_un25),digits = 2)
#
# E3_fixtures_co$e3_ov25_odds
# E3_fixtures_co$e3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E3_fixtures_co$e3_ov25 <- percent(E3_fixtures_co$e3_ov25, accuracy = 0.1)
#
# E3_fixtures_co$e3_un25 <- percent(E3_fixtures_co$e3_un25, accuracy = 0.1)
# E3_fixtures_co$e3_pscore <- paste(round(E3_fixtures_co$e3_xHCOC,digits = 0),round(E3_fixtures_co$e3_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(E3_fixtures,'Divisions/E3.xlsx',sheetName = "E3", append = TRUE)
# #################################################################################################################
# #EC
# HomeTeam_ec_co <- rep(ec_teams, each = length(ec_teams))
# AwayTeam_ec_co <- rep(ec_teams, length(ec_teams))
# EC_fixtures_co <- cbind(HomeTeam_ec_co,AwayTeam_ec_co)
# EC_fixtures_co <- as.data.frame(EC_fixtures_co)
# EC_fixtures_co <- EC_fixtures_co[!EC_fixtures_co$HomeTeam_ec_co == EC_fixtures_co$AwayTeam_ec_co,]
# rownames(EC_fixtures_co) <- NULL
# EC_fixtures_co$Div <- "EC"
# EC_fixtures_co <- EC_fixtures_co[,c(3,1,2)]
#
# EC_fixtures_co$avg_HCO_ec <- ec_avg_HCO
#
# EC_fixtures_co$ec_homecoas <- rep(ec_home_coas,each = length(ec_teams)-1)
#
# ec_awaycods_lookup <- cbind(ec_teams,ec_away_cods)
#
# ec_awaycods_lookup <- as.data.frame(ec_awaycods_lookup)
#
# colnames(ec_awaycods_lookup) <- c("AwayTeam_ec_co","ec_awaycods")
#
#
# require('RH2')
# EC_fixtures_co$ec_awaycods <- sqldf("SELECT ec_awaycods_lookup.ec_awaycods FROM ec_awaycods_lookup INNER JOIN EC_fixtures_co ON ec_awaycods_lookup.AwayTeam_ec_co = EC_fixtures_co.AwayTeam_ec_co")
#
# EC_fixtures_co$avg_ACO_ec <- ec_avg_ACO
#
# ec_awaycoas_lookup <- cbind(ec_teams,ec_away_coas)
#
# ec_awaycoas_lookup <- as.data.frame(ec_awaycoas_lookup)
#
# colnames(ec_awaycoas_lookup) <- c("AwayTeam_ec_co","ec_awaycoas")
#
# EC_fixtures_co$ec_awaycoas <- sqldf("SELECT ec_awaycoas_lookup.ec_awaycoas FROM ec_awaycoas_lookup INNER JOIN EC_fixtures_co ON ec_awaycoas_lookup.AwayTeam_ec_co = EC_fixtures_co.AwayTeam_ec_co")
#
# EC_fixtures_co$ec_homecods <- rep(ec_home_cods,each = length(ec_teams)-1)
#
# EC_fixtures_co$ec_awaycods <- as.numeric(unlist(EC_fixtures_co$ec_awaycods))
# #xGH
# EC_fixtures_co$ec_xHCOC <- EC_fixtures_co$avg_HCO_ec * EC_fixtures_co$ec_homecoas * EC_fixtures_co$ec_awaycods
# #xGA
#
# EC_fixtures_co$ec_awaycoas <- as.numeric(unlist(EC_fixtures_co$ec_awaycoas))
#
# EC_fixtures_co$ec_xACOC <- EC_fixtures_co$avg_ACO_ec * EC_fixtures_co$ec_awaycoas * EC_fixtures_co$ec_homecods
#
# EC_fixtures_co$ec_0_0 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_0 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_1 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_1 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_0 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_2 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_2 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_1 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_2 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_3 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_0 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_1 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_2 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_3 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_3 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_3 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_4 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_0 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_1 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_2 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_3 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_4 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_4 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_4 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_4 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_5 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_0 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_1 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_2 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_3 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_4 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_5 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_5 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_5 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_5 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_5 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_6 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_0 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(0,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_1 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(1,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_2 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(2,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_3 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(3,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_4 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(4,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_6_5 <- round(stats::dpois(6,EC_fixtures_co$ec_xHCOC) * stats::dpois(5,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_0_6 <- round(stats::dpois(0,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_1_6 <- round(stats::dpois(1,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_2_6 <- round(stats::dpois(2,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_3_6 <- round(stats::dpois(3,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_4_6 <- round(stats::dpois(4,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# EC_fixtures_co$ec_5_6 <- round(stats::dpois(5,EC_fixtures_co$ec_xHCOC) * stats::dpois(6,EC_fixtures_co$ec_xACOC), digits = 4)
# #Home win
# EC_fixtures_co$ec_H <- (
#   EC_fixtures_co$ec_1_0 + EC_fixtures_co$ec_2_0 + EC_fixtures_co$ec_2_1 + EC_fixtures_co$ec_3_0 + EC_fixtures_co$ec_3_1 +
#     EC_fixtures_co$ec_3_2 + EC_fixtures_co$ec_4_0 + EC_fixtures_co$ec_4_1 + EC_fixtures_co$ec_4_2 + EC_fixtures_co$ec_4_3 +
#     EC_fixtures_co$ec_5_0 + EC_fixtures_co$ec_5_1 + EC_fixtures_co$ec_5_2 + EC_fixtures_co$ec_5_3 + EC_fixtures_co$ec_5_4 +
#     EC_fixtures_co$ec_6_0 + EC_fixtures_co$ec_6_1 + EC_fixtures_co$ec_6_2 + EC_fixtures_co$ec_6_3 + EC_fixtures_co$ec_6_4 +
#     EC_fixtures_co$ec_6_5
# )
#
# EC_fixtures_co$ec_H <- percent(EC_fixtures_co$ec_H, accuracy = 0.1)
#
# #Draw
# EC_fixtures_co$ec_D <- (
#
#   EC_fixtures_co$ec_0_0 + EC_fixtures_co$ec_1_1 + EC_fixtures_co$ec_2_2 + EC_fixtures_co$ec_3_3 + EC_fixtures_co$ec_4_4 +
#     EC_fixtures_co$ec_5_5 + EC_fixtures_co$ec_6_6
# )
#
# EC_fixtures_co$ec_D <- percent(EC_fixtures_co$ec_D, accuracy = 0.1)
#
# #Away
#
# EC_fixtures_co$ec_A <- (
#   EC_fixtures_co$ec_0_1 + EC_fixtures_co$ec_0_2 + EC_fixtures_co$ec_1_2 + EC_fixtures_co$ec_0_3 + EC_fixtures_co$ec_1_3 +
#     EC_fixtures_co$ec_2_3 + EC_fixtures_co$ec_0_4 + EC_fixtures_co$ec_1_4 + EC_fixtures_co$ec_2_4 + EC_fixtures_co$ec_3_4 +
#     EC_fixtures_co$ec_0_5 + EC_fixtures_co$ec_1_5 + EC_fixtures_co$ec_2_5 + EC_fixtures_co$ec_3_5 + EC_fixtures_co$ec_4_5 +
#     EC_fixtures_co$ec_0_6 + EC_fixtures_co$ec_1_6 + EC_fixtures_co$ec_2_6 + EC_fixtures_co$ec_3_6 + EC_fixtures_co$ec_4_6 +
#     EC_fixtures_co$ec_5_6
# )
#
# EC_fixtures_co$ec_A <- percent(EC_fixtures_co$ec_A, accuracy = 0.1)
#
# #ov25
# EC_fixtures_co$ec_ov25 <- (
#   EC_fixtures_co$ec_2_1 + EC_fixtures_co$ec_1_2 + EC_fixtures_co$ec_2_2 + EC_fixtures_co$ec_3_0 + EC_fixtures_co$ec_3_1 +
#     EC_fixtures_co$ec_3_2 + EC_fixtures_co$ec_0_3 + EC_fixtures_co$ec_1_3 + EC_fixtures_co$ec_2_3 + EC_fixtures_co$ec_3_3 +
#     EC_fixtures_co$ec_4_0 + EC_fixtures_co$ec_4_1 + EC_fixtures_co$ec_4_2 + EC_fixtures_co$ec_4_3 + EC_fixtures_co$ec_0_4 +
#     EC_fixtures_co$ec_1_4 + EC_fixtures_co$ec_2_4 + EC_fixtures_co$ec_3_4 + EC_fixtures_co$ec_4_4 + EC_fixtures_co$ec_5_0 +
#     EC_fixtures_co$ec_5_1 + EC_fixtures_co$ec_5_2 + EC_fixtures_co$ec_5_3 + EC_fixtures_co$ec_5_4 + EC_fixtures_co$ec_0_5 +
#     EC_fixtures_co$ec_1_5 + EC_fixtures_co$ec_2_5 + EC_fixtures_co$ec_3_5 + EC_fixtures_co$ec_4_5 + EC_fixtures_co$ec_5_5 +
#     EC_fixtures_co$ec_6_0 + EC_fixtures_co$ec_6_1 + EC_fixtures_co$ec_6_2 + EC_fixtures_co$ec_6_3 + EC_fixtures_co$ec_6_4 +
#     EC_fixtures_co$ec_6_5 + EC_fixtures_co$ec_0_6 + EC_fixtures_co$ec_1_6 + EC_fixtures_co$ec_2_6 + EC_fixtures_co$ec_3_6 +
#     EC_fixtures_co$ec_4_6 + EC_fixtures_co$ec_5_6 + EC_fixtures_co$ec_6_6
# )
# #un25
# EC_fixtures_co$ec_un25 <- (
#   EC_fixtures_co$ec_0_0 + EC_fixtures_co$ec_1_0 + EC_fixtures_co$ec_0_1 + EC_fixtures_co$ec_1_1 + EC_fixtures_co$ec_2_0 + EC_fixtures_co$ec_0_2
# )
# #odds
# EC_fixtures_co$ec_ov25_odds <- round((1/EC_fixtures_co$ec_ov25),digits = 2)
# EC_fixtures_co$ec_un25_odds <- round((1/EC_fixtures_co$ec_un25),digits = 2)
#
# EC_fixtures_co$ec_ov25_odds
# EC_fixtures_co$ec_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# EC_fixtures_co$ec_ov25 <- percent(EC_fixtures_co$ec_ov25, accuracy = 0.1)
#
# EC_fixtures_co$ec_un25 <- percent(EC_fixtures_co$ec_un25, accuracy = 0.1)
# EC_fixtures_co$ec_pscore <- paste(round(EC_fixtures_co$ec_xHCOC,digits = 0),round(EC_fixtures_co$ec_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(EC_fixtures,'Divisions/EC.xlsx',sheetName = "EC", append = TRUE)
# #################################################################################################################
# #F1
# HomeTeam_f1_co <- rep(f1_teams, each = length(f1_teams))
# AwayTeam_f1_co <- rep(f1_teams, length(f1_teams))
# F1_fixtures_co <- cbind(HomeTeam_f1_co,AwayTeam_f1_co)
# F1_fixtures_co <- as.data.frame(F1_fixtures_co)
# F1_fixtures_co <- F1_fixtures_co[!F1_fixtures_co$HomeTeam_f1_co == F1_fixtures_co$AwayTeam_f1_co,]
# rownames(F1_fixtures_co) <- NULL
# F1_fixtures_co$Div <- "F1"
# F1_fixtures_co <- F1_fixtures_co[,c(3,1,2)]
#
# F1_fixtures_co$avg_HCO_f1 <- f1_avg_HCO
#
# F1_fixtures_co$f1_homecoas <- rep(f1_home_coas,each = length(f1_teams)-1)
#
# f1_awaycods_lookup <- cbind(f1_teams,f1_away_cods)
#
# f1_awaycods_lookup <- as.data.frame(f1_awaycods_lookup)
#
# colnames(f1_awaycods_lookup) <- c("AwayTeam_f1_co","f1_awaycods")
#
#
# require('RH2')
# F1_fixtures_co$f1_awaycods <- sqldf("SELECT f1_awaycods_lookup.f1_awaycods FROM f1_awaycods_lookup INNER JOIN F1_fixtures_co ON f1_awaycods_lookup.AwayTeam_f1_co = F1_fixtures_co.AwayTeam_f1_co")
#
# F1_fixtures_co$avg_ACO_f1 <- f1_avg_ACO
#
# f1_awaycoas_lookup <- cbind(f1_teams,f1_away_coas)
#
# f1_awaycoas_lookup <- as.data.frame(f1_awaycoas_lookup)
#
# colnames(f1_awaycoas_lookup) <- c("AwayTeam_f1_co","f1_awaycoas")
#
# F1_fixtures_co$f1_awaycoas <- sqldf("SELECT f1_awaycoas_lookup.f1_awaycoas FROM f1_awaycoas_lookup INNER JOIN F1_fixtures_co ON f1_awaycoas_lookup.AwayTeam_f1_co = F1_fixtures_co.AwayTeam_f1_co")
#
# F1_fixtures_co$f1_homecods <- rep(f1_home_cods,each = length(f1_teams)-1)
#
# F1_fixtures_co$f1_awaycods <- as.numeric(unlist(F1_fixtures_co$f1_awaycods))
# #xGH
# F1_fixtures_co$f1_xHCOC <- F1_fixtures_co$avg_HCO_f1 * F1_fixtures_co$f1_homecoas * F1_fixtures_co$f1_awaycods
# #xGA
#
# F1_fixtures_co$f1_awaycoas <- as.numeric(unlist(F1_fixtures_co$f1_awaycoas))
#
# F1_fixtures_co$f1_xACOC <- F1_fixtures_co$avg_ACO_f1 * F1_fixtures_co$f1_awaycoas * F1_fixtures_co$f1_homecods
#
# F1_fixtures_co$f1_0_0 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_0 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_1 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_1 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_0 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_2 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_2 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_1 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_2 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_3 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_0 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_1 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_2 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_3 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_3 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_3 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_4 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_0 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_1 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_2 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_3 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_4 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_4 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_4 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_4 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_5 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_0 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_1 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_2 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_3 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_4 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_5 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_5 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_5 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_5 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_5 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_6 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_0 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(0,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_1 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(1,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_2 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(2,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_3 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(3,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_4 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(4,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_6_5 <- round(stats::dpois(6,F1_fixtures_co$f1_xHCOC) * stats::dpois(5,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_0_6 <- round(stats::dpois(0,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_1_6 <- round(stats::dpois(1,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_2_6 <- round(stats::dpois(2,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_3_6 <- round(stats::dpois(3,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_4_6 <- round(stats::dpois(4,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# F1_fixtures_co$f1_5_6 <- round(stats::dpois(5,F1_fixtures_co$f1_xHCOC) * stats::dpois(6,F1_fixtures_co$f1_xACOC), digits = 4)
# #Home win
# F1_fixtures_co$f1_H <- (
#   F1_fixtures_co$f1_1_0 + F1_fixtures_co$f1_2_0 + F1_fixtures_co$f1_2_1 + F1_fixtures_co$f1_3_0 + F1_fixtures_co$f1_3_1 +
#     F1_fixtures_co$f1_3_2 + F1_fixtures_co$f1_4_0 + F1_fixtures_co$f1_4_1 + F1_fixtures_co$f1_4_2 + F1_fixtures_co$f1_4_3 +
#     F1_fixtures_co$f1_5_0 + F1_fixtures_co$f1_5_1 + F1_fixtures_co$f1_5_2 + F1_fixtures_co$f1_5_3 + F1_fixtures_co$f1_5_4 +
#     F1_fixtures_co$f1_6_0 + F1_fixtures_co$f1_6_1 + F1_fixtures_co$f1_6_2 + F1_fixtures_co$f1_6_3 + F1_fixtures_co$f1_6_4 +
#     F1_fixtures_co$f1_6_5
# )
#
# F1_fixtures_co$f1_H <- percent(F1_fixtures_co$f1_H, accuracy = 0.1)
#
# #Draw
# F1_fixtures_co$f1_D <- (
#
#   F1_fixtures_co$f1_0_0 + F1_fixtures_co$f1_1_1 + F1_fixtures_co$f1_2_2 + F1_fixtures_co$f1_3_3 + F1_fixtures_co$f1_4_4 +
#     F1_fixtures_co$f1_5_5 + F1_fixtures_co$f1_6_6
# )
#
# F1_fixtures_co$f1_D <- percent(F1_fixtures_co$f1_D, accuracy = 0.1)
#
# #Away
#
# F1_fixtures_co$f1_A <- (
#   F1_fixtures_co$f1_0_1 + F1_fixtures_co$f1_0_2 + F1_fixtures_co$f1_1_2 + F1_fixtures_co$f1_0_3 + F1_fixtures_co$f1_1_3 +
#     F1_fixtures_co$f1_2_3 + F1_fixtures_co$f1_0_4 + F1_fixtures_co$f1_1_4 + F1_fixtures_co$f1_2_4 + F1_fixtures_co$f1_3_4 +
#     F1_fixtures_co$f1_0_5 + F1_fixtures_co$f1_1_5 + F1_fixtures_co$f1_2_5 + F1_fixtures_co$f1_3_5 + F1_fixtures_co$f1_4_5 +
#     F1_fixtures_co$f1_0_6 + F1_fixtures_co$f1_1_6 + F1_fixtures_co$f1_2_6 + F1_fixtures_co$f1_3_6 + F1_fixtures_co$f1_4_6 +
#     F1_fixtures_co$f1_5_6
# )
#
# F1_fixtures_co$f1_A <- percent(F1_fixtures_co$f1_A, accuracy = 0.1)
#
# #ov25
# F1_fixtures_co$f1_ov25 <- (
#   F1_fixtures_co$f1_2_1 + F1_fixtures_co$f1_1_2 + F1_fixtures_co$f1_2_2 + F1_fixtures_co$f1_3_0 + F1_fixtures_co$f1_3_1 +
#     F1_fixtures_co$f1_3_2 + F1_fixtures_co$f1_0_3 + F1_fixtures_co$f1_1_3 + F1_fixtures_co$f1_2_3 + F1_fixtures_co$f1_3_3 +
#     F1_fixtures_co$f1_4_0 + F1_fixtures_co$f1_4_1 + F1_fixtures_co$f1_4_2 + F1_fixtures_co$f1_4_3 + F1_fixtures_co$f1_0_4 +
#     F1_fixtures_co$f1_1_4 + F1_fixtures_co$f1_2_4 + F1_fixtures_co$f1_3_4 + F1_fixtures_co$f1_4_4 + F1_fixtures_co$f1_5_0 +
#     F1_fixtures_co$f1_5_1 + F1_fixtures_co$f1_5_2 + F1_fixtures_co$f1_5_3 + F1_fixtures_co$f1_5_4 + F1_fixtures_co$f1_0_5 +
#     F1_fixtures_co$f1_1_5 + F1_fixtures_co$f1_2_5 + F1_fixtures_co$f1_3_5 + F1_fixtures_co$f1_4_5 + F1_fixtures_co$f1_5_5 +
#     F1_fixtures_co$f1_6_0 + F1_fixtures_co$f1_6_1 + F1_fixtures_co$f1_6_2 + F1_fixtures_co$f1_6_3 + F1_fixtures_co$f1_6_4 +
#     F1_fixtures_co$f1_6_5 + F1_fixtures_co$f1_0_6 + F1_fixtures_co$f1_1_6 + F1_fixtures_co$f1_2_6 + F1_fixtures_co$f1_3_6 +
#     F1_fixtures_co$f1_4_6 + F1_fixtures_co$f1_5_6 + F1_fixtures_co$f1_6_6
# )
# #un25
# F1_fixtures_co$f1_un25 <- (
#   F1_fixtures_co$f1_0_0 + F1_fixtures_co$f1_1_0 + F1_fixtures_co$f1_0_1 + F1_fixtures_co$f1_1_1 + F1_fixtures_co$f1_2_0 + F1_fixtures_co$f1_0_2
# )
# #odds
# F1_fixtures_co$f1_ov25_odds <- round((1/F1_fixtures_co$f1_ov25),digits = 2)
# F1_fixtures_co$f1_un25_odds <- round((1/F1_fixtures_co$f1_un25),digits = 2)
#
# F1_fixtures_co$f1_ov25_odds
# F1_fixtures_co$f1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F1_fixtures_co$f1_ov25 <- percent(F1_fixtures_co$f1_ov25, accuracy = 0.1)
#
# F1_fixtures_co$f1_un25 <- percent(F1_fixtures_co$f1_un25, accuracy = 0.1)
# F1_fixtures_co$f1_pscore <- paste(round(F1_fixtures_co$f1_xHCOC,digits = 0),round(F1_fixtures_co$f1_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #F2
# HomeTeam_f2_co <- rep(f2_teams, each = length(f2_teams))
# AwayTeam_f2_co <- rep(f2_teams, length(f2_teams))
# F2_fixtures_co <- cbind(HomeTeam_f2_co,AwayTeam_f2_co)
# F2_fixtures_co <- as.data.frame(F2_fixtures_co)
# F2_fixtures_co <- F2_fixtures_co[!F2_fixtures_co$HomeTeam_f2_co == F2_fixtures_co$AwayTeam_f2_co,]
# rownames(F2_fixtures_co) <- NULL
# F2_fixtures_co$Div <- "F2"
# F2_fixtures_co <- F2_fixtures_co[,c(3,1,2)]
#
# F2_fixtures_co$avg_HCO_f2 <- f2_avg_HCO
#
# F2_fixtures_co$f2_homecoas <- rep(f2_home_coas,each = length(f2_teams)-1)
#
# f2_awaycods_lookup <- cbind(f2_teams,f2_away_cods)
#
# f2_awaycods_lookup <- as.data.frame(f2_awaycods_lookup)
#
# colnames(f2_awaycods_lookup) <- c("AwayTeam_f2_co","f2_awaycods")
#
#
# require('RH2')
# F2_fixtures_co$f2_awaycods <- sqldf("SELECT f2_awaycods_lookup.f2_awaycods FROM f2_awaycods_lookup INNER JOIN F2_fixtures_co ON f2_awaycods_lookup.AwayTeam_f2_co = F2_fixtures_co.AwayTeam_f2_co")
#
# F2_fixtures_co$avg_ACO_f2 <- f2_avg_ACO
#
# f2_awaycoas_lookup <- cbind(f2_teams,f2_away_coas)
#
# f2_awaycoas_lookup <- as.data.frame(f2_awaycoas_lookup)
#
# colnames(f2_awaycoas_lookup) <- c("AwayTeam_f2_co","f2_awaycoas")
#
# F2_fixtures_co$f2_awaycoas <- sqldf("SELECT f2_awaycoas_lookup.f2_awaycoas FROM f2_awaycoas_lookup INNER JOIN F2_fixtures_co ON f2_awaycoas_lookup.AwayTeam_f2_co = F2_fixtures_co.AwayTeam_f2_co")
#
# F2_fixtures_co$f2_homecods <- rep(f2_home_cods,each = length(f2_teams)-1)
#
# F2_fixtures_co$f2_awaycods <- as.numeric(unlist(F2_fixtures_co$f2_awaycods))
# #xGH
# F2_fixtures_co$f2_xHCOC <- F2_fixtures_co$avg_HCO_f2 * F2_fixtures_co$f2_homecoas * F2_fixtures_co$f2_awaycods
# #xGA
#
# F2_fixtures_co$f2_awaycoas <- as.numeric(unlist(F2_fixtures_co$f2_awaycoas))
#
# F2_fixtures_co$f2_xACOC <- F2_fixtures_co$avg_ACO_f2 * F2_fixtures_co$f2_awaycoas * F2_fixtures_co$f2_homecods
#
# F2_fixtures_co$f2_0_0 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_0 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_1 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_1 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_0 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_2 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_2 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_1 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_2 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_3 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_0 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_1 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_2 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_3 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_3 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_3 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_4 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_0 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_1 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_2 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_3 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_4 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_4 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_4 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_4 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_5 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_0 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_1 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_2 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_3 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_4 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_5 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_5 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_5 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_5 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_5 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_6 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_0 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(0,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_1 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(1,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_2 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(2,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_3 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(3,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_4 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(4,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_6_5 <- round(stats::dpois(6,F2_fixtures_co$f2_xHCOC) * stats::dpois(5,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_0_6 <- round(stats::dpois(0,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_1_6 <- round(stats::dpois(1,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_2_6 <- round(stats::dpois(2,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_3_6 <- round(stats::dpois(3,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_4_6 <- round(stats::dpois(4,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# F2_fixtures_co$f2_5_6 <- round(stats::dpois(5,F2_fixtures_co$f2_xHCOC) * stats::dpois(6,F2_fixtures_co$f2_xACOC), digits = 4)
# #Home win
# F2_fixtures_co$f2_H <- (
#   F2_fixtures_co$f2_1_0 + F2_fixtures_co$f2_2_0 + F2_fixtures_co$f2_2_1 + F2_fixtures_co$f2_3_0 + F2_fixtures_co$f2_3_1 +
#     F2_fixtures_co$f2_3_2 + F2_fixtures_co$f2_4_0 + F2_fixtures_co$f2_4_1 + F2_fixtures_co$f2_4_2 + F2_fixtures_co$f2_4_3 +
#     F2_fixtures_co$f2_5_0 + F2_fixtures_co$f2_5_1 + F2_fixtures_co$f2_5_2 + F2_fixtures_co$f2_5_3 + F2_fixtures_co$f2_5_4 +
#     F2_fixtures_co$f2_6_0 + F2_fixtures_co$f2_6_1 + F2_fixtures_co$f2_6_2 + F2_fixtures_co$f2_6_3 + F2_fixtures_co$f2_6_4 +
#     F2_fixtures_co$f2_6_5
# )
#
# F2_fixtures_co$f2_H <- percent(F2_fixtures_co$f2_H, accuracy = 0.1)
#
# #Draw
# F2_fixtures_co$f2_D <- (
#
#   F2_fixtures_co$f2_0_0 + F2_fixtures_co$f2_1_1 + F2_fixtures_co$f2_2_2 + F2_fixtures_co$f2_3_3 + F2_fixtures_co$f2_4_4 +
#     F2_fixtures_co$f2_5_5 + F2_fixtures_co$f2_6_6
# )
#
# F2_fixtures_co$f2_D <- percent(F2_fixtures_co$f2_D, accuracy = 0.1)
#
# #Away
#
# F2_fixtures_co$f2_A <- (
#   F2_fixtures_co$f2_0_1 + F2_fixtures_co$f2_0_2 + F2_fixtures_co$f2_1_2 + F2_fixtures_co$f2_0_3 + F2_fixtures_co$f2_1_3 +
#     F2_fixtures_co$f2_2_3 + F2_fixtures_co$f2_0_4 + F2_fixtures_co$f2_1_4 + F2_fixtures_co$f2_2_4 + F2_fixtures_co$f2_3_4 +
#     F2_fixtures_co$f2_0_5 + F2_fixtures_co$f2_1_5 + F2_fixtures_co$f2_2_5 + F2_fixtures_co$f2_3_5 + F2_fixtures_co$f2_4_5 +
#     F2_fixtures_co$f2_0_6 + F2_fixtures_co$f2_1_6 + F2_fixtures_co$f2_2_6 + F2_fixtures_co$f2_3_6 + F2_fixtures_co$f2_4_6 +
#     F2_fixtures_co$f2_5_6
# )
#
# F2_fixtures_co$f2_A <- percent(F2_fixtures_co$f2_A, accuracy = 0.1)
#
# #ov25
# F2_fixtures_co$f2_ov25 <- (
#   F2_fixtures_co$f2_2_1 + F2_fixtures_co$f2_1_2 + F2_fixtures_co$f2_2_2 + F2_fixtures_co$f2_3_0 + F2_fixtures_co$f2_3_1 +
#     F2_fixtures_co$f2_3_2 + F2_fixtures_co$f2_0_3 + F2_fixtures_co$f2_1_3 + F2_fixtures_co$f2_2_3 + F2_fixtures_co$f2_3_3 +
#     F2_fixtures_co$f2_4_0 + F2_fixtures_co$f2_4_1 + F2_fixtures_co$f2_4_2 + F2_fixtures_co$f2_4_3 + F2_fixtures_co$f2_0_4 +
#     F2_fixtures_co$f2_1_4 + F2_fixtures_co$f2_2_4 + F2_fixtures_co$f2_3_4 + F2_fixtures_co$f2_4_4 + F2_fixtures_co$f2_5_0 +
#     F2_fixtures_co$f2_5_1 + F2_fixtures_co$f2_5_2 + F2_fixtures_co$f2_5_3 + F2_fixtures_co$f2_5_4 + F2_fixtures_co$f2_0_5 +
#     F2_fixtures_co$f2_1_5 + F2_fixtures_co$f2_2_5 + F2_fixtures_co$f2_3_5 + F2_fixtures_co$f2_4_5 + F2_fixtures_co$f2_5_5 +
#     F2_fixtures_co$f2_6_0 + F2_fixtures_co$f2_6_1 + F2_fixtures_co$f2_6_2 + F2_fixtures_co$f2_6_3 + F2_fixtures_co$f2_6_4 +
#     F2_fixtures_co$f2_6_5 + F2_fixtures_co$f2_0_6 + F2_fixtures_co$f2_1_6 + F2_fixtures_co$f2_2_6 + F2_fixtures_co$f2_3_6 +
#     F2_fixtures_co$f2_4_6 + F2_fixtures_co$f2_5_6 + F2_fixtures_co$f2_6_6
# )
# #un25
# F2_fixtures_co$f2_un25 <- (
#   F2_fixtures_co$f2_0_0 + F2_fixtures_co$f2_1_0 + F2_fixtures_co$f2_0_1 + F2_fixtures_co$f2_1_1 + F2_fixtures_co$f2_2_0 + F2_fixtures_co$f2_0_2
# )
# #odds
# F2_fixtures_co$f2_ov25_odds <- round((1/F2_fixtures_co$f2_ov25),digits = 2)
# F2_fixtures_co$f2_un25_odds <- round((1/F2_fixtures_co$f2_un25),digits = 2)
#
# F2_fixtures_co$f2_ov25_odds
# F2_fixtures_co$f2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F2_fixtures_co$f2_ov25 <- percent(F2_fixtures_co$f2_ov25, accuracy = 0.1)
#
# F2_fixtures_co$f2_un25 <- percent(F2_fixtures_co$f2_un25, accuracy = 0.1)
# F2_fixtures_co$f2_pscore <- paste(round(F2_fixtures_co$f2_xHCOC,digits = 0),round(F2_fixtures_co$f2_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #G1
# HomeTeam_g1_co <- rep(g1_teams, each = length(g1_teams))
# AwayTeam_g1_co <- rep(g1_teams, length(g1_teams))
# G1_fixtures_co <- cbind(HomeTeam_g1_co,AwayTeam_g1_co)
# G1_fixtures_co <- as.data.frame(G1_fixtures_co)
# G1_fixtures_co <- G1_fixtures_co[!G1_fixtures_co$HomeTeam_g1_co == G1_fixtures_co$AwayTeam_g1_co,]
# rownames(G1_fixtures_co) <- NULL
# G1_fixtures_co$Div <- "G1"
# G1_fixtures_co <- G1_fixtures_co[,c(3,1,2)]
#
# G1_fixtures_co$avg_HCO_g1 <- g1_avg_HCO
#
# G1_fixtures_co$g1_homecoas <- rep(g1_home_coas,each = length(g1_teams)-1)
#
# g1_awaycods_lookup <- cbind(g1_teams,g1_away_cods)
#
# g1_awaycods_lookup <- as.data.frame(g1_awaycods_lookup)
#
# colnames(g1_awaycods_lookup) <- c("AwayTeam_g1_co","g1_awaycods")
#
#
# require('RH2')
# G1_fixtures_co$g1_awaycods <- sqldf("SELECT g1_awaycods_lookup.g1_awaycods FROM g1_awaycods_lookup INNER JOIN G1_fixtures_co ON g1_awaycods_lookup.AwayTeam_g1_co = G1_fixtures_co.AwayTeam_g1_co")
#
# G1_fixtures_co$avg_ACO_g1 <- g1_avg_ACO
#
# g1_awaycoas_lookup <- cbind(g1_teams,g1_away_coas)
#
# g1_awaycoas_lookup <- as.data.frame(g1_awaycoas_lookup)
#
# colnames(g1_awaycoas_lookup) <- c("AwayTeam_g1_co","g1_awaycoas")
#
# G1_fixtures_co$g1_awaycoas <- sqldf("SELECT g1_awaycoas_lookup.g1_awaycoas FROM g1_awaycoas_lookup INNER JOIN G1_fixtures_co ON g1_awaycoas_lookup.AwayTeam_g1_co = G1_fixtures_co.AwayTeam_g1_co")
#
# G1_fixtures_co$g1_homecods <- rep(g1_home_cods,each = length(g1_teams)-1)
#
# G1_fixtures_co$g1_awaycods <- as.numeric(unlist(G1_fixtures_co$g1_awaycods))
# #xGH
# G1_fixtures_co$g1_xHCOC <- G1_fixtures_co$avg_HCO_g1 * G1_fixtures_co$g1_homecoas * G1_fixtures_co$g1_awaycods
# #xGA
#
# G1_fixtures_co$g1_awaycoas <- as.numeric(unlist(G1_fixtures_co$g1_awaycoas))
#
# G1_fixtures_co$g1_xACOC <- G1_fixtures_co$avg_ACO_g1 * G1_fixtures_co$g1_awaycoas * G1_fixtures_co$g1_homecods
#
# G1_fixtures_co$g1_0_0 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_0 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_1 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_1 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_0 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_2 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_2 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_1 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_2 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_3 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_0 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_1 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_2 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_3 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_3 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_3 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_4 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_0 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_1 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_2 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_3 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_4 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_4 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_4 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_4 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_5 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_0 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_1 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_2 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_3 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_4 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_5 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_5 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_5 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_5 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_5 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_6 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_0 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(0,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_1 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(1,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_2 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(2,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_3 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(3,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_4 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(4,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_6_5 <- round(stats::dpois(6,G1_fixtures_co$g1_xHCOC) * stats::dpois(5,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_0_6 <- round(stats::dpois(0,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_1_6 <- round(stats::dpois(1,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_2_6 <- round(stats::dpois(2,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_3_6 <- round(stats::dpois(3,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_4_6 <- round(stats::dpois(4,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# G1_fixtures_co$g1_5_6 <- round(stats::dpois(5,G1_fixtures_co$g1_xHCOC) * stats::dpois(6,G1_fixtures_co$g1_xACOC), digits = 4)
# #Home win
# G1_fixtures_co$g1_H <- (
#   G1_fixtures_co$g1_1_0 + G1_fixtures_co$g1_2_0 + G1_fixtures_co$g1_2_1 + G1_fixtures_co$g1_3_0 + G1_fixtures_co$g1_3_1 +
#     G1_fixtures_co$g1_3_2 + G1_fixtures_co$g1_4_0 + G1_fixtures_co$g1_4_1 + G1_fixtures_co$g1_4_2 + G1_fixtures_co$g1_4_3 +
#     G1_fixtures_co$g1_5_0 + G1_fixtures_co$g1_5_1 + G1_fixtures_co$g1_5_2 + G1_fixtures_co$g1_5_3 + G1_fixtures_co$g1_5_4 +
#     G1_fixtures_co$g1_6_0 + G1_fixtures_co$g1_6_1 + G1_fixtures_co$g1_6_2 + G1_fixtures_co$g1_6_3 + G1_fixtures_co$g1_6_4 +
#     G1_fixtures_co$g1_6_5
# )
#
# G1_fixtures_co$g1_H <- percent(G1_fixtures_co$g1_H, accuracy = 0.1)
#
# #Draw
# G1_fixtures_co$g1_D <- (
#
#   G1_fixtures_co$g1_0_0 + G1_fixtures_co$g1_1_1 + G1_fixtures_co$g1_2_2 + G1_fixtures_co$g1_3_3 + G1_fixtures_co$g1_4_4 +
#     G1_fixtures_co$g1_5_5 + G1_fixtures_co$g1_6_6
# )
#
# G1_fixtures_co$g1_D <- percent(G1_fixtures_co$g1_D, accuracy = 0.1)
#
# #Away
#
# G1_fixtures_co$g1_A <- (
#   G1_fixtures_co$g1_0_1 + G1_fixtures_co$g1_0_2 + G1_fixtures_co$g1_1_2 + G1_fixtures_co$g1_0_3 + G1_fixtures_co$g1_1_3 +
#     G1_fixtures_co$g1_2_3 + G1_fixtures_co$g1_0_4 + G1_fixtures_co$g1_1_4 + G1_fixtures_co$g1_2_4 + G1_fixtures_co$g1_3_4 +
#     G1_fixtures_co$g1_0_5 + G1_fixtures_co$g1_1_5 + G1_fixtures_co$g1_2_5 + G1_fixtures_co$g1_3_5 + G1_fixtures_co$g1_4_5 +
#     G1_fixtures_co$g1_0_6 + G1_fixtures_co$g1_1_6 + G1_fixtures_co$g1_2_6 + G1_fixtures_co$g1_3_6 + G1_fixtures_co$g1_4_6 +
#     G1_fixtures_co$g1_5_6
# )
#
# G1_fixtures_co$g1_A <- percent(G1_fixtures_co$g1_A, accuracy = 0.1)
#
# #ov25
# G1_fixtures_co$g1_ov25 <- (
#   G1_fixtures_co$g1_2_1 + G1_fixtures_co$g1_1_2 + G1_fixtures_co$g1_2_2 + G1_fixtures_co$g1_3_0 + G1_fixtures_co$g1_3_1 +
#     G1_fixtures_co$g1_3_2 + G1_fixtures_co$g1_0_3 + G1_fixtures_co$g1_1_3 + G1_fixtures_co$g1_2_3 + G1_fixtures_co$g1_3_3 +
#     G1_fixtures_co$g1_4_0 + G1_fixtures_co$g1_4_1 + G1_fixtures_co$g1_4_2 + G1_fixtures_co$g1_4_3 + G1_fixtures_co$g1_0_4 +
#     G1_fixtures_co$g1_1_4 + G1_fixtures_co$g1_2_4 + G1_fixtures_co$g1_3_4 + G1_fixtures_co$g1_4_4 + G1_fixtures_co$g1_5_0 +
#     G1_fixtures_co$g1_5_1 + G1_fixtures_co$g1_5_2 + G1_fixtures_co$g1_5_3 + G1_fixtures_co$g1_5_4 + G1_fixtures_co$g1_0_5 +
#     G1_fixtures_co$g1_1_5 + G1_fixtures_co$g1_2_5 + G1_fixtures_co$g1_3_5 + G1_fixtures_co$g1_4_5 + G1_fixtures_co$g1_5_5 +
#     G1_fixtures_co$g1_6_0 + G1_fixtures_co$g1_6_1 + G1_fixtures_co$g1_6_2 + G1_fixtures_co$g1_6_3 + G1_fixtures_co$g1_6_4 +
#     G1_fixtures_co$g1_6_5 + G1_fixtures_co$g1_0_6 + G1_fixtures_co$g1_1_6 + G1_fixtures_co$g1_2_6 + G1_fixtures_co$g1_3_6 +
#     G1_fixtures_co$g1_4_6 + G1_fixtures_co$g1_5_6 + G1_fixtures_co$g1_6_6
# )
# #un25
# G1_fixtures_co$g1_un25 <- (
#   G1_fixtures_co$g1_0_0 + G1_fixtures_co$g1_1_0 + G1_fixtures_co$g1_0_1 + G1_fixtures_co$g1_1_1 + G1_fixtures_co$g1_2_0 + G1_fixtures_co$g1_0_2
# )
# #odds
# G1_fixtures_co$g1_ov25_odds <- round((1/G1_fixtures_co$g1_ov25),digits = 2)
# G1_fixtures_co$g1_un25_odds <- round((1/G1_fixtures_co$g1_un25),digits = 2)
#
# G1_fixtures_co$g1_ov25_odds
# G1_fixtures_co$g1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# G1_fixtures_co$g1_ov25 <- percent(G1_fixtures_co$g1_ov25, accuracy = 0.1)
#
# G1_fixtures_co$g1_un25 <- percent(G1_fixtures_co$g1_un25, accuracy = 0.1)
# G1_fixtures_co$g1_pscore <- paste(round(G1_fixtures_co$g1_xHCOC,digits = 0),round(G1_fixtures_co$g1_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #I1
# HomeTeam_i1_co <- rep(i1_teams, each = length(i1_teams))
# AwayTeam_i1_co <- rep(i1_teams, length(i1_teams))
# I1_fixtures_co <- cbind(HomeTeam_i1_co,AwayTeam_i1_co)
# I1_fixtures_co <- as.data.frame(I1_fixtures_co)
# I1_fixtures_co <- I1_fixtures_co[!I1_fixtures_co$HomeTeam_i1_co == I1_fixtures_co$AwayTeam_i1_co,]
# rownames(I1_fixtures_co) <- NULL
# I1_fixtures_co$Div <- "I1"
# I1_fixtures_co <- I1_fixtures_co[,c(3,1,2)]
#
# I1_fixtures_co$avg_HCO_i1 <- i1_avg_HCO
#
# I1_fixtures_co$i1_homecoas <- rep(i1_home_coas,each = length(i1_teams)-1)
#
# i1_awaycods_lookup <- cbind(i1_teams,i1_away_cods)
#
# i1_awaycods_lookup <- as.data.frame(i1_awaycods_lookup)
#
# colnames(i1_awaycods_lookup) <- c("AwayTeam_i1_co","i1_awaycods")
#
#
# require('RH2')
# I1_fixtures_co$i1_awaycods <- sqldf("SELECT i1_awaycods_lookup.i1_awaycods FROM i1_awaycods_lookup INNER JOIN I1_fixtures_co ON i1_awaycods_lookup.AwayTeam_i1_co = I1_fixtures_co.AwayTeam_i1_co")
#
# I1_fixtures_co$avg_ACO_i1 <- i1_avg_ACO
#
# i1_awaycoas_lookup <- cbind(i1_teams,i1_away_coas)
#
# i1_awaycoas_lookup <- as.data.frame(i1_awaycoas_lookup)
#
# colnames(i1_awaycoas_lookup) <- c("AwayTeam_i1_co","i1_awaycoas")
#
# I1_fixtures_co$i1_awaycoas <- sqldf("SELECT i1_awaycoas_lookup.i1_awaycoas FROM i1_awaycoas_lookup INNER JOIN I1_fixtures_co ON i1_awaycoas_lookup.AwayTeam_i1_co = I1_fixtures_co.AwayTeam_i1_co")
#
# I1_fixtures_co$i1_homecods <- rep(i1_home_cods,each = length(i1_teams)-1)
#
# I1_fixtures_co$i1_awaycods <- as.numeric(unlist(I1_fixtures_co$i1_awaycods))
# #xGH
# I1_fixtures_co$i1_xHCOC <- I1_fixtures_co$avg_HCO_i1 * I1_fixtures_co$i1_homecoas * I1_fixtures_co$i1_awaycods
# #xGA
#
# I1_fixtures_co$i1_awaycoas <- as.numeric(unlist(I1_fixtures_co$i1_awaycoas))
#
# I1_fixtures_co$i1_xACOC <- I1_fixtures_co$avg_ACO_i1 * I1_fixtures_co$i1_awaycoas * I1_fixtures_co$i1_homecods
#
# I1_fixtures_co$i1_0_0 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_0 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_1 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_1 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_0 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_2 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_2 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_1 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_2 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_3 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_0 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_1 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_2 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_3 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_3 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_3 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_4 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_0 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_1 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_2 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_3 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_4 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_4 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_4 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_4 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_5 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_0 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_1 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_2 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_3 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_4 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_5 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_5 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_5 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_5 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_5 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_6 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_0 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(0,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_1 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(1,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_2 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(2,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_3 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(3,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_4 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(4,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_6_5 <- round(stats::dpois(6,I1_fixtures_co$i1_xHCOC) * stats::dpois(5,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_0_6 <- round(stats::dpois(0,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_1_6 <- round(stats::dpois(1,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_2_6 <- round(stats::dpois(2,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_3_6 <- round(stats::dpois(3,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_4_6 <- round(stats::dpois(4,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# I1_fixtures_co$i1_5_6 <- round(stats::dpois(5,I1_fixtures_co$i1_xHCOC) * stats::dpois(6,I1_fixtures_co$i1_xACOC), digits = 4)
# #Home win
# I1_fixtures_co$i1_H <- (
#   I1_fixtures_co$i1_1_0 + I1_fixtures_co$i1_2_0 + I1_fixtures_co$i1_2_1 + I1_fixtures_co$i1_3_0 + I1_fixtures_co$i1_3_1 +
#     I1_fixtures_co$i1_3_2 + I1_fixtures_co$i1_4_0 + I1_fixtures_co$i1_4_1 + I1_fixtures_co$i1_4_2 + I1_fixtures_co$i1_4_3 +
#     I1_fixtures_co$i1_5_0 + I1_fixtures_co$i1_5_1 + I1_fixtures_co$i1_5_2 + I1_fixtures_co$i1_5_3 + I1_fixtures_co$i1_5_4 +
#     I1_fixtures_co$i1_6_0 + I1_fixtures_co$i1_6_1 + I1_fixtures_co$i1_6_2 + I1_fixtures_co$i1_6_3 + I1_fixtures_co$i1_6_4 +
#     I1_fixtures_co$i1_6_5
# )
#
# I1_fixtures_co$i1_H <- percent(I1_fixtures_co$i1_H, accuracy = 0.1)
#
# #Draw
# I1_fixtures_co$i1_D <- (
#
#   I1_fixtures_co$i1_0_0 + I1_fixtures_co$i1_1_1 + I1_fixtures_co$i1_2_2 + I1_fixtures_co$i1_3_3 + I1_fixtures_co$i1_4_4 +
#     I1_fixtures_co$i1_5_5 + I1_fixtures_co$i1_6_6
# )
#
# I1_fixtures_co$i1_D <- percent(I1_fixtures_co$i1_D, accuracy = 0.1)
#
# #Away
#
# I1_fixtures_co$i1_A <- (
#   I1_fixtures_co$i1_0_1 + I1_fixtures_co$i1_0_2 + I1_fixtures_co$i1_1_2 + I1_fixtures_co$i1_0_3 + I1_fixtures_co$i1_1_3 +
#     I1_fixtures_co$i1_2_3 + I1_fixtures_co$i1_0_4 + I1_fixtures_co$i1_1_4 + I1_fixtures_co$i1_2_4 + I1_fixtures_co$i1_3_4 +
#     I1_fixtures_co$i1_0_5 + I1_fixtures_co$i1_1_5 + I1_fixtures_co$i1_2_5 + I1_fixtures_co$i1_3_5 + I1_fixtures_co$i1_4_5 +
#     I1_fixtures_co$i1_0_6 + I1_fixtures_co$i1_1_6 + I1_fixtures_co$i1_2_6 + I1_fixtures_co$i1_3_6 + I1_fixtures_co$i1_4_6 +
#     I1_fixtures_co$i1_5_6
# )
#
# I1_fixtures_co$i1_A <- percent(I1_fixtures_co$i1_A, accuracy = 0.1)
#
# #ov25
# I1_fixtures_co$i1_ov25 <- (
#   I1_fixtures_co$i1_2_1 + I1_fixtures_co$i1_1_2 + I1_fixtures_co$i1_2_2 + I1_fixtures_co$i1_3_0 + I1_fixtures_co$i1_3_1 +
#     I1_fixtures_co$i1_3_2 + I1_fixtures_co$i1_0_3 + I1_fixtures_co$i1_1_3 + I1_fixtures_co$i1_2_3 + I1_fixtures_co$i1_3_3 +
#     I1_fixtures_co$i1_4_0 + I1_fixtures_co$i1_4_1 + I1_fixtures_co$i1_4_2 + I1_fixtures_co$i1_4_3 + I1_fixtures_co$i1_0_4 +
#     I1_fixtures_co$i1_1_4 + I1_fixtures_co$i1_2_4 + I1_fixtures_co$i1_3_4 + I1_fixtures_co$i1_4_4 + I1_fixtures_co$i1_5_0 +
#     I1_fixtures_co$i1_5_1 + I1_fixtures_co$i1_5_2 + I1_fixtures_co$i1_5_3 + I1_fixtures_co$i1_5_4 + I1_fixtures_co$i1_0_5 +
#     I1_fixtures_co$i1_1_5 + I1_fixtures_co$i1_2_5 + I1_fixtures_co$i1_3_5 + I1_fixtures_co$i1_4_5 + I1_fixtures_co$i1_5_5 +
#     I1_fixtures_co$i1_6_0 + I1_fixtures_co$i1_6_1 + I1_fixtures_co$i1_6_2 + I1_fixtures_co$i1_6_3 + I1_fixtures_co$i1_6_4 +
#     I1_fixtures_co$i1_6_5 + I1_fixtures_co$i1_0_6 + I1_fixtures_co$i1_1_6 + I1_fixtures_co$i1_2_6 + I1_fixtures_co$i1_3_6 +
#     I1_fixtures_co$i1_4_6 + I1_fixtures_co$i1_5_6 + I1_fixtures_co$i1_6_6
# )
# #un25
# I1_fixtures_co$i1_un25 <- (
#   I1_fixtures_co$i1_0_0 + I1_fixtures_co$i1_1_0 + I1_fixtures_co$i1_0_1 + I1_fixtures_co$i1_1_1 + I1_fixtures_co$i1_2_0 + I1_fixtures_co$i1_0_2
# )
# #odds
# I1_fixtures_co$i1_ov25_odds <- round((1/I1_fixtures_co$i1_ov25),digits = 2)
# I1_fixtures_co$i1_un25_odds <- round((1/I1_fixtures_co$i1_un25),digits = 2)
#
# I1_fixtures_co$i1_ov25_odds
# I1_fixtures_co$i1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I1_fixtures_co$i1_ov25 <- percent(I1_fixtures_co$i1_ov25, accuracy = 0.1)
#
# I1_fixtures_co$i1_un25 <- percent(I1_fixtures_co$i1_un25, accuracy = 0.1)
# I1_fixtures_co$i1_pscore <- paste(round(I1_fixtures_co$i1_xHCOC,digits = 0),round(I1_fixtures_co$i1_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(I1_fixtures,'Divisions/I1.xlsx',sheetName = "I1", append = TRUE)
# ##################
# #I2
# HomeTeam_i2_co <- rep(i2_teams, each = length(i2_teams))
# AwayTeam_i2_co <- rep(i2_teams, length(i2_teams))
# I2_fixtures_co <- cbind(HomeTeam_i2_co,AwayTeam_i2_co)
# I2_fixtures_co <- as.data.frame(I2_fixtures_co)
# I2_fixtures_co <- I2_fixtures_co[!I2_fixtures_co$HomeTeam_i2_co == I2_fixtures_co$AwayTeam_i2_co,]
# rownames(I2_fixtures_co) <- NULL
# I2_fixtures_co$Div <- "I2"
# I2_fixtures_co <- I2_fixtures_co[,c(3,1,2)]
#
# I2_fixtures_co$avg_HCO_i2 <- i2_avg_HCO
#
# I2_fixtures_co$i2_homecoas <- rep(i2_home_coas,each = length(i2_teams)-1)
#
# i2_awaycods_lookup <- cbind(i2_teams,i2_away_cods)
#
# i2_awaycods_lookup <- as.data.frame(i2_awaycods_lookup)
#
# colnames(i2_awaycods_lookup) <- c("AwayTeam_i2_co","i2_awaycods")
#
#
# require('RH2')
# I2_fixtures_co$i2_awaycods <- sqldf("SELECT i2_awaycods_lookup.i2_awaycods FROM i2_awaycods_lookup INNER JOIN I2_fixtures_co ON i2_awaycods_lookup.AwayTeam_i2_co = I2_fixtures_co.AwayTeam_i2_co")
#
# I2_fixtures_co$avg_ACO_i2 <- i2_avg_ACO
#
# i2_awaycoas_lookup <- cbind(i2_teams,i2_away_coas)
#
# i2_awaycoas_lookup <- as.data.frame(i2_awaycoas_lookup)
#
# colnames(i2_awaycoas_lookup) <- c("AwayTeam_i2_co","i2_awaycoas")
#
# I2_fixtures_co$i2_awaycoas <- sqldf("SELECT i2_awaycoas_lookup.i2_awaycoas FROM i2_awaycoas_lookup INNER JOIN I2_fixtures_co ON i2_awaycoas_lookup.AwayTeam_i2_co = I2_fixtures_co.AwayTeam_i2_co")
#
# I2_fixtures_co$i2_homecods <- rep(i2_home_cods,each = length(i2_teams)-1)
#
# I2_fixtures_co$i2_awaycods <- as.numeric(unlist(I2_fixtures_co$i2_awaycods))
# #xGH
# I2_fixtures_co$i2_xHCOC <- I2_fixtures_co$avg_HCO_i2 * I2_fixtures_co$i2_homecoas * I2_fixtures_co$i2_awaycods
# #xGA
#
# I2_fixtures_co$i2_awaycoas <- as.numeric(unlist(I2_fixtures_co$i2_awaycoas))
#
# I2_fixtures_co$i2_xACOC <- I2_fixtures_co$avg_ACO_i2 * I2_fixtures_co$i2_awaycoas * I2_fixtures_co$i2_homecods
#
# I2_fixtures_co$i2_0_0 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_0 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_1 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_1 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_0 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_2 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_2 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_1 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_2 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_3 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_0 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_1 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_2 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_3 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_3 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_3 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_4 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_0 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_1 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_2 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_3 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_4 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_4 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_4 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_4 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_5 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_0 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_1 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_2 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_3 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_4 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_5 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_5 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_5 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_5 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_5 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_6 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_0 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(0,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_1 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(1,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_2 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(2,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_3 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(3,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_4 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(4,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_6_5 <- round(stats::dpois(6,I2_fixtures_co$i2_xHCOC) * stats::dpois(5,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_0_6 <- round(stats::dpois(0,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_1_6 <- round(stats::dpois(1,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_2_6 <- round(stats::dpois(2,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_3_6 <- round(stats::dpois(3,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_4_6 <- round(stats::dpois(4,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# I2_fixtures_co$i2_5_6 <- round(stats::dpois(5,I2_fixtures_co$i2_xHCOC) * stats::dpois(6,I2_fixtures_co$i2_xACOC), digits = 4)
# #Home win
# I2_fixtures_co$i2_H <- (
#   I2_fixtures_co$i2_1_0 + I2_fixtures_co$i2_2_0 + I2_fixtures_co$i2_2_1 + I2_fixtures_co$i2_3_0 + I2_fixtures_co$i2_3_1 +
#     I2_fixtures_co$i2_3_2 + I2_fixtures_co$i2_4_0 + I2_fixtures_co$i2_4_1 + I2_fixtures_co$i2_4_2 + I2_fixtures_co$i2_4_3 +
#     I2_fixtures_co$i2_5_0 + I2_fixtures_co$i2_5_1 + I2_fixtures_co$i2_5_2 + I2_fixtures_co$i2_5_3 + I2_fixtures_co$i2_5_4 +
#     I2_fixtures_co$i2_6_0 + I2_fixtures_co$i2_6_1 + I2_fixtures_co$i2_6_2 + I2_fixtures_co$i2_6_3 + I2_fixtures_co$i2_6_4 +
#     I2_fixtures_co$i2_6_5
# )
#
# I2_fixtures_co$i2_H <- percent(I2_fixtures_co$i2_H, accuracy = 0.1)
#
# #Draw
# I2_fixtures_co$i2_D <- (
#
#   I2_fixtures_co$i2_0_0 + I2_fixtures_co$i2_1_1 + I2_fixtures_co$i2_2_2 + I2_fixtures_co$i2_3_3 + I2_fixtures_co$i2_4_4 +
#     I2_fixtures_co$i2_5_5 + I2_fixtures_co$i2_6_6
# )
#
# I2_fixtures_co$i2_D <- percent(I2_fixtures_co$i2_D, accuracy = 0.1)
#
# #Away
#
# I2_fixtures_co$i2_A <- (
#   I2_fixtures_co$i2_0_1 + I2_fixtures_co$i2_0_2 + I2_fixtures_co$i2_1_2 + I2_fixtures_co$i2_0_3 + I2_fixtures_co$i2_1_3 +
#     I2_fixtures_co$i2_2_3 + I2_fixtures_co$i2_0_4 + I2_fixtures_co$i2_1_4 + I2_fixtures_co$i2_2_4 + I2_fixtures_co$i2_3_4 +
#     I2_fixtures_co$i2_0_5 + I2_fixtures_co$i2_1_5 + I2_fixtures_co$i2_2_5 + I2_fixtures_co$i2_3_5 + I2_fixtures_co$i2_4_5 +
#     I2_fixtures_co$i2_0_6 + I2_fixtures_co$i2_1_6 + I2_fixtures_co$i2_2_6 + I2_fixtures_co$i2_3_6 + I2_fixtures_co$i2_4_6 +
#     I2_fixtures_co$i2_5_6
# )
#
# I2_fixtures_co$i2_A <- percent(I2_fixtures_co$i2_A, accuracy = 0.1)
#
# #ov25
# I2_fixtures_co$i2_ov25 <- (
#   I2_fixtures_co$i2_2_1 + I2_fixtures_co$i2_1_2 + I2_fixtures_co$i2_2_2 + I2_fixtures_co$i2_3_0 + I2_fixtures_co$i2_3_1 +
#     I2_fixtures_co$i2_3_2 + I2_fixtures_co$i2_0_3 + I2_fixtures_co$i2_1_3 + I2_fixtures_co$i2_2_3 + I2_fixtures_co$i2_3_3 +
#     I2_fixtures_co$i2_4_0 + I2_fixtures_co$i2_4_1 + I2_fixtures_co$i2_4_2 + I2_fixtures_co$i2_4_3 + I2_fixtures_co$i2_0_4 +
#     I2_fixtures_co$i2_1_4 + I2_fixtures_co$i2_2_4 + I2_fixtures_co$i2_3_4 + I2_fixtures_co$i2_4_4 + I2_fixtures_co$i2_5_0 +
#     I2_fixtures_co$i2_5_1 + I2_fixtures_co$i2_5_2 + I2_fixtures_co$i2_5_3 + I2_fixtures_co$i2_5_4 + I2_fixtures_co$i2_0_5 +
#     I2_fixtures_co$i2_1_5 + I2_fixtures_co$i2_2_5 + I2_fixtures_co$i2_3_5 + I2_fixtures_co$i2_4_5 + I2_fixtures_co$i2_5_5 +
#     I2_fixtures_co$i2_6_0 + I2_fixtures_co$i2_6_1 + I2_fixtures_co$i2_6_2 + I2_fixtures_co$i2_6_3 + I2_fixtures_co$i2_6_4 +
#     I2_fixtures_co$i2_6_5 + I2_fixtures_co$i2_0_6 + I2_fixtures_co$i2_1_6 + I2_fixtures_co$i2_2_6 + I2_fixtures_co$i2_3_6 +
#     I2_fixtures_co$i2_4_6 + I2_fixtures_co$i2_5_6 + I2_fixtures_co$i2_6_6
# )
# #un25
# I2_fixtures_co$i2_un25 <- (
#   I2_fixtures_co$i2_0_0 + I2_fixtures_co$i2_1_0 + I2_fixtures_co$i2_0_1 + I2_fixtures_co$i2_1_1 + I2_fixtures_co$i2_2_0 + I2_fixtures_co$i2_0_2
# )
# #odds
# I2_fixtures_co$i2_ov25_odds <- round((1/I2_fixtures_co$i2_ov25),digits = 2)
# I2_fixtures_co$i2_un25_odds <- round((1/I2_fixtures_co$i2_un25),digits = 2)
#
# I2_fixtures_co$i2_ov25_odds
# I2_fixtures_co$i2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I2_fixtures_co$i2_ov25 <- percent(I2_fixtures_co$i2_ov25, accuracy = 0.1)
#
# I2_fixtures_co$i2_un25 <- percent(I2_fixtures_co$i2_un25, accuracy = 0.1)
# I2_fixtures_co$i2_pscore <- paste(round(I2_fixtures_co$i2_xHCOC,digits = 0),round(I2_fixtures_co$i2_xACOC,digits = 0),sep = "-")
#
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################################################################################################################
# #N1
# HomeTeam_n1_co <- rep(n1_teams, each = length(n1_teams))
# AwayTeam_n1_co <- rep(n1_teams, length(n1_teams))
# N1_fixtures_co <- cbind(HomeTeam_n1_co,AwayTeam_n1_co)
# N1_fixtures_co <- as.data.frame(N1_fixtures_co)
# N1_fixtures_co <- N1_fixtures_co[!N1_fixtures_co$HomeTeam_n1_co == N1_fixtures_co$AwayTeam_n1_co,]
# rownames(N1_fixtures_co) <- NULL
# N1_fixtures_co$Div <- "N1"
# N1_fixtures_co <- N1_fixtures_co[,c(3,1,2)]
#
# N1_fixtures_co$avg_HCO_n1 <- n1_avg_HCO
#
# N1_fixtures_co$n1_homecoas <- rep(n1_home_coas,each = length(n1_teams)-1)
#
# n1_awaycods_lookup <- cbind(n1_teams,n1_away_cods)
#
# n1_awaycods_lookup <- as.data.frame(n1_awaycods_lookup)
#
# colnames(n1_awaycods_lookup) <- c("AwayTeam_n1_co","n1_awaycods")
#
#
# require('RH2')
# N1_fixtures_co$n1_awaycods <- sqldf("SELECT n1_awaycods_lookup.n1_awaycods FROM n1_awaycods_lookup INNER JOIN N1_fixtures_co ON n1_awaycods_lookup.AwayTeam_n1_co = N1_fixtures_co.AwayTeam_n1_co")
#
# N1_fixtures_co$avg_ACO_n1 <- n1_avg_ACO
#
# n1_awaycoas_lookup <- cbind(n1_teams,n1_away_coas)
#
# n1_awaycoas_lookup <- as.data.frame(n1_awaycoas_lookup)
#
# colnames(n1_awaycoas_lookup) <- c("AwayTeam_n1_co","n1_awaycoas")
#
# N1_fixtures_co$n1_awaycoas <- sqldf("SELECT n1_awaycoas_lookup.n1_awaycoas FROM n1_awaycoas_lookup INNER JOIN N1_fixtures_co ON n1_awaycoas_lookup.AwayTeam_n1_co = N1_fixtures_co.AwayTeam_n1_co")
#
# N1_fixtures_co$n1_homecods <- rep(n1_home_cods,each = length(n1_teams)-1)
#
# N1_fixtures_co$n1_awaycods <- as.numeric(unlist(N1_fixtures_co$n1_awaycods))
# #xGH
# N1_fixtures_co$n1_xHCOC <- N1_fixtures_co$avg_HCO_n1 * N1_fixtures_co$n1_homecoas * N1_fixtures_co$n1_awaycods
# #xGA
#
# N1_fixtures_co$n1_awaycoas <- as.numeric(unlist(N1_fixtures_co$n1_awaycoas))
#
# N1_fixtures_co$n1_xACOC <- N1_fixtures_co$avg_ACO_n1 * N1_fixtures_co$n1_awaycoas * N1_fixtures_co$n1_homecods
#
# N1_fixtures_co$n1_0_0 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_0 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_1 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_1 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_0 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_2 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_2 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_1 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_2 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_3 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_0 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_1 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_2 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_3 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_3 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_3 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_4 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_0 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_1 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_2 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_3 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_4 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_4 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_4 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_4 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_5 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_0 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_1 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_2 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_3 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_4 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_5 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_5 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_5 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_5 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_5 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_6 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_0 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(0,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_1 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(1,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_2 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(2,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_3 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(3,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_4 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(4,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_6_5 <- round(stats::dpois(6,N1_fixtures_co$n1_xHCOC) * stats::dpois(5,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_0_6 <- round(stats::dpois(0,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_1_6 <- round(stats::dpois(1,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_2_6 <- round(stats::dpois(2,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_3_6 <- round(stats::dpois(3,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_4_6 <- round(stats::dpois(4,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# N1_fixtures_co$n1_5_6 <- round(stats::dpois(5,N1_fixtures_co$n1_xHCOC) * stats::dpois(6,N1_fixtures_co$n1_xACOC), digits = 4)
# #Home win
# N1_fixtures_co$n1_H <- (
#   N1_fixtures_co$n1_1_0 + N1_fixtures_co$n1_2_0 + N1_fixtures_co$n1_2_1 + N1_fixtures_co$n1_3_0 + N1_fixtures_co$n1_3_1 +
#     N1_fixtures_co$n1_3_2 + N1_fixtures_co$n1_4_0 + N1_fixtures_co$n1_4_1 + N1_fixtures_co$n1_4_2 + N1_fixtures_co$n1_4_3 +
#     N1_fixtures_co$n1_5_0 + N1_fixtures_co$n1_5_1 + N1_fixtures_co$n1_5_2 + N1_fixtures_co$n1_5_3 + N1_fixtures_co$n1_5_4 +
#     N1_fixtures_co$n1_6_0 + N1_fixtures_co$n1_6_1 + N1_fixtures_co$n1_6_2 + N1_fixtures_co$n1_6_3 + N1_fixtures_co$n1_6_4 +
#     N1_fixtures_co$n1_6_5
# )
#
# N1_fixtures_co$n1_H <- percent(N1_fixtures_co$n1_H, accuracy = 0.1)
#
# #Draw
# N1_fixtures_co$n1_D <- (
#
#   N1_fixtures_co$n1_0_0 + N1_fixtures_co$n1_1_1 + N1_fixtures_co$n1_2_2 + N1_fixtures_co$n1_3_3 + N1_fixtures_co$n1_4_4 +
#     N1_fixtures_co$n1_5_5 + N1_fixtures_co$n1_6_6
# )
#
# N1_fixtures_co$n1_D <- percent(N1_fixtures_co$n1_D, accuracy = 0.1)
#
# #Away
#
# N1_fixtures_co$n1_A <- (
#   N1_fixtures_co$n1_0_1 + N1_fixtures_co$n1_0_2 + N1_fixtures_co$n1_1_2 + N1_fixtures_co$n1_0_3 + N1_fixtures_co$n1_1_3 +
#     N1_fixtures_co$n1_2_3 + N1_fixtures_co$n1_0_4 + N1_fixtures_co$n1_1_4 + N1_fixtures_co$n1_2_4 + N1_fixtures_co$n1_3_4 +
#     N1_fixtures_co$n1_0_5 + N1_fixtures_co$n1_1_5 + N1_fixtures_co$n1_2_5 + N1_fixtures_co$n1_3_5 + N1_fixtures_co$n1_4_5 +
#     N1_fixtures_co$n1_0_6 + N1_fixtures_co$n1_1_6 + N1_fixtures_co$n1_2_6 + N1_fixtures_co$n1_3_6 + N1_fixtures_co$n1_4_6 +
#     N1_fixtures_co$n1_5_6
# )
#
# N1_fixtures_co$n1_A <- percent(N1_fixtures_co$n1_A, accuracy = 0.1)
#
# #ov25
# N1_fixtures_co$n1_ov25 <- (
#   N1_fixtures_co$n1_2_1 + N1_fixtures_co$n1_1_2 + N1_fixtures_co$n1_2_2 + N1_fixtures_co$n1_3_0 + N1_fixtures_co$n1_3_1 +
#     N1_fixtures_co$n1_3_2 + N1_fixtures_co$n1_0_3 + N1_fixtures_co$n1_1_3 + N1_fixtures_co$n1_2_3 + N1_fixtures_co$n1_3_3 +
#     N1_fixtures_co$n1_4_0 + N1_fixtures_co$n1_4_1 + N1_fixtures_co$n1_4_2 + N1_fixtures_co$n1_4_3 + N1_fixtures_co$n1_0_4 +
#     N1_fixtures_co$n1_1_4 + N1_fixtures_co$n1_2_4 + N1_fixtures_co$n1_3_4 + N1_fixtures_co$n1_4_4 + N1_fixtures_co$n1_5_0 +
#     N1_fixtures_co$n1_5_1 + N1_fixtures_co$n1_5_2 + N1_fixtures_co$n1_5_3 + N1_fixtures_co$n1_5_4 + N1_fixtures_co$n1_0_5 +
#     N1_fixtures_co$n1_1_5 + N1_fixtures_co$n1_2_5 + N1_fixtures_co$n1_3_5 + N1_fixtures_co$n1_4_5 + N1_fixtures_co$n1_5_5 +
#     N1_fixtures_co$n1_6_0 + N1_fixtures_co$n1_6_1 + N1_fixtures_co$n1_6_2 + N1_fixtures_co$n1_6_3 + N1_fixtures_co$n1_6_4 +
#     N1_fixtures_co$n1_6_5 + N1_fixtures_co$n1_0_6 + N1_fixtures_co$n1_1_6 + N1_fixtures_co$n1_2_6 + N1_fixtures_co$n1_3_6 +
#     N1_fixtures_co$n1_4_6 + N1_fixtures_co$n1_5_6 + N1_fixtures_co$n1_6_6
# )
# #un25
# N1_fixtures_co$n1_un25 <- (
#   N1_fixtures_co$n1_0_0 + N1_fixtures_co$n1_1_0 + N1_fixtures_co$n1_0_1 + N1_fixtures_co$n1_1_1 + N1_fixtures_co$n1_2_0 + N1_fixtures_co$n1_0_2
# )
# #odds
# N1_fixtures_co$n1_ov25_odds <- round((1/N1_fixtures_co$n1_ov25),digits = 2)
# N1_fixtures_co$n1_un25_odds <- round((1/N1_fixtures_co$n1_un25),digits = 2)
#
# N1_fixtures_co$n1_ov25_odds
# N1_fixtures_co$n1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# N1_fixtures_co$n1_ov25 <- percent(N1_fixtures_co$n1_ov25, accuracy = 0.1)
#
# N1_fixtures_co$n1_un25 <- percent(N1_fixtures_co$n1_un25, accuracy = 0.1)
# N1_fixtures_co$n1_pscore <- paste(round(N1_fixtures_co$n1_xHCOC,digits = 0),round(N1_fixtures_co$n1_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(N1_fixtures,'Divisions/N1.xlsx',sheetName = "N1", append = TRUE)
# #################################################################################################################
# #P1
# HomeTeam_p1_co <- rep(p1_teams, each = length(p1_teams))
# AwayTeam_p1_co <- rep(p1_teams, length(p1_teams))
# P1_fixtures_co <- cbind(HomeTeam_p1_co,AwayTeam_p1_co)
# P1_fixtures_co <- as.data.frame(P1_fixtures_co)
# P1_fixtures_co <- P1_fixtures_co[!P1_fixtures_co$HomeTeam_p1_co == P1_fixtures_co$AwayTeam_p1_co,]
# rownames(P1_fixtures_co) <- NULL
# P1_fixtures_co$Div <- "P1"
# P1_fixtures_co <- P1_fixtures_co[,c(3,1,2)]
#
# P1_fixtures_co$avg_HCO_p1 <- p1_avg_HCO
#
# P1_fixtures_co$p1_homecoas <- rep(p1_home_coas,each = length(p1_teams)-1)
#
# p1_awaycods_lookup <- cbind(p1_teams,p1_away_cods)
#
# p1_awaycods_lookup <- as.data.frame(p1_awaycods_lookup)
#
# colnames(p1_awaycods_lookup) <- c("AwayTeam_p1_co","p1_awaycods")
#
#
# require('RH2')
# P1_fixtures_co$p1_awaycods <- sqldf("SELECT p1_awaycods_lookup.p1_awaycods FROM p1_awaycods_lookup INNER JOIN P1_fixtures_co ON p1_awaycods_lookup.AwayTeam_p1_co = P1_fixtures_co.AwayTeam_p1_co")
#
# P1_fixtures_co$avg_ACO_p1 <- p1_avg_ACO
#
# p1_awaycoas_lookup <- cbind(p1_teams,p1_away_coas)
#
# p1_awaycoas_lookup <- as.data.frame(p1_awaycoas_lookup)
#
# colnames(p1_awaycoas_lookup) <- c("AwayTeam_p1_co","p1_awaycoas")
#
# P1_fixtures_co$p1_awaycoas <- sqldf("SELECT p1_awaycoas_lookup.p1_awaycoas FROM p1_awaycoas_lookup INNER JOIN P1_fixtures_co ON p1_awaycoas_lookup.AwayTeam_p1_co = P1_fixtures_co.AwayTeam_p1_co")
#
# P1_fixtures_co$p1_homecods <- rep(p1_home_cods,each = length(p1_teams)-1)
#
# P1_fixtures_co$p1_awaycods <- as.numeric(unlist(P1_fixtures_co$p1_awaycods))
# #xGH
# P1_fixtures_co$p1_xHCOC <- P1_fixtures_co$avg_HCO_p1 * P1_fixtures_co$p1_homecoas * P1_fixtures_co$p1_awaycods
# #xGA
#
# P1_fixtures_co$p1_awaycoas <- as.numeric(unlist(P1_fixtures_co$p1_awaycoas))
#
# P1_fixtures_co$p1_xACOC <- P1_fixtures_co$avg_ACO_p1 * P1_fixtures_co$p1_awaycoas * P1_fixtures_co$p1_homecods
#
# P1_fixtures_co$p1_0_0 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_0 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_1 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_1 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_0 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_2 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_2 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_1 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_2 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_3 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_0 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_1 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_2 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_3 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_3 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_3 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_4 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_0 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_1 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_2 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_3 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_4 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_4 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_4 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_4 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_5 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_0 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_1 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_2 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_3 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_4 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_5 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_5 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_5 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_5 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_5 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_6 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_0 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(0,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_1 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(1,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_2 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(2,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_3 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(3,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_4 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(4,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_6_5 <- round(stats::dpois(6,P1_fixtures_co$p1_xHCOC) * stats::dpois(5,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_0_6 <- round(stats::dpois(0,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_1_6 <- round(stats::dpois(1,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_2_6 <- round(stats::dpois(2,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_3_6 <- round(stats::dpois(3,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_4_6 <- round(stats::dpois(4,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# P1_fixtures_co$p1_5_6 <- round(stats::dpois(5,P1_fixtures_co$p1_xHCOC) * stats::dpois(6,P1_fixtures_co$p1_xACOC), digits = 4)
# #Home win
# P1_fixtures_co$p1_H <- (
#   P1_fixtures_co$p1_1_0 + P1_fixtures_co$p1_2_0 + P1_fixtures_co$p1_2_1 + P1_fixtures_co$p1_3_0 + P1_fixtures_co$p1_3_1 +
#     P1_fixtures_co$p1_3_2 + P1_fixtures_co$p1_4_0 + P1_fixtures_co$p1_4_1 + P1_fixtures_co$p1_4_2 + P1_fixtures_co$p1_4_3 +
#     P1_fixtures_co$p1_5_0 + P1_fixtures_co$p1_5_1 + P1_fixtures_co$p1_5_2 + P1_fixtures_co$p1_5_3 + P1_fixtures_co$p1_5_4 +
#     P1_fixtures_co$p1_6_0 + P1_fixtures_co$p1_6_1 + P1_fixtures_co$p1_6_2 + P1_fixtures_co$p1_6_3 + P1_fixtures_co$p1_6_4 +
#     P1_fixtures_co$p1_6_5
# )
#
# P1_fixtures_co$p1_H <- percent(P1_fixtures_co$p1_H, accuracy = 0.1)
#
# #Draw
# P1_fixtures_co$p1_D <- (
#
#   P1_fixtures_co$p1_0_0 + P1_fixtures_co$p1_1_1 + P1_fixtures_co$p1_2_2 + P1_fixtures_co$p1_3_3 + P1_fixtures_co$p1_4_4 +
#     P1_fixtures_co$p1_5_5 + P1_fixtures_co$p1_6_6
# )
#
# P1_fixtures_co$p1_D <- percent(P1_fixtures_co$p1_D, accuracy = 0.1)
#
# #Away
#
# P1_fixtures_co$p1_A <- (
#   P1_fixtures_co$p1_0_1 + P1_fixtures_co$p1_0_2 + P1_fixtures_co$p1_1_2 + P1_fixtures_co$p1_0_3 + P1_fixtures_co$p1_1_3 +
#     P1_fixtures_co$p1_2_3 + P1_fixtures_co$p1_0_4 + P1_fixtures_co$p1_1_4 + P1_fixtures_co$p1_2_4 + P1_fixtures_co$p1_3_4 +
#     P1_fixtures_co$p1_0_5 + P1_fixtures_co$p1_1_5 + P1_fixtures_co$p1_2_5 + P1_fixtures_co$p1_3_5 + P1_fixtures_co$p1_4_5 +
#     P1_fixtures_co$p1_0_6 + P1_fixtures_co$p1_1_6 + P1_fixtures_co$p1_2_6 + P1_fixtures_co$p1_3_6 + P1_fixtures_co$p1_4_6 +
#     P1_fixtures_co$p1_5_6
# )
#
# P1_fixtures_co$p1_A <- percent(P1_fixtures_co$p1_A, accuracy = 0.1)
#
# #ov25
# P1_fixtures_co$p1_ov25 <- (
#   P1_fixtures_co$p1_2_1 + P1_fixtures_co$p1_1_2 + P1_fixtures_co$p1_2_2 + P1_fixtures_co$p1_3_0 + P1_fixtures_co$p1_3_1 +
#     P1_fixtures_co$p1_3_2 + P1_fixtures_co$p1_0_3 + P1_fixtures_co$p1_1_3 + P1_fixtures_co$p1_2_3 + P1_fixtures_co$p1_3_3 +
#     P1_fixtures_co$p1_4_0 + P1_fixtures_co$p1_4_1 + P1_fixtures_co$p1_4_2 + P1_fixtures_co$p1_4_3 + P1_fixtures_co$p1_0_4 +
#     P1_fixtures_co$p1_1_4 + P1_fixtures_co$p1_2_4 + P1_fixtures_co$p1_3_4 + P1_fixtures_co$p1_4_4 + P1_fixtures_co$p1_5_0 +
#     P1_fixtures_co$p1_5_1 + P1_fixtures_co$p1_5_2 + P1_fixtures_co$p1_5_3 + P1_fixtures_co$p1_5_4 + P1_fixtures_co$p1_0_5 +
#     P1_fixtures_co$p1_1_5 + P1_fixtures_co$p1_2_5 + P1_fixtures_co$p1_3_5 + P1_fixtures_co$p1_4_5 + P1_fixtures_co$p1_5_5 +
#     P1_fixtures_co$p1_6_0 + P1_fixtures_co$p1_6_1 + P1_fixtures_co$p1_6_2 + P1_fixtures_co$p1_6_3 + P1_fixtures_co$p1_6_4 +
#     P1_fixtures_co$p1_6_5 + P1_fixtures_co$p1_0_6 + P1_fixtures_co$p1_1_6 + P1_fixtures_co$p1_2_6 + P1_fixtures_co$p1_3_6 +
#     P1_fixtures_co$p1_4_6 + P1_fixtures_co$p1_5_6 + P1_fixtures_co$p1_6_6
# )
# #un25
# P1_fixtures_co$p1_un25 <- (
#   P1_fixtures_co$p1_0_0 + P1_fixtures_co$p1_1_0 + P1_fixtures_co$p1_0_1 + P1_fixtures_co$p1_1_1 + P1_fixtures_co$p1_2_0 + P1_fixtures_co$p1_0_2
# )
# #odds
# P1_fixtures_co$p1_ov25_odds <- round((1/P1_fixtures_co$p1_ov25),digits = 2)
# P1_fixtures_co$p1_un25_odds <- round((1/P1_fixtures_co$p1_un25),digits = 2)
#
# P1_fixtures_co$p1_ov25_odds
# P1_fixtures_co$p1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# P1_fixtures_co$p1_ov25 <- percent(P1_fixtures_co$p1_ov25, accuracy = 0.1)
#
# P1_fixtures_co$p1_un25 <- percent(P1_fixtures_co$p1_un25, accuracy = 0.1)
# P1_fixtures_co$p1_pscore <- paste(round(P1_fixtures_co$p1_xHCOC,digits = 0),round(P1_fixtures_co$p1_xACOC,digits = 0),sep = "-")
# #write.xlsx(P1_fixtures,'Divisions/P1.xlsx',sheetName = "P1", append = TRUE)
# #################################################################################################################
# #SC0
# HomeTeam_sc0_co <- rep(sc0_teams, each = length(sc0_teams))
# AwayTeam_sc0_co <- rep(sc0_teams, length(sc0_teams))
# SC0_fixtures_co <- cbind(HomeTeam_sc0_co,AwayTeam_sc0_co)
# SC0_fixtures_co <- as.data.frame(SC0_fixtures_co)
# SC0_fixtures_co <- SC0_fixtures_co[!SC0_fixtures_co$HomeTeam_sc0_co == SC0_fixtures_co$AwayTeam_sc0_co,]
# rownames(SC0_fixtures_co) <- NULL
# SC0_fixtures_co$Div <- "SC0"
# SC0_fixtures_co <- SC0_fixtures_co[,c(3,1,2)]
#
# SC0_fixtures_co$avg_HCO_sc0 <- sc0_avg_HCO
#
# SC0_fixtures_co$sc0_homecoas <- rep(sc0_home_coas,each = length(sc0_teams)-1)
#
# sc0_awaycods_lookup <- cbind(sc0_teams,sc0_away_cods)
#
# sc0_awaycods_lookup <- as.data.frame(sc0_awaycods_lookup)
#
# colnames(sc0_awaycods_lookup) <- c("AwayTeam_sc0_co","sc0_awaycods")
#
#
# require('RH2')
# SC0_fixtures_co$sc0_awaycods <- sqldf("SELECT sc0_awaycods_lookup.sc0_awaycods FROM sc0_awaycods_lookup INNER JOIN SC0_fixtures_co ON sc0_awaycods_lookup.AwayTeam_sc0_co = SC0_fixtures_co.AwayTeam_sc0_co")
#
# SC0_fixtures_co$avg_ACO_sc0 <- sc0_avg_ACO
#
# sc0_awaycoas_lookup <- cbind(sc0_teams,sc0_away_coas)
#
# sc0_awaycoas_lookup <- as.data.frame(sc0_awaycoas_lookup)
#
# colnames(sc0_awaycoas_lookup) <- c("AwayTeam_sc0_co","sc0_awaycoas")
#
# SC0_fixtures_co$sc0_awaycoas <- sqldf("SELECT sc0_awaycoas_lookup.sc0_awaycoas FROM sc0_awaycoas_lookup INNER JOIN SC0_fixtures_co ON sc0_awaycoas_lookup.AwayTeam_sc0_co = SC0_fixtures_co.AwayTeam_sc0_co")
#
# SC0_fixtures_co$sc0_homecods <- rep(sc0_home_cods,each = length(sc0_teams)-1)
#
# SC0_fixtures_co$sc0_awaycods <- as.numeric(unlist(SC0_fixtures_co$sc0_awaycods))
# #xGH
# SC0_fixtures_co$sc0_xHCOC <- SC0_fixtures_co$avg_HCO_sc0 * SC0_fixtures_co$sc0_homecoas * SC0_fixtures_co$sc0_awaycods
# #xGA
#
# SC0_fixtures_co$sc0_awaycoas <- as.numeric(unlist(SC0_fixtures_co$sc0_awaycoas))
#
# SC0_fixtures_co$sc0_xACOC <- SC0_fixtures_co$avg_ACO_sc0 * SC0_fixtures_co$sc0_awaycoas * SC0_fixtures_co$sc0_homecods
#
# SC0_fixtures_co$sc0_0_0 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_0 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_1 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_1 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_0 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_2 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_2 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_1 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_2 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_3 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_0 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_1 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_2 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_3 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_3 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_3 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_4 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_0 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_1 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_2 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_3 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_4 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_4 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_4 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_4 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_5 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_0 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_1 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_2 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_3 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_4 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_5 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_5 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_5 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_5 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_5 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_6 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_0 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(0,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_1 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(1,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_2 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(2,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_3 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(3,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_4 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(4,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_6_5 <- round(stats::dpois(6,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(5,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_0_6 <- round(stats::dpois(0,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_1_6 <- round(stats::dpois(1,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_2_6 <- round(stats::dpois(2,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_3_6 <- round(stats::dpois(3,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_4_6 <- round(stats::dpois(4,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# SC0_fixtures_co$sc0_5_6 <- round(stats::dpois(5,SC0_fixtures_co$sc0_xHCOC) * stats::dpois(6,SC0_fixtures_co$sc0_xACOC), digits = 4)
# #Home win
# SC0_fixtures_co$sc0_H <- (
#   SC0_fixtures_co$sc0_1_0 + SC0_fixtures_co$sc0_2_0 + SC0_fixtures_co$sc0_2_1 + SC0_fixtures_co$sc0_3_0 + SC0_fixtures_co$sc0_3_1 +
#     SC0_fixtures_co$sc0_3_2 + SC0_fixtures_co$sc0_4_0 + SC0_fixtures_co$sc0_4_1 + SC0_fixtures_co$sc0_4_2 + SC0_fixtures_co$sc0_4_3 +
#     SC0_fixtures_co$sc0_5_0 + SC0_fixtures_co$sc0_5_1 + SC0_fixtures_co$sc0_5_2 + SC0_fixtures_co$sc0_5_3 + SC0_fixtures_co$sc0_5_4 +
#     SC0_fixtures_co$sc0_6_0 + SC0_fixtures_co$sc0_6_1 + SC0_fixtures_co$sc0_6_2 + SC0_fixtures_co$sc0_6_3 + SC0_fixtures_co$sc0_6_4 +
#     SC0_fixtures_co$sc0_6_5
# )
#
# SC0_fixtures_co$sc0_H <- percent(SC0_fixtures_co$sc0_H, accuracy = 0.1)
#
# #Draw
# SC0_fixtures_co$sc0_D <- (
#
#   SC0_fixtures_co$sc0_0_0 + SC0_fixtures_co$sc0_1_1 + SC0_fixtures_co$sc0_2_2 + SC0_fixtures_co$sc0_3_3 + SC0_fixtures_co$sc0_4_4 +
#     SC0_fixtures_co$sc0_5_5 + SC0_fixtures_co$sc0_6_6
# )
#
# SC0_fixtures_co$sc0_D <- percent(SC0_fixtures_co$sc0_D, accuracy = 0.1)
#
# #Away
#
# SC0_fixtures_co$sc0_A <- (
#   SC0_fixtures_co$sc0_0_1 + SC0_fixtures_co$sc0_0_2 + SC0_fixtures_co$sc0_1_2 + SC0_fixtures_co$sc0_0_3 + SC0_fixtures_co$sc0_1_3 +
#     SC0_fixtures_co$sc0_2_3 + SC0_fixtures_co$sc0_0_4 + SC0_fixtures_co$sc0_1_4 + SC0_fixtures_co$sc0_2_4 + SC0_fixtures_co$sc0_3_4 +
#     SC0_fixtures_co$sc0_0_5 + SC0_fixtures_co$sc0_1_5 + SC0_fixtures_co$sc0_2_5 + SC0_fixtures_co$sc0_3_5 + SC0_fixtures_co$sc0_4_5 +
#     SC0_fixtures_co$sc0_0_6 + SC0_fixtures_co$sc0_1_6 + SC0_fixtures_co$sc0_2_6 + SC0_fixtures_co$sc0_3_6 + SC0_fixtures_co$sc0_4_6 +
#     SC0_fixtures_co$sc0_5_6
# )
#
# SC0_fixtures_co$sc0_A <- percent(SC0_fixtures_co$sc0_A, accuracy = 0.1)
#
# #ov25
# SC0_fixtures_co$sc0_ov25 <- (
#   SC0_fixtures_co$sc0_2_1 + SC0_fixtures_co$sc0_1_2 + SC0_fixtures_co$sc0_2_2 + SC0_fixtures_co$sc0_3_0 + SC0_fixtures_co$sc0_3_1 +
#     SC0_fixtures_co$sc0_3_2 + SC0_fixtures_co$sc0_0_3 + SC0_fixtures_co$sc0_1_3 + SC0_fixtures_co$sc0_2_3 + SC0_fixtures_co$sc0_3_3 +
#     SC0_fixtures_co$sc0_4_0 + SC0_fixtures_co$sc0_4_1 + SC0_fixtures_co$sc0_4_2 + SC0_fixtures_co$sc0_4_3 + SC0_fixtures_co$sc0_0_4 +
#     SC0_fixtures_co$sc0_1_4 + SC0_fixtures_co$sc0_2_4 + SC0_fixtures_co$sc0_3_4 + SC0_fixtures_co$sc0_4_4 + SC0_fixtures_co$sc0_5_0 +
#     SC0_fixtures_co$sc0_5_1 + SC0_fixtures_co$sc0_5_2 + SC0_fixtures_co$sc0_5_3 + SC0_fixtures_co$sc0_5_4 + SC0_fixtures_co$sc0_0_5 +
#     SC0_fixtures_co$sc0_1_5 + SC0_fixtures_co$sc0_2_5 + SC0_fixtures_co$sc0_3_5 + SC0_fixtures_co$sc0_4_5 + SC0_fixtures_co$sc0_5_5 +
#     SC0_fixtures_co$sc0_6_0 + SC0_fixtures_co$sc0_6_1 + SC0_fixtures_co$sc0_6_2 + SC0_fixtures_co$sc0_6_3 + SC0_fixtures_co$sc0_6_4 +
#     SC0_fixtures_co$sc0_6_5 + SC0_fixtures_co$sc0_0_6 + SC0_fixtures_co$sc0_1_6 + SC0_fixtures_co$sc0_2_6 + SC0_fixtures_co$sc0_3_6 +
#     SC0_fixtures_co$sc0_4_6 + SC0_fixtures_co$sc0_5_6 + SC0_fixtures_co$sc0_6_6
# )
# #un25
# SC0_fixtures_co$sc0_un25 <- (
#   SC0_fixtures_co$sc0_0_0 + SC0_fixtures_co$sc0_1_0 + SC0_fixtures_co$sc0_0_1 + SC0_fixtures_co$sc0_1_1 + SC0_fixtures_co$sc0_2_0 + SC0_fixtures_co$sc0_0_2
# )
# #odds
# SC0_fixtures_co$sc0_ov25_odds <- round((1/SC0_fixtures_co$sc0_ov25),digits = 2)
# SC0_fixtures_co$sc0_un25_odds <- round((1/SC0_fixtures_co$sc0_un25),digits = 2)
#
# SC0_fixtures_co$sc0_ov25_odds
# SC0_fixtures_co$sc0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC0_fixtures_co$sc0_ov25 <- percent(SC0_fixtures_co$sc0_ov25, accuracy = 0.1)
#
# SC0_fixtures_co$sc0_un25 <- percent(SC0_fixtures_co$sc0_un25, accuracy = 0.1)
# SC0_fixtures_co$sc0_pscore <- paste(round(SC0_fixtures_co$sc0_xHCOC,digits = 0),round(SC0_fixtures_co$sc0_xACOC,digits = 0),sep = "-")
# #write.xlsx(SC0_fixtures,'Divisions/SC0.xlsx',sheetName = "SC0", append = TRUE)
# #################################################################################################################
# #SC1
# HomeTeam_sc1_co <- rep(sc1_teams, each = length(sc1_teams))
# AwayTeam_sc1_co <- rep(sc1_teams, length(sc1_teams))
# SC1_fixtures_co <- cbind(HomeTeam_sc1_co,AwayTeam_sc1_co)
# SC1_fixtures_co <- as.data.frame(SC1_fixtures_co)
# SC1_fixtures_co <- SC1_fixtures_co[!SC1_fixtures_co$HomeTeam_sc1_co == SC1_fixtures_co$AwayTeam_sc1_co,]
# rownames(SC1_fixtures_co) <- NULL
# SC1_fixtures_co$Div <- "SC1"
# SC1_fixtures_co <- SC1_fixtures_co[,c(3,1,2)]
#
# SC1_fixtures_co$avg_HCO_sc1 <- sc1_avg_HCO
#
# SC1_fixtures_co$sc1_homecoas <- rep(sc1_home_coas,each = length(sc1_teams)-1)
#
# sc1_awaycods_lookup <- cbind(sc1_teams,sc1_away_cods)
#
# sc1_awaycods_lookup <- as.data.frame(sc1_awaycods_lookup)
#
# colnames(sc1_awaycods_lookup) <- c("AwayTeam_sc1_co","sc1_awaycods")
#
#
# require('RH2')
# SC1_fixtures_co$sc1_awaycods <- sqldf("SELECT sc1_awaycods_lookup.sc1_awaycods FROM sc1_awaycods_lookup INNER JOIN SC1_fixtures_co ON sc1_awaycods_lookup.AwayTeam_sc1_co = SC1_fixtures_co.AwayTeam_sc1_co")
#
# SC1_fixtures_co$avg_ACO_sc1 <- sc1_avg_ACO
#
# sc1_awaycoas_lookup <- cbind(sc1_teams,sc1_away_coas)
#
# sc1_awaycoas_lookup <- as.data.frame(sc1_awaycoas_lookup)
#
# colnames(sc1_awaycoas_lookup) <- c("AwayTeam_sc1_co","sc1_awaycoas")
#
# SC1_fixtures_co$sc1_awaycoas <- sqldf("SELECT sc1_awaycoas_lookup.sc1_awaycoas FROM sc1_awaycoas_lookup INNER JOIN SC1_fixtures_co ON sc1_awaycoas_lookup.AwayTeam_sc1_co = SC1_fixtures_co.AwayTeam_sc1_co")
#
# SC1_fixtures_co$sc1_homecods <- rep(sc1_home_cods,each = length(sc1_teams)-1)
#
# SC1_fixtures_co$sc1_awaycods <- as.numeric(unlist(SC1_fixtures_co$sc1_awaycods))
# #xGH
# SC1_fixtures_co$sc1_xHCOC <- SC1_fixtures_co$avg_HCO_sc1 * SC1_fixtures_co$sc1_homecoas * SC1_fixtures_co$sc1_awaycods
# #xGA
#
# SC1_fixtures_co$sc1_awaycoas <- as.numeric(unlist(SC1_fixtures_co$sc1_awaycoas))
#
# SC1_fixtures_co$sc1_xACOC <- SC1_fixtures_co$avg_ACO_sc1 * SC1_fixtures_co$sc1_awaycoas * SC1_fixtures_co$sc1_homecods
#
# SC1_fixtures_co$sc1_0_0 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_0 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_1 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_1 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_0 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_2 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_2 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_1 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_2 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_3 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_0 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_1 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_2 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_3 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_3 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_3 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_4 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_0 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_1 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_2 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_3 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_4 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_4 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_4 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_4 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_5 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_0 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_1 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_2 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_3 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_4 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_5 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_5 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_5 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_5 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_5 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_6 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_0 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(0,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_1 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(1,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_2 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(2,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_3 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(3,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_4 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(4,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_6_5 <- round(stats::dpois(6,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(5,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_0_6 <- round(stats::dpois(0,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_1_6 <- round(stats::dpois(1,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_2_6 <- round(stats::dpois(2,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_3_6 <- round(stats::dpois(3,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_4_6 <- round(stats::dpois(4,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# SC1_fixtures_co$sc1_5_6 <- round(stats::dpois(5,SC1_fixtures_co$sc1_xHCOC) * stats::dpois(6,SC1_fixtures_co$sc1_xACOC), digits = 4)
# #Home win
# SC1_fixtures_co$sc1_H <- (
#   SC1_fixtures_co$sc1_1_0 + SC1_fixtures_co$sc1_2_0 + SC1_fixtures_co$sc1_2_1 + SC1_fixtures_co$sc1_3_0 + SC1_fixtures_co$sc1_3_1 +
#     SC1_fixtures_co$sc1_3_2 + SC1_fixtures_co$sc1_4_0 + SC1_fixtures_co$sc1_4_1 + SC1_fixtures_co$sc1_4_2 + SC1_fixtures_co$sc1_4_3 +
#     SC1_fixtures_co$sc1_5_0 + SC1_fixtures_co$sc1_5_1 + SC1_fixtures_co$sc1_5_2 + SC1_fixtures_co$sc1_5_3 + SC1_fixtures_co$sc1_5_4 +
#     SC1_fixtures_co$sc1_6_0 + SC1_fixtures_co$sc1_6_1 + SC1_fixtures_co$sc1_6_2 + SC1_fixtures_co$sc1_6_3 + SC1_fixtures_co$sc1_6_4 +
#     SC1_fixtures_co$sc1_6_5
# )
#
# SC1_fixtures_co$sc1_H <- percent(SC1_fixtures_co$sc1_H, accuracy = 0.1)
#
# #Draw
# SC1_fixtures_co$sc1_D <- (
#
#   SC1_fixtures_co$sc1_0_0 + SC1_fixtures_co$sc1_1_1 + SC1_fixtures_co$sc1_2_2 + SC1_fixtures_co$sc1_3_3 + SC1_fixtures_co$sc1_4_4 +
#     SC1_fixtures_co$sc1_5_5 + SC1_fixtures_co$sc1_6_6
# )
#
# SC1_fixtures_co$sc1_D <- percent(SC1_fixtures_co$sc1_D, accuracy = 0.1)
#
# #Away
#
# SC1_fixtures_co$sc1_A <- (
#   SC1_fixtures_co$sc1_0_1 + SC1_fixtures_co$sc1_0_2 + SC1_fixtures_co$sc1_1_2 + SC1_fixtures_co$sc1_0_3 + SC1_fixtures_co$sc1_1_3 +
#     SC1_fixtures_co$sc1_2_3 + SC1_fixtures_co$sc1_0_4 + SC1_fixtures_co$sc1_1_4 + SC1_fixtures_co$sc1_2_4 + SC1_fixtures_co$sc1_3_4 +
#     SC1_fixtures_co$sc1_0_5 + SC1_fixtures_co$sc1_1_5 + SC1_fixtures_co$sc1_2_5 + SC1_fixtures_co$sc1_3_5 + SC1_fixtures_co$sc1_4_5 +
#     SC1_fixtures_co$sc1_0_6 + SC1_fixtures_co$sc1_1_6 + SC1_fixtures_co$sc1_2_6 + SC1_fixtures_co$sc1_3_6 + SC1_fixtures_co$sc1_4_6 +
#     SC1_fixtures_co$sc1_5_6
# )
#
# SC1_fixtures_co$sc1_A <- percent(SC1_fixtures_co$sc1_A, accuracy = 0.1)
#
# #ov25
# SC1_fixtures_co$sc1_ov25 <- (
#   SC1_fixtures_co$sc1_2_1 + SC1_fixtures_co$sc1_1_2 + SC1_fixtures_co$sc1_2_2 + SC1_fixtures_co$sc1_3_0 + SC1_fixtures_co$sc1_3_1 +
#     SC1_fixtures_co$sc1_3_2 + SC1_fixtures_co$sc1_0_3 + SC1_fixtures_co$sc1_1_3 + SC1_fixtures_co$sc1_2_3 + SC1_fixtures_co$sc1_3_3 +
#     SC1_fixtures_co$sc1_4_0 + SC1_fixtures_co$sc1_4_1 + SC1_fixtures_co$sc1_4_2 + SC1_fixtures_co$sc1_4_3 + SC1_fixtures_co$sc1_0_4 +
#     SC1_fixtures_co$sc1_1_4 + SC1_fixtures_co$sc1_2_4 + SC1_fixtures_co$sc1_3_4 + SC1_fixtures_co$sc1_4_4 + SC1_fixtures_co$sc1_5_0 +
#     SC1_fixtures_co$sc1_5_1 + SC1_fixtures_co$sc1_5_2 + SC1_fixtures_co$sc1_5_3 + SC1_fixtures_co$sc1_5_4 + SC1_fixtures_co$sc1_0_5 +
#     SC1_fixtures_co$sc1_1_5 + SC1_fixtures_co$sc1_2_5 + SC1_fixtures_co$sc1_3_5 + SC1_fixtures_co$sc1_4_5 + SC1_fixtures_co$sc1_5_5 +
#     SC1_fixtures_co$sc1_6_0 + SC1_fixtures_co$sc1_6_1 + SC1_fixtures_co$sc1_6_2 + SC1_fixtures_co$sc1_6_3 + SC1_fixtures_co$sc1_6_4 +
#     SC1_fixtures_co$sc1_6_5 + SC1_fixtures_co$sc1_0_6 + SC1_fixtures_co$sc1_1_6 + SC1_fixtures_co$sc1_2_6 + SC1_fixtures_co$sc1_3_6 +
#     SC1_fixtures_co$sc1_4_6 + SC1_fixtures_co$sc1_5_6 + SC1_fixtures_co$sc1_6_6
# )
# #un25
# SC1_fixtures_co$sc1_un25 <- (
#   SC1_fixtures_co$sc1_0_0 + SC1_fixtures_co$sc1_1_0 + SC1_fixtures_co$sc1_0_1 + SC1_fixtures_co$sc1_1_1 + SC1_fixtures_co$sc1_2_0 + SC1_fixtures_co$sc1_0_2
# )
# #odds
# SC1_fixtures_co$sc1_ov25_odds <- round((1/SC1_fixtures_co$sc1_ov25),digits = 2)
# SC1_fixtures_co$sc1_un25_odds <- round((1/SC1_fixtures_co$sc1_un25),digits = 2)
#
# SC1_fixtures_co$sc1_ov25_odds
# SC1_fixtures_co$sc1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC1_fixtures_co$sc1_ov25 <- percent(SC1_fixtures_co$sc1_ov25, accuracy = 0.1)
#
# SC1_fixtures_co$sc1_un25 <- percent(SC1_fixtures_co$sc1_un25, accuracy = 0.1)
# SC1_fixtures_co$sc1_pscore <- paste(round(SC1_fixtures_co$sc1_xHCOC,digits = 0),round(SC1_fixtures_co$sc1_xACOC,digits = 0),sep = "-")
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# ##########################################################################################################################################
# #SC2
# HomeTeam_sc2_co <- rep(sc2_teams, each = length(sc2_teams))
# AwayTeam_sc2_co <- rep(sc2_teams, length(sc2_teams))
# SC2_fixtures_co <- cbind(HomeTeam_sc2_co,AwayTeam_sc2_co)
# SC2_fixtures_co <- as.data.frame(SC2_fixtures_co)
# SC2_fixtures_co <- SC2_fixtures_co[!SC2_fixtures_co$HomeTeam_sc2_co == SC2_fixtures_co$AwayTeam_sc2_co,]
# rownames(SC2_fixtures_co) <- NULL
# SC2_fixtures_co$Div <- "SC2"
# SC2_fixtures_co <- SC2_fixtures_co[,c(3,1,2)]
#
# SC2_fixtures_co$avg_HCO_sc2 <- sc2_avg_HCO
#
# SC2_fixtures_co$sc2_homecoas <- rep(sc2_home_coas,each = length(sc2_teams)-1)
#
# sc2_awaycods_lookup <- cbind(sc2_teams,sc2_away_cods)
#
# sc2_awaycods_lookup <- as.data.frame(sc2_awaycods_lookup)
#
# colnames(sc2_awaycods_lookup) <- c("AwayTeam_sc2_co","sc2_awaycods")
#
#
# require('RH2')
# SC2_fixtures_co$sc2_awaycods <- sqldf("SELECT sc2_awaycods_lookup.sc2_awaycods FROM sc2_awaycods_lookup INNER JOIN SC2_fixtures_co ON sc2_awaycods_lookup.AwayTeam_sc2_co = SC2_fixtures_co.AwayTeam_sc2_co")
#
# SC2_fixtures_co$avg_ACO_sc2 <- sc2_avg_ACO
#
# sc2_awaycoas_lookup <- cbind(sc2_teams,sc2_away_coas)
#
# sc2_awaycoas_lookup <- as.data.frame(sc2_awaycoas_lookup)
#
# colnames(sc2_awaycoas_lookup) <- c("AwayTeam_sc2_co","sc2_awaycoas")
#
# SC2_fixtures_co$sc2_awaycoas <- sqldf("SELECT sc2_awaycoas_lookup.sc2_awaycoas FROM sc2_awaycoas_lookup INNER JOIN SC2_fixtures_co ON sc2_awaycoas_lookup.AwayTeam_sc2_co = SC2_fixtures_co.AwayTeam_sc2_co")
#
# SC2_fixtures_co$sc2_homecods <- rep(sc2_home_cods,each = length(sc2_teams)-1)
#
# SC2_fixtures_co$sc2_awaycods <- as.numeric(unlist(SC2_fixtures_co$sc2_awaycods))
# #xGH
# SC2_fixtures_co$sc2_xHCOC <- SC2_fixtures_co$avg_HCO_sc2 * SC2_fixtures_co$sc2_homecoas * SC2_fixtures_co$sc2_awaycods
# #xGA
#
# SC2_fixtures_co$sc2_awaycoas <- as.numeric(unlist(SC2_fixtures_co$sc2_awaycoas))
#
# SC2_fixtures_co$sc2_xACOC <- SC2_fixtures_co$avg_ACO_sc2 * SC2_fixtures_co$sc2_awaycoas * SC2_fixtures_co$sc2_homecods
#
# SC2_fixtures_co$sc2_0_0 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_0 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_1 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_1 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_0 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_2 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_2 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_1 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_2 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_3 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_0 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_1 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_2 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_3 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_3 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_3 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_4 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_0 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_1 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_2 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_3 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_4 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_4 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_4 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_4 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_5 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_0 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_1 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_2 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_3 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_4 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_5 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_5 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_5 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_5 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_5 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_6 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_0 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(0,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_1 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(1,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_2 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(2,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_3 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(3,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_4 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(4,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_6_5 <- round(stats::dpois(6,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(5,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_0_6 <- round(stats::dpois(0,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_1_6 <- round(stats::dpois(1,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_2_6 <- round(stats::dpois(2,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_3_6 <- round(stats::dpois(3,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_4_6 <- round(stats::dpois(4,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# SC2_fixtures_co$sc2_5_6 <- round(stats::dpois(5,SC2_fixtures_co$sc2_xHCOC) * stats::dpois(6,SC2_fixtures_co$sc2_xACOC), digits = 4)
# #Home win
# SC2_fixtures_co$sc2_H <- (
#   SC2_fixtures_co$sc2_1_0 + SC2_fixtures_co$sc2_2_0 + SC2_fixtures_co$sc2_2_1 + SC2_fixtures_co$sc2_3_0 + SC2_fixtures_co$sc2_3_1 +
#     SC2_fixtures_co$sc2_3_2 + SC2_fixtures_co$sc2_4_0 + SC2_fixtures_co$sc2_4_1 + SC2_fixtures_co$sc2_4_2 + SC2_fixtures_co$sc2_4_3 +
#     SC2_fixtures_co$sc2_5_0 + SC2_fixtures_co$sc2_5_1 + SC2_fixtures_co$sc2_5_2 + SC2_fixtures_co$sc2_5_3 + SC2_fixtures_co$sc2_5_4 +
#     SC2_fixtures_co$sc2_6_0 + SC2_fixtures_co$sc2_6_1 + SC2_fixtures_co$sc2_6_2 + SC2_fixtures_co$sc2_6_3 + SC2_fixtures_co$sc2_6_4 +
#     SC2_fixtures_co$sc2_6_5
# )
#
# SC2_fixtures_co$sc2_H <- percent(SC2_fixtures_co$sc2_H, accuracy = 0.1)
#
# #Draw
# SC2_fixtures_co$sc2_D <- (
#
#   SC2_fixtures_co$sc2_0_0 + SC2_fixtures_co$sc2_1_1 + SC2_fixtures_co$sc2_2_2 + SC2_fixtures_co$sc2_3_3 + SC2_fixtures_co$sc2_4_4 +
#     SC2_fixtures_co$sc2_5_5 + SC2_fixtures_co$sc2_6_6
# )
#
# SC2_fixtures_co$sc2_D <- percent(SC2_fixtures_co$sc2_D, accuracy = 0.1)
#
# #Away
#
# SC2_fixtures_co$sc2_A <- (
#   SC2_fixtures_co$sc2_0_1 + SC2_fixtures_co$sc2_0_2 + SC2_fixtures_co$sc2_1_2 + SC2_fixtures_co$sc2_0_3 + SC2_fixtures_co$sc2_1_3 +
#     SC2_fixtures_co$sc2_2_3 + SC2_fixtures_co$sc2_0_4 + SC2_fixtures_co$sc2_1_4 + SC2_fixtures_co$sc2_2_4 + SC2_fixtures_co$sc2_3_4 +
#     SC2_fixtures_co$sc2_0_5 + SC2_fixtures_co$sc2_1_5 + SC2_fixtures_co$sc2_2_5 + SC2_fixtures_co$sc2_3_5 + SC2_fixtures_co$sc2_4_5 +
#     SC2_fixtures_co$sc2_0_6 + SC2_fixtures_co$sc2_1_6 + SC2_fixtures_co$sc2_2_6 + SC2_fixtures_co$sc2_3_6 + SC2_fixtures_co$sc2_4_6 +
#     SC2_fixtures_co$sc2_5_6
# )
#
# SC2_fixtures_co$sc2_A <- percent(SC2_fixtures_co$sc2_A, accuracy = 0.1)
#
# #ov25
# SC2_fixtures_co$sc2_ov25 <- (
#   SC2_fixtures_co$sc2_2_1 + SC2_fixtures_co$sc2_1_2 + SC2_fixtures_co$sc2_2_2 + SC2_fixtures_co$sc2_3_0 + SC2_fixtures_co$sc2_3_1 +
#     SC2_fixtures_co$sc2_3_2 + SC2_fixtures_co$sc2_0_3 + SC2_fixtures_co$sc2_1_3 + SC2_fixtures_co$sc2_2_3 + SC2_fixtures_co$sc2_3_3 +
#     SC2_fixtures_co$sc2_4_0 + SC2_fixtures_co$sc2_4_1 + SC2_fixtures_co$sc2_4_2 + SC2_fixtures_co$sc2_4_3 + SC2_fixtures_co$sc2_0_4 +
#     SC2_fixtures_co$sc2_1_4 + SC2_fixtures_co$sc2_2_4 + SC2_fixtures_co$sc2_3_4 + SC2_fixtures_co$sc2_4_4 + SC2_fixtures_co$sc2_5_0 +
#     SC2_fixtures_co$sc2_5_1 + SC2_fixtures_co$sc2_5_2 + SC2_fixtures_co$sc2_5_3 + SC2_fixtures_co$sc2_5_4 + SC2_fixtures_co$sc2_0_5 +
#     SC2_fixtures_co$sc2_1_5 + SC2_fixtures_co$sc2_2_5 + SC2_fixtures_co$sc2_3_5 + SC2_fixtures_co$sc2_4_5 + SC2_fixtures_co$sc2_5_5 +
#     SC2_fixtures_co$sc2_6_0 + SC2_fixtures_co$sc2_6_1 + SC2_fixtures_co$sc2_6_2 + SC2_fixtures_co$sc2_6_3 + SC2_fixtures_co$sc2_6_4 +
#     SC2_fixtures_co$sc2_6_5 + SC2_fixtures_co$sc2_0_6 + SC2_fixtures_co$sc2_1_6 + SC2_fixtures_co$sc2_2_6 + SC2_fixtures_co$sc2_3_6 +
#     SC2_fixtures_co$sc2_4_6 + SC2_fixtures_co$sc2_5_6 + SC2_fixtures_co$sc2_6_6
# )
# #un25
# SC2_fixtures_co$sc2_un25 <- (
#   SC2_fixtures_co$sc2_0_0 + SC2_fixtures_co$sc2_1_0 + SC2_fixtures_co$sc2_0_1 + SC2_fixtures_co$sc2_1_1 + SC2_fixtures_co$sc2_2_0 + SC2_fixtures_co$sc2_0_2
# )
# #odds
# SC2_fixtures_co$sc2_ov25_odds <- round((1/SC2_fixtures_co$sc2_ov25),digits = 2)
# SC2_fixtures_co$sc2_un25_odds <- round((1/SC2_fixtures_co$sc2_un25),digits = 2)
#
# SC2_fixtures_co$sc2_ov25_odds
# SC2_fixtures_co$sc2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC2_fixtures_co$sc2_ov25 <- percent(SC2_fixtures_co$sc2_ov25, accuracy = 0.1)
#
# SC2_fixtures_co$sc2_un25 <- percent(SC2_fixtures_co$sc2_un25, accuracy = 0.1)
# SC2_fixtures_co$sc2_pscore <- paste(round(SC2_fixtures_co$sc2_xHCOC,digits = 0),round(SC2_fixtures_co$sc2_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC2_fixtures,'Divisions/SC2.xlsx',sheetName = "SC2", append = TRUE)
# ################################################################################################################################################################################################################################
# #SC3
# HomeTeam_sc3_co <- rep(sc3_teams, each = length(sc3_teams))
# AwayTeam_sc3_co <- rep(sc3_teams, length(sc3_teams))
# SC3_fixtures_co <- cbind(HomeTeam_sc3_co,AwayTeam_sc3_co)
# SC3_fixtures_co <- as.data.frame(SC3_fixtures_co)
# SC3_fixtures_co <- SC3_fixtures_co[!SC3_fixtures_co$HomeTeam_sc3_co == SC3_fixtures_co$AwayTeam_sc3_co,]
# rownames(SC3_fixtures_co) <- NULL
# SC3_fixtures_co$Div <- "SC3"
# SC3_fixtures_co <- SC3_fixtures_co[,c(3,1,2)]
#
# SC3_fixtures_co$avg_HCO_sc3 <- sc3_avg_HCO
#
# SC3_fixtures_co$sc3_homecoas <- rep(sc3_home_coas,each = length(sc3_teams)-1)
#
# sc3_awaycods_lookup <- cbind(sc3_teams,sc3_away_cods)
#
# sc3_awaycods_lookup <- as.data.frame(sc3_awaycods_lookup)
#
# colnames(sc3_awaycods_lookup) <- c("AwayTeam_sc3_co","sc3_awaycods")
#
#
# require('RH2')
# SC3_fixtures_co$sc3_awaycods <- sqldf("SELECT sc3_awaycods_lookup.sc3_awaycods FROM sc3_awaycods_lookup INNER JOIN SC3_fixtures_co ON sc3_awaycods_lookup.AwayTeam_sc3_co = SC3_fixtures_co.AwayTeam_sc3_co")
#
# SC3_fixtures_co$avg_ACO_sc3 <- sc3_avg_ACO
#
# sc3_awaycoas_lookup <- cbind(sc3_teams,sc3_away_coas)
#
# sc3_awaycoas_lookup <- as.data.frame(sc3_awaycoas_lookup)
#
# colnames(sc3_awaycoas_lookup) <- c("AwayTeam_sc3_co","sc3_awaycoas")
#
# SC3_fixtures_co$sc3_awaycoas <- sqldf("SELECT sc3_awaycoas_lookup.sc3_awaycoas FROM sc3_awaycoas_lookup INNER JOIN SC3_fixtures_co ON sc3_awaycoas_lookup.AwayTeam_sc3_co = SC3_fixtures_co.AwayTeam_sc3_co")
#
# SC3_fixtures_co$sc3_homecods <- rep(sc3_home_cods,each = length(sc3_teams)-1)
#
# SC3_fixtures_co$sc3_awaycods <- as.numeric(unlist(SC3_fixtures_co$sc3_awaycods))
# #xGH
# SC3_fixtures_co$sc3_xHCOC <- SC3_fixtures_co$avg_HCO_sc3 * SC3_fixtures_co$sc3_homecoas * SC3_fixtures_co$sc3_awaycods
# #xGA
#
# SC3_fixtures_co$sc3_awaycoas <- as.numeric(unlist(SC3_fixtures_co$sc3_awaycoas))
#
# SC3_fixtures_co$sc3_xACOC <- SC3_fixtures_co$avg_ACO_sc3 * SC3_fixtures_co$sc3_awaycoas * SC3_fixtures_co$sc3_homecods
#
# SC3_fixtures_co$sc3_0_0 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_0 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_1 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_1 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_0 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_2 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_2 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_1 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_2 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_3 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_0 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_1 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_2 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_3 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_3 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_3 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_4 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_0 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_1 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_2 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_3 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_4 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_4 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_4 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_4 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_5 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_0 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_1 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_2 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_3 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_4 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_5 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_5 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_5 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_5 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_5 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_6 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_0 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(0,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_1 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(1,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_2 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(2,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_3 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(3,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_4 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(4,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_6_5 <- round(stats::dpois(6,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(5,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_0_6 <- round(stats::dpois(0,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_1_6 <- round(stats::dpois(1,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_2_6 <- round(stats::dpois(2,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_3_6 <- round(stats::dpois(3,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_4_6 <- round(stats::dpois(4,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# SC3_fixtures_co$sc3_5_6 <- round(stats::dpois(5,SC3_fixtures_co$sc3_xHCOC) * stats::dpois(6,SC3_fixtures_co$sc3_xACOC), digits = 4)
# #Home win
# SC3_fixtures_co$sc3_H <- (
#   SC3_fixtures_co$sc3_1_0 + SC3_fixtures_co$sc3_2_0 + SC3_fixtures_co$sc3_2_1 + SC3_fixtures_co$sc3_3_0 + SC3_fixtures_co$sc3_3_1 +
#     SC3_fixtures_co$sc3_3_2 + SC3_fixtures_co$sc3_4_0 + SC3_fixtures_co$sc3_4_1 + SC3_fixtures_co$sc3_4_2 + SC3_fixtures_co$sc3_4_3 +
#     SC3_fixtures_co$sc3_5_0 + SC3_fixtures_co$sc3_5_1 + SC3_fixtures_co$sc3_5_2 + SC3_fixtures_co$sc3_5_3 + SC3_fixtures_co$sc3_5_4 +
#     SC3_fixtures_co$sc3_6_0 + SC3_fixtures_co$sc3_6_1 + SC3_fixtures_co$sc3_6_2 + SC3_fixtures_co$sc3_6_3 + SC3_fixtures_co$sc3_6_4 +
#     SC3_fixtures_co$sc3_6_5
# )
#
# SC3_fixtures_co$sc3_H <- percent(SC3_fixtures_co$sc3_H, accuracy = 0.1)
#
# #Draw
# SC3_fixtures_co$sc3_D <- (
#
#   SC3_fixtures_co$sc3_0_0 + SC3_fixtures_co$sc3_1_1 + SC3_fixtures_co$sc3_2_2 + SC3_fixtures_co$sc3_3_3 + SC3_fixtures_co$sc3_4_4 +
#     SC3_fixtures_co$sc3_5_5 + SC3_fixtures_co$sc3_6_6
# )
#
# SC3_fixtures_co$sc3_D <- percent(SC3_fixtures_co$sc3_D, accuracy = 0.1)
#
# #Away
#
# SC3_fixtures_co$sc3_A <- (
#   SC3_fixtures_co$sc3_0_1 + SC3_fixtures_co$sc3_0_2 + SC3_fixtures_co$sc3_1_2 + SC3_fixtures_co$sc3_0_3 + SC3_fixtures_co$sc3_1_3 +
#     SC3_fixtures_co$sc3_2_3 + SC3_fixtures_co$sc3_0_4 + SC3_fixtures_co$sc3_1_4 + SC3_fixtures_co$sc3_2_4 + SC3_fixtures_co$sc3_3_4 +
#     SC3_fixtures_co$sc3_0_5 + SC3_fixtures_co$sc3_1_5 + SC3_fixtures_co$sc3_2_5 + SC3_fixtures_co$sc3_3_5 + SC3_fixtures_co$sc3_4_5 +
#     SC3_fixtures_co$sc3_0_6 + SC3_fixtures_co$sc3_1_6 + SC3_fixtures_co$sc3_2_6 + SC3_fixtures_co$sc3_3_6 + SC3_fixtures_co$sc3_4_6 +
#     SC3_fixtures_co$sc3_5_6
# )
#
# SC3_fixtures_co$sc3_A <- percent(SC3_fixtures_co$sc3_A, accuracy = 0.1)
#
# #ov25
# SC3_fixtures_co$sc3_ov25 <- (
#   SC3_fixtures_co$sc3_2_1 + SC3_fixtures_co$sc3_1_2 + SC3_fixtures_co$sc3_2_2 + SC3_fixtures_co$sc3_3_0 + SC3_fixtures_co$sc3_3_1 +
#     SC3_fixtures_co$sc3_3_2 + SC3_fixtures_co$sc3_0_3 + SC3_fixtures_co$sc3_1_3 + SC3_fixtures_co$sc3_2_3 + SC3_fixtures_co$sc3_3_3 +
#     SC3_fixtures_co$sc3_4_0 + SC3_fixtures_co$sc3_4_1 + SC3_fixtures_co$sc3_4_2 + SC3_fixtures_co$sc3_4_3 + SC3_fixtures_co$sc3_0_4 +
#     SC3_fixtures_co$sc3_1_4 + SC3_fixtures_co$sc3_2_4 + SC3_fixtures_co$sc3_3_4 + SC3_fixtures_co$sc3_4_4 + SC3_fixtures_co$sc3_5_0 +
#     SC3_fixtures_co$sc3_5_1 + SC3_fixtures_co$sc3_5_2 + SC3_fixtures_co$sc3_5_3 + SC3_fixtures_co$sc3_5_4 + SC3_fixtures_co$sc3_0_5 +
#     SC3_fixtures_co$sc3_1_5 + SC3_fixtures_co$sc3_2_5 + SC3_fixtures_co$sc3_3_5 + SC3_fixtures_co$sc3_4_5 + SC3_fixtures_co$sc3_5_5 +
#     SC3_fixtures_co$sc3_6_0 + SC3_fixtures_co$sc3_6_1 + SC3_fixtures_co$sc3_6_2 + SC3_fixtures_co$sc3_6_3 + SC3_fixtures_co$sc3_6_4 +
#     SC3_fixtures_co$sc3_6_5 + SC3_fixtures_co$sc3_0_6 + SC3_fixtures_co$sc3_1_6 + SC3_fixtures_co$sc3_2_6 + SC3_fixtures_co$sc3_3_6 +
#     SC3_fixtures_co$sc3_4_6 + SC3_fixtures_co$sc3_5_6 + SC3_fixtures_co$sc3_6_6
# )
# #un25
# SC3_fixtures_co$sc3_un25 <- (
#   SC3_fixtures_co$sc3_0_0 + SC3_fixtures_co$sc3_1_0 + SC3_fixtures_co$sc3_0_1 + SC3_fixtures_co$sc3_1_1 + SC3_fixtures_co$sc3_2_0 + SC3_fixtures_co$sc3_0_2
# )
# #odds
# SC3_fixtures_co$sc3_ov25_odds <- round((1/SC3_fixtures_co$sc3_ov25),digits = 2)
# SC3_fixtures_co$sc3_un25_odds <- round((1/SC3_fixtures_co$sc3_un25),digits = 2)
#
# SC3_fixtures_co$sc3_ov25_odds
# SC3_fixtures_co$sc3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC3_fixtures_co$sc3_ov25 <- percent(SC3_fixtures_co$sc3_ov25, accuracy = 0.1)
#
# SC3_fixtures_co$sc3_un25 <- percent(SC3_fixtures_co$sc3_un25, accuracy = 0.1)
# SC3_fixtures_co$sc3_pscore <- paste(round(SC3_fixtures_co$sc3_xHCOC,digits = 0),round(SC3_fixtures_co$sc3_xACOC,digits = 0),sep = "-")
# #write.xlsx(SC3_fixtures,'Divisions/SC3.xlsx',sheetName = "SC3", append = TRUE)
# #################################################################################################################
# ################################################################################################################
# #################################################################################################################
# #SP1
# HomeTeam_sp1_co <- rep(sp1_teams, each = length(sp1_teams))
# AwayTeam_sp1_co <- rep(sp1_teams, length(sp1_teams))
# SP1_fixtures_co <- cbind(HomeTeam_sp1_co,AwayTeam_sp1_co)
# SP1_fixtures_co <- as.data.frame(SP1_fixtures_co)
# SP1_fixtures_co <- SP1_fixtures_co[!SP1_fixtures_co$HomeTeam_sp1_co == SP1_fixtures_co$AwayTeam_sp1_co,]
# rownames(SP1_fixtures_co) <- NULL
# SP1_fixtures_co$Div <- "SP1"
# SP1_fixtures_co <- SP1_fixtures_co[,c(3,1,2)]
#
# SP1_fixtures_co$avg_HCO_sp1 <- sp1_avg_HCO
#
# SP1_fixtures_co$sp1_homecoas <- rep(sp1_home_coas,each = length(sp1_teams)-1)
#
# sp1_awaycods_lookup <- cbind(sp1_teams,sp1_away_cods)
#
# sp1_awaycods_lookup <- as.data.frame(sp1_awaycods_lookup)
#
# colnames(sp1_awaycods_lookup) <- c("AwayTeam_sp1_co","sp1_awaycods")
#
#
# require('RH2')
# SP1_fixtures_co$sp1_awaycods <- sqldf("SELECT sp1_awaycods_lookup.sp1_awaycods FROM sp1_awaycods_lookup INNER JOIN SP1_fixtures_co ON sp1_awaycods_lookup.AwayTeam_sp1_co = SP1_fixtures_co.AwayTeam_sp1_co")
#
# SP1_fixtures_co$avg_ACO_sp1 <- sp1_avg_ACO
#
# sp1_awaycoas_lookup <- cbind(sp1_teams,sp1_away_coas)
#
# sp1_awaycoas_lookup <- as.data.frame(sp1_awaycoas_lookup)
#
# colnames(sp1_awaycoas_lookup) <- c("AwayTeam_sp1_co","sp1_awaycoas")
#
# SP1_fixtures_co$sp1_awaycoas <- sqldf("SELECT sp1_awaycoas_lookup.sp1_awaycoas FROM sp1_awaycoas_lookup INNER JOIN SP1_fixtures_co ON sp1_awaycoas_lookup.AwayTeam_sp1_co = SP1_fixtures_co.AwayTeam_sp1_co")
#
# SP1_fixtures_co$sp1_homecods <- rep(sp1_home_cods,each = length(sp1_teams)-1)
#
# SP1_fixtures_co$sp1_awaycods <- as.numeric(unlist(SP1_fixtures_co$sp1_awaycods))
# #xGH
# SP1_fixtures_co$sp1_xHCOC <- SP1_fixtures_co$avg_HCO_sp1 * SP1_fixtures_co$sp1_homecoas * SP1_fixtures_co$sp1_awaycods
# #xGA
#
# SP1_fixtures_co$sp1_awaycoas <- as.numeric(unlist(SP1_fixtures_co$sp1_awaycoas))
#
# SP1_fixtures_co$sp1_xACOC <- SP1_fixtures_co$avg_ACO_sp1 * SP1_fixtures_co$sp1_awaycoas * SP1_fixtures_co$sp1_homecods
#
# SP1_fixtures_co$sp1_0_0 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_0 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_1 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_1 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_0 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_2 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_2 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_1 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_2 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_3 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_0 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_1 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_2 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_3 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_3 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_3 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_4 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_0 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_1 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_2 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_3 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_4 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_4 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_4 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_4 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_5 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_0 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_1 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_2 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_3 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_4 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_5 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_5 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_5 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_5 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_5 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_6 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_0 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(0,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_1 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(1,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_2 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(2,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_3 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(3,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_4 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(4,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_6_5 <- round(stats::dpois(6,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(5,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_0_6 <- round(stats::dpois(0,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_1_6 <- round(stats::dpois(1,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_2_6 <- round(stats::dpois(2,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_3_6 <- round(stats::dpois(3,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_4_6 <- round(stats::dpois(4,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# SP1_fixtures_co$sp1_5_6 <- round(stats::dpois(5,SP1_fixtures_co$sp1_xHCOC) * stats::dpois(6,SP1_fixtures_co$sp1_xACOC), digits = 4)
# #Home win
# SP1_fixtures_co$sp1_H <- (
#   SP1_fixtures_co$sp1_1_0 + SP1_fixtures_co$sp1_2_0 + SP1_fixtures_co$sp1_2_1 + SP1_fixtures_co$sp1_3_0 + SP1_fixtures_co$sp1_3_1 +
#     SP1_fixtures_co$sp1_3_2 + SP1_fixtures_co$sp1_4_0 + SP1_fixtures_co$sp1_4_1 + SP1_fixtures_co$sp1_4_2 + SP1_fixtures_co$sp1_4_3 +
#     SP1_fixtures_co$sp1_5_0 + SP1_fixtures_co$sp1_5_1 + SP1_fixtures_co$sp1_5_2 + SP1_fixtures_co$sp1_5_3 + SP1_fixtures_co$sp1_5_4 +
#     SP1_fixtures_co$sp1_6_0 + SP1_fixtures_co$sp1_6_1 + SP1_fixtures_co$sp1_6_2 + SP1_fixtures_co$sp1_6_3 + SP1_fixtures_co$sp1_6_4 +
#     SP1_fixtures_co$sp1_6_5
# )
#
# SP1_fixtures_co$sp1_H <- percent(SP1_fixtures_co$sp1_H, accuracy = 0.1)
#
# #Draw
# SP1_fixtures_co$sp1_D <- (
#
#   SP1_fixtures_co$sp1_0_0 + SP1_fixtures_co$sp1_1_1 + SP1_fixtures_co$sp1_2_2 + SP1_fixtures_co$sp1_3_3 + SP1_fixtures_co$sp1_4_4 +
#     SP1_fixtures_co$sp1_5_5 + SP1_fixtures_co$sp1_6_6
# )
#
# SP1_fixtures_co$sp1_D <- percent(SP1_fixtures_co$sp1_D, accuracy = 0.1)
#
# #Away
#
# SP1_fixtures_co$sp1_A <- (
#   SP1_fixtures_co$sp1_0_1 + SP1_fixtures_co$sp1_0_2 + SP1_fixtures_co$sp1_1_2 + SP1_fixtures_co$sp1_0_3 + SP1_fixtures_co$sp1_1_3 +
#     SP1_fixtures_co$sp1_2_3 + SP1_fixtures_co$sp1_0_4 + SP1_fixtures_co$sp1_1_4 + SP1_fixtures_co$sp1_2_4 + SP1_fixtures_co$sp1_3_4 +
#     SP1_fixtures_co$sp1_0_5 + SP1_fixtures_co$sp1_1_5 + SP1_fixtures_co$sp1_2_5 + SP1_fixtures_co$sp1_3_5 + SP1_fixtures_co$sp1_4_5 +
#     SP1_fixtures_co$sp1_0_6 + SP1_fixtures_co$sp1_1_6 + SP1_fixtures_co$sp1_2_6 + SP1_fixtures_co$sp1_3_6 + SP1_fixtures_co$sp1_4_6 +
#     SP1_fixtures_co$sp1_5_6
# )
#
# SP1_fixtures_co$sp1_A <- percent(SP1_fixtures_co$sp1_A, accuracy = 0.1)
#
# #ov25
# SP1_fixtures_co$sp1_ov25 <- (
#   SP1_fixtures_co$sp1_2_1 + SP1_fixtures_co$sp1_1_2 + SP1_fixtures_co$sp1_2_2 + SP1_fixtures_co$sp1_3_0 + SP1_fixtures_co$sp1_3_1 +
#     SP1_fixtures_co$sp1_3_2 + SP1_fixtures_co$sp1_0_3 + SP1_fixtures_co$sp1_1_3 + SP1_fixtures_co$sp1_2_3 + SP1_fixtures_co$sp1_3_3 +
#     SP1_fixtures_co$sp1_4_0 + SP1_fixtures_co$sp1_4_1 + SP1_fixtures_co$sp1_4_2 + SP1_fixtures_co$sp1_4_3 + SP1_fixtures_co$sp1_0_4 +
#     SP1_fixtures_co$sp1_1_4 + SP1_fixtures_co$sp1_2_4 + SP1_fixtures_co$sp1_3_4 + SP1_fixtures_co$sp1_4_4 + SP1_fixtures_co$sp1_5_0 +
#     SP1_fixtures_co$sp1_5_1 + SP1_fixtures_co$sp1_5_2 + SP1_fixtures_co$sp1_5_3 + SP1_fixtures_co$sp1_5_4 + SP1_fixtures_co$sp1_0_5 +
#     SP1_fixtures_co$sp1_1_5 + SP1_fixtures_co$sp1_2_5 + SP1_fixtures_co$sp1_3_5 + SP1_fixtures_co$sp1_4_5 + SP1_fixtures_co$sp1_5_5 +
#     SP1_fixtures_co$sp1_6_0 + SP1_fixtures_co$sp1_6_1 + SP1_fixtures_co$sp1_6_2 + SP1_fixtures_co$sp1_6_3 + SP1_fixtures_co$sp1_6_4 +
#     SP1_fixtures_co$sp1_6_5 + SP1_fixtures_co$sp1_0_6 + SP1_fixtures_co$sp1_1_6 + SP1_fixtures_co$sp1_2_6 + SP1_fixtures_co$sp1_3_6 +
#     SP1_fixtures_co$sp1_4_6 + SP1_fixtures_co$sp1_5_6 + SP1_fixtures_co$sp1_6_6
# )
# #un25
# SP1_fixtures_co$sp1_un25 <- (
#   SP1_fixtures_co$sp1_0_0 + SP1_fixtures_co$sp1_1_0 + SP1_fixtures_co$sp1_0_1 + SP1_fixtures_co$sp1_1_1 + SP1_fixtures_co$sp1_2_0 + SP1_fixtures_co$sp1_0_2
# )
# #odds
# SP1_fixtures_co$sp1_ov25_odds <- round((1/SP1_fixtures_co$sp1_ov25),digits = 2)
# SP1_fixtures_co$sp1_un25_odds <- round((1/SP1_fixtures_co$sp1_un25),digits = 2)
#
# SP1_fixtures_co$sp1_ov25_odds
# SP1_fixtures_co$sp1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP1_fixtures_co$sp1_ov25 <- percent(SP1_fixtures_co$sp1_ov25, accuracy = 0.1)
#
# SP1_fixtures_co$sp1_un25 <- percent(SP1_fixtures_co$sp1_un25, accuracy = 0.1)
# SP1_fixtures_co$sp1_pscore <- paste(round(SP1_fixtures_co$sp1_xHCOC,digits = 0),round(SP1_fixtures_co$sp1_xACOC,digits = 0),sep = "-")
# #write.xlsx(SP1_fixtures,'Divisions/SP1.xlsx',sheetName = "SP1", append = TRUE)
# #################################################################################################################
# #SP2
# HomeTeam_sp2_co <- rep(sp2_teams, each = length(sp2_teams))
# AwayTeam_sp2_co <- rep(sp2_teams, length(sp2_teams))
# SP2_fixtures_co <- cbind(HomeTeam_sp2_co,AwayTeam_sp2_co)
# SP2_fixtures_co <- as.data.frame(SP2_fixtures_co)
# SP2_fixtures_co <- SP2_fixtures_co[!SP2_fixtures_co$HomeTeam_sp2_co == SP2_fixtures_co$AwayTeam_sp2_co,]
# rownames(SP2_fixtures_co) <- NULL
# SP2_fixtures_co$Div <- "SP2"
# SP2_fixtures_co <- SP2_fixtures_co[,c(3,1,2)]
#
# SP2_fixtures_co$avg_HCO_sp2 <- sp2_avg_HCO
#
# SP2_fixtures_co$sp2_homecoas <- rep(sp2_home_coas,each = length(sp2_teams)-1)
#
# sp2_awaycods_lookup <- cbind(sp2_teams,sp2_away_cods)
#
# sp2_awaycods_lookup <- as.data.frame(sp2_awaycods_lookup)
#
# colnames(sp2_awaycods_lookup) <- c("AwayTeam_sp2_co","sp2_awaycods")
#
#
# require('RH2')
# SP2_fixtures_co$sp2_awaycods <- sqldf("SELECT sp2_awaycods_lookup.sp2_awaycods FROM sp2_awaycods_lookup INNER JOIN SP2_fixtures_co ON sp2_awaycods_lookup.AwayTeam_sp2_co = SP2_fixtures_co.AwayTeam_sp2_co")
#
# SP2_fixtures_co$avg_ACO_sp2 <- sp2_avg_ACO
#
# sp2_awaycoas_lookup <- cbind(sp2_teams,sp2_away_coas)
#
# sp2_awaycoas_lookup <- as.data.frame(sp2_awaycoas_lookup)
#
# colnames(sp2_awaycoas_lookup) <- c("AwayTeam_sp2_co","sp2_awaycoas")
#
# SP2_fixtures_co$sp2_awaycoas <- sqldf("SELECT sp2_awaycoas_lookup.sp2_awaycoas FROM sp2_awaycoas_lookup INNER JOIN SP2_fixtures_co ON sp2_awaycoas_lookup.AwayTeam_sp2_co = SP2_fixtures_co.AwayTeam_sp2_co")
#
# SP2_fixtures_co$sp2_homecods <- rep(sp2_home_cods,each = length(sp2_teams)-1)
#
# SP2_fixtures_co$sp2_awaycods <- as.numeric(unlist(SP2_fixtures_co$sp2_awaycods))
# #xGH
# SP2_fixtures_co$sp2_xHCOC <- SP2_fixtures_co$avg_HCO_sp2 * SP2_fixtures_co$sp2_homecoas * SP2_fixtures_co$sp2_awaycods
# #xGA
#
# SP2_fixtures_co$sp2_awaycoas <- as.numeric(unlist(SP2_fixtures_co$sp2_awaycoas))
#
# SP2_fixtures_co$sp2_xACOC <- SP2_fixtures_co$avg_ACO_sp2 * SP2_fixtures_co$sp2_awaycoas * SP2_fixtures_co$sp2_homecods
#
# SP2_fixtures_co$sp2_0_0 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_0 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_1 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_1 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_0 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_2 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_2 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_1 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_2 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_3 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_0 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_1 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_2 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_3 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_3 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_3 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_4 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_0 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_1 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_2 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_3 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_4 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_4 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_4 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_4 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_5 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_0 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_1 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_2 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_3 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_4 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_5 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_5 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_5 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_5 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_5 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_6 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_0 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(0,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_1 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(1,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_2 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(2,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_3 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(3,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_4 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(4,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_6_5 <- round(stats::dpois(6,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(5,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_0_6 <- round(stats::dpois(0,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_1_6 <- round(stats::dpois(1,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_2_6 <- round(stats::dpois(2,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_3_6 <- round(stats::dpois(3,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_4_6 <- round(stats::dpois(4,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# SP2_fixtures_co$sp2_5_6 <- round(stats::dpois(5,SP2_fixtures_co$sp2_xHCOC) * stats::dpois(6,SP2_fixtures_co$sp2_xACOC), digits = 4)
# #Home win
# SP2_fixtures_co$sp2_H <- (
#   SP2_fixtures_co$sp2_1_0 + SP2_fixtures_co$sp2_2_0 + SP2_fixtures_co$sp2_2_1 + SP2_fixtures_co$sp2_3_0 + SP2_fixtures_co$sp2_3_1 +
#     SP2_fixtures_co$sp2_3_2 + SP2_fixtures_co$sp2_4_0 + SP2_fixtures_co$sp2_4_1 + SP2_fixtures_co$sp2_4_2 + SP2_fixtures_co$sp2_4_3 +
#     SP2_fixtures_co$sp2_5_0 + SP2_fixtures_co$sp2_5_1 + SP2_fixtures_co$sp2_5_2 + SP2_fixtures_co$sp2_5_3 + SP2_fixtures_co$sp2_5_4 +
#     SP2_fixtures_co$sp2_6_0 + SP2_fixtures_co$sp2_6_1 + SP2_fixtures_co$sp2_6_2 + SP2_fixtures_co$sp2_6_3 + SP2_fixtures_co$sp2_6_4 +
#     SP2_fixtures_co$sp2_6_5
# )
#
# SP2_fixtures_co$sp2_H <- percent(SP2_fixtures_co$sp2_H, accuracy = 0.1)
#
# #Draw
# SP2_fixtures_co$sp2_D <- (
#
#   SP2_fixtures_co$sp2_0_0 + SP2_fixtures_co$sp2_1_1 + SP2_fixtures_co$sp2_2_2 + SP2_fixtures_co$sp2_3_3 + SP2_fixtures_co$sp2_4_4 +
#     SP2_fixtures_co$sp2_5_5 + SP2_fixtures_co$sp2_6_6
# )
#
# SP2_fixtures_co$sp2_D <- percent(SP2_fixtures_co$sp2_D, accuracy = 0.1)
#
# #Away
#
# SP2_fixtures_co$sp2_A <- (
#   SP2_fixtures_co$sp2_0_1 + SP2_fixtures_co$sp2_0_2 + SP2_fixtures_co$sp2_1_2 + SP2_fixtures_co$sp2_0_3 + SP2_fixtures_co$sp2_1_3 +
#     SP2_fixtures_co$sp2_2_3 + SP2_fixtures_co$sp2_0_4 + SP2_fixtures_co$sp2_1_4 + SP2_fixtures_co$sp2_2_4 + SP2_fixtures_co$sp2_3_4 +
#     SP2_fixtures_co$sp2_0_5 + SP2_fixtures_co$sp2_1_5 + SP2_fixtures_co$sp2_2_5 + SP2_fixtures_co$sp2_3_5 + SP2_fixtures_co$sp2_4_5 +
#     SP2_fixtures_co$sp2_0_6 + SP2_fixtures_co$sp2_1_6 + SP2_fixtures_co$sp2_2_6 + SP2_fixtures_co$sp2_3_6 + SP2_fixtures_co$sp2_4_6 +
#     SP2_fixtures_co$sp2_5_6
# )
#
# SP2_fixtures_co$sp2_A <- percent(SP2_fixtures_co$sp2_A, accuracy = 0.1)
#
# #ov25
# SP2_fixtures_co$sp2_ov25 <- (
#   SP2_fixtures_co$sp2_2_1 + SP2_fixtures_co$sp2_1_2 + SP2_fixtures_co$sp2_2_2 + SP2_fixtures_co$sp2_3_0 + SP2_fixtures_co$sp2_3_1 +
#     SP2_fixtures_co$sp2_3_2 + SP2_fixtures_co$sp2_0_3 + SP2_fixtures_co$sp2_1_3 + SP2_fixtures_co$sp2_2_3 + SP2_fixtures_co$sp2_3_3 +
#     SP2_fixtures_co$sp2_4_0 + SP2_fixtures_co$sp2_4_1 + SP2_fixtures_co$sp2_4_2 + SP2_fixtures_co$sp2_4_3 + SP2_fixtures_co$sp2_0_4 +
#     SP2_fixtures_co$sp2_1_4 + SP2_fixtures_co$sp2_2_4 + SP2_fixtures_co$sp2_3_4 + SP2_fixtures_co$sp2_4_4 + SP2_fixtures_co$sp2_5_0 +
#     SP2_fixtures_co$sp2_5_1 + SP2_fixtures_co$sp2_5_2 + SP2_fixtures_co$sp2_5_3 + SP2_fixtures_co$sp2_5_4 + SP2_fixtures_co$sp2_0_5 +
#     SP2_fixtures_co$sp2_1_5 + SP2_fixtures_co$sp2_2_5 + SP2_fixtures_co$sp2_3_5 + SP2_fixtures_co$sp2_4_5 + SP2_fixtures_co$sp2_5_5 +
#     SP2_fixtures_co$sp2_6_0 + SP2_fixtures_co$sp2_6_1 + SP2_fixtures_co$sp2_6_2 + SP2_fixtures_co$sp2_6_3 + SP2_fixtures_co$sp2_6_4 +
#     SP2_fixtures_co$sp2_6_5 + SP2_fixtures_co$sp2_0_6 + SP2_fixtures_co$sp2_1_6 + SP2_fixtures_co$sp2_2_6 + SP2_fixtures_co$sp2_3_6 +
#     SP2_fixtures_co$sp2_4_6 + SP2_fixtures_co$sp2_5_6 + SP2_fixtures_co$sp2_6_6
# )
# #un25
# SP2_fixtures_co$sp2_un25 <- (
#   SP2_fixtures_co$sp2_0_0 + SP2_fixtures_co$sp2_1_0 + SP2_fixtures_co$sp2_0_1 + SP2_fixtures_co$sp2_1_1 + SP2_fixtures_co$sp2_2_0 + SP2_fixtures_co$sp2_0_2
# )
# #odds
# SP2_fixtures_co$sp2_ov25_odds <- round((1/SP2_fixtures_co$sp2_ov25),digits = 2)
# SP2_fixtures_co$sp2_un25_odds <- round((1/SP2_fixtures_co$sp2_un25),digits = 2)
#
# SP2_fixtures_co$sp2_ov25_odds
# SP2_fixtures_co$sp2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP2_fixtures_co$sp2_ov25 <- percent(SP2_fixtures_co$sp2_ov25, accuracy = 0.1)
#
# SP2_fixtures_co$sp2_un25 <- percent(SP2_fixtures_co$sp2_un25, accuracy = 0.1)
# SP2_fixtures_co$sp2_pscore <- paste(round(SP2_fixtures_co$sp2_xHCOC,digits = 0),round(SP2_fixtures_co$sp2_xACOC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SP2_fixtures,'Divisions/SP2.xlsx',sheetName = "SP2", append = TRUE)
# #################################################################################################################
# #T1
# HomeTeam_t1_co <- rep(t1_teams, each = length(t1_teams))
# AwayTeam_t1_co <- rep(t1_teams, length(t1_teams))
# T1_fixtures_co <- cbind(HomeTeam_t1_co,AwayTeam_t1_co)
# T1_fixtures_co <- as.data.frame(T1_fixtures_co)
# T1_fixtures_co <- T1_fixtures_co[!T1_fixtures_co$HomeTeam_t1_co == T1_fixtures_co$AwayTeam_t1_co,]
# rownames(T1_fixtures_co) <- NULL
# T1_fixtures_co$Div <- "T1"
# T1_fixtures_co <- T1_fixtures_co[,c(3,1,2)]
#
# T1_fixtures_co$avg_HCO_t1 <- t1_avg_HCO
#
# T1_fixtures_co$t1_homecoas <- rep(t1_home_coas,each = length(t1_teams)-1)
#
# t1_awaycods_lookup <- cbind(t1_teams,t1_away_cods)
#
# t1_awaycods_lookup <- as.data.frame(t1_awaycods_lookup)
#
# colnames(t1_awaycods_lookup) <- c("AwayTeam_t1_co","t1_awaycods")
#
#
# require('RH2')
# T1_fixtures_co$t1_awaycods <- sqldf("SELECT t1_awaycods_lookup.t1_awaycods FROM t1_awaycods_lookup INNER JOIN T1_fixtures_co ON t1_awaycods_lookup.AwayTeam_t1_co = T1_fixtures_co.AwayTeam_t1_co")
#
# T1_fixtures_co$avg_ACO_t1 <- t1_avg_ACO
#
# t1_awaycoas_lookup <- cbind(t1_teams,t1_away_coas)
#
# t1_awaycoas_lookup <- as.data.frame(t1_awaycoas_lookup)
#
# colnames(t1_awaycoas_lookup) <- c("AwayTeam_t1_co","t1_awaycoas")
#
# T1_fixtures_co$t1_awaycoas <- sqldf("SELECT t1_awaycoas_lookup.t1_awaycoas FROM t1_awaycoas_lookup INNER JOIN T1_fixtures_co ON t1_awaycoas_lookup.AwayTeam_t1_co = T1_fixtures_co.AwayTeam_t1_co")
#
# T1_fixtures_co$t1_homecods <- rep(t1_home_cods,each = length(t1_teams)-1)
#
# T1_fixtures_co$t1_awaycods <- as.numeric(unlist(T1_fixtures_co$t1_awaycods))
# #xGH
# T1_fixtures_co$t1_xHCOC <- T1_fixtures_co$avg_HCO_t1 * T1_fixtures_co$t1_homecoas * T1_fixtures_co$t1_awaycods
# #xGA
#
# T1_fixtures_co$t1_awaycoas <- as.numeric(unlist(T1_fixtures_co$t1_awaycoas))
#
# T1_fixtures_co$t1_xACOC <- T1_fixtures_co$avg_ACO_t1 * T1_fixtures_co$t1_awaycoas * T1_fixtures_co$t1_homecods
#
# T1_fixtures_co$t1_0_0 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_0 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_1 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_1 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_0 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_2 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_2 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_1 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_2 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_3 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_0 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_1 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_2 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_3 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_3 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_3 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_4 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_0 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_1 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_2 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_3 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_4 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_4 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_4 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_4 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_5 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_0 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_1 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_2 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_3 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_4 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_5 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_5 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_5 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_5 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_5 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_6 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_0 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(0,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_1 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(1,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_2 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(2,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_3 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(3,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_4 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(4,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_6_5 <- round(stats::dpois(6,T1_fixtures_co$t1_xHCOC) * stats::dpois(5,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_0_6 <- round(stats::dpois(0,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_1_6 <- round(stats::dpois(1,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_2_6 <- round(stats::dpois(2,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_3_6 <- round(stats::dpois(3,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_4_6 <- round(stats::dpois(4,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# T1_fixtures_co$t1_5_6 <- round(stats::dpois(5,T1_fixtures_co$t1_xHCOC) * stats::dpois(6,T1_fixtures_co$t1_xACOC), digits = 4)
# #Home win
# T1_fixtures_co$t1_H <- (
#   T1_fixtures_co$t1_1_0 + T1_fixtures_co$t1_2_0 + T1_fixtures_co$t1_2_1 + T1_fixtures_co$t1_3_0 + T1_fixtures_co$t1_3_1 +
#     T1_fixtures_co$t1_3_2 + T1_fixtures_co$t1_4_0 + T1_fixtures_co$t1_4_1 + T1_fixtures_co$t1_4_2 + T1_fixtures_co$t1_4_3 +
#     T1_fixtures_co$t1_5_0 + T1_fixtures_co$t1_5_1 + T1_fixtures_co$t1_5_2 + T1_fixtures_co$t1_5_3 + T1_fixtures_co$t1_5_4 +
#     T1_fixtures_co$t1_6_0 + T1_fixtures_co$t1_6_1 + T1_fixtures_co$t1_6_2 + T1_fixtures_co$t1_6_3 + T1_fixtures_co$t1_6_4 +
#     T1_fixtures_co$t1_6_5
# )
#
# T1_fixtures_co$t1_H <- percent(T1_fixtures_co$t1_H, accuracy = 0.1)
#
# #Draw
# T1_fixtures_co$t1_D <- (
#
#   T1_fixtures_co$t1_0_0 + T1_fixtures_co$t1_1_1 + T1_fixtures_co$t1_2_2 + T1_fixtures_co$t1_3_3 + T1_fixtures_co$t1_4_4 +
#     T1_fixtures_co$t1_5_5 + T1_fixtures_co$t1_6_6
# )
#
# T1_fixtures_co$t1_D <- percent(T1_fixtures_co$t1_D, accuracy = 0.1)
#
# #Away
#
# T1_fixtures_co$t1_A <- (
#   T1_fixtures_co$t1_0_1 + T1_fixtures_co$t1_0_2 + T1_fixtures_co$t1_1_2 + T1_fixtures_co$t1_0_3 + T1_fixtures_co$t1_1_3 +
#     T1_fixtures_co$t1_2_3 + T1_fixtures_co$t1_0_4 + T1_fixtures_co$t1_1_4 + T1_fixtures_co$t1_2_4 + T1_fixtures_co$t1_3_4 +
#     T1_fixtures_co$t1_0_5 + T1_fixtures_co$t1_1_5 + T1_fixtures_co$t1_2_5 + T1_fixtures_co$t1_3_5 + T1_fixtures_co$t1_4_5 +
#     T1_fixtures_co$t1_0_6 + T1_fixtures_co$t1_1_6 + T1_fixtures_co$t1_2_6 + T1_fixtures_co$t1_3_6 + T1_fixtures_co$t1_4_6 +
#     T1_fixtures_co$t1_5_6
# )
#
# T1_fixtures_co$t1_A <- percent(T1_fixtures_co$t1_A, accuracy = 0.1)
#
# #ov25
# T1_fixtures_co$t1_ov25 <- (
#   T1_fixtures_co$t1_2_1 + T1_fixtures_co$t1_1_2 + T1_fixtures_co$t1_2_2 + T1_fixtures_co$t1_3_0 + T1_fixtures_co$t1_3_1 +
#     T1_fixtures_co$t1_3_2 + T1_fixtures_co$t1_0_3 + T1_fixtures_co$t1_1_3 + T1_fixtures_co$t1_2_3 + T1_fixtures_co$t1_3_3 +
#     T1_fixtures_co$t1_4_0 + T1_fixtures_co$t1_4_1 + T1_fixtures_co$t1_4_2 + T1_fixtures_co$t1_4_3 + T1_fixtures_co$t1_0_4 +
#     T1_fixtures_co$t1_1_4 + T1_fixtures_co$t1_2_4 + T1_fixtures_co$t1_3_4 + T1_fixtures_co$t1_4_4 + T1_fixtures_co$t1_5_0 +
#     T1_fixtures_co$t1_5_1 + T1_fixtures_co$t1_5_2 + T1_fixtures_co$t1_5_3 + T1_fixtures_co$t1_5_4 + T1_fixtures_co$t1_0_5 +
#     T1_fixtures_co$t1_1_5 + T1_fixtures_co$t1_2_5 + T1_fixtures_co$t1_3_5 + T1_fixtures_co$t1_4_5 + T1_fixtures_co$t1_5_5 +
#     T1_fixtures_co$t1_6_0 + T1_fixtures_co$t1_6_1 + T1_fixtures_co$t1_6_2 + T1_fixtures_co$t1_6_3 + T1_fixtures_co$t1_6_4 +
#     T1_fixtures_co$t1_6_5 + T1_fixtures_co$t1_0_6 + T1_fixtures_co$t1_1_6 + T1_fixtures_co$t1_2_6 + T1_fixtures_co$t1_3_6 +
#     T1_fixtures_co$t1_4_6 + T1_fixtures_co$t1_5_6 + T1_fixtures_co$t1_6_6
# )
# #un25
# T1_fixtures_co$t1_un25 <- (
#   T1_fixtures_co$t1_0_0 + T1_fixtures_co$t1_1_0 + T1_fixtures_co$t1_0_1 + T1_fixtures_co$t1_1_1 + T1_fixtures_co$t1_2_0 + T1_fixtures_co$t1_0_2
# )
# #odds
# T1_fixtures_co$t1_ov25_odds <- round((1/T1_fixtures_co$t1_ov25),digits = 2)
# T1_fixtures_co$t1_un25_odds <- round((1/T1_fixtures_co$t1_un25),digits = 2)
#
# T1_fixtures_co$t1_ov25_odds
# T1_fixtures_co$t1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# T1_fixtures_co$t1_ov25 <- percent(T1_fixtures_co$t1_ov25, accuracy = 0.1)
#
# T1_fixtures_co$t1_un25 <- percent(T1_fixtures_co$t1_un25, accuracy = 0.1)
# T1_fixtures_co$t1_pscore <- paste(round(T1_fixtures_co$t1_xHCOC,digits = 0),round(T1_fixtures_co$t1_xACOC,digits = 0),sep = "-")
#
#
#












