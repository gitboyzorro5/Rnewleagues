library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx2048m")
library('xlsx')
library('scales')
library('lubridate')
library('pinnacle.data')
library('odds.converter')
library('sqldf')
##################################################################
#MLS
HomeTeam_mls <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls <- rep(mls_teams, length(mls_teams))
MLS_fixtures <- cbind(HomeTeam_mls,AwayTeam_mls)
MLS_fixtures <- as.data.frame(MLS_fixtures)
MLS_fixtures <- MLS_fixtures[!MLS_fixtures$HomeTeam_mls == MLS_fixtures$AwayTeam_mls,]
rownames(MLS_fixtures) <- NULL
MLS_fixtures$Div <- "MLS"
MLS_fixtures <- MLS_fixtures[,c(3,1,2)]

MLS_fixtures$avg_HG_mls <- mls_avg_HG

MLS_fixtures$mls_homeas <- rep(mls_home_as,each = length(mls_teams)-1)

mls_awayds_lookup <- cbind(mls_teams,mls_away_ds)

mls_awayds_lookup <- as.data.frame(mls_awayds_lookup)

colnames(mls_awayds_lookup) <- c("AwayTeam_mls","mls_awayds")


require('RH2')
MLS_fixtures$mls_awayds <- sqldf("SELECT mls_awayds_lookup.mls_awayds FROM mls_awayds_lookup INNER JOIN MLS_fixtures ON mls_awayds_lookup.AwayTeam_mls = MLS_fixtures.AwayTeam_mls")

MLS_fixtures$avg_AG_mls <- mls_avg_AG

mls_awayas_lookup <- cbind(mls_teams,mls_away_as)

mls_awayas_lookup <- as.data.frame(mls_awayas_lookup)

colnames(mls_awayas_lookup) <- c("AwayTeam_mls","mls_awayas")


MLS_fixtures$mls_awayas <- sqldf("SELECT mls_awayas_lookup.mls_awayas FROM mls_awayas_lookup INNER JOIN MLS_fixtures ON mls_awayas_lookup.AwayTeam_mls = MLS_fixtures.AwayTeam_mls")

MLS_fixtures$mls_homeds <- rep(mls_home_ds,each = length(mls_teams)-1)

MLS_fixtures$mls_awayds <- as.numeric(unlist(MLS_fixtures$mls_awayds))
#xGH
MLS_fixtures$mls_xGH <- MLS_fixtures$avg_HG_mls * MLS_fixtures$mls_homeas * MLS_fixtures$mls_awayds

#xGA

MLS_fixtures$mls_awayas <- as.numeric(unlist(MLS_fixtures$mls_awayas))

MLS_fixtures$mls_xGA <- MLS_fixtures$avg_AG_mls * MLS_fixtures$mls_awayas * MLS_fixtures$mls_homeds

MLS_fixtures$mls_0_0 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_0 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_1 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_1 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_0 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_2 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_2 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_1 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_2 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_3 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_0 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_1 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_2 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_3 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_3 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_3 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_4 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_0 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_1 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_2 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_3 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_4 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_4 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_4 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_4 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_5 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_0 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_1 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_2 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_3 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_4 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_5 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_5 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_5 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_5 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_5 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_6 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_0 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(0,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_1 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(1,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_2 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(2,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_3 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(3,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_4 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(4,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_6_5 <- round(stats::dpois(6,MLS_fixtures$mls_xGH) * stats::dpois(5,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_0_6 <- round(stats::dpois(0,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_1_6 <- round(stats::dpois(1,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_2_6 <- round(stats::dpois(2,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_3_6 <- round(stats::dpois(3,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_4_6 <- round(stats::dpois(4,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
MLS_fixtures$mls_5_6 <- round(stats::dpois(5,MLS_fixtures$mls_xGH) * stats::dpois(6,MLS_fixtures$mls_xGA), digits = 4)
#Home win
MLS_fixtures$mls_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5
)

MLS_fixtures$mls_H <- percent(MLS_fixtures$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures$mls_D <- (

  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
    MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6
)

MLS_fixtures$mls_D <- percent(MLS_fixtures$mls_D, accuracy = 0.1)

#Away

MLS_fixtures$mls_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6
)

MLS_fixtures$mls_A <- percent(MLS_fixtures$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures$mls_ov25 <- (
  MLS_fixtures$mls_2_1 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 + MLS_fixtures$mls_2_3 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 + MLS_fixtures$mls_0_4 +
    MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 + MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_0 +
    MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 + MLS_fixtures$mls_0_5 +
    MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 + MLS_fixtures$mls_5_5 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 +
    MLS_fixtures$mls_4_6 + MLS_fixtures$mls_5_6 + MLS_fixtures$mls_6_6
)
#un25
MLS_fixtures$mls_un25 <- (
  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_0 + MLS_fixtures$mls_0_1 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_0_2
)
#odds
MLS_fixtures$mls_ov25_odds <- round((1/MLS_fixtures$mls_ov25),digits = 2)
MLS_fixtures$mls_un25_odds <- round((1/MLS_fixtures$mls_un25),digits = 2)

MLS_fixtures$mls_ov25_odds
MLS_fixtures$mls_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
MLS_fixtures$mls_BTTSY <- (
  MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_3_1 + MLS_fixtures$mls_3_2 +
    MLS_fixtures$mls_2_2 + MLS_fixtures$mls_1_3 + MLS_fixtures$mls_2_3 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
    MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_3 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 +
    MLS_fixtures$mls_3_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 +
    MLS_fixtures$mls_5_4 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_6_6 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6
)
#BTTSN
MLS_fixtures$mls_BTTSN <- (
  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_0 + MLS_fixtures$mls_0_1 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_0_2 +
    MLS_fixtures$mls_3_0 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_5_0 +
    MLS_fixtures$mls_0_5 + MLS_fixtures$mls_6_0 + MLS_fixtures$mls_0_6
)

MLS_fixtures$mls_BTTSY_odds <- round((1/MLS_fixtures$mls_BTTSY),digits = 2)
MLS_fixtures$mls_BTTSN_odds <- round((1/MLS_fixtures$mls_BTTSN),digits = 2)

MLS_fixtures$mls_BTTSY <- percent(MLS_fixtures$mls_BTTSY, accuracy = 0.1)
MLS_fixtures$mls_BTTSN <- percent(MLS_fixtures$mls_BTTSN, accuracy = 0.1)
#odds
MLS_fixtures$mls_BTTSY_odds
MLS_fixtures$mls_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
MLS_fixtures$mls_AH_0_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 +MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6
)
#AH_0_A
MLS_fixtures$mls_AH_0_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 +MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6
)

#odds
MLS_fixtures$mls_AH_0_H_odds <- round((1/MLS_fixtures$mls_AH_0_H),digits = 2)
MLS_fixtures$mls_AH_0_A_odds <- round((1/MLS_fixtures$mls_AH_0_A),digits = 2)

MLS_fixtures$mls_AH_0_H_odds
MLS_fixtures$mls_AH_0_A_odds
#percentages
MLS_fixtures$mls_AH_0_H <- percent(MLS_fixtures$mls_AH_0_H, accuracy = 0.1)
MLS_fixtures$mls_AH_0_A <- percent(MLS_fixtures$mls_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
MLS_fixtures$mls_AH_n075_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 +MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5
)
#AH_n075_A
MLS_fixtures$mls_AH_n075_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 +MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6
)

#odds
MLS_fixtures$mls_AH_n075_H_odds <- round((1/MLS_fixtures$mls_AH_n075_H),digits = 2)
MLS_fixtures$mls_AH_n075_A_odds <- round((1/MLS_fixtures$mls_AH_n075_A),digits = 2)

MLS_fixtures$mls_AH_n075_H_odds
MLS_fixtures$mls_AH_n075_A_odds
#percentages
MLS_fixtures$mls_AH_n075_H <- percent(MLS_fixtures$mls_AH_n075_H, accuracy = 0.1)
MLS_fixtures$mls_AH_n075_A <- percent(MLS_fixtures$mls_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
MLS_fixtures$mls_AH_075_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 +MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6 + MLS_fixtures$mls_0_1 + MLS_fixtures$mls_1_2 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_3_4 + MLS_fixtures$mls_4_5 + MLS_fixtures$mls_5_6
)
#AH_075_A
MLS_fixtures$mls_AH_075_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 +MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6 + MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_3 + MLS_fixtures$mls_5_4 + MLS_fixtures$mls_6_5
)

#odds
MLS_fixtures$mls_AH_075_H_odds <- round((1/MLS_fixtures$mls_AH_075_H),digits = 2)
MLS_fixtures$mls_AH_075_A_odds <- round((1/MLS_fixtures$mls_AH_075_A),digits = 2)

MLS_fixtures$mls_AH_075_H_odds
MLS_fixtures$mls_AH_075_A_odds
#percentages
MLS_fixtures$mls_AH_075_H <- percent(MLS_fixtures$mls_AH_075_H, accuracy = 0.1)
MLS_fixtures$mls_AH_075_A <- percent(MLS_fixtures$mls_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
MLS_fixtures$mls_AH_n125_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 +MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5
)
#AH_n125_A
MLS_fixtures$mls_AH_n125_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 +MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6
)

#odds
MLS_fixtures$mls_AH_n125_H_odds <- round((1/MLS_fixtures$mls_AH_n125_H),digits = 2)
MLS_fixtures$mls_AH_n125_A_odds <- round((1/MLS_fixtures$mls_AH_n125_A),digits = 2)

MLS_fixtures$mls_AH_n125_H_odds
MLS_fixtures$mls_AH_n125_A_odds
#percentages
MLS_fixtures$mls_AH_n125_H <- percent(MLS_fixtures$mls_AH_n125_H, accuracy = 0.1)
MLS_fixtures$mls_AH_n125_A <- percent(MLS_fixtures$mls_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
MLS_fixtures$mls_AH_125_H <- (
  MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
    MLS_fixtures$mls_5_0 +MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
    MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
    MLS_fixtures$mls_6_5 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6 + MLS_fixtures$mls_0_1 + MLS_fixtures$mls_1_2 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_3_4 + MLS_fixtures$mls_4_5 + MLS_fixtures$mls_5_6
)
#AH_125_A
MLS_fixtures$mls_AH_125_A <- (
  MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
    MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
    MLS_fixtures$mls_0_5 +MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
    MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
    MLS_fixtures$mls_5_6 + MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 +
    MLS_fixtures$mls_4_4 + MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6 + MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_1 +
    MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_3 + MLS_fixtures$mls_5_4 + MLS_fixtures$mls_6_5
)

#odds
MLS_fixtures$mls_AH_125_H_odds <- round((1/MLS_fixtures$mls_AH_125_H),digits = 2)
MLS_fixtures$mls_AH_125_A_odds <- round((1/MLS_fixtures$mls_AH_125_A),digits = 2)

MLS_fixtures$mls_AH_125_H_odds
MLS_fixtures$mls_AH_125_A_odds
#percentages
MLS_fixtures$mls_AH_125_H <- percent(MLS_fixtures$mls_AH_125_H, accuracy = 0.1)
MLS_fixtures$mls_AH_125_A <- percent(MLS_fixtures$mls_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures$mls_ov25 <- percent(MLS_fixtures$mls_ov25, accuracy = 0.1)

MLS_fixtures$mls_un25 <- percent(MLS_fixtures$mls_un25, accuracy = 0.1)
MLS_fixtures$mls_pscore <- paste(round(MLS_fixtures$mls_xGH,digits = 0),round(MLS_fixtures$mls_xGA,digits = 0),sep = "-")
#write out
write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
#################################################################################################################
#ARG
HomeTeam_arg <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg <- rep(arg_teams, length(arg_teams))
ARG_fixtures <- cbind(HomeTeam_arg,AwayTeam_arg)
ARG_fixtures <- as.data.frame(ARG_fixtures)
ARG_fixtures <- ARG_fixtures[!ARG_fixtures$HomeTeam_arg == ARG_fixtures$AwayTeam_arg,]
rownames(ARG_fixtures) <- NULL
ARG_fixtures$Div <- "ARG"
ARG_fixtures <- ARG_fixtures[,c(3,1,2)]

ARG_fixtures$avg_HG_arg <- arg_avg_HG

ARG_fixtures$arg_homeas <- rep(arg_home_as,each = length(arg_teams)-1)

arg_awayds_lookup <- cbind(arg_teams,arg_away_ds)

arg_awayds_lookup <- as.data.frame(arg_awayds_lookup)

colnames(arg_awayds_lookup) <- c("AwayTeam_arg","arg_awayds")


require('RH2')
ARG_fixtures$arg_awayds <- sqldf("SELECT arg_awayds_lookup.arg_awayds FROM arg_awayds_lookup INNER JOIN ARG_fixtures ON arg_awayds_lookup.AwayTeam_arg = ARG_fixtures.AwayTeam_arg")

ARG_fixtures$avg_AG_arg <- arg_avg_AG

arg_awayas_lookup <- cbind(arg_teams,arg_away_as)

arg_awayas_lookup <- as.data.frame(arg_awayas_lookup)

colnames(arg_awayas_lookup) <- c("AwayTeam_arg","arg_awayas")


ARG_fixtures$arg_awayas <- sqldf("SELECT arg_awayas_lookup.arg_awayas FROM arg_awayas_lookup INNER JOIN ARG_fixtures ON arg_awayas_lookup.AwayTeam_arg = ARG_fixtures.AwayTeam_arg")

ARG_fixtures$arg_homeds <- rep(arg_home_ds,each = length(arg_teams)-1)

ARG_fixtures$arg_awayds <- as.numeric(unlist(ARG_fixtures$arg_awayds))
#xGH
ARG_fixtures$arg_xGH <- ARG_fixtures$avg_HG_arg * ARG_fixtures$arg_homeas * ARG_fixtures$arg_awayds

#xGA

ARG_fixtures$arg_awayas <- as.numeric(unlist(ARG_fixtures$arg_awayas))

ARG_fixtures$arg_xGA <- ARG_fixtures$avg_AG_arg * ARG_fixtures$arg_awayas * ARG_fixtures$arg_homeds

ARG_fixtures$arg_0_0 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_0 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_1 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_1 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_0 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_2 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_2 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_1 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_2 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_3 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_0 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_1 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_2 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_3 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_3 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_3 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_4 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_0 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_1 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_2 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_3 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_4 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_4 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_4 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_4 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_5 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_0 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_1 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_2 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_3 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_4 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_5 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_5 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_5 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_5 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_5 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_6 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_0 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(0,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_1 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(1,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_2 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(2,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_3 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(3,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_4 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(4,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_6_5 <- round(stats::dpois(6,ARG_fixtures$arg_xGH) * stats::dpois(5,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_0_6 <- round(stats::dpois(0,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_1_6 <- round(stats::dpois(1,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_2_6 <- round(stats::dpois(2,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_3_6 <- round(stats::dpois(3,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_4_6 <- round(stats::dpois(4,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
ARG_fixtures$arg_5_6 <- round(stats::dpois(5,ARG_fixtures$arg_xGH) * stats::dpois(6,ARG_fixtures$arg_xGA), digits = 4)
#Home win
ARG_fixtures$arg_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 + ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5
)

ARG_fixtures$arg_H <- percent(ARG_fixtures$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures$arg_D <- (

  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 + ARG_fixtures$arg_4_4 +
    ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6
)

ARG_fixtures$arg_D <- percent(ARG_fixtures$arg_D, accuracy = 0.1)

#Away

ARG_fixtures$arg_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 + ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6
)

ARG_fixtures$arg_A <- percent(ARG_fixtures$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures$arg_ov25 <- (
  ARG_fixtures$arg_2_1 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 + ARG_fixtures$arg_2_3 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 + ARG_fixtures$arg_0_4 +
    ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 + ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_0 +
    ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 + ARG_fixtures$arg_0_5 +
    ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 + ARG_fixtures$arg_5_5 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 +
    ARG_fixtures$arg_4_6 + ARG_fixtures$arg_5_6 + ARG_fixtures$arg_6_6
)
#un25
ARG_fixtures$arg_un25 <- (
  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_0 + ARG_fixtures$arg_0_1 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_0_2
)
#odds
ARG_fixtures$arg_ov25_odds <- round((1/ARG_fixtures$arg_ov25),digits = 2)
ARG_fixtures$arg_un25_odds <- round((1/ARG_fixtures$arg_un25),digits = 2)

ARG_fixtures$arg_ov25_odds
ARG_fixtures$arg_un25_odds
###############################################################################
###BTTS########################################################################
#BTTSY
ARG_fixtures$arg_BTTSY <- (
  ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_3_1 + ARG_fixtures$arg_3_2 +
    ARG_fixtures$arg_2_2 + ARG_fixtures$arg_1_3 + ARG_fixtures$arg_2_3 + ARG_fixtures$arg_3_3 + ARG_fixtures$arg_4_4 +
    ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_3 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 +
    ARG_fixtures$arg_3_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 +
    ARG_fixtures$arg_5_4 + ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_6_6 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6
)
#BTTSN
ARG_fixtures$arg_BTTSN <- (
  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_0 + ARG_fixtures$arg_0_1 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_0_2 +
    ARG_fixtures$arg_3_0 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_5_0 +
    ARG_fixtures$arg_0_5 + ARG_fixtures$arg_6_0 + ARG_fixtures$arg_0_6
)

ARG_fixtures$arg_BTTSY_odds <- round((1/ARG_fixtures$arg_BTTSY),digits = 2)
ARG_fixtures$arg_BTTSN_odds <- round((1/ARG_fixtures$arg_BTTSN),digits = 2)

ARG_fixtures$arg_BTTSY <- percent(ARG_fixtures$arg_BTTSY, accuracy = 0.1)
ARG_fixtures$arg_BTTSN <- percent(ARG_fixtures$arg_BTTSN, accuracy = 0.1)
#odds
ARG_fixtures$arg_BTTSY_odds
ARG_fixtures$arg_BTTSN_odds
########Asian Handicaps##########################################################################################################
##########################################################################
#AH(0)
#AH_0_H
ARG_fixtures$arg_AH_0_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 +ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6
)
#AH_0_A
ARG_fixtures$arg_AH_0_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 +ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6
)

#odds
ARG_fixtures$arg_AH_0_H_odds <- round((1/ARG_fixtures$arg_AH_0_H),digits = 2)
ARG_fixtures$arg_AH_0_A_odds <- round((1/ARG_fixtures$arg_AH_0_A),digits = 2)

ARG_fixtures$arg_AH_0_H_odds
ARG_fixtures$arg_AH_0_A_odds
#percentages
ARG_fixtures$arg_AH_0_H <- percent(ARG_fixtures$arg_AH_0_H, accuracy = 0.1)
ARG_fixtures$arg_AH_0_A <- percent(ARG_fixtures$arg_AH_0_A, accuracy = 0.1)
####################################################################################
##########################################################################
#AH(-0.75)
#AH_n075_H
ARG_fixtures$arg_AH_n075_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 +ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5
)
#AH_n075_A
ARG_fixtures$arg_AH_n075_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 +ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6
)

#odds
ARG_fixtures$arg_AH_n075_H_odds <- round((1/ARG_fixtures$arg_AH_n075_H),digits = 2)
ARG_fixtures$arg_AH_n075_A_odds <- round((1/ARG_fixtures$arg_AH_n075_A),digits = 2)

ARG_fixtures$arg_AH_n075_H_odds
ARG_fixtures$arg_AH_n075_A_odds
#percentages
ARG_fixtures$arg_AH_n075_H <- percent(ARG_fixtures$arg_AH_n075_H, accuracy = 0.1)
ARG_fixtures$arg_AH_n075_A <- percent(ARG_fixtures$arg_AH_n075_A, accuracy = 0.1)
##########################################################################
#AH(0.75)
#AH_075_H
ARG_fixtures$arg_AH_075_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 +ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6 + ARG_fixtures$arg_0_1 + ARG_fixtures$arg_1_2 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_3_4 + ARG_fixtures$arg_4_5 + ARG_fixtures$arg_5_6
)
#AH_075_A
ARG_fixtures$arg_AH_075_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 +ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6 + ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_3 + ARG_fixtures$arg_5_4 + ARG_fixtures$arg_6_5
)

#odds
ARG_fixtures$arg_AH_075_H_odds <- round((1/ARG_fixtures$arg_AH_075_H),digits = 2)
ARG_fixtures$arg_AH_075_A_odds <- round((1/ARG_fixtures$arg_AH_075_A),digits = 2)

ARG_fixtures$arg_AH_075_H_odds
ARG_fixtures$arg_AH_075_A_odds
#percentages
ARG_fixtures$arg_AH_075_H <- percent(ARG_fixtures$arg_AH_075_H, accuracy = 0.1)
ARG_fixtures$arg_AH_075_A <- percent(ARG_fixtures$arg_AH_075_A, accuracy = 0.1)
####################################################################################
#AH(-1.25)
#AH_n125_H
ARG_fixtures$arg_AH_n125_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 +ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5
)
#AH_n125_A
ARG_fixtures$arg_AH_n125_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 +ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6
)

#odds
ARG_fixtures$arg_AH_n125_H_odds <- round((1/ARG_fixtures$arg_AH_n125_H),digits = 2)
ARG_fixtures$arg_AH_n125_A_odds <- round((1/ARG_fixtures$arg_AH_n125_A),digits = 2)

ARG_fixtures$arg_AH_n125_H_odds
ARG_fixtures$arg_AH_n125_A_odds
#percentages
ARG_fixtures$arg_AH_n125_H <- percent(ARG_fixtures$arg_AH_n125_H, accuracy = 0.1)
ARG_fixtures$arg_AH_n125_A <- percent(ARG_fixtures$arg_AH_n125_A, accuracy = 0.1)

####################################################################################
##########################################################################
#AH(1.25)
#AH_125_H
ARG_fixtures$arg_AH_125_H <- (
  ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
    ARG_fixtures$arg_5_0 +ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
    ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
    ARG_fixtures$arg_6_5 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6 + ARG_fixtures$arg_0_1 + ARG_fixtures$arg_1_2 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_3_4 + ARG_fixtures$arg_4_5 + ARG_fixtures$arg_5_6
)
#AH_125_A
ARG_fixtures$arg_AH_125_A <- (
  ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
    ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
    ARG_fixtures$arg_0_5 +ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
    ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
    ARG_fixtures$arg_5_6 + ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 +
    ARG_fixtures$arg_4_4 + ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6 + ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_1 +
    ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_3 + ARG_fixtures$arg_5_4 + ARG_fixtures$arg_6_5
)

#odds
ARG_fixtures$arg_AH_125_H_odds <- round((1/ARG_fixtures$arg_AH_125_H),digits = 2)
ARG_fixtures$arg_AH_125_A_odds <- round((1/ARG_fixtures$arg_AH_125_A),digits = 2)

ARG_fixtures$arg_AH_125_H_odds
ARG_fixtures$arg_AH_125_A_odds
#percentages
ARG_fixtures$arg_AH_125_H <- percent(ARG_fixtures$arg_AH_125_H, accuracy = 0.1)
ARG_fixtures$arg_AH_125_A <- percent(ARG_fixtures$arg_AH_125_A, accuracy = 0.1)
####################################################################################
########Asian Handicaps######################################################################################################
#percentages
ARG_fixtures$arg_ov25 <- percent(ARG_fixtures$arg_ov25, accuracy = 0.1)

ARG_fixtures$arg_un25 <- percent(ARG_fixtures$arg_un25, accuracy = 0.1)
ARG_fixtures$arg_pscore <- paste(round(ARG_fixtures$arg_xGH,digits = 0),round(ARG_fixtures$arg_xGA,digits = 0),sep = "-")
#write out
write.xlsx(ARG_fixtures,'Divisions/ARG.xlsx',sheetName = "ARG", append = TRUE)
####################################################################################################################
#D2
# HomeTeam_d2 <- rep(d2_teams, each = length(d2_teams))
# AwayTeam_d2 <- rep(d2_teams, length(d2_teams))
# D2_fixtures <- cbind(HomeTeam_d2,AwayTeam_d2)
# D2_fixtures <- as.data.frame(D2_fixtures)
# D2_fixtures <- D2_fixtures[!D2_fixtures$HomeTeam_d2 == D2_fixtures$AwayTeam_d2,]
# rownames(D2_fixtures) <- NULL
# D2_fixtures$Div <- "D2"
# D2_fixtures <- D2_fixtures[,c(3,1,2)]
#
# D2_fixtures$avg_HG_d2 <- d2_avg_HG
#
# D2_fixtures$d2_homeas <- rep(d2_home_as,each = length(d2_teams)-1)
#
# d2_awayds_lookup <- cbind(d2_teams,d2_away_ds)
#
# d2_awayds_lookup <- as.data.frame(d2_awayds_lookup)
#
# colnames(d2_awayds_lookup) <- c("AwayTeam_d2","d2_awayds")
#
#
# require('RH2')
# D2_fixtures$d2_awayds <- sqldf("SELECT d2_awayds_lookup.d2_awayds FROM d2_awayds_lookup INNER JOIN D2_fixtures ON d2_awayds_lookup.AwayTeam_d2 = D2_fixtures.AwayTeam_d2")
#
# D2_fixtures$avg_AG_d2 <- d2_avg_AG
#
# d2_awayas_lookup <- cbind(d2_teams,d2_away_as)
#
# d2_awayas_lookup <- as.data.frame(d2_awayas_lookup)
#
# colnames(d2_awayas_lookup) <- c("AwayTeam_d2","d2_awayas")
#
#
# D2_fixtures$d2_awayas <- sqldf("SELECT d2_awayas_lookup.d2_awayas FROM d2_awayas_lookup INNER JOIN D2_fixtures ON d2_awayas_lookup.AwayTeam_d2 = D2_fixtures.AwayTeam_d2")
#
# D2_fixtures$d2_homeds <- rep(d2_home_ds,each = length(d2_teams)-1)
#
# D2_fixtures$d2_awayds <- as.numeric(unlist(D2_fixtures$d2_awayds))
# #xGH
# D2_fixtures$d2_xGH <- D2_fixtures$avg_HG_d2 * D2_fixtures$d2_homeas * D2_fixtures$d2_awayds
#
# #xGA
#
# D2_fixtures$d2_awayas <- as.numeric(unlist(D2_fixtures$d2_awayas))
#
# D2_fixtures$d2_xGA <- D2_fixtures$avg_AG_d2 * D2_fixtures$d2_awayas * D2_fixtures$d2_homeds
#
# D2_fixtures$d2_0_0 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_0 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_1 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_1 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_0 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_2 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_2 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_1 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_2 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_3 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_0 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_1 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_2 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_3 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_3 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_3 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_4 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_0 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_1 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_2 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_3 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_4 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_4 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_4 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_4 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_5 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_0 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_1 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_2 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_3 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_4 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_5 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_5 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_5 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_5 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_5 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_6 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_0 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(0,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_1 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(1,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_2 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(2,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_3 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(3,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_4 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(4,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_6_5 <- round(stats::dpois(6,D2_fixtures$d2_xGH) * stats::dpois(5,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_0_6 <- round(stats::dpois(0,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_1_6 <- round(stats::dpois(1,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_2_6 <- round(stats::dpois(2,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_3_6 <- round(stats::dpois(3,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_4_6 <- round(stats::dpois(4,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# D2_fixtures$d2_5_6 <- round(stats::dpois(5,D2_fixtures$d2_xGH) * stats::dpois(6,D2_fixtures$d2_xGA), digits = 4)
# #Home win
# D2_fixtures$d2_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 + D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5
# )
#
# D2_fixtures$d2_H <- percent(D2_fixtures$d2_H, accuracy = 0.1)
#
# #Draw
# D2_fixtures$d2_D <- (
#
#   D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 + D2_fixtures$d2_4_4 +
#     D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6
# )
#
# D2_fixtures$d2_D <- percent(D2_fixtures$d2_D, accuracy = 0.1)
#
# #Away
#
# D2_fixtures$d2_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 + D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6
# )
#
# D2_fixtures$d2_A <- percent(D2_fixtures$d2_A, accuracy = 0.1)
#
# #ov25
# D2_fixtures$d2_ov25 <- (
#   D2_fixtures$d2_2_1 + D2_fixtures$d2_1_2 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 + D2_fixtures$d2_2_3 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 + D2_fixtures$d2_0_4 +
#     D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 + D2_fixtures$d2_4_4 + D2_fixtures$d2_5_0 +
#     D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 + D2_fixtures$d2_0_5 +
#     D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 + D2_fixtures$d2_5_5 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5 + D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 +
#     D2_fixtures$d2_4_6 + D2_fixtures$d2_5_6 + D2_fixtures$d2_6_6
# )
# #un25
# D2_fixtures$d2_un25 <- (
#   D2_fixtures$d2_0_0 + D2_fixtures$d2_1_0 + D2_fixtures$d2_0_1 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_0 + D2_fixtures$d2_0_2
# )
# #odds
# D2_fixtures$d2_ov25_odds <- round((1/D2_fixtures$d2_ov25),digits = 2)
# D2_fixtures$d2_un25_odds <- round((1/D2_fixtures$d2_un25),digits = 2)
#
# D2_fixtures$d2_ov25_odds
# D2_fixtures$d2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# D2_fixtures$d2_BTTSY <- (
#   D2_fixtures$d2_1_1 + D2_fixtures$d2_2_1 + D2_fixtures$d2_1_2 + D2_fixtures$d2_3_1 + D2_fixtures$d2_3_2 +
#     D2_fixtures$d2_2_2 + D2_fixtures$d2_1_3 + D2_fixtures$d2_2_3 + D2_fixtures$d2_3_3 + D2_fixtures$d2_4_4 +
#     D2_fixtures$d2_4_1 + D2_fixtures$d2_4_3 + D2_fixtures$d2_4_2 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 +
#     D2_fixtures$d2_3_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 +
#     D2_fixtures$d2_5_4 + D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_6_6 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6
# )
# #BTTSN
# D2_fixtures$d2_BTTSN <- (
#   D2_fixtures$d2_0_0 + D2_fixtures$d2_1_0 + D2_fixtures$d2_0_1 + D2_fixtures$d2_2_0 + D2_fixtures$d2_0_2 +
#     D2_fixtures$d2_3_0 + D2_fixtures$d2_0_3 + D2_fixtures$d2_4_0 + D2_fixtures$d2_0_4 + D2_fixtures$d2_5_0 +
#     D2_fixtures$d2_0_5 + D2_fixtures$d2_6_0 + D2_fixtures$d2_0_6
# )
#
# D2_fixtures$d2_BTTSY_odds <- round((1/D2_fixtures$d2_BTTSY),digits = 2)
# D2_fixtures$d2_BTTSN_odds <- round((1/D2_fixtures$d2_BTTSN),digits = 2)
#
# D2_fixtures$d2_BTTSY <- percent(D2_fixtures$d2_BTTSY, accuracy = 0.1)
# D2_fixtures$d2_BTTSN <- percent(D2_fixtures$d2_BTTSN, accuracy = 0.1)
# #odds
# D2_fixtures$d2_BTTSY_odds
# D2_fixtures$d2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# D2_fixtures$d2_AH_0_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 +D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6
# )
# #AH_0_A
# D2_fixtures$d2_AH_0_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 +D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6
# )
#
# #odds
# D2_fixtures$d2_AH_0_H_odds <- round((1/D2_fixtures$d2_AH_0_H),digits = 2)
# D2_fixtures$d2_AH_0_A_odds <- round((1/D2_fixtures$d2_AH_0_A),digits = 2)
#
# D2_fixtures$d2_AH_0_H_odds
# D2_fixtures$d2_AH_0_A_odds
# #percentages
# D2_fixtures$d2_AH_0_H <- percent(D2_fixtures$d2_AH_0_H, accuracy = 0.1)
# D2_fixtures$d2_AH_0_A <- percent(D2_fixtures$d2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# D2_fixtures$d2_AH_n075_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 +D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5
# )
# #AH_n075_A
# D2_fixtures$d2_AH_n075_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 +D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6
# )
#
# #odds
# D2_fixtures$d2_AH_n075_H_odds <- round((1/D2_fixtures$d2_AH_n075_H),digits = 2)
# D2_fixtures$d2_AH_n075_A_odds <- round((1/D2_fixtures$d2_AH_n075_A),digits = 2)
#
# D2_fixtures$d2_AH_n075_H_odds
# D2_fixtures$d2_AH_n075_A_odds
# #percentages
# D2_fixtures$d2_AH_n075_H <- percent(D2_fixtures$d2_AH_n075_H, accuracy = 0.1)
# D2_fixtures$d2_AH_n075_A <- percent(D2_fixtures$d2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# D2_fixtures$d2_AH_075_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 +D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6 + D2_fixtures$d2_0_1 + D2_fixtures$d2_1_2 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_3_4 + D2_fixtures$d2_4_5 + D2_fixtures$d2_5_6
# )
# #AH_075_A
# D2_fixtures$d2_AH_075_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 +D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6 + D2_fixtures$d2_1_0 + D2_fixtures$d2_2_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_3 + D2_fixtures$d2_5_4 + D2_fixtures$d2_6_5
# )
#
# #odds
# D2_fixtures$d2_AH_075_H_odds <- round((1/D2_fixtures$d2_AH_075_H),digits = 2)
# D2_fixtures$d2_AH_075_A_odds <- round((1/D2_fixtures$d2_AH_075_A),digits = 2)
#
# D2_fixtures$d2_AH_075_H_odds
# D2_fixtures$d2_AH_075_A_odds
# #percentages
# D2_fixtures$d2_AH_075_H <- percent(D2_fixtures$d2_AH_075_H, accuracy = 0.1)
# D2_fixtures$d2_AH_075_A <- percent(D2_fixtures$d2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# D2_fixtures$d2_AH_n125_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 +D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5
# )
# #AH_n125_A
# D2_fixtures$d2_AH_n125_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 +D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6
# )
#
# #odds
# D2_fixtures$d2_AH_n125_H_odds <- round((1/D2_fixtures$d2_AH_n125_H),digits = 2)
# D2_fixtures$d2_AH_n125_A_odds <- round((1/D2_fixtures$d2_AH_n125_A),digits = 2)
#
# D2_fixtures$d2_AH_n125_H_odds
# D2_fixtures$d2_AH_n125_A_odds
# #percentages
# D2_fixtures$d2_AH_n125_H <- percent(D2_fixtures$d2_AH_n125_H, accuracy = 0.1)
# D2_fixtures$d2_AH_n125_A <- percent(D2_fixtures$d2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# D2_fixtures$d2_AH_125_H <- (
#   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#     D2_fixtures$d2_5_0 +D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#     D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#     D2_fixtures$d2_6_5 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6 + D2_fixtures$d2_0_1 + D2_fixtures$d2_1_2 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_3_4 + D2_fixtures$d2_4_5 + D2_fixtures$d2_5_6
# )
# #AH_125_A
# D2_fixtures$d2_AH_125_A <- (
#   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#     D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#     D2_fixtures$d2_0_5 +D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#     D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#     D2_fixtures$d2_5_6 + D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 +
#     D2_fixtures$d2_4_4 + D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6 + D2_fixtures$d2_1_0 + D2_fixtures$d2_2_1 +
#     D2_fixtures$d2_3_2 + D2_fixtures$d2_4_3 + D2_fixtures$d2_5_4 + D2_fixtures$d2_6_5
# )
#
# #odds
# D2_fixtures$d2_AH_125_H_odds <- round((1/D2_fixtures$d2_AH_125_H),digits = 2)
# D2_fixtures$d2_AH_125_A_odds <- round((1/D2_fixtures$d2_AH_125_A),digits = 2)
#
# D2_fixtures$d2_AH_125_H_odds
# D2_fixtures$d2_AH_125_A_odds
# #percentages
# D2_fixtures$d2_AH_125_H <- percent(D2_fixtures$d2_AH_125_H, accuracy = 0.1)
# D2_fixtures$d2_AH_125_A <- percent(D2_fixtures$d2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# D2_fixtures$d2_ov25 <- percent(D2_fixtures$d2_ov25, accuracy = 0.1)
#
# D2_fixtures$d2_un25 <- percent(D2_fixtures$d2_un25, accuracy = 0.1)
# D2_fixtures$d2_pscore <- paste(round(D2_fixtures$d2_xGH,digits = 0),round(D2_fixtures$d2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(D2_fixtures,'Divisions/D2.xlsx',sheetName = "D2", append = TRUE)
# #######################################################################################################################
# #E0
# HomeTeam_e0 <- rep(e0_teams, each = length(e0_teams))
# AwayTeam_e0 <- rep(e0_teams, length(e0_teams))
# E0_fixtures <- cbind(HomeTeam_e0,AwayTeam_e0)
# E0_fixtures <- as.data.frame(E0_fixtures)
# E0_fixtures <- E0_fixtures[!E0_fixtures$HomeTeam_e0 == E0_fixtures$AwayTeam_e0,]
# rownames(E0_fixtures) <- NULL
# E0_fixtures$Div <- "E0"
# E0_fixtures <- E0_fixtures[,c(3,1,2)]
#
# E0_fixtures$avg_HG_e0 <- e0_avg_HG
#
# E0_fixtures$e0_homeas <- rep(e0_home_as,each = length(e0_teams)-1)
#
# e0_awayds_lookup <- cbind(e0_teams,e0_away_ds)
#
# e0_awayds_lookup <- as.data.frame(e0_awayds_lookup)
#
# colnames(e0_awayds_lookup) <- c("AwayTeam_e0","e0_awayds")
#
#
# require('RH2')
# E0_fixtures$e0_awayds <- sqldf("SELECT e0_awayds_lookup.e0_awayds FROM e0_awayds_lookup INNER JOIN E0_fixtures ON e0_awayds_lookup.AwayTeam_e0 = E0_fixtures.AwayTeam_e0")
#
# E0_fixtures$avg_AG_e0 <- e0_avg_AG
#
# e0_awayas_lookup <- cbind(e0_teams,e0_away_as)
#
# e0_awayas_lookup <- as.data.frame(e0_awayas_lookup)
#
# colnames(e0_awayas_lookup) <- c("AwayTeam_e0","e0_awayas")
#
#
# E0_fixtures$e0_awayas <- sqldf("SELECT e0_awayas_lookup.e0_awayas FROM e0_awayas_lookup INNER JOIN E0_fixtures ON e0_awayas_lookup.AwayTeam_e0 = E0_fixtures.AwayTeam_e0")
#
# E0_fixtures$e0_homeds <- rep(e0_home_ds,each = length(e0_teams)-1)
#
# E0_fixtures$e0_awayds <- as.numeric(unlist(E0_fixtures$e0_awayds))
# #xGH
# E0_fixtures$e0_xGH <- E0_fixtures$avg_HG_e0 * E0_fixtures$e0_homeas * E0_fixtures$e0_awayds
#
# #xGA
#
# E0_fixtures$e0_awayas <- as.numeric(unlist(E0_fixtures$e0_awayas))
#
# E0_fixtures$e0_xGA <- E0_fixtures$avg_AG_e0 * E0_fixtures$e0_awayas * E0_fixtures$e0_homeds
#
# E0_fixtures$e0_0_0 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_0 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_1 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_1 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_0 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_2 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_2 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_1 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_2 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_3 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_0 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_1 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_2 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_3 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_3 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_3 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_4 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_0 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_1 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_2 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_3 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_4 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_4 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_4 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_4 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_5 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_0 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_1 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_2 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_3 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_4 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_5 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_5 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_5 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_5 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_5 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_6 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_0 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(0,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_1 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(1,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_2 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(2,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_3 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(3,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_4 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(4,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_6_5 <- round(stats::dpois(6,E0_fixtures$e0_xGH) * stats::dpois(5,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_0_6 <- round(stats::dpois(0,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_1_6 <- round(stats::dpois(1,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_2_6 <- round(stats::dpois(2,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_3_6 <- round(stats::dpois(3,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_4_6 <- round(stats::dpois(4,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# E0_fixtures$e0_5_6 <- round(stats::dpois(5,E0_fixtures$e0_xGH) * stats::dpois(6,E0_fixtures$e0_xGA), digits = 4)
# #Home win
# E0_fixtures$e0_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 + E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5
# )
#
# E0_fixtures$e0_H <- percent(E0_fixtures$e0_H, accuracy = 0.1)
#
# #Draw
# E0_fixtures$e0_D <- (
#
#   E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 + E0_fixtures$e0_4_4 +
#     E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6
# )
#
# E0_fixtures$e0_D <- percent(E0_fixtures$e0_D, accuracy = 0.1)
#
# #Away
#
# E0_fixtures$e0_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 + E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6
# )
#
# E0_fixtures$e0_A <- percent(E0_fixtures$e0_A, accuracy = 0.1)
#
# #ov25
# E0_fixtures$e0_ov25 <- (
#   E0_fixtures$e0_2_1 + E0_fixtures$e0_1_2 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 + E0_fixtures$e0_2_3 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 + E0_fixtures$e0_0_4 +
#     E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 + E0_fixtures$e0_4_4 + E0_fixtures$e0_5_0 +
#     E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 + E0_fixtures$e0_0_5 +
#     E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 + E0_fixtures$e0_5_5 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5 + E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 +
#     E0_fixtures$e0_4_6 + E0_fixtures$e0_5_6 + E0_fixtures$e0_6_6
# )
# #un25
# E0_fixtures$e0_un25 <- (
#   E0_fixtures$e0_0_0 + E0_fixtures$e0_1_0 + E0_fixtures$e0_0_1 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_0 + E0_fixtures$e0_0_2
# )
# #odds
# E0_fixtures$e0_ov25_odds <- round((1/E0_fixtures$e0_ov25),digits = 2)
# E0_fixtures$e0_un25_odds <- round((1/E0_fixtures$e0_un25),digits = 2)
#
# E0_fixtures$e0_ov25_odds
# E0_fixtures$e0_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# E0_fixtures$e0_BTTSY <- (
#   E0_fixtures$e0_1_1 + E0_fixtures$e0_2_1 + E0_fixtures$e0_1_2 + E0_fixtures$e0_3_1 + E0_fixtures$e0_3_2 +
#     E0_fixtures$e0_2_2 + E0_fixtures$e0_1_3 + E0_fixtures$e0_2_3 + E0_fixtures$e0_3_3 + E0_fixtures$e0_4_4 +
#     E0_fixtures$e0_4_1 + E0_fixtures$e0_4_3 + E0_fixtures$e0_4_2 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 +
#     E0_fixtures$e0_3_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 +
#     E0_fixtures$e0_5_4 + E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_6_6 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6
# )
# #BTTSN
# E0_fixtures$e0_BTTSN <- (
#   E0_fixtures$e0_0_0 + E0_fixtures$e0_1_0 + E0_fixtures$e0_0_1 + E0_fixtures$e0_2_0 + E0_fixtures$e0_0_2 +
#     E0_fixtures$e0_3_0 + E0_fixtures$e0_0_3 + E0_fixtures$e0_4_0 + E0_fixtures$e0_0_4 + E0_fixtures$e0_5_0 +
#     E0_fixtures$e0_0_5 + E0_fixtures$e0_6_0 + E0_fixtures$e0_0_6
# )
#
# E0_fixtures$e0_BTTSY_odds <- round((1/E0_fixtures$e0_BTTSY),digits = 2)
# E0_fixtures$e0_BTTSN_odds <- round((1/E0_fixtures$e0_BTTSN),digits = 2)
#
# E0_fixtures$e0_BTTSY <- percent(E0_fixtures$e0_BTTSY, accuracy = 0.1)
# E0_fixtures$e0_BTTSN <- percent(E0_fixtures$e0_BTTSN, accuracy = 0.1)
# #odds
# E0_fixtures$e0_BTTSY_odds
# E0_fixtures$e0_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# E0_fixtures$e0_AH_0_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 +E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6
# )
# #AH_0_A
# E0_fixtures$e0_AH_0_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 +E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6
# )
#
# #odds
# E0_fixtures$e0_AH_0_H_odds <- round((1/E0_fixtures$e0_AH_0_H),digits = 2)
# E0_fixtures$e0_AH_0_A_odds <- round((1/E0_fixtures$e0_AH_0_A),digits = 2)
#
# E0_fixtures$e0_AH_0_H_odds
# E0_fixtures$e0_AH_0_A_odds
# #percentages
# E0_fixtures$e0_AH_0_H <- percent(E0_fixtures$e0_AH_0_H, accuracy = 0.1)
# E0_fixtures$e0_AH_0_A <- percent(E0_fixtures$e0_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# E0_fixtures$e0_AH_n075_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 +E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5
# )
# #AH_n075_A
# E0_fixtures$e0_AH_n075_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 +E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6
# )
#
# #odds
# E0_fixtures$e0_AH_n075_H_odds <- round((1/E0_fixtures$e0_AH_n075_H),digits = 2)
# E0_fixtures$e0_AH_n075_A_odds <- round((1/E0_fixtures$e0_AH_n075_A),digits = 2)
#
# E0_fixtures$e0_AH_n075_H_odds
# E0_fixtures$e0_AH_n075_A_odds
# #percentages
# E0_fixtures$e0_AH_n075_H <- percent(E0_fixtures$e0_AH_n075_H, accuracy = 0.1)
# E0_fixtures$e0_AH_n075_A <- percent(E0_fixtures$e0_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# E0_fixtures$e0_AH_075_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 +E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6 + E0_fixtures$e0_0_1 + E0_fixtures$e0_1_2 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_3_4 + E0_fixtures$e0_4_5 + E0_fixtures$e0_5_6
# )
# #AH_075_A
# E0_fixtures$e0_AH_075_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 +E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6 + E0_fixtures$e0_1_0 + E0_fixtures$e0_2_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_3 + E0_fixtures$e0_5_4 + E0_fixtures$e0_6_5
# )
#
# #odds
# E0_fixtures$e0_AH_075_H_odds <- round((1/E0_fixtures$e0_AH_075_H),digits = 2)
# E0_fixtures$e0_AH_075_A_odds <- round((1/E0_fixtures$e0_AH_075_A),digits = 2)
#
# E0_fixtures$e0_AH_075_H_odds
# E0_fixtures$e0_AH_075_A_odds
# #percentages
# E0_fixtures$e0_AH_075_H <- percent(E0_fixtures$e0_AH_075_H, accuracy = 0.1)
# E0_fixtures$e0_AH_075_A <- percent(E0_fixtures$e0_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# E0_fixtures$e0_AH_n125_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 +E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5
# )
# #AH_n125_A
# E0_fixtures$e0_AH_n125_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 +E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6
# )
#
# #odds
# E0_fixtures$e0_AH_n125_H_odds <- round((1/E0_fixtures$e0_AH_n125_H),digits = 2)
# E0_fixtures$e0_AH_n125_A_odds <- round((1/E0_fixtures$e0_AH_n125_A),digits = 2)
#
# E0_fixtures$e0_AH_n125_H_odds
# E0_fixtures$e0_AH_n125_A_odds
# #percentages
# E0_fixtures$e0_AH_n125_H <- percent(E0_fixtures$e0_AH_n125_H, accuracy = 0.1)
# E0_fixtures$e0_AH_n125_A <- percent(E0_fixtures$e0_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# E0_fixtures$e0_AH_125_H <- (
#   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#     E0_fixtures$e0_5_0 +E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#     E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#     E0_fixtures$e0_6_5 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6 + E0_fixtures$e0_0_1 + E0_fixtures$e0_1_2 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_3_4 + E0_fixtures$e0_4_5 + E0_fixtures$e0_5_6
# )
# #AH_125_A
# E0_fixtures$e0_AH_125_A <- (
#   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#     E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#     E0_fixtures$e0_0_5 +E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#     E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#     E0_fixtures$e0_5_6 + E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 +
#     E0_fixtures$e0_4_4 + E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6 + E0_fixtures$e0_1_0 + E0_fixtures$e0_2_1 +
#     E0_fixtures$e0_3_2 + E0_fixtures$e0_4_3 + E0_fixtures$e0_5_4 + E0_fixtures$e0_6_5
# )
#
# #odds
# E0_fixtures$e0_AH_125_H_odds <- round((1/E0_fixtures$e0_AH_125_H),digits = 2)
# E0_fixtures$e0_AH_125_A_odds <- round((1/E0_fixtures$e0_AH_125_A),digits = 2)
#
# E0_fixtures$e0_AH_125_H_odds
# E0_fixtures$e0_AH_125_A_odds
# #percentages
# E0_fixtures$e0_AH_125_H <- percent(E0_fixtures$e0_AH_125_H, accuracy = 0.1)
# E0_fixtures$e0_AH_125_A <- percent(E0_fixtures$e0_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# E0_fixtures$e0_ov25 <- percent(E0_fixtures$e0_ov25, accuracy = 0.1)
#
# E0_fixtures$e0_un25 <- percent(E0_fixtures$e0_un25, accuracy = 0.1)
# E0_fixtures$e0_pscore <- paste(round(E0_fixtures$e0_xGH,digits = 0),round(E0_fixtures$e0_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(E0_fixtures,'Divisions/E0.xlsx',sheetName = "E0", append = TRUE)
# #########################################################################################################################
# #E1
# HomeTeam_e1 <- rep(e1_teams, each = length(e1_teams))
# AwayTeam_e1 <- rep(e1_teams, length(e1_teams))
# E1_fixtures <- cbind(HomeTeam_e1,AwayTeam_e1)
# E1_fixtures <- as.data.frame(E1_fixtures)
# E1_fixtures <- E1_fixtures[!E1_fixtures$HomeTeam_e1 == E1_fixtures$AwayTeam_e1,]
# rownames(E1_fixtures) <- NULL
# E1_fixtures$Div <- "E1"
# E1_fixtures <- E1_fixtures[,c(3,1,2)]
#
# E1_fixtures$avg_HG_e1 <- e1_avg_HG
#
# E1_fixtures$e1_homeas <- rep(e1_home_as,each = length(e1_teams)-1)
#
# e1_awayds_lookup <- cbind(e1_teams,e1_away_ds)
#
# e1_awayds_lookup <- as.data.frame(e1_awayds_lookup)
#
# colnames(e1_awayds_lookup) <- c("AwayTeam_e1","e1_awayds")
#
#
# require('RH2')
# E1_fixtures$e1_awayds <- sqldf("SELECT e1_awayds_lookup.e1_awayds FROM e1_awayds_lookup INNER JOIN E1_fixtures ON e1_awayds_lookup.AwayTeam_e1 = E1_fixtures.AwayTeam_e1")
#
# E1_fixtures$avg_AG_e1 <- e1_avg_AG
#
# e1_awayas_lookup <- cbind(e1_teams,e1_away_as)
#
# e1_awayas_lookup <- as.data.frame(e1_awayas_lookup)
#
# colnames(e1_awayas_lookup) <- c("AwayTeam_e1","e1_awayas")
#
#
# E1_fixtures$e1_awayas <- sqldf("SELECT e1_awayas_lookup.e1_awayas FROM e1_awayas_lookup INNER JOIN E1_fixtures ON e1_awayas_lookup.AwayTeam_e1 = E1_fixtures.AwayTeam_e1")
#
# E1_fixtures$e1_homeds <- rep(e1_home_ds,each = length(e1_teams)-1)
#
# E1_fixtures$e1_awayds <- as.numeric(unlist(E1_fixtures$e1_awayds))
# #xGH
# E1_fixtures$e1_xGH <- E1_fixtures$avg_HG_e1 * E1_fixtures$e1_homeas * E1_fixtures$e1_awayds
#
# #xGA
#
# E1_fixtures$e1_awayas <- as.numeric(unlist(E1_fixtures$e1_awayas))
#
# E1_fixtures$e1_xGA <- E1_fixtures$avg_AG_e1 * E1_fixtures$e1_awayas * E1_fixtures$e1_homeds
#
# E1_fixtures$e1_0_0 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_0 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_1 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_1 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_0 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_2 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_2 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_1 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_2 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_3 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_0 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_1 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_2 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_3 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_3 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_3 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_4 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_0 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_1 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_2 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_3 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_4 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_4 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_4 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_4 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_5 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_0 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_1 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_2 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_3 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_4 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_5 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_5 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_5 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_5 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_5 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_6 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_0 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(0,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_1 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(1,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_2 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(2,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_3 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(3,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_4 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(4,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_6_5 <- round(stats::dpois(6,E1_fixtures$e1_xGH) * stats::dpois(5,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_0_6 <- round(stats::dpois(0,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_1_6 <- round(stats::dpois(1,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_2_6 <- round(stats::dpois(2,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_3_6 <- round(stats::dpois(3,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_4_6 <- round(stats::dpois(4,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# E1_fixtures$e1_5_6 <- round(stats::dpois(5,E1_fixtures$e1_xGH) * stats::dpois(6,E1_fixtures$e1_xGA), digits = 4)
# #Home win
# E1_fixtures$e1_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 + E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5
# )
#
# E1_fixtures$e1_H <- percent(E1_fixtures$e1_H, accuracy = 0.1)
#
# #Draw
# E1_fixtures$e1_D <- (
#
#   E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 + E1_fixtures$e1_4_4 +
#     E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6
# )
#
# E1_fixtures$e1_D <- percent(E1_fixtures$e1_D, accuracy = 0.1)
#
# #Away
#
# E1_fixtures$e1_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 + E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6
# )
#
# E1_fixtures$e1_A <- percent(E1_fixtures$e1_A, accuracy = 0.1)
#
# #ov25
# E1_fixtures$e1_ov25 <- (
#   E1_fixtures$e1_2_1 + E1_fixtures$e1_1_2 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 + E1_fixtures$e1_2_3 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 + E1_fixtures$e1_0_4 +
#     E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 + E1_fixtures$e1_4_4 + E1_fixtures$e1_5_0 +
#     E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 + E1_fixtures$e1_0_5 +
#     E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 + E1_fixtures$e1_5_5 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5 + E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 +
#     E1_fixtures$e1_4_6 + E1_fixtures$e1_5_6 + E1_fixtures$e1_6_6
# )
# #un25
# E1_fixtures$e1_un25 <- (
#   E1_fixtures$e1_0_0 + E1_fixtures$e1_1_0 + E1_fixtures$e1_0_1 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_0 + E1_fixtures$e1_0_2
# )
# #odds
# E1_fixtures$e1_ov25_odds <- round((1/E1_fixtures$e1_ov25),digits = 2)
# E1_fixtures$e1_un25_odds <- round((1/E1_fixtures$e1_un25),digits = 2)
#
# E1_fixtures$e1_ov25_odds
# E1_fixtures$e1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# E1_fixtures$e1_BTTSY <- (
#   E1_fixtures$e1_1_1 + E1_fixtures$e1_2_1 + E1_fixtures$e1_1_2 + E1_fixtures$e1_3_1 + E1_fixtures$e1_3_2 +
#     E1_fixtures$e1_2_2 + E1_fixtures$e1_1_3 + E1_fixtures$e1_2_3 + E1_fixtures$e1_3_3 + E1_fixtures$e1_4_4 +
#     E1_fixtures$e1_4_1 + E1_fixtures$e1_4_3 + E1_fixtures$e1_4_2 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 +
#     E1_fixtures$e1_3_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 +
#     E1_fixtures$e1_5_4 + E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_6_6 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6
# )
# #BTTSN
# E1_fixtures$e1_BTTSN <- (
#   E1_fixtures$e1_0_0 + E1_fixtures$e1_1_0 + E1_fixtures$e1_0_1 + E1_fixtures$e1_2_0 + E1_fixtures$e1_0_2 +
#     E1_fixtures$e1_3_0 + E1_fixtures$e1_0_3 + E1_fixtures$e1_4_0 + E1_fixtures$e1_0_4 + E1_fixtures$e1_5_0 +
#     E1_fixtures$e1_0_5 + E1_fixtures$e1_6_0 + E1_fixtures$e1_0_6
# )
#
# E1_fixtures$e1_BTTSY_odds <- round((1/E1_fixtures$e1_BTTSY),digits = 2)
# E1_fixtures$e1_BTTSN_odds <- round((1/E1_fixtures$e1_BTTSN),digits = 2)
#
# E1_fixtures$e1_BTTSY <- percent(E1_fixtures$e1_BTTSY, accuracy = 0.1)
# E1_fixtures$e1_BTTSN <- percent(E1_fixtures$e1_BTTSN, accuracy = 0.1)
# #odds
# E1_fixtures$e1_BTTSY_odds
# E1_fixtures$e1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# E1_fixtures$e1_AH_0_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 +E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6
# )
# #AH_0_A
# E1_fixtures$e1_AH_0_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 +E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6
# )
#
# #odds
# E1_fixtures$e1_AH_0_H_odds <- round((1/E1_fixtures$e1_AH_0_H),digits = 2)
# E1_fixtures$e1_AH_0_A_odds <- round((1/E1_fixtures$e1_AH_0_A),digits = 2)
#
# E1_fixtures$e1_AH_0_H_odds
# E1_fixtures$e1_AH_0_A_odds
# #percentages
# E1_fixtures$e1_AH_0_H <- percent(E1_fixtures$e1_AH_0_H, accuracy = 0.1)
# E1_fixtures$e1_AH_0_A <- percent(E1_fixtures$e1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# E1_fixtures$e1_AH_n075_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 +E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5
# )
# #AH_n075_A
# E1_fixtures$e1_AH_n075_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 +E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6
# )
#
# #odds
# E1_fixtures$e1_AH_n075_H_odds <- round((1/E1_fixtures$e1_AH_n075_H),digits = 2)
# E1_fixtures$e1_AH_n075_A_odds <- round((1/E1_fixtures$e1_AH_n075_A),digits = 2)
#
# E1_fixtures$e1_AH_n075_H_odds
# E1_fixtures$e1_AH_n075_A_odds
# #percentages
# E1_fixtures$e1_AH_n075_H <- percent(E1_fixtures$e1_AH_n075_H, accuracy = 0.1)
# E1_fixtures$e1_AH_n075_A <- percent(E1_fixtures$e1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# E1_fixtures$e1_AH_075_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 +E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6 + E1_fixtures$e1_0_1 + E1_fixtures$e1_1_2 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_3_4 + E1_fixtures$e1_4_5 + E1_fixtures$e1_5_6
# )
# #AH_075_A
# E1_fixtures$e1_AH_075_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 +E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6 + E1_fixtures$e1_1_0 + E1_fixtures$e1_2_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_3 + E1_fixtures$e1_5_4 + E1_fixtures$e1_6_5
# )
#
# #odds
# E1_fixtures$e1_AH_075_H_odds <- round((1/E1_fixtures$e1_AH_075_H),digits = 2)
# E1_fixtures$e1_AH_075_A_odds <- round((1/E1_fixtures$e1_AH_075_A),digits = 2)
#
# E1_fixtures$e1_AH_075_H_odds
# E1_fixtures$e1_AH_075_A_odds
# #percentages
# E1_fixtures$e1_AH_075_H <- percent(E1_fixtures$e1_AH_075_H, accuracy = 0.1)
# E1_fixtures$e1_AH_075_A <- percent(E1_fixtures$e1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# E1_fixtures$e1_AH_n125_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 +E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5
# )
# #AH_n125_A
# E1_fixtures$e1_AH_n125_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 +E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6
# )
#
# #odds
# E1_fixtures$e1_AH_n125_H_odds <- round((1/E1_fixtures$e1_AH_n125_H),digits = 2)
# E1_fixtures$e1_AH_n125_A_odds <- round((1/E1_fixtures$e1_AH_n125_A),digits = 2)
#
# E1_fixtures$e1_AH_n125_H_odds
# E1_fixtures$e1_AH_n125_A_odds
# #percentages
# E1_fixtures$e1_AH_n125_H <- percent(E1_fixtures$e1_AH_n125_H, accuracy = 0.1)
# E1_fixtures$e1_AH_n125_A <- percent(E1_fixtures$e1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# E1_fixtures$e1_AH_125_H <- (
#   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#     E1_fixtures$e1_5_0 +E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#     E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#     E1_fixtures$e1_6_5 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6 + E1_fixtures$e1_0_1 + E1_fixtures$e1_1_2 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_3_4 + E1_fixtures$e1_4_5 + E1_fixtures$e1_5_6
# )
# #AH_125_A
# E1_fixtures$e1_AH_125_A <- (
#   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#     E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#     E1_fixtures$e1_0_5 +E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#     E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#     E1_fixtures$e1_5_6 + E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 +
#     E1_fixtures$e1_4_4 + E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6 + E1_fixtures$e1_1_0 + E1_fixtures$e1_2_1 +
#     E1_fixtures$e1_3_2 + E1_fixtures$e1_4_3 + E1_fixtures$e1_5_4 + E1_fixtures$e1_6_5
# )
#
# #odds
# E1_fixtures$e1_AH_125_H_odds <- round((1/E1_fixtures$e1_AH_125_H),digits = 2)
# E1_fixtures$e1_AH_125_A_odds <- round((1/E1_fixtures$e1_AH_125_A),digits = 2)
#
# E1_fixtures$e1_AH_125_H_odds
# E1_fixtures$e1_AH_125_A_odds
# #percentages
# E1_fixtures$e1_AH_125_H <- percent(E1_fixtures$e1_AH_125_H, accuracy = 0.1)
# E1_fixtures$e1_AH_125_A <- percent(E1_fixtures$e1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# E1_fixtures$e1_ov25 <- percent(E1_fixtures$e1_ov25, accuracy = 0.1)
#
# E1_fixtures$e1_un25 <- percent(E1_fixtures$e1_un25, accuracy = 0.1)
# E1_fixtures$e1_pscore <- paste(round(E1_fixtures$e1_xGH,digits = 0),round(E1_fixtures$e1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(E1_fixtures,'Divisions/E1.xlsx',sheetName = "E1", append = TRUE)
# ######################################################################################################################
# #E2
# HomeTeam_e2 <- rep(e2_teams, each = length(e2_teams))
# AwayTeam_e2 <- rep(e2_teams, length(e2_teams))
# E2_fixtures <- cbind(HomeTeam_e2,AwayTeam_e2)
# E2_fixtures <- as.data.frame(E2_fixtures)
# E2_fixtures <- E2_fixtures[!E2_fixtures$HomeTeam_e2 == E2_fixtures$AwayTeam_e2,]
# rownames(E2_fixtures) <- NULL
# E2_fixtures$Div <- "E2"
# E2_fixtures <- E2_fixtures[,c(3,1,2)]
#
# E2_fixtures$avg_HG_e2 <- e2_avg_HG
#
# E2_fixtures$e2_homeas <- rep(e2_home_as,each = length(e2_teams)-1)
#
# e2_awayds_lookup <- cbind(e2_teams,e2_away_ds)
#
# e2_awayds_lookup <- as.data.frame(e2_awayds_lookup)
#
# colnames(e2_awayds_lookup) <- c("AwayTeam_e2","e2_awayds")
#
#
# require('RH2')
# E2_fixtures$e2_awayds <- sqldf("SELECT e2_awayds_lookup.e2_awayds FROM e2_awayds_lookup INNER JOIN E2_fixtures ON e2_awayds_lookup.AwayTeam_e2 = E2_fixtures.AwayTeam_e2")
#
# E2_fixtures$avg_AG_e2 <- e2_avg_AG
#
# e2_awayas_lookup <- cbind(e2_teams,e2_away_as)
#
# e2_awayas_lookup <- as.data.frame(e2_awayas_lookup)
#
# colnames(e2_awayas_lookup) <- c("AwayTeam_e2","e2_awayas")
#
#
# E2_fixtures$e2_awayas <- sqldf("SELECT e2_awayas_lookup.e2_awayas FROM e2_awayas_lookup INNER JOIN E2_fixtures ON e2_awayas_lookup.AwayTeam_e2 = E2_fixtures.AwayTeam_e2")
#
# E2_fixtures$e2_homeds <- rep(e2_home_ds,each = length(e2_teams)-1)
#
# E2_fixtures$e2_awayds <- as.numeric(unlist(E2_fixtures$e2_awayds))
# #xGH
# E2_fixtures$e2_xGH <- E2_fixtures$avg_HG_e2 * E2_fixtures$e2_homeas * E2_fixtures$e2_awayds
#
# #xGA
#
# E2_fixtures$e2_awayas <- as.numeric(unlist(E2_fixtures$e2_awayas))
#
# E2_fixtures$e2_xGA <- E2_fixtures$avg_AG_e2 * E2_fixtures$e2_awayas * E2_fixtures$e2_homeds
#
# E2_fixtures$e2_0_0 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_0 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_1 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_1 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_0 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_2 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_2 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_1 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_2 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_3 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_0 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_1 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_2 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_3 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_3 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_3 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_4 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_0 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_1 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_2 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_3 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_4 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_4 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_4 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_4 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_5 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_0 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_1 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_2 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_3 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_4 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_5 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_5 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_5 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_5 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_5 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_6 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_0 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(0,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_1 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(1,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_2 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(2,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_3 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(3,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_4 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(4,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_6_5 <- round(stats::dpois(6,E2_fixtures$e2_xGH) * stats::dpois(5,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_0_6 <- round(stats::dpois(0,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_1_6 <- round(stats::dpois(1,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_2_6 <- round(stats::dpois(2,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_3_6 <- round(stats::dpois(3,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_4_6 <- round(stats::dpois(4,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# E2_fixtures$e2_5_6 <- round(stats::dpois(5,E2_fixtures$e2_xGH) * stats::dpois(6,E2_fixtures$e2_xGA), digits = 4)
# #Home win
# E2_fixtures$e2_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 + E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5
# )
#
# E2_fixtures$e2_H <- percent(E2_fixtures$e2_H, accuracy = 0.1)
#
# #Draw
# E2_fixtures$e2_D <- (
#
#   E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 + E2_fixtures$e2_4_4 +
#     E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6
# )
#
# E2_fixtures$e2_D <- percent(E2_fixtures$e2_D, accuracy = 0.1)
#
# #Away
#
# E2_fixtures$e2_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 + E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6
# )
#
# E2_fixtures$e2_A <- percent(E2_fixtures$e2_A, accuracy = 0.1)
#
# #ov25
# E2_fixtures$e2_ov25 <- (
#   E2_fixtures$e2_2_1 + E2_fixtures$e2_1_2 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 + E2_fixtures$e2_2_3 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 + E2_fixtures$e2_0_4 +
#     E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 + E2_fixtures$e2_4_4 + E2_fixtures$e2_5_0 +
#     E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 + E2_fixtures$e2_0_5 +
#     E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 + E2_fixtures$e2_5_5 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5 + E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 +
#     E2_fixtures$e2_4_6 + E2_fixtures$e2_5_6 + E2_fixtures$e2_6_6
# )
# #un25
# E2_fixtures$e2_un25 <- (
#   E2_fixtures$e2_0_0 + E2_fixtures$e2_1_0 + E2_fixtures$e2_0_1 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_0 + E2_fixtures$e2_0_2
# )
# #odds
# E2_fixtures$e2_ov25_odds <- round((1/E2_fixtures$e2_ov25),digits = 2)
# E2_fixtures$e2_un25_odds <- round((1/E2_fixtures$e2_un25),digits = 2)
#
# E2_fixtures$e2_ov25_odds
# E2_fixtures$e2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# E2_fixtures$e2_BTTSY <- (
#   E2_fixtures$e2_1_1 + E2_fixtures$e2_2_1 + E2_fixtures$e2_1_2 + E2_fixtures$e2_3_1 + E2_fixtures$e2_3_2 +
#     E2_fixtures$e2_2_2 + E2_fixtures$e2_1_3 + E2_fixtures$e2_2_3 + E2_fixtures$e2_3_3 + E2_fixtures$e2_4_4 +
#     E2_fixtures$e2_4_1 + E2_fixtures$e2_4_3 + E2_fixtures$e2_4_2 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 +
#     E2_fixtures$e2_3_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 +
#     E2_fixtures$e2_5_4 + E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_6_6 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6
# )
# #BTTSN
# E2_fixtures$e2_BTTSN <- (
#   E2_fixtures$e2_0_0 + E2_fixtures$e2_1_0 + E2_fixtures$e2_0_1 + E2_fixtures$e2_2_0 + E2_fixtures$e2_0_2 +
#     E2_fixtures$e2_3_0 + E2_fixtures$e2_0_3 + E2_fixtures$e2_4_0 + E2_fixtures$e2_0_4 + E2_fixtures$e2_5_0 +
#     E2_fixtures$e2_0_5 + E2_fixtures$e2_6_0 + E2_fixtures$e2_0_6
# )
#
# E2_fixtures$e2_BTTSY_odds <- round((1/E2_fixtures$e2_BTTSY),digits = 2)
# E2_fixtures$e2_BTTSN_odds <- round((1/E2_fixtures$e2_BTTSN),digits = 2)
#
# E2_fixtures$e2_BTTSY <- percent(E2_fixtures$e2_BTTSY, accuracy = 0.1)
# E2_fixtures$e2_BTTSN <- percent(E2_fixtures$e2_BTTSN, accuracy = 0.1)
# #odds
# E2_fixtures$e2_BTTSY_odds
# E2_fixtures$e2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# E2_fixtures$e2_AH_0_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 +E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6
# )
# #AH_0_A
# E2_fixtures$e2_AH_0_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 +E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6
# )
#
# #odds
# E2_fixtures$e2_AH_0_H_odds <- round((1/E2_fixtures$e2_AH_0_H),digits = 2)
# E2_fixtures$e2_AH_0_A_odds <- round((1/E2_fixtures$e2_AH_0_A),digits = 2)
#
# E2_fixtures$e2_AH_0_H_odds
# E2_fixtures$e2_AH_0_A_odds
# #percentages
# E2_fixtures$e2_AH_0_H <- percent(E2_fixtures$e2_AH_0_H, accuracy = 0.1)
# E2_fixtures$e2_AH_0_A <- percent(E2_fixtures$e2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# E2_fixtures$e2_AH_n075_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 +E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5
# )
# #AH_n075_A
# E2_fixtures$e2_AH_n075_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 +E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6
# )
#
# #odds
# E2_fixtures$e2_AH_n075_H_odds <- round((1/E2_fixtures$e2_AH_n075_H),digits = 2)
# E2_fixtures$e2_AH_n075_A_odds <- round((1/E2_fixtures$e2_AH_n075_A),digits = 2)
#
# E2_fixtures$e2_AH_n075_H_odds
# E2_fixtures$e2_AH_n075_A_odds
# #percentages
# E2_fixtures$e2_AH_n075_H <- percent(E2_fixtures$e2_AH_n075_H, accuracy = 0.1)
# E2_fixtures$e2_AH_n075_A <- percent(E2_fixtures$e2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# E2_fixtures$e2_AH_075_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 +E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6 + E2_fixtures$e2_0_1 + E2_fixtures$e2_1_2 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_3_4 + E2_fixtures$e2_4_5 + E2_fixtures$e2_5_6
# )
# #AH_075_A
# E2_fixtures$e2_AH_075_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 +E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6 + E2_fixtures$e2_1_0 + E2_fixtures$e2_2_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_3 + E2_fixtures$e2_5_4 + E2_fixtures$e2_6_5
# )
#
# #odds
# E2_fixtures$e2_AH_075_H_odds <- round((1/E2_fixtures$e2_AH_075_H),digits = 2)
# E2_fixtures$e2_AH_075_A_odds <- round((1/E2_fixtures$e2_AH_075_A),digits = 2)
#
# E2_fixtures$e2_AH_075_H_odds
# E2_fixtures$e2_AH_075_A_odds
# #percentages
# E2_fixtures$e2_AH_075_H <- percent(E2_fixtures$e2_AH_075_H, accuracy = 0.1)
# E2_fixtures$e2_AH_075_A <- percent(E2_fixtures$e2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# E2_fixtures$e2_AH_n125_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 +E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5
# )
# #AH_n125_A
# E2_fixtures$e2_AH_n125_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 +E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6
# )
#
# #odds
# E2_fixtures$e2_AH_n125_H_odds <- round((1/E2_fixtures$e2_AH_n125_H),digits = 2)
# E2_fixtures$e2_AH_n125_A_odds <- round((1/E2_fixtures$e2_AH_n125_A),digits = 2)
#
# E2_fixtures$e2_AH_n125_H_odds
# E2_fixtures$e2_AH_n125_A_odds
# #percentages
# E2_fixtures$e2_AH_n125_H <- percent(E2_fixtures$e2_AH_n125_H, accuracy = 0.1)
# E2_fixtures$e2_AH_n125_A <- percent(E2_fixtures$e2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# E2_fixtures$e2_AH_125_H <- (
#   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#     E2_fixtures$e2_5_0 +E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#     E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#     E2_fixtures$e2_6_5 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6 + E2_fixtures$e2_0_1 + E2_fixtures$e2_1_2 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_3_4 + E2_fixtures$e2_4_5 + E2_fixtures$e2_5_6
# )
# #AH_125_A
# E2_fixtures$e2_AH_125_A <- (
#   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#     E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#     E2_fixtures$e2_0_5 +E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#     E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#     E2_fixtures$e2_5_6 + E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 +
#     E2_fixtures$e2_4_4 + E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6 + E2_fixtures$e2_1_0 + E2_fixtures$e2_2_1 +
#     E2_fixtures$e2_3_2 + E2_fixtures$e2_4_3 + E2_fixtures$e2_5_4 + E2_fixtures$e2_6_5
# )
#
# #odds
# E2_fixtures$e2_AH_125_H_odds <- round((1/E2_fixtures$e2_AH_125_H),digits = 2)
# E2_fixtures$e2_AH_125_A_odds <- round((1/E2_fixtures$e2_AH_125_A),digits = 2)
#
# E2_fixtures$e2_AH_125_H_odds
# E2_fixtures$e2_AH_125_A_odds
# #percentages
# E2_fixtures$e2_AH_125_H <- percent(E2_fixtures$e2_AH_125_H, accuracy = 0.1)
# E2_fixtures$e2_AH_125_A <- percent(E2_fixtures$e2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# E2_fixtures$e2_ov25 <- percent(E2_fixtures$e2_ov25, accuracy = 0.1)
#
# E2_fixtures$e2_un25 <- percent(E2_fixtures$e2_un25, accuracy = 0.1)
# E2_fixtures$e2_pscore <- paste(round(E2_fixtures$e2_xGH,digits = 0),round(E2_fixtures$e2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(E2_fixtures,'Divisions/E2.xlsx',sheetName = "E2", append = TRUE)
# ###########################################################################################################################
# #E3
# HomeTeam_e3 <- rep(e3_teams, each = length(e3_teams))
# AwayTeam_e3 <- rep(e3_teams, length(e3_teams))
# E3_fixtures <- cbind(HomeTeam_e3,AwayTeam_e3)
# E3_fixtures <- as.data.frame(E3_fixtures)
# E3_fixtures <- E3_fixtures[!E3_fixtures$HomeTeam_e3 == E3_fixtures$AwayTeam_e3,]
# rownames(E3_fixtures) <- NULL
# E3_fixtures$Div <- "E3"
# E3_fixtures <- E3_fixtures[,c(3,1,2)]
#
# E3_fixtures$avg_HG_e3 <- e3_avg_HG
#
# E3_fixtures$e3_homeas <- rep(e3_home_as,each = length(e3_teams)-1)
#
# e3_awayds_lookup <- cbind(e3_teams,e3_away_ds)
#
# e3_awayds_lookup <- as.data.frame(e3_awayds_lookup)
#
# colnames(e3_awayds_lookup) <- c("AwayTeam_e3","e3_awayds")
#
#
# require('RH2')
# E3_fixtures$e3_awayds <- sqldf("SELECT e3_awayds_lookup.e3_awayds FROM e3_awayds_lookup INNER JOIN E3_fixtures ON e3_awayds_lookup.AwayTeam_e3 = E3_fixtures.AwayTeam_e3")
#
# E3_fixtures$avg_AG_e3 <- e3_avg_AG
#
# e3_awayas_lookup <- cbind(e3_teams,e3_away_as)
#
# e3_awayas_lookup <- as.data.frame(e3_awayas_lookup)
#
# colnames(e3_awayas_lookup) <- c("AwayTeam_e3","e3_awayas")
#
#
# E3_fixtures$e3_awayas <- sqldf("SELECT e3_awayas_lookup.e3_awayas FROM e3_awayas_lookup INNER JOIN E3_fixtures ON e3_awayas_lookup.AwayTeam_e3 = E3_fixtures.AwayTeam_e3")
#
# E3_fixtures$e3_homeds <- rep(e3_home_ds,each = length(e3_teams)-1)
#
# E3_fixtures$e3_awayds <- as.numeric(unlist(E3_fixtures$e3_awayds))
# #xGH
# E3_fixtures$e3_xGH <- E3_fixtures$avg_HG_e3 * E3_fixtures$e3_homeas * E3_fixtures$e3_awayds
#
# #xGA
#
# E3_fixtures$e3_awayas <- as.numeric(unlist(E3_fixtures$e3_awayas))
#
# E3_fixtures$e3_xGA <- E3_fixtures$avg_AG_e3 * E3_fixtures$e3_awayas * E3_fixtures$e3_homeds
#
# E3_fixtures$e3_0_0 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_0 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_1 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_1 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_0 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_2 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_2 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_1 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_2 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_3 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_0 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_1 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_2 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_3 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_3 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_3 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_4 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_0 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_1 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_2 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_3 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_4 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_4 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_4 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_4 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_5 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_0 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_1 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_2 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_3 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_4 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_5 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_5 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_5 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_5 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_5 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_6 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_0 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(0,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_1 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(1,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_2 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(2,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_3 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(3,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_4 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(4,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_6_5 <- round(stats::dpois(6,E3_fixtures$e3_xGH) * stats::dpois(5,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_0_6 <- round(stats::dpois(0,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_1_6 <- round(stats::dpois(1,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_2_6 <- round(stats::dpois(2,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_3_6 <- round(stats::dpois(3,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_4_6 <- round(stats::dpois(4,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# E3_fixtures$e3_5_6 <- round(stats::dpois(5,E3_fixtures$e3_xGH) * stats::dpois(6,E3_fixtures$e3_xGA), digits = 4)
# #Home win
# E3_fixtures$e3_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 + E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5
# )
#
# E3_fixtures$e3_H <- percent(E3_fixtures$e3_H, accuracy = 0.1)
#
# #Draw
# E3_fixtures$e3_D <- (
#
#   E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 + E3_fixtures$e3_4_4 +
#     E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6
# )
#
# E3_fixtures$e3_D <- percent(E3_fixtures$e3_D, accuracy = 0.1)
#
# #Away
#
# E3_fixtures$e3_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 + E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6
# )
#
# E3_fixtures$e3_A <- percent(E3_fixtures$e3_A, accuracy = 0.1)
#
# #ov25
# E3_fixtures$e3_ov25 <- (
#   E3_fixtures$e3_2_1 + E3_fixtures$e3_1_2 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 + E3_fixtures$e3_2_3 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 + E3_fixtures$e3_0_4 +
#     E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 + E3_fixtures$e3_4_4 + E3_fixtures$e3_5_0 +
#     E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 + E3_fixtures$e3_0_5 +
#     E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 + E3_fixtures$e3_5_5 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5 + E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 +
#     E3_fixtures$e3_4_6 + E3_fixtures$e3_5_6 + E3_fixtures$e3_6_6
# )
# #un25
# E3_fixtures$e3_un25 <- (
#   E3_fixtures$e3_0_0 + E3_fixtures$e3_1_0 + E3_fixtures$e3_0_1 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_0 + E3_fixtures$e3_0_2
# )
# #odds
# E3_fixtures$e3_ov25_odds <- round((1/E3_fixtures$e3_ov25),digits = 2)
# E3_fixtures$e3_un25_odds <- round((1/E3_fixtures$e3_un25),digits = 2)
#
# E3_fixtures$e3_ov25_odds
# E3_fixtures$e3_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# E3_fixtures$e3_BTTSY <- (
#   E3_fixtures$e3_1_1 + E3_fixtures$e3_2_1 + E3_fixtures$e3_1_2 + E3_fixtures$e3_3_1 + E3_fixtures$e3_3_2 +
#     E3_fixtures$e3_2_2 + E3_fixtures$e3_1_3 + E3_fixtures$e3_2_3 + E3_fixtures$e3_3_3 + E3_fixtures$e3_4_4 +
#     E3_fixtures$e3_4_1 + E3_fixtures$e3_4_3 + E3_fixtures$e3_4_2 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 +
#     E3_fixtures$e3_3_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 +
#     E3_fixtures$e3_5_4 + E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_6_6 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6
# )
# #BTTSN
# E3_fixtures$e3_BTTSN <- (
#   E3_fixtures$e3_0_0 + E3_fixtures$e3_1_0 + E3_fixtures$e3_0_1 + E3_fixtures$e3_2_0 + E3_fixtures$e3_0_2 +
#     E3_fixtures$e3_3_0 + E3_fixtures$e3_0_3 + E3_fixtures$e3_4_0 + E3_fixtures$e3_0_4 + E3_fixtures$e3_5_0 +
#     E3_fixtures$e3_0_5 + E3_fixtures$e3_6_0 + E3_fixtures$e3_0_6
# )
#
# E3_fixtures$e3_BTTSY_odds <- round((1/E3_fixtures$e3_BTTSY),digits = 2)
# E3_fixtures$e3_BTTSN_odds <- round((1/E3_fixtures$e3_BTTSN),digits = 2)
#
# E3_fixtures$e3_BTTSY <- percent(E3_fixtures$e3_BTTSY, accuracy = 0.1)
# E3_fixtures$e3_BTTSN <- percent(E3_fixtures$e3_BTTSN, accuracy = 0.1)
# #odds
# E3_fixtures$e3_BTTSY_odds
# E3_fixtures$e3_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# E3_fixtures$e3_AH_0_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 +E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6
# )
# #AH_0_A
# E3_fixtures$e3_AH_0_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 +E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6
# )
#
# #odds
# E3_fixtures$e3_AH_0_H_odds <- round((1/E3_fixtures$e3_AH_0_H),digits = 2)
# E3_fixtures$e3_AH_0_A_odds <- round((1/E3_fixtures$e3_AH_0_A),digits = 2)
#
# E3_fixtures$e3_AH_0_H_odds
# E3_fixtures$e3_AH_0_A_odds
# #percentages
# E3_fixtures$e3_AH_0_H <- percent(E3_fixtures$e3_AH_0_H, accuracy = 0.1)
# E3_fixtures$e3_AH_0_A <- percent(E3_fixtures$e3_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# E3_fixtures$e3_AH_n075_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 +E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5
# )
# #AH_n075_A
# E3_fixtures$e3_AH_n075_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 +E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6
# )
#
# #odds
# E3_fixtures$e3_AH_n075_H_odds <- round((1/E3_fixtures$e3_AH_n075_H),digits = 2)
# E3_fixtures$e3_AH_n075_A_odds <- round((1/E3_fixtures$e3_AH_n075_A),digits = 2)
#
# E3_fixtures$e3_AH_n075_H_odds
# E3_fixtures$e3_AH_n075_A_odds
# #percentages
# E3_fixtures$e3_AH_n075_H <- percent(E3_fixtures$e3_AH_n075_H, accuracy = 0.1)
# E3_fixtures$e3_AH_n075_A <- percent(E3_fixtures$e3_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# E3_fixtures$e3_AH_075_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 +E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6 + E3_fixtures$e3_0_1 + E3_fixtures$e3_1_2 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_3_4 + E3_fixtures$e3_4_5 + E3_fixtures$e3_5_6
# )
# #AH_075_A
# E3_fixtures$e3_AH_075_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 +E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6 + E3_fixtures$e3_1_0 + E3_fixtures$e3_2_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_3 + E3_fixtures$e3_5_4 + E3_fixtures$e3_6_5
# )
#
# #odds
# E3_fixtures$e3_AH_075_H_odds <- round((1/E3_fixtures$e3_AH_075_H),digits = 2)
# E3_fixtures$e3_AH_075_A_odds <- round((1/E3_fixtures$e3_AH_075_A),digits = 2)
#
# E3_fixtures$e3_AH_075_H_odds
# E3_fixtures$e3_AH_075_A_odds
# #percentages
# E3_fixtures$e3_AH_075_H <- percent(E3_fixtures$e3_AH_075_H, accuracy = 0.1)
# E3_fixtures$e3_AH_075_A <- percent(E3_fixtures$e3_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# E3_fixtures$e3_AH_n125_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 +E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5
# )
# #AH_n125_A
# E3_fixtures$e3_AH_n125_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 +E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6
# )
#
# #odds
# E3_fixtures$e3_AH_n125_H_odds <- round((1/E3_fixtures$e3_AH_n125_H),digits = 2)
# E3_fixtures$e3_AH_n125_A_odds <- round((1/E3_fixtures$e3_AH_n125_A),digits = 2)
#
# E3_fixtures$e3_AH_n125_H_odds
# E3_fixtures$e3_AH_n125_A_odds
# #percentages
# E3_fixtures$e3_AH_n125_H <- percent(E3_fixtures$e3_AH_n125_H, accuracy = 0.1)
# E3_fixtures$e3_AH_n125_A <- percent(E3_fixtures$e3_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# E3_fixtures$e3_AH_125_H <- (
#   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#     E3_fixtures$e3_5_0 +E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#     E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#     E3_fixtures$e3_6_5 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6 + E3_fixtures$e3_0_1 + E3_fixtures$e3_1_2 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_3_4 + E3_fixtures$e3_4_5 + E3_fixtures$e3_5_6
# )
# #AH_125_A
# E3_fixtures$e3_AH_125_A <- (
#   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#     E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#     E3_fixtures$e3_0_5 +E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#     E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#     E3_fixtures$e3_5_6 + E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 +
#     E3_fixtures$e3_4_4 + E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6 + E3_fixtures$e3_1_0 + E3_fixtures$e3_2_1 +
#     E3_fixtures$e3_3_2 + E3_fixtures$e3_4_3 + E3_fixtures$e3_5_4 + E3_fixtures$e3_6_5
# )
#
# #odds
# E3_fixtures$e3_AH_125_H_odds <- round((1/E3_fixtures$e3_AH_125_H),digits = 2)
# E3_fixtures$e3_AH_125_A_odds <- round((1/E3_fixtures$e3_AH_125_A),digits = 2)
#
# E3_fixtures$e3_AH_125_H_odds
# E3_fixtures$e3_AH_125_A_odds
# #percentages
# E3_fixtures$e3_AH_125_H <- percent(E3_fixtures$e3_AH_125_H, accuracy = 0.1)
# E3_fixtures$e3_AH_125_A <- percent(E3_fixtures$e3_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# E3_fixtures$e3_ov25 <- percent(E3_fixtures$e3_ov25, accuracy = 0.1)
#
# E3_fixtures$e3_un25 <- percent(E3_fixtures$e3_un25, accuracy = 0.1)
# E3_fixtures$e3_pscore <- paste(round(E3_fixtures$e3_xGH,digits = 0),round(E3_fixtures$e3_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(E3_fixtures,'Divisions/E3.xlsx',sheetName = "E3", append = TRUE)
# ##########################################################################################################################
# #EC
# HomeTeam_ec <- rep(ec_teams, each = length(ec_teams))
# AwayTeam_ec <- rep(ec_teams, length(ec_teams))
# EC_fixtures <- cbind(HomeTeam_ec,AwayTeam_ec)
# EC_fixtures <- as.data.frame(EC_fixtures)
# EC_fixtures <- EC_fixtures[!EC_fixtures$HomeTeam_ec == EC_fixtures$AwayTeam_ec,]
# rownames(EC_fixtures) <- NULL
# EC_fixtures$Div <- "EC"
# EC_fixtures <- EC_fixtures[,c(3,1,2)]
#
# EC_fixtures$avg_HG_ec <- ec_avg_HG
#
# EC_fixtures$ec_homeas <- rep(ec_home_as,each = length(ec_teams)-1)
#
# ec_awayds_lookup <- cbind(ec_teams,ec_away_ds)
#
# ec_awayds_lookup <- as.data.frame(ec_awayds_lookup)
#
# colnames(ec_awayds_lookup) <- c("AwayTeam_ec","ec_awayds")
#
#
# require('RH2')
# EC_fixtures$ec_awayds <- sqldf("SELECT ec_awayds_lookup.ec_awayds FROM ec_awayds_lookup INNER JOIN EC_fixtures ON ec_awayds_lookup.AwayTeam_ec = EC_fixtures.AwayTeam_ec")
#
# EC_fixtures$avg_AG_ec <- ec_avg_AG
#
# ec_awayas_lookup <- cbind(ec_teams,ec_away_as)
#
# ec_awayas_lookup <- as.data.frame(ec_awayas_lookup)
#
# colnames(ec_awayas_lookup) <- c("AwayTeam_ec","ec_awayas")
#
#
# EC_fixtures$ec_awayas <- sqldf("SELECT ec_awayas_lookup.ec_awayas FROM ec_awayas_lookup INNER JOIN EC_fixtures ON ec_awayas_lookup.AwayTeam_ec = EC_fixtures.AwayTeam_ec")
#
# EC_fixtures$ec_homeds <- rep(ec_home_ds,each = length(ec_teams)-1)
#
# EC_fixtures$ec_awayds <- as.numeric(unlist(EC_fixtures$ec_awayds))
# #xGH
# EC_fixtures$ec_xGH <- EC_fixtures$avg_HG_ec * EC_fixtures$ec_homeas * EC_fixtures$ec_awayds
#
# #xGA
#
# EC_fixtures$ec_awayas <- as.numeric(unlist(EC_fixtures$ec_awayas))
#
# EC_fixtures$ec_xGA <- EC_fixtures$avg_AG_ec * EC_fixtures$ec_awayas * EC_fixtures$ec_homeds
#
# EC_fixtures$ec_0_0 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_0 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_1 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_1 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_0 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_2 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_2 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_1 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_2 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_3 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_0 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_1 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_2 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_3 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_3 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_3 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_4 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_0 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_1 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_2 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_3 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_4 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_4 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_4 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_4 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_5 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_0 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_1 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_2 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_3 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_4 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_5 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_5 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_5 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_5 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_5 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_6 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_0 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(0,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_1 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(1,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_2 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(2,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_3 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(3,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_4 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(4,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_6_5 <- round(stats::dpois(6,EC_fixtures$ec_xGH) * stats::dpois(5,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_0_6 <- round(stats::dpois(0,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_1_6 <- round(stats::dpois(1,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_2_6 <- round(stats::dpois(2,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_3_6 <- round(stats::dpois(3,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_4_6 <- round(stats::dpois(4,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# EC_fixtures$ec_5_6 <- round(stats::dpois(5,EC_fixtures$ec_xGH) * stats::dpois(6,EC_fixtures$ec_xGA), digits = 4)
# #Home win
# EC_fixtures$ec_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 + EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5
# )
#
# EC_fixtures$ec_H <- percent(EC_fixtures$ec_H, accuracy = 0.1)
#
# #Draw
# EC_fixtures$ec_D <- (
#
#   EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 + EC_fixtures$ec_4_4 +
#     EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6
# )
#
# EC_fixtures$ec_D <- percent(EC_fixtures$ec_D, accuracy = 0.1)
#
# #Away
#
# EC_fixtures$ec_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 + EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6
# )
#
# EC_fixtures$ec_A <- percent(EC_fixtures$ec_A, accuracy = 0.1)
#
# #ov25
# EC_fixtures$ec_ov25 <- (
#   EC_fixtures$ec_2_1 + EC_fixtures$ec_1_2 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 + EC_fixtures$ec_2_3 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 + EC_fixtures$ec_0_4 +
#     EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 + EC_fixtures$ec_4_4 + EC_fixtures$ec_5_0 +
#     EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 + EC_fixtures$ec_0_5 +
#     EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 + EC_fixtures$ec_5_5 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5 + EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 +
#     EC_fixtures$ec_4_6 + EC_fixtures$ec_5_6 + EC_fixtures$ec_6_6
# )
# #un25
# EC_fixtures$ec_un25 <- (
#   EC_fixtures$ec_0_0 + EC_fixtures$ec_1_0 + EC_fixtures$ec_0_1 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_0 + EC_fixtures$ec_0_2
# )
# #odds
# EC_fixtures$ec_ov25_odds <- round((1/EC_fixtures$ec_ov25),digits = 2)
# EC_fixtures$ec_un25_odds <- round((1/EC_fixtures$ec_un25),digits = 2)
#
# EC_fixtures$ec_ov25_odds
# EC_fixtures$ec_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# EC_fixtures$ec_BTTSY <- (
#   EC_fixtures$ec_1_1 + EC_fixtures$ec_2_1 + EC_fixtures$ec_1_2 + EC_fixtures$ec_3_1 + EC_fixtures$ec_3_2 +
#     EC_fixtures$ec_2_2 + EC_fixtures$ec_1_3 + EC_fixtures$ec_2_3 + EC_fixtures$ec_3_3 + EC_fixtures$ec_4_4 +
#     EC_fixtures$ec_4_1 + EC_fixtures$ec_4_3 + EC_fixtures$ec_4_2 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 +
#     EC_fixtures$ec_3_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 +
#     EC_fixtures$ec_5_4 + EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_6_6 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6
# )
# #BTTSN
# EC_fixtures$ec_BTTSN <- (
#   EC_fixtures$ec_0_0 + EC_fixtures$ec_1_0 + EC_fixtures$ec_0_1 + EC_fixtures$ec_2_0 + EC_fixtures$ec_0_2 +
#     EC_fixtures$ec_3_0 + EC_fixtures$ec_0_3 + EC_fixtures$ec_4_0 + EC_fixtures$ec_0_4 + EC_fixtures$ec_5_0 +
#     EC_fixtures$ec_0_5 + EC_fixtures$ec_6_0 + EC_fixtures$ec_0_6
# )
#
# EC_fixtures$ec_BTTSY_odds <- round((1/EC_fixtures$ec_BTTSY),digits = 2)
# EC_fixtures$ec_BTTSN_odds <- round((1/EC_fixtures$ec_BTTSN),digits = 2)
#
# EC_fixtures$ec_BTTSY <- percent(EC_fixtures$ec_BTTSY, accuracy = 0.1)
# EC_fixtures$ec_BTTSN <- percent(EC_fixtures$ec_BTTSN, accuracy = 0.1)
# #odds
# EC_fixtures$ec_BTTSY_odds
# EC_fixtures$ec_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# EC_fixtures$ec_AH_0_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 +EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6
# )
# #AH_0_A
# EC_fixtures$ec_AH_0_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 +EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6
# )
#
# #odds
# EC_fixtures$ec_AH_0_H_odds <- round((1/EC_fixtures$ec_AH_0_H),digits = 2)
# EC_fixtures$ec_AH_0_A_odds <- round((1/EC_fixtures$ec_AH_0_A),digits = 2)
#
# EC_fixtures$ec_AH_0_H_odds
# EC_fixtures$ec_AH_0_A_odds
# #percentages
# EC_fixtures$ec_AH_0_H <- percent(EC_fixtures$ec_AH_0_H, accuracy = 0.1)
# EC_fixtures$ec_AH_0_A <- percent(EC_fixtures$ec_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# EC_fixtures$ec_AH_n075_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 +EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5
# )
# #AH_n075_A
# EC_fixtures$ec_AH_n075_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 +EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6
# )
#
# #odds
# EC_fixtures$ec_AH_n075_H_odds <- round((1/EC_fixtures$ec_AH_n075_H),digits = 2)
# EC_fixtures$ec_AH_n075_A_odds <- round((1/EC_fixtures$ec_AH_n075_A),digits = 2)
#
# EC_fixtures$ec_AH_n075_H_odds
# EC_fixtures$ec_AH_n075_A_odds
# #percentages
# EC_fixtures$ec_AH_n075_H <- percent(EC_fixtures$ec_AH_n075_H, accuracy = 0.1)
# EC_fixtures$ec_AH_n075_A <- percent(EC_fixtures$ec_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# EC_fixtures$ec_AH_075_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 +EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6 + EC_fixtures$ec_0_1 + EC_fixtures$ec_1_2 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_3_4 + EC_fixtures$ec_4_5 + EC_fixtures$ec_5_6
# )
# #AH_075_A
# EC_fixtures$ec_AH_075_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 +EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6 + EC_fixtures$ec_1_0 + EC_fixtures$ec_2_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_3 + EC_fixtures$ec_5_4 + EC_fixtures$ec_6_5
# )
#
# #odds
# EC_fixtures$ec_AH_075_H_odds <- round((1/EC_fixtures$ec_AH_075_H),digits = 2)
# EC_fixtures$ec_AH_075_A_odds <- round((1/EC_fixtures$ec_AH_075_A),digits = 2)
#
# EC_fixtures$ec_AH_075_H_odds
# EC_fixtures$ec_AH_075_A_odds
# #percentages
# EC_fixtures$ec_AH_075_H <- percent(EC_fixtures$ec_AH_075_H, accuracy = 0.1)
# EC_fixtures$ec_AH_075_A <- percent(EC_fixtures$ec_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# EC_fixtures$ec_AH_n125_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 +EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5
# )
# #AH_n125_A
# EC_fixtures$ec_AH_n125_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 +EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6
# )
#
# #odds
# EC_fixtures$ec_AH_n125_H_odds <- round((1/EC_fixtures$ec_AH_n125_H),digits = 2)
# EC_fixtures$ec_AH_n125_A_odds <- round((1/EC_fixtures$ec_AH_n125_A),digits = 2)
#
# EC_fixtures$ec_AH_n125_H_odds
# EC_fixtures$ec_AH_n125_A_odds
# #percentages
# EC_fixtures$ec_AH_n125_H <- percent(EC_fixtures$ec_AH_n125_H, accuracy = 0.1)
# EC_fixtures$ec_AH_n125_A <- percent(EC_fixtures$ec_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# EC_fixtures$ec_AH_125_H <- (
#   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#     EC_fixtures$ec_5_0 +EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#     EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#     EC_fixtures$ec_6_5 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6 + EC_fixtures$ec_0_1 + EC_fixtures$ec_1_2 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_3_4 + EC_fixtures$ec_4_5 + EC_fixtures$ec_5_6
# )
# #AH_125_A
# EC_fixtures$ec_AH_125_A <- (
#   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#     EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#     EC_fixtures$ec_0_5 +EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#     EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#     EC_fixtures$ec_5_6 + EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 +
#     EC_fixtures$ec_4_4 + EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6 + EC_fixtures$ec_1_0 + EC_fixtures$ec_2_1 +
#     EC_fixtures$ec_3_2 + EC_fixtures$ec_4_3 + EC_fixtures$ec_5_4 + EC_fixtures$ec_6_5
# )
#
# #odds
# EC_fixtures$ec_AH_125_H_odds <- round((1/EC_fixtures$ec_AH_125_H),digits = 2)
# EC_fixtures$ec_AH_125_A_odds <- round((1/EC_fixtures$ec_AH_125_A),digits = 2)
#
# EC_fixtures$ec_AH_125_H_odds
# EC_fixtures$ec_AH_125_A_odds
# #percentages
# EC_fixtures$ec_AH_125_H <- percent(EC_fixtures$ec_AH_125_H, accuracy = 0.1)
# EC_fixtures$ec_AH_125_A <- percent(EC_fixtures$ec_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# EC_fixtures$ec_ov25 <- percent(EC_fixtures$ec_ov25, accuracy = 0.1)
#
# EC_fixtures$ec_un25 <- percent(EC_fixtures$ec_un25, accuracy = 0.1)
# EC_fixtures$ec_pscore <- paste(round(EC_fixtures$ec_xGH,digits = 0),round(EC_fixtures$ec_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(EC_fixtures,'Divisions/EC.xlsx',sheetName = "EC", append = TRUE)
# ##########################################################################################################################
# #F1
# HomeTeam_f1 <- rep(f1_teams, each = length(f1_teams))
# AwayTeam_f1 <- rep(f1_teams, length(f1_teams))
# F1_fixtures <- cbind(HomeTeam_f1,AwayTeam_f1)
# F1_fixtures <- as.data.frame(F1_fixtures)
# F1_fixtures <- F1_fixtures[!F1_fixtures$HomeTeam_f1 == F1_fixtures$AwayTeam_f1,]
# rownames(F1_fixtures) <- NULL
# F1_fixtures$Div <- "F1"
# F1_fixtures <- F1_fixtures[,c(3,1,2)]
#
# F1_fixtures$avg_HG_f1 <- f1_avg_HG
#
# F1_fixtures$f1_homeas <- rep(f1_home_as,each = length(f1_teams)-1)
#
# f1_awayds_lookup <- cbind(f1_teams,f1_away_ds)
#
# f1_awayds_lookup <- as.data.frame(f1_awayds_lookup)
#
# colnames(f1_awayds_lookup) <- c("AwayTeam_f1","f1_awayds")
#
#
# require('RH2')
# F1_fixtures$f1_awayds <- sqldf("SELECT f1_awayds_lookup.f1_awayds FROM f1_awayds_lookup INNER JOIN F1_fixtures ON f1_awayds_lookup.AwayTeam_f1 = F1_fixtures.AwayTeam_f1")
#
# F1_fixtures$avg_AG_f1 <- f1_avg_AG
#
# f1_awayas_lookup <- cbind(f1_teams,f1_away_as)
#
# f1_awayas_lookup <- as.data.frame(f1_awayas_lookup)
#
# colnames(f1_awayas_lookup) <- c("AwayTeam_f1","f1_awayas")
#
#
# F1_fixtures$f1_awayas <- sqldf("SELECT f1_awayas_lookup.f1_awayas FROM f1_awayas_lookup INNER JOIN F1_fixtures ON f1_awayas_lookup.AwayTeam_f1 = F1_fixtures.AwayTeam_f1")
#
# F1_fixtures$f1_homeds <- rep(f1_home_ds,each = length(f1_teams)-1)
#
# F1_fixtures$f1_awayds <- as.numeric(unlist(F1_fixtures$f1_awayds))
# #xGH
# F1_fixtures$f1_xGH <- F1_fixtures$avg_HG_f1 * F1_fixtures$f1_homeas * F1_fixtures$f1_awayds
#
# #xGA
#
# F1_fixtures$f1_awayas <- as.numeric(unlist(F1_fixtures$f1_awayas))
#
# F1_fixtures$f1_xGA <- F1_fixtures$avg_AG_f1 * F1_fixtures$f1_awayas * F1_fixtures$f1_homeds
#
# F1_fixtures$f1_0_0 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_0 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_1 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_1 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_0 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_2 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_2 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_1 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_2 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_3 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_0 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_1 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_2 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_3 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_3 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_3 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_4 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_0 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_1 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_2 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_3 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_4 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_4 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_4 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_4 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_5 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_0 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_1 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_2 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_3 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_4 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_5 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_5 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_5 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_5 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_5 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_6 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_0 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(0,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_1 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(1,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_2 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(2,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_3 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(3,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_4 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(4,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_6_5 <- round(stats::dpois(6,F1_fixtures$f1_xGH) * stats::dpois(5,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_0_6 <- round(stats::dpois(0,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_1_6 <- round(stats::dpois(1,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_2_6 <- round(stats::dpois(2,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_3_6 <- round(stats::dpois(3,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_4_6 <- round(stats::dpois(4,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# F1_fixtures$f1_5_6 <- round(stats::dpois(5,F1_fixtures$f1_xGH) * stats::dpois(6,F1_fixtures$f1_xGA), digits = 4)
# #Home win
# F1_fixtures$f1_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 + F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5
# )
#
# F1_fixtures$f1_H <- percent(F1_fixtures$f1_H, accuracy = 0.1)
#
# #Draw
# F1_fixtures$f1_D <- (
#
#   F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 + F1_fixtures$f1_4_4 +
#     F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6
# )
#
# F1_fixtures$f1_D <- percent(F1_fixtures$f1_D, accuracy = 0.1)
#
# #Away
#
# F1_fixtures$f1_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 + F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6
# )
#
# F1_fixtures$f1_A <- percent(F1_fixtures$f1_A, accuracy = 0.1)
#
# #ov25
# F1_fixtures$f1_ov25 <- (
#   F1_fixtures$f1_2_1 + F1_fixtures$f1_1_2 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 + F1_fixtures$f1_2_3 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 + F1_fixtures$f1_0_4 +
#     F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 + F1_fixtures$f1_4_4 + F1_fixtures$f1_5_0 +
#     F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 + F1_fixtures$f1_0_5 +
#     F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 + F1_fixtures$f1_5_5 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5 + F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 +
#     F1_fixtures$f1_4_6 + F1_fixtures$f1_5_6 + F1_fixtures$f1_6_6
# )
# #un25
# F1_fixtures$f1_un25 <- (
#   F1_fixtures$f1_0_0 + F1_fixtures$f1_1_0 + F1_fixtures$f1_0_1 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_0 + F1_fixtures$f1_0_2
# )
# #odds
# F1_fixtures$f1_ov25_odds <- round((1/F1_fixtures$f1_ov25),digits = 2)
# F1_fixtures$f1_un25_odds <- round((1/F1_fixtures$f1_un25),digits = 2)
#
# F1_fixtures$f1_ov25_odds
# F1_fixtures$f1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# F1_fixtures$f1_BTTSY <- (
#   F1_fixtures$f1_1_1 + F1_fixtures$f1_2_1 + F1_fixtures$f1_1_2 + F1_fixtures$f1_3_1 + F1_fixtures$f1_3_2 +
#     F1_fixtures$f1_2_2 + F1_fixtures$f1_1_3 + F1_fixtures$f1_2_3 + F1_fixtures$f1_3_3 + F1_fixtures$f1_4_4 +
#     F1_fixtures$f1_4_1 + F1_fixtures$f1_4_3 + F1_fixtures$f1_4_2 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 +
#     F1_fixtures$f1_3_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 +
#     F1_fixtures$f1_5_4 + F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_6_6 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6
# )
# #BTTSN
# F1_fixtures$f1_BTTSN <- (
#   F1_fixtures$f1_0_0 + F1_fixtures$f1_1_0 + F1_fixtures$f1_0_1 + F1_fixtures$f1_2_0 + F1_fixtures$f1_0_2 +
#     F1_fixtures$f1_3_0 + F1_fixtures$f1_0_3 + F1_fixtures$f1_4_0 + F1_fixtures$f1_0_4 + F1_fixtures$f1_5_0 +
#     F1_fixtures$f1_0_5 + F1_fixtures$f1_6_0 + F1_fixtures$f1_0_6
# )
#
# F1_fixtures$f1_BTTSY_odds <- round((1/F1_fixtures$f1_BTTSY),digits = 2)
# F1_fixtures$f1_BTTSN_odds <- round((1/F1_fixtures$f1_BTTSN),digits = 2)
#
# F1_fixtures$f1_BTTSY <- percent(F1_fixtures$f1_BTTSY, accuracy = 0.1)
# F1_fixtures$f1_BTTSN <- percent(F1_fixtures$f1_BTTSN, accuracy = 0.1)
# #odds
# F1_fixtures$f1_BTTSY_odds
# F1_fixtures$f1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# F1_fixtures$f1_AH_0_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 +F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6
# )
# #AH_0_A
# F1_fixtures$f1_AH_0_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 +F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6
# )
#
# #odds
# F1_fixtures$f1_AH_0_H_odds <- round((1/F1_fixtures$f1_AH_0_H),digits = 2)
# F1_fixtures$f1_AH_0_A_odds <- round((1/F1_fixtures$f1_AH_0_A),digits = 2)
#
# F1_fixtures$f1_AH_0_H_odds
# F1_fixtures$f1_AH_0_A_odds
# #percentages
# F1_fixtures$f1_AH_0_H <- percent(F1_fixtures$f1_AH_0_H, accuracy = 0.1)
# F1_fixtures$f1_AH_0_A <- percent(F1_fixtures$f1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# F1_fixtures$f1_AH_n075_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 +F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5
# )
# #AH_n075_A
# F1_fixtures$f1_AH_n075_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 +F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6
# )
#
# #odds
# F1_fixtures$f1_AH_n075_H_odds <- round((1/F1_fixtures$f1_AH_n075_H),digits = 2)
# F1_fixtures$f1_AH_n075_A_odds <- round((1/F1_fixtures$f1_AH_n075_A),digits = 2)
#
# F1_fixtures$f1_AH_n075_H_odds
# F1_fixtures$f1_AH_n075_A_odds
# #percentages
# F1_fixtures$f1_AH_n075_H <- percent(F1_fixtures$f1_AH_n075_H, accuracy = 0.1)
# F1_fixtures$f1_AH_n075_A <- percent(F1_fixtures$f1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# F1_fixtures$f1_AH_075_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 +F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6 + F1_fixtures$f1_0_1 + F1_fixtures$f1_1_2 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_3_4 + F1_fixtures$f1_4_5 + F1_fixtures$f1_5_6
# )
# #AH_075_A
# F1_fixtures$f1_AH_075_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 +F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6 + F1_fixtures$f1_1_0 + F1_fixtures$f1_2_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_3 + F1_fixtures$f1_5_4 + F1_fixtures$f1_6_5
# )
#
# #odds
# F1_fixtures$f1_AH_075_H_odds <- round((1/F1_fixtures$f1_AH_075_H),digits = 2)
# F1_fixtures$f1_AH_075_A_odds <- round((1/F1_fixtures$f1_AH_075_A),digits = 2)
#
# F1_fixtures$f1_AH_075_H_odds
# F1_fixtures$f1_AH_075_A_odds
# #percentages
# F1_fixtures$f1_AH_075_H <- percent(F1_fixtures$f1_AH_075_H, accuracy = 0.1)
# F1_fixtures$f1_AH_075_A <- percent(F1_fixtures$f1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# F1_fixtures$f1_AH_n125_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 +F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5
# )
# #AH_n125_A
# F1_fixtures$f1_AH_n125_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 +F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6
# )
#
# #odds
# F1_fixtures$f1_AH_n125_H_odds <- round((1/F1_fixtures$f1_AH_n125_H),digits = 2)
# F1_fixtures$f1_AH_n125_A_odds <- round((1/F1_fixtures$f1_AH_n125_A),digits = 2)
#
# F1_fixtures$f1_AH_n125_H_odds
# F1_fixtures$f1_AH_n125_A_odds
# #percentages
# F1_fixtures$f1_AH_n125_H <- percent(F1_fixtures$f1_AH_n125_H, accuracy = 0.1)
# F1_fixtures$f1_AH_n125_A <- percent(F1_fixtures$f1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# F1_fixtures$f1_AH_125_H <- (
#   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#     F1_fixtures$f1_5_0 +F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#     F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#     F1_fixtures$f1_6_5 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6 + F1_fixtures$f1_0_1 + F1_fixtures$f1_1_2 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_3_4 + F1_fixtures$f1_4_5 + F1_fixtures$f1_5_6
# )
# #AH_125_A
# F1_fixtures$f1_AH_125_A <- (
#   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#     F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#     F1_fixtures$f1_0_5 +F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#     F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#     F1_fixtures$f1_5_6 + F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 +
#     F1_fixtures$f1_4_4 + F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6 + F1_fixtures$f1_1_0 + F1_fixtures$f1_2_1 +
#     F1_fixtures$f1_3_2 + F1_fixtures$f1_4_3 + F1_fixtures$f1_5_4 + F1_fixtures$f1_6_5
# )
#
# #odds
# F1_fixtures$f1_AH_125_H_odds <- round((1/F1_fixtures$f1_AH_125_H),digits = 2)
# F1_fixtures$f1_AH_125_A_odds <- round((1/F1_fixtures$f1_AH_125_A),digits = 2)
#
# F1_fixtures$f1_AH_125_H_odds
# F1_fixtures$f1_AH_125_A_odds
# #percentages
# F1_fixtures$f1_AH_125_H <- percent(F1_fixtures$f1_AH_125_H, accuracy = 0.1)
# F1_fixtures$f1_AH_125_A <- percent(F1_fixtures$f1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# F1_fixtures$f1_ov25 <- percent(F1_fixtures$f1_ov25, accuracy = 0.1)
#
# F1_fixtures$f1_un25 <- percent(F1_fixtures$f1_un25, accuracy = 0.1)
# F1_fixtures$f1_pscore <- paste(round(F1_fixtures$f1_xGH,digits = 0),round(F1_fixtures$f1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(F1_fixtures,'Divisions/F1.xlsx',sheetName = "F1", append = TRUE)
# ########################################################################################################################
# #F2
# HomeTeam_f2 <- rep(f2_teams, each = length(f2_teams))
# AwayTeam_f2 <- rep(f2_teams, length(f2_teams))
# F2_fixtures <- cbind(HomeTeam_f2,AwayTeam_f2)
# F2_fixtures <- as.data.frame(F2_fixtures)
# F2_fixtures <- F2_fixtures[!F2_fixtures$HomeTeam_f2 == F2_fixtures$AwayTeam_f2,]
# rownames(F2_fixtures) <- NULL
# F2_fixtures$Div <- "F2"
# F2_fixtures <- F2_fixtures[,c(3,1,2)]
#
# F2_fixtures$avg_HG_f2 <- f2_avg_HG
#
# F2_fixtures$f2_homeas <- rep(f2_home_as,each = length(f2_teams)-1)
#
# f2_awayds_lookup <- cbind(f2_teams,f2_away_ds)
#
# f2_awayds_lookup <- as.data.frame(f2_awayds_lookup)
#
# colnames(f2_awayds_lookup) <- c("AwayTeam_f2","f2_awayds")
#
#
# require('RH2')
# F2_fixtures$f2_awayds <- sqldf("SELECT f2_awayds_lookup.f2_awayds FROM f2_awayds_lookup INNER JOIN F2_fixtures ON f2_awayds_lookup.AwayTeam_f2 = F2_fixtures.AwayTeam_f2")
#
# F2_fixtures$avg_AG_f2 <- f2_avg_AG
#
# f2_awayas_lookup <- cbind(f2_teams,f2_away_as)
#
# f2_awayas_lookup <- as.data.frame(f2_awayas_lookup)
#
# colnames(f2_awayas_lookup) <- c("AwayTeam_f2","f2_awayas")
#
#
# F2_fixtures$f2_awayas <- sqldf("SELECT f2_awayas_lookup.f2_awayas FROM f2_awayas_lookup INNER JOIN F2_fixtures ON f2_awayas_lookup.AwayTeam_f2 = F2_fixtures.AwayTeam_f2")
#
# F2_fixtures$f2_homeds <- rep(f2_home_ds,each = length(f2_teams)-1)
#
# F2_fixtures$f2_awayds <- as.numeric(unlist(F2_fixtures$f2_awayds))
# #xGH
# F2_fixtures$f2_xGH <- F2_fixtures$avg_HG_f2 * F2_fixtures$f2_homeas * F2_fixtures$f2_awayds
#
# #xGA
#
# F2_fixtures$f2_awayas <- as.numeric(unlist(F2_fixtures$f2_awayas))
#
# F2_fixtures$f2_xGA <- F2_fixtures$avg_AG_f2 * F2_fixtures$f2_awayas * F2_fixtures$f2_homeds
#
# F2_fixtures$f2_0_0 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_0 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_1 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_1 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_0 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_2 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_2 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_1 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_2 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_3 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_0 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_1 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_2 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_3 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_3 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_3 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_4 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_0 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_1 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_2 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_3 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_4 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_4 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_4 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_4 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_5 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_0 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_1 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_2 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_3 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_4 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_5 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_5 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_5 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_5 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_5 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_6 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_0 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(0,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_1 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(1,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_2 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(2,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_3 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(3,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_4 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(4,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_6_5 <- round(stats::dpois(6,F2_fixtures$f2_xGH) * stats::dpois(5,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_0_6 <- round(stats::dpois(0,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_1_6 <- round(stats::dpois(1,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_2_6 <- round(stats::dpois(2,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_3_6 <- round(stats::dpois(3,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_4_6 <- round(stats::dpois(4,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# F2_fixtures$f2_5_6 <- round(stats::dpois(5,F2_fixtures$f2_xGH) * stats::dpois(6,F2_fixtures$f2_xGA), digits = 4)
# #Home win
# F2_fixtures$f2_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 + F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5
# )
#
# F2_fixtures$f2_H <- percent(F2_fixtures$f2_H, accuracy = 0.1)
#
# #Draw
# F2_fixtures$f2_D <- (
#
#   F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 + F2_fixtures$f2_4_4 +
#     F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6
# )
#
# F2_fixtures$f2_D <- percent(F2_fixtures$f2_D, accuracy = 0.1)
#
# #Away
#
# F2_fixtures$f2_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 + F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6
# )
#
# F2_fixtures$f2_A <- percent(F2_fixtures$f2_A, accuracy = 0.1)
#
# #ov25
# F2_fixtures$f2_ov25 <- (
#   F2_fixtures$f2_2_1 + F2_fixtures$f2_1_2 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 + F2_fixtures$f2_2_3 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 + F2_fixtures$f2_0_4 +
#     F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 + F2_fixtures$f2_4_4 + F2_fixtures$f2_5_0 +
#     F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 + F2_fixtures$f2_0_5 +
#     F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 + F2_fixtures$f2_5_5 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5 + F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 +
#     F2_fixtures$f2_4_6 + F2_fixtures$f2_5_6 + F2_fixtures$f2_6_6
# )
# #un25
# F2_fixtures$f2_un25 <- (
#   F2_fixtures$f2_0_0 + F2_fixtures$f2_1_0 + F2_fixtures$f2_0_1 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_0 + F2_fixtures$f2_0_2
# )
# #odds
# F2_fixtures$f2_ov25_odds <- round((1/F2_fixtures$f2_ov25),digits = 2)
# F2_fixtures$f2_un25_odds <- round((1/F2_fixtures$f2_un25),digits = 2)
#
# F2_fixtures$f2_ov25_odds
# F2_fixtures$f2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# F2_fixtures$f2_BTTSY <- (
#   F2_fixtures$f2_1_1 + F2_fixtures$f2_2_1 + F2_fixtures$f2_1_2 + F2_fixtures$f2_3_1 + F2_fixtures$f2_3_2 +
#     F2_fixtures$f2_2_2 + F2_fixtures$f2_1_3 + F2_fixtures$f2_2_3 + F2_fixtures$f2_3_3 + F2_fixtures$f2_4_4 +
#     F2_fixtures$f2_4_1 + F2_fixtures$f2_4_3 + F2_fixtures$f2_4_2 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 +
#     F2_fixtures$f2_3_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 +
#     F2_fixtures$f2_5_4 + F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_6_6 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6
# )
# #BTTSN
# F2_fixtures$f2_BTTSN <- (
#   F2_fixtures$f2_0_0 + F2_fixtures$f2_1_0 + F2_fixtures$f2_0_1 + F2_fixtures$f2_2_0 + F2_fixtures$f2_0_2 +
#     F2_fixtures$f2_3_0 + F2_fixtures$f2_0_3 + F2_fixtures$f2_4_0 + F2_fixtures$f2_0_4 + F2_fixtures$f2_5_0 +
#     F2_fixtures$f2_0_5 + F2_fixtures$f2_6_0 + F2_fixtures$f2_0_6
# )
#
# F2_fixtures$f2_BTTSY_odds <- round((1/F2_fixtures$f2_BTTSY),digits = 2)
# F2_fixtures$f2_BTTSN_odds <- round((1/F2_fixtures$f2_BTTSN),digits = 2)
#
# F2_fixtures$f2_BTTSY <- percent(F2_fixtures$f2_BTTSY, accuracy = 0.1)
# F2_fixtures$f2_BTTSN <- percent(F2_fixtures$f2_BTTSN, accuracy = 0.1)
# #odds
# F2_fixtures$f2_BTTSY_odds
# F2_fixtures$f2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# F2_fixtures$f2_AH_0_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 +F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6
# )
# #AH_0_A
# F2_fixtures$f2_AH_0_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 +F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6
# )
#
# #odds
# F2_fixtures$f2_AH_0_H_odds <- round((1/F2_fixtures$f2_AH_0_H),digits = 2)
# F2_fixtures$f2_AH_0_A_odds <- round((1/F2_fixtures$f2_AH_0_A),digits = 2)
#
# F2_fixtures$f2_AH_0_H_odds
# F2_fixtures$f2_AH_0_A_odds
# #percentages
# F2_fixtures$f2_AH_0_H <- percent(F2_fixtures$f2_AH_0_H, accuracy = 0.1)
# F2_fixtures$f2_AH_0_A <- percent(F2_fixtures$f2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# F2_fixtures$f2_AH_n075_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 +F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5
# )
# #AH_n075_A
# F2_fixtures$f2_AH_n075_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 +F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6
# )
#
# #odds
# F2_fixtures$f2_AH_n075_H_odds <- round((1/F2_fixtures$f2_AH_n075_H),digits = 2)
# F2_fixtures$f2_AH_n075_A_odds <- round((1/F2_fixtures$f2_AH_n075_A),digits = 2)
#
# F2_fixtures$f2_AH_n075_H_odds
# F2_fixtures$f2_AH_n075_A_odds
# #percentages
# F2_fixtures$f2_AH_n075_H <- percent(F2_fixtures$f2_AH_n075_H, accuracy = 0.1)
# F2_fixtures$f2_AH_n075_A <- percent(F2_fixtures$f2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# F2_fixtures$f2_AH_075_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 +F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6 + F2_fixtures$f2_0_1 + F2_fixtures$f2_1_2 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_3_4 + F2_fixtures$f2_4_5 + F2_fixtures$f2_5_6
# )
# #AH_075_A
# F2_fixtures$f2_AH_075_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 +F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6 + F2_fixtures$f2_1_0 + F2_fixtures$f2_2_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_3 + F2_fixtures$f2_5_4 + F2_fixtures$f2_6_5
# )
#
# #odds
# F2_fixtures$f2_AH_075_H_odds <- round((1/F2_fixtures$f2_AH_075_H),digits = 2)
# F2_fixtures$f2_AH_075_A_odds <- round((1/F2_fixtures$f2_AH_075_A),digits = 2)
#
# F2_fixtures$f2_AH_075_H_odds
# F2_fixtures$f2_AH_075_A_odds
# #percentages
# F2_fixtures$f2_AH_075_H <- percent(F2_fixtures$f2_AH_075_H, accuracy = 0.1)
# F2_fixtures$f2_AH_075_A <- percent(F2_fixtures$f2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# F2_fixtures$f2_AH_n125_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 +F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5
# )
# #AH_n125_A
# F2_fixtures$f2_AH_n125_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 +F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6
# )
#
# #odds
# F2_fixtures$f2_AH_n125_H_odds <- round((1/F2_fixtures$f2_AH_n125_H),digits = 2)
# F2_fixtures$f2_AH_n125_A_odds <- round((1/F2_fixtures$f2_AH_n125_A),digits = 2)
#
# F2_fixtures$f2_AH_n125_H_odds
# F2_fixtures$f2_AH_n125_A_odds
# #percentages
# F2_fixtures$f2_AH_n125_H <- percent(F2_fixtures$f2_AH_n125_H, accuracy = 0.1)
# F2_fixtures$f2_AH_n125_A <- percent(F2_fixtures$f2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# F2_fixtures$f2_AH_125_H <- (
#   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#     F2_fixtures$f2_5_0 +F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#     F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#     F2_fixtures$f2_6_5 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6 + F2_fixtures$f2_0_1 + F2_fixtures$f2_1_2 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_3_4 + F2_fixtures$f2_4_5 + F2_fixtures$f2_5_6
# )
# #AH_125_A
# F2_fixtures$f2_AH_125_A <- (
#   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#     F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#     F2_fixtures$f2_0_5 +F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#     F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#     F2_fixtures$f2_5_6 + F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 +
#     F2_fixtures$f2_4_4 + F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6 + F2_fixtures$f2_1_0 + F2_fixtures$f2_2_1 +
#     F2_fixtures$f2_3_2 + F2_fixtures$f2_4_3 + F2_fixtures$f2_5_4 + F2_fixtures$f2_6_5
# )
#
# #odds
# F2_fixtures$f2_AH_125_H_odds <- round((1/F2_fixtures$f2_AH_125_H),digits = 2)
# F2_fixtures$f2_AH_125_A_odds <- round((1/F2_fixtures$f2_AH_125_A),digits = 2)
#
# F2_fixtures$f2_AH_125_H_odds
# F2_fixtures$f2_AH_125_A_odds
# #percentages
# F2_fixtures$f2_AH_125_H <- percent(F2_fixtures$f2_AH_125_H, accuracy = 0.1)
# F2_fixtures$f2_AH_125_A <- percent(F2_fixtures$f2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# F2_fixtures$f2_ov25 <- percent(F2_fixtures$f2_ov25, accuracy = 0.1)
#
# F2_fixtures$f2_un25 <- percent(F2_fixtures$f2_un25, accuracy = 0.1)
# F2_fixtures$f2_pscore <- paste(round(F2_fixtures$f2_xGH,digits = 0),round(F2_fixtures$f2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(F2_fixtures,'Divisions/F2.xlsx',sheetName = "F2", append = TRUE)
# ############################################################################################################################
# #G1
# HomeTeam_g1 <- rep(g1_teams, each = length(g1_teams))
# AwayTeam_g1 <- rep(g1_teams, length(g1_teams))
# G1_fixtures <- cbind(HomeTeam_g1,AwayTeam_g1)
# G1_fixtures <- as.data.frame(G1_fixtures)
# G1_fixtures <- G1_fixtures[!G1_fixtures$HomeTeam_g1 == G1_fixtures$AwayTeam_g1,]
# rownames(G1_fixtures) <- NULL
# G1_fixtures$Div <- "G1"
# G1_fixtures <- G1_fixtures[,c(3,1,2)]
#
# G1_fixtures$avg_HG_g1 <- g1_avg_HG
#
# G1_fixtures$g1_homeas <- rep(g1_home_as,each = length(g1_teams)-1)
#
# g1_awayds_lookup <- cbind(g1_teams,g1_away_ds)
#
# g1_awayds_lookup <- as.data.frame(g1_awayds_lookup)
#
# colnames(g1_awayds_lookup) <- c("AwayTeam_g1","g1_awayds")
#
#
# require('RH2')
# G1_fixtures$g1_awayds <- sqldf("SELECT g1_awayds_lookup.g1_awayds FROM g1_awayds_lookup INNER JOIN G1_fixtures ON g1_awayds_lookup.AwayTeam_g1 = G1_fixtures.AwayTeam_g1")
#
# G1_fixtures$avg_AG_g1 <- g1_avg_AG
#
# g1_awayas_lookup <- cbind(g1_teams,g1_away_as)
#
# g1_awayas_lookup <- as.data.frame(g1_awayas_lookup)
#
# colnames(g1_awayas_lookup) <- c("AwayTeam_g1","g1_awayas")
#
#
# G1_fixtures$g1_awayas <- sqldf("SELECT g1_awayas_lookup.g1_awayas FROM g1_awayas_lookup INNER JOIN G1_fixtures ON g1_awayas_lookup.AwayTeam_g1 = G1_fixtures.AwayTeam_g1")
#
# G1_fixtures$g1_homeds <- rep(g1_home_ds,each = length(g1_teams)-1)
#
# G1_fixtures$g1_awayds <- as.numeric(unlist(G1_fixtures$g1_awayds))
# #xGH
# G1_fixtures$g1_xGH <- G1_fixtures$avg_HG_g1 * G1_fixtures$g1_homeas * G1_fixtures$g1_awayds
#
# #xGA
#
# G1_fixtures$g1_awayas <- as.numeric(unlist(G1_fixtures$g1_awayas))
#
# G1_fixtures$g1_xGA <- G1_fixtures$avg_AG_g1 * G1_fixtures$g1_awayas * G1_fixtures$g1_homeds
#
# G1_fixtures$g1_0_0 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_0 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_1 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_1 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_0 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_2 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_2 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_1 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_2 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_3 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_0 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_1 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_2 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_3 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_3 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_3 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_4 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_0 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_1 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_2 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_3 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_4 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_4 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_4 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_4 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_5 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_0 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_1 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_2 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_3 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_4 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_5 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_5 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_5 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_5 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_5 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_6 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_0 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(0,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_1 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(1,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_2 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(2,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_3 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(3,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_4 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(4,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_6_5 <- round(stats::dpois(6,G1_fixtures$g1_xGH) * stats::dpois(5,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_0_6 <- round(stats::dpois(0,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_1_6 <- round(stats::dpois(1,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_2_6 <- round(stats::dpois(2,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_3_6 <- round(stats::dpois(3,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_4_6 <- round(stats::dpois(4,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# G1_fixtures$g1_5_6 <- round(stats::dpois(5,G1_fixtures$g1_xGH) * stats::dpois(6,G1_fixtures$g1_xGA), digits = 4)
# #Home win
# G1_fixtures$g1_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 + G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5
# )
#
# G1_fixtures$g1_H <- percent(G1_fixtures$g1_H, accuracy = 0.1)
#
# #Draw
# G1_fixtures$g1_D <- (
#
#   G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 + G1_fixtures$g1_4_4 +
#     G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6
# )
#
# G1_fixtures$g1_D <- percent(G1_fixtures$g1_D, accuracy = 0.1)
#
# #Away
#
# G1_fixtures$g1_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 + G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6
# )
#
# G1_fixtures$g1_A <- percent(G1_fixtures$g1_A, accuracy = 0.1)
#
# #ov25
# G1_fixtures$g1_ov25 <- (
#   G1_fixtures$g1_2_1 + G1_fixtures$g1_1_2 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 + G1_fixtures$g1_2_3 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 + G1_fixtures$g1_0_4 +
#     G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 + G1_fixtures$g1_4_4 + G1_fixtures$g1_5_0 +
#     G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 + G1_fixtures$g1_0_5 +
#     G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 + G1_fixtures$g1_5_5 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5 + G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 +
#     G1_fixtures$g1_4_6 + G1_fixtures$g1_5_6 + G1_fixtures$g1_6_6
# )
# #un25
# G1_fixtures$g1_un25 <- (
#   G1_fixtures$g1_0_0 + G1_fixtures$g1_1_0 + G1_fixtures$g1_0_1 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_0 + G1_fixtures$g1_0_2
# )
# #odds
# G1_fixtures$g1_ov25_odds <- round((1/G1_fixtures$g1_ov25),digits = 2)
# G1_fixtures$g1_un25_odds <- round((1/G1_fixtures$g1_un25),digits = 2)
#
# G1_fixtures$g1_ov25_odds
# G1_fixtures$g1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# G1_fixtures$g1_BTTSY <- (
#   G1_fixtures$g1_1_1 + G1_fixtures$g1_2_1 + G1_fixtures$g1_1_2 + G1_fixtures$g1_3_1 + G1_fixtures$g1_3_2 +
#     G1_fixtures$g1_2_2 + G1_fixtures$g1_1_3 + G1_fixtures$g1_2_3 + G1_fixtures$g1_3_3 + G1_fixtures$g1_4_4 +
#     G1_fixtures$g1_4_1 + G1_fixtures$g1_4_3 + G1_fixtures$g1_4_2 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 +
#     G1_fixtures$g1_3_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 +
#     G1_fixtures$g1_5_4 + G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_6_6 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6
# )
# #BTTSN
# G1_fixtures$g1_BTTSN <- (
#   G1_fixtures$g1_0_0 + G1_fixtures$g1_1_0 + G1_fixtures$g1_0_1 + G1_fixtures$g1_2_0 + G1_fixtures$g1_0_2 +
#     G1_fixtures$g1_3_0 + G1_fixtures$g1_0_3 + G1_fixtures$g1_4_0 + G1_fixtures$g1_0_4 + G1_fixtures$g1_5_0 +
#     G1_fixtures$g1_0_5 + G1_fixtures$g1_6_0 + G1_fixtures$g1_0_6
# )
#
# G1_fixtures$g1_BTTSY_odds <- round((1/G1_fixtures$g1_BTTSY),digits = 2)
# G1_fixtures$g1_BTTSN_odds <- round((1/G1_fixtures$g1_BTTSN),digits = 2)
#
# G1_fixtures$g1_BTTSY <- percent(G1_fixtures$g1_BTTSY, accuracy = 0.1)
# G1_fixtures$g1_BTTSN <- percent(G1_fixtures$g1_BTTSN, accuracy = 0.1)
# #odds
# G1_fixtures$g1_BTTSY_odds
# G1_fixtures$g1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# G1_fixtures$g1_AH_0_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 +G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6
# )
# #AH_0_A
# G1_fixtures$g1_AH_0_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 +G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6
# )
#
# #odds
# G1_fixtures$g1_AH_0_H_odds <- round((1/G1_fixtures$g1_AH_0_H),digits = 2)
# G1_fixtures$g1_AH_0_A_odds <- round((1/G1_fixtures$g1_AH_0_A),digits = 2)
#
# G1_fixtures$g1_AH_0_H_odds
# G1_fixtures$g1_AH_0_A_odds
# #percentages
# G1_fixtures$g1_AH_0_H <- percent(G1_fixtures$g1_AH_0_H, accuracy = 0.1)
# G1_fixtures$g1_AH_0_A <- percent(G1_fixtures$g1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# G1_fixtures$g1_AH_n075_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 +G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5
# )
# #AH_n075_A
# G1_fixtures$g1_AH_n075_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 +G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6
# )
#
# #odds
# G1_fixtures$g1_AH_n075_H_odds <- round((1/G1_fixtures$g1_AH_n075_H),digits = 2)
# G1_fixtures$g1_AH_n075_A_odds <- round((1/G1_fixtures$g1_AH_n075_A),digits = 2)
#
# G1_fixtures$g1_AH_n075_H_odds
# G1_fixtures$g1_AH_n075_A_odds
# #percentages
# G1_fixtures$g1_AH_n075_H <- percent(G1_fixtures$g1_AH_n075_H, accuracy = 0.1)
# G1_fixtures$g1_AH_n075_A <- percent(G1_fixtures$g1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# G1_fixtures$g1_AH_075_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 +G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6 + G1_fixtures$g1_0_1 + G1_fixtures$g1_1_2 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_3_4 + G1_fixtures$g1_4_5 + G1_fixtures$g1_5_6
# )
# #AH_075_A
# G1_fixtures$g1_AH_075_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 +G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6 + G1_fixtures$g1_1_0 + G1_fixtures$g1_2_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_3 + G1_fixtures$g1_5_4 + G1_fixtures$g1_6_5
# )
#
# #odds
# G1_fixtures$g1_AH_075_H_odds <- round((1/G1_fixtures$g1_AH_075_H),digits = 2)
# G1_fixtures$g1_AH_075_A_odds <- round((1/G1_fixtures$g1_AH_075_A),digits = 2)
#
# G1_fixtures$g1_AH_075_H_odds
# G1_fixtures$g1_AH_075_A_odds
# #percentages
# G1_fixtures$g1_AH_075_H <- percent(G1_fixtures$g1_AH_075_H, accuracy = 0.1)
# G1_fixtures$g1_AH_075_A <- percent(G1_fixtures$g1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# G1_fixtures$g1_AH_n125_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 +G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5
# )
# #AH_n125_A
# G1_fixtures$g1_AH_n125_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 +G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6
# )
#
# #odds
# G1_fixtures$g1_AH_n125_H_odds <- round((1/G1_fixtures$g1_AH_n125_H),digits = 2)
# G1_fixtures$g1_AH_n125_A_odds <- round((1/G1_fixtures$g1_AH_n125_A),digits = 2)
#
# G1_fixtures$g1_AH_n125_H_odds
# G1_fixtures$g1_AH_n125_A_odds
# #percentages
# G1_fixtures$g1_AH_n125_H <- percent(G1_fixtures$g1_AH_n125_H, accuracy = 0.1)
# G1_fixtures$g1_AH_n125_A <- percent(G1_fixtures$g1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# G1_fixtures$g1_AH_125_H <- (
#   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#     G1_fixtures$g1_5_0 +G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#     G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#     G1_fixtures$g1_6_5 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6 + G1_fixtures$g1_0_1 + G1_fixtures$g1_1_2 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_3_4 + G1_fixtures$g1_4_5 + G1_fixtures$g1_5_6
# )
# #AH_125_A
# G1_fixtures$g1_AH_125_A <- (
#   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#     G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#     G1_fixtures$g1_0_5 +G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#     G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#     G1_fixtures$g1_5_6 + G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 +
#     G1_fixtures$g1_4_4 + G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6 + G1_fixtures$g1_1_0 + G1_fixtures$g1_2_1 +
#     G1_fixtures$g1_3_2 + G1_fixtures$g1_4_3 + G1_fixtures$g1_5_4 + G1_fixtures$g1_6_5
# )
#
# #odds
# G1_fixtures$g1_AH_125_H_odds <- round((1/G1_fixtures$g1_AH_125_H),digits = 2)
# G1_fixtures$g1_AH_125_A_odds <- round((1/G1_fixtures$g1_AH_125_A),digits = 2)
#
# G1_fixtures$g1_AH_125_H_odds
# G1_fixtures$g1_AH_125_A_odds
# #percentages
# G1_fixtures$g1_AH_125_H <- percent(G1_fixtures$g1_AH_125_H, accuracy = 0.1)
# G1_fixtures$g1_AH_125_A <- percent(G1_fixtures$g1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# G1_fixtures$g1_ov25 <- percent(G1_fixtures$g1_ov25, accuracy = 0.1)
#
# G1_fixtures$g1_un25 <- percent(G1_fixtures$g1_un25, accuracy = 0.1)
# G1_fixtures$g1_pscore <- paste(round(G1_fixtures$g1_xGH,digits = 0),round(G1_fixtures$g1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(G1_fixtures,'Divisions/G1.xlsx',sheetName = "G1", append = TRUE)
# ###############################################################################################################
# #I1
# HomeTeam_i1 <- rep(i1_teams, each = length(i1_teams))
# AwayTeam_i1 <- rep(i1_teams, length(i1_teams))
# I1_fixtures <- cbind(HomeTeam_i1,AwayTeam_i1)
# I1_fixtures <- as.data.frame(I1_fixtures)
# I1_fixtures <- I1_fixtures[!I1_fixtures$HomeTeam_i1 == I1_fixtures$AwayTeam_i1,]
# rownames(I1_fixtures) <- NULL
# I1_fixtures$Div <- "I1"
# I1_fixtures <- I1_fixtures[,c(3,1,2)]
#
# I1_fixtures$avg_HG_i1 <- i1_avg_HG
#
# I1_fixtures$i1_homeas <- rep(i1_home_as,each = length(i1_teams)-1)
#
# i1_awayds_lookup <- cbind(i1_teams,i1_away_ds)
#
# i1_awayds_lookup <- as.data.frame(i1_awayds_lookup)
#
# colnames(i1_awayds_lookup) <- c("AwayTeam_i1","i1_awayds")
#
#
# require('RH2')
# I1_fixtures$i1_awayds <- sqldf("SELECT i1_awayds_lookup.i1_awayds FROM i1_awayds_lookup INNER JOIN I1_fixtures ON i1_awayds_lookup.AwayTeam_i1 = I1_fixtures.AwayTeam_i1")
#
# I1_fixtures$avg_AG_i1 <- i1_avg_AG
#
# i1_awayas_lookup <- cbind(i1_teams,i1_away_as)
#
# i1_awayas_lookup <- as.data.frame(i1_awayas_lookup)
#
# colnames(i1_awayas_lookup) <- c("AwayTeam_i1","i1_awayas")
#
#
# I1_fixtures$i1_awayas <- sqldf("SELECT i1_awayas_lookup.i1_awayas FROM i1_awayas_lookup INNER JOIN I1_fixtures ON i1_awayas_lookup.AwayTeam_i1 = I1_fixtures.AwayTeam_i1")
#
# I1_fixtures$i1_homeds <- rep(i1_home_ds,each = length(i1_teams)-1)
#
# I1_fixtures$i1_awayds <- as.numeric(unlist(I1_fixtures$i1_awayds))
# #xGH
# I1_fixtures$i1_xGH <- I1_fixtures$avg_HG_i1 * I1_fixtures$i1_homeas * I1_fixtures$i1_awayds
#
# #xGA
#
# I1_fixtures$i1_awayas <- as.numeric(unlist(I1_fixtures$i1_awayas))
#
# I1_fixtures$i1_xGA <- I1_fixtures$avg_AG_i1 * I1_fixtures$i1_awayas * I1_fixtures$i1_homeds
#
# I1_fixtures$i1_0_0 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_0 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_1 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_1 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_0 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_2 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_2 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_1 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_2 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_3 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_0 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_1 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_2 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_3 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_3 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_3 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_4 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_0 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_1 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_2 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_3 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_4 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_4 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_4 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_4 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_5 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_0 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_1 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_2 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_3 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_4 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_5 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_5 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_5 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_5 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_5 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_6 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_0 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(0,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_1 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(1,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_2 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(2,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_3 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(3,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_4 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(4,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_6_5 <- round(stats::dpois(6,I1_fixtures$i1_xGH) * stats::dpois(5,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_0_6 <- round(stats::dpois(0,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_1_6 <- round(stats::dpois(1,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_2_6 <- round(stats::dpois(2,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_3_6 <- round(stats::dpois(3,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_4_6 <- round(stats::dpois(4,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# I1_fixtures$i1_5_6 <- round(stats::dpois(5,I1_fixtures$i1_xGH) * stats::dpois(6,I1_fixtures$i1_xGA), digits = 4)
# #Home win
# I1_fixtures$i1_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 + I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5
# )
#
# I1_fixtures$i1_H <- percent(I1_fixtures$i1_H, accuracy = 0.1)
#
# #Draw
# I1_fixtures$i1_D <- (
#
#   I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 + I1_fixtures$i1_4_4 +
#     I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6
# )
#
# I1_fixtures$i1_D <- percent(I1_fixtures$i1_D, accuracy = 0.1)
#
# #Away
#
# I1_fixtures$i1_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 + I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6
# )
#
# I1_fixtures$i1_A <- percent(I1_fixtures$i1_A, accuracy = 0.1)
#
# #ov25
# I1_fixtures$i1_ov25 <- (
#   I1_fixtures$i1_2_1 + I1_fixtures$i1_1_2 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 + I1_fixtures$i1_2_3 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 + I1_fixtures$i1_0_4 +
#     I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 + I1_fixtures$i1_4_4 + I1_fixtures$i1_5_0 +
#     I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 + I1_fixtures$i1_0_5 +
#     I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 + I1_fixtures$i1_5_5 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5 + I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 +
#     I1_fixtures$i1_4_6 + I1_fixtures$i1_5_6 + I1_fixtures$i1_6_6
# )
# #un25
# I1_fixtures$i1_un25 <- (
#   I1_fixtures$i1_0_0 + I1_fixtures$i1_1_0 + I1_fixtures$i1_0_1 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_0 + I1_fixtures$i1_0_2
# )
# #odds
# I1_fixtures$i1_ov25_odds <- round((1/I1_fixtures$i1_ov25),digits = 2)
# I1_fixtures$i1_un25_odds <- round((1/I1_fixtures$i1_un25),digits = 2)
#
# I1_fixtures$i1_ov25_odds
# I1_fixtures$i1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# I1_fixtures$i1_BTTSY <- (
#   I1_fixtures$i1_1_1 + I1_fixtures$i1_2_1 + I1_fixtures$i1_1_2 + I1_fixtures$i1_3_1 + I1_fixtures$i1_3_2 +
#     I1_fixtures$i1_2_2 + I1_fixtures$i1_1_3 + I1_fixtures$i1_2_3 + I1_fixtures$i1_3_3 + I1_fixtures$i1_4_4 +
#     I1_fixtures$i1_4_1 + I1_fixtures$i1_4_3 + I1_fixtures$i1_4_2 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 +
#     I1_fixtures$i1_3_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 +
#     I1_fixtures$i1_5_4 + I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_6_6 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6
# )
# #BTTSN
# I1_fixtures$i1_BTTSN <- (
#   I1_fixtures$i1_0_0 + I1_fixtures$i1_1_0 + I1_fixtures$i1_0_1 + I1_fixtures$i1_2_0 + I1_fixtures$i1_0_2 +
#     I1_fixtures$i1_3_0 + I1_fixtures$i1_0_3 + I1_fixtures$i1_4_0 + I1_fixtures$i1_0_4 + I1_fixtures$i1_5_0 +
#     I1_fixtures$i1_0_5 + I1_fixtures$i1_6_0 + I1_fixtures$i1_0_6
# )
#
# I1_fixtures$i1_BTTSY_odds <- round((1/I1_fixtures$i1_BTTSY),digits = 2)
# I1_fixtures$i1_BTTSN_odds <- round((1/I1_fixtures$i1_BTTSN),digits = 2)
#
# I1_fixtures$i1_BTTSY <- percent(I1_fixtures$i1_BTTSY, accuracy = 0.1)
# I1_fixtures$i1_BTTSN <- percent(I1_fixtures$i1_BTTSN, accuracy = 0.1)
# #odds
# I1_fixtures$i1_BTTSY_odds
# I1_fixtures$i1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# I1_fixtures$i1_AH_0_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 +I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6
# )
# #AH_0_A
# I1_fixtures$i1_AH_0_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 +I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6
# )
#
# #odds
# I1_fixtures$i1_AH_0_H_odds <- round((1/I1_fixtures$i1_AH_0_H),digits = 2)
# I1_fixtures$i1_AH_0_A_odds <- round((1/I1_fixtures$i1_AH_0_A),digits = 2)
#
# I1_fixtures$i1_AH_0_H_odds
# I1_fixtures$i1_AH_0_A_odds
# #percentages
# I1_fixtures$i1_AH_0_H <- percent(I1_fixtures$i1_AH_0_H, accuracy = 0.1)
# I1_fixtures$i1_AH_0_A <- percent(I1_fixtures$i1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# I1_fixtures$i1_AH_n075_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 +I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5
# )
# #AH_n075_A
# I1_fixtures$i1_AH_n075_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 +I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6
# )
#
# #odds
# I1_fixtures$i1_AH_n075_H_odds <- round((1/I1_fixtures$i1_AH_n075_H),digits = 2)
# I1_fixtures$i1_AH_n075_A_odds <- round((1/I1_fixtures$i1_AH_n075_A),digits = 2)
#
# I1_fixtures$i1_AH_n075_H_odds
# I1_fixtures$i1_AH_n075_A_odds
# #percentages
# I1_fixtures$i1_AH_n075_H <- percent(I1_fixtures$i1_AH_n075_H, accuracy = 0.1)
# I1_fixtures$i1_AH_n075_A <- percent(I1_fixtures$i1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# I1_fixtures$i1_AH_075_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 +I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6 + I1_fixtures$i1_0_1 + I1_fixtures$i1_1_2 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_3_4 + I1_fixtures$i1_4_5 + I1_fixtures$i1_5_6
# )
# #AH_075_A
# I1_fixtures$i1_AH_075_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 +I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6 + I1_fixtures$i1_1_0 + I1_fixtures$i1_2_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_3 + I1_fixtures$i1_5_4 + I1_fixtures$i1_6_5
# )
#
# #odds
# I1_fixtures$i1_AH_075_H_odds <- round((1/I1_fixtures$i1_AH_075_H),digits = 2)
# I1_fixtures$i1_AH_075_A_odds <- round((1/I1_fixtures$i1_AH_075_A),digits = 2)
#
# I1_fixtures$i1_AH_075_H_odds
# I1_fixtures$i1_AH_075_A_odds
# #percentages
# I1_fixtures$i1_AH_075_H <- percent(I1_fixtures$i1_AH_075_H, accuracy = 0.1)
# I1_fixtures$i1_AH_075_A <- percent(I1_fixtures$i1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# I1_fixtures$i1_AH_n125_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 +I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5
# )
# #AH_n125_A
# I1_fixtures$i1_AH_n125_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 +I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6
# )
#
# #odds
# I1_fixtures$i1_AH_n125_H_odds <- round((1/I1_fixtures$i1_AH_n125_H),digits = 2)
# I1_fixtures$i1_AH_n125_A_odds <- round((1/I1_fixtures$i1_AH_n125_A),digits = 2)
#
# I1_fixtures$i1_AH_n125_H_odds
# I1_fixtures$i1_AH_n125_A_odds
# #percentages
# I1_fixtures$i1_AH_n125_H <- percent(I1_fixtures$i1_AH_n125_H, accuracy = 0.1)
# I1_fixtures$i1_AH_n125_A <- percent(I1_fixtures$i1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# I1_fixtures$i1_AH_125_H <- (
#   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#     I1_fixtures$i1_5_0 +I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#     I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#     I1_fixtures$i1_6_5 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6 + I1_fixtures$i1_0_1 + I1_fixtures$i1_1_2 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_3_4 + I1_fixtures$i1_4_5 + I1_fixtures$i1_5_6
# )
# #AH_125_A
# I1_fixtures$i1_AH_125_A <- (
#   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#     I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#     I1_fixtures$i1_0_5 +I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#     I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#     I1_fixtures$i1_5_6 + I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 +
#     I1_fixtures$i1_4_4 + I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6 + I1_fixtures$i1_1_0 + I1_fixtures$i1_2_1 +
#     I1_fixtures$i1_3_2 + I1_fixtures$i1_4_3 + I1_fixtures$i1_5_4 + I1_fixtures$i1_6_5
# )
#
# #odds
# I1_fixtures$i1_AH_125_H_odds <- round((1/I1_fixtures$i1_AH_125_H),digits = 2)
# I1_fixtures$i1_AH_125_A_odds <- round((1/I1_fixtures$i1_AH_125_A),digits = 2)
#
# I1_fixtures$i1_AH_125_H_odds
# I1_fixtures$i1_AH_125_A_odds
# #percentages
# I1_fixtures$i1_AH_125_H <- percent(I1_fixtures$i1_AH_125_H, accuracy = 0.1)
# I1_fixtures$i1_AH_125_A <- percent(I1_fixtures$i1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# I1_fixtures$i1_ov25 <- percent(I1_fixtures$i1_ov25, accuracy = 0.1)
#
# I1_fixtures$i1_un25 <- percent(I1_fixtures$i1_un25, accuracy = 0.1)
# I1_fixtures$i1_pscore <- paste(round(I1_fixtures$i1_xGH,digits = 0),round(I1_fixtures$i1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(I1_fixtures,'Divisions/I1.xlsx',sheetName = "I1", append = TRUE)
# #####################################################################################################
# #I2
# HomeTeam_i2 <- rep(i2_teams, each = length(i2_teams))
# AwayTeam_i2 <- rep(i2_teams, length(i2_teams))
# I2_fixtures <- cbind(HomeTeam_i2,AwayTeam_i2)
# I2_fixtures <- as.data.frame(I2_fixtures)
# I2_fixtures <- I2_fixtures[!I2_fixtures$HomeTeam_i2 == I2_fixtures$AwayTeam_i2,]
# rownames(I2_fixtures) <- NULL
# I2_fixtures$Div <- "I2"
# I2_fixtures <- I2_fixtures[,c(3,1,2)]
#
# I2_fixtures$avg_HG_i2 <- i2_avg_HG
#
# I2_fixtures$i2_homeas <- rep(i2_home_as,each = length(i2_teams)-1)
#
# i2_awayds_lookup <- cbind(i2_teams,i2_away_ds)
#
# i2_awayds_lookup <- as.data.frame(i2_awayds_lookup)
#
# colnames(i2_awayds_lookup) <- c("AwayTeam_i2","i2_awayds")
#
#
# require('RH2')
# I2_fixtures$i2_awayds <- sqldf("SELECT i2_awayds_lookup.i2_awayds FROM i2_awayds_lookup INNER JOIN I2_fixtures ON i2_awayds_lookup.AwayTeam_i2 = I2_fixtures.AwayTeam_i2")
#
# I2_fixtures$avg_AG_i2 <- i2_avg_AG
#
# i2_awayas_lookup <- cbind(i2_teams,i2_away_as)
#
# i2_awayas_lookup <- as.data.frame(i2_awayas_lookup)
#
# colnames(i2_awayas_lookup) <- c("AwayTeam_i2","i2_awayas")
#
#
# I2_fixtures$i2_awayas <- sqldf("SELECT i2_awayas_lookup.i2_awayas FROM i2_awayas_lookup INNER JOIN I2_fixtures ON i2_awayas_lookup.AwayTeam_i2 = I2_fixtures.AwayTeam_i2")
#
# I2_fixtures$i2_homeds <- rep(i2_home_ds,each = length(i2_teams)-1)
#
# I2_fixtures$i2_awayds <- as.numeric(unlist(I2_fixtures$i2_awayds))
# #xGH
# I2_fixtures$i2_xGH <- I2_fixtures$avg_HG_i2 * I2_fixtures$i2_homeas * I2_fixtures$i2_awayds
#
# #xGA
#
# I2_fixtures$i2_awayas <- as.numeric(unlist(I2_fixtures$i2_awayas))
#
# I2_fixtures$i2_xGA <- I2_fixtures$avg_AG_i2 * I2_fixtures$i2_awayas * I2_fixtures$i2_homeds
#
# I2_fixtures$i2_0_0 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_0 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_1 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_1 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_0 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_2 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_2 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_1 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_2 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_3 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_0 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_1 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_2 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_3 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_3 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_3 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_4 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_0 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_1 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_2 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_3 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_4 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_4 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_4 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_4 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_5 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_0 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_1 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_2 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_3 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_4 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_5 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_5 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_5 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_5 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_5 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_6 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_0 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(0,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_1 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(1,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_2 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(2,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_3 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(3,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_4 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(4,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_6_5 <- round(stats::dpois(6,I2_fixtures$i2_xGH) * stats::dpois(5,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_0_6 <- round(stats::dpois(0,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_1_6 <- round(stats::dpois(1,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_2_6 <- round(stats::dpois(2,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_3_6 <- round(stats::dpois(3,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_4_6 <- round(stats::dpois(4,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# I2_fixtures$i2_5_6 <- round(stats::dpois(5,I2_fixtures$i2_xGH) * stats::dpois(6,I2_fixtures$i2_xGA), digits = 4)
# #Home win
# I2_fixtures$i2_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 + I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5
# )
#
# I2_fixtures$i2_H <- percent(I2_fixtures$i2_H, accuracy = 0.1)
#
# #Draw
# I2_fixtures$i2_D <- (
#
#   I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 + I2_fixtures$i2_4_4 +
#     I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6
# )
#
# I2_fixtures$i2_D <- percent(I2_fixtures$i2_D, accuracy = 0.1)
#
# #Away
#
# I2_fixtures$i2_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 + I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6
# )
#
# I2_fixtures$i2_A <- percent(I2_fixtures$i2_A, accuracy = 0.1)
#
# #ov25
# I2_fixtures$i2_ov25 <- (
#   I2_fixtures$i2_2_1 + I2_fixtures$i2_1_2 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 + I2_fixtures$i2_2_3 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 + I2_fixtures$i2_0_4 +
#     I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 + I2_fixtures$i2_4_4 + I2_fixtures$i2_5_0 +
#     I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 + I2_fixtures$i2_0_5 +
#     I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 + I2_fixtures$i2_5_5 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5 + I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 +
#     I2_fixtures$i2_4_6 + I2_fixtures$i2_5_6 + I2_fixtures$i2_6_6
# )
# #un25
# I2_fixtures$i2_un25 <- (
#   I2_fixtures$i2_0_0 + I2_fixtures$i2_1_0 + I2_fixtures$i2_0_1 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_0 + I2_fixtures$i2_0_2
# )
# #odds
# I2_fixtures$i2_ov25_odds <- round((1/I2_fixtures$i2_ov25),digits = 2)
# I2_fixtures$i2_un25_odds <- round((1/I2_fixtures$i2_un25),digits = 2)
#
# I2_fixtures$i2_ov25_odds
# I2_fixtures$i2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# I2_fixtures$i2_BTTSY <- (
#   I2_fixtures$i2_1_1 + I2_fixtures$i2_2_1 + I2_fixtures$i2_1_2 + I2_fixtures$i2_3_1 + I2_fixtures$i2_3_2 +
#     I2_fixtures$i2_2_2 + I2_fixtures$i2_1_3 + I2_fixtures$i2_2_3 + I2_fixtures$i2_3_3 + I2_fixtures$i2_4_4 +
#     I2_fixtures$i2_4_1 + I2_fixtures$i2_4_3 + I2_fixtures$i2_4_2 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 +
#     I2_fixtures$i2_3_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 +
#     I2_fixtures$i2_5_4 + I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_6_6 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6
# )
# #BTTSN
# I2_fixtures$i2_BTTSN <- (
#   I2_fixtures$i2_0_0 + I2_fixtures$i2_1_0 + I2_fixtures$i2_0_1 + I2_fixtures$i2_2_0 + I2_fixtures$i2_0_2 +
#     I2_fixtures$i2_3_0 + I2_fixtures$i2_0_3 + I2_fixtures$i2_4_0 + I2_fixtures$i2_0_4 + I2_fixtures$i2_5_0 +
#     I2_fixtures$i2_0_5 + I2_fixtures$i2_6_0 + I2_fixtures$i2_0_6
# )
#
# I2_fixtures$i2_BTTSY_odds <- round((1/I2_fixtures$i2_BTTSY),digits = 2)
# I2_fixtures$i2_BTTSN_odds <- round((1/I2_fixtures$i2_BTTSN),digits = 2)
#
# I2_fixtures$i2_BTTSY <- percent(I2_fixtures$i2_BTTSY, accuracy = 0.1)
# I2_fixtures$i2_BTTSN <- percent(I2_fixtures$i2_BTTSN, accuracy = 0.1)
# #odds
# I2_fixtures$i2_BTTSY_odds
# I2_fixtures$i2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# I2_fixtures$i2_AH_0_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 +I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6
# )
# #AH_0_A
# I2_fixtures$i2_AH_0_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 +I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6
# )
#
# #odds
# I2_fixtures$i2_AH_0_H_odds <- round((1/I2_fixtures$i2_AH_0_H),digits = 2)
# I2_fixtures$i2_AH_0_A_odds <- round((1/I2_fixtures$i2_AH_0_A),digits = 2)
#
# I2_fixtures$i2_AH_0_H_odds
# I2_fixtures$i2_AH_0_A_odds
# #percentages
# I2_fixtures$i2_AH_0_H <- percent(I2_fixtures$i2_AH_0_H, accuracy = 0.1)
# I2_fixtures$i2_AH_0_A <- percent(I2_fixtures$i2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# I2_fixtures$i2_AH_n075_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 +I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5
# )
# #AH_n075_A
# I2_fixtures$i2_AH_n075_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 +I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6
# )
#
# #odds
# I2_fixtures$i2_AH_n075_H_odds <- round((1/I2_fixtures$i2_AH_n075_H),digits = 2)
# I2_fixtures$i2_AH_n075_A_odds <- round((1/I2_fixtures$i2_AH_n075_A),digits = 2)
#
# I2_fixtures$i2_AH_n075_H_odds
# I2_fixtures$i2_AH_n075_A_odds
# #percentages
# I2_fixtures$i2_AH_n075_H <- percent(I2_fixtures$i2_AH_n075_H, accuracy = 0.1)
# I2_fixtures$i2_AH_n075_A <- percent(I2_fixtures$i2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# I2_fixtures$i2_AH_075_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 +I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6 + I2_fixtures$i2_0_1 + I2_fixtures$i2_1_2 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_3_4 + I2_fixtures$i2_4_5 + I2_fixtures$i2_5_6
# )
# #AH_075_A
# I2_fixtures$i2_AH_075_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 +I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6 + I2_fixtures$i2_1_0 + I2_fixtures$i2_2_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_3 + I2_fixtures$i2_5_4 + I2_fixtures$i2_6_5
# )
#
# #odds
# I2_fixtures$i2_AH_075_H_odds <- round((1/I2_fixtures$i2_AH_075_H),digits = 2)
# I2_fixtures$i2_AH_075_A_odds <- round((1/I2_fixtures$i2_AH_075_A),digits = 2)
#
# I2_fixtures$i2_AH_075_H_odds
# I2_fixtures$i2_AH_075_A_odds
# #percentages
# I2_fixtures$i2_AH_075_H <- percent(I2_fixtures$i2_AH_075_H, accuracy = 0.1)
# I2_fixtures$i2_AH_075_A <- percent(I2_fixtures$i2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# I2_fixtures$i2_AH_n125_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 +I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5
# )
# #AH_n125_A
# I2_fixtures$i2_AH_n125_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 +I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6
# )
#
# #odds
# I2_fixtures$i2_AH_n125_H_odds <- round((1/I2_fixtures$i2_AH_n125_H),digits = 2)
# I2_fixtures$i2_AH_n125_A_odds <- round((1/I2_fixtures$i2_AH_n125_A),digits = 2)
#
# I2_fixtures$i2_AH_n125_H_odds
# I2_fixtures$i2_AH_n125_A_odds
# #percentages
# I2_fixtures$i2_AH_n125_H <- percent(I2_fixtures$i2_AH_n125_H, accuracy = 0.1)
# I2_fixtures$i2_AH_n125_A <- percent(I2_fixtures$i2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# I2_fixtures$i2_AH_125_H <- (
#   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#     I2_fixtures$i2_5_0 +I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#     I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#     I2_fixtures$i2_6_5 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6 + I2_fixtures$i2_0_1 + I2_fixtures$i2_1_2 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_3_4 + I2_fixtures$i2_4_5 + I2_fixtures$i2_5_6
# )
# #AH_125_A
# I2_fixtures$i2_AH_125_A <- (
#   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#     I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#     I2_fixtures$i2_0_5 +I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#     I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#     I2_fixtures$i2_5_6 + I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 +
#     I2_fixtures$i2_4_4 + I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6 + I2_fixtures$i2_1_0 + I2_fixtures$i2_2_1 +
#     I2_fixtures$i2_3_2 + I2_fixtures$i2_4_3 + I2_fixtures$i2_5_4 + I2_fixtures$i2_6_5
# )
#
# #odds
# I2_fixtures$i2_AH_125_H_odds <- round((1/I2_fixtures$i2_AH_125_H),digits = 2)
# I2_fixtures$i2_AH_125_A_odds <- round((1/I2_fixtures$i2_AH_125_A),digits = 2)
#
# I2_fixtures$i2_AH_125_H_odds
# I2_fixtures$i2_AH_125_A_odds
# #percentages
# I2_fixtures$i2_AH_125_H <- percent(I2_fixtures$i2_AH_125_H, accuracy = 0.1)
# I2_fixtures$i2_AH_125_A <- percent(I2_fixtures$i2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# I2_fixtures$i2_ov25 <- percent(I2_fixtures$i2_ov25, accuracy = 0.1)
#
# I2_fixtures$i2_un25 <- percent(I2_fixtures$i2_un25, accuracy = 0.1)
# I2_fixtures$i2_pscore <- paste(round(I2_fixtures$i2_xGH,digits = 0),round(I2_fixtures$i2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(I2_fixtures,'Divisions/I2.xlsx',sheetName = "I2", append = TRUE)
# ##########################################################################################################
# #N1
# HomeTeam_n1 <- rep(n1_teams, each = length(n1_teams))
# AwayTeam_n1 <- rep(n1_teams, length(n1_teams))
# N1_fixtures <- cbind(HomeTeam_n1,AwayTeam_n1)
# N1_fixtures <- as.data.frame(N1_fixtures)
# N1_fixtures <- N1_fixtures[!N1_fixtures$HomeTeam_n1 == N1_fixtures$AwayTeam_n1,]
# rownames(N1_fixtures) <- NULL
# N1_fixtures$Div <- "N1"
# N1_fixtures <- N1_fixtures[,c(3,1,2)]
#
# N1_fixtures$avg_HG_n1 <- n1_avg_HG
#
# N1_fixtures$n1_homeas <- rep(n1_home_as,each = length(n1_teams)-1)
#
# n1_awayds_lookup <- cbind(n1_teams,n1_away_ds)
#
# n1_awayds_lookup <- as.data.frame(n1_awayds_lookup)
#
# colnames(n1_awayds_lookup) <- c("AwayTeam_n1","n1_awayds")
#
#
# require('RH2')
# N1_fixtures$n1_awayds <- sqldf("SELECT n1_awayds_lookup.n1_awayds FROM n1_awayds_lookup INNER JOIN N1_fixtures ON n1_awayds_lookup.AwayTeam_n1 = N1_fixtures.AwayTeam_n1")
#
# N1_fixtures$avg_AG_n1 <- n1_avg_AG
#
# n1_awayas_lookup <- cbind(n1_teams,n1_away_as)
#
# n1_awayas_lookup <- as.data.frame(n1_awayas_lookup)
#
# colnames(n1_awayas_lookup) <- c("AwayTeam_n1","n1_awayas")
#
#
# N1_fixtures$n1_awayas <- sqldf("SELECT n1_awayas_lookup.n1_awayas FROM n1_awayas_lookup INNER JOIN N1_fixtures ON n1_awayas_lookup.AwayTeam_n1 = N1_fixtures.AwayTeam_n1")
#
# N1_fixtures$n1_homeds <- rep(n1_home_ds,each = length(n1_teams)-1)
#
# N1_fixtures$n1_awayds <- as.numeric(unlist(N1_fixtures$n1_awayds))
# #xGH
# N1_fixtures$n1_xGH <- N1_fixtures$avg_HG_n1 * N1_fixtures$n1_homeas * N1_fixtures$n1_awayds
#
# #xGA
#
# N1_fixtures$n1_awayas <- as.numeric(unlist(N1_fixtures$n1_awayas))
#
# N1_fixtures$n1_xGA <- N1_fixtures$avg_AG_n1 * N1_fixtures$n1_awayas * N1_fixtures$n1_homeds
#
# N1_fixtures$n1_0_0 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_0 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_1 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_1 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_0 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_2 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_2 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_1 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_2 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_3 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_0 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_1 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_2 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_3 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_3 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_3 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_4 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_0 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_1 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_2 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_3 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_4 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_4 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_4 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_4 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_5 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_0 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_1 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_2 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_3 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_4 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_5 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_5 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_5 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_5 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_5 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_6 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_0 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(0,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_1 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(1,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_2 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(2,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_3 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(3,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_4 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(4,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_6_5 <- round(stats::dpois(6,N1_fixtures$n1_xGH) * stats::dpois(5,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_0_6 <- round(stats::dpois(0,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_1_6 <- round(stats::dpois(1,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_2_6 <- round(stats::dpois(2,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_3_6 <- round(stats::dpois(3,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_4_6 <- round(stats::dpois(4,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# N1_fixtures$n1_5_6 <- round(stats::dpois(5,N1_fixtures$n1_xGH) * stats::dpois(6,N1_fixtures$n1_xGA), digits = 4)
# #Home win
# N1_fixtures$n1_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 + N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5
# )
#
# N1_fixtures$n1_H <- percent(N1_fixtures$n1_H, accuracy = 0.1)
#
# #Draw
# N1_fixtures$n1_D <- (
#
#   N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 + N1_fixtures$n1_4_4 +
#     N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6
# )
#
# N1_fixtures$n1_D <- percent(N1_fixtures$n1_D, accuracy = 0.1)
#
# #Away
#
# N1_fixtures$n1_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 + N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6
# )
#
# N1_fixtures$n1_A <- percent(N1_fixtures$n1_A, accuracy = 0.1)
#
# #ov25
# N1_fixtures$n1_ov25 <- (
#   N1_fixtures$n1_2_1 + N1_fixtures$n1_1_2 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 + N1_fixtures$n1_2_3 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 + N1_fixtures$n1_0_4 +
#     N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 + N1_fixtures$n1_4_4 + N1_fixtures$n1_5_0 +
#     N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 + N1_fixtures$n1_0_5 +
#     N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 + N1_fixtures$n1_5_5 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5 + N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 +
#     N1_fixtures$n1_4_6 + N1_fixtures$n1_5_6 + N1_fixtures$n1_6_6
# )
# #un25
# N1_fixtures$n1_un25 <- (
#   N1_fixtures$n1_0_0 + N1_fixtures$n1_1_0 + N1_fixtures$n1_0_1 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_0 + N1_fixtures$n1_0_2
# )
# #odds
# N1_fixtures$n1_ov25_odds <- round((1/N1_fixtures$n1_ov25),digits = 2)
# N1_fixtures$n1_un25_odds <- round((1/N1_fixtures$n1_un25),digits = 2)
#
# N1_fixtures$n1_ov25_odds
# N1_fixtures$n1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# N1_fixtures$n1_BTTSY <- (
#   N1_fixtures$n1_1_1 + N1_fixtures$n1_2_1 + N1_fixtures$n1_1_2 + N1_fixtures$n1_3_1 + N1_fixtures$n1_3_2 +
#     N1_fixtures$n1_2_2 + N1_fixtures$n1_1_3 + N1_fixtures$n1_2_3 + N1_fixtures$n1_3_3 + N1_fixtures$n1_4_4 +
#     N1_fixtures$n1_4_1 + N1_fixtures$n1_4_3 + N1_fixtures$n1_4_2 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 +
#     N1_fixtures$n1_3_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 +
#     N1_fixtures$n1_5_4 + N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_6_6 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6
# )
# #BTTSN
# N1_fixtures$n1_BTTSN <- (
#   N1_fixtures$n1_0_0 + N1_fixtures$n1_1_0 + N1_fixtures$n1_0_1 + N1_fixtures$n1_2_0 + N1_fixtures$n1_0_2 +
#     N1_fixtures$n1_3_0 + N1_fixtures$n1_0_3 + N1_fixtures$n1_4_0 + N1_fixtures$n1_0_4 + N1_fixtures$n1_5_0 +
#     N1_fixtures$n1_0_5 + N1_fixtures$n1_6_0 + N1_fixtures$n1_0_6
# )
#
# N1_fixtures$n1_BTTSY_odds <- round((1/N1_fixtures$n1_BTTSY),digits = 2)
# N1_fixtures$n1_BTTSN_odds <- round((1/N1_fixtures$n1_BTTSN),digits = 2)
#
# N1_fixtures$n1_BTTSY <- percent(N1_fixtures$n1_BTTSY, accuracy = 0.1)
# N1_fixtures$n1_BTTSN <- percent(N1_fixtures$n1_BTTSN, accuracy = 0.1)
# #odds
# N1_fixtures$n1_BTTSY_odds
# N1_fixtures$n1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# N1_fixtures$n1_AH_0_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 +N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6
# )
# #AH_0_A
# N1_fixtures$n1_AH_0_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 +N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6
# )
#
# #odds
# N1_fixtures$n1_AH_0_H_odds <- round((1/N1_fixtures$n1_AH_0_H),digits = 2)
# N1_fixtures$n1_AH_0_A_odds <- round((1/N1_fixtures$n1_AH_0_A),digits = 2)
#
# N1_fixtures$n1_AH_0_H_odds
# N1_fixtures$n1_AH_0_A_odds
# #percentages
# N1_fixtures$n1_AH_0_H <- percent(N1_fixtures$n1_AH_0_H, accuracy = 0.1)
# N1_fixtures$n1_AH_0_A <- percent(N1_fixtures$n1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# N1_fixtures$n1_AH_n075_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 +N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5
# )
# #AH_n075_A
# N1_fixtures$n1_AH_n075_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 +N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6
# )
#
# #odds
# N1_fixtures$n1_AH_n075_H_odds <- round((1/N1_fixtures$n1_AH_n075_H),digits = 2)
# N1_fixtures$n1_AH_n075_A_odds <- round((1/N1_fixtures$n1_AH_n075_A),digits = 2)
#
# N1_fixtures$n1_AH_n075_H_odds
# N1_fixtures$n1_AH_n075_A_odds
# #percentages
# N1_fixtures$n1_AH_n075_H <- percent(N1_fixtures$n1_AH_n075_H, accuracy = 0.1)
# N1_fixtures$n1_AH_n075_A <- percent(N1_fixtures$n1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# N1_fixtures$n1_AH_075_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 +N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6 + N1_fixtures$n1_0_1 + N1_fixtures$n1_1_2 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_3_4 + N1_fixtures$n1_4_5 + N1_fixtures$n1_5_6
# )
# #AH_075_A
# N1_fixtures$n1_AH_075_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 +N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6 + N1_fixtures$n1_1_0 + N1_fixtures$n1_2_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_3 + N1_fixtures$n1_5_4 + N1_fixtures$n1_6_5
# )
#
# #odds
# N1_fixtures$n1_AH_075_H_odds <- round((1/N1_fixtures$n1_AH_075_H),digits = 2)
# N1_fixtures$n1_AH_075_A_odds <- round((1/N1_fixtures$n1_AH_075_A),digits = 2)
#
# N1_fixtures$n1_AH_075_H_odds
# N1_fixtures$n1_AH_075_A_odds
# #percentages
# N1_fixtures$n1_AH_075_H <- percent(N1_fixtures$n1_AH_075_H, accuracy = 0.1)
# N1_fixtures$n1_AH_075_A <- percent(N1_fixtures$n1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# N1_fixtures$n1_AH_n125_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 +N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5
# )
# #AH_n125_A
# N1_fixtures$n1_AH_n125_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 +N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6
# )
#
# #odds
# N1_fixtures$n1_AH_n125_H_odds <- round((1/N1_fixtures$n1_AH_n125_H),digits = 2)
# N1_fixtures$n1_AH_n125_A_odds <- round((1/N1_fixtures$n1_AH_n125_A),digits = 2)
#
# N1_fixtures$n1_AH_n125_H_odds
# N1_fixtures$n1_AH_n125_A_odds
# #percentages
# N1_fixtures$n1_AH_n125_H <- percent(N1_fixtures$n1_AH_n125_H, accuracy = 0.1)
# N1_fixtures$n1_AH_n125_A <- percent(N1_fixtures$n1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# N1_fixtures$n1_AH_125_H <- (
#   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#     N1_fixtures$n1_5_0 +N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#     N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#     N1_fixtures$n1_6_5 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6 + N1_fixtures$n1_0_1 + N1_fixtures$n1_1_2 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_3_4 + N1_fixtures$n1_4_5 + N1_fixtures$n1_5_6
# )
# #AH_125_A
# N1_fixtures$n1_AH_125_A <- (
#   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#     N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#     N1_fixtures$n1_0_5 +N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#     N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#     N1_fixtures$n1_5_6 + N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 +
#     N1_fixtures$n1_4_4 + N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6 + N1_fixtures$n1_1_0 + N1_fixtures$n1_2_1 +
#     N1_fixtures$n1_3_2 + N1_fixtures$n1_4_3 + N1_fixtures$n1_5_4 + N1_fixtures$n1_6_5
# )
#
# #odds
# N1_fixtures$n1_AH_125_H_odds <- round((1/N1_fixtures$n1_AH_125_H),digits = 2)
# N1_fixtures$n1_AH_125_A_odds <- round((1/N1_fixtures$n1_AH_125_A),digits = 2)
#
# N1_fixtures$n1_AH_125_H_odds
# N1_fixtures$n1_AH_125_A_odds
# #percentages
# N1_fixtures$n1_AH_125_H <- percent(N1_fixtures$n1_AH_125_H, accuracy = 0.1)
# N1_fixtures$n1_AH_125_A <- percent(N1_fixtures$n1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# N1_fixtures$n1_ov25 <- percent(N1_fixtures$n1_ov25, accuracy = 0.1)
#
# N1_fixtures$n1_un25 <- percent(N1_fixtures$n1_un25, accuracy = 0.1)
# N1_fixtures$n1_pscore <- paste(round(N1_fixtures$n1_xGH,digits = 0),round(N1_fixtures$n1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(N1_fixtures,'Divisions/N1.xlsx',sheetName = "N1", append = TRUE)
# ############################################################################################################
# #P1
# HomeTeam_p1 <- rep(p1_teams, each = length(p1_teams))
# AwayTeam_p1 <- rep(p1_teams, length(p1_teams))
# P1_fixtures <- cbind(HomeTeam_p1,AwayTeam_p1)
# P1_fixtures <- as.data.frame(P1_fixtures)
# P1_fixtures <- P1_fixtures[!P1_fixtures$HomeTeam_p1 == P1_fixtures$AwayTeam_p1,]
# rownames(P1_fixtures) <- NULL
# P1_fixtures$Div <- "P1"
# P1_fixtures <- P1_fixtures[,c(3,1,2)]
#
# P1_fixtures$avg_HG_p1 <- p1_avg_HG
#
# P1_fixtures$p1_homeas <- rep(p1_home_as,each = length(p1_teams)-1)
#
# p1_awayds_lookup <- cbind(p1_teams,p1_away_ds)
#
# p1_awayds_lookup <- as.data.frame(p1_awayds_lookup)
#
# colnames(p1_awayds_lookup) <- c("AwayTeam_p1","p1_awayds")
#
#
# require('RH2')
# P1_fixtures$p1_awayds <- sqldf("SELECT p1_awayds_lookup.p1_awayds FROM p1_awayds_lookup INNER JOIN P1_fixtures ON p1_awayds_lookup.AwayTeam_p1 = P1_fixtures.AwayTeam_p1")
#
# P1_fixtures$avg_AG_p1 <- p1_avg_AG
#
# p1_awayas_lookup <- cbind(p1_teams,p1_away_as)
#
# p1_awayas_lookup <- as.data.frame(p1_awayas_lookup)
#
# colnames(p1_awayas_lookup) <- c("AwayTeam_p1","p1_awayas")
#
#
# P1_fixtures$p1_awayas <- sqldf("SELECT p1_awayas_lookup.p1_awayas FROM p1_awayas_lookup INNER JOIN P1_fixtures ON p1_awayas_lookup.AwayTeam_p1 = P1_fixtures.AwayTeam_p1")
#
# P1_fixtures$p1_homeds <- rep(p1_home_ds,each = length(p1_teams)-1)
#
# P1_fixtures$p1_awayds <- as.numeric(unlist(P1_fixtures$p1_awayds))
# #xGH
# P1_fixtures$p1_xGH <- P1_fixtures$avg_HG_p1 * P1_fixtures$p1_homeas * P1_fixtures$p1_awayds
#
# #xGA
#
# P1_fixtures$p1_awayas <- as.numeric(unlist(P1_fixtures$p1_awayas))
#
# P1_fixtures$p1_xGA <- P1_fixtures$avg_AG_p1 * P1_fixtures$p1_awayas * P1_fixtures$p1_homeds
#
# P1_fixtures$p1_0_0 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_0 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_1 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_1 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_0 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_2 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_2 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_1 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_2 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_3 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_0 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_1 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_2 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_3 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_3 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_3 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_4 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_0 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_1 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_2 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_3 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_4 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_4 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_4 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_4 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_5 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_0 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_1 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_2 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_3 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_4 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_5 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_5 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_5 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_5 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_5 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_6 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_0 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(0,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_1 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(1,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_2 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(2,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_3 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(3,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_4 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(4,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_6_5 <- round(stats::dpois(6,P1_fixtures$p1_xGH) * stats::dpois(5,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_0_6 <- round(stats::dpois(0,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_1_6 <- round(stats::dpois(1,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_2_6 <- round(stats::dpois(2,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_3_6 <- round(stats::dpois(3,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_4_6 <- round(stats::dpois(4,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# P1_fixtures$p1_5_6 <- round(stats::dpois(5,P1_fixtures$p1_xGH) * stats::dpois(6,P1_fixtures$p1_xGA), digits = 4)
# #Home win
# P1_fixtures$p1_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 + P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5
# )
#
# P1_fixtures$p1_H <- percent(P1_fixtures$p1_H, accuracy = 0.1)
#
# #Draw
# P1_fixtures$p1_D <- (
#
#   P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 + P1_fixtures$p1_4_4 +
#     P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6
# )
#
# P1_fixtures$p1_D <- percent(P1_fixtures$p1_D, accuracy = 0.1)
#
# #Away
#
# P1_fixtures$p1_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 + P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6
# )
#
# P1_fixtures$p1_A <- percent(P1_fixtures$p1_A, accuracy = 0.1)
#
# #ov25
# P1_fixtures$p1_ov25 <- (
#   P1_fixtures$p1_2_1 + P1_fixtures$p1_1_2 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 + P1_fixtures$p1_2_3 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 + P1_fixtures$p1_0_4 +
#     P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 + P1_fixtures$p1_4_4 + P1_fixtures$p1_5_0 +
#     P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 + P1_fixtures$p1_0_5 +
#     P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 + P1_fixtures$p1_5_5 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5 + P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 +
#     P1_fixtures$p1_4_6 + P1_fixtures$p1_5_6 + P1_fixtures$p1_6_6
# )
# #un25
# P1_fixtures$p1_un25 <- (
#   P1_fixtures$p1_0_0 + P1_fixtures$p1_1_0 + P1_fixtures$p1_0_1 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_0 + P1_fixtures$p1_0_2
# )
# #odds
# P1_fixtures$p1_ov25_odds <- round((1/P1_fixtures$p1_ov25),digits = 2)
# P1_fixtures$p1_un25_odds <- round((1/P1_fixtures$p1_un25),digits = 2)
#
# P1_fixtures$p1_ov25_odds
# P1_fixtures$p1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# P1_fixtures$p1_BTTSY <- (
#   P1_fixtures$p1_1_1 + P1_fixtures$p1_2_1 + P1_fixtures$p1_1_2 + P1_fixtures$p1_3_1 + P1_fixtures$p1_3_2 +
#     P1_fixtures$p1_2_2 + P1_fixtures$p1_1_3 + P1_fixtures$p1_2_3 + P1_fixtures$p1_3_3 + P1_fixtures$p1_4_4 +
#     P1_fixtures$p1_4_1 + P1_fixtures$p1_4_3 + P1_fixtures$p1_4_2 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 +
#     P1_fixtures$p1_3_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 +
#     P1_fixtures$p1_5_4 + P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_6_6 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6
# )
# #BTTSN
# P1_fixtures$p1_BTTSN <- (
#   P1_fixtures$p1_0_0 + P1_fixtures$p1_1_0 + P1_fixtures$p1_0_1 + P1_fixtures$p1_2_0 + P1_fixtures$p1_0_2 +
#     P1_fixtures$p1_3_0 + P1_fixtures$p1_0_3 + P1_fixtures$p1_4_0 + P1_fixtures$p1_0_4 + P1_fixtures$p1_5_0 +
#     P1_fixtures$p1_0_5 + P1_fixtures$p1_6_0 + P1_fixtures$p1_0_6
# )
#
# P1_fixtures$p1_BTTSY_odds <- round((1/P1_fixtures$p1_BTTSY),digits = 2)
# P1_fixtures$p1_BTTSN_odds <- round((1/P1_fixtures$p1_BTTSN),digits = 2)
#
# P1_fixtures$p1_BTTSY <- percent(P1_fixtures$p1_BTTSY, accuracy = 0.1)
# P1_fixtures$p1_BTTSN <- percent(P1_fixtures$p1_BTTSN, accuracy = 0.1)
# #odds
# P1_fixtures$p1_BTTSY_odds
# P1_fixtures$p1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# P1_fixtures$p1_AH_0_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 +P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6
# )
# #AH_0_A
# P1_fixtures$p1_AH_0_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 +P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6
# )
#
# #odds
# P1_fixtures$p1_AH_0_H_odds <- round((1/P1_fixtures$p1_AH_0_H),digits = 2)
# P1_fixtures$p1_AH_0_A_odds <- round((1/P1_fixtures$p1_AH_0_A),digits = 2)
#
# P1_fixtures$p1_AH_0_H_odds
# P1_fixtures$p1_AH_0_A_odds
# #percentages
# P1_fixtures$p1_AH_0_H <- percent(P1_fixtures$p1_AH_0_H, accuracy = 0.1)
# P1_fixtures$p1_AH_0_A <- percent(P1_fixtures$p1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# P1_fixtures$p1_AH_n075_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 +P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5
# )
# #AH_n075_A
# P1_fixtures$p1_AH_n075_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 +P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6
# )
#
# #odds
# P1_fixtures$p1_AH_n075_H_odds <- round((1/P1_fixtures$p1_AH_n075_H),digits = 2)
# P1_fixtures$p1_AH_n075_A_odds <- round((1/P1_fixtures$p1_AH_n075_A),digits = 2)
#
# P1_fixtures$p1_AH_n075_H_odds
# P1_fixtures$p1_AH_n075_A_odds
# #percentages
# P1_fixtures$p1_AH_n075_H <- percent(P1_fixtures$p1_AH_n075_H, accuracy = 0.1)
# P1_fixtures$p1_AH_n075_A <- percent(P1_fixtures$p1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# P1_fixtures$p1_AH_075_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 +P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6 + P1_fixtures$p1_0_1 + P1_fixtures$p1_1_2 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_3_4 + P1_fixtures$p1_4_5 + P1_fixtures$p1_5_6
# )
# #AH_075_A
# P1_fixtures$p1_AH_075_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 +P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6 + P1_fixtures$p1_1_0 + P1_fixtures$p1_2_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_3 + P1_fixtures$p1_5_4 + P1_fixtures$p1_6_5
# )
#
# #odds
# P1_fixtures$p1_AH_075_H_odds <- round((1/P1_fixtures$p1_AH_075_H),digits = 2)
# P1_fixtures$p1_AH_075_A_odds <- round((1/P1_fixtures$p1_AH_075_A),digits = 2)
#
# P1_fixtures$p1_AH_075_H_odds
# P1_fixtures$p1_AH_075_A_odds
# #percentages
# P1_fixtures$p1_AH_075_H <- percent(P1_fixtures$p1_AH_075_H, accuracy = 0.1)
# P1_fixtures$p1_AH_075_A <- percent(P1_fixtures$p1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# P1_fixtures$p1_AH_n125_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 +P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5
# )
# #AH_n125_A
# P1_fixtures$p1_AH_n125_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 +P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6
# )
#
# #odds
# P1_fixtures$p1_AH_n125_H_odds <- round((1/P1_fixtures$p1_AH_n125_H),digits = 2)
# P1_fixtures$p1_AH_n125_A_odds <- round((1/P1_fixtures$p1_AH_n125_A),digits = 2)
#
# P1_fixtures$p1_AH_n125_H_odds
# P1_fixtures$p1_AH_n125_A_odds
# #percentages
# P1_fixtures$p1_AH_n125_H <- percent(P1_fixtures$p1_AH_n125_H, accuracy = 0.1)
# P1_fixtures$p1_AH_n125_A <- percent(P1_fixtures$p1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# P1_fixtures$p1_AH_125_H <- (
#   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#     P1_fixtures$p1_5_0 +P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#     P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#     P1_fixtures$p1_6_5 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6 + P1_fixtures$p1_0_1 + P1_fixtures$p1_1_2 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_3_4 + P1_fixtures$p1_4_5 + P1_fixtures$p1_5_6
# )
# #AH_125_A
# P1_fixtures$p1_AH_125_A <- (
#   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#     P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#     P1_fixtures$p1_0_5 +P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#     P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#     P1_fixtures$p1_5_6 + P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 +
#     P1_fixtures$p1_4_4 + P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6 + P1_fixtures$p1_1_0 + P1_fixtures$p1_2_1 +
#     P1_fixtures$p1_3_2 + P1_fixtures$p1_4_3 + P1_fixtures$p1_5_4 + P1_fixtures$p1_6_5
# )
#
# #odds
# P1_fixtures$p1_AH_125_H_odds <- round((1/P1_fixtures$p1_AH_125_H),digits = 2)
# P1_fixtures$p1_AH_125_A_odds <- round((1/P1_fixtures$p1_AH_125_A),digits = 2)
#
# P1_fixtures$p1_AH_125_H_odds
# P1_fixtures$p1_AH_125_A_odds
# #percentages
# P1_fixtures$p1_AH_125_H <- percent(P1_fixtures$p1_AH_125_H, accuracy = 0.1)
# P1_fixtures$p1_AH_125_A <- percent(P1_fixtures$p1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# P1_fixtures$p1_ov25 <- percent(P1_fixtures$p1_ov25, accuracy = 0.1)
#
# P1_fixtures$p1_un25 <- percent(P1_fixtures$p1_un25, accuracy = 0.1)
# P1_fixtures$p1_pscore <- paste(round(P1_fixtures$p1_xGH,digits = 0),round(P1_fixtures$p1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(P1_fixtures,'Divisions/P1.xlsx',sheetName = "P1", append = TRUE)
# ##################################################################################################################
# #SP1
# HomeTeam_sp1 <- rep(sp1_teams, each = length(sp1_teams))
# AwayTeam_sp1 <- rep(sp1_teams, length(sp1_teams))
# SP1_fixtures <- cbind(HomeTeam_sp1,AwayTeam_sp1)
# SP1_fixtures <- as.data.frame(SP1_fixtures)
# SP1_fixtures <- SP1_fixtures[!SP1_fixtures$HomeTeam_sp1 == SP1_fixtures$AwayTeam_sp1,]
# rownames(SP1_fixtures) <- NULL
# SP1_fixtures$Div <- "SP1"
# SP1_fixtures <- SP1_fixtures[,c(3,1,2)]
#
# SP1_fixtures$avg_HG_sp1 <- sp1_avg_HG
#
# SP1_fixtures$sp1_homeas <- rep(sp1_home_as,each = length(sp1_teams)-1)
#
# sp1_awayds_lookup <- cbind(sp1_teams,sp1_away_ds)
#
# sp1_awayds_lookup <- as.data.frame(sp1_awayds_lookup)
#
# colnames(sp1_awayds_lookup) <- c("AwayTeam_sp1","sp1_awayds")
#
#
# require('RH2')
# SP1_fixtures$sp1_awayds <- sqldf("SELECT sp1_awayds_lookup.sp1_awayds FROM sp1_awayds_lookup INNER JOIN SP1_fixtures ON sp1_awayds_lookup.AwayTeam_sp1 = SP1_fixtures.AwayTeam_sp1")
#
# SP1_fixtures$avg_AG_sp1 <- sp1_avg_AG
#
# sp1_awayas_lookup <- cbind(sp1_teams,sp1_away_as)
#
# sp1_awayas_lookup <- as.data.frame(sp1_awayas_lookup)
#
# colnames(sp1_awayas_lookup) <- c("AwayTeam_sp1","sp1_awayas")
#
#
# SP1_fixtures$sp1_awayas <- sqldf("SELECT sp1_awayas_lookup.sp1_awayas FROM sp1_awayas_lookup INNER JOIN SP1_fixtures ON sp1_awayas_lookup.AwayTeam_sp1 = SP1_fixtures.AwayTeam_sp1")
#
# SP1_fixtures$sp1_homeds <- rep(sp1_home_ds,each = length(sp1_teams)-1)
#
# SP1_fixtures$sp1_awayds <- as.numeric(unlist(SP1_fixtures$sp1_awayds))
# #xGH
# SP1_fixtures$sp1_xGH <- SP1_fixtures$avg_HG_sp1 * SP1_fixtures$sp1_homeas * SP1_fixtures$sp1_awayds
#
# #xGA
#
# SP1_fixtures$sp1_awayas <- as.numeric(unlist(SP1_fixtures$sp1_awayas))
#
# SP1_fixtures$sp1_xGA <- SP1_fixtures$avg_AG_sp1 * SP1_fixtures$sp1_awayas * SP1_fixtures$sp1_homeds
#
# SP1_fixtures$sp1_0_0 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_0 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_1 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_1 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_0 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_2 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_2 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_1 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_2 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_3 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_0 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_1 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_2 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_3 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_3 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_3 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_4 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_0 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_1 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_2 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_3 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_4 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_4 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_4 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_4 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_5 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_0 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_1 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_2 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_3 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_4 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_5 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_5 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_5 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_5 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_5 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_6 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_0 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(0,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_1 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(1,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_2 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(2,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_3 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(3,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_4 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(4,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_6_5 <- round(stats::dpois(6,SP1_fixtures$sp1_xGH) * stats::dpois(5,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_0_6 <- round(stats::dpois(0,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_1_6 <- round(stats::dpois(1,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_2_6 <- round(stats::dpois(2,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_3_6 <- round(stats::dpois(3,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_4_6 <- round(stats::dpois(4,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# SP1_fixtures$sp1_5_6 <- round(stats::dpois(5,SP1_fixtures$sp1_xGH) * stats::dpois(6,SP1_fixtures$sp1_xGA), digits = 4)
# #Home win
# SP1_fixtures$sp1_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 + SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5
# )
#
# SP1_fixtures$sp1_H <- percent(SP1_fixtures$sp1_H, accuracy = 0.1)
#
# #Draw
# SP1_fixtures$sp1_D <- (
#
#   SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 + SP1_fixtures$sp1_4_4 +
#     SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6
# )
#
# SP1_fixtures$sp1_D <- percent(SP1_fixtures$sp1_D, accuracy = 0.1)
#
# #Away
#
# SP1_fixtures$sp1_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 + SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6
# )
#
# SP1_fixtures$sp1_A <- percent(SP1_fixtures$sp1_A, accuracy = 0.1)
#
# #ov25
# SP1_fixtures$sp1_ov25 <- (
#   SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 + SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 + SP1_fixtures$sp1_0_4 +
#     SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 + SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_0 +
#     SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 + SP1_fixtures$sp1_0_5 +
#     SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 + SP1_fixtures$sp1_5_5 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5 + SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 +
#     SP1_fixtures$sp1_4_6 + SP1_fixtures$sp1_5_6 + SP1_fixtures$sp1_6_6
# )
# #un25
# SP1_fixtures$sp1_un25 <- (
#   SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_0_2
# )
# #odds
# SP1_fixtures$sp1_ov25_odds <- round((1/SP1_fixtures$sp1_ov25),digits = 2)
# SP1_fixtures$sp1_un25_odds <- round((1/SP1_fixtures$sp1_un25),digits = 2)
#
# SP1_fixtures$sp1_ov25_odds
# SP1_fixtures$sp1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SP1_fixtures$sp1_BTTSY <- (
#   SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_3_1 + SP1_fixtures$sp1_3_2 +
#     SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_1_3 + SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_3_3 + SP1_fixtures$sp1_4_4 +
#     SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_3 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 +
#     SP1_fixtures$sp1_3_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 +
#     SP1_fixtures$sp1_5_4 + SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_6_6 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6
# )
# #BTTSN
# SP1_fixtures$sp1_BTTSN <- (
#   SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_0_2 +
#     SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_5_0 +
#     SP1_fixtures$sp1_0_5 + SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_0_6
# )
#
# SP1_fixtures$sp1_BTTSY_odds <- round((1/SP1_fixtures$sp1_BTTSY),digits = 2)
# SP1_fixtures$sp1_BTTSN_odds <- round((1/SP1_fixtures$sp1_BTTSN),digits = 2)
#
# SP1_fixtures$sp1_BTTSY <- percent(SP1_fixtures$sp1_BTTSY, accuracy = 0.1)
# SP1_fixtures$sp1_BTTSN <- percent(SP1_fixtures$sp1_BTTSN, accuracy = 0.1)
# #odds
# SP1_fixtures$sp1_BTTSY_odds
# SP1_fixtures$sp1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SP1_fixtures$sp1_AH_0_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 +SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6
# )
# #AH_0_A
# SP1_fixtures$sp1_AH_0_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 +SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6
# )
#
# #odds
# SP1_fixtures$sp1_AH_0_H_odds <- round((1/SP1_fixtures$sp1_AH_0_H),digits = 2)
# SP1_fixtures$sp1_AH_0_A_odds <- round((1/SP1_fixtures$sp1_AH_0_A),digits = 2)
#
# SP1_fixtures$sp1_AH_0_H_odds
# SP1_fixtures$sp1_AH_0_A_odds
# #percentages
# SP1_fixtures$sp1_AH_0_H <- percent(SP1_fixtures$sp1_AH_0_H, accuracy = 0.1)
# SP1_fixtures$sp1_AH_0_A <- percent(SP1_fixtures$sp1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SP1_fixtures$sp1_AH_n075_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 +SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5
# )
# #AH_n075_A
# SP1_fixtures$sp1_AH_n075_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 +SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6
# )
#
# #odds
# SP1_fixtures$sp1_AH_n075_H_odds <- round((1/SP1_fixtures$sp1_AH_n075_H),digits = 2)
# SP1_fixtures$sp1_AH_n075_A_odds <- round((1/SP1_fixtures$sp1_AH_n075_A),digits = 2)
#
# SP1_fixtures$sp1_AH_n075_H_odds
# SP1_fixtures$sp1_AH_n075_A_odds
# #percentages
# SP1_fixtures$sp1_AH_n075_H <- percent(SP1_fixtures$sp1_AH_n075_H, accuracy = 0.1)
# SP1_fixtures$sp1_AH_n075_A <- percent(SP1_fixtures$sp1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SP1_fixtures$sp1_AH_075_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 +SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6 + SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_1_2 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_3_4 + SP1_fixtures$sp1_4_5 + SP1_fixtures$sp1_5_6
# )
# #AH_075_A
# SP1_fixtures$sp1_AH_075_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 +SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6 + SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_3 + SP1_fixtures$sp1_5_4 + SP1_fixtures$sp1_6_5
# )
#
# #odds
# SP1_fixtures$sp1_AH_075_H_odds <- round((1/SP1_fixtures$sp1_AH_075_H),digits = 2)
# SP1_fixtures$sp1_AH_075_A_odds <- round((1/SP1_fixtures$sp1_AH_075_A),digits = 2)
#
# SP1_fixtures$sp1_AH_075_H_odds
# SP1_fixtures$sp1_AH_075_A_odds
# #percentages
# SP1_fixtures$sp1_AH_075_H <- percent(SP1_fixtures$sp1_AH_075_H, accuracy = 0.1)
# SP1_fixtures$sp1_AH_075_A <- percent(SP1_fixtures$sp1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SP1_fixtures$sp1_AH_n125_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 +SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5
# )
# #AH_n125_A
# SP1_fixtures$sp1_AH_n125_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 +SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6
# )
#
# #odds
# SP1_fixtures$sp1_AH_n125_H_odds <- round((1/SP1_fixtures$sp1_AH_n125_H),digits = 2)
# SP1_fixtures$sp1_AH_n125_A_odds <- round((1/SP1_fixtures$sp1_AH_n125_A),digits = 2)
#
# SP1_fixtures$sp1_AH_n125_H_odds
# SP1_fixtures$sp1_AH_n125_A_odds
# #percentages
# SP1_fixtures$sp1_AH_n125_H <- percent(SP1_fixtures$sp1_AH_n125_H, accuracy = 0.1)
# SP1_fixtures$sp1_AH_n125_A <- percent(SP1_fixtures$sp1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SP1_fixtures$sp1_AH_125_H <- (
#   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#     SP1_fixtures$sp1_5_0 +SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#     SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#     SP1_fixtures$sp1_6_5 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6 + SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_1_2 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_3_4 + SP1_fixtures$sp1_4_5 + SP1_fixtures$sp1_5_6
# )
# #AH_125_A
# SP1_fixtures$sp1_AH_125_A <- (
#   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#     SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#     SP1_fixtures$sp1_0_5 +SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#     SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#     SP1_fixtures$sp1_5_6 + SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 +
#     SP1_fixtures$sp1_4_4 + SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6 + SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_1 +
#     SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_3 + SP1_fixtures$sp1_5_4 + SP1_fixtures$sp1_6_5
# )
#
# #odds
# SP1_fixtures$sp1_AH_125_H_odds <- round((1/SP1_fixtures$sp1_AH_125_H),digits = 2)
# SP1_fixtures$sp1_AH_125_A_odds <- round((1/SP1_fixtures$sp1_AH_125_A),digits = 2)
#
# SP1_fixtures$sp1_AH_125_H_odds
# SP1_fixtures$sp1_AH_125_A_odds
# #percentages
# SP1_fixtures$sp1_AH_125_H <- percent(SP1_fixtures$sp1_AH_125_H, accuracy = 0.1)
# SP1_fixtures$sp1_AH_125_A <- percent(SP1_fixtures$sp1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SP1_fixtures$sp1_ov25 <- percent(SP1_fixtures$sp1_ov25, accuracy = 0.1)
#
# SP1_fixtures$sp1_un25 <- percent(SP1_fixtures$sp1_un25, accuracy = 0.1)
# SP1_fixtures$sp1_pscore <- paste(round(SP1_fixtures$sp1_xGH,digits = 0),round(SP1_fixtures$sp1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SP1_fixtures,'Divisions/SP1.xlsx',sheetName = "SP1", append = TRUE)
# ##################################################################################################################
# #SP2
# HomeTeam_sp2 <- rep(sp2_teams, each = length(sp2_teams))
# AwayTeam_sp2 <- rep(sp2_teams, length(sp2_teams))
# SP2_fixtures <- cbind(HomeTeam_sp2,AwayTeam_sp2)
# SP2_fixtures <- as.data.frame(SP2_fixtures)
# SP2_fixtures <- SP2_fixtures[!SP2_fixtures$HomeTeam_sp2 == SP2_fixtures$AwayTeam_sp2,]
# rownames(SP2_fixtures) <- NULL
# SP2_fixtures$Div <- "SP2"
# SP2_fixtures <- SP2_fixtures[,c(3,1,2)]
#
# SP2_fixtures$avg_HG_sp2 <- sp2_avg_HG
#
# SP2_fixtures$sp2_homeas <- rep(sp2_home_as,each = length(sp2_teams)-1)
#
# sp2_awayds_lookup <- cbind(sp2_teams,sp2_away_ds)
#
# sp2_awayds_lookup <- as.data.frame(sp2_awayds_lookup)
#
# colnames(sp2_awayds_lookup) <- c("AwayTeam_sp2","sp2_awayds")
#
#
# require('RH2')
# SP2_fixtures$sp2_awayds <- sqldf("SELECT sp2_awayds_lookup.sp2_awayds FROM sp2_awayds_lookup INNER JOIN SP2_fixtures ON sp2_awayds_lookup.AwayTeam_sp2 = SP2_fixtures.AwayTeam_sp2")
#
# SP2_fixtures$avg_AG_sp2 <- sp2_avg_AG
#
# sp2_awayas_lookup <- cbind(sp2_teams,sp2_away_as)
#
# sp2_awayas_lookup <- as.data.frame(sp2_awayas_lookup)
#
# colnames(sp2_awayas_lookup) <- c("AwayTeam_sp2","sp2_awayas")
#
#
# SP2_fixtures$sp2_awayas <- sqldf("SELECT sp2_awayas_lookup.sp2_awayas FROM sp2_awayas_lookup INNER JOIN SP2_fixtures ON sp2_awayas_lookup.AwayTeam_sp2 = SP2_fixtures.AwayTeam_sp2")
#
# SP2_fixtures$sp2_homeds <- rep(sp2_home_ds,each = length(sp2_teams)-1)
#
# SP2_fixtures$sp2_awayds <- as.numeric(unlist(SP2_fixtures$sp2_awayds))
# #xGH
# SP2_fixtures$sp2_xGH <- SP2_fixtures$avg_HG_sp2 * SP2_fixtures$sp2_homeas * SP2_fixtures$sp2_awayds
#
# #xGA
#
# SP2_fixtures$sp2_awayas <- as.numeric(unlist(SP2_fixtures$sp2_awayas))
#
# SP2_fixtures$sp2_xGA <- SP2_fixtures$avg_AG_sp2 * SP2_fixtures$sp2_awayas * SP2_fixtures$sp2_homeds
#
# SP2_fixtures$sp2_0_0 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_0 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_1 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_1 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_0 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_2 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_2 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_1 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_2 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_3 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_0 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_1 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_2 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_3 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_3 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_3 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_4 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_0 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_1 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_2 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_3 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_4 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_4 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_4 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_4 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_5 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_0 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_1 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_2 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_3 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_4 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_5 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_5 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_5 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_5 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_5 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_6 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_0 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(0,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_1 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(1,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_2 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(2,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_3 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(3,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_4 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(4,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_6_5 <- round(stats::dpois(6,SP2_fixtures$sp2_xGH) * stats::dpois(5,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_0_6 <- round(stats::dpois(0,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_1_6 <- round(stats::dpois(1,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_2_6 <- round(stats::dpois(2,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_3_6 <- round(stats::dpois(3,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_4_6 <- round(stats::dpois(4,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# SP2_fixtures$sp2_5_6 <- round(stats::dpois(5,SP2_fixtures$sp2_xGH) * stats::dpois(6,SP2_fixtures$sp2_xGA), digits = 4)
# #Home win
# SP2_fixtures$sp2_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 + SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5
# )
#
# SP2_fixtures$sp2_H <- percent(SP2_fixtures$sp2_H, accuracy = 0.1)
#
# #Draw
# SP2_fixtures$sp2_D <- (
#
#   SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 + SP2_fixtures$sp2_4_4 +
#     SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6
# )
#
# SP2_fixtures$sp2_D <- percent(SP2_fixtures$sp2_D, accuracy = 0.1)
#
# #Away
#
# SP2_fixtures$sp2_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 + SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6
# )
#
# SP2_fixtures$sp2_A <- percent(SP2_fixtures$sp2_A, accuracy = 0.1)
#
# #ov25
# SP2_fixtures$sp2_ov25 <- (
#   SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 + SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 + SP2_fixtures$sp2_0_4 +
#     SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 + SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_0 +
#     SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 + SP2_fixtures$sp2_0_5 +
#     SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 + SP2_fixtures$sp2_5_5 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5 + SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 +
#     SP2_fixtures$sp2_4_6 + SP2_fixtures$sp2_5_6 + SP2_fixtures$sp2_6_6
# )
# #un25
# SP2_fixtures$sp2_un25 <- (
#   SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_0_2
# )
# #odds
# SP2_fixtures$sp2_ov25_odds <- round((1/SP2_fixtures$sp2_ov25),digits = 2)
# SP2_fixtures$sp2_un25_odds <- round((1/SP2_fixtures$sp2_un25),digits = 2)
#
# SP2_fixtures$sp2_ov25_odds
# SP2_fixtures$sp2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SP2_fixtures$sp2_BTTSY <- (
#   SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_3_1 + SP2_fixtures$sp2_3_2 +
#     SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_1_3 + SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_3_3 + SP2_fixtures$sp2_4_4 +
#     SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_3 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 +
#     SP2_fixtures$sp2_3_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 +
#     SP2_fixtures$sp2_5_4 + SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_6_6 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6
# )
# #BTTSN
# SP2_fixtures$sp2_BTTSN <- (
#   SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_0_2 +
#     SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_5_0 +
#     SP2_fixtures$sp2_0_5 + SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_0_6
# )
#
# SP2_fixtures$sp2_BTTSY_odds <- round((1/SP2_fixtures$sp2_BTTSY),digits = 2)
# SP2_fixtures$sp2_BTTSN_odds <- round((1/SP2_fixtures$sp2_BTTSN),digits = 2)
#
# SP2_fixtures$sp2_BTTSY <- percent(SP2_fixtures$sp2_BTTSY, accuracy = 0.1)
# SP2_fixtures$sp2_BTTSN <- percent(SP2_fixtures$sp2_BTTSN, accuracy = 0.1)
# #odds
# SP2_fixtures$sp2_BTTSY_odds
# SP2_fixtures$sp2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SP2_fixtures$sp2_AH_0_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 +SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6
# )
# #AH_0_A
# SP2_fixtures$sp2_AH_0_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 +SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6
# )
#
# #odds
# SP2_fixtures$sp2_AH_0_H_odds <- round((1/SP2_fixtures$sp2_AH_0_H),digits = 2)
# SP2_fixtures$sp2_AH_0_A_odds <- round((1/SP2_fixtures$sp2_AH_0_A),digits = 2)
#
# SP2_fixtures$sp2_AH_0_H_odds
# SP2_fixtures$sp2_AH_0_A_odds
# #percentages
# SP2_fixtures$sp2_AH_0_H <- percent(SP2_fixtures$sp2_AH_0_H, accuracy = 0.1)
# SP2_fixtures$sp2_AH_0_A <- percent(SP2_fixtures$sp2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SP2_fixtures$sp2_AH_n075_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 +SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5
# )
# #AH_n075_A
# SP2_fixtures$sp2_AH_n075_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 +SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6
# )
#
# #odds
# SP2_fixtures$sp2_AH_n075_H_odds <- round((1/SP2_fixtures$sp2_AH_n075_H),digits = 2)
# SP2_fixtures$sp2_AH_n075_A_odds <- round((1/SP2_fixtures$sp2_AH_n075_A),digits = 2)
#
# SP2_fixtures$sp2_AH_n075_H_odds
# SP2_fixtures$sp2_AH_n075_A_odds
# #percentages
# SP2_fixtures$sp2_AH_n075_H <- percent(SP2_fixtures$sp2_AH_n075_H, accuracy = 0.1)
# SP2_fixtures$sp2_AH_n075_A <- percent(SP2_fixtures$sp2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SP2_fixtures$sp2_AH_075_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 +SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6 + SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_1_2 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_3_4 + SP2_fixtures$sp2_4_5 + SP2_fixtures$sp2_5_6
# )
# #AH_075_A
# SP2_fixtures$sp2_AH_075_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 +SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6 + SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_3 + SP2_fixtures$sp2_5_4 + SP2_fixtures$sp2_6_5
# )
#
# #odds
# SP2_fixtures$sp2_AH_075_H_odds <- round((1/SP2_fixtures$sp2_AH_075_H),digits = 2)
# SP2_fixtures$sp2_AH_075_A_odds <- round((1/SP2_fixtures$sp2_AH_075_A),digits = 2)
#
# SP2_fixtures$sp2_AH_075_H_odds
# SP2_fixtures$sp2_AH_075_A_odds
# #percentages
# SP2_fixtures$sp2_AH_075_H <- percent(SP2_fixtures$sp2_AH_075_H, accuracy = 0.1)
# SP2_fixtures$sp2_AH_075_A <- percent(SP2_fixtures$sp2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SP2_fixtures$sp2_AH_n125_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 +SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5
# )
# #AH_n125_A
# SP2_fixtures$sp2_AH_n125_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 +SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6
# )
#
# #odds
# SP2_fixtures$sp2_AH_n125_H_odds <- round((1/SP2_fixtures$sp2_AH_n125_H),digits = 2)
# SP2_fixtures$sp2_AH_n125_A_odds <- round((1/SP2_fixtures$sp2_AH_n125_A),digits = 2)
#
# SP2_fixtures$sp2_AH_n125_H_odds
# SP2_fixtures$sp2_AH_n125_A_odds
# #percentages
# SP2_fixtures$sp2_AH_n125_H <- percent(SP2_fixtures$sp2_AH_n125_H, accuracy = 0.1)
# SP2_fixtures$sp2_AH_n125_A <- percent(SP2_fixtures$sp2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SP2_fixtures$sp2_AH_125_H <- (
#   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#     SP2_fixtures$sp2_5_0 +SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#     SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#     SP2_fixtures$sp2_6_5 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6 + SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_1_2 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_3_4 + SP2_fixtures$sp2_4_5 + SP2_fixtures$sp2_5_6
# )
# #AH_125_A
# SP2_fixtures$sp2_AH_125_A <- (
#   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#     SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#     SP2_fixtures$sp2_0_5 +SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#     SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#     SP2_fixtures$sp2_5_6 + SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 +
#     SP2_fixtures$sp2_4_4 + SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6 + SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_1 +
#     SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_3 + SP2_fixtures$sp2_5_4 + SP2_fixtures$sp2_6_5
# )
#
# #odds
# SP2_fixtures$sp2_AH_125_H_odds <- round((1/SP2_fixtures$sp2_AH_125_H),digits = 2)
# SP2_fixtures$sp2_AH_125_A_odds <- round((1/SP2_fixtures$sp2_AH_125_A),digits = 2)
#
# SP2_fixtures$sp2_AH_125_H_odds
# SP2_fixtures$sp2_AH_125_A_odds
# #percentages
# SP2_fixtures$sp2_AH_125_H <- percent(SP2_fixtures$sp2_AH_125_H, accuracy = 0.1)
# SP2_fixtures$sp2_AH_125_A <- percent(SP2_fixtures$sp2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SP2_fixtures$sp2_ov25 <- percent(SP2_fixtures$sp2_ov25, accuracy = 0.1)
#
# SP2_fixtures$sp2_un25 <- percent(SP2_fixtures$sp2_un25, accuracy = 0.1)
# SP2_fixtures$sp2_pscore <- paste(round(SP2_fixtures$sp2_xGH,digits = 0),round(SP2_fixtures$sp2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SP2_fixtures,'Divisions/SP2.xlsx',sheetName = "SP2", append = TRUE)
# ###################################################################################################################
# #SC0
# HomeTeam_sc0 <- rep(sc0_teams, each = length(sc0_teams))
# AwayTeam_sc0 <- rep(sc0_teams, length(sc0_teams))
# SC0_fixtures <- cbind(HomeTeam_sc0,AwayTeam_sc0)
# SC0_fixtures <- as.data.frame(SC0_fixtures)
# SC0_fixtures <- SC0_fixtures[!SC0_fixtures$HomeTeam_sc0 == SC0_fixtures$AwayTeam_sc0,]
# rownames(SC0_fixtures) <- NULL
# SC0_fixtures$Div <- "SC0"
# SC0_fixtures <- SC0_fixtures[,c(3,1,2)]
#
# SC0_fixtures$avg_HG_sc0 <- sc0_avg_HG
#
# SC0_fixtures$sc0_homeas <- rep(sc0_home_as,each = length(sc0_teams)-1)
#
# sc0_awayds_lookup <- cbind(sc0_teams,sc0_away_ds)
#
# sc0_awayds_lookup <- as.data.frame(sc0_awayds_lookup)
#
# colnames(sc0_awayds_lookup) <- c("AwayTeam_sc0","sc0_awayds")
#
#
# require('RH2')
# SC0_fixtures$sc0_awayds <- sqldf("SELECT sc0_awayds_lookup.sc0_awayds FROM sc0_awayds_lookup INNER JOIN SC0_fixtures ON sc0_awayds_lookup.AwayTeam_sc0 = SC0_fixtures.AwayTeam_sc0")
#
# SC0_fixtures$avg_AG_sc0 <- sc0_avg_AG
#
# sc0_awayas_lookup <- cbind(sc0_teams,sc0_away_as)
#
# sc0_awayas_lookup <- as.data.frame(sc0_awayas_lookup)
#
# colnames(sc0_awayas_lookup) <- c("AwayTeam_sc0","sc0_awayas")
#
#
# SC0_fixtures$sc0_awayas <- sqldf("SELECT sc0_awayas_lookup.sc0_awayas FROM sc0_awayas_lookup INNER JOIN SC0_fixtures ON sc0_awayas_lookup.AwayTeam_sc0 = SC0_fixtures.AwayTeam_sc0")
#
# SC0_fixtures$sc0_homeds <- rep(sc0_home_ds,each = length(sc0_teams)-1)
#
# SC0_fixtures$sc0_awayds <- as.numeric(unlist(SC0_fixtures$sc0_awayds))
# #xGH
# SC0_fixtures$sc0_xGH <- SC0_fixtures$avg_HG_sc0 * SC0_fixtures$sc0_homeas * SC0_fixtures$sc0_awayds
#
# #xGA
#
# SC0_fixtures$sc0_awayas <- as.numeric(unlist(SC0_fixtures$sc0_awayas))
#
# SC0_fixtures$sc0_xGA <- SC0_fixtures$avg_AG_sc0 * SC0_fixtures$sc0_awayas * SC0_fixtures$sc0_homeds
#
# SC0_fixtures$sc0_0_0 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_0 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_1 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_1 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_0 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_2 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_2 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_1 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_2 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_3 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_0 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_1 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_2 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_3 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_3 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_3 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_4 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_0 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_1 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_2 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_3 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_4 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_4 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_4 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_4 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_5 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_0 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_1 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_2 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_3 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_4 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_5 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_5 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_5 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_5 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_5 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_6 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_0 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(0,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_1 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(1,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_2 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(2,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_3 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(3,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_4 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(4,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_6_5 <- round(stats::dpois(6,SC0_fixtures$sc0_xGH) * stats::dpois(5,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_0_6 <- round(stats::dpois(0,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_1_6 <- round(stats::dpois(1,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_2_6 <- round(stats::dpois(2,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_3_6 <- round(stats::dpois(3,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_4_6 <- round(stats::dpois(4,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# SC0_fixtures$sc0_5_6 <- round(stats::dpois(5,SC0_fixtures$sc0_xGH) * stats::dpois(6,SC0_fixtures$sc0_xGA), digits = 4)
# #Home win
# SC0_fixtures$sc0_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 + SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5
# )
#
# SC0_fixtures$sc0_H <- percent(SC0_fixtures$sc0_H, accuracy = 0.1)
#
# #Draw
# SC0_fixtures$sc0_D <- (
#
#   SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 + SC0_fixtures$sc0_4_4 +
#     SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6
# )
#
# SC0_fixtures$sc0_D <- percent(SC0_fixtures$sc0_D, accuracy = 0.1)
#
# #Away
#
# SC0_fixtures$sc0_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 + SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6
# )
#
# SC0_fixtures$sc0_A <- percent(SC0_fixtures$sc0_A, accuracy = 0.1)
#
# #ov25
# SC0_fixtures$sc0_ov25 <- (
#   SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 + SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 + SC0_fixtures$sc0_0_4 +
#     SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 + SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_0 +
#     SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 + SC0_fixtures$sc0_0_5 +
#     SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 + SC0_fixtures$sc0_5_5 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5 + SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 +
#     SC0_fixtures$sc0_4_6 + SC0_fixtures$sc0_5_6 + SC0_fixtures$sc0_6_6
# )
# #un25
# SC0_fixtures$sc0_un25 <- (
#   SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_0_2
# )
# #odds
# SC0_fixtures$sc0_ov25_odds <- round((1/SC0_fixtures$sc0_ov25),digits = 2)
# SC0_fixtures$sc0_un25_odds <- round((1/SC0_fixtures$sc0_un25),digits = 2)
#
# SC0_fixtures$sc0_ov25_odds
# SC0_fixtures$sc0_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SC0_fixtures$sc0_BTTSY <- (
#   SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_3_1 + SC0_fixtures$sc0_3_2 +
#     SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_1_3 + SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_3_3 + SC0_fixtures$sc0_4_4 +
#     SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_3 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 +
#     SC0_fixtures$sc0_3_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 +
#     SC0_fixtures$sc0_5_4 + SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_6_6 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6
# )
# #BTTSN
# SC0_fixtures$sc0_BTTSN <- (
#   SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_0_2 +
#     SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_5_0 +
#     SC0_fixtures$sc0_0_5 + SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_0_6
# )
#
# SC0_fixtures$sc0_BTTSY_odds <- round((1/SC0_fixtures$sc0_BTTSY),digits = 2)
# SC0_fixtures$sc0_BTTSN_odds <- round((1/SC0_fixtures$sc0_BTTSN),digits = 2)
#
# SC0_fixtures$sc0_BTTSY <- percent(SC0_fixtures$sc0_BTTSY, accuracy = 0.1)
# SC0_fixtures$sc0_BTTSN <- percent(SC0_fixtures$sc0_BTTSN, accuracy = 0.1)
# #odds
# SC0_fixtures$sc0_BTTSY_odds
# SC0_fixtures$sc0_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SC0_fixtures$sc0_AH_0_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 +SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6
# )
# #AH_0_A
# SC0_fixtures$sc0_AH_0_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 +SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6
# )
#
# #odds
# SC0_fixtures$sc0_AH_0_H_odds <- round((1/SC0_fixtures$sc0_AH_0_H),digits = 2)
# SC0_fixtures$sc0_AH_0_A_odds <- round((1/SC0_fixtures$sc0_AH_0_A),digits = 2)
#
# SC0_fixtures$sc0_AH_0_H_odds
# SC0_fixtures$sc0_AH_0_A_odds
# #percentages
# SC0_fixtures$sc0_AH_0_H <- percent(SC0_fixtures$sc0_AH_0_H, accuracy = 0.1)
# SC0_fixtures$sc0_AH_0_A <- percent(SC0_fixtures$sc0_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SC0_fixtures$sc0_AH_n075_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 +SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5
# )
# #AH_n075_A
# SC0_fixtures$sc0_AH_n075_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 +SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6
# )
#
# #odds
# SC0_fixtures$sc0_AH_n075_H_odds <- round((1/SC0_fixtures$sc0_AH_n075_H),digits = 2)
# SC0_fixtures$sc0_AH_n075_A_odds <- round((1/SC0_fixtures$sc0_AH_n075_A),digits = 2)
#
# SC0_fixtures$sc0_AH_n075_H_odds
# SC0_fixtures$sc0_AH_n075_A_odds
# #percentages
# SC0_fixtures$sc0_AH_n075_H <- percent(SC0_fixtures$sc0_AH_n075_H, accuracy = 0.1)
# SC0_fixtures$sc0_AH_n075_A <- percent(SC0_fixtures$sc0_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SC0_fixtures$sc0_AH_075_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 +SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6 + SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_1_2 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_3_4 + SC0_fixtures$sc0_4_5 + SC0_fixtures$sc0_5_6
# )
# #AH_075_A
# SC0_fixtures$sc0_AH_075_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 +SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6 + SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_3 + SC0_fixtures$sc0_5_4 + SC0_fixtures$sc0_6_5
# )
#
# #odds
# SC0_fixtures$sc0_AH_075_H_odds <- round((1/SC0_fixtures$sc0_AH_075_H),digits = 2)
# SC0_fixtures$sc0_AH_075_A_odds <- round((1/SC0_fixtures$sc0_AH_075_A),digits = 2)
#
# SC0_fixtures$sc0_AH_075_H_odds
# SC0_fixtures$sc0_AH_075_A_odds
# #percentages
# SC0_fixtures$sc0_AH_075_H <- percent(SC0_fixtures$sc0_AH_075_H, accuracy = 0.1)
# SC0_fixtures$sc0_AH_075_A <- percent(SC0_fixtures$sc0_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SC0_fixtures$sc0_AH_n125_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 +SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5
# )
# #AH_n125_A
# SC0_fixtures$sc0_AH_n125_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 +SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6
# )
#
# #odds
# SC0_fixtures$sc0_AH_n125_H_odds <- round((1/SC0_fixtures$sc0_AH_n125_H),digits = 2)
# SC0_fixtures$sc0_AH_n125_A_odds <- round((1/SC0_fixtures$sc0_AH_n125_A),digits = 2)
#
# SC0_fixtures$sc0_AH_n125_H_odds
# SC0_fixtures$sc0_AH_n125_A_odds
# #percentages
# SC0_fixtures$sc0_AH_n125_H <- percent(SC0_fixtures$sc0_AH_n125_H, accuracy = 0.1)
# SC0_fixtures$sc0_AH_n125_A <- percent(SC0_fixtures$sc0_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SC0_fixtures$sc0_AH_125_H <- (
#   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#     SC0_fixtures$sc0_5_0 +SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#     SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#     SC0_fixtures$sc0_6_5 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6 + SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_1_2 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_3_4 + SC0_fixtures$sc0_4_5 + SC0_fixtures$sc0_5_6
# )
# #AH_125_A
# SC0_fixtures$sc0_AH_125_A <- (
#   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#     SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#     SC0_fixtures$sc0_0_5 +SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#     SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#     SC0_fixtures$sc0_5_6 + SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 +
#     SC0_fixtures$sc0_4_4 + SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6 + SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_1 +
#     SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_3 + SC0_fixtures$sc0_5_4 + SC0_fixtures$sc0_6_5
# )
#
# #odds
# SC0_fixtures$sc0_AH_125_H_odds <- round((1/SC0_fixtures$sc0_AH_125_H),digits = 2)
# SC0_fixtures$sc0_AH_125_A_odds <- round((1/SC0_fixtures$sc0_AH_125_A),digits = 2)
#
# SC0_fixtures$sc0_AH_125_H_odds
# SC0_fixtures$sc0_AH_125_A_odds
# #percentages
# SC0_fixtures$sc0_AH_125_H <- percent(SC0_fixtures$sc0_AH_125_H, accuracy = 0.1)
# SC0_fixtures$sc0_AH_125_A <- percent(SC0_fixtures$sc0_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SC0_fixtures$sc0_ov25 <- percent(SC0_fixtures$sc0_ov25, accuracy = 0.1)
#
# SC0_fixtures$sc0_un25 <- percent(SC0_fixtures$sc0_un25, accuracy = 0.1)
# SC0_fixtures$sc0_pscore <- paste(round(SC0_fixtures$sc0_xGH,digits = 0),round(SC0_fixtures$sc0_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SC0_fixtures,'Divisions/SC0.xlsx',sheetName = "SC0", append = TRUE)
# #####################################################################################################################
# #SC1
# HomeTeam_sc1 <- rep(sc1_teams, each = length(sc1_teams))
# AwayTeam_sc1 <- rep(sc1_teams, length(sc1_teams))
# SC1_fixtures <- cbind(HomeTeam_sc1,AwayTeam_sc1)
# SC1_fixtures <- as.data.frame(SC1_fixtures)
# SC1_fixtures <- SC1_fixtures[!SC1_fixtures$HomeTeam_sc1 == SC1_fixtures$AwayTeam_sc1,]
# rownames(SC1_fixtures) <- NULL
# SC1_fixtures$Div <- "SC1"
# SC1_fixtures <- SC1_fixtures[,c(3,1,2)]
#
# SC1_fixtures$avg_HG_sc1 <- sc1_avg_HG
#
# SC1_fixtures$sc1_homeas <- rep(sc1_home_as,each = length(sc1_teams)-1)
#
# sc1_awayds_lookup <- cbind(sc1_teams,sc1_away_ds)
#
# sc1_awayds_lookup <- as.data.frame(sc1_awayds_lookup)
#
# colnames(sc1_awayds_lookup) <- c("AwayTeam_sc1","sc1_awayds")
#
#
# require('RH2')
# SC1_fixtures$sc1_awayds <- sqldf("SELECT sc1_awayds_lookup.sc1_awayds FROM sc1_awayds_lookup INNER JOIN SC1_fixtures ON sc1_awayds_lookup.AwayTeam_sc1 = SC1_fixtures.AwayTeam_sc1")
#
# SC1_fixtures$avg_AG_sc1 <- sc1_avg_AG
#
# sc1_awayas_lookup <- cbind(sc1_teams,sc1_away_as)
#
# sc1_awayas_lookup <- as.data.frame(sc1_awayas_lookup)
#
# colnames(sc1_awayas_lookup) <- c("AwayTeam_sc1","sc1_awayas")
#
#
# SC1_fixtures$sc1_awayas <- sqldf("SELECT sc1_awayas_lookup.sc1_awayas FROM sc1_awayas_lookup INNER JOIN SC1_fixtures ON sc1_awayas_lookup.AwayTeam_sc1 = SC1_fixtures.AwayTeam_sc1")
#
# SC1_fixtures$sc1_homeds <- rep(sc1_home_ds,each = length(sc1_teams)-1)
#
# SC1_fixtures$sc1_awayds <- as.numeric(unlist(SC1_fixtures$sc1_awayds))
# #xGH
# SC1_fixtures$sc1_xGH <- SC1_fixtures$avg_HG_sc1 * SC1_fixtures$sc1_homeas * SC1_fixtures$sc1_awayds
#
# #xGA
#
# SC1_fixtures$sc1_awayas <- as.numeric(unlist(SC1_fixtures$sc1_awayas))
#
# SC1_fixtures$sc1_xGA <- SC1_fixtures$avg_AG_sc1 * SC1_fixtures$sc1_awayas * SC1_fixtures$sc1_homeds
#
# SC1_fixtures$sc1_0_0 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_0 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_1 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_1 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_0 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_2 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_2 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_1 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_2 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_3 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_0 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_1 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_2 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_3 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_3 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_3 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_4 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_0 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_1 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_2 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_3 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_4 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_4 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_4 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_4 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_5 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_0 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_1 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_2 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_3 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_4 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_5 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_5 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_5 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_5 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_5 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_6 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_0 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(0,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_1 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(1,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_2 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(2,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_3 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(3,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_4 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(4,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_6_5 <- round(stats::dpois(6,SC1_fixtures$sc1_xGH) * stats::dpois(5,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_0_6 <- round(stats::dpois(0,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_1_6 <- round(stats::dpois(1,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_2_6 <- round(stats::dpois(2,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_3_6 <- round(stats::dpois(3,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_4_6 <- round(stats::dpois(4,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# SC1_fixtures$sc1_5_6 <- round(stats::dpois(5,SC1_fixtures$sc1_xGH) * stats::dpois(6,SC1_fixtures$sc1_xGA), digits = 4)
# #Home win
# SC1_fixtures$sc1_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 + SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5
# )
#
# SC1_fixtures$sc1_H <- percent(SC1_fixtures$sc1_H, accuracy = 0.1)
#
# #Draw
# SC1_fixtures$sc1_D <- (
#
#   SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 + SC1_fixtures$sc1_4_4 +
#     SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6
# )
#
# SC1_fixtures$sc1_D <- percent(SC1_fixtures$sc1_D, accuracy = 0.1)
#
# #Away
#
# SC1_fixtures$sc1_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 + SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6
# )
#
# SC1_fixtures$sc1_A <- percent(SC1_fixtures$sc1_A, accuracy = 0.1)
#
# #ov25
# SC1_fixtures$sc1_ov25 <- (
#   SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 + SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 + SC1_fixtures$sc1_0_4 +
#     SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 + SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_0 +
#     SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 + SC1_fixtures$sc1_0_5 +
#     SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 + SC1_fixtures$sc1_5_5 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5 + SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 +
#     SC1_fixtures$sc1_4_6 + SC1_fixtures$sc1_5_6 + SC1_fixtures$sc1_6_6
# )
# #un25
# SC1_fixtures$sc1_un25 <- (
#   SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_0_2
# )
# #odds
# SC1_fixtures$sc1_ov25_odds <- round((1/SC1_fixtures$sc1_ov25),digits = 2)
# SC1_fixtures$sc1_un25_odds <- round((1/SC1_fixtures$sc1_un25),digits = 2)
#
# SC1_fixtures$sc1_ov25_odds
# SC1_fixtures$sc1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SC1_fixtures$sc1_BTTSY <- (
#   SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_3_1 + SC1_fixtures$sc1_3_2 +
#     SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_1_3 + SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_3_3 + SC1_fixtures$sc1_4_4 +
#     SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_3 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 +
#     SC1_fixtures$sc1_3_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 +
#     SC1_fixtures$sc1_5_4 + SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_6_6 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6
# )
# #BTTSN
# SC1_fixtures$sc1_BTTSN <- (
#   SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_0_2 +
#     SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_5_0 +
#     SC1_fixtures$sc1_0_5 + SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_0_6
# )
#
# SC1_fixtures$sc1_BTTSY_odds <- round((1/SC1_fixtures$sc1_BTTSY),digits = 2)
# SC1_fixtures$sc1_BTTSN_odds <- round((1/SC1_fixtures$sc1_BTTSN),digits = 2)
#
# SC1_fixtures$sc1_BTTSY <- percent(SC1_fixtures$sc1_BTTSY, accuracy = 0.1)
# SC1_fixtures$sc1_BTTSN <- percent(SC1_fixtures$sc1_BTTSN, accuracy = 0.1)
# #odds
# SC1_fixtures$sc1_BTTSY_odds
# SC1_fixtures$sc1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SC1_fixtures$sc1_AH_0_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 +SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6
# )
# #AH_0_A
# SC1_fixtures$sc1_AH_0_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 +SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6
# )
#
# #odds
# SC1_fixtures$sc1_AH_0_H_odds <- round((1/SC1_fixtures$sc1_AH_0_H),digits = 2)
# SC1_fixtures$sc1_AH_0_A_odds <- round((1/SC1_fixtures$sc1_AH_0_A),digits = 2)
#
# SC1_fixtures$sc1_AH_0_H_odds
# SC1_fixtures$sc1_AH_0_A_odds
# #percentages
# SC1_fixtures$sc1_AH_0_H <- percent(SC1_fixtures$sc1_AH_0_H, accuracy = 0.1)
# SC1_fixtures$sc1_AH_0_A <- percent(SC1_fixtures$sc1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SC1_fixtures$sc1_AH_n075_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 +SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5
# )
# #AH_n075_A
# SC1_fixtures$sc1_AH_n075_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 +SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6
# )
#
# #odds
# SC1_fixtures$sc1_AH_n075_H_odds <- round((1/SC1_fixtures$sc1_AH_n075_H),digits = 2)
# SC1_fixtures$sc1_AH_n075_A_odds <- round((1/SC1_fixtures$sc1_AH_n075_A),digits = 2)
#
# SC1_fixtures$sc1_AH_n075_H_odds
# SC1_fixtures$sc1_AH_n075_A_odds
# #percentages
# SC1_fixtures$sc1_AH_n075_H <- percent(SC1_fixtures$sc1_AH_n075_H, accuracy = 0.1)
# SC1_fixtures$sc1_AH_n075_A <- percent(SC1_fixtures$sc1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SC1_fixtures$sc1_AH_075_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 +SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6 + SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_1_2 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_3_4 + SC1_fixtures$sc1_4_5 + SC1_fixtures$sc1_5_6
# )
# #AH_075_A
# SC1_fixtures$sc1_AH_075_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 +SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6 + SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_3 + SC1_fixtures$sc1_5_4 + SC1_fixtures$sc1_6_5
# )
#
# #odds
# SC1_fixtures$sc1_AH_075_H_odds <- round((1/SC1_fixtures$sc1_AH_075_H),digits = 2)
# SC1_fixtures$sc1_AH_075_A_odds <- round((1/SC1_fixtures$sc1_AH_075_A),digits = 2)
#
# SC1_fixtures$sc1_AH_075_H_odds
# SC1_fixtures$sc1_AH_075_A_odds
# #percentages
# SC1_fixtures$sc1_AH_075_H <- percent(SC1_fixtures$sc1_AH_075_H, accuracy = 0.1)
# SC1_fixtures$sc1_AH_075_A <- percent(SC1_fixtures$sc1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SC1_fixtures$sc1_AH_n125_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 +SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5
# )
# #AH_n125_A
# SC1_fixtures$sc1_AH_n125_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 +SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6
# )
#
# #odds
# SC1_fixtures$sc1_AH_n125_H_odds <- round((1/SC1_fixtures$sc1_AH_n125_H),digits = 2)
# SC1_fixtures$sc1_AH_n125_A_odds <- round((1/SC1_fixtures$sc1_AH_n125_A),digits = 2)
#
# SC1_fixtures$sc1_AH_n125_H_odds
# SC1_fixtures$sc1_AH_n125_A_odds
# #percentages
# SC1_fixtures$sc1_AH_n125_H <- percent(SC1_fixtures$sc1_AH_n125_H, accuracy = 0.1)
# SC1_fixtures$sc1_AH_n125_A <- percent(SC1_fixtures$sc1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SC1_fixtures$sc1_AH_125_H <- (
#   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#     SC1_fixtures$sc1_5_0 +SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#     SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#     SC1_fixtures$sc1_6_5 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6 + SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_1_2 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_3_4 + SC1_fixtures$sc1_4_5 + SC1_fixtures$sc1_5_6
# )
# #AH_125_A
# SC1_fixtures$sc1_AH_125_A <- (
#   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#     SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#     SC1_fixtures$sc1_0_5 +SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#     SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#     SC1_fixtures$sc1_5_6 + SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 +
#     SC1_fixtures$sc1_4_4 + SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6 + SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_1 +
#     SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_3 + SC1_fixtures$sc1_5_4 + SC1_fixtures$sc1_6_5
# )
#
# #odds
# SC1_fixtures$sc1_AH_125_H_odds <- round((1/SC1_fixtures$sc1_AH_125_H),digits = 2)
# SC1_fixtures$sc1_AH_125_A_odds <- round((1/SC1_fixtures$sc1_AH_125_A),digits = 2)
#
# SC1_fixtures$sc1_AH_125_H_odds
# SC1_fixtures$sc1_AH_125_A_odds
# #percentages
# SC1_fixtures$sc1_AH_125_H <- percent(SC1_fixtures$sc1_AH_125_H, accuracy = 0.1)
# SC1_fixtures$sc1_AH_125_A <- percent(SC1_fixtures$sc1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SC1_fixtures$sc1_ov25 <- percent(SC1_fixtures$sc1_ov25, accuracy = 0.1)
#
# SC1_fixtures$sc1_un25 <- percent(SC1_fixtures$sc1_un25, accuracy = 0.1)
# SC1_fixtures$sc1_pscore <- paste(round(SC1_fixtures$sc1_xGH,digits = 0),round(SC1_fixtures$sc1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SC1_fixtures,'Divisions/SC1.xlsx',sheetName = "SC1", append = TRUE)
# #######################################################################################################################
# #SC2
# HomeTeam_sc2 <- rep(sc2_teams, each = length(sc2_teams))
# AwayTeam_sc2 <- rep(sc2_teams, length(sc2_teams))
# SC2_fixtures <- cbind(HomeTeam_sc2,AwayTeam_sc2)
# SC2_fixtures <- as.data.frame(SC2_fixtures)
# SC2_fixtures <- SC2_fixtures[!SC2_fixtures$HomeTeam_sc2 == SC2_fixtures$AwayTeam_sc2,]
# rownames(SC2_fixtures) <- NULL
# SC2_fixtures$Div <- "SC2"
# SC2_fixtures <- SC2_fixtures[,c(3,1,2)]
#
# SC2_fixtures$avg_HG_sc2 <- sc2_avg_HG
#
# SC2_fixtures$sc2_homeas <- rep(sc2_home_as,each = length(sc2_teams)-1)
#
# sc2_awayds_lookup <- cbind(sc2_teams,sc2_away_ds)
#
# sc2_awayds_lookup <- as.data.frame(sc2_awayds_lookup)
#
# colnames(sc2_awayds_lookup) <- c("AwayTeam_sc2","sc2_awayds")
#
#
# require('RH2')
# SC2_fixtures$sc2_awayds <- sqldf("SELECT sc2_awayds_lookup.sc2_awayds FROM sc2_awayds_lookup INNER JOIN SC2_fixtures ON sc2_awayds_lookup.AwayTeam_sc2 = SC2_fixtures.AwayTeam_sc2")
#
# SC2_fixtures$avg_AG_sc2 <- sc2_avg_AG
#
# sc2_awayas_lookup <- cbind(sc2_teams,sc2_away_as)
#
# sc2_awayas_lookup <- as.data.frame(sc2_awayas_lookup)
#
# colnames(sc2_awayas_lookup) <- c("AwayTeam_sc2","sc2_awayas")
#
#
# SC2_fixtures$sc2_awayas <- sqldf("SELECT sc2_awayas_lookup.sc2_awayas FROM sc2_awayas_lookup INNER JOIN SC2_fixtures ON sc2_awayas_lookup.AwayTeam_sc2 = SC2_fixtures.AwayTeam_sc2")
#
# SC2_fixtures$sc2_homeds <- rep(sc2_home_ds,each = length(sc2_teams)-1)
#
# SC2_fixtures$sc2_awayds <- as.numeric(unlist(SC2_fixtures$sc2_awayds))
# #xGH
# SC2_fixtures$sc2_xGH <- SC2_fixtures$avg_HG_sc2 * SC2_fixtures$sc2_homeas * SC2_fixtures$sc2_awayds
#
# #xGA
#
# SC2_fixtures$sc2_awayas <- as.numeric(unlist(SC2_fixtures$sc2_awayas))
#
# SC2_fixtures$sc2_xGA <- SC2_fixtures$avg_AG_sc2 * SC2_fixtures$sc2_awayas * SC2_fixtures$sc2_homeds
#
# SC2_fixtures$sc2_0_0 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_0 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_1 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_1 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_0 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_2 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_2 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_1 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_2 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_3 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_0 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_1 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_2 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_3 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_3 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_3 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_4 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_0 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_1 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_2 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_3 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_4 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_4 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_4 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_4 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_5 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_0 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_1 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_2 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_3 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_4 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_5 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_5 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_5 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_5 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_5 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_6 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_0 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(0,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_1 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(1,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_2 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(2,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_3 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(3,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_4 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(4,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_6_5 <- round(stats::dpois(6,SC2_fixtures$sc2_xGH) * stats::dpois(5,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_0_6 <- round(stats::dpois(0,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_1_6 <- round(stats::dpois(1,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_2_6 <- round(stats::dpois(2,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_3_6 <- round(stats::dpois(3,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_4_6 <- round(stats::dpois(4,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# SC2_fixtures$sc2_5_6 <- round(stats::dpois(5,SC2_fixtures$sc2_xGH) * stats::dpois(6,SC2_fixtures$sc2_xGA), digits = 4)
# #Home win
# SC2_fixtures$sc2_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 + SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5
# )
#
# SC2_fixtures$sc2_H <- percent(SC2_fixtures$sc2_H, accuracy = 0.1)
#
# #Draw
# SC2_fixtures$sc2_D <- (
#
#   SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 + SC2_fixtures$sc2_4_4 +
#     SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6
# )
#
# SC2_fixtures$sc2_D <- percent(SC2_fixtures$sc2_D, accuracy = 0.1)
#
# #Away
#
# SC2_fixtures$sc2_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 + SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6
# )
#
# SC2_fixtures$sc2_A <- percent(SC2_fixtures$sc2_A, accuracy = 0.1)
#
# #ov25
# SC2_fixtures$sc2_ov25 <- (
#   SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 + SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 + SC2_fixtures$sc2_0_4 +
#     SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 + SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_0 +
#     SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 + SC2_fixtures$sc2_0_5 +
#     SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 + SC2_fixtures$sc2_5_5 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5 + SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 +
#     SC2_fixtures$sc2_4_6 + SC2_fixtures$sc2_5_6 + SC2_fixtures$sc2_6_6
# )
# #un25
# SC2_fixtures$sc2_un25 <- (
#   SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_0_2
# )
# #odds
# SC2_fixtures$sc2_ov25_odds <- round((1/SC2_fixtures$sc2_ov25),digits = 2)
# SC2_fixtures$sc2_un25_odds <- round((1/SC2_fixtures$sc2_un25),digits = 2)
#
# SC2_fixtures$sc2_ov25_odds
# SC2_fixtures$sc2_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SC2_fixtures$sc2_BTTSY <- (
#   SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_3_1 + SC2_fixtures$sc2_3_2 +
#     SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_1_3 + SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_3_3 + SC2_fixtures$sc2_4_4 +
#     SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_3 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 +
#     SC2_fixtures$sc2_3_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 +
#     SC2_fixtures$sc2_5_4 + SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_6_6 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6
# )
# #BTTSN
# SC2_fixtures$sc2_BTTSN <- (
#   SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_0_2 +
#     SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_5_0 +
#     SC2_fixtures$sc2_0_5 + SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_0_6
# )
#
# SC2_fixtures$sc2_BTTSY_odds <- round((1/SC2_fixtures$sc2_BTTSY),digits = 2)
# SC2_fixtures$sc2_BTTSN_odds <- round((1/SC2_fixtures$sc2_BTTSN),digits = 2)
#
# SC2_fixtures$sc2_BTTSY <- percent(SC2_fixtures$sc2_BTTSY, accuracy = 0.1)
# SC2_fixtures$sc2_BTTSN <- percent(SC2_fixtures$sc2_BTTSN, accuracy = 0.1)
# #odds
# SC2_fixtures$sc2_BTTSY_odds
# SC2_fixtures$sc2_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SC2_fixtures$sc2_AH_0_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 +SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6
# )
# #AH_0_A
# SC2_fixtures$sc2_AH_0_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 +SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6
# )
#
# #odds
# SC2_fixtures$sc2_AH_0_H_odds <- round((1/SC2_fixtures$sc2_AH_0_H),digits = 2)
# SC2_fixtures$sc2_AH_0_A_odds <- round((1/SC2_fixtures$sc2_AH_0_A),digits = 2)
#
# SC2_fixtures$sc2_AH_0_H_odds
# SC2_fixtures$sc2_AH_0_A_odds
# #percentages
# SC2_fixtures$sc2_AH_0_H <- percent(SC2_fixtures$sc2_AH_0_H, accuracy = 0.1)
# SC2_fixtures$sc2_AH_0_A <- percent(SC2_fixtures$sc2_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SC2_fixtures$sc2_AH_n075_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 +SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5
# )
# #AH_n075_A
# SC2_fixtures$sc2_AH_n075_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 +SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6
# )
#
# #odds
# SC2_fixtures$sc2_AH_n075_H_odds <- round((1/SC2_fixtures$sc2_AH_n075_H),digits = 2)
# SC2_fixtures$sc2_AH_n075_A_odds <- round((1/SC2_fixtures$sc2_AH_n075_A),digits = 2)
#
# SC2_fixtures$sc2_AH_n075_H_odds
# SC2_fixtures$sc2_AH_n075_A_odds
# #percentages
# SC2_fixtures$sc2_AH_n075_H <- percent(SC2_fixtures$sc2_AH_n075_H, accuracy = 0.1)
# SC2_fixtures$sc2_AH_n075_A <- percent(SC2_fixtures$sc2_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SC2_fixtures$sc2_AH_075_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 +SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6 + SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_1_2 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_3_4 + SC2_fixtures$sc2_4_5 + SC2_fixtures$sc2_5_6
# )
# #AH_075_A
# SC2_fixtures$sc2_AH_075_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 +SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6 + SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_3 + SC2_fixtures$sc2_5_4 + SC2_fixtures$sc2_6_5
# )
#
# #odds
# SC2_fixtures$sc2_AH_075_H_odds <- round((1/SC2_fixtures$sc2_AH_075_H),digits = 2)
# SC2_fixtures$sc2_AH_075_A_odds <- round((1/SC2_fixtures$sc2_AH_075_A),digits = 2)
#
# SC2_fixtures$sc2_AH_075_H_odds
# SC2_fixtures$sc2_AH_075_A_odds
# #percentages
# SC2_fixtures$sc2_AH_075_H <- percent(SC2_fixtures$sc2_AH_075_H, accuracy = 0.1)
# SC2_fixtures$sc2_AH_075_A <- percent(SC2_fixtures$sc2_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SC2_fixtures$sc2_AH_n125_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 +SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5
# )
# #AH_n125_A
# SC2_fixtures$sc2_AH_n125_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 +SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6
# )
#
# #odds
# SC2_fixtures$sc2_AH_n125_H_odds <- round((1/SC2_fixtures$sc2_AH_n125_H),digits = 2)
# SC2_fixtures$sc2_AH_n125_A_odds <- round((1/SC2_fixtures$sc2_AH_n125_A),digits = 2)
#
# SC2_fixtures$sc2_AH_n125_H_odds
# SC2_fixtures$sc2_AH_n125_A_odds
# #percentages
# SC2_fixtures$sc2_AH_n125_H <- percent(SC2_fixtures$sc2_AH_n125_H, accuracy = 0.1)
# SC2_fixtures$sc2_AH_n125_A <- percent(SC2_fixtures$sc2_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SC2_fixtures$sc2_AH_125_H <- (
#   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#     SC2_fixtures$sc2_5_0 +SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#     SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#     SC2_fixtures$sc2_6_5 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6 + SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_1_2 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_3_4 + SC2_fixtures$sc2_4_5 + SC2_fixtures$sc2_5_6
# )
# #AH_125_A
# SC2_fixtures$sc2_AH_125_A <- (
#   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#     SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#     SC2_fixtures$sc2_0_5 +SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#     SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#     SC2_fixtures$sc2_5_6 + SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 +
#     SC2_fixtures$sc2_4_4 + SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6 + SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_1 +
#     SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_3 + SC2_fixtures$sc2_5_4 + SC2_fixtures$sc2_6_5
# )
#
# #odds
# SC2_fixtures$sc2_AH_125_H_odds <- round((1/SC2_fixtures$sc2_AH_125_H),digits = 2)
# SC2_fixtures$sc2_AH_125_A_odds <- round((1/SC2_fixtures$sc2_AH_125_A),digits = 2)
#
# SC2_fixtures$sc2_AH_125_H_odds
# SC2_fixtures$sc2_AH_125_A_odds
# #percentages
# SC2_fixtures$sc2_AH_125_H <- percent(SC2_fixtures$sc2_AH_125_H, accuracy = 0.1)
# SC2_fixtures$sc2_AH_125_A <- percent(SC2_fixtures$sc2_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SC2_fixtures$sc2_ov25 <- percent(SC2_fixtures$sc2_ov25, accuracy = 0.1)
#
# SC2_fixtures$sc2_un25 <- percent(SC2_fixtures$sc2_un25, accuracy = 0.1)
# SC2_fixtures$sc2_pscore <- paste(round(SC2_fixtures$sc2_xGH,digits = 0),round(SC2_fixtures$sc2_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SC2_fixtures,'Divisions/SC2.xlsx',sheetName = "SC2", append = TRUE)
# ##########################################################################################################################################
# #SC3
# HomeTeam_sc3 <- rep(sc3_teams, each = length(sc3_teams))
# AwayTeam_sc3 <- rep(sc3_teams, length(sc3_teams))
# SC3_fixtures <- cbind(HomeTeam_sc3,AwayTeam_sc3)
# SC3_fixtures <- as.data.frame(SC3_fixtures)
# SC3_fixtures <- SC3_fixtures[!SC3_fixtures$HomeTeam_sc3 == SC3_fixtures$AwayTeam_sc3,]
# rownames(SC3_fixtures) <- NULL
# SC3_fixtures$Div <- "SC3"
# SC3_fixtures <- SC3_fixtures[,c(3,1,2)]
#
# SC3_fixtures$avg_HG_sc3 <- sc3_avg_HG
#
# SC3_fixtures$sc3_homeas <- rep(sc3_home_as,each = length(sc3_teams)-1)
#
# sc3_awayds_lookup <- cbind(sc3_teams,sc3_away_ds)
#
# sc3_awayds_lookup <- as.data.frame(sc3_awayds_lookup)
#
# colnames(sc3_awayds_lookup) <- c("AwayTeam_sc3","sc3_awayds")
#
#
# require('RH2')
# SC3_fixtures$sc3_awayds <- sqldf("SELECT sc3_awayds_lookup.sc3_awayds FROM sc3_awayds_lookup INNER JOIN SC3_fixtures ON sc3_awayds_lookup.AwayTeam_sc3 = SC3_fixtures.AwayTeam_sc3")
#
# SC3_fixtures$avg_AG_sc3 <- sc3_avg_AG
#
# sc3_awayas_lookup <- cbind(sc3_teams,sc3_away_as)
#
# sc3_awayas_lookup <- as.data.frame(sc3_awayas_lookup)
#
# colnames(sc3_awayas_lookup) <- c("AwayTeam_sc3","sc3_awayas")
#
#
# SC3_fixtures$sc3_awayas <- sqldf("SELECT sc3_awayas_lookup.sc3_awayas FROM sc3_awayas_lookup INNER JOIN SC3_fixtures ON sc3_awayas_lookup.AwayTeam_sc3 = SC3_fixtures.AwayTeam_sc3")
#
# SC3_fixtures$sc3_homeds <- rep(sc3_home_ds,each = length(sc3_teams)-1)
#
# SC3_fixtures$sc3_awayds <- as.numeric(unlist(SC3_fixtures$sc3_awayds))
# #xGH
# SC3_fixtures$sc3_xGH <- SC3_fixtures$avg_HG_sc3 * SC3_fixtures$sc3_homeas * SC3_fixtures$sc3_awayds
#
# #xGA
#
# SC3_fixtures$sc3_awayas <- as.numeric(unlist(SC3_fixtures$sc3_awayas))
#
# SC3_fixtures$sc3_xGA <- SC3_fixtures$avg_AG_sc3 * SC3_fixtures$sc3_awayas * SC3_fixtures$sc3_homeds
#
# SC3_fixtures$sc3_0_0 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_0 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_1 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_1 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_0 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_2 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_2 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_1 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_2 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_3 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_0 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_1 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_2 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_3 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_3 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_3 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_4 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_0 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_1 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_2 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_3 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_4 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_4 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_4 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_4 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_5 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_0 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_1 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_2 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_3 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_4 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_5 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_5 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_5 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_5 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_5 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_6 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_0 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(0,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_1 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(1,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_2 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(2,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_3 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(3,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_4 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(4,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_6_5 <- round(stats::dpois(6,SC3_fixtures$sc3_xGH) * stats::dpois(5,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_0_6 <- round(stats::dpois(0,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_1_6 <- round(stats::dpois(1,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_2_6 <- round(stats::dpois(2,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_3_6 <- round(stats::dpois(3,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_4_6 <- round(stats::dpois(4,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# SC3_fixtures$sc3_5_6 <- round(stats::dpois(5,SC3_fixtures$sc3_xGH) * stats::dpois(6,SC3_fixtures$sc3_xGA), digits = 4)
# #Home win
# SC3_fixtures$sc3_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 + SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5
# )
#
# SC3_fixtures$sc3_H <- percent(SC3_fixtures$sc3_H, accuracy = 0.1)
#
# #Draw
# SC3_fixtures$sc3_D <- (
#
#   SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 + SC3_fixtures$sc3_4_4 +
#     SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6
# )
#
# SC3_fixtures$sc3_D <- percent(SC3_fixtures$sc3_D, accuracy = 0.1)
#
# #Away
#
# SC3_fixtures$sc3_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 + SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6
# )
#
# SC3_fixtures$sc3_A <- percent(SC3_fixtures$sc3_A, accuracy = 0.1)
#
# #ov25
# SC3_fixtures$sc3_ov25 <- (
#   SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 + SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 + SC3_fixtures$sc3_0_4 +
#     SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 + SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_0 +
#     SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 + SC3_fixtures$sc3_0_5 +
#     SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 + SC3_fixtures$sc3_5_5 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5 + SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 +
#     SC3_fixtures$sc3_4_6 + SC3_fixtures$sc3_5_6 + SC3_fixtures$sc3_6_6
# )
# #un25
# SC3_fixtures$sc3_un25 <- (
#   SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_0_2
# )
# #odds
# SC3_fixtures$sc3_ov25_odds <- round((1/SC3_fixtures$sc3_ov25),digits = 2)
# SC3_fixtures$sc3_un25_odds <- round((1/SC3_fixtures$sc3_un25),digits = 2)
#
# SC3_fixtures$sc3_ov25_odds
# SC3_fixtures$sc3_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# SC3_fixtures$sc3_BTTSY <- (
#   SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_3_1 + SC3_fixtures$sc3_3_2 +
#     SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_1_3 + SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_3_3 + SC3_fixtures$sc3_4_4 +
#     SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_3 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 +
#     SC3_fixtures$sc3_3_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 +
#     SC3_fixtures$sc3_5_4 + SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_6_6 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6
# )
# #BTTSN
# SC3_fixtures$sc3_BTTSN <- (
#   SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_0_2 +
#     SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_5_0 +
#     SC3_fixtures$sc3_0_5 + SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_0_6
# )
#
# SC3_fixtures$sc3_BTTSY_odds <- round((1/SC3_fixtures$sc3_BTTSY),digits = 2)
# SC3_fixtures$sc3_BTTSN_odds <- round((1/SC3_fixtures$sc3_BTTSN),digits = 2)
#
# SC3_fixtures$sc3_BTTSY <- percent(SC3_fixtures$sc3_BTTSY, accuracy = 0.1)
# SC3_fixtures$sc3_BTTSN <- percent(SC3_fixtures$sc3_BTTSN, accuracy = 0.1)
# #odds
# SC3_fixtures$sc3_BTTSY_odds
# SC3_fixtures$sc3_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# SC3_fixtures$sc3_AH_0_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 +SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6
# )
# #AH_0_A
# SC3_fixtures$sc3_AH_0_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 +SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6
# )
#
# #odds
# SC3_fixtures$sc3_AH_0_H_odds <- round((1/SC3_fixtures$sc3_AH_0_H),digits = 2)
# SC3_fixtures$sc3_AH_0_A_odds <- round((1/SC3_fixtures$sc3_AH_0_A),digits = 2)
#
# SC3_fixtures$sc3_AH_0_H_odds
# SC3_fixtures$sc3_AH_0_A_odds
# #percentages
# SC3_fixtures$sc3_AH_0_H <- percent(SC3_fixtures$sc3_AH_0_H, accuracy = 0.1)
# SC3_fixtures$sc3_AH_0_A <- percent(SC3_fixtures$sc3_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# SC3_fixtures$sc3_AH_n075_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 +SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5
# )
# #AH_n075_A
# SC3_fixtures$sc3_AH_n075_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 +SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6
# )
#
# #odds
# SC3_fixtures$sc3_AH_n075_H_odds <- round((1/SC3_fixtures$sc3_AH_n075_H),digits = 2)
# SC3_fixtures$sc3_AH_n075_A_odds <- round((1/SC3_fixtures$sc3_AH_n075_A),digits = 2)
#
# SC3_fixtures$sc3_AH_n075_H_odds
# SC3_fixtures$sc3_AH_n075_A_odds
# #percentages
# SC3_fixtures$sc3_AH_n075_H <- percent(SC3_fixtures$sc3_AH_n075_H, accuracy = 0.1)
# SC3_fixtures$sc3_AH_n075_A <- percent(SC3_fixtures$sc3_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# SC3_fixtures$sc3_AH_075_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 +SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6 + SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_1_2 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_3_4 + SC3_fixtures$sc3_4_5 + SC3_fixtures$sc3_5_6
# )
# #AH_075_A
# SC3_fixtures$sc3_AH_075_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 +SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6 + SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_3 + SC3_fixtures$sc3_5_4 + SC3_fixtures$sc3_6_5
# )
#
# #odds
# SC3_fixtures$sc3_AH_075_H_odds <- round((1/SC3_fixtures$sc3_AH_075_H),digits = 2)
# SC3_fixtures$sc3_AH_075_A_odds <- round((1/SC3_fixtures$sc3_AH_075_A),digits = 2)
#
# SC3_fixtures$sc3_AH_075_H_odds
# SC3_fixtures$sc3_AH_075_A_odds
# #percentages
# SC3_fixtures$sc3_AH_075_H <- percent(SC3_fixtures$sc3_AH_075_H, accuracy = 0.1)
# SC3_fixtures$sc3_AH_075_A <- percent(SC3_fixtures$sc3_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# SC3_fixtures$sc3_AH_n125_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 +SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5
# )
# #AH_n125_A
# SC3_fixtures$sc3_AH_n125_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 +SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6
# )
#
# #odds
# SC3_fixtures$sc3_AH_n125_H_odds <- round((1/SC3_fixtures$sc3_AH_n125_H),digits = 2)
# SC3_fixtures$sc3_AH_n125_A_odds <- round((1/SC3_fixtures$sc3_AH_n125_A),digits = 2)
#
# SC3_fixtures$sc3_AH_n125_H_odds
# SC3_fixtures$sc3_AH_n125_A_odds
# #percentages
# SC3_fixtures$sc3_AH_n125_H <- percent(SC3_fixtures$sc3_AH_n125_H, accuracy = 0.1)
# SC3_fixtures$sc3_AH_n125_A <- percent(SC3_fixtures$sc3_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# SC3_fixtures$sc3_AH_125_H <- (
#   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#     SC3_fixtures$sc3_5_0 +SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#     SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#     SC3_fixtures$sc3_6_5 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6 + SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_1_2 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_3_4 + SC3_fixtures$sc3_4_5 + SC3_fixtures$sc3_5_6
# )
# #AH_125_A
# SC3_fixtures$sc3_AH_125_A <- (
#   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#     SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#     SC3_fixtures$sc3_0_5 +SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#     SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#     SC3_fixtures$sc3_5_6 + SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 +
#     SC3_fixtures$sc3_4_4 + SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6 + SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_1 +
#     SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_3 + SC3_fixtures$sc3_5_4 + SC3_fixtures$sc3_6_5
# )
#
# #odds
# SC3_fixtures$sc3_AH_125_H_odds <- round((1/SC3_fixtures$sc3_AH_125_H),digits = 2)
# SC3_fixtures$sc3_AH_125_A_odds <- round((1/SC3_fixtures$sc3_AH_125_A),digits = 2)
#
# SC3_fixtures$sc3_AH_125_H_odds
# SC3_fixtures$sc3_AH_125_A_odds
# #percentages
# SC3_fixtures$sc3_AH_125_H <- percent(SC3_fixtures$sc3_AH_125_H, accuracy = 0.1)
# SC3_fixtures$sc3_AH_125_A <- percent(SC3_fixtures$sc3_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# SC3_fixtures$sc3_ov25 <- percent(SC3_fixtures$sc3_ov25, accuracy = 0.1)
#
# SC3_fixtures$sc3_un25 <- percent(SC3_fixtures$sc3_un25, accuracy = 0.1)
# SC3_fixtures$sc3_pscore <- paste(round(SC3_fixtures$sc3_xGH,digits = 0),round(SC3_fixtures$sc3_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(SC3_fixtures,'Divisions/SC3.xlsx',sheetName = "SC3", append = TRUE)
# ######################################################################################################################################
# #T1
# HomeTeam_t1 <- rep(t1_teams, each = length(t1_teams))
# AwayTeam_t1 <- rep(t1_teams, length(t1_teams))
# T1_fixtures <- cbind(HomeTeam_t1,AwayTeam_t1)
# T1_fixtures <- as.data.frame(T1_fixtures)
# T1_fixtures <- T1_fixtures[!T1_fixtures$HomeTeam_t1 == T1_fixtures$AwayTeam_t1,]
# rownames(T1_fixtures) <- NULL
# T1_fixtures$Div <- "T1"
# T1_fixtures <- T1_fixtures[,c(3,1,2)]
#
# T1_fixtures$avg_HG_t1 <- t1_avg_HG
#
# T1_fixtures$t1_homeas <- rep(t1_home_as,each = length(t1_teams)-1)
#
# t1_awayds_lookup <- cbind(t1_teams,t1_away_ds)
#
# t1_awayds_lookup <- as.data.frame(t1_awayds_lookup)
#
# colnames(t1_awayds_lookup) <- c("AwayTeam_t1","t1_awayds")
#
#
# require('RH2')
# T1_fixtures$t1_awayds <- sqldf("SELECT t1_awayds_lookup.t1_awayds FROM t1_awayds_lookup INNER JOIN T1_fixtures ON t1_awayds_lookup.AwayTeam_t1 = T1_fixtures.AwayTeam_t1")
#
# T1_fixtures$avg_AG_t1 <- t1_avg_AG
#
# t1_awayas_lookup <- cbind(t1_teams,t1_away_as)
#
# t1_awayas_lookup <- as.data.frame(t1_awayas_lookup)
#
# colnames(t1_awayas_lookup) <- c("AwayTeam_t1","t1_awayas")
#
#
# T1_fixtures$t1_awayas <- sqldf("SELECT t1_awayas_lookup.t1_awayas FROM t1_awayas_lookup INNER JOIN T1_fixtures ON t1_awayas_lookup.AwayTeam_t1 = T1_fixtures.AwayTeam_t1")
#
# T1_fixtures$t1_homeds <- rep(t1_home_ds,each = length(t1_teams)-1)
#
# T1_fixtures$t1_awayds <- as.numeric(unlist(T1_fixtures$t1_awayds))
# #xGH
# T1_fixtures$t1_xGH <- T1_fixtures$avg_HG_t1 * T1_fixtures$t1_homeas * T1_fixtures$t1_awayds
#
# #xGA
#
# T1_fixtures$t1_awayas <- as.numeric(unlist(T1_fixtures$t1_awayas))
#
# T1_fixtures$t1_xGA <- T1_fixtures$avg_AG_t1 * T1_fixtures$t1_awayas * T1_fixtures$t1_homeds
#
# T1_fixtures$t1_0_0 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_0 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_1 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_1 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_0 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_2 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_2 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_1 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_2 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_3 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_0 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_1 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_2 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_3 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_3 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_3 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_4 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_0 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_1 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_2 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_3 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_4 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_4 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_4 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_4 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_5 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_0 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_1 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_2 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_3 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_4 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_5 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_5 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_5 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_5 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_5 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_6 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_0 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(0,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_1 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(1,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_2 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(2,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_3 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(3,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_4 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(4,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_6_5 <- round(stats::dpois(6,T1_fixtures$t1_xGH) * stats::dpois(5,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_0_6 <- round(stats::dpois(0,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_1_6 <- round(stats::dpois(1,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_2_6 <- round(stats::dpois(2,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_3_6 <- round(stats::dpois(3,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_4_6 <- round(stats::dpois(4,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# T1_fixtures$t1_5_6 <- round(stats::dpois(5,T1_fixtures$t1_xGH) * stats::dpois(6,T1_fixtures$t1_xGA), digits = 4)
# #Home win
# T1_fixtures$t1_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 + T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5
# )
#
# T1_fixtures$t1_H <- percent(T1_fixtures$t1_H, accuracy = 0.1)
#
# #Draw
# T1_fixtures$t1_D <- (
#
#   T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 + T1_fixtures$t1_4_4 +
#     T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6
# )
#
# T1_fixtures$t1_D <- percent(T1_fixtures$t1_D, accuracy = 0.1)
#
# #Away
#
# T1_fixtures$t1_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 + T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6
# )
#
# T1_fixtures$t1_A <- percent(T1_fixtures$t1_A, accuracy = 0.1)
#
# #ov25
# T1_fixtures$t1_ov25 <- (
#   T1_fixtures$t1_2_1 + T1_fixtures$t1_1_2 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 + T1_fixtures$t1_2_3 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 + T1_fixtures$t1_0_4 +
#     T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 + T1_fixtures$t1_4_4 + T1_fixtures$t1_5_0 +
#     T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 + T1_fixtures$t1_0_5 +
#     T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 + T1_fixtures$t1_5_5 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5 + T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 +
#     T1_fixtures$t1_4_6 + T1_fixtures$t1_5_6 + T1_fixtures$t1_6_6
# )
# #un25
# T1_fixtures$t1_un25 <- (
#   T1_fixtures$t1_0_0 + T1_fixtures$t1_1_0 + T1_fixtures$t1_0_1 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_0 + T1_fixtures$t1_0_2
# )
# #odds
# T1_fixtures$t1_ov25_odds <- round((1/T1_fixtures$t1_ov25),digits = 2)
# T1_fixtures$t1_un25_odds <- round((1/T1_fixtures$t1_un25),digits = 2)
#
# T1_fixtures$t1_ov25_odds
# T1_fixtures$t1_un25_odds
# ###############################################################################
# ###BTTS########################################################################
# #BTTSY
# T1_fixtures$t1_BTTSY <- (
#   T1_fixtures$t1_1_1 + T1_fixtures$t1_2_1 + T1_fixtures$t1_1_2 + T1_fixtures$t1_3_1 + T1_fixtures$t1_3_2 +
#     T1_fixtures$t1_2_2 + T1_fixtures$t1_1_3 + T1_fixtures$t1_2_3 + T1_fixtures$t1_3_3 + T1_fixtures$t1_4_4 +
#     T1_fixtures$t1_4_1 + T1_fixtures$t1_4_3 + T1_fixtures$t1_4_2 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 +
#     T1_fixtures$t1_3_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 +
#     T1_fixtures$t1_5_4 + T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_6_6 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6
# )
# #BTTSN
# T1_fixtures$t1_BTTSN <- (
#   T1_fixtures$t1_0_0 + T1_fixtures$t1_1_0 + T1_fixtures$t1_0_1 + T1_fixtures$t1_2_0 + T1_fixtures$t1_0_2 +
#     T1_fixtures$t1_3_0 + T1_fixtures$t1_0_3 + T1_fixtures$t1_4_0 + T1_fixtures$t1_0_4 + T1_fixtures$t1_5_0 +
#     T1_fixtures$t1_0_5 + T1_fixtures$t1_6_0 + T1_fixtures$t1_0_6
# )
#
# T1_fixtures$t1_BTTSY_odds <- round((1/T1_fixtures$t1_BTTSY),digits = 2)
# T1_fixtures$t1_BTTSN_odds <- round((1/T1_fixtures$t1_BTTSN),digits = 2)
#
# T1_fixtures$t1_BTTSY <- percent(T1_fixtures$t1_BTTSY, accuracy = 0.1)
# T1_fixtures$t1_BTTSN <- percent(T1_fixtures$t1_BTTSN, accuracy = 0.1)
# #odds
# T1_fixtures$t1_BTTSY_odds
# T1_fixtures$t1_BTTSN_odds
# ########Asian Handicaps##########################################################################################################
# ##########################################################################
# #AH(0)
# #AH_0_H
# T1_fixtures$t1_AH_0_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 +T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6
# )
# #AH_0_A
# T1_fixtures$t1_AH_0_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 +T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6
# )
#
# #odds
# T1_fixtures$t1_AH_0_H_odds <- round((1/T1_fixtures$t1_AH_0_H),digits = 2)
# T1_fixtures$t1_AH_0_A_odds <- round((1/T1_fixtures$t1_AH_0_A),digits = 2)
#
# T1_fixtures$t1_AH_0_H_odds
# T1_fixtures$t1_AH_0_A_odds
# #percentages
# T1_fixtures$t1_AH_0_H <- percent(T1_fixtures$t1_AH_0_H, accuracy = 0.1)
# T1_fixtures$t1_AH_0_A <- percent(T1_fixtures$t1_AH_0_A, accuracy = 0.1)
# ####################################################################################
# ##########################################################################
# #AH(-0.75)
# #AH_n075_H
# T1_fixtures$t1_AH_n075_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 +T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5
# )
# #AH_n075_A
# T1_fixtures$t1_AH_n075_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 +T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6
# )
#
# #odds
# T1_fixtures$t1_AH_n075_H_odds <- round((1/T1_fixtures$t1_AH_n075_H),digits = 2)
# T1_fixtures$t1_AH_n075_A_odds <- round((1/T1_fixtures$t1_AH_n075_A),digits = 2)
#
# T1_fixtures$t1_AH_n075_H_odds
# T1_fixtures$t1_AH_n075_A_odds
# #percentages
# T1_fixtures$t1_AH_n075_H <- percent(T1_fixtures$t1_AH_n075_H, accuracy = 0.1)
# T1_fixtures$t1_AH_n075_A <- percent(T1_fixtures$t1_AH_n075_A, accuracy = 0.1)
# ##########################################################################
# #AH(0.75)
# #AH_075_H
# T1_fixtures$t1_AH_075_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 +T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6 + T1_fixtures$t1_0_1 + T1_fixtures$t1_1_2 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_3_4 + T1_fixtures$t1_4_5 + T1_fixtures$t1_5_6
# )
# #AH_075_A
# T1_fixtures$t1_AH_075_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 +T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6 + T1_fixtures$t1_1_0 + T1_fixtures$t1_2_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_3 + T1_fixtures$t1_5_4 + T1_fixtures$t1_6_5
# )
#
# #odds
# T1_fixtures$t1_AH_075_H_odds <- round((1/T1_fixtures$t1_AH_075_H),digits = 2)
# T1_fixtures$t1_AH_075_A_odds <- round((1/T1_fixtures$t1_AH_075_A),digits = 2)
#
# T1_fixtures$t1_AH_075_H_odds
# T1_fixtures$t1_AH_075_A_odds
# #percentages
# T1_fixtures$t1_AH_075_H <- percent(T1_fixtures$t1_AH_075_H, accuracy = 0.1)
# T1_fixtures$t1_AH_075_A <- percent(T1_fixtures$t1_AH_075_A, accuracy = 0.1)
# ####################################################################################
# #AH(-1.25)
# #AH_n125_H
# T1_fixtures$t1_AH_n125_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 +T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5
# )
# #AH_n125_A
# T1_fixtures$t1_AH_n125_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 +T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6
# )
#
# #odds
# T1_fixtures$t1_AH_n125_H_odds <- round((1/T1_fixtures$t1_AH_n125_H),digits = 2)
# T1_fixtures$t1_AH_n125_A_odds <- round((1/T1_fixtures$t1_AH_n125_A),digits = 2)
#
# T1_fixtures$t1_AH_n125_H_odds
# T1_fixtures$t1_AH_n125_A_odds
# #percentages
# T1_fixtures$t1_AH_n125_H <- percent(T1_fixtures$t1_AH_n125_H, accuracy = 0.1)
# T1_fixtures$t1_AH_n125_A <- percent(T1_fixtures$t1_AH_n125_A, accuracy = 0.1)
#
# ####################################################################################
# ##########################################################################
# #AH(1.25)
# #AH_125_H
# T1_fixtures$t1_AH_125_H <- (
#   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#     T1_fixtures$t1_5_0 +T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#     T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#     T1_fixtures$t1_6_5 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6 + T1_fixtures$t1_0_1 + T1_fixtures$t1_1_2 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_3_4 + T1_fixtures$t1_4_5 + T1_fixtures$t1_5_6
# )
# #AH_125_A
# T1_fixtures$t1_AH_125_A <- (
#   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#     T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#     T1_fixtures$t1_0_5 +T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#     T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#     T1_fixtures$t1_5_6 + T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 +
#     T1_fixtures$t1_4_4 + T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6 + T1_fixtures$t1_1_0 + T1_fixtures$t1_2_1 +
#     T1_fixtures$t1_3_2 + T1_fixtures$t1_4_3 + T1_fixtures$t1_5_4 + T1_fixtures$t1_6_5
# )
#
# #odds
# T1_fixtures$t1_AH_125_H_odds <- round((1/T1_fixtures$t1_AH_125_H),digits = 2)
# T1_fixtures$t1_AH_125_A_odds <- round((1/T1_fixtures$t1_AH_125_A),digits = 2)
#
# T1_fixtures$t1_AH_125_H_odds
# T1_fixtures$t1_AH_125_A_odds
# #percentages
# T1_fixtures$t1_AH_125_H <- percent(T1_fixtures$t1_AH_125_H, accuracy = 0.1)
# T1_fixtures$t1_AH_125_A <- percent(T1_fixtures$t1_AH_125_A, accuracy = 0.1)
# ####################################################################################
# ########Asian Handicaps######################################################################################################
# #percentages
# T1_fixtures$t1_ov25 <- percent(T1_fixtures$t1_ov25, accuracy = 0.1)
#
# T1_fixtures$t1_un25 <- percent(T1_fixtures$t1_un25, accuracy = 0.1)
# T1_fixtures$t1_pscore <- paste(round(T1_fixtures$t1_xGH,digits = 0),round(T1_fixtures$t1_xGA,digits = 0),sep = "-")
# #write out
# write.xlsx(T1_fixtures,'Divisions/T1.xlsx',sheetName = "T1", append = TRUE)
