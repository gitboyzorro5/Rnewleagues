library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')

unlink('myodds_fixtures_prediction.csv')
unlink('picks_fixtures_prediction.csv')
################################################################
#MLS
MLS_fixtures$Hometeam_mls_index <- match(MLS_fixtures$HomeTeam_mls,mls_teams)
MLS_fixtures$Awayteam_mls_index <- match(MLS_fixtures$AwayTeam_mls,mls_teams)
mls_prediction <- c()
mls_HWM <- c()
mls_AWM <- c()
mls_HWMLM <- c()
mls_AWMLM <- c()
mls_HY <- c()
mls_AY <- c()
mls_HCO <- c()
mls_ACO <- c()
mls_HXSC <- c()
mls_AXSC <- c()
mls_HYCPF <- c()
mls_AYCPF <- c()
for(mls_row in 1:nrow(MLS_fixtures))
{

mls_hometeamindex <- MLS_fixtures[mls_row,"Hometeam_mls_index"]
mls_awayteamindex <- MLS_fixtures[mls_row,"Awayteam_mls_index"]
#analyse team form
#home team
mls_form_vec_ht <- as.vector(mls_form_h[mls_hometeamindex,])
mls_form_vec_ht[is.na(mls_form_vec_ht)] <- ""
mls_form_vec_ht <- mls_form_vec_ht[mls_form_vec_ht != ""]
mls_form_vec_ht  <-tail(mls_form_vec_ht,6)
mls_ht_numberof_wins <- length(which(mls_form_vec_ht == "W"))
mls_ht_numberof_draws <- length(which(mls_form_vec_ht == "D"))
mls_ht_numberof_loss <- length(which(mls_form_vec_ht == "L"))
#awayteam
mls_form_vec_at <- as.vector(mls_form_h[mls_awayteamindex,])
mls_form_vec_at[is.na(mls_form_vec_at)] <- ""
mls_form_vec_at <- mls_form_vec_at[mls_form_vec_at != ""]
mls_form_vec_at  <-tail(mls_form_vec_at,6)
mls_at_numberof_wins <- length(which(mls_form_vec_at == "W"))
mls_at_numberof_draws <- length(which(mls_form_vec_at == "D"))
mls_at_numberof_loss <- length(which(mls_form_vec_at == "L"))

######################################################################
#analyse goals scored
#hometeam
mls_goalscored_vec_ht <- as.vector(mls_goalscored_h[mls_hometeamindex,])
mls_goalscored_vec_ht[is.na(mls_goalscored_vec_ht)] <- ""
mls_goalscored_vec_ht <- mls_goalscored_vec_ht[mls_goalscored_vec_ht != ""]
mls_goalscored_vec_ht  <-tail(mls_goalscored_vec_ht,6)
mls_goalscored_vec_ht  <- as.numeric(mls_goalscored_vec_ht)
mls_ht_totalgoalscored <- sum(mls_goalscored_vec_ht)
mls_ht_matches_scoring <- length(which(mls_goalscored_vec_ht > 0))
mls_ht_matches_without_scoring <- length(which(mls_goalscored_vec_ht == "0"))
#awayteam
mls_goalscored_vec_at <- as.vector(mls_goalscored_h[mls_awayteamindex,])
mls_goalscored_vec_at[is.na(mls_goalscored_vec_at)] <- ""
mls_goalscored_vec_at <- mls_goalscored_vec_at[mls_goalscored_vec_at != ""]
mls_goalscored_vec_at  <-tail(mls_goalscored_vec_at,6)
mls_goalscored_vec_at  <- as.numeric(mls_goalscored_vec_at)
mls_at_totalgoalscored <- sum(mls_goalscored_vec_at)
mls_at_matches_scoring <- length(which(mls_goalscored_vec_at > 0))
mls_at_matches_without_scoring <- length(which(mls_goalscored_vec_at == "0"))
#####################################################################################
#analyse goals conceded
#hometeam
mls_goalconceded_vec_ht <- as.vector(mls_goalconceded_h[mls_hometeamindex,])
mls_goalconceded_vec_ht[is.na(mls_goalconceded_vec_ht)] <- ""
mls_goalconceded_vec_ht <- mls_goalconceded_vec_ht[mls_goalconceded_vec_ht != ""]
mls_goalconceded_vec_ht  <-tail(mls_goalconceded_vec_ht,6)
mls_goalconceded_vec_ht  <- as.numeric(mls_goalconceded_vec_ht)
mls_goalconceded_vec_ht
mls_ht_totalgoalconceded <- sum(mls_goalconceded_vec_ht)
mls_ht_matches_concede <- length(which(mls_goalconceded_vec_ht > 0))
mls_ht_matches_without_concede <- length(which(mls_goalconceded_vec_ht == "0"))
#awayteam
mls_goalconceded_vec_at <- as.vector(mls_goalconceded_h[mls_awayteamindex,])
mls_goalconceded_vec_at[is.na(mls_goalconceded_vec_at)] <- ""
mls_goalconceded_vec_at <- mls_goalconceded_vec_at[mls_goalconceded_vec_at != ""]
mls_goalconceded_vec_at  <-tail(mls_goalconceded_vec_at,6)
mls_goalconceded_vec_at  <- as.numeric(mls_goalconceded_vec_at)
mls_at_totalgoalconceded <- sum(mls_goalconceded_vec_at)
mls_at_matches_concede <- length(which(mls_goalconceded_vec_at > 0))
mls_at_matches_without_concede <- length(which(mls_goalconceded_vec_at == "0"))

####################################################################################
#analyse total combined goals
#hometeam
mls_totalgoals_vec_ht <- as.vector(mls_totalgoals_h[mls_hometeamindex,])
mls_totalgoals_vec_ht[is.na(mls_totalgoals_vec_ht)] <- ""
mls_totalgoals_vec_ht <- mls_totalgoals_vec_ht[mls_totalgoals_vec_ht != ""]
mls_totalgoals_vec_ht  <-tail(mls_totalgoals_vec_ht,6)
mls_totalgoals_vec_ht  <- as.numeric(mls_totalgoals_vec_ht)
mls_totalgoals_vec_ht
mls_ht_totalgoals <- sum(mls_totalgoals_vec_ht)
mls_ht_avgtotalgoals <- (mls_ht_totalgoals/6)
mls_ht_no_of_ov25 <- length(which(mls_totalgoals_vec_ht >= 3))
mls_ht_no_of_un25 <- length(which(mls_totalgoals_vec_ht <= 2))
#awayteam
mls_totalgoals_vec_at <- as.vector(mls_totalgoals_h[mls_awayteamindex,])
mls_totalgoals_vec_at[is.na(mls_totalgoals_vec_at)] <- ""
mls_totalgoals_vec_at <- mls_totalgoals_vec_at[mls_totalgoals_vec_at != ""]
mls_totalgoals_vec_at  <-tail(mls_totalgoals_vec_at,6)
mls_totalgoals_vec_at  <- as.numeric(mls_totalgoals_vec_at)
mls_totalgoals_vec_at
mls_at_totalgoals <- sum(mls_totalgoals_vec_at)
mls_at_avgtotalgoals <- (mls_at_totalgoals/6)
mls_at_no_of_ov25 <- length(which(mls_totalgoals_vec_at >= 3))
mls_at_no_of_un25 <- length(which(mls_totalgoals_vec_at <= 2))
################################################################################
#analyse win margin
#hometeam
mls_winmargin_vec_ht <- as.vector(mls_winmargin_h[mls_hometeamindex,])
mls_winmargin_vec_ht[is.na(mls_winmargin_vec_ht)] <- ""
mls_winmargin_vec_ht <- mls_winmargin_vec_ht[mls_winmargin_vec_ht != ""]
mls_winmargin_vec_ht  <-tail(mls_winmargin_vec_ht,6)
mls_winmargin_vec_ht  <- as.numeric(mls_winmargin_vec_ht)

mls_ht_totalwinmargin <- sum(mls_winmargin_vec_ht)
mls_ht_no_of_winmargin_ov0 <- length(which(mls_winmargin_vec_ht >= 0))
mls_ht_no_of_winmargin_ov1 <- length(which(mls_winmargin_vec_ht >= 1))
mls_ht_no_of_winmargin_un0 <- length(which(mls_winmargin_vec_ht <= 0))
mls_ht_no_of_winmargin_un1 <- length(which(mls_winmargin_vec_ht <= 1))
#awayteam
mls_winmargin_vec_at <- as.vector(mls_winmargin_h[mls_awayteamindex,])
mls_winmargin_vec_at[is.na(mls_winmargin_vec_at)] <- ""
mls_winmargin_vec_at <- mls_winmargin_vec_at[mls_winmargin_vec_at != ""]
mls_winmargin_vec_at  <-tail(mls_winmargin_vec_at,6)
mls_winmargin_vec_at  <- as.numeric(mls_winmargin_vec_at)

mls_at_totalwinmargin <- sum(mls_winmargin_vec_at)
mls_at_no_of_winmargin_ov0 <- length(which(mls_winmargin_vec_at >= 0))
mls_at_no_of_winmargin_ov1 <- length(which(mls_winmargin_vec_at >= 1))
mls_at_no_of_winmargin_un0 <- length(which(mls_winmargin_vec_at <= 0))
mls_at_no_of_winmargin_un1 <- length(which(mls_winmargin_vec_at <= 1))
##################################################################################
#very last win margin
#hometeam
mls_winmargin_vec_ht_lm <- as.vector(mls_winmargin_h[mls_hometeamindex,])
mls_winmargin_vec_ht_lm[is.na(mls_winmargin_vec_ht_lm)] <- ""
mls_winmargin_vec_ht_lm <- mls_winmargin_vec_ht_lm[mls_winmargin_vec_ht_lm != ""]
mls_winmargin_vec_ht_lm  <-tail(mls_winmargin_vec_ht_lm,1)
#awayteam
mls_winmargin_vec_at_lm <- as.vector(mls_winmargin_h[mls_awayteamindex,])
mls_winmargin_vec_at_lm[is.na(mls_winmargin_vec_at_lm)] <- ""
mls_winmargin_vec_at_lm <- mls_winmargin_vec_at_lm[mls_winmargin_vec_at_lm != ""]
mls_winmargin_vec_at_lm  <-tail(mls_winmargin_vec_at_lm,1)
#################################################################################
#pick average yellow cards
#hometeam
mls_yellowtotals_vec_ht <- as.vector(mls_yellowtotalsv2[mls_hometeamindex,])
mls_yellowtotals_vec_ht[is.na(mls_yellowtotals_vec_ht)] <- ""
mls_yellowtotals_vec_ht <- mls_yellowtotals_vec_ht[mls_yellowtotals_vec_ht != ""]
mls_yellowtotals_vec_ht  <-tail(mls_yellowtotals_vec_ht,1)
#awayteam
mls_yellowtotals_vec_at <- as.vector(mls_yellowtotalsv2[mls_awayteamindex,])
mls_yellowtotals_vec_at[is.na(mls_yellowtotals_vec_at)] <- ""
mls_yellowtotals_vec_at <- mls_yellowtotals_vec_at[mls_yellowtotals_vec_at != ""]
mls_yellowtotals_vec_at  <-tail(mls_yellowtotals_vec_at,1)

#################################################################################
#pick average corners
#hometeam
mls_cornertotals_vec_ht <- as.vector(mls_cornertotalsv2[mls_hometeamindex,])
mls_cornertotals_vec_ht[is.na(mls_cornertotals_vec_ht)] <- ""
mls_cornertotals_vec_ht <- mls_cornertotals_vec_ht[mls_cornertotals_vec_ht != ""]
mls_cornertotals_vec_ht  <-tail(mls_cornertotals_vec_ht,1)
#awayteam
mls_cornertotals_vec_at <- as.vector(mls_cornertotalsv2[mls_awayteamindex,])
mls_cornertotals_vec_at[is.na(mls_cornertotals_vec_at)] <- ""
mls_cornertotals_vec_at <- mls_cornertotals_vec_at[mls_cornertotals_vec_at != ""]
mls_cornertotals_vec_at  <-tail(mls_cornertotals_vec_at,1)
#################################################################################
#pick xpected shots conversion
#hometeam
mls_xshotsconversion_vec_ht <- as.vector(mls_shots_analysis[mls_hometeamindex,])
mls_xshotsconversion_vec_ht[is.na(mls_xshotsconversion_vec_ht)] <- ""
mls_xshotsconversion_vec_ht <- mls_xshotsconversion_vec_ht[mls_xshotsconversion_vec_ht != ""]
mls_xshotsconversion_vec_ht  <-tail(mls_xshotsconversion_vec_ht,1)
#awayteam
mls_xshotsconversion_vec_at <- as.vector(mls_shots_analysis[mls_awayteamindex,])
mls_xshotsconversion_vec_at[is.na(mls_xshotsconversion_vec_at)] <- ""
mls_xshotsconversion_vec_at <- mls_xshotsconversion_vec_at[mls_xshotsconversion_vec_at != ""]
mls_xshotsconversion_vec_at  <-tail(mls_xshotsconversion_vec_at,1)
#################################################################################
#pick yellow cards per foul
#hometeam
mls_fouls_conversion_vec_ht <- as.vector(mls_fouls_conversion[mls_hometeamindex,])
mls_fouls_conversion_vec_ht[is.na(mls_fouls_conversion_vec_ht)] <- ""
mls_fouls_conversion_vec_ht <- mls_fouls_conversion_vec_ht[mls_fouls_conversion_vec_ht != ""]
mls_fouls_conversion_vec_ht  <-tail(mls_fouls_conversion_vec_ht,1)
#awayteam
mls_fouls_conversion_vec_at <- as.vector(mls_fouls_conversion[mls_awayteamindex,])
mls_fouls_conversion_vec_at[is.na(mls_fouls_conversion_vec_at)] <- ""
mls_fouls_conversion_vec_at <- mls_fouls_conversion_vec_at[mls_fouls_conversion_vec_at != ""]
mls_fouls_conversion_vec_at  <-tail(mls_fouls_conversion_vec_at,1)
#################################################################################

####we need to decide ############
#winner goals
mls_ht_last6points <- mls_ht_numberof_wins*3 + mls_ht_numberof_draws*1
mls_at_last6points <- mls_at_numberof_wins*3 + mls_at_numberof_draws*1

if(mls_ht_last6points > mls_at_last6points) {mls_3waypick <- "1"}  else {mls_3waypick <- "X2"}

if(mls_at_last6points > mls_ht_last6points ) {mls_3waypick <- "2"} else {mls_3waypick <- "1X"}

if(mls_ht_no_of_ov25 + mls_at_no_of_ov25 >= 6) {mls_goalspick <- "ov25"} else {mls_goalspick <- "un25"}

if(mls_ht_no_of_un25 + mls_at_no_of_un25 >= 6) {mls_goalspick <- "un25"} else {mls_goalspick <- "ov25"}

if(mls_ht_matches_scoring >= 4 && mls_at_matches_scoring >=4) {mls_btts <- "BTTS-Y"} else {mls_btts <- "BTTS-N"}


mls_prediction[mls_row] <- rbind(paste(mls_3waypick,mls_goalspick,mls_btts,sep = ","))
mls_HWM[mls_row] <- mls_ht_totalwinmargin
mls_AWM[mls_row] <- mls_at_totalwinmargin

mls_HWMLM[mls_row] <- mls_winmargin_vec_ht_lm
mls_AWMLM[mls_row] <- mls_winmargin_vec_at_lm

mls_HY[mls_row] <- mls_yellowtotals_vec_ht
mls_AY[mls_row] <- mls_yellowtotals_vec_at

mls_HCO[mls_row] <- mls_cornertotals_vec_ht
mls_ACO[mls_row] <- mls_cornertotals_vec_at

mls_HXSC[mls_row] <- mls_xshotsconversion_vec_ht
mls_AXSC[mls_row] <- mls_xshotsconversion_vec_at

mls_HYCPF[mls_row] <- mls_fouls_conversion_vec_ht
mls_AYCPF[mls_row] <- mls_fouls_conversion_vec_at
}

mls_prediction <- as.data.frame(mls_prediction)
colnames(mls_prediction) <- "prediction"

mls_HWM <- as.data.frame(mls_HWM)
colnames(mls_HWM) <- "HWM"

mls_AWM <- as.data.frame(mls_AWM)
colnames(mls_AWM) <- "AWM"

mls_HWMLM <- as.data.frame(mls_HWMLM)
colnames(mls_HWMLM) <- "HWMLM"

mls_AWMLM <- as.data.frame(mls_AWMLM)
colnames(mls_AWMLM) <- "AWMLM"

mls_HY <- as.data.frame(mls_HY)
colnames(mls_HY) <- "AVGHY"

mls_AY <- as.data.frame(mls_AY)
colnames(mls_AY) <- "AVGAY"

mls_HCO <- as.data.frame(mls_HCO)
colnames(mls_HCO) <- "AVGHCO"

mls_ACO <- as.data.frame(mls_ACO)
colnames(mls_ACO) <- "AVGACO"

mls_HXSC <- as.data.frame(mls_HXSC)
colnames(mls_HXSC) <- "HXSC"

mls_AXSC <- as.data.frame(mls_AXSC)
colnames(mls_AXSC) <- "AXSC"

mls_HYCPF <- as.data.frame(mls_HYCPF)
colnames(mls_HYCPF) <- "HYCPF"

mls_AYCPF <- as.data.frame(mls_AYCPF)
colnames(mls_AYCPF) <- "AYCPF"

mls_picks <- cbind(MLS_fixtures$Div,MLS_fixtures$HomeTeam_mls,MLS_fixtures$AwayTeam_mls,mls_prediction,mls_HWM,mls_AWM,mls_HWMLM,mls_AWMLM,mls_HY,mls_AY,mls_HCO,mls_ACO,mls_HXSC,mls_AXSC,mls_HYCPF,mls_AYCPF)

colnames(mls_picks)[1] <- "picks_Div"
colnames(mls_picks)[2] <- "picks_HomeTeam"
colnames(mls_picks)[3] <- "picks_AwayTeam"
mls_picks$matchid <- paste(mls_picks$picks_HomeTeam,mls_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of MLS
mls_picks
############################################################################################
#ARG
ARG_fixtures$Hometeam_arg_index <- match(ARG_fixtures$HomeTeam_arg,arg_teams)
ARG_fixtures$Awayteam_arg_index <- match(ARG_fixtures$AwayTeam_arg,arg_teams)
arg_prediction <- c()
arg_HWM <- c()
arg_AWM <- c()
arg_HWMLM <- c()
arg_AWMLM <- c()
arg_HY <- c()
arg_AY <- c()
arg_HCO <- c()
arg_ACO <- c()
arg_HXSC <- c()
arg_AXSC <- c()
arg_HYCPF <- c()
arg_AYCPF <- c()
for(arg_row in 1:nrow(ARG_fixtures))
{

  arg_hometeamindex <- ARG_fixtures[arg_row,"Hometeam_arg_index"]
  arg_awayteamindex <- ARG_fixtures[arg_row,"Awayteam_arg_index"]
  #analyse team form
  #home team
  arg_form_vec_ht <- as.vector(arg_form_h[arg_hometeamindex,])
  arg_form_vec_ht[is.na(arg_form_vec_ht)] <- ""
  arg_form_vec_ht <- arg_form_vec_ht[arg_form_vec_ht != ""]
  arg_form_vec_ht  <-tail(arg_form_vec_ht,6)
  arg_ht_numberof_wins <- length(which(arg_form_vec_ht == "W"))
  arg_ht_numberof_draws <- length(which(arg_form_vec_ht == "D"))
  arg_ht_numberof_loss <- length(which(arg_form_vec_ht == "L"))
  #awayteam
  arg_form_vec_at <- as.vector(arg_form_h[arg_awayteamindex,])
  arg_form_vec_at[is.na(arg_form_vec_at)] <- ""
  arg_form_vec_at <- arg_form_vec_at[arg_form_vec_at != ""]
  arg_form_vec_at  <-tail(arg_form_vec_at,6)
  arg_at_numberof_wins <- length(which(arg_form_vec_at == "W"))
  arg_at_numberof_draws <- length(which(arg_form_vec_at == "D"))
  arg_at_numberof_loss <- length(which(arg_form_vec_at == "L"))

  ######################################################################
  #analyse goals scored
  #hometeam
  arg_goalscored_vec_ht <- as.vector(arg_goalscored_h[arg_hometeamindex,])
  arg_goalscored_vec_ht[is.na(arg_goalscored_vec_ht)] <- ""
  arg_goalscored_vec_ht <- arg_goalscored_vec_ht[arg_goalscored_vec_ht != ""]
  arg_goalscored_vec_ht  <-tail(arg_goalscored_vec_ht,6)
  arg_goalscored_vec_ht  <- as.numeric(arg_goalscored_vec_ht)
  arg_ht_totalgoalscored <- sum(arg_goalscored_vec_ht)
  arg_ht_matches_scoring <- length(which(arg_goalscored_vec_ht > 0))
  arg_ht_matches_without_scoring <- length(which(arg_goalscored_vec_ht == "0"))
  #awayteam
  arg_goalscored_vec_at <- as.vector(arg_goalscored_h[arg_awayteamindex,])
  arg_goalscored_vec_at[is.na(arg_goalscored_vec_at)] <- ""
  arg_goalscored_vec_at <- arg_goalscored_vec_at[arg_goalscored_vec_at != ""]
  arg_goalscored_vec_at  <-tail(arg_goalscored_vec_at,6)
  arg_goalscored_vec_at  <- as.numeric(arg_goalscored_vec_at)
  arg_at_totalgoalscored <- sum(arg_goalscored_vec_at)
  arg_at_matches_scoring <- length(which(arg_goalscored_vec_at > 0))
  arg_at_matches_without_scoring <- length(which(arg_goalscored_vec_at == "0"))
  #####################################################################################
  #analyse goals conceded
  #hometeam
  arg_goalconceded_vec_ht <- as.vector(arg_goalconceded_h[arg_hometeamindex,])
  arg_goalconceded_vec_ht[is.na(arg_goalconceded_vec_ht)] <- ""
  arg_goalconceded_vec_ht <- arg_goalconceded_vec_ht[arg_goalconceded_vec_ht != ""]
  arg_goalconceded_vec_ht  <-tail(arg_goalconceded_vec_ht,6)
  arg_goalconceded_vec_ht  <- as.numeric(arg_goalconceded_vec_ht)
  arg_goalconceded_vec_ht
  arg_ht_totalgoalconceded <- sum(arg_goalconceded_vec_ht)
  arg_ht_matches_concede <- length(which(arg_goalconceded_vec_ht > 0))
  arg_ht_matches_without_concede <- length(which(arg_goalconceded_vec_ht == "0"))
  #awayteam
  arg_goalconceded_vec_at <- as.vector(arg_goalconceded_h[arg_awayteamindex,])
  arg_goalconceded_vec_at[is.na(arg_goalconceded_vec_at)] <- ""
  arg_goalconceded_vec_at <- arg_goalconceded_vec_at[arg_goalconceded_vec_at != ""]
  arg_goalconceded_vec_at  <-tail(arg_goalconceded_vec_at,6)
  arg_goalconceded_vec_at  <- as.numeric(arg_goalconceded_vec_at)
  arg_at_totalgoalconceded <- sum(arg_goalconceded_vec_at)
  arg_at_matches_concede <- length(which(arg_goalconceded_vec_at > 0))
  arg_at_matches_without_concede <- length(which(arg_goalconceded_vec_at == "0"))

  ####################################################################################
  #analyse total combined goals
  #hometeam
  arg_totalgoals_vec_ht <- as.vector(arg_totalgoals_h[arg_hometeamindex,])
  arg_totalgoals_vec_ht[is.na(arg_totalgoals_vec_ht)] <- ""
  arg_totalgoals_vec_ht <- arg_totalgoals_vec_ht[arg_totalgoals_vec_ht != ""]
  arg_totalgoals_vec_ht  <-tail(arg_totalgoals_vec_ht,6)
  arg_totalgoals_vec_ht  <- as.numeric(arg_totalgoals_vec_ht)
  arg_totalgoals_vec_ht
  arg_ht_totalgoals <- sum(arg_totalgoals_vec_ht)
  arg_ht_avgtotalgoals <- (arg_ht_totalgoals/6)
  arg_ht_no_of_ov25 <- length(which(arg_totalgoals_vec_ht >= 3))
  arg_ht_no_of_un25 <- length(which(arg_totalgoals_vec_ht <= 2))
  #awayteam
  arg_totalgoals_vec_at <- as.vector(arg_totalgoals_h[arg_awayteamindex,])
  arg_totalgoals_vec_at[is.na(arg_totalgoals_vec_at)] <- ""
  arg_totalgoals_vec_at <- arg_totalgoals_vec_at[arg_totalgoals_vec_at != ""]
  arg_totalgoals_vec_at  <-tail(arg_totalgoals_vec_at,6)
  arg_totalgoals_vec_at  <- as.numeric(arg_totalgoals_vec_at)
  arg_totalgoals_vec_at
  arg_at_totalgoals <- sum(arg_totalgoals_vec_at)
  arg_at_avgtotalgoals <- (arg_at_totalgoals/6)
  arg_at_no_of_ov25 <- length(which(arg_totalgoals_vec_at >= 3))
  arg_at_no_of_un25 <- length(which(arg_totalgoals_vec_at <= 2))
  ################################################################################
  #analyse win margin
  #hometeam
  arg_winmargin_vec_ht <- as.vector(arg_winmargin_h[arg_hometeamindex,])
  arg_winmargin_vec_ht[is.na(arg_winmargin_vec_ht)] <- ""
  arg_winmargin_vec_ht <- arg_winmargin_vec_ht[arg_winmargin_vec_ht != ""]
  arg_winmargin_vec_ht  <-tail(arg_winmargin_vec_ht,6)
  arg_winmargin_vec_ht  <- as.numeric(arg_winmargin_vec_ht)

  arg_ht_totalwinmargin <- sum(arg_winmargin_vec_ht)
  arg_ht_no_of_winmargin_ov0 <- length(which(arg_winmargin_vec_ht >= 0))
  arg_ht_no_of_winmargin_ov1 <- length(which(arg_winmargin_vec_ht >= 1))
  arg_ht_no_of_winmargin_un0 <- length(which(arg_winmargin_vec_ht <= 0))
  arg_ht_no_of_winmargin_un1 <- length(which(arg_winmargin_vec_ht <= 1))
  #awayteam
  arg_winmargin_vec_at <- as.vector(arg_winmargin_h[arg_awayteamindex,])
  arg_winmargin_vec_at[is.na(arg_winmargin_vec_at)] <- ""
  arg_winmargin_vec_at <- arg_winmargin_vec_at[arg_winmargin_vec_at != ""]
  arg_winmargin_vec_at  <-tail(arg_winmargin_vec_at,6)
  arg_winmargin_vec_at  <- as.numeric(arg_winmargin_vec_at)

  arg_at_totalwinmargin <- sum(arg_winmargin_vec_at)
  arg_at_no_of_winmargin_ov0 <- length(which(arg_winmargin_vec_at >= 0))
  arg_at_no_of_winmargin_ov1 <- length(which(arg_winmargin_vec_at >= 1))
  arg_at_no_of_winmargin_un0 <- length(which(arg_winmargin_vec_at <= 0))
  arg_at_no_of_winmargin_un1 <- length(which(arg_winmargin_vec_at <= 1))
  ##################################################################################
  #very last win margin
  #hometeam
  arg_winmargin_vec_ht_lm <- as.vector(arg_winmargin_h[arg_hometeamindex,])
  arg_winmargin_vec_ht_lm[is.na(arg_winmargin_vec_ht_lm)] <- ""
  arg_winmargin_vec_ht_lm <- arg_winmargin_vec_ht_lm[arg_winmargin_vec_ht_lm != ""]
  arg_winmargin_vec_ht_lm  <-tail(arg_winmargin_vec_ht_lm,1)
  #awayteam
  arg_winmargin_vec_at_lm <- as.vector(arg_winmargin_h[arg_awayteamindex,])
  arg_winmargin_vec_at_lm[is.na(arg_winmargin_vec_at_lm)] <- ""
  arg_winmargin_vec_at_lm <- arg_winmargin_vec_at_lm[arg_winmargin_vec_at_lm != ""]
  arg_winmargin_vec_at_lm  <-tail(arg_winmargin_vec_at_lm,1)
  #################################################################################
  #pick average yellow cards
  #hometeam
  arg_yellowtotals_vec_ht <- as.vector(arg_yellowtotalsv2[arg_hometeamindex,])
  arg_yellowtotals_vec_ht[is.na(arg_yellowtotals_vec_ht)] <- ""
  arg_yellowtotals_vec_ht <- arg_yellowtotals_vec_ht[arg_yellowtotals_vec_ht != ""]
  arg_yellowtotals_vec_ht  <-tail(arg_yellowtotals_vec_ht,1)
  #awayteam
  arg_yellowtotals_vec_at <- as.vector(arg_yellowtotalsv2[arg_awayteamindex,])
  arg_yellowtotals_vec_at[is.na(arg_yellowtotals_vec_at)] <- ""
  arg_yellowtotals_vec_at <- arg_yellowtotals_vec_at[arg_yellowtotals_vec_at != ""]
  arg_yellowtotals_vec_at  <-tail(arg_yellowtotals_vec_at,1)

  #################################################################################
  #pick average corners
  #hometeam
  arg_cornertotals_vec_ht <- as.vector(arg_cornertotalsv2[arg_hometeamindex,])
  arg_cornertotals_vec_ht[is.na(arg_cornertotals_vec_ht)] <- ""
  arg_cornertotals_vec_ht <- arg_cornertotals_vec_ht[arg_cornertotals_vec_ht != ""]
  arg_cornertotals_vec_ht  <-tail(arg_cornertotals_vec_ht,1)
  #awayteam
  arg_cornertotals_vec_at <- as.vector(arg_cornertotalsv2[arg_awayteamindex,])
  arg_cornertotals_vec_at[is.na(arg_cornertotals_vec_at)] <- ""
  arg_cornertotals_vec_at <- arg_cornertotals_vec_at[arg_cornertotals_vec_at != ""]
  arg_cornertotals_vec_at  <-tail(arg_cornertotals_vec_at,1)
  #################################################################################
  #pick xpected shots conversion
  #hometeam
  arg_xshotsconversion_vec_ht <- as.vector(arg_shots_analysis[arg_hometeamindex,])
  arg_xshotsconversion_vec_ht[is.na(arg_xshotsconversion_vec_ht)] <- ""
  arg_xshotsconversion_vec_ht <- arg_xshotsconversion_vec_ht[arg_xshotsconversion_vec_ht != ""]
  arg_xshotsconversion_vec_ht  <-tail(arg_xshotsconversion_vec_ht,1)
  #awayteam
  arg_xshotsconversion_vec_at <- as.vector(arg_shots_analysis[arg_awayteamindex,])
  arg_xshotsconversion_vec_at[is.na(arg_xshotsconversion_vec_at)] <- ""
  arg_xshotsconversion_vec_at <- arg_xshotsconversion_vec_at[arg_xshotsconversion_vec_at != ""]
  arg_xshotsconversion_vec_at  <-tail(arg_xshotsconversion_vec_at,1)
  #################################################################################
  #pick yellow cards per foul
  #hometeam
  arg_fouls_conversion_vec_ht <- as.vector(arg_fouls_conversion[arg_hometeamindex,])
  arg_fouls_conversion_vec_ht[is.na(arg_fouls_conversion_vec_ht)] <- ""
  arg_fouls_conversion_vec_ht <- arg_fouls_conversion_vec_ht[arg_fouls_conversion_vec_ht != ""]
  arg_fouls_conversion_vec_ht  <-tail(arg_fouls_conversion_vec_ht,1)
  #awayteam
  arg_fouls_conversion_vec_at <- as.vector(arg_fouls_conversion[arg_awayteamindex,])
  arg_fouls_conversion_vec_at[is.na(arg_fouls_conversion_vec_at)] <- ""
  arg_fouls_conversion_vec_at <- arg_fouls_conversion_vec_at[arg_fouls_conversion_vec_at != ""]
  arg_fouls_conversion_vec_at  <-tail(arg_fouls_conversion_vec_at,1)
  #################################################################################

  ####we need to decide ############
  #winner goals
  arg_ht_last6points <- arg_ht_numberof_wins*3 + arg_ht_numberof_draws*1
  arg_at_last6points <- arg_at_numberof_wins*3 + arg_at_numberof_draws*1

  if(arg_ht_last6points > arg_at_last6points) {arg_3waypick <- "1"}  else {arg_3waypick <- "X2"}

  if(arg_at_last6points > arg_ht_last6points ) {arg_3waypick <- "2"} else {arg_3waypick <- "1X"}

  if(arg_ht_no_of_ov25 + arg_at_no_of_ov25 >= 6) {arg_goalspick <- "ov25"} else {arg_goalspick <- "un25"}

  if(arg_ht_no_of_un25 + arg_at_no_of_un25 >= 6) {arg_goalspick <- "un25"} else {arg_goalspick <- "ov25"}

  if(arg_ht_matches_scoring >= 4 && arg_at_matches_scoring >=4) {arg_btts <- "BTTS-Y"} else {arg_btts <- "BTTS-N"}


  arg_prediction[arg_row] <- rbind(paste(arg_3waypick,arg_goalspick,arg_btts,sep = ","))
  arg_HWM[arg_row] <- arg_ht_totalwinmargin
  arg_AWM[arg_row] <- arg_at_totalwinmargin

  arg_HWMLM[arg_row] <- arg_winmargin_vec_ht_lm
  arg_AWMLM[arg_row] <- arg_winmargin_vec_at_lm

  arg_HY[arg_row] <- arg_yellowtotals_vec_ht
  arg_AY[arg_row] <- arg_yellowtotals_vec_at

  arg_HCO[arg_row] <- arg_cornertotals_vec_ht
  arg_ACO[arg_row] <- arg_cornertotals_vec_at

  arg_HXSC[arg_row] <- arg_xshotsconversion_vec_ht
  arg_AXSC[arg_row] <- arg_xshotsconversion_vec_at

  arg_HYCPF[arg_row] <- arg_fouls_conversion_vec_ht
  arg_AYCPF[arg_row] <- arg_fouls_conversion_vec_at
}

arg_prediction <- as.data.frame(arg_prediction)
colnames(arg_prediction) <- "prediction"

arg_HWM <- as.data.frame(arg_HWM)
colnames(arg_HWM) <- "HWM"

arg_AWM <- as.data.frame(arg_AWM)
colnames(arg_AWM) <- "AWM"

arg_HWMLM <- as.data.frame(arg_HWMLM)
colnames(arg_HWMLM) <- "HWMLM"

arg_AWMLM <- as.data.frame(arg_AWMLM)
colnames(arg_AWMLM) <- "AWMLM"

arg_HY <- as.data.frame(arg_HY)
colnames(arg_HY) <- "AVGHY"

arg_AY <- as.data.frame(arg_AY)
colnames(arg_AY) <- "AVGAY"

arg_HCO <- as.data.frame(arg_HCO)
colnames(arg_HCO) <- "AVGHCO"

arg_ACO <- as.data.frame(arg_ACO)
colnames(arg_ACO) <- "AVGACO"

arg_HXSC <- as.data.frame(arg_HXSC)
colnames(arg_HXSC) <- "HXSC"

arg_AXSC <- as.data.frame(arg_AXSC)
colnames(arg_AXSC) <- "AXSC"

arg_HYCPF <- as.data.frame(arg_HYCPF)
colnames(arg_HYCPF) <- "HYCPF"

arg_AYCPF <- as.data.frame(arg_AYCPF)
colnames(arg_AYCPF) <- "AYCPF"

arg_picks <- cbind(ARG_fixtures$Div,ARG_fixtures$HomeTeam_arg,ARG_fixtures$AwayTeam_arg,arg_prediction,arg_HWM,arg_AWM,arg_HWMLM,arg_AWMLM,arg_HY,arg_AY,arg_HCO,arg_ACO,arg_HXSC,arg_AXSC,arg_HYCPF,arg_AYCPF)

colnames(arg_picks)[1] <- "picks_Div"
colnames(arg_picks)[2] <- "picks_HomeTeam"
colnames(arg_picks)[3] <- "picks_AwayTeam"
arg_picks$matchid <- paste(arg_picks$picks_HomeTeam,arg_picks$picks_AwayTeam,sep = "-")
############################################################################################
#end of ARG
arg_picks
#D2
# D2_fixtures$Hometeam_d2_index <- match(D2_fixtures$HomeTeam_d2,d2_teams)
# D2_fixtures$Awayteam_d2_index <- match(D2_fixtures$AwayTeam_d2,d2_teams)
# d2_prediction <- c()
# d2_HWM <- c()
# d2_AWM <- c()
# d2_HWMLM <- c()
# d2_AWMLM <- c()
# d2_HY <- c()
# d2_AY <- c()
# d2_HCO <- c()
# d2_ACO <- c()
# d2_HXSC <- c()
# d2_AXSC <- c()
# d2_HYCPF <- c()
# d2_AYCPF <- c()
# for(d2_row in 1:nrow(D2_fixtures))
# {
#
#   d2_hometeamindex <- D2_fixtures[d2_row,"Hometeam_d2_index"]
#   d2_awayteamindex <- D2_fixtures[d2_row,"Awayteam_d2_index"]
#   #analyse team form
#   #home team
#   d2_form_vec_ht <- as.vector(d2_form_h[d2_hometeamindex,])
#   d2_form_vec_ht[is.na(d2_form_vec_ht)] <- ""
#   d2_form_vec_ht <- d2_form_vec_ht[d2_form_vec_ht != ""]
#   d2_form_vec_ht  <-tail(d2_form_vec_ht,6)
#   d2_ht_numberof_wins <- length(which(d2_form_vec_ht == "W"))
#   d2_ht_numberof_draws <- length(which(d2_form_vec_ht == "D"))
#   d2_ht_numberof_loss <- length(which(d2_form_vec_ht == "L"))
#   #awayteam
#   d2_form_vec_at <- as.vector(d2_form_h[d2_awayteamindex,])
#   d2_form_vec_at[is.na(d2_form_vec_at)] <- ""
#   d2_form_vec_at <- d2_form_vec_at[d2_form_vec_at != ""]
#   d2_form_vec_at  <-tail(d2_form_vec_at,6)
#   d2_at_numberof_wins <- length(which(d2_form_vec_at == "W"))
#   d2_at_numberof_draws <- length(which(d2_form_vec_at == "D"))
#   d2_at_numberof_loss <- length(which(d2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   d2_goalscored_vec_ht <- as.vector(d2_goalscored_h[d2_hometeamindex,])
#   d2_goalscored_vec_ht[is.na(d2_goalscored_vec_ht)] <- ""
#   d2_goalscored_vec_ht <- d2_goalscored_vec_ht[d2_goalscored_vec_ht != ""]
#   d2_goalscored_vec_ht  <-tail(d2_goalscored_vec_ht,6)
#   d2_goalscored_vec_ht  <- as.numeric(d2_goalscored_vec_ht)
#   d2_ht_totalgoalscored <- sum(d2_goalscored_vec_ht)
#   d2_ht_matches_scoring <- length(which(d2_goalscored_vec_ht > 0))
#   d2_ht_matches_without_scoring <- length(which(d2_goalscored_vec_ht == "0"))
#   #awayteam
#   d2_goalscored_vec_at <- as.vector(d2_goalscored_h[d2_awayteamindex,])
#   d2_goalscored_vec_at[is.na(d2_goalscored_vec_at)] <- ""
#   d2_goalscored_vec_at <- d2_goalscored_vec_at[d2_goalscored_vec_at != ""]
#   d2_goalscored_vec_at  <-tail(d2_goalscored_vec_at,6)
#   d2_goalscored_vec_at  <- as.numeric(d2_goalscored_vec_at)
#   d2_at_totalgoalscored <- sum(d2_goalscored_vec_at)
#   d2_at_matches_scoring <- length(which(d2_goalscored_vec_at > 0))
#   d2_at_matches_without_scoring <- length(which(d2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   d2_goalconceded_vec_ht <- as.vector(d2_goalconceded_h[d2_hometeamindex,])
#   d2_goalconceded_vec_ht[is.na(d2_goalconceded_vec_ht)] <- ""
#   d2_goalconceded_vec_ht <- d2_goalconceded_vec_ht[d2_goalconceded_vec_ht != ""]
#   d2_goalconceded_vec_ht  <-tail(d2_goalconceded_vec_ht,6)
#   d2_goalconceded_vec_ht  <- as.numeric(d2_goalconceded_vec_ht)
#   d2_goalconceded_vec_ht
#   d2_ht_totalgoalconceded <- sum(d2_goalconceded_vec_ht)
#   d2_ht_matches_concede <- length(which(d2_goalconceded_vec_ht > 0))
#   d2_ht_matches_without_concede <- length(which(d2_goalconceded_vec_ht == "0"))
#   #awayteam
#   d2_goalconceded_vec_at <- as.vector(d2_goalconceded_h[d2_awayteamindex,])
#   d2_goalconceded_vec_at[is.na(d2_goalconceded_vec_at)] <- ""
#   d2_goalconceded_vec_at <- d2_goalconceded_vec_at[d2_goalconceded_vec_at != ""]
#   d2_goalconceded_vec_at  <-tail(d2_goalconceded_vec_at,6)
#   d2_goalconceded_vec_at  <- as.numeric(d2_goalconceded_vec_at)
#   d2_at_totalgoalconceded <- sum(d2_goalconceded_vec_at)
#   d2_at_matches_concede <- length(which(d2_goalconceded_vec_at > 0))
#   d2_at_matches_without_concede <- length(which(d2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   d2_totalgoals_vec_ht <- as.vector(d2_totalgoals_h[d2_hometeamindex,])
#   d2_totalgoals_vec_ht[is.na(d2_totalgoals_vec_ht)] <- ""
#   d2_totalgoals_vec_ht <- d2_totalgoals_vec_ht[d2_totalgoals_vec_ht != ""]
#   d2_totalgoals_vec_ht  <-tail(d2_totalgoals_vec_ht,6)
#   d2_totalgoals_vec_ht  <- as.numeric(d2_totalgoals_vec_ht)
#   d2_totalgoals_vec_ht
#   d2_ht_totalgoals <- sum(d2_totalgoals_vec_ht)
#   d2_ht_avgtotalgoals <- (d2_ht_totalgoals/6)
#   d2_ht_no_of_ov25 <- length(which(d2_totalgoals_vec_ht >= 3))
#   d2_ht_no_of_un25 <- length(which(d2_totalgoals_vec_ht <= 2))
#   #awayteam
#   d2_totalgoals_vec_at <- as.vector(d2_totalgoals_h[d2_awayteamindex,])
#   d2_totalgoals_vec_at[is.na(d2_totalgoals_vec_at)] <- ""
#   d2_totalgoals_vec_at <- d2_totalgoals_vec_at[d2_totalgoals_vec_at != ""]
#   d2_totalgoals_vec_at  <-tail(d2_totalgoals_vec_at,6)
#   d2_totalgoals_vec_at  <- as.numeric(d2_totalgoals_vec_at)
#   d2_totalgoals_vec_at
#   d2_at_totalgoals <- sum(d2_totalgoals_vec_at)
#   d2_at_avgtotalgoals <- (d2_at_totalgoals/6)
#   d2_at_no_of_ov25 <- length(which(d2_totalgoals_vec_at >= 3))
#   d2_at_no_of_un25 <- length(which(d2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   d2_winmargin_vec_ht <- as.vector(d2_winmargin_h[d2_hometeamindex,])
#   d2_winmargin_vec_ht[is.na(d2_winmargin_vec_ht)] <- ""
#   d2_winmargin_vec_ht <- d2_winmargin_vec_ht[d2_winmargin_vec_ht != ""]
#   d2_winmargin_vec_ht  <-tail(d2_winmargin_vec_ht,6)
#   d2_winmargin_vec_ht  <- as.numeric(d2_winmargin_vec_ht)
#
#   d2_ht_totalwinmargin <- sum(d2_winmargin_vec_ht)
#   d2_ht_no_of_winmargin_ov0 <- length(which(d2_winmargin_vec_ht >= 0))
#   d2_ht_no_of_winmargin_ov1 <- length(which(d2_winmargin_vec_ht >= 1))
#   d2_ht_no_of_winmargin_un0 <- length(which(d2_winmargin_vec_ht <= 0))
#   d2_ht_no_of_winmargin_un1 <- length(which(d2_winmargin_vec_ht <= 1))
#   #awayteam
#   d2_winmargin_vec_at <- as.vector(d2_winmargin_h[d2_awayteamindex,])
#   d2_winmargin_vec_at[is.na(d2_winmargin_vec_at)] <- ""
#   d2_winmargin_vec_at <- d2_winmargin_vec_at[d2_winmargin_vec_at != ""]
#   d2_winmargin_vec_at  <-tail(d2_winmargin_vec_at,6)
#   d2_winmargin_vec_at  <- as.numeric(d2_winmargin_vec_at)
#
#   d2_at_totalwinmargin <- sum(d2_winmargin_vec_at)
#   d2_at_no_of_winmargin_ov0 <- length(which(d2_winmargin_vec_at >= 0))
#   d2_at_no_of_winmargin_ov1 <- length(which(d2_winmargin_vec_at >= 1))
#   d2_at_no_of_winmargin_un0 <- length(which(d2_winmargin_vec_at <= 0))
#   d2_at_no_of_winmargin_un1 <- length(which(d2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   d2_winmargin_vec_ht_lm <- as.vector(d2_winmargin_h[d2_hometeamindex,])
#   d2_winmargin_vec_ht_lm[is.na(d2_winmargin_vec_ht_lm)] <- ""
#   d2_winmargin_vec_ht_lm <- d2_winmargin_vec_ht_lm[d2_winmargin_vec_ht_lm != ""]
#   d2_winmargin_vec_ht_lm  <-tail(d2_winmargin_vec_ht_lm,1)
#   #awayteam
#   d2_winmargin_vec_at_lm <- as.vector(d2_winmargin_h[d2_awayteamindex,])
#   d2_winmargin_vec_at_lm[is.na(d2_winmargin_vec_at_lm)] <- ""
#   d2_winmargin_vec_at_lm <- d2_winmargin_vec_at_lm[d2_winmargin_vec_at_lm != ""]
#   d2_winmargin_vec_at_lm  <-tail(d2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   d2_yellowtotals_vec_ht <- as.vector(d2_yellowtotalsv2[d2_hometeamindex,])
#   d2_yellowtotals_vec_ht[is.na(d2_yellowtotals_vec_ht)] <- ""
#   d2_yellowtotals_vec_ht <- d2_yellowtotals_vec_ht[d2_yellowtotals_vec_ht != ""]
#   d2_yellowtotals_vec_ht  <-tail(d2_yellowtotals_vec_ht,1)
#   #awayteam
#   d2_yellowtotals_vec_at <- as.vector(d2_yellowtotalsv2[d2_awayteamindex,])
#   d2_yellowtotals_vec_at[is.na(d2_yellowtotals_vec_at)] <- ""
#   d2_yellowtotals_vec_at <- d2_yellowtotals_vec_at[d2_yellowtotals_vec_at != ""]
#   d2_yellowtotals_vec_at  <-tail(d2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   d2_cornertotals_vec_ht <- as.vector(d2_cornertotalsv2[d2_hometeamindex,])
#   d2_cornertotals_vec_ht[is.na(d2_cornertotals_vec_ht)] <- ""
#   d2_cornertotals_vec_ht <- d2_cornertotals_vec_ht[d2_cornertotals_vec_ht != ""]
#   d2_cornertotals_vec_ht  <-tail(d2_cornertotals_vec_ht,1)
#   #awayteam
#   d2_cornertotals_vec_at <- as.vector(d2_cornertotalsv2[d2_awayteamindex,])
#   d2_cornertotals_vec_at[is.na(d2_cornertotals_vec_at)] <- ""
#   d2_cornertotals_vec_at <- d2_cornertotals_vec_at[d2_cornertotals_vec_at != ""]
#   d2_cornertotals_vec_at  <-tail(d2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   d2_xshotsconversion_vec_ht <- as.vector(d2_shots_analysis[d2_hometeamindex,])
#   d2_xshotsconversion_vec_ht[is.na(d2_xshotsconversion_vec_ht)] <- ""
#   d2_xshotsconversion_vec_ht <- d2_xshotsconversion_vec_ht[d2_xshotsconversion_vec_ht != ""]
#   d2_xshotsconversion_vec_ht  <-tail(d2_xshotsconversion_vec_ht,1)
#   #awayteam
#   d2_xshotsconversion_vec_at <- as.vector(d2_shots_analysis[d2_awayteamindex,])
#   d2_xshotsconversion_vec_at[is.na(d2_xshotsconversion_vec_at)] <- ""
#   d2_xshotsconversion_vec_at <- d2_xshotsconversion_vec_at[d2_xshotsconversion_vec_at != ""]
#   d2_xshotsconversion_vec_at  <-tail(d2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   d2_fouls_conversion_vec_ht <- as.vector(d2_fouls_conversion[d2_hometeamindex,])
#   d2_fouls_conversion_vec_ht[is.na(d2_fouls_conversion_vec_ht)] <- ""
#   d2_fouls_conversion_vec_ht <- d2_fouls_conversion_vec_ht[d2_fouls_conversion_vec_ht != ""]
#   d2_fouls_conversion_vec_ht  <-tail(d2_fouls_conversion_vec_ht,1)
#   #awayteam
#   d2_fouls_conversion_vec_at <- as.vector(d2_fouls_conversion[d2_awayteamindex,])
#   d2_fouls_conversion_vec_at[is.na(d2_fouls_conversion_vec_at)] <- ""
#   d2_fouls_conversion_vec_at <- d2_fouls_conversion_vec_at[d2_fouls_conversion_vec_at != ""]
#   d2_fouls_conversion_vec_at  <-tail(d2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   d2_ht_last6points <- d2_ht_numberof_wins*3 + d2_ht_numberof_draws*1
#   d2_at_last6points <- d2_at_numberof_wins*3 + d2_at_numberof_draws*1
#
#   if(d2_ht_last6points > d2_at_last6points) {d2_3waypick <- "1"}  else {d2_3waypick <- "X2"}
#
#   if(d2_at_last6points > d2_ht_last6points ) {d2_3waypick <- "2"} else {d2_3waypick <- "1X"}
#
#   if(d2_ht_no_of_ov25 + d2_at_no_of_ov25 >= 6) {d2_goalspick <- "ov25"} else {d2_goalspick <- "un25"}
#
#   if(d2_ht_no_of_un25 + d2_at_no_of_un25 >= 6) {d2_goalspick <- "un25"} else {d2_goalspick <- "ov25"}
#
#   if(d2_ht_matches_scoring >= 4 && d2_at_matches_scoring >=4) {d2_btts <- "BTTS-Y"} else {d2_btts <- "BTTS-N"}
#
#
#   d2_prediction[d2_row] <- rbind(paste(d2_3waypick,d2_goalspick,d2_btts,sep = ","))
#   d2_HWM[d2_row] <- d2_ht_totalwinmargin
#   d2_AWM[d2_row] <- d2_at_totalwinmargin
#
#   d2_HWMLM[d2_row] <- d2_winmargin_vec_ht_lm
#   d2_AWMLM[d2_row] <- d2_winmargin_vec_at_lm
#
#   d2_HY[d2_row] <- d2_yellowtotals_vec_ht
#   d2_AY[d2_row] <- d2_yellowtotals_vec_at
#
#   d2_HCO[d2_row] <- d2_cornertotals_vec_ht
#   d2_ACO[d2_row] <- d2_cornertotals_vec_at
#
#   d2_HXSC[d2_row] <- d2_xshotsconversion_vec_ht
#   d2_AXSC[d2_row] <- d2_xshotsconversion_vec_at
#
#   d2_HYCPF[d2_row] <- d2_fouls_conversion_vec_ht
#   d2_AYCPF[d2_row] <- d2_fouls_conversion_vec_at
# }
#
# d2_prediction <- as.data.frame(d2_prediction)
# colnames(d2_prediction) <- "prediction"
#
# d2_HWM <- as.data.frame(d2_HWM)
# colnames(d2_HWM) <- "HWM"
#
# d2_AWM <- as.data.frame(d2_AWM)
# colnames(d2_AWM) <- "AWM"
#
# d2_HWMLM <- as.data.frame(d2_HWMLM)
# colnames(d2_HWMLM) <- "HWMLM"
#
# d2_AWMLM <- as.data.frame(d2_AWMLM)
# colnames(d2_AWMLM) <- "AWMLM"
#
# d2_HY <- as.data.frame(d2_HY)
# colnames(d2_HY) <- "AVGHY"
#
# d2_AY <- as.data.frame(d2_AY)
# colnames(d2_AY) <- "AVGAY"
#
# d2_HCO <- as.data.frame(d2_HCO)
# colnames(d2_HCO) <- "AVGHCO"
#
# d2_ACO <- as.data.frame(d2_ACO)
# colnames(d2_ACO) <- "AVGACO"
#
# d2_HXSC <- as.data.frame(d2_HXSC)
# colnames(d2_HXSC) <- "HXSC"
#
# d2_AXSC <- as.data.frame(d2_AXSC)
# colnames(d2_AXSC) <- "AXSC"
#
# d2_HYCPF <- as.data.frame(d2_HYCPF)
# colnames(d2_HYCPF) <- "HYCPF"
#
# d2_AYCPF <- as.data.frame(d2_AYCPF)
# colnames(d2_AYCPF) <- "AYCPF"
#
# d2_picks <- cbind(D2_fixtures$Div,D2_fixtures$HomeTeam_d2,D2_fixtures$AwayTeam_d2,d2_prediction,d2_HWM,d2_AWM,d2_HWMLM,d2_AWMLM,d2_HY,d2_AY,d2_HCO,d2_ACO,d2_HXSC,d2_AXSC,d2_HYCPF,d2_AYCPF)
#
# colnames(d2_picks)[1] <- "picks_Div"
# colnames(d2_picks)[2] <- "picks_HomeTeam"
# colnames(d2_picks)[3] <- "picks_AwayTeam"
# d2_picks$matchid <- paste(d2_picks$picks_HomeTeam,d2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of D2
# d2_picks
# ############################################################################################
# #E0
# E0_fixtures$Hometeam_e0_index <- match(E0_fixtures$HomeTeam_e0,e0_teams)
# E0_fixtures$Awayteam_e0_index <- match(E0_fixtures$AwayTeam_e0,e0_teams)
# e0_prediction <- c()
# e0_HWM <- c()
# e0_AWM <- c()
# e0_HWMLM <- c()
# e0_AWMLM <- c()
# e0_HY <- c()
# e0_AY <- c()
# e0_HCO <- c()
# e0_ACO <- c()
# e0_HXSC <- c()
# e0_AXSC <- c()
# e0_HYCPF <- c()
# e0_AYCPF <- c()
# for(e0_row in 1:nrow(E0_fixtures))
# {
#
#   e0_hometeamindex <- E0_fixtures[e0_row,"Hometeam_e0_index"]
#   e0_awayteamindex <- E0_fixtures[e0_row,"Awayteam_e0_index"]
#   #analyse team form
#   #home team
#   e0_form_vec_ht <- as.vector(e0_form_h[e0_hometeamindex,])
#   e0_form_vec_ht[is.na(e0_form_vec_ht)] <- ""
#   e0_form_vec_ht <- e0_form_vec_ht[e0_form_vec_ht != ""]
#   e0_form_vec_ht  <-tail(e0_form_vec_ht,6)
#   e0_ht_numberof_wins <- length(which(e0_form_vec_ht == "W"))
#   e0_ht_numberof_draws <- length(which(e0_form_vec_ht == "D"))
#   e0_ht_numberof_loss <- length(which(e0_form_vec_ht == "L"))
#   #awayteam
#   e0_form_vec_at <- as.vector(e0_form_h[e0_awayteamindex,])
#   e0_form_vec_at[is.na(e0_form_vec_at)] <- ""
#   e0_form_vec_at <- e0_form_vec_at[e0_form_vec_at != ""]
#   e0_form_vec_at  <-tail(e0_form_vec_at,6)
#   e0_at_numberof_wins <- length(which(e0_form_vec_at == "W"))
#   e0_at_numberof_draws <- length(which(e0_form_vec_at == "D"))
#   e0_at_numberof_loss <- length(which(e0_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   e0_goalscored_vec_ht <- as.vector(e0_goalscored_h[e0_hometeamindex,])
#   e0_goalscored_vec_ht[is.na(e0_goalscored_vec_ht)] <- ""
#   e0_goalscored_vec_ht <- e0_goalscored_vec_ht[e0_goalscored_vec_ht != ""]
#   e0_goalscored_vec_ht  <-tail(e0_goalscored_vec_ht,6)
#   e0_goalscored_vec_ht  <- as.numeric(e0_goalscored_vec_ht)
#   e0_ht_totalgoalscored <- sum(e0_goalscored_vec_ht)
#   e0_ht_matches_scoring <- length(which(e0_goalscored_vec_ht > 0))
#   e0_ht_matches_without_scoring <- length(which(e0_goalscored_vec_ht == "0"))
#   #awayteam
#   e0_goalscored_vec_at <- as.vector(e0_goalscored_h[e0_awayteamindex,])
#   e0_goalscored_vec_at[is.na(e0_goalscored_vec_at)] <- ""
#   e0_goalscored_vec_at <- e0_goalscored_vec_at[e0_goalscored_vec_at != ""]
#   e0_goalscored_vec_at  <-tail(e0_goalscored_vec_at,6)
#   e0_goalscored_vec_at  <- as.numeric(e0_goalscored_vec_at)
#   e0_at_totalgoalscored <- sum(e0_goalscored_vec_at)
#   e0_at_matches_scoring <- length(which(e0_goalscored_vec_at > 0))
#   e0_at_matches_without_scoring <- length(which(e0_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   e0_goalconceded_vec_ht <- as.vector(e0_goalconceded_h[e0_hometeamindex,])
#   e0_goalconceded_vec_ht[is.na(e0_goalconceded_vec_ht)] <- ""
#   e0_goalconceded_vec_ht <- e0_goalconceded_vec_ht[e0_goalconceded_vec_ht != ""]
#   e0_goalconceded_vec_ht  <-tail(e0_goalconceded_vec_ht,6)
#   e0_goalconceded_vec_ht  <- as.numeric(e0_goalconceded_vec_ht)
#   e0_goalconceded_vec_ht
#   e0_ht_totalgoalconceded <- sum(e0_goalconceded_vec_ht)
#   e0_ht_matches_concede <- length(which(e0_goalconceded_vec_ht > 0))
#   e0_ht_matches_without_concede <- length(which(e0_goalconceded_vec_ht == "0"))
#   #awayteam
#   e0_goalconceded_vec_at <- as.vector(e0_goalconceded_h[e0_awayteamindex,])
#   e0_goalconceded_vec_at[is.na(e0_goalconceded_vec_at)] <- ""
#   e0_goalconceded_vec_at <- e0_goalconceded_vec_at[e0_goalconceded_vec_at != ""]
#   e0_goalconceded_vec_at  <-tail(e0_goalconceded_vec_at,6)
#   e0_goalconceded_vec_at  <- as.numeric(e0_goalconceded_vec_at)
#   e0_at_totalgoalconceded <- sum(e0_goalconceded_vec_at)
#   e0_at_matches_concede <- length(which(e0_goalconceded_vec_at > 0))
#   e0_at_matches_without_concede <- length(which(e0_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   e0_totalgoals_vec_ht <- as.vector(e0_totalgoals_h[e0_hometeamindex,])
#   e0_totalgoals_vec_ht[is.na(e0_totalgoals_vec_ht)] <- ""
#   e0_totalgoals_vec_ht <- e0_totalgoals_vec_ht[e0_totalgoals_vec_ht != ""]
#   e0_totalgoals_vec_ht  <-tail(e0_totalgoals_vec_ht,6)
#   e0_totalgoals_vec_ht  <- as.numeric(e0_totalgoals_vec_ht)
#   e0_totalgoals_vec_ht
#   e0_ht_totalgoals <- sum(e0_totalgoals_vec_ht)
#   e0_ht_avgtotalgoals <- (e0_ht_totalgoals/6)
#   e0_ht_no_of_ov25 <- length(which(e0_totalgoals_vec_ht >= 3))
#   e0_ht_no_of_un25 <- length(which(e0_totalgoals_vec_ht <= 2))
#   #awayteam
#   e0_totalgoals_vec_at <- as.vector(e0_totalgoals_h[e0_awayteamindex,])
#   e0_totalgoals_vec_at[is.na(e0_totalgoals_vec_at)] <- ""
#   e0_totalgoals_vec_at <- e0_totalgoals_vec_at[e0_totalgoals_vec_at != ""]
#   e0_totalgoals_vec_at  <-tail(e0_totalgoals_vec_at,6)
#   e0_totalgoals_vec_at  <- as.numeric(e0_totalgoals_vec_at)
#   e0_totalgoals_vec_at
#   e0_at_totalgoals <- sum(e0_totalgoals_vec_at)
#   e0_at_avgtotalgoals <- (e0_at_totalgoals/6)
#   e0_at_no_of_ov25 <- length(which(e0_totalgoals_vec_at >= 3))
#   e0_at_no_of_un25 <- length(which(e0_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   e0_winmargin_vec_ht <- as.vector(e0_winmargin_h[e0_hometeamindex,])
#   e0_winmargin_vec_ht[is.na(e0_winmargin_vec_ht)] <- ""
#   e0_winmargin_vec_ht <- e0_winmargin_vec_ht[e0_winmargin_vec_ht != ""]
#   e0_winmargin_vec_ht  <-tail(e0_winmargin_vec_ht,6)
#   e0_winmargin_vec_ht  <- as.numeric(e0_winmargin_vec_ht)
#
#   e0_ht_totalwinmargin <- sum(e0_winmargin_vec_ht)
#   e0_ht_no_of_winmargin_ov0 <- length(which(e0_winmargin_vec_ht >= 0))
#   e0_ht_no_of_winmargin_ov1 <- length(which(e0_winmargin_vec_ht >= 1))
#   e0_ht_no_of_winmargin_un0 <- length(which(e0_winmargin_vec_ht <= 0))
#   e0_ht_no_of_winmargin_un1 <- length(which(e0_winmargin_vec_ht <= 1))
#   #awayteam
#   e0_winmargin_vec_at <- as.vector(e0_winmargin_h[e0_awayteamindex,])
#   e0_winmargin_vec_at[is.na(e0_winmargin_vec_at)] <- ""
#   e0_winmargin_vec_at <- e0_winmargin_vec_at[e0_winmargin_vec_at != ""]
#   e0_winmargin_vec_at  <-tail(e0_winmargin_vec_at,6)
#   e0_winmargin_vec_at  <- as.numeric(e0_winmargin_vec_at)
#
#   e0_at_totalwinmargin <- sum(e0_winmargin_vec_at)
#   e0_at_no_of_winmargin_ov0 <- length(which(e0_winmargin_vec_at >= 0))
#   e0_at_no_of_winmargin_ov1 <- length(which(e0_winmargin_vec_at >= 1))
#   e0_at_no_of_winmargin_un0 <- length(which(e0_winmargin_vec_at <= 0))
#   e0_at_no_of_winmargin_un1 <- length(which(e0_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   e0_winmargin_vec_ht_lm <- as.vector(e0_winmargin_h[e0_hometeamindex,])
#   e0_winmargin_vec_ht_lm[is.na(e0_winmargin_vec_ht_lm)] <- ""
#   e0_winmargin_vec_ht_lm <- e0_winmargin_vec_ht_lm[e0_winmargin_vec_ht_lm != ""]
#   e0_winmargin_vec_ht_lm  <-tail(e0_winmargin_vec_ht_lm,1)
#   #awayteam
#   e0_winmargin_vec_at_lm <- as.vector(e0_winmargin_h[e0_awayteamindex,])
#   e0_winmargin_vec_at_lm[is.na(e0_winmargin_vec_at_lm)] <- ""
#   e0_winmargin_vec_at_lm <- e0_winmargin_vec_at_lm[e0_winmargin_vec_at_lm != ""]
#   e0_winmargin_vec_at_lm  <-tail(e0_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   e0_yellowtotals_vec_ht <- as.vector(e0_yellowtotalsv2[e0_hometeamindex,])
#   e0_yellowtotals_vec_ht[is.na(e0_yellowtotals_vec_ht)] <- ""
#   e0_yellowtotals_vec_ht <- e0_yellowtotals_vec_ht[e0_yellowtotals_vec_ht != ""]
#   e0_yellowtotals_vec_ht  <-tail(e0_yellowtotals_vec_ht,1)
#   #awayteam
#   e0_yellowtotals_vec_at <- as.vector(e0_yellowtotalsv2[e0_awayteamindex,])
#   e0_yellowtotals_vec_at[is.na(e0_yellowtotals_vec_at)] <- ""
#   e0_yellowtotals_vec_at <- e0_yellowtotals_vec_at[e0_yellowtotals_vec_at != ""]
#   e0_yellowtotals_vec_at  <-tail(e0_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   e0_cornertotals_vec_ht <- as.vector(e0_cornertotalsv2[e0_hometeamindex,])
#   e0_cornertotals_vec_ht[is.na(e0_cornertotals_vec_ht)] <- ""
#   e0_cornertotals_vec_ht <- e0_cornertotals_vec_ht[e0_cornertotals_vec_ht != ""]
#   e0_cornertotals_vec_ht  <-tail(e0_cornertotals_vec_ht,1)
#   #awayteam
#   e0_cornertotals_vec_at <- as.vector(e0_cornertotalsv2[e0_awayteamindex,])
#   e0_cornertotals_vec_at[is.na(e0_cornertotals_vec_at)] <- ""
#   e0_cornertotals_vec_at <- e0_cornertotals_vec_at[e0_cornertotals_vec_at != ""]
#   e0_cornertotals_vec_at  <-tail(e0_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   e0_xshotsconversion_vec_ht <- as.vector(e0_shots_analysis[e0_hometeamindex,])
#   e0_xshotsconversion_vec_ht[is.na(e0_xshotsconversion_vec_ht)] <- ""
#   e0_xshotsconversion_vec_ht <- e0_xshotsconversion_vec_ht[e0_xshotsconversion_vec_ht != ""]
#   e0_xshotsconversion_vec_ht  <-tail(e0_xshotsconversion_vec_ht,1)
#   #awayteam
#   e0_xshotsconversion_vec_at <- as.vector(e0_shots_analysis[e0_awayteamindex,])
#   e0_xshotsconversion_vec_at[is.na(e0_xshotsconversion_vec_at)] <- ""
#   e0_xshotsconversion_vec_at <- e0_xshotsconversion_vec_at[e0_xshotsconversion_vec_at != ""]
#   e0_xshotsconversion_vec_at  <-tail(e0_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   e0_fouls_conversion_vec_ht <- as.vector(e0_fouls_conversion[e0_hometeamindex,])
#   e0_fouls_conversion_vec_ht[is.na(e0_fouls_conversion_vec_ht)] <- ""
#   e0_fouls_conversion_vec_ht <- e0_fouls_conversion_vec_ht[e0_fouls_conversion_vec_ht != ""]
#   e0_fouls_conversion_vec_ht  <-tail(e0_fouls_conversion_vec_ht,1)
#   #awayteam
#   e0_fouls_conversion_vec_at <- as.vector(e0_fouls_conversion[e0_awayteamindex,])
#   e0_fouls_conversion_vec_at[is.na(e0_fouls_conversion_vec_at)] <- ""
#   e0_fouls_conversion_vec_at <- e0_fouls_conversion_vec_at[e0_fouls_conversion_vec_at != ""]
#   e0_fouls_conversion_vec_at  <-tail(e0_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   e0_ht_last6points <- e0_ht_numberof_wins*3 + e0_ht_numberof_draws*1
#   e0_at_last6points <- e0_at_numberof_wins*3 + e0_at_numberof_draws*1
#
#   if(e0_ht_last6points > e0_at_last6points) {e0_3waypick <- "1"}  else {e0_3waypick <- "X2"}
#
#   if(e0_at_last6points > e0_ht_last6points ) {e0_3waypick <- "2"} else {e0_3waypick <- "1X"}
#
#   if(e0_ht_no_of_ov25 + e0_at_no_of_ov25 >= 6) {e0_goalspick <- "ov25"} else {e0_goalspick <- "un25"}
#
#   if(e0_ht_no_of_un25 + e0_at_no_of_un25 >= 6) {e0_goalspick <- "un25"} else {e0_goalspick <- "ov25"}
#
#   if(e0_ht_matches_scoring >= 4 && e0_at_matches_scoring >=4) {e0_btts <- "BTTS-Y"} else {e0_btts <- "BTTS-N"}
#
#
#   e0_prediction[e0_row] <- rbind(paste(e0_3waypick,e0_goalspick,e0_btts,sep = ","))
#   e0_HWM[e0_row] <- e0_ht_totalwinmargin
#   e0_AWM[e0_row] <- e0_at_totalwinmargin
#
#   e0_HWMLM[e0_row] <- e0_winmargin_vec_ht_lm
#   e0_AWMLM[e0_row] <- e0_winmargin_vec_at_lm
#
#   e0_HY[e0_row] <- e0_yellowtotals_vec_ht
#   e0_AY[e0_row] <- e0_yellowtotals_vec_at
#
#   e0_HCO[e0_row] <- e0_cornertotals_vec_ht
#   e0_ACO[e0_row] <- e0_cornertotals_vec_at
#
#   e0_HXSC[e0_row] <- e0_xshotsconversion_vec_ht
#   e0_AXSC[e0_row] <- e0_xshotsconversion_vec_at
#
#   e0_HYCPF[e0_row] <- e0_fouls_conversion_vec_ht
#   e0_AYCPF[e0_row] <- e0_fouls_conversion_vec_at
# }
#
# e0_prediction <- as.data.frame(e0_prediction)
# colnames(e0_prediction) <- "prediction"
#
# e0_HWM <- as.data.frame(e0_HWM)
# colnames(e0_HWM) <- "HWM"
#
# e0_AWM <- as.data.frame(e0_AWM)
# colnames(e0_AWM) <- "AWM"
#
# e0_HWMLM <- as.data.frame(e0_HWMLM)
# colnames(e0_HWMLM) <- "HWMLM"
#
# e0_AWMLM <- as.data.frame(e0_AWMLM)
# colnames(e0_AWMLM) <- "AWMLM"
#
# e0_HY <- as.data.frame(e0_HY)
# colnames(e0_HY) <- "AVGHY"
#
# e0_AY <- as.data.frame(e0_AY)
# colnames(e0_AY) <- "AVGAY"
#
# e0_HCO <- as.data.frame(e0_HCO)
# colnames(e0_HCO) <- "AVGHCO"
#
# e0_ACO <- as.data.frame(e0_ACO)
# colnames(e0_ACO) <- "AVGACO"
#
# e0_HXSC <- as.data.frame(e0_HXSC)
# colnames(e0_HXSC) <- "HXSC"
#
# e0_AXSC <- as.data.frame(e0_AXSC)
# colnames(e0_AXSC) <- "AXSC"
#
# e0_HYCPF <- as.data.frame(e0_HYCPF)
# colnames(e0_HYCPF) <- "HYCPF"
#
# e0_AYCPF <- as.data.frame(e0_AYCPF)
# colnames(e0_AYCPF) <- "AYCPF"
#
# e0_picks <- cbind(E0_fixtures$Div,E0_fixtures$HomeTeam_e0,E0_fixtures$AwayTeam_e0,e0_prediction,e0_HWM,e0_AWM,e0_HWMLM,e0_AWMLM,e0_HY,e0_AY,e0_HCO,e0_ACO,e0_HXSC,e0_AXSC,e0_HYCPF,e0_AYCPF)
#
# colnames(e0_picks)[1] <- "picks_Div"
# colnames(e0_picks)[2] <- "picks_HomeTeam"
# colnames(e0_picks)[3] <- "picks_AwayTeam"
# e0_picks$matchid <- paste(e0_picks$picks_HomeTeam,e0_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of E0
# e0_picks
# #E1
# E1_fixtures$Hometeam_e1_index <- match(E1_fixtures$HomeTeam_e1,e1_teams)
# E1_fixtures$Awayteam_e1_index <- match(E1_fixtures$AwayTeam_e1,e1_teams)
# e1_prediction <- c()
# e1_HWM <- c()
# e1_AWM <- c()
# e1_HWMLM <- c()
# e1_AWMLM <- c()
# e1_HY <- c()
# e1_AY <- c()
# e1_HCO <- c()
# e1_ACO <- c()
# e1_HXSC <- c()
# e1_AXSC <- c()
# e1_HYCPF <- c()
# e1_AYCPF <- c()
# for(e1_row in 1:nrow(E1_fixtures))
# {
#
#   e1_hometeamindex <- E1_fixtures[e1_row,"Hometeam_e1_index"]
#   e1_awayteamindex <- E1_fixtures[e1_row,"Awayteam_e1_index"]
#   #analyse team form
#   #home team
#   e1_form_vec_ht <- as.vector(e1_form_h[e1_hometeamindex,])
#   e1_form_vec_ht[is.na(e1_form_vec_ht)] <- ""
#   e1_form_vec_ht <- e1_form_vec_ht[e1_form_vec_ht != ""]
#   e1_form_vec_ht  <-tail(e1_form_vec_ht,6)
#   e1_ht_numberof_wins <- length(which(e1_form_vec_ht == "W"))
#   e1_ht_numberof_draws <- length(which(e1_form_vec_ht == "D"))
#   e1_ht_numberof_loss <- length(which(e1_form_vec_ht == "L"))
#   #awayteam
#   e1_form_vec_at <- as.vector(e1_form_h[e1_awayteamindex,])
#   e1_form_vec_at[is.na(e1_form_vec_at)] <- ""
#   e1_form_vec_at <- e1_form_vec_at[e1_form_vec_at != ""]
#   e1_form_vec_at  <-tail(e1_form_vec_at,6)
#   e1_at_numberof_wins <- length(which(e1_form_vec_at == "W"))
#   e1_at_numberof_draws <- length(which(e1_form_vec_at == "D"))
#   e1_at_numberof_loss <- length(which(e1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   e1_goalscored_vec_ht <- as.vector(e1_goalscored_h[e1_hometeamindex,])
#   e1_goalscored_vec_ht[is.na(e1_goalscored_vec_ht)] <- ""
#   e1_goalscored_vec_ht <- e1_goalscored_vec_ht[e1_goalscored_vec_ht != ""]
#   e1_goalscored_vec_ht  <-tail(e1_goalscored_vec_ht,6)
#   e1_goalscored_vec_ht  <- as.numeric(e1_goalscored_vec_ht)
#   e1_ht_totalgoalscored <- sum(e1_goalscored_vec_ht)
#   e1_ht_matches_scoring <- length(which(e1_goalscored_vec_ht > 0))
#   e1_ht_matches_without_scoring <- length(which(e1_goalscored_vec_ht == "0"))
#   #awayteam
#   e1_goalscored_vec_at <- as.vector(e1_goalscored_h[e1_awayteamindex,])
#   e1_goalscored_vec_at[is.na(e1_goalscored_vec_at)] <- ""
#   e1_goalscored_vec_at <- e1_goalscored_vec_at[e1_goalscored_vec_at != ""]
#   e1_goalscored_vec_at  <-tail(e1_goalscored_vec_at,6)
#   e1_goalscored_vec_at  <- as.numeric(e1_goalscored_vec_at)
#   e1_at_totalgoalscored <- sum(e1_goalscored_vec_at)
#   e1_at_matches_scoring <- length(which(e1_goalscored_vec_at > 0))
#   e1_at_matches_without_scoring <- length(which(e1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   e1_goalconceded_vec_ht <- as.vector(e1_goalconceded_h[e1_hometeamindex,])
#   e1_goalconceded_vec_ht[is.na(e1_goalconceded_vec_ht)] <- ""
#   e1_goalconceded_vec_ht <- e1_goalconceded_vec_ht[e1_goalconceded_vec_ht != ""]
#   e1_goalconceded_vec_ht  <-tail(e1_goalconceded_vec_ht,6)
#   e1_goalconceded_vec_ht  <- as.numeric(e1_goalconceded_vec_ht)
#   e1_goalconceded_vec_ht
#   e1_ht_totalgoalconceded <- sum(e1_goalconceded_vec_ht)
#   e1_ht_matches_concede <- length(which(e1_goalconceded_vec_ht > 0))
#   e1_ht_matches_without_concede <- length(which(e1_goalconceded_vec_ht == "0"))
#   #awayteam
#   e1_goalconceded_vec_at <- as.vector(e1_goalconceded_h[e1_awayteamindex,])
#   e1_goalconceded_vec_at[is.na(e1_goalconceded_vec_at)] <- ""
#   e1_goalconceded_vec_at <- e1_goalconceded_vec_at[e1_goalconceded_vec_at != ""]
#   e1_goalconceded_vec_at  <-tail(e1_goalconceded_vec_at,6)
#   e1_goalconceded_vec_at  <- as.numeric(e1_goalconceded_vec_at)
#   e1_at_totalgoalconceded <- sum(e1_goalconceded_vec_at)
#   e1_at_matches_concede <- length(which(e1_goalconceded_vec_at > 0))
#   e1_at_matches_without_concede <- length(which(e1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   e1_totalgoals_vec_ht <- as.vector(e1_totalgoals_h[e1_hometeamindex,])
#   e1_totalgoals_vec_ht[is.na(e1_totalgoals_vec_ht)] <- ""
#   e1_totalgoals_vec_ht <- e1_totalgoals_vec_ht[e1_totalgoals_vec_ht != ""]
#   e1_totalgoals_vec_ht  <-tail(e1_totalgoals_vec_ht,6)
#   e1_totalgoals_vec_ht  <- as.numeric(e1_totalgoals_vec_ht)
#   e1_totalgoals_vec_ht
#   e1_ht_totalgoals <- sum(e1_totalgoals_vec_ht)
#   e1_ht_avgtotalgoals <- (e1_ht_totalgoals/6)
#   e1_ht_no_of_ov25 <- length(which(e1_totalgoals_vec_ht >= 3))
#   e1_ht_no_of_un25 <- length(which(e1_totalgoals_vec_ht <= 2))
#   #awayteam
#   e1_totalgoals_vec_at <- as.vector(e1_totalgoals_h[e1_awayteamindex,])
#   e1_totalgoals_vec_at[is.na(e1_totalgoals_vec_at)] <- ""
#   e1_totalgoals_vec_at <- e1_totalgoals_vec_at[e1_totalgoals_vec_at != ""]
#   e1_totalgoals_vec_at  <-tail(e1_totalgoals_vec_at,6)
#   e1_totalgoals_vec_at  <- as.numeric(e1_totalgoals_vec_at)
#   e1_totalgoals_vec_at
#   e1_at_totalgoals <- sum(e1_totalgoals_vec_at)
#   e1_at_avgtotalgoals <- (e1_at_totalgoals/6)
#   e1_at_no_of_ov25 <- length(which(e1_totalgoals_vec_at >= 3))
#   e1_at_no_of_un25 <- length(which(e1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   e1_winmargin_vec_ht <- as.vector(e1_winmargin_h[e1_hometeamindex,])
#   e1_winmargin_vec_ht[is.na(e1_winmargin_vec_ht)] <- ""
#   e1_winmargin_vec_ht <- e1_winmargin_vec_ht[e1_winmargin_vec_ht != ""]
#   e1_winmargin_vec_ht  <-tail(e1_winmargin_vec_ht,6)
#   e1_winmargin_vec_ht  <- as.numeric(e1_winmargin_vec_ht)
#
#   e1_ht_totalwinmargin <- sum(e1_winmargin_vec_ht)
#   e1_ht_no_of_winmargin_ov0 <- length(which(e1_winmargin_vec_ht >= 0))
#   e1_ht_no_of_winmargin_ov1 <- length(which(e1_winmargin_vec_ht >= 1))
#   e1_ht_no_of_winmargin_un0 <- length(which(e1_winmargin_vec_ht <= 0))
#   e1_ht_no_of_winmargin_un1 <- length(which(e1_winmargin_vec_ht <= 1))
#   #awayteam
#   e1_winmargin_vec_at <- as.vector(e1_winmargin_h[e1_awayteamindex,])
#   e1_winmargin_vec_at[is.na(e1_winmargin_vec_at)] <- ""
#   e1_winmargin_vec_at <- e1_winmargin_vec_at[e1_winmargin_vec_at != ""]
#   e1_winmargin_vec_at  <-tail(e1_winmargin_vec_at,6)
#   e1_winmargin_vec_at  <- as.numeric(e1_winmargin_vec_at)
#
#   e1_at_totalwinmargin <- sum(e1_winmargin_vec_at)
#   e1_at_no_of_winmargin_ov0 <- length(which(e1_winmargin_vec_at >= 0))
#   e1_at_no_of_winmargin_ov1 <- length(which(e1_winmargin_vec_at >= 1))
#   e1_at_no_of_winmargin_un0 <- length(which(e1_winmargin_vec_at <= 0))
#   e1_at_no_of_winmargin_un1 <- length(which(e1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   e1_winmargin_vec_ht_lm <- as.vector(e1_winmargin_h[e1_hometeamindex,])
#   e1_winmargin_vec_ht_lm[is.na(e1_winmargin_vec_ht_lm)] <- ""
#   e1_winmargin_vec_ht_lm <- e1_winmargin_vec_ht_lm[e1_winmargin_vec_ht_lm != ""]
#   e1_winmargin_vec_ht_lm  <-tail(e1_winmargin_vec_ht_lm,1)
#   #awayteam
#   e1_winmargin_vec_at_lm <- as.vector(e1_winmargin_h[e1_awayteamindex,])
#   e1_winmargin_vec_at_lm[is.na(e1_winmargin_vec_at_lm)] <- ""
#   e1_winmargin_vec_at_lm <- e1_winmargin_vec_at_lm[e1_winmargin_vec_at_lm != ""]
#   e1_winmargin_vec_at_lm  <-tail(e1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   e1_yellowtotals_vec_ht <- as.vector(e1_yellowtotalsv2[e1_hometeamindex,])
#   e1_yellowtotals_vec_ht[is.na(e1_yellowtotals_vec_ht)] <- ""
#   e1_yellowtotals_vec_ht <- e1_yellowtotals_vec_ht[e1_yellowtotals_vec_ht != ""]
#   e1_yellowtotals_vec_ht  <-tail(e1_yellowtotals_vec_ht,1)
#   #awayteam
#   e1_yellowtotals_vec_at <- as.vector(e1_yellowtotalsv2[e1_awayteamindex,])
#   e1_yellowtotals_vec_at[is.na(e1_yellowtotals_vec_at)] <- ""
#   e1_yellowtotals_vec_at <- e1_yellowtotals_vec_at[e1_yellowtotals_vec_at != ""]
#   e1_yellowtotals_vec_at  <-tail(e1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   e1_cornertotals_vec_ht <- as.vector(e1_cornertotalsv2[e1_hometeamindex,])
#   e1_cornertotals_vec_ht[is.na(e1_cornertotals_vec_ht)] <- ""
#   e1_cornertotals_vec_ht <- e1_cornertotals_vec_ht[e1_cornertotals_vec_ht != ""]
#   e1_cornertotals_vec_ht  <-tail(e1_cornertotals_vec_ht,1)
#   #awayteam
#   e1_cornertotals_vec_at <- as.vector(e1_cornertotalsv2[e1_awayteamindex,])
#   e1_cornertotals_vec_at[is.na(e1_cornertotals_vec_at)] <- ""
#   e1_cornertotals_vec_at <- e1_cornertotals_vec_at[e1_cornertotals_vec_at != ""]
#   e1_cornertotals_vec_at  <-tail(e1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   e1_xshotsconversion_vec_ht <- as.vector(e1_shots_analysis[e1_hometeamindex,])
#   e1_xshotsconversion_vec_ht[is.na(e1_xshotsconversion_vec_ht)] <- ""
#   e1_xshotsconversion_vec_ht <- e1_xshotsconversion_vec_ht[e1_xshotsconversion_vec_ht != ""]
#   e1_xshotsconversion_vec_ht  <-tail(e1_xshotsconversion_vec_ht,1)
#   #awayteam
#   e1_xshotsconversion_vec_at <- as.vector(e1_shots_analysis[e1_awayteamindex,])
#   e1_xshotsconversion_vec_at[is.na(e1_xshotsconversion_vec_at)] <- ""
#   e1_xshotsconversion_vec_at <- e1_xshotsconversion_vec_at[e1_xshotsconversion_vec_at != ""]
#   e1_xshotsconversion_vec_at  <-tail(e1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   e1_fouls_conversion_vec_ht <- as.vector(e1_fouls_conversion[e1_hometeamindex,])
#   e1_fouls_conversion_vec_ht[is.na(e1_fouls_conversion_vec_ht)] <- ""
#   e1_fouls_conversion_vec_ht <- e1_fouls_conversion_vec_ht[e1_fouls_conversion_vec_ht != ""]
#   e1_fouls_conversion_vec_ht  <-tail(e1_fouls_conversion_vec_ht,1)
#   #awayteam
#   e1_fouls_conversion_vec_at <- as.vector(e1_fouls_conversion[e1_awayteamindex,])
#   e1_fouls_conversion_vec_at[is.na(e1_fouls_conversion_vec_at)] <- ""
#   e1_fouls_conversion_vec_at <- e1_fouls_conversion_vec_at[e1_fouls_conversion_vec_at != ""]
#   e1_fouls_conversion_vec_at  <-tail(e1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   e1_ht_last6points <- e1_ht_numberof_wins*3 + e1_ht_numberof_draws*1
#   e1_at_last6points <- e1_at_numberof_wins*3 + e1_at_numberof_draws*1
#
#   if(e1_ht_last6points > e1_at_last6points) {e1_3waypick <- "1"}  else {e1_3waypick <- "X2"}
#
#   if(e1_at_last6points > e1_ht_last6points ) {e1_3waypick <- "2"} else {e1_3waypick <- "1X"}
#
#   if(e1_ht_no_of_ov25 + e1_at_no_of_ov25 >= 6) {e1_goalspick <- "ov25"} else {e1_goalspick <- "un25"}
#
#   if(e1_ht_no_of_un25 + e1_at_no_of_un25 >= 6) {e1_goalspick <- "un25"} else {e1_goalspick <- "ov25"}
#
#   if(e1_ht_matches_scoring >= 4 && e1_at_matches_scoring >=4) {e1_btts <- "BTTS-Y"} else {e1_btts <- "BTTS-N"}
#
#
#   e1_prediction[e1_row] <- rbind(paste(e1_3waypick,e1_goalspick,e1_btts,sep = ","))
#   e1_HWM[e1_row] <- e1_ht_totalwinmargin
#   e1_AWM[e1_row] <- e1_at_totalwinmargin
#
#   e1_HWMLM[e1_row] <- e1_winmargin_vec_ht_lm
#   e1_AWMLM[e1_row] <- e1_winmargin_vec_at_lm
#
#   e1_HY[e1_row] <- e1_yellowtotals_vec_ht
#   e1_AY[e1_row] <- e1_yellowtotals_vec_at
#
#   e1_HCO[e1_row] <- e1_cornertotals_vec_ht
#   e1_ACO[e1_row] <- e1_cornertotals_vec_at
#
#   e1_HXSC[e1_row] <- e1_xshotsconversion_vec_ht
#   e1_AXSC[e1_row] <- e1_xshotsconversion_vec_at
#
#   e1_HYCPF[e1_row] <- e1_fouls_conversion_vec_ht
#   e1_AYCPF[e1_row] <- e1_fouls_conversion_vec_at
# }
#
# e1_prediction <- as.data.frame(e1_prediction)
# colnames(e1_prediction) <- "prediction"
#
# e1_HWM <- as.data.frame(e1_HWM)
# colnames(e1_HWM) <- "HWM"
#
# e1_AWM <- as.data.frame(e1_AWM)
# colnames(e1_AWM) <- "AWM"
#
# e1_HWMLM <- as.data.frame(e1_HWMLM)
# colnames(e1_HWMLM) <- "HWMLM"
#
# e1_AWMLM <- as.data.frame(e1_AWMLM)
# colnames(e1_AWMLM) <- "AWMLM"
#
# e1_HY <- as.data.frame(e1_HY)
# colnames(e1_HY) <- "AVGHY"
#
# e1_AY <- as.data.frame(e1_AY)
# colnames(e1_AY) <- "AVGAY"
#
# e1_HCO <- as.data.frame(e1_HCO)
# colnames(e1_HCO) <- "AVGHCO"
#
# e1_ACO <- as.data.frame(e1_ACO)
# colnames(e1_ACO) <- "AVGACO"
#
# e1_HXSC <- as.data.frame(e1_HXSC)
# colnames(e1_HXSC) <- "HXSC"
#
# e1_AXSC <- as.data.frame(e1_AXSC)
# colnames(e1_AXSC) <- "AXSC"
#
# e1_HYCPF <- as.data.frame(e1_HYCPF)
# colnames(e1_HYCPF) <- "HYCPF"
#
# e1_AYCPF <- as.data.frame(e1_AYCPF)
# colnames(e1_AYCPF) <- "AYCPF"
#
# e1_picks <- cbind(E1_fixtures$Div,E1_fixtures$HomeTeam_e1,E1_fixtures$AwayTeam_e1,e1_prediction,e1_HWM,e1_AWM,e1_HWMLM,e1_AWMLM,e1_HY,e1_AY,e1_HCO,e1_ACO,e1_HXSC,e1_AXSC,e1_HYCPF,e1_AYCPF)
#
# colnames(e1_picks)[1] <- "picks_Div"
# colnames(e1_picks)[2] <- "picks_HomeTeam"
# colnames(e1_picks)[3] <- "picks_AwayTeam"
# e1_picks$matchid <- paste(e1_picks$picks_HomeTeam,e1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of E1
# e1_picks
# #E2
# E2_fixtures$Hometeam_e2_index <- match(E2_fixtures$HomeTeam_e2,e2_teams)
# E2_fixtures$Awayteam_e2_index <- match(E2_fixtures$AwayTeam_e2,e2_teams)
# e2_prediction <- c()
# e2_HWM <- c()
# e2_AWM <- c()
# e2_HWMLM <- c()
# e2_AWMLM <- c()
# e2_HY <- c()
# e2_AY <- c()
# e2_HCO <- c()
# e2_ACO <- c()
# e2_HXSC <- c()
# e2_AXSC <- c()
# e2_HYCPF <- c()
# e2_AYCPF <- c()
# for(e2_row in 1:nrow(E2_fixtures))
# {
#
#   e2_hometeamindex <- E2_fixtures[e2_row,"Hometeam_e2_index"]
#   e2_awayteamindex <- E2_fixtures[e2_row,"Awayteam_e2_index"]
#   #analyse team form
#   #home team
#   e2_form_vec_ht <- as.vector(e2_form_h[e2_hometeamindex,])
#   e2_form_vec_ht[is.na(e2_form_vec_ht)] <- ""
#   e2_form_vec_ht <- e2_form_vec_ht[e2_form_vec_ht != ""]
#   e2_form_vec_ht  <-tail(e2_form_vec_ht,6)
#   e2_ht_numberof_wins <- length(which(e2_form_vec_ht == "W"))
#   e2_ht_numberof_draws <- length(which(e2_form_vec_ht == "D"))
#   e2_ht_numberof_loss <- length(which(e2_form_vec_ht == "L"))
#   #awayteam
#   e2_form_vec_at <- as.vector(e2_form_h[e2_awayteamindex,])
#   e2_form_vec_at[is.na(e2_form_vec_at)] <- ""
#   e2_form_vec_at <- e2_form_vec_at[e2_form_vec_at != ""]
#   e2_form_vec_at  <-tail(e2_form_vec_at,6)
#   e2_at_numberof_wins <- length(which(e2_form_vec_at == "W"))
#   e2_at_numberof_draws <- length(which(e2_form_vec_at == "D"))
#   e2_at_numberof_loss <- length(which(e2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   e2_goalscored_vec_ht <- as.vector(e2_goalscored_h[e2_hometeamindex,])
#   e2_goalscored_vec_ht[is.na(e2_goalscored_vec_ht)] <- ""
#   e2_goalscored_vec_ht <- e2_goalscored_vec_ht[e2_goalscored_vec_ht != ""]
#   e2_goalscored_vec_ht  <-tail(e2_goalscored_vec_ht,6)
#   e2_goalscored_vec_ht  <- as.numeric(e2_goalscored_vec_ht)
#   e2_ht_totalgoalscored <- sum(e2_goalscored_vec_ht)
#   e2_ht_matches_scoring <- length(which(e2_goalscored_vec_ht > 0))
#   e2_ht_matches_without_scoring <- length(which(e2_goalscored_vec_ht == "0"))
#   #awayteam
#   e2_goalscored_vec_at <- as.vector(e2_goalscored_h[e2_awayteamindex,])
#   e2_goalscored_vec_at[is.na(e2_goalscored_vec_at)] <- ""
#   e2_goalscored_vec_at <- e2_goalscored_vec_at[e2_goalscored_vec_at != ""]
#   e2_goalscored_vec_at  <-tail(e2_goalscored_vec_at,6)
#   e2_goalscored_vec_at  <- as.numeric(e2_goalscored_vec_at)
#   e2_at_totalgoalscored <- sum(e2_goalscored_vec_at)
#   e2_at_matches_scoring <- length(which(e2_goalscored_vec_at > 0))
#   e2_at_matches_without_scoring <- length(which(e2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   e2_goalconceded_vec_ht <- as.vector(e2_goalconceded_h[e2_hometeamindex,])
#   e2_goalconceded_vec_ht[is.na(e2_goalconceded_vec_ht)] <- ""
#   e2_goalconceded_vec_ht <- e2_goalconceded_vec_ht[e2_goalconceded_vec_ht != ""]
#   e2_goalconceded_vec_ht  <-tail(e2_goalconceded_vec_ht,6)
#   e2_goalconceded_vec_ht  <- as.numeric(e2_goalconceded_vec_ht)
#   e2_goalconceded_vec_ht
#   e2_ht_totalgoalconceded <- sum(e2_goalconceded_vec_ht)
#   e2_ht_matches_concede <- length(which(e2_goalconceded_vec_ht > 0))
#   e2_ht_matches_without_concede <- length(which(e2_goalconceded_vec_ht == "0"))
#   #awayteam
#   e2_goalconceded_vec_at <- as.vector(e2_goalconceded_h[e2_awayteamindex,])
#   e2_goalconceded_vec_at[is.na(e2_goalconceded_vec_at)] <- ""
#   e2_goalconceded_vec_at <- e2_goalconceded_vec_at[e2_goalconceded_vec_at != ""]
#   e2_goalconceded_vec_at  <-tail(e2_goalconceded_vec_at,6)
#   e2_goalconceded_vec_at  <- as.numeric(e2_goalconceded_vec_at)
#   e2_at_totalgoalconceded <- sum(e2_goalconceded_vec_at)
#   e2_at_matches_concede <- length(which(e2_goalconceded_vec_at > 0))
#   e2_at_matches_without_concede <- length(which(e2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   e2_totalgoals_vec_ht <- as.vector(e2_totalgoals_h[e2_hometeamindex,])
#   e2_totalgoals_vec_ht[is.na(e2_totalgoals_vec_ht)] <- ""
#   e2_totalgoals_vec_ht <- e2_totalgoals_vec_ht[e2_totalgoals_vec_ht != ""]
#   e2_totalgoals_vec_ht  <-tail(e2_totalgoals_vec_ht,6)
#   e2_totalgoals_vec_ht  <- as.numeric(e2_totalgoals_vec_ht)
#   e2_totalgoals_vec_ht
#   e2_ht_totalgoals <- sum(e2_totalgoals_vec_ht)
#   e2_ht_avgtotalgoals <- (e2_ht_totalgoals/6)
#   e2_ht_no_of_ov25 <- length(which(e2_totalgoals_vec_ht >= 3))
#   e2_ht_no_of_un25 <- length(which(e2_totalgoals_vec_ht <= 2))
#   #awayteam
#   e2_totalgoals_vec_at <- as.vector(e2_totalgoals_h[e2_awayteamindex,])
#   e2_totalgoals_vec_at[is.na(e2_totalgoals_vec_at)] <- ""
#   e2_totalgoals_vec_at <- e2_totalgoals_vec_at[e2_totalgoals_vec_at != ""]
#   e2_totalgoals_vec_at  <-tail(e2_totalgoals_vec_at,6)
#   e2_totalgoals_vec_at  <- as.numeric(e2_totalgoals_vec_at)
#   e2_totalgoals_vec_at
#   e2_at_totalgoals <- sum(e2_totalgoals_vec_at)
#   e2_at_avgtotalgoals <- (e2_at_totalgoals/6)
#   e2_at_no_of_ov25 <- length(which(e2_totalgoals_vec_at >= 3))
#   e2_at_no_of_un25 <- length(which(e2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   e2_winmargin_vec_ht <- as.vector(e2_winmargin_h[e2_hometeamindex,])
#   e2_winmargin_vec_ht[is.na(e2_winmargin_vec_ht)] <- ""
#   e2_winmargin_vec_ht <- e2_winmargin_vec_ht[e2_winmargin_vec_ht != ""]
#   e2_winmargin_vec_ht  <-tail(e2_winmargin_vec_ht,6)
#   e2_winmargin_vec_ht  <- as.numeric(e2_winmargin_vec_ht)
#
#   e2_ht_totalwinmargin <- sum(e2_winmargin_vec_ht)
#   e2_ht_no_of_winmargin_ov0 <- length(which(e2_winmargin_vec_ht >= 0))
#   e2_ht_no_of_winmargin_ov1 <- length(which(e2_winmargin_vec_ht >= 1))
#   e2_ht_no_of_winmargin_un0 <- length(which(e2_winmargin_vec_ht <= 0))
#   e2_ht_no_of_winmargin_un1 <- length(which(e2_winmargin_vec_ht <= 1))
#   #awayteam
#   e2_winmargin_vec_at <- as.vector(e2_winmargin_h[e2_awayteamindex,])
#   e2_winmargin_vec_at[is.na(e2_winmargin_vec_at)] <- ""
#   e2_winmargin_vec_at <- e2_winmargin_vec_at[e2_winmargin_vec_at != ""]
#   e2_winmargin_vec_at  <-tail(e2_winmargin_vec_at,6)
#   e2_winmargin_vec_at  <- as.numeric(e2_winmargin_vec_at)
#
#   e2_at_totalwinmargin <- sum(e2_winmargin_vec_at)
#   e2_at_no_of_winmargin_ov0 <- length(which(e2_winmargin_vec_at >= 0))
#   e2_at_no_of_winmargin_ov1 <- length(which(e2_winmargin_vec_at >= 1))
#   e2_at_no_of_winmargin_un0 <- length(which(e2_winmargin_vec_at <= 0))
#   e2_at_no_of_winmargin_un1 <- length(which(e2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   e2_winmargin_vec_ht_lm <- as.vector(e2_winmargin_h[e2_hometeamindex,])
#   e2_winmargin_vec_ht_lm[is.na(e2_winmargin_vec_ht_lm)] <- ""
#   e2_winmargin_vec_ht_lm <- e2_winmargin_vec_ht_lm[e2_winmargin_vec_ht_lm != ""]
#   e2_winmargin_vec_ht_lm  <-tail(e2_winmargin_vec_ht_lm,1)
#   #awayteam
#   e2_winmargin_vec_at_lm <- as.vector(e2_winmargin_h[e2_awayteamindex,])
#   e2_winmargin_vec_at_lm[is.na(e2_winmargin_vec_at_lm)] <- ""
#   e2_winmargin_vec_at_lm <- e2_winmargin_vec_at_lm[e2_winmargin_vec_at_lm != ""]
#   e2_winmargin_vec_at_lm  <-tail(e2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   e2_yellowtotals_vec_ht <- as.vector(e2_yellowtotalsv2[e2_hometeamindex,])
#   e2_yellowtotals_vec_ht[is.na(e2_yellowtotals_vec_ht)] <- ""
#   e2_yellowtotals_vec_ht <- e2_yellowtotals_vec_ht[e2_yellowtotals_vec_ht != ""]
#   e2_yellowtotals_vec_ht  <-tail(e2_yellowtotals_vec_ht,1)
#   #awayteam
#   e2_yellowtotals_vec_at <- as.vector(e2_yellowtotalsv2[e2_awayteamindex,])
#   e2_yellowtotals_vec_at[is.na(e2_yellowtotals_vec_at)] <- ""
#   e2_yellowtotals_vec_at <- e2_yellowtotals_vec_at[e2_yellowtotals_vec_at != ""]
#   e2_yellowtotals_vec_at  <-tail(e2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   e2_cornertotals_vec_ht <- as.vector(e2_cornertotalsv2[e2_hometeamindex,])
#   e2_cornertotals_vec_ht[is.na(e2_cornertotals_vec_ht)] <- ""
#   e2_cornertotals_vec_ht <- e2_cornertotals_vec_ht[e2_cornertotals_vec_ht != ""]
#   e2_cornertotals_vec_ht  <-tail(e2_cornertotals_vec_ht,1)
#   #awayteam
#   e2_cornertotals_vec_at <- as.vector(e2_cornertotalsv2[e2_awayteamindex,])
#   e2_cornertotals_vec_at[is.na(e2_cornertotals_vec_at)] <- ""
#   e2_cornertotals_vec_at <- e2_cornertotals_vec_at[e2_cornertotals_vec_at != ""]
#   e2_cornertotals_vec_at  <-tail(e2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   e2_xshotsconversion_vec_ht <- as.vector(e2_shots_analysis[e2_hometeamindex,])
#   e2_xshotsconversion_vec_ht[is.na(e2_xshotsconversion_vec_ht)] <- ""
#   e2_xshotsconversion_vec_ht <- e2_xshotsconversion_vec_ht[e2_xshotsconversion_vec_ht != ""]
#   e2_xshotsconversion_vec_ht  <-tail(e2_xshotsconversion_vec_ht,1)
#   #awayteam
#   e2_xshotsconversion_vec_at <- as.vector(e2_shots_analysis[e2_awayteamindex,])
#   e2_xshotsconversion_vec_at[is.na(e2_xshotsconversion_vec_at)] <- ""
#   e2_xshotsconversion_vec_at <- e2_xshotsconversion_vec_at[e2_xshotsconversion_vec_at != ""]
#   e2_xshotsconversion_vec_at  <-tail(e2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   e2_fouls_conversion_vec_ht <- as.vector(e2_fouls_conversion[e2_hometeamindex,])
#   e2_fouls_conversion_vec_ht[is.na(e2_fouls_conversion_vec_ht)] <- ""
#   e2_fouls_conversion_vec_ht <- e2_fouls_conversion_vec_ht[e2_fouls_conversion_vec_ht != ""]
#   e2_fouls_conversion_vec_ht  <-tail(e2_fouls_conversion_vec_ht,1)
#   #awayteam
#   e2_fouls_conversion_vec_at <- as.vector(e2_fouls_conversion[e2_awayteamindex,])
#   e2_fouls_conversion_vec_at[is.na(e2_fouls_conversion_vec_at)] <- ""
#   e2_fouls_conversion_vec_at <- e2_fouls_conversion_vec_at[e2_fouls_conversion_vec_at != ""]
#   e2_fouls_conversion_vec_at  <-tail(e2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   e2_ht_last6points <- e2_ht_numberof_wins*3 + e2_ht_numberof_draws*1
#   e2_at_last6points <- e2_at_numberof_wins*3 + e2_at_numberof_draws*1
#
#   if(e2_ht_last6points > e2_at_last6points) {e2_3waypick <- "1"}  else {e2_3waypick <- "X2"}
#
#   if(e2_at_last6points > e2_ht_last6points ) {e2_3waypick <- "2"} else {e2_3waypick <- "1X"}
#
#   if(e2_ht_no_of_ov25 + e2_at_no_of_ov25 >= 6) {e2_goalspick <- "ov25"} else {e2_goalspick <- "un25"}
#
#   if(e2_ht_no_of_un25 + e2_at_no_of_un25 >= 6) {e2_goalspick <- "un25"} else {e2_goalspick <- "ov25"}
#
#   if(e2_ht_matches_scoring >= 4 && e2_at_matches_scoring >=4) {e2_btts <- "BTTS-Y"} else {e2_btts <- "BTTS-N"}
#
#
#   e2_prediction[e2_row] <- rbind(paste(e2_3waypick,e2_goalspick,e2_btts,sep = ","))
#   e2_HWM[e2_row] <- e2_ht_totalwinmargin
#   e2_AWM[e2_row] <- e2_at_totalwinmargin
#
#   e2_HWMLM[e2_row] <- e2_winmargin_vec_ht_lm
#   e2_AWMLM[e2_row] <- e2_winmargin_vec_at_lm
#
#   e2_HY[e2_row] <- e2_yellowtotals_vec_ht
#   e2_AY[e2_row] <- e2_yellowtotals_vec_at
#
#   e2_HCO[e2_row] <- e2_cornertotals_vec_ht
#   e2_ACO[e2_row] <- e2_cornertotals_vec_at
#
#   e2_HXSC[e2_row] <- e2_xshotsconversion_vec_ht
#   e2_AXSC[e2_row] <- e2_xshotsconversion_vec_at
#
#   e2_HYCPF[e2_row] <- e2_fouls_conversion_vec_ht
#   e2_AYCPF[e2_row] <- e2_fouls_conversion_vec_at
# }
#
# e2_prediction <- as.data.frame(e2_prediction)
# colnames(e2_prediction) <- "prediction"
#
# e2_HWM <- as.data.frame(e2_HWM)
# colnames(e2_HWM) <- "HWM"
#
# e2_AWM <- as.data.frame(e2_AWM)
# colnames(e2_AWM) <- "AWM"
#
# e2_HWMLM <- as.data.frame(e2_HWMLM)
# colnames(e2_HWMLM) <- "HWMLM"
#
# e2_AWMLM <- as.data.frame(e2_AWMLM)
# colnames(e2_AWMLM) <- "AWMLM"
#
# e2_HY <- as.data.frame(e2_HY)
# colnames(e2_HY) <- "AVGHY"
#
# e2_AY <- as.data.frame(e2_AY)
# colnames(e2_AY) <- "AVGAY"
#
# e2_HCO <- as.data.frame(e2_HCO)
# colnames(e2_HCO) <- "AVGHCO"
#
# e2_ACO <- as.data.frame(e2_ACO)
# colnames(e2_ACO) <- "AVGACO"
#
# e2_HXSC <- as.data.frame(e2_HXSC)
# colnames(e2_HXSC) <- "HXSC"
#
# e2_AXSC <- as.data.frame(e2_AXSC)
# colnames(e2_AXSC) <- "AXSC"
#
# e2_HYCPF <- as.data.frame(e2_HYCPF)
# colnames(e2_HYCPF) <- "HYCPF"
#
# e2_AYCPF <- as.data.frame(e2_AYCPF)
# colnames(e2_AYCPF) <- "AYCPF"
#
# e2_picks <- cbind(E2_fixtures$Div,E2_fixtures$HomeTeam_e2,E2_fixtures$AwayTeam_e2,e2_prediction,e2_HWM,e2_AWM,e2_HWMLM,e2_AWMLM,e2_HY,e2_AY,e2_HCO,e2_ACO,e2_HXSC,e2_AXSC,e2_HYCPF,e2_AYCPF)
#
# colnames(e2_picks)[1] <- "picks_Div"
# colnames(e2_picks)[2] <- "picks_HomeTeam"
# colnames(e2_picks)[3] <- "picks_AwayTeam"
# e2_picks$matchid <- paste(e2_picks$picks_HomeTeam,e2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of E2
# e2_picks
# #E3
# E3_fixtures$Hometeam_e3_index <- match(E3_fixtures$HomeTeam_e3,e3_teams)
# E3_fixtures$Awayteam_e3_index <- match(E3_fixtures$AwayTeam_e3,e3_teams)
# e3_prediction <- c()
# e3_HWM <- c()
# e3_AWM <- c()
# e3_HWMLM <- c()
# e3_AWMLM <- c()
# e3_HY <- c()
# e3_AY <- c()
# e3_HCO <- c()
# e3_ACO <- c()
# e3_HXSC <- c()
# e3_AXSC <- c()
# e3_HYCPF <- c()
# e3_AYCPF <- c()
# for(e3_row in 1:nrow(E3_fixtures))
# {
#
#   e3_hometeamindex <- E3_fixtures[e3_row,"Hometeam_e3_index"]
#   e3_awayteamindex <- E3_fixtures[e3_row,"Awayteam_e3_index"]
#   #analyse team form
#   #home team
#   e3_form_vec_ht <- as.vector(e3_form_h[e3_hometeamindex,])
#   e3_form_vec_ht[is.na(e3_form_vec_ht)] <- ""
#   e3_form_vec_ht <- e3_form_vec_ht[e3_form_vec_ht != ""]
#   e3_form_vec_ht  <-tail(e3_form_vec_ht,6)
#   e3_ht_numberof_wins <- length(which(e3_form_vec_ht == "W"))
#   e3_ht_numberof_draws <- length(which(e3_form_vec_ht == "D"))
#   e3_ht_numberof_loss <- length(which(e3_form_vec_ht == "L"))
#   #awayteam
#   e3_form_vec_at <- as.vector(e3_form_h[e3_awayteamindex,])
#   e3_form_vec_at[is.na(e3_form_vec_at)] <- ""
#   e3_form_vec_at <- e3_form_vec_at[e3_form_vec_at != ""]
#   e3_form_vec_at  <-tail(e3_form_vec_at,6)
#   e3_at_numberof_wins <- length(which(e3_form_vec_at == "W"))
#   e3_at_numberof_draws <- length(which(e3_form_vec_at == "D"))
#   e3_at_numberof_loss <- length(which(e3_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   e3_goalscored_vec_ht <- as.vector(e3_goalscored_h[e3_hometeamindex,])
#   e3_goalscored_vec_ht[is.na(e3_goalscored_vec_ht)] <- ""
#   e3_goalscored_vec_ht <- e3_goalscored_vec_ht[e3_goalscored_vec_ht != ""]
#   e3_goalscored_vec_ht  <-tail(e3_goalscored_vec_ht,6)
#   e3_goalscored_vec_ht  <- as.numeric(e3_goalscored_vec_ht)
#   e3_ht_totalgoalscored <- sum(e3_goalscored_vec_ht)
#   e3_ht_matches_scoring <- length(which(e3_goalscored_vec_ht > 0))
#   e3_ht_matches_without_scoring <- length(which(e3_goalscored_vec_ht == "0"))
#   #awayteam
#   e3_goalscored_vec_at <- as.vector(e3_goalscored_h[e3_awayteamindex,])
#   e3_goalscored_vec_at[is.na(e3_goalscored_vec_at)] <- ""
#   e3_goalscored_vec_at <- e3_goalscored_vec_at[e3_goalscored_vec_at != ""]
#   e3_goalscored_vec_at  <-tail(e3_goalscored_vec_at,6)
#   e3_goalscored_vec_at  <- as.numeric(e3_goalscored_vec_at)
#   e3_at_totalgoalscored <- sum(e3_goalscored_vec_at)
#   e3_at_matches_scoring <- length(which(e3_goalscored_vec_at > 0))
#   e3_at_matches_without_scoring <- length(which(e3_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   e3_goalconceded_vec_ht <- as.vector(e3_goalconceded_h[e3_hometeamindex,])
#   e3_goalconceded_vec_ht[is.na(e3_goalconceded_vec_ht)] <- ""
#   e3_goalconceded_vec_ht <- e3_goalconceded_vec_ht[e3_goalconceded_vec_ht != ""]
#   e3_goalconceded_vec_ht  <-tail(e3_goalconceded_vec_ht,6)
#   e3_goalconceded_vec_ht  <- as.numeric(e3_goalconceded_vec_ht)
#   e3_goalconceded_vec_ht
#   e3_ht_totalgoalconceded <- sum(e3_goalconceded_vec_ht)
#   e3_ht_matches_concede <- length(which(e3_goalconceded_vec_ht > 0))
#   e3_ht_matches_without_concede <- length(which(e3_goalconceded_vec_ht == "0"))
#   #awayteam
#   e3_goalconceded_vec_at <- as.vector(e3_goalconceded_h[e3_awayteamindex,])
#   e3_goalconceded_vec_at[is.na(e3_goalconceded_vec_at)] <- ""
#   e3_goalconceded_vec_at <- e3_goalconceded_vec_at[e3_goalconceded_vec_at != ""]
#   e3_goalconceded_vec_at  <-tail(e3_goalconceded_vec_at,6)
#   e3_goalconceded_vec_at  <- as.numeric(e3_goalconceded_vec_at)
#   e3_at_totalgoalconceded <- sum(e3_goalconceded_vec_at)
#   e3_at_matches_concede <- length(which(e3_goalconceded_vec_at > 0))
#   e3_at_matches_without_concede <- length(which(e3_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   e3_totalgoals_vec_ht <- as.vector(e3_totalgoals_h[e3_hometeamindex,])
#   e3_totalgoals_vec_ht[is.na(e3_totalgoals_vec_ht)] <- ""
#   e3_totalgoals_vec_ht <- e3_totalgoals_vec_ht[e3_totalgoals_vec_ht != ""]
#   e3_totalgoals_vec_ht  <-tail(e3_totalgoals_vec_ht,6)
#   e3_totalgoals_vec_ht  <- as.numeric(e3_totalgoals_vec_ht)
#   e3_totalgoals_vec_ht
#   e3_ht_totalgoals <- sum(e3_totalgoals_vec_ht)
#   e3_ht_avgtotalgoals <- (e3_ht_totalgoals/6)
#   e3_ht_no_of_ov25 <- length(which(e3_totalgoals_vec_ht >= 3))
#   e3_ht_no_of_un25 <- length(which(e3_totalgoals_vec_ht <= 2))
#   #awayteam
#   e3_totalgoals_vec_at <- as.vector(e3_totalgoals_h[e3_awayteamindex,])
#   e3_totalgoals_vec_at[is.na(e3_totalgoals_vec_at)] <- ""
#   e3_totalgoals_vec_at <- e3_totalgoals_vec_at[e3_totalgoals_vec_at != ""]
#   e3_totalgoals_vec_at  <-tail(e3_totalgoals_vec_at,6)
#   e3_totalgoals_vec_at  <- as.numeric(e3_totalgoals_vec_at)
#   e3_totalgoals_vec_at
#   e3_at_totalgoals <- sum(e3_totalgoals_vec_at)
#   e3_at_avgtotalgoals <- (e3_at_totalgoals/6)
#   e3_at_no_of_ov25 <- length(which(e3_totalgoals_vec_at >= 3))
#   e3_at_no_of_un25 <- length(which(e3_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   e3_winmargin_vec_ht <- as.vector(e3_winmargin_h[e3_hometeamindex,])
#   e3_winmargin_vec_ht[is.na(e3_winmargin_vec_ht)] <- ""
#   e3_winmargin_vec_ht <- e3_winmargin_vec_ht[e3_winmargin_vec_ht != ""]
#   e3_winmargin_vec_ht  <-tail(e3_winmargin_vec_ht,6)
#   e3_winmargin_vec_ht  <- as.numeric(e3_winmargin_vec_ht)
#
#   e3_ht_totalwinmargin <- sum(e3_winmargin_vec_ht)
#   e3_ht_no_of_winmargin_ov0 <- length(which(e3_winmargin_vec_ht >= 0))
#   e3_ht_no_of_winmargin_ov1 <- length(which(e3_winmargin_vec_ht >= 1))
#   e3_ht_no_of_winmargin_un0 <- length(which(e3_winmargin_vec_ht <= 0))
#   e3_ht_no_of_winmargin_un1 <- length(which(e3_winmargin_vec_ht <= 1))
#   #awayteam
#   e3_winmargin_vec_at <- as.vector(e3_winmargin_h[e3_awayteamindex,])
#   e3_winmargin_vec_at[is.na(e3_winmargin_vec_at)] <- ""
#   e3_winmargin_vec_at <- e3_winmargin_vec_at[e3_winmargin_vec_at != ""]
#   e3_winmargin_vec_at  <-tail(e3_winmargin_vec_at,6)
#   e3_winmargin_vec_at  <- as.numeric(e3_winmargin_vec_at)
#
#   e3_at_totalwinmargin <- sum(e3_winmargin_vec_at)
#   e3_at_no_of_winmargin_ov0 <- length(which(e3_winmargin_vec_at >= 0))
#   e3_at_no_of_winmargin_ov1 <- length(which(e3_winmargin_vec_at >= 1))
#   e3_at_no_of_winmargin_un0 <- length(which(e3_winmargin_vec_at <= 0))
#   e3_at_no_of_winmargin_un1 <- length(which(e3_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   e3_winmargin_vec_ht_lm <- as.vector(e3_winmargin_h[e3_hometeamindex,])
#   e3_winmargin_vec_ht_lm[is.na(e3_winmargin_vec_ht_lm)] <- ""
#   e3_winmargin_vec_ht_lm <- e3_winmargin_vec_ht_lm[e3_winmargin_vec_ht_lm != ""]
#   e3_winmargin_vec_ht_lm  <-tail(e3_winmargin_vec_ht_lm,1)
#   #awayteam
#   e3_winmargin_vec_at_lm <- as.vector(e3_winmargin_h[e3_awayteamindex,])
#   e3_winmargin_vec_at_lm[is.na(e3_winmargin_vec_at_lm)] <- ""
#   e3_winmargin_vec_at_lm <- e3_winmargin_vec_at_lm[e3_winmargin_vec_at_lm != ""]
#   e3_winmargin_vec_at_lm  <-tail(e3_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   e3_yellowtotals_vec_ht <- as.vector(e3_yellowtotalsv2[e3_hometeamindex,])
#   e3_yellowtotals_vec_ht[is.na(e3_yellowtotals_vec_ht)] <- ""
#   e3_yellowtotals_vec_ht <- e3_yellowtotals_vec_ht[e3_yellowtotals_vec_ht != ""]
#   e3_yellowtotals_vec_ht  <-tail(e3_yellowtotals_vec_ht,1)
#   #awayteam
#   e3_yellowtotals_vec_at <- as.vector(e3_yellowtotalsv2[e3_awayteamindex,])
#   e3_yellowtotals_vec_at[is.na(e3_yellowtotals_vec_at)] <- ""
#   e3_yellowtotals_vec_at <- e3_yellowtotals_vec_at[e3_yellowtotals_vec_at != ""]
#   e3_yellowtotals_vec_at  <-tail(e3_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   e3_cornertotals_vec_ht <- as.vector(e3_cornertotalsv2[e3_hometeamindex,])
#   e3_cornertotals_vec_ht[is.na(e3_cornertotals_vec_ht)] <- ""
#   e3_cornertotals_vec_ht <- e3_cornertotals_vec_ht[e3_cornertotals_vec_ht != ""]
#   e3_cornertotals_vec_ht  <-tail(e3_cornertotals_vec_ht,1)
#   #awayteam
#   e3_cornertotals_vec_at <- as.vector(e3_cornertotalsv2[e3_awayteamindex,])
#   e3_cornertotals_vec_at[is.na(e3_cornertotals_vec_at)] <- ""
#   e3_cornertotals_vec_at <- e3_cornertotals_vec_at[e3_cornertotals_vec_at != ""]
#   e3_cornertotals_vec_at  <-tail(e3_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   e3_xshotsconversion_vec_ht <- as.vector(e3_shots_analysis[e3_hometeamindex,])
#   e3_xshotsconversion_vec_ht[is.na(e3_xshotsconversion_vec_ht)] <- ""
#   e3_xshotsconversion_vec_ht <- e3_xshotsconversion_vec_ht[e3_xshotsconversion_vec_ht != ""]
#   e3_xshotsconversion_vec_ht  <-tail(e3_xshotsconversion_vec_ht,1)
#   #awayteam
#   e3_xshotsconversion_vec_at <- as.vector(e3_shots_analysis[e3_awayteamindex,])
#   e3_xshotsconversion_vec_at[is.na(e3_xshotsconversion_vec_at)] <- ""
#   e3_xshotsconversion_vec_at <- e3_xshotsconversion_vec_at[e3_xshotsconversion_vec_at != ""]
#   e3_xshotsconversion_vec_at  <-tail(e3_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   e3_fouls_conversion_vec_ht <- as.vector(e3_fouls_conversion[e3_hometeamindex,])
#   e3_fouls_conversion_vec_ht[is.na(e3_fouls_conversion_vec_ht)] <- ""
#   e3_fouls_conversion_vec_ht <- e3_fouls_conversion_vec_ht[e3_fouls_conversion_vec_ht != ""]
#   e3_fouls_conversion_vec_ht  <-tail(e3_fouls_conversion_vec_ht,1)
#   #awayteam
#   e3_fouls_conversion_vec_at <- as.vector(e3_fouls_conversion[e3_awayteamindex,])
#   e3_fouls_conversion_vec_at[is.na(e3_fouls_conversion_vec_at)] <- ""
#   e3_fouls_conversion_vec_at <- e3_fouls_conversion_vec_at[e3_fouls_conversion_vec_at != ""]
#   e3_fouls_conversion_vec_at  <-tail(e3_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   e3_ht_last6points <- e3_ht_numberof_wins*3 + e3_ht_numberof_draws*1
#   e3_at_last6points <- e3_at_numberof_wins*3 + e3_at_numberof_draws*1
#
#   if(e3_ht_last6points > e3_at_last6points) {e3_3waypick <- "1"}  else {e3_3waypick <- "X2"}
#
#   if(e3_at_last6points > e3_ht_last6points ) {e3_3waypick <- "2"} else {e3_3waypick <- "1X"}
#
#   if(e3_ht_no_of_ov25 + e3_at_no_of_ov25 >= 6) {e3_goalspick <- "ov25"} else {e3_goalspick <- "un25"}
#
#   if(e3_ht_no_of_un25 + e3_at_no_of_un25 >= 6) {e3_goalspick <- "un25"} else {e3_goalspick <- "ov25"}
#
#   if(e3_ht_matches_scoring >= 4 && e3_at_matches_scoring >=4) {e3_btts <- "BTTS-Y"} else {e3_btts <- "BTTS-N"}
#
#
#   e3_prediction[e3_row] <- rbind(paste(e3_3waypick,e3_goalspick,e3_btts,sep = ","))
#   e3_HWM[e3_row] <- e3_ht_totalwinmargin
#   e3_AWM[e3_row] <- e3_at_totalwinmargin
#
#   e3_HWMLM[e3_row] <- e3_winmargin_vec_ht_lm
#   e3_AWMLM[e3_row] <- e3_winmargin_vec_at_lm
#
#   e3_HY[e3_row] <- e3_yellowtotals_vec_ht
#   e3_AY[e3_row] <- e3_yellowtotals_vec_at
#
#   e3_HCO[e3_row] <- e3_cornertotals_vec_ht
#   e3_ACO[e3_row] <- e3_cornertotals_vec_at
#
#   e3_HXSC[e3_row] <- e3_xshotsconversion_vec_ht
#   e3_AXSC[e3_row] <- e3_xshotsconversion_vec_at
#
#   e3_HYCPF[e3_row] <- e3_fouls_conversion_vec_ht
#   e3_AYCPF[e3_row] <- e3_fouls_conversion_vec_at
# }
#
# e3_prediction <- as.data.frame(e3_prediction)
# colnames(e3_prediction) <- "prediction"
#
# e3_HWM <- as.data.frame(e3_HWM)
# colnames(e3_HWM) <- "HWM"
#
# e3_AWM <- as.data.frame(e3_AWM)
# colnames(e3_AWM) <- "AWM"
#
# e3_HWMLM <- as.data.frame(e3_HWMLM)
# colnames(e3_HWMLM) <- "HWMLM"
#
# e3_AWMLM <- as.data.frame(e3_AWMLM)
# colnames(e3_AWMLM) <- "AWMLM"
#
# e3_HY <- as.data.frame(e3_HY)
# colnames(e3_HY) <- "AVGHY"
#
# e3_AY <- as.data.frame(e3_AY)
# colnames(e3_AY) <- "AVGAY"
#
# e3_HCO <- as.data.frame(e3_HCO)
# colnames(e3_HCO) <- "AVGHCO"
#
# e3_ACO <- as.data.frame(e3_ACO)
# colnames(e3_ACO) <- "AVGACO"
#
# e3_HXSC <- as.data.frame(e3_HXSC)
# colnames(e3_HXSC) <- "HXSC"
#
# e3_AXSC <- as.data.frame(e3_AXSC)
# colnames(e3_AXSC) <- "AXSC"
#
# e3_HYCPF <- as.data.frame(e3_HYCPF)
# colnames(e3_HYCPF) <- "HYCPF"
#
# e3_AYCPF <- as.data.frame(e3_AYCPF)
# colnames(e3_AYCPF) <- "AYCPF"
#
# e3_picks <- cbind(E3_fixtures$Div,E3_fixtures$HomeTeam_e3,E3_fixtures$AwayTeam_e3,e3_prediction,e3_HWM,e3_AWM,e3_HWMLM,e3_AWMLM,e3_HY,e3_AY,e3_HCO,e3_ACO,e3_HXSC,e3_AXSC,e3_HYCPF,e3_AYCPF)
#
# colnames(e3_picks)[1] <- "picks_Div"
# colnames(e3_picks)[2] <- "picks_HomeTeam"
# colnames(e3_picks)[3] <- "picks_AwayTeam"
# e3_picks$matchid <- paste(e3_picks$picks_HomeTeam,e3_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of E3
# e3_picks
# #EC
# EC_fixtures$Hometeam_ec_index <- match(EC_fixtures$HomeTeam_ec,ec_teams)
# EC_fixtures$Awayteam_ec_index <- match(EC_fixtures$AwayTeam_ec,ec_teams)
# ec_prediction <- c()
# ec_HWM <- c()
# ec_AWM <- c()
# ec_HWMLM <- c()
# ec_AWMLM <- c()
# ec_HY <- c()
# ec_AY <- c()
# ec_HCO <- c()
# ec_ACO <- c()
# ec_HXSC <- c()
# ec_AXSC <- c()
# ec_HYCPF <- c()
# ec_AYCPF <- c()
# for(ec_row in 1:nrow(EC_fixtures))
# {
#
#   ec_hometeamindex <- EC_fixtures[ec_row,"Hometeam_ec_index"]
#   ec_awayteamindex <- EC_fixtures[ec_row,"Awayteam_ec_index"]
#   #analyse team form
#   #home team
#   ec_form_vec_ht <- as.vector(ec_form_h[ec_hometeamindex,])
#   ec_form_vec_ht[is.na(ec_form_vec_ht)] <- ""
#   ec_form_vec_ht <- ec_form_vec_ht[ec_form_vec_ht != ""]
#   ec_form_vec_ht  <-tail(ec_form_vec_ht,6)
#   ec_ht_numberof_wins <- length(which(ec_form_vec_ht == "W"))
#   ec_ht_numberof_draws <- length(which(ec_form_vec_ht == "D"))
#   ec_ht_numberof_loss <- length(which(ec_form_vec_ht == "L"))
#   #awayteam
#   ec_form_vec_at <- as.vector(ec_form_h[ec_awayteamindex,])
#   ec_form_vec_at[is.na(ec_form_vec_at)] <- ""
#   ec_form_vec_at <- ec_form_vec_at[ec_form_vec_at != ""]
#   ec_form_vec_at  <-tail(ec_form_vec_at,6)
#   ec_at_numberof_wins <- length(which(ec_form_vec_at == "W"))
#   ec_at_numberof_draws <- length(which(ec_form_vec_at == "D"))
#   ec_at_numberof_loss <- length(which(ec_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   ec_goalscored_vec_ht <- as.vector(ec_goalscored_h[ec_hometeamindex,])
#   ec_goalscored_vec_ht[is.na(ec_goalscored_vec_ht)] <- ""
#   ec_goalscored_vec_ht <- ec_goalscored_vec_ht[ec_goalscored_vec_ht != ""]
#   ec_goalscored_vec_ht  <-tail(ec_goalscored_vec_ht,6)
#   ec_goalscored_vec_ht  <- as.numeric(ec_goalscored_vec_ht)
#   ec_ht_totalgoalscored <- sum(ec_goalscored_vec_ht)
#   ec_ht_matches_scoring <- length(which(ec_goalscored_vec_ht > 0))
#   ec_ht_matches_without_scoring <- length(which(ec_goalscored_vec_ht == "0"))
#   #awayteam
#   ec_goalscored_vec_at <- as.vector(ec_goalscored_h[ec_awayteamindex,])
#   ec_goalscored_vec_at[is.na(ec_goalscored_vec_at)] <- ""
#   ec_goalscored_vec_at <- ec_goalscored_vec_at[ec_goalscored_vec_at != ""]
#   ec_goalscored_vec_at  <-tail(ec_goalscored_vec_at,6)
#   ec_goalscored_vec_at  <- as.numeric(ec_goalscored_vec_at)
#   ec_at_totalgoalscored <- sum(ec_goalscored_vec_at)
#   ec_at_matches_scoring <- length(which(ec_goalscored_vec_at > 0))
#   ec_at_matches_without_scoring <- length(which(ec_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   ec_goalconceded_vec_ht <- as.vector(ec_goalconceded_h[ec_hometeamindex,])
#   ec_goalconceded_vec_ht[is.na(ec_goalconceded_vec_ht)] <- ""
#   ec_goalconceded_vec_ht <- ec_goalconceded_vec_ht[ec_goalconceded_vec_ht != ""]
#   ec_goalconceded_vec_ht  <-tail(ec_goalconceded_vec_ht,6)
#   ec_goalconceded_vec_ht  <- as.numeric(ec_goalconceded_vec_ht)
#   ec_goalconceded_vec_ht
#   ec_ht_totalgoalconceded <- sum(ec_goalconceded_vec_ht)
#   ec_ht_matches_concede <- length(which(ec_goalconceded_vec_ht > 0))
#   ec_ht_matches_without_concede <- length(which(ec_goalconceded_vec_ht == "0"))
#   #awayteam
#   ec_goalconceded_vec_at <- as.vector(ec_goalconceded_h[ec_awayteamindex,])
#   ec_goalconceded_vec_at[is.na(ec_goalconceded_vec_at)] <- ""
#   ec_goalconceded_vec_at <- ec_goalconceded_vec_at[ec_goalconceded_vec_at != ""]
#   ec_goalconceded_vec_at  <-tail(ec_goalconceded_vec_at,6)
#   ec_goalconceded_vec_at  <- as.numeric(ec_goalconceded_vec_at)
#   ec_at_totalgoalconceded <- sum(ec_goalconceded_vec_at)
#   ec_at_matches_concede <- length(which(ec_goalconceded_vec_at > 0))
#   ec_at_matches_without_concede <- length(which(ec_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   ec_totalgoals_vec_ht <- as.vector(ec_totalgoals_h[ec_hometeamindex,])
#   ec_totalgoals_vec_ht[is.na(ec_totalgoals_vec_ht)] <- ""
#   ec_totalgoals_vec_ht <- ec_totalgoals_vec_ht[ec_totalgoals_vec_ht != ""]
#   ec_totalgoals_vec_ht  <-tail(ec_totalgoals_vec_ht,6)
#   ec_totalgoals_vec_ht  <- as.numeric(ec_totalgoals_vec_ht)
#   ec_totalgoals_vec_ht
#   ec_ht_totalgoals <- sum(ec_totalgoals_vec_ht)
#   ec_ht_avgtotalgoals <- (ec_ht_totalgoals/6)
#   ec_ht_no_of_ov25 <- length(which(ec_totalgoals_vec_ht >= 3))
#   ec_ht_no_of_un25 <- length(which(ec_totalgoals_vec_ht <= 2))
#   #awayteam
#   ec_totalgoals_vec_at <- as.vector(ec_totalgoals_h[ec_awayteamindex,])
#   ec_totalgoals_vec_at[is.na(ec_totalgoals_vec_at)] <- ""
#   ec_totalgoals_vec_at <- ec_totalgoals_vec_at[ec_totalgoals_vec_at != ""]
#   ec_totalgoals_vec_at  <-tail(ec_totalgoals_vec_at,6)
#   ec_totalgoals_vec_at  <- as.numeric(ec_totalgoals_vec_at)
#   ec_totalgoals_vec_at
#   ec_at_totalgoals <- sum(ec_totalgoals_vec_at)
#   ec_at_avgtotalgoals <- (ec_at_totalgoals/6)
#   ec_at_no_of_ov25 <- length(which(ec_totalgoals_vec_at >= 3))
#   ec_at_no_of_un25 <- length(which(ec_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   ec_winmargin_vec_ht <- as.vector(ec_winmargin_h[ec_hometeamindex,])
#   ec_winmargin_vec_ht[is.na(ec_winmargin_vec_ht)] <- ""
#   ec_winmargin_vec_ht <- ec_winmargin_vec_ht[ec_winmargin_vec_ht != ""]
#   ec_winmargin_vec_ht  <-tail(ec_winmargin_vec_ht,6)
#   ec_winmargin_vec_ht  <- as.numeric(ec_winmargin_vec_ht)
#
#   ec_ht_totalwinmargin <- sum(ec_winmargin_vec_ht)
#   ec_ht_no_of_winmargin_ov0 <- length(which(ec_winmargin_vec_ht >= 0))
#   ec_ht_no_of_winmargin_ov1 <- length(which(ec_winmargin_vec_ht >= 1))
#   ec_ht_no_of_winmargin_un0 <- length(which(ec_winmargin_vec_ht <= 0))
#   ec_ht_no_of_winmargin_un1 <- length(which(ec_winmargin_vec_ht <= 1))
#   #awayteam
#   ec_winmargin_vec_at <- as.vector(ec_winmargin_h[ec_awayteamindex,])
#   ec_winmargin_vec_at[is.na(ec_winmargin_vec_at)] <- ""
#   ec_winmargin_vec_at <- ec_winmargin_vec_at[ec_winmargin_vec_at != ""]
#   ec_winmargin_vec_at  <-tail(ec_winmargin_vec_at,6)
#   ec_winmargin_vec_at  <- as.numeric(ec_winmargin_vec_at)
#
#   ec_at_totalwinmargin <- sum(ec_winmargin_vec_at)
#   ec_at_no_of_winmargin_ov0 <- length(which(ec_winmargin_vec_at >= 0))
#   ec_at_no_of_winmargin_ov1 <- length(which(ec_winmargin_vec_at >= 1))
#   ec_at_no_of_winmargin_un0 <- length(which(ec_winmargin_vec_at <= 0))
#   ec_at_no_of_winmargin_un1 <- length(which(ec_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   ec_winmargin_vec_ht_lm <- as.vector(ec_winmargin_h[ec_hometeamindex,])
#   ec_winmargin_vec_ht_lm[is.na(ec_winmargin_vec_ht_lm)] <- ""
#   ec_winmargin_vec_ht_lm <- ec_winmargin_vec_ht_lm[ec_winmargin_vec_ht_lm != ""]
#   ec_winmargin_vec_ht_lm  <-tail(ec_winmargin_vec_ht_lm,1)
#   #awayteam
#   ec_winmargin_vec_at_lm <- as.vector(ec_winmargin_h[ec_awayteamindex,])
#   ec_winmargin_vec_at_lm[is.na(ec_winmargin_vec_at_lm)] <- ""
#   ec_winmargin_vec_at_lm <- ec_winmargin_vec_at_lm[ec_winmargin_vec_at_lm != ""]
#   ec_winmargin_vec_at_lm  <-tail(ec_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   ec_yellowtotals_vec_ht <- as.vector(ec_yellowtotalsv2[ec_hometeamindex,])
#   ec_yellowtotals_vec_ht[is.na(ec_yellowtotals_vec_ht)] <- ""
#   ec_yellowtotals_vec_ht <- ec_yellowtotals_vec_ht[ec_yellowtotals_vec_ht != ""]
#   ec_yellowtotals_vec_ht  <-tail(ec_yellowtotals_vec_ht,1)
#   #awayteam
#   ec_yellowtotals_vec_at <- as.vector(ec_yellowtotalsv2[ec_awayteamindex,])
#   ec_yellowtotals_vec_at[is.na(ec_yellowtotals_vec_at)] <- ""
#   ec_yellowtotals_vec_at <- ec_yellowtotals_vec_at[ec_yellowtotals_vec_at != ""]
#   ec_yellowtotals_vec_at  <-tail(ec_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   ec_cornertotals_vec_ht <- as.vector(ec_cornertotalsv2[ec_hometeamindex,])
#   ec_cornertotals_vec_ht[is.na(ec_cornertotals_vec_ht)] <- ""
#   ec_cornertotals_vec_ht <- ec_cornertotals_vec_ht[ec_cornertotals_vec_ht != ""]
#   ec_cornertotals_vec_ht  <-tail(ec_cornertotals_vec_ht,1)
#   #awayteam
#   ec_cornertotals_vec_at <- as.vector(ec_cornertotalsv2[ec_awayteamindex,])
#   ec_cornertotals_vec_at[is.na(ec_cornertotals_vec_at)] <- ""
#   ec_cornertotals_vec_at <- ec_cornertotals_vec_at[ec_cornertotals_vec_at != ""]
#   ec_cornertotals_vec_at  <-tail(ec_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   ec_xshotsconversion_vec_ht <- as.vector(ec_shots_analysis[ec_hometeamindex,])
#   ec_xshotsconversion_vec_ht[is.na(ec_xshotsconversion_vec_ht)] <- ""
#   ec_xshotsconversion_vec_ht <- ec_xshotsconversion_vec_ht[ec_xshotsconversion_vec_ht != ""]
#   ec_xshotsconversion_vec_ht  <-tail(ec_xshotsconversion_vec_ht,1)
#   #awayteam
#   ec_xshotsconversion_vec_at <- as.vector(ec_shots_analysis[ec_awayteamindex,])
#   ec_xshotsconversion_vec_at[is.na(ec_xshotsconversion_vec_at)] <- ""
#   ec_xshotsconversion_vec_at <- ec_xshotsconversion_vec_at[ec_xshotsconversion_vec_at != ""]
#   ec_xshotsconversion_vec_at  <-tail(ec_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   ec_fouls_conversion_vec_ht <- as.vector(ec_fouls_conversion[ec_hometeamindex,])
#   ec_fouls_conversion_vec_ht[is.na(ec_fouls_conversion_vec_ht)] <- ""
#   ec_fouls_conversion_vec_ht <- ec_fouls_conversion_vec_ht[ec_fouls_conversion_vec_ht != ""]
#   ec_fouls_conversion_vec_ht  <-tail(ec_fouls_conversion_vec_ht,1)
#   #awayteam
#   ec_fouls_conversion_vec_at <- as.vector(ec_fouls_conversion[ec_awayteamindex,])
#   ec_fouls_conversion_vec_at[is.na(ec_fouls_conversion_vec_at)] <- ""
#   ec_fouls_conversion_vec_at <- ec_fouls_conversion_vec_at[ec_fouls_conversion_vec_at != ""]
#   ec_fouls_conversion_vec_at  <-tail(ec_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   ec_ht_last6points <- ec_ht_numberof_wins*3 + ec_ht_numberof_draws*1
#   ec_at_last6points <- ec_at_numberof_wins*3 + ec_at_numberof_draws*1
#
#   if(ec_ht_last6points > ec_at_last6points) {ec_3waypick <- "1"}  else {ec_3waypick <- "X2"}
#
#   if(ec_at_last6points > ec_ht_last6points ) {ec_3waypick <- "2"} else {ec_3waypick <- "1X"}
#
#   if(ec_ht_no_of_ov25 + ec_at_no_of_ov25 >= 6) {ec_goalspick <- "ov25"} else {ec_goalspick <- "un25"}
#
#   if(ec_ht_no_of_un25 + ec_at_no_of_un25 >= 6) {ec_goalspick <- "un25"} else {ec_goalspick <- "ov25"}
#
#   if(ec_ht_matches_scoring >= 4 && ec_at_matches_scoring >=4) {ec_btts <- "BTTS-Y"} else {ec_btts <- "BTTS-N"}
#
#
#   ec_prediction[ec_row] <- rbind(paste(ec_3waypick,ec_goalspick,ec_btts,sep = ","))
#   ec_HWM[ec_row] <- ec_ht_totalwinmargin
#   ec_AWM[ec_row] <- ec_at_totalwinmargin
#
#   ec_HWMLM[ec_row] <- ec_winmargin_vec_ht_lm
#   ec_AWMLM[ec_row] <- ec_winmargin_vec_at_lm
#
#   ec_HY[ec_row] <- ec_yellowtotals_vec_ht
#   ec_AY[ec_row] <- ec_yellowtotals_vec_at
#
#   ec_HCO[ec_row] <- ec_cornertotals_vec_ht
#   ec_ACO[ec_row] <- ec_cornertotals_vec_at
#
#   ec_HXSC[ec_row] <- ec_xshotsconversion_vec_ht
#   ec_AXSC[ec_row] <- ec_xshotsconversion_vec_at
#
#   ec_HYCPF[ec_row] <- ec_fouls_conversion_vec_ht
#   ec_AYCPF[ec_row] <- ec_fouls_conversion_vec_at
# }
#
# ec_prediction <- as.data.frame(ec_prediction)
# colnames(ec_prediction) <- "prediction"
#
# ec_HWM <- as.data.frame(ec_HWM)
# colnames(ec_HWM) <- "HWM"
#
# ec_AWM <- as.data.frame(ec_AWM)
# colnames(ec_AWM) <- "AWM"
#
# ec_HWMLM <- as.data.frame(ec_HWMLM)
# colnames(ec_HWMLM) <- "HWMLM"
#
# ec_AWMLM <- as.data.frame(ec_AWMLM)
# colnames(ec_AWMLM) <- "AWMLM"
#
# ec_HY <- as.data.frame(ec_HY)
# colnames(ec_HY) <- "AVGHY"
#
# ec_AY <- as.data.frame(ec_AY)
# colnames(ec_AY) <- "AVGAY"
#
# ec_HCO <- as.data.frame(ec_HCO)
# colnames(ec_HCO) <- "AVGHCO"
#
# ec_ACO <- as.data.frame(ec_ACO)
# colnames(ec_ACO) <- "AVGACO"
#
# ec_HXSC <- as.data.frame(ec_HXSC)
# colnames(ec_HXSC) <- "HXSC"
#
# ec_AXSC <- as.data.frame(ec_AXSC)
# colnames(ec_AXSC) <- "AXSC"
#
# ec_HYCPF <- as.data.frame(ec_HYCPF)
# colnames(ec_HYCPF) <- "HYCPF"
#
# ec_AYCPF <- as.data.frame(ec_AYCPF)
# colnames(ec_AYCPF) <- "AYCPF"
#
# ec_picks <- cbind(EC_fixtures$Div,EC_fixtures$HomeTeam_ec,EC_fixtures$AwayTeam_ec,ec_prediction,ec_HWM,ec_AWM,ec_HWMLM,ec_AWMLM,ec_HY,ec_AY,ec_HCO,ec_ACO,ec_HXSC,ec_AXSC,ec_HYCPF,ec_AYCPF)
#
# colnames(ec_picks)[1] <- "picks_Div"
# colnames(ec_picks)[2] <- "picks_HomeTeam"
# colnames(ec_picks)[3] <- "picks_AwayTeam"
# ec_picks$matchid <- paste(ec_picks$picks_HomeTeam,ec_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of EC
# ec_picks
# #F1
# F1_fixtures$Hometeam_f1_index <- match(F1_fixtures$HomeTeam_f1,f1_teams)
# F1_fixtures$Awayteam_f1_index <- match(F1_fixtures$AwayTeam_f1,f1_teams)
# f1_prediction <- c()
# f1_HWM <- c()
# f1_AWM <- c()
# f1_HWMLM <- c()
# f1_AWMLM <- c()
# f1_HY <- c()
# f1_AY <- c()
# f1_HCO <- c()
# f1_ACO <- c()
# f1_HXSC <- c()
# f1_AXSC <- c()
# f1_HYCPF <- c()
# f1_AYCPF <- c()
# for(f1_row in 1:nrow(F1_fixtures))
# {
#
#   f1_hometeamindex <- F1_fixtures[f1_row,"Hometeam_f1_index"]
#   f1_awayteamindex <- F1_fixtures[f1_row,"Awayteam_f1_index"]
#   #analyse team form
#   #home team
#   f1_form_vec_ht <- as.vector(f1_form_h[f1_hometeamindex,])
#   f1_form_vec_ht[is.na(f1_form_vec_ht)] <- ""
#   f1_form_vec_ht <- f1_form_vec_ht[f1_form_vec_ht != ""]
#   f1_form_vec_ht  <-tail(f1_form_vec_ht,6)
#   f1_ht_numberof_wins <- length(which(f1_form_vec_ht == "W"))
#   f1_ht_numberof_draws <- length(which(f1_form_vec_ht == "D"))
#   f1_ht_numberof_loss <- length(which(f1_form_vec_ht == "L"))
#   #awayteam
#   f1_form_vec_at <- as.vector(f1_form_h[f1_awayteamindex,])
#   f1_form_vec_at[is.na(f1_form_vec_at)] <- ""
#   f1_form_vec_at <- f1_form_vec_at[f1_form_vec_at != ""]
#   f1_form_vec_at  <-tail(f1_form_vec_at,6)
#   f1_at_numberof_wins <- length(which(f1_form_vec_at == "W"))
#   f1_at_numberof_draws <- length(which(f1_form_vec_at == "D"))
#   f1_at_numberof_loss <- length(which(f1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   f1_goalscored_vec_ht <- as.vector(f1_goalscored_h[f1_hometeamindex,])
#   f1_goalscored_vec_ht[is.na(f1_goalscored_vec_ht)] <- ""
#   f1_goalscored_vec_ht <- f1_goalscored_vec_ht[f1_goalscored_vec_ht != ""]
#   f1_goalscored_vec_ht  <-tail(f1_goalscored_vec_ht,6)
#   f1_goalscored_vec_ht  <- as.numeric(f1_goalscored_vec_ht)
#   f1_ht_totalgoalscored <- sum(f1_goalscored_vec_ht)
#   f1_ht_matches_scoring <- length(which(f1_goalscored_vec_ht > 0))
#   f1_ht_matches_without_scoring <- length(which(f1_goalscored_vec_ht == "0"))
#   #awayteam
#   f1_goalscored_vec_at <- as.vector(f1_goalscored_h[f1_awayteamindex,])
#   f1_goalscored_vec_at[is.na(f1_goalscored_vec_at)] <- ""
#   f1_goalscored_vec_at <- f1_goalscored_vec_at[f1_goalscored_vec_at != ""]
#   f1_goalscored_vec_at  <-tail(f1_goalscored_vec_at,6)
#   f1_goalscored_vec_at  <- as.numeric(f1_goalscored_vec_at)
#   f1_at_totalgoalscored <- sum(f1_goalscored_vec_at)
#   f1_at_matches_scoring <- length(which(f1_goalscored_vec_at > 0))
#   f1_at_matches_without_scoring <- length(which(f1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   f1_goalconceded_vec_ht <- as.vector(f1_goalconceded_h[f1_hometeamindex,])
#   f1_goalconceded_vec_ht[is.na(f1_goalconceded_vec_ht)] <- ""
#   f1_goalconceded_vec_ht <- f1_goalconceded_vec_ht[f1_goalconceded_vec_ht != ""]
#   f1_goalconceded_vec_ht  <-tail(f1_goalconceded_vec_ht,6)
#   f1_goalconceded_vec_ht  <- as.numeric(f1_goalconceded_vec_ht)
#   f1_goalconceded_vec_ht
#   f1_ht_totalgoalconceded <- sum(f1_goalconceded_vec_ht)
#   f1_ht_matches_concede <- length(which(f1_goalconceded_vec_ht > 0))
#   f1_ht_matches_without_concede <- length(which(f1_goalconceded_vec_ht == "0"))
#   #awayteam
#   f1_goalconceded_vec_at <- as.vector(f1_goalconceded_h[f1_awayteamindex,])
#   f1_goalconceded_vec_at[is.na(f1_goalconceded_vec_at)] <- ""
#   f1_goalconceded_vec_at <- f1_goalconceded_vec_at[f1_goalconceded_vec_at != ""]
#   f1_goalconceded_vec_at  <-tail(f1_goalconceded_vec_at,6)
#   f1_goalconceded_vec_at  <- as.numeric(f1_goalconceded_vec_at)
#   f1_at_totalgoalconceded <- sum(f1_goalconceded_vec_at)
#   f1_at_matches_concede <- length(which(f1_goalconceded_vec_at > 0))
#   f1_at_matches_without_concede <- length(which(f1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   f1_totalgoals_vec_ht <- as.vector(f1_totalgoals_h[f1_hometeamindex,])
#   f1_totalgoals_vec_ht[is.na(f1_totalgoals_vec_ht)] <- ""
#   f1_totalgoals_vec_ht <- f1_totalgoals_vec_ht[f1_totalgoals_vec_ht != ""]
#   f1_totalgoals_vec_ht  <-tail(f1_totalgoals_vec_ht,6)
#   f1_totalgoals_vec_ht  <- as.numeric(f1_totalgoals_vec_ht)
#   f1_totalgoals_vec_ht
#   f1_ht_totalgoals <- sum(f1_totalgoals_vec_ht)
#   f1_ht_avgtotalgoals <- (f1_ht_totalgoals/6)
#   f1_ht_no_of_ov25 <- length(which(f1_totalgoals_vec_ht >= 3))
#   f1_ht_no_of_un25 <- length(which(f1_totalgoals_vec_ht <= 2))
#   #awayteam
#   f1_totalgoals_vec_at <- as.vector(f1_totalgoals_h[f1_awayteamindex,])
#   f1_totalgoals_vec_at[is.na(f1_totalgoals_vec_at)] <- ""
#   f1_totalgoals_vec_at <- f1_totalgoals_vec_at[f1_totalgoals_vec_at != ""]
#   f1_totalgoals_vec_at  <-tail(f1_totalgoals_vec_at,6)
#   f1_totalgoals_vec_at  <- as.numeric(f1_totalgoals_vec_at)
#   f1_totalgoals_vec_at
#   f1_at_totalgoals <- sum(f1_totalgoals_vec_at)
#   f1_at_avgtotalgoals <- (f1_at_totalgoals/6)
#   f1_at_no_of_ov25 <- length(which(f1_totalgoals_vec_at >= 3))
#   f1_at_no_of_un25 <- length(which(f1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   f1_winmargin_vec_ht <- as.vector(f1_winmargin_h[f1_hometeamindex,])
#   f1_winmargin_vec_ht[is.na(f1_winmargin_vec_ht)] <- ""
#   f1_winmargin_vec_ht <- f1_winmargin_vec_ht[f1_winmargin_vec_ht != ""]
#   f1_winmargin_vec_ht  <-tail(f1_winmargin_vec_ht,6)
#   f1_winmargin_vec_ht  <- as.numeric(f1_winmargin_vec_ht)
#
#   f1_ht_totalwinmargin <- sum(f1_winmargin_vec_ht)
#   f1_ht_no_of_winmargin_ov0 <- length(which(f1_winmargin_vec_ht >= 0))
#   f1_ht_no_of_winmargin_ov1 <- length(which(f1_winmargin_vec_ht >= 1))
#   f1_ht_no_of_winmargin_un0 <- length(which(f1_winmargin_vec_ht <= 0))
#   f1_ht_no_of_winmargin_un1 <- length(which(f1_winmargin_vec_ht <= 1))
#   #awayteam
#   f1_winmargin_vec_at <- as.vector(f1_winmargin_h[f1_awayteamindex,])
#   f1_winmargin_vec_at[is.na(f1_winmargin_vec_at)] <- ""
#   f1_winmargin_vec_at <- f1_winmargin_vec_at[f1_winmargin_vec_at != ""]
#   f1_winmargin_vec_at  <-tail(f1_winmargin_vec_at,6)
#   f1_winmargin_vec_at  <- as.numeric(f1_winmargin_vec_at)
#
#   f1_at_totalwinmargin <- sum(f1_winmargin_vec_at)
#   f1_at_no_of_winmargin_ov0 <- length(which(f1_winmargin_vec_at >= 0))
#   f1_at_no_of_winmargin_ov1 <- length(which(f1_winmargin_vec_at >= 1))
#   f1_at_no_of_winmargin_un0 <- length(which(f1_winmargin_vec_at <= 0))
#   f1_at_no_of_winmargin_un1 <- length(which(f1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   f1_winmargin_vec_ht_lm <- as.vector(f1_winmargin_h[f1_hometeamindex,])
#   f1_winmargin_vec_ht_lm[is.na(f1_winmargin_vec_ht_lm)] <- ""
#   f1_winmargin_vec_ht_lm <- f1_winmargin_vec_ht_lm[f1_winmargin_vec_ht_lm != ""]
#   f1_winmargin_vec_ht_lm  <-tail(f1_winmargin_vec_ht_lm,1)
#   #awayteam
#   f1_winmargin_vec_at_lm <- as.vector(f1_winmargin_h[f1_awayteamindex,])
#   f1_winmargin_vec_at_lm[is.na(f1_winmargin_vec_at_lm)] <- ""
#   f1_winmargin_vec_at_lm <- f1_winmargin_vec_at_lm[f1_winmargin_vec_at_lm != ""]
#   f1_winmargin_vec_at_lm  <-tail(f1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   f1_yellowtotals_vec_ht <- as.vector(f1_yellowtotalsv2[f1_hometeamindex,])
#   f1_yellowtotals_vec_ht[is.na(f1_yellowtotals_vec_ht)] <- ""
#   f1_yellowtotals_vec_ht <- f1_yellowtotals_vec_ht[f1_yellowtotals_vec_ht != ""]
#   f1_yellowtotals_vec_ht  <-tail(f1_yellowtotals_vec_ht,1)
#   #awayteam
#   f1_yellowtotals_vec_at <- as.vector(f1_yellowtotalsv2[f1_awayteamindex,])
#   f1_yellowtotals_vec_at[is.na(f1_yellowtotals_vec_at)] <- ""
#   f1_yellowtotals_vec_at <- f1_yellowtotals_vec_at[f1_yellowtotals_vec_at != ""]
#   f1_yellowtotals_vec_at  <-tail(f1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   f1_cornertotals_vec_ht <- as.vector(f1_cornertotalsv2[f1_hometeamindex,])
#   f1_cornertotals_vec_ht[is.na(f1_cornertotals_vec_ht)] <- ""
#   f1_cornertotals_vec_ht <- f1_cornertotals_vec_ht[f1_cornertotals_vec_ht != ""]
#   f1_cornertotals_vec_ht  <-tail(f1_cornertotals_vec_ht,1)
#   #awayteam
#   f1_cornertotals_vec_at <- as.vector(f1_cornertotalsv2[f1_awayteamindex,])
#   f1_cornertotals_vec_at[is.na(f1_cornertotals_vec_at)] <- ""
#   f1_cornertotals_vec_at <- f1_cornertotals_vec_at[f1_cornertotals_vec_at != ""]
#   f1_cornertotals_vec_at  <-tail(f1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   f1_xshotsconversion_vec_ht <- as.vector(f1_shots_analysis[f1_hometeamindex,])
#   f1_xshotsconversion_vec_ht[is.na(f1_xshotsconversion_vec_ht)] <- ""
#   f1_xshotsconversion_vec_ht <- f1_xshotsconversion_vec_ht[f1_xshotsconversion_vec_ht != ""]
#   f1_xshotsconversion_vec_ht  <-tail(f1_xshotsconversion_vec_ht,1)
#   #awayteam
#   f1_xshotsconversion_vec_at <- as.vector(f1_shots_analysis[f1_awayteamindex,])
#   f1_xshotsconversion_vec_at[is.na(f1_xshotsconversion_vec_at)] <- ""
#   f1_xshotsconversion_vec_at <- f1_xshotsconversion_vec_at[f1_xshotsconversion_vec_at != ""]
#   f1_xshotsconversion_vec_at  <-tail(f1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   f1_fouls_conversion_vec_ht <- as.vector(f1_fouls_conversion[f1_hometeamindex,])
#   f1_fouls_conversion_vec_ht[is.na(f1_fouls_conversion_vec_ht)] <- ""
#   f1_fouls_conversion_vec_ht <- f1_fouls_conversion_vec_ht[f1_fouls_conversion_vec_ht != ""]
#   f1_fouls_conversion_vec_ht  <-tail(f1_fouls_conversion_vec_ht,1)
#   #awayteam
#   f1_fouls_conversion_vec_at <- as.vector(f1_fouls_conversion[f1_awayteamindex,])
#   f1_fouls_conversion_vec_at[is.na(f1_fouls_conversion_vec_at)] <- ""
#   f1_fouls_conversion_vec_at <- f1_fouls_conversion_vec_at[f1_fouls_conversion_vec_at != ""]
#   f1_fouls_conversion_vec_at  <-tail(f1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   f1_ht_last6points <- f1_ht_numberof_wins*3 + f1_ht_numberof_draws*1
#   f1_at_last6points <- f1_at_numberof_wins*3 + f1_at_numberof_draws*1
#
#   if(f1_ht_last6points > f1_at_last6points) {f1_3waypick <- "1"}  else {f1_3waypick <- "X2"}
#
#   if(f1_at_last6points > f1_ht_last6points ) {f1_3waypick <- "2"} else {f1_3waypick <- "1X"}
#
#   if(f1_ht_no_of_ov25 + f1_at_no_of_ov25 >= 6) {f1_goalspick <- "ov25"} else {f1_goalspick <- "un25"}
#
#   if(f1_ht_no_of_un25 + f1_at_no_of_un25 >= 6) {f1_goalspick <- "un25"} else {f1_goalspick <- "ov25"}
#
#   if(f1_ht_matches_scoring >= 4 && f1_at_matches_scoring >=4) {f1_btts <- "BTTS-Y"} else {f1_btts <- "BTTS-N"}
#
#
#   f1_prediction[f1_row] <- rbind(paste(f1_3waypick,f1_goalspick,f1_btts,sep = ","))
#   f1_HWM[f1_row] <- f1_ht_totalwinmargin
#   f1_AWM[f1_row] <- f1_at_totalwinmargin
#
#   f1_HWMLM[f1_row] <- f1_winmargin_vec_ht_lm
#   f1_AWMLM[f1_row] <- f1_winmargin_vec_at_lm
#
#   f1_HY[f1_row] <- f1_yellowtotals_vec_ht
#   f1_AY[f1_row] <- f1_yellowtotals_vec_at
#
#   f1_HCO[f1_row] <- f1_cornertotals_vec_ht
#   f1_ACO[f1_row] <- f1_cornertotals_vec_at
#
#   f1_HXSC[f1_row] <- f1_xshotsconversion_vec_ht
#   f1_AXSC[f1_row] <- f1_xshotsconversion_vec_at
#
#   f1_HYCPF[f1_row] <- f1_fouls_conversion_vec_ht
#   f1_AYCPF[f1_row] <- f1_fouls_conversion_vec_at
# }
#
# f1_prediction <- as.data.frame(f1_prediction)
# colnames(f1_prediction) <- "prediction"
#
# f1_HWM <- as.data.frame(f1_HWM)
# colnames(f1_HWM) <- "HWM"
#
# f1_AWM <- as.data.frame(f1_AWM)
# colnames(f1_AWM) <- "AWM"
#
# f1_HWMLM <- as.data.frame(f1_HWMLM)
# colnames(f1_HWMLM) <- "HWMLM"
#
# f1_AWMLM <- as.data.frame(f1_AWMLM)
# colnames(f1_AWMLM) <- "AWMLM"
#
# f1_HY <- as.data.frame(f1_HY)
# colnames(f1_HY) <- "AVGHY"
#
# f1_AY <- as.data.frame(f1_AY)
# colnames(f1_AY) <- "AVGAY"
#
# f1_HCO <- as.data.frame(f1_HCO)
# colnames(f1_HCO) <- "AVGHCO"
#
# f1_ACO <- as.data.frame(f1_ACO)
# colnames(f1_ACO) <- "AVGACO"
#
# f1_HXSC <- as.data.frame(f1_HXSC)
# colnames(f1_HXSC) <- "HXSC"
#
# f1_AXSC <- as.data.frame(f1_AXSC)
# colnames(f1_AXSC) <- "AXSC"
#
# f1_HYCPF <- as.data.frame(f1_HYCPF)
# colnames(f1_HYCPF) <- "HYCPF"
#
# f1_AYCPF <- as.data.frame(f1_AYCPF)
# colnames(f1_AYCPF) <- "AYCPF"
#
# f1_picks <- cbind(F1_fixtures$Div,F1_fixtures$HomeTeam_f1,F1_fixtures$AwayTeam_f1,f1_prediction,f1_HWM,f1_AWM,f1_HWMLM,f1_AWMLM,f1_HY,f1_AY,f1_HCO,f1_ACO,f1_HXSC,f1_AXSC,f1_HYCPF,f1_AYCPF)
#
# colnames(f1_picks)[1] <- "picks_Div"
# colnames(f1_picks)[2] <- "picks_HomeTeam"
# colnames(f1_picks)[3] <- "picks_AwayTeam"
# f1_picks$matchid <- paste(f1_picks$picks_HomeTeam,f1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of F1
# f1_picks
# #F2
# F2_fixtures$Hometeam_f2_index <- match(F2_fixtures$HomeTeam_f2,f2_teams)
# F2_fixtures$Awayteam_f2_index <- match(F2_fixtures$AwayTeam_f2,f2_teams)
# f2_prediction <- c()
# f2_HWM <- c()
# f2_AWM <- c()
# f2_HWMLM <- c()
# f2_AWMLM <- c()
# f2_HY <- c()
# f2_AY <- c()
# f2_HCO <- c()
# f2_ACO <- c()
# f2_HXSC <- c()
# f2_AXSC <- c()
# f2_HYCPF <- c()
# f2_AYCPF <- c()
# for(f2_row in 1:nrow(F2_fixtures))
# {
#
#   f2_hometeamindex <- F2_fixtures[f2_row,"Hometeam_f2_index"]
#   f2_awayteamindex <- F2_fixtures[f2_row,"Awayteam_f2_index"]
#   #analyse team form
#   #home team
#   f2_form_vec_ht <- as.vector(f2_form_h[f2_hometeamindex,])
#   f2_form_vec_ht[is.na(f2_form_vec_ht)] <- ""
#   f2_form_vec_ht <- f2_form_vec_ht[f2_form_vec_ht != ""]
#   f2_form_vec_ht  <-tail(f2_form_vec_ht,6)
#   f2_ht_numberof_wins <- length(which(f2_form_vec_ht == "W"))
#   f2_ht_numberof_draws <- length(which(f2_form_vec_ht == "D"))
#   f2_ht_numberof_loss <- length(which(f2_form_vec_ht == "L"))
#   #awayteam
#   f2_form_vec_at <- as.vector(f2_form_h[f2_awayteamindex,])
#   f2_form_vec_at[is.na(f2_form_vec_at)] <- ""
#   f2_form_vec_at <- f2_form_vec_at[f2_form_vec_at != ""]
#   f2_form_vec_at  <-tail(f2_form_vec_at,6)
#   f2_at_numberof_wins <- length(which(f2_form_vec_at == "W"))
#   f2_at_numberof_draws <- length(which(f2_form_vec_at == "D"))
#   f2_at_numberof_loss <- length(which(f2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   f2_goalscored_vec_ht <- as.vector(f2_goalscored_h[f2_hometeamindex,])
#   f2_goalscored_vec_ht[is.na(f2_goalscored_vec_ht)] <- ""
#   f2_goalscored_vec_ht <- f2_goalscored_vec_ht[f2_goalscored_vec_ht != ""]
#   f2_goalscored_vec_ht  <-tail(f2_goalscored_vec_ht,6)
#   f2_goalscored_vec_ht  <- as.numeric(f2_goalscored_vec_ht)
#   f2_ht_totalgoalscored <- sum(f2_goalscored_vec_ht)
#   f2_ht_matches_scoring <- length(which(f2_goalscored_vec_ht > 0))
#   f2_ht_matches_without_scoring <- length(which(f2_goalscored_vec_ht == "0"))
#   #awayteam
#   f2_goalscored_vec_at <- as.vector(f2_goalscored_h[f2_awayteamindex,])
#   f2_goalscored_vec_at[is.na(f2_goalscored_vec_at)] <- ""
#   f2_goalscored_vec_at <- f2_goalscored_vec_at[f2_goalscored_vec_at != ""]
#   f2_goalscored_vec_at  <-tail(f2_goalscored_vec_at,6)
#   f2_goalscored_vec_at  <- as.numeric(f2_goalscored_vec_at)
#   f2_at_totalgoalscored <- sum(f2_goalscored_vec_at)
#   f2_at_matches_scoring <- length(which(f2_goalscored_vec_at > 0))
#   f2_at_matches_without_scoring <- length(which(f2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   f2_goalconceded_vec_ht <- as.vector(f2_goalconceded_h[f2_hometeamindex,])
#   f2_goalconceded_vec_ht[is.na(f2_goalconceded_vec_ht)] <- ""
#   f2_goalconceded_vec_ht <- f2_goalconceded_vec_ht[f2_goalconceded_vec_ht != ""]
#   f2_goalconceded_vec_ht  <-tail(f2_goalconceded_vec_ht,6)
#   f2_goalconceded_vec_ht  <- as.numeric(f2_goalconceded_vec_ht)
#   f2_goalconceded_vec_ht
#   f2_ht_totalgoalconceded <- sum(f2_goalconceded_vec_ht)
#   f2_ht_matches_concede <- length(which(f2_goalconceded_vec_ht > 0))
#   f2_ht_matches_without_concede <- length(which(f2_goalconceded_vec_ht == "0"))
#   #awayteam
#   f2_goalconceded_vec_at <- as.vector(f2_goalconceded_h[f2_awayteamindex,])
#   f2_goalconceded_vec_at[is.na(f2_goalconceded_vec_at)] <- ""
#   f2_goalconceded_vec_at <- f2_goalconceded_vec_at[f2_goalconceded_vec_at != ""]
#   f2_goalconceded_vec_at  <-tail(f2_goalconceded_vec_at,6)
#   f2_goalconceded_vec_at  <- as.numeric(f2_goalconceded_vec_at)
#   f2_at_totalgoalconceded <- sum(f2_goalconceded_vec_at)
#   f2_at_matches_concede <- length(which(f2_goalconceded_vec_at > 0))
#   f2_at_matches_without_concede <- length(which(f2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   f2_totalgoals_vec_ht <- as.vector(f2_totalgoals_h[f2_hometeamindex,])
#   f2_totalgoals_vec_ht[is.na(f2_totalgoals_vec_ht)] <- ""
#   f2_totalgoals_vec_ht <- f2_totalgoals_vec_ht[f2_totalgoals_vec_ht != ""]
#   f2_totalgoals_vec_ht  <-tail(f2_totalgoals_vec_ht,6)
#   f2_totalgoals_vec_ht  <- as.numeric(f2_totalgoals_vec_ht)
#   f2_totalgoals_vec_ht
#   f2_ht_totalgoals <- sum(f2_totalgoals_vec_ht)
#   f2_ht_avgtotalgoals <- (f2_ht_totalgoals/6)
#   f2_ht_no_of_ov25 <- length(which(f2_totalgoals_vec_ht >= 3))
#   f2_ht_no_of_un25 <- length(which(f2_totalgoals_vec_ht <= 2))
#   #awayteam
#   f2_totalgoals_vec_at <- as.vector(f2_totalgoals_h[f2_awayteamindex,])
#   f2_totalgoals_vec_at[is.na(f2_totalgoals_vec_at)] <- ""
#   f2_totalgoals_vec_at <- f2_totalgoals_vec_at[f2_totalgoals_vec_at != ""]
#   f2_totalgoals_vec_at  <-tail(f2_totalgoals_vec_at,6)
#   f2_totalgoals_vec_at  <- as.numeric(f2_totalgoals_vec_at)
#   f2_totalgoals_vec_at
#   f2_at_totalgoals <- sum(f2_totalgoals_vec_at)
#   f2_at_avgtotalgoals <- (f2_at_totalgoals/6)
#   f2_at_no_of_ov25 <- length(which(f2_totalgoals_vec_at >= 3))
#   f2_at_no_of_un25 <- length(which(f2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   f2_winmargin_vec_ht <- as.vector(f2_winmargin_h[f2_hometeamindex,])
#   f2_winmargin_vec_ht[is.na(f2_winmargin_vec_ht)] <- ""
#   f2_winmargin_vec_ht <- f2_winmargin_vec_ht[f2_winmargin_vec_ht != ""]
#   f2_winmargin_vec_ht  <-tail(f2_winmargin_vec_ht,6)
#   f2_winmargin_vec_ht  <- as.numeric(f2_winmargin_vec_ht)
#
#   f2_ht_totalwinmargin <- sum(f2_winmargin_vec_ht)
#   f2_ht_no_of_winmargin_ov0 <- length(which(f2_winmargin_vec_ht >= 0))
#   f2_ht_no_of_winmargin_ov1 <- length(which(f2_winmargin_vec_ht >= 1))
#   f2_ht_no_of_winmargin_un0 <- length(which(f2_winmargin_vec_ht <= 0))
#   f2_ht_no_of_winmargin_un1 <- length(which(f2_winmargin_vec_ht <= 1))
#   #awayteam
#   f2_winmargin_vec_at <- as.vector(f2_winmargin_h[f2_awayteamindex,])
#   f2_winmargin_vec_at[is.na(f2_winmargin_vec_at)] <- ""
#   f2_winmargin_vec_at <- f2_winmargin_vec_at[f2_winmargin_vec_at != ""]
#   f2_winmargin_vec_at  <-tail(f2_winmargin_vec_at,6)
#   f2_winmargin_vec_at  <- as.numeric(f2_winmargin_vec_at)
#
#   f2_at_totalwinmargin <- sum(f2_winmargin_vec_at)
#   f2_at_no_of_winmargin_ov0 <- length(which(f2_winmargin_vec_at >= 0))
#   f2_at_no_of_winmargin_ov1 <- length(which(f2_winmargin_vec_at >= 1))
#   f2_at_no_of_winmargin_un0 <- length(which(f2_winmargin_vec_at <= 0))
#   f2_at_no_of_winmargin_un1 <- length(which(f2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   f2_winmargin_vec_ht_lm <- as.vector(f2_winmargin_h[f2_hometeamindex,])
#   f2_winmargin_vec_ht_lm[is.na(f2_winmargin_vec_ht_lm)] <- ""
#   f2_winmargin_vec_ht_lm <- f2_winmargin_vec_ht_lm[f2_winmargin_vec_ht_lm != ""]
#   f2_winmargin_vec_ht_lm  <-tail(f2_winmargin_vec_ht_lm,1)
#   #awayteam
#   f2_winmargin_vec_at_lm <- as.vector(f2_winmargin_h[f2_awayteamindex,])
#   f2_winmargin_vec_at_lm[is.na(f2_winmargin_vec_at_lm)] <- ""
#   f2_winmargin_vec_at_lm <- f2_winmargin_vec_at_lm[f2_winmargin_vec_at_lm != ""]
#   f2_winmargin_vec_at_lm  <-tail(f2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   f2_yellowtotals_vec_ht <- as.vector(f2_yellowtotalsv2[f2_hometeamindex,])
#   f2_yellowtotals_vec_ht[is.na(f2_yellowtotals_vec_ht)] <- ""
#   f2_yellowtotals_vec_ht <- f2_yellowtotals_vec_ht[f2_yellowtotals_vec_ht != ""]
#   f2_yellowtotals_vec_ht  <-tail(f2_yellowtotals_vec_ht,1)
#   #awayteam
#   f2_yellowtotals_vec_at <- as.vector(f2_yellowtotalsv2[f2_awayteamindex,])
#   f2_yellowtotals_vec_at[is.na(f2_yellowtotals_vec_at)] <- ""
#   f2_yellowtotals_vec_at <- f2_yellowtotals_vec_at[f2_yellowtotals_vec_at != ""]
#   f2_yellowtotals_vec_at  <-tail(f2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   f2_cornertotals_vec_ht <- as.vector(f2_cornertotalsv2[f2_hometeamindex,])
#   f2_cornertotals_vec_ht[is.na(f2_cornertotals_vec_ht)] <- ""
#   f2_cornertotals_vec_ht <- f2_cornertotals_vec_ht[f2_cornertotals_vec_ht != ""]
#   f2_cornertotals_vec_ht  <-tail(f2_cornertotals_vec_ht,1)
#   #awayteam
#   f2_cornertotals_vec_at <- as.vector(f2_cornertotalsv2[f2_awayteamindex,])
#   f2_cornertotals_vec_at[is.na(f2_cornertotals_vec_at)] <- ""
#   f2_cornertotals_vec_at <- f2_cornertotals_vec_at[f2_cornertotals_vec_at != ""]
#   f2_cornertotals_vec_at  <-tail(f2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   f2_xshotsconversion_vec_ht <- as.vector(f2_shots_analysis[f2_hometeamindex,])
#   f2_xshotsconversion_vec_ht[is.na(f2_xshotsconversion_vec_ht)] <- ""
#   f2_xshotsconversion_vec_ht <- f2_xshotsconversion_vec_ht[f2_xshotsconversion_vec_ht != ""]
#   f2_xshotsconversion_vec_ht  <-tail(f2_xshotsconversion_vec_ht,1)
#   #awayteam
#   f2_xshotsconversion_vec_at <- as.vector(f2_shots_analysis[f2_awayteamindex,])
#   f2_xshotsconversion_vec_at[is.na(f2_xshotsconversion_vec_at)] <- ""
#   f2_xshotsconversion_vec_at <- f2_xshotsconversion_vec_at[f2_xshotsconversion_vec_at != ""]
#   f2_xshotsconversion_vec_at  <-tail(f2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   f2_fouls_conversion_vec_ht <- as.vector(f2_fouls_conversion[f2_hometeamindex,])
#   f2_fouls_conversion_vec_ht[is.na(f2_fouls_conversion_vec_ht)] <- ""
#   f2_fouls_conversion_vec_ht <- f2_fouls_conversion_vec_ht[f2_fouls_conversion_vec_ht != ""]
#   f2_fouls_conversion_vec_ht  <-tail(f2_fouls_conversion_vec_ht,1)
#   #awayteam
#   f2_fouls_conversion_vec_at <- as.vector(f2_fouls_conversion[f2_awayteamindex,])
#   f2_fouls_conversion_vec_at[is.na(f2_fouls_conversion_vec_at)] <- ""
#   f2_fouls_conversion_vec_at <- f2_fouls_conversion_vec_at[f2_fouls_conversion_vec_at != ""]
#   f2_fouls_conversion_vec_at  <-tail(f2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   f2_ht_last6points <- f2_ht_numberof_wins*3 + f2_ht_numberof_draws*1
#   f2_at_last6points <- f2_at_numberof_wins*3 + f2_at_numberof_draws*1
#
#   if(f2_ht_last6points > f2_at_last6points) {f2_3waypick <- "1"}  else {f2_3waypick <- "X2"}
#
#   if(f2_at_last6points > f2_ht_last6points ) {f2_3waypick <- "2"} else {f2_3waypick <- "1X"}
#
#   if(f2_ht_no_of_ov25 + f2_at_no_of_ov25 >= 6) {f2_goalspick <- "ov25"} else {f2_goalspick <- "un25"}
#
#   if(f2_ht_no_of_un25 + f2_at_no_of_un25 >= 6) {f2_goalspick <- "un25"} else {f2_goalspick <- "ov25"}
#
#   if(f2_ht_matches_scoring >= 4 && f2_at_matches_scoring >=4) {f2_btts <- "BTTS-Y"} else {f2_btts <- "BTTS-N"}
#
#
#   f2_prediction[f2_row] <- rbind(paste(f2_3waypick,f2_goalspick,f2_btts,sep = ","))
#   f2_HWM[f2_row] <- f2_ht_totalwinmargin
#   f2_AWM[f2_row] <- f2_at_totalwinmargin
#
#   f2_HWMLM[f2_row] <- f2_winmargin_vec_ht_lm
#   f2_AWMLM[f2_row] <- f2_winmargin_vec_at_lm
#
#   f2_HY[f2_row] <- f2_yellowtotals_vec_ht
#   f2_AY[f2_row] <- f2_yellowtotals_vec_at
#
#   f2_HCO[f2_row] <- f2_cornertotals_vec_ht
#   f2_ACO[f2_row] <- f2_cornertotals_vec_at
#
#   f2_HXSC[f2_row] <- f2_xshotsconversion_vec_ht
#   f2_AXSC[f2_row] <- f2_xshotsconversion_vec_at
#
#   f2_HYCPF[f2_row] <- f2_fouls_conversion_vec_ht
#   f2_AYCPF[f2_row] <- f2_fouls_conversion_vec_at
# }
#
# f2_prediction <- as.data.frame(f2_prediction)
# colnames(f2_prediction) <- "prediction"
#
# f2_HWM <- as.data.frame(f2_HWM)
# colnames(f2_HWM) <- "HWM"
#
# f2_AWM <- as.data.frame(f2_AWM)
# colnames(f2_AWM) <- "AWM"
#
# f2_HWMLM <- as.data.frame(f2_HWMLM)
# colnames(f2_HWMLM) <- "HWMLM"
#
# f2_AWMLM <- as.data.frame(f2_AWMLM)
# colnames(f2_AWMLM) <- "AWMLM"
#
# f2_HY <- as.data.frame(f2_HY)
# colnames(f2_HY) <- "AVGHY"
#
# f2_AY <- as.data.frame(f2_AY)
# colnames(f2_AY) <- "AVGAY"
#
# f2_HCO <- as.data.frame(f2_HCO)
# colnames(f2_HCO) <- "AVGHCO"
#
# f2_ACO <- as.data.frame(f2_ACO)
# colnames(f2_ACO) <- "AVGACO"
#
# f2_HXSC <- as.data.frame(f2_HXSC)
# colnames(f2_HXSC) <- "HXSC"
#
# f2_AXSC <- as.data.frame(f2_AXSC)
# colnames(f2_AXSC) <- "AXSC"
#
# f2_HYCPF <- as.data.frame(f2_HYCPF)
# colnames(f2_HYCPF) <- "HYCPF"
#
# f2_AYCPF <- as.data.frame(f2_AYCPF)
# colnames(f2_AYCPF) <- "AYCPF"
#
# f2_picks <- cbind(F2_fixtures$Div,F2_fixtures$HomeTeam_f2,F2_fixtures$AwayTeam_f2,f2_prediction,f2_HWM,f2_AWM,f2_HWMLM,f2_AWMLM,f2_HY,f2_AY,f2_HCO,f2_ACO,f2_HXSC,f2_AXSC,f2_HYCPF,f2_AYCPF)
#
# colnames(f2_picks)[1] <- "picks_Div"
# colnames(f2_picks)[2] <- "picks_HomeTeam"
# colnames(f2_picks)[3] <- "picks_AwayTeam"
# f2_picks$matchid <- paste(f2_picks$picks_HomeTeam,f2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of F2
# f2_picks
# #G1
# G1_fixtures$Hometeam_g1_index <- match(G1_fixtures$HomeTeam_g1,g1_teams)
# G1_fixtures$Awayteam_g1_index <- match(G1_fixtures$AwayTeam_g1,g1_teams)
# g1_prediction <- c()
# g1_HWM <- c()
# g1_AWM <- c()
# g1_HWMLM <- c()
# g1_AWMLM <- c()
# g1_HY <- c()
# g1_AY <- c()
# g1_HCO <- c()
# g1_ACO <- c()
# g1_HXSC <- c()
# g1_AXSC <- c()
# g1_HYCPF <- c()
# g1_AYCPF <- c()
# for(g1_row in 1:nrow(G1_fixtures))
# {
#
#   g1_hometeamindex <- G1_fixtures[g1_row,"Hometeam_g1_index"]
#   g1_awayteamindex <- G1_fixtures[g1_row,"Awayteam_g1_index"]
#   #analyse team form
#   #home team
#   g1_form_vec_ht <- as.vector(g1_form_h[g1_hometeamindex,])
#   g1_form_vec_ht[is.na(g1_form_vec_ht)] <- ""
#   g1_form_vec_ht <- g1_form_vec_ht[g1_form_vec_ht != ""]
#   g1_form_vec_ht  <-tail(g1_form_vec_ht,6)
#   g1_ht_numberof_wins <- length(which(g1_form_vec_ht == "W"))
#   g1_ht_numberof_draws <- length(which(g1_form_vec_ht == "D"))
#   g1_ht_numberof_loss <- length(which(g1_form_vec_ht == "L"))
#   #awayteam
#   g1_form_vec_at <- as.vector(g1_form_h[g1_awayteamindex,])
#   g1_form_vec_at[is.na(g1_form_vec_at)] <- ""
#   g1_form_vec_at <- g1_form_vec_at[g1_form_vec_at != ""]
#   g1_form_vec_at  <-tail(g1_form_vec_at,6)
#   g1_at_numberof_wins <- length(which(g1_form_vec_at == "W"))
#   g1_at_numberof_draws <- length(which(g1_form_vec_at == "D"))
#   g1_at_numberof_loss <- length(which(g1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   g1_goalscored_vec_ht <- as.vector(g1_goalscored_h[g1_hometeamindex,])
#   g1_goalscored_vec_ht[is.na(g1_goalscored_vec_ht)] <- ""
#   g1_goalscored_vec_ht <- g1_goalscored_vec_ht[g1_goalscored_vec_ht != ""]
#   g1_goalscored_vec_ht  <-tail(g1_goalscored_vec_ht,6)
#   g1_goalscored_vec_ht  <- as.numeric(g1_goalscored_vec_ht)
#   g1_ht_totalgoalscored <- sum(g1_goalscored_vec_ht)
#   g1_ht_matches_scoring <- length(which(g1_goalscored_vec_ht > 0))
#   g1_ht_matches_without_scoring <- length(which(g1_goalscored_vec_ht == "0"))
#   #awayteam
#   g1_goalscored_vec_at <- as.vector(g1_goalscored_h[g1_awayteamindex,])
#   g1_goalscored_vec_at[is.na(g1_goalscored_vec_at)] <- ""
#   g1_goalscored_vec_at <- g1_goalscored_vec_at[g1_goalscored_vec_at != ""]
#   g1_goalscored_vec_at  <-tail(g1_goalscored_vec_at,6)
#   g1_goalscored_vec_at  <- as.numeric(g1_goalscored_vec_at)
#   g1_at_totalgoalscored <- sum(g1_goalscored_vec_at)
#   g1_at_matches_scoring <- length(which(g1_goalscored_vec_at > 0))
#   g1_at_matches_without_scoring <- length(which(g1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   g1_goalconceded_vec_ht <- as.vector(g1_goalconceded_h[g1_hometeamindex,])
#   g1_goalconceded_vec_ht[is.na(g1_goalconceded_vec_ht)] <- ""
#   g1_goalconceded_vec_ht <- g1_goalconceded_vec_ht[g1_goalconceded_vec_ht != ""]
#   g1_goalconceded_vec_ht  <-tail(g1_goalconceded_vec_ht,6)
#   g1_goalconceded_vec_ht  <- as.numeric(g1_goalconceded_vec_ht)
#   g1_goalconceded_vec_ht
#   g1_ht_totalgoalconceded <- sum(g1_goalconceded_vec_ht)
#   g1_ht_matches_concede <- length(which(g1_goalconceded_vec_ht > 0))
#   g1_ht_matches_without_concede <- length(which(g1_goalconceded_vec_ht == "0"))
#   #awayteam
#   g1_goalconceded_vec_at <- as.vector(g1_goalconceded_h[g1_awayteamindex,])
#   g1_goalconceded_vec_at[is.na(g1_goalconceded_vec_at)] <- ""
#   g1_goalconceded_vec_at <- g1_goalconceded_vec_at[g1_goalconceded_vec_at != ""]
#   g1_goalconceded_vec_at  <-tail(g1_goalconceded_vec_at,6)
#   g1_goalconceded_vec_at  <- as.numeric(g1_goalconceded_vec_at)
#   g1_at_totalgoalconceded <- sum(g1_goalconceded_vec_at)
#   g1_at_matches_concede <- length(which(g1_goalconceded_vec_at > 0))
#   g1_at_matches_without_concede <- length(which(g1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   g1_totalgoals_vec_ht <- as.vector(g1_totalgoals_h[g1_hometeamindex,])
#   g1_totalgoals_vec_ht[is.na(g1_totalgoals_vec_ht)] <- ""
#   g1_totalgoals_vec_ht <- g1_totalgoals_vec_ht[g1_totalgoals_vec_ht != ""]
#   g1_totalgoals_vec_ht  <-tail(g1_totalgoals_vec_ht,6)
#   g1_totalgoals_vec_ht  <- as.numeric(g1_totalgoals_vec_ht)
#   g1_totalgoals_vec_ht
#   g1_ht_totalgoals <- sum(g1_totalgoals_vec_ht)
#   g1_ht_avgtotalgoals <- (g1_ht_totalgoals/6)
#   g1_ht_no_of_ov25 <- length(which(g1_totalgoals_vec_ht >= 3))
#   g1_ht_no_of_un25 <- length(which(g1_totalgoals_vec_ht <= 2))
#   #awayteam
#   g1_totalgoals_vec_at <- as.vector(g1_totalgoals_h[g1_awayteamindex,])
#   g1_totalgoals_vec_at[is.na(g1_totalgoals_vec_at)] <- ""
#   g1_totalgoals_vec_at <- g1_totalgoals_vec_at[g1_totalgoals_vec_at != ""]
#   g1_totalgoals_vec_at  <-tail(g1_totalgoals_vec_at,6)
#   g1_totalgoals_vec_at  <- as.numeric(g1_totalgoals_vec_at)
#   g1_totalgoals_vec_at
#   g1_at_totalgoals <- sum(g1_totalgoals_vec_at)
#   g1_at_avgtotalgoals <- (g1_at_totalgoals/6)
#   g1_at_no_of_ov25 <- length(which(g1_totalgoals_vec_at >= 3))
#   g1_at_no_of_un25 <- length(which(g1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   g1_winmargin_vec_ht <- as.vector(g1_winmargin_h[g1_hometeamindex,])
#   g1_winmargin_vec_ht[is.na(g1_winmargin_vec_ht)] <- ""
#   g1_winmargin_vec_ht <- g1_winmargin_vec_ht[g1_winmargin_vec_ht != ""]
#   g1_winmargin_vec_ht  <-tail(g1_winmargin_vec_ht,6)
#   g1_winmargin_vec_ht  <- as.numeric(g1_winmargin_vec_ht)
#
#   g1_ht_totalwinmargin <- sum(g1_winmargin_vec_ht)
#   g1_ht_no_of_winmargin_ov0 <- length(which(g1_winmargin_vec_ht >= 0))
#   g1_ht_no_of_winmargin_ov1 <- length(which(g1_winmargin_vec_ht >= 1))
#   g1_ht_no_of_winmargin_un0 <- length(which(g1_winmargin_vec_ht <= 0))
#   g1_ht_no_of_winmargin_un1 <- length(which(g1_winmargin_vec_ht <= 1))
#   #awayteam
#   g1_winmargin_vec_at <- as.vector(g1_winmargin_h[g1_awayteamindex,])
#   g1_winmargin_vec_at[is.na(g1_winmargin_vec_at)] <- ""
#   g1_winmargin_vec_at <- g1_winmargin_vec_at[g1_winmargin_vec_at != ""]
#   g1_winmargin_vec_at  <-tail(g1_winmargin_vec_at,6)
#   g1_winmargin_vec_at  <- as.numeric(g1_winmargin_vec_at)
#
#   g1_at_totalwinmargin <- sum(g1_winmargin_vec_at)
#   g1_at_no_of_winmargin_ov0 <- length(which(g1_winmargin_vec_at >= 0))
#   g1_at_no_of_winmargin_ov1 <- length(which(g1_winmargin_vec_at >= 1))
#   g1_at_no_of_winmargin_un0 <- length(which(g1_winmargin_vec_at <= 0))
#   g1_at_no_of_winmargin_un1 <- length(which(g1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   g1_winmargin_vec_ht_lm <- as.vector(g1_winmargin_h[g1_hometeamindex,])
#   g1_winmargin_vec_ht_lm[is.na(g1_winmargin_vec_ht_lm)] <- ""
#   g1_winmargin_vec_ht_lm <- g1_winmargin_vec_ht_lm[g1_winmargin_vec_ht_lm != ""]
#   g1_winmargin_vec_ht_lm  <-tail(g1_winmargin_vec_ht_lm,1)
#   #awayteam
#   g1_winmargin_vec_at_lm <- as.vector(g1_winmargin_h[g1_awayteamindex,])
#   g1_winmargin_vec_at_lm[is.na(g1_winmargin_vec_at_lm)] <- ""
#   g1_winmargin_vec_at_lm <- g1_winmargin_vec_at_lm[g1_winmargin_vec_at_lm != ""]
#   g1_winmargin_vec_at_lm  <-tail(g1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   g1_yellowtotals_vec_ht <- as.vector(g1_yellowtotalsv2[g1_hometeamindex,])
#   g1_yellowtotals_vec_ht[is.na(g1_yellowtotals_vec_ht)] <- ""
#   g1_yellowtotals_vec_ht <- g1_yellowtotals_vec_ht[g1_yellowtotals_vec_ht != ""]
#   g1_yellowtotals_vec_ht  <-tail(g1_yellowtotals_vec_ht,1)
#   #awayteam
#   g1_yellowtotals_vec_at <- as.vector(g1_yellowtotalsv2[g1_awayteamindex,])
#   g1_yellowtotals_vec_at[is.na(g1_yellowtotals_vec_at)] <- ""
#   g1_yellowtotals_vec_at <- g1_yellowtotals_vec_at[g1_yellowtotals_vec_at != ""]
#   g1_yellowtotals_vec_at  <-tail(g1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   g1_cornertotals_vec_ht <- as.vector(g1_cornertotalsv2[g1_hometeamindex,])
#   g1_cornertotals_vec_ht[is.na(g1_cornertotals_vec_ht)] <- ""
#   g1_cornertotals_vec_ht <- g1_cornertotals_vec_ht[g1_cornertotals_vec_ht != ""]
#   g1_cornertotals_vec_ht  <-tail(g1_cornertotals_vec_ht,1)
#   #awayteam
#   g1_cornertotals_vec_at <- as.vector(g1_cornertotalsv2[g1_awayteamindex,])
#   g1_cornertotals_vec_at[is.na(g1_cornertotals_vec_at)] <- ""
#   g1_cornertotals_vec_at <- g1_cornertotals_vec_at[g1_cornertotals_vec_at != ""]
#   g1_cornertotals_vec_at  <-tail(g1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   g1_xshotsconversion_vec_ht <- as.vector(g1_shots_analysis[g1_hometeamindex,])
#   g1_xshotsconversion_vec_ht[is.na(g1_xshotsconversion_vec_ht)] <- ""
#   g1_xshotsconversion_vec_ht <- g1_xshotsconversion_vec_ht[g1_xshotsconversion_vec_ht != ""]
#   g1_xshotsconversion_vec_ht  <-tail(g1_xshotsconversion_vec_ht,1)
#   #awayteam
#   g1_xshotsconversion_vec_at <- as.vector(g1_shots_analysis[g1_awayteamindex,])
#   g1_xshotsconversion_vec_at[is.na(g1_xshotsconversion_vec_at)] <- ""
#   g1_xshotsconversion_vec_at <- g1_xshotsconversion_vec_at[g1_xshotsconversion_vec_at != ""]
#   g1_xshotsconversion_vec_at  <-tail(g1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   g1_fouls_conversion_vec_ht <- as.vector(g1_fouls_conversion[g1_hometeamindex,])
#   g1_fouls_conversion_vec_ht[is.na(g1_fouls_conversion_vec_ht)] <- ""
#   g1_fouls_conversion_vec_ht <- g1_fouls_conversion_vec_ht[g1_fouls_conversion_vec_ht != ""]
#   g1_fouls_conversion_vec_ht  <-tail(g1_fouls_conversion_vec_ht,1)
#   #awayteam
#   g1_fouls_conversion_vec_at <- as.vector(g1_fouls_conversion[g1_awayteamindex,])
#   g1_fouls_conversion_vec_at[is.na(g1_fouls_conversion_vec_at)] <- ""
#   g1_fouls_conversion_vec_at <- g1_fouls_conversion_vec_at[g1_fouls_conversion_vec_at != ""]
#   g1_fouls_conversion_vec_at  <-tail(g1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   g1_ht_last6points <- g1_ht_numberof_wins*3 + g1_ht_numberof_draws*1
#   g1_at_last6points <- g1_at_numberof_wins*3 + g1_at_numberof_draws*1
#
#   if(g1_ht_last6points > g1_at_last6points) {g1_3waypick <- "1"}  else {g1_3waypick <- "X2"}
#
#   if(g1_at_last6points > g1_ht_last6points ) {g1_3waypick <- "2"} else {g1_3waypick <- "1X"}
#
#   if(g1_ht_no_of_ov25 + g1_at_no_of_ov25 >= 6) {g1_goalspick <- "ov25"} else {g1_goalspick <- "un25"}
#
#   if(g1_ht_no_of_un25 + g1_at_no_of_un25 >= 6) {g1_goalspick <- "un25"} else {g1_goalspick <- "ov25"}
#
#   if(g1_ht_matches_scoring >= 4 && g1_at_matches_scoring >=4) {g1_btts <- "BTTS-Y"} else {g1_btts <- "BTTS-N"}
#
#
#   g1_prediction[g1_row] <- rbind(paste(g1_3waypick,g1_goalspick,g1_btts,sep = ","))
#   g1_HWM[g1_row] <- g1_ht_totalwinmargin
#   g1_AWM[g1_row] <- g1_at_totalwinmargin
#
#   g1_HWMLM[g1_row] <- g1_winmargin_vec_ht_lm
#   g1_AWMLM[g1_row] <- g1_winmargin_vec_at_lm
#
#   g1_HY[g1_row] <- g1_yellowtotals_vec_ht
#   g1_AY[g1_row] <- g1_yellowtotals_vec_at
#
#   g1_HCO[g1_row] <- g1_cornertotals_vec_ht
#   g1_ACO[g1_row] <- g1_cornertotals_vec_at
#
#   g1_HXSC[g1_row] <- g1_xshotsconversion_vec_ht
#   g1_AXSC[g1_row] <- g1_xshotsconversion_vec_at
#
#   g1_HYCPF[g1_row] <- g1_fouls_conversion_vec_ht
#   g1_AYCPF[g1_row] <- g1_fouls_conversion_vec_at
# }
#
# g1_prediction <- as.data.frame(g1_prediction)
# colnames(g1_prediction) <- "prediction"
#
# g1_HWM <- as.data.frame(g1_HWM)
# colnames(g1_HWM) <- "HWM"
#
# g1_AWM <- as.data.frame(g1_AWM)
# colnames(g1_AWM) <- "AWM"
#
# g1_HWMLM <- as.data.frame(g1_HWMLM)
# colnames(g1_HWMLM) <- "HWMLM"
#
# g1_AWMLM <- as.data.frame(g1_AWMLM)
# colnames(g1_AWMLM) <- "AWMLM"
#
# g1_HY <- as.data.frame(g1_HY)
# colnames(g1_HY) <- "AVGHY"
#
# g1_AY <- as.data.frame(g1_AY)
# colnames(g1_AY) <- "AVGAY"
#
# g1_HCO <- as.data.frame(g1_HCO)
# colnames(g1_HCO) <- "AVGHCO"
#
# g1_ACO <- as.data.frame(g1_ACO)
# colnames(g1_ACO) <- "AVGACO"
#
# g1_HXSC <- as.data.frame(g1_HXSC)
# colnames(g1_HXSC) <- "HXSC"
#
# g1_AXSC <- as.data.frame(g1_AXSC)
# colnames(g1_AXSC) <- "AXSC"
#
# g1_HYCPF <- as.data.frame(g1_HYCPF)
# colnames(g1_HYCPF) <- "HYCPF"
#
# g1_AYCPF <- as.data.frame(g1_AYCPF)
# colnames(g1_AYCPF) <- "AYCPF"
#
# g1_picks <- cbind(G1_fixtures$Div,G1_fixtures$HomeTeam_g1,G1_fixtures$AwayTeam_g1,g1_prediction,g1_HWM,g1_AWM,g1_HWMLM,g1_AWMLM,g1_HY,g1_AY,g1_HCO,g1_ACO,g1_HXSC,g1_AXSC,g1_HYCPF,g1_AYCPF)
#
# colnames(g1_picks)[1] <- "picks_Div"
# colnames(g1_picks)[2] <- "picks_HomeTeam"
# colnames(g1_picks)[3] <- "picks_AwayTeam"
# g1_picks$matchid <- paste(g1_picks$picks_HomeTeam,g1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of G1
# g1_picks
# #I1
# I1_fixtures$Hometeam_i1_index <- match(I1_fixtures$HomeTeam_i1,i1_teams)
# I1_fixtures$Awayteam_i1_index <- match(I1_fixtures$AwayTeam_i1,i1_teams)
# i1_prediction <- c()
# i1_HWM <- c()
# i1_AWM <- c()
# i1_HWMLM <- c()
# i1_AWMLM <- c()
# i1_HY <- c()
# i1_AY <- c()
# i1_HCO <- c()
# i1_ACO <- c()
# i1_HXSC <- c()
# i1_AXSC <- c()
# i1_HYCPF <- c()
# i1_AYCPF <- c()
# for(i1_row in 1:nrow(I1_fixtures))
# {
#
#   i1_hometeamindex <- I1_fixtures[i1_row,"Hometeam_i1_index"]
#   i1_awayteamindex <- I1_fixtures[i1_row,"Awayteam_i1_index"]
#   #analyse team form
#   #home team
#   i1_form_vec_ht <- as.vector(i1_form_h[i1_hometeamindex,])
#   i1_form_vec_ht[is.na(i1_form_vec_ht)] <- ""
#   i1_form_vec_ht <- i1_form_vec_ht[i1_form_vec_ht != ""]
#   i1_form_vec_ht  <-tail(i1_form_vec_ht,6)
#   i1_ht_numberof_wins <- length(which(i1_form_vec_ht == "W"))
#   i1_ht_numberof_draws <- length(which(i1_form_vec_ht == "D"))
#   i1_ht_numberof_loss <- length(which(i1_form_vec_ht == "L"))
#   #awayteam
#   i1_form_vec_at <- as.vector(i1_form_h[i1_awayteamindex,])
#   i1_form_vec_at[is.na(i1_form_vec_at)] <- ""
#   i1_form_vec_at <- i1_form_vec_at[i1_form_vec_at != ""]
#   i1_form_vec_at  <-tail(i1_form_vec_at,6)
#   i1_at_numberof_wins <- length(which(i1_form_vec_at == "W"))
#   i1_at_numberof_draws <- length(which(i1_form_vec_at == "D"))
#   i1_at_numberof_loss <- length(which(i1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   i1_goalscored_vec_ht <- as.vector(i1_goalscored_h[i1_hometeamindex,])
#   i1_goalscored_vec_ht[is.na(i1_goalscored_vec_ht)] <- ""
#   i1_goalscored_vec_ht <- i1_goalscored_vec_ht[i1_goalscored_vec_ht != ""]
#   i1_goalscored_vec_ht  <-tail(i1_goalscored_vec_ht,6)
#   i1_goalscored_vec_ht  <- as.numeric(i1_goalscored_vec_ht)
#   i1_ht_totalgoalscored <- sum(i1_goalscored_vec_ht)
#   i1_ht_matches_scoring <- length(which(i1_goalscored_vec_ht > 0))
#   i1_ht_matches_without_scoring <- length(which(i1_goalscored_vec_ht == "0"))
#   #awayteam
#   i1_goalscored_vec_at <- as.vector(i1_goalscored_h[i1_awayteamindex,])
#   i1_goalscored_vec_at[is.na(i1_goalscored_vec_at)] <- ""
#   i1_goalscored_vec_at <- i1_goalscored_vec_at[i1_goalscored_vec_at != ""]
#   i1_goalscored_vec_at  <-tail(i1_goalscored_vec_at,6)
#   i1_goalscored_vec_at  <- as.numeric(i1_goalscored_vec_at)
#   i1_at_totalgoalscored <- sum(i1_goalscored_vec_at)
#   i1_at_matches_scoring <- length(which(i1_goalscored_vec_at > 0))
#   i1_at_matches_without_scoring <- length(which(i1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   i1_goalconceded_vec_ht <- as.vector(i1_goalconceded_h[i1_hometeamindex,])
#   i1_goalconceded_vec_ht[is.na(i1_goalconceded_vec_ht)] <- ""
#   i1_goalconceded_vec_ht <- i1_goalconceded_vec_ht[i1_goalconceded_vec_ht != ""]
#   i1_goalconceded_vec_ht  <-tail(i1_goalconceded_vec_ht,6)
#   i1_goalconceded_vec_ht  <- as.numeric(i1_goalconceded_vec_ht)
#   i1_goalconceded_vec_ht
#   i1_ht_totalgoalconceded <- sum(i1_goalconceded_vec_ht)
#   i1_ht_matches_concede <- length(which(i1_goalconceded_vec_ht > 0))
#   i1_ht_matches_without_concede <- length(which(i1_goalconceded_vec_ht == "0"))
#   #awayteam
#   i1_goalconceded_vec_at <- as.vector(i1_goalconceded_h[i1_awayteamindex,])
#   i1_goalconceded_vec_at[is.na(i1_goalconceded_vec_at)] <- ""
#   i1_goalconceded_vec_at <- i1_goalconceded_vec_at[i1_goalconceded_vec_at != ""]
#   i1_goalconceded_vec_at  <-tail(i1_goalconceded_vec_at,6)
#   i1_goalconceded_vec_at  <- as.numeric(i1_goalconceded_vec_at)
#   i1_at_totalgoalconceded <- sum(i1_goalconceded_vec_at)
#   i1_at_matches_concede <- length(which(i1_goalconceded_vec_at > 0))
#   i1_at_matches_without_concede <- length(which(i1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   i1_totalgoals_vec_ht <- as.vector(i1_totalgoals_h[i1_hometeamindex,])
#   i1_totalgoals_vec_ht[is.na(i1_totalgoals_vec_ht)] <- ""
#   i1_totalgoals_vec_ht <- i1_totalgoals_vec_ht[i1_totalgoals_vec_ht != ""]
#   i1_totalgoals_vec_ht  <-tail(i1_totalgoals_vec_ht,6)
#   i1_totalgoals_vec_ht  <- as.numeric(i1_totalgoals_vec_ht)
#   i1_totalgoals_vec_ht
#   i1_ht_totalgoals <- sum(i1_totalgoals_vec_ht)
#   i1_ht_avgtotalgoals <- (i1_ht_totalgoals/6)
#   i1_ht_no_of_ov25 <- length(which(i1_totalgoals_vec_ht >= 3))
#   i1_ht_no_of_un25 <- length(which(i1_totalgoals_vec_ht <= 2))
#   #awayteam
#   i1_totalgoals_vec_at <- as.vector(i1_totalgoals_h[i1_awayteamindex,])
#   i1_totalgoals_vec_at[is.na(i1_totalgoals_vec_at)] <- ""
#   i1_totalgoals_vec_at <- i1_totalgoals_vec_at[i1_totalgoals_vec_at != ""]
#   i1_totalgoals_vec_at  <-tail(i1_totalgoals_vec_at,6)
#   i1_totalgoals_vec_at  <- as.numeric(i1_totalgoals_vec_at)
#   i1_totalgoals_vec_at
#   i1_at_totalgoals <- sum(i1_totalgoals_vec_at)
#   i1_at_avgtotalgoals <- (i1_at_totalgoals/6)
#   i1_at_no_of_ov25 <- length(which(i1_totalgoals_vec_at >= 3))
#   i1_at_no_of_un25 <- length(which(i1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   i1_winmargin_vec_ht <- as.vector(i1_winmargin_h[i1_hometeamindex,])
#   i1_winmargin_vec_ht[is.na(i1_winmargin_vec_ht)] <- ""
#   i1_winmargin_vec_ht <- i1_winmargin_vec_ht[i1_winmargin_vec_ht != ""]
#   i1_winmargin_vec_ht  <-tail(i1_winmargin_vec_ht,6)
#   i1_winmargin_vec_ht  <- as.numeric(i1_winmargin_vec_ht)
#
#   i1_ht_totalwinmargin <- sum(i1_winmargin_vec_ht)
#   i1_ht_no_of_winmargin_ov0 <- length(which(i1_winmargin_vec_ht >= 0))
#   i1_ht_no_of_winmargin_ov1 <- length(which(i1_winmargin_vec_ht >= 1))
#   i1_ht_no_of_winmargin_un0 <- length(which(i1_winmargin_vec_ht <= 0))
#   i1_ht_no_of_winmargin_un1 <- length(which(i1_winmargin_vec_ht <= 1))
#   #awayteam
#   i1_winmargin_vec_at <- as.vector(i1_winmargin_h[i1_awayteamindex,])
#   i1_winmargin_vec_at[is.na(i1_winmargin_vec_at)] <- ""
#   i1_winmargin_vec_at <- i1_winmargin_vec_at[i1_winmargin_vec_at != ""]
#   i1_winmargin_vec_at  <-tail(i1_winmargin_vec_at,6)
#   i1_winmargin_vec_at  <- as.numeric(i1_winmargin_vec_at)
#
#   i1_at_totalwinmargin <- sum(i1_winmargin_vec_at)
#   i1_at_no_of_winmargin_ov0 <- length(which(i1_winmargin_vec_at >= 0))
#   i1_at_no_of_winmargin_ov1 <- length(which(i1_winmargin_vec_at >= 1))
#   i1_at_no_of_winmargin_un0 <- length(which(i1_winmargin_vec_at <= 0))
#   i1_at_no_of_winmargin_un1 <- length(which(i1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   i1_winmargin_vec_ht_lm <- as.vector(i1_winmargin_h[i1_hometeamindex,])
#   i1_winmargin_vec_ht_lm[is.na(i1_winmargin_vec_ht_lm)] <- ""
#   i1_winmargin_vec_ht_lm <- i1_winmargin_vec_ht_lm[i1_winmargin_vec_ht_lm != ""]
#   i1_winmargin_vec_ht_lm  <-tail(i1_winmargin_vec_ht_lm,1)
#   #awayteam
#   i1_winmargin_vec_at_lm <- as.vector(i1_winmargin_h[i1_awayteamindex,])
#   i1_winmargin_vec_at_lm[is.na(i1_winmargin_vec_at_lm)] <- ""
#   i1_winmargin_vec_at_lm <- i1_winmargin_vec_at_lm[i1_winmargin_vec_at_lm != ""]
#   i1_winmargin_vec_at_lm  <-tail(i1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   i1_yellowtotals_vec_ht <- as.vector(i1_yellowtotalsv2[i1_hometeamindex,])
#   i1_yellowtotals_vec_ht[is.na(i1_yellowtotals_vec_ht)] <- ""
#   i1_yellowtotals_vec_ht <- i1_yellowtotals_vec_ht[i1_yellowtotals_vec_ht != ""]
#   i1_yellowtotals_vec_ht  <-tail(i1_yellowtotals_vec_ht,1)
#   #awayteam
#   i1_yellowtotals_vec_at <- as.vector(i1_yellowtotalsv2[i1_awayteamindex,])
#   i1_yellowtotals_vec_at[is.na(i1_yellowtotals_vec_at)] <- ""
#   i1_yellowtotals_vec_at <- i1_yellowtotals_vec_at[i1_yellowtotals_vec_at != ""]
#   i1_yellowtotals_vec_at  <-tail(i1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   i1_cornertotals_vec_ht <- as.vector(i1_cornertotalsv2[i1_hometeamindex,])
#   i1_cornertotals_vec_ht[is.na(i1_cornertotals_vec_ht)] <- ""
#   i1_cornertotals_vec_ht <- i1_cornertotals_vec_ht[i1_cornertotals_vec_ht != ""]
#   i1_cornertotals_vec_ht  <-tail(i1_cornertotals_vec_ht,1)
#   #awayteam
#   i1_cornertotals_vec_at <- as.vector(i1_cornertotalsv2[i1_awayteamindex,])
#   i1_cornertotals_vec_at[is.na(i1_cornertotals_vec_at)] <- ""
#   i1_cornertotals_vec_at <- i1_cornertotals_vec_at[i1_cornertotals_vec_at != ""]
#   i1_cornertotals_vec_at  <-tail(i1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   i1_xshotsconversion_vec_ht <- as.vector(i1_shots_analysis[i1_hometeamindex,])
#   i1_xshotsconversion_vec_ht[is.na(i1_xshotsconversion_vec_ht)] <- ""
#   i1_xshotsconversion_vec_ht <- i1_xshotsconversion_vec_ht[i1_xshotsconversion_vec_ht != ""]
#   i1_xshotsconversion_vec_ht  <-tail(i1_xshotsconversion_vec_ht,1)
#   #awayteam
#   i1_xshotsconversion_vec_at <- as.vector(i1_shots_analysis[i1_awayteamindex,])
#   i1_xshotsconversion_vec_at[is.na(i1_xshotsconversion_vec_at)] <- ""
#   i1_xshotsconversion_vec_at <- i1_xshotsconversion_vec_at[i1_xshotsconversion_vec_at != ""]
#   i1_xshotsconversion_vec_at  <-tail(i1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   i1_fouls_conversion_vec_ht <- as.vector(i1_fouls_conversion[i1_hometeamindex,])
#   i1_fouls_conversion_vec_ht[is.na(i1_fouls_conversion_vec_ht)] <- ""
#   i1_fouls_conversion_vec_ht <- i1_fouls_conversion_vec_ht[i1_fouls_conversion_vec_ht != ""]
#   i1_fouls_conversion_vec_ht  <-tail(i1_fouls_conversion_vec_ht,1)
#   #awayteam
#   i1_fouls_conversion_vec_at <- as.vector(i1_fouls_conversion[i1_awayteamindex,])
#   i1_fouls_conversion_vec_at[is.na(i1_fouls_conversion_vec_at)] <- ""
#   i1_fouls_conversion_vec_at <- i1_fouls_conversion_vec_at[i1_fouls_conversion_vec_at != ""]
#   i1_fouls_conversion_vec_at  <-tail(i1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   i1_ht_last6points <- i1_ht_numberof_wins*3 + i1_ht_numberof_draws*1
#   i1_at_last6points <- i1_at_numberof_wins*3 + i1_at_numberof_draws*1
#
#   if(i1_ht_last6points > i1_at_last6points) {i1_3waypick <- "1"}  else {i1_3waypick <- "X2"}
#
#   if(i1_at_last6points > i1_ht_last6points ) {i1_3waypick <- "2"} else {i1_3waypick <- "1X"}
#
#   if(i1_ht_no_of_ov25 + i1_at_no_of_ov25 >= 6) {i1_goalspick <- "ov25"} else {i1_goalspick <- "un25"}
#
#   if(i1_ht_no_of_un25 + i1_at_no_of_un25 >= 6) {i1_goalspick <- "un25"} else {i1_goalspick <- "ov25"}
#
#   if(i1_ht_matches_scoring >= 4 && i1_at_matches_scoring >=4) {i1_btts <- "BTTS-Y"} else {i1_btts <- "BTTS-N"}
#
#
#   i1_prediction[i1_row] <- rbind(paste(i1_3waypick,i1_goalspick,i1_btts,sep = ","))
#   i1_HWM[i1_row] <- i1_ht_totalwinmargin
#   i1_AWM[i1_row] <- i1_at_totalwinmargin
#
#   i1_HWMLM[i1_row] <- i1_winmargin_vec_ht_lm
#   i1_AWMLM[i1_row] <- i1_winmargin_vec_at_lm
#
#   i1_HY[i1_row] <- i1_yellowtotals_vec_ht
#   i1_AY[i1_row] <- i1_yellowtotals_vec_at
#
#   i1_HCO[i1_row] <- i1_cornertotals_vec_ht
#   i1_ACO[i1_row] <- i1_cornertotals_vec_at
#
#   i1_HXSC[i1_row] <- i1_xshotsconversion_vec_ht
#   i1_AXSC[i1_row] <- i1_xshotsconversion_vec_at
#
#   i1_HYCPF[i1_row] <- i1_fouls_conversion_vec_ht
#   i1_AYCPF[i1_row] <- i1_fouls_conversion_vec_at
# }
#
# i1_prediction <- as.data.frame(i1_prediction)
# colnames(i1_prediction) <- "prediction"
#
# i1_HWM <- as.data.frame(i1_HWM)
# colnames(i1_HWM) <- "HWM"
#
# i1_AWM <- as.data.frame(i1_AWM)
# colnames(i1_AWM) <- "AWM"
#
# i1_HWMLM <- as.data.frame(i1_HWMLM)
# colnames(i1_HWMLM) <- "HWMLM"
#
# i1_AWMLM <- as.data.frame(i1_AWMLM)
# colnames(i1_AWMLM) <- "AWMLM"
#
# i1_HY <- as.data.frame(i1_HY)
# colnames(i1_HY) <- "AVGHY"
#
# i1_AY <- as.data.frame(i1_AY)
# colnames(i1_AY) <- "AVGAY"
#
# i1_HCO <- as.data.frame(i1_HCO)
# colnames(i1_HCO) <- "AVGHCO"
#
# i1_ACO <- as.data.frame(i1_ACO)
# colnames(i1_ACO) <- "AVGACO"
#
# i1_HXSC <- as.data.frame(i1_HXSC)
# colnames(i1_HXSC) <- "HXSC"
#
# i1_AXSC <- as.data.frame(i1_AXSC)
# colnames(i1_AXSC) <- "AXSC"
#
# i1_HYCPF <- as.data.frame(i1_HYCPF)
# colnames(i1_HYCPF) <- "HYCPF"
#
# i1_AYCPF <- as.data.frame(i1_AYCPF)
# colnames(i1_AYCPF) <- "AYCPF"
#
# i1_picks <- cbind(I1_fixtures$Div,I1_fixtures$HomeTeam_i1,I1_fixtures$AwayTeam_i1,i1_prediction,i1_HWM,i1_AWM,i1_HWMLM,i1_AWMLM,i1_HY,i1_AY,i1_HCO,i1_ACO,i1_HXSC,i1_AXSC,i1_HYCPF,i1_AYCPF)
#
# colnames(i1_picks)[1] <- "picks_Div"
# colnames(i1_picks)[2] <- "picks_HomeTeam"
# colnames(i1_picks)[3] <- "picks_AwayTeam"
# i1_picks$matchid <- paste(i1_picks$picks_HomeTeam,i1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of I1
# i1_picks
# #I2
# I2_fixtures$Hometeam_i2_index <- match(I2_fixtures$HomeTeam_i2,i2_teams)
# I2_fixtures$Awayteam_i2_index <- match(I2_fixtures$AwayTeam_i2,i2_teams)
# i2_prediction <- c()
# i2_HWM <- c()
# i2_AWM <- c()
# i2_HWMLM <- c()
# i2_AWMLM <- c()
# i2_HY <- c()
# i2_AY <- c()
# i2_HCO <- c()
# i2_ACO <- c()
# i2_HXSC <- c()
# i2_AXSC <- c()
# i2_HYCPF <- c()
# i2_AYCPF <- c()
# for(i2_row in 1:nrow(I2_fixtures))
# {
#
#   i2_hometeamindex <- I2_fixtures[i2_row,"Hometeam_i2_index"]
#   i2_awayteamindex <- I2_fixtures[i2_row,"Awayteam_i2_index"]
#   #analyse team form
#   #home team
#   i2_form_vec_ht <- as.vector(i2_form_h[i2_hometeamindex,])
#   i2_form_vec_ht[is.na(i2_form_vec_ht)] <- ""
#   i2_form_vec_ht <- i2_form_vec_ht[i2_form_vec_ht != ""]
#   i2_form_vec_ht  <-tail(i2_form_vec_ht,6)
#   i2_ht_numberof_wins <- length(which(i2_form_vec_ht == "W"))
#   i2_ht_numberof_draws <- length(which(i2_form_vec_ht == "D"))
#   i2_ht_numberof_loss <- length(which(i2_form_vec_ht == "L"))
#   #awayteam
#   i2_form_vec_at <- as.vector(i2_form_h[i2_awayteamindex,])
#   i2_form_vec_at[is.na(i2_form_vec_at)] <- ""
#   i2_form_vec_at <- i2_form_vec_at[i2_form_vec_at != ""]
#   i2_form_vec_at  <-tail(i2_form_vec_at,6)
#   i2_at_numberof_wins <- length(which(i2_form_vec_at == "W"))
#   i2_at_numberof_draws <- length(which(i2_form_vec_at == "D"))
#   i2_at_numberof_loss <- length(which(i2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   i2_goalscored_vec_ht <- as.vector(i2_goalscored_h[i2_hometeamindex,])
#   i2_goalscored_vec_ht[is.na(i2_goalscored_vec_ht)] <- ""
#   i2_goalscored_vec_ht <- i2_goalscored_vec_ht[i2_goalscored_vec_ht != ""]
#   i2_goalscored_vec_ht  <-tail(i2_goalscored_vec_ht,6)
#   i2_goalscored_vec_ht  <- as.numeric(i2_goalscored_vec_ht)
#   i2_ht_totalgoalscored <- sum(i2_goalscored_vec_ht)
#   i2_ht_matches_scoring <- length(which(i2_goalscored_vec_ht > 0))
#   i2_ht_matches_without_scoring <- length(which(i2_goalscored_vec_ht == "0"))
#   #awayteam
#   i2_goalscored_vec_at <- as.vector(i2_goalscored_h[i2_awayteamindex,])
#   i2_goalscored_vec_at[is.na(i2_goalscored_vec_at)] <- ""
#   i2_goalscored_vec_at <- i2_goalscored_vec_at[i2_goalscored_vec_at != ""]
#   i2_goalscored_vec_at  <-tail(i2_goalscored_vec_at,6)
#   i2_goalscored_vec_at  <- as.numeric(i2_goalscored_vec_at)
#   i2_at_totalgoalscored <- sum(i2_goalscored_vec_at)
#   i2_at_matches_scoring <- length(which(i2_goalscored_vec_at > 0))
#   i2_at_matches_without_scoring <- length(which(i2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   i2_goalconceded_vec_ht <- as.vector(i2_goalconceded_h[i2_hometeamindex,])
#   i2_goalconceded_vec_ht[is.na(i2_goalconceded_vec_ht)] <- ""
#   i2_goalconceded_vec_ht <- i2_goalconceded_vec_ht[i2_goalconceded_vec_ht != ""]
#   i2_goalconceded_vec_ht  <-tail(i2_goalconceded_vec_ht,6)
#   i2_goalconceded_vec_ht  <- as.numeric(i2_goalconceded_vec_ht)
#   i2_goalconceded_vec_ht
#   i2_ht_totalgoalconceded <- sum(i2_goalconceded_vec_ht)
#   i2_ht_matches_concede <- length(which(i2_goalconceded_vec_ht > 0))
#   i2_ht_matches_without_concede <- length(which(i2_goalconceded_vec_ht == "0"))
#   #awayteam
#   i2_goalconceded_vec_at <- as.vector(i2_goalconceded_h[i2_awayteamindex,])
#   i2_goalconceded_vec_at[is.na(i2_goalconceded_vec_at)] <- ""
#   i2_goalconceded_vec_at <- i2_goalconceded_vec_at[i2_goalconceded_vec_at != ""]
#   i2_goalconceded_vec_at  <-tail(i2_goalconceded_vec_at,6)
#   i2_goalconceded_vec_at  <- as.numeric(i2_goalconceded_vec_at)
#   i2_at_totalgoalconceded <- sum(i2_goalconceded_vec_at)
#   i2_at_matches_concede <- length(which(i2_goalconceded_vec_at > 0))
#   i2_at_matches_without_concede <- length(which(i2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   i2_totalgoals_vec_ht <- as.vector(i2_totalgoals_h[i2_hometeamindex,])
#   i2_totalgoals_vec_ht[is.na(i2_totalgoals_vec_ht)] <- ""
#   i2_totalgoals_vec_ht <- i2_totalgoals_vec_ht[i2_totalgoals_vec_ht != ""]
#   i2_totalgoals_vec_ht  <-tail(i2_totalgoals_vec_ht,6)
#   i2_totalgoals_vec_ht  <- as.numeric(i2_totalgoals_vec_ht)
#   i2_totalgoals_vec_ht
#   i2_ht_totalgoals <- sum(i2_totalgoals_vec_ht)
#   i2_ht_avgtotalgoals <- (i2_ht_totalgoals/6)
#   i2_ht_no_of_ov25 <- length(which(i2_totalgoals_vec_ht >= 3))
#   i2_ht_no_of_un25 <- length(which(i2_totalgoals_vec_ht <= 2))
#   #awayteam
#   i2_totalgoals_vec_at <- as.vector(i2_totalgoals_h[i2_awayteamindex,])
#   i2_totalgoals_vec_at[is.na(i2_totalgoals_vec_at)] <- ""
#   i2_totalgoals_vec_at <- i2_totalgoals_vec_at[i2_totalgoals_vec_at != ""]
#   i2_totalgoals_vec_at  <-tail(i2_totalgoals_vec_at,6)
#   i2_totalgoals_vec_at  <- as.numeric(i2_totalgoals_vec_at)
#   i2_totalgoals_vec_at
#   i2_at_totalgoals <- sum(i2_totalgoals_vec_at)
#   i2_at_avgtotalgoals <- (i2_at_totalgoals/6)
#   i2_at_no_of_ov25 <- length(which(i2_totalgoals_vec_at >= 3))
#   i2_at_no_of_un25 <- length(which(i2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   i2_winmargin_vec_ht <- as.vector(i2_winmargin_h[i2_hometeamindex,])
#   i2_winmargin_vec_ht[is.na(i2_winmargin_vec_ht)] <- ""
#   i2_winmargin_vec_ht <- i2_winmargin_vec_ht[i2_winmargin_vec_ht != ""]
#   i2_winmargin_vec_ht  <-tail(i2_winmargin_vec_ht,6)
#   i2_winmargin_vec_ht  <- as.numeric(i2_winmargin_vec_ht)
#
#   i2_ht_totalwinmargin <- sum(i2_winmargin_vec_ht)
#   i2_ht_no_of_winmargin_ov0 <- length(which(i2_winmargin_vec_ht >= 0))
#   i2_ht_no_of_winmargin_ov1 <- length(which(i2_winmargin_vec_ht >= 1))
#   i2_ht_no_of_winmargin_un0 <- length(which(i2_winmargin_vec_ht <= 0))
#   i2_ht_no_of_winmargin_un1 <- length(which(i2_winmargin_vec_ht <= 1))
#   #awayteam
#   i2_winmargin_vec_at <- as.vector(i2_winmargin_h[i2_awayteamindex,])
#   i2_winmargin_vec_at[is.na(i2_winmargin_vec_at)] <- ""
#   i2_winmargin_vec_at <- i2_winmargin_vec_at[i2_winmargin_vec_at != ""]
#   i2_winmargin_vec_at  <-tail(i2_winmargin_vec_at,6)
#   i2_winmargin_vec_at  <- as.numeric(i2_winmargin_vec_at)
#
#   i2_at_totalwinmargin <- sum(i2_winmargin_vec_at)
#   i2_at_no_of_winmargin_ov0 <- length(which(i2_winmargin_vec_at >= 0))
#   i2_at_no_of_winmargin_ov1 <- length(which(i2_winmargin_vec_at >= 1))
#   i2_at_no_of_winmargin_un0 <- length(which(i2_winmargin_vec_at <= 0))
#   i2_at_no_of_winmargin_un1 <- length(which(i2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   i2_winmargin_vec_ht_lm <- as.vector(i2_winmargin_h[i2_hometeamindex,])
#   i2_winmargin_vec_ht_lm[is.na(i2_winmargin_vec_ht_lm)] <- ""
#   i2_winmargin_vec_ht_lm <- i2_winmargin_vec_ht_lm[i2_winmargin_vec_ht_lm != ""]
#   i2_winmargin_vec_ht_lm  <-tail(i2_winmargin_vec_ht_lm,1)
#   #awayteam
#   i2_winmargin_vec_at_lm <- as.vector(i2_winmargin_h[i2_awayteamindex,])
#   i2_winmargin_vec_at_lm[is.na(i2_winmargin_vec_at_lm)] <- ""
#   i2_winmargin_vec_at_lm <- i2_winmargin_vec_at_lm[i2_winmargin_vec_at_lm != ""]
#   i2_winmargin_vec_at_lm  <-tail(i2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   i2_yellowtotals_vec_ht <- as.vector(i2_yellowtotalsv2[i2_hometeamindex,])
#   i2_yellowtotals_vec_ht[is.na(i2_yellowtotals_vec_ht)] <- ""
#   i2_yellowtotals_vec_ht <- i2_yellowtotals_vec_ht[i2_yellowtotals_vec_ht != ""]
#   i2_yellowtotals_vec_ht  <-tail(i2_yellowtotals_vec_ht,1)
#   #awayteam
#   i2_yellowtotals_vec_at <- as.vector(i2_yellowtotalsv2[i2_awayteamindex,])
#   i2_yellowtotals_vec_at[is.na(i2_yellowtotals_vec_at)] <- ""
#   i2_yellowtotals_vec_at <- i2_yellowtotals_vec_at[i2_yellowtotals_vec_at != ""]
#   i2_yellowtotals_vec_at  <-tail(i2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   i2_cornertotals_vec_ht <- as.vector(i2_cornertotalsv2[i2_hometeamindex,])
#   i2_cornertotals_vec_ht[is.na(i2_cornertotals_vec_ht)] <- ""
#   i2_cornertotals_vec_ht <- i2_cornertotals_vec_ht[i2_cornertotals_vec_ht != ""]
#   i2_cornertotals_vec_ht  <-tail(i2_cornertotals_vec_ht,1)
#   #awayteam
#   i2_cornertotals_vec_at <- as.vector(i2_cornertotalsv2[i2_awayteamindex,])
#   i2_cornertotals_vec_at[is.na(i2_cornertotals_vec_at)] <- ""
#   i2_cornertotals_vec_at <- i2_cornertotals_vec_at[i2_cornertotals_vec_at != ""]
#   i2_cornertotals_vec_at  <-tail(i2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   i2_xshotsconversion_vec_ht <- as.vector(i2_shots_analysis[i2_hometeamindex,])
#   i2_xshotsconversion_vec_ht[is.na(i2_xshotsconversion_vec_ht)] <- ""
#   i2_xshotsconversion_vec_ht <- i2_xshotsconversion_vec_ht[i2_xshotsconversion_vec_ht != ""]
#   i2_xshotsconversion_vec_ht  <-tail(i2_xshotsconversion_vec_ht,1)
#   #awayteam
#   i2_xshotsconversion_vec_at <- as.vector(i2_shots_analysis[i2_awayteamindex,])
#   i2_xshotsconversion_vec_at[is.na(i2_xshotsconversion_vec_at)] <- ""
#   i2_xshotsconversion_vec_at <- i2_xshotsconversion_vec_at[i2_xshotsconversion_vec_at != ""]
#   i2_xshotsconversion_vec_at  <-tail(i2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   i2_fouls_conversion_vec_ht <- as.vector(i2_fouls_conversion[i2_hometeamindex,])
#   i2_fouls_conversion_vec_ht[is.na(i2_fouls_conversion_vec_ht)] <- ""
#   i2_fouls_conversion_vec_ht <- i2_fouls_conversion_vec_ht[i2_fouls_conversion_vec_ht != ""]
#   i2_fouls_conversion_vec_ht  <-tail(i2_fouls_conversion_vec_ht,1)
#   #awayteam
#   i2_fouls_conversion_vec_at <- as.vector(i2_fouls_conversion[i2_awayteamindex,])
#   i2_fouls_conversion_vec_at[is.na(i2_fouls_conversion_vec_at)] <- ""
#   i2_fouls_conversion_vec_at <- i2_fouls_conversion_vec_at[i2_fouls_conversion_vec_at != ""]
#   i2_fouls_conversion_vec_at  <-tail(i2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   i2_ht_last6points <- i2_ht_numberof_wins*3 + i2_ht_numberof_draws*1
#   i2_at_last6points <- i2_at_numberof_wins*3 + i2_at_numberof_draws*1
#
#   if(i2_ht_last6points > i2_at_last6points) {i2_3waypick <- "1"}  else {i2_3waypick <- "X2"}
#
#   if(i2_at_last6points > i2_ht_last6points ) {i2_3waypick <- "2"} else {i2_3waypick <- "1X"}
#
#   if(i2_ht_no_of_ov25 + i2_at_no_of_ov25 >= 6) {i2_goalspick <- "ov25"} else {i2_goalspick <- "un25"}
#
#   if(i2_ht_no_of_un25 + i2_at_no_of_un25 >= 6) {i2_goalspick <- "un25"} else {i2_goalspick <- "ov25"}
#
#   if(i2_ht_matches_scoring >= 4 && i2_at_matches_scoring >=4) {i2_btts <- "BTTS-Y"} else {i2_btts <- "BTTS-N"}
#
#
#   i2_prediction[i2_row] <- rbind(paste(i2_3waypick,i2_goalspick,i2_btts,sep = ","))
#   i2_HWM[i2_row] <- i2_ht_totalwinmargin
#   i2_AWM[i2_row] <- i2_at_totalwinmargin
#
#   i2_HWMLM[i2_row] <- i2_winmargin_vec_ht_lm
#   i2_AWMLM[i2_row] <- i2_winmargin_vec_at_lm
#
#   i2_HY[i2_row] <- i2_yellowtotals_vec_ht
#   i2_AY[i2_row] <- i2_yellowtotals_vec_at
#
#   i2_HCO[i2_row] <- i2_cornertotals_vec_ht
#   i2_ACO[i2_row] <- i2_cornertotals_vec_at
#
#   i2_HXSC[i2_row] <- i2_xshotsconversion_vec_ht
#   i2_AXSC[i2_row] <- i2_xshotsconversion_vec_at
#
#   i2_HYCPF[i2_row] <- i2_fouls_conversion_vec_ht
#   i2_AYCPF[i2_row] <- i2_fouls_conversion_vec_at
# }
#
# i2_prediction <- as.data.frame(i2_prediction)
# colnames(i2_prediction) <- "prediction"
#
# i2_HWM <- as.data.frame(i2_HWM)
# colnames(i2_HWM) <- "HWM"
#
# i2_AWM <- as.data.frame(i2_AWM)
# colnames(i2_AWM) <- "AWM"
#
# i2_HWMLM <- as.data.frame(i2_HWMLM)
# colnames(i2_HWMLM) <- "HWMLM"
#
# i2_AWMLM <- as.data.frame(i2_AWMLM)
# colnames(i2_AWMLM) <- "AWMLM"
#
# i2_HY <- as.data.frame(i2_HY)
# colnames(i2_HY) <- "AVGHY"
#
# i2_AY <- as.data.frame(i2_AY)
# colnames(i2_AY) <- "AVGAY"
#
# i2_HCO <- as.data.frame(i2_HCO)
# colnames(i2_HCO) <- "AVGHCO"
#
# i2_ACO <- as.data.frame(i2_ACO)
# colnames(i2_ACO) <- "AVGACO"
#
# i2_HXSC <- as.data.frame(i2_HXSC)
# colnames(i2_HXSC) <- "HXSC"
#
# i2_AXSC <- as.data.frame(i2_AXSC)
# colnames(i2_AXSC) <- "AXSC"
#
# i2_HYCPF <- as.data.frame(i2_HYCPF)
# colnames(i2_HYCPF) <- "HYCPF"
#
# i2_AYCPF <- as.data.frame(i2_AYCPF)
# colnames(i2_AYCPF) <- "AYCPF"
#
# i2_picks <- cbind(I2_fixtures$Div,I2_fixtures$HomeTeam_i2,I2_fixtures$AwayTeam_i2,i2_prediction,i2_HWM,i2_AWM,i2_HWMLM,i2_AWMLM,i2_HY,i2_AY,i2_HCO,i2_ACO,i2_HXSC,i2_AXSC,i2_HYCPF,i2_AYCPF)
#
# colnames(i2_picks)[1] <- "picks_Div"
# colnames(i2_picks)[2] <- "picks_HomeTeam"
# colnames(i2_picks)[3] <- "picks_AwayTeam"
# i2_picks$matchid <- paste(i2_picks$picks_HomeTeam,i2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of I2
# i2_picks
# #N1
# N1_fixtures$Hometeam_n1_index <- match(N1_fixtures$HomeTeam_n1,n1_teams)
# N1_fixtures$Awayteam_n1_index <- match(N1_fixtures$AwayTeam_n1,n1_teams)
# n1_prediction <- c()
# n1_HWM <- c()
# n1_AWM <- c()
# n1_HWMLM <- c()
# n1_AWMLM <- c()
# n1_HY <- c()
# n1_AY <- c()
# n1_HCO <- c()
# n1_ACO <- c()
# n1_HXSC <- c()
# n1_AXSC <- c()
# n1_HYCPF <- c()
# n1_AYCPF <- c()
# for(n1_row in 1:nrow(N1_fixtures))
# {
#
#   n1_hometeamindex <- N1_fixtures[n1_row,"Hometeam_n1_index"]
#   n1_awayteamindex <- N1_fixtures[n1_row,"Awayteam_n1_index"]
#   #analyse team form
#   #home team
#   n1_form_vec_ht <- as.vector(n1_form_h[n1_hometeamindex,])
#   n1_form_vec_ht[is.na(n1_form_vec_ht)] <- ""
#   n1_form_vec_ht <- n1_form_vec_ht[n1_form_vec_ht != ""]
#   n1_form_vec_ht  <-tail(n1_form_vec_ht,6)
#   n1_ht_numberof_wins <- length(which(n1_form_vec_ht == "W"))
#   n1_ht_numberof_draws <- length(which(n1_form_vec_ht == "D"))
#   n1_ht_numberof_loss <- length(which(n1_form_vec_ht == "L"))
#   #awayteam
#   n1_form_vec_at <- as.vector(n1_form_h[n1_awayteamindex,])
#   n1_form_vec_at[is.na(n1_form_vec_at)] <- ""
#   n1_form_vec_at <- n1_form_vec_at[n1_form_vec_at != ""]
#   n1_form_vec_at  <-tail(n1_form_vec_at,6)
#   n1_at_numberof_wins <- length(which(n1_form_vec_at == "W"))
#   n1_at_numberof_draws <- length(which(n1_form_vec_at == "D"))
#   n1_at_numberof_loss <- length(which(n1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   n1_goalscored_vec_ht <- as.vector(n1_goalscored_h[n1_hometeamindex,])
#   n1_goalscored_vec_ht[is.na(n1_goalscored_vec_ht)] <- ""
#   n1_goalscored_vec_ht <- n1_goalscored_vec_ht[n1_goalscored_vec_ht != ""]
#   n1_goalscored_vec_ht  <-tail(n1_goalscored_vec_ht,6)
#   n1_goalscored_vec_ht  <- as.numeric(n1_goalscored_vec_ht)
#   n1_ht_totalgoalscored <- sum(n1_goalscored_vec_ht)
#   n1_ht_matches_scoring <- length(which(n1_goalscored_vec_ht > 0))
#   n1_ht_matches_without_scoring <- length(which(n1_goalscored_vec_ht == "0"))
#   #awayteam
#   n1_goalscored_vec_at <- as.vector(n1_goalscored_h[n1_awayteamindex,])
#   n1_goalscored_vec_at[is.na(n1_goalscored_vec_at)] <- ""
#   n1_goalscored_vec_at <- n1_goalscored_vec_at[n1_goalscored_vec_at != ""]
#   n1_goalscored_vec_at  <-tail(n1_goalscored_vec_at,6)
#   n1_goalscored_vec_at  <- as.numeric(n1_goalscored_vec_at)
#   n1_at_totalgoalscored <- sum(n1_goalscored_vec_at)
#   n1_at_matches_scoring <- length(which(n1_goalscored_vec_at > 0))
#   n1_at_matches_without_scoring <- length(which(n1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   n1_goalconceded_vec_ht <- as.vector(n1_goalconceded_h[n1_hometeamindex,])
#   n1_goalconceded_vec_ht[is.na(n1_goalconceded_vec_ht)] <- ""
#   n1_goalconceded_vec_ht <- n1_goalconceded_vec_ht[n1_goalconceded_vec_ht != ""]
#   n1_goalconceded_vec_ht  <-tail(n1_goalconceded_vec_ht,6)
#   n1_goalconceded_vec_ht  <- as.numeric(n1_goalconceded_vec_ht)
#   n1_goalconceded_vec_ht
#   n1_ht_totalgoalconceded <- sum(n1_goalconceded_vec_ht)
#   n1_ht_matches_concede <- length(which(n1_goalconceded_vec_ht > 0))
#   n1_ht_matches_without_concede <- length(which(n1_goalconceded_vec_ht == "0"))
#   #awayteam
#   n1_goalconceded_vec_at <- as.vector(n1_goalconceded_h[n1_awayteamindex,])
#   n1_goalconceded_vec_at[is.na(n1_goalconceded_vec_at)] <- ""
#   n1_goalconceded_vec_at <- n1_goalconceded_vec_at[n1_goalconceded_vec_at != ""]
#   n1_goalconceded_vec_at  <-tail(n1_goalconceded_vec_at,6)
#   n1_goalconceded_vec_at  <- as.numeric(n1_goalconceded_vec_at)
#   n1_at_totalgoalconceded <- sum(n1_goalconceded_vec_at)
#   n1_at_matches_concede <- length(which(n1_goalconceded_vec_at > 0))
#   n1_at_matches_without_concede <- length(which(n1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   n1_totalgoals_vec_ht <- as.vector(n1_totalgoals_h[n1_hometeamindex,])
#   n1_totalgoals_vec_ht[is.na(n1_totalgoals_vec_ht)] <- ""
#   n1_totalgoals_vec_ht <- n1_totalgoals_vec_ht[n1_totalgoals_vec_ht != ""]
#   n1_totalgoals_vec_ht  <-tail(n1_totalgoals_vec_ht,6)
#   n1_totalgoals_vec_ht  <- as.numeric(n1_totalgoals_vec_ht)
#   n1_totalgoals_vec_ht
#   n1_ht_totalgoals <- sum(n1_totalgoals_vec_ht)
#   n1_ht_avgtotalgoals <- (n1_ht_totalgoals/6)
#   n1_ht_no_of_ov25 <- length(which(n1_totalgoals_vec_ht >= 3))
#   n1_ht_no_of_un25 <- length(which(n1_totalgoals_vec_ht <= 2))
#   #awayteam
#   n1_totalgoals_vec_at <- as.vector(n1_totalgoals_h[n1_awayteamindex,])
#   n1_totalgoals_vec_at[is.na(n1_totalgoals_vec_at)] <- ""
#   n1_totalgoals_vec_at <- n1_totalgoals_vec_at[n1_totalgoals_vec_at != ""]
#   n1_totalgoals_vec_at  <-tail(n1_totalgoals_vec_at,6)
#   n1_totalgoals_vec_at  <- as.numeric(n1_totalgoals_vec_at)
#   n1_totalgoals_vec_at
#   n1_at_totalgoals <- sum(n1_totalgoals_vec_at)
#   n1_at_avgtotalgoals <- (n1_at_totalgoals/6)
#   n1_at_no_of_ov25 <- length(which(n1_totalgoals_vec_at >= 3))
#   n1_at_no_of_un25 <- length(which(n1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   n1_winmargin_vec_ht <- as.vector(n1_winmargin_h[n1_hometeamindex,])
#   n1_winmargin_vec_ht[is.na(n1_winmargin_vec_ht)] <- ""
#   n1_winmargin_vec_ht <- n1_winmargin_vec_ht[n1_winmargin_vec_ht != ""]
#   n1_winmargin_vec_ht  <-tail(n1_winmargin_vec_ht,6)
#   n1_winmargin_vec_ht  <- as.numeric(n1_winmargin_vec_ht)
#
#   n1_ht_totalwinmargin <- sum(n1_winmargin_vec_ht)
#   n1_ht_no_of_winmargin_ov0 <- length(which(n1_winmargin_vec_ht >= 0))
#   n1_ht_no_of_winmargin_ov1 <- length(which(n1_winmargin_vec_ht >= 1))
#   n1_ht_no_of_winmargin_un0 <- length(which(n1_winmargin_vec_ht <= 0))
#   n1_ht_no_of_winmargin_un1 <- length(which(n1_winmargin_vec_ht <= 1))
#   #awayteam
#   n1_winmargin_vec_at <- as.vector(n1_winmargin_h[n1_awayteamindex,])
#   n1_winmargin_vec_at[is.na(n1_winmargin_vec_at)] <- ""
#   n1_winmargin_vec_at <- n1_winmargin_vec_at[n1_winmargin_vec_at != ""]
#   n1_winmargin_vec_at  <-tail(n1_winmargin_vec_at,6)
#   n1_winmargin_vec_at  <- as.numeric(n1_winmargin_vec_at)
#
#   n1_at_totalwinmargin <- sum(n1_winmargin_vec_at)
#   n1_at_no_of_winmargin_ov0 <- length(which(n1_winmargin_vec_at >= 0))
#   n1_at_no_of_winmargin_ov1 <- length(which(n1_winmargin_vec_at >= 1))
#   n1_at_no_of_winmargin_un0 <- length(which(n1_winmargin_vec_at <= 0))
#   n1_at_no_of_winmargin_un1 <- length(which(n1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   n1_winmargin_vec_ht_lm <- as.vector(n1_winmargin_h[n1_hometeamindex,])
#   n1_winmargin_vec_ht_lm[is.na(n1_winmargin_vec_ht_lm)] <- ""
#   n1_winmargin_vec_ht_lm <- n1_winmargin_vec_ht_lm[n1_winmargin_vec_ht_lm != ""]
#   n1_winmargin_vec_ht_lm  <-tail(n1_winmargin_vec_ht_lm,1)
#   #awayteam
#   n1_winmargin_vec_at_lm <- as.vector(n1_winmargin_h[n1_awayteamindex,])
#   n1_winmargin_vec_at_lm[is.na(n1_winmargin_vec_at_lm)] <- ""
#   n1_winmargin_vec_at_lm <- n1_winmargin_vec_at_lm[n1_winmargin_vec_at_lm != ""]
#   n1_winmargin_vec_at_lm  <-tail(n1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   n1_yellowtotals_vec_ht <- as.vector(n1_yellowtotalsv2[n1_hometeamindex,])
#   n1_yellowtotals_vec_ht[is.na(n1_yellowtotals_vec_ht)] <- ""
#   n1_yellowtotals_vec_ht <- n1_yellowtotals_vec_ht[n1_yellowtotals_vec_ht != ""]
#   n1_yellowtotals_vec_ht  <-tail(n1_yellowtotals_vec_ht,1)
#   #awayteam
#   n1_yellowtotals_vec_at <- as.vector(n1_yellowtotalsv2[n1_awayteamindex,])
#   n1_yellowtotals_vec_at[is.na(n1_yellowtotals_vec_at)] <- ""
#   n1_yellowtotals_vec_at <- n1_yellowtotals_vec_at[n1_yellowtotals_vec_at != ""]
#   n1_yellowtotals_vec_at  <-tail(n1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   n1_cornertotals_vec_ht <- as.vector(n1_cornertotalsv2[n1_hometeamindex,])
#   n1_cornertotals_vec_ht[is.na(n1_cornertotals_vec_ht)] <- ""
#   n1_cornertotals_vec_ht <- n1_cornertotals_vec_ht[n1_cornertotals_vec_ht != ""]
#   n1_cornertotals_vec_ht  <-tail(n1_cornertotals_vec_ht,1)
#   #awayteam
#   n1_cornertotals_vec_at <- as.vector(n1_cornertotalsv2[n1_awayteamindex,])
#   n1_cornertotals_vec_at[is.na(n1_cornertotals_vec_at)] <- ""
#   n1_cornertotals_vec_at <- n1_cornertotals_vec_at[n1_cornertotals_vec_at != ""]
#   n1_cornertotals_vec_at  <-tail(n1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   n1_xshotsconversion_vec_ht <- as.vector(n1_shots_analysis[n1_hometeamindex,])
#   n1_xshotsconversion_vec_ht[is.na(n1_xshotsconversion_vec_ht)] <- ""
#   n1_xshotsconversion_vec_ht <- n1_xshotsconversion_vec_ht[n1_xshotsconversion_vec_ht != ""]
#   n1_xshotsconversion_vec_ht  <-tail(n1_xshotsconversion_vec_ht,1)
#   #awayteam
#   n1_xshotsconversion_vec_at <- as.vector(n1_shots_analysis[n1_awayteamindex,])
#   n1_xshotsconversion_vec_at[is.na(n1_xshotsconversion_vec_at)] <- ""
#   n1_xshotsconversion_vec_at <- n1_xshotsconversion_vec_at[n1_xshotsconversion_vec_at != ""]
#   n1_xshotsconversion_vec_at  <-tail(n1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   n1_fouls_conversion_vec_ht <- as.vector(n1_fouls_conversion[n1_hometeamindex,])
#   n1_fouls_conversion_vec_ht[is.na(n1_fouls_conversion_vec_ht)] <- ""
#   n1_fouls_conversion_vec_ht <- n1_fouls_conversion_vec_ht[n1_fouls_conversion_vec_ht != ""]
#   n1_fouls_conversion_vec_ht  <-tail(n1_fouls_conversion_vec_ht,1)
#   #awayteam
#   n1_fouls_conversion_vec_at <- as.vector(n1_fouls_conversion[n1_awayteamindex,])
#   n1_fouls_conversion_vec_at[is.na(n1_fouls_conversion_vec_at)] <- ""
#   n1_fouls_conversion_vec_at <- n1_fouls_conversion_vec_at[n1_fouls_conversion_vec_at != ""]
#   n1_fouls_conversion_vec_at  <-tail(n1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   n1_ht_last6points <- n1_ht_numberof_wins*3 + n1_ht_numberof_draws*1
#   n1_at_last6points <- n1_at_numberof_wins*3 + n1_at_numberof_draws*1
#
#   if(n1_ht_last6points > n1_at_last6points) {n1_3waypick <- "1"}  else {n1_3waypick <- "X2"}
#
#   if(n1_at_last6points > n1_ht_last6points ) {n1_3waypick <- "2"} else {n1_3waypick <- "1X"}
#
#   if(n1_ht_no_of_ov25 + n1_at_no_of_ov25 >= 6) {n1_goalspick <- "ov25"} else {n1_goalspick <- "un25"}
#
#   if(n1_ht_no_of_un25 + n1_at_no_of_un25 >= 6) {n1_goalspick <- "un25"} else {n1_goalspick <- "ov25"}
#
#   if(n1_ht_matches_scoring >= 4 && n1_at_matches_scoring >=4) {n1_btts <- "BTTS-Y"} else {n1_btts <- "BTTS-N"}
#
#
#   n1_prediction[n1_row] <- rbind(paste(n1_3waypick,n1_goalspick,n1_btts,sep = ","))
#   n1_HWM[n1_row] <- n1_ht_totalwinmargin
#   n1_AWM[n1_row] <- n1_at_totalwinmargin
#
#   n1_HWMLM[n1_row] <- n1_winmargin_vec_ht_lm
#   n1_AWMLM[n1_row] <- n1_winmargin_vec_at_lm
#
#   n1_HY[n1_row] <- n1_yellowtotals_vec_ht
#   n1_AY[n1_row] <- n1_yellowtotals_vec_at
#
#   n1_HCO[n1_row] <- n1_cornertotals_vec_ht
#   n1_ACO[n1_row] <- n1_cornertotals_vec_at
#
#   n1_HXSC[n1_row] <- n1_xshotsconversion_vec_ht
#   n1_AXSC[n1_row] <- n1_xshotsconversion_vec_at
#
#   n1_HYCPF[n1_row] <- n1_fouls_conversion_vec_ht
#   n1_AYCPF[n1_row] <- n1_fouls_conversion_vec_at
# }
#
# n1_prediction <- as.data.frame(n1_prediction)
# colnames(n1_prediction) <- "prediction"
#
# n1_HWM <- as.data.frame(n1_HWM)
# colnames(n1_HWM) <- "HWM"
#
# n1_AWM <- as.data.frame(n1_AWM)
# colnames(n1_AWM) <- "AWM"
#
# n1_HWMLM <- as.data.frame(n1_HWMLM)
# colnames(n1_HWMLM) <- "HWMLM"
#
# n1_AWMLM <- as.data.frame(n1_AWMLM)
# colnames(n1_AWMLM) <- "AWMLM"
#
# n1_HY <- as.data.frame(n1_HY)
# colnames(n1_HY) <- "AVGHY"
#
# n1_AY <- as.data.frame(n1_AY)
# colnames(n1_AY) <- "AVGAY"
#
# n1_HCO <- as.data.frame(n1_HCO)
# colnames(n1_HCO) <- "AVGHCO"
#
# n1_ACO <- as.data.frame(n1_ACO)
# colnames(n1_ACO) <- "AVGACO"
#
# n1_HXSC <- as.data.frame(n1_HXSC)
# colnames(n1_HXSC) <- "HXSC"
#
# n1_AXSC <- as.data.frame(n1_AXSC)
# colnames(n1_AXSC) <- "AXSC"
#
# n1_HYCPF <- as.data.frame(n1_HYCPF)
# colnames(n1_HYCPF) <- "HYCPF"
#
# n1_AYCPF <- as.data.frame(n1_AYCPF)
# colnames(n1_AYCPF) <- "AYCPF"
#
# n1_picks <- cbind(N1_fixtures$Div,N1_fixtures$HomeTeam_n1,N1_fixtures$AwayTeam_n1,n1_prediction,n1_HWM,n1_AWM,n1_HWMLM,n1_AWMLM,n1_HY,n1_AY,n1_HCO,n1_ACO,n1_HXSC,n1_AXSC,n1_HYCPF,n1_AYCPF)
#
# colnames(n1_picks)[1] <- "picks_Div"
# colnames(n1_picks)[2] <- "picks_HomeTeam"
# colnames(n1_picks)[3] <- "picks_AwayTeam"
# n1_picks$matchid <- paste(n1_picks$picks_HomeTeam,n1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of N1
# n1_picks
# #P1
# P1_fixtures$Hometeam_p1_index <- match(P1_fixtures$HomeTeam_p1,p1_teams)
# P1_fixtures$Awayteam_p1_index <- match(P1_fixtures$AwayTeam_p1,p1_teams)
# p1_prediction <- c()
# p1_HWM <- c()
# p1_AWM <- c()
# p1_HWMLM <- c()
# p1_AWMLM <- c()
# p1_HY <- c()
# p1_AY <- c()
# p1_HCO <- c()
# p1_ACO <- c()
# p1_HXSC <- c()
# p1_AXSC <- c()
# p1_HYCPF <- c()
# p1_AYCPF <- c()
# for(p1_row in 1:nrow(P1_fixtures))
# {
#
#   p1_hometeamindex <- P1_fixtures[p1_row,"Hometeam_p1_index"]
#   p1_awayteamindex <- P1_fixtures[p1_row,"Awayteam_p1_index"]
#   #analyse team form
#   #home team
#   p1_form_vec_ht <- as.vector(p1_form_h[p1_hometeamindex,])
#   p1_form_vec_ht[is.na(p1_form_vec_ht)] <- ""
#   p1_form_vec_ht <- p1_form_vec_ht[p1_form_vec_ht != ""]
#   p1_form_vec_ht  <-tail(p1_form_vec_ht,6)
#   p1_ht_numberof_wins <- length(which(p1_form_vec_ht == "W"))
#   p1_ht_numberof_draws <- length(which(p1_form_vec_ht == "D"))
#   p1_ht_numberof_loss <- length(which(p1_form_vec_ht == "L"))
#   #awayteam
#   p1_form_vec_at <- as.vector(p1_form_h[p1_awayteamindex,])
#   p1_form_vec_at[is.na(p1_form_vec_at)] <- ""
#   p1_form_vec_at <- p1_form_vec_at[p1_form_vec_at != ""]
#   p1_form_vec_at  <-tail(p1_form_vec_at,6)
#   p1_at_numberof_wins <- length(which(p1_form_vec_at == "W"))
#   p1_at_numberof_draws <- length(which(p1_form_vec_at == "D"))
#   p1_at_numberof_loss <- length(which(p1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   p1_goalscored_vec_ht <- as.vector(p1_goalscored_h[p1_hometeamindex,])
#   p1_goalscored_vec_ht[is.na(p1_goalscored_vec_ht)] <- ""
#   p1_goalscored_vec_ht <- p1_goalscored_vec_ht[p1_goalscored_vec_ht != ""]
#   p1_goalscored_vec_ht  <-tail(p1_goalscored_vec_ht,6)
#   p1_goalscored_vec_ht  <- as.numeric(p1_goalscored_vec_ht)
#   p1_ht_totalgoalscored <- sum(p1_goalscored_vec_ht)
#   p1_ht_matches_scoring <- length(which(p1_goalscored_vec_ht > 0))
#   p1_ht_matches_without_scoring <- length(which(p1_goalscored_vec_ht == "0"))
#   #awayteam
#   p1_goalscored_vec_at <- as.vector(p1_goalscored_h[p1_awayteamindex,])
#   p1_goalscored_vec_at[is.na(p1_goalscored_vec_at)] <- ""
#   p1_goalscored_vec_at <- p1_goalscored_vec_at[p1_goalscored_vec_at != ""]
#   p1_goalscored_vec_at  <-tail(p1_goalscored_vec_at,6)
#   p1_goalscored_vec_at  <- as.numeric(p1_goalscored_vec_at)
#   p1_at_totalgoalscored <- sum(p1_goalscored_vec_at)
#   p1_at_matches_scoring <- length(which(p1_goalscored_vec_at > 0))
#   p1_at_matches_without_scoring <- length(which(p1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   p1_goalconceded_vec_ht <- as.vector(p1_goalconceded_h[p1_hometeamindex,])
#   p1_goalconceded_vec_ht[is.na(p1_goalconceded_vec_ht)] <- ""
#   p1_goalconceded_vec_ht <- p1_goalconceded_vec_ht[p1_goalconceded_vec_ht != ""]
#   p1_goalconceded_vec_ht  <-tail(p1_goalconceded_vec_ht,6)
#   p1_goalconceded_vec_ht  <- as.numeric(p1_goalconceded_vec_ht)
#   p1_goalconceded_vec_ht
#   p1_ht_totalgoalconceded <- sum(p1_goalconceded_vec_ht)
#   p1_ht_matches_concede <- length(which(p1_goalconceded_vec_ht > 0))
#   p1_ht_matches_without_concede <- length(which(p1_goalconceded_vec_ht == "0"))
#   #awayteam
#   p1_goalconceded_vec_at <- as.vector(p1_goalconceded_h[p1_awayteamindex,])
#   p1_goalconceded_vec_at[is.na(p1_goalconceded_vec_at)] <- ""
#   p1_goalconceded_vec_at <- p1_goalconceded_vec_at[p1_goalconceded_vec_at != ""]
#   p1_goalconceded_vec_at  <-tail(p1_goalconceded_vec_at,6)
#   p1_goalconceded_vec_at  <- as.numeric(p1_goalconceded_vec_at)
#   p1_at_totalgoalconceded <- sum(p1_goalconceded_vec_at)
#   p1_at_matches_concede <- length(which(p1_goalconceded_vec_at > 0))
#   p1_at_matches_without_concede <- length(which(p1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   p1_totalgoals_vec_ht <- as.vector(p1_totalgoals_h[p1_hometeamindex,])
#   p1_totalgoals_vec_ht[is.na(p1_totalgoals_vec_ht)] <- ""
#   p1_totalgoals_vec_ht <- p1_totalgoals_vec_ht[p1_totalgoals_vec_ht != ""]
#   p1_totalgoals_vec_ht  <-tail(p1_totalgoals_vec_ht,6)
#   p1_totalgoals_vec_ht  <- as.numeric(p1_totalgoals_vec_ht)
#   p1_totalgoals_vec_ht
#   p1_ht_totalgoals <- sum(p1_totalgoals_vec_ht)
#   p1_ht_avgtotalgoals <- (p1_ht_totalgoals/6)
#   p1_ht_no_of_ov25 <- length(which(p1_totalgoals_vec_ht >= 3))
#   p1_ht_no_of_un25 <- length(which(p1_totalgoals_vec_ht <= 2))
#   #awayteam
#   p1_totalgoals_vec_at <- as.vector(p1_totalgoals_h[p1_awayteamindex,])
#   p1_totalgoals_vec_at[is.na(p1_totalgoals_vec_at)] <- ""
#   p1_totalgoals_vec_at <- p1_totalgoals_vec_at[p1_totalgoals_vec_at != ""]
#   p1_totalgoals_vec_at  <-tail(p1_totalgoals_vec_at,6)
#   p1_totalgoals_vec_at  <- as.numeric(p1_totalgoals_vec_at)
#   p1_totalgoals_vec_at
#   p1_at_totalgoals <- sum(p1_totalgoals_vec_at)
#   p1_at_avgtotalgoals <- (p1_at_totalgoals/6)
#   p1_at_no_of_ov25 <- length(which(p1_totalgoals_vec_at >= 3))
#   p1_at_no_of_un25 <- length(which(p1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   p1_winmargin_vec_ht <- as.vector(p1_winmargin_h[p1_hometeamindex,])
#   p1_winmargin_vec_ht[is.na(p1_winmargin_vec_ht)] <- ""
#   p1_winmargin_vec_ht <- p1_winmargin_vec_ht[p1_winmargin_vec_ht != ""]
#   p1_winmargin_vec_ht  <-tail(p1_winmargin_vec_ht,6)
#   p1_winmargin_vec_ht  <- as.numeric(p1_winmargin_vec_ht)
#
#   p1_ht_totalwinmargin <- sum(p1_winmargin_vec_ht)
#   p1_ht_no_of_winmargin_ov0 <- length(which(p1_winmargin_vec_ht >= 0))
#   p1_ht_no_of_winmargin_ov1 <- length(which(p1_winmargin_vec_ht >= 1))
#   p1_ht_no_of_winmargin_un0 <- length(which(p1_winmargin_vec_ht <= 0))
#   p1_ht_no_of_winmargin_un1 <- length(which(p1_winmargin_vec_ht <= 1))
#   #awayteam
#   p1_winmargin_vec_at <- as.vector(p1_winmargin_h[p1_awayteamindex,])
#   p1_winmargin_vec_at[is.na(p1_winmargin_vec_at)] <- ""
#   p1_winmargin_vec_at <- p1_winmargin_vec_at[p1_winmargin_vec_at != ""]
#   p1_winmargin_vec_at  <-tail(p1_winmargin_vec_at,6)
#   p1_winmargin_vec_at  <- as.numeric(p1_winmargin_vec_at)
#
#   p1_at_totalwinmargin <- sum(p1_winmargin_vec_at)
#   p1_at_no_of_winmargin_ov0 <- length(which(p1_winmargin_vec_at >= 0))
#   p1_at_no_of_winmargin_ov1 <- length(which(p1_winmargin_vec_at >= 1))
#   p1_at_no_of_winmargin_un0 <- length(which(p1_winmargin_vec_at <= 0))
#   p1_at_no_of_winmargin_un1 <- length(which(p1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   p1_winmargin_vec_ht_lm <- as.vector(p1_winmargin_h[p1_hometeamindex,])
#   p1_winmargin_vec_ht_lm[is.na(p1_winmargin_vec_ht_lm)] <- ""
#   p1_winmargin_vec_ht_lm <- p1_winmargin_vec_ht_lm[p1_winmargin_vec_ht_lm != ""]
#   p1_winmargin_vec_ht_lm  <-tail(p1_winmargin_vec_ht_lm,1)
#   #awayteam
#   p1_winmargin_vec_at_lm <- as.vector(p1_winmargin_h[p1_awayteamindex,])
#   p1_winmargin_vec_at_lm[is.na(p1_winmargin_vec_at_lm)] <- ""
#   p1_winmargin_vec_at_lm <- p1_winmargin_vec_at_lm[p1_winmargin_vec_at_lm != ""]
#   p1_winmargin_vec_at_lm  <-tail(p1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   p1_yellowtotals_vec_ht <- as.vector(p1_yellowtotalsv2[p1_hometeamindex,])
#   p1_yellowtotals_vec_ht[is.na(p1_yellowtotals_vec_ht)] <- ""
#   p1_yellowtotals_vec_ht <- p1_yellowtotals_vec_ht[p1_yellowtotals_vec_ht != ""]
#   p1_yellowtotals_vec_ht  <-tail(p1_yellowtotals_vec_ht,1)
#   #awayteam
#   p1_yellowtotals_vec_at <- as.vector(p1_yellowtotalsv2[p1_awayteamindex,])
#   p1_yellowtotals_vec_at[is.na(p1_yellowtotals_vec_at)] <- ""
#   p1_yellowtotals_vec_at <- p1_yellowtotals_vec_at[p1_yellowtotals_vec_at != ""]
#   p1_yellowtotals_vec_at  <-tail(p1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   p1_cornertotals_vec_ht <- as.vector(p1_cornertotalsv2[p1_hometeamindex,])
#   p1_cornertotals_vec_ht[is.na(p1_cornertotals_vec_ht)] <- ""
#   p1_cornertotals_vec_ht <- p1_cornertotals_vec_ht[p1_cornertotals_vec_ht != ""]
#   p1_cornertotals_vec_ht  <-tail(p1_cornertotals_vec_ht,1)
#   #awayteam
#   p1_cornertotals_vec_at <- as.vector(p1_cornertotalsv2[p1_awayteamindex,])
#   p1_cornertotals_vec_at[is.na(p1_cornertotals_vec_at)] <- ""
#   p1_cornertotals_vec_at <- p1_cornertotals_vec_at[p1_cornertotals_vec_at != ""]
#   p1_cornertotals_vec_at  <-tail(p1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   p1_xshotsconversion_vec_ht <- as.vector(p1_shots_analysis[p1_hometeamindex,])
#   p1_xshotsconversion_vec_ht[is.na(p1_xshotsconversion_vec_ht)] <- ""
#   p1_xshotsconversion_vec_ht <- p1_xshotsconversion_vec_ht[p1_xshotsconversion_vec_ht != ""]
#   p1_xshotsconversion_vec_ht  <-tail(p1_xshotsconversion_vec_ht,1)
#   #awayteam
#   p1_xshotsconversion_vec_at <- as.vector(p1_shots_analysis[p1_awayteamindex,])
#   p1_xshotsconversion_vec_at[is.na(p1_xshotsconversion_vec_at)] <- ""
#   p1_xshotsconversion_vec_at <- p1_xshotsconversion_vec_at[p1_xshotsconversion_vec_at != ""]
#   p1_xshotsconversion_vec_at  <-tail(p1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   p1_fouls_conversion_vec_ht <- as.vector(p1_fouls_conversion[p1_hometeamindex,])
#   p1_fouls_conversion_vec_ht[is.na(p1_fouls_conversion_vec_ht)] <- ""
#   p1_fouls_conversion_vec_ht <- p1_fouls_conversion_vec_ht[p1_fouls_conversion_vec_ht != ""]
#   p1_fouls_conversion_vec_ht  <-tail(p1_fouls_conversion_vec_ht,1)
#   #awayteam
#   p1_fouls_conversion_vec_at <- as.vector(p1_fouls_conversion[p1_awayteamindex,])
#   p1_fouls_conversion_vec_at[is.na(p1_fouls_conversion_vec_at)] <- ""
#   p1_fouls_conversion_vec_at <- p1_fouls_conversion_vec_at[p1_fouls_conversion_vec_at != ""]
#   p1_fouls_conversion_vec_at  <-tail(p1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   p1_ht_last6points <- p1_ht_numberof_wins*3 + p1_ht_numberof_draws*1
#   p1_at_last6points <- p1_at_numberof_wins*3 + p1_at_numberof_draws*1
#
#   if(p1_ht_last6points > p1_at_last6points) {p1_3waypick <- "1"}  else {p1_3waypick <- "X2"}
#
#   if(p1_at_last6points > p1_ht_last6points ) {p1_3waypick <- "2"} else {p1_3waypick <- "1X"}
#
#   if(p1_ht_no_of_ov25 + p1_at_no_of_ov25 >= 6) {p1_goalspick <- "ov25"} else {p1_goalspick <- "un25"}
#
#   if(p1_ht_no_of_un25 + p1_at_no_of_un25 >= 6) {p1_goalspick <- "un25"} else {p1_goalspick <- "ov25"}
#
#   if(p1_ht_matches_scoring >= 4 && p1_at_matches_scoring >=4) {p1_btts <- "BTTS-Y"} else {p1_btts <- "BTTS-N"}
#
#
#   p1_prediction[p1_row] <- rbind(paste(p1_3waypick,p1_goalspick,p1_btts,sep = ","))
#   p1_HWM[p1_row] <- p1_ht_totalwinmargin
#   p1_AWM[p1_row] <- p1_at_totalwinmargin
#
#   p1_HWMLM[p1_row] <- p1_winmargin_vec_ht_lm
#   p1_AWMLM[p1_row] <- p1_winmargin_vec_at_lm
#
#   p1_HY[p1_row] <- p1_yellowtotals_vec_ht
#   p1_AY[p1_row] <- p1_yellowtotals_vec_at
#
#   p1_HCO[p1_row] <- p1_cornertotals_vec_ht
#   p1_ACO[p1_row] <- p1_cornertotals_vec_at
#
#   p1_HXSC[p1_row] <- p1_xshotsconversion_vec_ht
#   p1_AXSC[p1_row] <- p1_xshotsconversion_vec_at
#
#   p1_HYCPF[p1_row] <- p1_fouls_conversion_vec_ht
#   p1_AYCPF[p1_row] <- p1_fouls_conversion_vec_at
# }
#
# p1_prediction <- as.data.frame(p1_prediction)
# colnames(p1_prediction) <- "prediction"
#
# p1_HWM <- as.data.frame(p1_HWM)
# colnames(p1_HWM) <- "HWM"
#
# p1_AWM <- as.data.frame(p1_AWM)
# colnames(p1_AWM) <- "AWM"
#
# p1_HWMLM <- as.data.frame(p1_HWMLM)
# colnames(p1_HWMLM) <- "HWMLM"
#
# p1_AWMLM <- as.data.frame(p1_AWMLM)
# colnames(p1_AWMLM) <- "AWMLM"
#
# p1_HY <- as.data.frame(p1_HY)
# colnames(p1_HY) <- "AVGHY"
#
# p1_AY <- as.data.frame(p1_AY)
# colnames(p1_AY) <- "AVGAY"
#
# p1_HCO <- as.data.frame(p1_HCO)
# colnames(p1_HCO) <- "AVGHCO"
#
# p1_ACO <- as.data.frame(p1_ACO)
# colnames(p1_ACO) <- "AVGACO"
#
# p1_HXSC <- as.data.frame(p1_HXSC)
# colnames(p1_HXSC) <- "HXSC"
#
# p1_AXSC <- as.data.frame(p1_AXSC)
# colnames(p1_AXSC) <- "AXSC"
#
# p1_HYCPF <- as.data.frame(p1_HYCPF)
# colnames(p1_HYCPF) <- "HYCPF"
#
# p1_AYCPF <- as.data.frame(p1_AYCPF)
# colnames(p1_AYCPF) <- "AYCPF"
#
# p1_picks <- cbind(P1_fixtures$Div,P1_fixtures$HomeTeam_p1,P1_fixtures$AwayTeam_p1,p1_prediction,p1_HWM,p1_AWM,p1_HWMLM,p1_AWMLM,p1_HY,p1_AY,p1_HCO,p1_ACO,p1_HXSC,p1_AXSC,p1_HYCPF,p1_AYCPF)
#
# colnames(p1_picks)[1] <- "picks_Div"
# colnames(p1_picks)[2] <- "picks_HomeTeam"
# colnames(p1_picks)[3] <- "picks_AwayTeam"
# p1_picks$matchid <- paste(p1_picks$picks_HomeTeam,p1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of P1
# p1_picks
# ############################################################################################
# #SP1
# SP1_fixtures$Hometeam_sp1_index <- match(SP1_fixtures$HomeTeam_sp1,sp1_teams)
# SP1_fixtures$Awayteam_sp1_index <- match(SP1_fixtures$AwayTeam_sp1,sp1_teams)
# sp1_prediction <- c()
# sp1_HWM <- c()
# sp1_AWM <- c()
# sp1_HWMLM <- c()
# sp1_AWMLM <- c()
# sp1_HY <- c()
# sp1_AY <- c()
# sp1_HCO <- c()
# sp1_ACO <- c()
# sp1_HXSC <- c()
# sp1_AXSC <- c()
# sp1_HYCPF <- c()
# sp1_AYCPF <- c()
# for(sp1_row in 1:nrow(SP1_fixtures))
# {
#
#   sp1_hometeamindex <- SP1_fixtures[sp1_row,"Hometeam_sp1_index"]
#   sp1_awayteamindex <- SP1_fixtures[sp1_row,"Awayteam_sp1_index"]
#   #analyse team form
#   #home team
#   sp1_form_vec_ht <- as.vector(sp1_form_h[sp1_hometeamindex,])
#   sp1_form_vec_ht[is.na(sp1_form_vec_ht)] <- ""
#   sp1_form_vec_ht <- sp1_form_vec_ht[sp1_form_vec_ht != ""]
#   sp1_form_vec_ht  <-tail(sp1_form_vec_ht,6)
#   sp1_ht_numberof_wins <- length(which(sp1_form_vec_ht == "W"))
#   sp1_ht_numberof_draws <- length(which(sp1_form_vec_ht == "D"))
#   sp1_ht_numberof_loss <- length(which(sp1_form_vec_ht == "L"))
#   #awayteam
#   sp1_form_vec_at <- as.vector(sp1_form_h[sp1_awayteamindex,])
#   sp1_form_vec_at[is.na(sp1_form_vec_at)] <- ""
#   sp1_form_vec_at <- sp1_form_vec_at[sp1_form_vec_at != ""]
#   sp1_form_vec_at  <-tail(sp1_form_vec_at,6)
#   sp1_at_numberof_wins <- length(which(sp1_form_vec_at == "W"))
#   sp1_at_numberof_draws <- length(which(sp1_form_vec_at == "D"))
#   sp1_at_numberof_loss <- length(which(sp1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sp1_goalscored_vec_ht <- as.vector(sp1_goalscored_h[sp1_hometeamindex,])
#   sp1_goalscored_vec_ht[is.na(sp1_goalscored_vec_ht)] <- ""
#   sp1_goalscored_vec_ht <- sp1_goalscored_vec_ht[sp1_goalscored_vec_ht != ""]
#   sp1_goalscored_vec_ht  <-tail(sp1_goalscored_vec_ht,6)
#   sp1_goalscored_vec_ht  <- as.numeric(sp1_goalscored_vec_ht)
#   sp1_ht_totalgoalscored <- sum(sp1_goalscored_vec_ht)
#   sp1_ht_matches_scoring <- length(which(sp1_goalscored_vec_ht > 0))
#   sp1_ht_matches_without_scoring <- length(which(sp1_goalscored_vec_ht == "0"))
#   #awayteam
#   sp1_goalscored_vec_at <- as.vector(sp1_goalscored_h[sp1_awayteamindex,])
#   sp1_goalscored_vec_at[is.na(sp1_goalscored_vec_at)] <- ""
#   sp1_goalscored_vec_at <- sp1_goalscored_vec_at[sp1_goalscored_vec_at != ""]
#   sp1_goalscored_vec_at  <-tail(sp1_goalscored_vec_at,6)
#   sp1_goalscored_vec_at  <- as.numeric(sp1_goalscored_vec_at)
#   sp1_at_totalgoalscored <- sum(sp1_goalscored_vec_at)
#   sp1_at_matches_scoring <- length(which(sp1_goalscored_vec_at > 0))
#   sp1_at_matches_without_scoring <- length(which(sp1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sp1_goalconceded_vec_ht <- as.vector(sp1_goalconceded_h[sp1_hometeamindex,])
#   sp1_goalconceded_vec_ht[is.na(sp1_goalconceded_vec_ht)] <- ""
#   sp1_goalconceded_vec_ht <- sp1_goalconceded_vec_ht[sp1_goalconceded_vec_ht != ""]
#   sp1_goalconceded_vec_ht  <-tail(sp1_goalconceded_vec_ht,6)
#   sp1_goalconceded_vec_ht  <- as.numeric(sp1_goalconceded_vec_ht)
#   sp1_goalconceded_vec_ht
#   sp1_ht_totalgoalconceded <- sum(sp1_goalconceded_vec_ht)
#   sp1_ht_matches_concede <- length(which(sp1_goalconceded_vec_ht > 0))
#   sp1_ht_matches_without_concede <- length(which(sp1_goalconceded_vec_ht == "0"))
#   #awayteam
#   sp1_goalconceded_vec_at <- as.vector(sp1_goalconceded_h[sp1_awayteamindex,])
#   sp1_goalconceded_vec_at[is.na(sp1_goalconceded_vec_at)] <- ""
#   sp1_goalconceded_vec_at <- sp1_goalconceded_vec_at[sp1_goalconceded_vec_at != ""]
#   sp1_goalconceded_vec_at  <-tail(sp1_goalconceded_vec_at,6)
#   sp1_goalconceded_vec_at  <- as.numeric(sp1_goalconceded_vec_at)
#   sp1_at_totalgoalconceded <- sum(sp1_goalconceded_vec_at)
#   sp1_at_matches_concede <- length(which(sp1_goalconceded_vec_at > 0))
#   sp1_at_matches_without_concede <- length(which(sp1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sp1_totalgoals_vec_ht <- as.vector(sp1_totalgoals_h[sp1_hometeamindex,])
#   sp1_totalgoals_vec_ht[is.na(sp1_totalgoals_vec_ht)] <- ""
#   sp1_totalgoals_vec_ht <- sp1_totalgoals_vec_ht[sp1_totalgoals_vec_ht != ""]
#   sp1_totalgoals_vec_ht  <-tail(sp1_totalgoals_vec_ht,6)
#   sp1_totalgoals_vec_ht  <- as.numeric(sp1_totalgoals_vec_ht)
#   sp1_totalgoals_vec_ht
#   sp1_ht_totalgoals <- sum(sp1_totalgoals_vec_ht)
#   sp1_ht_avgtotalgoals <- (sp1_ht_totalgoals/6)
#   sp1_ht_no_of_ov25 <- length(which(sp1_totalgoals_vec_ht >= 3))
#   sp1_ht_no_of_un25 <- length(which(sp1_totalgoals_vec_ht <= 2))
#   #awayteam
#   sp1_totalgoals_vec_at <- as.vector(sp1_totalgoals_h[sp1_awayteamindex,])
#   sp1_totalgoals_vec_at[is.na(sp1_totalgoals_vec_at)] <- ""
#   sp1_totalgoals_vec_at <- sp1_totalgoals_vec_at[sp1_totalgoals_vec_at != ""]
#   sp1_totalgoals_vec_at  <-tail(sp1_totalgoals_vec_at,6)
#   sp1_totalgoals_vec_at  <- as.numeric(sp1_totalgoals_vec_at)
#   sp1_totalgoals_vec_at
#   sp1_at_totalgoals <- sum(sp1_totalgoals_vec_at)
#   sp1_at_avgtotalgoals <- (sp1_at_totalgoals/6)
#   sp1_at_no_of_ov25 <- length(which(sp1_totalgoals_vec_at >= 3))
#   sp1_at_no_of_un25 <- length(which(sp1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sp1_winmargin_vec_ht <- as.vector(sp1_winmargin_h[sp1_hometeamindex,])
#   sp1_winmargin_vec_ht[is.na(sp1_winmargin_vec_ht)] <- ""
#   sp1_winmargin_vec_ht <- sp1_winmargin_vec_ht[sp1_winmargin_vec_ht != ""]
#   sp1_winmargin_vec_ht  <-tail(sp1_winmargin_vec_ht,6)
#   sp1_winmargin_vec_ht  <- as.numeric(sp1_winmargin_vec_ht)
#
#   sp1_ht_totalwinmargin <- sum(sp1_winmargin_vec_ht)
#   sp1_ht_no_of_winmargin_ov0 <- length(which(sp1_winmargin_vec_ht >= 0))
#   sp1_ht_no_of_winmargin_ov1 <- length(which(sp1_winmargin_vec_ht >= 1))
#   sp1_ht_no_of_winmargin_un0 <- length(which(sp1_winmargin_vec_ht <= 0))
#   sp1_ht_no_of_winmargin_un1 <- length(which(sp1_winmargin_vec_ht <= 1))
#   #awayteam
#   sp1_winmargin_vec_at <- as.vector(sp1_winmargin_h[sp1_awayteamindex,])
#   sp1_winmargin_vec_at[is.na(sp1_winmargin_vec_at)] <- ""
#   sp1_winmargin_vec_at <- sp1_winmargin_vec_at[sp1_winmargin_vec_at != ""]
#   sp1_winmargin_vec_at  <-tail(sp1_winmargin_vec_at,6)
#   sp1_winmargin_vec_at  <- as.numeric(sp1_winmargin_vec_at)
#
#   sp1_at_totalwinmargin <- sum(sp1_winmargin_vec_at)
#   sp1_at_no_of_winmargin_ov0 <- length(which(sp1_winmargin_vec_at >= 0))
#   sp1_at_no_of_winmargin_ov1 <- length(which(sp1_winmargin_vec_at >= 1))
#   sp1_at_no_of_winmargin_un0 <- length(which(sp1_winmargin_vec_at <= 0))
#   sp1_at_no_of_winmargin_un1 <- length(which(sp1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sp1_winmargin_vec_ht_lm <- as.vector(sp1_winmargin_h[sp1_hometeamindex,])
#   sp1_winmargin_vec_ht_lm[is.na(sp1_winmargin_vec_ht_lm)] <- ""
#   sp1_winmargin_vec_ht_lm <- sp1_winmargin_vec_ht_lm[sp1_winmargin_vec_ht_lm != ""]
#   sp1_winmargin_vec_ht_lm  <-tail(sp1_winmargin_vec_ht_lm,1)
#   #awayteam
#   sp1_winmargin_vec_at_lm <- as.vector(sp1_winmargin_h[sp1_awayteamindex,])
#   sp1_winmargin_vec_at_lm[is.na(sp1_winmargin_vec_at_lm)] <- ""
#   sp1_winmargin_vec_at_lm <- sp1_winmargin_vec_at_lm[sp1_winmargin_vec_at_lm != ""]
#   sp1_winmargin_vec_at_lm  <-tail(sp1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sp1_yellowtotals_vec_ht <- as.vector(sp1_yellowtotalsv2[sp1_hometeamindex,])
#   sp1_yellowtotals_vec_ht[is.na(sp1_yellowtotals_vec_ht)] <- ""
#   sp1_yellowtotals_vec_ht <- sp1_yellowtotals_vec_ht[sp1_yellowtotals_vec_ht != ""]
#   sp1_yellowtotals_vec_ht  <-tail(sp1_yellowtotals_vec_ht,1)
#   #awayteam
#   sp1_yellowtotals_vec_at <- as.vector(sp1_yellowtotalsv2[sp1_awayteamindex,])
#   sp1_yellowtotals_vec_at[is.na(sp1_yellowtotals_vec_at)] <- ""
#   sp1_yellowtotals_vec_at <- sp1_yellowtotals_vec_at[sp1_yellowtotals_vec_at != ""]
#   sp1_yellowtotals_vec_at  <-tail(sp1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sp1_cornertotals_vec_ht <- as.vector(sp1_cornertotalsv2[sp1_hometeamindex,])
#   sp1_cornertotals_vec_ht[is.na(sp1_cornertotals_vec_ht)] <- ""
#   sp1_cornertotals_vec_ht <- sp1_cornertotals_vec_ht[sp1_cornertotals_vec_ht != ""]
#   sp1_cornertotals_vec_ht  <-tail(sp1_cornertotals_vec_ht,1)
#   #awayteam
#   sp1_cornertotals_vec_at <- as.vector(sp1_cornertotalsv2[sp1_awayteamindex,])
#   sp1_cornertotals_vec_at[is.na(sp1_cornertotals_vec_at)] <- ""
#   sp1_cornertotals_vec_at <- sp1_cornertotals_vec_at[sp1_cornertotals_vec_at != ""]
#   sp1_cornertotals_vec_at  <-tail(sp1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sp1_xshotsconversion_vec_ht <- as.vector(sp1_shots_analysis[sp1_hometeamindex,])
#   sp1_xshotsconversion_vec_ht[is.na(sp1_xshotsconversion_vec_ht)] <- ""
#   sp1_xshotsconversion_vec_ht <- sp1_xshotsconversion_vec_ht[sp1_xshotsconversion_vec_ht != ""]
#   sp1_xshotsconversion_vec_ht  <-tail(sp1_xshotsconversion_vec_ht,1)
#   #awayteam
#   sp1_xshotsconversion_vec_at <- as.vector(sp1_shots_analysis[sp1_awayteamindex,])
#   sp1_xshotsconversion_vec_at[is.na(sp1_xshotsconversion_vec_at)] <- ""
#   sp1_xshotsconversion_vec_at <- sp1_xshotsconversion_vec_at[sp1_xshotsconversion_vec_at != ""]
#   sp1_xshotsconversion_vec_at  <-tail(sp1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sp1_fouls_conversion_vec_ht <- as.vector(sp1_fouls_conversion[sp1_hometeamindex,])
#   sp1_fouls_conversion_vec_ht[is.na(sp1_fouls_conversion_vec_ht)] <- ""
#   sp1_fouls_conversion_vec_ht <- sp1_fouls_conversion_vec_ht[sp1_fouls_conversion_vec_ht != ""]
#   sp1_fouls_conversion_vec_ht  <-tail(sp1_fouls_conversion_vec_ht,1)
#   #awayteam
#   sp1_fouls_conversion_vec_at <- as.vector(sp1_fouls_conversion[sp1_awayteamindex,])
#   sp1_fouls_conversion_vec_at[is.na(sp1_fouls_conversion_vec_at)] <- ""
#   sp1_fouls_conversion_vec_at <- sp1_fouls_conversion_vec_at[sp1_fouls_conversion_vec_at != ""]
#   sp1_fouls_conversion_vec_at  <-tail(sp1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sp1_ht_last6points <- sp1_ht_numberof_wins*3 + sp1_ht_numberof_draws*1
#   sp1_at_last6points <- sp1_at_numberof_wins*3 + sp1_at_numberof_draws*1
#
#   if(sp1_ht_last6points > sp1_at_last6points) {sp1_3waypick <- "1"}  else {sp1_3waypick <- "X2"}
#
#   if(sp1_at_last6points > sp1_ht_last6points ) {sp1_3waypick <- "2"} else {sp1_3waypick <- "1X"}
#
#   if(sp1_ht_no_of_ov25 + sp1_at_no_of_ov25 >= 6) {sp1_goalspick <- "ov25"} else {sp1_goalspick <- "un25"}
#
#   if(sp1_ht_no_of_un25 + sp1_at_no_of_un25 >= 6) {sp1_goalspick <- "un25"} else {sp1_goalspick <- "ov25"}
#
#   if(sp1_ht_matches_scoring >= 4 && sp1_at_matches_scoring >=4) {sp1_btts <- "BTTS-Y"} else {sp1_btts <- "BTTS-N"}
#
#
#   sp1_prediction[sp1_row] <- rbind(paste(sp1_3waypick,sp1_goalspick,sp1_btts,sep = ","))
#   sp1_HWM[sp1_row] <- sp1_ht_totalwinmargin
#   sp1_AWM[sp1_row] <- sp1_at_totalwinmargin
#
#   sp1_HWMLM[sp1_row] <- sp1_winmargin_vec_ht_lm
#   sp1_AWMLM[sp1_row] <- sp1_winmargin_vec_at_lm
#
#   sp1_HY[sp1_row] <- sp1_yellowtotals_vec_ht
#   sp1_AY[sp1_row] <- sp1_yellowtotals_vec_at
#
#   sp1_HCO[sp1_row] <- sp1_cornertotals_vec_ht
#   sp1_ACO[sp1_row] <- sp1_cornertotals_vec_at
#
#   sp1_HXSC[sp1_row] <- sp1_xshotsconversion_vec_ht
#   sp1_AXSC[sp1_row] <- sp1_xshotsconversion_vec_at
#
#   sp1_HYCPF[sp1_row] <- sp1_fouls_conversion_vec_ht
#   sp1_AYCPF[sp1_row] <- sp1_fouls_conversion_vec_at
# }
#
# sp1_prediction <- as.data.frame(sp1_prediction)
# colnames(sp1_prediction) <- "prediction"
#
# sp1_HWM <- as.data.frame(sp1_HWM)
# colnames(sp1_HWM) <- "HWM"
#
# sp1_AWM <- as.data.frame(sp1_AWM)
# colnames(sp1_AWM) <- "AWM"
#
# sp1_HWMLM <- as.data.frame(sp1_HWMLM)
# colnames(sp1_HWMLM) <- "HWMLM"
#
# sp1_AWMLM <- as.data.frame(sp1_AWMLM)
# colnames(sp1_AWMLM) <- "AWMLM"
#
# sp1_HY <- as.data.frame(sp1_HY)
# colnames(sp1_HY) <- "AVGHY"
#
# sp1_AY <- as.data.frame(sp1_AY)
# colnames(sp1_AY) <- "AVGAY"
#
# sp1_HCO <- as.data.frame(sp1_HCO)
# colnames(sp1_HCO) <- "AVGHCO"
#
# sp1_ACO <- as.data.frame(sp1_ACO)
# colnames(sp1_ACO) <- "AVGACO"
#
# sp1_HXSC <- as.data.frame(sp1_HXSC)
# colnames(sp1_HXSC) <- "HXSC"
#
# sp1_AXSC <- as.data.frame(sp1_AXSC)
# colnames(sp1_AXSC) <- "AXSC"
#
# sp1_HYCPF <- as.data.frame(sp1_HYCPF)
# colnames(sp1_HYCPF) <- "HYCPF"
#
# sp1_AYCPF <- as.data.frame(sp1_AYCPF)
# colnames(sp1_AYCPF) <- "AYCPF"
#
# sp1_picks <- cbind(SP1_fixtures$Div,SP1_fixtures$HomeTeam_sp1,SP1_fixtures$AwayTeam_sp1,sp1_prediction,sp1_HWM,sp1_AWM,sp1_HWMLM,sp1_AWMLM,sp1_HY,sp1_AY,sp1_HCO,sp1_ACO,sp1_HXSC,sp1_AXSC,sp1_HYCPF,sp1_AYCPF)
#
# colnames(sp1_picks)[1] <- "picks_Div"
# colnames(sp1_picks)[2] <- "picks_HomeTeam"
# colnames(sp1_picks)[3] <- "picks_AwayTeam"
# sp1_picks$matchid <- paste(sp1_picks$picks_HomeTeam,sp1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SP1
# sp1_picks
# ############################################################################################
# #SP2
# SP2_fixtures$Hometeam_sp2_index <- match(SP2_fixtures$HomeTeam_sp2,sp2_teams)
# SP2_fixtures$Awayteam_sp2_index <- match(SP2_fixtures$AwayTeam_sp2,sp2_teams)
# sp2_prediction <- c()
# sp2_HWM <- c()
# sp2_AWM <- c()
# sp2_HWMLM <- c()
# sp2_AWMLM <- c()
# sp2_HY <- c()
# sp2_AY <- c()
# sp2_HCO <- c()
# sp2_ACO <- c()
# sp2_HXSC <- c()
# sp2_AXSC <- c()
# sp2_HYCPF <- c()
# sp2_AYCPF <- c()
# for(sp2_row in 1:nrow(SP2_fixtures))
# {
#
#   sp2_hometeamindex <- SP2_fixtures[sp2_row,"Hometeam_sp2_index"]
#   sp2_awayteamindex <- SP2_fixtures[sp2_row,"Awayteam_sp2_index"]
#   #analyse team form
#   #home team
#   sp2_form_vec_ht <- as.vector(sp2_form_h[sp2_hometeamindex,])
#   sp2_form_vec_ht[is.na(sp2_form_vec_ht)] <- ""
#   sp2_form_vec_ht <- sp2_form_vec_ht[sp2_form_vec_ht != ""]
#   sp2_form_vec_ht  <-tail(sp2_form_vec_ht,6)
#   sp2_ht_numberof_wins <- length(which(sp2_form_vec_ht == "W"))
#   sp2_ht_numberof_draws <- length(which(sp2_form_vec_ht == "D"))
#   sp2_ht_numberof_loss <- length(which(sp2_form_vec_ht == "L"))
#   #awayteam
#   sp2_form_vec_at <- as.vector(sp2_form_h[sp2_awayteamindex,])
#   sp2_form_vec_at[is.na(sp2_form_vec_at)] <- ""
#   sp2_form_vec_at <- sp2_form_vec_at[sp2_form_vec_at != ""]
#   sp2_form_vec_at  <-tail(sp2_form_vec_at,6)
#   sp2_at_numberof_wins <- length(which(sp2_form_vec_at == "W"))
#   sp2_at_numberof_draws <- length(which(sp2_form_vec_at == "D"))
#   sp2_at_numberof_loss <- length(which(sp2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sp2_goalscored_vec_ht <- as.vector(sp2_goalscored_h[sp2_hometeamindex,])
#   sp2_goalscored_vec_ht[is.na(sp2_goalscored_vec_ht)] <- ""
#   sp2_goalscored_vec_ht <- sp2_goalscored_vec_ht[sp2_goalscored_vec_ht != ""]
#   sp2_goalscored_vec_ht  <-tail(sp2_goalscored_vec_ht,6)
#   sp2_goalscored_vec_ht  <- as.numeric(sp2_goalscored_vec_ht)
#   sp2_ht_totalgoalscored <- sum(sp2_goalscored_vec_ht)
#   sp2_ht_matches_scoring <- length(which(sp2_goalscored_vec_ht > 0))
#   sp2_ht_matches_without_scoring <- length(which(sp2_goalscored_vec_ht == "0"))
#   #awayteam
#   sp2_goalscored_vec_at <- as.vector(sp2_goalscored_h[sp2_awayteamindex,])
#   sp2_goalscored_vec_at[is.na(sp2_goalscored_vec_at)] <- ""
#   sp2_goalscored_vec_at <- sp2_goalscored_vec_at[sp2_goalscored_vec_at != ""]
#   sp2_goalscored_vec_at  <-tail(sp2_goalscored_vec_at,6)
#   sp2_goalscored_vec_at  <- as.numeric(sp2_goalscored_vec_at)
#   sp2_at_totalgoalscored <- sum(sp2_goalscored_vec_at)
#   sp2_at_matches_scoring <- length(which(sp2_goalscored_vec_at > 0))
#   sp2_at_matches_without_scoring <- length(which(sp2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sp2_goalconceded_vec_ht <- as.vector(sp2_goalconceded_h[sp2_hometeamindex,])
#   sp2_goalconceded_vec_ht[is.na(sp2_goalconceded_vec_ht)] <- ""
#   sp2_goalconceded_vec_ht <- sp2_goalconceded_vec_ht[sp2_goalconceded_vec_ht != ""]
#   sp2_goalconceded_vec_ht  <-tail(sp2_goalconceded_vec_ht,6)
#   sp2_goalconceded_vec_ht  <- as.numeric(sp2_goalconceded_vec_ht)
#   sp2_goalconceded_vec_ht
#   sp2_ht_totalgoalconceded <- sum(sp2_goalconceded_vec_ht)
#   sp2_ht_matches_concede <- length(which(sp2_goalconceded_vec_ht > 0))
#   sp2_ht_matches_without_concede <- length(which(sp2_goalconceded_vec_ht == "0"))
#   #awayteam
#   sp2_goalconceded_vec_at <- as.vector(sp2_goalconceded_h[sp2_awayteamindex,])
#   sp2_goalconceded_vec_at[is.na(sp2_goalconceded_vec_at)] <- ""
#   sp2_goalconceded_vec_at <- sp2_goalconceded_vec_at[sp2_goalconceded_vec_at != ""]
#   sp2_goalconceded_vec_at  <-tail(sp2_goalconceded_vec_at,6)
#   sp2_goalconceded_vec_at  <- as.numeric(sp2_goalconceded_vec_at)
#   sp2_at_totalgoalconceded <- sum(sp2_goalconceded_vec_at)
#   sp2_at_matches_concede <- length(which(sp2_goalconceded_vec_at > 0))
#   sp2_at_matches_without_concede <- length(which(sp2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sp2_totalgoals_vec_ht <- as.vector(sp2_totalgoals_h[sp2_hometeamindex,])
#   sp2_totalgoals_vec_ht[is.na(sp2_totalgoals_vec_ht)] <- ""
#   sp2_totalgoals_vec_ht <- sp2_totalgoals_vec_ht[sp2_totalgoals_vec_ht != ""]
#   sp2_totalgoals_vec_ht  <-tail(sp2_totalgoals_vec_ht,6)
#   sp2_totalgoals_vec_ht  <- as.numeric(sp2_totalgoals_vec_ht)
#   sp2_totalgoals_vec_ht
#   sp2_ht_totalgoals <- sum(sp2_totalgoals_vec_ht)
#   sp2_ht_avgtotalgoals <- (sp2_ht_totalgoals/6)
#   sp2_ht_no_of_ov25 <- length(which(sp2_totalgoals_vec_ht >= 3))
#   sp2_ht_no_of_un25 <- length(which(sp2_totalgoals_vec_ht <= 2))
#   #awayteam
#   sp2_totalgoals_vec_at <- as.vector(sp2_totalgoals_h[sp2_awayteamindex,])
#   sp2_totalgoals_vec_at[is.na(sp2_totalgoals_vec_at)] <- ""
#   sp2_totalgoals_vec_at <- sp2_totalgoals_vec_at[sp2_totalgoals_vec_at != ""]
#   sp2_totalgoals_vec_at  <-tail(sp2_totalgoals_vec_at,6)
#   sp2_totalgoals_vec_at  <- as.numeric(sp2_totalgoals_vec_at)
#   sp2_totalgoals_vec_at
#   sp2_at_totalgoals <- sum(sp2_totalgoals_vec_at)
#   sp2_at_avgtotalgoals <- (sp2_at_totalgoals/6)
#   sp2_at_no_of_ov25 <- length(which(sp2_totalgoals_vec_at >= 3))
#   sp2_at_no_of_un25 <- length(which(sp2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sp2_winmargin_vec_ht <- as.vector(sp2_winmargin_h[sp2_hometeamindex,])
#   sp2_winmargin_vec_ht[is.na(sp2_winmargin_vec_ht)] <- ""
#   sp2_winmargin_vec_ht <- sp2_winmargin_vec_ht[sp2_winmargin_vec_ht != ""]
#   sp2_winmargin_vec_ht  <-tail(sp2_winmargin_vec_ht,6)
#   sp2_winmargin_vec_ht  <- as.numeric(sp2_winmargin_vec_ht)
#
#   sp2_ht_totalwinmargin <- sum(sp2_winmargin_vec_ht)
#   sp2_ht_no_of_winmargin_ov0 <- length(which(sp2_winmargin_vec_ht >= 0))
#   sp2_ht_no_of_winmargin_ov1 <- length(which(sp2_winmargin_vec_ht >= 1))
#   sp2_ht_no_of_winmargin_un0 <- length(which(sp2_winmargin_vec_ht <= 0))
#   sp2_ht_no_of_winmargin_un1 <- length(which(sp2_winmargin_vec_ht <= 1))
#   #awayteam
#   sp2_winmargin_vec_at <- as.vector(sp2_winmargin_h[sp2_awayteamindex,])
#   sp2_winmargin_vec_at[is.na(sp2_winmargin_vec_at)] <- ""
#   sp2_winmargin_vec_at <- sp2_winmargin_vec_at[sp2_winmargin_vec_at != ""]
#   sp2_winmargin_vec_at  <-tail(sp2_winmargin_vec_at,6)
#   sp2_winmargin_vec_at  <- as.numeric(sp2_winmargin_vec_at)
#
#   sp2_at_totalwinmargin <- sum(sp2_winmargin_vec_at)
#   sp2_at_no_of_winmargin_ov0 <- length(which(sp2_winmargin_vec_at >= 0))
#   sp2_at_no_of_winmargin_ov1 <- length(which(sp2_winmargin_vec_at >= 1))
#   sp2_at_no_of_winmargin_un0 <- length(which(sp2_winmargin_vec_at <= 0))
#   sp2_at_no_of_winmargin_un1 <- length(which(sp2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sp2_winmargin_vec_ht_lm <- as.vector(sp2_winmargin_h[sp2_hometeamindex,])
#   sp2_winmargin_vec_ht_lm[is.na(sp2_winmargin_vec_ht_lm)] <- ""
#   sp2_winmargin_vec_ht_lm <- sp2_winmargin_vec_ht_lm[sp2_winmargin_vec_ht_lm != ""]
#   sp2_winmargin_vec_ht_lm  <-tail(sp2_winmargin_vec_ht_lm,1)
#   #awayteam
#   sp2_winmargin_vec_at_lm <- as.vector(sp2_winmargin_h[sp2_awayteamindex,])
#   sp2_winmargin_vec_at_lm[is.na(sp2_winmargin_vec_at_lm)] <- ""
#   sp2_winmargin_vec_at_lm <- sp2_winmargin_vec_at_lm[sp2_winmargin_vec_at_lm != ""]
#   sp2_winmargin_vec_at_lm  <-tail(sp2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sp2_yellowtotals_vec_ht <- as.vector(sp2_yellowtotalsv2[sp2_hometeamindex,])
#   sp2_yellowtotals_vec_ht[is.na(sp2_yellowtotals_vec_ht)] <- ""
#   sp2_yellowtotals_vec_ht <- sp2_yellowtotals_vec_ht[sp2_yellowtotals_vec_ht != ""]
#   sp2_yellowtotals_vec_ht  <-tail(sp2_yellowtotals_vec_ht,1)
#   #awayteam
#   sp2_yellowtotals_vec_at <- as.vector(sp2_yellowtotalsv2[sp2_awayteamindex,])
#   sp2_yellowtotals_vec_at[is.na(sp2_yellowtotals_vec_at)] <- ""
#   sp2_yellowtotals_vec_at <- sp2_yellowtotals_vec_at[sp2_yellowtotals_vec_at != ""]
#   sp2_yellowtotals_vec_at  <-tail(sp2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sp2_cornertotals_vec_ht <- as.vector(sp2_cornertotalsv2[sp2_hometeamindex,])
#   sp2_cornertotals_vec_ht[is.na(sp2_cornertotals_vec_ht)] <- ""
#   sp2_cornertotals_vec_ht <- sp2_cornertotals_vec_ht[sp2_cornertotals_vec_ht != ""]
#   sp2_cornertotals_vec_ht  <-tail(sp2_cornertotals_vec_ht,1)
#   #awayteam
#   sp2_cornertotals_vec_at <- as.vector(sp2_cornertotalsv2[sp2_awayteamindex,])
#   sp2_cornertotals_vec_at[is.na(sp2_cornertotals_vec_at)] <- ""
#   sp2_cornertotals_vec_at <- sp2_cornertotals_vec_at[sp2_cornertotals_vec_at != ""]
#   sp2_cornertotals_vec_at  <-tail(sp2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sp2_xshotsconversion_vec_ht <- as.vector(sp2_shots_analysis[sp2_hometeamindex,])
#   sp2_xshotsconversion_vec_ht[is.na(sp2_xshotsconversion_vec_ht)] <- ""
#   sp2_xshotsconversion_vec_ht <- sp2_xshotsconversion_vec_ht[sp2_xshotsconversion_vec_ht != ""]
#   sp2_xshotsconversion_vec_ht  <-tail(sp2_xshotsconversion_vec_ht,1)
#   #awayteam
#   sp2_xshotsconversion_vec_at <- as.vector(sp2_shots_analysis[sp2_awayteamindex,])
#   sp2_xshotsconversion_vec_at[is.na(sp2_xshotsconversion_vec_at)] <- ""
#   sp2_xshotsconversion_vec_at <- sp2_xshotsconversion_vec_at[sp2_xshotsconversion_vec_at != ""]
#   sp2_xshotsconversion_vec_at  <-tail(sp2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sp2_fouls_conversion_vec_ht <- as.vector(sp2_fouls_conversion[sp2_hometeamindex,])
#   sp2_fouls_conversion_vec_ht[is.na(sp2_fouls_conversion_vec_ht)] <- ""
#   sp2_fouls_conversion_vec_ht <- sp2_fouls_conversion_vec_ht[sp2_fouls_conversion_vec_ht != ""]
#   sp2_fouls_conversion_vec_ht  <-tail(sp2_fouls_conversion_vec_ht,1)
#   #awayteam
#   sp2_fouls_conversion_vec_at <- as.vector(sp2_fouls_conversion[sp2_awayteamindex,])
#   sp2_fouls_conversion_vec_at[is.na(sp2_fouls_conversion_vec_at)] <- ""
#   sp2_fouls_conversion_vec_at <- sp2_fouls_conversion_vec_at[sp2_fouls_conversion_vec_at != ""]
#   sp2_fouls_conversion_vec_at  <-tail(sp2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sp2_ht_last6points <- sp2_ht_numberof_wins*3 + sp2_ht_numberof_draws*1
#   sp2_at_last6points <- sp2_at_numberof_wins*3 + sp2_at_numberof_draws*1
#
#   if(sp2_ht_last6points > sp2_at_last6points) {sp2_3waypick <- "1"}  else {sp2_3waypick <- "X2"}
#
#   if(sp2_at_last6points > sp2_ht_last6points ) {sp2_3waypick <- "2"} else {sp2_3waypick <- "1X"}
#
#   if(sp2_ht_no_of_ov25 + sp2_at_no_of_ov25 >= 6) {sp2_goalspick <- "ov25"} else {sp2_goalspick <- "un25"}
#
#   if(sp2_ht_no_of_un25 + sp2_at_no_of_un25 >= 6) {sp2_goalspick <- "un25"} else {sp2_goalspick <- "ov25"}
#
#   if(sp2_ht_matches_scoring >= 4 && sp2_at_matches_scoring >=4) {sp2_btts <- "BTTS-Y"} else {sp2_btts <- "BTTS-N"}
#
#
#   sp2_prediction[sp2_row] <- rbind(paste(sp2_3waypick,sp2_goalspick,sp2_btts,sep = ","))
#   sp2_HWM[sp2_row] <- sp2_ht_totalwinmargin
#   sp2_AWM[sp2_row] <- sp2_at_totalwinmargin
#
#   sp2_HWMLM[sp2_row] <- sp2_winmargin_vec_ht_lm
#   sp2_AWMLM[sp2_row] <- sp2_winmargin_vec_at_lm
#
#   sp2_HY[sp2_row] <- sp2_yellowtotals_vec_ht
#   sp2_AY[sp2_row] <- sp2_yellowtotals_vec_at
#
#   sp2_HCO[sp2_row] <- sp2_cornertotals_vec_ht
#   sp2_ACO[sp2_row] <- sp2_cornertotals_vec_at
#
#   sp2_HXSC[sp2_row] <- sp2_xshotsconversion_vec_ht
#   sp2_AXSC[sp2_row] <- sp2_xshotsconversion_vec_at
#
#   sp2_HYCPF[sp2_row] <- sp2_fouls_conversion_vec_ht
#   sp2_AYCPF[sp2_row] <- sp2_fouls_conversion_vec_at
# }
#
# sp2_prediction <- as.data.frame(sp2_prediction)
# colnames(sp2_prediction) <- "prediction"
#
# sp2_HWM <- as.data.frame(sp2_HWM)
# colnames(sp2_HWM) <- "HWM"
#
# sp2_AWM <- as.data.frame(sp2_AWM)
# colnames(sp2_AWM) <- "AWM"
#
# sp2_HWMLM <- as.data.frame(sp2_HWMLM)
# colnames(sp2_HWMLM) <- "HWMLM"
#
# sp2_AWMLM <- as.data.frame(sp2_AWMLM)
# colnames(sp2_AWMLM) <- "AWMLM"
#
# sp2_HY <- as.data.frame(sp2_HY)
# colnames(sp2_HY) <- "AVGHY"
#
# sp2_AY <- as.data.frame(sp2_AY)
# colnames(sp2_AY) <- "AVGAY"
#
# sp2_HCO <- as.data.frame(sp2_HCO)
# colnames(sp2_HCO) <- "AVGHCO"
#
# sp2_ACO <- as.data.frame(sp2_ACO)
# colnames(sp2_ACO) <- "AVGACO"
#
# sp2_HXSC <- as.data.frame(sp2_HXSC)
# colnames(sp2_HXSC) <- "HXSC"
#
# sp2_AXSC <- as.data.frame(sp2_AXSC)
# colnames(sp2_AXSC) <- "AXSC"
#
# sp2_HYCPF <- as.data.frame(sp2_HYCPF)
# colnames(sp2_HYCPF) <- "HYCPF"
#
# sp2_AYCPF <- as.data.frame(sp2_AYCPF)
# colnames(sp2_AYCPF) <- "AYCPF"
#
# sp2_picks <- cbind(SP2_fixtures$Div,SP2_fixtures$HomeTeam_sp2,SP2_fixtures$AwayTeam_sp2,sp2_prediction,sp2_HWM,sp2_AWM,sp2_HWMLM,sp2_AWMLM,sp2_HY,sp2_AY,sp2_HCO,sp2_ACO,sp2_HXSC,sp2_AXSC,sp2_HYCPF,sp2_AYCPF)
#
# colnames(sp2_picks)[1] <- "picks_Div"
# colnames(sp2_picks)[2] <- "picks_HomeTeam"
# colnames(sp2_picks)[3] <- "picks_AwayTeam"
# sp2_picks$matchid <- paste(sp2_picks$picks_HomeTeam,sp2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SP2
# sp2_picks
# ############################################################################################
# #SC0
# SC0_fixtures$Hometeam_sc0_index <- match(SC0_fixtures$HomeTeam_sc0,sc0_teams)
# SC0_fixtures$Awayteam_sc0_index <- match(SC0_fixtures$AwayTeam_sc0,sc0_teams)
# sc0_prediction <- c()
# sc0_HWM <- c()
# sc0_AWM <- c()
# sc0_HWMLM <- c()
# sc0_AWMLM <- c()
# sc0_HY <- c()
# sc0_AY <- c()
# sc0_HCO <- c()
# sc0_ACO <- c()
# sc0_HXSC <- c()
# sc0_AXSC <- c()
# sc0_HYCPF <- c()
# sc0_AYCPF <- c()
# for(sc0_row in 1:nrow(SC0_fixtures))
# {
#
#   sc0_hometeamindex <- SC0_fixtures[sc0_row,"Hometeam_sc0_index"]
#   sc0_awayteamindex <- SC0_fixtures[sc0_row,"Awayteam_sc0_index"]
#   #analyse team form
#   #home team
#   sc0_form_vec_ht <- as.vector(sc0_form_h[sc0_hometeamindex,])
#   sc0_form_vec_ht[is.na(sc0_form_vec_ht)] <- ""
#   sc0_form_vec_ht <- sc0_form_vec_ht[sc0_form_vec_ht != ""]
#   sc0_form_vec_ht  <-tail(sc0_form_vec_ht,6)
#   sc0_ht_numberof_wins <- length(which(sc0_form_vec_ht == "W"))
#   sc0_ht_numberof_draws <- length(which(sc0_form_vec_ht == "D"))
#   sc0_ht_numberof_loss <- length(which(sc0_form_vec_ht == "L"))
#   #awayteam
#   sc0_form_vec_at <- as.vector(sc0_form_h[sc0_awayteamindex,])
#   sc0_form_vec_at[is.na(sc0_form_vec_at)] <- ""
#   sc0_form_vec_at <- sc0_form_vec_at[sc0_form_vec_at != ""]
#   sc0_form_vec_at  <-tail(sc0_form_vec_at,6)
#   sc0_at_numberof_wins <- length(which(sc0_form_vec_at == "W"))
#   sc0_at_numberof_draws <- length(which(sc0_form_vec_at == "D"))
#   sc0_at_numberof_loss <- length(which(sc0_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sc0_goalscored_vec_ht <- as.vector(sc0_goalscored_h[sc0_hometeamindex,])
#   sc0_goalscored_vec_ht[is.na(sc0_goalscored_vec_ht)] <- ""
#   sc0_goalscored_vec_ht <- sc0_goalscored_vec_ht[sc0_goalscored_vec_ht != ""]
#   sc0_goalscored_vec_ht  <-tail(sc0_goalscored_vec_ht,6)
#   sc0_goalscored_vec_ht  <- as.numeric(sc0_goalscored_vec_ht)
#   sc0_ht_totalgoalscored <- sum(sc0_goalscored_vec_ht)
#   sc0_ht_matches_scoring <- length(which(sc0_goalscored_vec_ht > 0))
#   sc0_ht_matches_without_scoring <- length(which(sc0_goalscored_vec_ht == "0"))
#   #awayteam
#   sc0_goalscored_vec_at <- as.vector(sc0_goalscored_h[sc0_awayteamindex,])
#   sc0_goalscored_vec_at[is.na(sc0_goalscored_vec_at)] <- ""
#   sc0_goalscored_vec_at <- sc0_goalscored_vec_at[sc0_goalscored_vec_at != ""]
#   sc0_goalscored_vec_at  <-tail(sc0_goalscored_vec_at,6)
#   sc0_goalscored_vec_at  <- as.numeric(sc0_goalscored_vec_at)
#   sc0_at_totalgoalscored <- sum(sc0_goalscored_vec_at)
#   sc0_at_matches_scoring <- length(which(sc0_goalscored_vec_at > 0))
#   sc0_at_matches_without_scoring <- length(which(sc0_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sc0_goalconceded_vec_ht <- as.vector(sc0_goalconceded_h[sc0_hometeamindex,])
#   sc0_goalconceded_vec_ht[is.na(sc0_goalconceded_vec_ht)] <- ""
#   sc0_goalconceded_vec_ht <- sc0_goalconceded_vec_ht[sc0_goalconceded_vec_ht != ""]
#   sc0_goalconceded_vec_ht  <-tail(sc0_goalconceded_vec_ht,6)
#   sc0_goalconceded_vec_ht  <- as.numeric(sc0_goalconceded_vec_ht)
#   sc0_goalconceded_vec_ht
#   sc0_ht_totalgoalconceded <- sum(sc0_goalconceded_vec_ht)
#   sc0_ht_matches_concede <- length(which(sc0_goalconceded_vec_ht > 0))
#   sc0_ht_matches_without_concede <- length(which(sc0_goalconceded_vec_ht == "0"))
#   #awayteam
#   sc0_goalconceded_vec_at <- as.vector(sc0_goalconceded_h[sc0_awayteamindex,])
#   sc0_goalconceded_vec_at[is.na(sc0_goalconceded_vec_at)] <- ""
#   sc0_goalconceded_vec_at <- sc0_goalconceded_vec_at[sc0_goalconceded_vec_at != ""]
#   sc0_goalconceded_vec_at  <-tail(sc0_goalconceded_vec_at,6)
#   sc0_goalconceded_vec_at  <- as.numeric(sc0_goalconceded_vec_at)
#   sc0_at_totalgoalconceded <- sum(sc0_goalconceded_vec_at)
#   sc0_at_matches_concede <- length(which(sc0_goalconceded_vec_at > 0))
#   sc0_at_matches_without_concede <- length(which(sc0_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sc0_totalgoals_vec_ht <- as.vector(sc0_totalgoals_h[sc0_hometeamindex,])
#   sc0_totalgoals_vec_ht[is.na(sc0_totalgoals_vec_ht)] <- ""
#   sc0_totalgoals_vec_ht <- sc0_totalgoals_vec_ht[sc0_totalgoals_vec_ht != ""]
#   sc0_totalgoals_vec_ht  <-tail(sc0_totalgoals_vec_ht,6)
#   sc0_totalgoals_vec_ht  <- as.numeric(sc0_totalgoals_vec_ht)
#   sc0_totalgoals_vec_ht
#   sc0_ht_totalgoals <- sum(sc0_totalgoals_vec_ht)
#   sc0_ht_avgtotalgoals <- (sc0_ht_totalgoals/6)
#   sc0_ht_no_of_ov25 <- length(which(sc0_totalgoals_vec_ht >= 3))
#   sc0_ht_no_of_un25 <- length(which(sc0_totalgoals_vec_ht <= 2))
#   #awayteam
#   sc0_totalgoals_vec_at <- as.vector(sc0_totalgoals_h[sc0_awayteamindex,])
#   sc0_totalgoals_vec_at[is.na(sc0_totalgoals_vec_at)] <- ""
#   sc0_totalgoals_vec_at <- sc0_totalgoals_vec_at[sc0_totalgoals_vec_at != ""]
#   sc0_totalgoals_vec_at  <-tail(sc0_totalgoals_vec_at,6)
#   sc0_totalgoals_vec_at  <- as.numeric(sc0_totalgoals_vec_at)
#   sc0_totalgoals_vec_at
#   sc0_at_totalgoals <- sum(sc0_totalgoals_vec_at)
#   sc0_at_avgtotalgoals <- (sc0_at_totalgoals/6)
#   sc0_at_no_of_ov25 <- length(which(sc0_totalgoals_vec_at >= 3))
#   sc0_at_no_of_un25 <- length(which(sc0_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sc0_winmargin_vec_ht <- as.vector(sc0_winmargin_h[sc0_hometeamindex,])
#   sc0_winmargin_vec_ht[is.na(sc0_winmargin_vec_ht)] <- ""
#   sc0_winmargin_vec_ht <- sc0_winmargin_vec_ht[sc0_winmargin_vec_ht != ""]
#   sc0_winmargin_vec_ht  <-tail(sc0_winmargin_vec_ht,6)
#   sc0_winmargin_vec_ht  <- as.numeric(sc0_winmargin_vec_ht)
#
#   sc0_ht_totalwinmargin <- sum(sc0_winmargin_vec_ht)
#   sc0_ht_no_of_winmargin_ov0 <- length(which(sc0_winmargin_vec_ht >= 0))
#   sc0_ht_no_of_winmargin_ov1 <- length(which(sc0_winmargin_vec_ht >= 1))
#   sc0_ht_no_of_winmargin_un0 <- length(which(sc0_winmargin_vec_ht <= 0))
#   sc0_ht_no_of_winmargin_un1 <- length(which(sc0_winmargin_vec_ht <= 1))
#   #awayteam
#   sc0_winmargin_vec_at <- as.vector(sc0_winmargin_h[sc0_awayteamindex,])
#   sc0_winmargin_vec_at[is.na(sc0_winmargin_vec_at)] <- ""
#   sc0_winmargin_vec_at <- sc0_winmargin_vec_at[sc0_winmargin_vec_at != ""]
#   sc0_winmargin_vec_at  <-tail(sc0_winmargin_vec_at,6)
#   sc0_winmargin_vec_at  <- as.numeric(sc0_winmargin_vec_at)
#
#   sc0_at_totalwinmargin <- sum(sc0_winmargin_vec_at)
#   sc0_at_no_of_winmargin_ov0 <- length(which(sc0_winmargin_vec_at >= 0))
#   sc0_at_no_of_winmargin_ov1 <- length(which(sc0_winmargin_vec_at >= 1))
#   sc0_at_no_of_winmargin_un0 <- length(which(sc0_winmargin_vec_at <= 0))
#   sc0_at_no_of_winmargin_un1 <- length(which(sc0_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sc0_winmargin_vec_ht_lm <- as.vector(sc0_winmargin_h[sc0_hometeamindex,])
#   sc0_winmargin_vec_ht_lm[is.na(sc0_winmargin_vec_ht_lm)] <- ""
#   sc0_winmargin_vec_ht_lm <- sc0_winmargin_vec_ht_lm[sc0_winmargin_vec_ht_lm != ""]
#   sc0_winmargin_vec_ht_lm  <-tail(sc0_winmargin_vec_ht_lm,1)
#   #awayteam
#   sc0_winmargin_vec_at_lm <- as.vector(sc0_winmargin_h[sc0_awayteamindex,])
#   sc0_winmargin_vec_at_lm[is.na(sc0_winmargin_vec_at_lm)] <- ""
#   sc0_winmargin_vec_at_lm <- sc0_winmargin_vec_at_lm[sc0_winmargin_vec_at_lm != ""]
#   sc0_winmargin_vec_at_lm  <-tail(sc0_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sc0_yellowtotals_vec_ht <- as.vector(sc0_yellowtotalsv2[sc0_hometeamindex,])
#   sc0_yellowtotals_vec_ht[is.na(sc0_yellowtotals_vec_ht)] <- ""
#   sc0_yellowtotals_vec_ht <- sc0_yellowtotals_vec_ht[sc0_yellowtotals_vec_ht != ""]
#   sc0_yellowtotals_vec_ht  <-tail(sc0_yellowtotals_vec_ht,1)
#   #awayteam
#   sc0_yellowtotals_vec_at <- as.vector(sc0_yellowtotalsv2[sc0_awayteamindex,])
#   sc0_yellowtotals_vec_at[is.na(sc0_yellowtotals_vec_at)] <- ""
#   sc0_yellowtotals_vec_at <- sc0_yellowtotals_vec_at[sc0_yellowtotals_vec_at != ""]
#   sc0_yellowtotals_vec_at  <-tail(sc0_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sc0_cornertotals_vec_ht <- as.vector(sc0_cornertotalsv2[sc0_hometeamindex,])
#   sc0_cornertotals_vec_ht[is.na(sc0_cornertotals_vec_ht)] <- ""
#   sc0_cornertotals_vec_ht <- sc0_cornertotals_vec_ht[sc0_cornertotals_vec_ht != ""]
#   sc0_cornertotals_vec_ht  <-tail(sc0_cornertotals_vec_ht,1)
#   #awayteam
#   sc0_cornertotals_vec_at <- as.vector(sc0_cornertotalsv2[sc0_awayteamindex,])
#   sc0_cornertotals_vec_at[is.na(sc0_cornertotals_vec_at)] <- ""
#   sc0_cornertotals_vec_at <- sc0_cornertotals_vec_at[sc0_cornertotals_vec_at != ""]
#   sc0_cornertotals_vec_at  <-tail(sc0_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sc0_xshotsconversion_vec_ht <- as.vector(sc0_shots_analysis[sc0_hometeamindex,])
#   sc0_xshotsconversion_vec_ht[is.na(sc0_xshotsconversion_vec_ht)] <- ""
#   sc0_xshotsconversion_vec_ht <- sc0_xshotsconversion_vec_ht[sc0_xshotsconversion_vec_ht != ""]
#   sc0_xshotsconversion_vec_ht  <-tail(sc0_xshotsconversion_vec_ht,1)
#   #awayteam
#   sc0_xshotsconversion_vec_at <- as.vector(sc0_shots_analysis[sc0_awayteamindex,])
#   sc0_xshotsconversion_vec_at[is.na(sc0_xshotsconversion_vec_at)] <- ""
#   sc0_xshotsconversion_vec_at <- sc0_xshotsconversion_vec_at[sc0_xshotsconversion_vec_at != ""]
#   sc0_xshotsconversion_vec_at  <-tail(sc0_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sc0_fouls_conversion_vec_ht <- as.vector(sc0_fouls_conversion[sc0_hometeamindex,])
#   sc0_fouls_conversion_vec_ht[is.na(sc0_fouls_conversion_vec_ht)] <- ""
#   sc0_fouls_conversion_vec_ht <- sc0_fouls_conversion_vec_ht[sc0_fouls_conversion_vec_ht != ""]
#   sc0_fouls_conversion_vec_ht  <-tail(sc0_fouls_conversion_vec_ht,1)
#   #awayteam
#   sc0_fouls_conversion_vec_at <- as.vector(sc0_fouls_conversion[sc0_awayteamindex,])
#   sc0_fouls_conversion_vec_at[is.na(sc0_fouls_conversion_vec_at)] <- ""
#   sc0_fouls_conversion_vec_at <- sc0_fouls_conversion_vec_at[sc0_fouls_conversion_vec_at != ""]
#   sc0_fouls_conversion_vec_at  <-tail(sc0_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sc0_ht_last6points <- sc0_ht_numberof_wins*3 + sc0_ht_numberof_draws*1
#   sc0_at_last6points <- sc0_at_numberof_wins*3 + sc0_at_numberof_draws*1
#
#   if(sc0_ht_last6points > sc0_at_last6points) {sc0_3waypick <- "1"}  else {sc0_3waypick <- "X2"}
#
#   if(sc0_at_last6points > sc0_ht_last6points ) {sc0_3waypick <- "2"} else {sc0_3waypick <- "1X"}
#
#   if(sc0_ht_no_of_ov25 + sc0_at_no_of_ov25 >= 6) {sc0_goalspick <- "ov25"} else {sc0_goalspick <- "un25"}
#
#   if(sc0_ht_no_of_un25 + sc0_at_no_of_un25 >= 6) {sc0_goalspick <- "un25"} else {sc0_goalspick <- "ov25"}
#
#   if(sc0_ht_matches_scoring >= 4 && sc0_at_matches_scoring >=4) {sc0_btts <- "BTTS-Y"} else {sc0_btts <- "BTTS-N"}
#
#
#   sc0_prediction[sc0_row] <- rbind(paste(sc0_3waypick,sc0_goalspick,sc0_btts,sep = ","))
#   sc0_HWM[sc0_row] <- sc0_ht_totalwinmargin
#   sc0_AWM[sc0_row] <- sc0_at_totalwinmargin
#
#   sc0_HWMLM[sc0_row] <- sc0_winmargin_vec_ht_lm
#   sc0_AWMLM[sc0_row] <- sc0_winmargin_vec_at_lm
#
#   sc0_HY[sc0_row] <- sc0_yellowtotals_vec_ht
#   sc0_AY[sc0_row] <- sc0_yellowtotals_vec_at
#
#   sc0_HCO[sc0_row] <- sc0_cornertotals_vec_ht
#   sc0_ACO[sc0_row] <- sc0_cornertotals_vec_at
#
#   sc0_HXSC[sc0_row] <- sc0_xshotsconversion_vec_ht
#   sc0_AXSC[sc0_row] <- sc0_xshotsconversion_vec_at
#
#   sc0_HYCPF[sc0_row] <- sc0_fouls_conversion_vec_ht
#   sc0_AYCPF[sc0_row] <- sc0_fouls_conversion_vec_at
# }
#
# sc0_prediction <- as.data.frame(sc0_prediction)
# colnames(sc0_prediction) <- "prediction"
#
# sc0_HWM <- as.data.frame(sc0_HWM)
# colnames(sc0_HWM) <- "HWM"
#
# sc0_AWM <- as.data.frame(sc0_AWM)
# colnames(sc0_AWM) <- "AWM"
#
# sc0_HWMLM <- as.data.frame(sc0_HWMLM)
# colnames(sc0_HWMLM) <- "HWMLM"
#
# sc0_AWMLM <- as.data.frame(sc0_AWMLM)
# colnames(sc0_AWMLM) <- "AWMLM"
#
# sc0_HY <- as.data.frame(sc0_HY)
# colnames(sc0_HY) <- "AVGHY"
#
# sc0_AY <- as.data.frame(sc0_AY)
# colnames(sc0_AY) <- "AVGAY"
#
# sc0_HCO <- as.data.frame(sc0_HCO)
# colnames(sc0_HCO) <- "AVGHCO"
#
# sc0_ACO <- as.data.frame(sc0_ACO)
# colnames(sc0_ACO) <- "AVGACO"
#
# sc0_HXSC <- as.data.frame(sc0_HXSC)
# colnames(sc0_HXSC) <- "HXSC"
#
# sc0_AXSC <- as.data.frame(sc0_AXSC)
# colnames(sc0_AXSC) <- "AXSC"
#
# sc0_HYCPF <- as.data.frame(sc0_HYCPF)
# colnames(sc0_HYCPF) <- "HYCPF"
#
# sc0_AYCPF <- as.data.frame(sc0_AYCPF)
# colnames(sc0_AYCPF) <- "AYCPF"
#
# sc0_picks <- cbind(SC0_fixtures$Div,SC0_fixtures$HomeTeam_sc0,SC0_fixtures$AwayTeam_sc0,sc0_prediction,sc0_HWM,sc0_AWM,sc0_HWMLM,sc0_AWMLM,sc0_HY,sc0_AY,sc0_HCO,sc0_ACO,sc0_HXSC,sc0_AXSC,sc0_HYCPF,sc0_AYCPF)
#
# colnames(sc0_picks)[1] <- "picks_Div"
# colnames(sc0_picks)[2] <- "picks_HomeTeam"
# colnames(sc0_picks)[3] <- "picks_AwayTeam"
# sc0_picks$matchid <- paste(sc0_picks$picks_HomeTeam,sc0_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SC0
# sc0_picks
# ############################################################################################
# #SC1
# SC1_fixtures$Hometeam_sc1_index <- match(SC1_fixtures$HomeTeam_sc1,sc1_teams)
# SC1_fixtures$Awayteam_sc1_index <- match(SC1_fixtures$AwayTeam_sc1,sc1_teams)
# sc1_prediction <- c()
# sc1_HWM <- c()
# sc1_AWM <- c()
# sc1_HWMLM <- c()
# sc1_AWMLM <- c()
# sc1_HY <- c()
# sc1_AY <- c()
# sc1_HCO <- c()
# sc1_ACO <- c()
# sc1_HXSC <- c()
# sc1_AXSC <- c()
# sc1_HYCPF <- c()
# sc1_AYCPF <- c()
# for(sc1_row in 1:nrow(SC1_fixtures))
# {
#
#   sc1_hometeamindex <- SC1_fixtures[sc1_row,"Hometeam_sc1_index"]
#   sc1_awayteamindex <- SC1_fixtures[sc1_row,"Awayteam_sc1_index"]
#   #analyse team form
#   #home team
#   sc1_form_vec_ht <- as.vector(sc1_form_h[sc1_hometeamindex,])
#   sc1_form_vec_ht[is.na(sc1_form_vec_ht)] <- ""
#   sc1_form_vec_ht <- sc1_form_vec_ht[sc1_form_vec_ht != ""]
#   sc1_form_vec_ht  <-tail(sc1_form_vec_ht,6)
#   sc1_ht_numberof_wins <- length(which(sc1_form_vec_ht == "W"))
#   sc1_ht_numberof_draws <- length(which(sc1_form_vec_ht == "D"))
#   sc1_ht_numberof_loss <- length(which(sc1_form_vec_ht == "L"))
#   #awayteam
#   sc1_form_vec_at <- as.vector(sc1_form_h[sc1_awayteamindex,])
#   sc1_form_vec_at[is.na(sc1_form_vec_at)] <- ""
#   sc1_form_vec_at <- sc1_form_vec_at[sc1_form_vec_at != ""]
#   sc1_form_vec_at  <-tail(sc1_form_vec_at,6)
#   sc1_at_numberof_wins <- length(which(sc1_form_vec_at == "W"))
#   sc1_at_numberof_draws <- length(which(sc1_form_vec_at == "D"))
#   sc1_at_numberof_loss <- length(which(sc1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sc1_goalscored_vec_ht <- as.vector(sc1_goalscored_h[sc1_hometeamindex,])
#   sc1_goalscored_vec_ht[is.na(sc1_goalscored_vec_ht)] <- ""
#   sc1_goalscored_vec_ht <- sc1_goalscored_vec_ht[sc1_goalscored_vec_ht != ""]
#   sc1_goalscored_vec_ht  <-tail(sc1_goalscored_vec_ht,6)
#   sc1_goalscored_vec_ht  <- as.numeric(sc1_goalscored_vec_ht)
#   sc1_ht_totalgoalscored <- sum(sc1_goalscored_vec_ht)
#   sc1_ht_matches_scoring <- length(which(sc1_goalscored_vec_ht > 0))
#   sc1_ht_matches_without_scoring <- length(which(sc1_goalscored_vec_ht == "0"))
#   #awayteam
#   sc1_goalscored_vec_at <- as.vector(sc1_goalscored_h[sc1_awayteamindex,])
#   sc1_goalscored_vec_at[is.na(sc1_goalscored_vec_at)] <- ""
#   sc1_goalscored_vec_at <- sc1_goalscored_vec_at[sc1_goalscored_vec_at != ""]
#   sc1_goalscored_vec_at  <-tail(sc1_goalscored_vec_at,6)
#   sc1_goalscored_vec_at  <- as.numeric(sc1_goalscored_vec_at)
#   sc1_at_totalgoalscored <- sum(sc1_goalscored_vec_at)
#   sc1_at_matches_scoring <- length(which(sc1_goalscored_vec_at > 0))
#   sc1_at_matches_without_scoring <- length(which(sc1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sc1_goalconceded_vec_ht <- as.vector(sc1_goalconceded_h[sc1_hometeamindex,])
#   sc1_goalconceded_vec_ht[is.na(sc1_goalconceded_vec_ht)] <- ""
#   sc1_goalconceded_vec_ht <- sc1_goalconceded_vec_ht[sc1_goalconceded_vec_ht != ""]
#   sc1_goalconceded_vec_ht  <-tail(sc1_goalconceded_vec_ht,6)
#   sc1_goalconceded_vec_ht  <- as.numeric(sc1_goalconceded_vec_ht)
#   sc1_goalconceded_vec_ht
#   sc1_ht_totalgoalconceded <- sum(sc1_goalconceded_vec_ht)
#   sc1_ht_matches_concede <- length(which(sc1_goalconceded_vec_ht > 0))
#   sc1_ht_matches_without_concede <- length(which(sc1_goalconceded_vec_ht == "0"))
#   #awayteam
#   sc1_goalconceded_vec_at <- as.vector(sc1_goalconceded_h[sc1_awayteamindex,])
#   sc1_goalconceded_vec_at[is.na(sc1_goalconceded_vec_at)] <- ""
#   sc1_goalconceded_vec_at <- sc1_goalconceded_vec_at[sc1_goalconceded_vec_at != ""]
#   sc1_goalconceded_vec_at  <-tail(sc1_goalconceded_vec_at,6)
#   sc1_goalconceded_vec_at  <- as.numeric(sc1_goalconceded_vec_at)
#   sc1_at_totalgoalconceded <- sum(sc1_goalconceded_vec_at)
#   sc1_at_matches_concede <- length(which(sc1_goalconceded_vec_at > 0))
#   sc1_at_matches_without_concede <- length(which(sc1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sc1_totalgoals_vec_ht <- as.vector(sc1_totalgoals_h[sc1_hometeamindex,])
#   sc1_totalgoals_vec_ht[is.na(sc1_totalgoals_vec_ht)] <- ""
#   sc1_totalgoals_vec_ht <- sc1_totalgoals_vec_ht[sc1_totalgoals_vec_ht != ""]
#   sc1_totalgoals_vec_ht  <-tail(sc1_totalgoals_vec_ht,6)
#   sc1_totalgoals_vec_ht  <- as.numeric(sc1_totalgoals_vec_ht)
#   sc1_totalgoals_vec_ht
#   sc1_ht_totalgoals <- sum(sc1_totalgoals_vec_ht)
#   sc1_ht_avgtotalgoals <- (sc1_ht_totalgoals/6)
#   sc1_ht_no_of_ov25 <- length(which(sc1_totalgoals_vec_ht >= 3))
#   sc1_ht_no_of_un25 <- length(which(sc1_totalgoals_vec_ht <= 2))
#   #awayteam
#   sc1_totalgoals_vec_at <- as.vector(sc1_totalgoals_h[sc1_awayteamindex,])
#   sc1_totalgoals_vec_at[is.na(sc1_totalgoals_vec_at)] <- ""
#   sc1_totalgoals_vec_at <- sc1_totalgoals_vec_at[sc1_totalgoals_vec_at != ""]
#   sc1_totalgoals_vec_at  <-tail(sc1_totalgoals_vec_at,6)
#   sc1_totalgoals_vec_at  <- as.numeric(sc1_totalgoals_vec_at)
#   sc1_totalgoals_vec_at
#   sc1_at_totalgoals <- sum(sc1_totalgoals_vec_at)
#   sc1_at_avgtotalgoals <- (sc1_at_totalgoals/6)
#   sc1_at_no_of_ov25 <- length(which(sc1_totalgoals_vec_at >= 3))
#   sc1_at_no_of_un25 <- length(which(sc1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sc1_winmargin_vec_ht <- as.vector(sc1_winmargin_h[sc1_hometeamindex,])
#   sc1_winmargin_vec_ht[is.na(sc1_winmargin_vec_ht)] <- ""
#   sc1_winmargin_vec_ht <- sc1_winmargin_vec_ht[sc1_winmargin_vec_ht != ""]
#   sc1_winmargin_vec_ht  <-tail(sc1_winmargin_vec_ht,6)
#   sc1_winmargin_vec_ht  <- as.numeric(sc1_winmargin_vec_ht)
#
#   sc1_ht_totalwinmargin <- sum(sc1_winmargin_vec_ht)
#   sc1_ht_no_of_winmargin_ov0 <- length(which(sc1_winmargin_vec_ht >= 0))
#   sc1_ht_no_of_winmargin_ov1 <- length(which(sc1_winmargin_vec_ht >= 1))
#   sc1_ht_no_of_winmargin_un0 <- length(which(sc1_winmargin_vec_ht <= 0))
#   sc1_ht_no_of_winmargin_un1 <- length(which(sc1_winmargin_vec_ht <= 1))
#   #awayteam
#   sc1_winmargin_vec_at <- as.vector(sc1_winmargin_h[sc1_awayteamindex,])
#   sc1_winmargin_vec_at[is.na(sc1_winmargin_vec_at)] <- ""
#   sc1_winmargin_vec_at <- sc1_winmargin_vec_at[sc1_winmargin_vec_at != ""]
#   sc1_winmargin_vec_at  <-tail(sc1_winmargin_vec_at,6)
#   sc1_winmargin_vec_at  <- as.numeric(sc1_winmargin_vec_at)
#
#   sc1_at_totalwinmargin <- sum(sc1_winmargin_vec_at)
#   sc1_at_no_of_winmargin_ov0 <- length(which(sc1_winmargin_vec_at >= 0))
#   sc1_at_no_of_winmargin_ov1 <- length(which(sc1_winmargin_vec_at >= 1))
#   sc1_at_no_of_winmargin_un0 <- length(which(sc1_winmargin_vec_at <= 0))
#   sc1_at_no_of_winmargin_un1 <- length(which(sc1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sc1_winmargin_vec_ht_lm <- as.vector(sc1_winmargin_h[sc1_hometeamindex,])
#   sc1_winmargin_vec_ht_lm[is.na(sc1_winmargin_vec_ht_lm)] <- ""
#   sc1_winmargin_vec_ht_lm <- sc1_winmargin_vec_ht_lm[sc1_winmargin_vec_ht_lm != ""]
#   sc1_winmargin_vec_ht_lm  <-tail(sc1_winmargin_vec_ht_lm,1)
#   #awayteam
#   sc1_winmargin_vec_at_lm <- as.vector(sc1_winmargin_h[sc1_awayteamindex,])
#   sc1_winmargin_vec_at_lm[is.na(sc1_winmargin_vec_at_lm)] <- ""
#   sc1_winmargin_vec_at_lm <- sc1_winmargin_vec_at_lm[sc1_winmargin_vec_at_lm != ""]
#   sc1_winmargin_vec_at_lm  <-tail(sc1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sc1_yellowtotals_vec_ht <- as.vector(sc1_yellowtotalsv2[sc1_hometeamindex,])
#   sc1_yellowtotals_vec_ht[is.na(sc1_yellowtotals_vec_ht)] <- ""
#   sc1_yellowtotals_vec_ht <- sc1_yellowtotals_vec_ht[sc1_yellowtotals_vec_ht != ""]
#   sc1_yellowtotals_vec_ht  <-tail(sc1_yellowtotals_vec_ht,1)
#   #awayteam
#   sc1_yellowtotals_vec_at <- as.vector(sc1_yellowtotalsv2[sc1_awayteamindex,])
#   sc1_yellowtotals_vec_at[is.na(sc1_yellowtotals_vec_at)] <- ""
#   sc1_yellowtotals_vec_at <- sc1_yellowtotals_vec_at[sc1_yellowtotals_vec_at != ""]
#   sc1_yellowtotals_vec_at  <-tail(sc1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sc1_cornertotals_vec_ht <- as.vector(sc1_cornertotalsv2[sc1_hometeamindex,])
#   sc1_cornertotals_vec_ht[is.na(sc1_cornertotals_vec_ht)] <- ""
#   sc1_cornertotals_vec_ht <- sc1_cornertotals_vec_ht[sc1_cornertotals_vec_ht != ""]
#   sc1_cornertotals_vec_ht  <-tail(sc1_cornertotals_vec_ht,1)
#   #awayteam
#   sc1_cornertotals_vec_at <- as.vector(sc1_cornertotalsv2[sc1_awayteamindex,])
#   sc1_cornertotals_vec_at[is.na(sc1_cornertotals_vec_at)] <- ""
#   sc1_cornertotals_vec_at <- sc1_cornertotals_vec_at[sc1_cornertotals_vec_at != ""]
#   sc1_cornertotals_vec_at  <-tail(sc1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sc1_xshotsconversion_vec_ht <- as.vector(sc1_shots_analysis[sc1_hometeamindex,])
#   sc1_xshotsconversion_vec_ht[is.na(sc1_xshotsconversion_vec_ht)] <- ""
#   sc1_xshotsconversion_vec_ht <- sc1_xshotsconversion_vec_ht[sc1_xshotsconversion_vec_ht != ""]
#   sc1_xshotsconversion_vec_ht  <-tail(sc1_xshotsconversion_vec_ht,1)
#   #awayteam
#   sc1_xshotsconversion_vec_at <- as.vector(sc1_shots_analysis[sc1_awayteamindex,])
#   sc1_xshotsconversion_vec_at[is.na(sc1_xshotsconversion_vec_at)] <- ""
#   sc1_xshotsconversion_vec_at <- sc1_xshotsconversion_vec_at[sc1_xshotsconversion_vec_at != ""]
#   sc1_xshotsconversion_vec_at  <-tail(sc1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sc1_fouls_conversion_vec_ht <- as.vector(sc1_fouls_conversion[sc1_hometeamindex,])
#   sc1_fouls_conversion_vec_ht[is.na(sc1_fouls_conversion_vec_ht)] <- ""
#   sc1_fouls_conversion_vec_ht <- sc1_fouls_conversion_vec_ht[sc1_fouls_conversion_vec_ht != ""]
#   sc1_fouls_conversion_vec_ht  <-tail(sc1_fouls_conversion_vec_ht,1)
#   #awayteam
#   sc1_fouls_conversion_vec_at <- as.vector(sc1_fouls_conversion[sc1_awayteamindex,])
#   sc1_fouls_conversion_vec_at[is.na(sc1_fouls_conversion_vec_at)] <- ""
#   sc1_fouls_conversion_vec_at <- sc1_fouls_conversion_vec_at[sc1_fouls_conversion_vec_at != ""]
#   sc1_fouls_conversion_vec_at  <-tail(sc1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sc1_ht_last6points <- sc1_ht_numberof_wins*3 + sc1_ht_numberof_draws*1
#   sc1_at_last6points <- sc1_at_numberof_wins*3 + sc1_at_numberof_draws*1
#
#   if(sc1_ht_last6points > sc1_at_last6points) {sc1_3waypick <- "1"}  else {sc1_3waypick <- "X2"}
#
#   if(sc1_at_last6points > sc1_ht_last6points ) {sc1_3waypick <- "2"} else {sc1_3waypick <- "1X"}
#
#   if(sc1_ht_no_of_ov25 + sc1_at_no_of_ov25 >= 6) {sc1_goalspick <- "ov25"} else {sc1_goalspick <- "un25"}
#
#   if(sc1_ht_no_of_un25 + sc1_at_no_of_un25 >= 6) {sc1_goalspick <- "un25"} else {sc1_goalspick <- "ov25"}
#
#   if(sc1_ht_matches_scoring >= 4 && sc1_at_matches_scoring >=4) {sc1_btts <- "BTTS-Y"} else {sc1_btts <- "BTTS-N"}
#
#
#   sc1_prediction[sc1_row] <- rbind(paste(sc1_3waypick,sc1_goalspick,sc1_btts,sep = ","))
#   sc1_HWM[sc1_row] <- sc1_ht_totalwinmargin
#   sc1_AWM[sc1_row] <- sc1_at_totalwinmargin
#
#   sc1_HWMLM[sc1_row] <- sc1_winmargin_vec_ht_lm
#   sc1_AWMLM[sc1_row] <- sc1_winmargin_vec_at_lm
#
#   sc1_HY[sc1_row] <- sc1_yellowtotals_vec_ht
#   sc1_AY[sc1_row] <- sc1_yellowtotals_vec_at
#
#   sc1_HCO[sc1_row] <- sc1_cornertotals_vec_ht
#   sc1_ACO[sc1_row] <- sc1_cornertotals_vec_at
#
#   sc1_HXSC[sc1_row] <- sc1_xshotsconversion_vec_ht
#   sc1_AXSC[sc1_row] <- sc1_xshotsconversion_vec_at
#
#   sc1_HYCPF[sc1_row] <- sc1_fouls_conversion_vec_ht
#   sc1_AYCPF[sc1_row] <- sc1_fouls_conversion_vec_at
# }
#
# sc1_prediction <- as.data.frame(sc1_prediction)
# colnames(sc1_prediction) <- "prediction"
#
# sc1_HWM <- as.data.frame(sc1_HWM)
# colnames(sc1_HWM) <- "HWM"
#
# sc1_AWM <- as.data.frame(sc1_AWM)
# colnames(sc1_AWM) <- "AWM"
#
# sc1_HWMLM <- as.data.frame(sc1_HWMLM)
# colnames(sc1_HWMLM) <- "HWMLM"
#
# sc1_AWMLM <- as.data.frame(sc1_AWMLM)
# colnames(sc1_AWMLM) <- "AWMLM"
#
# sc1_HY <- as.data.frame(sc1_HY)
# colnames(sc1_HY) <- "AVGHY"
#
# sc1_AY <- as.data.frame(sc1_AY)
# colnames(sc1_AY) <- "AVGAY"
#
# sc1_HCO <- as.data.frame(sc1_HCO)
# colnames(sc1_HCO) <- "AVGHCO"
#
# sc1_ACO <- as.data.frame(sc1_ACO)
# colnames(sc1_ACO) <- "AVGACO"
#
# sc1_HXSC <- as.data.frame(sc1_HXSC)
# colnames(sc1_HXSC) <- "HXSC"
#
# sc1_AXSC <- as.data.frame(sc1_AXSC)
# colnames(sc1_AXSC) <- "AXSC"
#
# sc1_HYCPF <- as.data.frame(sc1_HYCPF)
# colnames(sc1_HYCPF) <- "HYCPF"
#
# sc1_AYCPF <- as.data.frame(sc1_AYCPF)
# colnames(sc1_AYCPF) <- "AYCPF"
#
# sc1_picks <- cbind(SC1_fixtures$Div,SC1_fixtures$HomeTeam_sc1,SC1_fixtures$AwayTeam_sc1,sc1_prediction,sc1_HWM,sc1_AWM,sc1_HWMLM,sc1_AWMLM,sc1_HY,sc1_AY,sc1_HCO,sc1_ACO,sc1_HXSC,sc1_AXSC,sc1_HYCPF,sc1_AYCPF)
#
# colnames(sc1_picks)[1] <- "picks_Div"
# colnames(sc1_picks)[2] <- "picks_HomeTeam"
# colnames(sc1_picks)[3] <- "picks_AwayTeam"
# sc1_picks$matchid <- paste(sc1_picks$picks_HomeTeam,sc1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SC1
# sc1_picks
# ############################################################################################
# #SC2
# SC2_fixtures$Hometeam_sc2_index <- match(SC2_fixtures$HomeTeam_sc2,sc2_teams)
# SC2_fixtures$Awayteam_sc2_index <- match(SC2_fixtures$AwayTeam_sc2,sc2_teams)
# sc2_prediction <- c()
# sc2_HWM <- c()
# sc2_AWM <- c()
# sc2_HWMLM <- c()
# sc2_AWMLM <- c()
# sc2_HY <- c()
# sc2_AY <- c()
# sc2_HCO <- c()
# sc2_ACO <- c()
# sc2_HXSC <- c()
# sc2_AXSC <- c()
# sc2_HYCPF <- c()
# sc2_AYCPF <- c()
# for(sc2_row in 1:nrow(SC2_fixtures))
# {
#
#   sc2_hometeamindex <- SC2_fixtures[sc2_row,"Hometeam_sc2_index"]
#   sc2_awayteamindex <- SC2_fixtures[sc2_row,"Awayteam_sc2_index"]
#   #analyse team form
#   #home team
#   sc2_form_vec_ht <- as.vector(sc2_form_h[sc2_hometeamindex,])
#   sc2_form_vec_ht[is.na(sc2_form_vec_ht)] <- ""
#   sc2_form_vec_ht <- sc2_form_vec_ht[sc2_form_vec_ht != ""]
#   sc2_form_vec_ht  <-tail(sc2_form_vec_ht,6)
#   sc2_ht_numberof_wins <- length(which(sc2_form_vec_ht == "W"))
#   sc2_ht_numberof_draws <- length(which(sc2_form_vec_ht == "D"))
#   sc2_ht_numberof_loss <- length(which(sc2_form_vec_ht == "L"))
#   #awayteam
#   sc2_form_vec_at <- as.vector(sc2_form_h[sc2_awayteamindex,])
#   sc2_form_vec_at[is.na(sc2_form_vec_at)] <- ""
#   sc2_form_vec_at <- sc2_form_vec_at[sc2_form_vec_at != ""]
#   sc2_form_vec_at  <-tail(sc2_form_vec_at,6)
#   sc2_at_numberof_wins <- length(which(sc2_form_vec_at == "W"))
#   sc2_at_numberof_draws <- length(which(sc2_form_vec_at == "D"))
#   sc2_at_numberof_loss <- length(which(sc2_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sc2_goalscored_vec_ht <- as.vector(sc2_goalscored_h[sc2_hometeamindex,])
#   sc2_goalscored_vec_ht[is.na(sc2_goalscored_vec_ht)] <- ""
#   sc2_goalscored_vec_ht <- sc2_goalscored_vec_ht[sc2_goalscored_vec_ht != ""]
#   sc2_goalscored_vec_ht  <-tail(sc2_goalscored_vec_ht,6)
#   sc2_goalscored_vec_ht  <- as.numeric(sc2_goalscored_vec_ht)
#   sc2_ht_totalgoalscored <- sum(sc2_goalscored_vec_ht)
#   sc2_ht_matches_scoring <- length(which(sc2_goalscored_vec_ht > 0))
#   sc2_ht_matches_without_scoring <- length(which(sc2_goalscored_vec_ht == "0"))
#   #awayteam
#   sc2_goalscored_vec_at <- as.vector(sc2_goalscored_h[sc2_awayteamindex,])
#   sc2_goalscored_vec_at[is.na(sc2_goalscored_vec_at)] <- ""
#   sc2_goalscored_vec_at <- sc2_goalscored_vec_at[sc2_goalscored_vec_at != ""]
#   sc2_goalscored_vec_at  <-tail(sc2_goalscored_vec_at,6)
#   sc2_goalscored_vec_at  <- as.numeric(sc2_goalscored_vec_at)
#   sc2_at_totalgoalscored <- sum(sc2_goalscored_vec_at)
#   sc2_at_matches_scoring <- length(which(sc2_goalscored_vec_at > 0))
#   sc2_at_matches_without_scoring <- length(which(sc2_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sc2_goalconceded_vec_ht <- as.vector(sc2_goalconceded_h[sc2_hometeamindex,])
#   sc2_goalconceded_vec_ht[is.na(sc2_goalconceded_vec_ht)] <- ""
#   sc2_goalconceded_vec_ht <- sc2_goalconceded_vec_ht[sc2_goalconceded_vec_ht != ""]
#   sc2_goalconceded_vec_ht  <-tail(sc2_goalconceded_vec_ht,6)
#   sc2_goalconceded_vec_ht  <- as.numeric(sc2_goalconceded_vec_ht)
#   sc2_goalconceded_vec_ht
#   sc2_ht_totalgoalconceded <- sum(sc2_goalconceded_vec_ht)
#   sc2_ht_matches_concede <- length(which(sc2_goalconceded_vec_ht > 0))
#   sc2_ht_matches_without_concede <- length(which(sc2_goalconceded_vec_ht == "0"))
#   #awayteam
#   sc2_goalconceded_vec_at <- as.vector(sc2_goalconceded_h[sc2_awayteamindex,])
#   sc2_goalconceded_vec_at[is.na(sc2_goalconceded_vec_at)] <- ""
#   sc2_goalconceded_vec_at <- sc2_goalconceded_vec_at[sc2_goalconceded_vec_at != ""]
#   sc2_goalconceded_vec_at  <-tail(sc2_goalconceded_vec_at,6)
#   sc2_goalconceded_vec_at  <- as.numeric(sc2_goalconceded_vec_at)
#   sc2_at_totalgoalconceded <- sum(sc2_goalconceded_vec_at)
#   sc2_at_matches_concede <- length(which(sc2_goalconceded_vec_at > 0))
#   sc2_at_matches_without_concede <- length(which(sc2_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sc2_totalgoals_vec_ht <- as.vector(sc2_totalgoals_h[sc2_hometeamindex,])
#   sc2_totalgoals_vec_ht[is.na(sc2_totalgoals_vec_ht)] <- ""
#   sc2_totalgoals_vec_ht <- sc2_totalgoals_vec_ht[sc2_totalgoals_vec_ht != ""]
#   sc2_totalgoals_vec_ht  <-tail(sc2_totalgoals_vec_ht,6)
#   sc2_totalgoals_vec_ht  <- as.numeric(sc2_totalgoals_vec_ht)
#   sc2_totalgoals_vec_ht
#   sc2_ht_totalgoals <- sum(sc2_totalgoals_vec_ht)
#   sc2_ht_avgtotalgoals <- (sc2_ht_totalgoals/6)
#   sc2_ht_no_of_ov25 <- length(which(sc2_totalgoals_vec_ht >= 3))
#   sc2_ht_no_of_un25 <- length(which(sc2_totalgoals_vec_ht <= 2))
#   #awayteam
#   sc2_totalgoals_vec_at <- as.vector(sc2_totalgoals_h[sc2_awayteamindex,])
#   sc2_totalgoals_vec_at[is.na(sc2_totalgoals_vec_at)] <- ""
#   sc2_totalgoals_vec_at <- sc2_totalgoals_vec_at[sc2_totalgoals_vec_at != ""]
#   sc2_totalgoals_vec_at  <-tail(sc2_totalgoals_vec_at,6)
#   sc2_totalgoals_vec_at  <- as.numeric(sc2_totalgoals_vec_at)
#   sc2_totalgoals_vec_at
#   sc2_at_totalgoals <- sum(sc2_totalgoals_vec_at)
#   sc2_at_avgtotalgoals <- (sc2_at_totalgoals/6)
#   sc2_at_no_of_ov25 <- length(which(sc2_totalgoals_vec_at >= 3))
#   sc2_at_no_of_un25 <- length(which(sc2_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sc2_winmargin_vec_ht <- as.vector(sc2_winmargin_h[sc2_hometeamindex,])
#   sc2_winmargin_vec_ht[is.na(sc2_winmargin_vec_ht)] <- ""
#   sc2_winmargin_vec_ht <- sc2_winmargin_vec_ht[sc2_winmargin_vec_ht != ""]
#   sc2_winmargin_vec_ht  <-tail(sc2_winmargin_vec_ht,6)
#   sc2_winmargin_vec_ht  <- as.numeric(sc2_winmargin_vec_ht)
#
#   sc2_ht_totalwinmargin <- sum(sc2_winmargin_vec_ht)
#   sc2_ht_no_of_winmargin_ov0 <- length(which(sc2_winmargin_vec_ht >= 0))
#   sc2_ht_no_of_winmargin_ov1 <- length(which(sc2_winmargin_vec_ht >= 1))
#   sc2_ht_no_of_winmargin_un0 <- length(which(sc2_winmargin_vec_ht <= 0))
#   sc2_ht_no_of_winmargin_un1 <- length(which(sc2_winmargin_vec_ht <= 1))
#   #awayteam
#   sc2_winmargin_vec_at <- as.vector(sc2_winmargin_h[sc2_awayteamindex,])
#   sc2_winmargin_vec_at[is.na(sc2_winmargin_vec_at)] <- ""
#   sc2_winmargin_vec_at <- sc2_winmargin_vec_at[sc2_winmargin_vec_at != ""]
#   sc2_winmargin_vec_at  <-tail(sc2_winmargin_vec_at,6)
#   sc2_winmargin_vec_at  <- as.numeric(sc2_winmargin_vec_at)
#
#   sc2_at_totalwinmargin <- sum(sc2_winmargin_vec_at)
#   sc2_at_no_of_winmargin_ov0 <- length(which(sc2_winmargin_vec_at >= 0))
#   sc2_at_no_of_winmargin_ov1 <- length(which(sc2_winmargin_vec_at >= 1))
#   sc2_at_no_of_winmargin_un0 <- length(which(sc2_winmargin_vec_at <= 0))
#   sc2_at_no_of_winmargin_un1 <- length(which(sc2_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sc2_winmargin_vec_ht_lm <- as.vector(sc2_winmargin_h[sc2_hometeamindex,])
#   sc2_winmargin_vec_ht_lm[is.na(sc2_winmargin_vec_ht_lm)] <- ""
#   sc2_winmargin_vec_ht_lm <- sc2_winmargin_vec_ht_lm[sc2_winmargin_vec_ht_lm != ""]
#   sc2_winmargin_vec_ht_lm  <-tail(sc2_winmargin_vec_ht_lm,1)
#   #awayteam
#   sc2_winmargin_vec_at_lm <- as.vector(sc2_winmargin_h[sc2_awayteamindex,])
#   sc2_winmargin_vec_at_lm[is.na(sc2_winmargin_vec_at_lm)] <- ""
#   sc2_winmargin_vec_at_lm <- sc2_winmargin_vec_at_lm[sc2_winmargin_vec_at_lm != ""]
#   sc2_winmargin_vec_at_lm  <-tail(sc2_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sc2_yellowtotals_vec_ht <- as.vector(sc2_yellowtotalsv2[sc2_hometeamindex,])
#   sc2_yellowtotals_vec_ht[is.na(sc2_yellowtotals_vec_ht)] <- ""
#   sc2_yellowtotals_vec_ht <- sc2_yellowtotals_vec_ht[sc2_yellowtotals_vec_ht != ""]
#   sc2_yellowtotals_vec_ht  <-tail(sc2_yellowtotals_vec_ht,1)
#   #awayteam
#   sc2_yellowtotals_vec_at <- as.vector(sc2_yellowtotalsv2[sc2_awayteamindex,])
#   sc2_yellowtotals_vec_at[is.na(sc2_yellowtotals_vec_at)] <- ""
#   sc2_yellowtotals_vec_at <- sc2_yellowtotals_vec_at[sc2_yellowtotals_vec_at != ""]
#   sc2_yellowtotals_vec_at  <-tail(sc2_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sc2_cornertotals_vec_ht <- as.vector(sc2_cornertotalsv2[sc2_hometeamindex,])
#   sc2_cornertotals_vec_ht[is.na(sc2_cornertotals_vec_ht)] <- ""
#   sc2_cornertotals_vec_ht <- sc2_cornertotals_vec_ht[sc2_cornertotals_vec_ht != ""]
#   sc2_cornertotals_vec_ht  <-tail(sc2_cornertotals_vec_ht,1)
#   #awayteam
#   sc2_cornertotals_vec_at <- as.vector(sc2_cornertotalsv2[sc2_awayteamindex,])
#   sc2_cornertotals_vec_at[is.na(sc2_cornertotals_vec_at)] <- ""
#   sc2_cornertotals_vec_at <- sc2_cornertotals_vec_at[sc2_cornertotals_vec_at != ""]
#   sc2_cornertotals_vec_at  <-tail(sc2_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sc2_xshotsconversion_vec_ht <- as.vector(sc2_shots_analysis[sc2_hometeamindex,])
#   sc2_xshotsconversion_vec_ht[is.na(sc2_xshotsconversion_vec_ht)] <- ""
#   sc2_xshotsconversion_vec_ht <- sc2_xshotsconversion_vec_ht[sc2_xshotsconversion_vec_ht != ""]
#   sc2_xshotsconversion_vec_ht  <-tail(sc2_xshotsconversion_vec_ht,1)
#   #awayteam
#   sc2_xshotsconversion_vec_at <- as.vector(sc2_shots_analysis[sc2_awayteamindex,])
#   sc2_xshotsconversion_vec_at[is.na(sc2_xshotsconversion_vec_at)] <- ""
#   sc2_xshotsconversion_vec_at <- sc2_xshotsconversion_vec_at[sc2_xshotsconversion_vec_at != ""]
#   sc2_xshotsconversion_vec_at  <-tail(sc2_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sc2_fouls_conversion_vec_ht <- as.vector(sc2_fouls_conversion[sc2_hometeamindex,])
#   sc2_fouls_conversion_vec_ht[is.na(sc2_fouls_conversion_vec_ht)] <- ""
#   sc2_fouls_conversion_vec_ht <- sc2_fouls_conversion_vec_ht[sc2_fouls_conversion_vec_ht != ""]
#   sc2_fouls_conversion_vec_ht  <-tail(sc2_fouls_conversion_vec_ht,1)
#   #awayteam
#   sc2_fouls_conversion_vec_at <- as.vector(sc2_fouls_conversion[sc2_awayteamindex,])
#   sc2_fouls_conversion_vec_at[is.na(sc2_fouls_conversion_vec_at)] <- ""
#   sc2_fouls_conversion_vec_at <- sc2_fouls_conversion_vec_at[sc2_fouls_conversion_vec_at != ""]
#   sc2_fouls_conversion_vec_at  <-tail(sc2_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sc2_ht_last6points <- sc2_ht_numberof_wins*3 + sc2_ht_numberof_draws*1
#   sc2_at_last6points <- sc2_at_numberof_wins*3 + sc2_at_numberof_draws*1
#
#   if(sc2_ht_last6points > sc2_at_last6points) {sc2_3waypick <- "1"}  else {sc2_3waypick <- "X2"}
#
#   if(sc2_at_last6points > sc2_ht_last6points ) {sc2_3waypick <- "2"} else {sc2_3waypick <- "1X"}
#
#   if(sc2_ht_no_of_ov25 + sc2_at_no_of_ov25 >= 6) {sc2_goalspick <- "ov25"} else {sc2_goalspick <- "un25"}
#
#   if(sc2_ht_no_of_un25 + sc2_at_no_of_un25 >= 6) {sc2_goalspick <- "un25"} else {sc2_goalspick <- "ov25"}
#
#   if(sc2_ht_matches_scoring >= 4 && sc2_at_matches_scoring >=4) {sc2_btts <- "BTTS-Y"} else {sc2_btts <- "BTTS-N"}
#
#
#   sc2_prediction[sc2_row] <- rbind(paste(sc2_3waypick,sc2_goalspick,sc2_btts,sep = ","))
#   sc2_HWM[sc2_row] <- sc2_ht_totalwinmargin
#   sc2_AWM[sc2_row] <- sc2_at_totalwinmargin
#
#   sc2_HWMLM[sc2_row] <- sc2_winmargin_vec_ht_lm
#   sc2_AWMLM[sc2_row] <- sc2_winmargin_vec_at_lm
#
#   sc2_HY[sc2_row] <- sc2_yellowtotals_vec_ht
#   sc2_AY[sc2_row] <- sc2_yellowtotals_vec_at
#
#   sc2_HCO[sc2_row] <- sc2_cornertotals_vec_ht
#   sc2_ACO[sc2_row] <- sc2_cornertotals_vec_at
#
#   sc2_HXSC[sc2_row] <- sc2_xshotsconversion_vec_ht
#   sc2_AXSC[sc2_row] <- sc2_xshotsconversion_vec_at
#
#   sc2_HYCPF[sc2_row] <- sc2_fouls_conversion_vec_ht
#   sc2_AYCPF[sc2_row] <- sc2_fouls_conversion_vec_at
# }
#
# sc2_prediction <- as.data.frame(sc2_prediction)
# colnames(sc2_prediction) <- "prediction"
#
# sc2_HWM <- as.data.frame(sc2_HWM)
# colnames(sc2_HWM) <- "HWM"
#
# sc2_AWM <- as.data.frame(sc2_AWM)
# colnames(sc2_AWM) <- "AWM"
#
# sc2_HWMLM <- as.data.frame(sc2_HWMLM)
# colnames(sc2_HWMLM) <- "HWMLM"
#
# sc2_AWMLM <- as.data.frame(sc2_AWMLM)
# colnames(sc2_AWMLM) <- "AWMLM"
#
# sc2_HY <- as.data.frame(sc2_HY)
# colnames(sc2_HY) <- "AVGHY"
#
# sc2_AY <- as.data.frame(sc2_AY)
# colnames(sc2_AY) <- "AVGAY"
#
# sc2_HCO <- as.data.frame(sc2_HCO)
# colnames(sc2_HCO) <- "AVGHCO"
#
# sc2_ACO <- as.data.frame(sc2_ACO)
# colnames(sc2_ACO) <- "AVGACO"
#
# sc2_HXSC <- as.data.frame(sc2_HXSC)
# colnames(sc2_HXSC) <- "HXSC"
#
# sc2_AXSC <- as.data.frame(sc2_AXSC)
# colnames(sc2_AXSC) <- "AXSC"
#
# sc2_HYCPF <- as.data.frame(sc2_HYCPF)
# colnames(sc2_HYCPF) <- "HYCPF"
#
# sc2_AYCPF <- as.data.frame(sc2_AYCPF)
# colnames(sc2_AYCPF) <- "AYCPF"
#
# sc2_picks <- cbind(SC2_fixtures$Div,SC2_fixtures$HomeTeam_sc2,SC2_fixtures$AwayTeam_sc2,sc2_prediction,sc2_HWM,sc2_AWM,sc2_HWMLM,sc2_AWMLM,sc2_HY,sc2_AY,sc2_HCO,sc2_ACO,sc2_HXSC,sc2_AXSC,sc2_HYCPF,sc2_AYCPF)
#
# colnames(sc2_picks)[1] <- "picks_Div"
# colnames(sc2_picks)[2] <- "picks_HomeTeam"
# colnames(sc2_picks)[3] <- "picks_AwayTeam"
# sc2_picks$matchid <- paste(sc2_picks$picks_HomeTeam,sc2_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SC2
# sc2_picks
# ############################################################################################
# #SC3
# SC3_fixtures$Hometeam_sc3_index <- match(SC3_fixtures$HomeTeam_sc3,sc3_teams)
# SC3_fixtures$Awayteam_sc3_index <- match(SC3_fixtures$AwayTeam_sc3,sc3_teams)
# sc3_prediction <- c()
# sc3_HWM <- c()
# sc3_AWM <- c()
# sc3_HWMLM <- c()
# sc3_AWMLM <- c()
# sc3_HY <- c()
# sc3_AY <- c()
# sc3_HCO <- c()
# sc3_ACO <- c()
# sc3_HXSC <- c()
# sc3_AXSC <- c()
# sc3_HYCPF <- c()
# sc3_AYCPF <- c()
# for(sc3_row in 1:nrow(SC3_fixtures))
# {
#
#   sc3_hometeamindex <- SC3_fixtures[sc3_row,"Hometeam_sc3_index"]
#   sc3_awayteamindex <- SC3_fixtures[sc3_row,"Awayteam_sc3_index"]
#   #analyse team form
#   #home team
#   sc3_form_vec_ht <- as.vector(sc3_form_h[sc3_hometeamindex,])
#   sc3_form_vec_ht[is.na(sc3_form_vec_ht)] <- ""
#   sc3_form_vec_ht <- sc3_form_vec_ht[sc3_form_vec_ht != ""]
#   sc3_form_vec_ht  <-tail(sc3_form_vec_ht,6)
#   sc3_ht_numberof_wins <- length(which(sc3_form_vec_ht == "W"))
#   sc3_ht_numberof_draws <- length(which(sc3_form_vec_ht == "D"))
#   sc3_ht_numberof_loss <- length(which(sc3_form_vec_ht == "L"))
#   #awayteam
#   sc3_form_vec_at <- as.vector(sc3_form_h[sc3_awayteamindex,])
#   sc3_form_vec_at[is.na(sc3_form_vec_at)] <- ""
#   sc3_form_vec_at <- sc3_form_vec_at[sc3_form_vec_at != ""]
#   sc3_form_vec_at  <-tail(sc3_form_vec_at,6)
#   sc3_at_numberof_wins <- length(which(sc3_form_vec_at == "W"))
#   sc3_at_numberof_draws <- length(which(sc3_form_vec_at == "D"))
#   sc3_at_numberof_loss <- length(which(sc3_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   sc3_goalscored_vec_ht <- as.vector(sc3_goalscored_h[sc3_hometeamindex,])
#   sc3_goalscored_vec_ht[is.na(sc3_goalscored_vec_ht)] <- ""
#   sc3_goalscored_vec_ht <- sc3_goalscored_vec_ht[sc3_goalscored_vec_ht != ""]
#   sc3_goalscored_vec_ht  <-tail(sc3_goalscored_vec_ht,6)
#   sc3_goalscored_vec_ht  <- as.numeric(sc3_goalscored_vec_ht)
#   sc3_ht_totalgoalscored <- sum(sc3_goalscored_vec_ht)
#   sc3_ht_matches_scoring <- length(which(sc3_goalscored_vec_ht > 0))
#   sc3_ht_matches_without_scoring <- length(which(sc3_goalscored_vec_ht == "0"))
#   #awayteam
#   sc3_goalscored_vec_at <- as.vector(sc3_goalscored_h[sc3_awayteamindex,])
#   sc3_goalscored_vec_at[is.na(sc3_goalscored_vec_at)] <- ""
#   sc3_goalscored_vec_at <- sc3_goalscored_vec_at[sc3_goalscored_vec_at != ""]
#   sc3_goalscored_vec_at  <-tail(sc3_goalscored_vec_at,6)
#   sc3_goalscored_vec_at  <- as.numeric(sc3_goalscored_vec_at)
#   sc3_at_totalgoalscored <- sum(sc3_goalscored_vec_at)
#   sc3_at_matches_scoring <- length(which(sc3_goalscored_vec_at > 0))
#   sc3_at_matches_without_scoring <- length(which(sc3_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   sc3_goalconceded_vec_ht <- as.vector(sc3_goalconceded_h[sc3_hometeamindex,])
#   sc3_goalconceded_vec_ht[is.na(sc3_goalconceded_vec_ht)] <- ""
#   sc3_goalconceded_vec_ht <- sc3_goalconceded_vec_ht[sc3_goalconceded_vec_ht != ""]
#   sc3_goalconceded_vec_ht  <-tail(sc3_goalconceded_vec_ht,6)
#   sc3_goalconceded_vec_ht  <- as.numeric(sc3_goalconceded_vec_ht)
#   sc3_goalconceded_vec_ht
#   sc3_ht_totalgoalconceded <- sum(sc3_goalconceded_vec_ht)
#   sc3_ht_matches_concede <- length(which(sc3_goalconceded_vec_ht > 0))
#   sc3_ht_matches_without_concede <- length(which(sc3_goalconceded_vec_ht == "0"))
#   #awayteam
#   sc3_goalconceded_vec_at <- as.vector(sc3_goalconceded_h[sc3_awayteamindex,])
#   sc3_goalconceded_vec_at[is.na(sc3_goalconceded_vec_at)] <- ""
#   sc3_goalconceded_vec_at <- sc3_goalconceded_vec_at[sc3_goalconceded_vec_at != ""]
#   sc3_goalconceded_vec_at  <-tail(sc3_goalconceded_vec_at,6)
#   sc3_goalconceded_vec_at  <- as.numeric(sc3_goalconceded_vec_at)
#   sc3_at_totalgoalconceded <- sum(sc3_goalconceded_vec_at)
#   sc3_at_matches_concede <- length(which(sc3_goalconceded_vec_at > 0))
#   sc3_at_matches_without_concede <- length(which(sc3_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   sc3_totalgoals_vec_ht <- as.vector(sc3_totalgoals_h[sc3_hometeamindex,])
#   sc3_totalgoals_vec_ht[is.na(sc3_totalgoals_vec_ht)] <- ""
#   sc3_totalgoals_vec_ht <- sc3_totalgoals_vec_ht[sc3_totalgoals_vec_ht != ""]
#   sc3_totalgoals_vec_ht  <-tail(sc3_totalgoals_vec_ht,6)
#   sc3_totalgoals_vec_ht  <- as.numeric(sc3_totalgoals_vec_ht)
#   sc3_totalgoals_vec_ht
#   sc3_ht_totalgoals <- sum(sc3_totalgoals_vec_ht)
#   sc3_ht_avgtotalgoals <- (sc3_ht_totalgoals/6)
#   sc3_ht_no_of_ov25 <- length(which(sc3_totalgoals_vec_ht >= 3))
#   sc3_ht_no_of_un25 <- length(which(sc3_totalgoals_vec_ht <= 2))
#   #awayteam
#   sc3_totalgoals_vec_at <- as.vector(sc3_totalgoals_h[sc3_awayteamindex,])
#   sc3_totalgoals_vec_at[is.na(sc3_totalgoals_vec_at)] <- ""
#   sc3_totalgoals_vec_at <- sc3_totalgoals_vec_at[sc3_totalgoals_vec_at != ""]
#   sc3_totalgoals_vec_at  <-tail(sc3_totalgoals_vec_at,6)
#   sc3_totalgoals_vec_at  <- as.numeric(sc3_totalgoals_vec_at)
#   sc3_totalgoals_vec_at
#   sc3_at_totalgoals <- sum(sc3_totalgoals_vec_at)
#   sc3_at_avgtotalgoals <- (sc3_at_totalgoals/6)
#   sc3_at_no_of_ov25 <- length(which(sc3_totalgoals_vec_at >= 3))
#   sc3_at_no_of_un25 <- length(which(sc3_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   sc3_winmargin_vec_ht <- as.vector(sc3_winmargin_h[sc3_hometeamindex,])
#   sc3_winmargin_vec_ht[is.na(sc3_winmargin_vec_ht)] <- ""
#   sc3_winmargin_vec_ht <- sc3_winmargin_vec_ht[sc3_winmargin_vec_ht != ""]
#   sc3_winmargin_vec_ht  <-tail(sc3_winmargin_vec_ht,6)
#   sc3_winmargin_vec_ht  <- as.numeric(sc3_winmargin_vec_ht)
#
#   sc3_ht_totalwinmargin <- sum(sc3_winmargin_vec_ht)
#   sc3_ht_no_of_winmargin_ov0 <- length(which(sc3_winmargin_vec_ht >= 0))
#   sc3_ht_no_of_winmargin_ov1 <- length(which(sc3_winmargin_vec_ht >= 1))
#   sc3_ht_no_of_winmargin_un0 <- length(which(sc3_winmargin_vec_ht <= 0))
#   sc3_ht_no_of_winmargin_un1 <- length(which(sc3_winmargin_vec_ht <= 1))
#   #awayteam
#   sc3_winmargin_vec_at <- as.vector(sc3_winmargin_h[sc3_awayteamindex,])
#   sc3_winmargin_vec_at[is.na(sc3_winmargin_vec_at)] <- ""
#   sc3_winmargin_vec_at <- sc3_winmargin_vec_at[sc3_winmargin_vec_at != ""]
#   sc3_winmargin_vec_at  <-tail(sc3_winmargin_vec_at,6)
#   sc3_winmargin_vec_at  <- as.numeric(sc3_winmargin_vec_at)
#
#   sc3_at_totalwinmargin <- sum(sc3_winmargin_vec_at)
#   sc3_at_no_of_winmargin_ov0 <- length(which(sc3_winmargin_vec_at >= 0))
#   sc3_at_no_of_winmargin_ov1 <- length(which(sc3_winmargin_vec_at >= 1))
#   sc3_at_no_of_winmargin_un0 <- length(which(sc3_winmargin_vec_at <= 0))
#   sc3_at_no_of_winmargin_un1 <- length(which(sc3_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   sc3_winmargin_vec_ht_lm <- as.vector(sc3_winmargin_h[sc3_hometeamindex,])
#   sc3_winmargin_vec_ht_lm[is.na(sc3_winmargin_vec_ht_lm)] <- ""
#   sc3_winmargin_vec_ht_lm <- sc3_winmargin_vec_ht_lm[sc3_winmargin_vec_ht_lm != ""]
#   sc3_winmargin_vec_ht_lm  <-tail(sc3_winmargin_vec_ht_lm,1)
#   #awayteam
#   sc3_winmargin_vec_at_lm <- as.vector(sc3_winmargin_h[sc3_awayteamindex,])
#   sc3_winmargin_vec_at_lm[is.na(sc3_winmargin_vec_at_lm)] <- ""
#   sc3_winmargin_vec_at_lm <- sc3_winmargin_vec_at_lm[sc3_winmargin_vec_at_lm != ""]
#   sc3_winmargin_vec_at_lm  <-tail(sc3_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   sc3_yellowtotals_vec_ht <- as.vector(sc3_yellowtotalsv2[sc3_hometeamindex,])
#   sc3_yellowtotals_vec_ht[is.na(sc3_yellowtotals_vec_ht)] <- ""
#   sc3_yellowtotals_vec_ht <- sc3_yellowtotals_vec_ht[sc3_yellowtotals_vec_ht != ""]
#   sc3_yellowtotals_vec_ht  <-tail(sc3_yellowtotals_vec_ht,1)
#   #awayteam
#   sc3_yellowtotals_vec_at <- as.vector(sc3_yellowtotalsv2[sc3_awayteamindex,])
#   sc3_yellowtotals_vec_at[is.na(sc3_yellowtotals_vec_at)] <- ""
#   sc3_yellowtotals_vec_at <- sc3_yellowtotals_vec_at[sc3_yellowtotals_vec_at != ""]
#   sc3_yellowtotals_vec_at  <-tail(sc3_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   sc3_cornertotals_vec_ht <- as.vector(sc3_cornertotalsv2[sc3_hometeamindex,])
#   sc3_cornertotals_vec_ht[is.na(sc3_cornertotals_vec_ht)] <- ""
#   sc3_cornertotals_vec_ht <- sc3_cornertotals_vec_ht[sc3_cornertotals_vec_ht != ""]
#   sc3_cornertotals_vec_ht  <-tail(sc3_cornertotals_vec_ht,1)
#   #awayteam
#   sc3_cornertotals_vec_at <- as.vector(sc3_cornertotalsv2[sc3_awayteamindex,])
#   sc3_cornertotals_vec_at[is.na(sc3_cornertotals_vec_at)] <- ""
#   sc3_cornertotals_vec_at <- sc3_cornertotals_vec_at[sc3_cornertotals_vec_at != ""]
#   sc3_cornertotals_vec_at  <-tail(sc3_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   sc3_xshotsconversion_vec_ht <- as.vector(sc3_shots_analysis[sc3_hometeamindex,])
#   sc3_xshotsconversion_vec_ht[is.na(sc3_xshotsconversion_vec_ht)] <- ""
#   sc3_xshotsconversion_vec_ht <- sc3_xshotsconversion_vec_ht[sc3_xshotsconversion_vec_ht != ""]
#   sc3_xshotsconversion_vec_ht  <-tail(sc3_xshotsconversion_vec_ht,1)
#   #awayteam
#   sc3_xshotsconversion_vec_at <- as.vector(sc3_shots_analysis[sc3_awayteamindex,])
#   sc3_xshotsconversion_vec_at[is.na(sc3_xshotsconversion_vec_at)] <- ""
#   sc3_xshotsconversion_vec_at <- sc3_xshotsconversion_vec_at[sc3_xshotsconversion_vec_at != ""]
#   sc3_xshotsconversion_vec_at  <-tail(sc3_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   sc3_fouls_conversion_vec_ht <- as.vector(sc3_fouls_conversion[sc3_hometeamindex,])
#   sc3_fouls_conversion_vec_ht[is.na(sc3_fouls_conversion_vec_ht)] <- ""
#   sc3_fouls_conversion_vec_ht <- sc3_fouls_conversion_vec_ht[sc3_fouls_conversion_vec_ht != ""]
#   sc3_fouls_conversion_vec_ht  <-tail(sc3_fouls_conversion_vec_ht,1)
#   #awayteam
#   sc3_fouls_conversion_vec_at <- as.vector(sc3_fouls_conversion[sc3_awayteamindex,])
#   sc3_fouls_conversion_vec_at[is.na(sc3_fouls_conversion_vec_at)] <- ""
#   sc3_fouls_conversion_vec_at <- sc3_fouls_conversion_vec_at[sc3_fouls_conversion_vec_at != ""]
#   sc3_fouls_conversion_vec_at  <-tail(sc3_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   sc3_ht_last6points <- sc3_ht_numberof_wins*3 + sc3_ht_numberof_draws*1
#   sc3_at_last6points <- sc3_at_numberof_wins*3 + sc3_at_numberof_draws*1
#
#   if(sc3_ht_last6points > sc3_at_last6points) {sc3_3waypick <- "1"}  else {sc3_3waypick <- "X2"}
#
#   if(sc3_at_last6points > sc3_ht_last6points ) {sc3_3waypick <- "2"} else {sc3_3waypick <- "1X"}
#
#   if(sc3_ht_no_of_ov25 + sc3_at_no_of_ov25 >= 6) {sc3_goalspick <- "ov25"} else {sc3_goalspick <- "un25"}
#
#   if(sc3_ht_no_of_un25 + sc3_at_no_of_un25 >= 6) {sc3_goalspick <- "un25"} else {sc3_goalspick <- "ov25"}
#
#   if(sc3_ht_matches_scoring >= 4 && sc3_at_matches_scoring >=4) {sc3_btts <- "BTTS-Y"} else {sc3_btts <- "BTTS-N"}
#
#
#   sc3_prediction[sc3_row] <- rbind(paste(sc3_3waypick,sc3_goalspick,sc3_btts,sep = ","))
#   sc3_HWM[sc3_row] <- sc3_ht_totalwinmargin
#   sc3_AWM[sc3_row] <- sc3_at_totalwinmargin
#
#   sc3_HWMLM[sc3_row] <- sc3_winmargin_vec_ht_lm
#   sc3_AWMLM[sc3_row] <- sc3_winmargin_vec_at_lm
#
#   sc3_HY[sc3_row] <- sc3_yellowtotals_vec_ht
#   sc3_AY[sc3_row] <- sc3_yellowtotals_vec_at
#
#   sc3_HCO[sc3_row] <- sc3_cornertotals_vec_ht
#   sc3_ACO[sc3_row] <- sc3_cornertotals_vec_at
#
#   sc3_HXSC[sc3_row] <- sc3_xshotsconversion_vec_ht
#   sc3_AXSC[sc3_row] <- sc3_xshotsconversion_vec_at
#
#   sc3_HYCPF[sc3_row] <- sc3_fouls_conversion_vec_ht
#   sc3_AYCPF[sc3_row] <- sc3_fouls_conversion_vec_at
# }
#
# sc3_prediction <- as.data.frame(sc3_prediction)
# colnames(sc3_prediction) <- "prediction"
#
# sc3_HWM <- as.data.frame(sc3_HWM)
# colnames(sc3_HWM) <- "HWM"
#
# sc3_AWM <- as.data.frame(sc3_AWM)
# colnames(sc3_AWM) <- "AWM"
#
# sc3_HWMLM <- as.data.frame(sc3_HWMLM)
# colnames(sc3_HWMLM) <- "HWMLM"
#
# sc3_AWMLM <- as.data.frame(sc3_AWMLM)
# colnames(sc3_AWMLM) <- "AWMLM"
#
# sc3_HY <- as.data.frame(sc3_HY)
# colnames(sc3_HY) <- "AVGHY"
#
# sc3_AY <- as.data.frame(sc3_AY)
# colnames(sc3_AY) <- "AVGAY"
#
# sc3_HCO <- as.data.frame(sc3_HCO)
# colnames(sc3_HCO) <- "AVGHCO"
#
# sc3_ACO <- as.data.frame(sc3_ACO)
# colnames(sc3_ACO) <- "AVGACO"
#
# sc3_HXSC <- as.data.frame(sc3_HXSC)
# colnames(sc3_HXSC) <- "HXSC"
#
# sc3_AXSC <- as.data.frame(sc3_AXSC)
# colnames(sc3_AXSC) <- "AXSC"
#
# sc3_HYCPF <- as.data.frame(sc3_HYCPF)
# colnames(sc3_HYCPF) <- "HYCPF"
#
# sc3_AYCPF <- as.data.frame(sc3_AYCPF)
# colnames(sc3_AYCPF) <- "AYCPF"
#
# sc3_picks <- cbind(SC3_fixtures$Div,SC3_fixtures$HomeTeam_sc3,SC3_fixtures$AwayTeam_sc3,sc3_prediction,sc3_HWM,sc3_AWM,sc3_HWMLM,sc3_AWMLM,sc3_HY,sc3_AY,sc3_HCO,sc3_ACO,sc3_HXSC,sc3_AXSC,sc3_HYCPF,sc3_AYCPF)
#
# colnames(sc3_picks)[1] <- "picks_Div"
# colnames(sc3_picks)[2] <- "picks_HomeTeam"
# colnames(sc3_picks)[3] <- "picks_AwayTeam"
# sc3_picks$matchid <- paste(sc3_picks$picks_HomeTeam,sc3_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of SC3
# sc3_picks
# ############################################################################################
# #T1
# T1_fixtures$Hometeam_t1_index <- match(T1_fixtures$HomeTeam_t1,t1_teams)
# T1_fixtures$Awayteam_t1_index <- match(T1_fixtures$AwayTeam_t1,t1_teams)
# t1_prediction <- c()
# t1_HWM <- c()
# t1_AWM <- c()
# t1_HWMLM <- c()
# t1_AWMLM <- c()
# t1_HY <- c()
# t1_AY <- c()
# t1_HCO <- c()
# t1_ACO <- c()
# t1_HXSC <- c()
# t1_AXSC <- c()
# t1_HYCPF <- c()
# t1_AYCPF <- c()
# for(t1_row in 1:nrow(T1_fixtures))
# {
#
#   t1_hometeamindex <- T1_fixtures[t1_row,"Hometeam_t1_index"]
#   t1_awayteamindex <- T1_fixtures[t1_row,"Awayteam_t1_index"]
#   #analyse team form
#   #home team
#   t1_form_vec_ht <- as.vector(t1_form_h[t1_hometeamindex,])
#   t1_form_vec_ht[is.na(t1_form_vec_ht)] <- ""
#   t1_form_vec_ht <- t1_form_vec_ht[t1_form_vec_ht != ""]
#   t1_form_vec_ht  <-tail(t1_form_vec_ht,6)
#   t1_ht_numberof_wins <- length(which(t1_form_vec_ht == "W"))
#   t1_ht_numberof_draws <- length(which(t1_form_vec_ht == "D"))
#   t1_ht_numberof_loss <- length(which(t1_form_vec_ht == "L"))
#   #awayteam
#   t1_form_vec_at <- as.vector(t1_form_h[t1_awayteamindex,])
#   t1_form_vec_at[is.na(t1_form_vec_at)] <- ""
#   t1_form_vec_at <- t1_form_vec_at[t1_form_vec_at != ""]
#   t1_form_vec_at  <-tail(t1_form_vec_at,6)
#   t1_at_numberof_wins <- length(which(t1_form_vec_at == "W"))
#   t1_at_numberof_draws <- length(which(t1_form_vec_at == "D"))
#   t1_at_numberof_loss <- length(which(t1_form_vec_at == "L"))
#
#   ######################################################################
#   #analyse goals scored
#   #hometeam
#   t1_goalscored_vec_ht <- as.vector(t1_goalscored_h[t1_hometeamindex,])
#   t1_goalscored_vec_ht[is.na(t1_goalscored_vec_ht)] <- ""
#   t1_goalscored_vec_ht <- t1_goalscored_vec_ht[t1_goalscored_vec_ht != ""]
#   t1_goalscored_vec_ht  <-tail(t1_goalscored_vec_ht,6)
#   t1_goalscored_vec_ht  <- as.numeric(t1_goalscored_vec_ht)
#   t1_ht_totalgoalscored <- sum(t1_goalscored_vec_ht)
#   t1_ht_matches_scoring <- length(which(t1_goalscored_vec_ht > 0))
#   t1_ht_matches_without_scoring <- length(which(t1_goalscored_vec_ht == "0"))
#   #awayteam
#   t1_goalscored_vec_at <- as.vector(t1_goalscored_h[t1_awayteamindex,])
#   t1_goalscored_vec_at[is.na(t1_goalscored_vec_at)] <- ""
#   t1_goalscored_vec_at <- t1_goalscored_vec_at[t1_goalscored_vec_at != ""]
#   t1_goalscored_vec_at  <-tail(t1_goalscored_vec_at,6)
#   t1_goalscored_vec_at  <- as.numeric(t1_goalscored_vec_at)
#   t1_at_totalgoalscored <- sum(t1_goalscored_vec_at)
#   t1_at_matches_scoring <- length(which(t1_goalscored_vec_at > 0))
#   t1_at_matches_without_scoring <- length(which(t1_goalscored_vec_at == "0"))
#   #####################################################################################
#   #analyse goals conceded
#   #hometeam
#   t1_goalconceded_vec_ht <- as.vector(t1_goalconceded_h[t1_hometeamindex,])
#   t1_goalconceded_vec_ht[is.na(t1_goalconceded_vec_ht)] <- ""
#   t1_goalconceded_vec_ht <- t1_goalconceded_vec_ht[t1_goalconceded_vec_ht != ""]
#   t1_goalconceded_vec_ht  <-tail(t1_goalconceded_vec_ht,6)
#   t1_goalconceded_vec_ht  <- as.numeric(t1_goalconceded_vec_ht)
#   t1_goalconceded_vec_ht
#   t1_ht_totalgoalconceded <- sum(t1_goalconceded_vec_ht)
#   t1_ht_matches_concede <- length(which(t1_goalconceded_vec_ht > 0))
#   t1_ht_matches_without_concede <- length(which(t1_goalconceded_vec_ht == "0"))
#   #awayteam
#   t1_goalconceded_vec_at <- as.vector(t1_goalconceded_h[t1_awayteamindex,])
#   t1_goalconceded_vec_at[is.na(t1_goalconceded_vec_at)] <- ""
#   t1_goalconceded_vec_at <- t1_goalconceded_vec_at[t1_goalconceded_vec_at != ""]
#   t1_goalconceded_vec_at  <-tail(t1_goalconceded_vec_at,6)
#   t1_goalconceded_vec_at  <- as.numeric(t1_goalconceded_vec_at)
#   t1_at_totalgoalconceded <- sum(t1_goalconceded_vec_at)
#   t1_at_matches_concede <- length(which(t1_goalconceded_vec_at > 0))
#   t1_at_matches_without_concede <- length(which(t1_goalconceded_vec_at == "0"))
#
#   ####################################################################################
#   #analyse total combined goals
#   #hometeam
#   t1_totalgoals_vec_ht <- as.vector(t1_totalgoals_h[t1_hometeamindex,])
#   t1_totalgoals_vec_ht[is.na(t1_totalgoals_vec_ht)] <- ""
#   t1_totalgoals_vec_ht <- t1_totalgoals_vec_ht[t1_totalgoals_vec_ht != ""]
#   t1_totalgoals_vec_ht  <-tail(t1_totalgoals_vec_ht,6)
#   t1_totalgoals_vec_ht  <- as.numeric(t1_totalgoals_vec_ht)
#   t1_totalgoals_vec_ht
#   t1_ht_totalgoals <- sum(t1_totalgoals_vec_ht)
#   t1_ht_avgtotalgoals <- (t1_ht_totalgoals/6)
#   t1_ht_no_of_ov25 <- length(which(t1_totalgoals_vec_ht >= 3))
#   t1_ht_no_of_un25 <- length(which(t1_totalgoals_vec_ht <= 2))
#   #awayteam
#   t1_totalgoals_vec_at <- as.vector(t1_totalgoals_h[t1_awayteamindex,])
#   t1_totalgoals_vec_at[is.na(t1_totalgoals_vec_at)] <- ""
#   t1_totalgoals_vec_at <- t1_totalgoals_vec_at[t1_totalgoals_vec_at != ""]
#   t1_totalgoals_vec_at  <-tail(t1_totalgoals_vec_at,6)
#   t1_totalgoals_vec_at  <- as.numeric(t1_totalgoals_vec_at)
#   t1_totalgoals_vec_at
#   t1_at_totalgoals <- sum(t1_totalgoals_vec_at)
#   t1_at_avgtotalgoals <- (t1_at_totalgoals/6)
#   t1_at_no_of_ov25 <- length(which(t1_totalgoals_vec_at >= 3))
#   t1_at_no_of_un25 <- length(which(t1_totalgoals_vec_at <= 2))
#   ################################################################################
#   #analyse win margin
#   #hometeam
#   t1_winmargin_vec_ht <- as.vector(t1_winmargin_h[t1_hometeamindex,])
#   t1_winmargin_vec_ht[is.na(t1_winmargin_vec_ht)] <- ""
#   t1_winmargin_vec_ht <- t1_winmargin_vec_ht[t1_winmargin_vec_ht != ""]
#   t1_winmargin_vec_ht  <-tail(t1_winmargin_vec_ht,6)
#   t1_winmargin_vec_ht  <- as.numeric(t1_winmargin_vec_ht)
#
#   t1_ht_totalwinmargin <- sum(t1_winmargin_vec_ht)
#   t1_ht_no_of_winmargin_ov0 <- length(which(t1_winmargin_vec_ht >= 0))
#   t1_ht_no_of_winmargin_ov1 <- length(which(t1_winmargin_vec_ht >= 1))
#   t1_ht_no_of_winmargin_un0 <- length(which(t1_winmargin_vec_ht <= 0))
#   t1_ht_no_of_winmargin_un1 <- length(which(t1_winmargin_vec_ht <= 1))
#   #awayteam
#   t1_winmargin_vec_at <- as.vector(t1_winmargin_h[t1_awayteamindex,])
#   t1_winmargin_vec_at[is.na(t1_winmargin_vec_at)] <- ""
#   t1_winmargin_vec_at <- t1_winmargin_vec_at[t1_winmargin_vec_at != ""]
#   t1_winmargin_vec_at  <-tail(t1_winmargin_vec_at,6)
#   t1_winmargin_vec_at  <- as.numeric(t1_winmargin_vec_at)
#
#   t1_at_totalwinmargin <- sum(t1_winmargin_vec_at)
#   t1_at_no_of_winmargin_ov0 <- length(which(t1_winmargin_vec_at >= 0))
#   t1_at_no_of_winmargin_ov1 <- length(which(t1_winmargin_vec_at >= 1))
#   t1_at_no_of_winmargin_un0 <- length(which(t1_winmargin_vec_at <= 0))
#   t1_at_no_of_winmargin_un1 <- length(which(t1_winmargin_vec_at <= 1))
#   ##################################################################################
#   #very last win margin
#   #hometeam
#   t1_winmargin_vec_ht_lm <- as.vector(t1_winmargin_h[t1_hometeamindex,])
#   t1_winmargin_vec_ht_lm[is.na(t1_winmargin_vec_ht_lm)] <- ""
#   t1_winmargin_vec_ht_lm <- t1_winmargin_vec_ht_lm[t1_winmargin_vec_ht_lm != ""]
#   t1_winmargin_vec_ht_lm  <-tail(t1_winmargin_vec_ht_lm,1)
#   #awayteam
#   t1_winmargin_vec_at_lm <- as.vector(t1_winmargin_h[t1_awayteamindex,])
#   t1_winmargin_vec_at_lm[is.na(t1_winmargin_vec_at_lm)] <- ""
#   t1_winmargin_vec_at_lm <- t1_winmargin_vec_at_lm[t1_winmargin_vec_at_lm != ""]
#   t1_winmargin_vec_at_lm  <-tail(t1_winmargin_vec_at_lm,1)
#   #################################################################################
#   #pick average yellow cards
#   #hometeam
#   t1_yellowtotals_vec_ht <- as.vector(t1_yellowtotalsv2[t1_hometeamindex,])
#   t1_yellowtotals_vec_ht[is.na(t1_yellowtotals_vec_ht)] <- ""
#   t1_yellowtotals_vec_ht <- t1_yellowtotals_vec_ht[t1_yellowtotals_vec_ht != ""]
#   t1_yellowtotals_vec_ht  <-tail(t1_yellowtotals_vec_ht,1)
#   #awayteam
#   t1_yellowtotals_vec_at <- as.vector(t1_yellowtotalsv2[t1_awayteamindex,])
#   t1_yellowtotals_vec_at[is.na(t1_yellowtotals_vec_at)] <- ""
#   t1_yellowtotals_vec_at <- t1_yellowtotals_vec_at[t1_yellowtotals_vec_at != ""]
#   t1_yellowtotals_vec_at  <-tail(t1_yellowtotals_vec_at,1)
#
#   #################################################################################
#   #pick average corners
#   #hometeam
#   t1_cornertotals_vec_ht <- as.vector(t1_cornertotalsv2[t1_hometeamindex,])
#   t1_cornertotals_vec_ht[is.na(t1_cornertotals_vec_ht)] <- ""
#   t1_cornertotals_vec_ht <- t1_cornertotals_vec_ht[t1_cornertotals_vec_ht != ""]
#   t1_cornertotals_vec_ht  <-tail(t1_cornertotals_vec_ht,1)
#   #awayteam
#   t1_cornertotals_vec_at <- as.vector(t1_cornertotalsv2[t1_awayteamindex,])
#   t1_cornertotals_vec_at[is.na(t1_cornertotals_vec_at)] <- ""
#   t1_cornertotals_vec_at <- t1_cornertotals_vec_at[t1_cornertotals_vec_at != ""]
#   t1_cornertotals_vec_at  <-tail(t1_cornertotals_vec_at,1)
#   #################################################################################
#   #pick xpected shots conversion
#   #hometeam
#   t1_xshotsconversion_vec_ht <- as.vector(t1_shots_analysis[t1_hometeamindex,])
#   t1_xshotsconversion_vec_ht[is.na(t1_xshotsconversion_vec_ht)] <- ""
#   t1_xshotsconversion_vec_ht <- t1_xshotsconversion_vec_ht[t1_xshotsconversion_vec_ht != ""]
#   t1_xshotsconversion_vec_ht  <-tail(t1_xshotsconversion_vec_ht,1)
#   #awayteam
#   t1_xshotsconversion_vec_at <- as.vector(t1_shots_analysis[t1_awayteamindex,])
#   t1_xshotsconversion_vec_at[is.na(t1_xshotsconversion_vec_at)] <- ""
#   t1_xshotsconversion_vec_at <- t1_xshotsconversion_vec_at[t1_xshotsconversion_vec_at != ""]
#   t1_xshotsconversion_vec_at  <-tail(t1_xshotsconversion_vec_at,1)
#   #################################################################################
#   #pick yellow cards per foul
#   #hometeam
#   t1_fouls_conversion_vec_ht <- as.vector(t1_fouls_conversion[t1_hometeamindex,])
#   t1_fouls_conversion_vec_ht[is.na(t1_fouls_conversion_vec_ht)] <- ""
#   t1_fouls_conversion_vec_ht <- t1_fouls_conversion_vec_ht[t1_fouls_conversion_vec_ht != ""]
#   t1_fouls_conversion_vec_ht  <-tail(t1_fouls_conversion_vec_ht,1)
#   #awayteam
#   t1_fouls_conversion_vec_at <- as.vector(t1_fouls_conversion[t1_awayteamindex,])
#   t1_fouls_conversion_vec_at[is.na(t1_fouls_conversion_vec_at)] <- ""
#   t1_fouls_conversion_vec_at <- t1_fouls_conversion_vec_at[t1_fouls_conversion_vec_at != ""]
#   t1_fouls_conversion_vec_at  <-tail(t1_fouls_conversion_vec_at,1)
#   #################################################################################
#
#   ####we need to decide ############
#   #winner goals
#   t1_ht_last6points <- t1_ht_numberof_wins*3 + t1_ht_numberof_draws*1
#   t1_at_last6points <- t1_at_numberof_wins*3 + t1_at_numberof_draws*1
#
#   if(t1_ht_last6points > t1_at_last6points) {t1_3waypick <- "1"}  else {t1_3waypick <- "X2"}
#
#   if(t1_at_last6points > t1_ht_last6points ) {t1_3waypick <- "2"} else {t1_3waypick <- "1X"}
#
#   if(t1_ht_no_of_ov25 + t1_at_no_of_ov25 >= 6) {t1_goalspick <- "ov25"} else {t1_goalspick <- "un25"}
#
#   if(t1_ht_no_of_un25 + t1_at_no_of_un25 >= 6) {t1_goalspick <- "un25"} else {t1_goalspick <- "ov25"}
#
#   if(t1_ht_matches_scoring >= 4 && t1_at_matches_scoring >=4) {t1_btts <- "BTTS-Y"} else {t1_btts <- "BTTS-N"}
#
#
#   t1_prediction[t1_row] <- rbind(paste(t1_3waypick,t1_goalspick,t1_btts,sep = ","))
#   t1_HWM[t1_row] <- t1_ht_totalwinmargin
#   t1_AWM[t1_row] <- t1_at_totalwinmargin
#
#   t1_HWMLM[t1_row] <- t1_winmargin_vec_ht_lm
#   t1_AWMLM[t1_row] <- t1_winmargin_vec_at_lm
#
#   t1_HY[t1_row] <- t1_yellowtotals_vec_ht
#   t1_AY[t1_row] <- t1_yellowtotals_vec_at
#
#   t1_HCO[t1_row] <- t1_cornertotals_vec_ht
#   t1_ACO[t1_row] <- t1_cornertotals_vec_at
#
#   t1_HXSC[t1_row] <- t1_xshotsconversion_vec_ht
#   t1_AXSC[t1_row] <- t1_xshotsconversion_vec_at
#
#   t1_HYCPF[t1_row] <- t1_fouls_conversion_vec_ht
#   t1_AYCPF[t1_row] <- t1_fouls_conversion_vec_at
# }
#
# t1_prediction <- as.data.frame(t1_prediction)
# colnames(t1_prediction) <- "prediction"
#
# t1_HWM <- as.data.frame(t1_HWM)
# colnames(t1_HWM) <- "HWM"
#
# t1_AWM <- as.data.frame(t1_AWM)
# colnames(t1_AWM) <- "AWM"
#
# t1_HWMLM <- as.data.frame(t1_HWMLM)
# colnames(t1_HWMLM) <- "HWMLM"
#
# t1_AWMLM <- as.data.frame(t1_AWMLM)
# colnames(t1_AWMLM) <- "AWMLM"
#
# t1_HY <- as.data.frame(t1_HY)
# colnames(t1_HY) <- "AVGHY"
#
# t1_AY <- as.data.frame(t1_AY)
# colnames(t1_AY) <- "AVGAY"
#
# t1_HCO <- as.data.frame(t1_HCO)
# colnames(t1_HCO) <- "AVGHCO"
#
# t1_ACO <- as.data.frame(t1_ACO)
# colnames(t1_ACO) <- "AVGACO"
#
# t1_HXSC <- as.data.frame(t1_HXSC)
# colnames(t1_HXSC) <- "HXSC"
#
# t1_AXSC <- as.data.frame(t1_AXSC)
# colnames(t1_AXSC) <- "AXSC"
#
# t1_HYCPF <- as.data.frame(t1_HYCPF)
# colnames(t1_HYCPF) <- "HYCPF"
#
# t1_AYCPF <- as.data.frame(t1_AYCPF)
# colnames(t1_AYCPF) <- "AYCPF"
#
# t1_picks <- cbind(T1_fixtures$Div,T1_fixtures$HomeTeam_t1,T1_fixtures$AwayTeam_t1,t1_prediction,t1_HWM,t1_AWM,t1_HWMLM,t1_AWMLM,t1_HY,t1_AY,t1_HCO,t1_ACO,t1_HXSC,t1_AXSC,t1_HYCPF,t1_AYCPF)
#
# colnames(t1_picks)[1] <- "picks_Div"
# colnames(t1_picks)[2] <- "picks_HomeTeam"
# colnames(t1_picks)[3] <- "picks_AwayTeam"
# t1_picks$matchid <- paste(t1_picks$picks_HomeTeam,t1_picks$picks_AwayTeam,sep = "-")
# ############################################################################################
# #end of T1
# t1_picks

############################################################################################
#end of T1
############################################################################################
#combine picks from divisions
allteamsnewleagues2024picks <- rbind(mls_picks,arg_picks)
#join the data
#######myodds file##########
#myodds_fixtures <- readxl::read_excel('../FDAS/myodds_20222023.xlsx', sheet = '3way')
#myodds_fixtures$matchid <- paste(myodds_fixtures$HT,myodds_fixtures$AT, sep = "-")
#myodds_fixtures_prediction <- dplyr::left_join(myodds_fixtures,allteams20222023picks)
#write.xlsx(myodds_fixtures_prediction,'myodds_fixtures_prediction.xlsx')
############################
#picks_fixtures <- read.csv('myfixtures.csv')
#picks_fixtures$matchid <- paste(picks_fixtures$Home_Team,picks_fixtures$Away_Team, sep = "-")

#picks_fixtures_prediction <- dplyr::left_join(picks_fixtures,allteams20222023picks)
#write.csv(picks_fixtures_prediction,'picks_fixtures_prediction.csv')
###########################
#reset allteams20222023picks
#rm(allteams20222023picks)


