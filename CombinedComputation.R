#load the new data frames
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('sqldf')
library('scales')
source('divisions.R')
source('Matchday.R')
mls_currentround

#first_df <- D1_rounds[D1_rounds$d1_matchday > 25,]
#second_df <- F1_rounds[F1_rounds$f1_matchday > 25,]
#third_df <- E2_rounds[E2_rounds$e2_matchday > 40,]
#first_df <- first_df[,-37]
#second_df <- second_df[,-37]
#third_df <- third_df[,-37]
#MLS <- rbind(first_df,second_df)
MLS <- MLS_rounds[MLS_rounds$mls_matchday > 7,]
#MLS <- na.omit(MLS)
#goaltotals v2
mls_goaltotalsv2 <- tapply(MLS$TG, MLS[c("HomeTeam", "AwayTeam")],mean)
mls_hgtotals <- rowSums(mls_goaltotalsv2, na.rm = T)
mls_agtotals <- colSums(mls_goaltotalsv2, na.rm = T)
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_hgtotals,mls_agtotals)
mls_totalgoals <- mls_hgtotals + mls_agtotals
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_totalgoals)
mls_teams <- sort(unique(MLS$HomeTeam))
mls_home_games <- c()
mls_away_games <-c()
for (i_mls in 1:length(mls_teams))
{

  mls_home_games[i_mls] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls],])
  mls_away_games[i_mls]  <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls],])

}
mls_games_played <- mls_home_games + mls_away_games
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_games_played)
mls_avg_totalgoals <- round((mls_totalgoals/ mls_games_played), digits = 4)
mls_goaltotalsv2[is.na(mls_goaltotalsv2)] <- ""
mls_goaltotalsv2 <- cbind(mls_goaltotalsv2,mls_avg_totalgoals)

############################################################################################################
#Cornertotals v2
mls_cornertotalsv2 <- tapply(MLS$TC, MLS[c("HomeTeam", "AwayTeam")],mean)
mls_hcototals <- rowSums(mls_cornertotalsv2, na.rm = T)
mls_acototals <- colSums(mls_cornertotalsv2, na.rm = T)
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_hcototals,mls_acototals)
mls_totalcorners <- mls_hcototals + mls_acototals
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_totalcorners)
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_games_played)
mls_avg_totalcorners <- round((mls_totalcorners/ mls_games_played), digits = 4)
mls_cornertotalsv2[is.na(mls_cornertotalsv2)] <- ""
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_avg_totalcorners)
############################################################################################################
#GS matrix
mls_goalscored_h <- tapply(MLS$FTHG, MLS[c("HomeTeam", "Date")],mean)
mls_goalscored_a <- tapply(MLS$FTAG, MLS[c("AwayTeam", "Date")],mean)
mls_goalscored_h[is.na(mls_goalscored_h)] <- ""
mls_goalscored_a[is.na(mls_goalscored_a)] <- ""
for(mls_rowhgs in 1:nrow(mls_goalscored_h)) {
  for(mls_colhgs in 1:ncol(mls_goalscored_h)) {

    # print(my_matrix[row, col])
    for(mls_rowags in 1:nrow(mls_goalscored_a)) {
      for(mls_colags in 1:ncol(mls_goalscored_a)) {
        ifelse(!mls_goalscored_a[mls_rowags,mls_colags]=="",mls_goalscored_h[mls_rowags,mls_colags] <- mls_goalscored_a[mls_rowags,mls_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#############################################################################################################
#Goal conceded matrix
mls_goalconceded_h <- tapply(MLS$FTAG, MLS[c("HomeTeam", "Date")],mean)
mls_goalconceded_a <- tapply(MLS$FTHG, MLS[c("AwayTeam", "Date")],mean)
mls_goalconceded_h[is.na(mls_goalconceded_h)] <- ""
mls_goalconceded_a[is.na(mls_goalconceded_a)] <- ""
for(mls_rowhgc in 1:nrow(mls_goalconceded_h)) {
  for(mls_colhgc in 1:ncol(mls_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowagc in 1:nrow(mls_goalconceded_a)) {
      for(mls_colagc in 1:ncol(mls_goalconceded_a)) {
        ifelse(!mls_goalconceded_a[mls_rowagc,mls_colagc]=="",mls_goalconceded_h[mls_rowagc,mls_colagc] <- mls_goalconceded_a[mls_rowagc,mls_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################
#corner matrix
mls_totalcorners_h <- tapply(MLS$TC, MLS[c("HomeTeam", "Date")],mean)
mls_totalcorners_a <- tapply(MLS$TC, MLS[c("AwayTeam", "Date")],mean)
mls_totalcorners_h[is.na(mls_totalcorners_h)] <- ""
mls_totalcorners_a[is.na(mls_totalcorners_a)] <- ""
#MLS
for(mls_rowTC in 1:nrow(mls_totalcorners_h)) {
  for(mls_colTC in 1:ncol(mls_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(mls_rowTC in 1:nrow(mls_totalcorners_a)) {
      for(mls_colTC in 1:ncol(mls_totalcorners_a)) {
        ifelse(!mls_totalcorners_a[mls_rowTC,mls_colTC]=="",mls_totalcorners_h[mls_rowTC,mls_colTC] <- mls_totalcorners_a[mls_rowTC,mls_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###################################################################################################################################
#corners awarded
mls_coawarded_h <- tapply(MLS$HCO, MLS[c("HomeTeam", "Date")],mean)
mls_coawarded_a <- tapply(MLS$ACO, MLS[c("AwayTeam", "Date")],mean)
mls_coawarded_h[is.na(mls_coawarded_h)] <- ""
mls_coawarded_a[is.na(mls_coawarded_a)] <- ""
#MLS
for(mls_rowhco in 1:nrow(mls_coawarded_h)) {
  for(mls_colhco in 1:ncol(mls_coawarded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowaco in 1:nrow(mls_coawarded_a)) {
      for(mls_colaco in 1:ncol(mls_coawarded_a)) {
        ifelse(!mls_coawarded_a[mls_rowaco,mls_colaco]=="",mls_coawarded_h[mls_rowaco,mls_colaco] <- mls_coawarded_a[mls_rowaco,mls_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

#######################################################################################################################################
#corners conceded
mls_cornersconceded_h <- tapply(MLS$ACO, MLS[c("HomeTeam", "Date")],mean)
mls_cornersconceded_a <- tapply(MLS$HCO, MLS[c("AwayTeam", "Date")],mean)
mls_cornersconceded_h[is.na(mls_cornersconceded_h)] <- ""
mls_cornersconceded_a[is.na(mls_cornersconceded_a)] <- ""
#MLS
for(mls_rowhcc in 1:nrow(mls_cornersconceded_h)) {
  for(mls_colhcc in 1:ncol(mls_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowacc in 1:nrow(mls_cornersconceded_a)) {
      for(mls_colacc in 1:ncol(mls_cornersconceded_a)) {
        ifelse(!mls_cornersconceded_a[mls_rowacc,mls_colacc]=="",mls_cornersconceded_h[mls_rowacc,mls_colacc] <- mls_cornersconceded_a[mls_rowacc,mls_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
############################################################################################################################################
#corners form
#create home and away coscform matrices
mls_coscform_h <- tapply(MLS$COSC, MLS[c("HomeTeam", "Date")],median)
mls_coscform_a <- tapply(MLS$COSC, MLS[c("AwayTeam", "Date")],median)
mls_coscform_h[is.na(mls_coscform_h)] <- ""
mls_coscform_a[is.na(mls_coscform_a)] <- ""
#MLS
for(mls_rowh_f_cosc in 1:nrow(mls_coscform_h)) {
  for(mls_colh_f_cosc in 1:ncol(mls_coscform_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_cosc in 1:nrow(mls_coscform_a)) {
      for(mls_cola_f_cosc in 1:ncol(mls_coscform_a)) {
        ifelse(!mls_coscform_a[mls_rowa_f_cosc,mls_cola_f_cosc]=="",mls_coscform_h[mls_rowa_f_cosc,mls_cola_f_cosc] <- mls_coscform_a[mls_rowa_f_cosc,mls_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
################################################################################################################################################
#winmargin
mls_winmargin_h <- tapply(MLS$FTHG - MLS$FTAG, MLS[c("HomeTeam", "Date")],mean)
mls_winmargin_a <- tapply(MLS$FTAG - MLS$FTHG, MLS[c("AwayTeam", "Date")],mean)
mls_winmargin_h[is.na(mls_winmargin_h)] <- ""
mls_winmargin_a[is.na(mls_winmargin_a)] <- ""
#MLS
for(mls_rowhwm in 1:nrow(mls_winmargin_h)) {
  for(mls_colhwm in 1:ncol(mls_winmargin_h)) {

    # print(my_matrix[row, col])
    for(mls_rowawm in 1:nrow(mls_winmargin_a)) {
      for(mls_colawm in 1:ncol(mls_winmargin_a)) {
        ifelse(!mls_winmargin_a[mls_rowawm,mls_colawm]=="",mls_winmargin_h[mls_rowawm,mls_colawm] <- mls_winmargin_a[mls_rowawm,mls_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#################################################################################################################################################
#yellow card matrix
mls_yellowscored_h <- tapply(MLS$HY, MLS[c("HomeTeam", "Date")],mean)
mls_yellowscored_a <- tapply(MLS$AY, MLS[c("AwayTeam", "Date")],mean)
mls_yellowscored_h[is.na(mls_yellowscored_h)] <- ""
mls_yellowscored_a[is.na(mls_yellowscored_a)] <- ""
#MLS
for(mls_rowhys in 1:nrow(mls_yellowscored_h)) {
  for(mls_colhys in 1:ncol(mls_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(mls_roways in 1:nrow(mls_yellowscored_a)) {
      for(mls_colays in 1:ncol(mls_yellowscored_a)) {
        ifelse(!mls_yellowscored_a[mls_roways,mls_colays]=="",mls_yellowscored_h[mls_roways,mls_colays] <- mls_yellowscored_a[mls_roways,mls_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################
#red card matrix
mls_redscored_h <- tapply(MLS$HR, MLS[c("HomeTeam", "Date")],mean)
mls_redscored_a <- tapply(MLS$AR, MLS[c("AwayTeam", "Date")],mean)
mls_redscored_h[is.na(mls_redscored_h)] <- ""
mls_redscored_a[is.na(mls_redscored_a)] <- ""
for(mls_rowhrs in 1:nrow(mls_redscored_h)) {
  for(mls_colhrs in 1:ncol(mls_redscored_h)) {

    # print(my_matrix[row, col])
    for(mls_rowars in 1:nrow(mls_redscored_a)) {
      for(mls_colars in 1:ncol(mls_redscored_a)) {
        ifelse(!mls_redscored_a[mls_rowars,mls_colars]=="",mls_redscored_h[mls_rowars,mls_colars] <- mls_redscored_a[mls_rowars,mls_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################################
#red totals
mls_redtotalsv2 <- tapply(MLS$TR, MLS[c("HomeTeam", "AwayTeam")],mean)
mls_hrtotals <- rowSums(mls_redtotalsv2, na.rm = T)
mls_artotals <- colSums(mls_redtotalsv2, na.rm = T)
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_hrtotals,mls_artotals)
mls_totalreds <- mls_hrtotals + mls_artotals
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_totalreds)
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_games_played)
mls_avg_totalreds <- round((mls_totalreds/ mls_games_played), digits = 4)
mls_redtotalsv2[is.na(mls_redtotalsv2)] <- ""
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_avg_totalreds)
############################################################################################################################################################
#yellowtotals
mls_yellowtotalsv2 <- tapply(MLS$TY, MLS[c("HomeTeam", "AwayTeam")],mean)
mls_hytotals <- rowSums(mls_yellowtotalsv2, na.rm = T)
mls_aytotals <- colSums(mls_yellowtotalsv2, na.rm = T)
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_hytotals,mls_aytotals)
mls_totalyellows <- mls_hytotals + mls_aytotals
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_totalyellows)
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_games_played)
mls_avg_totalyellows <- round((mls_totalyellows/ mls_games_played), digits = 4)
mls_yellowtotalsv2[is.na(mls_yellowtotalsv2)] <- ""
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_avg_totalyellows)
##################################################################################################################################################
#team form
mls_form_h <- tapply(MLS$FTR, MLS[c("HomeTeam", "Date")],median)
mls_form_a <- tapply(MLS$FTR, MLS[c("AwayTeam", "Date")],median)
mls_form_h[is.na(mls_form_h)] <- ""
mls_form_a[is.na(mls_form_a)] <- ""
mls_form_h <- sub("A","L",mls_form_h)
mls_form_h <- sub("H","W",mls_form_h)
mls_form_a <- sub("A","W",mls_form_a)
mls_form_a <- sub("H","L",mls_form_a)
for(mls_rowh_f in 1:nrow(mls_form_h)) {
  for(mls_colh_f in 1:ncol(mls_form_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f in 1:nrow(mls_form_a)) {
      for(mls_cola_f in 1:ncol(mls_form_a)) {
        ifelse(!mls_form_a[mls_rowa_f,mls_cola_f]=="",mls_form_h[mls_rowa_f,mls_cola_f] <- mls_form_a[mls_rowa_f,mls_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###########################################################################################################################################
#CS form
mls_csform_h <- tapply(MLS$CS, MLS[c("HomeTeam", "Date")],median)
mls_csform_a <- tapply(MLS$CS, MLS[c("AwayTeam", "Date")],median)
mls_csform_h[is.na(mls_csform_h)] <- ""
mls_csform_a[is.na(mls_csform_a)] <- ""
#MLS
for(mls_rowh_f_cs in 1:nrow(mls_csform_h)) {
  for(mls_colh_f_cs in 1:ncol(mls_csform_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_cs in 1:nrow(mls_csform_a)) {
      for(mls_cola_f_cs in 1:ncol(mls_csform_a)) {
        ifelse(!mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs]=="",mls_csform_h[mls_rowa_f_cs,mls_cola_f_cs] <- mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
####################################################################################################################################
#TG matrix
mls_totalgoals_h <- tapply(MLS$TG, MLS[c("HomeTeam", "Date")],mean)
mls_totalgoals_a <- tapply(MLS$TG, MLS[c("AwayTeam", "Date")],mean)
mls_totalgoals_h[is.na(mls_totalgoals_h)] <- ""
mls_totalgoals_a[is.na(mls_totalgoals_a)] <- ""
for(mls_rowh in 1:nrow(mls_totalgoals_h)) {
  for(mls_colh in 1:ncol(mls_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa in 1:nrow(mls_totalgoals_a)) {
      for(mls_cola in 1:ncol(mls_totalgoals_a)) {
        ifelse(!mls_totalgoals_a[mls_rowa,mls_cola]=="",mls_totalgoals_h[mls_rowa,mls_cola] <- mls_totalgoals_a[mls_rowa,mls_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
##############################################################################################################
#Totalgoals
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
#################################################################################################################################################################
#team against
mls_form_team_against_h <- tapply(MLS$AwayTeam, MLS[c("HomeTeam", "Date")],median)
mls_form_team_against_a <- tapply(MLS$HomeTeam, MLS[c("AwayTeam", "Date")],median)
mls_form_team_against_h[is.na(mls_form_team_against_h)] <- ""
mls_form_team_against_a[is.na(mls_form_team_against_a)] <- ""
#MLS
for(mls_rowh_f_against in 1:nrow(mls_form_team_against_h)) {
  for(mls_colh_f_against in 1:ncol(mls_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_against in 1:nrow(mls_form_team_against_a)) {
      for(mls_cola_f_against in 1:ncol(mls_form_team_against_a)) {
        ifelse(!mls_form_team_against_a[mls_rowa_f_against,mls_cola_f_against]=="",mls_form_team_against_h[mls_rowa_f_against,mls_cola_f_against] <- mls_form_team_against_a[mls_rowa_f_against,mls_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
###############################################################################################################################################3
#shotsanalysis
#MLS
#home goals scored
mls_home_gs <- aggregate(MLS$FTHG, by = list(MLS$HomeTeam), FUN = sum)
mls_home_gs_avg <- aggregate(MLS$FTHG, by = list(MLS$HomeTeam),mean)
mls_home_scoring <- merge(mls_home_gs,mls_home_gs_avg, by='Group.1',all = T)
names(mls_home_scoring)[names(mls_home_scoring) == "x.x"] <- "TFthg"
names(mls_home_scoring)[names(mls_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mls_away_gs <- aggregate(MLS$FTAG, by = list(MLS$AwayTeam), FUN = sum)
mls_away_gs_avg <- aggregate(MLS$FTAG, by = list(MLS$AwayTeam),mean)
mls_away_scoring <- merge(mls_away_gs,mls_away_gs_avg, by='Group.1',all = T)
names(mls_away_scoring)[names(mls_away_scoring) == "x.x"] <- "TFtag"
names(mls_away_scoring)[names(mls_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mls_scoring <- merge(mls_home_scoring,mls_away_scoring,by='Group.1',all = T)
mls_scoring$TGS <- mls_scoring$TFthg + mls_scoring$TFtag

#Home shots on target
mls_home_hst <- aggregate(MLS$HST, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ast <- aggregate(MLS$AST, by = list(MLS$AwayTeam), FUN = sum)
mls_tst <- merge(mls_home_hst,mls_away_ast, by='Group.1',all = T)
names(mls_tst)[names(mls_tst) == "x.x"] <- "hst"
names(mls_tst)[names(mls_tst) == "x.y"] <- "ast"
mls_tst$TST <- mls_tst$hst + mls_tst$ast
#merge goals scored and shots on target
mls_scoring_conversion <- merge(mls_tst,mls_scoring,by='Group.1',all = T)
#add HSC ASC TSC
mls_scoring_conversion$HSTC <- percent(mls_scoring_conversion$TFthg/mls_scoring_conversion$hst, accuracy = 0.01)
mls_scoring_conversion$ASTC <- percent(mls_scoring_conversion$TFtag/mls_scoring_conversion$ast, accuracy = 0.01)
mls_scoring_conversion$TSTC <- percent(mls_scoring_conversion$TGS/mls_scoring_conversion$TST, accuracy = 0.01)
#merge games played
mls_scoring_conversion <- cbind(mls_scoring_conversion,mls_games_played)
#create the second part
#home goals conceded
mls_home_gc <- aggregate(MLS$FTAG, by = list(MLS$HomeTeam), FUN = sum)
mls_home_gc_avg <- aggregate(MLS$FTAG, by = list(MLS$HomeTeam),mean)
mls_home_conceding <- merge(mls_home_gc,mls_home_gc_avg, by='Group.1',all = T)
names(mls_home_conceding)[names(mls_home_conceding) == "x.x"] <- "TFthc"
names(mls_home_conceding)[names(mls_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mls_away_gc <- aggregate(MLS$FTHG, by = list(MLS$AwayTeam), FUN = sum)
mls_away_gc_avg <- aggregate(MLS$FTHG, by = list(MLS$AwayTeam),mean)
mls_away_conceding <- merge(mls_away_gc,mls_away_gc_avg, by='Group.1',all = T)
names(mls_away_conceding)[names(mls_away_conceding) == "x.x"] <- "TFtac"
names(mls_away_conceding)[names(mls_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mls_conceding <- merge(mls_home_conceding,mls_away_conceding,by='Group.1',all = T)
mls_conceding$TGC <- mls_conceding$TFthc + mls_conceding$TFtac
mls_home_hst
#Home shots conceded
mls_home_hsc <- aggregate(MLS$AST, by = list(MLS$HomeTeam), FUN = sum)
mls_away_asc <- aggregate(MLS$HST, by = list(MLS$AwayTeam), FUN = sum)
mls_tsc <- merge(mls_home_hsc,mls_away_asc, by='Group.1',all = T)
names(mls_tsc)[names(mls_tsc) == "x.x"] <- "hsc"
names(mls_tsc)[names(mls_tsc) == "x.y"] <- "asc"
mls_tsc$TSC <- mls_tsc$hsc + mls_tsc$asc
#merge goals conceded and shots conceded
mls_conceding_conversion <- merge(mls_tsc,mls_conceding,by='Group.1',all = T)

#add HSC ASC TSC
mls_conceding_conversion$HSCC <- percent(mls_conceding_conversion$TFthc/mls_conceding_conversion$hsc, accuracy = 0.01)
mls_conceding_conversion$ASCC <- percent(mls_conceding_conversion$TFtac/mls_conceding_conversion$asc, accuracy = 0.01)
mls_conceding_conversion$TSCC <- percent(mls_conceding_conversion$TGC/mls_conceding_conversion$TSC, accuracy = 0.01)
mls_conceding_conversion$XSTC <- round(mls_scoring$TGS/(mls_tst$TST - mls_scoring$TGS), digits = 2)

#merge the two parts
mls_shots_analysis <- merge(mls_scoring_conversion,mls_conceding_conversion,by='Group.1',all = T)
#####################################################################################################################################
#fouls analysis
#MLS
#home fouls for
mls_home_fouls <- aggregate(MLS$HF, by = list(MLS$HomeTeam), FUN = sum)
mls_home_fouls_avg <- aggregate(MLS$HF, by = list(MLS$HomeTeam),mean)
mls_home_foulsdata <- merge(mls_home_fouls,mls_home_fouls_avg, by='Group.1',all = T)
names(mls_home_foulsdata)[names(mls_home_foulsdata) == "x.x"] <- "THfouls"
names(mls_home_foulsdata)[names(mls_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
mls_away_fouls <- aggregate(MLS$HF, by = list(MLS$AwayTeam), FUN = sum)
mls_away_fouls_avg <- aggregate(MLS$HF, by = list(MLS$AwayTeam),mean)
mls_away_foulsdata <- merge(mls_away_fouls,mls_away_fouls_avg, by='Group.1',all = T)
names(mls_away_foulsdata)[names(mls_away_foulsdata) == "x.x"] <- "TAfouls"
names(mls_away_foulsdata)[names(mls_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
mls_fouls <- merge(mls_home_foulsdata,mls_away_foulsdata,by='Group.1',all = T)
mls_fouls$TotalFouls <- mls_fouls$THfouls + mls_fouls$TAfouls

#yellow cards
mls_home_hyc <- aggregate(MLS$HY, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ayc <- aggregate(MLS$AY, by = list(MLS$AwayTeam), FUN = sum)
mls_tyc <- merge(mls_home_hyc,mls_away_ayc, by='Group.1',all = T)
names(mls_tyc)[names(mls_tyc) == "x.x"] <- "hyc"
names(mls_tyc)[names(mls_tyc) == "x.y"] <- "ayc"
mls_tyc$TotalYellows <- mls_tyc$hyc + mls_tyc$ayc

#merge fouls for and yellow cards
mls_fouls_conversion <- merge(mls_tyc,mls_fouls,by='Group.1',all = T)
mls_fouls_conversion$YcPerfoul <- round((mls_fouls_conversion$TotalYellows/mls_fouls_conversion$TotalFouls), digits = 2)
##################################################################################################################################################
##
#make div form uniform in entire data frame
MLS$Div <- "MLS"
##
###################################################################################################################################################
#poisson cards
mls_GP <- nrow(MLS)
#Calculate total home goals for each division
mls_T_HY <- sum(mls_home_hyc$x)
#calculate average home goal
mls_avg_HY <- round(mls_T_HY /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AY <- sum(mls_away_ayc$x)
#calculate average away goal
mls_avg_AY <- round(mls_T_AY /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_yas <- round(((mls_home_hyc$x/mls_home_games))/mls_avg_HY, digits = 4)
#calculate away attack strength
mls_away_yas <- round(((mls_away_ayc$x/mls_away_games))/mls_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HYC <- round(mls_T_AY /mls_GP, digits = 4)
#avg away concede
mls_avg_AYC <- round(mls_T_HY /mls_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
mls_home_ycc <- aggregate(MLS$AY, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ycc <- aggregate(MLS$HY, by = list(MLS$AwayTeam), FUN = sum)
#home defense strength
mls_home_yds <- round(((mls_home_ycc$x/mls_home_games))/mls_avg_HYC, digits = 4)
#away defense strength
mls_away_yds <- round(((mls_away_ycc$x/mls_away_games))/mls_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_yc <- cbind(mls_division,mls_teams,mls_avg_HY,mls_home_yas,mls_home_yds)
#away poisson data
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_yc <- cbind(mls_division,mls_teams,mls_avg_AY,mls_away_yas,mls_away_yds)
###
HomeTeam_mls_yc <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_yc <- rep(mls_teams, length(mls_teams))
MLS_fixtures_yc <- cbind(HomeTeam_mls_yc,AwayTeam_mls_yc)
MLS_fixtures_yc <- as.data.frame(MLS_fixtures_yc)
MLS_fixtures_yc <- MLS_fixtures_yc[!MLS_fixtures_yc$HomeTeam_mls_yc == MLS_fixtures_yc$AwayTeam_mls_yc,]
rownames(MLS_fixtures_yc) <- NULL
MLS_fixtures_yc$Div <- "MLS"
MLS_fixtures_yc <- MLS_fixtures_yc[,c(3,1,2)]

MLS_fixtures_yc$avg_HY_mls <- mls_avg_HY

MLS_fixtures_yc$mls_homeyas <- rep(mls_home_yas,each = length(mls_teams)-1)

mls_awayyds_lookup <- cbind(mls_teams,mls_away_yds)

mls_awayyds_lookup <- as.data.frame(mls_awayyds_lookup)

colnames(mls_awayyds_lookup) <- c("AwayTeam_mls_yc","mls_awayyds")


require('RH2')
MLS_fixtures_yc$mls_awayyds <- sqldf("SELECT mls_awayyds_lookup.mls_awayyds FROM mls_awayyds_lookup INNER JOIN MLS_fixtures_yc ON mls_awayyds_lookup.AwayTeam_mls_yc = MLS_fixtures_yc.AwayTeam_mls_yc")

MLS_fixtures_yc$avg_AY_mls <- mls_avg_AY

mls_awayyas_lookup <- cbind(mls_teams,mls_away_yas)

mls_awayyas_lookup <- as.data.frame(mls_awayyas_lookup)

colnames(mls_awayyas_lookup) <- c("AwayTeam_mls_yc","mls_awayyas")

MLS_fixtures_yc$mls_awayyas <- sqldf("SELECT mls_awayyas_lookup.mls_awayyas FROM mls_awayyas_lookup INNER JOIN MLS_fixtures_yc ON mls_awayyas_lookup.AwayTeam_mls_yc = MLS_fixtures_yc.AwayTeam_mls_yc")

MLS_fixtures_yc$mls_homeyds <- rep(mls_home_yds,each = length(mls_teams)-1)

MLS_fixtures_yc$mls_awayyds <- as.numeric(unlist(MLS_fixtures_yc$mls_awayyds))
#xGH
MLS_fixtures_yc$mls_xHYC <- MLS_fixtures_yc$avg_HY_mls * MLS_fixtures_yc$mls_homeyas * MLS_fixtures_yc$mls_awayyds
#xGA

MLS_fixtures_yc$mls_awayyas <- as.numeric(unlist(MLS_fixtures_yc$mls_awayyas))

MLS_fixtures_yc$mls_xAYC <- MLS_fixtures_yc$avg_AY_mls * MLS_fixtures_yc$mls_awayyas * MLS_fixtures_yc$mls_homeyds

MLS_fixtures_yc$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
#Home win
MLS_fixtures_yc$mls_H <- (
  MLS_fixtures_yc$mls_1_0 + MLS_fixtures_yc$mls_2_0 + MLS_fixtures_yc$mls_2_1 + MLS_fixtures_yc$mls_3_0 + MLS_fixtures_yc$mls_3_1 +
    MLS_fixtures_yc$mls_3_2 + MLS_fixtures_yc$mls_4_0 + MLS_fixtures_yc$mls_4_1 + MLS_fixtures_yc$mls_4_2 + MLS_fixtures_yc$mls_4_3 +
    MLS_fixtures_yc$mls_5_0 + MLS_fixtures_yc$mls_5_1 + MLS_fixtures_yc$mls_5_2 + MLS_fixtures_yc$mls_5_3 + MLS_fixtures_yc$mls_5_4 +
    MLS_fixtures_yc$mls_6_0 + MLS_fixtures_yc$mls_6_1 + MLS_fixtures_yc$mls_6_2 + MLS_fixtures_yc$mls_6_3 + MLS_fixtures_yc$mls_6_4 +
    MLS_fixtures_yc$mls_6_5
)

MLS_fixtures_yc$mls_H <- percent(MLS_fixtures_yc$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_yc$mls_D <- (

  MLS_fixtures_yc$mls_0_0 + MLS_fixtures_yc$mls_1_1 + MLS_fixtures_yc$mls_2_2 + MLS_fixtures_yc$mls_3_3 + MLS_fixtures_yc$mls_4_4 +
    MLS_fixtures_yc$mls_5_5 + MLS_fixtures_yc$mls_6_6
)

MLS_fixtures_yc$mls_D <- percent(MLS_fixtures_yc$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_yc$mls_A <- (
  MLS_fixtures_yc$mls_0_1 + MLS_fixtures_yc$mls_0_2 + MLS_fixtures_yc$mls_1_2 + MLS_fixtures_yc$mls_0_3 + MLS_fixtures_yc$mls_1_3 +
    MLS_fixtures_yc$mls_2_3 + MLS_fixtures_yc$mls_0_4 + MLS_fixtures_yc$mls_1_4 + MLS_fixtures_yc$mls_2_4 + MLS_fixtures_yc$mls_3_4 +
    MLS_fixtures_yc$mls_0_5 + MLS_fixtures_yc$mls_1_5 + MLS_fixtures_yc$mls_2_5 + MLS_fixtures_yc$mls_3_5 + MLS_fixtures_yc$mls_4_5 +
    MLS_fixtures_yc$mls_0_6 + MLS_fixtures_yc$mls_1_6 + MLS_fixtures_yc$mls_2_6 + MLS_fixtures_yc$mls_3_6 + MLS_fixtures_yc$mls_4_6 +
    MLS_fixtures_yc$mls_5_6
)

MLS_fixtures_yc$mls_A <- percent(MLS_fixtures_yc$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_yc$mls_ov25 <- (
  MLS_fixtures_yc$mls_2_1 + MLS_fixtures_yc$mls_1_2 + MLS_fixtures_yc$mls_2_2 + MLS_fixtures_yc$mls_3_0 + MLS_fixtures_yc$mls_3_1 +
    MLS_fixtures_yc$mls_3_2 + MLS_fixtures_yc$mls_0_3 + MLS_fixtures_yc$mls_1_3 + MLS_fixtures_yc$mls_2_3 + MLS_fixtures_yc$mls_3_3 +
    MLS_fixtures_yc$mls_4_0 + MLS_fixtures_yc$mls_4_1 + MLS_fixtures_yc$mls_4_2 + MLS_fixtures_yc$mls_4_3 + MLS_fixtures_yc$mls_0_4 +
    MLS_fixtures_yc$mls_1_4 + MLS_fixtures_yc$mls_2_4 + MLS_fixtures_yc$mls_3_4 + MLS_fixtures_yc$mls_4_4 + MLS_fixtures_yc$mls_5_0 +
    MLS_fixtures_yc$mls_5_1 + MLS_fixtures_yc$mls_5_2 + MLS_fixtures_yc$mls_5_3 + MLS_fixtures_yc$mls_5_4 + MLS_fixtures_yc$mls_0_5 +
    MLS_fixtures_yc$mls_1_5 + MLS_fixtures_yc$mls_2_5 + MLS_fixtures_yc$mls_3_5 + MLS_fixtures_yc$mls_4_5 + MLS_fixtures_yc$mls_5_5 +
    MLS_fixtures_yc$mls_6_0 + MLS_fixtures_yc$mls_6_1 + MLS_fixtures_yc$mls_6_2 + MLS_fixtures_yc$mls_6_3 + MLS_fixtures_yc$mls_6_4 +
    MLS_fixtures_yc$mls_6_5 + MLS_fixtures_yc$mls_0_6 + MLS_fixtures_yc$mls_1_6 + MLS_fixtures_yc$mls_2_6 + MLS_fixtures_yc$mls_3_6 +
    MLS_fixtures_yc$mls_4_6 + MLS_fixtures_yc$mls_5_6 + MLS_fixtures_yc$mls_6_6
)
#un25
MLS_fixtures_yc$mls_un25 <- (
  MLS_fixtures_yc$mls_0_0 + MLS_fixtures_yc$mls_1_0 + MLS_fixtures_yc$mls_0_1 + MLS_fixtures_yc$mls_1_1 + MLS_fixtures_yc$mls_2_0 + MLS_fixtures_yc$mls_0_2
)
#odds
MLS_fixtures_yc$mls_ov25_odds <- round((1/MLS_fixtures_yc$mls_ov25),digits = 2)
MLS_fixtures_yc$mls_un25_odds <- round((1/MLS_fixtures_yc$mls_un25),digits = 2)

MLS_fixtures_yc$mls_ov25_odds
MLS_fixtures_yc$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_yc$mls_ov25 <- percent(MLS_fixtures_yc$mls_ov25, accuracy = 0.1)

MLS_fixtures_yc$mls_un25 <- percent(MLS_fixtures_yc$mls_un25, accuracy = 0.1)
MLS_fixtures_yc$mls_pscore <- paste(round(MLS_fixtures_yc$mls_xHYC,digits = 0),round(MLS_fixtures_yc$mls_xAYC,digits = 0),sep = "-")

################################################################################################################################################################################
#poisson corners
mls_GP <- nrow(MLS)
#Calculate total home corners for each division
mls_home_corners <- aggregate(MLS$HCO, by = list(MLS$HomeTeam), FUN = sum)
mls_away_corners <- aggregate(MLS$ACO, by = list(MLS$AwayTeam), FUN = sum)
###############################################################################
mls_T_HCO <- sum(mls_home_corners$x)
#calculate average home corners
mls_avg_HCO <- round(mls_T_HCO /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_ACO <- sum(mls_away_corners$x)
#calculate average away goal
mls_avg_ACO <- round(mls_T_ACO /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_coas <- round(((mls_home_corners$x/mls_home_games))/mls_avg_HCO, digits = 4)
#calculate away attack strength
mls_away_coas <- round(((mls_away_corners$x/mls_away_games))/mls_avg_ACO, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HCOC <- round(mls_T_ACO /mls_GP, digits = 4)
#avg away concede
mls_avg_ACOC <- round(mls_T_HCO /mls_GP, digits = 4)
#calculate home and away defense strength
#home corners conceded
mls_home_coc <- aggregate(MLS$ACO, by = list(MLS$HomeTeam), FUN = sum)
mls_away_coc <- aggregate(MLS$HCO, by = list(MLS$AwayTeam), FUN = sum)
#home defense strength
mls_home_cods <- round(((mls_home_coc$x/mls_home_games))/mls_avg_HCOC, digits = 4)
#away defense strength
mls_away_cods <- round(((mls_away_coc$x/mls_away_games))/mls_avg_ACOC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_corners <- cbind(mls_division,mls_teams,mls_avg_HCO,mls_home_coas,mls_home_cods)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_corners <- cbind(mls_division,mls_teams,mls_avg_ACO,mls_away_coas,mls_away_cods)

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
######################################################################################################################################################################
#poisson fouls
mls_GP <- nrow(MLS)
#Calculate total home goals for each division
mls_T_HF <- sum(mls_home_fouls$x)
#calculate average home goal
mls_avg_HF <- round(mls_T_HF /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AF <- sum(mls_away_fouls$x)
#calculate average away goal
mls_avg_AF <- round(mls_T_AF /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_fas <- round(((mls_home_fouls$x/mls_home_games))/mls_avg_HF, digits = 4)
#calculate away attack strength
mls_away_fas <- round(((mls_away_fouls$x/mls_away_games))/mls_avg_AF, digits = 4)

################################################################################
#get average home concede and away concede
mls_avg_HFC <- round(mls_T_AF /mls_GP, digits = 4)
#avg away concede
mls_avg_AFC <- round(mls_T_HF /mls_GP, digits = 4)
#calculate home and away defense strength
#home yellow cards conceded
mls_home_fcc <- aggregate(MLS$AF, by = list(MLS$HomeTeam), FUN = sum)
mls_away_fcc <- aggregate(MLS$HF, by = list(MLS$AwayTeam), FUN = sum)

#home defense strength
mls_home_fds <- round(((mls_home_fcc$x/mls_home_games))/mls_avg_HFC, digits = 4)

#away defense strength
mls_away_fds <- round(((mls_away_fcc$x/mls_away_games))/mls_avg_AFC, digits = 4)

#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_fo <- cbind(mls_division,mls_teams,mls_avg_HF,mls_home_fas,mls_home_fds)

#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_fo <- cbind(mls_division,mls_teams,mls_avg_AF,mls_away_fas,mls_away_fds)

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
####################################################################################################################################################################
#poisson shots
mls_GP <- nrow(MLS)

#Calculate total home goals for each division
mls_T_HST <- sum(mls_home_hst$x)
#calculate average home goal

mls_avg_HST <- round(mls_T_HST /mls_GP, digits = 4)

############################################################
#Calculate total away goals for each division
mls_T_AST <- sum(mls_away_ast$x)
#calculate average away goal
mls_avg_AST <- round(mls_T_AST /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_sotas <- round(((mls_home_hst$x/mls_home_games))/mls_avg_HST, digits = 4)
#calculate away attack strength
mls_away_sotas <- round(((mls_away_ast$x/mls_away_games))/mls_avg_AST, digits = 4)

################################################################################
#get average home concede and away concede
mls_avg_HSC <- round(mls_T_AST /mls_GP, digits = 4)

#avg away concede
mls_avg_ASC <- round(mls_T_HST /mls_GP, digits = 4)
#home defense strength
mls_home_sods <- round(((mls_home_hsc$x/mls_home_games))/mls_avg_HSC, digits = 4)

#away defense strength
mls_away_sods <- round(((mls_away_ast$x/mls_away_games))/mls_avg_ASC, digits = 4)

#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_sot <- cbind(mls_division,mls_teams,mls_avg_HST,mls_home_sotas,mls_home_sods)

#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_sot <- cbind(mls_division,mls_teams,mls_avg_AST,mls_away_sotas,mls_away_sods)

#MLS
HomeTeam_mls_sot <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_sot <- rep(mls_teams, length(mls_teams))
MLS_fixtures_sot <- cbind(HomeTeam_mls_sot,AwayTeam_mls_sot)
MLS_fixtures_sot <- as.data.frame(MLS_fixtures_sot)
MLS_fixtures_sot <- MLS_fixtures_sot[!MLS_fixtures_sot$HomeTeam_mls_sot == MLS_fixtures_sot$AwayTeam_mls_sot,]
rownames(MLS_fixtures_sot) <- NULL
MLS_fixtures_sot$Div <- "MLS"
MLS_fixtures_sot <- MLS_fixtures_sot[,c(3,1,2)]

MLS_fixtures_sot$avg_HST_mls <- mls_avg_HST

MLS_fixtures_sot$mls_homesotas <- rep(mls_home_sotas,each = length(mls_teams)-1)

mls_awaysods_lookup <- cbind(mls_teams,mls_away_sods)

mls_awaysods_lookup <- as.data.frame(mls_awaysods_lookup)

colnames(mls_awaysods_lookup) <- c("AwayTeam_mls_sot","mls_awaysods")


require('RH2')
MLS_fixtures_sot$mls_awaysods <- sqldf("SELECT mls_awaysods_lookup.mls_awaysods FROM mls_awaysods_lookup INNER JOIN MLS_fixtures_sot ON mls_awaysods_lookup.AwayTeam_mls_sot = MLS_fixtures_sot.AwayTeam_mls_sot")

MLS_fixtures_sot$avg_AST_mls <- mls_avg_AST

mls_awaysotas_lookup <- cbind(mls_teams,mls_away_sotas)

mls_awaysotas_lookup <- as.data.frame(mls_awaysotas_lookup)

colnames(mls_awaysotas_lookup) <- c("AwayTeam_mls_sot","mls_awaysotas")

MLS_fixtures_sot$mls_awaysotas <- sqldf("SELECT mls_awaysotas_lookup.mls_awaysotas FROM mls_awaysotas_lookup INNER JOIN MLS_fixtures_sot ON mls_awaysotas_lookup.AwayTeam_mls_sot = MLS_fixtures_sot.AwayTeam_mls_sot")

MLS_fixtures_sot$mls_homesods <- rep(mls_home_sods,each = length(mls_teams)-1)

MLS_fixtures_sot$mls_awaysods <- as.numeric(unlist(MLS_fixtures_sot$mls_awaysods))
#xGH
MLS_fixtures_sot$mls_xHST <- MLS_fixtures_sot$avg_HST_mls * MLS_fixtures_sot$mls_homesotas * MLS_fixtures_sot$mls_awaysods
#xGA

MLS_fixtures_sot$mls_awaysotas <- as.numeric(unlist(MLS_fixtures_sot$mls_awaysotas))

MLS_fixtures_sot$mls_xAST <- MLS_fixtures_sot$avg_AST_mls * MLS_fixtures_sot$mls_awaysotas * MLS_fixtures_sot$mls_homesods

MLS_fixtures_sot$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
#Home win
MLS_fixtures_sot$mls_H <- (
  MLS_fixtures_sot$mls_1_0 + MLS_fixtures_sot$mls_2_0 + MLS_fixtures_sot$mls_2_1 + MLS_fixtures_sot$mls_3_0 + MLS_fixtures_sot$mls_3_1 +
    MLS_fixtures_sot$mls_3_2 + MLS_fixtures_sot$mls_4_0 + MLS_fixtures_sot$mls_4_1 + MLS_fixtures_sot$mls_4_2 + MLS_fixtures_sot$mls_4_3 +
    MLS_fixtures_sot$mls_5_0 + MLS_fixtures_sot$mls_5_1 + MLS_fixtures_sot$mls_5_2 + MLS_fixtures_sot$mls_5_3 + MLS_fixtures_sot$mls_5_4 +
    MLS_fixtures_sot$mls_6_0 + MLS_fixtures_sot$mls_6_1 + MLS_fixtures_sot$mls_6_2 + MLS_fixtures_sot$mls_6_3 + MLS_fixtures_sot$mls_6_4 +
    MLS_fixtures_sot$mls_6_5
)

MLS_fixtures_sot$mls_H <- percent(MLS_fixtures_sot$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_sot$mls_D <- (

  MLS_fixtures_sot$mls_0_0 + MLS_fixtures_sot$mls_1_1 + MLS_fixtures_sot$mls_2_2 + MLS_fixtures_sot$mls_3_3 + MLS_fixtures_sot$mls_4_4 +
    MLS_fixtures_sot$mls_5_5 + MLS_fixtures_sot$mls_6_6
)

MLS_fixtures_sot$mls_D <- percent(MLS_fixtures_sot$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_sot$mls_A <- (
  MLS_fixtures_sot$mls_0_1 + MLS_fixtures_sot$mls_0_2 + MLS_fixtures_sot$mls_1_2 + MLS_fixtures_sot$mls_0_3 + MLS_fixtures_sot$mls_1_3 +
    MLS_fixtures_sot$mls_2_3 + MLS_fixtures_sot$mls_0_4 + MLS_fixtures_sot$mls_1_4 + MLS_fixtures_sot$mls_2_4 + MLS_fixtures_sot$mls_3_4 +
    MLS_fixtures_sot$mls_0_5 + MLS_fixtures_sot$mls_1_5 + MLS_fixtures_sot$mls_2_5 + MLS_fixtures_sot$mls_3_5 + MLS_fixtures_sot$mls_4_5 +
    MLS_fixtures_sot$mls_0_6 + MLS_fixtures_sot$mls_1_6 + MLS_fixtures_sot$mls_2_6 + MLS_fixtures_sot$mls_3_6 + MLS_fixtures_sot$mls_4_6 +
    MLS_fixtures_sot$mls_5_6
)

MLS_fixtures_sot$mls_A <- percent(MLS_fixtures_sot$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_sot$mls_ov25 <- (
  MLS_fixtures_sot$mls_2_1 + MLS_fixtures_sot$mls_1_2 + MLS_fixtures_sot$mls_2_2 + MLS_fixtures_sot$mls_3_0 + MLS_fixtures_sot$mls_3_1 +
    MLS_fixtures_sot$mls_3_2 + MLS_fixtures_sot$mls_0_3 + MLS_fixtures_sot$mls_1_3 + MLS_fixtures_sot$mls_2_3 + MLS_fixtures_sot$mls_3_3 +
    MLS_fixtures_sot$mls_4_0 + MLS_fixtures_sot$mls_4_1 + MLS_fixtures_sot$mls_4_2 + MLS_fixtures_sot$mls_4_3 + MLS_fixtures_sot$mls_0_4 +
    MLS_fixtures_sot$mls_1_4 + MLS_fixtures_sot$mls_2_4 + MLS_fixtures_sot$mls_3_4 + MLS_fixtures_sot$mls_4_4 + MLS_fixtures_sot$mls_5_0 +
    MLS_fixtures_sot$mls_5_1 + MLS_fixtures_sot$mls_5_2 + MLS_fixtures_sot$mls_5_3 + MLS_fixtures_sot$mls_5_4 + MLS_fixtures_sot$mls_0_5 +
    MLS_fixtures_sot$mls_1_5 + MLS_fixtures_sot$mls_2_5 + MLS_fixtures_sot$mls_3_5 + MLS_fixtures_sot$mls_4_5 + MLS_fixtures_sot$mls_5_5 +
    MLS_fixtures_sot$mls_6_0 + MLS_fixtures_sot$mls_6_1 + MLS_fixtures_sot$mls_6_2 + MLS_fixtures_sot$mls_6_3 + MLS_fixtures_sot$mls_6_4 +
    MLS_fixtures_sot$mls_6_5 + MLS_fixtures_sot$mls_0_6 + MLS_fixtures_sot$mls_1_6 + MLS_fixtures_sot$mls_2_6 + MLS_fixtures_sot$mls_3_6 +
    MLS_fixtures_sot$mls_4_6 + MLS_fixtures_sot$mls_5_6 + MLS_fixtures_sot$mls_6_6
)
#un25
MLS_fixtures_sot$mls_un25 <- (
  MLS_fixtures_sot$mls_0_0 + MLS_fixtures_sot$mls_1_0 + MLS_fixtures_sot$mls_0_1 + MLS_fixtures_sot$mls_1_1 + MLS_fixtures_sot$mls_2_0 + MLS_fixtures_sot$mls_0_2
)
#odds
MLS_fixtures_sot$mls_ov25_odds <- round((1/MLS_fixtures_sot$mls_ov25),digits = 2)
MLS_fixtures_sot$mls_un25_odds <- round((1/MLS_fixtures_sot$mls_un25),digits = 2)

MLS_fixtures_sot$mls_ov25_odds
MLS_fixtures_sot$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_sot$mls_ov25 <- percent(MLS_fixtures_sot$mls_ov25, accuracy = 0.1)

MLS_fixtures_sot$mls_un25 <- percent(MLS_fixtures_sot$mls_un25, accuracy = 0.1)
MLS_fixtures_sot$mls_pssotre <- paste(round(MLS_fixtures_sot$mls_xHST,digits = 0),round(MLS_fixtures_sot$mls_xAST,digits = 0),sep = "-")
######################################################################################################################################################
#league table
#B1
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

#################################################################################################################################################
#################################################################################################################################################
#poisson model
mls_GP <- nrow(MLS)

#Calculate total home goals for each division
mls_T_HG <- sum(mls_home_gs$x)

#calculate average home goal
mls_avg_HG <- round(mls_T_HG /mls_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AG <- sum(mls_away_gs$x)
#calculate average away goal
mls_avg_AG <- round(mls_T_AG /mls_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength
mls_home_as <- round(((mls_home_gs$x/mls_home_games))/mls_avg_HG, digits = 4)
#calculate away attack strength
mls_away_as <- round(((mls_away_gs$x/mls_away_games))/mls_avg_AG, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HC <- round(mls_T_AG /mls_GP, digits = 4)
#avg away concede
mls_avg_AC <- round(mls_T_HG /mls_GP, digits = 4)
#calculate home and away defense strength
#home defense strength
mls_home_ds <- round(((mls_home_gc$x/mls_home_games))/mls_avg_HC, digits = 4)
#away defense strength
mls_away_ds <- round(((mls_away_gc$x/mls_away_games))/mls_avg_AC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson <- cbind(mls_division,mls_teams,mls_avg_HG,mls_home_as,mls_home_ds)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson <- cbind(mls_division,mls_teams,mls_avg_AG,mls_away_as,mls_away_ds)

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
############################################################################################################################################################
#Last six
mls_last_n_games <- 6

#create final_mls_hf object
final_mls_hf <- c()
for(index_mls_hf in 1:length(mls_teams))
{
  index_mls_hf <- row.names(mls_form_h) == mls_teams[index_mls_hf]
  form_mls_hf <- mls_form_h[index_mls_hf]
  deleted_form_mls_hf <- form_mls_hf[!form_mls_hf[] == ""]
  l6_form_mls_hf <- tail(deleted_form_mls_hf,mls_last_n_games)
  l6_form_mls_hf <- paste(l6_form_mls_hf,collapse = " ")
  final_mls_hf[index_mls_hf] <- rbind(paste(mls_teams[index_mls_hf],l6_form_mls_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}

#change column nam
final_mls_hf <- as.data.frame(final_mls_hf)
colnames(final_mls_hf) <- "Form"
#goals scored
#create final_mls_gs object
final_mls_gs <- c()
suml6_mls_gs <- c()
for(index_mls_gs in 1:length(mls_teams))
{
  index_mls_gs <- row.names(mls_goalscored_h) == mls_teams[index_mls_gs]
  form_mls_gs <- mls_goalscored_h[index_mls_gs]
  deleted_form_mls_gs <- form_mls_gs[!form_mls_gs[] == ""]
  l6_form_mls_gs <- tail(deleted_form_mls_gs,mls_last_n_games)
  l6_form_mls_gs <- as.numeric(l6_form_mls_gs)
  suml6_mls_gs[index_mls_gs] <- sum(l6_form_mls_gs)
  suml6_mls_gs[index_mls_gs] <- paste("(",suml6_mls_gs[index_mls_gs],")",sep = "")
  l6_form_mls_gs <- paste(l6_form_mls_gs,collapse = " ")
  final_mls_gs[index_mls_gs] <- rbind(paste(mls_teams[index_mls_gs],l6_form_mls_gs,suml6_mls_gs[index_mls_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_gs
#change column names
final_mls_gs <- as.data.frame(final_mls_gs)
colnames(final_mls_gs) <- "Goals scored"
#goal conceded
#create final_mls_gc object
final_mls_gc <- c()
suml6_mls_gc <- c()
for(index_mls_gc in 1:length(mls_teams))
{
  index_mls_gc <- row.names(mls_goalconceded_h) == mls_teams[index_mls_gc]
  form_mls_gc <- mls_goalconceded_h[index_mls_gc]
  deleted_form_mls_gc <- form_mls_gc[!form_mls_gc[] == ""]
  l6_form_mls_gc <- tail(deleted_form_mls_gc,mls_last_n_games)
  l6_form_mls_gc <- as.numeric(l6_form_mls_gc)
  suml6_mls_gc[index_mls_gc] <- sum(l6_form_mls_gc)
  suml6_mls_gc[index_mls_gc] <- paste("(",suml6_mls_gc[index_mls_gc],")",sep = "")
  l6_form_mls_gc <- paste(l6_form_mls_gc,collapse = " ")
  final_mls_gc[index_mls_gc] <- rbind(paste(mls_teams[index_mls_gc],l6_form_mls_gc,suml6_mls_gc[index_mls_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
#change column names
final_mls_gc <- as.data.frame(final_mls_gc)
colnames(final_mls_gc) <- "Goals conceded"


toString(l6_form_mls_gc)
#total goals
#create final_mls_tg object
final_mls_tg <- c()
suml6_mls_tg <- c()
for(index_mls_tg in 1:length(mls_teams))
{
  index_mls_tg <- row.names(mls_totalgoals_h) == mls_teams[index_mls_tg]
  form_mls_tg <- mls_totalgoals_h[index_mls_tg]
  deleted_form_mls_tg <- form_mls_tg[!form_mls_tg[] == ""]
  l6_form_mls_tg <- tail(deleted_form_mls_tg,mls_last_n_games)
  l6_form_mls_tg <- as.numeric(l6_form_mls_tg)
  suml6_mls_tg[index_mls_tg] <- sum(l6_form_mls_tg)
  suml6_mls_tg[index_mls_tg] <- paste("(",suml6_mls_tg[index_mls_tg],")",sep = "")
  l6_form_mls_tg <- paste(l6_form_mls_tg,collapse = " ")
  final_mls_tg[index_mls_tg] <- rbind(paste(mls_teams[index_mls_tg],l6_form_mls_tg,suml6_mls_tg[index_mls_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
#change column names
final_mls_tg <- as.data.frame(final_mls_tg)
colnames(final_mls_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_mls_hf object
final_mls_cs <- c()
for(index_mls_cs in 1:length(mls_teams))
{
  index_mls_cs <- row.names(mls_csform_h) == mls_teams[index_mls_cs]
  csform_mls_cs <- mls_csform_h[index_mls_cs]
  deleted_csform_mls_cs <- csform_mls_cs[!csform_mls_cs[] == ""]
  l6_csform_mls_cs <- tail(deleted_csform_mls_cs,mls_last_n_games)
  l6_csform_mls_cs <- paste(l6_csform_mls_cs,collapse = " ")
  final_mls_cs[index_mls_cs] <- rbind(paste(mls_teams[index_mls_cs],l6_csform_mls_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",mls_teams[index],l6_csform)

}

#change column names
final_mls_cs <- as.data.frame(final_mls_cs)
colnames(final_mls_cs) <- "CSForm"
#################################################
#Win Margin
#goals scored
#create final_mls_wm object
final_mls_wm <- c()
suml6_mls_wm <- c()
for(index_mls_wm in 1:length(mls_teams))
{
  index_mls_wm <- row.names(mls_winmargin_h) == mls_teams[index_mls_wm]
  form_mls_wm <- mls_winmargin_h[index_mls_wm]
  deleted_form_mls_wm <- form_mls_wm[!form_mls_wm[] == ""]
  l6_form_mls_wm <- tail(deleted_form_mls_wm,mls_last_n_games)
  l6_form_mls_wm <- as.numeric(l6_form_mls_wm)
  suml6_mls_wm[index_mls_wm] <- sum(l6_form_mls_wm)
  suml6_mls_wm[index_mls_wm] <- paste("(",suml6_mls_wm[index_mls_wm],")",sep = "")
  l6_form_mls_wm <- paste(l6_form_mls_wm,collapse = " ")
  final_mls_wm[index_mls_wm] <- rbind(paste(mls_teams[index_mls_wm],l6_form_mls_wm,suml6_mls_wm[index_mls_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_wm
#change column names
final_mls_wm <- as.data.frame(final_mls_wm)
colnames(final_mls_wm) <- "Win Margin"
#################################################
##################################################
#corners awarded
#create final_mls_ca object
final_mls_ca <- c()
suml6_mls_ca <- c()
for(index_mls_ca in 1:length(mls_teams))
{
  index_mls_ca <- row.names(mls_coawarded_h) == mls_teams[index_mls_ca]
  form_mls_ca <- mls_coawarded_h[index_mls_ca]
  deleted_form_mls_ca <- form_mls_ca[!form_mls_ca[] == ""]
  l6_form_mls_ca <- tail(deleted_form_mls_ca,mls_last_n_games)
  l6_form_mls_ca <- as.numeric(l6_form_mls_ca)
  suml6_mls_ca[index_mls_ca] <- sum(l6_form_mls_ca)
  suml6_mls_ca[index_mls_ca] <- paste("(",suml6_mls_ca[index_mls_ca],")",sep = "")
  l6_form_mls_ca <- paste(l6_form_mls_ca,collapse = " ")
  final_mls_ca[index_mls_ca] <- rbind(paste(mls_teams[index_mls_ca],l6_form_mls_ca,suml6_mls_ca[index_mls_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_ca
#change column names
final_mls_ca <- as.data.frame(final_mls_ca)
colnames(final_mls_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_mls_ca object
final_mls_cc <- c()
suml6_mls_cc <- c()
for(index_mls_cc in 1:length(mls_teams))
{
  index_mls_cc <- row.names(mls_cornersconceded_h) == mls_teams[index_mls_cc]
  form_mls_cc <- mls_cornersconceded_h[index_mls_cc]
  deleted_form_mls_cc <- form_mls_cc[!form_mls_cc[] == ""]
  l6_form_mls_cc <- tail(deleted_form_mls_cc,mls_last_n_games)
  l6_form_mls_cc <- as.numeric(l6_form_mls_cc)
  suml6_mls_cc[index_mls_cc] <- sum(l6_form_mls_cc)
  suml6_mls_cc[index_mls_cc] <- paste("(",suml6_mls_cc[index_mls_cc],")",sep = "")
  l6_form_mls_cc <- paste(l6_form_mls_cc,collapse = " ")
  final_mls_cc[index_mls_cc] <- rbind(paste(mls_teams[index_mls_cc],l6_form_mls_cc,suml6_mls_cc[index_mls_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_cc
#change column names
final_mls_cc <- as.data.frame(final_mls_cc)
colnames(final_mls_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_mls_cosc <- c()
for(index_mls_cosc in 1:length(mls_teams))
{
  index_mls_cosc <- row.names(mls_coscform_h) == mls_teams[index_mls_cosc]
  coscform_mls_cosc <- mls_coscform_h[index_mls_cosc]
  deleted_coscform_mls_cosc <- coscform_mls_cosc[!coscform_mls_cosc[] == ""]
  l6_coscform_mls_cosc <- tail(deleted_coscform_mls_cosc,mls_last_n_games)
  l6_coscform_mls_cosc <- paste(l6_coscform_mls_cosc,collapse = " ")
  final_mls_cosc[index_mls_cosc] <- rbind(paste(mls_teams[index_mls_cosc],l6_coscform_mls_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",mls_teams[index],l6_coscform)

}
final_mls_cosc
#change column names
final_mls_cosc <- as.data.frame(final_mls_cosc)
colnames(final_mls_cosc) <- "CornersForm"
##################################################
#total corners
#create final_mls_tcorners object
final_mls_tcorners <- c()
suml6_mls_tcorners <- c()
for(index_mls_tcorners in 1:length(mls_teams))
{
  index_mls_tcorners <- row.names(mls_totalcorners_h) == mls_teams[index_mls_tcorners]
  form_mls_tcorners <- mls_totalcorners_h[index_mls_tcorners]
  deleted_form_mls_tcorners <- form_mls_tcorners[!form_mls_tcorners[] == ""]
  l6_form_mls_tcorners <- tail(deleted_form_mls_tcorners,mls_last_n_games)
  l6_form_mls_tcorners <- as.numeric(l6_form_mls_tcorners)
  suml6_mls_tcorners[index_mls_tcorners] <- sum(l6_form_mls_tcorners)
  suml6_mls_tcorners[index_mls_tcorners] <- paste("(",suml6_mls_tcorners[index_mls_tcorners],")",sep = "")
  l6_form_mls_tcorners <- paste(l6_form_mls_tcorners,collapse = " ")
  final_mls_tcorners[index_mls_tcorners] <- rbind(paste(mls_teams[index_mls_tcorners],l6_form_mls_tcorners,suml6_mls_tcorners[index_mls_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
#change column names
final_mls_tcorners <- as.data.frame(final_mls_tcorners)
colnames(final_mls_tcorners) <- "TotalCorners"
###################################################
#Team against
#create final_mls_hf_against
final_mls_hf_against <- c()
for(index_mls_hf_against in 1:length(mls_teams))
{
  index_mls_hf_against <- row.names(mls_form_team_against_h) == mls_teams[index_mls_hf_against]
  form_mls_hf_against <- mls_form_team_against_h[index_mls_hf_against]
  deleted_form_mls_hf_against <- form_mls_hf_against[!form_mls_hf_against[] == ""]
  l6_form_mls_hf_against <- tail(deleted_form_mls_hf_against,mls_last_n_games)
  l6_form_mls_hf_against <- paste(l6_form_mls_hf_against,collapse = " ")
  final_mls_hf_against[index_mls_hf_against] <- rbind(paste(mls_teams[index_mls_hf_against],l6_form_mls_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",mls_teams[index],l6_form)

}
final_mls_hf_against <- as.data.frame(final_mls_hf_against)
colnames(final_mls_hf_against) <- "Team against"
#combine the columns
final_mls_all <- cbind(final_mls_hf,final_mls_gs,final_mls_gc,final_mls_tg,final_mls_ca,final_mls_cc,final_mls_tcorners,final_mls_cosc,final_mls_hf_against)
###################################################################################################################################################################################
#TABLE SIMULATION
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
###########################################################################################################################################################
#decision model
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
#############################################################################################################################################################################
#clone fixtures
MLS_fixtures_clone <- MLS_fixtures
colnames(MLS_fixtures_clone)[61] <- "Hwin"
colnames(MLS_fixtures_clone)[62] <- "Draw"
colnames(MLS_fixtures_clone)[63] <- "Awin"

MLS_fixtures_clone$Hwinodds <-   MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
  MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
  MLS_fixtures$mls_5_0 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
  MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
  MLS_fixtures$mls_6_5
MLS_fixtures_clone$Hwinodds <- round(1/MLS_fixtures_clone$Hwinodds, digits = 3)

MLS_fixtures_clone$Drawodds <-  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
  MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6

MLS_fixtures_clone$Drawodds <- round(1/MLS_fixtures_clone$Drawodds, digits = 3)

MLS_fixtures_clone$Awinodds <-   MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
  MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
  MLS_fixtures$mls_0_5 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
  MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
  MLS_fixtures$mls_5_6

MLS_fixtures_clone$Awinodds <- round(1/MLS_fixtures_clone$Awinodds, digits = 3)

colnames(MLS_fixtures_clone)[15] <- "CS_1-1"
colnames(MLS_fixtures_clone)[13] <- "CS_1-0"
colnames(MLS_fixtures_clone)[14] <- "CS_0-1"
colnames(MLS_fixtures_clone)[16] <- "CS_2-0"
colnames(MLS_fixtures_clone)[17] <- "CS_0-2"
colnames(MLS_fixtures_clone)[19] <- "CS_2-1"
colnames(MLS_fixtures_clone)[20] <- "CS_1-2"

MLS_fixtures_clone$`CS_1-1` <- round(1/MLS_fixtures_clone$`CS_1-1`, digits = 3)
MLS_fixtures_clone$`CS_1-0` <- round(1/MLS_fixtures_clone$`CS_1-0`, digits = 3)
MLS_fixtures_clone$`CS_0-1` <- round(1/MLS_fixtures_clone$`CS_0-1`, digits = 3)
MLS_fixtures_clone$`CS_2-0` <- round(1/MLS_fixtures_clone$`CS_2-0`, digits = 3)
MLS_fixtures_clone$`CS_0-2` <- round(1/MLS_fixtures_clone$`CS_0-2`, digits = 3)
MLS_fixtures_clone$`CS_2-1` <- round(1/MLS_fixtures_clone$`CS_2-1`, digits = 3)
MLS_fixtures_clone$`CS_1-2` <- round(1/MLS_fixtures_clone$`CS_1-2`, digits = 3)

colnames(MLS_fixtures_clone)[1] <- "league"
colnames(MLS_fixtures_clone)[2] <- "Hometeam"
colnames(MLS_fixtures_clone)[3] <- "Awayteam"
colnames(MLS_fixtures_clone)[92] <- "predscore"
colnames(MLS_fixtures_clone)[64] <- "ov25"
colnames(MLS_fixtures_clone)[66] <- "ov25odds"
colnames(MLS_fixtures_clone)[65] <- "un25"
colnames(MLS_fixtures_clone)[67] <- "un25odds"
colnames(MLS_fixtures_clone)[68] <- "BTTSY"
colnames(MLS_fixtures_clone)[69] <- "BTTSN"
colnames(MLS_fixtures_clone)[70] <- "BTTSYodds"
colnames(MLS_fixtures_clone)[71] <- "BTTSNodds"

MLS_fixtures_clone <- MLS_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
MLS_fixtures_clone$matchid <- paste(MLS_fixtures_clone$Hometeam,MLS_fixtures_clone$Awayteam,sep = '-')
####################################################################################################################################################
#all events
MLS_fixtures_clone_final <- MLS_fixtures_clone[,-c(8,9,10,27)]
MLS_fixtures_clone_final[,'sep'] <- ''

mls_dmprediction <-  mls_picks[,c(4,5,6,7,8)]
mls_dmprediction[,'sep2'] <- ''

mls_avgyellow <- mls_picks[,c(9,10)]
mls_avgyellow[,'sep3'] <- ''

mls_avgcorners <- mls_picks[,c(11,12)]
mls_avgcorners[,'sep4'] <- ''

mls_goals <- MLS_fixtures[,c(10,11)]
mls_goals$mls_xGH <- round(mls_goals$mls_xGH, digits = 2)
mls_goals$mls_xGA <- round(mls_goals$mls_xGA, digits = 2)
mls_goals$mls_TxG <- mls_goals$mls_xGH + mls_goals$mls_xGA
mls_goals[,'sep5'] <- ''

mls_shots <- MLS_fixtures_sot[,c(10,11)]
mls_shots$mls_xHST <- round(mls_shots$mls_xHST, digits = 2)
mls_shots$mls_xAST <- round(mls_shots$mls_xAST, digits = 2)
mls_shots$TxSOT <- mls_shots$mls_xHST + mls_shots$mls_xAST
mls_shots[,'sep6'] <- ''

mls_fouls <- MLS_fixtures_fo[,c(10,11)]
mls_fouls$mls_xHF <- round(mls_fouls$mls_xHF, digits = 2)
mls_fouls$mls_xAF <- round(mls_fouls$mls_xAF, digits = 2)
mls_fouls$mls_TxF <- mls_fouls$mls_xHF + mls_fouls$mls_xAF

mls_ycpf <- mls_picks[,c(15,16)]
mls_fouls <- cbind(mls_fouls,mls_ycpf)
mls_fouls$HYCPF <- as.numeric(mls_fouls$HYCPF)
mls_fouls$AYCPF <- as.numeric(mls_fouls$AYCPF)
mls_fouls$x_hyc <- (mls_fouls$mls_xHF) * (mls_fouls$HYCPF)
mls_fouls$x_ayc <- (mls_fouls$mls_xAF) * (mls_fouls$AYCPF)
mls_fouls$x_TYC <- round((mls_fouls$x_hyc + mls_fouls$x_ayc),digits = 2)
mls_fouls[,'sep7'] <- ''

mls_bookings <- MLS_fixtures_yc[,c(10,11)]
mls_bookings$mls_xHYC <- round(mls_bookings$mls_xHYC, digits = 2)
mls_bookings$mls_xAYC <- round(mls_bookings$mls_xAYC, digits = 2)
mls_bookings$mls_TYcards <- mls_bookings$mls_xHYC + mls_bookings$mls_xAYC
mls_bookings[,'sep8'] <- ''

mls_corners <- MLS_fixtures_co[,c(10,11)]
mls_corners$mls_xHCOC <- round(mls_corners$mls_xHCOC, digits = 2)
mls_corners$mls_xACOC <- round(mls_corners$mls_xACOC, digits = 2)
mls_corners$mls_TCOs <- mls_corners$mls_xHCOC + mls_corners$mls_xACOC
mls_corners[,'sep9'] <- ''

mls_shotsconversion <- mls_picks[,c(13,14)]
mls_shotsconversion <- cbind(mls_shotsconversion,mls_shots)
mls_shotsconversion$HXSC <- as.numeric(mls_shotsconversion$HXSC)
mls_shotsconversion$AXSC <- as.numeric(mls_shotsconversion$AXSC)
mls_shotsconversion$mls_hXgoals <- round((mls_shotsconversion$HXSC * mls_shotsconversion$mls_xHST), digits = 2)
mls_shotsconversion$mls_aXgoals <- round((mls_shotsconversion$AXSC * mls_shotsconversion$mls_xAST), digits = 2)
mls_shotsconversion$Xgoals <- mls_shotsconversion$mls_hXgoals + mls_shotsconversion$mls_aXgoals
options(java.parameters = "-Xmx4g")
MLS_all <- cbind(MLS_fixtures_clone_final,mls_dmprediction,mls_avgyellow,mls_avgcorners,mls_goals,mls_shots,mls_fouls,mls_bookings,mls_corners,mls_shotsconversion)
unlink('LASTMATCHES/MLS.xlsx')
write.xlsx(MLS_all,'LASTMATCHES/MLS.xlsx', sheetName = "MLS_all", append = TRUE)
write.xlsx(points_mls,'LASTMATCHES/MLS.xlsx', sheetName = "Table", append = TRUE)
write.xlsx(mls_cornertotalsv2,'LASTMATCHES/MLS.xlsx', sheetName = "Cornertotals", append = TRUE)
write.xlsx(mls_goaltotalsv2,'LASTMATCHES/MLS.xlsx', sheetName = "Goaltotals", append = TRUE)
write.xlsx(mls_yellowtotalsv2,'LASTMATCHES/MLS.xlsx', sheetName = "Yellowtotals", append = TRUE)


##write.csv(MLS_fixtures[,c(1,2,3,4,5,6)],'SP2_schedule20232024.csv')
