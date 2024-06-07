library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('R.utils')
############################
# #create last n games
mls_last_n_games <- 6
arg_last_n_games <- 6
# d2_last_n_games <- 6
# e0_last_n_games <- 6
# e1_last_n_games <- 6
# e2_last_n_games <- 6
# e3_last_n_games <- 6
# ec_last_n_games <- 6
# f1_last_n_games <- 6
# f2_last_n_games <- 6
# g1_last_n_games <- 6
# i1_last_n_games <- 6
# i2_last_n_games <- 6
# n1_last_n_games <- 6
# p1_last_n_games <- 6
# sc0_last_n_games <- 6
# sc1_last_n_games <- 6
# sc2_last_n_games <- 6
# sc3_last_n_games <- 6
# sp1_last_n_games <- 6
# sp2_last_n_games <- 6
# t1_last_n_games <- 6
#create last total games
 mls_last_n_games <- mls_games_played[1]
 arg_last_n_games <- arg_games_played[1]
# d2_last_n_games <- d2_games_played[1]
# e0_last_n_games <- e0_games_played[1]
# e1_last_n_games <- e1_games_played[1]
# e2_last_n_games <- e2_games_played[1]
# e3_last_n_games <- e3_games_played[1]
# ec_last_n_games <- ec_games_played[1]
# f1_last_n_games <- f1_games_played[1]
# f2_last_n_games <- f2_games_played[1]
# g1_last_n_games <- g1_games_played[1]
# i1_last_n_games <- i1_games_played[1]
# i2_last_n_games <- i2_games_played[1]
# n1_last_n_games <- n1_games_played[1]
# p1_last_n_games <- p1_games_played[1]
# sc0_last_n_games <- sc0_games_played[1]
# sc1_last_n_games <- sc1_games_played[1]
# sc2_last_n_games <- sc2_games_played[1]
# sc3_last_n_games <- sc3_games_played[1]
# sp1_last_n_games <- sp1_games_played[1]
# sp2_last_n_games <- sp2_games_played[1]
# t1_last_n_games <- t1_games_played[1]
# ########################################
########################################

#MLS
#form
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
write.xlsx(final_mls_all,'Divisions/MLS.xlsx',sheetName = "L6", append = TRUE)
################################################################################
#ARG
#form
#create final_arg_hf object
final_arg_hf <- c()
for(index_arg_hf in 1:length(arg_teams))
{
  index_arg_hf <- row.names(arg_form_h) == arg_teams[index_arg_hf]
  form_arg_hf <- arg_form_h[index_arg_hf]
  deleted_form_arg_hf <- form_arg_hf[!form_arg_hf[] == ""]
  l6_form_arg_hf <- tail(deleted_form_arg_hf,arg_last_n_games)
  l6_form_arg_hf <- paste(l6_form_arg_hf,collapse = " ")
  final_arg_hf[index_arg_hf] <- rbind(paste(arg_teams[index_arg_hf],l6_form_arg_hf, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}

#change column names
final_arg_hf <- as.data.frame(final_arg_hf)
colnames(final_arg_hf) <- "Form"
#goals scored
#create final_arg_gs object
final_arg_gs <- c()
suml6_arg_gs <- c()
for(index_arg_gs in 1:length(arg_teams))
{
  index_arg_gs <- row.names(arg_goalscored_h) == arg_teams[index_arg_gs]
  form_arg_gs <- arg_goalscored_h[index_arg_gs]
  deleted_form_arg_gs <- form_arg_gs[!form_arg_gs[] == ""]
  l6_form_arg_gs <- tail(deleted_form_arg_gs,arg_last_n_games)
  l6_form_arg_gs <- as.numeric(l6_form_arg_gs)
  suml6_arg_gs[index_arg_gs] <- sum(l6_form_arg_gs)
  suml6_arg_gs[index_arg_gs] <- paste("(",suml6_arg_gs[index_arg_gs],")",sep = "")
  l6_form_arg_gs <- paste(l6_form_arg_gs,collapse = " ")
  final_arg_gs[index_arg_gs] <- rbind(paste(arg_teams[index_arg_gs],l6_form_arg_gs,suml6_arg_gs[index_arg_gs], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_gs
#change column names
final_arg_gs <- as.data.frame(final_arg_gs)
colnames(final_arg_gs) <- "Goals scored"
#goal conceded
#create final_arg_gc object
final_arg_gc <- c()
suml6_arg_gc <- c()
for(index_arg_gc in 1:length(arg_teams))
{
  index_arg_gc <- row.names(arg_goalconceded_h) == arg_teams[index_arg_gc]
  form_arg_gc <- arg_goalconceded_h[index_arg_gc]
  deleted_form_arg_gc <- form_arg_gc[!form_arg_gc[] == ""]
  l6_form_arg_gc <- tail(deleted_form_arg_gc,arg_last_n_games)
  l6_form_arg_gc <- as.numeric(l6_form_arg_gc)
  suml6_arg_gc[index_arg_gc] <- sum(l6_form_arg_gc)
  suml6_arg_gc[index_arg_gc] <- paste("(",suml6_arg_gc[index_arg_gc],")",sep = "")
  l6_form_arg_gc <- paste(l6_form_arg_gc,collapse = " ")
  final_arg_gc[index_arg_gc] <- rbind(paste(arg_teams[index_arg_gc],l6_form_arg_gc,suml6_arg_gc[index_arg_gc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}

#change column names
final_arg_gc <- as.data.frame(final_arg_gc)
colnames(final_arg_gc) <- "Goals conceded"
#total goals
#create final_arg_tg object
final_arg_tg <- c()
suml6_arg_tg <- c()
for(index_arg_tg in 1:length(arg_teams))
{
  index_arg_tg <- row.names(arg_totalgoals_h) == arg_teams[index_arg_tg]
  form_arg_tg <- arg_totalgoals_h[index_arg_tg]
  deleted_form_arg_tg <- form_arg_tg[!form_arg_tg[] == ""]
  l6_form_arg_tg <- tail(deleted_form_arg_tg,arg_last_n_games)
  l6_form_arg_tg <- as.numeric(l6_form_arg_tg)
  suml6_arg_tg[index_arg_tg] <- sum(l6_form_arg_tg)
  suml6_arg_tg[index_arg_tg] <- paste("(",suml6_arg_tg[index_arg_tg],")",sep = "")
  l6_form_arg_tg <- paste(l6_form_arg_tg,collapse = " ")
  final_arg_tg[index_arg_tg] <- rbind(paste(arg_teams[index_arg_tg],l6_form_arg_tg,suml6_arg_tg[index_arg_tg], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
#change column names
final_arg_tg <- as.data.frame(final_arg_tg)
colnames(final_arg_tg) <- "Total Goals"
###############################################
#Csfrom
#create final_arg_hf object
final_arg_cs <- c()
for(index_arg_cs in 1:length(arg_teams))
{
  index_arg_cs <- row.names(arg_csform_h) == arg_teams[index_arg_cs]
  csform_arg_cs <- arg_csform_h[index_arg_cs]
  deleted_csform_arg_cs <- csform_arg_cs[!csform_arg_cs[] == ""]
  l6_csform_arg_cs <- tail(deleted_csform_arg_cs,arg_last_n_games)
  l6_csform_arg_cs <- paste(l6_csform_arg_cs,collapse = " ")
  final_arg_cs[index_arg_cs] <- rbind(paste(arg_teams[index_arg_cs],l6_csform_arg_cs, sep = ",",collapse = ""))
  #bundescsform[] <- printf("%s\t%s\n",arg_teams[index],l6_csform)

}

#change column names
final_arg_cs <- as.data.frame(final_arg_cs)
colnames(final_arg_cs) <- "CSForm"
#################################################
#################################################
#Win Margin
#goals scored
#create final_arg_wm object
final_arg_wm <- c()
suml6_arg_wm <- c()
for(index_arg_wm in 1:length(arg_teams))
{
  index_arg_wm <- row.names(arg_winmargin_h) == arg_teams[index_arg_wm]
  form_arg_wm <- arg_winmargin_h[index_arg_wm]
  deleted_form_arg_wm <- form_arg_wm[!form_arg_wm[] == ""]
  l6_form_arg_wm <- tail(deleted_form_arg_wm,arg_last_n_games)
  l6_form_arg_wm <- as.numeric(l6_form_arg_wm)
  suml6_arg_wm[index_arg_wm] <- sum(l6_form_arg_wm)
  suml6_arg_wm[index_arg_wm] <- paste("(",suml6_arg_wm[index_arg_wm],")",sep = "")
  l6_form_arg_wm <- paste(l6_form_arg_wm,collapse = " ")
  final_arg_wm[index_arg_wm] <- rbind(paste(arg_teams[index_arg_wm],l6_form_arg_wm,suml6_arg_wm[index_arg_wm], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_wm
#change column names
final_arg_wm <- as.data.frame(final_arg_wm)
colnames(final_arg_wm) <- "Win Margin"
#################################################
#corners awarded
#create final_arg_ca object
final_arg_ca <- c()
suml6_arg_ca <- c()
for(index_arg_ca in 1:length(arg_teams))
{
  index_arg_ca <- row.names(arg_coawarded_h) == arg_teams[index_arg_ca]
  form_arg_ca <- arg_coawarded_h[index_arg_ca]
  deleted_form_arg_ca <- form_arg_ca[!form_arg_ca[] == ""]
  l6_form_arg_ca <- tail(deleted_form_arg_ca,arg_last_n_games)
  l6_form_arg_ca <- as.numeric(l6_form_arg_ca)
  suml6_arg_ca[index_arg_ca] <- sum(l6_form_arg_ca)
  suml6_arg_ca[index_arg_ca] <- paste("(",suml6_arg_ca[index_arg_ca],")",sep = "")
  l6_form_arg_ca <- paste(l6_form_arg_ca,collapse = " ")
  final_arg_ca[index_arg_ca] <- rbind(paste(arg_teams[index_arg_ca],l6_form_arg_ca,suml6_arg_ca[index_arg_ca], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_ca
#change column names
final_arg_ca <- as.data.frame(final_arg_ca)
colnames(final_arg_ca) <- "CornersAwarded"
##################################################
##################################################
#corners awarded
#create final_arg_ca object
final_arg_cc <- c()
suml6_arg_cc <- c()
for(index_arg_cc in 1:length(arg_teams))
{
  index_arg_cc <- row.names(arg_cornersconceded_h) == arg_teams[index_arg_cc]
  form_arg_cc <- arg_cornersconceded_h[index_arg_cc]
  deleted_form_arg_cc <- form_arg_cc[!form_arg_cc[] == ""]
  l6_form_arg_cc <- tail(deleted_form_arg_cc,arg_last_n_games)
  l6_form_arg_cc <- as.numeric(l6_form_arg_cc)
  suml6_arg_cc[index_arg_cc] <- sum(l6_form_arg_cc)
  suml6_arg_cc[index_arg_cc] <- paste("(",suml6_arg_cc[index_arg_cc],")",sep = "")
  l6_form_arg_cc <- paste(l6_form_arg_cc,collapse = " ")
  final_arg_cc[index_arg_cc] <- rbind(paste(arg_teams[index_arg_cc],l6_form_arg_cc,suml6_arg_cc[index_arg_cc], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_cc
#change column names
final_arg_cc <- as.data.frame(final_arg_cc)
colnames(final_arg_cc) <- "CornersConceded"
##################################################
##################################################
#corners form
final_arg_cosc <- c()
for(index_arg_cosc in 1:length(arg_teams))
{
  index_arg_cosc <- row.names(arg_coscform_h) == arg_teams[index_arg_cosc]
  coscform_arg_cosc <- arg_coscform_h[index_arg_cosc]
  deleted_coscform_arg_cosc <- coscform_arg_cosc[!coscform_arg_cosc[] == ""]
  l6_coscform_arg_cosc <- tail(deleted_coscform_arg_cosc,arg_last_n_games)
  l6_coscform_arg_cosc <- paste(l6_coscform_arg_cosc,collapse = " ")
  final_arg_cosc[index_arg_cosc] <- rbind(paste(arg_teams[index_arg_cosc],l6_coscform_arg_cosc, sep = ",",collapse = ""))
  #bundescoscform[] <- printf("%s\t%s\n",arg_teams[index],l6_coscform)

}
final_arg_cosc
#change column names
final_arg_cosc <- as.data.frame(final_arg_cosc)
colnames(final_arg_cosc) <- "CornersForm"
##################################################
#total corners
#create final_arg_tcorners object
final_arg_tcorners <- c()
suml6_arg_tcorners <- c()
for(index_arg_tcorners in 1:length(arg_teams))
{
  index_arg_tcorners <- row.names(arg_totalcorners_h) == arg_teams[index_arg_tcorners]
  form_arg_tcorners <- arg_totalcorners_h[index_arg_tcorners]
  deleted_form_arg_tcorners <- form_arg_tcorners[!form_arg_tcorners[] == ""]
  l6_form_arg_tcorners <- tail(deleted_form_arg_tcorners,arg_last_n_games)
  l6_form_arg_tcorners <- as.numeric(l6_form_arg_tcorners)
  suml6_arg_tcorners[index_arg_tcorners] <- sum(l6_form_arg_tcorners)
  suml6_arg_tcorners[index_arg_tcorners] <- paste("(",suml6_arg_tcorners[index_arg_tcorners],")",sep = "")
  l6_form_arg_tcorners <- paste(l6_form_arg_tcorners,collapse = " ")
  final_arg_tcorners[index_arg_tcorners] <- rbind(paste(arg_teams[index_arg_tcorners],l6_form_arg_tcorners,suml6_arg_tcorners[index_arg_tcorners], sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
#change column names
final_arg_tcorners <- as.data.frame(final_arg_tcorners)
colnames(final_arg_tcorners) <- "TotalCorners"
###################################################
##################################################
#Team against
#create final_arg_hf_against
final_arg_hf_against <- c()
for(index_arg_hf_against in 1:length(arg_teams))
{
  index_arg_hf_against <- row.names(arg_form_team_against_h) == arg_teams[index_arg_hf_against]
  form_arg_hf_against <- arg_form_team_against_h[index_arg_hf_against]
  deleted_form_arg_hf_against <- form_arg_hf_against[!form_arg_hf_against[] == ""]
  l6_form_arg_hf_against <- tail(deleted_form_arg_hf_against,arg_last_n_games)
  l6_form_arg_hf_against <- paste(l6_form_arg_hf_against,collapse = " ")
  final_arg_hf_against[index_arg_hf_against] <- rbind(paste(arg_teams[index_arg_hf_against],l6_form_arg_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
#################################################
#Team against
#create final_arg_hf_against
final_arg_hf_against <- c()
for(index_arg_hf_against in 1:length(arg_teams))
{
  index_arg_hf_against <- row.names(arg_form_team_against_h) == arg_teams[index_arg_hf_against]
  form_arg_hf_against <- arg_form_team_against_h[index_arg_hf_against]
  deleted_form_arg_hf_against <- form_arg_hf_against[!form_arg_hf_against[] == ""]
  l6_form_arg_hf_against <- tail(deleted_form_arg_hf_against,arg_last_n_games)
  l6_form_arg_hf_against <- paste(l6_form_arg_hf_against,collapse = " ")
  final_arg_hf_against[index_arg_hf_against] <- rbind(paste(arg_teams[index_arg_hf_against],l6_form_arg_hf_against, sep = ",",collapse = ""))
  #bundesform[] <- printf("%s\t%s\n",arg_teams[index],l6_form)

}
final_arg_hf_against <- as.data.frame(final_arg_hf_against)
colnames(final_arg_hf_against) <- "Team against"
#combine the columns
final_arg_all <- cbind(final_arg_hf,final_arg_gs,final_arg_gc,final_arg_tg,final_arg_ca,final_arg_cc,final_arg_tcorners,final_arg_cosc,final_arg_hf_against)
write.xlsx(final_arg_all,'Divisions/ARG.xlsx',sheetName = "L6", append = TRUE)
############################################################################
#D2
#form
#create final_d2_hf object
# final_d2_hf <- c()
# for(index_d2_hf in 1:length(d2_teams))
# {
#   index_d2_hf <- row.names(d2_form_h) == d2_teams[index_d2_hf]
#   form_d2_hf <- d2_form_h[index_d2_hf]
#   deleted_form_d2_hf <- form_d2_hf[!form_d2_hf[] == ""]
#   l6_form_d2_hf <- tail(deleted_form_d2_hf,d2_last_n_games)
#   l6_form_d2_hf <- paste(l6_form_d2_hf,collapse = " ")
#   final_d2_hf[index_d2_hf] <- rbind(paste(d2_teams[index_d2_hf],l6_form_d2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
#
# #change column names
# final_d2_hf <- as.data.frame(final_d2_hf)
# colnames(final_d2_hf) <- "Form"
# #goals scored
# #create final_d2_gs object
# final_d2_gs <- c()
# suml6_d2_gs <- c()
# for(index_d2_gs in 1:length(d2_teams))
# {
#   index_d2_gs <- row.names(d2_goalscored_h) == d2_teams[index_d2_gs]
#   form_d2_gs <- d2_goalscored_h[index_d2_gs]
#   deleted_form_d2_gs <- form_d2_gs[!form_d2_gs[] == ""]
#   l6_form_d2_gs <- tail(deleted_form_d2_gs,d2_last_n_games)
#   l6_form_d2_gs <- as.numeric(l6_form_d2_gs)
#   suml6_d2_gs[index_d2_gs] <- sum(l6_form_d2_gs)
#   suml6_d2_gs[index_d2_gs] <- paste("(",suml6_d2_gs[index_d2_gs],")",sep = "")
#   l6_form_d2_gs <- paste(l6_form_d2_gs,collapse = " ")
#   final_d2_gs[index_d2_gs] <- rbind(paste(d2_teams[index_d2_gs],l6_form_d2_gs,suml6_d2_gs[index_d2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# final_d2_gs
# #change column names
# final_d2_gs <- as.data.frame(final_d2_gs)
# colnames(final_d2_gs) <- "Goals scored"
# #goal conceded
# #create final_d2_gc object
# final_d2_gc <- c()
# suml6_d2_gc <- c()
# for(index_d2_gc in 1:length(d2_teams))
# {
#   index_d2_gc <- row.names(d2_goalconceded_h) == d2_teams[index_d2_gc]
#   form_d2_gc <- d2_goalconceded_h[index_d2_gc]
#   deleted_form_d2_gc <- form_d2_gc[!form_d2_gc[] == ""]
#   l6_form_d2_gc <- tail(deleted_form_d2_gc,d2_last_n_games)
#   l6_form_d2_gc <- as.numeric(l6_form_d2_gc)
#   suml6_d2_gc[index_d2_gc] <- sum(l6_form_d2_gc)
#   suml6_d2_gc[index_d2_gc] <- paste("(",suml6_d2_gc[index_d2_gc],")",sep = "")
#   l6_form_d2_gc <- paste(l6_form_d2_gc,collapse = " ")
#   final_d2_gc[index_d2_gc] <- rbind(paste(d2_teams[index_d2_gc],l6_form_d2_gc,suml6_d2_gc[index_d2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
#
# #change column names
# final_d2_gc <- as.data.frame(final_d2_gc)
# colnames(final_d2_gc) <- "Goals conceded"
# #total goals
# #create final_d2_tg object
# final_d2_tg <- c()
# suml6_d2_tg <- c()
# for(index_d2_tg in 1:length(d2_teams))
# {
#   index_d2_tg <- row.names(d2_totalgoals_h) == d2_teams[index_d2_tg]
#   form_d2_tg <- d2_totalgoals_h[index_d2_tg]
#   deleted_form_d2_tg <- form_d2_tg[!form_d2_tg[] == ""]
#   l6_form_d2_tg <- tail(deleted_form_d2_tg,d2_last_n_games)
#   l6_form_d2_tg <- as.numeric(l6_form_d2_tg)
#   suml6_d2_tg[index_d2_tg] <- sum(l6_form_d2_tg)
#   suml6_d2_tg[index_d2_tg] <- paste("(",suml6_d2_tg[index_d2_tg],")",sep = "")
#   l6_form_d2_tg <- paste(l6_form_d2_tg,collapse = " ")
#   final_d2_tg[index_d2_tg] <- rbind(paste(d2_teams[index_d2_tg],l6_form_d2_tg,suml6_d2_tg[index_d2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# #change column names
# final_d2_tg <- as.data.frame(final_d2_tg)
# colnames(final_d2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_d2_hf object
# final_d2_cs <- c()
# for(index_d2_cs in 1:length(d2_teams))
# {
#   index_d2_cs <- row.names(d2_csform_h) == d2_teams[index_d2_cs]
#   csform_d2_cs <- d2_csform_h[index_d2_cs]
#   deleted_csform_d2_cs <- csform_d2_cs[!csform_d2_cs[] == ""]
#   l6_csform_d2_cs <- tail(deleted_csform_d2_cs,d2_last_n_games)
#   l6_csform_d2_cs <- paste(l6_csform_d2_cs,collapse = " ")
#   final_d2_cs[index_d2_cs] <- rbind(paste(d2_teams[index_d2_cs],l6_csform_d2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",d2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_d2_cs <- as.data.frame(final_d2_cs)
# colnames(final_d2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_d2_wm object
# final_d2_wm <- c()
# suml6_d2_wm <- c()
# for(index_d2_wm in 1:length(d2_teams))
# {
#   index_d2_wm <- row.names(d2_winmargin_h) == d2_teams[index_d2_wm]
#   form_d2_wm <- d2_winmargin_h[index_d2_wm]
#   deleted_form_d2_wm <- form_d2_wm[!form_d2_wm[] == ""]
#   l6_form_d2_wm <- tail(deleted_form_d2_wm,d2_last_n_games)
#   l6_form_d2_wm <- as.numeric(l6_form_d2_wm)
#   suml6_d2_wm[index_d2_wm] <- sum(l6_form_d2_wm)
#   suml6_d2_wm[index_d2_wm] <- paste("(",suml6_d2_wm[index_d2_wm],")",sep = "")
#   l6_form_d2_wm <- paste(l6_form_d2_wm,collapse = " ")
#   final_d2_wm[index_d2_wm] <- rbind(paste(d2_teams[index_d2_wm],l6_form_d2_wm,suml6_d2_wm[index_d2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# final_d2_wm
# #change column names
# final_d2_wm <- as.data.frame(final_d2_wm)
# colnames(final_d2_wm) <- "Win Margin"
# #################################################
# #############################################################
# #corners awarded
# #create final_d2_ca object
# final_d2_ca <- c()
# suml6_d2_ca <- c()
# for(index_d2_ca in 1:length(d2_teams))
# {
#   index_d2_ca <- row.names(d2_coawarded_h) == d2_teams[index_d2_ca]
#   form_d2_ca <- d2_coawarded_h[index_d2_ca]
#   deleted_form_d2_ca <- form_d2_ca[!form_d2_ca[] == ""]
#   l6_form_d2_ca <- tail(deleted_form_d2_ca,d2_last_n_games)
#   l6_form_d2_ca <- as.numeric(l6_form_d2_ca)
#   suml6_d2_ca[index_d2_ca] <- sum(l6_form_d2_ca)
#   suml6_d2_ca[index_d2_ca] <- paste("(",suml6_d2_ca[index_d2_ca],")",sep = "")
#   l6_form_d2_ca <- paste(l6_form_d2_ca,collapse = " ")
#   final_d2_ca[index_d2_ca] <- rbind(paste(d2_teams[index_d2_ca],l6_form_d2_ca,suml6_d2_ca[index_d2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# final_d2_ca
# #change column names
# final_d2_ca <- as.data.frame(final_d2_ca)
# colnames(final_d2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_d2_ca object
# final_d2_cc <- c()
# suml6_d2_cc <- c()
# for(index_d2_cc in 1:length(d2_teams))
# {
#   index_d2_cc <- row.names(d2_cornersconceded_h) == d2_teams[index_d2_cc]
#   form_d2_cc <- d2_cornersconceded_h[index_d2_cc]
#   deleted_form_d2_cc <- form_d2_cc[!form_d2_cc[] == ""]
#   l6_form_d2_cc <- tail(deleted_form_d2_cc,d2_last_n_games)
#   l6_form_d2_cc <- as.numeric(l6_form_d2_cc)
#   suml6_d2_cc[index_d2_cc] <- sum(l6_form_d2_cc)
#   suml6_d2_cc[index_d2_cc] <- paste("(",suml6_d2_cc[index_d2_cc],")",sep = "")
#   l6_form_d2_cc <- paste(l6_form_d2_cc,collapse = " ")
#   final_d2_cc[index_d2_cc] <- rbind(paste(d2_teams[index_d2_cc],l6_form_d2_cc,suml6_d2_cc[index_d2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# final_d2_cc
# #change column names
# final_d2_cc <- as.data.frame(final_d2_cc)
# colnames(final_d2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_d2_cosc <- c()
# for(index_d2_cosc in 1:length(d2_teams))
# {
#   index_d2_cosc <- row.names(d2_coscform_h) == d2_teams[index_d2_cosc]
#   coscform_d2_cosc <- d2_coscform_h[index_d2_cosc]
#   deleted_coscform_d2_cosc <- coscform_d2_cosc[!coscform_d2_cosc[] == ""]
#   l6_coscform_d2_cosc <- tail(deleted_coscform_d2_cosc,d2_last_n_games)
#   l6_coscform_d2_cosc <- paste(l6_coscform_d2_cosc,collapse = " ")
#   final_d2_cosc[index_d2_cosc] <- rbind(paste(d2_teams[index_d2_cosc],l6_coscform_d2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",d2_teams[index],l6_coscform)
#
# }
# final_d2_cosc
# #change column names
# final_d2_cosc <- as.data.frame(final_d2_cosc)
# colnames(final_d2_cosc) <- "CornersForm"
# ##################################################
# #############################################################
# ##################################################
# #total corners
# #create final_d2_tcorners object
# final_d2_tcorners <- c()
# suml6_d2_tcorners <- c()
# for(index_d2_tcorners in 1:length(d2_teams))
# {
#   index_d2_tcorners <- row.names(d2_totalcorners_h) == d2_teams[index_d2_tcorners]
#   form_d2_tcorners <- d2_totalcorners_h[index_d2_tcorners]
#   deleted_form_d2_tcorners <- form_d2_tcorners[!form_d2_tcorners[] == ""]
#   l6_form_d2_tcorners <- tail(deleted_form_d2_tcorners,d2_last_n_games)
#   l6_form_d2_tcorners <- as.numeric(l6_form_d2_tcorners)
#   suml6_d2_tcorners[index_d2_tcorners] <- sum(l6_form_d2_tcorners)
#   suml6_d2_tcorners[index_d2_tcorners] <- paste("(",suml6_d2_tcorners[index_d2_tcorners],")",sep = "")
#   l6_form_d2_tcorners <- paste(l6_form_d2_tcorners,collapse = " ")
#   final_d2_tcorners[index_d2_tcorners] <- rbind(paste(d2_teams[index_d2_tcorners],l6_form_d2_tcorners,suml6_d2_tcorners[index_d2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# #change column names
# final_d2_tcorners <- as.data.frame(final_d2_tcorners)
# colnames(final_d2_tcorners) <- "TotalCorners"
# ###########################################################
# ############################################################
#
# #Team against
# #create final_d2_hf_against
# final_d2_hf_against <- c()
# for(index_d2_hf_against in 1:length(d2_teams))
# {
#   index_d2_hf_against <- row.names(d2_form_team_against_h) == d2_teams[index_d2_hf_against]
#   form_d2_hf_against <- d2_form_team_against_h[index_d2_hf_against]
#   deleted_form_d2_hf_against <- form_d2_hf_against[!form_d2_hf_against[] == ""]
#   l6_form_d2_hf_against <- tail(deleted_form_d2_hf_against,d2_last_n_games)
#   l6_form_d2_hf_against <- paste(l6_form_d2_hf_against,collapse = " ")
#   final_d2_hf_against[index_d2_hf_against] <- rbind(paste(d2_teams[index_d2_hf_against],l6_form_d2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",d2_teams[index],l6_form)
#
# }
# final_d2_hf_against <- as.data.frame(final_d2_hf_against)
# colnames(final_d2_hf_against) <- "Team against"
# #combine the columns
# final_d2_all <- cbind(final_d2_hf,final_d2_gs,final_d2_gc,final_d2_tg,final_d2_ca,final_d2_cc,final_d2_tcorners,final_d2_cosc,final_d2_hf_against)
# write.xlsx(final_d2_all,'Divisions/D2.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #E0
# #form
# #create final_e0_hf object
# final_e0_hf <- c()
# for(index_e0_hf in 1:length(e0_teams))
# {
#   index_e0_hf <- row.names(e0_form_h) == e0_teams[index_e0_hf]
#   form_e0_hf <- e0_form_h[index_e0_hf]
#   deleted_form_e0_hf <- form_e0_hf[!form_e0_hf[] == ""]
#   l6_form_e0_hf <- tail(deleted_form_e0_hf,e0_last_n_games)
#   l6_form_e0_hf <- paste(l6_form_e0_hf,collapse = " ")
#   final_e0_hf[index_e0_hf] <- rbind(paste(e0_teams[index_e0_hf],l6_form_e0_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
#
# #change column names
# final_e0_hf <- as.data.frame(final_e0_hf)
# colnames(final_e0_hf) <- "Form"
# #goals scored
# #create final_e0_gs object
# final_e0_gs <- c()
# suml6_e0_gs <- c()
# for(index_e0_gs in 1:length(e0_teams))
# {
#   index_e0_gs <- row.names(e0_goalscored_h) == e0_teams[index_e0_gs]
#   form_e0_gs <- e0_goalscored_h[index_e0_gs]
#   deleted_form_e0_gs <- form_e0_gs[!form_e0_gs[] == ""]
#   l6_form_e0_gs <- tail(deleted_form_e0_gs,e0_last_n_games)
#   l6_form_e0_gs <- as.numeric(l6_form_e0_gs)
#   suml6_e0_gs[index_e0_gs] <- sum(l6_form_e0_gs)
#   suml6_e0_gs[index_e0_gs] <- paste("(",suml6_e0_gs[index_e0_gs],")",sep = "")
#   l6_form_e0_gs <- paste(l6_form_e0_gs,collapse = " ")
#   final_e0_gs[index_e0_gs] <- rbind(paste(e0_teams[index_e0_gs],l6_form_e0_gs,suml6_e0_gs[index_e0_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# final_e0_gs
# #change column names
# final_e0_gs <- as.data.frame(final_e0_gs)
# colnames(final_e0_gs) <- "Goals scored"
# #goal conceded
# #create final_e0_gc object
# final_e0_gc <- c()
# suml6_e0_gc <- c()
# for(index_e0_gc in 1:length(e0_teams))
# {
#   index_e0_gc <- row.names(e0_goalconceded_h) == e0_teams[index_e0_gc]
#   form_e0_gc <- e0_goalconceded_h[index_e0_gc]
#   deleted_form_e0_gc <- form_e0_gc[!form_e0_gc[] == ""]
#   l6_form_e0_gc <- tail(deleted_form_e0_gc,e0_last_n_games)
#   l6_form_e0_gc <- as.numeric(l6_form_e0_gc)
#   suml6_e0_gc[index_e0_gc] <- sum(l6_form_e0_gc)
#   suml6_e0_gc[index_e0_gc] <- paste("(",suml6_e0_gc[index_e0_gc],")",sep = "")
#   l6_form_e0_gc <- paste(l6_form_e0_gc,collapse = " ")
#   final_e0_gc[index_e0_gc] <- rbind(paste(e0_teams[index_e0_gc],l6_form_e0_gc,suml6_e0_gc[index_e0_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
#
# #change column names
# final_e0_gc <- as.data.frame(final_e0_gc)
# colnames(final_e0_gc) <- "Goals conceded"
# #total goals
# #create final_e0_tg object
# final_e0_tg <- c()
# suml6_e0_tg <- c()
# for(index_e0_tg in 1:length(e0_teams))
# {
#   index_e0_tg <- row.names(e0_totalgoals_h) == e0_teams[index_e0_tg]
#   form_e0_tg <- e0_totalgoals_h[index_e0_tg]
#   deleted_form_e0_tg <- form_e0_tg[!form_e0_tg[] == ""]
#   l6_form_e0_tg <- tail(deleted_form_e0_tg,e0_last_n_games)
#   l6_form_e0_tg <- as.numeric(l6_form_e0_tg)
#   suml6_e0_tg[index_e0_tg] <- sum(l6_form_e0_tg)
#   suml6_e0_tg[index_e0_tg] <- paste("(",suml6_e0_tg[index_e0_tg],")",sep = "")
#   l6_form_e0_tg <- paste(l6_form_e0_tg,collapse = " ")
#   final_e0_tg[index_e0_tg] <- rbind(paste(e0_teams[index_e0_tg],l6_form_e0_tg,suml6_e0_tg[index_e0_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# #change column names
# final_e0_tg <- as.data.frame(final_e0_tg)
# colnames(final_e0_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_e0_hf object
# final_e0_cs <- c()
# for(index_e0_cs in 1:length(e0_teams))
# {
#   index_e0_cs <- row.names(e0_csform_h) == e0_teams[index_e0_cs]
#   csform_e0_cs <- e0_csform_h[index_e0_cs]
#   deleted_csform_e0_cs <- csform_e0_cs[!csform_e0_cs[] == ""]
#   l6_csform_e0_cs <- tail(deleted_csform_e0_cs,e0_last_n_games)
#   l6_csform_e0_cs <- paste(l6_csform_e0_cs,collapse = " ")
#   final_e0_cs[index_e0_cs] <- rbind(paste(e0_teams[index_e0_cs],l6_csform_e0_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",e0_teams[index],l6_csform)
#
# }
#
# #change column names
# final_e0_cs <- as.data.frame(final_e0_cs)
# colnames(final_e0_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_e0_wm object
# final_e0_wm <- c()
# suml6_e0_wm <- c()
# for(index_e0_wm in 1:length(e0_teams))
# {
#   index_e0_wm <- row.names(e0_winmargin_h) == e0_teams[index_e0_wm]
#   form_e0_wm <- e0_winmargin_h[index_e0_wm]
#   deleted_form_e0_wm <- form_e0_wm[!form_e0_wm[] == ""]
#   l6_form_e0_wm <- tail(deleted_form_e0_wm,e0_last_n_games)
#   l6_form_e0_wm <- as.numeric(l6_form_e0_wm)
#   suml6_e0_wm[index_e0_wm] <- sum(l6_form_e0_wm)
#   suml6_e0_wm[index_e0_wm] <- paste("(",suml6_e0_wm[index_e0_wm],")",sep = "")
#   l6_form_e0_wm <- paste(l6_form_e0_wm,collapse = " ")
#   final_e0_wm[index_e0_wm] <- rbind(paste(e0_teams[index_e0_wm],l6_form_e0_wm,suml6_e0_wm[index_e0_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# final_e0_wm
# #change column names
# final_e0_wm <- as.data.frame(final_e0_wm)
# colnames(final_e0_wm) <- "Win Margin"
# #################################################
#
# #################################################
# #corners awarded
# #create final_e0_ca object
# final_e0_ca <- c()
# suml6_e0_ca <- c()
# for(index_e0_ca in 1:length(e0_teams))
# {
#   index_e0_ca <- row.names(e0_coawarded_h) == e0_teams[index_e0_ca]
#   form_e0_ca <- e0_coawarded_h[index_e0_ca]
#   deleted_form_e0_ca <- form_e0_ca[!form_e0_ca[] == ""]
#   l6_form_e0_ca <- tail(deleted_form_e0_ca,e0_last_n_games)
#   l6_form_e0_ca <- as.numeric(l6_form_e0_ca)
#   suml6_e0_ca[index_e0_ca] <- sum(l6_form_e0_ca)
#   suml6_e0_ca[index_e0_ca] <- paste("(",suml6_e0_ca[index_e0_ca],")",sep = "")
#   l6_form_e0_ca <- paste(l6_form_e0_ca,collapse = " ")
#   final_e0_ca[index_e0_ca] <- rbind(paste(e0_teams[index_e0_ca],l6_form_e0_ca,suml6_e0_ca[index_e0_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# final_e0_ca
# #change column names
# final_e0_ca <- as.data.frame(final_e0_ca)
# colnames(final_e0_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_e0_ca object
# final_e0_cc <- c()
# suml6_e0_cc <- c()
# for(index_e0_cc in 1:length(e0_teams))
# {
#   index_e0_cc <- row.names(e0_cornersconceded_h) == e0_teams[index_e0_cc]
#   form_e0_cc <- e0_cornersconceded_h[index_e0_cc]
#   deleted_form_e0_cc <- form_e0_cc[!form_e0_cc[] == ""]
#   l6_form_e0_cc <- tail(deleted_form_e0_cc,e0_last_n_games)
#   l6_form_e0_cc <- as.numeric(l6_form_e0_cc)
#   suml6_e0_cc[index_e0_cc] <- sum(l6_form_e0_cc)
#   suml6_e0_cc[index_e0_cc] <- paste("(",suml6_e0_cc[index_e0_cc],")",sep = "")
#   l6_form_e0_cc <- paste(l6_form_e0_cc,collapse = " ")
#   final_e0_cc[index_e0_cc] <- rbind(paste(e0_teams[index_e0_cc],l6_form_e0_cc,suml6_e0_cc[index_e0_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# final_e0_cc
# #change column names
# final_e0_cc <- as.data.frame(final_e0_cc)
# colnames(final_e0_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_e0_cosc <- c()
# for(index_e0_cosc in 1:length(e0_teams))
# {
#   index_e0_cosc <- row.names(e0_coscform_h) == e0_teams[index_e0_cosc]
#   coscform_e0_cosc <- e0_coscform_h[index_e0_cosc]
#   deleted_coscform_e0_cosc <- coscform_e0_cosc[!coscform_e0_cosc[] == ""]
#   l6_coscform_e0_cosc <- tail(deleted_coscform_e0_cosc,e0_last_n_games)
#   l6_coscform_e0_cosc <- paste(l6_coscform_e0_cosc,collapse = " ")
#   final_e0_cosc[index_e0_cosc] <- rbind(paste(e0_teams[index_e0_cosc],l6_coscform_e0_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",e0_teams[index],l6_coscform)
#
# }
# final_e0_cosc
# #change column names
# final_e0_cosc <- as.data.frame(final_e0_cosc)
# colnames(final_e0_cosc) <- "CornersForm"
# ##################################################
#
# ##################################################
# #total corners
# #create final_e0_tcorners object
# final_e0_tcorners <- c()
# suml6_e0_tcorners <- c()
# for(index_e0_tcorners in 1:length(e0_teams))
# {
#   index_e0_tcorners <- row.names(e0_totalcorners_h) == e0_teams[index_e0_tcorners]
#   form_e0_tcorners <- e0_totalcorners_h[index_e0_tcorners]
#   deleted_form_e0_tcorners <- form_e0_tcorners[!form_e0_tcorners[] == ""]
#   l6_form_e0_tcorners <- tail(deleted_form_e0_tcorners,e0_last_n_games)
#   l6_form_e0_tcorners <- as.numeric(l6_form_e0_tcorners)
#   suml6_e0_tcorners[index_e0_tcorners] <- sum(l6_form_e0_tcorners)
#   suml6_e0_tcorners[index_e0_tcorners] <- paste("(",suml6_e0_tcorners[index_e0_tcorners],")",sep = "")
#   l6_form_e0_tcorners <- paste(l6_form_e0_tcorners,collapse = " ")
#   final_e0_tcorners[index_e0_tcorners] <- rbind(paste(e0_teams[index_e0_tcorners],l6_form_e0_tcorners,suml6_e0_tcorners[index_e0_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# #change column names
# final_e0_tcorners <- as.data.frame(final_e0_tcorners)
# colnames(final_e0_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_e0_hf_against
# final_e0_hf_against <- c()
# for(index_e0_hf_against in 1:length(e0_teams))
# {
#   index_e0_hf_against <- row.names(e0_form_team_against_h) == e0_teams[index_e0_hf_against]
#   form_e0_hf_against <- e0_form_team_against_h[index_e0_hf_against]
#   deleted_form_e0_hf_against <- form_e0_hf_against[!form_e0_hf_against[] == ""]
#   l6_form_e0_hf_against <- tail(deleted_form_e0_hf_against,e0_last_n_games)
#   l6_form_e0_hf_against <- paste(l6_form_e0_hf_against,collapse = " ")
#   final_e0_hf_against[index_e0_hf_against] <- rbind(paste(e0_teams[index_e0_hf_against],l6_form_e0_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e0_teams[index],l6_form)
#
# }
# final_e0_hf_against <- as.data.frame(final_e0_hf_against)
# colnames(final_e0_hf_against) <- "Team against"
# #combine the columns
# final_e0_all <- cbind(final_e0_hf,final_e0_gs,final_e0_gc,final_e0_tg,final_e0_ca,final_e0_cc,final_e0_tcorners,final_e0_cosc,final_e0_hf_against)
# write.xlsx(final_e0_all,'Divisions/E0.xlsx',sheetName = "L6", append = TRUE)
# ##################################################################################
# #E1
# #form
# #create final_e1_hf object
# final_e1_hf <- c()
# for(index_e1_hf in 1:length(e1_teams))
# {
#   index_e1_hf <- row.names(e1_form_h) == e1_teams[index_e1_hf]
#   form_e1_hf <- e1_form_h[index_e1_hf]
#   deleted_form_e1_hf <- form_e1_hf[!form_e1_hf[] == ""]
#   l6_form_e1_hf <- tail(deleted_form_e1_hf,e1_last_n_games)
#   l6_form_e1_hf <- paste(l6_form_e1_hf,collapse = " ")
#   final_e1_hf[index_e1_hf] <- rbind(paste(e1_teams[index_e1_hf],l6_form_e1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
#
# #change column names
# final_e1_hf <- as.data.frame(final_e1_hf)
# colnames(final_e1_hf) <- "Form"
# #goals scored
# #create final_e1_gs object
# final_e1_gs <- c()
# suml6_e1_gs <- c()
# for(index_e1_gs in 1:length(e1_teams))
# {
#   index_e1_gs <- row.names(e1_goalscored_h) == e1_teams[index_e1_gs]
#   form_e1_gs <- e1_goalscored_h[index_e1_gs]
#   deleted_form_e1_gs <- form_e1_gs[!form_e1_gs[] == ""]
#   l6_form_e1_gs <- tail(deleted_form_e1_gs,e1_last_n_games)
#   l6_form_e1_gs <- as.numeric(l6_form_e1_gs)
#   suml6_e1_gs[index_e1_gs] <- sum(l6_form_e1_gs)
#   suml6_e1_gs[index_e1_gs] <- paste("(",suml6_e1_gs[index_e1_gs],")",sep = "")
#   l6_form_e1_gs <- paste(l6_form_e1_gs,collapse = " ")
#   final_e1_gs[index_e1_gs] <- rbind(paste(e1_teams[index_e1_gs],l6_form_e1_gs,suml6_e1_gs[index_e1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# final_e1_gs
# #change column names
# final_e1_gs <- as.data.frame(final_e1_gs)
# colnames(final_e1_gs) <- "Goals scored"
# #goal conceded
# #create final_e1_gc object
# final_e1_gc <- c()
# suml6_e1_gc <- c()
# for(index_e1_gc in 1:length(e1_teams))
# {
#   index_e1_gc <- row.names(e1_goalconceded_h) == e1_teams[index_e1_gc]
#   form_e1_gc <- e1_goalconceded_h[index_e1_gc]
#   deleted_form_e1_gc <- form_e1_gc[!form_e1_gc[] == ""]
#   l6_form_e1_gc <- tail(deleted_form_e1_gc,e1_last_n_games)
#   l6_form_e1_gc <- as.numeric(l6_form_e1_gc)
#   suml6_e1_gc[index_e1_gc] <- sum(l6_form_e1_gc)
#   suml6_e1_gc[index_e1_gc] <- paste("(",suml6_e1_gc[index_e1_gc],")",sep = "")
#   l6_form_e1_gc <- paste(l6_form_e1_gc,collapse = " ")
#   final_e1_gc[index_e1_gc] <- rbind(paste(e1_teams[index_e1_gc],l6_form_e1_gc,suml6_e1_gc[index_e1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
#
# #change column names
# final_e1_gc <- as.data.frame(final_e1_gc)
# colnames(final_e1_gc) <- "Goals conceded"
# #total goals
# #create final_e1_tg object
# final_e1_tg <- c()
# suml6_e1_tg <- c()
# for(index_e1_tg in 1:length(e1_teams))
# {
#   index_e1_tg <- row.names(e1_totalgoals_h) == e1_teams[index_e1_tg]
#   form_e1_tg <- e1_totalgoals_h[index_e1_tg]
#   deleted_form_e1_tg <- form_e1_tg[!form_e1_tg[] == ""]
#   l6_form_e1_tg <- tail(deleted_form_e1_tg,e1_last_n_games)
#   l6_form_e1_tg <- as.numeric(l6_form_e1_tg)
#   suml6_e1_tg[index_e1_tg] <- sum(l6_form_e1_tg)
#   suml6_e1_tg[index_e1_tg] <- paste("(",suml6_e1_tg[index_e1_tg],")",sep = "")
#   l6_form_e1_tg <- paste(l6_form_e1_tg,collapse = " ")
#   final_e1_tg[index_e1_tg] <- rbind(paste(e1_teams[index_e1_tg],l6_form_e1_tg,suml6_e1_tg[index_e1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# #change column names
# final_e1_tg <- as.data.frame(final_e1_tg)
# colnames(final_e1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_e1_hf object
# final_e1_cs <- c()
# for(index_e1_cs in 1:length(e1_teams))
# {
#   index_e1_cs <- row.names(e1_csform_h) == e1_teams[index_e1_cs]
#   csform_e1_cs <- e1_csform_h[index_e1_cs]
#   deleted_csform_e1_cs <- csform_e1_cs[!csform_e1_cs[] == ""]
#   l6_csform_e1_cs <- tail(deleted_csform_e1_cs,e1_last_n_games)
#   l6_csform_e1_cs <- paste(l6_csform_e1_cs,collapse = " ")
#   final_e1_cs[index_e1_cs] <- rbind(paste(e1_teams[index_e1_cs],l6_csform_e1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",e1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_e1_cs <- as.data.frame(final_e1_cs)
# colnames(final_e1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_e1_wm object
# final_e1_wm <- c()
# suml6_e1_wm <- c()
# for(index_e1_wm in 1:length(e1_teams))
# {
#   index_e1_wm <- row.names(e1_winmargin_h) == e1_teams[index_e1_wm]
#   form_e1_wm <- e1_winmargin_h[index_e1_wm]
#   deleted_form_e1_wm <- form_e1_wm[!form_e1_wm[] == ""]
#   l6_form_e1_wm <- tail(deleted_form_e1_wm,e1_last_n_games)
#   l6_form_e1_wm <- as.numeric(l6_form_e1_wm)
#   suml6_e1_wm[index_e1_wm] <- sum(l6_form_e1_wm)
#   suml6_e1_wm[index_e1_wm] <- paste("(",suml6_e1_wm[index_e1_wm],")",sep = "")
#   l6_form_e1_wm <- paste(l6_form_e1_wm,collapse = " ")
#   final_e1_wm[index_e1_wm] <- rbind(paste(e1_teams[index_e1_wm],l6_form_e1_wm,suml6_e1_wm[index_e1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# final_e1_wm
# #change column names
# final_e1_wm <- as.data.frame(final_e1_wm)
# colnames(final_e1_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_e1_ca object
# final_e1_ca <- c()
# suml6_e1_ca <- c()
# for(index_e1_ca in 1:length(e1_teams))
# {
#   index_e1_ca <- row.names(e1_coawarded_h) == e1_teams[index_e1_ca]
#   form_e1_ca <- e1_coawarded_h[index_e1_ca]
#   deleted_form_e1_ca <- form_e1_ca[!form_e1_ca[] == ""]
#   l6_form_e1_ca <- tail(deleted_form_e1_ca,e1_last_n_games)
#   l6_form_e1_ca <- as.numeric(l6_form_e1_ca)
#   suml6_e1_ca[index_e1_ca] <- sum(l6_form_e1_ca)
#   suml6_e1_ca[index_e1_ca] <- paste("(",suml6_e1_ca[index_e1_ca],")",sep = "")
#   l6_form_e1_ca <- paste(l6_form_e1_ca,collapse = " ")
#   final_e1_ca[index_e1_ca] <- rbind(paste(e1_teams[index_e1_ca],l6_form_e1_ca,suml6_e1_ca[index_e1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# final_e1_ca
# #change column names
# final_e1_ca <- as.data.frame(final_e1_ca)
# colnames(final_e1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_e1_ca object
# final_e1_cc <- c()
# suml6_e1_cc <- c()
# for(index_e1_cc in 1:length(e1_teams))
# {
#   index_e1_cc <- row.names(e1_cornersconceded_h) == e1_teams[index_e1_cc]
#   form_e1_cc <- e1_cornersconceded_h[index_e1_cc]
#   deleted_form_e1_cc <- form_e1_cc[!form_e1_cc[] == ""]
#   l6_form_e1_cc <- tail(deleted_form_e1_cc,e1_last_n_games)
#   l6_form_e1_cc <- as.numeric(l6_form_e1_cc)
#   suml6_e1_cc[index_e1_cc] <- sum(l6_form_e1_cc)
#   suml6_e1_cc[index_e1_cc] <- paste("(",suml6_e1_cc[index_e1_cc],")",sep = "")
#   l6_form_e1_cc <- paste(l6_form_e1_cc,collapse = " ")
#   final_e1_cc[index_e1_cc] <- rbind(paste(e1_teams[index_e1_cc],l6_form_e1_cc,suml6_e1_cc[index_e1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# final_e1_cc
# #change column names
# final_e1_cc <- as.data.frame(final_e1_cc)
# colnames(final_e1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_e1_cosc <- c()
# for(index_e1_cosc in 1:length(e1_teams))
# {
#   index_e1_cosc <- row.names(e1_coscform_h) == e1_teams[index_e1_cosc]
#   coscform_e1_cosc <- e1_coscform_h[index_e1_cosc]
#   deleted_coscform_e1_cosc <- coscform_e1_cosc[!coscform_e1_cosc[] == ""]
#   l6_coscform_e1_cosc <- tail(deleted_coscform_e1_cosc,e1_last_n_games)
#   l6_coscform_e1_cosc <- paste(l6_coscform_e1_cosc,collapse = " ")
#   final_e1_cosc[index_e1_cosc] <- rbind(paste(e1_teams[index_e1_cosc],l6_coscform_e1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",e1_teams[index],l6_coscform)
#
# }
# final_e1_cosc
# #change column names
# final_e1_cosc <- as.data.frame(final_e1_cosc)
# colnames(final_e1_cosc) <- "CornersForm"
# ##################################################
#
# ##################################################
# #total corners
# #create final_e1_tcorners object
# final_e1_tcorners <- c()
# suml6_e1_tcorners <- c()
# for(index_e1_tcorners in 1:length(e1_teams))
# {
#   index_e1_tcorners <- row.names(e1_totalcorners_h) == e1_teams[index_e1_tcorners]
#   form_e1_tcorners <- e1_totalcorners_h[index_e1_tcorners]
#   deleted_form_e1_tcorners <- form_e1_tcorners[!form_e1_tcorners[] == ""]
#   l6_form_e1_tcorners <- tail(deleted_form_e1_tcorners,e1_last_n_games)
#   l6_form_e1_tcorners <- as.numeric(l6_form_e1_tcorners)
#   suml6_e1_tcorners[index_e1_tcorners] <- sum(l6_form_e1_tcorners)
#   suml6_e1_tcorners[index_e1_tcorners] <- paste("(",suml6_e1_tcorners[index_e1_tcorners],")",sep = "")
#   l6_form_e1_tcorners <- paste(l6_form_e1_tcorners,collapse = " ")
#   final_e1_tcorners[index_e1_tcorners] <- rbind(paste(e1_teams[index_e1_tcorners],l6_form_e1_tcorners,suml6_e1_tcorners[index_e1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# #change column names
# final_e1_tcorners <- as.data.frame(final_e1_tcorners)
# colnames(final_e1_tcorners) <- "TotalCorners"
# ###################################################
# ###################################################
# #Team against
# #create final_e1_hf_against
# final_e1_hf_against <- c()
# for(index_e1_hf_against in 1:length(e1_teams))
# {
#   index_e1_hf_against <- row.names(e1_form_team_against_h) == e1_teams[index_e1_hf_against]
#   form_e1_hf_against <- e1_form_team_against_h[index_e1_hf_against]
#   deleted_form_e1_hf_against <- form_e1_hf_against[!form_e1_hf_against[] == ""]
#   l6_form_e1_hf_against <- tail(deleted_form_e1_hf_against,e1_last_n_games)
#   l6_form_e1_hf_against <- paste(l6_form_e1_hf_against,collapse = " ")
#   final_e1_hf_against[index_e1_hf_against] <- rbind(paste(e1_teams[index_e1_hf_against],l6_form_e1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e1_teams[index],l6_form)
#
# }
# final_e1_hf_against <- as.data.frame(final_e1_hf_against)
# colnames(final_e1_hf_against) <- "Team against"
# #combine the columns
# final_e1_all <- cbind(final_e1_hf,final_e1_gs,final_e1_gc,final_e1_tg,final_e1_ca,final_e1_cc,final_e1_tcorners,final_e1_cosc,final_e1_hf_against)
# write.xlsx(final_e1_all,'Divisions/E1.xlsx',sheetName = "L6", append = TRUE)
# ###############################################################################
# #E2
# #form
# #create final_e2_hf object
# final_e2_hf <- c()
# for(index_e2_hf in 1:length(e2_teams))
# {
#   index_e2_hf <- row.names(e2_form_h) == e2_teams[index_e2_hf]
#   form_e2_hf <- e2_form_h[index_e2_hf]
#   deleted_form_e2_hf <- form_e2_hf[!form_e2_hf[] == ""]
#   l6_form_e2_hf <- tail(deleted_form_e2_hf,e2_last_n_games)
#   l6_form_e2_hf <- paste(l6_form_e2_hf,collapse = " ")
#   final_e2_hf[index_e2_hf] <- rbind(paste(e2_teams[index_e2_hf],l6_form_e2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
#
# #change column names
# final_e2_hf <- as.data.frame(final_e2_hf)
# colnames(final_e2_hf) <- "Form"
# #goals scored
# #create final_e2_gs object
# final_e2_gs <- c()
# suml6_e2_gs <- c()
# for(index_e2_gs in 1:length(e2_teams))
# {
#   index_e2_gs <- row.names(e2_goalscored_h) == e2_teams[index_e2_gs]
#   form_e2_gs <- e2_goalscored_h[index_e2_gs]
#   deleted_form_e2_gs <- form_e2_gs[!form_e2_gs[] == ""]
#   l6_form_e2_gs <- tail(deleted_form_e2_gs,e2_last_n_games)
#   l6_form_e2_gs <- as.numeric(l6_form_e2_gs)
#   suml6_e2_gs[index_e2_gs] <- sum(l6_form_e2_gs)
#   suml6_e2_gs[index_e2_gs] <- paste("(",suml6_e2_gs[index_e2_gs],")",sep = "")
#   l6_form_e2_gs <- paste(l6_form_e2_gs,collapse = " ")
#   final_e2_gs[index_e2_gs] <- rbind(paste(e2_teams[index_e2_gs],l6_form_e2_gs,suml6_e2_gs[index_e2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# final_e2_gs
# #change column names
# final_e2_gs <- as.data.frame(final_e2_gs)
# colnames(final_e2_gs) <- "Goals scored"
# #goal conceded
# #create final_e2_gc object
# final_e2_gc <- c()
# suml6_e2_gc <- c()
# for(index_e2_gc in 1:length(e2_teams))
# {
#   index_e2_gc <- row.names(e2_goalconceded_h) == e2_teams[index_e2_gc]
#   form_e2_gc <- e2_goalconceded_h[index_e2_gc]
#   deleted_form_e2_gc <- form_e2_gc[!form_e2_gc[] == ""]
#   l6_form_e2_gc <- tail(deleted_form_e2_gc,e2_last_n_games)
#   l6_form_e2_gc <- as.numeric(l6_form_e2_gc)
#   suml6_e2_gc[index_e2_gc] <- sum(l6_form_e2_gc)
#   suml6_e2_gc[index_e2_gc] <- paste("(",suml6_e2_gc[index_e2_gc],")",sep = "")
#   l6_form_e2_gc <- paste(l6_form_e2_gc,collapse = " ")
#   final_e2_gc[index_e2_gc] <- rbind(paste(e2_teams[index_e2_gc],l6_form_e2_gc,suml6_e2_gc[index_e2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
#
# #change column names
# final_e2_gc <- as.data.frame(final_e2_gc)
# colnames(final_e2_gc) <- "Goals conceded"
# #total goals
# #create final_e2_tg object
# final_e2_tg <- c()
# suml6_e2_tg <- c()
# for(index_e2_tg in 1:length(e2_teams))
# {
#   index_e2_tg <- row.names(e2_totalgoals_h) == e2_teams[index_e2_tg]
#   form_e2_tg <- e2_totalgoals_h[index_e2_tg]
#   deleted_form_e2_tg <- form_e2_tg[!form_e2_tg[] == ""]
#   l6_form_e2_tg <- tail(deleted_form_e2_tg,e2_last_n_games)
#   l6_form_e2_tg <- as.numeric(l6_form_e2_tg)
#   suml6_e2_tg[index_e2_tg] <- sum(l6_form_e2_tg)
#   suml6_e2_tg[index_e2_tg] <- paste("(",suml6_e2_tg[index_e2_tg],")",sep = "")
#   l6_form_e2_tg <- paste(l6_form_e2_tg,collapse = " ")
#   final_e2_tg[index_e2_tg] <- rbind(paste(e2_teams[index_e2_tg],l6_form_e2_tg,suml6_e2_tg[index_e2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# #change column names
# final_e2_tg <- as.data.frame(final_e2_tg)
# colnames(final_e2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_e2_hf object
# final_e2_cs <- c()
# for(index_e2_cs in 1:length(e2_teams))
# {
#   index_e2_cs <- row.names(e2_csform_h) == e2_teams[index_e2_cs]
#   csform_e2_cs <- e2_csform_h[index_e2_cs]
#   deleted_csform_e2_cs <- csform_e2_cs[!csform_e2_cs[] == ""]
#   l6_csform_e2_cs <- tail(deleted_csform_e2_cs,e2_last_n_games)
#   l6_csform_e2_cs <- paste(l6_csform_e2_cs,collapse = " ")
#   final_e2_cs[index_e2_cs] <- rbind(paste(e2_teams[index_e2_cs],l6_csform_e2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",e2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_e2_cs <- as.data.frame(final_e2_cs)
# colnames(final_e2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_e2_wm object
# final_e2_wm <- c()
# suml6_e2_wm <- c()
# for(index_e2_wm in 1:length(e2_teams))
# {
#   index_e2_wm <- row.names(e2_winmargin_h) == e2_teams[index_e2_wm]
#   form_e2_wm <- e2_winmargin_h[index_e2_wm]
#   deleted_form_e2_wm <- form_e2_wm[!form_e2_wm[] == ""]
#   l6_form_e2_wm <- tail(deleted_form_e2_wm,e2_last_n_games)
#   l6_form_e2_wm <- as.numeric(l6_form_e2_wm)
#   suml6_e2_wm[index_e2_wm] <- sum(l6_form_e2_wm)
#   suml6_e2_wm[index_e2_wm] <- paste("(",suml6_e2_wm[index_e2_wm],")",sep = "")
#   l6_form_e2_wm <- paste(l6_form_e2_wm,collapse = " ")
#   final_e2_wm[index_e2_wm] <- rbind(paste(e2_teams[index_e2_wm],l6_form_e2_wm,suml6_e2_wm[index_e2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# final_e2_wm
# #change column names
# final_e2_wm <- as.data.frame(final_e2_wm)
# colnames(final_e2_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_e2_ca object
# final_e2_ca <- c()
# suml6_e2_ca <- c()
# for(index_e2_ca in 1:length(e2_teams))
# {
#   index_e2_ca <- row.names(e2_coawarded_h) == e2_teams[index_e2_ca]
#   form_e2_ca <- e2_coawarded_h[index_e2_ca]
#   deleted_form_e2_ca <- form_e2_ca[!form_e2_ca[] == ""]
#   l6_form_e2_ca <- tail(deleted_form_e2_ca,e2_last_n_games)
#   l6_form_e2_ca <- as.numeric(l6_form_e2_ca)
#   suml6_e2_ca[index_e2_ca] <- sum(l6_form_e2_ca)
#   suml6_e2_ca[index_e2_ca] <- paste("(",suml6_e2_ca[index_e2_ca],")",sep = "")
#   l6_form_e2_ca <- paste(l6_form_e2_ca,collapse = " ")
#   final_e2_ca[index_e2_ca] <- rbind(paste(e2_teams[index_e2_ca],l6_form_e2_ca,suml6_e2_ca[index_e2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# final_e2_ca
# #change column names
# final_e2_ca <- as.data.frame(final_e2_ca)
# colnames(final_e2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_e2_ca object
# final_e2_cc <- c()
# suml6_e2_cc <- c()
# for(index_e2_cc in 1:length(e2_teams))
# {
#   index_e2_cc <- row.names(e2_cornersconceded_h) == e2_teams[index_e2_cc]
#   form_e2_cc <- e2_cornersconceded_h[index_e2_cc]
#   deleted_form_e2_cc <- form_e2_cc[!form_e2_cc[] == ""]
#   l6_form_e2_cc <- tail(deleted_form_e2_cc,e2_last_n_games)
#   l6_form_e2_cc <- as.numeric(l6_form_e2_cc)
#   suml6_e2_cc[index_e2_cc] <- sum(l6_form_e2_cc)
#   suml6_e2_cc[index_e2_cc] <- paste("(",suml6_e2_cc[index_e2_cc],")",sep = "")
#   l6_form_e2_cc <- paste(l6_form_e2_cc,collapse = " ")
#   final_e2_cc[index_e2_cc] <- rbind(paste(e2_teams[index_e2_cc],l6_form_e2_cc,suml6_e2_cc[index_e2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# final_e2_cc
# #change column names
# final_e2_cc <- as.data.frame(final_e2_cc)
# colnames(final_e2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_e2_cosc <- c()
# for(index_e2_cosc in 1:length(e2_teams))
# {
#   index_e2_cosc <- row.names(e2_coscform_h) == e2_teams[index_e2_cosc]
#   coscform_e2_cosc <- e2_coscform_h[index_e2_cosc]
#   deleted_coscform_e2_cosc <- coscform_e2_cosc[!coscform_e2_cosc[] == ""]
#   l6_coscform_e2_cosc <- tail(deleted_coscform_e2_cosc,e2_last_n_games)
#   l6_coscform_e2_cosc <- paste(l6_coscform_e2_cosc,collapse = " ")
#   final_e2_cosc[index_e2_cosc] <- rbind(paste(e2_teams[index_e2_cosc],l6_coscform_e2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",e2_teams[index],l6_coscform)
#
# }
# final_e2_cosc
# #change column names
# final_e2_cosc <- as.data.frame(final_e2_cosc)
# colnames(final_e2_cosc) <- "CornersForm"
# ##################################################
#
# ##################################################
# #total corners
# #create final_e2_tcorners object
# final_e2_tcorners <- c()
# suml6_e2_tcorners <- c()
# for(index_e2_tcorners in 1:length(e2_teams))
# {
#   index_e2_tcorners <- row.names(e2_totalcorners_h) == e2_teams[index_e2_tcorners]
#   form_e2_tcorners <- e2_totalcorners_h[index_e2_tcorners]
#   deleted_form_e2_tcorners <- form_e2_tcorners[!form_e2_tcorners[] == ""]
#   l6_form_e2_tcorners <- tail(deleted_form_e2_tcorners,e2_last_n_games)
#   l6_form_e2_tcorners <- as.numeric(l6_form_e2_tcorners)
#   suml6_e2_tcorners[index_e2_tcorners] <- sum(l6_form_e2_tcorners)
#   suml6_e2_tcorners[index_e2_tcorners] <- paste("(",suml6_e2_tcorners[index_e2_tcorners],")",sep = "")
#   l6_form_e2_tcorners <- paste(l6_form_e2_tcorners,collapse = " ")
#   final_e2_tcorners[index_e2_tcorners] <- rbind(paste(e2_teams[index_e2_tcorners],l6_form_e2_tcorners,suml6_e2_tcorners[index_e2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# #change column names
# final_e2_tcorners <- as.data.frame(final_e2_tcorners)
# colnames(final_e2_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_e2_hf_against
# final_e2_hf_against <- c()
# for(index_e2_hf_against in 1:length(e2_teams))
# {
#   index_e2_hf_against <- row.names(e2_form_team_against_h) == e2_teams[index_e2_hf_against]
#   form_e2_hf_against <- e2_form_team_against_h[index_e2_hf_against]
#   deleted_form_e2_hf_against <- form_e2_hf_against[!form_e2_hf_against[] == ""]
#   l6_form_e2_hf_against <- tail(deleted_form_e2_hf_against,e2_last_n_games)
#   l6_form_e2_hf_against <- paste(l6_form_e2_hf_against,collapse = " ")
#   final_e2_hf_against[index_e2_hf_against] <- rbind(paste(e2_teams[index_e2_hf_against],l6_form_e2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e2_teams[index],l6_form)
#
# }
# final_e2_hf_against <- as.data.frame(final_e2_hf_against)
# colnames(final_e2_hf_against) <- "Team against"
# #combine the columns
# final_e2_all <- cbind(final_e2_hf,final_e2_gs,final_e2_gc,final_e2_tg,final_e2_ca,final_e2_cc,final_e2_tcorners,final_e2_cosc,final_e2_hf_against)
# write.xlsx(final_e2_all,'Divisions/E2.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #E3
# #form
# #create final_e3_hf object
# final_e3_hf <- c()
# for(index_e3_hf in 1:length(e3_teams))
# {
#   index_e3_hf <- row.names(e3_form_h) == e3_teams[index_e3_hf]
#   form_e3_hf <- e3_form_h[index_e3_hf]
#   deleted_form_e3_hf <- form_e3_hf[!form_e3_hf[] == ""]
#   l6_form_e3_hf <- tail(deleted_form_e3_hf,e3_last_n_games)
#   l6_form_e3_hf <- paste(l6_form_e3_hf,collapse = " ")
#   final_e3_hf[index_e3_hf] <- rbind(paste(e3_teams[index_e3_hf],l6_form_e3_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
#
# #change column names
# final_e3_hf <- as.data.frame(final_e3_hf)
# colnames(final_e3_hf) <- "Form"
# #goals scored
# #create final_e3_gs object
# final_e3_gs <- c()
# suml6_e3_gs <- c()
# for(index_e3_gs in 1:length(e3_teams))
# {
#   index_e3_gs <- row.names(e3_goalscored_h) == e3_teams[index_e3_gs]
#   form_e3_gs <- e3_goalscored_h[index_e3_gs]
#   deleted_form_e3_gs <- form_e3_gs[!form_e3_gs[] == ""]
#   l6_form_e3_gs <- tail(deleted_form_e3_gs,e3_last_n_games)
#   l6_form_e3_gs <- as.numeric(l6_form_e3_gs)
#   suml6_e3_gs[index_e3_gs] <- sum(l6_form_e3_gs)
#   suml6_e3_gs[index_e3_gs] <- paste("(",suml6_e3_gs[index_e3_gs],")",sep = "")
#   l6_form_e3_gs <- paste(l6_form_e3_gs,collapse = " ")
#   final_e3_gs[index_e3_gs] <- rbind(paste(e3_teams[index_e3_gs],l6_form_e3_gs,suml6_e3_gs[index_e3_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# final_e3_gs
# #change column names
# final_e3_gs <- as.data.frame(final_e3_gs)
# colnames(final_e3_gs) <- "Goals scored"
# #goal conceded
# #create final_e3_gc object
# final_e3_gc <- c()
# suml6_e3_gc <- c()
# for(index_e3_gc in 1:length(e3_teams))
# {
#   index_e3_gc <- row.names(e3_goalconceded_h) == e3_teams[index_e3_gc]
#   form_e3_gc <- e3_goalconceded_h[index_e3_gc]
#   deleted_form_e3_gc <- form_e3_gc[!form_e3_gc[] == ""]
#   l6_form_e3_gc <- tail(deleted_form_e3_gc,e3_last_n_games)
#   l6_form_e3_gc <- as.numeric(l6_form_e3_gc)
#   suml6_e3_gc[index_e3_gc] <- sum(l6_form_e3_gc)
#   suml6_e3_gc[index_e3_gc] <- paste("(",suml6_e3_gc[index_e3_gc],")",sep = "")
#   l6_form_e3_gc <- paste(l6_form_e3_gc,collapse = " ")
#   final_e3_gc[index_e3_gc] <- rbind(paste(e3_teams[index_e3_gc],l6_form_e3_gc,suml6_e3_gc[index_e3_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
#
# #change column names
# final_e3_gc <- as.data.frame(final_e3_gc)
# colnames(final_e3_gc) <- "Goals conceded"
# #total goals
# #create final_e3_tg object
# final_e3_tg <- c()
# suml6_e3_tg <- c()
# for(index_e3_tg in 1:length(e3_teams))
# {
#   index_e3_tg <- row.names(e3_totalgoals_h) == e3_teams[index_e3_tg]
#   form_e3_tg <- e3_totalgoals_h[index_e3_tg]
#   deleted_form_e3_tg <- form_e3_tg[!form_e3_tg[] == ""]
#   l6_form_e3_tg <- tail(deleted_form_e3_tg,e3_last_n_games)
#   l6_form_e3_tg <- as.numeric(l6_form_e3_tg)
#   suml6_e3_tg[index_e3_tg] <- sum(l6_form_e3_tg)
#   suml6_e3_tg[index_e3_tg] <- paste("(",suml6_e3_tg[index_e3_tg],")",sep = "")
#   l6_form_e3_tg <- paste(l6_form_e3_tg,collapse = " ")
#   final_e3_tg[index_e3_tg] <- rbind(paste(e3_teams[index_e3_tg],l6_form_e3_tg,suml6_e3_tg[index_e3_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# #change column names
# final_e3_tg <- as.data.frame(final_e3_tg)
# colnames(final_e3_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_e3_hf object
# final_e3_cs <- c()
# for(index_e3_cs in 1:length(e3_teams))
# {
#   index_e3_cs <- row.names(e3_csform_h) == e3_teams[index_e3_cs]
#   csform_e3_cs <- e3_csform_h[index_e3_cs]
#   deleted_csform_e3_cs <- csform_e3_cs[!csform_e3_cs[] == ""]
#   l6_csform_e3_cs <- tail(deleted_csform_e3_cs,e3_last_n_games)
#   l6_csform_e3_cs <- paste(l6_csform_e3_cs,collapse = " ")
#   final_e3_cs[index_e3_cs] <- rbind(paste(e3_teams[index_e3_cs],l6_csform_e3_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",e3_teams[index],l6_csform)
#
# }
#
# #change column names
# final_e3_cs <- as.data.frame(final_e3_cs)
# colnames(final_e3_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_e3_wm object
# final_e3_wm <- c()
# suml6_e3_wm <- c()
# for(index_e3_wm in 1:length(e3_teams))
# {
#   index_e3_wm <- row.names(e3_winmargin_h) == e3_teams[index_e3_wm]
#   form_e3_wm <- e3_winmargin_h[index_e3_wm]
#   deleted_form_e3_wm <- form_e3_wm[!form_e3_wm[] == ""]
#   l6_form_e3_wm <- tail(deleted_form_e3_wm,e3_last_n_games)
#   l6_form_e3_wm <- as.numeric(l6_form_e3_wm)
#   suml6_e3_wm[index_e3_wm] <- sum(l6_form_e3_wm)
#   suml6_e3_wm[index_e3_wm] <- paste("(",suml6_e3_wm[index_e3_wm],")",sep = "")
#   l6_form_e3_wm <- paste(l6_form_e3_wm,collapse = " ")
#   final_e3_wm[index_e3_wm] <- rbind(paste(e3_teams[index_e3_wm],l6_form_e3_wm,suml6_e3_wm[index_e3_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# final_e3_wm
# #change column names
# final_e3_wm <- as.data.frame(final_e3_wm)
# colnames(final_e3_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_e3_ca object
# final_e3_ca <- c()
# suml6_e3_ca <- c()
# for(index_e3_ca in 1:length(e3_teams))
# {
#   index_e3_ca <- row.names(e3_coawarded_h) == e3_teams[index_e3_ca]
#   form_e3_ca <- e3_coawarded_h[index_e3_ca]
#   deleted_form_e3_ca <- form_e3_ca[!form_e3_ca[] == ""]
#   l6_form_e3_ca <- tail(deleted_form_e3_ca,e3_last_n_games)
#   l6_form_e3_ca <- as.numeric(l6_form_e3_ca)
#   suml6_e3_ca[index_e3_ca] <- sum(l6_form_e3_ca)
#   suml6_e3_ca[index_e3_ca] <- paste("(",suml6_e3_ca[index_e3_ca],")",sep = "")
#   l6_form_e3_ca <- paste(l6_form_e3_ca,collapse = " ")
#   final_e3_ca[index_e3_ca] <- rbind(paste(e3_teams[index_e3_ca],l6_form_e3_ca,suml6_e3_ca[index_e3_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# final_e3_ca
# #change column names
# final_e3_ca <- as.data.frame(final_e3_ca)
# colnames(final_e3_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_e3_ca object
# final_e3_cc <- c()
# suml6_e3_cc <- c()
# for(index_e3_cc in 1:length(e3_teams))
# {
#   index_e3_cc <- row.names(e3_cornersconceded_h) == e3_teams[index_e3_cc]
#   form_e3_cc <- e3_cornersconceded_h[index_e3_cc]
#   deleted_form_e3_cc <- form_e3_cc[!form_e3_cc[] == ""]
#   l6_form_e3_cc <- tail(deleted_form_e3_cc,e3_last_n_games)
#   l6_form_e3_cc <- as.numeric(l6_form_e3_cc)
#   suml6_e3_cc[index_e3_cc] <- sum(l6_form_e3_cc)
#   suml6_e3_cc[index_e3_cc] <- paste("(",suml6_e3_cc[index_e3_cc],")",sep = "")
#   l6_form_e3_cc <- paste(l6_form_e3_cc,collapse = " ")
#   final_e3_cc[index_e3_cc] <- rbind(paste(e3_teams[index_e3_cc],l6_form_e3_cc,suml6_e3_cc[index_e3_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# final_e3_cc
# #change column names
# final_e3_cc <- as.data.frame(final_e3_cc)
# colnames(final_e3_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_e3_cosc <- c()
# for(index_e3_cosc in 1:length(e3_teams))
# {
#   index_e3_cosc <- row.names(e3_coscform_h) == e3_teams[index_e3_cosc]
#   coscform_e3_cosc <- e3_coscform_h[index_e3_cosc]
#   deleted_coscform_e3_cosc <- coscform_e3_cosc[!coscform_e3_cosc[] == ""]
#   l6_coscform_e3_cosc <- tail(deleted_coscform_e3_cosc,e3_last_n_games)
#   l6_coscform_e3_cosc <- paste(l6_coscform_e3_cosc,collapse = " ")
#   final_e3_cosc[index_e3_cosc] <- rbind(paste(e3_teams[index_e3_cosc],l6_coscform_e3_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",e3_teams[index],l6_coscform)
#
# }
# final_e3_cosc
# #change column names
# final_e3_cosc <- as.data.frame(final_e3_cosc)
# colnames(final_e3_cosc) <- "CornersForm"
# ##################################################
# ##################################################
# #total corners
# #create final_e3_tcorners object
# final_e3_tcorners <- c()
# suml6_e3_tcorners <- c()
# for(index_e3_tcorners in 1:length(e3_teams))
# {
#   index_e3_tcorners <- row.names(e3_totalcorners_h) == e3_teams[index_e3_tcorners]
#   form_e3_tcorners <- e3_totalcorners_h[index_e3_tcorners]
#   deleted_form_e3_tcorners <- form_e3_tcorners[!form_e3_tcorners[] == ""]
#   l6_form_e3_tcorners <- tail(deleted_form_e3_tcorners,e3_last_n_games)
#   l6_form_e3_tcorners <- as.numeric(l6_form_e3_tcorners)
#   suml6_e3_tcorners[index_e3_tcorners] <- sum(l6_form_e3_tcorners)
#   suml6_e3_tcorners[index_e3_tcorners] <- paste("(",suml6_e3_tcorners[index_e3_tcorners],")",sep = "")
#   l6_form_e3_tcorners <- paste(l6_form_e3_tcorners,collapse = " ")
#   final_e3_tcorners[index_e3_tcorners] <- rbind(paste(e3_teams[index_e3_tcorners],l6_form_e3_tcorners,suml6_e3_tcorners[index_e3_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# #change column names
# final_e3_tcorners <- as.data.frame(final_e3_tcorners)
# colnames(final_e3_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_e3_hf_against
# final_e3_hf_against <- c()
# for(index_e3_hf_against in 1:length(e3_teams))
# {
#   index_e3_hf_against <- row.names(e3_form_team_against_h) == e3_teams[index_e3_hf_against]
#   form_e3_hf_against <- e3_form_team_against_h[index_e3_hf_against]
#   deleted_form_e3_hf_against <- form_e3_hf_against[!form_e3_hf_against[] == ""]
#   l6_form_e3_hf_against <- tail(deleted_form_e3_hf_against,e3_last_n_games)
#   l6_form_e3_hf_against <- paste(l6_form_e3_hf_against,collapse = " ")
#   final_e3_hf_against[index_e3_hf_against] <- rbind(paste(e3_teams[index_e3_hf_against],l6_form_e3_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",e3_teams[index],l6_form)
#
# }
# final_e3_hf_against <- as.data.frame(final_e3_hf_against)
# colnames(final_e3_hf_against) <- "Team against"
# #combine the columns
# final_e3_all <- cbind(final_e3_hf,final_e3_gs,final_e3_gc,final_e3_tg,final_e3_ca,final_e3_cc,final_e3_tcorners,final_e3_cosc,final_e3_hf_against)
# write.xlsx(final_e3_all,'Divisions/E3.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #EC
# #form
# #create final_ec_hf object
# final_ec_hf <- c()
# for(index_ec_hf in 1:length(ec_teams))
# {
#   index_ec_hf <- row.names(ec_form_h) == ec_teams[index_ec_hf]
#   form_ec_hf <- ec_form_h[index_ec_hf]
#   deleted_form_ec_hf <- form_ec_hf[!form_ec_hf[] == ""]
#   l6_form_ec_hf <- tail(deleted_form_ec_hf,ec_last_n_games)
#   l6_form_ec_hf <- paste(l6_form_ec_hf,collapse = " ")
#   final_ec_hf[index_ec_hf] <- rbind(paste(ec_teams[index_ec_hf],l6_form_ec_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
#
# #change column names
# final_ec_hf <- as.data.frame(final_ec_hf)
# colnames(final_ec_hf) <- "Form"
# #goals scored
# #create final_ec_gs object
# final_ec_gs <- c()
# suml6_ec_gs <- c()
# for(index_ec_gs in 1:length(ec_teams))
# {
#   index_ec_gs <- row.names(ec_goalscored_h) == ec_teams[index_ec_gs]
#   form_ec_gs <- ec_goalscored_h[index_ec_gs]
#   deleted_form_ec_gs <- form_ec_gs[!form_ec_gs[] == ""]
#   l6_form_ec_gs <- tail(deleted_form_ec_gs,ec_last_n_games)
#   l6_form_ec_gs <- as.numeric(l6_form_ec_gs)
#   suml6_ec_gs[index_ec_gs] <- sum(l6_form_ec_gs)
#   suml6_ec_gs[index_ec_gs] <- paste("(",suml6_ec_gs[index_ec_gs],")",sep = "")
#   l6_form_ec_gs <- paste(l6_form_ec_gs,collapse = " ")
#   final_ec_gs[index_ec_gs] <- rbind(paste(ec_teams[index_ec_gs],l6_form_ec_gs,suml6_ec_gs[index_ec_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
# final_ec_gs
# #change column names
# final_ec_gs <- as.data.frame(final_ec_gs)
# colnames(final_ec_gs) <- "Goals scored"
# #goal conceded
# #create final_ec_gc object
# final_ec_gc <- c()
# suml6_ec_gc <- c()
# for(index_ec_gc in 1:length(ec_teams))
# {
#   index_ec_gc <- row.names(ec_goalconceded_h) == ec_teams[index_ec_gc]
#   form_ec_gc <- ec_goalconceded_h[index_ec_gc]
#   deleted_form_ec_gc <- form_ec_gc[!form_ec_gc[] == ""]
#   l6_form_ec_gc <- tail(deleted_form_ec_gc,ec_last_n_games)
#   l6_form_ec_gc <- as.numeric(l6_form_ec_gc)
#   suml6_ec_gc[index_ec_gc] <- sum(l6_form_ec_gc)
#   suml6_ec_gc[index_ec_gc] <- paste("(",suml6_ec_gc[index_ec_gc],")",sep = "")
#   l6_form_ec_gc <- paste(l6_form_ec_gc,collapse = " ")
#   final_ec_gc[index_ec_gc] <- rbind(paste(ec_teams[index_ec_gc],l6_form_ec_gc,suml6_ec_gc[index_ec_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
#
# #change column names
# final_ec_gc <- as.data.frame(final_ec_gc)
# colnames(final_ec_gc) <- "Goals conceded"
# #total goals
# #create final_ec_tg object
# final_ec_tg <- c()
# suml6_ec_tg <- c()
# for(index_ec_tg in 1:length(ec_teams))
# {
#   index_ec_tg <- row.names(ec_totalgoals_h) == ec_teams[index_ec_tg]
#   form_ec_tg <- ec_totalgoals_h[index_ec_tg]
#   deleted_form_ec_tg <- form_ec_tg[!form_ec_tg[] == ""]
#   l6_form_ec_tg <- tail(deleted_form_ec_tg,ec_last_n_games)
#   l6_form_ec_tg <- as.numeric(l6_form_ec_tg)
#   suml6_ec_tg[index_ec_tg] <- sum(l6_form_ec_tg)
#   suml6_ec_tg[index_ec_tg] <- paste("(",suml6_ec_tg[index_ec_tg],")",sep = "")
#   l6_form_ec_tg <- paste(l6_form_ec_tg,collapse = " ")
#   final_ec_tg[index_ec_tg] <- rbind(paste(ec_teams[index_ec_tg],l6_form_ec_tg,suml6_ec_tg[index_ec_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
# #change column names
# final_ec_tg <- as.data.frame(final_ec_tg)
# colnames(final_ec_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_ec_hf object
# final_ec_cs <- c()
# for(index_ec_cs in 1:length(ec_teams))
# {
#   index_ec_cs <- row.names(ec_csform_h) == ec_teams[index_ec_cs]
#   csform_ec_cs <- ec_csform_h[index_ec_cs]
#   deleted_csform_ec_cs <- csform_ec_cs[!csform_ec_cs[] == ""]
#   l6_csform_ec_cs <- tail(deleted_csform_ec_cs,ec_last_n_games)
#   l6_csform_ec_cs <- paste(l6_csform_ec_cs,collapse = " ")
#   final_ec_cs[index_ec_cs] <- rbind(paste(ec_teams[index_ec_cs],l6_csform_ec_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",ec_teams[index],l6_csform)
#
# }
#
# #change column names
# final_ec_cs <- as.data.frame(final_ec_cs)
# colnames(final_ec_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_ec_wm object
# final_ec_wm <- c()
# suml6_ec_wm <- c()
# for(index_ec_wm in 1:length(ec_teams))
# {
#   index_ec_wm <- row.names(ec_winmargin_h) == ec_teams[index_ec_wm]
#   form_ec_wm <- ec_winmargin_h[index_ec_wm]
#   deleted_form_ec_wm <- form_ec_wm[!form_ec_wm[] == ""]
#   l6_form_ec_wm <- tail(deleted_form_ec_wm,ec_last_n_games)
#   l6_form_ec_wm <- as.numeric(l6_form_ec_wm)
#   suml6_ec_wm[index_ec_wm] <- sum(l6_form_ec_wm)
#   suml6_ec_wm[index_ec_wm] <- paste("(",suml6_ec_wm[index_ec_wm],")",sep = "")
#   l6_form_ec_wm <- paste(l6_form_ec_wm,collapse = " ")
#   final_ec_wm[index_ec_wm] <- rbind(paste(ec_teams[index_ec_wm],l6_form_ec_wm,suml6_ec_wm[index_ec_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
# final_ec_wm
# #change column names
# final_ec_wm <- as.data.frame(final_ec_wm)
# colnames(final_ec_wm) <- "Win Margin"
# #################################################
# #Team against
# #create final_ec_hf_against
# final_ec_hf_against <- c()
# for(index_ec_hf_against in 1:length(ec_teams))
# {
#   index_ec_hf_against <- row.names(ec_form_team_against_h) == ec_teams[index_ec_hf_against]
#   form_ec_hf_against <- ec_form_team_against_h[index_ec_hf_against]
#   deleted_form_ec_hf_against <- form_ec_hf_against[!form_ec_hf_against[] == ""]
#   l6_form_ec_hf_against <- tail(deleted_form_ec_hf_against,ec_last_n_games)
#   l6_form_ec_hf_against <- paste(l6_form_ec_hf_against,collapse = " ")
#   final_ec_hf_against[index_ec_hf_against] <- rbind(paste(ec_teams[index_ec_hf_against],l6_form_ec_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",ec_teams[index],l6_form)
#
# }
# final_ec_hf_against <- as.data.frame(final_ec_hf_against)
# colnames(final_ec_hf_against) <- "Team against"
# #combine the columns
# final_ec_all <- cbind(final_ec_hf,final_ec_gs,final_ec_gc,final_ec_tg,final_ec_cs,final_ec_wm,final_ec_hf_against)
# write.xlsx(final_ec_all,'Divisions/EC.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #F1
# #form
# #create final_f1_hf object
# final_f1_hf <- c()
# for(index_f1_hf in 1:length(f1_teams))
# {
#   index_f1_hf <- row.names(f1_form_h) == f1_teams[index_f1_hf]
#   form_f1_hf <- f1_form_h[index_f1_hf]
#   deleted_form_f1_hf <- form_f1_hf[!form_f1_hf[] == ""]
#   l6_form_f1_hf <- tail(deleted_form_f1_hf,f1_last_n_games)
#   l6_form_f1_hf <- paste(l6_form_f1_hf,collapse = " ")
#   final_f1_hf[index_f1_hf] <- rbind(paste(f1_teams[index_f1_hf],l6_form_f1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
#
# #change column names
# final_f1_hf <- as.data.frame(final_f1_hf)
# colnames(final_f1_hf) <- "Form"
# #goals scored
# #create final_f1_gs object
# final_f1_gs <- c()
# suml6_f1_gs <- c()
# for(index_f1_gs in 1:length(f1_teams))
# {
#   index_f1_gs <- row.names(f1_goalscored_h) == f1_teams[index_f1_gs]
#   form_f1_gs <- f1_goalscored_h[index_f1_gs]
#   deleted_form_f1_gs <- form_f1_gs[!form_f1_gs[] == ""]
#   l6_form_f1_gs <- tail(deleted_form_f1_gs,f1_last_n_games)
#   l6_form_f1_gs <- as.numeric(l6_form_f1_gs)
#   suml6_f1_gs[index_f1_gs] <- sum(l6_form_f1_gs)
#   suml6_f1_gs[index_f1_gs] <- paste("(",suml6_f1_gs[index_f1_gs],")",sep = "")
#   l6_form_f1_gs <- paste(l6_form_f1_gs,collapse = " ")
#   final_f1_gs[index_f1_gs] <- rbind(paste(f1_teams[index_f1_gs],l6_form_f1_gs,suml6_f1_gs[index_f1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# final_f1_gs
# #change column names
# final_f1_gs <- as.data.frame(final_f1_gs)
# colnames(final_f1_gs) <- "Goals scored"
# #goal conceded
# #create final_f1_gc object
# final_f1_gc <- c()
# suml6_f1_gc <- c()
# for(index_f1_gc in 1:length(f1_teams))
# {
#   index_f1_gc <- row.names(f1_goalconceded_h) == f1_teams[index_f1_gc]
#   form_f1_gc <- f1_goalconceded_h[index_f1_gc]
#   deleted_form_f1_gc <- form_f1_gc[!form_f1_gc[] == ""]
#   l6_form_f1_gc <- tail(deleted_form_f1_gc,f1_last_n_games)
#   l6_form_f1_gc <- as.numeric(l6_form_f1_gc)
#   suml6_f1_gc[index_f1_gc] <- sum(l6_form_f1_gc)
#   suml6_f1_gc[index_f1_gc] <- paste("(",suml6_f1_gc[index_f1_gc],")",sep = "")
#   l6_form_f1_gc <- paste(l6_form_f1_gc,collapse = " ")
#   final_f1_gc[index_f1_gc] <- rbind(paste(f1_teams[index_f1_gc],l6_form_f1_gc,suml6_f1_gc[index_f1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
#
# #change column names
# final_f1_gc <- as.data.frame(final_f1_gc)
# colnames(final_f1_gc) <- "Goals conceded"
# #total goals
# #create final_f1_tg object
# final_f1_tg <- c()
# suml6_f1_tg <- c()
# for(index_f1_tg in 1:length(f1_teams))
# {
#   index_f1_tg <- row.names(f1_totalgoals_h) == f1_teams[index_f1_tg]
#   form_f1_tg <- f1_totalgoals_h[index_f1_tg]
#   deleted_form_f1_tg <- form_f1_tg[!form_f1_tg[] == ""]
#   l6_form_f1_tg <- tail(deleted_form_f1_tg,f1_last_n_games)
#   l6_form_f1_tg <- as.numeric(l6_form_f1_tg)
#   suml6_f1_tg[index_f1_tg] <- sum(l6_form_f1_tg)
#   suml6_f1_tg[index_f1_tg] <- paste("(",suml6_f1_tg[index_f1_tg],")",sep = "")
#   l6_form_f1_tg <- paste(l6_form_f1_tg,collapse = " ")
#   final_f1_tg[index_f1_tg] <- rbind(paste(f1_teams[index_f1_tg],l6_form_f1_tg,suml6_f1_tg[index_f1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# #change column names
# final_f1_tg <- as.data.frame(final_f1_tg)
# colnames(final_f1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_f1_hf object
# final_f1_cs <- c()
# for(index_f1_cs in 1:length(f1_teams))
# {
#   index_f1_cs <- row.names(f1_csform_h) == f1_teams[index_f1_cs]
#   csform_f1_cs <- f1_csform_h[index_f1_cs]
#   deleted_csform_f1_cs <- csform_f1_cs[!csform_f1_cs[] == ""]
#   l6_csform_f1_cs <- tail(deleted_csform_f1_cs,f1_last_n_games)
#   l6_csform_f1_cs <- paste(l6_csform_f1_cs,collapse = " ")
#   final_f1_cs[index_f1_cs] <- rbind(paste(f1_teams[index_f1_cs],l6_csform_f1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",f1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_f1_cs <- as.data.frame(final_f1_cs)
# colnames(final_f1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_f1_wm object
# final_f1_wm <- c()
# suml6_f1_wm <- c()
# for(index_f1_wm in 1:length(f1_teams))
# {
#   index_f1_wm <- row.names(f1_winmargin_h) == f1_teams[index_f1_wm]
#   form_f1_wm <- f1_winmargin_h[index_f1_wm]
#   deleted_form_f1_wm <- form_f1_wm[!form_f1_wm[] == ""]
#   l6_form_f1_wm <- tail(deleted_form_f1_wm,f1_last_n_games)
#   l6_form_f1_wm <- as.numeric(l6_form_f1_wm)
#   suml6_f1_wm[index_f1_wm] <- sum(l6_form_f1_wm)
#   suml6_f1_wm[index_f1_wm] <- paste("(",suml6_f1_wm[index_f1_wm],")",sep = "")
#   l6_form_f1_wm <- paste(l6_form_f1_wm,collapse = " ")
#   final_f1_wm[index_f1_wm] <- rbind(paste(f1_teams[index_f1_wm],l6_form_f1_wm,suml6_f1_wm[index_f1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# final_f1_wm
# #change column names
# final_f1_wm <- as.data.frame(final_f1_wm)
# colnames(final_f1_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_f1_ca object
# final_f1_ca <- c()
# suml6_f1_ca <- c()
# for(index_f1_ca in 1:length(f1_teams))
# {
#   index_f1_ca <- row.names(f1_coawarded_h) == f1_teams[index_f1_ca]
#   form_f1_ca <- f1_coawarded_h[index_f1_ca]
#   deleted_form_f1_ca <- form_f1_ca[!form_f1_ca[] == ""]
#   l6_form_f1_ca <- tail(deleted_form_f1_ca,f1_last_n_games)
#   l6_form_f1_ca <- as.numeric(l6_form_f1_ca)
#   suml6_f1_ca[index_f1_ca] <- sum(l6_form_f1_ca)
#   suml6_f1_ca[index_f1_ca] <- paste("(",suml6_f1_ca[index_f1_ca],")",sep = "")
#   l6_form_f1_ca <- paste(l6_form_f1_ca,collapse = " ")
#   final_f1_ca[index_f1_ca] <- rbind(paste(f1_teams[index_f1_ca],l6_form_f1_ca,suml6_f1_ca[index_f1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# final_f1_ca
# #change column names
# final_f1_ca <- as.data.frame(final_f1_ca)
# colnames(final_f1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_f1_ca object
# final_f1_cc <- c()
# suml6_f1_cc <- c()
# for(index_f1_cc in 1:length(f1_teams))
# {
#   index_f1_cc <- row.names(f1_cornersconceded_h) == f1_teams[index_f1_cc]
#   form_f1_cc <- f1_cornersconceded_h[index_f1_cc]
#   deleted_form_f1_cc <- form_f1_cc[!form_f1_cc[] == ""]
#   l6_form_f1_cc <- tail(deleted_form_f1_cc,f1_last_n_games)
#   l6_form_f1_cc <- as.numeric(l6_form_f1_cc)
#   suml6_f1_cc[index_f1_cc] <- sum(l6_form_f1_cc)
#   suml6_f1_cc[index_f1_cc] <- paste("(",suml6_f1_cc[index_f1_cc],")",sep = "")
#   l6_form_f1_cc <- paste(l6_form_f1_cc,collapse = " ")
#   final_f1_cc[index_f1_cc] <- rbind(paste(f1_teams[index_f1_cc],l6_form_f1_cc,suml6_f1_cc[index_f1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# final_f1_cc
# #change column names
# final_f1_cc <- as.data.frame(final_f1_cc)
# colnames(final_f1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_f1_cosc <- c()
# for(index_f1_cosc in 1:length(f1_teams))
# {
#   index_f1_cosc <- row.names(f1_coscform_h) == f1_teams[index_f1_cosc]
#   coscform_f1_cosc <- f1_coscform_h[index_f1_cosc]
#   deleted_coscform_f1_cosc <- coscform_f1_cosc[!coscform_f1_cosc[] == ""]
#   l6_coscform_f1_cosc <- tail(deleted_coscform_f1_cosc,f1_last_n_games)
#   l6_coscform_f1_cosc <- paste(l6_coscform_f1_cosc,collapse = " ")
#   final_f1_cosc[index_f1_cosc] <- rbind(paste(f1_teams[index_f1_cosc],l6_coscform_f1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",f1_teams[index],l6_coscform)
#
# }
# final_f1_cosc
# #change column names
# final_f1_cosc <- as.data.frame(final_f1_cosc)
# colnames(final_f1_cosc) <- "CornersForm"
# ##################################################
#
# ##################################################
# #total corners
# #create final_f1_tcorners object
# final_f1_tcorners <- c()
# suml6_f1_tcorners <- c()
# for(index_f1_tcorners in 1:length(f1_teams))
# {
#   index_f1_tcorners <- row.names(f1_totalcorners_h) == f1_teams[index_f1_tcorners]
#   form_f1_tcorners <- f1_totalcorners_h[index_f1_tcorners]
#   deleted_form_f1_tcorners <- form_f1_tcorners[!form_f1_tcorners[] == ""]
#   l6_form_f1_tcorners <- tail(deleted_form_f1_tcorners,f1_last_n_games)
#   l6_form_f1_tcorners <- as.numeric(l6_form_f1_tcorners)
#   suml6_f1_tcorners[index_f1_tcorners] <- sum(l6_form_f1_tcorners)
#   suml6_f1_tcorners[index_f1_tcorners] <- paste("(",suml6_f1_tcorners[index_f1_tcorners],")",sep = "")
#   l6_form_f1_tcorners <- paste(l6_form_f1_tcorners,collapse = " ")
#   final_f1_tcorners[index_f1_tcorners] <- rbind(paste(f1_teams[index_f1_tcorners],l6_form_f1_tcorners,suml6_f1_tcorners[index_f1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# #change column names
# final_f1_tcorners <- as.data.frame(final_f1_tcorners)
# colnames(final_f1_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_f1_hf_against
# final_f1_hf_against <- c()
# for(index_f1_hf_against in 1:length(f1_teams))
# {
#   index_f1_hf_against <- row.names(f1_form_team_against_h) == f1_teams[index_f1_hf_against]
#   form_f1_hf_against <- f1_form_team_against_h[index_f1_hf_against]
#   deleted_form_f1_hf_against <- form_f1_hf_against[!form_f1_hf_against[] == ""]
#   l6_form_f1_hf_against <- tail(deleted_form_f1_hf_against,f1_last_n_games)
#   l6_form_f1_hf_against <- paste(l6_form_f1_hf_against,collapse = " ")
#   final_f1_hf_against[index_f1_hf_against] <- rbind(paste(f1_teams[index_f1_hf_against],l6_form_f1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f1_teams[index],l6_form)
#
# }
# final_f1_hf_against <- as.data.frame(final_f1_hf_against)
# colnames(final_f1_hf_against) <- "Team against"
# #combine the columns
# final_f1_all <- cbind(final_f1_hf,final_f1_gs,final_f1_gc,final_f1_tg,final_f1_ca,final_f1_cc,final_f1_tcorners,final_f1_cosc,final_f1_hf_against)
# write.xlsx(final_f1_all,'Divisions/F1.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# ################################################################################
# #F2
# #form
# #create final_f2_hf object
# final_f2_hf <- c()
# for(index_f2_hf in 1:length(f2_teams))
# {
#   index_f2_hf <- row.names(f2_form_h) == f2_teams[index_f2_hf]
#   form_f2_hf <- f2_form_h[index_f2_hf]
#   deleted_form_f2_hf <- form_f2_hf[!form_f2_hf[] == ""]
#   l6_form_f2_hf <- tail(deleted_form_f2_hf,f2_last_n_games)
#   l6_form_f2_hf <- paste(l6_form_f2_hf,collapse = " ")
#   final_f2_hf[index_f2_hf] <- rbind(paste(f2_teams[index_f2_hf],l6_form_f2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
#
# #change column names
# final_f2_hf <- as.data.frame(final_f2_hf)
# colnames(final_f2_hf) <- "Form"
# #goals scored
# #create final_f2_gs object
# final_f2_gs <- c()
# suml6_f2_gs <- c()
# for(index_f2_gs in 1:length(f2_teams))
# {
#   index_f2_gs <- row.names(f2_goalscored_h) == f2_teams[index_f2_gs]
#   form_f2_gs <- f2_goalscored_h[index_f2_gs]
#   deleted_form_f2_gs <- form_f2_gs[!form_f2_gs[] == ""]
#   l6_form_f2_gs <- tail(deleted_form_f2_gs,f2_last_n_games)
#   l6_form_f2_gs <- as.numeric(l6_form_f2_gs)
#   suml6_f2_gs[index_f2_gs] <- sum(l6_form_f2_gs)
#   suml6_f2_gs[index_f2_gs] <- paste("(",suml6_f2_gs[index_f2_gs],")",sep = "")
#   l6_form_f2_gs <- paste(l6_form_f2_gs,collapse = " ")
#   final_f2_gs[index_f2_gs] <- rbind(paste(f2_teams[index_f2_gs],l6_form_f2_gs,suml6_f2_gs[index_f2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# final_f2_gs
# #change column names
# final_f2_gs <- as.data.frame(final_f2_gs)
# colnames(final_f2_gs) <- "Goals scored"
# #goal conceded
# #create final_f2_gc object
# final_f2_gc <- c()
# suml6_f2_gc <- c()
# for(index_f2_gc in 1:length(f2_teams))
# {
#   index_f2_gc <- row.names(f2_goalconceded_h) == f2_teams[index_f2_gc]
#   form_f2_gc <- f2_goalconceded_h[index_f2_gc]
#   deleted_form_f2_gc <- form_f2_gc[!form_f2_gc[] == ""]
#   l6_form_f2_gc <- tail(deleted_form_f2_gc,f2_last_n_games)
#   l6_form_f2_gc <- as.numeric(l6_form_f2_gc)
#   suml6_f2_gc[index_f2_gc] <- sum(l6_form_f2_gc)
#   suml6_f2_gc[index_f2_gc] <- paste("(",suml6_f2_gc[index_f2_gc],")",sep = "")
#   l6_form_f2_gc <- paste(l6_form_f2_gc,collapse = " ")
#   final_f2_gc[index_f2_gc] <- rbind(paste(f2_teams[index_f2_gc],l6_form_f2_gc,suml6_f2_gc[index_f2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
#
# #change column names
# final_f2_gc <- as.data.frame(final_f2_gc)
# colnames(final_f2_gc) <- "Goals conceded"
# #total goals
# #create final_f2_tg object
# final_f2_tg <- c()
# suml6_f2_tg <- c()
# for(index_f2_tg in 1:length(f2_teams))
# {
#   index_f2_tg <- row.names(f2_totalgoals_h) == f2_teams[index_f2_tg]
#   form_f2_tg <- f2_totalgoals_h[index_f2_tg]
#   deleted_form_f2_tg <- form_f2_tg[!form_f2_tg[] == ""]
#   l6_form_f2_tg <- tail(deleted_form_f2_tg,f2_last_n_games)
#   l6_form_f2_tg <- as.numeric(l6_form_f2_tg)
#   suml6_f2_tg[index_f2_tg] <- sum(l6_form_f2_tg)
#   suml6_f2_tg[index_f2_tg] <- paste("(",suml6_f2_tg[index_f2_tg],")",sep = "")
#   l6_form_f2_tg <- paste(l6_form_f2_tg,collapse = " ")
#   final_f2_tg[index_f2_tg] <- rbind(paste(f2_teams[index_f2_tg],l6_form_f2_tg,suml6_f2_tg[index_f2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# #change column names
# final_f2_tg <- as.data.frame(final_f2_tg)
# colnames(final_f2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_f2_hf object
# final_f2_cs <- c()
# for(index_f2_cs in 1:length(f2_teams))
# {
#   index_f2_cs <- row.names(f2_csform_h) == f2_teams[index_f2_cs]
#   csform_f2_cs <- f2_csform_h[index_f2_cs]
#   deleted_csform_f2_cs <- csform_f2_cs[!csform_f2_cs[] == ""]
#   l6_csform_f2_cs <- tail(deleted_csform_f2_cs,f2_last_n_games)
#   l6_csform_f2_cs <- paste(l6_csform_f2_cs,collapse = " ")
#   final_f2_cs[index_f2_cs] <- rbind(paste(f2_teams[index_f2_cs],l6_csform_f2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",f2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_f2_cs <- as.data.frame(final_f2_cs)
# colnames(final_f2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_f2_wm object
# final_f2_wm <- c()
# suml6_f2_wm <- c()
# for(index_f2_wm in 1:length(f2_teams))
# {
#   index_f2_wm <- row.names(f2_winmargin_h) == f2_teams[index_f2_wm]
#   form_f2_wm <- f2_winmargin_h[index_f2_wm]
#   deleted_form_f2_wm <- form_f2_wm[!form_f2_wm[] == ""]
#   l6_form_f2_wm <- tail(deleted_form_f2_wm,f2_last_n_games)
#   l6_form_f2_wm <- as.numeric(l6_form_f2_wm)
#   suml6_f2_wm[index_f2_wm] <- sum(l6_form_f2_wm)
#   suml6_f2_wm[index_f2_wm] <- paste("(",suml6_f2_wm[index_f2_wm],")",sep = "")
#   l6_form_f2_wm <- paste(l6_form_f2_wm,collapse = " ")
#   final_f2_wm[index_f2_wm] <- rbind(paste(f2_teams[index_f2_wm],l6_form_f2_wm,suml6_f2_wm[index_f2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# final_f2_wm
# #change column names
# final_f2_wm <- as.data.frame(final_f2_wm)
# colnames(final_f2_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_f2_ca object
# final_f2_ca <- c()
# suml6_f2_ca <- c()
# for(index_f2_ca in 1:length(f2_teams))
# {
#   index_f2_ca <- row.names(f2_coawarded_h) == f2_teams[index_f2_ca]
#   form_f2_ca <- f2_coawarded_h[index_f2_ca]
#   deleted_form_f2_ca <- form_f2_ca[!form_f2_ca[] == ""]
#   l6_form_f2_ca <- tail(deleted_form_f2_ca,f2_last_n_games)
#   l6_form_f2_ca <- as.numeric(l6_form_f2_ca)
#   suml6_f2_ca[index_f2_ca] <- sum(l6_form_f2_ca)
#   suml6_f2_ca[index_f2_ca] <- paste("(",suml6_f2_ca[index_f2_ca],")",sep = "")
#   l6_form_f2_ca <- paste(l6_form_f2_ca,collapse = " ")
#   final_f2_ca[index_f2_ca] <- rbind(paste(f2_teams[index_f2_ca],l6_form_f2_ca,suml6_f2_ca[index_f2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# final_f2_ca
# #change column names
# final_f2_ca <- as.data.frame(final_f2_ca)
# colnames(final_f2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_f2_ca object
# final_f2_cc <- c()
# suml6_f2_cc <- c()
# for(index_f2_cc in 1:length(f2_teams))
# {
#   index_f2_cc <- row.names(f2_cornersconceded_h) == f2_teams[index_f2_cc]
#   form_f2_cc <- f2_cornersconceded_h[index_f2_cc]
#   deleted_form_f2_cc <- form_f2_cc[!form_f2_cc[] == ""]
#   l6_form_f2_cc <- tail(deleted_form_f2_cc,f2_last_n_games)
#   l6_form_f2_cc <- as.numeric(l6_form_f2_cc)
#   suml6_f2_cc[index_f2_cc] <- sum(l6_form_f2_cc)
#   suml6_f2_cc[index_f2_cc] <- paste("(",suml6_f2_cc[index_f2_cc],")",sep = "")
#   l6_form_f2_cc <- paste(l6_form_f2_cc,collapse = " ")
#   final_f2_cc[index_f2_cc] <- rbind(paste(f2_teams[index_f2_cc],l6_form_f2_cc,suml6_f2_cc[index_f2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# final_f2_cc
# #change column names
# final_f2_cc <- as.data.frame(final_f2_cc)
# colnames(final_f2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_f2_cosc <- c()
# for(index_f2_cosc in 1:length(f2_teams))
# {
#   index_f2_cosc <- row.names(f2_coscform_h) == f2_teams[index_f2_cosc]
#   coscform_f2_cosc <- f2_coscform_h[index_f2_cosc]
#   deleted_coscform_f2_cosc <- coscform_f2_cosc[!coscform_f2_cosc[] == ""]
#   l6_coscform_f2_cosc <- tail(deleted_coscform_f2_cosc,f2_last_n_games)
#   l6_coscform_f2_cosc <- paste(l6_coscform_f2_cosc,collapse = " ")
#   final_f2_cosc[index_f2_cosc] <- rbind(paste(f2_teams[index_f2_cosc],l6_coscform_f2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",f2_teams[index],l6_coscform)
#
# }
# final_f2_cosc
# #change column names
# final_f2_cosc <- as.data.frame(final_f2_cosc)
# colnames(final_f2_cosc) <- "CornersForm"
# ##################################################
#
# ##################################################
# #total corners
# #create final_f2_tcorners object
# final_f2_tcorners <- c()
# suml6_f2_tcorners <- c()
# for(index_f2_tcorners in 1:length(f2_teams))
# {
#   index_f2_tcorners <- row.names(f2_totalcorners_h) == f2_teams[index_f2_tcorners]
#   form_f2_tcorners <- f2_totalcorners_h[index_f2_tcorners]
#   deleted_form_f2_tcorners <- form_f2_tcorners[!form_f2_tcorners[] == ""]
#   l6_form_f2_tcorners <- tail(deleted_form_f2_tcorners,f2_last_n_games)
#   l6_form_f2_tcorners <- as.numeric(l6_form_f2_tcorners)
#   suml6_f2_tcorners[index_f2_tcorners] <- sum(l6_form_f2_tcorners)
#   suml6_f2_tcorners[index_f2_tcorners] <- paste("(",suml6_f2_tcorners[index_f2_tcorners],")",sep = "")
#   l6_form_f2_tcorners <- paste(l6_form_f2_tcorners,collapse = " ")
#   final_f2_tcorners[index_f2_tcorners] <- rbind(paste(f2_teams[index_f2_tcorners],l6_form_f2_tcorners,suml6_f2_tcorners[index_f2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# #change column names
# final_f2_tcorners <- as.data.frame(final_f2_tcorners)
# colnames(final_f2_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_f2_hf_against
# final_f2_hf_against <- c()
# for(index_f2_hf_against in 1:length(f2_teams))
# {
#   index_f2_hf_against <- row.names(f2_form_team_against_h) == f2_teams[index_f2_hf_against]
#   form_f2_hf_against <- f2_form_team_against_h[index_f2_hf_against]
#   deleted_form_f2_hf_against <- form_f2_hf_against[!form_f2_hf_against[] == ""]
#   l6_form_f2_hf_against <- tail(deleted_form_f2_hf_against,f2_last_n_games)
#   l6_form_f2_hf_against <- paste(l6_form_f2_hf_against,collapse = " ")
#   final_f2_hf_against[index_f2_hf_against] <- rbind(paste(f2_teams[index_f2_hf_against],l6_form_f2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",f2_teams[index],l6_form)
#
# }
# final_f2_hf_against <- as.data.frame(final_f2_hf_against)
# colnames(final_f2_hf_against) <- "Team against"
# #combine the columns
# final_f2_all <- cbind(final_f2_hf,final_f2_gs,final_f2_gc,final_f2_tg,final_f2_ca,final_f2_cc,final_f2_tcorners,final_f2_cosc,final_f2_hf_against)
# write.xlsx(final_f2_all,'Divisions/F2.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #G1
# #form
# #create final_g1_hf object
# final_g1_hf <- c()
# for(index_g1_hf in 1:length(g1_teams))
# {
#   index_g1_hf <- row.names(g1_form_h) == g1_teams[index_g1_hf]
#   form_g1_hf <- g1_form_h[index_g1_hf]
#   deleted_form_g1_hf <- form_g1_hf[!form_g1_hf[] == ""]
#   l6_form_g1_hf <- tail(deleted_form_g1_hf,g1_last_n_games)
#   l6_form_g1_hf <- paste(l6_form_g1_hf,collapse = " ")
#   final_g1_hf[index_g1_hf] <- rbind(paste(g1_teams[index_g1_hf],l6_form_g1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
#
# #change column names
# final_g1_hf <- as.data.frame(final_g1_hf)
# colnames(final_g1_hf) <- "Form"
# #goals scored
# #create final_g1_gs object
# final_g1_gs <- c()
# suml6_g1_gs <- c()
# for(index_g1_gs in 1:length(g1_teams))
# {
#   index_g1_gs <- row.names(g1_goalscored_h) == g1_teams[index_g1_gs]
#   form_g1_gs <- g1_goalscored_h[index_g1_gs]
#   deleted_form_g1_gs <- form_g1_gs[!form_g1_gs[] == ""]
#   l6_form_g1_gs <- tail(deleted_form_g1_gs,g1_last_n_games)
#   l6_form_g1_gs <- as.numeric(l6_form_g1_gs)
#   suml6_g1_gs[index_g1_gs] <- sum(l6_form_g1_gs)
#   suml6_g1_gs[index_g1_gs] <- paste("(",suml6_g1_gs[index_g1_gs],")",sep = "")
#   l6_form_g1_gs <- paste(l6_form_g1_gs,collapse = " ")
#   final_g1_gs[index_g1_gs] <- rbind(paste(g1_teams[index_g1_gs],l6_form_g1_gs,suml6_g1_gs[index_g1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# final_g1_gs
# #change column names
# final_g1_gs <- as.data.frame(final_g1_gs)
# colnames(final_g1_gs) <- "Goals scored"
# #goal conceded
# #create final_g1_gc object
# final_g1_gc <- c()
# suml6_g1_gc <- c()
# for(index_g1_gc in 1:length(g1_teams))
# {
#   index_g1_gc <- row.names(g1_goalconceded_h) == g1_teams[index_g1_gc]
#   form_g1_gc <- g1_goalconceded_h[index_g1_gc]
#   deleted_form_g1_gc <- form_g1_gc[!form_g1_gc[] == ""]
#   l6_form_g1_gc <- tail(deleted_form_g1_gc,g1_last_n_games)
#   l6_form_g1_gc <- as.numeric(l6_form_g1_gc)
#   suml6_g1_gc[index_g1_gc] <- sum(l6_form_g1_gc)
#   suml6_g1_gc[index_g1_gc] <- paste("(",suml6_g1_gc[index_g1_gc],")",sep = "")
#   l6_form_g1_gc <- paste(l6_form_g1_gc,collapse = " ")
#   final_g1_gc[index_g1_gc] <- rbind(paste(g1_teams[index_g1_gc],l6_form_g1_gc,suml6_g1_gc[index_g1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
#
# #change column names
# final_g1_gc <- as.data.frame(final_g1_gc)
# colnames(final_g1_gc) <- "Goals conceded"
# #total goals
# #create final_g1_tg object
# final_g1_tg <- c()
# suml6_g1_tg <- c()
# for(index_g1_tg in 1:length(g1_teams))
# {
#   index_g1_tg <- row.names(g1_totalgoals_h) == g1_teams[index_g1_tg]
#   form_g1_tg <- g1_totalgoals_h[index_g1_tg]
#   deleted_form_g1_tg <- form_g1_tg[!form_g1_tg[] == ""]
#   l6_form_g1_tg <- tail(deleted_form_g1_tg,g1_last_n_games)
#   l6_form_g1_tg <- as.numeric(l6_form_g1_tg)
#   suml6_g1_tg[index_g1_tg] <- sum(l6_form_g1_tg)
#   suml6_g1_tg[index_g1_tg] <- paste("(",suml6_g1_tg[index_g1_tg],")",sep = "")
#   l6_form_g1_tg <- paste(l6_form_g1_tg,collapse = " ")
#   final_g1_tg[index_g1_tg] <- rbind(paste(g1_teams[index_g1_tg],l6_form_g1_tg,suml6_g1_tg[index_g1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# #change column names
# final_g1_tg <- as.data.frame(final_g1_tg)
# colnames(final_g1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_g1_hf object
# final_g1_cs <- c()
# for(index_g1_cs in 1:length(g1_teams))
# {
#   index_g1_cs <- row.names(g1_csform_h) == g1_teams[index_g1_cs]
#   csform_g1_cs <- g1_csform_h[index_g1_cs]
#   deleted_csform_g1_cs <- csform_g1_cs[!csform_g1_cs[] == ""]
#   l6_csform_g1_cs <- tail(deleted_csform_g1_cs,g1_last_n_games)
#   l6_csform_g1_cs <- paste(l6_csform_g1_cs,collapse = " ")
#   final_g1_cs[index_g1_cs] <- rbind(paste(g1_teams[index_g1_cs],l6_csform_g1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",g1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_g1_cs <- as.data.frame(final_g1_cs)
# colnames(final_g1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_g1_wm object
# final_g1_wm <- c()
# suml6_g1_wm <- c()
# for(index_g1_wm in 1:length(g1_teams))
# {
#   index_g1_wm <- row.names(g1_winmargin_h) == g1_teams[index_g1_wm]
#   form_g1_wm <- g1_winmargin_h[index_g1_wm]
#   deleted_form_g1_wm <- form_g1_wm[!form_g1_wm[] == ""]
#   l6_form_g1_wm <- tail(deleted_form_g1_wm,g1_last_n_games)
#   l6_form_g1_wm <- as.numeric(l6_form_g1_wm)
#   suml6_g1_wm[index_g1_wm] <- sum(l6_form_g1_wm)
#   suml6_g1_wm[index_g1_wm] <- paste("(",suml6_g1_wm[index_g1_wm],")",sep = "")
#   l6_form_g1_wm <- paste(l6_form_g1_wm,collapse = " ")
#   final_g1_wm[index_g1_wm] <- rbind(paste(g1_teams[index_g1_wm],l6_form_g1_wm,suml6_g1_wm[index_g1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# final_g1_wm
# #change column names
# final_g1_wm <- as.data.frame(final_g1_wm)
# colnames(final_g1_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_g1_ca object
# final_g1_ca <- c()
# suml6_g1_ca <- c()
# for(index_g1_ca in 1:length(g1_teams))
# {
#   index_g1_ca <- row.names(g1_coawarded_h) == g1_teams[index_g1_ca]
#   form_g1_ca <- g1_coawarded_h[index_g1_ca]
#   deleted_form_g1_ca <- form_g1_ca[!form_g1_ca[] == ""]
#   l6_form_g1_ca <- tail(deleted_form_g1_ca,g1_last_n_games)
#   l6_form_g1_ca <- as.numeric(l6_form_g1_ca)
#   suml6_g1_ca[index_g1_ca] <- sum(l6_form_g1_ca)
#   suml6_g1_ca[index_g1_ca] <- paste("(",suml6_g1_ca[index_g1_ca],")",sep = "")
#   l6_form_g1_ca <- paste(l6_form_g1_ca,collapse = " ")
#   final_g1_ca[index_g1_ca] <- rbind(paste(g1_teams[index_g1_ca],l6_form_g1_ca,suml6_g1_ca[index_g1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# final_g1_ca
# #change column names
# final_g1_ca <- as.data.frame(final_g1_ca)
# colnames(final_g1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_g1_ca object
# final_g1_cc <- c()
# suml6_g1_cc <- c()
# for(index_g1_cc in 1:length(g1_teams))
# {
#   index_g1_cc <- row.names(g1_cornersconceded_h) == g1_teams[index_g1_cc]
#   form_g1_cc <- g1_cornersconceded_h[index_g1_cc]
#   deleted_form_g1_cc <- form_g1_cc[!form_g1_cc[] == ""]
#   l6_form_g1_cc <- tail(deleted_form_g1_cc,g1_last_n_games)
#   l6_form_g1_cc <- as.numeric(l6_form_g1_cc)
#   suml6_g1_cc[index_g1_cc] <- sum(l6_form_g1_cc)
#   suml6_g1_cc[index_g1_cc] <- paste("(",suml6_g1_cc[index_g1_cc],")",sep = "")
#   l6_form_g1_cc <- paste(l6_form_g1_cc,collapse = " ")
#   final_g1_cc[index_g1_cc] <- rbind(paste(g1_teams[index_g1_cc],l6_form_g1_cc,suml6_g1_cc[index_g1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# final_g1_cc
# #change column names
# final_g1_cc <- as.data.frame(final_g1_cc)
# colnames(final_g1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_g1_cosc <- c()
# for(index_g1_cosc in 1:length(g1_teams))
# {
#   index_g1_cosc <- row.names(g1_coscform_h) == g1_teams[index_g1_cosc]
#   coscform_g1_cosc <- g1_coscform_h[index_g1_cosc]
#   deleted_coscform_g1_cosc <- coscform_g1_cosc[!coscform_g1_cosc[] == ""]
#   l6_coscform_g1_cosc <- tail(deleted_coscform_g1_cosc,g1_last_n_games)
#   l6_coscform_g1_cosc <- paste(l6_coscform_g1_cosc,collapse = " ")
#   final_g1_cosc[index_g1_cosc] <- rbind(paste(g1_teams[index_g1_cosc],l6_coscform_g1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",g1_teams[index],l6_coscform)
#
# }
# final_g1_cosc
# #change column names
# final_g1_cosc <- as.data.frame(final_g1_cosc)
# colnames(final_g1_cosc) <- "CornersForm"
# #################################################
# ##################################################
# #total corners
# #create final_g1_tcorners object
# final_g1_tcorners <- c()
# suml6_g1_tcorners <- c()
# for(index_g1_tcorners in 1:length(g1_teams))
# {
#   index_g1_tcorners <- row.names(g1_totalcorners_h) == g1_teams[index_g1_tcorners]
#   form_g1_tcorners <- g1_totalcorners_h[index_g1_tcorners]
#   deleted_form_g1_tcorners <- form_g1_tcorners[!form_g1_tcorners[] == ""]
#   l6_form_g1_tcorners <- tail(deleted_form_g1_tcorners,g1_last_n_games)
#   l6_form_g1_tcorners <- as.numeric(l6_form_g1_tcorners)
#   suml6_g1_tcorners[index_g1_tcorners] <- sum(l6_form_g1_tcorners)
#   suml6_g1_tcorners[index_g1_tcorners] <- paste("(",suml6_g1_tcorners[index_g1_tcorners],")",sep = "")
#   l6_form_g1_tcorners <- paste(l6_form_g1_tcorners,collapse = " ")
#   final_g1_tcorners[index_g1_tcorners] <- rbind(paste(g1_teams[index_g1_tcorners],l6_form_g1_tcorners,suml6_g1_tcorners[index_g1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# #change column names
# final_g1_tcorners <- as.data.frame(final_g1_tcorners)
# colnames(final_g1_tcorners) <- "TotalCorners"
# ###################################################
# ################################################
# #Team against
# #create final_g1_hf_against
# final_g1_hf_against <- c()
# for(index_g1_hf_against in 1:length(g1_teams))
# {
#   index_g1_hf_against <- row.names(g1_form_team_against_h) == g1_teams[index_g1_hf_against]
#   form_g1_hf_against <- g1_form_team_against_h[index_g1_hf_against]
#   deleted_form_g1_hf_against <- form_g1_hf_against[!form_g1_hf_against[] == ""]
#   l6_form_g1_hf_against <- tail(deleted_form_g1_hf_against,g1_last_n_games)
#   l6_form_g1_hf_against <- paste(l6_form_g1_hf_against,collapse = " ")
#   final_g1_hf_against[index_g1_hf_against] <- rbind(paste(g1_teams[index_g1_hf_against],l6_form_g1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",g1_teams[index],l6_form)
#
# }
# final_g1_hf_against <- as.data.frame(final_g1_hf_against)
# colnames(final_g1_hf_against) <- "Team against"
# #combine the columns
# final_g1_all <- cbind(final_g1_hf,final_g1_gs,final_g1_gc,final_g1_tg,final_g1_ca,final_g1_cc,final_g1_tcorners,final_g1_cosc,final_g1_hf_against)
# write.xlsx(final_g1_all,'Divisions/G1.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #I1
# #form
# #create final_i1_hf object
# final_i1_hf <- c()
# for(index_i1_hf in 1:length(i1_teams))
# {
#   index_i1_hf <- row.names(i1_form_h) == i1_teams[index_i1_hf]
#   form_i1_hf <- i1_form_h[index_i1_hf]
#   deleted_form_i1_hf <- form_i1_hf[!form_i1_hf[] == ""]
#   l6_form_i1_hf <- tail(deleted_form_i1_hf,i1_last_n_games)
#   l6_form_i1_hf <- paste(l6_form_i1_hf,collapse = " ")
#   final_i1_hf[index_i1_hf] <- rbind(paste(i1_teams[index_i1_hf],l6_form_i1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
#
# #change column names
# final_i1_hf <- as.data.frame(final_i1_hf)
# colnames(final_i1_hf) <- "Form"
# #goals scored
# #create final_i1_gs object
# final_i1_gs <- c()
# suml6_i1_gs <- c()
# for(index_i1_gs in 1:length(i1_teams))
# {
#   index_i1_gs <- row.names(i1_goalscored_h) == i1_teams[index_i1_gs]
#   form_i1_gs <- i1_goalscored_h[index_i1_gs]
#   deleted_form_i1_gs <- form_i1_gs[!form_i1_gs[] == ""]
#   l6_form_i1_gs <- tail(deleted_form_i1_gs,i1_last_n_games)
#   l6_form_i1_gs <- as.numeric(l6_form_i1_gs)
#   suml6_i1_gs[index_i1_gs] <- sum(l6_form_i1_gs)
#   suml6_i1_gs[index_i1_gs] <- paste("(",suml6_i1_gs[index_i1_gs],")",sep = "")
#   l6_form_i1_gs <- paste(l6_form_i1_gs,collapse = " ")
#   final_i1_gs[index_i1_gs] <- rbind(paste(i1_teams[index_i1_gs],l6_form_i1_gs,suml6_i1_gs[index_i1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# final_i1_gs
# #change column names
# final_i1_gs <- as.data.frame(final_i1_gs)
# colnames(final_i1_gs) <- "Goals scored"
# #goal conceded
# #create final_i1_gc object
# final_i1_gc <- c()
# suml6_i1_gc <- c()
# for(index_i1_gc in 1:length(i1_teams))
# {
#   index_i1_gc <- row.names(i1_goalconceded_h) == i1_teams[index_i1_gc]
#   form_i1_gc <- i1_goalconceded_h[index_i1_gc]
#   deleted_form_i1_gc <- form_i1_gc[!form_i1_gc[] == ""]
#   l6_form_i1_gc <- tail(deleted_form_i1_gc,i1_last_n_games)
#   l6_form_i1_gc <- as.numeric(l6_form_i1_gc)
#   suml6_i1_gc[index_i1_gc] <- sum(l6_form_i1_gc)
#   suml6_i1_gc[index_i1_gc] <- paste("(",suml6_i1_gc[index_i1_gc],")",sep = "")
#   l6_form_i1_gc <- paste(l6_form_i1_gc,collapse = " ")
#   final_i1_gc[index_i1_gc] <- rbind(paste(i1_teams[index_i1_gc],l6_form_i1_gc,suml6_i1_gc[index_i1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
#
# #change column names
# final_i1_gc <- as.data.frame(final_i1_gc)
# colnames(final_i1_gc) <- "Goals conceded"
# #total goals
# #create final_i1_tg object
# final_i1_tg <- c()
# suml6_i1_tg <- c()
# for(index_i1_tg in 1:length(i1_teams))
# {
#   index_i1_tg <- row.names(i1_totalgoals_h) == i1_teams[index_i1_tg]
#   form_i1_tg <- i1_totalgoals_h[index_i1_tg]
#   deleted_form_i1_tg <- form_i1_tg[!form_i1_tg[] == ""]
#   l6_form_i1_tg <- tail(deleted_form_i1_tg,i1_last_n_games)
#   l6_form_i1_tg <- as.numeric(l6_form_i1_tg)
#   suml6_i1_tg[index_i1_tg] <- sum(l6_form_i1_tg)
#   suml6_i1_tg[index_i1_tg] <- paste("(",suml6_i1_tg[index_i1_tg],")",sep = "")
#   l6_form_i1_tg <- paste(l6_form_i1_tg,collapse = " ")
#   final_i1_tg[index_i1_tg] <- rbind(paste(i1_teams[index_i1_tg],l6_form_i1_tg,suml6_i1_tg[index_i1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# #change column names
# final_i1_tg <- as.data.frame(final_i1_tg)
# colnames(final_i1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_i1_hf object
# final_i1_cs <- c()
# for(index_i1_cs in 1:length(i1_teams))
# {
#   index_i1_cs <- row.names(i1_csform_h) == i1_teams[index_i1_cs]
#   csform_i1_cs <- i1_csform_h[index_i1_cs]
#   deleted_csform_i1_cs <- csform_i1_cs[!csform_i1_cs[] == ""]
#   l6_csform_i1_cs <- tail(deleted_csform_i1_cs,i1_last_n_games)
#   l6_csform_i1_cs <- paste(l6_csform_i1_cs,collapse = " ")
#   final_i1_cs[index_i1_cs] <- rbind(paste(i1_teams[index_i1_cs],l6_csform_i1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",i1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_i1_cs <- as.data.frame(final_i1_cs)
# colnames(final_i1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_i1_wm object
# final_i1_wm <- c()
# suml6_i1_wm <- c()
# for(index_i1_wm in 1:length(i1_teams))
# {
#   index_i1_wm <- row.names(i1_winmargin_h) == i1_teams[index_i1_wm]
#   form_i1_wm <- i1_winmargin_h[index_i1_wm]
#   deleted_form_i1_wm <- form_i1_wm[!form_i1_wm[] == ""]
#   l6_form_i1_wm <- tail(deleted_form_i1_wm,i1_last_n_games)
#   l6_form_i1_wm <- as.numeric(l6_form_i1_wm)
#   suml6_i1_wm[index_i1_wm] <- sum(l6_form_i1_wm)
#   suml6_i1_wm[index_i1_wm] <- paste("(",suml6_i1_wm[index_i1_wm],")",sep = "")
#   l6_form_i1_wm <- paste(l6_form_i1_wm,collapse = " ")
#   final_i1_wm[index_i1_wm] <- rbind(paste(i1_teams[index_i1_wm],l6_form_i1_wm,suml6_i1_wm[index_i1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# final_i1_wm
# #change column names
# final_i1_wm <- as.data.frame(final_i1_wm)
# colnames(final_i1_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_i1_ca object
# final_i1_ca <- c()
# suml6_i1_ca <- c()
# for(index_i1_ca in 1:length(i1_teams))
# {
#   index_i1_ca <- row.names(i1_coawarded_h) == i1_teams[index_i1_ca]
#   form_i1_ca <- i1_coawarded_h[index_i1_ca]
#   deleted_form_i1_ca <- form_i1_ca[!form_i1_ca[] == ""]
#   l6_form_i1_ca <- tail(deleted_form_i1_ca,i1_last_n_games)
#   l6_form_i1_ca <- as.numeric(l6_form_i1_ca)
#   suml6_i1_ca[index_i1_ca] <- sum(l6_form_i1_ca)
#   suml6_i1_ca[index_i1_ca] <- paste("(",suml6_i1_ca[index_i1_ca],")",sep = "")
#   l6_form_i1_ca <- paste(l6_form_i1_ca,collapse = " ")
#   final_i1_ca[index_i1_ca] <- rbind(paste(i1_teams[index_i1_ca],l6_form_i1_ca,suml6_i1_ca[index_i1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# final_i1_ca
# #change column names
# final_i1_ca <- as.data.frame(final_i1_ca)
# colnames(final_i1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_i1_ca object
# final_i1_cc <- c()
# suml6_i1_cc <- c()
# for(index_i1_cc in 1:length(i1_teams))
# {
#   index_i1_cc <- row.names(i1_cornersconceded_h) == i1_teams[index_i1_cc]
#   form_i1_cc <- i1_cornersconceded_h[index_i1_cc]
#   deleted_form_i1_cc <- form_i1_cc[!form_i1_cc[] == ""]
#   l6_form_i1_cc <- tail(deleted_form_i1_cc,i1_last_n_games)
#   l6_form_i1_cc <- as.numeric(l6_form_i1_cc)
#   suml6_i1_cc[index_i1_cc] <- sum(l6_form_i1_cc)
#   suml6_i1_cc[index_i1_cc] <- paste("(",suml6_i1_cc[index_i1_cc],")",sep = "")
#   l6_form_i1_cc <- paste(l6_form_i1_cc,collapse = " ")
#   final_i1_cc[index_i1_cc] <- rbind(paste(i1_teams[index_i1_cc],l6_form_i1_cc,suml6_i1_cc[index_i1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# final_i1_cc
# #change column names
# final_i1_cc <- as.data.frame(final_i1_cc)
# colnames(final_i1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_i1_cosc <- c()
# for(index_i1_cosc in 1:length(i1_teams))
# {
#   index_i1_cosc <- row.names(i1_coscform_h) == i1_teams[index_i1_cosc]
#   coscform_i1_cosc <- i1_coscform_h[index_i1_cosc]
#   deleted_coscform_i1_cosc <- coscform_i1_cosc[!coscform_i1_cosc[] == ""]
#   l6_coscform_i1_cosc <- tail(deleted_coscform_i1_cosc,i1_last_n_games)
#   l6_coscform_i1_cosc <- paste(l6_coscform_i1_cosc,collapse = " ")
#   final_i1_cosc[index_i1_cosc] <- rbind(paste(i1_teams[index_i1_cosc],l6_coscform_i1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",i1_teams[index],l6_coscform)
#
# }
# final_i1_cosc
# #change column names
# final_i1_cosc <- as.data.frame(final_i1_cosc)
# colnames(final_i1_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_i1_tcorners object
# final_i1_tcorners <- c()
# suml6_i1_tcorners <- c()
# for(index_i1_tcorners in 1:length(i1_teams))
# {
#   index_i1_tcorners <- row.names(i1_totalcorners_h) == i1_teams[index_i1_tcorners]
#   form_i1_tcorners <- i1_totalcorners_h[index_i1_tcorners]
#   deleted_form_i1_tcorners <- form_i1_tcorners[!form_i1_tcorners[] == ""]
#   l6_form_i1_tcorners <- tail(deleted_form_i1_tcorners,i1_last_n_games)
#   l6_form_i1_tcorners <- as.numeric(l6_form_i1_tcorners)
#   suml6_i1_tcorners[index_i1_tcorners] <- sum(l6_form_i1_tcorners)
#   suml6_i1_tcorners[index_i1_tcorners] <- paste("(",suml6_i1_tcorners[index_i1_tcorners],")",sep = "")
#   l6_form_i1_tcorners <- paste(l6_form_i1_tcorners,collapse = " ")
#   final_i1_tcorners[index_i1_tcorners] <- rbind(paste(i1_teams[index_i1_tcorners],l6_form_i1_tcorners,suml6_i1_tcorners[index_i1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# #change column names
# final_i1_tcorners <- as.data.frame(final_i1_tcorners)
# colnames(final_i1_tcorners) <- "TotalCorners"
# ###################################################
# ###################################################
# #Team against
# #create final_i1_hf_against
# final_i1_hf_against <- c()
# for(index_i1_hf_against in 1:length(i1_teams))
# {
#   index_i1_hf_against <- row.names(i1_form_team_against_h) == i1_teams[index_i1_hf_against]
#   form_i1_hf_against <- i1_form_team_against_h[index_i1_hf_against]
#   deleted_form_i1_hf_against <- form_i1_hf_against[!form_i1_hf_against[] == ""]
#   l6_form_i1_hf_against <- tail(deleted_form_i1_hf_against,i1_last_n_games)
#   l6_form_i1_hf_against <- paste(l6_form_i1_hf_against,collapse = " ")
#   final_i1_hf_against[index_i1_hf_against] <- rbind(paste(i1_teams[index_i1_hf_against],l6_form_i1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i1_teams[index],l6_form)
#
# }
# final_i1_hf_against <- as.data.frame(final_i1_hf_against)
# colnames(final_i1_hf_against) <- "Team against"
# #combine the columns
# final_i1_all <- cbind(final_i1_hf,final_i1_gs,final_i1_gc,final_i1_tg,final_i1_ca,final_i1_cc,final_i1_tcorners,final_i1_cosc,final_i1_hf_against)
# write.xlsx(final_i1_all,'Divisions/I1.xlsx',sheetName = "L6", append = TRUE)
# ################################################################################
# #I2
# #form
# #create final_i2_hf object
# final_i2_hf <- c()
# for(index_i2_hf in 1:length(i2_teams))
# {
#   index_i2_hf <- row.names(i2_form_h) == i2_teams[index_i2_hf]
#   form_i2_hf <- i2_form_h[index_i2_hf]
#   deleted_form_i2_hf <- form_i2_hf[!form_i2_hf[] == ""]
#   l6_form_i2_hf <- tail(deleted_form_i2_hf,i2_last_n_games)
#   l6_form_i2_hf <- paste(l6_form_i2_hf,collapse = " ")
#   final_i2_hf[index_i2_hf] <- rbind(paste(i2_teams[index_i2_hf],l6_form_i2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
#
# #change column names
# final_i2_hf <- as.data.frame(final_i2_hf)
# colnames(final_i2_hf) <- "Form"
# #goals scored
# #create final_i2_gs object
# final_i2_gs <- c()
# suml6_i2_gs <- c()
# for(index_i2_gs in 1:length(i2_teams))
# {
#   index_i2_gs <- row.names(i2_goalscored_h) == i2_teams[index_i2_gs]
#   form_i2_gs <- i2_goalscored_h[index_i2_gs]
#   deleted_form_i2_gs <- form_i2_gs[!form_i2_gs[] == ""]
#   l6_form_i2_gs <- tail(deleted_form_i2_gs,i2_last_n_games)
#   l6_form_i2_gs <- as.numeric(l6_form_i2_gs)
#   suml6_i2_gs[index_i2_gs] <- sum(l6_form_i2_gs)
#   suml6_i2_gs[index_i2_gs] <- paste("(",suml6_i2_gs[index_i2_gs],")",sep = "")
#   l6_form_i2_gs <- paste(l6_form_i2_gs,collapse = " ")
#   final_i2_gs[index_i2_gs] <- rbind(paste(i2_teams[index_i2_gs],l6_form_i2_gs,suml6_i2_gs[index_i2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# final_i2_gs
# #change column names
# final_i2_gs <- as.data.frame(final_i2_gs)
# colnames(final_i2_gs) <- "Goals scored"
# #goal conceded
# #create final_i2_gc object
# final_i2_gc <- c()
# suml6_i2_gc <- c()
# for(index_i2_gc in 1:length(i2_teams))
# {
#   index_i2_gc <- row.names(i2_goalconceded_h) == i2_teams[index_i2_gc]
#   form_i2_gc <- i2_goalconceded_h[index_i2_gc]
#   deleted_form_i2_gc <- form_i2_gc[!form_i2_gc[] == ""]
#   l6_form_i2_gc <- tail(deleted_form_i2_gc,i2_last_n_games)
#   l6_form_i2_gc <- as.numeric(l6_form_i2_gc)
#   suml6_i2_gc[index_i2_gc] <- sum(l6_form_i2_gc)
#   suml6_i2_gc[index_i2_gc] <- paste("(",suml6_i2_gc[index_i2_gc],")",sep = "")
#   l6_form_i2_gc <- paste(l6_form_i2_gc,collapse = " ")
#   final_i2_gc[index_i2_gc] <- rbind(paste(i2_teams[index_i2_gc],l6_form_i2_gc,suml6_i2_gc[index_i2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
#
# #change column names
# final_i2_gc <- as.data.frame(final_i2_gc)
# colnames(final_i2_gc) <- "Goals conceded"
# #total goals
# #create final_i2_tg object
# final_i2_tg <- c()
# suml6_i2_tg <- c()
# for(index_i2_tg in 1:length(i2_teams))
# {
#   index_i2_tg <- row.names(i2_totalgoals_h) == i2_teams[index_i2_tg]
#   form_i2_tg <- i2_totalgoals_h[index_i2_tg]
#   deleted_form_i2_tg <- form_i2_tg[!form_i2_tg[] == ""]
#   l6_form_i2_tg <- tail(deleted_form_i2_tg,i2_last_n_games)
#   l6_form_i2_tg <- as.numeric(l6_form_i2_tg)
#   suml6_i2_tg[index_i2_tg] <- sum(l6_form_i2_tg)
#   suml6_i2_tg[index_i2_tg] <- paste("(",suml6_i2_tg[index_i2_tg],")",sep = "")
#   l6_form_i2_tg <- paste(l6_form_i2_tg,collapse = " ")
#   final_i2_tg[index_i2_tg] <- rbind(paste(i2_teams[index_i2_tg],l6_form_i2_tg,suml6_i2_tg[index_i2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# #change column names
# final_i2_tg <- as.data.frame(final_i2_tg)
# colnames(final_i2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_i2_hf object
# final_i2_cs <- c()
# for(index_i2_cs in 1:length(i2_teams))
# {
#   index_i2_cs <- row.names(i2_csform_h) == i2_teams[index_i2_cs]
#   csform_i2_cs <- i2_csform_h[index_i2_cs]
#   deleted_csform_i2_cs <- csform_i2_cs[!csform_i2_cs[] == ""]
#   l6_csform_i2_cs <- tail(deleted_csform_i2_cs,i2_last_n_games)
#   l6_csform_i2_cs <- paste(l6_csform_i2_cs,collapse = " ")
#   final_i2_cs[index_i2_cs] <- rbind(paste(i2_teams[index_i2_cs],l6_csform_i2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",i2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_i2_cs <- as.data.frame(final_i2_cs)
# colnames(final_i2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_i2_wm object
# final_i2_wm <- c()
# suml6_i2_wm <- c()
# for(index_i2_wm in 1:length(i2_teams))
# {
#   index_i2_wm <- row.names(i2_winmargin_h) == i2_teams[index_i2_wm]
#   form_i2_wm <- i2_winmargin_h[index_i2_wm]
#   deleted_form_i2_wm <- form_i2_wm[!form_i2_wm[] == ""]
#   l6_form_i2_wm <- tail(deleted_form_i2_wm,i2_last_n_games)
#   l6_form_i2_wm <- as.numeric(l6_form_i2_wm)
#   suml6_i2_wm[index_i2_wm] <- sum(l6_form_i2_wm)
#   suml6_i2_wm[index_i2_wm] <- paste("(",suml6_i2_wm[index_i2_wm],")",sep = "")
#   l6_form_i2_wm <- paste(l6_form_i2_wm,collapse = " ")
#   final_i2_wm[index_i2_wm] <- rbind(paste(i2_teams[index_i2_wm],l6_form_i2_wm,suml6_i2_wm[index_i2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# final_i2_wm
# #change column names
# final_i2_wm <- as.data.frame(final_i2_wm)
# colnames(final_i2_wm) <- "Win Margin"
# #################################################
#
# #################################################
# #corners awarded
# #create final_i2_ca object
# final_i2_ca <- c()
# suml6_i2_ca <- c()
# for(index_i2_ca in 1:length(i2_teams))
# {
#   index_i2_ca <- row.names(i2_coawarded_h) == i2_teams[index_i2_ca]
#   form_i2_ca <- i2_coawarded_h[index_i2_ca]
#   deleted_form_i2_ca <- form_i2_ca[!form_i2_ca[] == ""]
#   l6_form_i2_ca <- tail(deleted_form_i2_ca,i2_last_n_games)
#   l6_form_i2_ca <- as.numeric(l6_form_i2_ca)
#   suml6_i2_ca[index_i2_ca] <- sum(l6_form_i2_ca)
#   suml6_i2_ca[index_i2_ca] <- paste("(",suml6_i2_ca[index_i2_ca],")",sep = "")
#   l6_form_i2_ca <- paste(l6_form_i2_ca,collapse = " ")
#   final_i2_ca[index_i2_ca] <- rbind(paste(i2_teams[index_i2_ca],l6_form_i2_ca,suml6_i2_ca[index_i2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# final_i2_ca
# #change column names
# final_i2_ca <- as.data.frame(final_i2_ca)
# colnames(final_i2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_i2_ca object
# final_i2_cc <- c()
# suml6_i2_cc <- c()
# for(index_i2_cc in 1:length(i2_teams))
# {
#   index_i2_cc <- row.names(i2_cornersconceded_h) == i2_teams[index_i2_cc]
#   form_i2_cc <- i2_cornersconceded_h[index_i2_cc]
#   deleted_form_i2_cc <- form_i2_cc[!form_i2_cc[] == ""]
#   l6_form_i2_cc <- tail(deleted_form_i2_cc,i2_last_n_games)
#   l6_form_i2_cc <- as.numeric(l6_form_i2_cc)
#   suml6_i2_cc[index_i2_cc] <- sum(l6_form_i2_cc)
#   suml6_i2_cc[index_i2_cc] <- paste("(",suml6_i2_cc[index_i2_cc],")",sep = "")
#   l6_form_i2_cc <- paste(l6_form_i2_cc,collapse = " ")
#   final_i2_cc[index_i2_cc] <- rbind(paste(i2_teams[index_i2_cc],l6_form_i2_cc,suml6_i2_cc[index_i2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# final_i2_cc
# #change column names
# final_i2_cc <- as.data.frame(final_i2_cc)
# colnames(final_i2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_i2_cosc <- c()
# for(index_i2_cosc in 1:length(i2_teams))
# {
#   index_i2_cosc <- row.names(i2_coscform_h) == i2_teams[index_i2_cosc]
#   coscform_i2_cosc <- i2_coscform_h[index_i2_cosc]
#   deleted_coscform_i2_cosc <- coscform_i2_cosc[!coscform_i2_cosc[] == ""]
#   l6_coscform_i2_cosc <- tail(deleted_coscform_i2_cosc,i2_last_n_games)
#   l6_coscform_i2_cosc <- paste(l6_coscform_i2_cosc,collapse = " ")
#   final_i2_cosc[index_i2_cosc] <- rbind(paste(i2_teams[index_i2_cosc],l6_coscform_i2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",i2_teams[index],l6_coscform)
#
# }
# final_i2_cosc
# #change column names
# final_i2_cosc <- as.data.frame(final_i2_cosc)
# colnames(final_i2_cosc) <- "CornersForm"
# #################################################
# ##################################################
# #total corners
# #create final_i2_tcorners object
# final_i2_tcorners <- c()
# suml6_i2_tcorners <- c()
# for(index_i2_tcorners in 1:length(i2_teams))
# {
#   index_i2_tcorners <- row.names(i2_totalcorners_h) == i2_teams[index_i2_tcorners]
#   form_i2_tcorners <- i2_totalcorners_h[index_i2_tcorners]
#   deleted_form_i2_tcorners <- form_i2_tcorners[!form_i2_tcorners[] == ""]
#   l6_form_i2_tcorners <- tail(deleted_form_i2_tcorners,i2_last_n_games)
#   l6_form_i2_tcorners <- as.numeric(l6_form_i2_tcorners)
#   suml6_i2_tcorners[index_i2_tcorners] <- sum(l6_form_i2_tcorners)
#   suml6_i2_tcorners[index_i2_tcorners] <- paste("(",suml6_i2_tcorners[index_i2_tcorners],")",sep = "")
#   l6_form_i2_tcorners <- paste(l6_form_i2_tcorners,collapse = " ")
#   final_i2_tcorners[index_i2_tcorners] <- rbind(paste(i2_teams[index_i2_tcorners],l6_form_i2_tcorners,suml6_i2_tcorners[index_i2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# #change column names
# final_i2_tcorners <- as.data.frame(final_i2_tcorners)
# colnames(final_i2_tcorners) <- "TotalCorners"
# ###################################################
# ################################################
# #Team against
# #create final_i2_hf_against
# final_i2_hf_against <- c()
# for(index_i2_hf_against in 1:length(i2_teams))
# {
#   index_i2_hf_against <- row.names(i2_form_team_against_h) == i2_teams[index_i2_hf_against]
#   form_i2_hf_against <- i2_form_team_against_h[index_i2_hf_against]
#   deleted_form_i2_hf_against <- form_i2_hf_against[!form_i2_hf_against[] == ""]
#   l6_form_i2_hf_against <- tail(deleted_form_i2_hf_against,i2_last_n_games)
#   l6_form_i2_hf_against <- paste(l6_form_i2_hf_against,collapse = " ")
#   final_i2_hf_against[index_i2_hf_against] <- rbind(paste(i2_teams[index_i2_hf_against],l6_form_i2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",i2_teams[index],l6_form)
#
# }
# final_i2_hf_against <- as.data.frame(final_i2_hf_against)
# colnames(final_i2_hf_against) <- "Team against"
# #combine the columns
# final_i2_all <- cbind(final_i2_hf,final_i2_gs,final_i2_gc,final_i2_tg,final_i2_ca,final_i2_cc,final_i2_tcorners,final_i2_cosc,final_i2_hf_against)
# write.xlsx(final_i2_all,'Divisions/I2.xlsx',sheetName = "L6", append = TRUE)
# ###############################################################################
# #N1
# #form
# #create final_n1_hf object
# final_n1_hf <- c()
# for(index_n1_hf in 1:length(n1_teams))
# {
#   index_n1_hf <- row.names(n1_form_h) == n1_teams[index_n1_hf]
#   form_n1_hf <- n1_form_h[index_n1_hf]
#   deleted_form_n1_hf <- form_n1_hf[!form_n1_hf[] == ""]
#   l6_form_n1_hf <- tail(deleted_form_n1_hf,n1_last_n_games)
#   l6_form_n1_hf <- paste(l6_form_n1_hf,collapse = " ")
#   final_n1_hf[index_n1_hf] <- rbind(paste(n1_teams[index_n1_hf],l6_form_n1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
#
# #change column names
# final_n1_hf <- as.data.frame(final_n1_hf)
# colnames(final_n1_hf) <- "Form"
# #goals scored
# #create final_n1_gs object
# final_n1_gs <- c()
# suml6_n1_gs <- c()
# for(index_n1_gs in 1:length(n1_teams))
# {
#   index_n1_gs <- row.names(n1_goalscored_h) == n1_teams[index_n1_gs]
#   form_n1_gs <- n1_goalscored_h[index_n1_gs]
#   deleted_form_n1_gs <- form_n1_gs[!form_n1_gs[] == ""]
#   l6_form_n1_gs <- tail(deleted_form_n1_gs,n1_last_n_games)
#   l6_form_n1_gs <- as.numeric(l6_form_n1_gs)
#   suml6_n1_gs[index_n1_gs] <- sum(l6_form_n1_gs)
#   suml6_n1_gs[index_n1_gs] <- paste("(",suml6_n1_gs[index_n1_gs],")",sep = "")
#   l6_form_n1_gs <- paste(l6_form_n1_gs,collapse = " ")
#   final_n1_gs[index_n1_gs] <- rbind(paste(n1_teams[index_n1_gs],l6_form_n1_gs,suml6_n1_gs[index_n1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# final_n1_gs
# #change column names
# final_n1_gs <- as.data.frame(final_n1_gs)
# colnames(final_n1_gs) <- "Goals scored"
# #goal conceded
# #create final_n1_gc object
# final_n1_gc <- c()
# suml6_n1_gc <- c()
# for(index_n1_gc in 1:length(n1_teams))
# {
#   index_n1_gc <- row.names(n1_goalconceded_h) == n1_teams[index_n1_gc]
#   form_n1_gc <- n1_goalconceded_h[index_n1_gc]
#   deleted_form_n1_gc <- form_n1_gc[!form_n1_gc[] == ""]
#   l6_form_n1_gc <- tail(deleted_form_n1_gc,n1_last_n_games)
#   l6_form_n1_gc <- as.numeric(l6_form_n1_gc)
#   suml6_n1_gc[index_n1_gc] <- sum(l6_form_n1_gc)
#   suml6_n1_gc[index_n1_gc] <- paste("(",suml6_n1_gc[index_n1_gc],")",sep = "")
#   l6_form_n1_gc <- paste(l6_form_n1_gc,collapse = " ")
#   final_n1_gc[index_n1_gc] <- rbind(paste(n1_teams[index_n1_gc],l6_form_n1_gc,suml6_n1_gc[index_n1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
#
# #change column names
# final_n1_gc <- as.data.frame(final_n1_gc)
# colnames(final_n1_gc) <- "Goals conceded"
# #total goals
# #create final_n1_tg object
# final_n1_tg <- c()
# suml6_n1_tg <- c()
# for(index_n1_tg in 1:length(n1_teams))
# {
#   index_n1_tg <- row.names(n1_totalgoals_h) == n1_teams[index_n1_tg]
#   form_n1_tg <- n1_totalgoals_h[index_n1_tg]
#   deleted_form_n1_tg <- form_n1_tg[!form_n1_tg[] == ""]
#   l6_form_n1_tg <- tail(deleted_form_n1_tg,n1_last_n_games)
#   l6_form_n1_tg <- as.numeric(l6_form_n1_tg)
#   suml6_n1_tg[index_n1_tg] <- sum(l6_form_n1_tg)
#   suml6_n1_tg[index_n1_tg] <- paste("(",suml6_n1_tg[index_n1_tg],")",sep = "")
#   l6_form_n1_tg <- paste(l6_form_n1_tg,collapse = " ")
#   final_n1_tg[index_n1_tg] <- rbind(paste(n1_teams[index_n1_tg],l6_form_n1_tg,suml6_n1_tg[index_n1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# #change column names
# final_n1_tg <- as.data.frame(final_n1_tg)
# colnames(final_n1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_n1_hf object
# final_n1_cs <- c()
# for(index_n1_cs in 1:length(n1_teams))
# {
#   index_n1_cs <- row.names(n1_csform_h) == n1_teams[index_n1_cs]
#   csform_n1_cs <- n1_csform_h[index_n1_cs]
#   deleted_csform_n1_cs <- csform_n1_cs[!csform_n1_cs[] == ""]
#   l6_csform_n1_cs <- tail(deleted_csform_n1_cs,n1_last_n_games)
#   l6_csform_n1_cs <- paste(l6_csform_n1_cs,collapse = " ")
#   final_n1_cs[index_n1_cs] <- rbind(paste(n1_teams[index_n1_cs],l6_csform_n1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",n1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_n1_cs <- as.data.frame(final_n1_cs)
# colnames(final_n1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_n1_wm object
# final_n1_wm <- c()
# suml6_n1_wm <- c()
# for(index_n1_wm in 1:length(n1_teams))
# {
#   index_n1_wm <- row.names(n1_winmargin_h) == n1_teams[index_n1_wm]
#   form_n1_wm <- n1_winmargin_h[index_n1_wm]
#   deleted_form_n1_wm <- form_n1_wm[!form_n1_wm[] == ""]
#   l6_form_n1_wm <- tail(deleted_form_n1_wm,n1_last_n_games)
#   l6_form_n1_wm <- as.numeric(l6_form_n1_wm)
#   suml6_n1_wm[index_n1_wm] <- sum(l6_form_n1_wm)
#   suml6_n1_wm[index_n1_wm] <- paste("(",suml6_n1_wm[index_n1_wm],")",sep = "")
#   l6_form_n1_wm <- paste(l6_form_n1_wm,collapse = " ")
#   final_n1_wm[index_n1_wm] <- rbind(paste(n1_teams[index_n1_wm],l6_form_n1_wm,suml6_n1_wm[index_n1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# final_n1_wm
# #change column names
# final_n1_wm <- as.data.frame(final_n1_wm)
# colnames(final_n1_wm) <- "Win Margin"
# #################################################
# #corners awarded
# #create final_n1_ca object
# final_n1_ca <- c()
# suml6_n1_ca <- c()
# for(index_n1_ca in 1:length(n1_teams))
# {
#   index_n1_ca <- row.names(n1_coawarded_h) == n1_teams[index_n1_ca]
#   form_n1_ca <- n1_coawarded_h[index_n1_ca]
#   deleted_form_n1_ca <- form_n1_ca[!form_n1_ca[] == ""]
#   l6_form_n1_ca <- tail(deleted_form_n1_ca,n1_last_n_games)
#   l6_form_n1_ca <- as.numeric(l6_form_n1_ca)
#   suml6_n1_ca[index_n1_ca] <- sum(l6_form_n1_ca)
#   suml6_n1_ca[index_n1_ca] <- paste("(",suml6_n1_ca[index_n1_ca],")",sep = "")
#   l6_form_n1_ca <- paste(l6_form_n1_ca,collapse = " ")
#   final_n1_ca[index_n1_ca] <- rbind(paste(n1_teams[index_n1_ca],l6_form_n1_ca,suml6_n1_ca[index_n1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# final_n1_ca
# #change column names
# final_n1_ca <- as.data.frame(final_n1_ca)
# colnames(final_n1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_n1_ca object
# final_n1_cc <- c()
# suml6_n1_cc <- c()
# for(index_n1_cc in 1:length(n1_teams))
# {
#   index_n1_cc <- row.names(n1_cornersconceded_h) == n1_teams[index_n1_cc]
#   form_n1_cc <- n1_cornersconceded_h[index_n1_cc]
#   deleted_form_n1_cc <- form_n1_cc[!form_n1_cc[] == ""]
#   l6_form_n1_cc <- tail(deleted_form_n1_cc,n1_last_n_games)
#   l6_form_n1_cc <- as.numeric(l6_form_n1_cc)
#   suml6_n1_cc[index_n1_cc] <- sum(l6_form_n1_cc)
#   suml6_n1_cc[index_n1_cc] <- paste("(",suml6_n1_cc[index_n1_cc],")",sep = "")
#   l6_form_n1_cc <- paste(l6_form_n1_cc,collapse = " ")
#   final_n1_cc[index_n1_cc] <- rbind(paste(n1_teams[index_n1_cc],l6_form_n1_cc,suml6_n1_cc[index_n1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# final_n1_cc
# #change column names
# final_n1_cc <- as.data.frame(final_n1_cc)
# colnames(final_n1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_n1_cosc <- c()
# for(index_n1_cosc in 1:length(n1_teams))
# {
#   index_n1_cosc <- row.names(n1_coscform_h) == n1_teams[index_n1_cosc]
#   coscform_n1_cosc <- n1_coscform_h[index_n1_cosc]
#   deleted_coscform_n1_cosc <- coscform_n1_cosc[!coscform_n1_cosc[] == ""]
#   l6_coscform_n1_cosc <- tail(deleted_coscform_n1_cosc,n1_last_n_games)
#   l6_coscform_n1_cosc <- paste(l6_coscform_n1_cosc,collapse = " ")
#   final_n1_cosc[index_n1_cosc] <- rbind(paste(n1_teams[index_n1_cosc],l6_coscform_n1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",n1_teams[index],l6_coscform)
#
# }
# final_n1_cosc
# #change column names
# final_n1_cosc <- as.data.frame(final_n1_cosc)
# colnames(final_n1_cosc) <- "CornersForm"
# ################################################################
# ##################################################
# #total corners
# #create final_n1_tcorners object
# final_n1_tcorners <- c()
# suml6_n1_tcorners <- c()
# for(index_n1_tcorners in 1:length(n1_teams))
# {
#   index_n1_tcorners <- row.names(n1_totalcorners_h) == n1_teams[index_n1_tcorners]
#   form_n1_tcorners <- n1_totalcorners_h[index_n1_tcorners]
#   deleted_form_n1_tcorners <- form_n1_tcorners[!form_n1_tcorners[] == ""]
#   l6_form_n1_tcorners <- tail(deleted_form_n1_tcorners,n1_last_n_games)
#   l6_form_n1_tcorners <- as.numeric(l6_form_n1_tcorners)
#   suml6_n1_tcorners[index_n1_tcorners] <- sum(l6_form_n1_tcorners)
#   suml6_n1_tcorners[index_n1_tcorners] <- paste("(",suml6_n1_tcorners[index_n1_tcorners],")",sep = "")
#   l6_form_n1_tcorners <- paste(l6_form_n1_tcorners,collapse = " ")
#   final_n1_tcorners[index_n1_tcorners] <- rbind(paste(n1_teams[index_n1_tcorners],l6_form_n1_tcorners,suml6_n1_tcorners[index_n1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# #change column names
# final_n1_tcorners <- as.data.frame(final_n1_tcorners)
# colnames(final_n1_tcorners) <- "TotalCorners"
# ###################################################
# ################################################################
# #Team against
# #create final_n1_hf_against
# final_n1_hf_against <- c()
# for(index_n1_hf_against in 1:length(n1_teams))
# {
#   index_n1_hf_against <- row.names(n1_form_team_against_h) == n1_teams[index_n1_hf_against]
#   form_n1_hf_against <- n1_form_team_against_h[index_n1_hf_against]
#   deleted_form_n1_hf_against <- form_n1_hf_against[!form_n1_hf_against[] == ""]
#   l6_form_n1_hf_against <- tail(deleted_form_n1_hf_against,n1_last_n_games)
#   l6_form_n1_hf_against <- paste(l6_form_n1_hf_against,collapse = " ")
#   final_n1_hf_against[index_n1_hf_against] <- rbind(paste(n1_teams[index_n1_hf_against],l6_form_n1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",n1_teams[index],l6_form)
#
# }
# final_n1_hf_against <- as.data.frame(final_n1_hf_against)
# colnames(final_n1_hf_against) <- "Team against"
# #combine the columns
# final_n1_all <- cbind(final_n1_hf,final_n1_gs,final_n1_gc,final_n1_tg,final_n1_ca,final_n1_cc,final_n1_tcorners,final_n1_cosc,final_n1_hf_against)
# write.xlsx(final_n1_all,'Divisions/N1.xlsx',sheetName = "L6", append = TRUE)
# #################################################################################
# #P1
# #form
# #create final_p1_hf object
# final_p1_hf <- c()
# for(index_p1_hf in 1:length(p1_teams))
# {
#   index_p1_hf <- row.names(p1_form_h) == p1_teams[index_p1_hf]
#   form_p1_hf <- p1_form_h[index_p1_hf]
#   deleted_form_p1_hf <- form_p1_hf[!form_p1_hf[] == ""]
#   l6_form_p1_hf <- tail(deleted_form_p1_hf,p1_last_n_games)
#   l6_form_p1_hf <- paste(l6_form_p1_hf,collapse = " ")
#   final_p1_hf[index_p1_hf] <- rbind(paste(p1_teams[index_p1_hf],l6_form_p1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
#
# #change column names
# final_p1_hf <- as.data.frame(final_p1_hf)
# colnames(final_p1_hf) <- "Form"
# #goals scored
# #create final_p1_gs object
# final_p1_gs <- c()
# suml6_p1_gs <- c()
# for(index_p1_gs in 1:length(p1_teams))
# {
#   index_p1_gs <- row.names(p1_goalscored_h) == p1_teams[index_p1_gs]
#   form_p1_gs <- p1_goalscored_h[index_p1_gs]
#   deleted_form_p1_gs <- form_p1_gs[!form_p1_gs[] == ""]
#   l6_form_p1_gs <- tail(deleted_form_p1_gs,p1_last_n_games)
#   l6_form_p1_gs <- as.numeric(l6_form_p1_gs)
#   suml6_p1_gs[index_p1_gs] <- sum(l6_form_p1_gs)
#   suml6_p1_gs[index_p1_gs] <- paste("(",suml6_p1_gs[index_p1_gs],")",sep = "")
#   l6_form_p1_gs <- paste(l6_form_p1_gs,collapse = " ")
#   final_p1_gs[index_p1_gs] <- rbind(paste(p1_teams[index_p1_gs],l6_form_p1_gs,suml6_p1_gs[index_p1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# final_p1_gs
# #change column names
# final_p1_gs <- as.data.frame(final_p1_gs)
# colnames(final_p1_gs) <- "Goals scored"
# #goal conceded
# #create final_p1_gc object
# final_p1_gc <- c()
# suml6_p1_gc <- c()
# for(index_p1_gc in 1:length(p1_teams))
# {
#   index_p1_gc <- row.names(p1_goalconceded_h) == p1_teams[index_p1_gc]
#   form_p1_gc <- p1_goalconceded_h[index_p1_gc]
#   deleted_form_p1_gc <- form_p1_gc[!form_p1_gc[] == ""]
#   l6_form_p1_gc <- tail(deleted_form_p1_gc,p1_last_n_games)
#   l6_form_p1_gc <- as.numeric(l6_form_p1_gc)
#   suml6_p1_gc[index_p1_gc] <- sum(l6_form_p1_gc)
#   suml6_p1_gc[index_p1_gc] <- paste("(",suml6_p1_gc[index_p1_gc],")",sep = "")
#   l6_form_p1_gc <- paste(l6_form_p1_gc,collapse = " ")
#   final_p1_gc[index_p1_gc] <- rbind(paste(p1_teams[index_p1_gc],l6_form_p1_gc,suml6_p1_gc[index_p1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
#
# #change column names
# final_p1_gc <- as.data.frame(final_p1_gc)
# colnames(final_p1_gc) <- "Goals conceded"
# #total goals
# #create final_p1_tg object
# final_p1_tg <- c()
# suml6_p1_tg <- c()
# for(index_p1_tg in 1:length(p1_teams))
# {
#   index_p1_tg <- row.names(p1_totalgoals_h) == p1_teams[index_p1_tg]
#   form_p1_tg <- p1_totalgoals_h[index_p1_tg]
#   deleted_form_p1_tg <- form_p1_tg[!form_p1_tg[] == ""]
#   l6_form_p1_tg <- tail(deleted_form_p1_tg,p1_last_n_games)
#   l6_form_p1_tg <- as.numeric(l6_form_p1_tg)
#   suml6_p1_tg[index_p1_tg] <- sum(l6_form_p1_tg)
#   suml6_p1_tg[index_p1_tg] <- paste("(",suml6_p1_tg[index_p1_tg],")",sep = "")
#   l6_form_p1_tg <- paste(l6_form_p1_tg,collapse = " ")
#   final_p1_tg[index_p1_tg] <- rbind(paste(p1_teams[index_p1_tg],l6_form_p1_tg,suml6_p1_tg[index_p1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# #change column names
# final_p1_tg <- as.data.frame(final_p1_tg)
# colnames(final_p1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_p1_hf object
# final_p1_cs <- c()
# for(index_p1_cs in 1:length(p1_teams))
# {
#   index_p1_cs <- row.names(p1_csform_h) == p1_teams[index_p1_cs]
#   csform_p1_cs <- p1_csform_h[index_p1_cs]
#   deleted_csform_p1_cs <- csform_p1_cs[!csform_p1_cs[] == ""]
#   l6_csform_p1_cs <- tail(deleted_csform_p1_cs,p1_last_n_games)
#   l6_csform_p1_cs <- paste(l6_csform_p1_cs,collapse = " ")
#   final_p1_cs[index_p1_cs] <- rbind(paste(p1_teams[index_p1_cs],l6_csform_p1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",p1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_p1_cs <- as.data.frame(final_p1_cs)
# colnames(final_p1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_p1_wm object
# final_p1_wm <- c()
# suml6_p1_wm <- c()
# for(index_p1_wm in 1:length(p1_teams))
# {
#   index_p1_wm <- row.names(p1_winmargin_h) == p1_teams[index_p1_wm]
#   form_p1_wm <- p1_winmargin_h[index_p1_wm]
#   deleted_form_p1_wm <- form_p1_wm[!form_p1_wm[] == ""]
#   l6_form_p1_wm <- tail(deleted_form_p1_wm,p1_last_n_games)
#   l6_form_p1_wm <- as.numeric(l6_form_p1_wm)
#   suml6_p1_wm[index_p1_wm] <- sum(l6_form_p1_wm)
#   suml6_p1_wm[index_p1_wm] <- paste("(",suml6_p1_wm[index_p1_wm],")",sep = "")
#   l6_form_p1_wm <- paste(l6_form_p1_wm,collapse = " ")
#   final_p1_wm[index_p1_wm] <- rbind(paste(p1_teams[index_p1_wm],l6_form_p1_wm,suml6_p1_wm[index_p1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# final_p1_wm
# #change column names
# final_p1_wm <- as.data.frame(final_p1_wm)
# colnames(final_p1_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_p1_ca object
# final_p1_ca <- c()
# suml6_p1_ca <- c()
# for(index_p1_ca in 1:length(p1_teams))
# {
#   index_p1_ca <- row.names(p1_coawarded_h) == p1_teams[index_p1_ca]
#   form_p1_ca <- p1_coawarded_h[index_p1_ca]
#   deleted_form_p1_ca <- form_p1_ca[!form_p1_ca[] == ""]
#   l6_form_p1_ca <- tail(deleted_form_p1_ca,p1_last_n_games)
#   l6_form_p1_ca <- as.numeric(l6_form_p1_ca)
#   suml6_p1_ca[index_p1_ca] <- sum(l6_form_p1_ca)
#   suml6_p1_ca[index_p1_ca] <- paste("(",suml6_p1_ca[index_p1_ca],")",sep = "")
#   l6_form_p1_ca <- paste(l6_form_p1_ca,collapse = " ")
#   final_p1_ca[index_p1_ca] <- rbind(paste(p1_teams[index_p1_ca],l6_form_p1_ca,suml6_p1_ca[index_p1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# final_p1_ca
# #change column names
# final_p1_ca <- as.data.frame(final_p1_ca)
# colnames(final_p1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_p1_ca object
# final_p1_cc <- c()
# suml6_p1_cc <- c()
# for(index_p1_cc in 1:length(p1_teams))
# {
#   index_p1_cc <- row.names(p1_cornersconceded_h) == p1_teams[index_p1_cc]
#   form_p1_cc <- p1_cornersconceded_h[index_p1_cc]
#   deleted_form_p1_cc <- form_p1_cc[!form_p1_cc[] == ""]
#   l6_form_p1_cc <- tail(deleted_form_p1_cc,p1_last_n_games)
#   l6_form_p1_cc <- as.numeric(l6_form_p1_cc)
#   suml6_p1_cc[index_p1_cc] <- sum(l6_form_p1_cc)
#   suml6_p1_cc[index_p1_cc] <- paste("(",suml6_p1_cc[index_p1_cc],")",sep = "")
#   l6_form_p1_cc <- paste(l6_form_p1_cc,collapse = " ")
#   final_p1_cc[index_p1_cc] <- rbind(paste(p1_teams[index_p1_cc],l6_form_p1_cc,suml6_p1_cc[index_p1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# final_p1_cc
# #change column names
# final_p1_cc <- as.data.frame(final_p1_cc)
# colnames(final_p1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_p1_cosc <- c()
# for(index_p1_cosc in 1:length(p1_teams))
# {
#   index_p1_cosc <- row.names(p1_coscform_h) == p1_teams[index_p1_cosc]
#   coscform_p1_cosc <- p1_coscform_h[index_p1_cosc]
#   deleted_coscform_p1_cosc <- coscform_p1_cosc[!coscform_p1_cosc[] == ""]
#   l6_coscform_p1_cosc <- tail(deleted_coscform_p1_cosc,p1_last_n_games)
#   l6_coscform_p1_cosc <- paste(l6_coscform_p1_cosc,collapse = " ")
#   final_p1_cosc[index_p1_cosc] <- rbind(paste(p1_teams[index_p1_cosc],l6_coscform_p1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",p1_teams[index],l6_coscform)
#
# }
# final_p1_cosc
# #change column names
# final_p1_cosc <- as.data.frame(final_p1_cosc)
# colnames(final_p1_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_p1_tcorners object
# final_p1_tcorners <- c()
# suml6_p1_tcorners <- c()
# for(index_p1_tcorners in 1:length(p1_teams))
# {
#   index_p1_tcorners <- row.names(p1_totalcorners_h) == p1_teams[index_p1_tcorners]
#   form_p1_tcorners <- p1_totalcorners_h[index_p1_tcorners]
#   deleted_form_p1_tcorners <- form_p1_tcorners[!form_p1_tcorners[] == ""]
#   l6_form_p1_tcorners <- tail(deleted_form_p1_tcorners,p1_last_n_games)
#   l6_form_p1_tcorners <- as.numeric(l6_form_p1_tcorners)
#   suml6_p1_tcorners[index_p1_tcorners] <- sum(l6_form_p1_tcorners)
#   suml6_p1_tcorners[index_p1_tcorners] <- paste("(",suml6_p1_tcorners[index_p1_tcorners],")",sep = "")
#   l6_form_p1_tcorners <- paste(l6_form_p1_tcorners,collapse = " ")
#   final_p1_tcorners[index_p1_tcorners] <- rbind(paste(p1_teams[index_p1_tcorners],l6_form_p1_tcorners,suml6_p1_tcorners[index_p1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# #change column names
# final_p1_tcorners <- as.data.frame(final_p1_tcorners)
# colnames(final_p1_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_p1_hf_against
# final_p1_hf_against <- c()
# for(index_p1_hf_against in 1:length(p1_teams))
# {
#   index_p1_hf_against <- row.names(p1_form_team_against_h) == p1_teams[index_p1_hf_against]
#   form_p1_hf_against <- p1_form_team_against_h[index_p1_hf_against]
#   deleted_form_p1_hf_against <- form_p1_hf_against[!form_p1_hf_against[] == ""]
#   l6_form_p1_hf_against <- tail(deleted_form_p1_hf_against,p1_last_n_games)
#   l6_form_p1_hf_against <- paste(l6_form_p1_hf_against,collapse = " ")
#   final_p1_hf_against[index_p1_hf_against] <- rbind(paste(p1_teams[index_p1_hf_against],l6_form_p1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",p1_teams[index],l6_form)
#
# }
# final_p1_hf_against <- as.data.frame(final_p1_hf_against)
# colnames(final_p1_hf_against) <- "Team against"
# #combine the columns
# final_p1_all <- cbind(final_p1_hf,final_p1_gs,final_p1_gc,final_p1_tg,final_p1_ca,final_p1_cc,final_p1_tcorners,final_p1_cosc,final_p1_hf_against)
# write.xlsx(final_p1_all,'Divisions/P1.xlsx',sheetName = "L6", append = TRUE)
# ########################################################################################
# #SC0
# #form
# #create final_sc0_hf object
# final_sc0_hf <- c()
# for(index_sc0_hf in 1:length(sc0_teams))
# {
#   index_sc0_hf <- row.names(sc0_form_h) == sc0_teams[index_sc0_hf]
#   form_sc0_hf <- sc0_form_h[index_sc0_hf]
#   deleted_form_sc0_hf <- form_sc0_hf[!form_sc0_hf[] == ""]
#   l6_form_sc0_hf <- tail(deleted_form_sc0_hf,sc0_last_n_games)
#   l6_form_sc0_hf <- paste(l6_form_sc0_hf,collapse = " ")
#   final_sc0_hf[index_sc0_hf] <- rbind(paste(sc0_teams[index_sc0_hf],l6_form_sc0_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc0_hf <- as.data.frame(final_sc0_hf)
# colnames(final_sc0_hf) <- "Form"
# #goals scored
# #create final_sc0_gs object
# final_sc0_gs <- c()
# suml6_sc0_gs <- c()
# for(index_sc0_gs in 1:length(sc0_teams))
# {
#   index_sc0_gs <- row.names(sc0_goalscored_h) == sc0_teams[index_sc0_gs]
#   form_sc0_gs <- sc0_goalscored_h[index_sc0_gs]
#   deleted_form_sc0_gs <- form_sc0_gs[!form_sc0_gs[] == ""]
#   l6_form_sc0_gs <- tail(deleted_form_sc0_gs,sc0_last_n_games)
#   l6_form_sc0_gs <- as.numeric(l6_form_sc0_gs)
#   suml6_sc0_gs[index_sc0_gs] <- sum(l6_form_sc0_gs)
#   suml6_sc0_gs[index_sc0_gs] <- paste("(",suml6_sc0_gs[index_sc0_gs],")",sep = "")
#   l6_form_sc0_gs <- paste(l6_form_sc0_gs,collapse = " ")
#   final_sc0_gs[index_sc0_gs] <- rbind(paste(sc0_teams[index_sc0_gs],l6_form_sc0_gs,suml6_sc0_gs[index_sc0_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# final_sc0_gs
# #change column names
# final_sc0_gs <- as.data.frame(final_sc0_gs)
# colnames(final_sc0_gs) <- "Goals scored"
# #goal conceded
# #create final_sc0_gc object
# final_sc0_gc <- c()
# suml6_sc0_gc <- c()
# for(index_sc0_gc in 1:length(sc0_teams))
# {
#   index_sc0_gc <- row.names(sc0_goalconceded_h) == sc0_teams[index_sc0_gc]
#   form_sc0_gc <- sc0_goalconceded_h[index_sc0_gc]
#   deleted_form_sc0_gc <- form_sc0_gc[!form_sc0_gc[] == ""]
#   l6_form_sc0_gc <- tail(deleted_form_sc0_gc,sc0_last_n_games)
#   l6_form_sc0_gc <- as.numeric(l6_form_sc0_gc)
#   suml6_sc0_gc[index_sc0_gc] <- sum(l6_form_sc0_gc)
#   suml6_sc0_gc[index_sc0_gc] <- paste("(",suml6_sc0_gc[index_sc0_gc],")",sep = "")
#   l6_form_sc0_gc <- paste(l6_form_sc0_gc,collapse = " ")
#   final_sc0_gc[index_sc0_gc] <- rbind(paste(sc0_teams[index_sc0_gc],l6_form_sc0_gc,suml6_sc0_gc[index_sc0_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc0_gc <- as.data.frame(final_sc0_gc)
# colnames(final_sc0_gc) <- "Goals conceded"
# #total goals
# #create final_sc0_tg object
# final_sc0_tg <- c()
# suml6_sc0_tg <- c()
# for(index_sc0_tg in 1:length(sc0_teams))
# {
#   index_sc0_tg <- row.names(sc0_totalgoals_h) == sc0_teams[index_sc0_tg]
#   form_sc0_tg <- sc0_totalgoals_h[index_sc0_tg]
#   deleted_form_sc0_tg <- form_sc0_tg[!form_sc0_tg[] == ""]
#   l6_form_sc0_tg <- tail(deleted_form_sc0_tg,sc0_last_n_games)
#   l6_form_sc0_tg <- as.numeric(l6_form_sc0_tg)
#   suml6_sc0_tg[index_sc0_tg] <- sum(l6_form_sc0_tg)
#   suml6_sc0_tg[index_sc0_tg] <- paste("(",suml6_sc0_tg[index_sc0_tg],")",sep = "")
#   l6_form_sc0_tg <- paste(l6_form_sc0_tg,collapse = " ")
#   final_sc0_tg[index_sc0_tg] <- rbind(paste(sc0_teams[index_sc0_tg],l6_form_sc0_tg,suml6_sc0_tg[index_sc0_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# #change column names
# final_sc0_tg <- as.data.frame(final_sc0_tg)
# colnames(final_sc0_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sc0_hf object
# final_sc0_cs <- c()
# for(index_sc0_cs in 1:length(sc0_teams))
# {
#   index_sc0_cs <- row.names(sc0_csform_h) == sc0_teams[index_sc0_cs]
#   csform_sc0_cs <- sc0_csform_h[index_sc0_cs]
#   deleted_csform_sc0_cs <- csform_sc0_cs[!csform_sc0_cs[] == ""]
#   l6_csform_sc0_cs <- tail(deleted_csform_sc0_cs,sc0_last_n_games)
#   l6_csform_sc0_cs <- paste(l6_csform_sc0_cs,collapse = " ")
#   final_sc0_cs[index_sc0_cs] <- rbind(paste(sc0_teams[index_sc0_cs],l6_csform_sc0_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sc0_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sc0_cs <- as.data.frame(final_sc0_cs)
# colnames(final_sc0_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sc0_wm object
# final_sc0_wm <- c()
# suml6_sc0_wm <- c()
# for(index_sc0_wm in 1:length(sc0_teams))
# {
#   index_sc0_wm <- row.names(sc0_winmargin_h) == sc0_teams[index_sc0_wm]
#   form_sc0_wm <- sc0_winmargin_h[index_sc0_wm]
#   deleted_form_sc0_wm <- form_sc0_wm[!form_sc0_wm[] == ""]
#   l6_form_sc0_wm <- tail(deleted_form_sc0_wm,sc0_last_n_games)
#   l6_form_sc0_wm <- as.numeric(l6_form_sc0_wm)
#   suml6_sc0_wm[index_sc0_wm] <- sum(l6_form_sc0_wm)
#   suml6_sc0_wm[index_sc0_wm] <- paste("(",suml6_sc0_wm[index_sc0_wm],")",sep = "")
#   l6_form_sc0_wm <- paste(l6_form_sc0_wm,collapse = " ")
#   final_sc0_wm[index_sc0_wm] <- rbind(paste(sc0_teams[index_sc0_wm],l6_form_sc0_wm,suml6_sc0_wm[index_sc0_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# final_sc0_wm
# #change column names
# final_sc0_wm <- as.data.frame(final_sc0_wm)
# colnames(final_sc0_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sc0_ca object
# final_sc0_ca <- c()
# suml6_sc0_ca <- c()
# for(index_sc0_ca in 1:length(sc0_teams))
# {
#   index_sc0_ca <- row.names(sc0_coawarded_h) == sc0_teams[index_sc0_ca]
#   form_sc0_ca <- sc0_coawarded_h[index_sc0_ca]
#   deleted_form_sc0_ca <- form_sc0_ca[!form_sc0_ca[] == ""]
#   l6_form_sc0_ca <- tail(deleted_form_sc0_ca,sc0_last_n_games)
#   l6_form_sc0_ca <- as.numeric(l6_form_sc0_ca)
#   suml6_sc0_ca[index_sc0_ca] <- sum(l6_form_sc0_ca)
#   suml6_sc0_ca[index_sc0_ca] <- paste("(",suml6_sc0_ca[index_sc0_ca],")",sep = "")
#   l6_form_sc0_ca <- paste(l6_form_sc0_ca,collapse = " ")
#   final_sc0_ca[index_sc0_ca] <- rbind(paste(sc0_teams[index_sc0_ca],l6_form_sc0_ca,suml6_sc0_ca[index_sc0_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# final_sc0_ca
# #change column names
# final_sc0_ca <- as.data.frame(final_sc0_ca)
# colnames(final_sc0_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sc0_ca object
# final_sc0_cc <- c()
# suml6_sc0_cc <- c()
# for(index_sc0_cc in 1:length(sc0_teams))
# {
#   index_sc0_cc <- row.names(sc0_cornersconceded_h) == sc0_teams[index_sc0_cc]
#   form_sc0_cc <- sc0_cornersconceded_h[index_sc0_cc]
#   deleted_form_sc0_cc <- form_sc0_cc[!form_sc0_cc[] == ""]
#   l6_form_sc0_cc <- tail(deleted_form_sc0_cc,sc0_last_n_games)
#   l6_form_sc0_cc <- as.numeric(l6_form_sc0_cc)
#   suml6_sc0_cc[index_sc0_cc] <- sum(l6_form_sc0_cc)
#   suml6_sc0_cc[index_sc0_cc] <- paste("(",suml6_sc0_cc[index_sc0_cc],")",sep = "")
#   l6_form_sc0_cc <- paste(l6_form_sc0_cc,collapse = " ")
#   final_sc0_cc[index_sc0_cc] <- rbind(paste(sc0_teams[index_sc0_cc],l6_form_sc0_cc,suml6_sc0_cc[index_sc0_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# final_sc0_cc
# #change column names
# final_sc0_cc <- as.data.frame(final_sc0_cc)
# colnames(final_sc0_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sc0_cosc <- c()
# for(index_sc0_cosc in 1:length(sc0_teams))
# {
#   index_sc0_cosc <- row.names(sc0_coscform_h) == sc0_teams[index_sc0_cosc]
#   coscform_sc0_cosc <- sc0_coscform_h[index_sc0_cosc]
#   deleted_coscform_sc0_cosc <- coscform_sc0_cosc[!coscform_sc0_cosc[] == ""]
#   l6_coscform_sc0_cosc <- tail(deleted_coscform_sc0_cosc,sc0_last_n_games)
#   l6_coscform_sc0_cosc <- paste(l6_coscform_sc0_cosc,collapse = " ")
#   final_sc0_cosc[index_sc0_cosc] <- rbind(paste(sc0_teams[index_sc0_cosc],l6_coscform_sc0_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sc0_teams[index],l6_coscform)
#
# }
# final_sc0_cosc
# #change column names
# final_sc0_cosc <- as.data.frame(final_sc0_cosc)
# colnames(final_sc0_cosc) <- "CornersForm"
# #################################################
# ##################################################
# #total corners
# #create final_sc0_tcorners object
# final_sc0_tcorners <- c()
# suml6_sc0_tcorners <- c()
# for(index_sc0_tcorners in 1:length(sc0_teams))
# {
#   index_sc0_tcorners <- row.names(sc0_totalcorners_h) == sc0_teams[index_sc0_tcorners]
#   form_sc0_tcorners <- sc0_totalcorners_h[index_sc0_tcorners]
#   deleted_form_sc0_tcorners <- form_sc0_tcorners[!form_sc0_tcorners[] == ""]
#   l6_form_sc0_tcorners <- tail(deleted_form_sc0_tcorners,sc0_last_n_games)
#   l6_form_sc0_tcorners <- as.numeric(l6_form_sc0_tcorners)
#   suml6_sc0_tcorners[index_sc0_tcorners] <- sum(l6_form_sc0_tcorners)
#   suml6_sc0_tcorners[index_sc0_tcorners] <- paste("(",suml6_sc0_tcorners[index_sc0_tcorners],")",sep = "")
#   l6_form_sc0_tcorners <- paste(l6_form_sc0_tcorners,collapse = " ")
#   final_sc0_tcorners[index_sc0_tcorners] <- rbind(paste(sc0_teams[index_sc0_tcorners],l6_form_sc0_tcorners,suml6_sc0_tcorners[index_sc0_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# #change column names
# final_sc0_tcorners <- as.data.frame(final_sc0_tcorners)
# colnames(final_sc0_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_sc0_hf_against
# final_sc0_hf_against <- c()
# for(index_sc0_hf_against in 1:length(sc0_teams))
# {
#   index_sc0_hf_against <- row.names(sc0_form_team_against_h) == sc0_teams[index_sc0_hf_against]
#   form_sc0_hf_against <- sc0_form_team_against_h[index_sc0_hf_against]
#   deleted_form_sc0_hf_against <- form_sc0_hf_against[!form_sc0_hf_against[] == ""]
#   l6_form_sc0_hf_against <- tail(deleted_form_sc0_hf_against,sc0_last_n_games)
#   l6_form_sc0_hf_against <- paste(l6_form_sc0_hf_against,collapse = " ")
#   final_sc0_hf_against[index_sc0_hf_against] <- rbind(paste(sc0_teams[index_sc0_hf_against],l6_form_sc0_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc0_teams[index],l6_form)
#
# }
# final_sc0_hf_against <- as.data.frame(final_sc0_hf_against)
# colnames(final_sc0_hf_against) <- "Team against"
# #combine the columns
# final_sc0_all <- cbind(final_sc0_hf,final_sc0_gs,final_sc0_gc,final_sc0_tg,final_sc0_ca,final_sc0_cc,final_sc0_tcorners,final_sc0_cosc,final_sc0_hf_against)
# write.xlsx(final_sc0_all,'Divisions/SC0.xlsx',sheetName = "L6", append = TRUE)
# ######################################################################################
# #SC1
# #form
# #create final_sc1_hf object
# final_sc1_hf <- c()
# for(index_sc1_hf in 1:length(sc1_teams))
# {
#   index_sc1_hf <- row.names(sc1_form_h) == sc1_teams[index_sc1_hf]
#   form_sc1_hf <- sc1_form_h[index_sc1_hf]
#   deleted_form_sc1_hf <- form_sc1_hf[!form_sc1_hf[] == ""]
#   l6_form_sc1_hf <- tail(deleted_form_sc1_hf,sc1_last_n_games)
#   l6_form_sc1_hf <- paste(l6_form_sc1_hf,collapse = " ")
#   final_sc1_hf[index_sc1_hf] <- rbind(paste(sc1_teams[index_sc1_hf],l6_form_sc1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc1_hf <- as.data.frame(final_sc1_hf)
# colnames(final_sc1_hf) <- "Form"
# #goals scored
# #create final_sc1_gs object
# final_sc1_gs <- c()
# suml6_sc1_gs <- c()
# for(index_sc1_gs in 1:length(sc1_teams))
# {
#   index_sc1_gs <- row.names(sc1_goalscored_h) == sc1_teams[index_sc1_gs]
#   form_sc1_gs <- sc1_goalscored_h[index_sc1_gs]
#   deleted_form_sc1_gs <- form_sc1_gs[!form_sc1_gs[] == ""]
#   l6_form_sc1_gs <- tail(deleted_form_sc1_gs,sc1_last_n_games)
#   l6_form_sc1_gs <- as.numeric(l6_form_sc1_gs)
#   suml6_sc1_gs[index_sc1_gs] <- sum(l6_form_sc1_gs)
#   suml6_sc1_gs[index_sc1_gs] <- paste("(",suml6_sc1_gs[index_sc1_gs],")",sep = "")
#   l6_form_sc1_gs <- paste(l6_form_sc1_gs,collapse = " ")
#   final_sc1_gs[index_sc1_gs] <- rbind(paste(sc1_teams[index_sc1_gs],l6_form_sc1_gs,suml6_sc1_gs[index_sc1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# final_sc1_gs
# #change column names
# final_sc1_gs <- as.data.frame(final_sc1_gs)
# colnames(final_sc1_gs) <- "Goals scored"
# #goal conceded
# #create final_sc1_gc object
# final_sc1_gc <- c()
# suml6_sc1_gc <- c()
# for(index_sc1_gc in 1:length(sc1_teams))
# {
#   index_sc1_gc <- row.names(sc1_goalconceded_h) == sc1_teams[index_sc1_gc]
#   form_sc1_gc <- sc1_goalconceded_h[index_sc1_gc]
#   deleted_form_sc1_gc <- form_sc1_gc[!form_sc1_gc[] == ""]
#   l6_form_sc1_gc <- tail(deleted_form_sc1_gc,sc1_last_n_games)
#   l6_form_sc1_gc <- as.numeric(l6_form_sc1_gc)
#   suml6_sc1_gc[index_sc1_gc] <- sum(l6_form_sc1_gc)
#   suml6_sc1_gc[index_sc1_gc] <- paste("(",suml6_sc1_gc[index_sc1_gc],")",sep = "")
#   l6_form_sc1_gc <- paste(l6_form_sc1_gc,collapse = " ")
#   final_sc1_gc[index_sc1_gc] <- rbind(paste(sc1_teams[index_sc1_gc],l6_form_sc1_gc,suml6_sc1_gc[index_sc1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc1_gc <- as.data.frame(final_sc1_gc)
# colnames(final_sc1_gc) <- "Goals conceded"
# #total goals
# #create final_sc1_tg object
# final_sc1_tg <- c()
# suml6_sc1_tg <- c()
# for(index_sc1_tg in 1:length(sc1_teams))
# {
#   index_sc1_tg <- row.names(sc1_totalgoals_h) == sc1_teams[index_sc1_tg]
#   form_sc1_tg <- sc1_totalgoals_h[index_sc1_tg]
#   deleted_form_sc1_tg <- form_sc1_tg[!form_sc1_tg[] == ""]
#   l6_form_sc1_tg <- tail(deleted_form_sc1_tg,sc1_last_n_games)
#   l6_form_sc1_tg <- as.numeric(l6_form_sc1_tg)
#   suml6_sc1_tg[index_sc1_tg] <- sum(l6_form_sc1_tg)
#   suml6_sc1_tg[index_sc1_tg] <- paste("(",suml6_sc1_tg[index_sc1_tg],")",sep = "")
#   l6_form_sc1_tg <- paste(l6_form_sc1_tg,collapse = " ")
#   final_sc1_tg[index_sc1_tg] <- rbind(paste(sc1_teams[index_sc1_tg],l6_form_sc1_tg,suml6_sc1_tg[index_sc1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# #change column names
# final_sc1_tg <- as.data.frame(final_sc1_tg)
# colnames(final_sc1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sc1_hf object
# final_sc1_cs <- c()
# for(index_sc1_cs in 1:length(sc1_teams))
# {
#   index_sc1_cs <- row.names(sc1_csform_h) == sc1_teams[index_sc1_cs]
#   csform_sc1_cs <- sc1_csform_h[index_sc1_cs]
#   deleted_csform_sc1_cs <- csform_sc1_cs[!csform_sc1_cs[] == ""]
#   l6_csform_sc1_cs <- tail(deleted_csform_sc1_cs,sc1_last_n_games)
#   l6_csform_sc1_cs <- paste(l6_csform_sc1_cs,collapse = " ")
#   final_sc1_cs[index_sc1_cs] <- rbind(paste(sc1_teams[index_sc1_cs],l6_csform_sc1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sc1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sc1_cs <- as.data.frame(final_sc1_cs)
# colnames(final_sc1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sc1_wm object
# final_sc1_wm <- c()
# suml6_sc1_wm <- c()
# for(index_sc1_wm in 1:length(sc1_teams))
# {
#   index_sc1_wm <- row.names(sc1_winmargin_h) == sc1_teams[index_sc1_wm]
#   form_sc1_wm <- sc1_winmargin_h[index_sc1_wm]
#   deleted_form_sc1_wm <- form_sc1_wm[!form_sc1_wm[] == ""]
#   l6_form_sc1_wm <- tail(deleted_form_sc1_wm,sc1_last_n_games)
#   l6_form_sc1_wm <- as.numeric(l6_form_sc1_wm)
#   suml6_sc1_wm[index_sc1_wm] <- sum(l6_form_sc1_wm)
#   suml6_sc1_wm[index_sc1_wm] <- paste("(",suml6_sc1_wm[index_sc1_wm],")",sep = "")
#   l6_form_sc1_wm <- paste(l6_form_sc1_wm,collapse = " ")
#   final_sc1_wm[index_sc1_wm] <- rbind(paste(sc1_teams[index_sc1_wm],l6_form_sc1_wm,suml6_sc1_wm[index_sc1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# final_sc1_wm
# #change column names
# final_sc1_wm <- as.data.frame(final_sc1_wm)
# colnames(final_sc1_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sc1_ca object
# final_sc1_ca <- c()
# suml6_sc1_ca <- c()
# for(index_sc1_ca in 1:length(sc1_teams))
# {
#   index_sc1_ca <- row.names(sc1_coawarded_h) == sc1_teams[index_sc1_ca]
#   form_sc1_ca <- sc1_coawarded_h[index_sc1_ca]
#   deleted_form_sc1_ca <- form_sc1_ca[!form_sc1_ca[] == ""]
#   l6_form_sc1_ca <- tail(deleted_form_sc1_ca,sc1_last_n_games)
#   l6_form_sc1_ca <- as.numeric(l6_form_sc1_ca)
#   suml6_sc1_ca[index_sc1_ca] <- sum(l6_form_sc1_ca)
#   suml6_sc1_ca[index_sc1_ca] <- paste("(",suml6_sc1_ca[index_sc1_ca],")",sep = "")
#   l6_form_sc1_ca <- paste(l6_form_sc1_ca,collapse = " ")
#   final_sc1_ca[index_sc1_ca] <- rbind(paste(sc1_teams[index_sc1_ca],l6_form_sc1_ca,suml6_sc1_ca[index_sc1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# final_sc1_ca
# #change column names
# final_sc1_ca <- as.data.frame(final_sc1_ca)
# colnames(final_sc1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sc1_ca object
# final_sc1_cc <- c()
# suml6_sc1_cc <- c()
# for(index_sc1_cc in 1:length(sc1_teams))
# {
#   index_sc1_cc <- row.names(sc1_cornersconceded_h) == sc1_teams[index_sc1_cc]
#   form_sc1_cc <- sc1_cornersconceded_h[index_sc1_cc]
#   deleted_form_sc1_cc <- form_sc1_cc[!form_sc1_cc[] == ""]
#   l6_form_sc1_cc <- tail(deleted_form_sc1_cc,sc1_last_n_games)
#   l6_form_sc1_cc <- as.numeric(l6_form_sc1_cc)
#   suml6_sc1_cc[index_sc1_cc] <- sum(l6_form_sc1_cc)
#   suml6_sc1_cc[index_sc1_cc] <- paste("(",suml6_sc1_cc[index_sc1_cc],")",sep = "")
#   l6_form_sc1_cc <- paste(l6_form_sc1_cc,collapse = " ")
#   final_sc1_cc[index_sc1_cc] <- rbind(paste(sc1_teams[index_sc1_cc],l6_form_sc1_cc,suml6_sc1_cc[index_sc1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# final_sc1_cc
# #change column names
# final_sc1_cc <- as.data.frame(final_sc1_cc)
# colnames(final_sc1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sc1_cosc <- c()
# for(index_sc1_cosc in 1:length(sc1_teams))
# {
#   index_sc1_cosc <- row.names(sc1_coscform_h) == sc1_teams[index_sc1_cosc]
#   coscform_sc1_cosc <- sc1_coscform_h[index_sc1_cosc]
#   deleted_coscform_sc1_cosc <- coscform_sc1_cosc[!coscform_sc1_cosc[] == ""]
#   l6_coscform_sc1_cosc <- tail(deleted_coscform_sc1_cosc,sc1_last_n_games)
#   l6_coscform_sc1_cosc <- paste(l6_coscform_sc1_cosc,collapse = " ")
#   final_sc1_cosc[index_sc1_cosc] <- rbind(paste(sc1_teams[index_sc1_cosc],l6_coscform_sc1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sc1_teams[index],l6_coscform)
#
# }
# final_sc1_cosc
# #change column names
# final_sc1_cosc <- as.data.frame(final_sc1_cosc)
# colnames(final_sc1_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_sc1_tcorners object
# final_sc1_tcorners <- c()
# suml6_sc1_tcorners <- c()
# for(index_sc1_tcorners in 1:length(sc1_teams))
# {
#   index_sc1_tcorners <- row.names(sc1_totalcorners_h) == sc1_teams[index_sc1_tcorners]
#   form_sc1_tcorners <- sc1_totalcorners_h[index_sc1_tcorners]
#   deleted_form_sc1_tcorners <- form_sc1_tcorners[!form_sc1_tcorners[] == ""]
#   l6_form_sc1_tcorners <- tail(deleted_form_sc1_tcorners,sc1_last_n_games)
#   l6_form_sc1_tcorners <- as.numeric(l6_form_sc1_tcorners)
#   suml6_sc1_tcorners[index_sc1_tcorners] <- sum(l6_form_sc1_tcorners)
#   suml6_sc1_tcorners[index_sc1_tcorners] <- paste("(",suml6_sc1_tcorners[index_sc1_tcorners],")",sep = "")
#   l6_form_sc1_tcorners <- paste(l6_form_sc1_tcorners,collapse = " ")
#   final_sc1_tcorners[index_sc1_tcorners] <- rbind(paste(sc1_teams[index_sc1_tcorners],l6_form_sc1_tcorners,suml6_sc1_tcorners[index_sc1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# #change column names
# final_sc1_tcorners <- as.data.frame(final_sc1_tcorners)
# colnames(final_sc1_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_sc1_hf_against
# final_sc1_hf_against <- c()
# for(index_sc1_hf_against in 1:length(sc1_teams))
# {
#   index_sc1_hf_against <- row.names(sc1_form_team_against_h) == sc1_teams[index_sc1_hf_against]
#   form_sc1_hf_against <- sc1_form_team_against_h[index_sc1_hf_against]
#   deleted_form_sc1_hf_against <- form_sc1_hf_against[!form_sc1_hf_against[] == ""]
#   l6_form_sc1_hf_against <- tail(deleted_form_sc1_hf_against,sc1_last_n_games)
#   l6_form_sc1_hf_against <- paste(l6_form_sc1_hf_against,collapse = " ")
#   final_sc1_hf_against[index_sc1_hf_against] <- rbind(paste(sc1_teams[index_sc1_hf_against],l6_form_sc1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc1_teams[index],l6_form)
#
# }
# final_sc1_hf_against <- as.data.frame(final_sc1_hf_against)
# colnames(final_sc1_hf_against) <- "Team against"
# #combine the columns
# final_sc1_all <- cbind(final_sc1_hf,final_sc1_gs,final_sc1_gc,final_sc1_tg,final_sc1_ca,final_sc1_cc,final_sc1_tcorners,final_sc1_cosc,final_sc1_hf_against)
# write.xlsx(final_sc1_all,'Divisions/SC1.xlsx',sheetName = "L6", append = TRUE)
# ###################################################################################
# #SC2
# #form
# #create final_sc2_hf object
# final_sc2_hf <- c()
# for(index_sc2_hf in 1:length(sc2_teams))
# {
#   index_sc2_hf <- row.names(sc2_form_h) == sc2_teams[index_sc2_hf]
#   form_sc2_hf <- sc2_form_h[index_sc2_hf]
#   deleted_form_sc2_hf <- form_sc2_hf[!form_sc2_hf[] == ""]
#   l6_form_sc2_hf <- tail(deleted_form_sc2_hf,sc2_last_n_games)
#   l6_form_sc2_hf <- paste(l6_form_sc2_hf,collapse = " ")
#   final_sc2_hf[index_sc2_hf] <- rbind(paste(sc2_teams[index_sc2_hf],l6_form_sc2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc2_hf <- as.data.frame(final_sc2_hf)
# colnames(final_sc2_hf) <- "Form"
# #goals scored
# #create final_sc2_gs object
# final_sc2_gs <- c()
# suml6_sc2_gs <- c()
# for(index_sc2_gs in 1:length(sc2_teams))
# {
#   index_sc2_gs <- row.names(sc2_goalscored_h) == sc2_teams[index_sc2_gs]
#   form_sc2_gs <- sc2_goalscored_h[index_sc2_gs]
#   deleted_form_sc2_gs <- form_sc2_gs[!form_sc2_gs[] == ""]
#   l6_form_sc2_gs <- tail(deleted_form_sc2_gs,sc2_last_n_games)
#   l6_form_sc2_gs <- as.numeric(l6_form_sc2_gs)
#   suml6_sc2_gs[index_sc2_gs] <- sum(l6_form_sc2_gs)
#   suml6_sc2_gs[index_sc2_gs] <- paste("(",suml6_sc2_gs[index_sc2_gs],")",sep = "")
#   l6_form_sc2_gs <- paste(l6_form_sc2_gs,collapse = " ")
#   final_sc2_gs[index_sc2_gs] <- rbind(paste(sc2_teams[index_sc2_gs],l6_form_sc2_gs,suml6_sc2_gs[index_sc2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# final_sc2_gs
# #change column names
# final_sc2_gs <- as.data.frame(final_sc2_gs)
# colnames(final_sc2_gs) <- "Goals scored"
# #goal conceded
# #create final_sc2_gc object
# final_sc2_gc <- c()
# suml6_sc2_gc <- c()
# for(index_sc2_gc in 1:length(sc2_teams))
# {
#   index_sc2_gc <- row.names(sc2_goalconceded_h) == sc2_teams[index_sc2_gc]
#   form_sc2_gc <- sc2_goalconceded_h[index_sc2_gc]
#   deleted_form_sc2_gc <- form_sc2_gc[!form_sc2_gc[] == ""]
#   l6_form_sc2_gc <- tail(deleted_form_sc2_gc,sc2_last_n_games)
#   l6_form_sc2_gc <- as.numeric(l6_form_sc2_gc)
#   suml6_sc2_gc[index_sc2_gc] <- sum(l6_form_sc2_gc)
#   suml6_sc2_gc[index_sc2_gc] <- paste("(",suml6_sc2_gc[index_sc2_gc],")",sep = "")
#   l6_form_sc2_gc <- paste(l6_form_sc2_gc,collapse = " ")
#   final_sc2_gc[index_sc2_gc] <- rbind(paste(sc2_teams[index_sc2_gc],l6_form_sc2_gc,suml6_sc2_gc[index_sc2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc2_gc <- as.data.frame(final_sc2_gc)
# colnames(final_sc2_gc) <- "Goals conceded"
# #total goals
# #create final_sc2_tg object
# final_sc2_tg <- c()
# suml6_sc2_tg <- c()
# for(index_sc2_tg in 1:length(sc2_teams))
# {
#   index_sc2_tg <- row.names(sc2_totalgoals_h) == sc2_teams[index_sc2_tg]
#   form_sc2_tg <- sc2_totalgoals_h[index_sc2_tg]
#   deleted_form_sc2_tg <- form_sc2_tg[!form_sc2_tg[] == ""]
#   l6_form_sc2_tg <- tail(deleted_form_sc2_tg,sc2_last_n_games)
#   l6_form_sc2_tg <- as.numeric(l6_form_sc2_tg)
#   suml6_sc2_tg[index_sc2_tg] <- sum(l6_form_sc2_tg)
#   suml6_sc2_tg[index_sc2_tg] <- paste("(",suml6_sc2_tg[index_sc2_tg],")",sep = "")
#   l6_form_sc2_tg <- paste(l6_form_sc2_tg,collapse = " ")
#   final_sc2_tg[index_sc2_tg] <- rbind(paste(sc2_teams[index_sc2_tg],l6_form_sc2_tg,suml6_sc2_tg[index_sc2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# #change column names
# final_sc2_tg <- as.data.frame(final_sc2_tg)
# colnames(final_sc2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sc2_hf object
# final_sc2_cs <- c()
# for(index_sc2_cs in 1:length(sc2_teams))
# {
#   index_sc2_cs <- row.names(sc2_csform_h) == sc2_teams[index_sc2_cs]
#   csform_sc2_cs <- sc2_csform_h[index_sc2_cs]
#   deleted_csform_sc2_cs <- csform_sc2_cs[!csform_sc2_cs[] == ""]
#   l6_csform_sc2_cs <- tail(deleted_csform_sc2_cs,sc2_last_n_games)
#   l6_csform_sc2_cs <- paste(l6_csform_sc2_cs,collapse = " ")
#   final_sc2_cs[index_sc2_cs] <- rbind(paste(sc2_teams[index_sc2_cs],l6_csform_sc2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sc2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sc2_cs <- as.data.frame(final_sc2_cs)
# colnames(final_sc2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sc2_wm object
# final_sc2_wm <- c()
# suml6_sc2_wm <- c()
# for(index_sc2_wm in 1:length(sc2_teams))
# {
#   index_sc2_wm <- row.names(sc2_winmargin_h) == sc2_teams[index_sc2_wm]
#   form_sc2_wm <- sc2_winmargin_h[index_sc2_wm]
#   deleted_form_sc2_wm <- form_sc2_wm[!form_sc2_wm[] == ""]
#   l6_form_sc2_wm <- tail(deleted_form_sc2_wm,sc2_last_n_games)
#   l6_form_sc2_wm <- as.numeric(l6_form_sc2_wm)
#   suml6_sc2_wm[index_sc2_wm] <- sum(l6_form_sc2_wm)
#   suml6_sc2_wm[index_sc2_wm] <- paste("(",suml6_sc2_wm[index_sc2_wm],")",sep = "")
#   l6_form_sc2_wm <- paste(l6_form_sc2_wm,collapse = " ")
#   final_sc2_wm[index_sc2_wm] <- rbind(paste(sc2_teams[index_sc2_wm],l6_form_sc2_wm,suml6_sc2_wm[index_sc2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# final_sc2_wm
# #change column names
# final_sc2_wm <- as.data.frame(final_sc2_wm)
# colnames(final_sc2_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sc2_ca object
# final_sc2_ca <- c()
# suml6_sc2_ca <- c()
# for(index_sc2_ca in 1:length(sc2_teams))
# {
#   index_sc2_ca <- row.names(sc2_coawarded_h) == sc2_teams[index_sc2_ca]
#   form_sc2_ca <- sc2_coawarded_h[index_sc2_ca]
#   deleted_form_sc2_ca <- form_sc2_ca[!form_sc2_ca[] == ""]
#   l6_form_sc2_ca <- tail(deleted_form_sc2_ca,sc2_last_n_games)
#   l6_form_sc2_ca <- as.numeric(l6_form_sc2_ca)
#   suml6_sc2_ca[index_sc2_ca] <- sum(l6_form_sc2_ca)
#   suml6_sc2_ca[index_sc2_ca] <- paste("(",suml6_sc2_ca[index_sc2_ca],")",sep = "")
#   l6_form_sc2_ca <- paste(l6_form_sc2_ca,collapse = " ")
#   final_sc2_ca[index_sc2_ca] <- rbind(paste(sc2_teams[index_sc2_ca],l6_form_sc2_ca,suml6_sc2_ca[index_sc2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# final_sc2_ca
# #change column names
# final_sc2_ca <- as.data.frame(final_sc2_ca)
# colnames(final_sc2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sc2_ca object
# final_sc2_cc <- c()
# suml6_sc2_cc <- c()
# for(index_sc2_cc in 1:length(sc2_teams))
# {
#   index_sc2_cc <- row.names(sc2_cornersconceded_h) == sc2_teams[index_sc2_cc]
#   form_sc2_cc <- sc2_cornersconceded_h[index_sc2_cc]
#   deleted_form_sc2_cc <- form_sc2_cc[!form_sc2_cc[] == ""]
#   l6_form_sc2_cc <- tail(deleted_form_sc2_cc,sc2_last_n_games)
#   l6_form_sc2_cc <- as.numeric(l6_form_sc2_cc)
#   suml6_sc2_cc[index_sc2_cc] <- sum(l6_form_sc2_cc)
#   suml6_sc2_cc[index_sc2_cc] <- paste("(",suml6_sc2_cc[index_sc2_cc],")",sep = "")
#   l6_form_sc2_cc <- paste(l6_form_sc2_cc,collapse = " ")
#   final_sc2_cc[index_sc2_cc] <- rbind(paste(sc2_teams[index_sc2_cc],l6_form_sc2_cc,suml6_sc2_cc[index_sc2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# final_sc2_cc
# #change column names
# final_sc2_cc <- as.data.frame(final_sc2_cc)
# colnames(final_sc2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sc2_cosc <- c()
# for(index_sc2_cosc in 1:length(sc2_teams))
# {
#   index_sc2_cosc <- row.names(sc2_coscform_h) == sc2_teams[index_sc2_cosc]
#   coscform_sc2_cosc <- sc2_coscform_h[index_sc2_cosc]
#   deleted_coscform_sc2_cosc <- coscform_sc2_cosc[!coscform_sc2_cosc[] == ""]
#   l6_coscform_sc2_cosc <- tail(deleted_coscform_sc2_cosc,sc2_last_n_games)
#   l6_coscform_sc2_cosc <- paste(l6_coscform_sc2_cosc,collapse = " ")
#   final_sc2_cosc[index_sc2_cosc] <- rbind(paste(sc2_teams[index_sc2_cosc],l6_coscform_sc2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sc2_teams[index],l6_coscform)
#
# }
# final_sc2_cosc
# #change column names
# final_sc2_cosc <- as.data.frame(final_sc2_cosc)
# colnames(final_sc2_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_sc2_tcorners object
# final_sc2_tcorners <- c()
# suml6_sc2_tcorners <- c()
# for(index_sc2_tcorners in 1:length(sc2_teams))
# {
#   index_sc2_tcorners <- row.names(sc2_totalcorners_h) == sc2_teams[index_sc2_tcorners]
#   form_sc2_tcorners <- sc2_totalcorners_h[index_sc2_tcorners]
#   deleted_form_sc2_tcorners <- form_sc2_tcorners[!form_sc2_tcorners[] == ""]
#   l6_form_sc2_tcorners <- tail(deleted_form_sc2_tcorners,sc2_last_n_games)
#   l6_form_sc2_tcorners <- as.numeric(l6_form_sc2_tcorners)
#   suml6_sc2_tcorners[index_sc2_tcorners] <- sum(l6_form_sc2_tcorners)
#   suml6_sc2_tcorners[index_sc2_tcorners] <- paste("(",suml6_sc2_tcorners[index_sc2_tcorners],")",sep = "")
#   l6_form_sc2_tcorners <- paste(l6_form_sc2_tcorners,collapse = " ")
#   final_sc2_tcorners[index_sc2_tcorners] <- rbind(paste(sc2_teams[index_sc2_tcorners],l6_form_sc2_tcorners,suml6_sc2_tcorners[index_sc2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# #change column names
# final_sc2_tcorners <- as.data.frame(final_sc2_tcorners)
# colnames(final_sc2_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_sc2_hf_against
# final_sc2_hf_against <- c()
# for(index_sc2_hf_against in 1:length(sc2_teams))
# {
#   index_sc2_hf_against <- row.names(sc2_form_team_against_h) == sc2_teams[index_sc2_hf_against]
#   form_sc2_hf_against <- sc2_form_team_against_h[index_sc2_hf_against]
#   deleted_form_sc2_hf_against <- form_sc2_hf_against[!form_sc2_hf_against[] == ""]
#   l6_form_sc2_hf_against <- tail(deleted_form_sc2_hf_against,sc2_last_n_games)
#   l6_form_sc2_hf_against <- paste(l6_form_sc2_hf_against,collapse = " ")
#   final_sc2_hf_against[index_sc2_hf_against] <- rbind(paste(sc2_teams[index_sc2_hf_against],l6_form_sc2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc2_teams[index],l6_form)
#
# }
# final_sc2_hf_against <- as.data.frame(final_sc2_hf_against)
# colnames(final_sc2_hf_against) <- "Team against"
# #combine the columns
# final_sc2_all <- cbind(final_sc2_hf,final_sc2_gs,final_sc2_gc,final_sc2_tg,final_sc2_ca,final_sc2_cc,final_sc2_tcorners,final_sc2_cosc,final_sc2_hf_against)
# write.xlsx(final_sc2_all,'Divisions/SC2.xlsx',sheetName = "L6", append = TRUE)
# #####################################################################################
# #SC3
# #form
# #create final_sc3_hf object
# final_sc3_hf <- c()
# for(index_sc3_hf in 1:length(sc3_teams))
# {
#   index_sc3_hf <- row.names(sc3_form_h) == sc3_teams[index_sc3_hf]
#   form_sc3_hf <- sc3_form_h[index_sc3_hf]
#   deleted_form_sc3_hf <- form_sc3_hf[!form_sc3_hf[] == ""]
#   l6_form_sc3_hf <- tail(deleted_form_sc3_hf,sc3_last_n_games)
#   l6_form_sc3_hf <- paste(l6_form_sc3_hf,collapse = " ")
#   final_sc3_hf[index_sc3_hf] <- rbind(paste(sc3_teams[index_sc3_hf],l6_form_sc3_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc3_hf <- as.data.frame(final_sc3_hf)
# colnames(final_sc3_hf) <- "Form"
# #goals scored
# #create final_sc3_gs object
# final_sc3_gs <- c()
# suml6_sc3_gs <- c()
# for(index_sc3_gs in 1:length(sc3_teams))
# {
#   index_sc3_gs <- row.names(sc3_goalscored_h) == sc3_teams[index_sc3_gs]
#   form_sc3_gs <- sc3_goalscored_h[index_sc3_gs]
#   deleted_form_sc3_gs <- form_sc3_gs[!form_sc3_gs[] == ""]
#   l6_form_sc3_gs <- tail(deleted_form_sc3_gs,sc3_last_n_games)
#   l6_form_sc3_gs <- as.numeric(l6_form_sc3_gs)
#   suml6_sc3_gs[index_sc3_gs] <- sum(l6_form_sc3_gs)
#   suml6_sc3_gs[index_sc3_gs] <- paste("(",suml6_sc3_gs[index_sc3_gs],")",sep = "")
#   l6_form_sc3_gs <- paste(l6_form_sc3_gs,collapse = " ")
#   final_sc3_gs[index_sc3_gs] <- rbind(paste(sc3_teams[index_sc3_gs],l6_form_sc3_gs,suml6_sc3_gs[index_sc3_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# final_sc3_gs
# #change column names
# final_sc3_gs <- as.data.frame(final_sc3_gs)
# colnames(final_sc3_gs) <- "Goals scored"
# #goal conceded
# #create final_sc3_gc object
# final_sc3_gc <- c()
# suml6_sc3_gc <- c()
# for(index_sc3_gc in 1:length(sc3_teams))
# {
#   index_sc3_gc <- row.names(sc3_goalconceded_h) == sc3_teams[index_sc3_gc]
#   form_sc3_gc <- sc3_goalconceded_h[index_sc3_gc]
#   deleted_form_sc3_gc <- form_sc3_gc[!form_sc3_gc[] == ""]
#   l6_form_sc3_gc <- tail(deleted_form_sc3_gc,sc3_last_n_games)
#   l6_form_sc3_gc <- as.numeric(l6_form_sc3_gc)
#   suml6_sc3_gc[index_sc3_gc] <- sum(l6_form_sc3_gc)
#   suml6_sc3_gc[index_sc3_gc] <- paste("(",suml6_sc3_gc[index_sc3_gc],")",sep = "")
#   l6_form_sc3_gc <- paste(l6_form_sc3_gc,collapse = " ")
#   final_sc3_gc[index_sc3_gc] <- rbind(paste(sc3_teams[index_sc3_gc],l6_form_sc3_gc,suml6_sc3_gc[index_sc3_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
#
# #change column names
# final_sc3_gc <- as.data.frame(final_sc3_gc)
# colnames(final_sc3_gc) <- "Goals conceded"
# #total goals
# #create final_sc3_tg object
# final_sc3_tg <- c()
# suml6_sc3_tg <- c()
# for(index_sc3_tg in 1:length(sc3_teams))
# {
#   index_sc3_tg <- row.names(sc3_totalgoals_h) == sc3_teams[index_sc3_tg]
#   form_sc3_tg <- sc3_totalgoals_h[index_sc3_tg]
#   deleted_form_sc3_tg <- form_sc3_tg[!form_sc3_tg[] == ""]
#   l6_form_sc3_tg <- tail(deleted_form_sc3_tg,sc3_last_n_games)
#   l6_form_sc3_tg <- as.numeric(l6_form_sc3_tg)
#   suml6_sc3_tg[index_sc3_tg] <- sum(l6_form_sc3_tg)
#   suml6_sc3_tg[index_sc3_tg] <- paste("(",suml6_sc3_tg[index_sc3_tg],")",sep = "")
#   l6_form_sc3_tg <- paste(l6_form_sc3_tg,collapse = " ")
#   final_sc3_tg[index_sc3_tg] <- rbind(paste(sc3_teams[index_sc3_tg],l6_form_sc3_tg,suml6_sc3_tg[index_sc3_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# #change column names
# final_sc3_tg <- as.data.frame(final_sc3_tg)
# colnames(final_sc3_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sc3_hf object
# final_sc3_cs <- c()
# for(index_sc3_cs in 1:length(sc3_teams))
# {
#   index_sc3_cs <- row.names(sc3_csform_h) == sc3_teams[index_sc3_cs]
#   csform_sc3_cs <- sc3_csform_h[index_sc3_cs]
#   deleted_csform_sc3_cs <- csform_sc3_cs[!csform_sc3_cs[] == ""]
#   l6_csform_sc3_cs <- tail(deleted_csform_sc3_cs,sc3_last_n_games)
#   l6_csform_sc3_cs <- paste(l6_csform_sc3_cs,collapse = " ")
#   final_sc3_cs[index_sc3_cs] <- rbind(paste(sc3_teams[index_sc3_cs],l6_csform_sc3_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sc3_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sc3_cs <- as.data.frame(final_sc3_cs)
# colnames(final_sc3_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sc3_wm object
# final_sc3_wm <- c()
# suml6_sc3_wm <- c()
# for(index_sc3_wm in 1:length(sc3_teams))
# {
#   index_sc3_wm <- row.names(sc3_winmargin_h) == sc3_teams[index_sc3_wm]
#   form_sc3_wm <- sc3_winmargin_h[index_sc3_wm]
#   deleted_form_sc3_wm <- form_sc3_wm[!form_sc3_wm[] == ""]
#   l6_form_sc3_wm <- tail(deleted_form_sc3_wm,sc3_last_n_games)
#   l6_form_sc3_wm <- as.numeric(l6_form_sc3_wm)
#   suml6_sc3_wm[index_sc3_wm] <- sum(l6_form_sc3_wm)
#   suml6_sc3_wm[index_sc3_wm] <- paste("(",suml6_sc3_wm[index_sc3_wm],")",sep = "")
#   l6_form_sc3_wm <- paste(l6_form_sc3_wm,collapse = " ")
#   final_sc3_wm[index_sc3_wm] <- rbind(paste(sc3_teams[index_sc3_wm],l6_form_sc3_wm,suml6_sc3_wm[index_sc3_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# final_sc3_wm
# #change column names
# final_sc3_wm <- as.data.frame(final_sc3_wm)
# colnames(final_sc3_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sc3_ca object
# final_sc3_ca <- c()
# suml6_sc3_ca <- c()
# for(index_sc3_ca in 1:length(sc3_teams))
# {
#   index_sc3_ca <- row.names(sc3_coawarded_h) == sc3_teams[index_sc3_ca]
#   form_sc3_ca <- sc3_coawarded_h[index_sc3_ca]
#   deleted_form_sc3_ca <- form_sc3_ca[!form_sc3_ca[] == ""]
#   l6_form_sc3_ca <- tail(deleted_form_sc3_ca,sc3_last_n_games)
#   l6_form_sc3_ca <- as.numeric(l6_form_sc3_ca)
#   suml6_sc3_ca[index_sc3_ca] <- sum(l6_form_sc3_ca)
#   suml6_sc3_ca[index_sc3_ca] <- paste("(",suml6_sc3_ca[index_sc3_ca],")",sep = "")
#   l6_form_sc3_ca <- paste(l6_form_sc3_ca,collapse = " ")
#   final_sc3_ca[index_sc3_ca] <- rbind(paste(sc3_teams[index_sc3_ca],l6_form_sc3_ca,suml6_sc3_ca[index_sc3_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# final_sc3_ca
# #change column names
# final_sc3_ca <- as.data.frame(final_sc3_ca)
# colnames(final_sc3_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sc3_ca object
# final_sc3_cc <- c()
# suml6_sc3_cc <- c()
# for(index_sc3_cc in 1:length(sc3_teams))
# {
#   index_sc3_cc <- row.names(sc3_cornersconceded_h) == sc3_teams[index_sc3_cc]
#   form_sc3_cc <- sc3_cornersconceded_h[index_sc3_cc]
#   deleted_form_sc3_cc <- form_sc3_cc[!form_sc3_cc[] == ""]
#   l6_form_sc3_cc <- tail(deleted_form_sc3_cc,sc3_last_n_games)
#   l6_form_sc3_cc <- as.numeric(l6_form_sc3_cc)
#   suml6_sc3_cc[index_sc3_cc] <- sum(l6_form_sc3_cc)
#   suml6_sc3_cc[index_sc3_cc] <- paste("(",suml6_sc3_cc[index_sc3_cc],")",sep = "")
#   l6_form_sc3_cc <- paste(l6_form_sc3_cc,collapse = " ")
#   final_sc3_cc[index_sc3_cc] <- rbind(paste(sc3_teams[index_sc3_cc],l6_form_sc3_cc,suml6_sc3_cc[index_sc3_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# final_sc3_cc
# #change column names
# final_sc3_cc <- as.data.frame(final_sc3_cc)
# colnames(final_sc3_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sc3_cosc <- c()
# for(index_sc3_cosc in 1:length(sc3_teams))
# {
#   index_sc3_cosc <- row.names(sc3_coscform_h) == sc3_teams[index_sc3_cosc]
#   coscform_sc3_cosc <- sc3_coscform_h[index_sc3_cosc]
#   deleted_coscform_sc3_cosc <- coscform_sc3_cosc[!coscform_sc3_cosc[] == ""]
#   l6_coscform_sc3_cosc <- tail(deleted_coscform_sc3_cosc,sc3_last_n_games)
#   l6_coscform_sc3_cosc <- paste(l6_coscform_sc3_cosc,collapse = " ")
#   final_sc3_cosc[index_sc3_cosc] <- rbind(paste(sc3_teams[index_sc3_cosc],l6_coscform_sc3_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sc3_teams[index],l6_coscform)
#
# }
# final_sc3_cosc
# #change column names
# final_sc3_cosc <- as.data.frame(final_sc3_cosc)
# colnames(final_sc3_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_sc3_tcorners object
# final_sc3_tcorners <- c()
# suml6_sc3_tcorners <- c()
# for(index_sc3_tcorners in 1:length(sc3_teams))
# {
#   index_sc3_tcorners <- row.names(sc3_totalcorners_h) == sc3_teams[index_sc3_tcorners]
#   form_sc3_tcorners <- sc3_totalcorners_h[index_sc3_tcorners]
#   deleted_form_sc3_tcorners <- form_sc3_tcorners[!form_sc3_tcorners[] == ""]
#   l6_form_sc3_tcorners <- tail(deleted_form_sc3_tcorners,sc3_last_n_games)
#   l6_form_sc3_tcorners <- as.numeric(l6_form_sc3_tcorners)
#   suml6_sc3_tcorners[index_sc3_tcorners] <- sum(l6_form_sc3_tcorners)
#   suml6_sc3_tcorners[index_sc3_tcorners] <- paste("(",suml6_sc3_tcorners[index_sc3_tcorners],")",sep = "")
#   l6_form_sc3_tcorners <- paste(l6_form_sc3_tcorners,collapse = " ")
#   final_sc3_tcorners[index_sc3_tcorners] <- rbind(paste(sc3_teams[index_sc3_tcorners],l6_form_sc3_tcorners,suml6_sc3_tcorners[index_sc3_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# #change column names
# final_sc3_tcorners <- as.data.frame(final_sc3_tcorners)
# colnames(final_sc3_tcorners) <- "TotalCorners"
# ###################################################
# ##################################################
# #Team against
# #create final_sc3_hf_against
# final_sc3_hf_against <- c()
# for(index_sc3_hf_against in 1:length(sc3_teams))
# {
#   index_sc3_hf_against <- row.names(sc3_form_team_against_h) == sc3_teams[index_sc3_hf_against]
#   form_sc3_hf_against <- sc3_form_team_against_h[index_sc3_hf_against]
#   deleted_form_sc3_hf_against <- form_sc3_hf_against[!form_sc3_hf_against[] == ""]
#   l6_form_sc3_hf_against <- tail(deleted_form_sc3_hf_against,sc3_last_n_games)
#   l6_form_sc3_hf_against <- paste(l6_form_sc3_hf_against,collapse = " ")
#   final_sc3_hf_against[index_sc3_hf_against] <- rbind(paste(sc3_teams[index_sc3_hf_against],l6_form_sc3_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sc3_teams[index],l6_form)
#
# }
# final_sc3_hf_against <- as.data.frame(final_sc3_hf_against)
# colnames(final_sc3_hf_against) <- "Team against"
# #combine the columns
# final_sc3_all <- cbind(final_sc3_hf,final_sc3_gs,final_sc3_gc,final_sc3_tg,final_sc3_ca,final_sc3_cc,final_sc3_tcorners,final_sc3_cosc,final_sc3_hf_against)
# write.xlsx(final_sc3_all,'Divisions/SC3.xlsx',sheetName = "L6", append = TRUE)
# ######################################################################################
# #SP1
# #form
# #create final_sp1_hf object
# final_sp1_hf <- c()
# for(index_sp1_hf in 1:length(sp1_teams))
# {
#   index_sp1_hf <- row.names(sp1_form_h) == sp1_teams[index_sp1_hf]
#   form_sp1_hf <- sp1_form_h[index_sp1_hf]
#   deleted_form_sp1_hf <- form_sp1_hf[!form_sp1_hf[] == ""]
#   l6_form_sp1_hf <- tail(deleted_form_sp1_hf,sp1_last_n_games)
#   l6_form_sp1_hf <- paste(l6_form_sp1_hf,collapse = " ")
#   final_sp1_hf[index_sp1_hf] <- rbind(paste(sp1_teams[index_sp1_hf],l6_form_sp1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
#
# #change column names
# final_sp1_hf <- as.data.frame(final_sp1_hf)
# colnames(final_sp1_hf) <- "Form"
# #goals scored
# #create final_sp1_gs object
# final_sp1_gs <- c()
# suml6_sp1_gs <- c()
# for(index_sp1_gs in 1:length(sp1_teams))
# {
#   index_sp1_gs <- row.names(sp1_goalscored_h) == sp1_teams[index_sp1_gs]
#   form_sp1_gs <- sp1_goalscored_h[index_sp1_gs]
#   deleted_form_sp1_gs <- form_sp1_gs[!form_sp1_gs[] == ""]
#   l6_form_sp1_gs <- tail(deleted_form_sp1_gs,sp1_last_n_games)
#   l6_form_sp1_gs <- as.numeric(l6_form_sp1_gs)
#   suml6_sp1_gs[index_sp1_gs] <- sum(l6_form_sp1_gs)
#   suml6_sp1_gs[index_sp1_gs] <- paste("(",suml6_sp1_gs[index_sp1_gs],")",sep = "")
#   l6_form_sp1_gs <- paste(l6_form_sp1_gs,collapse = " ")
#   final_sp1_gs[index_sp1_gs] <- rbind(paste(sp1_teams[index_sp1_gs],l6_form_sp1_gs,suml6_sp1_gs[index_sp1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# final_sp1_gs
# #change column names
# final_sp1_gs <- as.data.frame(final_sp1_gs)
# colnames(final_sp1_gs) <- "Goals scored"
# #goal conceded
# #create final_sp1_gc object
# final_sp1_gc <- c()
# suml6_sp1_gc <- c()
# for(index_sp1_gc in 1:length(sp1_teams))
# {
#   index_sp1_gc <- row.names(sp1_goalconceded_h) == sp1_teams[index_sp1_gc]
#   form_sp1_gc <- sp1_goalconceded_h[index_sp1_gc]
#   deleted_form_sp1_gc <- form_sp1_gc[!form_sp1_gc[] == ""]
#   l6_form_sp1_gc <- tail(deleted_form_sp1_gc,sp1_last_n_games)
#   l6_form_sp1_gc <- as.numeric(l6_form_sp1_gc)
#   suml6_sp1_gc[index_sp1_gc] <- sum(l6_form_sp1_gc)
#   suml6_sp1_gc[index_sp1_gc] <- paste("(",suml6_sp1_gc[index_sp1_gc],")",sep = "")
#   l6_form_sp1_gc <- paste(l6_form_sp1_gc,collapse = " ")
#   final_sp1_gc[index_sp1_gc] <- rbind(paste(sp1_teams[index_sp1_gc],l6_form_sp1_gc,suml6_sp1_gc[index_sp1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
#
# #change column names
# final_sp1_gc <- as.data.frame(final_sp1_gc)
# colnames(final_sp1_gc) <- "Goals conceded"
# #total goals
# #create final_sp1_tg object
# final_sp1_tg <- c()
# suml6_sp1_tg <- c()
# for(index_sp1_tg in 1:length(sp1_teams))
# {
#   index_sp1_tg <- row.names(sp1_totalgoals_h) == sp1_teams[index_sp1_tg]
#   form_sp1_tg <- sp1_totalgoals_h[index_sp1_tg]
#   deleted_form_sp1_tg <- form_sp1_tg[!form_sp1_tg[] == ""]
#   l6_form_sp1_tg <- tail(deleted_form_sp1_tg,sp1_last_n_games)
#   l6_form_sp1_tg <- as.numeric(l6_form_sp1_tg)
#   suml6_sp1_tg[index_sp1_tg] <- sum(l6_form_sp1_tg)
#   suml6_sp1_tg[index_sp1_tg] <- paste("(",suml6_sp1_tg[index_sp1_tg],")",sep = "")
#   l6_form_sp1_tg <- paste(l6_form_sp1_tg,collapse = " ")
#   final_sp1_tg[index_sp1_tg] <- rbind(paste(sp1_teams[index_sp1_tg],l6_form_sp1_tg,suml6_sp1_tg[index_sp1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# #change column names
# final_sp1_tg <- as.data.frame(final_sp1_tg)
# colnames(final_sp1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sp1_hf object
# final_sp1_cs <- c()
# for(index_sp1_cs in 1:length(sp1_teams))
# {
#   index_sp1_cs <- row.names(sp1_csform_h) == sp1_teams[index_sp1_cs]
#   csform_sp1_cs <- sp1_csform_h[index_sp1_cs]
#   deleted_csform_sp1_cs <- csform_sp1_cs[!csform_sp1_cs[] == ""]
#   l6_csform_sp1_cs <- tail(deleted_csform_sp1_cs,sp1_last_n_games)
#   l6_csform_sp1_cs <- paste(l6_csform_sp1_cs,collapse = " ")
#   final_sp1_cs[index_sp1_cs] <- rbind(paste(sp1_teams[index_sp1_cs],l6_csform_sp1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sp1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sp1_cs <- as.data.frame(final_sp1_cs)
# colnames(final_sp1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sp1_wm object
# final_sp1_wm <- c()
# suml6_sp1_wm <- c()
# for(index_sp1_wm in 1:length(sp1_teams))
# {
#   index_sp1_wm <- row.names(sp1_winmargin_h) == sp1_teams[index_sp1_wm]
#   form_sp1_wm <- sp1_winmargin_h[index_sp1_wm]
#   deleted_form_sp1_wm <- form_sp1_wm[!form_sp1_wm[] == ""]
#   l6_form_sp1_wm <- tail(deleted_form_sp1_wm,sp1_last_n_games)
#   l6_form_sp1_wm <- as.numeric(l6_form_sp1_wm)
#   suml6_sp1_wm[index_sp1_wm] <- sum(l6_form_sp1_wm)
#   suml6_sp1_wm[index_sp1_wm] <- paste("(",suml6_sp1_wm[index_sp1_wm],")",sep = "")
#   l6_form_sp1_wm <- paste(l6_form_sp1_wm,collapse = " ")
#   final_sp1_wm[index_sp1_wm] <- rbind(paste(sp1_teams[index_sp1_wm],l6_form_sp1_wm,suml6_sp1_wm[index_sp1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# final_sp1_wm
# #change column names
# final_sp1_wm <- as.data.frame(final_sp1_wm)
# colnames(final_sp1_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sp1_ca object
# final_sp1_ca <- c()
# suml6_sp1_ca <- c()
# for(index_sp1_ca in 1:length(sp1_teams))
# {
#   index_sp1_ca <- row.names(sp1_coawarded_h) == sp1_teams[index_sp1_ca]
#   form_sp1_ca <- sp1_coawarded_h[index_sp1_ca]
#   deleted_form_sp1_ca <- form_sp1_ca[!form_sp1_ca[] == ""]
#   l6_form_sp1_ca <- tail(deleted_form_sp1_ca,sp1_last_n_games)
#   l6_form_sp1_ca <- as.numeric(l6_form_sp1_ca)
#   suml6_sp1_ca[index_sp1_ca] <- sum(l6_form_sp1_ca)
#   suml6_sp1_ca[index_sp1_ca] <- paste("(",suml6_sp1_ca[index_sp1_ca],")",sep = "")
#   l6_form_sp1_ca <- paste(l6_form_sp1_ca,collapse = " ")
#   final_sp1_ca[index_sp1_ca] <- rbind(paste(sp1_teams[index_sp1_ca],l6_form_sp1_ca,suml6_sp1_ca[index_sp1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# final_sp1_ca
# #change column names
# final_sp1_ca <- as.data.frame(final_sp1_ca)
# colnames(final_sp1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sp1_ca object
# final_sp1_cc <- c()
# suml6_sp1_cc <- c()
# for(index_sp1_cc in 1:length(sp1_teams))
# {
#   index_sp1_cc <- row.names(sp1_cornersconceded_h) == sp1_teams[index_sp1_cc]
#   form_sp1_cc <- sp1_cornersconceded_h[index_sp1_cc]
#   deleted_form_sp1_cc <- form_sp1_cc[!form_sp1_cc[] == ""]
#   l6_form_sp1_cc <- tail(deleted_form_sp1_cc,sp1_last_n_games)
#   l6_form_sp1_cc <- as.numeric(l6_form_sp1_cc)
#   suml6_sp1_cc[index_sp1_cc] <- sum(l6_form_sp1_cc)
#   suml6_sp1_cc[index_sp1_cc] <- paste("(",suml6_sp1_cc[index_sp1_cc],")",sep = "")
#   l6_form_sp1_cc <- paste(l6_form_sp1_cc,collapse = " ")
#   final_sp1_cc[index_sp1_cc] <- rbind(paste(sp1_teams[index_sp1_cc],l6_form_sp1_cc,suml6_sp1_cc[index_sp1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# final_sp1_cc
# #change column names
# final_sp1_cc <- as.data.frame(final_sp1_cc)
# colnames(final_sp1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sp1_cosc <- c()
# for(index_sp1_cosc in 1:length(sp1_teams))
# {
#   index_sp1_cosc <- row.names(sp1_coscform_h) == sp1_teams[index_sp1_cosc]
#   coscform_sp1_cosc <- sp1_coscform_h[index_sp1_cosc]
#   deleted_coscform_sp1_cosc <- coscform_sp1_cosc[!coscform_sp1_cosc[] == ""]
#   l6_coscform_sp1_cosc <- tail(deleted_coscform_sp1_cosc,sp1_last_n_games)
#   l6_coscform_sp1_cosc <- paste(l6_coscform_sp1_cosc,collapse = " ")
#   final_sp1_cosc[index_sp1_cosc] <- rbind(paste(sp1_teams[index_sp1_cosc],l6_coscform_sp1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sp1_teams[index],l6_coscform)
#
# }
# final_sp1_cosc
# #change column names
# final_sp1_cosc <- as.data.frame(final_sp1_cosc)
# colnames(final_sp1_cosc) <- "CornersForm"
# #################################################
#
# ##################################################
# #total corners
# #create final_sp1_tcorners object
# final_sp1_tcorners <- c()
# suml6_sp1_tcorners <- c()
# for(index_sp1_tcorners in 1:length(sp1_teams))
# {
#   index_sp1_tcorners <- row.names(sp1_totalcorners_h) == sp1_teams[index_sp1_tcorners]
#   form_sp1_tcorners <- sp1_totalcorners_h[index_sp1_tcorners]
#   deleted_form_sp1_tcorners <- form_sp1_tcorners[!form_sp1_tcorners[] == ""]
#   l6_form_sp1_tcorners <- tail(deleted_form_sp1_tcorners,sp1_last_n_games)
#   l6_form_sp1_tcorners <- as.numeric(l6_form_sp1_tcorners)
#   suml6_sp1_tcorners[index_sp1_tcorners] <- sum(l6_form_sp1_tcorners)
#   suml6_sp1_tcorners[index_sp1_tcorners] <- paste("(",suml6_sp1_tcorners[index_sp1_tcorners],")",sep = "")
#   l6_form_sp1_tcorners <- paste(l6_form_sp1_tcorners,collapse = " ")
#   final_sp1_tcorners[index_sp1_tcorners] <- rbind(paste(sp1_teams[index_sp1_tcorners],l6_form_sp1_tcorners,suml6_sp1_tcorners[index_sp1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# #change column names
# final_sp1_tcorners <- as.data.frame(final_sp1_tcorners)
# colnames(final_sp1_tcorners) <- "TotalCorners"
# ###################################################
# ##################################################
# #Team against
# #create final_sp1_hf_against
# final_sp1_hf_against <- c()
# for(index_sp1_hf_against in 1:length(sp1_teams))
# {
#   index_sp1_hf_against <- row.names(sp1_form_team_against_h) == sp1_teams[index_sp1_hf_against]
#   form_sp1_hf_against <- sp1_form_team_against_h[index_sp1_hf_against]
#   deleted_form_sp1_hf_against <- form_sp1_hf_against[!form_sp1_hf_against[] == ""]
#   l6_form_sp1_hf_against <- tail(deleted_form_sp1_hf_against,sp1_last_n_games)
#   l6_form_sp1_hf_against <- paste(l6_form_sp1_hf_against,collapse = " ")
#   final_sp1_hf_against[index_sp1_hf_against] <- rbind(paste(sp1_teams[index_sp1_hf_against],l6_form_sp1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp1_teams[index],l6_form)
#
# }
# final_sp1_hf_against <- as.data.frame(final_sp1_hf_against)
# colnames(final_sp1_hf_against) <- "Team against"
# #combine the columns
# final_sp1_all <- cbind(final_sp1_hf,final_sp1_gs,final_sp1_gc,final_sp1_tg,final_sp1_ca,final_sp1_cc,final_sp1_tcorners,final_sp1_cosc,final_sp1_hf_against)
# write.xlsx(final_sp1_all,'Divisions/SP1.xlsx',sheetName = "L6", append = TRUE)
# #####################################################################################
# #SP2
# #form
# #create final_sp2_hf object
# final_sp2_hf <- c()
# for(index_sp2_hf in 1:length(sp2_teams))
# {
#   index_sp2_hf <- row.names(sp2_form_h) == sp2_teams[index_sp2_hf]
#   form_sp2_hf <- sp2_form_h[index_sp2_hf]
#   deleted_form_sp2_hf <- form_sp2_hf[!form_sp2_hf[] == ""]
#   l6_form_sp2_hf <- tail(deleted_form_sp2_hf,sp2_last_n_games)
#   l6_form_sp2_hf <- paste(l6_form_sp2_hf,collapse = " ")
#   final_sp2_hf[index_sp2_hf] <- rbind(paste(sp2_teams[index_sp2_hf],l6_form_sp2_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
#
# #change column names
# final_sp2_hf <- as.data.frame(final_sp2_hf)
# colnames(final_sp2_hf) <- "Form"
# #goals scored
# #create final_sp2_gs object
# final_sp2_gs <- c()
# suml6_sp2_gs <- c()
# for(index_sp2_gs in 1:length(sp2_teams))
# {
#   index_sp2_gs <- row.names(sp2_goalscored_h) == sp2_teams[index_sp2_gs]
#   form_sp2_gs <- sp2_goalscored_h[index_sp2_gs]
#   deleted_form_sp2_gs <- form_sp2_gs[!form_sp2_gs[] == ""]
#   l6_form_sp2_gs <- tail(deleted_form_sp2_gs,sp2_last_n_games)
#   l6_form_sp2_gs <- as.numeric(l6_form_sp2_gs)
#   suml6_sp2_gs[index_sp2_gs] <- sum(l6_form_sp2_gs)
#   suml6_sp2_gs[index_sp2_gs] <- paste("(",suml6_sp2_gs[index_sp2_gs],")",sep = "")
#   l6_form_sp2_gs <- paste(l6_form_sp2_gs,collapse = " ")
#   final_sp2_gs[index_sp2_gs] <- rbind(paste(sp2_teams[index_sp2_gs],l6_form_sp2_gs,suml6_sp2_gs[index_sp2_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# final_sp2_gs
# #change column names
# final_sp2_gs <- as.data.frame(final_sp2_gs)
# colnames(final_sp2_gs) <- "Goals scored"
# #goal conceded
# #create final_sp2_gc object
# final_sp2_gc <- c()
# suml6_sp2_gc <- c()
# for(index_sp2_gc in 1:length(sp2_teams))
# {
#   index_sp2_gc <- row.names(sp2_goalconceded_h) == sp2_teams[index_sp2_gc]
#   form_sp2_gc <- sp2_goalconceded_h[index_sp2_gc]
#   deleted_form_sp2_gc <- form_sp2_gc[!form_sp2_gc[] == ""]
#   l6_form_sp2_gc <- tail(deleted_form_sp2_gc,sp2_last_n_games)
#   l6_form_sp2_gc <- as.numeric(l6_form_sp2_gc)
#   suml6_sp2_gc[index_sp2_gc] <- sum(l6_form_sp2_gc)
#   suml6_sp2_gc[index_sp2_gc] <- paste("(",suml6_sp2_gc[index_sp2_gc],")",sep = "")
#   l6_form_sp2_gc <- paste(l6_form_sp2_gc,collapse = " ")
#   final_sp2_gc[index_sp2_gc] <- rbind(paste(sp2_teams[index_sp2_gc],l6_form_sp2_gc,suml6_sp2_gc[index_sp2_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
#
# #change column names
# final_sp2_gc <- as.data.frame(final_sp2_gc)
# colnames(final_sp2_gc) <- "Goals conceded"
# #total goals
# #create final_sp2_tg object
# final_sp2_tg <- c()
# suml6_sp2_tg <- c()
# for(index_sp2_tg in 1:length(sp2_teams))
# {
#   index_sp2_tg <- row.names(sp2_totalgoals_h) == sp2_teams[index_sp2_tg]
#   form_sp2_tg <- sp2_totalgoals_h[index_sp2_tg]
#   deleted_form_sp2_tg <- form_sp2_tg[!form_sp2_tg[] == ""]
#   l6_form_sp2_tg <- tail(deleted_form_sp2_tg,sp2_last_n_games)
#   l6_form_sp2_tg <- as.numeric(l6_form_sp2_tg)
#   suml6_sp2_tg[index_sp2_tg] <- sum(l6_form_sp2_tg)
#   suml6_sp2_tg[index_sp2_tg] <- paste("(",suml6_sp2_tg[index_sp2_tg],")",sep = "")
#   l6_form_sp2_tg <- paste(l6_form_sp2_tg,collapse = " ")
#   final_sp2_tg[index_sp2_tg] <- rbind(paste(sp2_teams[index_sp2_tg],l6_form_sp2_tg,suml6_sp2_tg[index_sp2_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# #change column names
# final_sp2_tg <- as.data.frame(final_sp2_tg)
# colnames(final_sp2_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_sp2_hf object
# final_sp2_cs <- c()
# for(index_sp2_cs in 1:length(sp2_teams))
# {
#   index_sp2_cs <- row.names(sp2_csform_h) == sp2_teams[index_sp2_cs]
#   csform_sp2_cs <- sp2_csform_h[index_sp2_cs]
#   deleted_csform_sp2_cs <- csform_sp2_cs[!csform_sp2_cs[] == ""]
#   l6_csform_sp2_cs <- tail(deleted_csform_sp2_cs,sp2_last_n_games)
#   l6_csform_sp2_cs <- paste(l6_csform_sp2_cs,collapse = " ")
#   final_sp2_cs[index_sp2_cs] <- rbind(paste(sp2_teams[index_sp2_cs],l6_csform_sp2_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",sp2_teams[index],l6_csform)
#
# }
#
# #change column names
# final_sp2_cs <- as.data.frame(final_sp2_cs)
# colnames(final_sp2_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_sp2_wm object
# final_sp2_wm <- c()
# suml6_sp2_wm <- c()
# for(index_sp2_wm in 1:length(sp2_teams))
# {
#   index_sp2_wm <- row.names(sp2_winmargin_h) == sp2_teams[index_sp2_wm]
#   form_sp2_wm <- sp2_winmargin_h[index_sp2_wm]
#   deleted_form_sp2_wm <- form_sp2_wm[!form_sp2_wm[] == ""]
#   l6_form_sp2_wm <- tail(deleted_form_sp2_wm,sp2_last_n_games)
#   l6_form_sp2_wm <- as.numeric(l6_form_sp2_wm)
#   suml6_sp2_wm[index_sp2_wm] <- sum(l6_form_sp2_wm)
#   suml6_sp2_wm[index_sp2_wm] <- paste("(",suml6_sp2_wm[index_sp2_wm],")",sep = "")
#   l6_form_sp2_wm <- paste(l6_form_sp2_wm,collapse = " ")
#   final_sp2_wm[index_sp2_wm] <- rbind(paste(sp2_teams[index_sp2_wm],l6_form_sp2_wm,suml6_sp2_wm[index_sp2_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# final_sp2_wm
# #change column names
# final_sp2_wm <- as.data.frame(final_sp2_wm)
# colnames(final_sp2_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_sp2_ca object
# final_sp2_ca <- c()
# suml6_sp2_ca <- c()
# for(index_sp2_ca in 1:length(sp2_teams))
# {
#   index_sp2_ca <- row.names(sp2_coawarded_h) == sp2_teams[index_sp2_ca]
#   form_sp2_ca <- sp2_coawarded_h[index_sp2_ca]
#   deleted_form_sp2_ca <- form_sp2_ca[!form_sp2_ca[] == ""]
#   l6_form_sp2_ca <- tail(deleted_form_sp2_ca,sp2_last_n_games)
#   l6_form_sp2_ca <- as.numeric(l6_form_sp2_ca)
#   suml6_sp2_ca[index_sp2_ca] <- sum(l6_form_sp2_ca)
#   suml6_sp2_ca[index_sp2_ca] <- paste("(",suml6_sp2_ca[index_sp2_ca],")",sep = "")
#   l6_form_sp2_ca <- paste(l6_form_sp2_ca,collapse = " ")
#   final_sp2_ca[index_sp2_ca] <- rbind(paste(sp2_teams[index_sp2_ca],l6_form_sp2_ca,suml6_sp2_ca[index_sp2_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# final_sp2_ca
# #change column names
# final_sp2_ca <- as.data.frame(final_sp2_ca)
# colnames(final_sp2_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_sp2_ca object
# final_sp2_cc <- c()
# suml6_sp2_cc <- c()
# for(index_sp2_cc in 1:length(sp2_teams))
# {
#   index_sp2_cc <- row.names(sp2_cornersconceded_h) == sp2_teams[index_sp2_cc]
#   form_sp2_cc <- sp2_cornersconceded_h[index_sp2_cc]
#   deleted_form_sp2_cc <- form_sp2_cc[!form_sp2_cc[] == ""]
#   l6_form_sp2_cc <- tail(deleted_form_sp2_cc,sp2_last_n_games)
#   l6_form_sp2_cc <- as.numeric(l6_form_sp2_cc)
#   suml6_sp2_cc[index_sp2_cc] <- sum(l6_form_sp2_cc)
#   suml6_sp2_cc[index_sp2_cc] <- paste("(",suml6_sp2_cc[index_sp2_cc],")",sep = "")
#   l6_form_sp2_cc <- paste(l6_form_sp2_cc,collapse = " ")
#   final_sp2_cc[index_sp2_cc] <- rbind(paste(sp2_teams[index_sp2_cc],l6_form_sp2_cc,suml6_sp2_cc[index_sp2_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# final_sp2_cc
# #change column names
# final_sp2_cc <- as.data.frame(final_sp2_cc)
# colnames(final_sp2_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_sp2_cosc <- c()
# for(index_sp2_cosc in 1:length(sp2_teams))
# {
#   index_sp2_cosc <- row.names(sp2_coscform_h) == sp2_teams[index_sp2_cosc]
#   coscform_sp2_cosc <- sp2_coscform_h[index_sp2_cosc]
#   deleted_coscform_sp2_cosc <- coscform_sp2_cosc[!coscform_sp2_cosc[] == ""]
#   l6_coscform_sp2_cosc <- tail(deleted_coscform_sp2_cosc,sp2_last_n_games)
#   l6_coscform_sp2_cosc <- paste(l6_coscform_sp2_cosc,collapse = " ")
#   final_sp2_cosc[index_sp2_cosc] <- rbind(paste(sp2_teams[index_sp2_cosc],l6_coscform_sp2_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",sp2_teams[index],l6_coscform)
#
# }
# final_sp2_cosc
# #change column names
# final_sp2_cosc <- as.data.frame(final_sp2_cosc)
# colnames(final_sp2_cosc) <- "CornersForm"
# ##########################################################
# ##################################################
# #total corners
# #create final_sp2_tcorners object
# final_sp2_tcorners <- c()
# suml6_sp2_tcorners <- c()
# for(index_sp2_tcorners in 1:length(sp2_teams))
# {
#   index_sp2_tcorners <- row.names(sp2_totalcorners_h) == sp2_teams[index_sp2_tcorners]
#   form_sp2_tcorners <- sp2_totalcorners_h[index_sp2_tcorners]
#   deleted_form_sp2_tcorners <- form_sp2_tcorners[!form_sp2_tcorners[] == ""]
#   l6_form_sp2_tcorners <- tail(deleted_form_sp2_tcorners,sp2_last_n_games)
#   l6_form_sp2_tcorners <- as.numeric(l6_form_sp2_tcorners)
#   suml6_sp2_tcorners[index_sp2_tcorners] <- sum(l6_form_sp2_tcorners)
#   suml6_sp2_tcorners[index_sp2_tcorners] <- paste("(",suml6_sp2_tcorners[index_sp2_tcorners],")",sep = "")
#   l6_form_sp2_tcorners <- paste(l6_form_sp2_tcorners,collapse = " ")
#   final_sp2_tcorners[index_sp2_tcorners] <- rbind(paste(sp2_teams[index_sp2_tcorners],l6_form_sp2_tcorners,suml6_sp2_tcorners[index_sp2_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# #change column names
# final_sp2_tcorners <- as.data.frame(final_sp2_tcorners)
# colnames(final_sp2_tcorners) <- "TotalCorners"
# ###################################################
# ##########################################################
# #Team against
# #create final_sp2_hf_against
# final_sp2_hf_against <- c()
# for(index_sp2_hf_against in 1:length(sp2_teams))
# {
#   index_sp2_hf_against <- row.names(sp2_form_team_against_h) == sp2_teams[index_sp2_hf_against]
#   form_sp2_hf_against <- sp2_form_team_against_h[index_sp2_hf_against]
#   deleted_form_sp2_hf_against <- form_sp2_hf_against[!form_sp2_hf_against[] == ""]
#   l6_form_sp2_hf_against <- tail(deleted_form_sp2_hf_against,sp2_last_n_games)
#   l6_form_sp2_hf_against <- paste(l6_form_sp2_hf_against,collapse = " ")
#   final_sp2_hf_against[index_sp2_hf_against] <- rbind(paste(sp2_teams[index_sp2_hf_against],l6_form_sp2_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",sp2_teams[index],l6_form)
#
# }
# final_sp2_hf_against <- as.data.frame(final_sp2_hf_against)
# colnames(final_sp2_hf_against) <- "Team against"
# #combine the columns
# final_sp2_all <- cbind(final_sp2_hf,final_sp2_gs,final_sp2_gc,final_sp2_tg,final_sp2_ca,final_sp2_cc,final_sp2_tcorners,final_sp2_cosc,final_sp2_hf_against)
# write.xlsx(final_sp2_all,'Divisions/SP2.xlsx',sheetName = "L6", append = TRUE)
# ####################################################################################################
# #T1
# #form
# #create final_t1_hf object
# final_t1_hf <- c()
# for(index_t1_hf in 1:length(t1_teams))
# {
#   index_t1_hf <- row.names(t1_form_h) == t1_teams[index_t1_hf]
#   form_t1_hf <- t1_form_h[index_t1_hf]
#   deleted_form_t1_hf <- form_t1_hf[!form_t1_hf[] == ""]
#   l6_form_t1_hf <- tail(deleted_form_t1_hf,t1_last_n_games)
#   l6_form_t1_hf <- paste(l6_form_t1_hf,collapse = " ")
#   final_t1_hf[index_t1_hf] <- rbind(paste(t1_teams[index_t1_hf],l6_form_t1_hf, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
#
# #change column names
# final_t1_hf <- as.data.frame(final_t1_hf)
# colnames(final_t1_hf) <- "Form"
# #goals scored
# #create final_t1_gs object
# final_t1_gs <- c()
# suml6_t1_gs <- c()
# for(index_t1_gs in 1:length(t1_teams))
# {
#   index_t1_gs <- row.names(t1_goalscored_h) == t1_teams[index_t1_gs]
#   form_t1_gs <- t1_goalscored_h[index_t1_gs]
#   deleted_form_t1_gs <- form_t1_gs[!form_t1_gs[] == ""]
#   l6_form_t1_gs <- tail(deleted_form_t1_gs,t1_last_n_games)
#   l6_form_t1_gs <- as.numeric(l6_form_t1_gs)
#   suml6_t1_gs[index_t1_gs] <- sum(l6_form_t1_gs)
#   suml6_t1_gs[index_t1_gs] <- paste("(",suml6_t1_gs[index_t1_gs],")",sep = "")
#   l6_form_t1_gs <- paste(l6_form_t1_gs,collapse = " ")
#   final_t1_gs[index_t1_gs] <- rbind(paste(t1_teams[index_t1_gs],l6_form_t1_gs,suml6_t1_gs[index_t1_gs], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# final_t1_gs
# #change column names
# final_t1_gs <- as.data.frame(final_t1_gs)
# colnames(final_t1_gs) <- "Goals scored"
# #goal conceded
# #create final_t1_gc object
# final_t1_gc <- c()
# suml6_t1_gc <- c()
# for(index_t1_gc in 1:length(t1_teams))
# {
#   index_t1_gc <- row.names(t1_goalconceded_h) == t1_teams[index_t1_gc]
#   form_t1_gc <- t1_goalconceded_h[index_t1_gc]
#   deleted_form_t1_gc <- form_t1_gc[!form_t1_gc[] == ""]
#   l6_form_t1_gc <- tail(deleted_form_t1_gc,t1_last_n_games)
#   l6_form_t1_gc <- as.numeric(l6_form_t1_gc)
#   suml6_t1_gc[index_t1_gc] <- sum(l6_form_t1_gc)
#   suml6_t1_gc[index_t1_gc] <- paste("(",suml6_t1_gc[index_t1_gc],")",sep = "")
#   l6_form_t1_gc <- paste(l6_form_t1_gc,collapse = " ")
#   final_t1_gc[index_t1_gc] <- rbind(paste(t1_teams[index_t1_gc],l6_form_t1_gc,suml6_t1_gc[index_t1_gc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
#
# #change column names
# final_t1_gc <- as.data.frame(final_t1_gc)
# colnames(final_t1_gc) <- "Goals conceded"
# #total goals
# #create final_t1_tg object
# final_t1_tg <- c()
# suml6_t1_tg <- c()
# for(index_t1_tg in 1:length(t1_teams))
# {
#   index_t1_tg <- row.names(t1_totalgoals_h) == t1_teams[index_t1_tg]
#   form_t1_tg <- t1_totalgoals_h[index_t1_tg]
#   deleted_form_t1_tg <- form_t1_tg[!form_t1_tg[] == ""]
#   l6_form_t1_tg <- tail(deleted_form_t1_tg,t1_last_n_games)
#   l6_form_t1_tg <- as.numeric(l6_form_t1_tg)
#   suml6_t1_tg[index_t1_tg] <- sum(l6_form_t1_tg)
#   suml6_t1_tg[index_t1_tg] <- paste("(",suml6_t1_tg[index_t1_tg],")",sep = "")
#   l6_form_t1_tg <- paste(l6_form_t1_tg,collapse = " ")
#   final_t1_tg[index_t1_tg] <- rbind(paste(t1_teams[index_t1_tg],l6_form_t1_tg,suml6_t1_tg[index_t1_tg], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# #change column names
# final_t1_tg <- as.data.frame(final_t1_tg)
# colnames(final_t1_tg) <- "Total Goals"
# ###############################################
# #Csfrom
# #create final_t1_hf object
# final_t1_cs <- c()
# for(index_t1_cs in 1:length(t1_teams))
# {
#   index_t1_cs <- row.names(t1_csform_h) == t1_teams[index_t1_cs]
#   csform_t1_cs <- t1_csform_h[index_t1_cs]
#   deleted_csform_t1_cs <- csform_t1_cs[!csform_t1_cs[] == ""]
#   l6_csform_t1_cs <- tail(deleted_csform_t1_cs,t1_last_n_games)
#   l6_csform_t1_cs <- paste(l6_csform_t1_cs,collapse = " ")
#   final_t1_cs[index_t1_cs] <- rbind(paste(t1_teams[index_t1_cs],l6_csform_t1_cs, sep = ",",collapse = ""))
#   #bundescsform[] <- printf("%s\t%s\n",t1_teams[index],l6_csform)
#
# }
#
# #change column names
# final_t1_cs <- as.data.frame(final_t1_cs)
# colnames(final_t1_cs) <- "CSForm"
# #################################################
# #################################################
# #Win Margin
# #goals scored
# #create final_t1_wm object
# final_t1_wm <- c()
# suml6_t1_wm <- c()
# for(index_t1_wm in 1:length(t1_teams))
# {
#   index_t1_wm <- row.names(t1_winmargin_h) == t1_teams[index_t1_wm]
#   form_t1_wm <- t1_winmargin_h[index_t1_wm]
#   deleted_form_t1_wm <- form_t1_wm[!form_t1_wm[] == ""]
#   l6_form_t1_wm <- tail(deleted_form_t1_wm,t1_last_n_games)
#   l6_form_t1_wm <- as.numeric(l6_form_t1_wm)
#   suml6_t1_wm[index_t1_wm] <- sum(l6_form_t1_wm)
#   suml6_t1_wm[index_t1_wm] <- paste("(",suml6_t1_wm[index_t1_wm],")",sep = "")
#   l6_form_t1_wm <- paste(l6_form_t1_wm,collapse = " ")
#   final_t1_wm[index_t1_wm] <- rbind(paste(t1_teams[index_t1_wm],l6_form_t1_wm,suml6_t1_wm[index_t1_wm], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# final_t1_wm
# #change column names
# final_t1_wm <- as.data.frame(final_t1_wm)
# colnames(final_t1_wm) <- "Win Margin"
# #################################################
# #################################################
# #corners awarded
# #create final_t1_ca object
# final_t1_ca <- c()
# suml6_t1_ca <- c()
# for(index_t1_ca in 1:length(t1_teams))
# {
#   index_t1_ca <- row.names(t1_coawarded_h) == t1_teams[index_t1_ca]
#   form_t1_ca <- t1_coawarded_h[index_t1_ca]
#   deleted_form_t1_ca <- form_t1_ca[!form_t1_ca[] == ""]
#   l6_form_t1_ca <- tail(deleted_form_t1_ca,t1_last_n_games)
#   l6_form_t1_ca <- as.numeric(l6_form_t1_ca)
#   suml6_t1_ca[index_t1_ca] <- sum(l6_form_t1_ca)
#   suml6_t1_ca[index_t1_ca] <- paste("(",suml6_t1_ca[index_t1_ca],")",sep = "")
#   l6_form_t1_ca <- paste(l6_form_t1_ca,collapse = " ")
#   final_t1_ca[index_t1_ca] <- rbind(paste(t1_teams[index_t1_ca],l6_form_t1_ca,suml6_t1_ca[index_t1_ca], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# final_t1_ca
# #change column names
# final_t1_ca <- as.data.frame(final_t1_ca)
# colnames(final_t1_ca) <- "CornersAwarded"
# ##################################################
# ##################################################
# #corners awarded
# #create final_t1_ca object
# final_t1_cc <- c()
# suml6_t1_cc <- c()
# for(index_t1_cc in 1:length(t1_teams))
# {
#   index_t1_cc <- row.names(t1_cornersconceded_h) == t1_teams[index_t1_cc]
#   form_t1_cc <- t1_cornersconceded_h[index_t1_cc]
#   deleted_form_t1_cc <- form_t1_cc[!form_t1_cc[] == ""]
#   l6_form_t1_cc <- tail(deleted_form_t1_cc,t1_last_n_games)
#   l6_form_t1_cc <- as.numeric(l6_form_t1_cc)
#   suml6_t1_cc[index_t1_cc] <- sum(l6_form_t1_cc)
#   suml6_t1_cc[index_t1_cc] <- paste("(",suml6_t1_cc[index_t1_cc],")",sep = "")
#   l6_form_t1_cc <- paste(l6_form_t1_cc,collapse = " ")
#   final_t1_cc[index_t1_cc] <- rbind(paste(t1_teams[index_t1_cc],l6_form_t1_cc,suml6_t1_cc[index_t1_cc], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# final_t1_cc
# #change column names
# final_t1_cc <- as.data.frame(final_t1_cc)
# colnames(final_t1_cc) <- "CornersConceded"
# ##################################################
# ##################################################
# #corners form
# final_t1_cosc <- c()
# for(index_t1_cosc in 1:length(t1_teams))
# {
#   index_t1_cosc <- row.names(t1_coscform_h) == t1_teams[index_t1_cosc]
#   coscform_t1_cosc <- t1_coscform_h[index_t1_cosc]
#   deleted_coscform_t1_cosc <- coscform_t1_cosc[!coscform_t1_cosc[] == ""]
#   l6_coscform_t1_cosc <- tail(deleted_coscform_t1_cosc,t1_last_n_games)
#   l6_coscform_t1_cosc <- paste(l6_coscform_t1_cosc,collapse = " ")
#   final_t1_cosc[index_t1_cosc] <- rbind(paste(t1_teams[index_t1_cosc],l6_coscform_t1_cosc, sep = ",",collapse = ""))
#   #bundescoscform[] <- printf("%s\t%s\n",t1_teams[index],l6_coscform)
#
# }
# final_t1_cosc
# #change column names
# final_t1_cosc <- as.data.frame(final_t1_cosc)
# colnames(final_t1_cosc) <- "CornersForm"
# #################################################
# ##################################################
# #total corners
# #create final_t1_tcorners object
# final_t1_tcorners <- c()
# suml6_t1_tcorners <- c()
# for(index_t1_tcorners in 1:length(t1_teams))
# {
#   index_t1_tcorners <- row.names(t1_totalcorners_h) == t1_teams[index_t1_tcorners]
#   form_t1_tcorners <- t1_totalcorners_h[index_t1_tcorners]
#   deleted_form_t1_tcorners <- form_t1_tcorners[!form_t1_tcorners[] == ""]
#   l6_form_t1_tcorners <- tail(deleted_form_t1_tcorners,t1_last_n_games)
#   l6_form_t1_tcorners <- as.numeric(l6_form_t1_tcorners)
#   suml6_t1_tcorners[index_t1_tcorners] <- sum(l6_form_t1_tcorners)
#   suml6_t1_tcorners[index_t1_tcorners] <- paste("(",suml6_t1_tcorners[index_t1_tcorners],")",sep = "")
#   l6_form_t1_tcorners <- paste(l6_form_t1_tcorners,collapse = " ")
#   final_t1_tcorners[index_t1_tcorners] <- rbind(paste(t1_teams[index_t1_tcorners],l6_form_t1_tcorners,suml6_t1_tcorners[index_t1_tcorners], sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# #change column names
# final_t1_tcorners <- as.data.frame(final_t1_tcorners)
# colnames(final_t1_tcorners) <- "TotalCorners"
# ###################################################
# #################################################
# #Team against
# #create final_t1_hf_against
# final_t1_hf_against <- c()
# for(index_t1_hf_against in 1:length(t1_teams))
# {
#   index_t1_hf_against <- row.names(t1_form_team_against_h) == t1_teams[index_t1_hf_against]
#   form_t1_hf_against <- t1_form_team_against_h[index_t1_hf_against]
#   deleted_form_t1_hf_against <- form_t1_hf_against[!form_t1_hf_against[] == ""]
#   l6_form_t1_hf_against <- tail(deleted_form_t1_hf_against,t1_last_n_games)
#   l6_form_t1_hf_against <- paste(l6_form_t1_hf_against,collapse = " ")
#   final_t1_hf_against[index_t1_hf_against] <- rbind(paste(t1_teams[index_t1_hf_against],l6_form_t1_hf_against, sep = ",",collapse = ""))
#   #bundesform[] <- printf("%s\t%s\n",t1_teams[index],l6_form)
#
# }
# final_t1_hf_against <- as.data.frame(final_t1_hf_against)
# colnames(final_t1_hf_against) <- "Team against"
# #combine the columns
# final_t1_all <- cbind(final_t1_hf,final_t1_gs,final_t1_gc,final_t1_tg,final_t1_ca,final_t1_cc,final_t1_tcorners,final_t1_cosc,final_t1_hf_against)
# write.xlsx(final_t1_all,'Divisions/T1.xlsx',sheetName = "L6", append = TRUE)




