library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_gs <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_gs <- c()
sum_mls_zero_gs <- c()
sum_mls_one_gs <- c()
sum_mls_two_gs <- c()
sum_mls_three_gs <- c()
avgr_mls_gs <- c()
sdr_mls_gs <- c()
l6_form_mls_gssplitted <- c()
form_mls_gs <- c()
for(index_mls_gs in 1:length(mls_teams))
{
  for(index_mls_gs_cols in 1:mls_totalrounds)
  {
    index_mls_gs  <- row.names(mls_goalscored_h) == mls_teams[index_mls_gs]
    form_mls_gs <- mls_goalscored_h[index_mls_gs ]
    deleted_form_mls_gs <- form_mls_gs[!form_mls_gs[] == ""]
    l6_form_mls_gs <- tail(deleted_form_mls_gs,mls_last_n_games)
    l6_form_mls_gs <- as.numeric(l6_form_mls_gs)
    suml6_mls_gs[index_mls_gs] <- sum(l6_form_mls_gs)
    suml6_mls_gs[index_mls_gs] <- paste(suml6_mls_gs[index_mls_gs],sep = "")
    sum_mls_zero_gs[index_mls_gs] <- length(which(l6_form_mls_gs == 0))
    sum_mls_zero_gs[index_mls_gs] <- paste(sum_mls_zero_gs[index_mls_gs],sep = "")
    sum_mls_one_gs[index_mls_gs] <- length(which(l6_form_mls_gs == 1))
    sum_mls_one_gs[index_mls_gs] <- paste(sum_mls_one_gs[index_mls_gs],sep = "")
    sum_mls_two_gs[index_mls_gs] <- length(which(l6_form_mls_gs >= 2))
    sum_mls_two_gs[index_mls_gs] <- paste(sum_mls_two_gs[index_mls_gs],sep = "")
    sum_mls_three_gs[index_mls_gs] <- length(which(l6_form_mls_gs >= 3))
    sum_mls_three_gs[index_mls_gs] <- paste(sum_mls_three_gs[index_mls_gs],sep = "")
    avgr_mls_gs[index_mls_gs] <- mean(l6_form_mls_gs)
    avgr_mls_gs[index_mls_gs] <- paste(avgr_mls_gs[index_mls_gs],sep = "")
    sdr_mls_gs[index_mls_gs] <- sd(l6_form_mls_gs)
    sdr_mls_gs[index_mls_gs] <- paste(sdr_mls_gs[index_mls_gs],sep = "")
    l6_form_mls_gs <- as.character(l6_form_mls_gs)
    l6_form_mls_gs_flattened <- stri_paste(l6_form_mls_gs,collapse = '')
    l6_form_mls_gssplitted <- as.numeric(strsplit(as.character(l6_form_mls_gs_flattened),"")[[1]])
    final_mls_gs[index_mls_gs,index_mls_gs_cols] <- l6_form_mls_gssplitted[index_mls_gs_cols]
  }
}

final_mls_gs[is.na(final_mls_gs)] <- ""
mls_goalscoredmatrix <- cbind(mls_teams,final_mls_gs,suml6_mls_gs,sum_mls_zero_gs,sum_mls_one_gs,sum_mls_two_gs,sum_mls_three_gs,avgr_mls_gs,sdr_mls_gs)
####################################################################################################################################################
#arg
final_arg_gs <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_gs <- c()
sum_arg_zero_gs <- c()
sum_arg_one_gs <- c()
sum_arg_two_gs <- c()
sum_arg_three_gs <- c()
avgr_arg_gs <- c()
sdr_arg_gs <- c()
l6_form_arg_gssplitted <- c()
form_arg_gs <- c()
for(index_arg_gs in 1:length(arg_teams))
{
  for(index_arg_gs_cols in 1:arg_totalrounds)
  {
    index_arg_gs  <- row.names(arg_goalscored_h) == arg_teams[index_arg_gs]
    form_arg_gs <- arg_goalscored_h[index_arg_gs ]
    deleted_form_arg_gs <- form_arg_gs[!form_arg_gs[] == ""]
    l6_form_arg_gs <- tail(deleted_form_arg_gs,arg_last_n_games)
    l6_form_arg_gs <- as.numeric(l6_form_arg_gs)
    suml6_arg_gs[index_arg_gs] <- sum(l6_form_arg_gs)
    suml6_arg_gs[index_arg_gs] <- paste(suml6_arg_gs[index_arg_gs],sep = "")
    sum_arg_zero_gs[index_arg_gs] <- length(which(l6_form_arg_gs == 0))
    sum_arg_zero_gs[index_arg_gs] <- paste(sum_arg_zero_gs[index_arg_gs],sep = "")
    sum_arg_one_gs[index_arg_gs] <- length(which(l6_form_arg_gs == 1))
    sum_arg_one_gs[index_arg_gs] <- paste(sum_arg_one_gs[index_arg_gs],sep = "")
    sum_arg_two_gs[index_arg_gs] <- length(which(l6_form_arg_gs >= 2))
    sum_arg_two_gs[index_arg_gs] <- paste(sum_arg_two_gs[index_arg_gs],sep = "")
    sum_arg_three_gs[index_arg_gs] <- length(which(l6_form_arg_gs >= 3))
    sum_arg_three_gs[index_arg_gs] <- paste(sum_arg_three_gs[index_arg_gs],sep = "")
    avgr_arg_gs[index_arg_gs] <- mean(l6_form_arg_gs)
    avgr_arg_gs[index_arg_gs] <- paste(avgr_arg_gs[index_arg_gs],sep = "")
    sdr_arg_gs[index_arg_gs] <- sd(l6_form_arg_gs)
    sdr_arg_gs[index_arg_gs] <- paste(sdr_arg_gs[index_arg_gs],sep = "")
    l6_form_arg_gs <- as.character(l6_form_arg_gs)
    l6_form_arg_gs_flattened <- stri_paste(l6_form_arg_gs,collapse = '')
    l6_form_arg_gssplitted <- as.numeric(strsplit(as.character(l6_form_arg_gs_flattened),"")[[1]])
    final_arg_gs[index_arg_gs,index_arg_gs_cols] <- l6_form_arg_gssplitted[index_arg_gs_cols]
  }
}

final_arg_gs[is.na(final_arg_gs)] <- ""
arg_goalscoredmatrix <- cbind(arg_teams,final_arg_gs,suml6_arg_gs,sum_arg_zero_gs,sum_arg_one_gs,sum_arg_two_gs,sum_arg_three_gs,avgr_arg_gs,sdr_arg_gs)
#####################################################################################################################################################
#d2
# final_d2_gs <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_gs <- c()
# sum_d2_zero_gs <- c()
# sum_d2_one_gs <- c()
# sum_d2_two_gs <- c()
# sum_d2_three_gs <- c()
# avgr_d2_gs <- c()
# sdr_d2_gs <- c()
# l6_form_d2_gssplitted <- c()
# form_d2_gs <- c()
# for(index_d2_gs in 1:length(d2_teams))
# {
#   for(index_d2_gs_cols in 1:d2_totalrounds)
#   {
#     index_d2_gs  <- row.names(d2_goalscored_h) == d2_teams[index_d2_gs]
#     form_d2_gs <- d2_goalscored_h[index_d2_gs ]
#     deleted_form_d2_gs <- form_d2_gs[!form_d2_gs[] == ""]
#     l6_form_d2_gs <- tail(deleted_form_d2_gs,d2_last_n_games)
#     l6_form_d2_gs <- as.numeric(l6_form_d2_gs)
#     suml6_d2_gs[index_d2_gs] <- sum(l6_form_d2_gs)
#     suml6_d2_gs[index_d2_gs] <- paste(suml6_d2_gs[index_d2_gs],sep = "")
#     sum_d2_zero_gs[index_d2_gs] <- length(which(l6_form_d2_gs == 0))
#     sum_d2_zero_gs[index_d2_gs] <- paste(sum_d2_zero_gs[index_d2_gs],sep = "")
#     sum_d2_one_gs[index_d2_gs] <- length(which(l6_form_d2_gs == 1))
#     sum_d2_one_gs[index_d2_gs] <- paste(sum_d2_one_gs[index_d2_gs],sep = "")
#     sum_d2_two_gs[index_d2_gs] <- length(which(l6_form_d2_gs >= 2))
#     sum_d2_two_gs[index_d2_gs] <- paste(sum_d2_two_gs[index_d2_gs],sep = "")
#     sum_d2_three_gs[index_d2_gs] <- length(which(l6_form_d2_gs >= 3))
#     sum_d2_three_gs[index_d2_gs] <- paste(sum_d2_three_gs[index_d2_gs],sep = "")
#     avgr_d2_gs[index_d2_gs] <- mean(l6_form_d2_gs)
#     avgr_d2_gs[index_d2_gs] <- paste(avgr_d2_gs[index_d2_gs],sep = "")
#     sdr_d2_gs[index_d2_gs] <- sd(l6_form_d2_gs)
#     sdr_d2_gs[index_d2_gs] <- paste(sdr_d2_gs[index_d2_gs],sep = "")
#     l6_form_d2_gs <- as.character(l6_form_d2_gs)
#     l6_form_d2_gs_flattened <- stri_paste(l6_form_d2_gs,collapse = '')
#     l6_form_d2_gssplitted <- as.numeric(strsplit(as.character(l6_form_d2_gs_flattened),"")[[1]])
#     final_d2_gs[index_d2_gs,index_d2_gs_cols] <- l6_form_d2_gssplitted[index_d2_gs_cols]
#   }
# }
#
# final_d2_gs[is.na(final_d2_gs)] <- ""
# d2_goalscoredmatrix <- cbind(d2_teams,final_d2_gs,suml6_d2_gs,sum_d2_zero_gs,sum_d2_one_gs,sum_d2_two_gs,sum_d2_three_gs,avgr_d2_gs,sdr_d2_gs)
# ###############################################################################################################################################
# #e0
# final_e0_gs <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_gs <- c()
# sum_e0_zero_gs <- c()
# sum_e0_one_gs <- c()
# sum_e0_two_gs <- c()
# sum_e0_three_gs <- c()
# avgr_e0_gs <- c()
# sdr_e0_gs <- c()
# l6_form_e0_gssplitted <- c()
# form_e0_gs <- c()
# for(index_e0_gs in 1:length(e0_teams))
# {
#   for(index_e0_gs_cols in 1:e0_totalrounds)
#   {
#     index_e0_gs  <- row.names(e0_goalscored_h) == e0_teams[index_e0_gs]
#     form_e0_gs <- e0_goalscored_h[index_e0_gs ]
#     deleted_form_e0_gs <- form_e0_gs[!form_e0_gs[] == ""]
#     l6_form_e0_gs <- tail(deleted_form_e0_gs,e0_last_n_games)
#     l6_form_e0_gs <- as.numeric(l6_form_e0_gs)
#     suml6_e0_gs[index_e0_gs] <- sum(l6_form_e0_gs)
#     suml6_e0_gs[index_e0_gs] <- paste(suml6_e0_gs[index_e0_gs],sep = "")
#     sum_e0_zero_gs[index_e0_gs] <- length(which(l6_form_e0_gs == 0))
#     sum_e0_zero_gs[index_e0_gs] <- paste(sum_e0_zero_gs[index_e0_gs],sep = "")
#     sum_e0_one_gs[index_e0_gs] <- length(which(l6_form_e0_gs == 1))
#     sum_e0_one_gs[index_e0_gs] <- paste(sum_e0_one_gs[index_e0_gs],sep = "")
#     sum_e0_two_gs[index_e0_gs] <- length(which(l6_form_e0_gs >= 2))
#     sum_e0_two_gs[index_e0_gs] <- paste(sum_e0_two_gs[index_e0_gs],sep = "")
#     sum_e0_three_gs[index_e0_gs] <- length(which(l6_form_e0_gs >= 3))
#     sum_e0_three_gs[index_e0_gs] <- paste(sum_e0_three_gs[index_e0_gs],sep = "")
#     avgr_e0_gs[index_e0_gs] <- mean(l6_form_e0_gs)
#     avgr_e0_gs[index_e0_gs] <- paste(avgr_e0_gs[index_e0_gs],sep = "")
#     sdr_e0_gs[index_e0_gs] <- sd(l6_form_e0_gs)
#     sdr_e0_gs[index_e0_gs] <- paste(sdr_e0_gs[index_e0_gs],sep = "")
#     l6_form_e0_gs <- as.character(l6_form_e0_gs)
#     l6_form_e0_gs_flattened <- stri_paste(l6_form_e0_gs,collapse = '')
#     l6_form_e0_gssplitted <- as.numeric(strsplit(as.character(l6_form_e0_gs_flattened),"")[[1]])
#     final_e0_gs[index_e0_gs,index_e0_gs_cols] <- l6_form_e0_gssplitted[index_e0_gs_cols]
#   }
# }
#
# final_e0_gs[is.na(final_e0_gs)] <- ""
# e0_goalscoredmatrix <- cbind(e0_teams,final_e0_gs,suml6_e0_gs,sum_e0_zero_gs,sum_e0_one_gs,sum_e0_two_gs,sum_e0_three_gs,avgr_e0_gs,sdr_e0_gs)
# ################################################################################################################################################
# #e1
# final_e1_gs <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_gs <- c()
# sum_e1_zero_gs <- c()
# sum_e1_one_gs <- c()
# sum_e1_two_gs <- c()
# sum_e1_three_gs <- c()
# avgr_e1_gs <- c()
# sdr_e1_gs <- c()
# l6_form_e1_gssplitted <- c()
# form_e1_gs <- c()
# for(index_e1_gs in 1:length(e1_teams))
# {
#   for(index_e1_gs_cols in 1:e1_totalrounds)
#   {
#     index_e1_gs  <- row.names(e1_goalscored_h) == e1_teams[index_e1_gs]
#     form_e1_gs <- e1_goalscored_h[index_e1_gs ]
#     deleted_form_e1_gs <- form_e1_gs[!form_e1_gs[] == ""]
#     l6_form_e1_gs <- tail(deleted_form_e1_gs,e1_last_n_games)
#     l6_form_e1_gs <- as.numeric(l6_form_e1_gs)
#     suml6_e1_gs[index_e1_gs] <- sum(l6_form_e1_gs)
#     suml6_e1_gs[index_e1_gs] <- paste(suml6_e1_gs[index_e1_gs],sep = "")
#     sum_e1_zero_gs[index_e1_gs] <- length(which(l6_form_e1_gs == 0))
#     sum_e1_zero_gs[index_e1_gs] <- paste(sum_e1_zero_gs[index_e1_gs],sep = "")
#     sum_e1_one_gs[index_e1_gs] <- length(which(l6_form_e1_gs == 1))
#     sum_e1_one_gs[index_e1_gs] <- paste(sum_e1_one_gs[index_e1_gs],sep = "")
#     sum_e1_two_gs[index_e1_gs] <- length(which(l6_form_e1_gs >= 2))
#     sum_e1_two_gs[index_e1_gs] <- paste(sum_e1_two_gs[index_e1_gs],sep = "")
#     sum_e1_three_gs[index_e1_gs] <- length(which(l6_form_e1_gs >= 3))
#     sum_e1_three_gs[index_e1_gs] <- paste(sum_e1_three_gs[index_e1_gs],sep = "")
#     avgr_e1_gs[index_e1_gs] <- mean(l6_form_e1_gs)
#     avgr_e1_gs[index_e1_gs] <- paste(avgr_e1_gs[index_e1_gs],sep = "")
#     sdr_e1_gs[index_e1_gs] <- sd(l6_form_e1_gs)
#     sdr_e1_gs[index_e1_gs] <- paste(sdr_e1_gs[index_e1_gs],sep = "")
#     l6_form_e1_gs <- as.character(l6_form_e1_gs)
#     l6_form_e1_gs_flattened <- stri_paste(l6_form_e1_gs,collapse = '')
#     l6_form_e1_gssplitted <- as.numeric(strsplit(as.character(l6_form_e1_gs_flattened),"")[[1]])
#     final_e1_gs[index_e1_gs,index_e1_gs_cols] <- l6_form_e1_gssplitted[index_e1_gs_cols]
#   }
# }
#
# final_e1_gs[is.na(final_e1_gs)] <- ""
# e1_goalscoredmatrix <- cbind(e1_teams,final_e1_gs,suml6_e1_gs,sum_e1_zero_gs,sum_e1_one_gs,sum_e1_two_gs,sum_e1_three_gs,avgr_e1_gs,sdr_e1_gs)
# ###############################################################################################################################################
# #e2
# final_e2_gs <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_gs <- c()
# sum_e2_zero_gs <- c()
# sum_e2_one_gs <- c()
# sum_e2_two_gs <- c()
# sum_e2_three_gs <- c()
# avgr_e2_gs <- c()
# sdr_e2_gs <- c()
# l6_form_e2_gssplitted <- c()
# form_e2_gs <- c()
# for(index_e2_gs in 1:length(e2_teams))
# {
#   for(index_e2_gs_cols in 1:e2_totalrounds)
#   {
#     index_e2_gs  <- row.names(e2_goalscored_h) == e2_teams[index_e2_gs]
#     form_e2_gs <- e2_goalscored_h[index_e2_gs ]
#     deleted_form_e2_gs <- form_e2_gs[!form_e2_gs[] == ""]
#     l6_form_e2_gs <- tail(deleted_form_e2_gs,e2_last_n_games)
#     l6_form_e2_gs <- as.numeric(l6_form_e2_gs)
#     suml6_e2_gs[index_e2_gs] <- sum(l6_form_e2_gs)
#     suml6_e2_gs[index_e2_gs] <- paste(suml6_e2_gs[index_e2_gs],sep = "")
#     sum_e2_zero_gs[index_e2_gs] <- length(which(l6_form_e2_gs == 0))
#     sum_e2_zero_gs[index_e2_gs] <- paste(sum_e2_zero_gs[index_e2_gs],sep = "")
#     sum_e2_one_gs[index_e2_gs] <- length(which(l6_form_e2_gs == 1))
#     sum_e2_one_gs[index_e2_gs] <- paste(sum_e2_one_gs[index_e2_gs],sep = "")
#     sum_e2_two_gs[index_e2_gs] <- length(which(l6_form_e2_gs >= 2))
#     sum_e2_two_gs[index_e2_gs] <- paste(sum_e2_two_gs[index_e2_gs],sep = "")
#     sum_e2_three_gs[index_e2_gs] <- length(which(l6_form_e2_gs >= 3))
#     sum_e2_three_gs[index_e2_gs] <- paste(sum_e2_three_gs[index_e2_gs],sep = "")
#     avgr_e2_gs[index_e2_gs] <- mean(l6_form_e2_gs)
#     avgr_e2_gs[index_e2_gs] <- paste(avgr_e2_gs[index_e2_gs],sep = "")
#     sdr_e2_gs[index_e2_gs] <- sd(l6_form_e2_gs)
#     sdr_e2_gs[index_e2_gs] <- paste(sdr_e2_gs[index_e2_gs],sep = "")
#     l6_form_e2_gs <- as.character(l6_form_e2_gs)
#     l6_form_e2_gs_flattened <- stri_paste(l6_form_e2_gs,collapse = '')
#     l6_form_e2_gssplitted <- as.numeric(strsplit(as.character(l6_form_e2_gs_flattened),"")[[1]])
#     final_e2_gs[index_e2_gs,index_e2_gs_cols] <- l6_form_e2_gssplitted[index_e2_gs_cols]
#   }
# }
#
# final_e2_gs[is.na(final_e2_gs)] <- ""
# e2_goalscoredmatrix <- cbind(e2_teams,final_e2_gs,suml6_e2_gs,sum_e2_zero_gs,sum_e2_one_gs,sum_e2_two_gs,sum_e2_three_gs,avgr_e2_gs,sdr_e2_gs)
# ##############################################################################################################################################
# #e3
# final_e3_gs <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_gs <- c()
# sum_e3_zero_gs <- c()
# sum_e3_one_gs <- c()
# sum_e3_two_gs <- c()
# sum_e3_three_gs <- c()
# avgr_e3_gs <- c()
# sdr_e3_gs <- c()
# l6_form_e3_gssplitted <- c()
# form_e3_gs <- c()
# for(index_e3_gs in 1:length(e3_teams))
# {
#   for(index_e3_gs_cols in 1:e3_totalrounds)
#   {
#     index_e3_gs  <- row.names(e3_goalscored_h) == e3_teams[index_e3_gs]
#     form_e3_gs <- e3_goalscored_h[index_e3_gs ]
#     deleted_form_e3_gs <- form_e3_gs[!form_e3_gs[] == ""]
#     l6_form_e3_gs <- tail(deleted_form_e3_gs,e3_last_n_games)
#     l6_form_e3_gs <- as.numeric(l6_form_e3_gs)
#     suml6_e3_gs[index_e3_gs] <- sum(l6_form_e3_gs)
#     suml6_e3_gs[index_e3_gs] <- paste(suml6_e3_gs[index_e3_gs],sep = "")
#     sum_e3_zero_gs[index_e3_gs] <- length(which(l6_form_e3_gs == 0))
#     sum_e3_zero_gs[index_e3_gs] <- paste(sum_e3_zero_gs[index_e3_gs],sep = "")
#     sum_e3_one_gs[index_e3_gs] <- length(which(l6_form_e3_gs == 1))
#     sum_e3_one_gs[index_e3_gs] <- paste(sum_e3_one_gs[index_e3_gs],sep = "")
#     sum_e3_two_gs[index_e3_gs] <- length(which(l6_form_e3_gs >= 2))
#     sum_e3_two_gs[index_e3_gs] <- paste(sum_e3_two_gs[index_e3_gs],sep = "")
#     sum_e3_three_gs[index_e3_gs] <- length(which(l6_form_e3_gs >= 3))
#     sum_e3_three_gs[index_e3_gs] <- paste(sum_e3_three_gs[index_e3_gs],sep = "")
#     avgr_e3_gs[index_e3_gs] <- mean(l6_form_e3_gs)
#     avgr_e3_gs[index_e3_gs] <- paste(avgr_e3_gs[index_e3_gs],sep = "")
#     sdr_e3_gs[index_e3_gs] <- sd(l6_form_e3_gs)
#     sdr_e3_gs[index_e3_gs] <- paste(sdr_e3_gs[index_e3_gs],sep = "")
#     l6_form_e3_gs <- as.character(l6_form_e3_gs)
#     l6_form_e3_gs_flattened <- stri_paste(l6_form_e3_gs,collapse = '')
#     l6_form_e3_gssplitted <- as.numeric(strsplit(as.character(l6_form_e3_gs_flattened),"")[[1]])
#     final_e3_gs[index_e3_gs,index_e3_gs_cols] <- l6_form_e3_gssplitted[index_e3_gs_cols]
#   }
# }
#
# final_e3_gs[is.na(final_e3_gs)] <- ""
# e3_goalscoredmatrix <- cbind(e3_teams,final_e3_gs,suml6_e3_gs,sum_e3_zero_gs,sum_e3_one_gs,sum_e3_two_gs,sum_e3_three_gs,avgr_e3_gs,sdr_e3_gs)
# #################################################################################################################################################
# #ec
# final_ec_gs <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_gs <- c()
# sum_ec_zero_gs <- c()
# sum_ec_one_gs <- c()
# sum_ec_two_gs <- c()
# sum_ec_three_gs <- c()
# avgr_ec_gs <- c()
# sdr_ec_gs <- c()
# l6_form_ec_gssplitted <- c()
# form_ec_gs <- c()
# for(index_ec_gs in 1:length(ec_teams))
# {
#   for(index_ec_gs_cols in 1:ec_totalrounds)
#   {
#     index_ec_gs  <- row.names(ec_goalscored_h) == ec_teams[index_ec_gs]
#     form_ec_gs <- ec_goalscored_h[index_ec_gs ]
#     deleted_form_ec_gs <- form_ec_gs[!form_ec_gs[] == ""]
#     l6_form_ec_gs <- tail(deleted_form_ec_gs,ec_last_n_games)
#     l6_form_ec_gs <- as.numeric(l6_form_ec_gs)
#     suml6_ec_gs[index_ec_gs] <- sum(l6_form_ec_gs)
#     suml6_ec_gs[index_ec_gs] <- paste(suml6_ec_gs[index_ec_gs],sep = "")
#     sum_ec_zero_gs[index_ec_gs] <- length(which(l6_form_ec_gs == 0))
#     sum_ec_zero_gs[index_ec_gs] <- paste(sum_ec_zero_gs[index_ec_gs],sep = "")
#     sum_ec_one_gs[index_ec_gs] <- length(which(l6_form_ec_gs == 1))
#     sum_ec_one_gs[index_ec_gs] <- paste(sum_ec_one_gs[index_ec_gs],sep = "")
#     sum_ec_two_gs[index_ec_gs] <- length(which(l6_form_ec_gs >= 2))
#     sum_ec_two_gs[index_ec_gs] <- paste(sum_ec_two_gs[index_ec_gs],sep = "")
#     sum_ec_three_gs[index_ec_gs] <- length(which(l6_form_ec_gs >= 3))
#     sum_ec_three_gs[index_ec_gs] <- paste(sum_ec_three_gs[index_ec_gs],sep = "")
#     avgr_ec_gs[index_ec_gs] <- mean(l6_form_ec_gs)
#     avgr_ec_gs[index_ec_gs] <- paste(avgr_ec_gs[index_ec_gs],sep = "")
#     sdr_ec_gs[index_ec_gs] <- sd(l6_form_ec_gs)
#     sdr_ec_gs[index_ec_gs] <- paste(sdr_ec_gs[index_ec_gs],sep = "")
#     l6_form_ec_gs <- as.character(l6_form_ec_gs)
#     l6_form_ec_gs_flattened <- stri_paste(l6_form_ec_gs,collapse = '')
#     l6_form_ec_gssplitted <- as.numeric(strsplit(as.character(l6_form_ec_gs_flattened),"")[[1]])
#     final_ec_gs[index_ec_gs,index_ec_gs_cols] <- l6_form_ec_gssplitted[index_ec_gs_cols]
#   }
# }
#
# final_ec_gs[is.na(final_ec_gs)] <- ""
# ec_goalscoredmatrix <- cbind(ec_teams,final_ec_gs,suml6_ec_gs,sum_ec_zero_gs,sum_ec_one_gs,sum_ec_two_gs,sum_ec_three_gs,avgr_ec_gs,sdr_ec_gs)
# ###############################################################################################################################################
# #f1
# final_f1_gs <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_gs <- c()
# sum_f1_zero_gs <- c()
# sum_f1_one_gs <- c()
# sum_f1_two_gs <- c()
# sum_f1_three_gs <- c()
# avgr_f1_gs <- c()
# sdr_f1_gs <- c()
# l6_form_f1_gssplitted <- c()
# form_f1_gs <- c()
# for(index_f1_gs in 1:length(f1_teams))
# {
#   for(index_f1_gs_cols in 1:f1_totalrounds)
#   {
#     index_f1_gs  <- row.names(f1_goalscored_h) == f1_teams[index_f1_gs]
#     form_f1_gs <- f1_goalscored_h[index_f1_gs ]
#     deleted_form_f1_gs <- form_f1_gs[!form_f1_gs[] == ""]
#     l6_form_f1_gs <- tail(deleted_form_f1_gs,f1_last_n_games)
#     l6_form_f1_gs <- as.numeric(l6_form_f1_gs)
#     suml6_f1_gs[index_f1_gs] <- sum(l6_form_f1_gs)
#     suml6_f1_gs[index_f1_gs] <- paste(suml6_f1_gs[index_f1_gs],sep = "")
#     sum_f1_zero_gs[index_f1_gs] <- length(which(l6_form_f1_gs == 0))
#     sum_f1_zero_gs[index_f1_gs] <- paste(sum_f1_zero_gs[index_f1_gs],sep = "")
#     sum_f1_one_gs[index_f1_gs] <- length(which(l6_form_f1_gs == 1))
#     sum_f1_one_gs[index_f1_gs] <- paste(sum_f1_one_gs[index_f1_gs],sep = "")
#     sum_f1_two_gs[index_f1_gs] <- length(which(l6_form_f1_gs >= 2))
#     sum_f1_two_gs[index_f1_gs] <- paste(sum_f1_two_gs[index_f1_gs],sep = "")
#     sum_f1_three_gs[index_f1_gs] <- length(which(l6_form_f1_gs >= 3))
#     sum_f1_three_gs[index_f1_gs] <- paste(sum_f1_three_gs[index_f1_gs],sep = "")
#     avgr_f1_gs[index_f1_gs] <- mean(l6_form_f1_gs)
#     avgr_f1_gs[index_f1_gs] <- paste(avgr_f1_gs[index_f1_gs],sep = "")
#     sdr_f1_gs[index_f1_gs] <- sd(l6_form_f1_gs)
#     sdr_f1_gs[index_f1_gs] <- paste(sdr_f1_gs[index_f1_gs],sep = "")
#     l6_form_f1_gs <- as.character(l6_form_f1_gs)
#     l6_form_f1_gs_flattened <- stri_paste(l6_form_f1_gs,collapse = '')
#     l6_form_f1_gssplitted <- as.numeric(strsplit(as.character(l6_form_f1_gs_flattened),"")[[1]])
#     final_f1_gs[index_f1_gs,index_f1_gs_cols] <- l6_form_f1_gssplitted[index_f1_gs_cols]
#   }
# }
#
# final_f1_gs[is.na(final_f1_gs)] <- ""
# f1_goalscoredmatrix <- cbind(f1_teams,final_f1_gs,suml6_f1_gs,sum_f1_zero_gs,sum_f1_one_gs,sum_f1_two_gs,sum_f1_three_gs,avgr_f1_gs,sdr_f1_gs)
# ################################################################################################################################################
# #f2
# final_f2_gs <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_gs <- c()
# sum_f2_zero_gs <- c()
# sum_f2_one_gs <- c()
# sum_f2_two_gs <- c()
# sum_f2_three_gs <- c()
# avgr_f2_gs <- c()
# sdr_f2_gs <- c()
# l6_form_f2_gssplitted <- c()
# form_f2_gs <- c()
# for(index_f2_gs in 1:length(f2_teams))
# {
#   for(index_f2_gs_cols in 1:f2_totalrounds)
#   {
#     index_f2_gs  <- row.names(f2_goalscored_h) == f2_teams[index_f2_gs]
#     form_f2_gs <- f2_goalscored_h[index_f2_gs ]
#     deleted_form_f2_gs <- form_f2_gs[!form_f2_gs[] == ""]
#     l6_form_f2_gs <- tail(deleted_form_f2_gs,f2_last_n_games)
#     l6_form_f2_gs <- as.numeric(l6_form_f2_gs)
#     suml6_f2_gs[index_f2_gs] <- sum(l6_form_f2_gs)
#     suml6_f2_gs[index_f2_gs] <- paste(suml6_f2_gs[index_f2_gs],sep = "")
#     sum_f2_zero_gs[index_f2_gs] <- length(which(l6_form_f2_gs == 0))
#     sum_f2_zero_gs[index_f2_gs] <- paste(sum_f2_zero_gs[index_f2_gs],sep = "")
#     sum_f2_one_gs[index_f2_gs] <- length(which(l6_form_f2_gs == 1))
#     sum_f2_one_gs[index_f2_gs] <- paste(sum_f2_one_gs[index_f2_gs],sep = "")
#     sum_f2_two_gs[index_f2_gs] <- length(which(l6_form_f2_gs >= 2))
#     sum_f2_two_gs[index_f2_gs] <- paste(sum_f2_two_gs[index_f2_gs],sep = "")
#     sum_f2_three_gs[index_f2_gs] <- length(which(l6_form_f2_gs >= 3))
#     sum_f2_three_gs[index_f2_gs] <- paste(sum_f2_three_gs[index_f2_gs],sep = "")
#     avgr_f2_gs[index_f2_gs] <- mean(l6_form_f2_gs)
#     avgr_f2_gs[index_f2_gs] <- paste(avgr_f2_gs[index_f2_gs],sep = "")
#     sdr_f2_gs[index_f2_gs] <- sd(l6_form_f2_gs)
#     sdr_f2_gs[index_f2_gs] <- paste(sdr_f2_gs[index_f2_gs],sep = "")
#     l6_form_f2_gs <- as.character(l6_form_f2_gs)
#     l6_form_f2_gs_flattened <- stri_paste(l6_form_f2_gs,collapse = '')
#     l6_form_f2_gssplitted <- as.numeric(strsplit(as.character(l6_form_f2_gs_flattened),"")[[1]])
#     final_f2_gs[index_f2_gs,index_f2_gs_cols] <- l6_form_f2_gssplitted[index_f2_gs_cols]
#   }
# }
#
# final_f2_gs[is.na(final_f2_gs)] <- ""
# f2_goalscoredmatrix <- cbind(f2_teams,final_f2_gs,suml6_f2_gs,sum_f2_zero_gs,sum_f2_one_gs,sum_f2_two_gs,sum_f2_three_gs,avgr_f2_gs,sdr_f2_gs)
# ###############################################################################################################################################
# #g1
# final_g1_gs <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_gs <- c()
# sum_g1_zero_gs <- c()
# sum_g1_one_gs <- c()
# sum_g1_two_gs <- c()
# sum_g1_three_gs <- c()
# avgr_g1_gs <- c()
# sdr_g1_gs <- c()
# l6_form_g1_gssplitted <- c()
# form_g1_gs <- c()
# for(index_g1_gs in 1:length(g1_teams))
# {
#   for(index_g1_gs_cols in 1:g1_totalrounds)
#   {
#     index_g1_gs  <- row.names(g1_goalscored_h) == g1_teams[index_g1_gs]
#     form_g1_gs <- g1_goalscored_h[index_g1_gs ]
#     deleted_form_g1_gs <- form_g1_gs[!form_g1_gs[] == ""]
#     l6_form_g1_gs <- tail(deleted_form_g1_gs,g1_last_n_games)
#     l6_form_g1_gs <- as.numeric(l6_form_g1_gs)
#     suml6_g1_gs[index_g1_gs] <- sum(l6_form_g1_gs)
#     suml6_g1_gs[index_g1_gs] <- paste(suml6_g1_gs[index_g1_gs],sep = "")
#     sum_g1_zero_gs[index_g1_gs] <- length(which(l6_form_g1_gs == 0))
#     sum_g1_zero_gs[index_g1_gs] <- paste(sum_g1_zero_gs[index_g1_gs],sep = "")
#     sum_g1_one_gs[index_g1_gs] <- length(which(l6_form_g1_gs == 1))
#     sum_g1_one_gs[index_g1_gs] <- paste(sum_g1_one_gs[index_g1_gs],sep = "")
#     sum_g1_two_gs[index_g1_gs] <- length(which(l6_form_g1_gs >= 2))
#     sum_g1_two_gs[index_g1_gs] <- paste(sum_g1_two_gs[index_g1_gs],sep = "")
#     sum_g1_three_gs[index_g1_gs] <- length(which(l6_form_g1_gs >= 3))
#     sum_g1_three_gs[index_g1_gs] <- paste(sum_g1_three_gs[index_g1_gs],sep = "")
#     avgr_g1_gs[index_g1_gs] <- mean(l6_form_g1_gs)
#     avgr_g1_gs[index_g1_gs] <- paste(avgr_g1_gs[index_g1_gs],sep = "")
#     sdr_g1_gs[index_g1_gs] <- sd(l6_form_g1_gs)
#     sdr_g1_gs[index_g1_gs] <- paste(sdr_g1_gs[index_g1_gs],sep = "")
#     l6_form_g1_gs <- as.character(l6_form_g1_gs)
#     l6_form_g1_gs_flattened <- stri_paste(l6_form_g1_gs,collapse = '')
#     l6_form_g1_gssplitted <- as.numeric(strsplit(as.character(l6_form_g1_gs_flattened),"")[[1]])
#     final_g1_gs[index_g1_gs,index_g1_gs_cols] <- l6_form_g1_gssplitted[index_g1_gs_cols]
#   }
# }
#
# final_g1_gs[is.na(final_g1_gs)] <- ""
# g1_goalscoredmatrix <- cbind(g1_teams,final_g1_gs,suml6_g1_gs,sum_g1_zero_gs,sum_g1_one_gs,sum_g1_two_gs,sum_g1_three_gs,avgr_g1_gs,sdr_g1_gs)
# ##################################################################################################################################################
# #i1
# final_i1_gs <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_gs <- c()
# sum_i1_zero_gs <- c()
# sum_i1_one_gs <- c()
# sum_i1_two_gs <- c()
# sum_i1_three_gs <- c()
# avgr_i1_gs <- c()
# sdr_i1_gs <- c()
# l6_form_i1_gssplitted <- c()
# form_i1_gs <- c()
# for(index_i1_gs in 1:length(i1_teams))
# {
#   for(index_i1_gs_cols in 1:i1_totalrounds)
#   {
#     index_i1_gs  <- row.names(i1_goalscored_h) == i1_teams[index_i1_gs]
#     form_i1_gs <- i1_goalscored_h[index_i1_gs ]
#     deleted_form_i1_gs <- form_i1_gs[!form_i1_gs[] == ""]
#     l6_form_i1_gs <- tail(deleted_form_i1_gs,i1_last_n_games)
#     l6_form_i1_gs <- as.numeric(l6_form_i1_gs)
#     suml6_i1_gs[index_i1_gs] <- sum(l6_form_i1_gs)
#     suml6_i1_gs[index_i1_gs] <- paste(suml6_i1_gs[index_i1_gs],sep = "")
#     sum_i1_zero_gs[index_i1_gs] <- length(which(l6_form_i1_gs == 0))
#     sum_i1_zero_gs[index_i1_gs] <- paste(sum_i1_zero_gs[index_i1_gs],sep = "")
#     sum_i1_one_gs[index_i1_gs] <- length(which(l6_form_i1_gs == 1))
#     sum_i1_one_gs[index_i1_gs] <- paste(sum_i1_one_gs[index_i1_gs],sep = "")
#     sum_i1_two_gs[index_i1_gs] <- length(which(l6_form_i1_gs >= 2))
#     sum_i1_two_gs[index_i1_gs] <- paste(sum_i1_two_gs[index_i1_gs],sep = "")
#     sum_i1_three_gs[index_i1_gs] <- length(which(l6_form_i1_gs >= 3))
#     sum_i1_three_gs[index_i1_gs] <- paste(sum_i1_three_gs[index_i1_gs],sep = "")
#     avgr_i1_gs[index_i1_gs] <- mean(l6_form_i1_gs)
#     avgr_i1_gs[index_i1_gs] <- paste(avgr_i1_gs[index_i1_gs],sep = "")
#     sdr_i1_gs[index_i1_gs] <- sd(l6_form_i1_gs)
#     sdr_i1_gs[index_i1_gs] <- paste(sdr_i1_gs[index_i1_gs],sep = "")
#     l6_form_i1_gs <- as.character(l6_form_i1_gs)
#     l6_form_i1_gs_flattened <- stri_paste(l6_form_i1_gs,collapse = '')
#     l6_form_i1_gssplitted <- as.numeric(strsplit(as.character(l6_form_i1_gs_flattened),"")[[1]])
#     final_i1_gs[index_i1_gs,index_i1_gs_cols] <- l6_form_i1_gssplitted[index_i1_gs_cols]
#   }
# }
#
# final_i1_gs[is.na(final_i1_gs)] <- ""
# i1_goalscoredmatrix <- cbind(i1_teams,final_i1_gs,suml6_i1_gs,sum_i1_zero_gs,sum_i1_one_gs,sum_i1_two_gs,sum_i1_three_gs,avgr_i1_gs,sdr_i1_gs)
# ###############################################################################################################################################
# ##i2
# final_i2_gs <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_gs <- c()
# sum_i2_zero_gs <- c()
# sum_i2_one_gs <- c()
# sum_i2_two_gs <- c()
# sum_i2_three_gs <- c()
# avgr_i2_gs <- c()
# sdr_i2_gs <- c()
# l6_form_i2_gssplitted <- c()
# form_i2_gs <- c()
# for(index_i2_gs in 1:length(i2_teams))
# {
#   for(index_i2_gs_cols in 1:i2_totalrounds)
#   {
#     index_i2_gs  <- row.names(i2_goalscored_h) == i2_teams[index_i2_gs]
#     form_i2_gs <- i2_goalscored_h[index_i2_gs ]
#     deleted_form_i2_gs <- form_i2_gs[!form_i2_gs[] == ""]
#     l6_form_i2_gs <- tail(deleted_form_i2_gs,i2_last_n_games)
#     l6_form_i2_gs <- as.numeric(l6_form_i2_gs)
#     suml6_i2_gs[index_i2_gs] <- sum(l6_form_i2_gs)
#     suml6_i2_gs[index_i2_gs] <- paste(suml6_i2_gs[index_i2_gs],sep = "")
#     sum_i2_zero_gs[index_i2_gs] <- length(which(l6_form_i2_gs == 0))
#     sum_i2_zero_gs[index_i2_gs] <- paste(sum_i2_zero_gs[index_i2_gs],sep = "")
#     sum_i2_one_gs[index_i2_gs] <- length(which(l6_form_i2_gs == 1))
#     sum_i2_one_gs[index_i2_gs] <- paste(sum_i2_one_gs[index_i2_gs],sep = "")
#     sum_i2_two_gs[index_i2_gs] <- length(which(l6_form_i2_gs >= 2))
#     sum_i2_two_gs[index_i2_gs] <- paste(sum_i2_two_gs[index_i2_gs],sep = "")
#     sum_i2_three_gs[index_i2_gs] <- length(which(l6_form_i2_gs >= 3))
#     sum_i2_three_gs[index_i2_gs] <- paste(sum_i2_three_gs[index_i2_gs],sep = "")
#     avgr_i2_gs[index_i2_gs] <- mean(l6_form_i2_gs)
#     avgr_i2_gs[index_i2_gs] <- paste(avgr_i2_gs[index_i2_gs],sep = "")
#     sdr_i2_gs[index_i2_gs] <- sd(l6_form_i2_gs)
#     sdr_i2_gs[index_i2_gs] <- paste(sdr_i2_gs[index_i2_gs],sep = "")
#     l6_form_i2_gs <- as.character(l6_form_i2_gs)
#     l6_form_i2_gs_flattened <- stri_paste(l6_form_i2_gs,collapse = '')
#     l6_form_i2_gssplitted <- as.numeric(strsplit(as.character(l6_form_i2_gs_flattened),"")[[1]])
#     final_i2_gs[index_i2_gs,index_i2_gs_cols] <- l6_form_i2_gssplitted[index_i2_gs_cols]
#   }
# }
#
# final_i2_gs[is.na(final_i2_gs)] <- ""
# i2_goalscoredmatrix <- cbind(i2_teams,final_i2_gs,suml6_i2_gs,sum_i2_zero_gs,sum_i2_one_gs,sum_i2_two_gs,sum_i2_three_gs,avgr_i2_gs,sdr_i2_gs)
# ################################################################################################################################################
# #n1
# final_n1_gs <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_gs <- c()
# sum_n1_zero_gs <- c()
# sum_n1_one_gs <- c()
# sum_n1_two_gs <- c()
# sum_n1_three_gs <- c()
# avgr_n1_gs <- c()
# sdr_n1_gs <- c()
# l6_form_n1_gssplitted <- c()
# form_n1_gs <- c()
# for(index_n1_gs in 1:length(n1_teams))
# {
#   for(index_n1_gs_cols in 1:n1_totalrounds)
#   {
#     index_n1_gs  <- row.names(n1_goalscored_h) == n1_teams[index_n1_gs]
#     form_n1_gs <- n1_goalscored_h[index_n1_gs ]
#     deleted_form_n1_gs <- form_n1_gs[!form_n1_gs[] == ""]
#     l6_form_n1_gs <- tail(deleted_form_n1_gs,n1_last_n_games)
#     l6_form_n1_gs <- as.numeric(l6_form_n1_gs)
#     suml6_n1_gs[index_n1_gs] <- sum(l6_form_n1_gs)
#     suml6_n1_gs[index_n1_gs] <- paste(suml6_n1_gs[index_n1_gs],sep = "")
#     sum_n1_zero_gs[index_n1_gs] <- length(which(l6_form_n1_gs == 0))
#     sum_n1_zero_gs[index_n1_gs] <- paste(sum_n1_zero_gs[index_n1_gs],sep = "")
#     sum_n1_one_gs[index_n1_gs] <- length(which(l6_form_n1_gs == 1))
#     sum_n1_one_gs[index_n1_gs] <- paste(sum_n1_one_gs[index_n1_gs],sep = "")
#     sum_n1_two_gs[index_n1_gs] <- length(which(l6_form_n1_gs >= 2))
#     sum_n1_two_gs[index_n1_gs] <- paste(sum_n1_two_gs[index_n1_gs],sep = "")
#     sum_n1_three_gs[index_n1_gs] <- length(which(l6_form_n1_gs >= 3))
#     sum_n1_three_gs[index_n1_gs] <- paste(sum_n1_three_gs[index_n1_gs],sep = "")
#     avgr_n1_gs[index_n1_gs] <- mean(l6_form_n1_gs)
#     avgr_n1_gs[index_n1_gs] <- paste(avgr_n1_gs[index_n1_gs],sep = "")
#     sdr_n1_gs[index_n1_gs] <- sd(l6_form_n1_gs)
#     sdr_n1_gs[index_n1_gs] <- paste(sdr_n1_gs[index_n1_gs],sep = "")
#     l6_form_n1_gs <- as.character(l6_form_n1_gs)
#     l6_form_n1_gs_flattened <- stri_paste(l6_form_n1_gs,collapse = '')
#     l6_form_n1_gssplitted <- as.numeric(strsplit(as.character(l6_form_n1_gs_flattened),"")[[1]])
#     final_n1_gs[index_n1_gs,index_n1_gs_cols] <- l6_form_n1_gssplitted[index_n1_gs_cols]
#   }
# }
#
# final_n1_gs[is.na(final_n1_gs)] <- ""
# n1_goalscoredmatrix <- cbind(n1_teams,final_n1_gs,suml6_n1_gs,sum_n1_zero_gs,sum_n1_one_gs,sum_n1_two_gs,sum_n1_three_gs,avgr_n1_gs,sdr_n1_gs)
# ################################################################################################################################################
# #p1
# final_p1_gs <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_gs <- c()
# sum_p1_zero_gs <- c()
# sum_p1_one_gs <- c()
# sum_p1_two_gs <- c()
# sum_p1_three_gs <- c()
# avgr_p1_gs <- c()
# sdr_p1_gs <- c()
# l6_form_p1_gssplitted <- c()
# form_p1_gs <- c()
# for(index_p1_gs in 1:length(p1_teams))
# {
#   for(index_p1_gs_cols in 1:p1_totalrounds)
#   {
#     index_p1_gs  <- row.names(p1_goalscored_h) == p1_teams[index_p1_gs]
#     form_p1_gs <- p1_goalscored_h[index_p1_gs ]
#     deleted_form_p1_gs <- form_p1_gs[!form_p1_gs[] == ""]
#     l6_form_p1_gs <- tail(deleted_form_p1_gs,p1_last_n_games)
#     l6_form_p1_gs <- as.numeric(l6_form_p1_gs)
#     suml6_p1_gs[index_p1_gs] <- sum(l6_form_p1_gs)
#     suml6_p1_gs[index_p1_gs] <- paste(suml6_p1_gs[index_p1_gs],sep = "")
#     sum_p1_zero_gs[index_p1_gs] <- length(which(l6_form_p1_gs == 0))
#     sum_p1_zero_gs[index_p1_gs] <- paste(sum_p1_zero_gs[index_p1_gs],sep = "")
#     sum_p1_one_gs[index_p1_gs] <- length(which(l6_form_p1_gs == 1))
#     sum_p1_one_gs[index_p1_gs] <- paste(sum_p1_one_gs[index_p1_gs],sep = "")
#     sum_p1_two_gs[index_p1_gs] <- length(which(l6_form_p1_gs >= 2))
#     sum_p1_two_gs[index_p1_gs] <- paste(sum_p1_two_gs[index_p1_gs],sep = "")
#     sum_p1_three_gs[index_p1_gs] <- length(which(l6_form_p1_gs >= 3))
#     sum_p1_three_gs[index_p1_gs] <- paste(sum_p1_three_gs[index_p1_gs],sep = "")
#     avgr_p1_gs[index_p1_gs] <- mean(l6_form_p1_gs)
#     avgr_p1_gs[index_p1_gs] <- paste(avgr_p1_gs[index_p1_gs],sep = "")
#     sdr_p1_gs[index_p1_gs] <- sd(l6_form_p1_gs)
#     sdr_p1_gs[index_p1_gs] <- paste(sdr_p1_gs[index_p1_gs],sep = "")
#     l6_form_p1_gs <- as.character(l6_form_p1_gs)
#     l6_form_p1_gs_flattened <- stri_paste(l6_form_p1_gs,collapse = '')
#     l6_form_p1_gssplitted <- as.numeric(strsplit(as.character(l6_form_p1_gs_flattened),"")[[1]])
#     final_p1_gs[index_p1_gs,index_p1_gs_cols] <- l6_form_p1_gssplitted[index_p1_gs_cols]
#   }
# }
#
# final_p1_gs[is.na(final_p1_gs)] <- ""
# p1_goalscoredmatrix <- cbind(p1_teams,final_p1_gs,suml6_p1_gs,sum_p1_zero_gs,sum_p1_one_gs,sum_p1_two_gs,sum_p1_three_gs,avgr_p1_gs,sdr_p1_gs)
# ###############################################################################################################################################
# #sp1
# final_sp1_gs <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_gs <- c()
# sum_sp1_zero_gs <- c()
# sum_sp1_one_gs <- c()
# sum_sp1_two_gs <- c()
# sum_sp1_three_gs <- c()
# avgr_sp1_gs <- c()
# sdr_sp1_gs <- c()
# l6_form_sp1_gssplitted <- c()
# form_sp1_gs <- c()
# for(index_sp1_gs in 1:length(sp1_teams))
# {
#   for(index_sp1_gs_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_gs  <- row.names(sp1_goalscored_h) == sp1_teams[index_sp1_gs]
#     form_sp1_gs <- sp1_goalscored_h[index_sp1_gs ]
#     deleted_form_sp1_gs <- form_sp1_gs[!form_sp1_gs[] == ""]
#     l6_form_sp1_gs <- tail(deleted_form_sp1_gs,sp1_last_n_games)
#     l6_form_sp1_gs <- as.numeric(l6_form_sp1_gs)
#     suml6_sp1_gs[index_sp1_gs] <- sum(l6_form_sp1_gs)
#     suml6_sp1_gs[index_sp1_gs] <- paste(suml6_sp1_gs[index_sp1_gs],sep = "")
#     sum_sp1_zero_gs[index_sp1_gs] <- length(which(l6_form_sp1_gs == 0))
#     sum_sp1_zero_gs[index_sp1_gs] <- paste(sum_sp1_zero_gs[index_sp1_gs],sep = "")
#     sum_sp1_one_gs[index_sp1_gs] <- length(which(l6_form_sp1_gs == 1))
#     sum_sp1_one_gs[index_sp1_gs] <- paste(sum_sp1_one_gs[index_sp1_gs],sep = "")
#     sum_sp1_two_gs[index_sp1_gs] <- length(which(l6_form_sp1_gs >= 2))
#     sum_sp1_two_gs[index_sp1_gs] <- paste(sum_sp1_two_gs[index_sp1_gs],sep = "")
#     sum_sp1_three_gs[index_sp1_gs] <- length(which(l6_form_sp1_gs >= 3))
#     sum_sp1_three_gs[index_sp1_gs] <- paste(sum_sp1_three_gs[index_sp1_gs],sep = "")
#     avgr_sp1_gs[index_sp1_gs] <- mean(l6_form_sp1_gs)
#     avgr_sp1_gs[index_sp1_gs] <- paste(avgr_sp1_gs[index_sp1_gs],sep = "")
#     sdr_sp1_gs[index_sp1_gs] <- sd(l6_form_sp1_gs)
#     sdr_sp1_gs[index_sp1_gs] <- paste(sdr_sp1_gs[index_sp1_gs],sep = "")
#     l6_form_sp1_gs <- as.character(l6_form_sp1_gs)
#     l6_form_sp1_gs_flattened <- stri_paste(l6_form_sp1_gs,collapse = '')
#     l6_form_sp1_gssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_gs_flattened),"")[[1]])
#     final_sp1_gs[index_sp1_gs,index_sp1_gs_cols] <- l6_form_sp1_gssplitted[index_sp1_gs_cols]
#   }
# }
#
# final_sp1_gs[is.na(final_sp1_gs)] <- ""
# sp1_goalscoredmatrix <- cbind(sp1_teams,final_sp1_gs,suml6_sp1_gs,sum_sp1_zero_gs,sum_sp1_one_gs,sum_sp1_two_gs,sum_sp1_three_gs,avgr_sp1_gs,sdr_sp1_gs)
# #########################################################################################################################################################
# #sp2
# final_sp2_gs <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_gs <- c()
# sum_sp2_zero_gs <- c()
# sum_sp2_one_gs <- c()
# sum_sp2_two_gs <- c()
# sum_sp2_three_gs <- c()
# avgr_sp2_gs <- c()
# sdr_sp2_gs <- c()
# l6_form_sp2_gssplitted <- c()
# form_sp2_gs <- c()
# for(index_sp2_gs in 1:length(sp2_teams))
# {
#   for(index_sp2_gs_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_gs  <- row.names(sp2_goalscored_h) == sp2_teams[index_sp2_gs]
#     form_sp2_gs <- sp2_goalscored_h[index_sp2_gs ]
#     deleted_form_sp2_gs <- form_sp2_gs[!form_sp2_gs[] == ""]
#     l6_form_sp2_gs <- tail(deleted_form_sp2_gs,sp2_last_n_games)
#     l6_form_sp2_gs <- as.numeric(l6_form_sp2_gs)
#     suml6_sp2_gs[index_sp2_gs] <- sum(l6_form_sp2_gs)
#     suml6_sp2_gs[index_sp2_gs] <- paste(suml6_sp2_gs[index_sp2_gs],sep = "")
#     sum_sp2_zero_gs[index_sp2_gs] <- length(which(l6_form_sp2_gs == 0))
#     sum_sp2_zero_gs[index_sp2_gs] <- paste(sum_sp2_zero_gs[index_sp2_gs],sep = "")
#     sum_sp2_one_gs[index_sp2_gs] <- length(which(l6_form_sp2_gs == 1))
#     sum_sp2_one_gs[index_sp2_gs] <- paste(sum_sp2_one_gs[index_sp2_gs],sep = "")
#     sum_sp2_two_gs[index_sp2_gs] <- length(which(l6_form_sp2_gs >= 2))
#     sum_sp2_two_gs[index_sp2_gs] <- paste(sum_sp2_two_gs[index_sp2_gs],sep = "")
#     sum_sp2_three_gs[index_sp2_gs] <- length(which(l6_form_sp2_gs >= 3))
#     sum_sp2_three_gs[index_sp2_gs] <- paste(sum_sp2_three_gs[index_sp2_gs],sep = "")
#     avgr_sp2_gs[index_sp2_gs] <- mean(l6_form_sp2_gs)
#     avgr_sp2_gs[index_sp2_gs] <- paste(avgr_sp2_gs[index_sp2_gs],sep = "")
#     sdr_sp2_gs[index_sp2_gs] <- sd(l6_form_sp2_gs)
#     sdr_sp2_gs[index_sp2_gs] <- paste(sdr_sp2_gs[index_sp2_gs],sep = "")
#     l6_form_sp2_gs <- as.character(l6_form_sp2_gs)
#     l6_form_sp2_gs_flattened <- stri_paste(l6_form_sp2_gs,collapse = '')
#     l6_form_sp2_gssplitted <- as.numeric(strsplit(as.character(l6_form_sp2_gs_flattened),"")[[1]])
#     final_sp2_gs[index_sp2_gs,index_sp2_gs_cols] <- l6_form_sp2_gssplitted[index_sp2_gs_cols]
#   }
# }
#
# final_sp2_gs[is.na(final_sp2_gs)] <- ""
# sp2_goalscoredmatrix <- cbind(sp2_teams,final_sp2_gs,suml6_sp2_gs,sum_sp2_zero_gs,sum_sp2_one_gs,sum_sp2_two_gs,sum_sp2_three_gs,avgr_sp2_gs,sdr_sp2_gs)
# ########################################################################################################################################################
# #sc0
# final_sc0_gs <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_gs <- c()
# sum_sc0_zero_gs <- c()
# sum_sc0_one_gs <- c()
# sum_sc0_two_gs <- c()
# sum_sc0_three_gs <- c()
# avgr_sc0_gs <- c()
# sdr_sc0_gs <- c()
# l6_form_sc0_gssplitted <- c()
# form_sc0_gs <- c()
# for(index_sc0_gs in 1:length(sc0_teams))
# {
#   for(index_sc0_gs_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_gs  <- row.names(sc0_goalscored_h) == sc0_teams[index_sc0_gs]
#     form_sc0_gs <- sc0_goalscored_h[index_sc0_gs ]
#     deleted_form_sc0_gs <- form_sc0_gs[!form_sc0_gs[] == ""]
#     l6_form_sc0_gs <- tail(deleted_form_sc0_gs,sc0_last_n_games)
#     l6_form_sc0_gs <- as.numeric(l6_form_sc0_gs)
#     suml6_sc0_gs[index_sc0_gs] <- sum(l6_form_sc0_gs)
#     suml6_sc0_gs[index_sc0_gs] <- paste(suml6_sc0_gs[index_sc0_gs],sep = "")
#     sum_sc0_zero_gs[index_sc0_gs] <- length(which(l6_form_sc0_gs == 0))
#     sum_sc0_zero_gs[index_sc0_gs] <- paste(sum_sc0_zero_gs[index_sc0_gs],sep = "")
#     sum_sc0_one_gs[index_sc0_gs] <- length(which(l6_form_sc0_gs == 1))
#     sum_sc0_one_gs[index_sc0_gs] <- paste(sum_sc0_one_gs[index_sc0_gs],sep = "")
#     sum_sc0_two_gs[index_sc0_gs] <- length(which(l6_form_sc0_gs >= 2))
#     sum_sc0_two_gs[index_sc0_gs] <- paste(sum_sc0_two_gs[index_sc0_gs],sep = "")
#     sum_sc0_three_gs[index_sc0_gs] <- length(which(l6_form_sc0_gs >= 3))
#     sum_sc0_three_gs[index_sc0_gs] <- paste(sum_sc0_three_gs[index_sc0_gs],sep = "")
#     avgr_sc0_gs[index_sc0_gs] <- mean(l6_form_sc0_gs)
#     avgr_sc0_gs[index_sc0_gs] <- paste(avgr_sc0_gs[index_sc0_gs],sep = "")
#     sdr_sc0_gs[index_sc0_gs] <- sd(l6_form_sc0_gs)
#     sdr_sc0_gs[index_sc0_gs] <- paste(sdr_sc0_gs[index_sc0_gs],sep = "")
#     l6_form_sc0_gs <- as.character(l6_form_sc0_gs)
#     l6_form_sc0_gs_flattened <- stri_paste(l6_form_sc0_gs,collapse = '')
#     l6_form_sc0_gssplitted <- as.numeric(strsplit(as.character(l6_form_sc0_gs_flattened),"")[[1]])
#     final_sc0_gs[index_sc0_gs,index_sc0_gs_cols] <- l6_form_sc0_gssplitted[index_sc0_gs_cols]
#   }
# }
#
# final_sc0_gs[is.na(final_sc0_gs)] <- ""
# sc0_goalscoredmatrix <- cbind(sc0_teams,final_sc0_gs,suml6_sc0_gs,sum_sc0_zero_gs,sum_sc0_one_gs,sum_sc0_two_gs,sum_sc0_three_gs,avgr_sc0_gs,sdr_sc0_gs)
# ##########################################################################################################################################################
# #sc1
# final_sc1_gs <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_gs <- c()
# sum_sc1_zero_gs <- c()
# sum_sc1_one_gs <- c()
# sum_sc1_two_gs <- c()
# sum_sc1_three_gs <- c()
# avgr_sc1_gs <- c()
# sdr_sc1_gs <- c()
# l6_form_sc1_gssplitted <- c()
# form_sc1_gs <- c()
# for(index_sc1_gs in 1:length(sc1_teams))
# {
#   for(index_sc1_gs_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_gs  <- row.names(sc1_goalscored_h) == sc1_teams[index_sc1_gs]
#     form_sc1_gs <- sc1_goalscored_h[index_sc1_gs ]
#     deleted_form_sc1_gs <- form_sc1_gs[!form_sc1_gs[] == ""]
#     l6_form_sc1_gs <- tail(deleted_form_sc1_gs,sc1_last_n_games)
#     l6_form_sc1_gs <- as.numeric(l6_form_sc1_gs)
#     suml6_sc1_gs[index_sc1_gs] <- sum(l6_form_sc1_gs)
#     suml6_sc1_gs[index_sc1_gs] <- paste(suml6_sc1_gs[index_sc1_gs],sep = "")
#     sum_sc1_zero_gs[index_sc1_gs] <- length(which(l6_form_sc1_gs == 0))
#     sum_sc1_zero_gs[index_sc1_gs] <- paste(sum_sc1_zero_gs[index_sc1_gs],sep = "")
#     sum_sc1_one_gs[index_sc1_gs] <- length(which(l6_form_sc1_gs == 1))
#     sum_sc1_one_gs[index_sc1_gs] <- paste(sum_sc1_one_gs[index_sc1_gs],sep = "")
#     sum_sc1_two_gs[index_sc1_gs] <- length(which(l6_form_sc1_gs >= 2))
#     sum_sc1_two_gs[index_sc1_gs] <- paste(sum_sc1_two_gs[index_sc1_gs],sep = "")
#     sum_sc1_three_gs[index_sc1_gs] <- length(which(l6_form_sc1_gs >= 3))
#     sum_sc1_three_gs[index_sc1_gs] <- paste(sum_sc1_three_gs[index_sc1_gs],sep = "")
#     avgr_sc1_gs[index_sc1_gs] <- mean(l6_form_sc1_gs)
#     avgr_sc1_gs[index_sc1_gs] <- paste(avgr_sc1_gs[index_sc1_gs],sep = "")
#     sdr_sc1_gs[index_sc1_gs] <- sd(l6_form_sc1_gs)
#     sdr_sc1_gs[index_sc1_gs] <- paste(sdr_sc1_gs[index_sc1_gs],sep = "")
#     l6_form_sc1_gs <- as.character(l6_form_sc1_gs)
#     l6_form_sc1_gs_flattened <- stri_paste(l6_form_sc1_gs,collapse = '')
#     l6_form_sc1_gssplitted <- as.numeric(strsplit(as.character(l6_form_sc1_gs_flattened),"")[[1]])
#     final_sc1_gs[index_sc1_gs,index_sc1_gs_cols] <- l6_form_sc1_gssplitted[index_sc1_gs_cols]
#   }
# }
#
# final_sc1_gs[is.na(final_sc1_gs)] <- ""
# sc1_goalscoredmatrix <- cbind(sc1_teams,final_sc1_gs,suml6_sc1_gs,sum_sc1_zero_gs,sum_sc1_one_gs,sum_sc1_two_gs,sum_sc1_three_gs,avgr_sc1_gs,sdr_sc1_gs)
# ##########################################################################################################################################################
# #sc2
# final_sc2_gs <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_gs <- c()
# sum_sc2_zero_gs <- c()
# sum_sc2_one_gs <- c()
# sum_sc2_two_gs <- c()
# sum_sc2_three_gs <- c()
# avgr_sc2_gs <- c()
# sdr_sc2_gs <- c()
# l6_form_sc2_gssplitted <- c()
# form_sc2_gs <- c()
# for(index_sc2_gs in 1:length(sc2_teams))
# {
#   for(index_sc2_gs_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_gs  <- row.names(sc2_goalscored_h) == sc2_teams[index_sc2_gs]
#     form_sc2_gs <- sc2_goalscored_h[index_sc2_gs ]
#     deleted_form_sc2_gs <- form_sc2_gs[!form_sc2_gs[] == ""]
#     l6_form_sc2_gs <- tail(deleted_form_sc2_gs,sc2_last_n_games)
#     l6_form_sc2_gs <- as.numeric(l6_form_sc2_gs)
#     suml6_sc2_gs[index_sc2_gs] <- sum(l6_form_sc2_gs)
#     suml6_sc2_gs[index_sc2_gs] <- paste(suml6_sc2_gs[index_sc2_gs],sep = "")
#     sum_sc2_zero_gs[index_sc2_gs] <- length(which(l6_form_sc2_gs == 0))
#     sum_sc2_zero_gs[index_sc2_gs] <- paste(sum_sc2_zero_gs[index_sc2_gs],sep = "")
#     sum_sc2_one_gs[index_sc2_gs] <- length(which(l6_form_sc2_gs == 1))
#     sum_sc2_one_gs[index_sc2_gs] <- paste(sum_sc2_one_gs[index_sc2_gs],sep = "")
#     sum_sc2_two_gs[index_sc2_gs] <- length(which(l6_form_sc2_gs >= 2))
#     sum_sc2_two_gs[index_sc2_gs] <- paste(sum_sc2_two_gs[index_sc2_gs],sep = "")
#     sum_sc2_three_gs[index_sc2_gs] <- length(which(l6_form_sc2_gs >= 3))
#     sum_sc2_three_gs[index_sc2_gs] <- paste(sum_sc2_three_gs[index_sc2_gs],sep = "")
#     avgr_sc2_gs[index_sc2_gs] <- mean(l6_form_sc2_gs)
#     avgr_sc2_gs[index_sc2_gs] <- paste(avgr_sc2_gs[index_sc2_gs],sep = "")
#     sdr_sc2_gs[index_sc2_gs] <- sd(l6_form_sc2_gs)
#     sdr_sc2_gs[index_sc2_gs] <- paste(sdr_sc2_gs[index_sc2_gs],sep = "")
#     l6_form_sc2_gs <- as.character(l6_form_sc2_gs)
#     l6_form_sc2_gs_flattened <- stri_paste(l6_form_sc2_gs,collapse = '')
#     l6_form_sc2_gssplitted <- as.numeric(strsplit(as.character(l6_form_sc2_gs_flattened),"")[[1]])
#     final_sc2_gs[index_sc2_gs,index_sc2_gs_cols] <- l6_form_sc2_gssplitted[index_sc2_gs_cols]
#   }
# }
#
# final_sc2_gs[is.na(final_sc2_gs)] <- ""
# sc2_goalscoredmatrix <- cbind(sc2_teams,final_sc2_gs,suml6_sc2_gs,sum_sc2_zero_gs,sum_sc2_one_gs,sum_sc2_two_gs,sum_sc2_three_gs,avgr_sc2_gs,sdr_sc2_gs)
# ##########################################################################################################################################################
# #sc3
# final_sc3_gs <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_gs <- c()
# sum_sc3_zero_gs <- c()
# sum_sc3_one_gs <- c()
# sum_sc3_two_gs <- c()
# sum_sc3_three_gs <- c()
# avgr_sc3_gs <- c()
# sdr_sc3_gs <- c()
# l6_form_sc3_gssplitted <- c()
# form_sc3_gs <- c()
# for(index_sc3_gs in 1:length(sc3_teams))
# {
#   for(index_sc3_gs_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_gs  <- row.names(sc3_goalscored_h) == sc3_teams[index_sc3_gs]
#     form_sc3_gs <- sc3_goalscored_h[index_sc3_gs ]
#     deleted_form_sc3_gs <- form_sc3_gs[!form_sc3_gs[] == ""]
#     l6_form_sc3_gs <- tail(deleted_form_sc3_gs,sc3_last_n_games)
#     l6_form_sc3_gs <- as.numeric(l6_form_sc3_gs)
#     suml6_sc3_gs[index_sc3_gs] <- sum(l6_form_sc3_gs)
#     suml6_sc3_gs[index_sc3_gs] <- paste(suml6_sc3_gs[index_sc3_gs],sep = "")
#     sum_sc3_zero_gs[index_sc3_gs] <- length(which(l6_form_sc3_gs == 0))
#     sum_sc3_zero_gs[index_sc3_gs] <- paste(sum_sc3_zero_gs[index_sc3_gs],sep = "")
#     sum_sc3_one_gs[index_sc3_gs] <- length(which(l6_form_sc3_gs == 1))
#     sum_sc3_one_gs[index_sc3_gs] <- paste(sum_sc3_one_gs[index_sc3_gs],sep = "")
#     sum_sc3_two_gs[index_sc3_gs] <- length(which(l6_form_sc3_gs >= 2))
#     sum_sc3_two_gs[index_sc3_gs] <- paste(sum_sc3_two_gs[index_sc3_gs],sep = "")
#     sum_sc3_three_gs[index_sc3_gs] <- length(which(l6_form_sc3_gs >= 3))
#     sum_sc3_three_gs[index_sc3_gs] <- paste(sum_sc3_three_gs[index_sc3_gs],sep = "")
#     avgr_sc3_gs[index_sc3_gs] <- mean(l6_form_sc3_gs)
#     avgr_sc3_gs[index_sc3_gs] <- paste(avgr_sc3_gs[index_sc3_gs],sep = "")
#     sdr_sc3_gs[index_sc3_gs] <- sd(l6_form_sc3_gs)
#     sdr_sc3_gs[index_sc3_gs] <- paste(sdr_sc3_gs[index_sc3_gs],sep = "")
#     l6_form_sc3_gs <- as.character(l6_form_sc3_gs)
#     l6_form_sc3_gs_flattened <- stri_paste(l6_form_sc3_gs,collapse = '')
#     l6_form_sc3_gssplitted <- as.numeric(strsplit(as.character(l6_form_sc3_gs_flattened),"")[[1]])
#     final_sc3_gs[index_sc3_gs,index_sc3_gs_cols] <- l6_form_sc3_gssplitted[index_sc3_gs_cols]
#   }
# }
#
# final_sc3_gs[is.na(final_sc3_gs)] <- ""
# sc3_goalscoredmatrix <- cbind(sc3_teams,final_sc3_gs,suml6_sc3_gs,sum_sc3_zero_gs,sum_sc3_one_gs,sum_sc3_two_gs,sum_sc3_three_gs,avgr_sc3_gs,sdr_sc3_gs)
# ##################################################################################################################################
# #t1
# final_t1_gs <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_gs <- c()
# sum_t1_zero_gs <- c()
# sum_t1_one_gs <- c()
# sum_t1_two_gs <- c()
# sum_t1_three_gs <- c()
# avgr_t1_gs <- c()
# sdr_t1_gs <- c()
# l6_form_t1_gssplitted <- c()
# form_t1_gs <- c()
# for(index_t1_gs in 1:length(t1_teams))
# {
#   for(index_t1_gs_cols in 1:t1_totalrounds)
#   {
#     index_t1_gs  <- row.names(t1_goalscored_h) == t1_teams[index_t1_gs]
#     form_t1_gs <- t1_goalscored_h[index_t1_gs ]
#     deleted_form_t1_gs <- form_t1_gs[!form_t1_gs[] == ""]
#     l6_form_t1_gs <- tail(deleted_form_t1_gs,t1_last_n_games)
#     l6_form_t1_gs <- as.numeric(l6_form_t1_gs)
#     suml6_t1_gs[index_t1_gs] <- sum(l6_form_t1_gs)
#     suml6_t1_gs[index_t1_gs] <- paste(suml6_t1_gs[index_t1_gs],sep = "")
#     sum_t1_zero_gs[index_t1_gs] <- length(which(l6_form_t1_gs == 0))
#     sum_t1_zero_gs[index_t1_gs] <- paste(sum_t1_zero_gs[index_t1_gs],sep = "")
#     sum_t1_one_gs[index_t1_gs] <- length(which(l6_form_t1_gs == 1))
#     sum_t1_one_gs[index_t1_gs] <- paste(sum_t1_one_gs[index_t1_gs],sep = "")
#     sum_t1_two_gs[index_t1_gs] <- length(which(l6_form_t1_gs >= 2))
#     sum_t1_two_gs[index_t1_gs] <- paste(sum_t1_two_gs[index_t1_gs],sep = "")
#     sum_t1_three_gs[index_t1_gs] <- length(which(l6_form_t1_gs >= 3))
#     sum_t1_three_gs[index_t1_gs] <- paste(sum_t1_three_gs[index_t1_gs],sep = "")
#     avgr_t1_gs[index_t1_gs] <- mean(l6_form_t1_gs)
#     avgr_t1_gs[index_t1_gs] <- paste(avgr_t1_gs[index_t1_gs],sep = "")
#     sdr_t1_gs[index_t1_gs] <- sd(l6_form_t1_gs)
#     sdr_t1_gs[index_t1_gs] <- paste(sdr_t1_gs[index_t1_gs],sep = "")
#     l6_form_t1_gs <- as.character(l6_form_t1_gs)
#     l6_form_t1_gs_flattened <- stri_paste(l6_form_t1_gs,collapse = '')
#     l6_form_t1_gssplitted <- as.numeric(strsplit(as.character(l6_form_t1_gs_flattened),"")[[1]])
#     final_t1_gs[index_t1_gs,index_t1_gs_cols] <- l6_form_t1_gssplitted[index_t1_gs_cols]
#   }
# }
#
# final_t1_gs[is.na(final_t1_gs)] <- ""
# t1_goalscoredmatrix <- cbind(t1_teams,final_t1_gs,suml6_t1_gs,sum_t1_zero_gs,sum_t1_one_gs,sum_t1_two_gs,sum_t1_three_gs,avgr_t1_gs,sdr_t1_gs)
# ###############################################################################################################################################





































