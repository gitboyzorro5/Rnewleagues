library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_gc <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_gc <- c()
sum_mls_zero_gc <- c()
sum_mls_one_gc <- c()
sum_mls_two_gc <- c()
sum_mls_three_gc <- c()
avgr_mls_gc <- c()
sdr_mls_gc <- c()
l6_form_mls_gcsplitted <- c()
form_mls_gc <- c()
for(index_mls_gc in 1:length(mls_teams))
{
  for(index_mls_gc_cols in 1:mls_totalrounds)
  {
    index_mls_gc  <- row.names(mls_goalconceded_h) == mls_teams[index_mls_gc]
    form_mls_gc <- mls_goalconceded_h[index_mls_gc ]
    deleted_form_mls_gc <- form_mls_gc[!form_mls_gc[] == ""]
    l6_form_mls_gc <- tail(deleted_form_mls_gc,mls_last_n_games)
    l6_form_mls_gc <- as.numeric(l6_form_mls_gc)
    suml6_mls_gc[index_mls_gc] <- sum(l6_form_mls_gc)
    suml6_mls_gc[index_mls_gc] <- paste(suml6_mls_gc[index_mls_gc],sep = "")
    sum_mls_zero_gc[index_mls_gc] <- length(which(l6_form_mls_gc == 0))
    sum_mls_zero_gc[index_mls_gc] <- paste(sum_mls_zero_gc[index_mls_gc],sep = "")
    sum_mls_one_gc[index_mls_gc] <- length(which(l6_form_mls_gc == 1))
    sum_mls_one_gc[index_mls_gc] <- paste(sum_mls_one_gc[index_mls_gc],sep = "")
    sum_mls_two_gc[index_mls_gc] <- length(which(l6_form_mls_gc >= 2))
    sum_mls_two_gc[index_mls_gc] <- paste(sum_mls_two_gc[index_mls_gc],sep = "")
    sum_mls_three_gc[index_mls_gc] <- length(which(l6_form_mls_gc >= 3))
    sum_mls_three_gc[index_mls_gc] <- paste(sum_mls_three_gc[index_mls_gc],sep = "")
    avgr_mls_gc[index_mls_gc] <- mean(l6_form_mls_gc)
    avgr_mls_gc[index_mls_gc] <- paste(avgr_mls_gc[index_mls_gc],sep = "")
    sdr_mls_gc[index_mls_gc] <- sd(l6_form_mls_gc)
    sdr_mls_gc[index_mls_gc] <- paste(sdr_mls_gc[index_mls_gc],sep = "")
    l6_form_mls_gc <- as.character(l6_form_mls_gc)
    l6_form_mls_gc_flattened <- stri_paste(l6_form_mls_gc,collapse = '')
    l6_form_mls_gcsplitted <- as.numeric(strsplit(as.character(l6_form_mls_gc_flattened),"")[[1]])
    final_mls_gc[index_mls_gc,index_mls_gc_cols] <- l6_form_mls_gcsplitted[index_mls_gc_cols]
  }
}

final_mls_gc[is.na(final_mls_gc)] <- ""
mls_goalconcededmatrix <- cbind(mls_teams,final_mls_gc,suml6_mls_gc,sum_mls_zero_gc,sum_mls_one_gc,sum_mls_two_gc,sum_mls_three_gc,avgr_mls_gc,sdr_mls_gc)

###################################################################################################################################################
#arg
final_arg_gc <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_gc <- c()
sum_arg_zero_gc <- c()
sum_arg_one_gc <- c()
sum_arg_two_gc <- c()
sum_arg_three_gc <- c()
avgr_arg_gc <- c()
sdr_arg_gc <- c()
l6_form_arg_gcsplitted <- c()
form_arg_gc <- c()
for(index_arg_gc in 1:length(arg_teams))
{
  for(index_arg_gc_cols in 1:arg_totalrounds)
  {
    index_arg_gc  <- row.names(arg_goalconceded_h) == arg_teams[index_arg_gc]
    form_arg_gc <- arg_goalconceded_h[index_arg_gc ]
    deleted_form_arg_gc <- form_arg_gc[!form_arg_gc[] == ""]
    l6_form_arg_gc <- tail(deleted_form_arg_gc,arg_last_n_games)
    l6_form_arg_gc <- as.numeric(l6_form_arg_gc)
    suml6_arg_gc[index_arg_gc] <- sum(l6_form_arg_gc)
    suml6_arg_gc[index_arg_gc] <- paste(suml6_arg_gc[index_arg_gc],sep = "")
    sum_arg_zero_gc[index_arg_gc] <- length(which(l6_form_arg_gc == 0))
    sum_arg_zero_gc[index_arg_gc] <- paste(sum_arg_zero_gc[index_arg_gc],sep = "")
    sum_arg_one_gc[index_arg_gc] <- length(which(l6_form_arg_gc == 1))
    sum_arg_one_gc[index_arg_gc] <- paste(sum_arg_one_gc[index_arg_gc],sep = "")
    sum_arg_two_gc[index_arg_gc] <- length(which(l6_form_arg_gc >= 2))
    sum_arg_two_gc[index_arg_gc] <- paste(sum_arg_two_gc[index_arg_gc],sep = "")
    sum_arg_three_gc[index_arg_gc] <- length(which(l6_form_arg_gc >= 3))
    sum_arg_three_gc[index_arg_gc] <- paste(sum_arg_three_gc[index_arg_gc],sep = "")
    avgr_arg_gc[index_arg_gc] <- mean(l6_form_arg_gc)
    avgr_arg_gc[index_arg_gc] <- paste(avgr_arg_gc[index_arg_gc],sep = "")
    sdr_arg_gc[index_arg_gc] <- sd(l6_form_arg_gc)
    sdr_arg_gc[index_arg_gc] <- paste(sdr_arg_gc[index_arg_gc],sep = "")
    l6_form_arg_gc <- as.character(l6_form_arg_gc)
    l6_form_arg_gc_flattened <- stri_paste(l6_form_arg_gc,collapse = '')
    l6_form_arg_gcsplitted <- as.numeric(strsplit(as.character(l6_form_arg_gc_flattened),"")[[1]])
    final_arg_gc[index_arg_gc,index_arg_gc_cols] <- l6_form_arg_gcsplitted[index_arg_gc_cols]
  }
}

final_arg_gc[is.na(final_arg_gc)] <- ""
arg_goalconcededmatrix <- cbind(arg_teams,final_arg_gc,suml6_arg_gc,sum_arg_zero_gc,sum_arg_one_gc,sum_arg_two_gc,sum_arg_three_gc,avgr_arg_gc,sdr_arg_gc)
####################################################################################################################################################
#d2
# final_d2_gc <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_gc <- c()
# sum_d2_zero_gc <- c()
# sum_d2_one_gc <- c()
# sum_d2_two_gc <- c()
# sum_d2_three_gc <- c()
# avgr_d2_gc <- c()
# sdr_d2_gc <- c()
# l6_form_d2_gcsplitted <- c()
# form_d2_gc <- c()
# for(index_d2_gc in 1:length(d2_teams))
# {
#   for(index_d2_gc_cols in 1:d2_totalrounds)
#   {
#     index_d2_gc  <- row.names(d2_goalconceded_h) == d2_teams[index_d2_gc]
#     form_d2_gc <- d2_goalconceded_h[index_d2_gc ]
#     deleted_form_d2_gc <- form_d2_gc[!form_d2_gc[] == ""]
#     l6_form_d2_gc <- tail(deleted_form_d2_gc,d2_last_n_games)
#     l6_form_d2_gc <- as.numeric(l6_form_d2_gc)
#     suml6_d2_gc[index_d2_gc] <- sum(l6_form_d2_gc)
#     suml6_d2_gc[index_d2_gc] <- paste(suml6_d2_gc[index_d2_gc],sep = "")
#     sum_d2_zero_gc[index_d2_gc] <- length(which(l6_form_d2_gc == 0))
#     sum_d2_zero_gc[index_d2_gc] <- paste(sum_d2_zero_gc[index_d2_gc],sep = "")
#     sum_d2_one_gc[index_d2_gc] <- length(which(l6_form_d2_gc == 1))
#     sum_d2_one_gc[index_d2_gc] <- paste(sum_d2_one_gc[index_d2_gc],sep = "")
#     sum_d2_two_gc[index_d2_gc] <- length(which(l6_form_d2_gc >= 2))
#     sum_d2_two_gc[index_d2_gc] <- paste(sum_d2_two_gc[index_d2_gc],sep = "")
#     sum_d2_three_gc[index_d2_gc] <- length(which(l6_form_d2_gc >= 3))
#     sum_d2_three_gc[index_d2_gc] <- paste(sum_d2_three_gc[index_d2_gc],sep = "")
#     avgr_d2_gc[index_d2_gc] <- mean(l6_form_d2_gc)
#     avgr_d2_gc[index_d2_gc] <- paste(avgr_d2_gc[index_d2_gc],sep = "")
#     sdr_d2_gc[index_d2_gc] <- sd(l6_form_d2_gc)
#     sdr_d2_gc[index_d2_gc] <- paste(sdr_d2_gc[index_d2_gc],sep = "")
#     l6_form_d2_gc <- as.character(l6_form_d2_gc)
#     l6_form_d2_gc_flattened <- stri_paste(l6_form_d2_gc,collapse = '')
#     l6_form_d2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_d2_gc_flattened),"")[[1]])
#     final_d2_gc[index_d2_gc,index_d2_gc_cols] <- l6_form_d2_gcsplitted[index_d2_gc_cols]
#   }
# }
#
# final_d2_gc[is.na(final_d2_gc)] <- ""
# d2_goalconcededmatrix <- cbind(d2_teams,final_d2_gc,suml6_d2_gc,sum_d2_zero_gc,sum_d2_one_gc,sum_d2_two_gc,sum_d2_three_gc,avgr_d2_gc,sdr_d2_gc)
# #######################################################################################################################################################
# #e0
# final_e0_gc <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_gc <- c()
# sum_e0_zero_gc <- c()
# sum_e0_one_gc <- c()
# sum_e0_two_gc <- c()
# sum_e0_three_gc <- c()
# avgr_e0_gc <- c()
# sdr_e0_gc <- c()
# l6_form_e0_gcsplitted <- c()
# form_e0_gc <- c()
# for(index_e0_gc in 1:length(e0_teams))
# {
#   for(index_e0_gc_cols in 1:e0_totalrounds)
#   {
#     index_e0_gc  <- row.names(e0_goalconceded_h) == e0_teams[index_e0_gc]
#     form_e0_gc <- e0_goalconceded_h[index_e0_gc ]
#     deleted_form_e0_gc <- form_e0_gc[!form_e0_gc[] == ""]
#     l6_form_e0_gc <- tail(deleted_form_e0_gc,e0_last_n_games)
#     l6_form_e0_gc <- as.numeric(l6_form_e0_gc)
#     suml6_e0_gc[index_e0_gc] <- sum(l6_form_e0_gc)
#     suml6_e0_gc[index_e0_gc] <- paste(suml6_e0_gc[index_e0_gc],sep = "")
#     sum_e0_zero_gc[index_e0_gc] <- length(which(l6_form_e0_gc == 0))
#     sum_e0_zero_gc[index_e0_gc] <- paste(sum_e0_zero_gc[index_e0_gc],sep = "")
#     sum_e0_one_gc[index_e0_gc] <- length(which(l6_form_e0_gc == 1))
#     sum_e0_one_gc[index_e0_gc] <- paste(sum_e0_one_gc[index_e0_gc],sep = "")
#     sum_e0_two_gc[index_e0_gc] <- length(which(l6_form_e0_gc >= 2))
#     sum_e0_two_gc[index_e0_gc] <- paste(sum_e0_two_gc[index_e0_gc],sep = "")
#     sum_e0_three_gc[index_e0_gc] <- length(which(l6_form_e0_gc >= 3))
#     sum_e0_three_gc[index_e0_gc] <- paste(sum_e0_three_gc[index_e0_gc],sep = "")
#     avgr_e0_gc[index_e0_gc] <- mean(l6_form_e0_gc)
#     avgr_e0_gc[index_e0_gc] <- paste(avgr_e0_gc[index_e0_gc],sep = "")
#     sdr_e0_gc[index_e0_gc] <- sd(l6_form_e0_gc)
#     sdr_e0_gc[index_e0_gc] <- paste(sdr_e0_gc[index_e0_gc],sep = "")
#     l6_form_e0_gc <- as.character(l6_form_e0_gc)
#     l6_form_e0_gc_flattened <- stri_paste(l6_form_e0_gc,collapse = '')
#     l6_form_e0_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e0_gc_flattened),"")[[1]])
#     final_e0_gc[index_e0_gc,index_e0_gc_cols] <- l6_form_e0_gcsplitted[index_e0_gc_cols]
#   }
# }
#
# final_e0_gc[is.na(final_e0_gc)] <- ""
# e0_goalconcededmatrix <- cbind(e0_teams,final_e0_gc,suml6_e0_gc,sum_e0_zero_gc,sum_e0_one_gc,sum_e0_two_gc,sum_e0_three_gc,avgr_e0_gc,sdr_e0_gc)
# ###################################################################################################################################################
# #e1
# final_e1_gc <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_gc <- c()
# sum_e1_zero_gc <- c()
# sum_e1_one_gc <- c()
# sum_e1_two_gc <- c()
# sum_e1_three_gc <- c()
# avgr_e1_gc <- c()
# sdr_e1_gc <- c()
# l6_form_e1_gcsplitted <- c()
# form_e1_gc <- c()
# for(index_e1_gc in 1:length(e1_teams))
# {
#   for(index_e1_gc_cols in 1:e1_totalrounds)
#   {
#     index_e1_gc  <- row.names(e1_goalconceded_h) == e1_teams[index_e1_gc]
#     form_e1_gc <- e1_goalconceded_h[index_e1_gc ]
#     deleted_form_e1_gc <- form_e1_gc[!form_e1_gc[] == ""]
#     l6_form_e1_gc <- tail(deleted_form_e1_gc,e1_last_n_games)
#     l6_form_e1_gc <- as.numeric(l6_form_e1_gc)
#     suml6_e1_gc[index_e1_gc] <- sum(l6_form_e1_gc)
#     suml6_e1_gc[index_e1_gc] <- paste(suml6_e1_gc[index_e1_gc],sep = "")
#     sum_e1_zero_gc[index_e1_gc] <- length(which(l6_form_e1_gc == 0))
#     sum_e1_zero_gc[index_e1_gc] <- paste(sum_e1_zero_gc[index_e1_gc],sep = "")
#     sum_e1_one_gc[index_e1_gc] <- length(which(l6_form_e1_gc == 1))
#     sum_e1_one_gc[index_e1_gc] <- paste(sum_e1_one_gc[index_e1_gc],sep = "")
#     sum_e1_two_gc[index_e1_gc] <- length(which(l6_form_e1_gc >= 2))
#     sum_e1_two_gc[index_e1_gc] <- paste(sum_e1_two_gc[index_e1_gc],sep = "")
#     sum_e1_three_gc[index_e1_gc] <- length(which(l6_form_e1_gc >= 3))
#     sum_e1_three_gc[index_e1_gc] <- paste(sum_e1_three_gc[index_e1_gc],sep = "")
#     avgr_e1_gc[index_e1_gc] <- mean(l6_form_e1_gc)
#     avgr_e1_gc[index_e1_gc] <- paste(avgr_e1_gc[index_e1_gc],sep = "")
#     sdr_e1_gc[index_e1_gc] <- sd(l6_form_e1_gc)
#     sdr_e1_gc[index_e1_gc] <- paste(sdr_e1_gc[index_e1_gc],sep = "")
#     l6_form_e1_gc <- as.character(l6_form_e1_gc)
#     l6_form_e1_gc_flattened <- stri_paste(l6_form_e1_gc,collapse = '')
#     l6_form_e1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e1_gc_flattened),"")[[1]])
#     final_e1_gc[index_e1_gc,index_e1_gc_cols] <- l6_form_e1_gcsplitted[index_e1_gc_cols]
#   }
# }
#
# final_e1_gc[is.na(final_e1_gc)] <- ""
# e1_goalconcededmatrix <- cbind(e1_teams,final_e1_gc,suml6_e1_gc,sum_e1_zero_gc,sum_e1_one_gc,sum_e1_two_gc,sum_e1_three_gc,avgr_e1_gc,sdr_e1_gc)
# ######################################################################################################################################################
# #e2
# final_e2_gc <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_gc <- c()
# sum_e2_zero_gc <- c()
# sum_e2_one_gc <- c()
# sum_e2_two_gc <- c()
# sum_e2_three_gc <- c()
# avgr_e2_gc <- c()
# sdr_e2_gc <- c()
# l6_form_e2_gcsplitted <- c()
# form_e2_gc <- c()
# for(index_e2_gc in 1:length(e2_teams))
# {
#   for(index_e2_gc_cols in 1:e2_totalrounds)
#   {
#     index_e2_gc  <- row.names(e2_goalconceded_h) == e2_teams[index_e2_gc]
#     form_e2_gc <- e2_goalconceded_h[index_e2_gc ]
#     deleted_form_e2_gc <- form_e2_gc[!form_e2_gc[] == ""]
#     l6_form_e2_gc <- tail(deleted_form_e2_gc,e2_last_n_games)
#     l6_form_e2_gc <- as.numeric(l6_form_e2_gc)
#     suml6_e2_gc[index_e2_gc] <- sum(l6_form_e2_gc)
#     suml6_e2_gc[index_e2_gc] <- paste(suml6_e2_gc[index_e2_gc],sep = "")
#     sum_e2_zero_gc[index_e2_gc] <- length(which(l6_form_e2_gc == 0))
#     sum_e2_zero_gc[index_e2_gc] <- paste(sum_e2_zero_gc[index_e2_gc],sep = "")
#     sum_e2_one_gc[index_e2_gc] <- length(which(l6_form_e2_gc == 1))
#     sum_e2_one_gc[index_e2_gc] <- paste(sum_e2_one_gc[index_e2_gc],sep = "")
#     sum_e2_two_gc[index_e2_gc] <- length(which(l6_form_e2_gc >= 2))
#     sum_e2_two_gc[index_e2_gc] <- paste(sum_e2_two_gc[index_e2_gc],sep = "")
#     sum_e2_three_gc[index_e2_gc] <- length(which(l6_form_e2_gc >= 3))
#     sum_e2_three_gc[index_e2_gc] <- paste(sum_e2_three_gc[index_e2_gc],sep = "")
#     avgr_e2_gc[index_e2_gc] <- mean(l6_form_e2_gc)
#     avgr_e2_gc[index_e2_gc] <- paste(avgr_e2_gc[index_e2_gc],sep = "")
#     sdr_e2_gc[index_e2_gc] <- sd(l6_form_e2_gc)
#     sdr_e2_gc[index_e2_gc] <- paste(sdr_e2_gc[index_e2_gc],sep = "")
#     l6_form_e2_gc <- as.character(l6_form_e2_gc)
#     l6_form_e2_gc_flattened <- stri_paste(l6_form_e2_gc,collapse = '')
#     l6_form_e2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e2_gc_flattened),"")[[1]])
#     final_e2_gc[index_e2_gc,index_e2_gc_cols] <- l6_form_e2_gcsplitted[index_e2_gc_cols]
#   }
# }
#
# final_e2_gc[is.na(final_e2_gc)] <- ""
# e2_goalconcededmatrix <- cbind(e2_teams,final_e2_gc,suml6_e2_gc,sum_e2_zero_gc,sum_e2_one_gc,sum_e2_two_gc,sum_e2_three_gc,avgr_e2_gc,sdr_e2_gc)
# ###################################################################################################################################################
# #e3
# final_e3_gc <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_gc <- c()
# sum_e3_zero_gc <- c()
# sum_e3_one_gc <- c()
# sum_e3_two_gc <- c()
# sum_e3_three_gc <- c()
# avgr_e3_gc <- c()
# sdr_e3_gc <- c()
# l6_form_e3_gcsplitted <- c()
# form_e3_gc <- c()
# for(index_e3_gc in 1:length(e3_teams))
# {
#   for(index_e3_gc_cols in 1:e3_totalrounds)
#   {
#     index_e3_gc  <- row.names(e3_goalconceded_h) == e3_teams[index_e3_gc]
#     form_e3_gc <- e3_goalconceded_h[index_e3_gc ]
#     deleted_form_e3_gc <- form_e3_gc[!form_e3_gc[] == ""]
#     l6_form_e3_gc <- tail(deleted_form_e3_gc,e3_last_n_games)
#     l6_form_e3_gc <- as.numeric(l6_form_e3_gc)
#     suml6_e3_gc[index_e3_gc] <- sum(l6_form_e3_gc)
#     suml6_e3_gc[index_e3_gc] <- paste(suml6_e3_gc[index_e3_gc],sep = "")
#     sum_e3_zero_gc[index_e3_gc] <- length(which(l6_form_e3_gc == 0))
#     sum_e3_zero_gc[index_e3_gc] <- paste(sum_e3_zero_gc[index_e3_gc],sep = "")
#     sum_e3_one_gc[index_e3_gc] <- length(which(l6_form_e3_gc == 1))
#     sum_e3_one_gc[index_e3_gc] <- paste(sum_e3_one_gc[index_e3_gc],sep = "")
#     sum_e3_two_gc[index_e3_gc] <- length(which(l6_form_e3_gc >= 2))
#     sum_e3_two_gc[index_e3_gc] <- paste(sum_e3_two_gc[index_e3_gc],sep = "")
#     sum_e3_three_gc[index_e3_gc] <- length(which(l6_form_e3_gc >= 3))
#     sum_e3_three_gc[index_e3_gc] <- paste(sum_e3_three_gc[index_e3_gc],sep = "")
#     avgr_e3_gc[index_e3_gc] <- mean(l6_form_e3_gc)
#     avgr_e3_gc[index_e3_gc] <- paste(avgr_e3_gc[index_e3_gc],sep = "")
#     sdr_e3_gc[index_e3_gc] <- sd(l6_form_e3_gc)
#     sdr_e3_gc[index_e3_gc] <- paste(sdr_e3_gc[index_e3_gc],sep = "")
#     l6_form_e3_gc <- as.character(l6_form_e3_gc)
#     l6_form_e3_gc_flattened <- stri_paste(l6_form_e3_gc,collapse = '')
#     l6_form_e3_gcsplitted <- as.numeric(strsplit(as.character(l6_form_e3_gc_flattened),"")[[1]])
#     final_e3_gc[index_e3_gc,index_e3_gc_cols] <- l6_form_e3_gcsplitted[index_e3_gc_cols]
#   }
# }
#
# final_e3_gc[is.na(final_e3_gc)] <- ""
# e3_goalconcededmatrix <- cbind(e3_teams,final_e3_gc,suml6_e3_gc,sum_e3_zero_gc,sum_e3_one_gc,sum_e3_two_gc,sum_e3_three_gc,avgr_e3_gc,sdr_e3_gc)
# ###################################################################################################################################################
# #ec
# final_ec_gc <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_gc <- c()
# sum_ec_zero_gc <- c()
# sum_ec_one_gc <- c()
# sum_ec_two_gc <- c()
# sum_ec_three_gc <- c()
# avgr_ec_gc <- c()
# sdr_ec_gc <- c()
# l6_form_ec_gcsplitted <- c()
# form_ec_gc <- c()
# for(index_ec_gc in 1:length(ec_teams))
# {
#   for(index_ec_gc_cols in 1:ec_totalrounds)
#   {
#     index_ec_gc  <- row.names(ec_goalconceded_h) == ec_teams[index_ec_gc]
#     form_ec_gc <- ec_goalconceded_h[index_ec_gc ]
#     deleted_form_ec_gc <- form_ec_gc[!form_ec_gc[] == ""]
#     l6_form_ec_gc <- tail(deleted_form_ec_gc,ec_last_n_games)
#     l6_form_ec_gc <- as.numeric(l6_form_ec_gc)
#     suml6_ec_gc[index_ec_gc] <- sum(l6_form_ec_gc)
#     suml6_ec_gc[index_ec_gc] <- paste(suml6_ec_gc[index_ec_gc],sep = "")
#     sum_ec_zero_gc[index_ec_gc] <- length(which(l6_form_ec_gc == 0))
#     sum_ec_zero_gc[index_ec_gc] <- paste(sum_ec_zero_gc[index_ec_gc],sep = "")
#     sum_ec_one_gc[index_ec_gc] <- length(which(l6_form_ec_gc == 1))
#     sum_ec_one_gc[index_ec_gc] <- paste(sum_ec_one_gc[index_ec_gc],sep = "")
#     sum_ec_two_gc[index_ec_gc] <- length(which(l6_form_ec_gc >= 2))
#     sum_ec_two_gc[index_ec_gc] <- paste(sum_ec_two_gc[index_ec_gc],sep = "")
#     sum_ec_three_gc[index_ec_gc] <- length(which(l6_form_ec_gc >= 3))
#     sum_ec_three_gc[index_ec_gc] <- paste(sum_ec_three_gc[index_ec_gc],sep = "")
#     avgr_ec_gc[index_ec_gc] <- mean(l6_form_ec_gc)
#     avgr_ec_gc[index_ec_gc] <- paste(avgr_ec_gc[index_ec_gc],sep = "")
#     sdr_ec_gc[index_ec_gc] <- sd(l6_form_ec_gc)
#     sdr_ec_gc[index_ec_gc] <- paste(sdr_ec_gc[index_ec_gc],sep = "")
#     l6_form_ec_gc <- as.character(l6_form_ec_gc)
#     l6_form_ec_gc_flattened <- stri_paste(l6_form_ec_gc,collapse = '')
#     l6_form_ec_gcsplitted <- as.numeric(strsplit(as.character(l6_form_ec_gc_flattened),"")[[1]])
#     final_ec_gc[index_ec_gc,index_ec_gc_cols] <- l6_form_ec_gcsplitted[index_ec_gc_cols]
#   }
# }
#
# final_ec_gc[is.na(final_ec_gc)] <- ""
# ec_goalconcededmatrix <- cbind(ec_teams,final_ec_gc,suml6_ec_gc,sum_ec_zero_gc,sum_ec_one_gc,sum_ec_two_gc,sum_ec_three_gc,avgr_ec_gc,sdr_ec_gc)
# #################################################################################################################################################
# #f1
# final_f1_gc <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_gc <- c()
# sum_f1_zero_gc <- c()
# sum_f1_one_gc <- c()
# sum_f1_two_gc <- c()
# sum_f1_three_gc <- c()
# avgr_f1_gc <- c()
# sdr_f1_gc <- c()
# l6_form_f1_gcsplitted <- c()
# form_f1_gc <- c()
# for(index_f1_gc in 1:length(f1_teams))
# {
#   for(index_f1_gc_cols in 1:f1_totalrounds)
#   {
#     index_f1_gc  <- row.names(f1_goalconceded_h) == f1_teams[index_f1_gc]
#     form_f1_gc <- f1_goalconceded_h[index_f1_gc ]
#     deleted_form_f1_gc <- form_f1_gc[!form_f1_gc[] == ""]
#     l6_form_f1_gc <- tail(deleted_form_f1_gc,f1_last_n_games)
#     l6_form_f1_gc <- as.numeric(l6_form_f1_gc)
#     suml6_f1_gc[index_f1_gc] <- sum(l6_form_f1_gc)
#     suml6_f1_gc[index_f1_gc] <- paste(suml6_f1_gc[index_f1_gc],sep = "")
#     sum_f1_zero_gc[index_f1_gc] <- length(which(l6_form_f1_gc == 0))
#     sum_f1_zero_gc[index_f1_gc] <- paste(sum_f1_zero_gc[index_f1_gc],sep = "")
#     sum_f1_one_gc[index_f1_gc] <- length(which(l6_form_f1_gc == 1))
#     sum_f1_one_gc[index_f1_gc] <- paste(sum_f1_one_gc[index_f1_gc],sep = "")
#     sum_f1_two_gc[index_f1_gc] <- length(which(l6_form_f1_gc >= 2))
#     sum_f1_two_gc[index_f1_gc] <- paste(sum_f1_two_gc[index_f1_gc],sep = "")
#     sum_f1_three_gc[index_f1_gc] <- length(which(l6_form_f1_gc >= 3))
#     sum_f1_three_gc[index_f1_gc] <- paste(sum_f1_three_gc[index_f1_gc],sep = "")
#     avgr_f1_gc[index_f1_gc] <- mean(l6_form_f1_gc)
#     avgr_f1_gc[index_f1_gc] <- paste(avgr_f1_gc[index_f1_gc],sep = "")
#     sdr_f1_gc[index_f1_gc] <- sd(l6_form_f1_gc)
#     sdr_f1_gc[index_f1_gc] <- paste(sdr_f1_gc[index_f1_gc],sep = "")
#     l6_form_f1_gc <- as.character(l6_form_f1_gc)
#     l6_form_f1_gc_flattened <- stri_paste(l6_form_f1_gc,collapse = '')
#     l6_form_f1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_f1_gc_flattened),"")[[1]])
#     final_f1_gc[index_f1_gc,index_f1_gc_cols] <- l6_form_f1_gcsplitted[index_f1_gc_cols]
#   }
# }
#
# final_f1_gc[is.na(final_f1_gc)] <- ""
# f1_goalconcededmatrix <- cbind(f1_teams,final_f1_gc,suml6_f1_gc,sum_f1_zero_gc,sum_f1_one_gc,sum_f1_two_gc,sum_f1_three_gc,avgr_f1_gc,sdr_f1_gc)
# ###################################################################################################################################################
# #f2
# final_f2_gc <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_gc <- c()
# sum_f2_zero_gc <- c()
# sum_f2_one_gc <- c()
# sum_f2_two_gc <- c()
# sum_f2_three_gc <- c()
# avgr_f2_gc <- c()
# sdr_f2_gc <- c()
# l6_form_f2_gcsplitted <- c()
# form_f2_gc <- c()
# for(index_f2_gc in 1:length(f2_teams))
# {
#   for(index_f2_gc_cols in 1:f2_totalrounds)
#   {
#     index_f2_gc  <- row.names(f2_goalconceded_h) == f2_teams[index_f2_gc]
#     form_f2_gc <- f2_goalconceded_h[index_f2_gc ]
#     deleted_form_f2_gc <- form_f2_gc[!form_f2_gc[] == ""]
#     l6_form_f2_gc <- tail(deleted_form_f2_gc,f2_last_n_games)
#     l6_form_f2_gc <- as.numeric(l6_form_f2_gc)
#     suml6_f2_gc[index_f2_gc] <- sum(l6_form_f2_gc)
#     suml6_f2_gc[index_f2_gc] <- paste(suml6_f2_gc[index_f2_gc],sep = "")
#     sum_f2_zero_gc[index_f2_gc] <- length(which(l6_form_f2_gc == 0))
#     sum_f2_zero_gc[index_f2_gc] <- paste(sum_f2_zero_gc[index_f2_gc],sep = "")
#     sum_f2_one_gc[index_f2_gc] <- length(which(l6_form_f2_gc == 1))
#     sum_f2_one_gc[index_f2_gc] <- paste(sum_f2_one_gc[index_f2_gc],sep = "")
#     sum_f2_two_gc[index_f2_gc] <- length(which(l6_form_f2_gc >= 2))
#     sum_f2_two_gc[index_f2_gc] <- paste(sum_f2_two_gc[index_f2_gc],sep = "")
#     sum_f2_three_gc[index_f2_gc] <- length(which(l6_form_f2_gc >= 3))
#     sum_f2_three_gc[index_f2_gc] <- paste(sum_f2_three_gc[index_f2_gc],sep = "")
#     avgr_f2_gc[index_f2_gc] <- mean(l6_form_f2_gc)
#     avgr_f2_gc[index_f2_gc] <- paste(avgr_f2_gc[index_f2_gc],sep = "")
#     sdr_f2_gc[index_f2_gc] <- sd(l6_form_f2_gc)
#     sdr_f2_gc[index_f2_gc] <- paste(sdr_f2_gc[index_f2_gc],sep = "")
#     l6_form_f2_gc <- as.character(l6_form_f2_gc)
#     l6_form_f2_gc_flattened <- stri_paste(l6_form_f2_gc,collapse = '')
#     l6_form_f2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_f2_gc_flattened),"")[[1]])
#     final_f2_gc[index_f2_gc,index_f2_gc_cols] <- l6_form_f2_gcsplitted[index_f2_gc_cols]
#   }
# }
#
# final_f2_gc[is.na(final_f2_gc)] <- ""
# f2_goalconcededmatrix <- cbind(f2_teams,final_f2_gc,suml6_f2_gc,sum_f2_zero_gc,sum_f2_one_gc,sum_f2_two_gc,sum_f2_three_gc,avgr_f2_gc,sdr_f2_gc)
# #################################################################################################################################################
# #g1
# final_g1_gc <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_gc <- c()
# sum_g1_zero_gc <- c()
# sum_g1_one_gc <- c()
# sum_g1_two_gc <- c()
# sum_g1_three_gc <- c()
# avgr_g1_gc <- c()
# sdr_g1_gc <- c()
# l6_form_g1_gcsplitted <- c()
# form_g1_gc <- c()
# for(index_g1_gc in 1:length(g1_teams))
# {
#   for(index_g1_gc_cols in 1:g1_totalrounds)
#   {
#     index_g1_gc  <- row.names(g1_goalconceded_h) == g1_teams[index_g1_gc]
#     form_g1_gc <- g1_goalconceded_h[index_g1_gc ]
#     deleted_form_g1_gc <- form_g1_gc[!form_g1_gc[] == ""]
#     l6_form_g1_gc <- tail(deleted_form_g1_gc,g1_last_n_games)
#     l6_form_g1_gc <- as.numeric(l6_form_g1_gc)
#     suml6_g1_gc[index_g1_gc] <- sum(l6_form_g1_gc)
#     suml6_g1_gc[index_g1_gc] <- paste(suml6_g1_gc[index_g1_gc],sep = "")
#     sum_g1_zero_gc[index_g1_gc] <- length(which(l6_form_g1_gc == 0))
#     sum_g1_zero_gc[index_g1_gc] <- paste(sum_g1_zero_gc[index_g1_gc],sep = "")
#     sum_g1_one_gc[index_g1_gc] <- length(which(l6_form_g1_gc == 1))
#     sum_g1_one_gc[index_g1_gc] <- paste(sum_g1_one_gc[index_g1_gc],sep = "")
#     sum_g1_two_gc[index_g1_gc] <- length(which(l6_form_g1_gc >= 2))
#     sum_g1_two_gc[index_g1_gc] <- paste(sum_g1_two_gc[index_g1_gc],sep = "")
#     sum_g1_three_gc[index_g1_gc] <- length(which(l6_form_g1_gc >= 3))
#     sum_g1_three_gc[index_g1_gc] <- paste(sum_g1_three_gc[index_g1_gc],sep = "")
#     avgr_g1_gc[index_g1_gc] <- mean(l6_form_g1_gc)
#     avgr_g1_gc[index_g1_gc] <- paste(avgr_g1_gc[index_g1_gc],sep = "")
#     sdr_g1_gc[index_g1_gc] <- sd(l6_form_g1_gc)
#     sdr_g1_gc[index_g1_gc] <- paste(sdr_g1_gc[index_g1_gc],sep = "")
#     l6_form_g1_gc <- as.character(l6_form_g1_gc)
#     l6_form_g1_gc_flattened <- stri_paste(l6_form_g1_gc,collapse = '')
#     l6_form_g1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_g1_gc_flattened),"")[[1]])
#     final_g1_gc[index_g1_gc,index_g1_gc_cols] <- l6_form_g1_gcsplitted[index_g1_gc_cols]
#   }
# }
#
# final_g1_gc[is.na(final_g1_gc)] <- ""
# g1_goalconcededmatrix <- cbind(g1_teams,final_g1_gc,suml6_g1_gc,sum_g1_zero_gc,sum_g1_one_gc,sum_g1_two_gc,sum_g1_three_gc,avgr_g1_gc,sdr_g1_gc)
# #################################################################################################################################################
# #i1
# final_i1_gc <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_gc <- c()
# sum_i1_zero_gc <- c()
# sum_i1_one_gc <- c()
# sum_i1_two_gc <- c()
# sum_i1_three_gc <- c()
# avgr_i1_gc <- c()
# sdr_i1_gc <- c()
# l6_form_i1_gcsplitted <- c()
# form_i1_gc <- c()
# for(index_i1_gc in 1:length(i1_teams))
# {
#   for(index_i1_gc_cols in 1:i1_totalrounds)
#   {
#     index_i1_gc  <- row.names(i1_goalconceded_h) == i1_teams[index_i1_gc]
#     form_i1_gc <- i1_goalconceded_h[index_i1_gc ]
#     deleted_form_i1_gc <- form_i1_gc[!form_i1_gc[] == ""]
#     l6_form_i1_gc <- tail(deleted_form_i1_gc,i1_last_n_games)
#     l6_form_i1_gc <- as.numeric(l6_form_i1_gc)
#     suml6_i1_gc[index_i1_gc] <- sum(l6_form_i1_gc)
#     suml6_i1_gc[index_i1_gc] <- paste(suml6_i1_gc[index_i1_gc],sep = "")
#     sum_i1_zero_gc[index_i1_gc] <- length(which(l6_form_i1_gc == 0))
#     sum_i1_zero_gc[index_i1_gc] <- paste(sum_i1_zero_gc[index_i1_gc],sep = "")
#     sum_i1_one_gc[index_i1_gc] <- length(which(l6_form_i1_gc == 1))
#     sum_i1_one_gc[index_i1_gc] <- paste(sum_i1_one_gc[index_i1_gc],sep = "")
#     sum_i1_two_gc[index_i1_gc] <- length(which(l6_form_i1_gc >= 2))
#     sum_i1_two_gc[index_i1_gc] <- paste(sum_i1_two_gc[index_i1_gc],sep = "")
#     sum_i1_three_gc[index_i1_gc] <- length(which(l6_form_i1_gc >= 3))
#     sum_i1_three_gc[index_i1_gc] <- paste(sum_i1_three_gc[index_i1_gc],sep = "")
#     avgr_i1_gc[index_i1_gc] <- mean(l6_form_i1_gc)
#     avgr_i1_gc[index_i1_gc] <- paste(avgr_i1_gc[index_i1_gc],sep = "")
#     sdr_i1_gc[index_i1_gc] <- sd(l6_form_i1_gc)
#     sdr_i1_gc[index_i1_gc] <- paste(sdr_i1_gc[index_i1_gc],sep = "")
#     l6_form_i1_gc <- as.character(l6_form_i1_gc)
#     l6_form_i1_gc_flattened <- stri_paste(l6_form_i1_gc,collapse = '')
#     l6_form_i1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_i1_gc_flattened),"")[[1]])
#     final_i1_gc[index_i1_gc,index_i1_gc_cols] <- l6_form_i1_gcsplitted[index_i1_gc_cols]
#   }
# }
#
# final_i1_gc[is.na(final_i1_gc)] <- ""
# i1_goalconcededmatrix <- cbind(i1_teams,final_i1_gc,suml6_i1_gc,sum_i1_zero_gc,sum_i1_one_gc,sum_i1_two_gc,sum_i1_three_gc,avgr_i1_gc,sdr_i1_gc)
# ####################################################################################################################################################
# ##i2
# final_i2_gc <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_gc <- c()
# sum_i2_zero_gc <- c()
# sum_i2_one_gc <- c()
# sum_i2_two_gc <- c()
# sum_i2_three_gc <- c()
# avgr_i2_gc <- c()
# sdr_i2_gc <- c()
# l6_form_i2_gcsplitted <- c()
# form_i2_gc <- c()
# for(index_i2_gc in 1:length(i2_teams))
# {
#   for(index_i2_gc_cols in 1:i2_totalrounds)
#   {
#     index_i2_gc  <- row.names(i2_goalconceded_h) == i2_teams[index_i2_gc]
#     form_i2_gc <- i2_goalconceded_h[index_i2_gc ]
#     deleted_form_i2_gc <- form_i2_gc[!form_i2_gc[] == ""]
#     l6_form_i2_gc <- tail(deleted_form_i2_gc,i2_last_n_games)
#     l6_form_i2_gc <- as.numeric(l6_form_i2_gc)
#     suml6_i2_gc[index_i2_gc] <- sum(l6_form_i2_gc)
#     suml6_i2_gc[index_i2_gc] <- paste(suml6_i2_gc[index_i2_gc],sep = "")
#     sum_i2_zero_gc[index_i2_gc] <- length(which(l6_form_i2_gc == 0))
#     sum_i2_zero_gc[index_i2_gc] <- paste(sum_i2_zero_gc[index_i2_gc],sep = "")
#     sum_i2_one_gc[index_i2_gc] <- length(which(l6_form_i2_gc == 1))
#     sum_i2_one_gc[index_i2_gc] <- paste(sum_i2_one_gc[index_i2_gc],sep = "")
#     sum_i2_two_gc[index_i2_gc] <- length(which(l6_form_i2_gc >= 2))
#     sum_i2_two_gc[index_i2_gc] <- paste(sum_i2_two_gc[index_i2_gc],sep = "")
#     sum_i2_three_gc[index_i2_gc] <- length(which(l6_form_i2_gc >= 3))
#     sum_i2_three_gc[index_i2_gc] <- paste(sum_i2_three_gc[index_i2_gc],sep = "")
#     avgr_i2_gc[index_i2_gc] <- mean(l6_form_i2_gc)
#     avgr_i2_gc[index_i2_gc] <- paste(avgr_i2_gc[index_i2_gc],sep = "")
#     sdr_i2_gc[index_i2_gc] <- sd(l6_form_i2_gc)
#     sdr_i2_gc[index_i2_gc] <- paste(sdr_i2_gc[index_i2_gc],sep = "")
#     l6_form_i2_gc <- as.character(l6_form_i2_gc)
#     l6_form_i2_gc_flattened <- stri_paste(l6_form_i2_gc,collapse = '')
#     l6_form_i2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_i2_gc_flattened),"")[[1]])
#     final_i2_gc[index_i2_gc,index_i2_gc_cols] <- l6_form_i2_gcsplitted[index_i2_gc_cols]
#   }
# }
#
# final_i2_gc[is.na(final_i2_gc)] <- ""
# i2_goalconcededmatrix <- cbind(i2_teams,final_i2_gc,suml6_i2_gc,sum_i2_zero_gc,sum_i2_one_gc,sum_i2_two_gc,sum_i2_three_gc,avgr_i2_gc,sdr_i2_gc)
# ###########################################################################################################################
# #n1
# final_n1_gc <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_gc <- c()
# sum_n1_zero_gc <- c()
# sum_n1_one_gc <- c()
# sum_n1_two_gc <- c()
# sum_n1_three_gc <- c()
# avgr_n1_gc <- c()
# sdr_n1_gc <- c()
# l6_form_n1_gcsplitted <- c()
# form_n1_gc <- c()
# for(index_n1_gc in 1:length(n1_teams))
# {
#   for(index_n1_gc_cols in 1:n1_totalrounds)
#   {
#     index_n1_gc  <- row.names(n1_goalconceded_h) == n1_teams[index_n1_gc]
#     form_n1_gc <- n1_goalconceded_h[index_n1_gc ]
#     deleted_form_n1_gc <- form_n1_gc[!form_n1_gc[] == ""]
#     l6_form_n1_gc <- tail(deleted_form_n1_gc,n1_last_n_games)
#     l6_form_n1_gc <- as.numeric(l6_form_n1_gc)
#     suml6_n1_gc[index_n1_gc] <- sum(l6_form_n1_gc)
#     suml6_n1_gc[index_n1_gc] <- paste(suml6_n1_gc[index_n1_gc],sep = "")
#     sum_n1_zero_gc[index_n1_gc] <- length(which(l6_form_n1_gc == 0))
#     sum_n1_zero_gc[index_n1_gc] <- paste(sum_n1_zero_gc[index_n1_gc],sep = "")
#     sum_n1_one_gc[index_n1_gc] <- length(which(l6_form_n1_gc == 1))
#     sum_n1_one_gc[index_n1_gc] <- paste(sum_n1_one_gc[index_n1_gc],sep = "")
#     sum_n1_two_gc[index_n1_gc] <- length(which(l6_form_n1_gc >= 2))
#     sum_n1_two_gc[index_n1_gc] <- paste(sum_n1_two_gc[index_n1_gc],sep = "")
#     sum_n1_three_gc[index_n1_gc] <- length(which(l6_form_n1_gc >= 3))
#     sum_n1_three_gc[index_n1_gc] <- paste(sum_n1_three_gc[index_n1_gc],sep = "")
#     avgr_n1_gc[index_n1_gc] <- mean(l6_form_n1_gc)
#     avgr_n1_gc[index_n1_gc] <- paste(avgr_n1_gc[index_n1_gc],sep = "")
#     sdr_n1_gc[index_n1_gc] <- sd(l6_form_n1_gc)
#     sdr_n1_gc[index_n1_gc] <- paste(sdr_n1_gc[index_n1_gc],sep = "")
#     l6_form_n1_gc <- as.character(l6_form_n1_gc)
#     l6_form_n1_gc_flattened <- stri_paste(l6_form_n1_gc,collapse = '')
#     l6_form_n1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_n1_gc_flattened),"")[[1]])
#     final_n1_gc[index_n1_gc,index_n1_gc_cols] <- l6_form_n1_gcsplitted[index_n1_gc_cols]
#   }
# }
#
# final_n1_gc[is.na(final_n1_gc)] <- ""
# n1_goalconcededmatrix <- cbind(n1_teams,final_n1_gc,suml6_n1_gc,sum_n1_zero_gc,sum_n1_one_gc,sum_n1_two_gc,sum_n1_three_gc,avgr_n1_gc,sdr_n1_gc)
# ##################################################################################################################################################
# #p1
# final_p1_gc <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_gc <- c()
# sum_p1_zero_gc <- c()
# sum_p1_one_gc <- c()
# sum_p1_two_gc <- c()
# sum_p1_three_gc <- c()
# avgr_p1_gc <- c()
# sdr_p1_gc <- c()
# l6_form_p1_gcsplitted <- c()
# form_p1_gc <- c()
# for(index_p1_gc in 1:length(p1_teams))
# {
#   for(index_p1_gc_cols in 1:p1_totalrounds)
#   {
#     index_p1_gc  <- row.names(p1_goalconceded_h) == p1_teams[index_p1_gc]
#     form_p1_gc <- p1_goalconceded_h[index_p1_gc ]
#     deleted_form_p1_gc <- form_p1_gc[!form_p1_gc[] == ""]
#     l6_form_p1_gc <- tail(deleted_form_p1_gc,p1_last_n_games)
#     l6_form_p1_gc <- as.numeric(l6_form_p1_gc)
#     suml6_p1_gc[index_p1_gc] <- sum(l6_form_p1_gc)
#     suml6_p1_gc[index_p1_gc] <- paste(suml6_p1_gc[index_p1_gc],sep = "")
#     sum_p1_zero_gc[index_p1_gc] <- length(which(l6_form_p1_gc == 0))
#     sum_p1_zero_gc[index_p1_gc] <- paste(sum_p1_zero_gc[index_p1_gc],sep = "")
#     sum_p1_one_gc[index_p1_gc] <- length(which(l6_form_p1_gc == 1))
#     sum_p1_one_gc[index_p1_gc] <- paste(sum_p1_one_gc[index_p1_gc],sep = "")
#     sum_p1_two_gc[index_p1_gc] <- length(which(l6_form_p1_gc >= 2))
#     sum_p1_two_gc[index_p1_gc] <- paste(sum_p1_two_gc[index_p1_gc],sep = "")
#     sum_p1_three_gc[index_p1_gc] <- length(which(l6_form_p1_gc >= 3))
#     sum_p1_three_gc[index_p1_gc] <- paste(sum_p1_three_gc[index_p1_gc],sep = "")
#     avgr_p1_gc[index_p1_gc] <- mean(l6_form_p1_gc)
#     avgr_p1_gc[index_p1_gc] <- paste(avgr_p1_gc[index_p1_gc],sep = "")
#     sdr_p1_gc[index_p1_gc] <- sd(l6_form_p1_gc)
#     sdr_p1_gc[index_p1_gc] <- paste(sdr_p1_gc[index_p1_gc],sep = "")
#     l6_form_p1_gc <- as.character(l6_form_p1_gc)
#     l6_form_p1_gc_flattened <- stri_paste(l6_form_p1_gc,collapse = '')
#     l6_form_p1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_p1_gc_flattened),"")[[1]])
#     final_p1_gc[index_p1_gc,index_p1_gc_cols] <- l6_form_p1_gcsplitted[index_p1_gc_cols]
#   }
# }
#
# final_p1_gc[is.na(final_p1_gc)] <- ""
# p1_goalconcededmatrix <- cbind(p1_teams,final_p1_gc,suml6_p1_gc,sum_p1_zero_gc,sum_p1_one_gc,sum_p1_two_gc,sum_p1_three_gc,avgr_p1_gc,sdr_p1_gc)
# ###################################################################################################################################################
# #sp1
# final_sp1_gc <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_gc <- c()
# sum_sp1_zero_gc <- c()
# sum_sp1_one_gc <- c()
# sum_sp1_two_gc <- c()
# sum_sp1_three_gc <- c()
# avgr_sp1_gc <- c()
# sdr_sp1_gc <- c()
# l6_form_sp1_gcsplitted <- c()
# form_sp1_gc <- c()
# for(index_sp1_gc in 1:length(sp1_teams))
# {
#   for(index_sp1_gc_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_gc  <- row.names(sp1_goalconceded_h) == sp1_teams[index_sp1_gc]
#     form_sp1_gc <- sp1_goalconceded_h[index_sp1_gc ]
#     deleted_form_sp1_gc <- form_sp1_gc[!form_sp1_gc[] == ""]
#     l6_form_sp1_gc <- tail(deleted_form_sp1_gc,sp1_last_n_games)
#     l6_form_sp1_gc <- as.numeric(l6_form_sp1_gc)
#     suml6_sp1_gc[index_sp1_gc] <- sum(l6_form_sp1_gc)
#     suml6_sp1_gc[index_sp1_gc] <- paste(suml6_sp1_gc[index_sp1_gc],sep = "")
#     sum_sp1_zero_gc[index_sp1_gc] <- length(which(l6_form_sp1_gc == 0))
#     sum_sp1_zero_gc[index_sp1_gc] <- paste(sum_sp1_zero_gc[index_sp1_gc],sep = "")
#     sum_sp1_one_gc[index_sp1_gc] <- length(which(l6_form_sp1_gc == 1))
#     sum_sp1_one_gc[index_sp1_gc] <- paste(sum_sp1_one_gc[index_sp1_gc],sep = "")
#     sum_sp1_two_gc[index_sp1_gc] <- length(which(l6_form_sp1_gc >= 2))
#     sum_sp1_two_gc[index_sp1_gc] <- paste(sum_sp1_two_gc[index_sp1_gc],sep = "")
#     sum_sp1_three_gc[index_sp1_gc] <- length(which(l6_form_sp1_gc >= 3))
#     sum_sp1_three_gc[index_sp1_gc] <- paste(sum_sp1_three_gc[index_sp1_gc],sep = "")
#     avgr_sp1_gc[index_sp1_gc] <- mean(l6_form_sp1_gc)
#     avgr_sp1_gc[index_sp1_gc] <- paste(avgr_sp1_gc[index_sp1_gc],sep = "")
#     sdr_sp1_gc[index_sp1_gc] <- sd(l6_form_sp1_gc)
#     sdr_sp1_gc[index_sp1_gc] <- paste(sdr_sp1_gc[index_sp1_gc],sep = "")
#     l6_form_sp1_gc <- as.character(l6_form_sp1_gc)
#     l6_form_sp1_gc_flattened <- stri_paste(l6_form_sp1_gc,collapse = '')
#     l6_form_sp1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_gc_flattened),"")[[1]])
#     final_sp1_gc[index_sp1_gc,index_sp1_gc_cols] <- l6_form_sp1_gcsplitted[index_sp1_gc_cols]
#   }
# }
#
# final_sp1_gc[is.na(final_sp1_gc)] <- ""
# sp1_goalconcededmatrix <- cbind(sp1_teams,final_sp1_gc,suml6_sp1_gc,sum_sp1_zero_gc,sum_sp1_one_gc,sum_sp1_two_gc,sum_sp1_three_gc,avgr_sp1_gc,sdr_sp1_gc)
# ##########################################################################################################################
# #sp2
# final_sp2_gc <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_gc <- c()
# sum_sp2_zero_gc <- c()
# sum_sp2_one_gc <- c()
# sum_sp2_two_gc <- c()
# sum_sp2_three_gc <- c()
# avgr_sp2_gc <- c()
# sdr_sp2_gc <- c()
# l6_form_sp2_gcsplitted <- c()
# form_sp2_gc <- c()
# for(index_sp2_gc in 1:length(sp2_teams))
# {
#   for(index_sp2_gc_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_gc  <- row.names(sp2_goalconceded_h) == sp2_teams[index_sp2_gc]
#     form_sp2_gc <- sp2_goalconceded_h[index_sp2_gc ]
#     deleted_form_sp2_gc <- form_sp2_gc[!form_sp2_gc[] == ""]
#     l6_form_sp2_gc <- tail(deleted_form_sp2_gc,sp2_last_n_games)
#     l6_form_sp2_gc <- as.numeric(l6_form_sp2_gc)
#     suml6_sp2_gc[index_sp2_gc] <- sum(l6_form_sp2_gc)
#     suml6_sp2_gc[index_sp2_gc] <- paste(suml6_sp2_gc[index_sp2_gc],sep = "")
#     sum_sp2_zero_gc[index_sp2_gc] <- length(which(l6_form_sp2_gc == 0))
#     sum_sp2_zero_gc[index_sp2_gc] <- paste(sum_sp2_zero_gc[index_sp2_gc],sep = "")
#     sum_sp2_one_gc[index_sp2_gc] <- length(which(l6_form_sp2_gc == 1))
#     sum_sp2_one_gc[index_sp2_gc] <- paste(sum_sp2_one_gc[index_sp2_gc],sep = "")
#     sum_sp2_two_gc[index_sp2_gc] <- length(which(l6_form_sp2_gc >= 2))
#     sum_sp2_two_gc[index_sp2_gc] <- paste(sum_sp2_two_gc[index_sp2_gc],sep = "")
#     sum_sp2_three_gc[index_sp2_gc] <- length(which(l6_form_sp2_gc >= 3))
#     sum_sp2_three_gc[index_sp2_gc] <- paste(sum_sp2_three_gc[index_sp2_gc],sep = "")
#     avgr_sp2_gc[index_sp2_gc] <- mean(l6_form_sp2_gc)
#     avgr_sp2_gc[index_sp2_gc] <- paste(avgr_sp2_gc[index_sp2_gc],sep = "")
#     sdr_sp2_gc[index_sp2_gc] <- sd(l6_form_sp2_gc)
#     sdr_sp2_gc[index_sp2_gc] <- paste(sdr_sp2_gc[index_sp2_gc],sep = "")
#     l6_form_sp2_gc <- as.character(l6_form_sp2_gc)
#     l6_form_sp2_gc_flattened <- stri_paste(l6_form_sp2_gc,collapse = '')
#     l6_form_sp2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sp2_gc_flattened),"")[[1]])
#     final_sp2_gc[index_sp2_gc,index_sp2_gc_cols] <- l6_form_sp2_gcsplitted[index_sp2_gc_cols]
#   }
# }
#
# final_sp2_gc[is.na(final_sp2_gc)] <- ""
# sp2_goalconcededmatrix <- cbind(sp2_teams,final_sp2_gc,suml6_sp2_gc,sum_sp2_zero_gc,sum_sp2_one_gc,sum_sp2_two_gc,sum_sp2_three_gc,avgr_sp2_gc,sdr_sp2_gc)
# ###############################################################################################################################################################
# #sc0
# final_sc0_gc <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_gc <- c()
# sum_sc0_zero_gc <- c()
# sum_sc0_one_gc <- c()
# sum_sc0_two_gc <- c()
# sum_sc0_three_gc <- c()
# avgr_sc0_gc <- c()
# sdr_sc0_gc <- c()
# l6_form_sc0_gcsplitted <- c()
# form_sc0_gc <- c()
# for(index_sc0_gc in 1:length(sc0_teams))
# {
#   for(index_sc0_gc_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_gc  <- row.names(sc0_goalconceded_h) == sc0_teams[index_sc0_gc]
#     form_sc0_gc <- sc0_goalconceded_h[index_sc0_gc ]
#     deleted_form_sc0_gc <- form_sc0_gc[!form_sc0_gc[] == ""]
#     l6_form_sc0_gc <- tail(deleted_form_sc0_gc,sc0_last_n_games)
#     l6_form_sc0_gc <- as.numeric(l6_form_sc0_gc)
#     suml6_sc0_gc[index_sc0_gc] <- sum(l6_form_sc0_gc)
#     suml6_sc0_gc[index_sc0_gc] <- paste(suml6_sc0_gc[index_sc0_gc],sep = "")
#     sum_sc0_zero_gc[index_sc0_gc] <- length(which(l6_form_sc0_gc == 0))
#     sum_sc0_zero_gc[index_sc0_gc] <- paste(sum_sc0_zero_gc[index_sc0_gc],sep = "")
#     sum_sc0_one_gc[index_sc0_gc] <- length(which(l6_form_sc0_gc == 1))
#     sum_sc0_one_gc[index_sc0_gc] <- paste(sum_sc0_one_gc[index_sc0_gc],sep = "")
#     sum_sc0_two_gc[index_sc0_gc] <- length(which(l6_form_sc0_gc >= 2))
#     sum_sc0_two_gc[index_sc0_gc] <- paste(sum_sc0_two_gc[index_sc0_gc],sep = "")
#     sum_sc0_three_gc[index_sc0_gc] <- length(which(l6_form_sc0_gc >= 3))
#     sum_sc0_three_gc[index_sc0_gc] <- paste(sum_sc0_three_gc[index_sc0_gc],sep = "")
#     avgr_sc0_gc[index_sc0_gc] <- mean(l6_form_sc0_gc)
#     avgr_sc0_gc[index_sc0_gc] <- paste(avgr_sc0_gc[index_sc0_gc],sep = "")
#     sdr_sc0_gc[index_sc0_gc] <- sd(l6_form_sc0_gc)
#     sdr_sc0_gc[index_sc0_gc] <- paste(sdr_sc0_gc[index_sc0_gc],sep = "")
#     l6_form_sc0_gc <- as.character(l6_form_sc0_gc)
#     l6_form_sc0_gc_flattened <- stri_paste(l6_form_sc0_gc,collapse = '')
#     l6_form_sc0_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_gc_flattened),"")[[1]])
#     final_sc0_gc[index_sc0_gc,index_sc0_gc_cols] <- l6_form_sc0_gcsplitted[index_sc0_gc_cols]
#   }
# }
#
# final_sc0_gc[is.na(final_sc0_gc)] <- ""
# sc0_goalconcededmatrix <- cbind(sc0_teams,final_sc0_gc,suml6_sc0_gc,sum_sc0_zero_gc,sum_sc0_one_gc,sum_sc0_two_gc,sum_sc0_three_gc,avgr_sc0_gc,sdr_sc0_gc)
# ##########################################################################################################################
# #sc1
# final_sc1_gc <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_gc <- c()
# sum_sc1_zero_gc <- c()
# sum_sc1_one_gc <- c()
# sum_sc1_two_gc <- c()
# sum_sc1_three_gc <- c()
# avgr_sc1_gc <- c()
# sdr_sc1_gc <- c()
# l6_form_sc1_gcsplitted <- c()
# form_sc1_gc <- c()
# for(index_sc1_gc in 1:length(sc1_teams))
# {
#   for(index_sc1_gc_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_gc  <- row.names(sc1_goalconceded_h) == sc1_teams[index_sc1_gc]
#     form_sc1_gc <- sc1_goalconceded_h[index_sc1_gc ]
#     deleted_form_sc1_gc <- form_sc1_gc[!form_sc1_gc[] == ""]
#     l6_form_sc1_gc <- tail(deleted_form_sc1_gc,sc1_last_n_games)
#     l6_form_sc1_gc <- as.numeric(l6_form_sc1_gc)
#     suml6_sc1_gc[index_sc1_gc] <- sum(l6_form_sc1_gc)
#     suml6_sc1_gc[index_sc1_gc] <- paste(suml6_sc1_gc[index_sc1_gc],sep = "")
#     sum_sc1_zero_gc[index_sc1_gc] <- length(which(l6_form_sc1_gc == 0))
#     sum_sc1_zero_gc[index_sc1_gc] <- paste(sum_sc1_zero_gc[index_sc1_gc],sep = "")
#     sum_sc1_one_gc[index_sc1_gc] <- length(which(l6_form_sc1_gc == 1))
#     sum_sc1_one_gc[index_sc1_gc] <- paste(sum_sc1_one_gc[index_sc1_gc],sep = "")
#     sum_sc1_two_gc[index_sc1_gc] <- length(which(l6_form_sc1_gc >= 2))
#     sum_sc1_two_gc[index_sc1_gc] <- paste(sum_sc1_two_gc[index_sc1_gc],sep = "")
#     sum_sc1_three_gc[index_sc1_gc] <- length(which(l6_form_sc1_gc >= 3))
#     sum_sc1_three_gc[index_sc1_gc] <- paste(sum_sc1_three_gc[index_sc1_gc],sep = "")
#     avgr_sc1_gc[index_sc1_gc] <- mean(l6_form_sc1_gc)
#     avgr_sc1_gc[index_sc1_gc] <- paste(avgr_sc1_gc[index_sc1_gc],sep = "")
#     sdr_sc1_gc[index_sc1_gc] <- sd(l6_form_sc1_gc)
#     sdr_sc1_gc[index_sc1_gc] <- paste(sdr_sc1_gc[index_sc1_gc],sep = "")
#     l6_form_sc1_gc <- as.character(l6_form_sc1_gc)
#     l6_form_sc1_gc_flattened <- stri_paste(l6_form_sc1_gc,collapse = '')
#     l6_form_sc1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc1_gc_flattened),"")[[1]])
#     final_sc1_gc[index_sc1_gc,index_sc1_gc_cols] <- l6_form_sc1_gcsplitted[index_sc1_gc_cols]
#   }
# }
#
# final_sc1_gc[is.na(final_sc1_gc)] <- ""
# sc1_goalconcededmatrix <- cbind(sc1_teams,final_sc1_gc,suml6_sc1_gc,sum_sc1_zero_gc,sum_sc1_one_gc,sum_sc1_two_gc,sum_sc1_three_gc,avgr_sc1_gc,sdr_sc1_gc)
# ################################################################################################################################
# #sc2
# final_sc2_gc <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_gc <- c()
# sum_sc2_zero_gc <- c()
# sum_sc2_one_gc <- c()
# sum_sc2_two_gc <- c()
# sum_sc2_three_gc <- c()
# avgr_sc2_gc <- c()
# sdr_sc2_gc <- c()
# l6_form_sc2_gcsplitted <- c()
# form_sc2_gc <- c()
# for(index_sc2_gc in 1:length(sc2_teams))
# {
#   for(index_sc2_gc_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_gc  <- row.names(sc2_goalconceded_h) == sc2_teams[index_sc2_gc]
#     form_sc2_gc <- sc2_goalconceded_h[index_sc2_gc ]
#     deleted_form_sc2_gc <- form_sc2_gc[!form_sc2_gc[] == ""]
#     l6_form_sc2_gc <- tail(deleted_form_sc2_gc,sc2_last_n_games)
#     l6_form_sc2_gc <- as.numeric(l6_form_sc2_gc)
#     suml6_sc2_gc[index_sc2_gc] <- sum(l6_form_sc2_gc)
#     suml6_sc2_gc[index_sc2_gc] <- paste(suml6_sc2_gc[index_sc2_gc],sep = "")
#     sum_sc2_zero_gc[index_sc2_gc] <- length(which(l6_form_sc2_gc == 0))
#     sum_sc2_zero_gc[index_sc2_gc] <- paste(sum_sc2_zero_gc[index_sc2_gc],sep = "")
#     sum_sc2_one_gc[index_sc2_gc] <- length(which(l6_form_sc2_gc == 1))
#     sum_sc2_one_gc[index_sc2_gc] <- paste(sum_sc2_one_gc[index_sc2_gc],sep = "")
#     sum_sc2_two_gc[index_sc2_gc] <- length(which(l6_form_sc2_gc >= 2))
#     sum_sc2_two_gc[index_sc2_gc] <- paste(sum_sc2_two_gc[index_sc2_gc],sep = "")
#     sum_sc2_three_gc[index_sc2_gc] <- length(which(l6_form_sc2_gc >= 3))
#     sum_sc2_three_gc[index_sc2_gc] <- paste(sum_sc2_three_gc[index_sc2_gc],sep = "")
#     avgr_sc2_gc[index_sc2_gc] <- mean(l6_form_sc2_gc)
#     avgr_sc2_gc[index_sc2_gc] <- paste(avgr_sc2_gc[index_sc2_gc],sep = "")
#     sdr_sc2_gc[index_sc2_gc] <- sd(l6_form_sc2_gc)
#     sdr_sc2_gc[index_sc2_gc] <- paste(sdr_sc2_gc[index_sc2_gc],sep = "")
#     l6_form_sc2_gc <- as.character(l6_form_sc2_gc)
#     l6_form_sc2_gc_flattened <- stri_paste(l6_form_sc2_gc,collapse = '')
#     l6_form_sc2_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc2_gc_flattened),"")[[1]])
#     final_sc2_gc[index_sc2_gc,index_sc2_gc_cols] <- l6_form_sc2_gcsplitted[index_sc2_gc_cols]
#   }
# }
#
# final_sc2_gc[is.na(final_sc2_gc)] <- ""
# sc2_goalconcededmatrix <- cbind(sc2_teams,final_sc2_gc,suml6_sc2_gc,sum_sc2_zero_gc,sum_sc2_one_gc,sum_sc2_two_gc,sum_sc2_three_gc,avgr_sc2_gc,sdr_sc2_gc)
# #################################################################################################################################
# #sc3
# final_sc3_gc <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_gc <- c()
# sum_sc3_zero_gc <- c()
# sum_sc3_one_gc <- c()
# sum_sc3_two_gc <- c()
# sum_sc3_three_gc <- c()
# avgr_sc3_gc <- c()
# sdr_sc3_gc <- c()
# l6_form_sc3_gcsplitted <- c()
# form_sc3_gc <- c()
# for(index_sc3_gc in 1:length(sc3_teams))
# {
#   for(index_sc3_gc_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_gc  <- row.names(sc3_goalconceded_h) == sc3_teams[index_sc3_gc]
#     form_sc3_gc <- sc3_goalconceded_h[index_sc3_gc ]
#     deleted_form_sc3_gc <- form_sc3_gc[!form_sc3_gc[] == ""]
#     l6_form_sc3_gc <- tail(deleted_form_sc3_gc,sc3_last_n_games)
#     l6_form_sc3_gc <- as.numeric(l6_form_sc3_gc)
#     suml6_sc3_gc[index_sc3_gc] <- sum(l6_form_sc3_gc)
#     suml6_sc3_gc[index_sc3_gc] <- paste(suml6_sc3_gc[index_sc3_gc],sep = "")
#     sum_sc3_zero_gc[index_sc3_gc] <- length(which(l6_form_sc3_gc == 0))
#     sum_sc3_zero_gc[index_sc3_gc] <- paste(sum_sc3_zero_gc[index_sc3_gc],sep = "")
#     sum_sc3_one_gc[index_sc3_gc] <- length(which(l6_form_sc3_gc == 1))
#     sum_sc3_one_gc[index_sc3_gc] <- paste(sum_sc3_one_gc[index_sc3_gc],sep = "")
#     sum_sc3_two_gc[index_sc3_gc] <- length(which(l6_form_sc3_gc >= 2))
#     sum_sc3_two_gc[index_sc3_gc] <- paste(sum_sc3_two_gc[index_sc3_gc],sep = "")
#     sum_sc3_three_gc[index_sc3_gc] <- length(which(l6_form_sc3_gc >= 3))
#     sum_sc3_three_gc[index_sc3_gc] <- paste(sum_sc3_three_gc[index_sc3_gc],sep = "")
#     avgr_sc3_gc[index_sc3_gc] <- mean(l6_form_sc3_gc)
#     avgr_sc3_gc[index_sc3_gc] <- paste(avgr_sc3_gc[index_sc3_gc],sep = "")
#     sdr_sc3_gc[index_sc3_gc] <- sd(l6_form_sc3_gc)
#     sdr_sc3_gc[index_sc3_gc] <- paste(sdr_sc3_gc[index_sc3_gc],sep = "")
#     l6_form_sc3_gc <- as.character(l6_form_sc3_gc)
#     l6_form_sc3_gc_flattened <- stri_paste(l6_form_sc3_gc,collapse = '')
#     l6_form_sc3_gcsplitted <- as.numeric(strsplit(as.character(l6_form_sc3_gc_flattened),"")[[1]])
#     final_sc3_gc[index_sc3_gc,index_sc3_gc_cols] <- l6_form_sc3_gcsplitted[index_sc3_gc_cols]
#   }
# }
#
# final_sc3_gc[is.na(final_sc3_gc)] <- ""
# sc3_goalconcededmatrix <- cbind(sc3_teams,final_sc3_gc,suml6_sc3_gc,sum_sc3_zero_gc,sum_sc3_one_gc,sum_sc3_two_gc,sum_sc3_three_gc,avgr_sc3_gc,sdr_sc3_gc)
# ##################################################################################################################################
# #t1
# final_t1_gc <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_gc <- c()
# sum_t1_zero_gc <- c()
# sum_t1_one_gc <- c()
# sum_t1_two_gc <- c()
# sum_t1_three_gc <- c()
# avgr_t1_gc <- c()
# sdr_t1_gc <- c()
# l6_form_t1_gcsplitted <- c()
# form_t1_gc <- c()
# for(index_t1_gc in 1:length(t1_teams))
# {
#   for(index_t1_gc_cols in 1:t1_totalrounds)
#   {
#     index_t1_gc  <- row.names(t1_goalconceded_h) == t1_teams[index_t1_gc]
#     form_t1_gc <- t1_goalconceded_h[index_t1_gc ]
#     deleted_form_t1_gc <- form_t1_gc[!form_t1_gc[] == ""]
#     l6_form_t1_gc <- tail(deleted_form_t1_gc,t1_last_n_games)
#     l6_form_t1_gc <- as.numeric(l6_form_t1_gc)
#     suml6_t1_gc[index_t1_gc] <- sum(l6_form_t1_gc)
#     suml6_t1_gc[index_t1_gc] <- paste(suml6_t1_gc[index_t1_gc],sep = "")
#     sum_t1_zero_gc[index_t1_gc] <- length(which(l6_form_t1_gc == 0))
#     sum_t1_zero_gc[index_t1_gc] <- paste(sum_t1_zero_gc[index_t1_gc],sep = "")
#     sum_t1_one_gc[index_t1_gc] <- length(which(l6_form_t1_gc == 1))
#     sum_t1_one_gc[index_t1_gc] <- paste(sum_t1_one_gc[index_t1_gc],sep = "")
#     sum_t1_two_gc[index_t1_gc] <- length(which(l6_form_t1_gc >= 2))
#     sum_t1_two_gc[index_t1_gc] <- paste(sum_t1_two_gc[index_t1_gc],sep = "")
#     sum_t1_three_gc[index_t1_gc] <- length(which(l6_form_t1_gc >= 3))
#     sum_t1_three_gc[index_t1_gc] <- paste(sum_t1_three_gc[index_t1_gc],sep = "")
#     avgr_t1_gc[index_t1_gc] <- mean(l6_form_t1_gc)
#     avgr_t1_gc[index_t1_gc] <- paste(avgr_t1_gc[index_t1_gc],sep = "")
#     sdr_t1_gc[index_t1_gc] <- sd(l6_form_t1_gc)
#     sdr_t1_gc[index_t1_gc] <- paste(sdr_t1_gc[index_t1_gc],sep = "")
#     l6_form_t1_gc <- as.character(l6_form_t1_gc)
#     l6_form_t1_gc_flattened <- stri_paste(l6_form_t1_gc,collapse = '')
#     l6_form_t1_gcsplitted <- as.numeric(strsplit(as.character(l6_form_t1_gc_flattened),"")[[1]])
#     final_t1_gc[index_t1_gc,index_t1_gc_cols] <- l6_form_t1_gcsplitted[index_t1_gc_cols]
#   }
# }
#
# final_t1_gc[is.na(final_t1_gc)] <- ""
# t1_goalconcededmatrix <- cbind(t1_teams,final_t1_gc,suml6_t1_gc,sum_t1_zero_gc,sum_t1_one_gc,sum_t1_two_gc,sum_t1_three_gc,avgr_t1_gc,sdr_t1_gc)
# ##################################################################################################################################################





































