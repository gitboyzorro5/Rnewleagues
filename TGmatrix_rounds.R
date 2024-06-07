library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_tg <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_tg <- c()
avgr_mls_tg <- c()
sdr_mls_tg <- c()
l6_form_mls_tgsplitted <- c()
form_mls_tg <- c()
for(index_mls_tg in 1:length(mls_teams))
{
  for(index_mls_tg_cols in 1:mls_totalrounds)
  {
    index_mls_tg  <- row.names(mls_totalgoals_h) == mls_teams[index_mls_tg]
    form_mls_tg <- mls_totalgoals_h[index_mls_tg ]
    deleted_form_mls_tg <- form_mls_tg[!form_mls_tg[] == ""]
    l6_form_mls_tg <- tail(deleted_form_mls_tg,mls_last_n_games)
    l6_form_mls_tg <- as.numeric(l6_form_mls_tg)
    suml6_mls_tg[index_mls_tg] <- sum(l6_form_mls_tg)
    suml6_mls_tg[index_mls_tg] <- paste(suml6_mls_tg[index_mls_tg],sep = "")
    avgr_mls_tg[index_mls_tg] <- mean(l6_form_mls_tg)
    avgr_mls_tg[index_mls_tg] <- paste(avgr_mls_tg[index_mls_tg],sep = "")
    sdr_mls_tg[index_mls_tg] <- sd(l6_form_mls_tg)
    sdr_mls_tg[index_mls_tg] <- paste(sdr_mls_tg[index_mls_tg],sep = "")
    l6_form_mls_tg <- as.character(l6_form_mls_tg)
    l6_form_mls_tg_flattened <- stri_paste(l6_form_mls_tg,collapse = '')
    l6_form_mls_tgsplitted <- as.numeric(strsplit(as.character(l6_form_mls_tg_flattened),"")[[1]])
    final_mls_tg[index_mls_tg,index_mls_tg_cols] <- l6_form_mls_tgsplitted[index_mls_tg_cols]
  }
}

final_mls_tg[is.na(final_mls_tg)] <- ""
mls_goaltotalmatrix <- cbind(mls_teams,final_mls_tg,suml6_mls_tg,avgr_mls_tg,sdr_mls_tg)

##############################################################################################
#arg
final_arg_tg <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_tg <- c()
avgr_arg_tg <- c()
sdr_arg_tg <- c()
l6_form_arg_tgsplitted <- c()
form_arg_tg <- c()
for(index_arg_tg in 1:length(arg_teams))
{
  for(index_arg_tg_cols in 1:arg_totalrounds)
  {
    index_arg_tg  <- row.names(arg_totalgoals_h) == arg_teams[index_arg_tg]
    form_arg_tg <- arg_totalgoals_h[index_arg_tg ]
    deleted_form_arg_tg <- form_arg_tg[!form_arg_tg[] == ""]
    l6_form_arg_tg <- tail(deleted_form_arg_tg,arg_last_n_games)
    l6_form_arg_tg <- as.numeric(l6_form_arg_tg)
    suml6_arg_tg[index_arg_tg] <- sum(l6_form_arg_tg)
    suml6_arg_tg[index_arg_tg] <- paste(suml6_arg_tg[index_arg_tg],sep = "")
    avgr_arg_tg[index_arg_tg] <- mean(l6_form_arg_tg)
    avgr_arg_tg[index_arg_tg] <- paste(avgr_arg_tg[index_arg_tg],sep = "")
    sdr_arg_tg[index_arg_tg] <- sd(l6_form_arg_tg)
    sdr_arg_tg[index_arg_tg] <- paste(sdr_arg_tg[index_arg_tg],sep = "")
    l6_form_arg_tg <- as.character(l6_form_arg_tg)
    l6_form_arg_tg_flattened <- stri_paste(l6_form_arg_tg,collapse = '')
    l6_form_arg_tgsplitted <- as.numeric(strsplit(as.character(l6_form_arg_tg_flattened),"")[[1]])
    final_arg_tg[index_arg_tg,index_arg_tg_cols] <- l6_form_arg_tgsplitted[index_arg_tg_cols]
  }
}

final_arg_tg[is.na(final_arg_tg)] <- ""
arg_goaltotalmatrix <- cbind(arg_teams,final_arg_tg,suml6_arg_tg,avgr_arg_tg,sdr_arg_tg)

##############################################################################################
#d2
# final_d2_tg <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_tg <- c()
# avgr_d2_tg <- c()
# sdr_d2_tg <- c()
# l6_form_d2_tgsplitted <- c()
# form_d2_tg <- c()
# for(index_d2_tg in 1:length(d2_teams))
# {
#   for(index_d2_tg_cols in 1:d2_totalrounds)
#   {
#     index_d2_tg  <- row.names(d2_totalgoals_h) == d2_teams[index_d2_tg]
#     form_d2_tg <- d2_totalgoals_h[index_d2_tg ]
#     deleted_form_d2_tg <- form_d2_tg[!form_d2_tg[] == ""]
#     l6_form_d2_tg <- tail(deleted_form_d2_tg,d2_last_n_games)
#     l6_form_d2_tg <- as.numeric(l6_form_d2_tg)
#     suml6_d2_tg[index_d2_tg] <- sum(l6_form_d2_tg)
#     suml6_d2_tg[index_d2_tg] <- paste(suml6_d2_tg[index_d2_tg],sep = "")
#     avgr_d2_tg[index_d2_tg] <- mean(l6_form_d2_tg)
#     avgr_d2_tg[index_d2_tg] <- paste(avgr_d2_tg[index_d2_tg],sep = "")
#     sdr_d2_tg[index_d2_tg] <- sd(l6_form_d2_tg)
#     sdr_d2_tg[index_d2_tg] <- paste(sdr_d2_tg[index_d2_tg],sep = "")
#     l6_form_d2_tg <- as.character(l6_form_d2_tg)
#     l6_form_d2_tg_flattened <- stri_paste(l6_form_d2_tg,collapse = '')
#     l6_form_d2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_d2_tg_flattened),"")[[1]])
#     final_d2_tg[index_d2_tg,index_d2_tg_cols] <- l6_form_d2_tgsplitted[index_d2_tg_cols]
#   }
# }
#
# final_d2_tg[is.na(final_d2_tg)] <- ""
# d2_goaltotalmatrix <- cbind(d2_teams,final_d2_tg,suml6_d2_tg,avgr_d2_tg,sdr_d2_tg)
# ##############################################################################################
# #e0
# final_e0_tg <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_tg <- c()
# avgr_e0_tg <- c()
# sdr_e0_tg <- c()
# l6_form_e0_tgsplitted <- c()
# form_e0_tg <- c()
# for(index_e0_tg in 1:length(e0_teams))
# {
#   for(index_e0_tg_cols in 1:e0_totalrounds)
#   {
#     index_e0_tg  <- row.names(e0_totalgoals_h) == e0_teams[index_e0_tg]
#     form_e0_tg <- e0_totalgoals_h[index_e0_tg ]
#     deleted_form_e0_tg <- form_e0_tg[!form_e0_tg[] == ""]
#     l6_form_e0_tg <- tail(deleted_form_e0_tg,e0_last_n_games)
#     l6_form_e0_tg <- as.numeric(l6_form_e0_tg)
#     suml6_e0_tg[index_e0_tg] <- sum(l6_form_e0_tg)
#     suml6_e0_tg[index_e0_tg] <- paste(suml6_e0_tg[index_e0_tg],sep = "")
#     avgr_e0_tg[index_e0_tg] <- mean(l6_form_e0_tg)
#     avgr_e0_tg[index_e0_tg] <- paste(avgr_e0_tg[index_e0_tg],sep = "")
#     sdr_e0_tg[index_e0_tg] <- sd(l6_form_e0_tg)
#     sdr_e0_tg[index_e0_tg] <- paste(sdr_e0_tg[index_e0_tg],sep = "")
#     l6_form_e0_tg <- as.character(l6_form_e0_tg)
#     l6_form_e0_tg_flattened <- stri_paste(l6_form_e0_tg,collapse = '')
#     l6_form_e0_tgsplitted <- as.numeric(strsplit(as.character(l6_form_e0_tg_flattened),"")[[1]])
#     final_e0_tg[index_e0_tg,index_e0_tg_cols] <- l6_form_e0_tgsplitted[index_e0_tg_cols]
#   }
# }
#
# final_e0_tg[is.na(final_e0_tg)] <- ""
# e0_goaltotalmatrix <- cbind(e0_teams,final_e0_tg,suml6_e0_tg,avgr_e0_tg,sdr_e0_tg)
# ##############################################################################################
# #e1
# final_e1_tg <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_tg <- c()
# avgr_e1_tg <- c()
# sdr_e1_tg <- c()
# l6_form_e1_tgsplitted <- c()
# form_e1_tg <- c()
# for(index_e1_tg in 1:length(e1_teams))
# {
#   for(index_e1_tg_cols in 1:e1_totalrounds)
#   {
#     index_e1_tg  <- row.names(e1_totalgoals_h) == e1_teams[index_e1_tg]
#     form_e1_tg <- e1_totalgoals_h[index_e1_tg ]
#     deleted_form_e1_tg <- form_e1_tg[!form_e1_tg[] == ""]
#     l6_form_e1_tg <- tail(deleted_form_e1_tg,e1_last_n_games)
#     l6_form_e1_tg <- as.numeric(l6_form_e1_tg)
#     suml6_e1_tg[index_e1_tg] <- sum(l6_form_e1_tg)
#     suml6_e1_tg[index_e1_tg] <- paste(suml6_e1_tg[index_e1_tg],sep = "")
#     avgr_e1_tg[index_e1_tg] <- mean(l6_form_e1_tg)
#     avgr_e1_tg[index_e1_tg] <- paste(avgr_e1_tg[index_e1_tg],sep = "")
#     sdr_e1_tg[index_e1_tg] <- sd(l6_form_e1_tg)
#     sdr_e1_tg[index_e1_tg] <- paste(sdr_e1_tg[index_e1_tg],sep = "")
#     l6_form_e1_tg <- as.character(l6_form_e1_tg)
#     l6_form_e1_tg_flattened <- stri_paste(l6_form_e1_tg,collapse = '')
#     l6_form_e1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_e1_tg_flattened),"")[[1]])
#     final_e1_tg[index_e1_tg,index_e1_tg_cols] <- l6_form_e1_tgsplitted[index_e1_tg_cols]
#   }
# }
#
# final_e1_tg[is.na(final_e1_tg)] <- ""
# e1_goaltotalmatrix <- cbind(e1_teams,final_e1_tg,suml6_e1_tg,avgr_e1_tg,sdr_e1_tg)
# ##############################################################################################
# #e2
# final_e2_tg <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_tg <- c()
# avgr_e2_tg <- c()
# sdr_e2_tg <- c()
# l6_form_e2_tgsplitted <- c()
# form_e2_tg <- c()
# for(index_e2_tg in 1:length(e2_teams))
# {
#   for(index_e2_tg_cols in 1:e2_totalrounds)
#   {
#     index_e2_tg  <- row.names(e2_totalgoals_h) == e2_teams[index_e2_tg]
#     form_e2_tg <- e2_totalgoals_h[index_e2_tg ]
#     deleted_form_e2_tg <- form_e2_tg[!form_e2_tg[] == ""]
#     l6_form_e2_tg <- tail(deleted_form_e2_tg,e2_last_n_games)
#     l6_form_e2_tg <- as.numeric(l6_form_e2_tg)
#     suml6_e2_tg[index_e2_tg] <- sum(l6_form_e2_tg)
#     suml6_e2_tg[index_e2_tg] <- paste(suml6_e2_tg[index_e2_tg],sep = "")
#     avgr_e2_tg[index_e2_tg] <- mean(l6_form_e2_tg)
#     avgr_e2_tg[index_e2_tg] <- paste(avgr_e2_tg[index_e2_tg],sep = "")
#     sdr_e2_tg[index_e2_tg] <- sd(l6_form_e2_tg)
#     sdr_e2_tg[index_e2_tg] <- paste(sdr_e2_tg[index_e2_tg],sep = "")
#     l6_form_e2_tg <- as.character(l6_form_e2_tg)
#     l6_form_e2_tg_flattened <- stri_paste(l6_form_e2_tg,collapse = '')
#     l6_form_e2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_e2_tg_flattened),"")[[1]])
#     final_e2_tg[index_e2_tg,index_e2_tg_cols] <- l6_form_e2_tgsplitted[index_e2_tg_cols]
#   }
# }
#
# final_e2_tg[is.na(final_e2_tg)] <- ""
# e2_goaltotalmatrix <- cbind(e2_teams,final_e2_tg,suml6_e2_tg,avgr_e2_tg,sdr_e2_tg)
# ##############################################################################################
# #e3
# final_e3_tg <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_tg <- c()
# avgr_e3_tg <- c()
# sdr_e3_tg <- c()
# l6_form_e3_tgsplitted <- c()
# form_e3_tg <- c()
# for(index_e3_tg in 1:length(e3_teams))
# {
#   for(index_e3_tg_cols in 1:e3_totalrounds)
#   {
#     index_e3_tg  <- row.names(e3_totalgoals_h) == e3_teams[index_e3_tg]
#     form_e3_tg <- e3_totalgoals_h[index_e3_tg ]
#     deleted_form_e3_tg <- form_e3_tg[!form_e3_tg[] == ""]
#     l6_form_e3_tg <- tail(deleted_form_e3_tg,e3_last_n_games)
#     l6_form_e3_tg <- as.numeric(l6_form_e3_tg)
#     suml6_e3_tg[index_e3_tg] <- sum(l6_form_e3_tg)
#     suml6_e3_tg[index_e3_tg] <- paste(suml6_e3_tg[index_e3_tg],sep = "")
#     avgr_e3_tg[index_e3_tg] <- mean(l6_form_e3_tg)
#     avgr_e3_tg[index_e3_tg] <- paste(avgr_e3_tg[index_e3_tg],sep = "")
#     sdr_e3_tg[index_e3_tg] <- sd(l6_form_e3_tg)
#     sdr_e3_tg[index_e3_tg] <- paste(sdr_e3_tg[index_e3_tg],sep = "")
#     l6_form_e3_tg <- as.character(l6_form_e3_tg)
#     l6_form_e3_tg_flattened <- stri_paste(l6_form_e3_tg,collapse = '')
#     l6_form_e3_tgsplitted <- as.numeric(strsplit(as.character(l6_form_e3_tg_flattened),"")[[1]])
#     final_e3_tg[index_e3_tg,index_e3_tg_cols] <- l6_form_e3_tgsplitted[index_e3_tg_cols]
#   }
# }
#
# final_e3_tg[is.na(final_e3_tg)] <- ""
# e3_goaltotalmatrix <- cbind(e3_teams,final_e3_tg,suml6_e3_tg,avgr_e3_tg,sdr_e3_tg)
# ##############################################################################################
# #ec
# final_ec_tg <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_tg <- c()
# avgr_ec_tg <- c()
# sdr_ec_tg <- c()
# l6_form_ec_tgsplitted <- c()
# form_ec_tg <- c()
# for(index_ec_tg in 1:length(ec_teams))
# {
#   for(index_ec_tg_cols in 1:ec_totalrounds)
#   {
#     index_ec_tg  <- row.names(ec_totalgoals_h) == ec_teams[index_ec_tg]
#     form_ec_tg <- ec_totalgoals_h[index_ec_tg ]
#     deleted_form_ec_tg <- form_ec_tg[!form_ec_tg[] == ""]
#     l6_form_ec_tg <- tail(deleted_form_ec_tg,ec_last_n_games)
#     l6_form_ec_tg <- as.numeric(l6_form_ec_tg)
#     suml6_ec_tg[index_ec_tg] <- sum(l6_form_ec_tg)
#     suml6_ec_tg[index_ec_tg] <- paste(suml6_ec_tg[index_ec_tg],sep = "")
#     avgr_ec_tg[index_ec_tg] <- mean(l6_form_ec_tg)
#     avgr_ec_tg[index_ec_tg] <- paste(avgr_ec_tg[index_ec_tg],sep = "")
#     sdr_ec_tg[index_ec_tg] <- sd(l6_form_ec_tg)
#     sdr_ec_tg[index_ec_tg] <- paste(sdr_ec_tg[index_ec_tg],sep = "")
#     l6_form_ec_tg <- as.character(l6_form_ec_tg)
#     l6_form_ec_tg_flattened <- stri_paste(l6_form_ec_tg,collapse = '')
#     l6_form_ec_tgsplitted <- as.numeric(strsplit(as.character(l6_form_ec_tg_flattened),"")[[1]])
#     final_ec_tg[index_ec_tg,index_ec_tg_cols] <- l6_form_ec_tgsplitted[index_ec_tg_cols]
#   }
# }
#
# final_ec_tg[is.na(final_ec_tg)] <- ""
# ec_goaltotalmatrix <- cbind(ec_teams,final_ec_tg,suml6_ec_tg,avgr_ec_tg,sdr_ec_tg)
# ##############################################################################################
# #f1
# final_f1_tg <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_tg <- c()
# avgr_f1_tg <- c()
# sdr_f1_tg <- c()
# l6_form_f1_tgsplitted <- c()
# form_f1_tg <- c()
# for(index_f1_tg in 1:length(f1_teams))
# {
#   for(index_f1_tg_cols in 1:f1_totalrounds)
#   {
#     index_f1_tg  <- row.names(f1_totalgoals_h) == f1_teams[index_f1_tg]
#     form_f1_tg <- f1_totalgoals_h[index_f1_tg ]
#     deleted_form_f1_tg <- form_f1_tg[!form_f1_tg[] == ""]
#     l6_form_f1_tg <- tail(deleted_form_f1_tg,f1_last_n_games)
#     l6_form_f1_tg <- as.numeric(l6_form_f1_tg)
#     suml6_f1_tg[index_f1_tg] <- sum(l6_form_f1_tg)
#     suml6_f1_tg[index_f1_tg] <- paste(suml6_f1_tg[index_f1_tg],sep = "")
#     avgr_f1_tg[index_f1_tg] <- mean(l6_form_f1_tg)
#     avgr_f1_tg[index_f1_tg] <- paste(avgr_f1_tg[index_f1_tg],sep = "")
#     sdr_f1_tg[index_f1_tg] <- sd(l6_form_f1_tg)
#     sdr_f1_tg[index_f1_tg] <- paste(sdr_f1_tg[index_f1_tg],sep = "")
#     l6_form_f1_tg <- as.character(l6_form_f1_tg)
#     l6_form_f1_tg_flattened <- stri_paste(l6_form_f1_tg,collapse = '')
#     l6_form_f1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_f1_tg_flattened),"")[[1]])
#     final_f1_tg[index_f1_tg,index_f1_tg_cols] <- l6_form_f1_tgsplitted[index_f1_tg_cols]
#   }
# }
#
# final_f1_tg[is.na(final_f1_tg)] <- ""
# f1_goaltotalmatrix <- cbind(f1_teams,final_f1_tg,suml6_f1_tg,avgr_f1_tg,sdr_f1_tg)
# ##############################################################################################
# #f2
# final_f2_tg <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_tg <- c()
# avgr_f2_tg <- c()
# sdr_f2_tg <- c()
# l6_form_f2_tgsplitted <- c()
# form_f2_tg <- c()
# for(index_f2_tg in 1:length(f2_teams))
# {
#   for(index_f2_tg_cols in 1:f2_totalrounds)
#   {
#     index_f2_tg  <- row.names(f2_totalgoals_h) == f2_teams[index_f2_tg]
#     form_f2_tg <- f2_totalgoals_h[index_f2_tg ]
#     deleted_form_f2_tg <- form_f2_tg[!form_f2_tg[] == ""]
#     l6_form_f2_tg <- tail(deleted_form_f2_tg,f2_last_n_games)
#     l6_form_f2_tg <- as.numeric(l6_form_f2_tg)
#     suml6_f2_tg[index_f2_tg] <- sum(l6_form_f2_tg)
#     suml6_f2_tg[index_f2_tg] <- paste(suml6_f2_tg[index_f2_tg],sep = "")
#     avgr_f2_tg[index_f2_tg] <- mean(l6_form_f2_tg)
#     avgr_f2_tg[index_f2_tg] <- paste(avgr_f2_tg[index_f2_tg],sep = "")
#     sdr_f2_tg[index_f2_tg] <- sd(l6_form_f2_tg)
#     sdr_f2_tg[index_f2_tg] <- paste(sdr_f2_tg[index_f2_tg],sep = "")
#     l6_form_f2_tg <- as.character(l6_form_f2_tg)
#     l6_form_f2_tg_flattened <- stri_paste(l6_form_f2_tg,collapse = '')
#     l6_form_f2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_f2_tg_flattened),"")[[1]])
#     final_f2_tg[index_f2_tg,index_f2_tg_cols] <- l6_form_f2_tgsplitted[index_f2_tg_cols]
#   }
# }
#
# final_f2_tg[is.na(final_f2_tg)] <- ""
# f2_goaltotalmatrix <- cbind(f2_teams,final_f2_tg,suml6_f2_tg,avgr_f2_tg,sdr_f2_tg)
# ##############################################################################################
# #g1
# final_g1_tg <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_tg <- c()
# avgr_g1_tg <- c()
# sdr_g1_tg <- c()
# l6_form_g1_tgsplitted <- c()
# form_g1_tg <- c()
# for(index_g1_tg in 1:length(g1_teams))
# {
#   for(index_g1_tg_cols in 1:g1_totalrounds)
#   {
#     index_g1_tg  <- row.names(g1_totalgoals_h) == g1_teams[index_g1_tg]
#     form_g1_tg <- g1_totalgoals_h[index_g1_tg ]
#     deleted_form_g1_tg <- form_g1_tg[!form_g1_tg[] == ""]
#     l6_form_g1_tg <- tail(deleted_form_g1_tg,g1_last_n_games)
#     l6_form_g1_tg <- as.numeric(l6_form_g1_tg)
#     suml6_g1_tg[index_g1_tg] <- sum(l6_form_g1_tg)
#     suml6_g1_tg[index_g1_tg] <- paste(suml6_g1_tg[index_g1_tg],sep = "")
#     avgr_g1_tg[index_g1_tg] <- mean(l6_form_g1_tg)
#     avgr_g1_tg[index_g1_tg] <- paste(avgr_g1_tg[index_g1_tg],sep = "")
#     sdr_g1_tg[index_g1_tg] <- sd(l6_form_g1_tg)
#     sdr_g1_tg[index_g1_tg] <- paste(sdr_g1_tg[index_g1_tg],sep = "")
#     l6_form_g1_tg <- as.character(l6_form_g1_tg)
#     l6_form_g1_tg_flattened <- stri_paste(l6_form_g1_tg,collapse = '')
#     l6_form_g1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_g1_tg_flattened),"")[[1]])
#     final_g1_tg[index_g1_tg,index_g1_tg_cols] <- l6_form_g1_tgsplitted[index_g1_tg_cols]
#   }
# }
#
# final_g1_tg[is.na(final_g1_tg)] <- ""
# g1_goaltotalmatrix <- cbind(g1_teams,final_g1_tg,suml6_g1_tg,avgr_g1_tg,sdr_g1_tg)
# ##############################################################################################
# #i1
# final_i1_tg <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_tg <- c()
# avgr_i1_tg <- c()
# sdr_i1_tg <- c()
# l6_form_i1_tgsplitted <- c()
# form_i1_tg <- c()
# for(index_i1_tg in 1:length(i1_teams))
# {
#   for(index_i1_tg_cols in 1:i1_totalrounds)
#   {
#     index_i1_tg  <- row.names(i1_totalgoals_h) == i1_teams[index_i1_tg]
#     form_i1_tg <- i1_totalgoals_h[index_i1_tg ]
#     deleted_form_i1_tg <- form_i1_tg[!form_i1_tg[] == ""]
#     l6_form_i1_tg <- tail(deleted_form_i1_tg,i1_last_n_games)
#     l6_form_i1_tg <- as.numeric(l6_form_i1_tg)
#     suml6_i1_tg[index_i1_tg] <- sum(l6_form_i1_tg)
#     suml6_i1_tg[index_i1_tg] <- paste(suml6_i1_tg[index_i1_tg],sep = "")
#     avgr_i1_tg[index_i1_tg] <- mean(l6_form_i1_tg)
#     avgr_i1_tg[index_i1_tg] <- paste(avgr_i1_tg[index_i1_tg],sep = "")
#     sdr_i1_tg[index_i1_tg] <- sd(l6_form_i1_tg)
#     sdr_i1_tg[index_i1_tg] <- paste(sdr_i1_tg[index_i1_tg],sep = "")
#     l6_form_i1_tg <- as.character(l6_form_i1_tg)
#     l6_form_i1_tg_flattened <- stri_paste(l6_form_i1_tg,collapse = '')
#     l6_form_i1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_i1_tg_flattened),"")[[1]])
#     final_i1_tg[index_i1_tg,index_i1_tg_cols] <- l6_form_i1_tgsplitted[index_i1_tg_cols]
#   }
# }
#
# final_i1_tg[is.na(final_i1_tg)] <- ""
# i1_goaltotalmatrix <- cbind(i1_teams,final_i1_tg,suml6_i1_tg,avgr_i1_tg,sdr_i1_tg)
# ##############################################################################################
# ##i2
# final_i2_tg <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_tg <- c()
# avgr_i2_tg <- c()
# sdr_i2_tg <- c()
# l6_form_i2_tgsplitted <- c()
# form_i2_tg <- c()
# for(index_i2_tg in 1:length(i2_teams))
# {
#   for(index_i2_tg_cols in 1:i2_totalrounds)
#   {
#     index_i2_tg  <- row.names(i2_totalgoals_h) == i2_teams[index_i2_tg]
#     form_i2_tg <- i2_totalgoals_h[index_i2_tg ]
#     deleted_form_i2_tg <- form_i2_tg[!form_i2_tg[] == ""]
#     l6_form_i2_tg <- tail(deleted_form_i2_tg,i2_last_n_games)
#     l6_form_i2_tg <- as.numeric(l6_form_i2_tg)
#     suml6_i2_tg[index_i2_tg] <- sum(l6_form_i2_tg)
#     suml6_i2_tg[index_i2_tg] <- paste(suml6_i2_tg[index_i2_tg],sep = "")
#     avgr_i2_tg[index_i2_tg] <- mean(l6_form_i2_tg)
#     avgr_i2_tg[index_i2_tg] <- paste(avgr_i2_tg[index_i2_tg],sep = "")
#     sdr_i2_tg[index_i2_tg] <- sd(l6_form_i2_tg)
#     sdr_i2_tg[index_i2_tg] <- paste(sdr_i2_tg[index_i2_tg],sep = "")
#     l6_form_i2_tg <- as.character(l6_form_i2_tg)
#     l6_form_i2_tg_flattened <- stri_paste(l6_form_i2_tg,collapse = '')
#     l6_form_i2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_i2_tg_flattened),"")[[1]])
#     final_i2_tg[index_i2_tg,index_i2_tg_cols] <- l6_form_i2_tgsplitted[index_i2_tg_cols]
#   }
# }
#
# final_i2_tg[is.na(final_i2_tg)] <- ""
# i2_goaltotalmatrix <- cbind(i2_teams,final_i2_tg,suml6_i2_tg,avgr_i2_tg,sdr_i2_tg)
# ##############################################################################################
# #n1
# final_n1_tg <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_tg <- c()
# avgr_n1_tg <- c()
# sdr_n1_tg <- c()
# l6_form_n1_tgsplitted <- c()
# form_n1_tg <- c()
# for(index_n1_tg in 1:length(n1_teams))
# {
#   for(index_n1_tg_cols in 1:n1_totalrounds)
#   {
#     index_n1_tg  <- row.names(n1_totalgoals_h) == n1_teams[index_n1_tg]
#     form_n1_tg <- n1_totalgoals_h[index_n1_tg ]
#     deleted_form_n1_tg <- form_n1_tg[!form_n1_tg[] == ""]
#     l6_form_n1_tg <- tail(deleted_form_n1_tg,n1_last_n_games)
#     l6_form_n1_tg <- as.numeric(l6_form_n1_tg)
#     suml6_n1_tg[index_n1_tg] <- sum(l6_form_n1_tg)
#     suml6_n1_tg[index_n1_tg] <- paste(suml6_n1_tg[index_n1_tg],sep = "")
#     avgr_n1_tg[index_n1_tg] <- mean(l6_form_n1_tg)
#     avgr_n1_tg[index_n1_tg] <- paste(avgr_n1_tg[index_n1_tg],sep = "")
#     sdr_n1_tg[index_n1_tg] <- sd(l6_form_n1_tg)
#     sdr_n1_tg[index_n1_tg] <- paste(sdr_n1_tg[index_n1_tg],sep = "")
#     l6_form_n1_tg <- as.character(l6_form_n1_tg)
#     l6_form_n1_tg_flattened <- stri_paste(l6_form_n1_tg,collapse = '')
#     l6_form_n1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_n1_tg_flattened),"")[[1]])
#     final_n1_tg[index_n1_tg,index_n1_tg_cols] <- l6_form_n1_tgsplitted[index_n1_tg_cols]
#   }
# }
#
# final_n1_tg[is.na(final_n1_tg)] <- ""
# n1_goaltotalmatrix <- cbind(n1_teams,final_n1_tg,suml6_n1_tg,avgr_n1_tg,sdr_n1_tg)
# ##############################################################################################
# #p1
# final_p1_tg <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_tg <- c()
# avgr_p1_tg <- c()
# sdr_p1_tg <- c()
# l6_form_p1_tgsplitted <- c()
# form_p1_tg <- c()
# for(index_p1_tg in 1:length(p1_teams))
# {
#   for(index_p1_tg_cols in 1:p1_totalrounds)
#   {
#     index_p1_tg  <- row.names(p1_totalgoals_h) == p1_teams[index_p1_tg]
#     form_p1_tg <- p1_totalgoals_h[index_p1_tg ]
#     deleted_form_p1_tg <- form_p1_tg[!form_p1_tg[] == ""]
#     l6_form_p1_tg <- tail(deleted_form_p1_tg,p1_last_n_games)
#     l6_form_p1_tg <- as.numeric(l6_form_p1_tg)
#     suml6_p1_tg[index_p1_tg] <- sum(l6_form_p1_tg)
#     suml6_p1_tg[index_p1_tg] <- paste(suml6_p1_tg[index_p1_tg],sep = "")
#     avgr_p1_tg[index_p1_tg] <- mean(l6_form_p1_tg)
#     avgr_p1_tg[index_p1_tg] <- paste(avgr_p1_tg[index_p1_tg],sep = "")
#     sdr_p1_tg[index_p1_tg] <- sd(l6_form_p1_tg)
#     sdr_p1_tg[index_p1_tg] <- paste(sdr_p1_tg[index_p1_tg],sep = "")
#     l6_form_p1_tg <- as.character(l6_form_p1_tg)
#     l6_form_p1_tg_flattened <- stri_paste(l6_form_p1_tg,collapse = '')
#     l6_form_p1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_p1_tg_flattened),"")[[1]])
#     final_p1_tg[index_p1_tg,index_p1_tg_cols] <- l6_form_p1_tgsplitted[index_p1_tg_cols]
#   }
# }
#
# final_p1_tg[is.na(final_p1_tg)] <- ""
# p1_goaltotalmatrix <- cbind(p1_teams,final_p1_tg,suml6_p1_tg,avgr_p1_tg,sdr_p1_tg)
# ##############################################################################################
# #sp1
# final_sp1_tg <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_tg <- c()
# avgr_sp1_tg <- c()
# sdr_sp1_tg <- c()
# l6_form_sp1_tgsplitted <- c()
# form_sp1_tg <- c()
# for(index_sp1_tg in 1:length(sp1_teams))
# {
#   for(index_sp1_tg_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_tg  <- row.names(sp1_totalgoals_h) == sp1_teams[index_sp1_tg]
#     form_sp1_tg <- sp1_totalgoals_h[index_sp1_tg ]
#     deleted_form_sp1_tg <- form_sp1_tg[!form_sp1_tg[] == ""]
#     l6_form_sp1_tg <- tail(deleted_form_sp1_tg,sp1_last_n_games)
#     l6_form_sp1_tg <- as.numeric(l6_form_sp1_tg)
#     suml6_sp1_tg[index_sp1_tg] <- sum(l6_form_sp1_tg)
#     suml6_sp1_tg[index_sp1_tg] <- paste(suml6_sp1_tg[index_sp1_tg],sep = "")
#     avgr_sp1_tg[index_sp1_tg] <- mean(l6_form_sp1_tg)
#     avgr_sp1_tg[index_sp1_tg] <- paste(avgr_sp1_tg[index_sp1_tg],sep = "")
#     sdr_sp1_tg[index_sp1_tg] <- sd(l6_form_sp1_tg)
#     sdr_sp1_tg[index_sp1_tg] <- paste(sdr_sp1_tg[index_sp1_tg],sep = "")
#     l6_form_sp1_tg <- as.character(l6_form_sp1_tg)
#     l6_form_sp1_tg_flattened <- stri_paste(l6_form_sp1_tg,collapse = '')
#     l6_form_sp1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_tg_flattened),"")[[1]])
#     final_sp1_tg[index_sp1_tg,index_sp1_tg_cols] <- l6_form_sp1_tgsplitted[index_sp1_tg_cols]
#   }
# }
#
# final_sp1_tg[is.na(final_sp1_tg)] <- ""
# sp1_goaltotalmatrix <- cbind(sp1_teams,final_sp1_tg,suml6_sp1_tg,avgr_sp1_tg,sdr_sp1_tg)
# ##############################################################################################
# #sp2
# final_sp2_tg <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_tg <- c()
# avgr_sp2_tg <- c()
# sdr_sp2_tg <- c()
# l6_form_sp2_tgsplitted <- c()
# form_sp2_tg <- c()
# for(index_sp2_tg in 1:length(sp2_teams))
# {
#   for(index_sp2_tg_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_tg  <- row.names(sp2_totalgoals_h) == sp2_teams[index_sp2_tg]
#     form_sp2_tg <- sp2_totalgoals_h[index_sp2_tg ]
#     deleted_form_sp2_tg <- form_sp2_tg[!form_sp2_tg[] == ""]
#     l6_form_sp2_tg <- tail(deleted_form_sp2_tg,sp2_last_n_games)
#     l6_form_sp2_tg <- as.numeric(l6_form_sp2_tg)
#     suml6_sp2_tg[index_sp2_tg] <- sum(l6_form_sp2_tg)
#     suml6_sp2_tg[index_sp2_tg] <- paste(suml6_sp2_tg[index_sp2_tg],sep = "")
#     avgr_sp2_tg[index_sp2_tg] <- mean(l6_form_sp2_tg)
#     avgr_sp2_tg[index_sp2_tg] <- paste(avgr_sp2_tg[index_sp2_tg],sep = "")
#     sdr_sp2_tg[index_sp2_tg] <- sd(l6_form_sp2_tg)
#     sdr_sp2_tg[index_sp2_tg] <- paste(sdr_sp2_tg[index_sp2_tg],sep = "")
#     l6_form_sp2_tg <- as.character(l6_form_sp2_tg)
#     l6_form_sp2_tg_flattened <- stri_paste(l6_form_sp2_tg,collapse = '')
#     l6_form_sp2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sp2_tg_flattened),"")[[1]])
#     final_sp2_tg[index_sp2_tg,index_sp2_tg_cols] <- l6_form_sp2_tgsplitted[index_sp2_tg_cols]
#   }
# }
#
# final_sp2_tg[is.na(final_sp2_tg)] <- ""
# sp2_goaltotalmatrix <- cbind(sp2_teams,final_sp2_tg,suml6_sp2_tg,avgr_sp2_tg,sdr_sp2_tg)
# ##############################################################################################
# #sc0
# final_sc0_tg <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_tg <- c()
# avgr_sc0_tg <- c()
# sdr_sc0_tg <- c()
# l6_form_sc0_tgsplitted <- c()
# form_sc0_tg <- c()
# for(index_sc0_tg in 1:length(sc0_teams))
# {
#   for(index_sc0_tg_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_tg  <- row.names(sc0_totalgoals_h) == sc0_teams[index_sc0_tg]
#     form_sc0_tg <- sc0_totalgoals_h[index_sc0_tg ]
#     deleted_form_sc0_tg <- form_sc0_tg[!form_sc0_tg[] == ""]
#     l6_form_sc0_tg <- tail(deleted_form_sc0_tg,sc0_last_n_games)
#     l6_form_sc0_tg <- as.numeric(l6_form_sc0_tg)
#     suml6_sc0_tg[index_sc0_tg] <- sum(l6_form_sc0_tg)
#     suml6_sc0_tg[index_sc0_tg] <- paste(suml6_sc0_tg[index_sc0_tg],sep = "")
#     avgr_sc0_tg[index_sc0_tg] <- mean(l6_form_sc0_tg)
#     avgr_sc0_tg[index_sc0_tg] <- paste(avgr_sc0_tg[index_sc0_tg],sep = "")
#     sdr_sc0_tg[index_sc0_tg] <- sd(l6_form_sc0_tg)
#     sdr_sc0_tg[index_sc0_tg] <- paste(sdr_sc0_tg[index_sc0_tg],sep = "")
#     l6_form_sc0_tg <- as.character(l6_form_sc0_tg)
#     l6_form_sc0_tg_flattened <- stri_paste(l6_form_sc0_tg,collapse = '')
#     l6_form_sc0_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_tg_flattened),"")[[1]])
#     final_sc0_tg[index_sc0_tg,index_sc0_tg_cols] <- l6_form_sc0_tgsplitted[index_sc0_tg_cols]
#   }
# }
#
# final_sc0_tg[is.na(final_sc0_tg)] <- ""
# sc0_goaltotalmatrix <- cbind(sc0_teams,final_sc0_tg,suml6_sc0_tg,avgr_sc0_tg,sdr_sc0_tg)
# ##############################################################################################
# #sc1
# final_sc1_tg <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_tg <- c()
# avgr_sc1_tg <- c()
# sdr_sc1_tg <- c()
# l6_form_sc1_tgsplitted <- c()
# form_sc1_tg <- c()
# for(index_sc1_tg in 1:length(sc1_teams))
# {
#   for(index_sc1_tg_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_tg  <- row.names(sc1_totalgoals_h) == sc1_teams[index_sc1_tg]
#     form_sc1_tg <- sc1_totalgoals_h[index_sc1_tg ]
#     deleted_form_sc1_tg <- form_sc1_tg[!form_sc1_tg[] == ""]
#     l6_form_sc1_tg <- tail(deleted_form_sc1_tg,sc1_last_n_games)
#     l6_form_sc1_tg <- as.numeric(l6_form_sc1_tg)
#     suml6_sc1_tg[index_sc1_tg] <- sum(l6_form_sc1_tg)
#     suml6_sc1_tg[index_sc1_tg] <- paste(suml6_sc1_tg[index_sc1_tg],sep = "")
#     avgr_sc1_tg[index_sc1_tg] <- mean(l6_form_sc1_tg)
#     avgr_sc1_tg[index_sc1_tg] <- paste(avgr_sc1_tg[index_sc1_tg],sep = "")
#     sdr_sc1_tg[index_sc1_tg] <- sd(l6_form_sc1_tg)
#     sdr_sc1_tg[index_sc1_tg] <- paste(sdr_sc1_tg[index_sc1_tg],sep = "")
#     l6_form_sc1_tg <- as.character(l6_form_sc1_tg)
#     l6_form_sc1_tg_flattened <- stri_paste(l6_form_sc1_tg,collapse = '')
#     l6_form_sc1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sc1_tg_flattened),"")[[1]])
#     final_sc1_tg[index_sc1_tg,index_sc1_tg_cols] <- l6_form_sc1_tgsplitted[index_sc1_tg_cols]
#   }
# }
#
# final_sc1_tg[is.na(final_sc1_tg)] <- ""
# sc1_goaltotalmatrix <- cbind(sc1_teams,final_sc1_tg,suml6_sc1_tg,avgr_sc1_tg,sdr_sc1_tg)
# ##############################################################################################
# #sc2
# final_sc2_tg <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_tg <- c()
# avgr_sc2_tg <- c()
# sdr_sc2_tg <- c()
# l6_form_sc2_tgsplitted <- c()
# form_sc2_tg <- c()
# for(index_sc2_tg in 1:length(sc2_teams))
# {
#   for(index_sc2_tg_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_tg  <- row.names(sc2_totalgoals_h) == sc2_teams[index_sc2_tg]
#     form_sc2_tg <- sc2_totalgoals_h[index_sc2_tg ]
#     deleted_form_sc2_tg <- form_sc2_tg[!form_sc2_tg[] == ""]
#     l6_form_sc2_tg <- tail(deleted_form_sc2_tg,sc2_last_n_games)
#     l6_form_sc2_tg <- as.numeric(l6_form_sc2_tg)
#     suml6_sc2_tg[index_sc2_tg] <- sum(l6_form_sc2_tg)
#     suml6_sc2_tg[index_sc2_tg] <- paste(suml6_sc2_tg[index_sc2_tg],sep = "")
#     avgr_sc2_tg[index_sc2_tg] <- mean(l6_form_sc2_tg)
#     avgr_sc2_tg[index_sc2_tg] <- paste(avgr_sc2_tg[index_sc2_tg],sep = "")
#     sdr_sc2_tg[index_sc2_tg] <- sd(l6_form_sc2_tg)
#     sdr_sc2_tg[index_sc2_tg] <- paste(sdr_sc2_tg[index_sc2_tg],sep = "")
#     l6_form_sc2_tg <- as.character(l6_form_sc2_tg)
#     l6_form_sc2_tg_flattened <- stri_paste(l6_form_sc2_tg,collapse = '')
#     l6_form_sc2_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sc2_tg_flattened),"")[[1]])
#     final_sc2_tg[index_sc2_tg,index_sc2_tg_cols] <- l6_form_sc2_tgsplitted[index_sc2_tg_cols]
#   }
# }
#
# final_sc2_tg[is.na(final_sc2_tg)] <- ""
# sc2_goaltotalmatrix <- cbind(sc2_teams,final_sc2_tg,suml6_sc2_tg,avgr_sc2_tg,sdr_sc2_tg)
# ##############################################################################################
# #sc3
# final_sc3_tg <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_tg <- c()
# avgr_sc3_tg <- c()
# sdr_sc3_tg <- c()
# l6_form_sc3_tgsplitted <- c()
# form_sc3_tg <- c()
# for(index_sc3_tg in 1:length(sc3_teams))
# {
#   for(index_sc3_tg_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_tg  <- row.names(sc3_totalgoals_h) == sc3_teams[index_sc3_tg]
#     form_sc3_tg <- sc3_totalgoals_h[index_sc3_tg ]
#     deleted_form_sc3_tg <- form_sc3_tg[!form_sc3_tg[] == ""]
#     l6_form_sc3_tg <- tail(deleted_form_sc3_tg,sc3_last_n_games)
#     l6_form_sc3_tg <- as.numeric(l6_form_sc3_tg)
#     suml6_sc3_tg[index_sc3_tg] <- sum(l6_form_sc3_tg)
#     suml6_sc3_tg[index_sc3_tg] <- paste(suml6_sc3_tg[index_sc3_tg],sep = "")
#     avgr_sc3_tg[index_sc3_tg] <- mean(l6_form_sc3_tg)
#     avgr_sc3_tg[index_sc3_tg] <- paste(avgr_sc3_tg[index_sc3_tg],sep = "")
#     sdr_sc3_tg[index_sc3_tg] <- sd(l6_form_sc3_tg)
#     sdr_sc3_tg[index_sc3_tg] <- paste(sdr_sc3_tg[index_sc3_tg],sep = "")
#     l6_form_sc3_tg <- as.character(l6_form_sc3_tg)
#     l6_form_sc3_tg_flattened <- stri_paste(l6_form_sc3_tg,collapse = '')
#     l6_form_sc3_tgsplitted <- as.numeric(strsplit(as.character(l6_form_sc3_tg_flattened),"")[[1]])
#     final_sc3_tg[index_sc3_tg,index_sc3_tg_cols] <- l6_form_sc3_tgsplitted[index_sc3_tg_cols]
#   }
# }
#
# final_sc3_tg[is.na(final_sc3_tg)] <- ""
# sc3_goaltotalmatrix <- cbind(sc3_teams,final_sc3_tg,suml6_sc3_tg,avgr_sc3_tg,sdr_sc3_tg)
# ##############################################################################################
# #t1
# final_t1_tg <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_tg <- c()
# avgr_t1_tg <- c()
# sdr_t1_tg <- c()
# l6_form_t1_tgsplitted <- c()
# form_t1_tg <- c()
# for(index_t1_tg in 1:length(t1_teams))
# {
#   for(index_t1_tg_cols in 1:t1_totalrounds)
#   {
#     index_t1_tg  <- row.names(t1_totalgoals_h) == t1_teams[index_t1_tg]
#     form_t1_tg <- t1_totalgoals_h[index_t1_tg ]
#     deleted_form_t1_tg <- form_t1_tg[!form_t1_tg[] == ""]
#     l6_form_t1_tg <- tail(deleted_form_t1_tg,t1_last_n_games)
#     l6_form_t1_tg <- as.numeric(l6_form_t1_tg)
#     suml6_t1_tg[index_t1_tg] <- sum(l6_form_t1_tg)
#     suml6_t1_tg[index_t1_tg] <- paste(suml6_t1_tg[index_t1_tg],sep = "")
#     avgr_t1_tg[index_t1_tg] <- mean(l6_form_t1_tg)
#     avgr_t1_tg[index_t1_tg] <- paste(avgr_t1_tg[index_t1_tg],sep = "")
#     sdr_t1_tg[index_t1_tg] <- sd(l6_form_t1_tg)
#     sdr_t1_tg[index_t1_tg] <- paste(sdr_t1_tg[index_t1_tg],sep = "")
#     l6_form_t1_tg <- as.character(l6_form_t1_tg)
#     l6_form_t1_tg_flattened <- stri_paste(l6_form_t1_tg,collapse = '')
#     l6_form_t1_tgsplitted <- as.numeric(strsplit(as.character(l6_form_t1_tg_flattened),"")[[1]])
#     final_t1_tg[index_t1_tg,index_t1_tg_cols] <- l6_form_t1_tgsplitted[index_t1_tg_cols]
#   }
# }
#
# final_t1_tg[is.na(final_t1_tg)] <- ""
# t1_goaltotalmatrix <- cbind(t1_teams,final_t1_tg,suml6_t1_tg,avgr_t1_tg,sdr_t1_tg)
##############################################################################################







































