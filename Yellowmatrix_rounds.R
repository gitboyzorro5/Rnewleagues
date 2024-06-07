library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_ycards <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_ycards <- c()
avg_mls_ycards <- c()
sd_mls_one_ycards <- c()
sum_mls_threemore_ycards <- c()
sum_mls_twoless_ycards <- c()
l6_form_mls_ycardssplitted <- c()
form_mls_ycards <- c()
for(index_mls_ycards in 1:length(mls_teams))
{
  for(index_mls_ycards_cols in 1:mls_totalrounds)
  {
    index_mls_ycards  <- row.names(mls_yellowscored_h) == mls_teams[index_mls_ycards]
    form_mls_ycards <- mls_yellowscored_h[index_mls_ycards]
    deleted_form_mls_ycards <- form_mls_ycards[!form_mls_ycards[] == ""]
    l6_form_mls_ycards <- deleted_form_mls_ycards #tail(deleted_form_mls_ycards,mls_last_n_games)
    l6_form_mls_ycards <- as.numeric(l6_form_mls_ycards)
    suml6_mls_ycards[index_mls_ycards] <- sum(l6_form_mls_ycards)
    suml6_mls_ycards[index_mls_ycards] <- paste(suml6_mls_ycards[index_mls_ycards],sep = "")
    avg_mls_ycards[index_mls_ycards] <- mean(l6_form_mls_ycards)
    avg_mls_ycards[index_mls_ycards] <- paste(avg_mls_ycards[index_mls_ycards],sep = "")
    sd_mls_one_ycards[index_mls_ycards] <- sd(l6_form_mls_ycards)
    sd_mls_one_ycards[index_mls_ycards] <- paste(sd_mls_one_ycards[index_mls_ycards],sep = "")
    sum_mls_threemore_ycards[index_mls_ycards] <- length(which(l6_form_mls_ycards >= 3))
    sum_mls_threemore_ycards[index_mls_ycards] <- paste(sum_mls_threemore_ycards[index_mls_ycards],sep = "")
    sum_mls_twoless_ycards[index_mls_ycards] <- length(which(l6_form_mls_ycards <= 2))
    sum_mls_twoless_ycards[index_mls_ycards] <- paste(sum_mls_twoless_ycards[index_mls_ycards],sep = "")
    #l6_form_mls_ycards <- as.character(l6_form_mls_ycards)
    #l6_form_mls_ycards_flattened <- stri_paste(l6_form_mls_ycards,collapse = '')
    #l6_form_mls_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_mls_ycards_flattened),"")[[1]])
    final_mls_ycards[index_mls_ycards,index_mls_ycards_cols] <- l6_form_mls_ycards[index_mls_ycards_cols]
  }
}

final_mls_ycards[is.na(final_mls_ycards)] <- ""
mls_yellowscoredmatrix <- cbind(mls_teams,final_mls_ycards,suml6_mls_ycards,avg_mls_ycards,sd_mls_one_ycards,sum_mls_threemore_ycards,sum_mls_twoless_ycards)
########################################################################################################################################################
#arg
final_arg_ycards <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_ycards <- c()
avg_arg_ycards <- c()
sd_arg_one_ycards <- c()
sum_arg_threemore_ycards <- c()
sum_arg_twoless_ycards <- c()
l6_form_arg_ycardssplitted <- c()
form_arg_ycards <- c()
for(index_arg_ycards in 1:length(arg_teams))
{
  for(index_arg_ycards_cols in 1:arg_totalrounds)
  {
    index_arg_ycards  <- row.names(arg_yellowscored_h) == arg_teams[index_arg_ycards]
    form_arg_ycards <- arg_yellowscored_h[index_arg_ycards]
    deleted_form_arg_ycards <- form_arg_ycards[!form_arg_ycards[] == ""]
    l6_form_arg_ycards <- deleted_form_arg_ycards #tail(deleted_form_arg_ycards,arg_last_n_games)
    l6_form_arg_ycards <- as.numeric(l6_form_arg_ycards)
    suml6_arg_ycards[index_arg_ycards] <- sum(l6_form_arg_ycards)
    suml6_arg_ycards[index_arg_ycards] <- paste(suml6_arg_ycards[index_arg_ycards],sep = "")
    avg_arg_ycards[index_arg_ycards] <- mean(l6_form_arg_ycards)
    avg_arg_ycards[index_arg_ycards] <- paste(avg_arg_ycards[index_arg_ycards],sep = "")
    sd_arg_one_ycards[index_arg_ycards] <- sd(l6_form_arg_ycards)
    sd_arg_one_ycards[index_arg_ycards] <- paste(sd_arg_one_ycards[index_arg_ycards],sep = "")
    sum_arg_threemore_ycards[index_arg_ycards] <- length(which(l6_form_arg_ycards >= 3))
    sum_arg_threemore_ycards[index_arg_ycards] <- paste(sum_arg_threemore_ycards[index_arg_ycards],sep = "")
    sum_arg_twoless_ycards[index_arg_ycards] <- length(which(l6_form_arg_ycards <= 2))
    sum_arg_twoless_ycards[index_arg_ycards] <- paste(sum_arg_twoless_ycards[index_arg_ycards],sep = "")
    #l6_form_arg_ycards <- as.character(l6_form_arg_ycards)
    #l6_form_arg_ycards_flattened <- stri_paste(l6_form_arg_ycards,collapse = '')
    #l6_form_arg_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_arg_ycards_flattened),"")[[1]])
    final_arg_ycards[index_arg_ycards,index_arg_ycards_cols] <- l6_form_arg_ycards[index_arg_ycards_cols]
  }
}

final_arg_ycards[is.na(final_arg_ycards)] <- ""
arg_yellowscoredmatrix <- cbind(arg_teams,final_arg_ycards,suml6_arg_ycards,avg_arg_ycards,sd_arg_one_ycards,sum_arg_threemore_ycards,sum_arg_twoless_ycards)
########################################################################################################################################################
#d2
# final_d2_ycards <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_ycards <- c()
# avg_d2_ycards <- c()
# sd_d2_one_ycards <- c()
# sum_d2_threemore_ycards <- c()
# sum_d2_twoless_ycards <- c()
# l6_form_d2_ycardssplitted <- c()
# form_d2_ycards <- c()
# for(index_d2_ycards in 1:length(d2_teams))
# {
#   for(index_d2_ycards_cols in 1:d2_totalrounds)
#   {
#     index_d2_ycards  <- row.names(d2_yellowscored_h) == d2_teams[index_d2_ycards]
#     form_d2_ycards <- d2_yellowscored_h[index_d2_ycards]
#     deleted_form_d2_ycards <- form_d2_ycards[!form_d2_ycards[] == ""]
#     l6_form_d2_ycards <- deleted_form_d2_ycards #tail(deleted_form_d2_ycards,d2_last_n_games)
#     l6_form_d2_ycards <- as.numeric(l6_form_d2_ycards)
#     suml6_d2_ycards[index_d2_ycards] <- sum(l6_form_d2_ycards)
#     suml6_d2_ycards[index_d2_ycards] <- paste(suml6_d2_ycards[index_d2_ycards],sep = "")
#     avg_d2_ycards[index_d2_ycards] <- mean(l6_form_d2_ycards)
#     avg_d2_ycards[index_d2_ycards] <- paste(avg_d2_ycards[index_d2_ycards],sep = "")
#     sd_d2_one_ycards[index_d2_ycards] <- sd(l6_form_d2_ycards)
#     sd_d2_one_ycards[index_d2_ycards] <- paste(sd_d2_one_ycards[index_d2_ycards],sep = "")
#     sum_d2_threemore_ycards[index_d2_ycards] <- length(which(l6_form_d2_ycards >= 3))
#     sum_d2_threemore_ycards[index_d2_ycards] <- paste(sum_d2_threemore_ycards[index_d2_ycards],sep = "")
#     sum_d2_twoless_ycards[index_d2_ycards] <- length(which(l6_form_d2_ycards <= 2))
#     sum_d2_twoless_ycards[index_d2_ycards] <- paste(sum_d2_twoless_ycards[index_d2_ycards],sep = "")
#     #l6_form_d2_ycards <- as.character(l6_form_d2_ycards)
#     #l6_form_d2_ycards_flattened <- stri_paste(l6_form_d2_ycards,collapse = '')
#     #l6_form_d2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_d2_ycards_flattened),"")[[1]])
#     final_d2_ycards[index_d2_ycards,index_d2_ycards_cols] <- l6_form_d2_ycards[index_d2_ycards_cols]
#   }
# }
#
# final_d2_ycards[is.na(final_d2_ycards)] <- ""
# d2_yellowscoredmatrix <- cbind(d2_teams,final_d2_ycards,suml6_d2_ycards,avg_d2_ycards,sd_d2_one_ycards,sum_d2_threemore_ycards,sum_d2_twoless_ycards)
# ########################################################################################################################################################
# #e0
# final_e0_ycards <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_ycards <- c()
# avg_e0_ycards <- c()
# sd_e0_one_ycards <- c()
# sum_e0_threemore_ycards <- c()
# sum_e0_twoless_ycards <- c()
# l6_form_e0_ycardssplitted <- c()
# form_e0_ycards <- c()
# for(index_e0_ycards in 1:length(e0_teams))
# {
#   for(index_e0_ycards_cols in 1:e0_totalrounds)
#   {
#     index_e0_ycards  <- row.names(e0_yellowscored_h) == e0_teams[index_e0_ycards]
#     form_e0_ycards <- e0_yellowscored_h[index_e0_ycards]
#     deleted_form_e0_ycards <- form_e0_ycards[!form_e0_ycards[] == ""]
#     l6_form_e0_ycards <- deleted_form_e0_ycards #tail(deleted_form_e0_ycards,e0_last_n_games)
#     l6_form_e0_ycards <- as.numeric(l6_form_e0_ycards)
#     suml6_e0_ycards[index_e0_ycards] <- sum(l6_form_e0_ycards)
#     suml6_e0_ycards[index_e0_ycards] <- paste(suml6_e0_ycards[index_e0_ycards],sep = "")
#     avg_e0_ycards[index_e0_ycards] <- mean(l6_form_e0_ycards)
#     avg_e0_ycards[index_e0_ycards] <- paste(avg_e0_ycards[index_e0_ycards],sep = "")
#     sd_e0_one_ycards[index_e0_ycards] <- sd(l6_form_e0_ycards)
#     sd_e0_one_ycards[index_e0_ycards] <- paste(sd_e0_one_ycards[index_e0_ycards],sep = "")
#     sum_e0_threemore_ycards[index_e0_ycards] <- length(which(l6_form_e0_ycards >= 3))
#     sum_e0_threemore_ycards[index_e0_ycards] <- paste(sum_e0_threemore_ycards[index_e0_ycards],sep = "")
#     sum_e0_twoless_ycards[index_e0_ycards] <- length(which(l6_form_e0_ycards <= 2))
#     sum_e0_twoless_ycards[index_e0_ycards] <- paste(sum_e0_twoless_ycards[index_e0_ycards],sep = "")
#     #l6_form_e0_ycards <- as.character(l6_form_e0_ycards)
#     #l6_form_e0_ycards_flattened <- stri_paste(l6_form_e0_ycards,collapse = '')
#     #l6_form_e0_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_e0_ycards_flattened),"")[[1]])
#     final_e0_ycards[index_e0_ycards,index_e0_ycards_cols] <- l6_form_e0_ycards[index_e0_ycards_cols]
#   }
# }
#
# final_e0_ycards[is.na(final_e0_ycards)] <- ""
# e0_yellowscoredmatrix <- cbind(e0_teams,final_e0_ycards,suml6_e0_ycards,avg_e0_ycards,sd_e0_one_ycards,sum_e0_threemore_ycards,sum_e0_twoless_ycards)
# ########################################################################################################################################################
# #e1
# final_e1_ycards <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_ycards <- c()
# avg_e1_ycards <- c()
# sd_e1_one_ycards <- c()
# sum_e1_threemore_ycards <- c()
# sum_e1_twoless_ycards <- c()
# l6_form_e1_ycardssplitted <- c()
# form_e1_ycards <- c()
# for(index_e1_ycards in 1:length(e1_teams))
# {
#   for(index_e1_ycards_cols in 1:e1_totalrounds)
#   {
#     index_e1_ycards  <- row.names(e1_yellowscored_h) == e1_teams[index_e1_ycards]
#     form_e1_ycards <- e1_yellowscored_h[index_e1_ycards]
#     deleted_form_e1_ycards <- form_e1_ycards[!form_e1_ycards[] == ""]
#     l6_form_e1_ycards <- deleted_form_e1_ycards #tail(deleted_form_e1_ycards,e1_last_n_games)
#     l6_form_e1_ycards <- as.numeric(l6_form_e1_ycards)
#     suml6_e1_ycards[index_e1_ycards] <- sum(l6_form_e1_ycards)
#     suml6_e1_ycards[index_e1_ycards] <- paste(suml6_e1_ycards[index_e1_ycards],sep = "")
#     avg_e1_ycards[index_e1_ycards] <- mean(l6_form_e1_ycards)
#     avg_e1_ycards[index_e1_ycards] <- paste(avg_e1_ycards[index_e1_ycards],sep = "")
#     sd_e1_one_ycards[index_e1_ycards] <- sd(l6_form_e1_ycards)
#     sd_e1_one_ycards[index_e1_ycards] <- paste(sd_e1_one_ycards[index_e1_ycards],sep = "")
#     sum_e1_threemore_ycards[index_e1_ycards] <- length(which(l6_form_e1_ycards >= 3))
#     sum_e1_threemore_ycards[index_e1_ycards] <- paste(sum_e1_threemore_ycards[index_e1_ycards],sep = "")
#     sum_e1_twoless_ycards[index_e1_ycards] <- length(which(l6_form_e1_ycards <= 2))
#     sum_e1_twoless_ycards[index_e1_ycards] <- paste(sum_e1_twoless_ycards[index_e1_ycards],sep = "")
#     #l6_form_e1_ycards <- as.character(l6_form_e1_ycards)
#     #l6_form_e1_ycards_flattened <- stri_paste(l6_form_e1_ycards,collapse = '')
#     #l6_form_e1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_e1_ycards_flattened),"")[[1]])
#     final_e1_ycards[index_e1_ycards,index_e1_ycards_cols] <- l6_form_e1_ycards[index_e1_ycards_cols]
#   }
# }
#
# final_e1_ycards[is.na(final_e1_ycards)] <- ""
# e1_yellowscoredmatrix <- cbind(e1_teams,final_e1_ycards,suml6_e1_ycards,avg_e1_ycards,sd_e1_one_ycards,sum_e1_threemore_ycards,sum_e1_twoless_ycards)
# ########################################################################################################################################################
# #e2
# final_e2_ycards <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_ycards <- c()
# avg_e2_ycards <- c()
# sd_e2_one_ycards <- c()
# sum_e2_threemore_ycards <- c()
# sum_e2_twoless_ycards <- c()
# l6_form_e2_ycardssplitted <- c()
# form_e2_ycards <- c()
# for(index_e2_ycards in 1:length(e2_teams))
# {
#   for(index_e2_ycards_cols in 1:e2_totalrounds)
#   {
#     index_e2_ycards  <- row.names(e2_yellowscored_h) == e2_teams[index_e2_ycards]
#     form_e2_ycards <- e2_yellowscored_h[index_e2_ycards]
#     deleted_form_e2_ycards <- form_e2_ycards[!form_e2_ycards[] == ""]
#     l6_form_e2_ycards <- deleted_form_e2_ycards #tail(deleted_form_e2_ycards,e2_last_n_games)
#     l6_form_e2_ycards <- as.numeric(l6_form_e2_ycards)
#     suml6_e2_ycards[index_e2_ycards] <- sum(l6_form_e2_ycards)
#     suml6_e2_ycards[index_e2_ycards] <- paste(suml6_e2_ycards[index_e2_ycards],sep = "")
#     avg_e2_ycards[index_e2_ycards] <- mean(l6_form_e2_ycards)
#     avg_e2_ycards[index_e2_ycards] <- paste(avg_e2_ycards[index_e2_ycards],sep = "")
#     sd_e2_one_ycards[index_e2_ycards] <- sd(l6_form_e2_ycards)
#     sd_e2_one_ycards[index_e2_ycards] <- paste(sd_e2_one_ycards[index_e2_ycards],sep = "")
#     sum_e2_threemore_ycards[index_e2_ycards] <- length(which(l6_form_e2_ycards >= 3))
#     sum_e2_threemore_ycards[index_e2_ycards] <- paste(sum_e2_threemore_ycards[index_e2_ycards],sep = "")
#     sum_e2_twoless_ycards[index_e2_ycards] <- length(which(l6_form_e2_ycards <= 2))
#     sum_e2_twoless_ycards[index_e2_ycards] <- paste(sum_e2_twoless_ycards[index_e2_ycards],sep = "")
#     #l6_form_e2_ycards <- as.character(l6_form_e2_ycards)
#     #l6_form_e2_ycards_flattened <- stri_paste(l6_form_e2_ycards,collapse = '')
#     #l6_form_e2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_e2_ycards_flattened),"")[[1]])
#     final_e2_ycards[index_e2_ycards,index_e2_ycards_cols] <- l6_form_e2_ycards[index_e2_ycards_cols]
#   }
# }
#
# final_e2_ycards[is.na(final_e2_ycards)] <- ""
# e2_yellowscoredmatrix <- cbind(e2_teams,final_e2_ycards,suml6_e2_ycards,avg_e2_ycards,sd_e2_one_ycards,sum_e2_threemore_ycards,sum_e2_twoless_ycards)
# ########################################################################################################################################################
# #e3
# final_e3_ycards <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_ycards <- c()
# avg_e3_ycards <- c()
# sd_e3_one_ycards <- c()
# sum_e3_threemore_ycards <- c()
# sum_e3_twoless_ycards <- c()
# l6_form_e3_ycardssplitted <- c()
# form_e3_ycards <- c()
# for(index_e3_ycards in 1:length(e3_teams))
# {
#   for(index_e3_ycards_cols in 1:e3_totalrounds)
#   {
#     index_e3_ycards  <- row.names(e3_yellowscored_h) == e3_teams[index_e3_ycards]
#     form_e3_ycards <- e3_yellowscored_h[index_e3_ycards]
#     deleted_form_e3_ycards <- form_e3_ycards[!form_e3_ycards[] == ""]
#     l6_form_e3_ycards <- deleted_form_e3_ycards #tail(deleted_form_e3_ycards,e3_last_n_games)
#     l6_form_e3_ycards <- as.numeric(l6_form_e3_ycards)
#     suml6_e3_ycards[index_e3_ycards] <- sum(l6_form_e3_ycards)
#     suml6_e3_ycards[index_e3_ycards] <- paste(suml6_e3_ycards[index_e3_ycards],sep = "")
#     avg_e3_ycards[index_e3_ycards] <- mean(l6_form_e3_ycards)
#     avg_e3_ycards[index_e3_ycards] <- paste(avg_e3_ycards[index_e3_ycards],sep = "")
#     sd_e3_one_ycards[index_e3_ycards] <- sd(l6_form_e3_ycards)
#     sd_e3_one_ycards[index_e3_ycards] <- paste(sd_e3_one_ycards[index_e3_ycards],sep = "")
#     sum_e3_threemore_ycards[index_e3_ycards] <- length(which(l6_form_e3_ycards >= 3))
#     sum_e3_threemore_ycards[index_e3_ycards] <- paste(sum_e3_threemore_ycards[index_e3_ycards],sep = "")
#     sum_e3_twoless_ycards[index_e3_ycards] <- length(which(l6_form_e3_ycards <= 2))
#     sum_e3_twoless_ycards[index_e3_ycards] <- paste(sum_e3_twoless_ycards[index_e3_ycards],sep = "")
#     #l6_form_e3_ycards <- as.character(l6_form_e3_ycards)
#     #l6_form_e3_ycards_flattened <- stri_paste(l6_form_e3_ycards,collapse = '')
#     #l6_form_e3_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_e3_ycards_flattened),"")[[1]])
#     final_e3_ycards[index_e3_ycards,index_e3_ycards_cols] <- l6_form_e3_ycards[index_e3_ycards_cols]
#   }
# }
#
# final_e3_ycards[is.na(final_e3_ycards)] <- ""
# e3_yellowscoredmatrix <- cbind(e3_teams,final_e3_ycards,suml6_e3_ycards,avg_e3_ycards,sd_e3_one_ycards,sum_e3_threemore_ycards,sum_e3_twoless_ycards)
# ########################################################################################################################################################
# #ec
# final_ec_ycards <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_ycards <- c()
# avg_ec_ycards <- c()
# sd_ec_one_ycards <- c()
# sum_ec_threemore_ycards <- c()
# sum_ec_twoless_ycards <- c()
# l6_form_ec_ycardssplitted <- c()
# form_ec_ycards <- c()
# for(index_ec_ycards in 1:length(ec_teams))
# {
#   for(index_ec_ycards_cols in 1:ec_totalrounds)
#   {
#     index_ec_ycards  <- row.names(ec_yellowscored_h) == ec_teams[index_ec_ycards]
#     form_ec_ycards <- ec_yellowscored_h[index_ec_ycards]
#     deleted_form_ec_ycards <- form_ec_ycards[!form_ec_ycards[] == ""]
#     l6_form_ec_ycards <- deleted_form_ec_ycards #tail(deleted_form_ec_ycards,ec_last_n_games)
#     l6_form_ec_ycards <- as.numeric(l6_form_ec_ycards)
#     suml6_ec_ycards[index_ec_ycards] <- sum(l6_form_ec_ycards)
#     suml6_ec_ycards[index_ec_ycards] <- paste(suml6_ec_ycards[index_ec_ycards],sep = "")
#     avg_ec_ycards[index_ec_ycards] <- mean(l6_form_ec_ycards)
#     avg_ec_ycards[index_ec_ycards] <- paste(avg_ec_ycards[index_ec_ycards],sep = "")
#     sd_ec_one_ycards[index_ec_ycards] <- sd(l6_form_ec_ycards)
#     sd_ec_one_ycards[index_ec_ycards] <- paste(sd_ec_one_ycards[index_ec_ycards],sep = "")
#     sum_ec_threemore_ycards[index_ec_ycards] <- length(which(l6_form_ec_ycards >= 3))
#     sum_ec_threemore_ycards[index_ec_ycards] <- paste(sum_ec_threemore_ycards[index_ec_ycards],sep = "")
#     sum_ec_twoless_ycards[index_ec_ycards] <- length(which(l6_form_ec_ycards <= 2))
#     sum_ec_twoless_ycards[index_ec_ycards] <- paste(sum_ec_twoless_ycards[index_ec_ycards],sep = "")
#     #l6_form_ec_ycards <- as.character(l6_form_ec_ycards)
#     #l6_form_ec_ycards_flattened <- stri_paste(l6_form_ec_ycards,collapse = '')
#     #l6_form_ec_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_ec_ycards_flattened),"")[[1]])
#     final_ec_ycards[index_ec_ycards,index_ec_ycards_cols] <- l6_form_ec_ycards[index_ec_ycards_cols]
#   }
# }
#
# final_ec_ycards[is.na(final_ec_ycards)] <- ""
# ec_yellowscoredmatrix <- cbind(ec_teams,final_ec_ycards,suml6_ec_ycards,avg_ec_ycards,sd_ec_one_ycards,sum_ec_threemore_ycards,sum_ec_twoless_ycards)
# ########################################################################################################################################################
# #f1
# final_f1_ycards <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_ycards <- c()
# avg_f1_ycards <- c()
# sd_f1_one_ycards <- c()
# sum_f1_threemore_ycards <- c()
# sum_f1_twoless_ycards <- c()
# l6_form_f1_ycardssplitted <- c()
# form_f1_ycards <- c()
# for(index_f1_ycards in 1:length(f1_teams))
# {
#   for(index_f1_ycards_cols in 1:f1_totalrounds)
#   {
#     index_f1_ycards  <- row.names(f1_yellowscored_h) == f1_teams[index_f1_ycards]
#     form_f1_ycards <- f1_yellowscored_h[index_f1_ycards]
#     deleted_form_f1_ycards <- form_f1_ycards[!form_f1_ycards[] == ""]
#     l6_form_f1_ycards <- deleted_form_f1_ycards #tail(deleted_form_f1_ycards,f1_last_n_games)
#     l6_form_f1_ycards <- as.numeric(l6_form_f1_ycards)
#     suml6_f1_ycards[index_f1_ycards] <- sum(l6_form_f1_ycards)
#     suml6_f1_ycards[index_f1_ycards] <- paste(suml6_f1_ycards[index_f1_ycards],sep = "")
#     avg_f1_ycards[index_f1_ycards] <- mean(l6_form_f1_ycards)
#     avg_f1_ycards[index_f1_ycards] <- paste(avg_f1_ycards[index_f1_ycards],sep = "")
#     sd_f1_one_ycards[index_f1_ycards] <- sd(l6_form_f1_ycards)
#     sd_f1_one_ycards[index_f1_ycards] <- paste(sd_f1_one_ycards[index_f1_ycards],sep = "")
#     sum_f1_threemore_ycards[index_f1_ycards] <- length(which(l6_form_f1_ycards >= 3))
#     sum_f1_threemore_ycards[index_f1_ycards] <- paste(sum_f1_threemore_ycards[index_f1_ycards],sep = "")
#     sum_f1_twoless_ycards[index_f1_ycards] <- length(which(l6_form_f1_ycards <= 2))
#     sum_f1_twoless_ycards[index_f1_ycards] <- paste(sum_f1_twoless_ycards[index_f1_ycards],sep = "")
#     #l6_form_f1_ycards <- as.character(l6_form_f1_ycards)
#     #l6_form_f1_ycards_flattened <- stri_paste(l6_form_f1_ycards,collapse = '')
#     #l6_form_f1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_f1_ycards_flattened),"")[[1]])
#     final_f1_ycards[index_f1_ycards,index_f1_ycards_cols] <- l6_form_f1_ycards[index_f1_ycards_cols]
#   }
# }
#
# final_f1_ycards[is.na(final_f1_ycards)] <- ""
# f1_yellowscoredmatrix <- cbind(f1_teams,final_f1_ycards,suml6_f1_ycards,avg_f1_ycards,sd_f1_one_ycards,sum_f1_threemore_ycards,sum_f1_twoless_ycards)
# ########################################################################################################################################################
# #f2
# final_f2_ycards <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_ycards <- c()
# avg_f2_ycards <- c()
# sd_f2_one_ycards <- c()
# sum_f2_threemore_ycards <- c()
# sum_f2_twoless_ycards <- c()
# l6_form_f2_ycardssplitted <- c()
# form_f2_ycards <- c()
# for(index_f2_ycards in 1:length(f2_teams))
# {
#   for(index_f2_ycards_cols in 1:f2_totalrounds)
#   {
#     index_f2_ycards  <- row.names(f2_yellowscored_h) == f2_teams[index_f2_ycards]
#     form_f2_ycards <- f2_yellowscored_h[index_f2_ycards]
#     deleted_form_f2_ycards <- form_f2_ycards[!form_f2_ycards[] == ""]
#     l6_form_f2_ycards <- deleted_form_f2_ycards #tail(deleted_form_f2_ycards,f2_last_n_games)
#     l6_form_f2_ycards <- as.numeric(l6_form_f2_ycards)
#     suml6_f2_ycards[index_f2_ycards] <- sum(l6_form_f2_ycards)
#     suml6_f2_ycards[index_f2_ycards] <- paste(suml6_f2_ycards[index_f2_ycards],sep = "")
#     avg_f2_ycards[index_f2_ycards] <- mean(l6_form_f2_ycards)
#     avg_f2_ycards[index_f2_ycards] <- paste(avg_f2_ycards[index_f2_ycards],sep = "")
#     sd_f2_one_ycards[index_f2_ycards] <- sd(l6_form_f2_ycards)
#     sd_f2_one_ycards[index_f2_ycards] <- paste(sd_f2_one_ycards[index_f2_ycards],sep = "")
#     sum_f2_threemore_ycards[index_f2_ycards] <- length(which(l6_form_f2_ycards >= 3))
#     sum_f2_threemore_ycards[index_f2_ycards] <- paste(sum_f2_threemore_ycards[index_f2_ycards],sep = "")
#     sum_f2_twoless_ycards[index_f2_ycards] <- length(which(l6_form_f2_ycards <= 2))
#     sum_f2_twoless_ycards[index_f2_ycards] <- paste(sum_f2_twoless_ycards[index_f2_ycards],sep = "")
#     #l6_form_f2_ycards <- as.character(l6_form_f2_ycards)
#     #l6_form_f2_ycards_flattened <- stri_paste(l6_form_f2_ycards,collapse = '')
#     #l6_form_f2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_f2_ycards_flattened),"")[[1]])
#     final_f2_ycards[index_f2_ycards,index_f2_ycards_cols] <- l6_form_f2_ycards[index_f2_ycards_cols]
#   }
# }
#
# final_f2_ycards[is.na(final_f2_ycards)] <- ""
# f2_yellowscoredmatrix <- cbind(f2_teams,final_f2_ycards,suml6_f2_ycards,avg_f2_ycards,sd_f2_one_ycards,sum_f2_threemore_ycards,sum_f2_twoless_ycards)
# ########################################################################################################################################################
# #g1
# final_g1_ycards <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_ycards <- c()
# avg_g1_ycards <- c()
# sd_g1_one_ycards <- c()
# sum_g1_threemore_ycards <- c()
# sum_g1_twoless_ycards <- c()
# l6_form_g1_ycardssplitted <- c()
# form_g1_ycards <- c()
# for(index_g1_ycards in 1:length(g1_teams))
# {
#   for(index_g1_ycards_cols in 1:g1_totalrounds)
#   {
#     index_g1_ycards  <- row.names(g1_yellowscored_h) == g1_teams[index_g1_ycards]
#     form_g1_ycards <- g1_yellowscored_h[index_g1_ycards]
#     deleted_form_g1_ycards <- form_g1_ycards[!form_g1_ycards[] == ""]
#     l6_form_g1_ycards <- deleted_form_g1_ycards #tail(deleted_form_g1_ycards,g1_last_n_games)
#     l6_form_g1_ycards <- as.numeric(l6_form_g1_ycards)
#     suml6_g1_ycards[index_g1_ycards] <- sum(l6_form_g1_ycards)
#     suml6_g1_ycards[index_g1_ycards] <- paste(suml6_g1_ycards[index_g1_ycards],sep = "")
#     avg_g1_ycards[index_g1_ycards] <- mean(l6_form_g1_ycards)
#     avg_g1_ycards[index_g1_ycards] <- paste(avg_g1_ycards[index_g1_ycards],sep = "")
#     sd_g1_one_ycards[index_g1_ycards] <- sd(l6_form_g1_ycards)
#     sd_g1_one_ycards[index_g1_ycards] <- paste(sd_g1_one_ycards[index_g1_ycards],sep = "")
#     sum_g1_threemore_ycards[index_g1_ycards] <- length(which(l6_form_g1_ycards >= 3))
#     sum_g1_threemore_ycards[index_g1_ycards] <- paste(sum_g1_threemore_ycards[index_g1_ycards],sep = "")
#     sum_g1_twoless_ycards[index_g1_ycards] <- length(which(l6_form_g1_ycards <= 2))
#     sum_g1_twoless_ycards[index_g1_ycards] <- paste(sum_g1_twoless_ycards[index_g1_ycards],sep = "")
#     #l6_form_g1_ycards <- as.character(l6_form_g1_ycards)
#     #l6_form_g1_ycards_flattened <- stri_paste(l6_form_g1_ycards,collapse = '')
#     #l6_form_g1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_g1_ycards_flattened),"")[[1]])
#     final_g1_ycards[index_g1_ycards,index_g1_ycards_cols] <- l6_form_g1_ycards[index_g1_ycards_cols]
#   }
# }
#
# final_g1_ycards[is.na(final_g1_ycards)] <- ""
# g1_yellowscoredmatrix <- cbind(g1_teams,final_g1_ycards,suml6_g1_ycards,avg_g1_ycards,sd_g1_one_ycards,sum_g1_threemore_ycards,sum_g1_twoless_ycards)
# ########################################################################################################################################################
# #i1
# final_i1_ycards <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_ycards <- c()
# avg_i1_ycards <- c()
# sd_i1_one_ycards <- c()
# sum_i1_threemore_ycards <- c()
# sum_i1_twoless_ycards <- c()
# l6_form_i1_ycardssplitted <- c()
# form_i1_ycards <- c()
# for(index_i1_ycards in 1:length(i1_teams))
# {
#   for(index_i1_ycards_cols in 1:i1_totalrounds)
#   {
#     index_i1_ycards  <- row.names(i1_yellowscored_h) == i1_teams[index_i1_ycards]
#     form_i1_ycards <- i1_yellowscored_h[index_i1_ycards]
#     deleted_form_i1_ycards <- form_i1_ycards[!form_i1_ycards[] == ""]
#     l6_form_i1_ycards <- deleted_form_i1_ycards #tail(deleted_form_i1_ycards,i1_last_n_games)
#     l6_form_i1_ycards <- as.numeric(l6_form_i1_ycards)
#     suml6_i1_ycards[index_i1_ycards] <- sum(l6_form_i1_ycards)
#     suml6_i1_ycards[index_i1_ycards] <- paste(suml6_i1_ycards[index_i1_ycards],sep = "")
#     avg_i1_ycards[index_i1_ycards] <- mean(l6_form_i1_ycards)
#     avg_i1_ycards[index_i1_ycards] <- paste(avg_i1_ycards[index_i1_ycards],sep = "")
#     sd_i1_one_ycards[index_i1_ycards] <- sd(l6_form_i1_ycards)
#     sd_i1_one_ycards[index_i1_ycards] <- paste(sd_i1_one_ycards[index_i1_ycards],sep = "")
#     sum_i1_threemore_ycards[index_i1_ycards] <- length(which(l6_form_i1_ycards >= 3))
#     sum_i1_threemore_ycards[index_i1_ycards] <- paste(sum_i1_threemore_ycards[index_i1_ycards],sep = "")
#     sum_i1_twoless_ycards[index_i1_ycards] <- length(which(l6_form_i1_ycards <= 2))
#     sum_i1_twoless_ycards[index_i1_ycards] <- paste(sum_i1_twoless_ycards[index_i1_ycards],sep = "")
#     #l6_form_i1_ycards <- as.character(l6_form_i1_ycards)
#     #l6_form_i1_ycards_flattened <- stri_paste(l6_form_i1_ycards,collapse = '')
#     #l6_form_i1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_i1_ycards_flattened),"")[[1]])
#     final_i1_ycards[index_i1_ycards,index_i1_ycards_cols] <- l6_form_i1_ycards[index_i1_ycards_cols]
#   }
# }
#
# final_i1_ycards[is.na(final_i1_ycards)] <- ""
# i1_yellowscoredmatrix <- cbind(i1_teams,final_i1_ycards,suml6_i1_ycards,avg_i1_ycards,sd_i1_one_ycards,sum_i1_threemore_ycards,sum_i1_twoless_ycards)
# ########################################################################################################################################################
# #i2
# final_i2_ycards <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_ycards <- c()
# avg_i2_ycards <- c()
# sd_i2_one_ycards <- c()
# sum_i2_threemore_ycards <- c()
# sum_i2_twoless_ycards <- c()
# l6_form_i2_ycardssplitted <- c()
# form_i2_ycards <- c()
# for(index_i2_ycards in 1:length(i2_teams))
# {
#   for(index_i2_ycards_cols in 1:i2_totalrounds)
#   {
#     index_i2_ycards  <- row.names(i2_yellowscored_h) == i2_teams[index_i2_ycards]
#     form_i2_ycards <- i2_yellowscored_h[index_i2_ycards]
#     deleted_form_i2_ycards <- form_i2_ycards[!form_i2_ycards[] == ""]
#     l6_form_i2_ycards <- deleted_form_i2_ycards #tail(deleted_form_i2_ycards,i2_last_n_games)
#     l6_form_i2_ycards <- as.numeric(l6_form_i2_ycards)
#     suml6_i2_ycards[index_i2_ycards] <- sum(l6_form_i2_ycards)
#     suml6_i2_ycards[index_i2_ycards] <- paste(suml6_i2_ycards[index_i2_ycards],sep = "")
#     avg_i2_ycards[index_i2_ycards] <- mean(l6_form_i2_ycards)
#     avg_i2_ycards[index_i2_ycards] <- paste(avg_i2_ycards[index_i2_ycards],sep = "")
#     sd_i2_one_ycards[index_i2_ycards] <- sd(l6_form_i2_ycards)
#     sd_i2_one_ycards[index_i2_ycards] <- paste(sd_i2_one_ycards[index_i2_ycards],sep = "")
#     sum_i2_threemore_ycards[index_i2_ycards] <- length(which(l6_form_i2_ycards >= 3))
#     sum_i2_threemore_ycards[index_i2_ycards] <- paste(sum_i2_threemore_ycards[index_i2_ycards],sep = "")
#     sum_i2_twoless_ycards[index_i2_ycards] <- length(which(l6_form_i2_ycards <= 2))
#     sum_i2_twoless_ycards[index_i2_ycards] <- paste(sum_i2_twoless_ycards[index_i2_ycards],sep = "")
#     #l6_form_i2_ycards <- as.character(l6_form_i2_ycards)
#     #l6_form_i2_ycards_flattened <- stri_paste(l6_form_i2_ycards,collapse = '')
#     #l6_form_i2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_i2_ycards_flattened),"")[[1]])
#     final_i2_ycards[index_i2_ycards,index_i2_ycards_cols] <- l6_form_i2_ycards[index_i2_ycards_cols]
#   }
# }
#
# final_i2_ycards[is.na(final_i2_ycards)] <- ""
# i2_yellowscoredmatrix <- cbind(i2_teams,final_i2_ycards,suml6_i2_ycards,avg_i2_ycards,sd_i2_one_ycards,sum_i2_threemore_ycards,sum_i2_twoless_ycards)
# ########################################################################################################################################################
# #n1
# final_n1_ycards <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_ycards <- c()
# avg_n1_ycards <- c()
# sd_n1_one_ycards <- c()
# sum_n1_threemore_ycards <- c()
# sum_n1_twoless_ycards <- c()
# l6_form_n1_ycardssplitted <- c()
# form_n1_ycards <- c()
# for(index_n1_ycards in 1:length(n1_teams))
# {
#   for(index_n1_ycards_cols in 1:n1_totalrounds)
#   {
#     index_n1_ycards  <- row.names(n1_yellowscored_h) == n1_teams[index_n1_ycards]
#     form_n1_ycards <- n1_yellowscored_h[index_n1_ycards]
#     deleted_form_n1_ycards <- form_n1_ycards[!form_n1_ycards[] == ""]
#     l6_form_n1_ycards <- deleted_form_n1_ycards #tail(deleted_form_n1_ycards,n1_last_n_games)
#     l6_form_n1_ycards <- as.numeric(l6_form_n1_ycards)
#     suml6_n1_ycards[index_n1_ycards] <- sum(l6_form_n1_ycards)
#     suml6_n1_ycards[index_n1_ycards] <- paste(suml6_n1_ycards[index_n1_ycards],sep = "")
#     avg_n1_ycards[index_n1_ycards] <- mean(l6_form_n1_ycards)
#     avg_n1_ycards[index_n1_ycards] <- paste(avg_n1_ycards[index_n1_ycards],sep = "")
#     sd_n1_one_ycards[index_n1_ycards] <- sd(l6_form_n1_ycards)
#     sd_n1_one_ycards[index_n1_ycards] <- paste(sd_n1_one_ycards[index_n1_ycards],sep = "")
#     sum_n1_threemore_ycards[index_n1_ycards] <- length(which(l6_form_n1_ycards >= 3))
#     sum_n1_threemore_ycards[index_n1_ycards] <- paste(sum_n1_threemore_ycards[index_n1_ycards],sep = "")
#     sum_n1_twoless_ycards[index_n1_ycards] <- length(which(l6_form_n1_ycards <= 2))
#     sum_n1_twoless_ycards[index_n1_ycards] <- paste(sum_n1_twoless_ycards[index_n1_ycards],sep = "")
#     #l6_form_n1_ycards <- as.character(l6_form_n1_ycards)
#     #l6_form_n1_ycards_flattened <- stri_paste(l6_form_n1_ycards,collapse = '')
#     #l6_form_n1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_n1_ycards_flattened),"")[[1]])
#     final_n1_ycards[index_n1_ycards,index_n1_ycards_cols] <- l6_form_n1_ycards[index_n1_ycards_cols]
#   }
# }
#
# final_n1_ycards[is.na(final_n1_ycards)] <- ""
# n1_yellowscoredmatrix <- cbind(n1_teams,final_n1_ycards,suml6_n1_ycards,avg_n1_ycards,sd_n1_one_ycards,sum_n1_threemore_ycards,sum_n1_twoless_ycards)
# ########################################################################################################################################################
# #p1
# final_p1_ycards <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_ycards <- c()
# avg_p1_ycards <- c()
# sd_p1_one_ycards <- c()
# sum_p1_threemore_ycards <- c()
# sum_p1_twoless_ycards <- c()
# l6_form_p1_ycardssplitted <- c()
# form_p1_ycards <- c()
# for(index_p1_ycards in 1:length(p1_teams))
# {
#   for(index_p1_ycards_cols in 1:p1_totalrounds)
#   {
#     index_p1_ycards  <- row.names(p1_yellowscored_h) == p1_teams[index_p1_ycards]
#     form_p1_ycards <- p1_yellowscored_h[index_p1_ycards]
#     deleted_form_p1_ycards <- form_p1_ycards[!form_p1_ycards[] == ""]
#     l6_form_p1_ycards <- deleted_form_p1_ycards #tail(deleted_form_p1_ycards,p1_last_n_games)
#     l6_form_p1_ycards <- as.numeric(l6_form_p1_ycards)
#     suml6_p1_ycards[index_p1_ycards] <- sum(l6_form_p1_ycards)
#     suml6_p1_ycards[index_p1_ycards] <- paste(suml6_p1_ycards[index_p1_ycards],sep = "")
#     avg_p1_ycards[index_p1_ycards] <- mean(l6_form_p1_ycards)
#     avg_p1_ycards[index_p1_ycards] <- paste(avg_p1_ycards[index_p1_ycards],sep = "")
#     sd_p1_one_ycards[index_p1_ycards] <- sd(l6_form_p1_ycards)
#     sd_p1_one_ycards[index_p1_ycards] <- paste(sd_p1_one_ycards[index_p1_ycards],sep = "")
#     sum_p1_threemore_ycards[index_p1_ycards] <- length(which(l6_form_p1_ycards >= 3))
#     sum_p1_threemore_ycards[index_p1_ycards] <- paste(sum_p1_threemore_ycards[index_p1_ycards],sep = "")
#     sum_p1_twoless_ycards[index_p1_ycards] <- length(which(l6_form_p1_ycards <= 2))
#     sum_p1_twoless_ycards[index_p1_ycards] <- paste(sum_p1_twoless_ycards[index_p1_ycards],sep = "")
#     #l6_form_p1_ycards <- as.character(l6_form_p1_ycards)
#     #l6_form_p1_ycards_flattened <- stri_paste(l6_form_p1_ycards,collapse = '')
#     #l6_form_p1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_p1_ycards_flattened),"")[[1]])
#     final_p1_ycards[index_p1_ycards,index_p1_ycards_cols] <- l6_form_p1_ycards[index_p1_ycards_cols]
#   }
# }
#
# final_p1_ycards[is.na(final_p1_ycards)] <- ""
# p1_yellowscoredmatrix <- cbind(p1_teams,final_p1_ycards,suml6_p1_ycards,avg_p1_ycards,sd_p1_one_ycards,sum_p1_threemore_ycards,sum_p1_twoless_ycards)
# ########################################################################################################################################################
# #sc0
# final_sc0_ycards <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_ycards <- c()
# avg_sc0_ycards <- c()
# sd_sc0_one_ycards <- c()
# sum_sc0_threemore_ycards <- c()
# sum_sc0_twoless_ycards <- c()
# l6_form_sc0_ycardssplitted <- c()
# form_sc0_ycards <- c()
# for(index_sc0_ycards in 1:length(sc0_teams))
# {
#   for(index_sc0_ycards_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_ycards  <- row.names(sc0_yellowscored_h) == sc0_teams[index_sc0_ycards]
#     form_sc0_ycards <- sc0_yellowscored_h[index_sc0_ycards]
#     deleted_form_sc0_ycards <- form_sc0_ycards[!form_sc0_ycards[] == ""]
#     l6_form_sc0_ycards <- deleted_form_sc0_ycards #tail(deleted_form_sc0_ycards,sc0_last_n_games)
#     l6_form_sc0_ycards <- as.numeric(l6_form_sc0_ycards)
#     suml6_sc0_ycards[index_sc0_ycards] <- sum(l6_form_sc0_ycards)
#     suml6_sc0_ycards[index_sc0_ycards] <- paste(suml6_sc0_ycards[index_sc0_ycards],sep = "")
#     avg_sc0_ycards[index_sc0_ycards] <- mean(l6_form_sc0_ycards)
#     avg_sc0_ycards[index_sc0_ycards] <- paste(avg_sc0_ycards[index_sc0_ycards],sep = "")
#     sd_sc0_one_ycards[index_sc0_ycards] <- sd(l6_form_sc0_ycards)
#     sd_sc0_one_ycards[index_sc0_ycards] <- paste(sd_sc0_one_ycards[index_sc0_ycards],sep = "")
#     sum_sc0_threemore_ycards[index_sc0_ycards] <- length(which(l6_form_sc0_ycards >= 3))
#     sum_sc0_threemore_ycards[index_sc0_ycards] <- paste(sum_sc0_threemore_ycards[index_sc0_ycards],sep = "")
#     sum_sc0_twoless_ycards[index_sc0_ycards] <- length(which(l6_form_sc0_ycards <= 2))
#     sum_sc0_twoless_ycards[index_sc0_ycards] <- paste(sum_sc0_twoless_ycards[index_sc0_ycards],sep = "")
#     #l6_form_sc0_ycards <- as.character(l6_form_sc0_ycards)
#     #l6_form_sc0_ycards_flattened <- stri_paste(l6_form_sc0_ycards,collapse = '')
#     #l6_form_sc0_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sc0_ycards_flattened),"")[[1]])
#     final_sc0_ycards[index_sc0_ycards,index_sc0_ycards_cols] <- l6_form_sc0_ycards[index_sc0_ycards_cols]
#   }
# }
#
# final_sc0_ycards[is.na(final_sc0_ycards)] <- ""
# sc0_yellowscoredmatrix <- cbind(sc0_teams,final_sc0_ycards,suml6_sc0_ycards,avg_sc0_ycards,sd_sc0_one_ycards,sum_sc0_threemore_ycards,sum_sc0_twoless_ycards)
# ########################################################################################################################################################
# #sc1
# final_sc1_ycards <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_ycards <- c()
# avg_sc1_ycards <- c()
# sd_sc1_one_ycards <- c()
# sum_sc1_threemore_ycards <- c()
# sum_sc1_twoless_ycards <- c()
# l6_form_sc1_ycardssplitted <- c()
# form_sc1_ycards <- c()
# for(index_sc1_ycards in 1:length(sc1_teams))
# {
#   for(index_sc1_ycards_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_ycards  <- row.names(sc1_yellowscored_h) == sc1_teams[index_sc1_ycards]
#     form_sc1_ycards <- sc1_yellowscored_h[index_sc1_ycards]
#     deleted_form_sc1_ycards <- form_sc1_ycards[!form_sc1_ycards[] == ""]
#     l6_form_sc1_ycards <- deleted_form_sc1_ycards #tail(deleted_form_sc1_ycards,sc1_last_n_games)
#     l6_form_sc1_ycards <- as.numeric(l6_form_sc1_ycards)
#     suml6_sc1_ycards[index_sc1_ycards] <- sum(l6_form_sc1_ycards)
#     suml6_sc1_ycards[index_sc1_ycards] <- paste(suml6_sc1_ycards[index_sc1_ycards],sep = "")
#     avg_sc1_ycards[index_sc1_ycards] <- mean(l6_form_sc1_ycards)
#     avg_sc1_ycards[index_sc1_ycards] <- paste(avg_sc1_ycards[index_sc1_ycards],sep = "")
#     sd_sc1_one_ycards[index_sc1_ycards] <- sd(l6_form_sc1_ycards)
#     sd_sc1_one_ycards[index_sc1_ycards] <- paste(sd_sc1_one_ycards[index_sc1_ycards],sep = "")
#     sum_sc1_threemore_ycards[index_sc1_ycards] <- length(which(l6_form_sc1_ycards >= 3))
#     sum_sc1_threemore_ycards[index_sc1_ycards] <- paste(sum_sc1_threemore_ycards[index_sc1_ycards],sep = "")
#     sum_sc1_twoless_ycards[index_sc1_ycards] <- length(which(l6_form_sc1_ycards <= 2))
#     sum_sc1_twoless_ycards[index_sc1_ycards] <- paste(sum_sc1_twoless_ycards[index_sc1_ycards],sep = "")
#     #l6_form_sc1_ycards <- as.character(l6_form_sc1_ycards)
#     #l6_form_sc1_ycards_flattened <- stri_paste(l6_form_sc1_ycards,collapse = '')
#     #l6_form_sc1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sc1_ycards_flattened),"")[[1]])
#     final_sc1_ycards[index_sc1_ycards,index_sc1_ycards_cols] <- l6_form_sc1_ycards[index_sc1_ycards_cols]
#   }
# }
#
# final_sc1_ycards[is.na(final_sc1_ycards)] <- ""
# sc1_yellowscoredmatrix <- cbind(sc1_teams,final_sc1_ycards,suml6_sc1_ycards,avg_sc1_ycards,sd_sc1_one_ycards,sum_sc1_threemore_ycards,sum_sc1_twoless_ycards)
# ########################################################################################################################################################
# #sc2
# final_sc2_ycards <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_ycards <- c()
# avg_sc2_ycards <- c()
# sd_sc2_one_ycards <- c()
# sum_sc2_threemore_ycards <- c()
# sum_sc2_twoless_ycards <- c()
# l6_form_sc2_ycardssplitted <- c()
# form_sc2_ycards <- c()
# for(index_sc2_ycards in 1:length(sc2_teams))
# {
#   for(index_sc2_ycards_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_ycards  <- row.names(sc2_yellowscored_h) == sc2_teams[index_sc2_ycards]
#     form_sc2_ycards <- sc2_yellowscored_h[index_sc2_ycards]
#     deleted_form_sc2_ycards <- form_sc2_ycards[!form_sc2_ycards[] == ""]
#     l6_form_sc2_ycards <- deleted_form_sc2_ycards #tail(deleted_form_sc2_ycards,sc2_last_n_games)
#     l6_form_sc2_ycards <- as.numeric(l6_form_sc2_ycards)
#     suml6_sc2_ycards[index_sc2_ycards] <- sum(l6_form_sc2_ycards)
#     suml6_sc2_ycards[index_sc2_ycards] <- paste(suml6_sc2_ycards[index_sc2_ycards],sep = "")
#     avg_sc2_ycards[index_sc2_ycards] <- mean(l6_form_sc2_ycards)
#     avg_sc2_ycards[index_sc2_ycards] <- paste(avg_sc2_ycards[index_sc2_ycards],sep = "")
#     sd_sc2_one_ycards[index_sc2_ycards] <- sd(l6_form_sc2_ycards)
#     sd_sc2_one_ycards[index_sc2_ycards] <- paste(sd_sc2_one_ycards[index_sc2_ycards],sep = "")
#     sum_sc2_threemore_ycards[index_sc2_ycards] <- length(which(l6_form_sc2_ycards >= 3))
#     sum_sc2_threemore_ycards[index_sc2_ycards] <- paste(sum_sc2_threemore_ycards[index_sc2_ycards],sep = "")
#     sum_sc2_twoless_ycards[index_sc2_ycards] <- length(which(l6_form_sc2_ycards <= 2))
#     sum_sc2_twoless_ycards[index_sc2_ycards] <- paste(sum_sc2_twoless_ycards[index_sc2_ycards],sep = "")
#     #l6_form_sc2_ycards <- as.character(l6_form_sc2_ycards)
#     #l6_form_sc2_ycards_flattened <- stri_paste(l6_form_sc2_ycards,collapse = '')
#     #l6_form_sc2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sc2_ycards_flattened),"")[[1]])
#     final_sc2_ycards[index_sc2_ycards,index_sc2_ycards_cols] <- l6_form_sc2_ycards[index_sc2_ycards_cols]
#   }
# }
#
# final_sc2_ycards[is.na(final_sc2_ycards)] <- ""
# sc2_yellowscoredmatrix <- cbind(sc2_teams,final_sc2_ycards,suml6_sc2_ycards,avg_sc2_ycards,sd_sc2_one_ycards,sum_sc2_threemore_ycards,sum_sc2_twoless_ycards)
# ########################################################################################################################################################
# #sc3
# final_sc3_ycards <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_ycards <- c()
# avg_sc3_ycards <- c()
# sd_sc3_one_ycards <- c()
# sum_sc3_threemore_ycards <- c()
# sum_sc3_twoless_ycards <- c()
# l6_form_sc3_ycardssplitted <- c()
# form_sc3_ycards <- c()
# for(index_sc3_ycards in 1:length(sc3_teams))
# {
#   for(index_sc3_ycards_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_ycards  <- row.names(sc3_yellowscored_h) == sc3_teams[index_sc3_ycards]
#     form_sc3_ycards <- sc3_yellowscored_h[index_sc3_ycards]
#     deleted_form_sc3_ycards <- form_sc3_ycards[!form_sc3_ycards[] == ""]
#     l6_form_sc3_ycards <- deleted_form_sc3_ycards #tail(deleted_form_sc3_ycards,sc3_last_n_games)
#     l6_form_sc3_ycards <- as.numeric(l6_form_sc3_ycards)
#     suml6_sc3_ycards[index_sc3_ycards] <- sum(l6_form_sc3_ycards)
#     suml6_sc3_ycards[index_sc3_ycards] <- paste(suml6_sc3_ycards[index_sc3_ycards],sep = "")
#     avg_sc3_ycards[index_sc3_ycards] <- mean(l6_form_sc3_ycards)
#     avg_sc3_ycards[index_sc3_ycards] <- paste(avg_sc3_ycards[index_sc3_ycards],sep = "")
#     sd_sc3_one_ycards[index_sc3_ycards] <- sd(l6_form_sc3_ycards)
#     sd_sc3_one_ycards[index_sc3_ycards] <- paste(sd_sc3_one_ycards[index_sc3_ycards],sep = "")
#     sum_sc3_threemore_ycards[index_sc3_ycards] <- length(which(l6_form_sc3_ycards >= 3))
#     sum_sc3_threemore_ycards[index_sc3_ycards] <- paste(sum_sc3_threemore_ycards[index_sc3_ycards],sep = "")
#     sum_sc3_twoless_ycards[index_sc3_ycards] <- length(which(l6_form_sc3_ycards <= 2))
#     sum_sc3_twoless_ycards[index_sc3_ycards] <- paste(sum_sc3_twoless_ycards[index_sc3_ycards],sep = "")
#     #l6_form_sc3_ycards <- as.character(l6_form_sc3_ycards)
#     #l6_form_sc3_ycards_flattened <- stri_paste(l6_form_sc3_ycards,collapse = '')
#     #l6_form_sc3_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sc3_ycards_flattened),"")[[1]])
#     final_sc3_ycards[index_sc3_ycards,index_sc3_ycards_cols] <- l6_form_sc3_ycards[index_sc3_ycards_cols]
#   }
# }
#
# final_sc3_ycards[is.na(final_sc3_ycards)] <- ""
# sc3_yellowscoredmatrix <- cbind(sc3_teams,final_sc3_ycards,suml6_sc3_ycards,avg_sc3_ycards,sd_sc3_one_ycards,sum_sc3_threemore_ycards,sum_sc3_twoless_ycards)
# ########################################################################################################################################################
# #sp1
# final_sp1_ycards <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_ycards <- c()
# avg_sp1_ycards <- c()
# sd_sp1_one_ycards <- c()
# sum_sp1_threemore_ycards <- c()
# sum_sp1_twoless_ycards <- c()
# l6_form_sp1_ycardssplitted <- c()
# form_sp1_ycards <- c()
# for(index_sp1_ycards in 1:length(sp1_teams))
# {
#   for(index_sp1_ycards_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_ycards  <- row.names(sp1_yellowscored_h) == sp1_teams[index_sp1_ycards]
#     form_sp1_ycards <- sp1_yellowscored_h[index_sp1_ycards]
#     deleted_form_sp1_ycards <- form_sp1_ycards[!form_sp1_ycards[] == ""]
#     l6_form_sp1_ycards <- deleted_form_sp1_ycards #tail(deleted_form_sp1_ycards,sp1_last_n_games)
#     l6_form_sp1_ycards <- as.numeric(l6_form_sp1_ycards)
#     suml6_sp1_ycards[index_sp1_ycards] <- sum(l6_form_sp1_ycards)
#     suml6_sp1_ycards[index_sp1_ycards] <- paste(suml6_sp1_ycards[index_sp1_ycards],sep = "")
#     avg_sp1_ycards[index_sp1_ycards] <- mean(l6_form_sp1_ycards)
#     avg_sp1_ycards[index_sp1_ycards] <- paste(avg_sp1_ycards[index_sp1_ycards],sep = "")
#     sd_sp1_one_ycards[index_sp1_ycards] <- sd(l6_form_sp1_ycards)
#     sd_sp1_one_ycards[index_sp1_ycards] <- paste(sd_sp1_one_ycards[index_sp1_ycards],sep = "")
#     sum_sp1_threemore_ycards[index_sp1_ycards] <- length(which(l6_form_sp1_ycards >= 3))
#     sum_sp1_threemore_ycards[index_sp1_ycards] <- paste(sum_sp1_threemore_ycards[index_sp1_ycards],sep = "")
#     sum_sp1_twoless_ycards[index_sp1_ycards] <- length(which(l6_form_sp1_ycards <= 2))
#     sum_sp1_twoless_ycards[index_sp1_ycards] <- paste(sum_sp1_twoless_ycards[index_sp1_ycards],sep = "")
#     #l6_form_sp1_ycards <- as.character(l6_form_sp1_ycards)
#     #l6_form_sp1_ycards_flattened <- stri_paste(l6_form_sp1_ycards,collapse = '')
#     #l6_form_sp1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sp1_ycards_flattened),"")[[1]])
#     final_sp1_ycards[index_sp1_ycards,index_sp1_ycards_cols] <- l6_form_sp1_ycards[index_sp1_ycards_cols]
#   }
# }
#
# final_sp1_ycards[is.na(final_sp1_ycards)] <- ""
# sp1_yellowscoredmatrix <- cbind(sp1_teams,final_sp1_ycards,suml6_sp1_ycards,avg_sp1_ycards,sd_sp1_one_ycards,sum_sp1_threemore_ycards,sum_sp1_twoless_ycards)
# ########################################################################################################################################################
# #sp2
# final_sp2_ycards <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_ycards <- c()
# avg_sp2_ycards <- c()
# sd_sp2_one_ycards <- c()
# sum_sp2_threemore_ycards <- c()
# sum_sp2_twoless_ycards <- c()
# l6_form_sp2_ycardssplitted <- c()
# form_sp2_ycards <- c()
# for(index_sp2_ycards in 1:length(sp2_teams))
# {
#   for(index_sp2_ycards_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_ycards  <- row.names(sp2_yellowscored_h) == sp2_teams[index_sp2_ycards]
#     form_sp2_ycards <- sp2_yellowscored_h[index_sp2_ycards]
#     deleted_form_sp2_ycards <- form_sp2_ycards[!form_sp2_ycards[] == ""]
#     l6_form_sp2_ycards <- deleted_form_sp2_ycards #tail(deleted_form_sp2_ycards,sp2_last_n_games)
#     l6_form_sp2_ycards <- as.numeric(l6_form_sp2_ycards)
#     suml6_sp2_ycards[index_sp2_ycards] <- sum(l6_form_sp2_ycards)
#     suml6_sp2_ycards[index_sp2_ycards] <- paste(suml6_sp2_ycards[index_sp2_ycards],sep = "")
#     avg_sp2_ycards[index_sp2_ycards] <- mean(l6_form_sp2_ycards)
#     avg_sp2_ycards[index_sp2_ycards] <- paste(avg_sp2_ycards[index_sp2_ycards],sep = "")
#     sd_sp2_one_ycards[index_sp2_ycards] <- sd(l6_form_sp2_ycards)
#     sd_sp2_one_ycards[index_sp2_ycards] <- paste(sd_sp2_one_ycards[index_sp2_ycards],sep = "")
#     sum_sp2_threemore_ycards[index_sp2_ycards] <- length(which(l6_form_sp2_ycards >= 3))
#     sum_sp2_threemore_ycards[index_sp2_ycards] <- paste(sum_sp2_threemore_ycards[index_sp2_ycards],sep = "")
#     sum_sp2_twoless_ycards[index_sp2_ycards] <- length(which(l6_form_sp2_ycards <= 2))
#     sum_sp2_twoless_ycards[index_sp2_ycards] <- paste(sum_sp2_twoless_ycards[index_sp2_ycards],sep = "")
#     #l6_form_sp2_ycards <- as.character(l6_form_sp2_ycards)
#     #l6_form_sp2_ycards_flattened <- stri_paste(l6_form_sp2_ycards,collapse = '')
#     #l6_form_sp2_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_sp2_ycards_flattened),"")[[1]])
#     final_sp2_ycards[index_sp2_ycards,index_sp2_ycards_cols] <- l6_form_sp2_ycards[index_sp2_ycards_cols]
#   }
# }
#
# final_sp2_ycards[is.na(final_sp2_ycards)] <- ""
# sp2_yellowscoredmatrix <- cbind(sp2_teams,final_sp2_ycards,suml6_sp2_ycards,avg_sp2_ycards,sd_sp2_one_ycards,sum_sp2_threemore_ycards,sum_sp2_twoless_ycards)
# ########################################################################################################################################################
# #t1
# final_t1_ycards <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_ycards <- c()
# avg_t1_ycards <- c()
# sd_t1_one_ycards <- c()
# sum_t1_threemore_ycards <- c()
# sum_t1_twoless_ycards <- c()
# l6_form_t1_ycardssplitted <- c()
# form_t1_ycards <- c()
# for(index_t1_ycards in 1:length(t1_teams))
# {
#   for(index_t1_ycards_cols in 1:t1_totalrounds)
#   {
#     index_t1_ycards  <- row.names(t1_yellowscored_h) == t1_teams[index_t1_ycards]
#     form_t1_ycards <- t1_yellowscored_h[index_t1_ycards]
#     deleted_form_t1_ycards <- form_t1_ycards[!form_t1_ycards[] == ""]
#     l6_form_t1_ycards <- deleted_form_t1_ycards #tail(deleted_form_t1_ycards,t1_last_n_games)
#     l6_form_t1_ycards <- as.numeric(l6_form_t1_ycards)
#     suml6_t1_ycards[index_t1_ycards] <- sum(l6_form_t1_ycards)
#     suml6_t1_ycards[index_t1_ycards] <- paste(suml6_t1_ycards[index_t1_ycards],sep = "")
#     avg_t1_ycards[index_t1_ycards] <- mean(l6_form_t1_ycards)
#     avg_t1_ycards[index_t1_ycards] <- paste(avg_t1_ycards[index_t1_ycards],sep = "")
#     sd_t1_one_ycards[index_t1_ycards] <- sd(l6_form_t1_ycards)
#     sd_t1_one_ycards[index_t1_ycards] <- paste(sd_t1_one_ycards[index_t1_ycards],sep = "")
#     sum_t1_threemore_ycards[index_t1_ycards] <- length(which(l6_form_t1_ycards >= 3))
#     sum_t1_threemore_ycards[index_t1_ycards] <- paste(sum_t1_threemore_ycards[index_t1_ycards],sep = "")
#     sum_t1_twoless_ycards[index_t1_ycards] <- length(which(l6_form_t1_ycards <= 2))
#     sum_t1_twoless_ycards[index_t1_ycards] <- paste(sum_t1_twoless_ycards[index_t1_ycards],sep = "")
#     #l6_form_t1_ycards <- as.character(l6_form_t1_ycards)
#     #l6_form_t1_ycards_flattened <- stri_paste(l6_form_t1_ycards,collapse = '')
#     #l6_form_t1_ycardssplitted <- as.numeric(strsplit(as.character(l6_form_t1_ycards_flattened),"")[[1]])
#     final_t1_ycards[index_t1_ycards,index_t1_ycards_cols] <- l6_form_t1_ycards[index_t1_ycards_cols]
#   }
# }
#
# final_t1_ycards[is.na(final_t1_ycards)] <- ""
# t1_yellowscoredmatrix <- cbind(t1_teams,final_t1_ycards,suml6_t1_ycards,avg_t1_ycards,sd_t1_one_ycards,sum_t1_threemore_ycards,sum_t1_twoless_ycards)
########################################################################################################################################################



















