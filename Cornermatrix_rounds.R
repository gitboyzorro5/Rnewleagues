library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_cor <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_cor <- c()
avg_mls_cor <- c()
sd_mls_one_cor <- c()
sum_mls_fivemore_cor <- c()
sum_mls_fourless_cor <- c()
l6_form_mls_corsplitted <- c()
form_mls_cor <- c()
for(index_mls_cor in 1:length(mls_teams))
{
  for(index_mls_cor_cols in 1:mls_totalrounds)
  {
    index_mls_cor  <- row.names(mls_coawarded_h) == mls_teams[index_mls_cor]
    form_mls_cor <- mls_coawarded_h[index_mls_cor]
    deleted_form_mls_cor <- form_mls_cor[!form_mls_cor[] == ""]
    l6_form_mls_cor <- deleted_form_mls_cor #tail(deleted_form_mls_cor,mls_last_n_games)
    l6_form_mls_cor <- as.numeric(l6_form_mls_cor)
    suml6_mls_cor[index_mls_cor] <- sum(l6_form_mls_cor)
    suml6_mls_cor[index_mls_cor] <- paste(suml6_mls_cor[index_mls_cor],sep = "")
    avg_mls_cor[index_mls_cor] <- mean(l6_form_mls_cor)
    avg_mls_cor[index_mls_cor] <- paste(avg_mls_cor[index_mls_cor],sep = "")
    sd_mls_one_cor[index_mls_cor] <- sd(l6_form_mls_cor)
    sd_mls_one_cor[index_mls_cor] <- paste(sd_mls_one_cor[index_mls_cor],sep = "")
    sum_mls_fivemore_cor[index_mls_cor] <- length(which(l6_form_mls_cor >= 5))
    sum_mls_fivemore_cor[index_mls_cor] <- paste(sum_mls_fivemore_cor[index_mls_cor],sep = "")
    sum_mls_fourless_cor[index_mls_cor] <- length(which(l6_form_mls_cor <= 4))
    sum_mls_fourless_cor[index_mls_cor] <- paste(sum_mls_fourless_cor[index_mls_cor],sep = "")
    #l6_form_mls_cor <- as.character(l6_form_mls_cor)
    #l6_form_mls_cor_flattened <- stri_paste(l6_form_mls_cor,collapse = '')
    #l6_form_mls_corsplitted <- as.numeric(strsplit(as.character(l6_form_mls_cor_flattened),"")[[1]])
    final_mls_cor[index_mls_cor,index_mls_cor_cols] <- l6_form_mls_cor[index_mls_cor_cols]
  }
}


final_mls_cor[is.na(final_mls_cor)] <- ""
mls_coawardedmatrix <- cbind(mls_teams,final_mls_cor,suml6_mls_cor,avg_mls_cor,sd_mls_one_cor,sum_mls_fivemore_cor,sum_mls_fourless_cor)
#############################################################################################################################################
#D1
final_arg_cor <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_cor <- c()
avg_arg_cor <- c()
sd_arg_one_cor <- c()
sum_arg_fivemore_cor <- c()
sum_arg_fourless_cor <- c()
l6_form_arg_corsplitted <- c()
form_arg_cor <- c()
for(index_arg_cor in 1:length(arg_teams))
{
  for(index_arg_cor_cols in 1:arg_totalrounds)
  {
    index_arg_cor  <- row.names(arg_coawarded_h) == arg_teams[index_arg_cor]
    form_arg_cor <- arg_coawarded_h[index_arg_cor]
    deleted_form_arg_cor <- form_arg_cor[!form_arg_cor[] == ""]
    l6_form_arg_cor <- deleted_form_arg_cor #tail(deleted_form_arg_cor,arg_last_n_games)
    l6_form_arg_cor <- as.numeric(l6_form_arg_cor)
    suml6_arg_cor[index_arg_cor] <- sum(l6_form_arg_cor)
    suml6_arg_cor[index_arg_cor] <- paste(suml6_arg_cor[index_arg_cor],sep = "")
    avg_arg_cor[index_arg_cor] <- mean(l6_form_arg_cor)
    avg_arg_cor[index_arg_cor] <- paste(avg_arg_cor[index_arg_cor],sep = "")
    sd_arg_one_cor[index_arg_cor] <- sd(l6_form_arg_cor)
    sd_arg_one_cor[index_arg_cor] <- paste(sd_arg_one_cor[index_arg_cor],sep = "")
    sum_arg_fivemore_cor[index_arg_cor] <- length(which(l6_form_arg_cor >= 5))
    sum_arg_fivemore_cor[index_arg_cor] <- paste(sum_arg_fivemore_cor[index_arg_cor],sep = "")
    sum_arg_fourless_cor[index_arg_cor] <- length(which(l6_form_arg_cor <= 4))
    sum_arg_fourless_cor[index_arg_cor] <- paste(sum_arg_fourless_cor[index_arg_cor],sep = "")
    #l6_form_arg_cor <- as.character(l6_form_arg_cor)
    #l6_form_arg_cor_flattened <- stri_paste(l6_form_arg_cor,collapse = '')
    #l6_form_arg_corsplitted <- as.numeric(strsplit(as.character(l6_form_arg_cor_flattened),"")[[1]])
    final_arg_cor[index_arg_cor,index_arg_cor_cols] <- l6_form_arg_cor[index_arg_cor_cols]
  }
}


final_arg_cor[is.na(final_arg_cor)] <- ""
arg_coawardedmatrix <- cbind(arg_teams,final_arg_cor,suml6_arg_cor,avg_arg_cor,sd_arg_one_cor,sum_arg_fivemore_cor,sum_arg_fourless_cor)
################################################################################################################################
#d2
# final_d2_cor <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_cor <- c()
# avg_d2_cor <- c()
# sd_d2_one_cor <- c()
# sum_d2_fivemore_cor <- c()
# sum_d2_fourless_cor <- c()
# l6_form_d2_corsplitted <- c()
# form_d2_cor <- c()
# for(index_d2_cor in 1:length(d2_teams))
# {
#   for(index_d2_cor_cols in 1:d2_totalrounds)
#   {
#     index_d2_cor  <- row.names(d2_coawarded_h) == d2_teams[index_d2_cor]
#     form_d2_cor <- d2_coawarded_h[index_d2_cor]
#     deleted_form_d2_cor <- form_d2_cor[!form_d2_cor[] == ""]
#     l6_form_d2_cor <- deleted_form_d2_cor #tail(deleted_form_d2_cor,d2_last_n_games)
#     l6_form_d2_cor <- as.numeric(l6_form_d2_cor)
#     suml6_d2_cor[index_d2_cor] <- sum(l6_form_d2_cor)
#     suml6_d2_cor[index_d2_cor] <- paste(suml6_d2_cor[index_d2_cor],sep = "")
#     avg_d2_cor[index_d2_cor] <- mean(l6_form_d2_cor)
#     avg_d2_cor[index_d2_cor] <- paste(avg_d2_cor[index_d2_cor],sep = "")
#     sd_d2_one_cor[index_d2_cor] <- sd(l6_form_d2_cor)
#     sd_d2_one_cor[index_d2_cor] <- paste(sd_d2_one_cor[index_d2_cor],sep = "")
#     sum_d2_fivemore_cor[index_d2_cor] <- length(which(l6_form_d2_cor >= 5))
#     sum_d2_fivemore_cor[index_d2_cor] <- paste(sum_d2_fivemore_cor[index_d2_cor],sep = "")
#     sum_d2_fourless_cor[index_d2_cor] <- length(which(l6_form_d2_cor <= 4))
#     sum_d2_fourless_cor[index_d2_cor] <- paste(sum_d2_fourless_cor[index_d2_cor],sep = "")
#     #l6_form_d2_cor <- as.character(l6_form_d2_cor)
#     #l6_form_d2_cor_flattened <- stri_paste(l6_form_d2_cor,collapse = '')
#     #l6_form_d2_corsplitted <- as.numeric(strsplit(as.character(l6_form_d2_cor_flattened),"")[[1]])
#     final_d2_cor[index_d2_cor,index_d2_cor_cols] <- l6_form_d2_cor[index_d2_cor_cols]
#   }
# }
#
#
# final_d2_cor[is.na(final_d2_cor)] <- ""
# d2_coawardedmatrix <- cbind(d2_teams,final_d2_cor,suml6_d2_cor,avg_d2_cor,sd_d2_one_cor,sum_d2_fivemore_cor,sum_d2_fourless_cor)
# ###########################################################################################################################################
# #e0
# final_e0_cor <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_cor <- c()
# avg_e0_cor <- c()
# sd_e0_one_cor <- c()
# sum_e0_fivemore_cor <- c()
# sum_e0_fourless_cor <- c()
# l6_form_e0_corsplitted <- c()
# form_e0_cor <- c()
# for(index_e0_cor in 1:length(e0_teams))
# {
#   for(index_e0_cor_cols in 1:e0_totalrounds)
#   {
#     index_e0_cor  <- row.names(e0_coawarded_h) == e0_teams[index_e0_cor]
#     form_e0_cor <- e0_coawarded_h[index_e0_cor]
#     deleted_form_e0_cor <- form_e0_cor[!form_e0_cor[] == ""]
#     l6_form_e0_cor <- deleted_form_e0_cor #tail(deleted_form_e0_cor,e0_last_n_games)
#     l6_form_e0_cor <- as.numeric(l6_form_e0_cor)
#     suml6_e0_cor[index_e0_cor] <- sum(l6_form_e0_cor)
#     suml6_e0_cor[index_e0_cor] <- paste(suml6_e0_cor[index_e0_cor],sep = "")
#     avg_e0_cor[index_e0_cor] <- mean(l6_form_e0_cor)
#     avg_e0_cor[index_e0_cor] <- paste(avg_e0_cor[index_e0_cor],sep = "")
#     sd_e0_one_cor[index_e0_cor] <- sd(l6_form_e0_cor)
#     sd_e0_one_cor[index_e0_cor] <- paste(sd_e0_one_cor[index_e0_cor],sep = "")
#     sum_e0_fivemore_cor[index_e0_cor] <- length(which(l6_form_e0_cor >= 5))
#     sum_e0_fivemore_cor[index_e0_cor] <- paste(sum_e0_fivemore_cor[index_e0_cor],sep = "")
#     sum_e0_fourless_cor[index_e0_cor] <- length(which(l6_form_e0_cor <= 4))
#     sum_e0_fourless_cor[index_e0_cor] <- paste(sum_e0_fourless_cor[index_e0_cor],sep = "")
#     #l6_form_e0_cor <- as.character(l6_form_e0_cor)
#     #l6_form_e0_cor_flattened <- stri_paste(l6_form_e0_cor,collapse = '')
#     #l6_form_e0_corsplitted <- as.numeric(strsplit(as.character(l6_form_e0_cor_flattened),"")[[1]])
#     final_e0_cor[index_e0_cor,index_e0_cor_cols] <- l6_form_e0_cor[index_e0_cor_cols]
#   }
# }
#
#
# final_e0_cor[is.na(final_e0_cor)] <- ""
# e0_coawardedmatrix <- cbind(e0_teams,final_e0_cor,suml6_e0_cor,avg_e0_cor,sd_e0_one_cor,sum_e0_fivemore_cor,sum_e0_fourless_cor)
# #############################################################################################################################################
# #e1
# final_e1_cor <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_cor <- c()
# avg_e1_cor <- c()
# sd_e1_one_cor <- c()
# sum_e1_fivemore_cor <- c()
# sum_e1_fourless_cor <- c()
# l6_form_e1_corsplitted <- c()
# form_e1_cor <- c()
# for(index_e1_cor in 1:length(e1_teams))
# {
#   for(index_e1_cor_cols in 1:e1_totalrounds)
#   {
#     index_e1_cor  <- row.names(e1_coawarded_h) == e1_teams[index_e1_cor]
#     form_e1_cor <- e1_coawarded_h[index_e1_cor]
#     deleted_form_e1_cor <- form_e1_cor[!form_e1_cor[] == ""]
#     l6_form_e1_cor <- deleted_form_e1_cor #tail(deleted_form_e1_cor,e1_last_n_games)
#     l6_form_e1_cor <- as.numeric(l6_form_e1_cor)
#     suml6_e1_cor[index_e1_cor] <- sum(l6_form_e1_cor)
#     suml6_e1_cor[index_e1_cor] <- paste(suml6_e1_cor[index_e1_cor],sep = "")
#     avg_e1_cor[index_e1_cor] <- mean(l6_form_e1_cor)
#     avg_e1_cor[index_e1_cor] <- paste(avg_e1_cor[index_e1_cor],sep = "")
#     sd_e1_one_cor[index_e1_cor] <- sd(l6_form_e1_cor)
#     sd_e1_one_cor[index_e1_cor] <- paste(sd_e1_one_cor[index_e1_cor],sep = "")
#     sum_e1_fivemore_cor[index_e1_cor] <- length(which(l6_form_e1_cor >= 5))
#     sum_e1_fivemore_cor[index_e1_cor] <- paste(sum_e1_fivemore_cor[index_e1_cor],sep = "")
#     sum_e1_fourless_cor[index_e1_cor] <- length(which(l6_form_e1_cor <= 4))
#     sum_e1_fourless_cor[index_e1_cor] <- paste(sum_e1_fourless_cor[index_e1_cor],sep = "")
#     #l6_form_e1_cor <- as.character(l6_form_e1_cor)
#     #l6_form_e1_cor_flattened <- stri_paste(l6_form_e1_cor,collapse = '')
#     #l6_form_e1_corsplitted <- as.numeric(strsplit(as.character(l6_form_e1_cor_flattened),"")[[1]])
#     final_e1_cor[index_e1_cor,index_e1_cor_cols] <- l6_form_e1_cor[index_e1_cor_cols]
#   }
# }
#
#
# final_e1_cor[is.na(final_e1_cor)] <- ""
# e1_coawardedmatrix <- cbind(e1_teams,final_e1_cor,suml6_e1_cor,avg_e1_cor,sd_e1_one_cor,sum_e1_fivemore_cor,sum_e1_fourless_cor)
# #############################################################################################################################################
# #e2
# final_e2_cor <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_cor <- c()
# avg_e2_cor <- c()
# sd_e2_one_cor <- c()
# sum_e2_fivemore_cor <- c()
# sum_e2_fourless_cor <- c()
# l6_form_e2_corsplitted <- c()
# form_e2_cor <- c()
# for(index_e2_cor in 1:length(e2_teams))
# {
#   for(index_e2_cor_cols in 1:e2_totalrounds)
#   {
#     index_e2_cor  <- row.names(e2_coawarded_h) == e2_teams[index_e2_cor]
#     form_e2_cor <- e2_coawarded_h[index_e2_cor]
#     deleted_form_e2_cor <- form_e2_cor[!form_e2_cor[] == ""]
#     l6_form_e2_cor <- deleted_form_e2_cor #tail(deleted_form_e2_cor,e2_last_n_games)
#     l6_form_e2_cor <- as.numeric(l6_form_e2_cor)
#     suml6_e2_cor[index_e2_cor] <- sum(l6_form_e2_cor)
#     suml6_e2_cor[index_e2_cor] <- paste(suml6_e2_cor[index_e2_cor],sep = "")
#     avg_e2_cor[index_e2_cor] <- mean(l6_form_e2_cor)
#     avg_e2_cor[index_e2_cor] <- paste(avg_e2_cor[index_e2_cor],sep = "")
#     sd_e2_one_cor[index_e2_cor] <- sd(l6_form_e2_cor)
#     sd_e2_one_cor[index_e2_cor] <- paste(sd_e2_one_cor[index_e2_cor],sep = "")
#     sum_e2_fivemore_cor[index_e2_cor] <- length(which(l6_form_e2_cor >= 5))
#     sum_e2_fivemore_cor[index_e2_cor] <- paste(sum_e2_fivemore_cor[index_e2_cor],sep = "")
#     sum_e2_fourless_cor[index_e2_cor] <- length(which(l6_form_e2_cor <= 4))
#     sum_e2_fourless_cor[index_e2_cor] <- paste(sum_e2_fourless_cor[index_e2_cor],sep = "")
#     #l6_form_e2_cor <- as.character(l6_form_e2_cor)
#     #l6_form_e2_cor_flattened <- stri_paste(l6_form_e2_cor,collapse = '')
#     #l6_form_e2_corsplitted <- as.numeric(strsplit(as.character(l6_form_e2_cor_flattened),"")[[1]])
#     final_e2_cor[index_e2_cor,index_e2_cor_cols] <- l6_form_e2_cor[index_e2_cor_cols]
#   }
# }
#
#
# final_e2_cor[is.na(final_e2_cor)] <- ""
# e2_coawardedmatrix <- cbind(e2_teams,final_e2_cor,suml6_e2_cor,avg_e2_cor,sd_e2_one_cor,sum_e2_fivemore_cor,sum_e2_fourless_cor)
# #############################################################################################################################################
# #e3
# final_e3_cor <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_cor <- c()
# avg_e3_cor <- c()
# sd_e3_one_cor <- c()
# sum_e3_fivemore_cor <- c()
# sum_e3_fourless_cor <- c()
# l6_form_e3_corsplitted <- c()
# form_e3_cor <- c()
# for(index_e3_cor in 1:length(e3_teams))
# {
#   for(index_e3_cor_cols in 1:e3_totalrounds)
#   {
#     index_e3_cor  <- row.names(e3_coawarded_h) == e3_teams[index_e3_cor]
#     form_e3_cor <- e3_coawarded_h[index_e3_cor]
#     deleted_form_e3_cor <- form_e3_cor[!form_e3_cor[] == ""]
#     l6_form_e3_cor <- deleted_form_e3_cor #tail(deleted_form_e3_cor,e3_last_n_games)
#     l6_form_e3_cor <- as.numeric(l6_form_e3_cor)
#     suml6_e3_cor[index_e3_cor] <- sum(l6_form_e3_cor)
#     suml6_e3_cor[index_e3_cor] <- paste(suml6_e3_cor[index_e3_cor],sep = "")
#     avg_e3_cor[index_e3_cor] <- mean(l6_form_e3_cor)
#     avg_e3_cor[index_e3_cor] <- paste(avg_e3_cor[index_e3_cor],sep = "")
#     sd_e3_one_cor[index_e3_cor] <- sd(l6_form_e3_cor)
#     sd_e3_one_cor[index_e3_cor] <- paste(sd_e3_one_cor[index_e3_cor],sep = "")
#     sum_e3_fivemore_cor[index_e3_cor] <- length(which(l6_form_e3_cor >= 5))
#     sum_e3_fivemore_cor[index_e3_cor] <- paste(sum_e3_fivemore_cor[index_e3_cor],sep = "")
#     sum_e3_fourless_cor[index_e3_cor] <- length(which(l6_form_e3_cor <= 4))
#     sum_e3_fourless_cor[index_e3_cor] <- paste(sum_e3_fourless_cor[index_e3_cor],sep = "")
#     #l6_form_e3_cor <- as.character(l6_form_e3_cor)
#     #l6_form_e3_cor_flattened <- stri_paste(l6_form_e3_cor,collapse = '')
#     #l6_form_e3_corsplitted <- as.numeric(strsplit(as.character(l6_form_e3_cor_flattened),"")[[1]])
#     final_e3_cor[index_e3_cor,index_e3_cor_cols] <- l6_form_e3_cor[index_e3_cor_cols]
#   }
# }
#
#
# final_e3_cor[is.na(final_e3_cor)] <- ""
# e3_coawardedmatrix <- cbind(e3_teams,final_e3_cor,suml6_e3_cor,avg_e3_cor,sd_e3_one_cor,sum_e3_fivemore_cor,sum_e3_fourless_cor)
# #############################################################################################################################################
# #ec
# final_ec_cor <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_cor <- c()
# avg_ec_cor <- c()
# sd_ec_one_cor <- c()
# sum_ec_fivemore_cor <- c()
# sum_ec_fourless_cor <- c()
# l6_form_ec_corsplitted <- c()
# form_ec_cor <- c()
# for(index_ec_cor in 1:length(ec_teams))
# {
#   for(index_ec_cor_cols in 1:ec_totalrounds)
#   {
#     index_ec_cor  <- row.names(ec_coawarded_h) == ec_teams[index_ec_cor]
#     form_ec_cor <- ec_coawarded_h[index_ec_cor]
#     deleted_form_ec_cor <- form_ec_cor[!form_ec_cor[] == ""]
#     l6_form_ec_cor <- deleted_form_ec_cor #tail(deleted_form_ec_cor,ec_last_n_games)
#     l6_form_ec_cor <- as.numeric(l6_form_ec_cor)
#     suml6_ec_cor[index_ec_cor] <- sum(l6_form_ec_cor)
#     suml6_ec_cor[index_ec_cor] <- paste(suml6_ec_cor[index_ec_cor],sep = "")
#     avg_ec_cor[index_ec_cor] <- mean(l6_form_ec_cor)
#     avg_ec_cor[index_ec_cor] <- paste(avg_ec_cor[index_ec_cor],sep = "")
#     sd_ec_one_cor[index_ec_cor] <- sd(l6_form_ec_cor)
#     sd_ec_one_cor[index_ec_cor] <- paste(sd_ec_one_cor[index_ec_cor],sep = "")
#     sum_ec_fivemore_cor[index_ec_cor] <- length(which(l6_form_ec_cor >= 5))
#     sum_ec_fivemore_cor[index_ec_cor] <- paste(sum_ec_fivemore_cor[index_ec_cor],sep = "")
#     sum_ec_fourless_cor[index_ec_cor] <- length(which(l6_form_ec_cor <= 4))
#     sum_ec_fourless_cor[index_ec_cor] <- paste(sum_ec_fourless_cor[index_ec_cor],sep = "")
#     #l6_form_ec_cor <- as.character(l6_form_ec_cor)
#     #l6_form_ec_cor_flattened <- stri_paste(l6_form_ec_cor,collapse = '')
#     #l6_form_ec_corsplitted <- as.numeric(strsplit(as.character(l6_form_ec_cor_flattened),"")[[1]])
#     final_ec_cor[index_ec_cor,index_ec_cor_cols] <- l6_form_ec_cor[index_ec_cor_cols]
#   }
# }
#
#
# final_ec_cor[is.na(final_ec_cor)] <- ""
# ec_coawardedmatrix <- cbind(ec_teams,final_ec_cor,suml6_ec_cor,avg_ec_cor,sd_ec_one_cor,sum_ec_fivemore_cor,sum_ec_fourless_cor)
# #############################################################################################################################################
# #f1
# final_f1_cor <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_cor <- c()
# avg_f1_cor <- c()
# sd_f1_one_cor <- c()
# sum_f1_fivemore_cor <- c()
# sum_f1_fourless_cor <- c()
# l6_form_f1_corsplitted <- c()
# form_f1_cor <- c()
# for(index_f1_cor in 1:length(f1_teams))
# {
#   for(index_f1_cor_cols in 1:f1_totalrounds)
#   {
#     index_f1_cor  <- row.names(f1_coawarded_h) == f1_teams[index_f1_cor]
#     form_f1_cor <- f1_coawarded_h[index_f1_cor]
#     deleted_form_f1_cor <- form_f1_cor[!form_f1_cor[] == ""]
#     l6_form_f1_cor <- deleted_form_f1_cor #tail(deleted_form_f1_cor,f1_last_n_games)
#     l6_form_f1_cor <- as.numeric(l6_form_f1_cor)
#     suml6_f1_cor[index_f1_cor] <- sum(l6_form_f1_cor)
#     suml6_f1_cor[index_f1_cor] <- paste(suml6_f1_cor[index_f1_cor],sep = "")
#     avg_f1_cor[index_f1_cor] <- mean(l6_form_f1_cor)
#     avg_f1_cor[index_f1_cor] <- paste(avg_f1_cor[index_f1_cor],sep = "")
#     sd_f1_one_cor[index_f1_cor] <- sd(l6_form_f1_cor)
#     sd_f1_one_cor[index_f1_cor] <- paste(sd_f1_one_cor[index_f1_cor],sep = "")
#     sum_f1_fivemore_cor[index_f1_cor] <- length(which(l6_form_f1_cor >= 5))
#     sum_f1_fivemore_cor[index_f1_cor] <- paste(sum_f1_fivemore_cor[index_f1_cor],sep = "")
#     sum_f1_fourless_cor[index_f1_cor] <- length(which(l6_form_f1_cor <= 4))
#     sum_f1_fourless_cor[index_f1_cor] <- paste(sum_f1_fourless_cor[index_f1_cor],sep = "")
#     #l6_form_f1_cor <- as.character(l6_form_f1_cor)
#     #l6_form_f1_cor_flattened <- stri_paste(l6_form_f1_cor,collapse = '')
#     #l6_form_f1_corsplitted <- as.numeric(strsplit(as.character(l6_form_f1_cor_flattened),"")[[1]])
#     final_f1_cor[index_f1_cor,index_f1_cor_cols] <- l6_form_f1_cor[index_f1_cor_cols]
#   }
# }
#
#
# final_f1_cor[is.na(final_f1_cor)] <- ""
# f1_coawardedmatrix <- cbind(f1_teams,final_f1_cor,suml6_f1_cor,avg_f1_cor,sd_f1_one_cor,sum_f1_fivemore_cor,sum_f1_fourless_cor)
# #############################################################################################################################################
# #f2
# final_f2_cor <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_cor <- c()
# avg_f2_cor <- c()
# sd_f2_one_cor <- c()
# sum_f2_fivemore_cor <- c()
# sum_f2_fourless_cor <- c()
# l6_form_f2_corsplitted <- c()
# form_f2_cor <- c()
# for(index_f2_cor in 1:length(f2_teams))
# {
#   for(index_f2_cor_cols in 1:f2_totalrounds)
#   {
#     index_f2_cor  <- row.names(f2_coawarded_h) == f2_teams[index_f2_cor]
#     form_f2_cor <- f2_coawarded_h[index_f2_cor]
#     deleted_form_f2_cor <- form_f2_cor[!form_f2_cor[] == ""]
#     l6_form_f2_cor <- deleted_form_f2_cor #tail(deleted_form_f2_cor,f2_last_n_games)
#     l6_form_f2_cor <- as.numeric(l6_form_f2_cor)
#     suml6_f2_cor[index_f2_cor] <- sum(l6_form_f2_cor)
#     suml6_f2_cor[index_f2_cor] <- paste(suml6_f2_cor[index_f2_cor],sep = "")
#     avg_f2_cor[index_f2_cor] <- mean(l6_form_f2_cor)
#     avg_f2_cor[index_f2_cor] <- paste(avg_f2_cor[index_f2_cor],sep = "")
#     sd_f2_one_cor[index_f2_cor] <- sd(l6_form_f2_cor)
#     sd_f2_one_cor[index_f2_cor] <- paste(sd_f2_one_cor[index_f2_cor],sep = "")
#     sum_f2_fivemore_cor[index_f2_cor] <- length(which(l6_form_f2_cor >= 5))
#     sum_f2_fivemore_cor[index_f2_cor] <- paste(sum_f2_fivemore_cor[index_f2_cor],sep = "")
#     sum_f2_fourless_cor[index_f2_cor] <- length(which(l6_form_f2_cor <= 4))
#     sum_f2_fourless_cor[index_f2_cor] <- paste(sum_f2_fourless_cor[index_f2_cor],sep = "")
#     #l6_form_f2_cor <- as.character(l6_form_f2_cor)
#     #l6_form_f2_cor_flattened <- stri_paste(l6_form_f2_cor,collapse = '')
#     #l6_form_f2_corsplitted <- as.numeric(strsplit(as.character(l6_form_f2_cor_flattened),"")[[1]])
#     final_f2_cor[index_f2_cor,index_f2_cor_cols] <- l6_form_f2_cor[index_f2_cor_cols]
#   }
# }
#
#
# final_f2_cor[is.na(final_f2_cor)] <- ""
# f2_coawardedmatrix <- cbind(f2_teams,final_f2_cor,suml6_f2_cor,avg_f2_cor,sd_f2_one_cor,sum_f2_fivemore_cor,sum_f2_fourless_cor)
# #############################################################################################################################################
# #g1
# final_g1_cor <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_cor <- c()
# avg_g1_cor <- c()
# sd_g1_one_cor <- c()
# sum_g1_fivemore_cor <- c()
# sum_g1_fourless_cor <- c()
# l6_form_g1_corsplitted <- c()
# form_g1_cor <- c()
# for(index_g1_cor in 1:length(g1_teams))
# {
#   for(index_g1_cor_cols in 1:g1_totalrounds)
#   {
#     index_g1_cor  <- row.names(g1_coawarded_h) == g1_teams[index_g1_cor]
#     form_g1_cor <- g1_coawarded_h[index_g1_cor]
#     deleted_form_g1_cor <- form_g1_cor[!form_g1_cor[] == ""]
#     l6_form_g1_cor <- deleted_form_g1_cor #tail(deleted_form_g1_cor,g1_last_n_games)
#     l6_form_g1_cor <- as.numeric(l6_form_g1_cor)
#     suml6_g1_cor[index_g1_cor] <- sum(l6_form_g1_cor)
#     suml6_g1_cor[index_g1_cor] <- paste(suml6_g1_cor[index_g1_cor],sep = "")
#     avg_g1_cor[index_g1_cor] <- mean(l6_form_g1_cor)
#     avg_g1_cor[index_g1_cor] <- paste(avg_g1_cor[index_g1_cor],sep = "")
#     sd_g1_one_cor[index_g1_cor] <- sd(l6_form_g1_cor)
#     sd_g1_one_cor[index_g1_cor] <- paste(sd_g1_one_cor[index_g1_cor],sep = "")
#     sum_g1_fivemore_cor[index_g1_cor] <- length(which(l6_form_g1_cor >= 5))
#     sum_g1_fivemore_cor[index_g1_cor] <- paste(sum_g1_fivemore_cor[index_g1_cor],sep = "")
#     sum_g1_fourless_cor[index_g1_cor] <- length(which(l6_form_g1_cor <= 4))
#     sum_g1_fourless_cor[index_g1_cor] <- paste(sum_g1_fourless_cor[index_g1_cor],sep = "")
#     #l6_form_g1_cor <- as.character(l6_form_g1_cor)
#     #l6_form_g1_cor_flattened <- stri_paste(l6_form_g1_cor,collapse = '')
#     #l6_form_g1_corsplitted <- as.numeric(strsplit(as.character(l6_form_g1_cor_flattened),"")[[1]])
#     final_g1_cor[index_g1_cor,index_g1_cor_cols] <- l6_form_g1_cor[index_g1_cor_cols]
#   }
# }
#
#
# final_g1_cor[is.na(final_g1_cor)] <- ""
# g1_coawardedmatrix <- cbind(g1_teams,final_g1_cor,suml6_g1_cor,avg_g1_cor,sd_g1_one_cor,sum_g1_fivemore_cor,sum_g1_fourless_cor)
# #############################################################################################################################################
# #i1
# final_i1_cor <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_cor <- c()
# avg_i1_cor <- c()
# sd_i1_one_cor <- c()
# sum_i1_fivemore_cor <- c()
# sum_i1_fourless_cor <- c()
# l6_form_i1_corsplitted <- c()
# form_i1_cor <- c()
# for(index_i1_cor in 1:length(i1_teams))
# {
#   for(index_i1_cor_cols in 1:i1_totalrounds)
#   {
#     index_i1_cor  <- row.names(i1_coawarded_h) == i1_teams[index_i1_cor]
#     form_i1_cor <- i1_coawarded_h[index_i1_cor]
#     deleted_form_i1_cor <- form_i1_cor[!form_i1_cor[] == ""]
#     l6_form_i1_cor <- deleted_form_i1_cor #tail(deleted_form_i1_cor,i1_last_n_games)
#     l6_form_i1_cor <- as.numeric(l6_form_i1_cor)
#     suml6_i1_cor[index_i1_cor] <- sum(l6_form_i1_cor)
#     suml6_i1_cor[index_i1_cor] <- paste(suml6_i1_cor[index_i1_cor],sep = "")
#     avg_i1_cor[index_i1_cor] <- mean(l6_form_i1_cor)
#     avg_i1_cor[index_i1_cor] <- paste(avg_i1_cor[index_i1_cor],sep = "")
#     sd_i1_one_cor[index_i1_cor] <- sd(l6_form_i1_cor)
#     sd_i1_one_cor[index_i1_cor] <- paste(sd_i1_one_cor[index_i1_cor],sep = "")
#     sum_i1_fivemore_cor[index_i1_cor] <- length(which(l6_form_i1_cor >= 5))
#     sum_i1_fivemore_cor[index_i1_cor] <- paste(sum_i1_fivemore_cor[index_i1_cor],sep = "")
#     sum_i1_fourless_cor[index_i1_cor] <- length(which(l6_form_i1_cor <= 4))
#     sum_i1_fourless_cor[index_i1_cor] <- paste(sum_i1_fourless_cor[index_i1_cor],sep = "")
#     #l6_form_i1_cor <- as.character(l6_form_i1_cor)
#     #l6_form_i1_cor_flattened <- stri_paste(l6_form_i1_cor,collapse = '')
#     #l6_form_i1_corsplitted <- as.numeric(strsplit(as.character(l6_form_i1_cor_flattened),"")[[1]])
#     final_i1_cor[index_i1_cor,index_i1_cor_cols] <- l6_form_i1_cor[index_i1_cor_cols]
#   }
# }
#
#
# final_i1_cor[is.na(final_i1_cor)] <- ""
# i1_coawardedmatrix <- cbind(i1_teams,final_i1_cor,suml6_i1_cor,avg_i1_cor,sd_i1_one_cor,sum_i1_fivemore_cor,sum_i1_fourless_cor)
# #############################################################################################################################################
# #i2
# final_i2_cor <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_cor <- c()
# avg_i2_cor <- c()
# sd_i2_one_cor <- c()
# sum_i2_fivemore_cor <- c()
# sum_i2_fourless_cor <- c()
# l6_form_i2_corsplitted <- c()
# form_i2_cor <- c()
# for(index_i2_cor in 1:length(i2_teams))
# {
#   for(index_i2_cor_cols in 1:i2_totalrounds)
#   {
#     index_i2_cor  <- row.names(i2_coawarded_h) == i2_teams[index_i2_cor]
#     form_i2_cor <- i2_coawarded_h[index_i2_cor]
#     deleted_form_i2_cor <- form_i2_cor[!form_i2_cor[] == ""]
#     l6_form_i2_cor <- deleted_form_i2_cor #tail(deleted_form_i2_cor,i2_last_n_games)
#     l6_form_i2_cor <- as.numeric(l6_form_i2_cor)
#     suml6_i2_cor[index_i2_cor] <- sum(l6_form_i2_cor)
#     suml6_i2_cor[index_i2_cor] <- paste(suml6_i2_cor[index_i2_cor],sep = "")
#     avg_i2_cor[index_i2_cor] <- mean(l6_form_i2_cor)
#     avg_i2_cor[index_i2_cor] <- paste(avg_i2_cor[index_i2_cor],sep = "")
#     sd_i2_one_cor[index_i2_cor] <- sd(l6_form_i2_cor)
#     sd_i2_one_cor[index_i2_cor] <- paste(sd_i2_one_cor[index_i2_cor],sep = "")
#     sum_i2_fivemore_cor[index_i2_cor] <- length(which(l6_form_i2_cor >= 5))
#     sum_i2_fivemore_cor[index_i2_cor] <- paste(sum_i2_fivemore_cor[index_i2_cor],sep = "")
#     sum_i2_fourless_cor[index_i2_cor] <- length(which(l6_form_i2_cor <= 4))
#     sum_i2_fourless_cor[index_i2_cor] <- paste(sum_i2_fourless_cor[index_i2_cor],sep = "")
#     #l6_form_i2_cor <- as.character(l6_form_i2_cor)
#     #l6_form_i2_cor_flattened <- stri_paste(l6_form_i2_cor,collapse = '')
#     #l6_form_i2_corsplitted <- as.numeric(strsplit(as.character(l6_form_i2_cor_flattened),"")[[1]])
#     final_i2_cor[index_i2_cor,index_i2_cor_cols] <- l6_form_i2_cor[index_i2_cor_cols]
#   }
# }
#
#
# final_i2_cor[is.na(final_i2_cor)] <- ""
# i2_coawardedmatrix <- cbind(i2_teams,final_i2_cor,suml6_i2_cor,avg_i2_cor,sd_i2_one_cor,sum_i2_fivemore_cor,sum_i2_fourless_cor)
# #############################################################################################################################################
# #n1
# final_n1_cor <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_cor <- c()
# avg_n1_cor <- c()
# sd_n1_one_cor <- c()
# sum_n1_fivemore_cor <- c()
# sum_n1_fourless_cor <- c()
# l6_form_n1_corsplitted <- c()
# form_n1_cor <- c()
# for(index_n1_cor in 1:length(n1_teams))
# {
#   for(index_n1_cor_cols in 1:n1_totalrounds)
#   {
#     index_n1_cor  <- row.names(n1_coawarded_h) == n1_teams[index_n1_cor]
#     form_n1_cor <- n1_coawarded_h[index_n1_cor]
#     deleted_form_n1_cor <- form_n1_cor[!form_n1_cor[] == ""]
#     l6_form_n1_cor <- deleted_form_n1_cor #tail(deleted_form_n1_cor,n1_last_n_games)
#     l6_form_n1_cor <- as.numeric(l6_form_n1_cor)
#     suml6_n1_cor[index_n1_cor] <- sum(l6_form_n1_cor)
#     suml6_n1_cor[index_n1_cor] <- paste(suml6_n1_cor[index_n1_cor],sep = "")
#     avg_n1_cor[index_n1_cor] <- mean(l6_form_n1_cor)
#     avg_n1_cor[index_n1_cor] <- paste(avg_n1_cor[index_n1_cor],sep = "")
#     sd_n1_one_cor[index_n1_cor] <- sd(l6_form_n1_cor)
#     sd_n1_one_cor[index_n1_cor] <- paste(sd_n1_one_cor[index_n1_cor],sep = "")
#     sum_n1_fivemore_cor[index_n1_cor] <- length(which(l6_form_n1_cor >= 5))
#     sum_n1_fivemore_cor[index_n1_cor] <- paste(sum_n1_fivemore_cor[index_n1_cor],sep = "")
#     sum_n1_fourless_cor[index_n1_cor] <- length(which(l6_form_n1_cor <= 4))
#     sum_n1_fourless_cor[index_n1_cor] <- paste(sum_n1_fourless_cor[index_n1_cor],sep = "")
#     #l6_form_n1_cor <- as.character(l6_form_n1_cor)
#     #l6_form_n1_cor_flattened <- stri_paste(l6_form_n1_cor,collapse = '')
#     #l6_form_n1_corsplitted <- as.numeric(strsplit(as.character(l6_form_n1_cor_flattened),"")[[1]])
#     final_n1_cor[index_n1_cor,index_n1_cor_cols] <- l6_form_n1_cor[index_n1_cor_cols]
#   }
# }
#
#
# final_n1_cor[is.na(final_n1_cor)] <- ""
# n1_coawardedmatrix <- cbind(n1_teams,final_n1_cor,suml6_n1_cor,avg_n1_cor,sd_n1_one_cor,sum_n1_fivemore_cor,sum_n1_fourless_cor)
# #############################################################################################################################################
# #p1
# final_p1_cor <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_cor <- c()
# avg_p1_cor <- c()
# sd_p1_one_cor <- c()
# sum_p1_fivemore_cor <- c()
# sum_p1_fourless_cor <- c()
# l6_form_p1_corsplitted <- c()
# form_p1_cor <- c()
# for(index_p1_cor in 1:length(p1_teams))
# {
#   for(index_p1_cor_cols in 1:p1_totalrounds)
#   {
#     index_p1_cor  <- row.names(p1_coawarded_h) == p1_teams[index_p1_cor]
#     form_p1_cor <- p1_coawarded_h[index_p1_cor]
#     deleted_form_p1_cor <- form_p1_cor[!form_p1_cor[] == ""]
#     l6_form_p1_cor <- deleted_form_p1_cor #tail(deleted_form_p1_cor,p1_last_n_games)
#     l6_form_p1_cor <- as.numeric(l6_form_p1_cor)
#     suml6_p1_cor[index_p1_cor] <- sum(l6_form_p1_cor)
#     suml6_p1_cor[index_p1_cor] <- paste(suml6_p1_cor[index_p1_cor],sep = "")
#     avg_p1_cor[index_p1_cor] <- mean(l6_form_p1_cor)
#     avg_p1_cor[index_p1_cor] <- paste(avg_p1_cor[index_p1_cor],sep = "")
#     sd_p1_one_cor[index_p1_cor] <- sd(l6_form_p1_cor)
#     sd_p1_one_cor[index_p1_cor] <- paste(sd_p1_one_cor[index_p1_cor],sep = "")
#     sum_p1_fivemore_cor[index_p1_cor] <- length(which(l6_form_p1_cor >= 5))
#     sum_p1_fivemore_cor[index_p1_cor] <- paste(sum_p1_fivemore_cor[index_p1_cor],sep = "")
#     sum_p1_fourless_cor[index_p1_cor] <- length(which(l6_form_p1_cor <= 4))
#     sum_p1_fourless_cor[index_p1_cor] <- paste(sum_p1_fourless_cor[index_p1_cor],sep = "")
#     #l6_form_p1_cor <- as.character(l6_form_p1_cor)
#     #l6_form_p1_cor_flattened <- stri_paste(l6_form_p1_cor,collapse = '')
#     #l6_form_p1_corsplitted <- as.numeric(strsplit(as.character(l6_form_p1_cor_flattened),"")[[1]])
#     final_p1_cor[index_p1_cor,index_p1_cor_cols] <- l6_form_p1_cor[index_p1_cor_cols]
#   }
# }
#
#
# final_p1_cor[is.na(final_p1_cor)] <- ""
# p1_coawardedmatrix <- cbind(p1_teams,final_p1_cor,suml6_p1_cor,avg_p1_cor,sd_p1_one_cor,sum_p1_fivemore_cor,sum_p1_fourless_cor)
# #############################################################################################################################################
# #sc0
# final_sc0_cor <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_cor <- c()
# avg_sc0_cor <- c()
# sd_sc0_one_cor <- c()
# sum_sc0_fivemore_cor <- c()
# sum_sc0_fourless_cor <- c()
# l6_form_sc0_corsplitted <- c()
# form_sc0_cor <- c()
# for(index_sc0_cor in 1:length(sc0_teams))
# {
#   for(index_sc0_cor_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_cor  <- row.names(sc0_coawarded_h) == sc0_teams[index_sc0_cor]
#     form_sc0_cor <- sc0_coawarded_h[index_sc0_cor]
#     deleted_form_sc0_cor <- form_sc0_cor[!form_sc0_cor[] == ""]
#     l6_form_sc0_cor <- deleted_form_sc0_cor #tail(deleted_form_sc0_cor,sc0_last_n_games)
#     l6_form_sc0_cor <- as.numeric(l6_form_sc0_cor)
#     suml6_sc0_cor[index_sc0_cor] <- sum(l6_form_sc0_cor)
#     suml6_sc0_cor[index_sc0_cor] <- paste(suml6_sc0_cor[index_sc0_cor],sep = "")
#     avg_sc0_cor[index_sc0_cor] <- mean(l6_form_sc0_cor)
#     avg_sc0_cor[index_sc0_cor] <- paste(avg_sc0_cor[index_sc0_cor],sep = "")
#     sd_sc0_one_cor[index_sc0_cor] <- sd(l6_form_sc0_cor)
#     sd_sc0_one_cor[index_sc0_cor] <- paste(sd_sc0_one_cor[index_sc0_cor],sep = "")
#     sum_sc0_fivemore_cor[index_sc0_cor] <- length(which(l6_form_sc0_cor >= 5))
#     sum_sc0_fivemore_cor[index_sc0_cor] <- paste(sum_sc0_fivemore_cor[index_sc0_cor],sep = "")
#     sum_sc0_fourless_cor[index_sc0_cor] <- length(which(l6_form_sc0_cor <= 4))
#     sum_sc0_fourless_cor[index_sc0_cor] <- paste(sum_sc0_fourless_cor[index_sc0_cor],sep = "")
#     #l6_form_sc0_cor <- as.character(l6_form_sc0_cor)
#     #l6_form_sc0_cor_flattened <- stri_paste(l6_form_sc0_cor,collapse = '')
#     #l6_form_sc0_corsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_cor_flattened),"")[[1]])
#     final_sc0_cor[index_sc0_cor,index_sc0_cor_cols] <- l6_form_sc0_cor[index_sc0_cor_cols]
#   }
# }
#
#
# final_sc0_cor[is.na(final_sc0_cor)] <- ""
# sc0_coawardedmatrix <- cbind(sc0_teams,final_sc0_cor,suml6_sc0_cor,avg_sc0_cor,sd_sc0_one_cor,sum_sc0_fivemore_cor,sum_sc0_fourless_cor)
# #############################################################################################################################################
# #sc1
# final_sc1_cor <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_cor <- c()
# avg_sc1_cor <- c()
# sd_sc1_one_cor <- c()
# sum_sc1_fivemore_cor <- c()
# sum_sc1_fourless_cor <- c()
# l6_form_sc1_corsplitted <- c()
# form_sc1_cor <- c()
# for(index_sc1_cor in 1:length(sc1_teams))
# {
#   for(index_sc1_cor_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_cor  <- row.names(sc1_coawarded_h) == sc1_teams[index_sc1_cor]
#     form_sc1_cor <- sc1_coawarded_h[index_sc1_cor]
#     deleted_form_sc1_cor <- form_sc1_cor[!form_sc1_cor[] == ""]
#     l6_form_sc1_cor <- deleted_form_sc1_cor #tail(deleted_form_sc1_cor,sc1_last_n_games)
#     l6_form_sc1_cor <- as.numeric(l6_form_sc1_cor)
#     suml6_sc1_cor[index_sc1_cor] <- sum(l6_form_sc1_cor)
#     suml6_sc1_cor[index_sc1_cor] <- paste(suml6_sc1_cor[index_sc1_cor],sep = "")
#     avg_sc1_cor[index_sc1_cor] <- mean(l6_form_sc1_cor)
#     avg_sc1_cor[index_sc1_cor] <- paste(avg_sc1_cor[index_sc1_cor],sep = "")
#     sd_sc1_one_cor[index_sc1_cor] <- sd(l6_form_sc1_cor)
#     sd_sc1_one_cor[index_sc1_cor] <- paste(sd_sc1_one_cor[index_sc1_cor],sep = "")
#     sum_sc1_fivemore_cor[index_sc1_cor] <- length(which(l6_form_sc1_cor >= 5))
#     sum_sc1_fivemore_cor[index_sc1_cor] <- paste(sum_sc1_fivemore_cor[index_sc1_cor],sep = "")
#     sum_sc1_fourless_cor[index_sc1_cor] <- length(which(l6_form_sc1_cor <= 4))
#     sum_sc1_fourless_cor[index_sc1_cor] <- paste(sum_sc1_fourless_cor[index_sc1_cor],sep = "")
#     #l6_form_sc1_cor <- as.character(l6_form_sc1_cor)
#     #l6_form_sc1_cor_flattened <- stri_paste(l6_form_sc1_cor,collapse = '')
#     #l6_form_sc1_corsplitted <- as.numeric(strsplit(as.character(l6_form_sc1_cor_flattened),"")[[1]])
#     final_sc1_cor[index_sc1_cor,index_sc1_cor_cols] <- l6_form_sc1_cor[index_sc1_cor_cols]
#   }
# }
#
#
# final_sc1_cor[is.na(final_sc1_cor)] <- ""
# sc1_coawardedmatrix <- cbind(sc1_teams,final_sc1_cor,suml6_sc1_cor,avg_sc1_cor,sd_sc1_one_cor,sum_sc1_fivemore_cor,sum_sc1_fourless_cor)
# #############################################################################################################################################
# #sc2
# final_sc2_cor <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_cor <- c()
# avg_sc2_cor <- c()
# sd_sc2_one_cor <- c()
# sum_sc2_fivemore_cor <- c()
# sum_sc2_fourless_cor <- c()
# l6_form_sc2_corsplitted <- c()
# form_sc2_cor <- c()
# for(index_sc2_cor in 1:length(sc2_teams))
# {
#   for(index_sc2_cor_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_cor  <- row.names(sc2_coawarded_h) == sc2_teams[index_sc2_cor]
#     form_sc2_cor <- sc2_coawarded_h[index_sc2_cor]
#     deleted_form_sc2_cor <- form_sc2_cor[!form_sc2_cor[] == ""]
#     l6_form_sc2_cor <- deleted_form_sc2_cor #tail(deleted_form_sc2_cor,sc2_last_n_games)
#     l6_form_sc2_cor <- as.numeric(l6_form_sc2_cor)
#     suml6_sc2_cor[index_sc2_cor] <- sum(l6_form_sc2_cor)
#     suml6_sc2_cor[index_sc2_cor] <- paste(suml6_sc2_cor[index_sc2_cor],sep = "")
#     avg_sc2_cor[index_sc2_cor] <- mean(l6_form_sc2_cor)
#     avg_sc2_cor[index_sc2_cor] <- paste(avg_sc2_cor[index_sc2_cor],sep = "")
#     sd_sc2_one_cor[index_sc2_cor] <- sd(l6_form_sc2_cor)
#     sd_sc2_one_cor[index_sc2_cor] <- paste(sd_sc2_one_cor[index_sc2_cor],sep = "")
#     sum_sc2_fivemore_cor[index_sc2_cor] <- length(which(l6_form_sc2_cor >= 5))
#     sum_sc2_fivemore_cor[index_sc2_cor] <- paste(sum_sc2_fivemore_cor[index_sc2_cor],sep = "")
#     sum_sc2_fourless_cor[index_sc2_cor] <- length(which(l6_form_sc2_cor <= 4))
#     sum_sc2_fourless_cor[index_sc2_cor] <- paste(sum_sc2_fourless_cor[index_sc2_cor],sep = "")
#     #l6_form_sc2_cor <- as.character(l6_form_sc2_cor)
#     #l6_form_sc2_cor_flattened <- stri_paste(l6_form_sc2_cor,collapse = '')
#     #l6_form_sc2_corsplitted <- as.numeric(strsplit(as.character(l6_form_sc2_cor_flattened),"")[[1]])
#     final_sc2_cor[index_sc2_cor,index_sc2_cor_cols] <- l6_form_sc2_cor[index_sc2_cor_cols]
#   }
# }
#
#
# final_sc2_cor[is.na(final_sc2_cor)] <- ""
# sc2_coawardedmatrix <- cbind(sc2_teams,final_sc2_cor,suml6_sc2_cor,avg_sc2_cor,sd_sc2_one_cor,sum_sc2_fivemore_cor,sum_sc2_fourless_cor)
# #############################################################################################################################################
# #sc3
# final_sc3_cor <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_cor <- c()
# avg_sc3_cor <- c()
# sd_sc3_one_cor <- c()
# sum_sc3_fivemore_cor <- c()
# sum_sc3_fourless_cor <- c()
# l6_form_sc3_corsplitted <- c()
# form_sc3_cor <- c()
# for(index_sc3_cor in 1:length(sc3_teams))
# {
#   for(index_sc3_cor_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_cor  <- row.names(sc3_coawarded_h) == sc3_teams[index_sc3_cor]
#     form_sc3_cor <- sc3_coawarded_h[index_sc3_cor]
#     deleted_form_sc3_cor <- form_sc3_cor[!form_sc3_cor[] == ""]
#     l6_form_sc3_cor <- deleted_form_sc3_cor #tail(deleted_form_sc3_cor,sc3_last_n_games)
#     l6_form_sc3_cor <- as.numeric(l6_form_sc3_cor)
#     suml6_sc3_cor[index_sc3_cor] <- sum(l6_form_sc3_cor)
#     suml6_sc3_cor[index_sc3_cor] <- paste(suml6_sc3_cor[index_sc3_cor],sep = "")
#     avg_sc3_cor[index_sc3_cor] <- mean(l6_form_sc3_cor)
#     avg_sc3_cor[index_sc3_cor] <- paste(avg_sc3_cor[index_sc3_cor],sep = "")
#     sd_sc3_one_cor[index_sc3_cor] <- sd(l6_form_sc3_cor)
#     sd_sc3_one_cor[index_sc3_cor] <- paste(sd_sc3_one_cor[index_sc3_cor],sep = "")
#     sum_sc3_fivemore_cor[index_sc3_cor] <- length(which(l6_form_sc3_cor >= 5))
#     sum_sc3_fivemore_cor[index_sc3_cor] <- paste(sum_sc3_fivemore_cor[index_sc3_cor],sep = "")
#     sum_sc3_fourless_cor[index_sc3_cor] <- length(which(l6_form_sc3_cor <= 4))
#     sum_sc3_fourless_cor[index_sc3_cor] <- paste(sum_sc3_fourless_cor[index_sc3_cor],sep = "")
#     #l6_form_sc3_cor <- as.character(l6_form_sc3_cor)
#     #l6_form_sc3_cor_flattened <- stri_paste(l6_form_sc3_cor,collapse = '')
#     #l6_form_sc3_corsplitted <- as.numeric(strsplit(as.character(l6_form_sc3_cor_flattened),"")[[1]])
#     final_sc3_cor[index_sc3_cor,index_sc3_cor_cols] <- l6_form_sc3_cor[index_sc3_cor_cols]
#   }
# }
#
#
# final_sc3_cor[is.na(final_sc3_cor)] <- ""
# sc3_coawardedmatrix <- cbind(sc3_teams,final_sc3_cor,suml6_sc3_cor,avg_sc3_cor,sd_sc3_one_cor,sum_sc3_fivemore_cor,sum_sc3_fourless_cor)
# #############################################################################################################################################
# #sp1
# final_sp1_cor <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_cor <- c()
# avg_sp1_cor <- c()
# sd_sp1_one_cor <- c()
# sum_sp1_fivemore_cor <- c()
# sum_sp1_fourless_cor <- c()
# l6_form_sp1_corsplitted <- c()
# form_sp1_cor <- c()
# for(index_sp1_cor in 1:length(sp1_teams))
# {
#   for(index_sp1_cor_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_cor  <- row.names(sp1_coawarded_h) == sp1_teams[index_sp1_cor]
#     form_sp1_cor <- sp1_coawarded_h[index_sp1_cor]
#     deleted_form_sp1_cor <- form_sp1_cor[!form_sp1_cor[] == ""]
#     l6_form_sp1_cor <- deleted_form_sp1_cor #tail(deleted_form_sp1_cor,sp1_last_n_games)
#     l6_form_sp1_cor <- as.numeric(l6_form_sp1_cor)
#     suml6_sp1_cor[index_sp1_cor] <- sum(l6_form_sp1_cor)
#     suml6_sp1_cor[index_sp1_cor] <- paste(suml6_sp1_cor[index_sp1_cor],sep = "")
#     avg_sp1_cor[index_sp1_cor] <- mean(l6_form_sp1_cor)
#     avg_sp1_cor[index_sp1_cor] <- paste(avg_sp1_cor[index_sp1_cor],sep = "")
#     sd_sp1_one_cor[index_sp1_cor] <- sd(l6_form_sp1_cor)
#     sd_sp1_one_cor[index_sp1_cor] <- paste(sd_sp1_one_cor[index_sp1_cor],sep = "")
#     sum_sp1_fivemore_cor[index_sp1_cor] <- length(which(l6_form_sp1_cor >= 5))
#     sum_sp1_fivemore_cor[index_sp1_cor] <- paste(sum_sp1_fivemore_cor[index_sp1_cor],sep = "")
#     sum_sp1_fourless_cor[index_sp1_cor] <- length(which(l6_form_sp1_cor <= 4))
#     sum_sp1_fourless_cor[index_sp1_cor] <- paste(sum_sp1_fourless_cor[index_sp1_cor],sep = "")
#     #l6_form_sp1_cor <- as.character(l6_form_sp1_cor)
#     #l6_form_sp1_cor_flattened <- stri_paste(l6_form_sp1_cor,collapse = '')
#     #l6_form_sp1_corsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_cor_flattened),"")[[1]])
#     final_sp1_cor[index_sp1_cor,index_sp1_cor_cols] <- l6_form_sp1_cor[index_sp1_cor_cols]
#   }
# }
#
#
# final_sp1_cor[is.na(final_sp1_cor)] <- ""
# sp1_coawardedmatrix <- cbind(sp1_teams,final_sp1_cor,suml6_sp1_cor,avg_sp1_cor,sd_sp1_one_cor,sum_sp1_fivemore_cor,sum_sp1_fourless_cor)
# #############################################################################################################################################
# #sp2
# final_sp2_cor <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_cor <- c()
# avg_sp2_cor <- c()
# sd_sp2_one_cor <- c()
# sum_sp2_fivemore_cor <- c()
# sum_sp2_fourless_cor <- c()
# l6_form_sp2_corsplitted <- c()
# form_sp2_cor <- c()
# for(index_sp2_cor in 1:length(sp2_teams))
# {
#   for(index_sp2_cor_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_cor  <- row.names(sp2_coawarded_h) == sp2_teams[index_sp2_cor]
#     form_sp2_cor <- sp2_coawarded_h[index_sp2_cor]
#     deleted_form_sp2_cor <- form_sp2_cor[!form_sp2_cor[] == ""]
#     l6_form_sp2_cor <- deleted_form_sp2_cor #tail(deleted_form_sp2_cor,sp2_last_n_games)
#     l6_form_sp2_cor <- as.numeric(l6_form_sp2_cor)
#     suml6_sp2_cor[index_sp2_cor] <- sum(l6_form_sp2_cor)
#     suml6_sp2_cor[index_sp2_cor] <- paste(suml6_sp2_cor[index_sp2_cor],sep = "")
#     avg_sp2_cor[index_sp2_cor] <- mean(l6_form_sp2_cor)
#     avg_sp2_cor[index_sp2_cor] <- paste(avg_sp2_cor[index_sp2_cor],sep = "")
#     sd_sp2_one_cor[index_sp2_cor] <- sd(l6_form_sp2_cor)
#     sd_sp2_one_cor[index_sp2_cor] <- paste(sd_sp2_one_cor[index_sp2_cor],sep = "")
#     sum_sp2_fivemore_cor[index_sp2_cor] <- length(which(l6_form_sp2_cor >= 5))
#     sum_sp2_fivemore_cor[index_sp2_cor] <- paste(sum_sp2_fivemore_cor[index_sp2_cor],sep = "")
#     sum_sp2_fourless_cor[index_sp2_cor] <- length(which(l6_form_sp2_cor <= 4))
#     sum_sp2_fourless_cor[index_sp2_cor] <- paste(sum_sp2_fourless_cor[index_sp2_cor],sep = "")
#     #l6_form_sp2_cor <- as.character(l6_form_sp2_cor)
#     #l6_form_sp2_cor_flattened <- stri_paste(l6_form_sp2_cor,collapse = '')
#     #l6_form_sp2_corsplitted <- as.numeric(strsplit(as.character(l6_form_sp2_cor_flattened),"")[[1]])
#     final_sp2_cor[index_sp2_cor,index_sp2_cor_cols] <- l6_form_sp2_cor[index_sp2_cor_cols]
#   }
# }
#
#
# final_sp2_cor[is.na(final_sp2_cor)] <- ""
# sp2_coawardedmatrix <- cbind(sp2_teams,final_sp2_cor,suml6_sp2_cor,avg_sp2_cor,sd_sp2_one_cor,sum_sp2_fivemore_cor,sum_sp2_fourless_cor)
# #############################################################################################################################################
# #t1
# final_t1_cor <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_cor <- c()
# avg_t1_cor <- c()
# sd_t1_one_cor <- c()
# sum_t1_fivemore_cor <- c()
# sum_t1_fourless_cor <- c()
# l6_form_t1_corsplitted <- c()
# form_t1_cor <- c()
# for(index_t1_cor in 1:length(t1_teams))
# {
#   for(index_t1_cor_cols in 1:t1_totalrounds)
#   {
#     index_t1_cor  <- row.names(t1_coawarded_h) == t1_teams[index_t1_cor]
#     form_t1_cor <- t1_coawarded_h[index_t1_cor]
#     deleted_form_t1_cor <- form_t1_cor[!form_t1_cor[] == ""]
#     l6_form_t1_cor <- deleted_form_t1_cor #tail(deleted_form_t1_cor,t1_last_n_games)
#     l6_form_t1_cor <- as.numeric(l6_form_t1_cor)
#     suml6_t1_cor[index_t1_cor] <- sum(l6_form_t1_cor)
#     suml6_t1_cor[index_t1_cor] <- paste(suml6_t1_cor[index_t1_cor],sep = "")
#     avg_t1_cor[index_t1_cor] <- mean(l6_form_t1_cor)
#     avg_t1_cor[index_t1_cor] <- paste(avg_t1_cor[index_t1_cor],sep = "")
#     sd_t1_one_cor[index_t1_cor] <- sd(l6_form_t1_cor)
#     sd_t1_one_cor[index_t1_cor] <- paste(sd_t1_one_cor[index_t1_cor],sep = "")
#     sum_t1_fivemore_cor[index_t1_cor] <- length(which(l6_form_t1_cor >= 5))
#     sum_t1_fivemore_cor[index_t1_cor] <- paste(sum_t1_fivemore_cor[index_t1_cor],sep = "")
#     sum_t1_fourless_cor[index_t1_cor] <- length(which(l6_form_t1_cor <= 4))
#     sum_t1_fourless_cor[index_t1_cor] <- paste(sum_t1_fourless_cor[index_t1_cor],sep = "")
#     #l6_form_t1_cor <- as.character(l6_form_t1_cor)
#     #l6_form_t1_cor_flattened <- stri_paste(l6_form_t1_cor,collapse = '')
#     #l6_form_t1_corsplitted <- as.numeric(strsplit(as.character(l6_form_t1_cor_flattened),"")[[1]])
#     final_t1_cor[index_t1_cor,index_t1_cor_cols] <- l6_form_t1_cor[index_t1_cor_cols]
#   }
# }
#
#
# final_t1_cor[is.na(final_t1_cor)] <- ""
# t1_coawardedmatrix <- cbind(t1_teams,final_t1_cor,suml6_t1_cor,avg_t1_cor,sd_t1_one_cor,sum_t1_fivemore_cor,sum_t1_fourless_cor)
#############################################################################################################################################






























