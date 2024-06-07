library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
#mls
final_mls_wm <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
suml6_mls_wm <- c()
suml6_mls_wm_negone <- c()
suml6_mls_wm_negtwo <- c()
suml6_mls_wm_zero <- c()
suml6_mls_wm_posone <- c()
suml6_mls_wm_postwo <- c()
l6_form_mls_wmsplitted <- c()
form_mls_wm <- c()
for(index_mls_wm in 1:length(mls_teams))
{
  for(index_mls_wm_cols in 1:mls_totalrounds)
  {
    index_mls_wm  <- row.names(mls_winmargin_h) == mls_teams[index_mls_wm]
    form_mls_wm <- mls_winmargin_h[index_mls_wm ]
    deleted_form_mls_wm <- form_mls_wm[!form_mls_wm[] == ""]
    l6_form_mls_wm <- tail(deleted_form_mls_wm,mls_last_n_games)
    l6_form_mls_wm <- as.numeric(l6_form_mls_wm)
    suml6_mls_wm[index_mls_wm] <- sum(l6_form_mls_wm)
    suml6_mls_wm[index_mls_wm] <- paste(suml6_mls_wm[index_mls_wm],sep = "")
    suml6_mls_wm_negone[index_mls_wm] <- length(which(l6_form_mls_wm == -1))
    suml6_mls_wm_negone[index_mls_wm] <- paste(suml6_mls_wm_negone[index_mls_wm],sep = "")
    suml6_mls_wm_negtwo[index_mls_wm] <- length(which(l6_form_mls_wm <= -2))
    suml6_mls_wm_negtwo[index_mls_wm] <- paste(suml6_mls_wm_negtwo[index_mls_wm],sep = "")
    suml6_mls_wm_zero[index_mls_wm] <- length(which(l6_form_mls_wm == 0))
    suml6_mls_wm_zero[index_mls_wm] <- paste(suml6_mls_wm_zero[index_mls_wm],sep = "")
    suml6_mls_wm_posone[index_mls_wm] <- length(which(l6_form_mls_wm == 1))
    suml6_mls_wm_posone[index_mls_wm] <- paste(suml6_mls_wm_posone[index_mls_wm],sep = "")
    suml6_mls_wm_postwo[index_mls_wm] <- length(which(l6_form_mls_wm == 2))
    suml6_mls_wm_postwo[index_mls_wm] <- paste(suml6_mls_wm_postwo[index_mls_wm],sep = "")
    l6_form_mls_wm <- as.character(l6_form_mls_wm)
    l6_form_mls_wm_flattened <- stri_paste(l6_form_mls_wm,collapse = ',')
    l6_form_mls_wmsplitted <- (strsplit(as.character(l6_form_mls_wm_flattened),",")[[1]])
    final_mls_wm[index_mls_wm,index_mls_wm_cols] <- l6_form_mls_wmsplitted[index_mls_wm_cols]
  }
}
final_mls_wm[is.na(final_mls_wm)] <- ""
mls_winmarginmatrix <- cbind(mls_teams,final_mls_wm,suml6_mls_wm,suml6_mls_wm_negtwo,suml6_mls_wm_negone,suml6_mls_wm_zero,suml6_mls_wm_posone,suml6_mls_wm_postwo)
##########################################################################################################################################################
#arg
final_arg_wm <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
suml6_arg_wm <- c()
suml6_arg_wm_negone <- c()
suml6_arg_wm_negtwo <- c()
suml6_arg_wm_zero <- c()
suml6_arg_wm_posone <- c()
suml6_arg_wm_postwo <- c()
l6_form_arg_wmsplitted <- c()
form_arg_wm <- c()
for(index_arg_wm in 1:length(arg_teams))
{
  for(index_arg_wm_cols in 1:arg_totalrounds)
  {
    index_arg_wm  <- row.names(arg_winmargin_h) == arg_teams[index_arg_wm]
    form_arg_wm <- arg_winmargin_h[index_arg_wm ]
    deleted_form_arg_wm <- form_arg_wm[!form_arg_wm[] == ""]
    l6_form_arg_wm <- tail(deleted_form_arg_wm,arg_last_n_games)
    l6_form_arg_wm <- as.numeric(l6_form_arg_wm)
    suml6_arg_wm[index_arg_wm] <- sum(l6_form_arg_wm)
    suml6_arg_wm[index_arg_wm] <- paste(suml6_arg_wm[index_arg_wm],sep = "")
    suml6_arg_wm_negone[index_arg_wm] <- length(which(l6_form_arg_wm == -1))
    suml6_arg_wm_negone[index_arg_wm] <- paste(suml6_arg_wm_negone[index_arg_wm],sep = "")
    suml6_arg_wm_negtwo[index_arg_wm] <- length(which(l6_form_arg_wm <= -2))
    suml6_arg_wm_negtwo[index_arg_wm] <- paste(suml6_arg_wm_negtwo[index_arg_wm],sep = "")
    suml6_arg_wm_zero[index_arg_wm] <- length(which(l6_form_arg_wm == 0))
    suml6_arg_wm_zero[index_arg_wm] <- paste(suml6_arg_wm_zero[index_arg_wm],sep = "")
    suml6_arg_wm_posone[index_arg_wm] <- length(which(l6_form_arg_wm == 1))
    suml6_arg_wm_posone[index_arg_wm] <- paste(suml6_arg_wm_posone[index_arg_wm],sep = "")
    suml6_arg_wm_postwo[index_arg_wm] <- length(which(l6_form_arg_wm == 2))
    suml6_arg_wm_postwo[index_arg_wm] <- paste(suml6_arg_wm_postwo[index_arg_wm],sep = "")
    l6_form_arg_wm <- as.character(l6_form_arg_wm)
    l6_form_arg_wm_flattened <- stri_paste(l6_form_arg_wm,collapse = ',')
    l6_form_arg_wmsplitted <- (strsplit(as.character(l6_form_arg_wm_flattened),",")[[1]])
    final_arg_wm[index_arg_wm,index_arg_wm_cols] <- l6_form_arg_wmsplitted[index_arg_wm_cols]
  }
}
final_arg_wm[is.na(final_arg_wm)] <- ""
arg_winmarginmatrix <- cbind(arg_teams,final_arg_wm,suml6_arg_wm,suml6_arg_wm_negtwo,suml6_arg_wm_negone,suml6_arg_wm_zero,suml6_arg_wm_posone,suml6_arg_wm_postwo)
##########################################################################################################################################################
#d2
# final_d2_wm <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# suml6_d2_wm <- c()
# suml6_d2_wm_negone <- c()
# suml6_d2_wm_negtwo <- c()
# suml6_d2_wm_zero <- c()
# suml6_d2_wm_posone <- c()
# suml6_d2_wm_postwo <- c()
# l6_form_d2_wmsplitted <- c()
# form_d2_wm <- c()
# for(index_d2_wm in 1:length(d2_teams))
# {
#   for(index_d2_wm_cols in 1:d2_totalrounds)
#   {
#     index_d2_wm  <- row.names(d2_winmargin_h) == d2_teams[index_d2_wm]
#     form_d2_wm <- d2_winmargin_h[index_d2_wm ]
#     deleted_form_d2_wm <- form_d2_wm[!form_d2_wm[] == ""]
#     l6_form_d2_wm <- tail(deleted_form_d2_wm,d2_last_n_games)
#     l6_form_d2_wm <- as.numeric(l6_form_d2_wm)
#     suml6_d2_wm[index_d2_wm] <- sum(l6_form_d2_wm)
#     suml6_d2_wm[index_d2_wm] <- paste(suml6_d2_wm[index_d2_wm],sep = "")
#     suml6_d2_wm_negone[index_d2_wm] <- length(which(l6_form_d2_wm == -1))
#     suml6_d2_wm_negone[index_d2_wm] <- paste(suml6_d2_wm_negone[index_d2_wm],sep = "")
#     suml6_d2_wm_negtwo[index_d2_wm] <- length(which(l6_form_d2_wm <= -2))
#     suml6_d2_wm_negtwo[index_d2_wm] <- paste(suml6_d2_wm_negtwo[index_d2_wm],sep = "")
#     suml6_d2_wm_zero[index_d2_wm] <- length(which(l6_form_d2_wm == 0))
#     suml6_d2_wm_zero[index_d2_wm] <- paste(suml6_d2_wm_zero[index_d2_wm],sep = "")
#     suml6_d2_wm_posone[index_d2_wm] <- length(which(l6_form_d2_wm == 1))
#     suml6_d2_wm_posone[index_d2_wm] <- paste(suml6_d2_wm_posone[index_d2_wm],sep = "")
#     suml6_d2_wm_postwo[index_d2_wm] <- length(which(l6_form_d2_wm == 2))
#     suml6_d2_wm_postwo[index_d2_wm] <- paste(suml6_d2_wm_postwo[index_d2_wm],sep = "")
#     l6_form_d2_wm <- as.character(l6_form_d2_wm)
#     l6_form_d2_wm_flattened <- stri_paste(l6_form_d2_wm,collapse = ',')
#     l6_form_d2_wmsplitted <- (strsplit(as.character(l6_form_d2_wm_flattened),",")[[1]])
#     final_d2_wm[index_d2_wm,index_d2_wm_cols] <- l6_form_d2_wmsplitted[index_d2_wm_cols]
#   }
# }
# final_d2_wm[is.na(final_d2_wm)] <- ""
# d2_winmarginmatrix <- cbind(d2_teams,final_d2_wm,suml6_d2_wm,suml6_d2_wm_negtwo,suml6_d2_wm_negone,suml6_d2_wm_zero,suml6_d2_wm_posone,suml6_d2_wm_postwo)
# ############################################################################################################################################################
# #e0
# final_e0_wm <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# suml6_e0_wm <- c()
# suml6_e0_wm_negone <- c()
# suml6_e0_wm_negtwo <- c()
# suml6_e0_wm_zero <- c()
# suml6_e0_wm_posone <- c()
# suml6_e0_wm_postwo <- c()
# l6_form_e0_wmsplitted <- c()
# form_e0_wm <- c()
# for(index_e0_wm in 1:length(e0_teams))
# {
#   for(index_e0_wm_cols in 1:e0_totalrounds)
#   {
#     index_e0_wm  <- row.names(e0_winmargin_h) == e0_teams[index_e0_wm]
#     form_e0_wm <- e0_winmargin_h[index_e0_wm ]
#     deleted_form_e0_wm <- form_e0_wm[!form_e0_wm[] == ""]
#     l6_form_e0_wm <- tail(deleted_form_e0_wm,e0_last_n_games)
#     l6_form_e0_wm <- as.numeric(l6_form_e0_wm)
#     suml6_e0_wm[index_e0_wm] <- sum(l6_form_e0_wm)
#     suml6_e0_wm[index_e0_wm] <- paste(suml6_e0_wm[index_e0_wm],sep = "")
#     suml6_e0_wm_negone[index_e0_wm] <- length(which(l6_form_e0_wm == -1))
#     suml6_e0_wm_negone[index_e0_wm] <- paste(suml6_e0_wm_negone[index_e0_wm],sep = "")
#     suml6_e0_wm_negtwo[index_e0_wm] <- length(which(l6_form_e0_wm <= -2))
#     suml6_e0_wm_negtwo[index_e0_wm] <- paste(suml6_e0_wm_negtwo[index_e0_wm],sep = "")
#     suml6_e0_wm_zero[index_e0_wm] <- length(which(l6_form_e0_wm == 0))
#     suml6_e0_wm_zero[index_e0_wm] <- paste(suml6_e0_wm_zero[index_e0_wm],sep = "")
#     suml6_e0_wm_posone[index_e0_wm] <- length(which(l6_form_e0_wm == 1))
#     suml6_e0_wm_posone[index_e0_wm] <- paste(suml6_e0_wm_posone[index_e0_wm],sep = "")
#     suml6_e0_wm_postwo[index_e0_wm] <- length(which(l6_form_e0_wm == 2))
#     suml6_e0_wm_postwo[index_e0_wm] <- paste(suml6_e0_wm_postwo[index_e0_wm],sep = "")
#     l6_form_e0_wm <- as.character(l6_form_e0_wm)
#     l6_form_e0_wm_flattened <- stri_paste(l6_form_e0_wm,collapse = ',')
#     l6_form_e0_wmsplitted <- (strsplit(as.character(l6_form_e0_wm_flattened),",")[[1]])
#     final_e0_wm[index_e0_wm,index_e0_wm_cols] <- l6_form_e0_wmsplitted[index_e0_wm_cols]
#   }
# }
# final_e0_wm[is.na(final_e0_wm)] <- ""
# e0_winmarginmatrix <- cbind(e0_teams,final_e0_wm,suml6_e0_wm,suml6_e0_wm_negtwo,suml6_e0_wm_negone,suml6_e0_wm_zero,suml6_e0_wm_posone,suml6_e0_wm_postwo)
# ##########################################################################################################################################################
# #e1
# final_e1_wm <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# suml6_e1_wm <- c()
# suml6_e1_wm_negone <- c()
# suml6_e1_wm_negtwo <- c()
# suml6_e1_wm_zero <- c()
# suml6_e1_wm_posone <- c()
# suml6_e1_wm_postwo <- c()
# l6_form_e1_wmsplitted <- c()
# form_e1_wm <- c()
# for(index_e1_wm in 1:length(e1_teams))
# {
#   for(index_e1_wm_cols in 1:e1_totalrounds)
#   {
#     index_e1_wm  <- row.names(e1_winmargin_h) == e1_teams[index_e1_wm]
#     form_e1_wm <- e1_winmargin_h[index_e1_wm ]
#     deleted_form_e1_wm <- form_e1_wm[!form_e1_wm[] == ""]
#     l6_form_e1_wm <- tail(deleted_form_e1_wm,e1_last_n_games)
#     l6_form_e1_wm <- as.numeric(l6_form_e1_wm)
#     suml6_e1_wm[index_e1_wm] <- sum(l6_form_e1_wm)
#     suml6_e1_wm[index_e1_wm] <- paste(suml6_e1_wm[index_e1_wm],sep = "")
#     suml6_e1_wm_negone[index_e1_wm] <- length(which(l6_form_e1_wm == -1))
#     suml6_e1_wm_negone[index_e1_wm] <- paste(suml6_e1_wm_negone[index_e1_wm],sep = "")
#     suml6_e1_wm_negtwo[index_e1_wm] <- length(which(l6_form_e1_wm <= -2))
#     suml6_e1_wm_negtwo[index_e1_wm] <- paste(suml6_e1_wm_negtwo[index_e1_wm],sep = "")
#     suml6_e1_wm_zero[index_e1_wm] <- length(which(l6_form_e1_wm == 0))
#     suml6_e1_wm_zero[index_e1_wm] <- paste(suml6_e1_wm_zero[index_e1_wm],sep = "")
#     suml6_e1_wm_posone[index_e1_wm] <- length(which(l6_form_e1_wm == 1))
#     suml6_e1_wm_posone[index_e1_wm] <- paste(suml6_e1_wm_posone[index_e1_wm],sep = "")
#     suml6_e1_wm_postwo[index_e1_wm] <- length(which(l6_form_e1_wm == 2))
#     suml6_e1_wm_postwo[index_e1_wm] <- paste(suml6_e1_wm_postwo[index_e1_wm],sep = "")
#     l6_form_e1_wm <- as.character(l6_form_e1_wm)
#     l6_form_e1_wm_flattened <- stri_paste(l6_form_e1_wm,collapse = ',')
#     l6_form_e1_wmsplitted <- (strsplit(as.character(l6_form_e1_wm_flattened),",")[[1]])
#     final_e1_wm[index_e1_wm,index_e1_wm_cols] <- l6_form_e1_wmsplitted[index_e1_wm_cols]
#   }
# }
# final_e1_wm[is.na(final_e1_wm)] <- ""
# e1_winmarginmatrix <- cbind(e1_teams,final_e1_wm,suml6_e1_wm,suml6_e1_wm_negtwo,suml6_e1_wm_negone,suml6_e1_wm_zero,suml6_e1_wm_posone,suml6_e1_wm_postwo)
# ###########################################################################################################################################################
# #e2
# final_e2_wm <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# suml6_e2_wm <- c()
# suml6_e2_wm_negone <- c()
# suml6_e2_wm_negtwo <- c()
# suml6_e2_wm_zero <- c()
# suml6_e2_wm_posone <- c()
# suml6_e2_wm_postwo <- c()
# l6_form_e2_wmsplitted <- c()
# form_e2_wm <- c()
# for(index_e2_wm in 1:length(e2_teams))
# {
#   for(index_e2_wm_cols in 1:e2_totalrounds)
#   {
#     index_e2_wm  <- row.names(e2_winmargin_h) == e2_teams[index_e2_wm]
#     form_e2_wm <- e2_winmargin_h[index_e2_wm ]
#     deleted_form_e2_wm <- form_e2_wm[!form_e2_wm[] == ""]
#     l6_form_e2_wm <- tail(deleted_form_e2_wm,e2_last_n_games)
#     l6_form_e2_wm <- as.numeric(l6_form_e2_wm)
#     suml6_e2_wm[index_e2_wm] <- sum(l6_form_e2_wm)
#     suml6_e2_wm[index_e2_wm] <- paste(suml6_e2_wm[index_e2_wm],sep = "")
#     suml6_e2_wm_negone[index_e2_wm] <- length(which(l6_form_e2_wm == -1))
#     suml6_e2_wm_negone[index_e2_wm] <- paste(suml6_e2_wm_negone[index_e2_wm],sep = "")
#     suml6_e2_wm_negtwo[index_e2_wm] <- length(which(l6_form_e2_wm <= -2))
#     suml6_e2_wm_negtwo[index_e2_wm] <- paste(suml6_e2_wm_negtwo[index_e2_wm],sep = "")
#     suml6_e2_wm_zero[index_e2_wm] <- length(which(l6_form_e2_wm == 0))
#     suml6_e2_wm_zero[index_e2_wm] <- paste(suml6_e2_wm_zero[index_e2_wm],sep = "")
#     suml6_e2_wm_posone[index_e2_wm] <- length(which(l6_form_e2_wm == 1))
#     suml6_e2_wm_posone[index_e2_wm] <- paste(suml6_e2_wm_posone[index_e2_wm],sep = "")
#     suml6_e2_wm_postwo[index_e2_wm] <- length(which(l6_form_e2_wm == 2))
#     suml6_e2_wm_postwo[index_e2_wm] <- paste(suml6_e2_wm_postwo[index_e2_wm],sep = "")
#     l6_form_e2_wm <- as.character(l6_form_e2_wm)
#     l6_form_e2_wm_flattened <- stri_paste(l6_form_e2_wm,collapse = ',')
#     l6_form_e2_wmsplitted <- (strsplit(as.character(l6_form_e2_wm_flattened),",")[[1]])
#     final_e2_wm[index_e2_wm,index_e2_wm_cols] <- l6_form_e2_wmsplitted[index_e2_wm_cols]
#   }
# }
# final_e2_wm[is.na(final_e2_wm)] <- ""
# e2_winmarginmatrix <- cbind(e2_teams,final_e2_wm,suml6_e2_wm,suml6_e2_wm_negtwo,suml6_e2_wm_negone,suml6_e2_wm_zero,suml6_e2_wm_posone,suml6_e2_wm_postwo)
# #########################################################################################################################################################
# #e3
# final_e3_wm <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# suml6_e3_wm <- c()
# suml6_e3_wm_negone <- c()
# suml6_e3_wm_negtwo <- c()
# suml6_e3_wm_zero <- c()
# suml6_e3_wm_posone <- c()
# suml6_e3_wm_postwo <- c()
# l6_form_e3_wmsplitted <- c()
# form_e3_wm <- c()
# for(index_e3_wm in 1:length(e3_teams))
# {
#   for(index_e3_wm_cols in 1:e3_totalrounds)
#   {
#     index_e3_wm  <- row.names(e3_winmargin_h) == e3_teams[index_e3_wm]
#     form_e3_wm <- e3_winmargin_h[index_e3_wm ]
#     deleted_form_e3_wm <- form_e3_wm[!form_e3_wm[] == ""]
#     l6_form_e3_wm <- tail(deleted_form_e3_wm,e3_last_n_games)
#     l6_form_e3_wm <- as.numeric(l6_form_e3_wm)
#     suml6_e3_wm[index_e3_wm] <- sum(l6_form_e3_wm)
#     suml6_e3_wm[index_e3_wm] <- paste(suml6_e3_wm[index_e3_wm],sep = "")
#     suml6_e3_wm_negone[index_e3_wm] <- length(which(l6_form_e3_wm == -1))
#     suml6_e3_wm_negone[index_e3_wm] <- paste(suml6_e3_wm_negone[index_e3_wm],sep = "")
#     suml6_e3_wm_negtwo[index_e3_wm] <- length(which(l6_form_e3_wm <= -2))
#     suml6_e3_wm_negtwo[index_e3_wm] <- paste(suml6_e3_wm_negtwo[index_e3_wm],sep = "")
#     suml6_e3_wm_zero[index_e3_wm] <- length(which(l6_form_e3_wm == 0))
#     suml6_e3_wm_zero[index_e3_wm] <- paste(suml6_e3_wm_zero[index_e3_wm],sep = "")
#     suml6_e3_wm_posone[index_e3_wm] <- length(which(l6_form_e3_wm == 1))
#     suml6_e3_wm_posone[index_e3_wm] <- paste(suml6_e3_wm_posone[index_e3_wm],sep = "")
#     suml6_e3_wm_postwo[index_e3_wm] <- length(which(l6_form_e3_wm == 2))
#     suml6_e3_wm_postwo[index_e3_wm] <- paste(suml6_e3_wm_postwo[index_e3_wm],sep = "")
#     l6_form_e3_wm <- as.character(l6_form_e3_wm)
#     l6_form_e3_wm_flattened <- stri_paste(l6_form_e3_wm,collapse = ',')
#     l6_form_e3_wmsplitted <- (strsplit(as.character(l6_form_e3_wm_flattened),",")[[1]])
#     final_e3_wm[index_e3_wm,index_e3_wm_cols] <- l6_form_e3_wmsplitted[index_e3_wm_cols]
#   }
# }
# final_e3_wm[is.na(final_e3_wm)] <- ""
# e3_winmarginmatrix <- cbind(e3_teams,final_e3_wm,suml6_e3_wm,suml6_e3_wm_negtwo,suml6_e3_wm_negone,suml6_e3_wm_zero,suml6_e3_wm_posone,suml6_e3_wm_postwo)
# ##########################################################################################################################################################
# #ec
# final_ec_wm <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# suml6_ec_wm <- c()
# suml6_ec_wm_negone <- c()
# suml6_ec_wm_negtwo <- c()
# suml6_ec_wm_zero <- c()
# suml6_ec_wm_posone <- c()
# suml6_ec_wm_postwo <- c()
# l6_form_ec_wmsplitted <- c()
# form_ec_wm <- c()
# for(index_ec_wm in 1:length(ec_teams))
# {
#   for(index_ec_wm_cols in 1:ec_totalrounds)
#   {
#     index_ec_wm  <- row.names(ec_winmargin_h) == ec_teams[index_ec_wm]
#     form_ec_wm <- ec_winmargin_h[index_ec_wm ]
#     deleted_form_ec_wm <- form_ec_wm[!form_ec_wm[] == ""]
#     l6_form_ec_wm <- tail(deleted_form_ec_wm,ec_last_n_games)
#     l6_form_ec_wm <- as.numeric(l6_form_ec_wm)
#     suml6_ec_wm[index_ec_wm] <- sum(l6_form_ec_wm)
#     suml6_ec_wm[index_ec_wm] <- paste(suml6_ec_wm[index_ec_wm],sep = "")
#     suml6_ec_wm_negone[index_ec_wm] <- length(which(l6_form_ec_wm == -1))
#     suml6_ec_wm_negone[index_ec_wm] <- paste(suml6_ec_wm_negone[index_ec_wm],sep = "")
#     suml6_ec_wm_negtwo[index_ec_wm] <- length(which(l6_form_ec_wm <= -2))
#     suml6_ec_wm_negtwo[index_ec_wm] <- paste(suml6_ec_wm_negtwo[index_ec_wm],sep = "")
#     suml6_ec_wm_zero[index_ec_wm] <- length(which(l6_form_ec_wm == 0))
#     suml6_ec_wm_zero[index_ec_wm] <- paste(suml6_ec_wm_zero[index_ec_wm],sep = "")
#     suml6_ec_wm_posone[index_ec_wm] <- length(which(l6_form_ec_wm == 1))
#     suml6_ec_wm_posone[index_ec_wm] <- paste(suml6_ec_wm_posone[index_ec_wm],sep = "")
#     suml6_ec_wm_postwo[index_ec_wm] <- length(which(l6_form_ec_wm == 2))
#     suml6_ec_wm_postwo[index_ec_wm] <- paste(suml6_ec_wm_postwo[index_ec_wm],sep = "")
#     l6_form_ec_wm <- as.character(l6_form_ec_wm)
#     l6_form_ec_wm_flattened <- stri_paste(l6_form_ec_wm,collapse = ',')
#     l6_form_ec_wmsplitted <- (strsplit(as.character(l6_form_ec_wm_flattened),",")[[1]])
#     final_ec_wm[index_ec_wm,index_ec_wm_cols] <- l6_form_ec_wmsplitted[index_ec_wm_cols]
#   }
# }
# final_ec_wm[is.na(final_ec_wm)] <- ""
# ec_winmarginmatrix <- cbind(ec_teams,final_ec_wm,suml6_ec_wm,suml6_ec_wm_negtwo,suml6_ec_wm_negone,suml6_ec_wm_zero,suml6_ec_wm_posone,suml6_ec_wm_postwo)
# ############################################################################################################################################################
# #f1
# final_f1_wm <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# suml6_f1_wm <- c()
# suml6_f1_wm_negone <- c()
# suml6_f1_wm_negtwo <- c()
# suml6_f1_wm_zero <- c()
# suml6_f1_wm_posone <- c()
# suml6_f1_wm_postwo <- c()
# l6_form_f1_wmsplitted <- c()
# form_f1_wm <- c()
# for(index_f1_wm in 1:length(f1_teams))
# {
#   for(index_f1_wm_cols in 1:f1_totalrounds)
#   {
#     index_f1_wm  <- row.names(f1_winmargin_h) == f1_teams[index_f1_wm]
#     form_f1_wm <- f1_winmargin_h[index_f1_wm ]
#     deleted_form_f1_wm <- form_f1_wm[!form_f1_wm[] == ""]
#     l6_form_f1_wm <- tail(deleted_form_f1_wm,f1_last_n_games)
#     l6_form_f1_wm <- as.numeric(l6_form_f1_wm)
#     suml6_f1_wm[index_f1_wm] <- sum(l6_form_f1_wm)
#     suml6_f1_wm[index_f1_wm] <- paste(suml6_f1_wm[index_f1_wm],sep = "")
#     suml6_f1_wm_negone[index_f1_wm] <- length(which(l6_form_f1_wm == -1))
#     suml6_f1_wm_negone[index_f1_wm] <- paste(suml6_f1_wm_negone[index_f1_wm],sep = "")
#     suml6_f1_wm_negtwo[index_f1_wm] <- length(which(l6_form_f1_wm <= -2))
#     suml6_f1_wm_negtwo[index_f1_wm] <- paste(suml6_f1_wm_negtwo[index_f1_wm],sep = "")
#     suml6_f1_wm_zero[index_f1_wm] <- length(which(l6_form_f1_wm == 0))
#     suml6_f1_wm_zero[index_f1_wm] <- paste(suml6_f1_wm_zero[index_f1_wm],sep = "")
#     suml6_f1_wm_posone[index_f1_wm] <- length(which(l6_form_f1_wm == 1))
#     suml6_f1_wm_posone[index_f1_wm] <- paste(suml6_f1_wm_posone[index_f1_wm],sep = "")
#     suml6_f1_wm_postwo[index_f1_wm] <- length(which(l6_form_f1_wm == 2))
#     suml6_f1_wm_postwo[index_f1_wm] <- paste(suml6_f1_wm_postwo[index_f1_wm],sep = "")
#     l6_form_f1_wm <- as.character(l6_form_f1_wm)
#     l6_form_f1_wm_flattened <- stri_paste(l6_form_f1_wm,collapse = ',')
#     l6_form_f1_wmsplitted <- (strsplit(as.character(l6_form_f1_wm_flattened),",")[[1]])
#     final_f1_wm[index_f1_wm,index_f1_wm_cols] <- l6_form_f1_wmsplitted[index_f1_wm_cols]
#   }
# }
# final_f1_wm[is.na(final_f1_wm)] <- ""
# f1_winmarginmatrix <- cbind(f1_teams,final_f1_wm,suml6_f1_wm,suml6_f1_wm_negtwo,suml6_f1_wm_negone,suml6_f1_wm_zero,suml6_f1_wm_posone,suml6_f1_wm_postwo)
# ##########################################################################################################################################################
# #f2
# final_f2_wm <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# suml6_f2_wm <- c()
# suml6_f2_wm_negone <- c()
# suml6_f2_wm_negtwo <- c()
# suml6_f2_wm_zero <- c()
# suml6_f2_wm_posone <- c()
# suml6_f2_wm_postwo <- c()
# l6_form_f2_wmsplitted <- c()
# form_f2_wm <- c()
# for(index_f2_wm in 1:length(f2_teams))
# {
#   for(index_f2_wm_cols in 1:f2_totalrounds)
#   {
#     index_f2_wm  <- row.names(f2_winmargin_h) == f2_teams[index_f2_wm]
#     form_f2_wm <- f2_winmargin_h[index_f2_wm ]
#     deleted_form_f2_wm <- form_f2_wm[!form_f2_wm[] == ""]
#     l6_form_f2_wm <- tail(deleted_form_f2_wm,f2_last_n_games)
#     l6_form_f2_wm <- as.numeric(l6_form_f2_wm)
#     suml6_f2_wm[index_f2_wm] <- sum(l6_form_f2_wm)
#     suml6_f2_wm[index_f2_wm] <- paste(suml6_f2_wm[index_f2_wm],sep = "")
#     suml6_f2_wm_negone[index_f2_wm] <- length(which(l6_form_f2_wm == -1))
#     suml6_f2_wm_negone[index_f2_wm] <- paste(suml6_f2_wm_negone[index_f2_wm],sep = "")
#     suml6_f2_wm_negtwo[index_f2_wm] <- length(which(l6_form_f2_wm <= -2))
#     suml6_f2_wm_negtwo[index_f2_wm] <- paste(suml6_f2_wm_negtwo[index_f2_wm],sep = "")
#     suml6_f2_wm_zero[index_f2_wm] <- length(which(l6_form_f2_wm == 0))
#     suml6_f2_wm_zero[index_f2_wm] <- paste(suml6_f2_wm_zero[index_f2_wm],sep = "")
#     suml6_f2_wm_posone[index_f2_wm] <- length(which(l6_form_f2_wm == 1))
#     suml6_f2_wm_posone[index_f2_wm] <- paste(suml6_f2_wm_posone[index_f2_wm],sep = "")
#     suml6_f2_wm_postwo[index_f2_wm] <- length(which(l6_form_f2_wm == 2))
#     suml6_f2_wm_postwo[index_f2_wm] <- paste(suml6_f2_wm_postwo[index_f2_wm],sep = "")
#     l6_form_f2_wm <- as.character(l6_form_f2_wm)
#     l6_form_f2_wm_flattened <- stri_paste(l6_form_f2_wm,collapse = ',')
#     l6_form_f2_wmsplitted <- (strsplit(as.character(l6_form_f2_wm_flattened),",")[[1]])
#     final_f2_wm[index_f2_wm,index_f2_wm_cols] <- l6_form_f2_wmsplitted[index_f2_wm_cols]
#   }
# }
# final_f2_wm[is.na(final_f2_wm)] <- ""
# f2_winmarginmatrix <- cbind(f2_teams,final_f2_wm,suml6_f2_wm,suml6_f2_wm_negtwo,suml6_f2_wm_negone,suml6_f2_wm_zero,suml6_f2_wm_posone,suml6_f2_wm_postwo)
# ###########################################################################################################################################################
# #g1
# final_g1_wm <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# suml6_g1_wm <- c()
# suml6_g1_wm_negone <- c()
# suml6_g1_wm_negtwo <- c()
# suml6_g1_wm_zero <- c()
# suml6_g1_wm_posone <- c()
# suml6_g1_wm_postwo <- c()
# l6_form_g1_wmsplitted <- c()
# form_g1_wm <- c()
# for(index_g1_wm in 1:length(g1_teams))
# {
#   for(index_g1_wm_cols in 1:g1_totalrounds)
#   {
#     index_g1_wm  <- row.names(g1_winmargin_h) == g1_teams[index_g1_wm]
#     form_g1_wm <- g1_winmargin_h[index_g1_wm ]
#     deleted_form_g1_wm <- form_g1_wm[!form_g1_wm[] == ""]
#     l6_form_g1_wm <- tail(deleted_form_g1_wm,g1_last_n_games)
#     l6_form_g1_wm <- as.numeric(l6_form_g1_wm)
#     suml6_g1_wm[index_g1_wm] <- sum(l6_form_g1_wm)
#     suml6_g1_wm[index_g1_wm] <- paste(suml6_g1_wm[index_g1_wm],sep = "")
#     suml6_g1_wm_negone[index_g1_wm] <- length(which(l6_form_g1_wm == -1))
#     suml6_g1_wm_negone[index_g1_wm] <- paste(suml6_g1_wm_negone[index_g1_wm],sep = "")
#     suml6_g1_wm_negtwo[index_g1_wm] <- length(which(l6_form_g1_wm <= -2))
#     suml6_g1_wm_negtwo[index_g1_wm] <- paste(suml6_g1_wm_negtwo[index_g1_wm],sep = "")
#     suml6_g1_wm_zero[index_g1_wm] <- length(which(l6_form_g1_wm == 0))
#     suml6_g1_wm_zero[index_g1_wm] <- paste(suml6_g1_wm_zero[index_g1_wm],sep = "")
#     suml6_g1_wm_posone[index_g1_wm] <- length(which(l6_form_g1_wm == 1))
#     suml6_g1_wm_posone[index_g1_wm] <- paste(suml6_g1_wm_posone[index_g1_wm],sep = "")
#     suml6_g1_wm_postwo[index_g1_wm] <- length(which(l6_form_g1_wm == 2))
#     suml6_g1_wm_postwo[index_g1_wm] <- paste(suml6_g1_wm_postwo[index_g1_wm],sep = "")
#     l6_form_g1_wm <- as.character(l6_form_g1_wm)
#     l6_form_g1_wm_flattened <- stri_paste(l6_form_g1_wm,collapse = ',')
#     l6_form_g1_wmsplitted <- (strsplit(as.character(l6_form_g1_wm_flattened),",")[[1]])
#     final_g1_wm[index_g1_wm,index_g1_wm_cols] <- l6_form_g1_wmsplitted[index_g1_wm_cols]
#   }
# }
# final_g1_wm[is.na(final_g1_wm)] <- ""
# g1_winmarginmatrix <- cbind(g1_teams,final_g1_wm,suml6_g1_wm,suml6_g1_wm_negtwo,suml6_g1_wm_negone,suml6_g1_wm_zero,suml6_g1_wm_posone,suml6_g1_wm_postwo)
# ###########################################################################################################################################################
# #i1
# final_i1_wm <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# suml6_i1_wm <- c()
# suml6_i1_wm_negone <- c()
# suml6_i1_wm_negtwo <- c()
# suml6_i1_wm_zero <- c()
# suml6_i1_wm_posone <- c()
# suml6_i1_wm_postwo <- c()
# l6_form_i1_wmsplitted <- c()
# form_i1_wm <- c()
# for(index_i1_wm in 1:length(i1_teams))
# {
#   for(index_i1_wm_cols in 1:i1_totalrounds)
#   {
#     index_i1_wm  <- row.names(i1_winmargin_h) == i1_teams[index_i1_wm]
#     form_i1_wm <- i1_winmargin_h[index_i1_wm ]
#     deleted_form_i1_wm <- form_i1_wm[!form_i1_wm[] == ""]
#     l6_form_i1_wm <- tail(deleted_form_i1_wm,i1_last_n_games)
#     l6_form_i1_wm <- as.numeric(l6_form_i1_wm)
#     suml6_i1_wm[index_i1_wm] <- sum(l6_form_i1_wm)
#     suml6_i1_wm[index_i1_wm] <- paste(suml6_i1_wm[index_i1_wm],sep = "")
#     suml6_i1_wm_negone[index_i1_wm] <- length(which(l6_form_i1_wm == -1))
#     suml6_i1_wm_negone[index_i1_wm] <- paste(suml6_i1_wm_negone[index_i1_wm],sep = "")
#     suml6_i1_wm_negtwo[index_i1_wm] <- length(which(l6_form_i1_wm <= -2))
#     suml6_i1_wm_negtwo[index_i1_wm] <- paste(suml6_i1_wm_negtwo[index_i1_wm],sep = "")
#     suml6_i1_wm_zero[index_i1_wm] <- length(which(l6_form_i1_wm == 0))
#     suml6_i1_wm_zero[index_i1_wm] <- paste(suml6_i1_wm_zero[index_i1_wm],sep = "")
#     suml6_i1_wm_posone[index_i1_wm] <- length(which(l6_form_i1_wm == 1))
#     suml6_i1_wm_posone[index_i1_wm] <- paste(suml6_i1_wm_posone[index_i1_wm],sep = "")
#     suml6_i1_wm_postwo[index_i1_wm] <- length(which(l6_form_i1_wm == 2))
#     suml6_i1_wm_postwo[index_i1_wm] <- paste(suml6_i1_wm_postwo[index_i1_wm],sep = "")
#     l6_form_i1_wm <- as.character(l6_form_i1_wm)
#     l6_form_i1_wm_flattened <- stri_paste(l6_form_i1_wm,collapse = ',')
#     l6_form_i1_wmsplitted <- (strsplit(as.character(l6_form_i1_wm_flattened),",")[[1]])
#     final_i1_wm[index_i1_wm,index_i1_wm_cols] <- l6_form_i1_wmsplitted[index_i1_wm_cols]
#   }
# }
# final_i1_wm[is.na(final_i1_wm)] <- ""
# i1_winmarginmatrix <- cbind(i1_teams,final_i1_wm,suml6_i1_wm,suml6_i1_wm_negtwo,suml6_i1_wm_negone,suml6_i1_wm_zero,suml6_i1_wm_posone,suml6_i1_wm_postwo)
# #############################################################################################################################################################
# ##i2
# final_i2_wm <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# suml6_i2_wm <- c()
# suml6_i2_wm_negone <- c()
# suml6_i2_wm_negtwo <- c()
# suml6_i2_wm_zero <- c()
# suml6_i2_wm_posone <- c()
# suml6_i2_wm_postwo <- c()
# l6_form_i2_wmsplitted <- c()
# form_i2_wm <- c()
# for(index_i2_wm in 1:length(i2_teams))
# {
#   for(index_i2_wm_cols in 1:i2_totalrounds)
#   {
#     index_i2_wm  <- row.names(i2_winmargin_h) == i2_teams[index_i2_wm]
#     form_i2_wm <- i2_winmargin_h[index_i2_wm ]
#     deleted_form_i2_wm <- form_i2_wm[!form_i2_wm[] == ""]
#     l6_form_i2_wm <- tail(deleted_form_i2_wm,i2_last_n_games)
#     l6_form_i2_wm <- as.numeric(l6_form_i2_wm)
#     suml6_i2_wm[index_i2_wm] <- sum(l6_form_i2_wm)
#     suml6_i2_wm[index_i2_wm] <- paste(suml6_i2_wm[index_i2_wm],sep = "")
#     suml6_i2_wm_negone[index_i2_wm] <- length(which(l6_form_i2_wm == -1))
#     suml6_i2_wm_negone[index_i2_wm] <- paste(suml6_i2_wm_negone[index_i2_wm],sep = "")
#     suml6_i2_wm_negtwo[index_i2_wm] <- length(which(l6_form_i2_wm <= -2))
#     suml6_i2_wm_negtwo[index_i2_wm] <- paste(suml6_i2_wm_negtwo[index_i2_wm],sep = "")
#     suml6_i2_wm_zero[index_i2_wm] <- length(which(l6_form_i2_wm == 0))
#     suml6_i2_wm_zero[index_i2_wm] <- paste(suml6_i2_wm_zero[index_i2_wm],sep = "")
#     suml6_i2_wm_posone[index_i2_wm] <- length(which(l6_form_i2_wm == 1))
#     suml6_i2_wm_posone[index_i2_wm] <- paste(suml6_i2_wm_posone[index_i2_wm],sep = "")
#     suml6_i2_wm_postwo[index_i2_wm] <- length(which(l6_form_i2_wm == 2))
#     suml6_i2_wm_postwo[index_i2_wm] <- paste(suml6_i2_wm_postwo[index_i2_wm],sep = "")
#     l6_form_i2_wm <- as.character(l6_form_i2_wm)
#     l6_form_i2_wm_flattened <- stri_paste(l6_form_i2_wm,collapse = ',')
#     l6_form_i2_wmsplitted <- (strsplit(as.character(l6_form_i2_wm_flattened),",")[[1]])
#     final_i2_wm[index_i2_wm,index_i2_wm_cols] <- l6_form_i2_wmsplitted[index_i2_wm_cols]
#   }
# }
# final_i2_wm[is.na(final_i2_wm)] <- ""
# i2_winmarginmatrix <- cbind(i2_teams,final_i2_wm,suml6_i2_wm,suml6_i2_wm_negtwo,suml6_i2_wm_negone,suml6_i2_wm_zero,suml6_i2_wm_posone,suml6_i2_wm_postwo)
# ###############################################################################################################################################################
# #n1
# final_n1_wm <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# suml6_n1_wm <- c()
# suml6_n1_wm_negone <- c()
# suml6_n1_wm_negtwo <- c()
# suml6_n1_wm_zero <- c()
# suml6_n1_wm_posone <- c()
# suml6_n1_wm_postwo <- c()
# l6_form_n1_wmsplitted <- c()
# form_n1_wm <- c()
# for(index_n1_wm in 1:length(n1_teams))
# {
#   for(index_n1_wm_cols in 1:n1_totalrounds)
#   {
#     index_n1_wm  <- row.names(n1_winmargin_h) == n1_teams[index_n1_wm]
#     form_n1_wm <- n1_winmargin_h[index_n1_wm ]
#     deleted_form_n1_wm <- form_n1_wm[!form_n1_wm[] == ""]
#     l6_form_n1_wm <- tail(deleted_form_n1_wm,n1_last_n_games)
#     l6_form_n1_wm <- as.numeric(l6_form_n1_wm)
#     suml6_n1_wm[index_n1_wm] <- sum(l6_form_n1_wm)
#     suml6_n1_wm[index_n1_wm] <- paste(suml6_n1_wm[index_n1_wm],sep = "")
#     suml6_n1_wm_negone[index_n1_wm] <- length(which(l6_form_n1_wm == -1))
#     suml6_n1_wm_negone[index_n1_wm] <- paste(suml6_n1_wm_negone[index_n1_wm],sep = "")
#     suml6_n1_wm_negtwo[index_n1_wm] <- length(which(l6_form_n1_wm <= -2))
#     suml6_n1_wm_negtwo[index_n1_wm] <- paste(suml6_n1_wm_negtwo[index_n1_wm],sep = "")
#     suml6_n1_wm_zero[index_n1_wm] <- length(which(l6_form_n1_wm == 0))
#     suml6_n1_wm_zero[index_n1_wm] <- paste(suml6_n1_wm_zero[index_n1_wm],sep = "")
#     suml6_n1_wm_posone[index_n1_wm] <- length(which(l6_form_n1_wm == 1))
#     suml6_n1_wm_posone[index_n1_wm] <- paste(suml6_n1_wm_posone[index_n1_wm],sep = "")
#     suml6_n1_wm_postwo[index_n1_wm] <- length(which(l6_form_n1_wm == 2))
#     suml6_n1_wm_postwo[index_n1_wm] <- paste(suml6_n1_wm_postwo[index_n1_wm],sep = "")
#     l6_form_n1_wm <- as.character(l6_form_n1_wm)
#     l6_form_n1_wm_flattened <- stri_paste(l6_form_n1_wm,collapse = ',')
#     l6_form_n1_wmsplitted <- (strsplit(as.character(l6_form_n1_wm_flattened),",")[[1]])
#     final_n1_wm[index_n1_wm,index_n1_wm_cols] <- l6_form_n1_wmsplitted[index_n1_wm_cols]
#   }
# }
# final_n1_wm[is.na(final_n1_wm)] <- ""
# n1_winmarginmatrix <- cbind(n1_teams,final_n1_wm,suml6_n1_wm,suml6_n1_wm_negtwo,suml6_n1_wm_negone,suml6_n1_wm_zero,suml6_n1_wm_posone,suml6_n1_wm_postwo)
# ##############################################################################################################################################################
# #p1
# final_p1_wm <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# suml6_p1_wm <- c()
# suml6_p1_wm_negone <- c()
# suml6_p1_wm_negtwo <- c()
# suml6_p1_wm_zero <- c()
# suml6_p1_wm_posone <- c()
# suml6_p1_wm_postwo <- c()
# l6_form_p1_wmsplitted <- c()
# form_p1_wm <- c()
# for(index_p1_wm in 1:length(p1_teams))
# {
#   for(index_p1_wm_cols in 1:p1_totalrounds)
#   {
#     index_p1_wm  <- row.names(p1_winmargin_h) == p1_teams[index_p1_wm]
#     form_p1_wm <- p1_winmargin_h[index_p1_wm ]
#     deleted_form_p1_wm <- form_p1_wm[!form_p1_wm[] == ""]
#     l6_form_p1_wm <- tail(deleted_form_p1_wm,p1_last_n_games)
#     l6_form_p1_wm <- as.numeric(l6_form_p1_wm)
#     suml6_p1_wm[index_p1_wm] <- sum(l6_form_p1_wm)
#     suml6_p1_wm[index_p1_wm] <- paste(suml6_p1_wm[index_p1_wm],sep = "")
#     suml6_p1_wm_negone[index_p1_wm] <- length(which(l6_form_p1_wm == -1))
#     suml6_p1_wm_negone[index_p1_wm] <- paste(suml6_p1_wm_negone[index_p1_wm],sep = "")
#     suml6_p1_wm_negtwo[index_p1_wm] <- length(which(l6_form_p1_wm <= -2))
#     suml6_p1_wm_negtwo[index_p1_wm] <- paste(suml6_p1_wm_negtwo[index_p1_wm],sep = "")
#     suml6_p1_wm_zero[index_p1_wm] <- length(which(l6_form_p1_wm == 0))
#     suml6_p1_wm_zero[index_p1_wm] <- paste(suml6_p1_wm_zero[index_p1_wm],sep = "")
#     suml6_p1_wm_posone[index_p1_wm] <- length(which(l6_form_p1_wm == 1))
#     suml6_p1_wm_posone[index_p1_wm] <- paste(suml6_p1_wm_posone[index_p1_wm],sep = "")
#     suml6_p1_wm_postwo[index_p1_wm] <- length(which(l6_form_p1_wm == 2))
#     suml6_p1_wm_postwo[index_p1_wm] <- paste(suml6_p1_wm_postwo[index_p1_wm],sep = "")
#     l6_form_p1_wm <- as.character(l6_form_p1_wm)
#     l6_form_p1_wm_flattened <- stri_paste(l6_form_p1_wm,collapse = ',')
#     l6_form_p1_wmsplitted <- (strsplit(as.character(l6_form_p1_wm_flattened),",")[[1]])
#     final_p1_wm[index_p1_wm,index_p1_wm_cols] <- l6_form_p1_wmsplitted[index_p1_wm_cols]
#   }
# }
# final_p1_wm[is.na(final_p1_wm)] <- ""
# p1_winmarginmatrix <- cbind(p1_teams,final_p1_wm,suml6_p1_wm,suml6_p1_wm_negtwo,suml6_p1_wm_negone,suml6_p1_wm_zero,suml6_p1_wm_posone,suml6_p1_wm_postwo)
# #############################################################################################################################################################
# #sp1
# final_sp1_wm <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# suml6_sp1_wm <- c()
# suml6_sp1_wm_negone <- c()
# suml6_sp1_wm_negtwo <- c()
# suml6_sp1_wm_zero <- c()
# suml6_sp1_wm_posone <- c()
# suml6_sp1_wm_postwo <- c()
# l6_form_sp1_wmsplitted <- c()
# form_sp1_wm <- c()
# for(index_sp1_wm in 1:length(sp1_teams))
# {
#   for(index_sp1_wm_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_wm  <- row.names(sp1_winmargin_h) == sp1_teams[index_sp1_wm]
#     form_sp1_wm <- sp1_winmargin_h[index_sp1_wm ]
#     deleted_form_sp1_wm <- form_sp1_wm[!form_sp1_wm[] == ""]
#     l6_form_sp1_wm <- tail(deleted_form_sp1_wm,sp1_last_n_games)
#     l6_form_sp1_wm <- as.numeric(l6_form_sp1_wm)
#     suml6_sp1_wm[index_sp1_wm] <- sum(l6_form_sp1_wm)
#     suml6_sp1_wm[index_sp1_wm] <- paste(suml6_sp1_wm[index_sp1_wm],sep = "")
#     suml6_sp1_wm_negone[index_sp1_wm] <- length(which(l6_form_sp1_wm == -1))
#     suml6_sp1_wm_negone[index_sp1_wm] <- paste(suml6_sp1_wm_negone[index_sp1_wm],sep = "")
#     suml6_sp1_wm_negtwo[index_sp1_wm] <- length(which(l6_form_sp1_wm <= -2))
#     suml6_sp1_wm_negtwo[index_sp1_wm] <- paste(suml6_sp1_wm_negtwo[index_sp1_wm],sep = "")
#     suml6_sp1_wm_zero[index_sp1_wm] <- length(which(l6_form_sp1_wm == 0))
#     suml6_sp1_wm_zero[index_sp1_wm] <- paste(suml6_sp1_wm_zero[index_sp1_wm],sep = "")
#     suml6_sp1_wm_posone[index_sp1_wm] <- length(which(l6_form_sp1_wm == 1))
#     suml6_sp1_wm_posone[index_sp1_wm] <- paste(suml6_sp1_wm_posone[index_sp1_wm],sep = "")
#     suml6_sp1_wm_postwo[index_sp1_wm] <- length(which(l6_form_sp1_wm == 2))
#     suml6_sp1_wm_postwo[index_sp1_wm] <- paste(suml6_sp1_wm_postwo[index_sp1_wm],sep = "")
#     l6_form_sp1_wm <- as.character(l6_form_sp1_wm)
#     l6_form_sp1_wm_flattened <- stri_paste(l6_form_sp1_wm,collapse = ',')
#     l6_form_sp1_wmsplitted <- (strsplit(as.character(l6_form_sp1_wm_flattened),",")[[1]])
#     final_sp1_wm[index_sp1_wm,index_sp1_wm_cols] <- l6_form_sp1_wmsplitted[index_sp1_wm_cols]
#   }
# }
# final_sp1_wm[is.na(final_sp1_wm)] <- ""
# sp1_winmarginmatrix <- cbind(sp1_teams,final_sp1_wm,suml6_sp1_wm,suml6_sp1_wm_negtwo,suml6_sp1_wm_negone,suml6_sp1_wm_zero,suml6_sp1_wm_posone,suml6_sp1_wm_postwo)
# ##############################################################################################################################################################
# #sp2
# final_sp2_wm <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# suml6_sp2_wm <- c()
# suml6_sp2_wm_negone <- c()
# suml6_sp2_wm_negtwo <- c()
# suml6_sp2_wm_zero <- c()
# suml6_sp2_wm_posone <- c()
# suml6_sp2_wm_postwo <- c()
# l6_form_sp2_wmsplitted <- c()
# form_sp2_wm <- c()
# for(index_sp2_wm in 1:length(sp2_teams))
# {
#   for(index_sp2_wm_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_wm  <- row.names(sp2_winmargin_h) == sp2_teams[index_sp2_wm]
#     form_sp2_wm <- sp2_winmargin_h[index_sp2_wm ]
#     deleted_form_sp2_wm <- form_sp2_wm[!form_sp2_wm[] == ""]
#     l6_form_sp2_wm <- tail(deleted_form_sp2_wm,sp2_last_n_games)
#     l6_form_sp2_wm <- as.numeric(l6_form_sp2_wm)
#     suml6_sp2_wm[index_sp2_wm] <- sum(l6_form_sp2_wm)
#     suml6_sp2_wm[index_sp2_wm] <- paste(suml6_sp2_wm[index_sp2_wm],sep = "")
#     suml6_sp2_wm_negone[index_sp2_wm] <- length(which(l6_form_sp2_wm == -1))
#     suml6_sp2_wm_negone[index_sp2_wm] <- paste(suml6_sp2_wm_negone[index_sp2_wm],sep = "")
#     suml6_sp2_wm_negtwo[index_sp2_wm] <- length(which(l6_form_sp2_wm <= -2))
#     suml6_sp2_wm_negtwo[index_sp2_wm] <- paste(suml6_sp2_wm_negtwo[index_sp2_wm],sep = "")
#     suml6_sp2_wm_zero[index_sp2_wm] <- length(which(l6_form_sp2_wm == 0))
#     suml6_sp2_wm_zero[index_sp2_wm] <- paste(suml6_sp2_wm_zero[index_sp2_wm],sep = "")
#     suml6_sp2_wm_posone[index_sp2_wm] <- length(which(l6_form_sp2_wm == 1))
#     suml6_sp2_wm_posone[index_sp2_wm] <- paste(suml6_sp2_wm_posone[index_sp2_wm],sep = "")
#     suml6_sp2_wm_postwo[index_sp2_wm] <- length(which(l6_form_sp2_wm == 2))
#     suml6_sp2_wm_postwo[index_sp2_wm] <- paste(suml6_sp2_wm_postwo[index_sp2_wm],sep = "")
#     l6_form_sp2_wm <- as.character(l6_form_sp2_wm)
#     l6_form_sp2_wm_flattened <- stri_paste(l6_form_sp2_wm,collapse = ',')
#     l6_form_sp2_wmsplitted <- (strsplit(as.character(l6_form_sp2_wm_flattened),",")[[1]])
#     final_sp2_wm[index_sp2_wm,index_sp2_wm_cols] <- l6_form_sp2_wmsplitted[index_sp2_wm_cols]
#   }
# }
# final_sp2_wm[is.na(final_sp2_wm)] <- ""
# sp2_winmarginmatrix <- cbind(sp2_teams,final_sp2_wm,suml6_sp2_wm,suml6_sp2_wm_negtwo,suml6_sp2_wm_negone,suml6_sp2_wm_zero,suml6_sp2_wm_posone,suml6_sp2_wm_postwo)
# ##############################################################################################################################################################
# #sc0
# final_sc0_wm <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_wm <- c()
# suml6_sc0_wm_negone <- c()
# suml6_sc0_wm_negtwo <- c()
# suml6_sc0_wm_zero <- c()
# suml6_sc0_wm_posone <- c()
# suml6_sc0_wm_postwo <- c()
# l6_form_sc0_wmsplitted <- c()
# form_sc0_wm <- c()
# for(index_sc0_wm in 1:length(sc0_teams))
# {
#   for(index_sc0_wm_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_wm  <- row.names(sc0_winmargin_h) == sc0_teams[index_sc0_wm]
#     form_sc0_wm <- sc0_winmargin_h[index_sc0_wm ]
#     deleted_form_sc0_wm <- form_sc0_wm[!form_sc0_wm[] == ""]
#     l6_form_sc0_wm <- tail(deleted_form_sc0_wm,sc0_last_n_games)
#     l6_form_sc0_wm <- as.numeric(l6_form_sc0_wm)
#     suml6_sc0_wm[index_sc0_wm] <- sum(l6_form_sc0_wm)
#     suml6_sc0_wm[index_sc0_wm] <- paste(suml6_sc0_wm[index_sc0_wm],sep = "")
#     suml6_sc0_wm_negone[index_sc0_wm] <- length(which(l6_form_sc0_wm == -1))
#     suml6_sc0_wm_negone[index_sc0_wm] <- paste(suml6_sc0_wm_negone[index_sc0_wm],sep = "")
#     suml6_sc0_wm_negtwo[index_sc0_wm] <- length(which(l6_form_sc0_wm <= -2))
#     suml6_sc0_wm_negtwo[index_sc0_wm] <- paste(suml6_sc0_wm_negtwo[index_sc0_wm],sep = "")
#     suml6_sc0_wm_zero[index_sc0_wm] <- length(which(l6_form_sc0_wm == 0))
#     suml6_sc0_wm_zero[index_sc0_wm] <- paste(suml6_sc0_wm_zero[index_sc0_wm],sep = "")
#     suml6_sc0_wm_posone[index_sc0_wm] <- length(which(l6_form_sc0_wm == 1))
#     suml6_sc0_wm_posone[index_sc0_wm] <- paste(suml6_sc0_wm_posone[index_sc0_wm],sep = "")
#     suml6_sc0_wm_postwo[index_sc0_wm] <- length(which(l6_form_sc0_wm == 2))
#     suml6_sc0_wm_postwo[index_sc0_wm] <- paste(suml6_sc0_wm_postwo[index_sc0_wm],sep = "")
#     l6_form_sc0_wm <- as.character(l6_form_sc0_wm)
#     l6_form_sc0_wm_flattened <- stri_paste(l6_form_sc0_wm,collapse = ',')
#     l6_form_sc0_wmsplitted <- (strsplit(as.character(l6_form_sc0_wm_flattened),",")[[1]])
#     final_sc0_wm[index_sc0_wm,index_sc0_wm_cols] <- l6_form_sc0_wmsplitted[index_sc0_wm_cols]
#   }
# }
# final_sc0_wm[is.na(final_sc0_wm)] <- ""
# sc0_winmarginmatrix <- cbind(sc0_teams,final_sc0_wm,suml6_sc0_wm,suml6_sc0_wm_negtwo,suml6_sc0_wm_negone,suml6_sc0_wm_zero,suml6_sc0_wm_posone,suml6_sc0_wm_postwo)
# ############################################################################################################################################################
# #sc1
# final_sc1_wm <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# suml6_sc1_wm <- c()
# suml6_sc1_wm_negone <- c()
# suml6_sc1_wm_negtwo <- c()
# suml6_sc1_wm_zero <- c()
# suml6_sc1_wm_posone <- c()
# suml6_sc1_wm_postwo <- c()
# l6_form_sc1_wmsplitted <- c()
# form_sc1_wm <- c()
# for(index_sc1_wm in 1:length(sc1_teams))
# {
#   for(index_sc1_wm_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_wm  <- row.names(sc1_winmargin_h) == sc1_teams[index_sc1_wm]
#     form_sc1_wm <- sc1_winmargin_h[index_sc1_wm ]
#     deleted_form_sc1_wm <- form_sc1_wm[!form_sc1_wm[] == ""]
#     l6_form_sc1_wm <- tail(deleted_form_sc1_wm,sc1_last_n_games)
#     l6_form_sc1_wm <- as.numeric(l6_form_sc1_wm)
#     suml6_sc1_wm[index_sc1_wm] <- sum(l6_form_sc1_wm)
#     suml6_sc1_wm[index_sc1_wm] <- paste(suml6_sc1_wm[index_sc1_wm],sep = "")
#     suml6_sc1_wm_negone[index_sc1_wm] <- length(which(l6_form_sc1_wm == -1))
#     suml6_sc1_wm_negone[index_sc1_wm] <- paste(suml6_sc1_wm_negone[index_sc1_wm],sep = "")
#     suml6_sc1_wm_negtwo[index_sc1_wm] <- length(which(l6_form_sc1_wm <= -2))
#     suml6_sc1_wm_negtwo[index_sc1_wm] <- paste(suml6_sc1_wm_negtwo[index_sc1_wm],sep = "")
#     suml6_sc1_wm_zero[index_sc1_wm] <- length(which(l6_form_sc1_wm == 0))
#     suml6_sc1_wm_zero[index_sc1_wm] <- paste(suml6_sc1_wm_zero[index_sc1_wm],sep = "")
#     suml6_sc1_wm_posone[index_sc1_wm] <- length(which(l6_form_sc1_wm == 1))
#     suml6_sc1_wm_posone[index_sc1_wm] <- paste(suml6_sc1_wm_posone[index_sc1_wm],sep = "")
#     suml6_sc1_wm_postwo[index_sc1_wm] <- length(which(l6_form_sc1_wm == 2))
#     suml6_sc1_wm_postwo[index_sc1_wm] <- paste(suml6_sc1_wm_postwo[index_sc1_wm],sep = "")
#     l6_form_sc1_wm <- as.character(l6_form_sc1_wm)
#     l6_form_sc1_wm_flattened <- stri_paste(l6_form_sc1_wm,collapse = ',')
#     l6_form_sc1_wmsplitted <- (strsplit(as.character(l6_form_sc1_wm_flattened),",")[[1]])
#     final_sc1_wm[index_sc1_wm,index_sc1_wm_cols] <- l6_form_sc1_wmsplitted[index_sc1_wm_cols]
#   }
# }
# final_sc1_wm[is.na(final_sc1_wm)] <- ""
# sc1_winmarginmatrix <- cbind(sc1_teams,final_sc1_wm,suml6_sc1_wm,suml6_sc1_wm_negtwo,suml6_sc1_wm_negone,suml6_sc1_wm_zero,suml6_sc1_wm_posone,suml6_sc1_wm_postwo)
# ###############################################################################################################################################################
# #sc2
# final_sc2_wm <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# suml6_sc2_wm <- c()
# suml6_sc2_wm_negone <- c()
# suml6_sc2_wm_negtwo <- c()
# suml6_sc2_wm_zero <- c()
# suml6_sc2_wm_posone <- c()
# suml6_sc2_wm_postwo <- c()
# l6_form_sc2_wmsplitted <- c()
# form_sc2_wm <- c()
# for(index_sc2_wm in 1:length(sc2_teams))
# {
#   for(index_sc2_wm_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_wm  <- row.names(sc2_winmargin_h) == sc2_teams[index_sc2_wm]
#     form_sc2_wm <- sc2_winmargin_h[index_sc2_wm ]
#     deleted_form_sc2_wm <- form_sc2_wm[!form_sc2_wm[] == ""]
#     l6_form_sc2_wm <- tail(deleted_form_sc2_wm,sc2_last_n_games)
#     l6_form_sc2_wm <- as.numeric(l6_form_sc2_wm)
#     suml6_sc2_wm[index_sc2_wm] <- sum(l6_form_sc2_wm)
#     suml6_sc2_wm[index_sc2_wm] <- paste(suml6_sc2_wm[index_sc2_wm],sep = "")
#     suml6_sc2_wm_negone[index_sc2_wm] <- length(which(l6_form_sc2_wm == -1))
#     suml6_sc2_wm_negone[index_sc2_wm] <- paste(suml6_sc2_wm_negone[index_sc2_wm],sep = "")
#     suml6_sc2_wm_negtwo[index_sc2_wm] <- length(which(l6_form_sc2_wm <= -2))
#     suml6_sc2_wm_negtwo[index_sc2_wm] <- paste(suml6_sc2_wm_negtwo[index_sc2_wm],sep = "")
#     suml6_sc2_wm_zero[index_sc2_wm] <- length(which(l6_form_sc2_wm == 0))
#     suml6_sc2_wm_zero[index_sc2_wm] <- paste(suml6_sc2_wm_zero[index_sc2_wm],sep = "")
#     suml6_sc2_wm_posone[index_sc2_wm] <- length(which(l6_form_sc2_wm == 1))
#     suml6_sc2_wm_posone[index_sc2_wm] <- paste(suml6_sc2_wm_posone[index_sc2_wm],sep = "")
#     suml6_sc2_wm_postwo[index_sc2_wm] <- length(which(l6_form_sc2_wm == 2))
#     suml6_sc2_wm_postwo[index_sc2_wm] <- paste(suml6_sc2_wm_postwo[index_sc2_wm],sep = "")
#     l6_form_sc2_wm <- as.character(l6_form_sc2_wm)
#     l6_form_sc2_wm_flattened <- stri_paste(l6_form_sc2_wm,collapse = ',')
#     l6_form_sc2_wmsplitted <- (strsplit(as.character(l6_form_sc2_wm_flattened),",")[[1]])
#     final_sc2_wm[index_sc2_wm,index_sc2_wm_cols] <- l6_form_sc2_wmsplitted[index_sc2_wm_cols]
#   }
# }
# final_sc2_wm[is.na(final_sc2_wm)] <- ""
# sc2_winmarginmatrix <- cbind(sc2_teams,final_sc2_wm,suml6_sc2_wm,suml6_sc2_wm_negtwo,suml6_sc2_wm_negone,suml6_sc2_wm_zero,suml6_sc2_wm_posone,suml6_sc2_wm_postwo)
# ############################################################################################################################################################
# #sc3
# final_sc3_wm <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# suml6_sc3_wm <- c()
# suml6_sc3_wm_negone <- c()
# suml6_sc3_wm_negtwo <- c()
# suml6_sc3_wm_zero <- c()
# suml6_sc3_wm_posone <- c()
# suml6_sc3_wm_postwo <- c()
# l6_form_sc3_wmsplitted <- c()
# form_sc3_wm <- c()
# for(index_sc3_wm in 1:length(sc3_teams))
# {
#   for(index_sc3_wm_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_wm  <- row.names(sc3_winmargin_h) == sc3_teams[index_sc3_wm]
#     form_sc3_wm <- sc3_winmargin_h[index_sc3_wm ]
#     deleted_form_sc3_wm <- form_sc3_wm[!form_sc3_wm[] == ""]
#     l6_form_sc3_wm <- tail(deleted_form_sc3_wm,sc3_last_n_games)
#     l6_form_sc3_wm <- as.numeric(l6_form_sc3_wm)
#     suml6_sc3_wm[index_sc3_wm] <- sum(l6_form_sc3_wm)
#     suml6_sc3_wm[index_sc3_wm] <- paste(suml6_sc3_wm[index_sc3_wm],sep = "")
#     suml6_sc3_wm_negone[index_sc3_wm] <- length(which(l6_form_sc3_wm == -1))
#     suml6_sc3_wm_negone[index_sc3_wm] <- paste(suml6_sc3_wm_negone[index_sc3_wm],sep = "")
#     suml6_sc3_wm_negtwo[index_sc3_wm] <- length(which(l6_form_sc3_wm <= -2))
#     suml6_sc3_wm_negtwo[index_sc3_wm] <- paste(suml6_sc3_wm_negtwo[index_sc3_wm],sep = "")
#     suml6_sc3_wm_zero[index_sc3_wm] <- length(which(l6_form_sc3_wm == 0))
#     suml6_sc3_wm_zero[index_sc3_wm] <- paste(suml6_sc3_wm_zero[index_sc3_wm],sep = "")
#     suml6_sc3_wm_posone[index_sc3_wm] <- length(which(l6_form_sc3_wm == 1))
#     suml6_sc3_wm_posone[index_sc3_wm] <- paste(suml6_sc3_wm_posone[index_sc3_wm],sep = "")
#     suml6_sc3_wm_postwo[index_sc3_wm] <- length(which(l6_form_sc3_wm == 2))
#     suml6_sc3_wm_postwo[index_sc3_wm] <- paste(suml6_sc3_wm_postwo[index_sc3_wm],sep = "")
#     l6_form_sc3_wm <- as.character(l6_form_sc3_wm)
#     l6_form_sc3_wm_flattened <- stri_paste(l6_form_sc3_wm,collapse = ',')
#     l6_form_sc3_wmsplitted <- (strsplit(as.character(l6_form_sc3_wm_flattened),",")[[1]])
#     final_sc3_wm[index_sc3_wm,index_sc3_wm_cols] <- l6_form_sc3_wmsplitted[index_sc3_wm_cols]
#   }
# }
# final_sc3_wm[is.na(final_sc3_wm)] <- ""
# sc3_winmarginmatrix <- cbind(sc3_teams,final_sc3_wm,suml6_sc3_wm,suml6_sc3_wm_negtwo,suml6_sc3_wm_negone,suml6_sc3_wm_zero,suml6_sc3_wm_posone,suml6_sc3_wm_postwo)
# ###########################################################################################################################################################
# #t1
# final_t1_wm <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# suml6_t1_wm <- c()
# suml6_t1_wm_negone <- c()
# suml6_t1_wm_negtwo <- c()
# suml6_t1_wm_zero <- c()
# suml6_t1_wm_posone <- c()
# suml6_t1_wm_postwo <- c()
# l6_form_t1_wmsplitted <- c()
# form_t1_wm <- c()
# for(index_t1_wm in 1:length(t1_teams))
# {
#   for(index_t1_wm_cols in 1:t1_totalrounds)
#   {
#     index_t1_wm  <- row.names(t1_winmargin_h) == t1_teams[index_t1_wm]
#     form_t1_wm <- t1_winmargin_h[index_t1_wm ]
#     deleted_form_t1_wm <- form_t1_wm[!form_t1_wm[] == ""]
#     l6_form_t1_wm <- tail(deleted_form_t1_wm,t1_last_n_games)
#     l6_form_t1_wm <- as.numeric(l6_form_t1_wm)
#     suml6_t1_wm[index_t1_wm] <- sum(l6_form_t1_wm)
#     suml6_t1_wm[index_t1_wm] <- paste(suml6_t1_wm[index_t1_wm],sep = "")
#     suml6_t1_wm_negone[index_t1_wm] <- length(which(l6_form_t1_wm == -1))
#     suml6_t1_wm_negone[index_t1_wm] <- paste(suml6_t1_wm_negone[index_t1_wm],sep = "")
#     suml6_t1_wm_negtwo[index_t1_wm] <- length(which(l6_form_t1_wm <= -2))
#     suml6_t1_wm_negtwo[index_t1_wm] <- paste(suml6_t1_wm_negtwo[index_t1_wm],sep = "")
#     suml6_t1_wm_zero[index_t1_wm] <- length(which(l6_form_t1_wm == 0))
#     suml6_t1_wm_zero[index_t1_wm] <- paste(suml6_t1_wm_zero[index_t1_wm],sep = "")
#     suml6_t1_wm_posone[index_t1_wm] <- length(which(l6_form_t1_wm == 1))
#     suml6_t1_wm_posone[index_t1_wm] <- paste(suml6_t1_wm_posone[index_t1_wm],sep = "")
#     suml6_t1_wm_postwo[index_t1_wm] <- length(which(l6_form_t1_wm == 2))
#     suml6_t1_wm_postwo[index_t1_wm] <- paste(suml6_t1_wm_postwo[index_t1_wm],sep = "")
#     l6_form_t1_wm <- as.character(l6_form_t1_wm)
#     l6_form_t1_wm_flattened <- stri_paste(l6_form_t1_wm,collapse = ',')
#     l6_form_t1_wmsplitted <- (strsplit(as.character(l6_form_t1_wm_flattened),",")[[1]])
#     final_t1_wm[index_t1_wm,index_t1_wm_cols] <- l6_form_t1_wmsplitted[index_t1_wm_cols]
#   }
# }
# final_t1_wm[is.na(final_t1_wm)] <- ""
# t1_winmarginmatrix <- cbind(t1_teams,final_t1_wm,suml6_t1_wm,suml6_t1_wm_negtwo,suml6_t1_wm_negone,suml6_t1_wm_zero,suml6_t1_wm_posone,suml6_t1_wm_postwo)
###########################################################################################################################################################





































