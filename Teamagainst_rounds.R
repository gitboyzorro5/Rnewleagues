Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)

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


final_mls_teamagainst <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
l6_form_mls_teamagainstsplitted <- c()
form_mls_teamagainst <- c()
for(index_mls_teamagainst in 1:length(mls_teams))
{
  for(index_mls_teamagainst_cols in 1:mls_totalrounds)
  {
    index_mls_teamagainst  <- row.names(mls_form_team_against_h) == mls_teams[index_mls_teamagainst]
    form_mls_teamagainst <- mls_form_team_against_h[index_mls_teamagainst ]
    deleted_form_mls_teamagainst <- form_mls_teamagainst[!form_mls_teamagainst[] == ""]
    l6_form_mls_teamagainst <- tail(deleted_form_mls_teamagainst,mls_last_n_games)
    l6_form_mls_teamagainst <- as.character(l6_form_mls_teamagainst)
    #l6_form_mls_teamagainst_flattened <- stri_paste(l6_form_mls_teamagainst,collapse = '')
    #l6_form_mls_teamagainstsplitted <- strsplit(l6_form_mls_teamagainst_flattened,"")[[1]]
    final_mls_teamagainst[index_mls_teamagainst,index_mls_teamagainst_cols] <- l6_form_mls_teamagainst[index_mls_teamagainst_cols]
  }
}

mls_teamagainst <- cbind(mls_teams,final_mls_teamagainst)
write.xlsx(mls_teamagainst,"Divisions/MLS.xlsx",append = T , sheetName = "Teamagainst")
###########################################################################################################################################
final_arg_teamagainst <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
l6_form_arg_teamagainstsplitted <- c()
form_arg_teamagainst <- c()
for(index_arg_teamagainst in 1:length(arg_teams))
{
  for(index_arg_teamagainst_cols in 1:arg_totalrounds)
  {
    index_arg_teamagainst  <- row.names(arg_form_team_against_h) == arg_teams[index_arg_teamagainst]
    form_arg_teamagainst <- arg_form_team_against_h[index_arg_teamagainst ]
    deleted_form_arg_teamagainst <- form_arg_teamagainst[!form_arg_teamagainst[] == ""]
    l6_form_arg_teamagainst <- tail(deleted_form_arg_teamagainst,arg_last_n_games)
    l6_form_arg_teamagainst <- as.character(l6_form_arg_teamagainst)
    #l6_form_arg_teamagainst_flattened <- stri_paste(l6_form_arg_teamagainst,collapse = '')
    #l6_form_arg_teamagainstsplitted <- strsplit(l6_form_arg_teamagainst_flattened,"")[[1]]
    final_arg_teamagainst[index_arg_teamagainst,index_arg_teamagainst_cols] <- l6_form_arg_teamagainst[index_arg_teamagainst_cols]
  }
}

arg_teamagainst <- cbind(arg_teams,final_arg_teamagainst)
write.xlsx(arg_teamagainst,"Divisions/ARG.xlsx",append = T , sheetName = "Teamagainst")
##############################################################################################################################################
# final_d2_teamagainst <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# l6_form_d2_teamagainstsplitted <- c()
# form_d2_teamagainst <- c()
# for(index_d2_teamagainst in 1:length(d2_teams))
# {
#   for(index_d2_teamagainst_cols in 1:d2_totalrounds)
#   {
#     index_d2_teamagainst  <- row.names(d2_form_team_against_h) == d2_teams[index_d2_teamagainst]
#     form_d2_teamagainst <- d2_form_team_against_h[index_d2_teamagainst ]
#     deleted_form_d2_teamagainst <- form_d2_teamagainst[!form_d2_teamagainst[] == ""]
#     l6_form_d2_teamagainst <- tail(deleted_form_d2_teamagainst,d2_last_n_games)
#     l6_form_d2_teamagainst <- as.character(l6_form_d2_teamagainst)
#     #l6_form_d2_teamagainst_flattened <- stri_paste(l6_form_d2_teamagainst,collapse = '')
#     #l6_form_d2_teamagainstsplitted <- strsplit(l6_form_d2_teamagainst_flattened,"")[[1]]
#     final_d2_teamagainst[index_d2_teamagainst,index_d2_teamagainst_cols] <- l6_form_d2_teamagainst[index_d2_teamagainst_cols]
#   }
# }
#
# d2_teamagainst <- cbind(d2_teams,final_d2_teamagainst)
# write.xlsx(d2_teamagainst,"Divisions/D2.xlsx",append = T , sheetName = "Teamagainst")
# #####################################################################################################################################################
# final_e0_teamagainst <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# l6_form_e0_teamagainstsplitted <- c()
# form_e0_teamagainst <- c()
# for(index_e0_teamagainst in 1:length(e0_teams))
# {
#   for(index_e0_teamagainst_cols in 1:e0_totalrounds)
#   {
#     index_e0_teamagainst  <- row.names(e0_form_team_against_h) == e0_teams[index_e0_teamagainst]
#     form_e0_teamagainst <- e0_form_team_against_h[index_e0_teamagainst ]
#     deleted_form_e0_teamagainst <- form_e0_teamagainst[!form_e0_teamagainst[] == ""]
#     l6_form_e0_teamagainst <- tail(deleted_form_e0_teamagainst,e0_last_n_games)
#     l6_form_e0_teamagainst <- as.character(l6_form_e0_teamagainst)
#     #l6_form_e0_teamagainst_flattened <- stri_paste(l6_form_e0_teamagainst,collapse = '')
#     #l6_form_e0_teamagainstsplitted <- strsplit(l6_form_e0_teamagainst_flattened,"")[[1]]
#     final_e0_teamagainst[index_e0_teamagainst,index_e0_teamagainst_cols] <- l6_form_e0_teamagainst[index_e0_teamagainst_cols]
#   }
# }
#
# e0_teamagainst <- cbind(e0_teams,final_e0_teamagainst)
# write.xlsx(e0_teamagainst,"Divisions/E0.xlsx",append = T , sheetName = "Teamagainst")
# #######################################################################################################################################################
# final_e1_teamagainst <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# l6_form_e1_teamagainstsplitted <- c()
# form_e1_teamagainst <- c()
# for(index_e1_teamagainst in 1:length(e1_teams))
# {
#   for(index_e1_teamagainst_cols in 1:e1_totalrounds)
#   {
#     index_e1_teamagainst  <- row.names(e1_form_team_against_h) == e1_teams[index_e1_teamagainst]
#     form_e1_teamagainst <- e1_form_team_against_h[index_e1_teamagainst ]
#     deleted_form_e1_teamagainst <- form_e1_teamagainst[!form_e1_teamagainst[] == ""]
#     l6_form_e1_teamagainst <- tail(deleted_form_e1_teamagainst,e1_last_n_games)
#     l6_form_e1_teamagainst <- as.character(l6_form_e1_teamagainst)
#     #l6_form_e1_teamagainst_flattened <- stri_paste(l6_form_e1_teamagainst,collapse = '')
#     #l6_form_e1_teamagainstsplitted <- strsplit(l6_form_e1_teamagainst_flattened,"")[[1]]
#     final_e1_teamagainst[index_e1_teamagainst,index_e1_teamagainst_cols] <- l6_form_e1_teamagainst[index_e1_teamagainst_cols]
#   }
# }
#
# e1_teamagainst <- cbind(e1_teams,final_e1_teamagainst)
# write.xlsx(e1_teamagainst,"Divisions/E1.xlsx",append = T , sheetName = "Teamagainst")
# ###########################################################################################################################################################
# final_e2_teamagainst <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# l6_form_e2_teamagainstsplitted <- c()
# form_e2_teamagainst <- c()
# for(index_e2_teamagainst in 1:length(e2_teams))
# {
#   for(index_e2_teamagainst_cols in 1:e2_totalrounds)
#   {
#     index_e2_teamagainst  <- row.names(e2_form_team_against_h) == e2_teams[index_e2_teamagainst]
#     form_e2_teamagainst <- e2_form_team_against_h[index_e2_teamagainst ]
#     deleted_form_e2_teamagainst <- form_e2_teamagainst[!form_e2_teamagainst[] == ""]
#     l6_form_e2_teamagainst <- tail(deleted_form_e2_teamagainst,e2_last_n_games)
#     l6_form_e2_teamagainst <- as.character(l6_form_e2_teamagainst)
#     #l6_form_e2_teamagainst_flattened <- stri_paste(l6_form_e2_teamagainst,collapse = '')
#     #l6_form_e2_teamagainstsplitted <- strsplit(l6_form_e2_teamagainst_flattened,"")[[1]]
#     final_e2_teamagainst[index_e2_teamagainst,index_e2_teamagainst_cols] <- l6_form_e2_teamagainst[index_e2_teamagainst_cols]
#   }
# }
#
# e2_teamagainst <- cbind(e2_teams,final_e2_teamagainst)
# write.xlsx(e2_teamagainst,"Divisions/E2.xlsx",append = T , sheetName = "Teamagainst")
# ###############################################################################################################################################################
# final_e3_teamagainst <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# l6_form_e3_teamagainstsplitted <- c()
# form_e3_teamagainst <- c()
# for(index_e3_teamagainst in 1:length(e3_teams))
# {
#   for(index_e3_teamagainst_cols in 1:e3_totalrounds)
#   {
#     index_e3_teamagainst  <- row.names(e3_form_team_against_h) == e3_teams[index_e3_teamagainst]
#     form_e3_teamagainst <- e3_form_team_against_h[index_e3_teamagainst ]
#     deleted_form_e3_teamagainst <- form_e3_teamagainst[!form_e3_teamagainst[] == ""]
#     l6_form_e3_teamagainst <- tail(deleted_form_e3_teamagainst,e3_last_n_games)
#     l6_form_e3_teamagainst <- as.character(l6_form_e3_teamagainst)
#     #l6_form_e3_teamagainst_flattened <- stri_paste(l6_form_e3_teamagainst,collapse = '')
#     #l6_form_e3_teamagainstsplitted <- strsplit(l6_form_e3_teamagainst_flattened,"")[[1]]
#     final_e3_teamagainst[index_e3_teamagainst,index_e3_teamagainst_cols] <- l6_form_e3_teamagainst[index_e3_teamagainst_cols]
#   }
# }
#
# e3_teamagainst <- cbind(e3_teams,final_e3_teamagainst)
# write.xlsx(e3_teamagainst,"Divisions/E3.xlsx",append = T , sheetName = "Teamagainst")
# ###################################################################################################################################
# final_ec_teamagainst <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# l6_form_ec_teamagainstsplitted <- c()
# form_ec_teamagainst <- c()
# for(index_ec_teamagainst in 1:length(ec_teams))
# {
#   for(index_ec_teamagainst_cols in 1:ec_totalrounds)
#   {
#     index_ec_teamagainst  <- row.names(ec_form_team_against_h) == ec_teams[index_ec_teamagainst]
#     form_ec_teamagainst <- ec_form_team_against_h[index_ec_teamagainst ]
#     deleted_form_ec_teamagainst <- form_ec_teamagainst[!form_ec_teamagainst[] == ""]
#     l6_form_ec_teamagainst <- tail(deleted_form_ec_teamagainst,ec_last_n_games)
#     l6_form_ec_teamagainst <- as.character(l6_form_ec_teamagainst)
#     #l6_form_ec_teamagainst_flattened <- stri_paste(l6_form_ec_teamagainst,collapse = '')
#     #l6_form_ec_teamagainstsplitted <- strsplit(l6_form_ec_teamagainst_flattened,"")[[1]]
#     final_ec_teamagainst[index_ec_teamagainst,index_ec_teamagainst_cols] <- l6_form_ec_teamagainst[index_ec_teamagainst_cols]
#   }
# }
#
# ec_teamagainst <- cbind(ec_teams,final_ec_teamagainst)
# write.xlsx(ec_teamagainst,"Divisions/EC.xlsx",append = T , sheetName = "Teamagainst")
# ########################################################################################################################################
# final_f1_teamagainst <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# l6_form_f1_teamagainstsplitted <- c()
# form_f1_teamagainst <- c()
# for(index_f1_teamagainst in 1:length(f1_teams))
# {
#   for(index_f1_teamagainst_cols in 1:f1_totalrounds)
#   {
#     index_f1_teamagainst  <- row.names(f1_form_team_against_h) == f1_teams[index_f1_teamagainst]
#     form_f1_teamagainst <- f1_form_team_against_h[index_f1_teamagainst ]
#     deleted_form_f1_teamagainst <- form_f1_teamagainst[!form_f1_teamagainst[] == ""]
#     l6_form_f1_teamagainst <- tail(deleted_form_f1_teamagainst,f1_last_n_games)
#     l6_form_f1_teamagainst <- as.character(l6_form_f1_teamagainst)
#     #l6_form_f1_teamagainst_flattened <- stri_paste(l6_form_f1_teamagainst,collapse = '')
#     #l6_form_f1_teamagainstsplitted <- strsplit(l6_form_f1_teamagainst_flattened,"")[[1]]
#     final_f1_teamagainst[index_f1_teamagainst,index_f1_teamagainst_cols] <- l6_form_f1_teamagainst[index_f1_teamagainst_cols]
#   }
# }
#
# f1_teamagainst <- cbind(f1_teams,final_f1_teamagainst)
# write.xlsx(f1_teamagainst,"Divisions/F1.xlsx",append = T , sheetName = "Teamagainst")
# ##########################################################################################################################################
# final_f2_teamagainst <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# l6_form_f2_teamagainstsplitted <- c()
# form_f2_teamagainst <- c()
# for(index_f2_teamagainst in 1:length(f2_teams))
# {
#   for(index_f2_teamagainst_cols in 1:f2_totalrounds)
#   {
#     index_f2_teamagainst  <- row.names(f2_form_team_against_h) == f2_teams[index_f2_teamagainst]
#     form_f2_teamagainst <- f2_form_team_against_h[index_f2_teamagainst ]
#     deleted_form_f2_teamagainst <- form_f2_teamagainst[!form_f2_teamagainst[] == ""]
#     l6_form_f2_teamagainst <- tail(deleted_form_f2_teamagainst,f2_last_n_games)
#     l6_form_f2_teamagainst <- as.character(l6_form_f2_teamagainst)
#     #l6_form_f2_teamagainst_flattened <- stri_paste(l6_form_f2_teamagainst,collapse = '')
#     #l6_form_f2_teamagainstsplitted <- strsplit(l6_form_f2_teamagainst_flattened,"")[[1]]
#     final_f2_teamagainst[index_f2_teamagainst,index_f2_teamagainst_cols] <- l6_form_f2_teamagainst[index_f2_teamagainst_cols]
#   }
# }
#
# f2_teamagainst <- cbind(f2_teams,final_f2_teamagainst)
# write.xlsx(f2_teamagainst,"Divisions/F2.xlsx",append = T , sheetName = "Teamagainst")
# ########################################################################################################################################
# final_g1_teamagainst <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# l6_form_g1_teamagainstsplitted <- c()
# form_g1_teamagainst <- c()
# for(index_g1_teamagainst in 1:length(g1_teams))
# {
#   for(index_g1_teamagainst_cols in 1:g1_totalrounds)
#   {
#     index_g1_teamagainst  <- row.names(g1_form_team_against_h) == g1_teams[index_g1_teamagainst]
#     form_g1_teamagainst <- g1_form_team_against_h[index_g1_teamagainst ]
#     deleted_form_g1_teamagainst <- form_g1_teamagainst[!form_g1_teamagainst[] == ""]
#     l6_form_g1_teamagainst <- tail(deleted_form_g1_teamagainst,g1_last_n_games)
#     l6_form_g1_teamagainst <- as.character(l6_form_g1_teamagainst)
#     #l6_form_g1_teamagainst_flattened <- stri_paste(l6_form_g1_teamagainst,collapse = '')
#     #l6_form_g1_teamagainstsplitted <- strsplit(l6_form_g1_teamagainst_flattened,"")[[1]]
#     final_g1_teamagainst[index_g1_teamagainst,index_g1_teamagainst_cols] <- l6_form_g1_teamagainst[index_g1_teamagainst_cols]
#   }
# }
#
# g1_teamagainst <- cbind(g1_teams,final_g1_teamagainst)
# write.xlsx(g1_teamagainst,"Divisions/G1.xlsx",append = T , sheetName = "Teamagainst")
# ############################################################################################################################################
# final_i1_teamagainst <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# l6_form_i1_teamagainstsplitted <- c()
# form_i1_teamagainst <- c()
# for(index_i1_teamagainst in 1:length(i1_teams))
# {
#   for(index_i1_teamagainst_cols in 1:i1_totalrounds)
#   {
#     index_i1_teamagainst  <- row.names(i1_form_team_against_h) == i1_teams[index_i1_teamagainst]
#     form_i1_teamagainst <- i1_form_team_against_h[index_i1_teamagainst ]
#     deleted_form_i1_teamagainst <- form_i1_teamagainst[!form_i1_teamagainst[] == ""]
#     l6_form_i1_teamagainst <- tail(deleted_form_i1_teamagainst,i1_last_n_games)
#     l6_form_i1_teamagainst <- as.character(l6_form_i1_teamagainst)
#     #l6_form_i1_teamagainst_flattened <- stri_paste(l6_form_i1_teamagainst,collapse = '')
#     #l6_form_i1_teamagainstsplitted <- strsplit(l6_form_i1_teamagainst_flattened,"")[[1]]
#     final_i1_teamagainst[index_i1_teamagainst,index_i1_teamagainst_cols] <- l6_form_i1_teamagainst[index_i1_teamagainst_cols]
#   }
# }
#
# i1_teamagainst <- cbind(i1_teams,final_i1_teamagainst)
# write.xlsx(i1_teamagainst,"Divisions/I1.xlsx",append = T , sheetName = "Teamagainst")
# #############################################################################################################################################
# final_i2_teamagainst <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# l6_form_i2_teamagainstsplitted <- c()
# form_i2_teamagainst <- c()
# for(index_i2_teamagainst in 1:length(i2_teams))
# {
#   for(index_i2_teamagainst_cols in 1:i2_totalrounds)
#   {
#     index_i2_teamagainst  <- row.names(i2_form_team_against_h) == i2_teams[index_i2_teamagainst]
#     form_i2_teamagainst <- i2_form_team_against_h[index_i2_teamagainst ]
#     deleted_form_i2_teamagainst <- form_i2_teamagainst[!form_i2_teamagainst[] == ""]
#     l6_form_i2_teamagainst <- tail(deleted_form_i2_teamagainst,i2_last_n_games)
#     l6_form_i2_teamagainst <- as.character(l6_form_i2_teamagainst)
#     #l6_form_i2_teamagainst_flattened <- stri_paste(l6_form_i2_teamagainst,collapse = '')
#     #l6_form_i2_teamagainstsplitted <- strsplit(l6_form_i2_teamagainst_flattened,"")[[1]]
#     final_i2_teamagainst[index_i2_teamagainst,index_i2_teamagainst_cols] <- l6_form_i2_teamagainst[index_i2_teamagainst_cols]
#   }
# }
#
# i2_teamagainst <- cbind(i2_teams,final_i2_teamagainst)
# write.xlsx(i2_teamagainst,"Divisions/I2.xlsx",append = T , sheetName = "Teamagainst")
# ###############################################################################################################################################
# final_n1_teamagainst <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# l6_form_n1_teamagainstsplitted <- c()
# form_n1_teamagainst <- c()
# for(index_n1_teamagainst in 1:length(n1_teams))
# {
#   for(index_n1_teamagainst_cols in 1:n1_totalrounds)
#   {
#     index_n1_teamagainst  <- row.names(n1_form_team_against_h) == n1_teams[index_n1_teamagainst]
#     form_n1_teamagainst <- n1_form_team_against_h[index_n1_teamagainst ]
#     deleted_form_n1_teamagainst <- form_n1_teamagainst[!form_n1_teamagainst[] == ""]
#     l6_form_n1_teamagainst <- tail(deleted_form_n1_teamagainst,n1_last_n_games)
#     l6_form_n1_teamagainst <- as.character(l6_form_n1_teamagainst)
#     #l6_form_n1_teamagainst_flattened <- stri_paste(l6_form_n1_teamagainst,collapse = '')
#     #l6_form_n1_teamagainstsplitted <- strsplit(l6_form_n1_teamagainst_flattened,"")[[1]]
#     final_n1_teamagainst[index_n1_teamagainst,index_n1_teamagainst_cols] <- l6_form_n1_teamagainst[index_n1_teamagainst_cols]
#   }
# }
#
# n1_teamagainst <- cbind(n1_teams,final_n1_teamagainst)
# write.xlsx(n1_teamagainst,"Divisions/N1.xlsx",append = T , sheetName = "Teamagainst")
# ##################################################################################################################################################
# final_p1_teamagainst <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# l6_form_p1_teamagainstsplitted <- c()
# form_p1_teamagainst <- c()
# for(index_p1_teamagainst in 1:length(p1_teams))
# {
#   for(index_p1_teamagainst_cols in 1:p1_totalrounds)
#   {
#     index_p1_teamagainst  <- row.names(p1_form_team_against_h) == p1_teams[index_p1_teamagainst]
#     form_p1_teamagainst <- p1_form_team_against_h[index_p1_teamagainst ]
#     deleted_form_p1_teamagainst <- form_p1_teamagainst[!form_p1_teamagainst[] == ""]
#     l6_form_p1_teamagainst <- tail(deleted_form_p1_teamagainst,p1_last_n_games)
#     l6_form_p1_teamagainst <- as.character(l6_form_p1_teamagainst)
#     #l6_form_p1_teamagainst_flattened <- stri_paste(l6_form_p1_teamagainst,collapse = '')
#     #l6_form_p1_teamagainstsplitted <- strsplit(l6_form_p1_teamagainst_flattened,"")[[1]]
#     final_p1_teamagainst[index_p1_teamagainst,index_p1_teamagainst_cols] <- l6_form_p1_teamagainst[index_p1_teamagainst_cols]
#   }
# }
#
# p1_teamagainst <- cbind(p1_teams,final_p1_teamagainst)
# write.xlsx(p1_teamagainst,"Divisions/P1.xlsx",append = T , sheetName = "Teamagainst")
# ######################################################################################################################################################
# final_sc0_teamagainst <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# l6_form_sc0_teamagainstsplitted <- c()
# form_sc0_teamagainst <- c()
# for(index_sc0_teamagainst in 1:length(sc0_teams))
# {
#   for(index_sc0_teamagainst_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_teamagainst  <- row.names(sc0_form_team_against_h) == sc0_teams[index_sc0_teamagainst]
#     form_sc0_teamagainst <- sc0_form_team_against_h[index_sc0_teamagainst ]
#     deleted_form_sc0_teamagainst <- form_sc0_teamagainst[!form_sc0_teamagainst[] == ""]
#     l6_form_sc0_teamagainst <- tail(deleted_form_sc0_teamagainst,sc0_last_n_games)
#     l6_form_sc0_teamagainst <- as.character(l6_form_sc0_teamagainst)
#     #l6_form_sc0_teamagainst_flattened <- stri_paste(l6_form_sc0_teamagainst,collapse = '')
#     #l6_form_sc0_teamagainstsplitted <- strsplit(l6_form_sc0_teamagainst_flattened,"")[[1]]
#     final_sc0_teamagainst[index_sc0_teamagainst,index_sc0_teamagainst_cols] <- l6_form_sc0_teamagainst[index_sc0_teamagainst_cols]
#   }
# }
#
# sc0_teamagainst <- cbind(sc0_teams,final_sc0_teamagainst)
# write.xlsx(sc0_teamagainst,"Divisions/SC0.xlsx",append = T , sheetName = "Teamagainst")
# ################################################################################################################################################
# final_sc1_teamagainst <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# l6_form_sc1_teamagainstsplitted <- c()
# form_sc1_teamagainst <- c()
# for(index_sc1_teamagainst in 1:length(sc1_teams))
# {
#   for(index_sc1_teamagainst_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_teamagainst  <- row.names(sc1_form_team_against_h) == sc1_teams[index_sc1_teamagainst]
#     form_sc1_teamagainst <- sc1_form_team_against_h[index_sc1_teamagainst ]
#     deleted_form_sc1_teamagainst <- form_sc1_teamagainst[!form_sc1_teamagainst[] == ""]
#     l6_form_sc1_teamagainst <- tail(deleted_form_sc1_teamagainst,sc1_last_n_games)
#     l6_form_sc1_teamagainst <- as.character(l6_form_sc1_teamagainst)
#     #l6_form_sc1_teamagainst_flattened <- stri_paste(l6_form_sc1_teamagainst,collapse = '')
#     #l6_form_sc1_teamagainstsplitted <- strsplit(l6_form_sc1_teamagainst_flattened,"")[[1]]
#     final_sc1_teamagainst[index_sc1_teamagainst,index_sc1_teamagainst_cols] <- l6_form_sc1_teamagainst[index_sc1_teamagainst_cols]
#   }
# }
#
# sc1_teamagainst <- cbind(sc1_teams,final_sc1_teamagainst)
# write.xlsx(sc1_teamagainst,"Divisions/SC1.xlsx",append = T , sheetName = "Teamagainst")
# #################################################################################################################################################
# final_sc2_teamagainst <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# l6_form_sc2_teamagainstsplitted <- c()
# form_sc2_teamagainst <- c()
# for(index_sc2_teamagainst in 1:length(sc2_teams))
# {
#   for(index_sc2_teamagainst_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_teamagainst  <- row.names(sc2_form_team_against_h) == sc2_teams[index_sc2_teamagainst]
#     form_sc2_teamagainst <- sc2_form_team_against_h[index_sc2_teamagainst ]
#     deleted_form_sc2_teamagainst <- form_sc2_teamagainst[!form_sc2_teamagainst[] == ""]
#     l6_form_sc2_teamagainst <- tail(deleted_form_sc2_teamagainst,sc2_last_n_games)
#     l6_form_sc2_teamagainst <- as.character(l6_form_sc2_teamagainst)
#     #l6_form_sc2_teamagainst_flattened <- stri_paste(l6_form_sc2_teamagainst,collapse = '')
#     #l6_form_sc2_teamagainstsplitted <- strsplit(l6_form_sc2_teamagainst_flattened,"")[[1]]
#     final_sc2_teamagainst[index_sc2_teamagainst,index_sc2_teamagainst_cols] <- l6_form_sc2_teamagainst[index_sc2_teamagainst_cols]
#   }
# }
#
# sc2_teamagainst <- cbind(sc2_teams,final_sc2_teamagainst)
# write.xlsx(sc2_teamagainst,"Divisions/SC2.xlsx",append = T , sheetName = "Teamagainst")
# ##################################################################################################################################
# final_sc3_teamagainst <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# l6_form_sc3_teamagainstsplitted <- c()
# form_sc3_teamagainst <- c()
# for(index_sc3_teamagainst in 1:length(sc3_teams))
# {
#   for(index_sc3_teamagainst_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_teamagainst  <- row.names(sc3_form_team_against_h) == sc3_teams[index_sc3_teamagainst]
#     form_sc3_teamagainst <- sc3_form_team_against_h[index_sc3_teamagainst ]
#     deleted_form_sc3_teamagainst <- form_sc3_teamagainst[!form_sc3_teamagainst[] == ""]
#     l6_form_sc3_teamagainst <- tail(deleted_form_sc3_teamagainst,sc3_last_n_games)
#     l6_form_sc3_teamagainst <- as.character(l6_form_sc3_teamagainst)
#     #l6_form_sc3_teamagainst_flattened <- stri_paste(l6_form_sc3_teamagainst,collapse = '')
#     #l6_form_sc3_teamagainstsplitted <- strsplit(l6_form_sc3_teamagainst_flattened,"")[[1]]
#     final_sc3_teamagainst[index_sc3_teamagainst,index_sc3_teamagainst_cols] <- l6_form_sc3_teamagainst[index_sc3_teamagainst_cols]
#   }
# }
#
# sc3_teamagainst <- cbind(sc3_teams,final_sc3_teamagainst)
# write.xlsx(sc3_teamagainst,"Divisions/SC3.xlsx",append = T , sheetName = "Teamagainst")
# #####################################################################################################################################
# final_sp1_teamagainst <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# l6_form_sp1_teamagainstsplitted <- c()
# form_sp1_teamagainst <- c()
# for(index_sp1_teamagainst in 1:length(sp1_teams))
# {
#   for(index_sp1_teamagainst_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_teamagainst  <- row.names(sp1_form_team_against_h) == sp1_teams[index_sp1_teamagainst]
#     form_sp1_teamagainst <- sp1_form_team_against_h[index_sp1_teamagainst ]
#     deleted_form_sp1_teamagainst <- form_sp1_teamagainst[!form_sp1_teamagainst[] == ""]
#     l6_form_sp1_teamagainst <- tail(deleted_form_sp1_teamagainst,sp1_last_n_games)
#     l6_form_sp1_teamagainst <- as.character(l6_form_sp1_teamagainst)
#     #l6_form_sp1_teamagainst_flattened <- stri_paste(l6_form_sp1_teamagainst,collapse = '')
#     #l6_form_sp1_teamagainstsplitted <- strsplit(l6_form_sp1_teamagainst_flattened,"")[[1]]
#     final_sp1_teamagainst[index_sp1_teamagainst,index_sp1_teamagainst_cols] <- l6_form_sp1_teamagainst[index_sp1_teamagainst_cols]
#   }
# }
#
# sp1_teamagainst <- cbind(sp1_teams,final_sp1_teamagainst)
# write.xlsx(sp1_teamagainst,"Divisions/SP1.xlsx",append = T , sheetName = "Teamagainst")
# ########################################################################################################################################
# final_sp2_teamagainst <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# l6_form_sp2_teamagainstsplitted <- c()
# form_sp2_teamagainst <- c()
# for(index_sp2_teamagainst in 1:length(sp2_teams))
# {
#   for(index_sp2_teamagainst_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_teamagainst  <- row.names(sp2_form_team_against_h) == sp2_teams[index_sp2_teamagainst]
#     form_sp2_teamagainst <- sp2_form_team_against_h[index_sp2_teamagainst ]
#     deleted_form_sp2_teamagainst <- form_sp2_teamagainst[!form_sp2_teamagainst[] == ""]
#     l6_form_sp2_teamagainst <- tail(deleted_form_sp2_teamagainst,sp2_last_n_games)
#     l6_form_sp2_teamagainst <- as.character(l6_form_sp2_teamagainst)
#     #l6_form_sp2_teamagainst_flattened <- stri_paste(l6_form_sp2_teamagainst,collapse = '')
#     #l6_form_sp2_teamagainstsplitted <- strsplit(l6_form_sp2_teamagainst_flattened,"")[[1]]
#     final_sp2_teamagainst[index_sp2_teamagainst,index_sp2_teamagainst_cols] <- l6_form_sp2_teamagainst[index_sp2_teamagainst_cols]
#   }
# }
#
# sp2_teamagainst <- cbind(sp2_teams,final_sp2_teamagainst)
# write.xlsx(sp2_teamagainst,"Divisions/SP2.xlsx",append = T , sheetName = "Teamagainst")
# ########################################################################################################################################
# final_t1_teamagainst <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# l6_form_t1_teamagainstsplitted <- c()
# form_t1_teamagainst <- c()
# for(index_t1_teamagainst in 1:length(t1_teams))
# {
#   for(index_t1_teamagainst_cols in 1:t1_totalrounds)
#   {
#     index_t1_teamagainst  <- row.names(t1_form_team_against_h) == t1_teams[index_t1_teamagainst]
#     form_t1_teamagainst <- t1_form_team_against_h[index_t1_teamagainst ]
#     deleted_form_t1_teamagainst <- form_t1_teamagainst[!form_t1_teamagainst[] == ""]
#     l6_form_t1_teamagainst <- tail(deleted_form_t1_teamagainst,t1_last_n_games)
#     l6_form_t1_teamagainst <- as.character(l6_form_t1_teamagainst)
#     #l6_form_t1_teamagainst_flattened <- stri_paste(l6_form_t1_teamagainst,collapse = '')
#     #l6_form_t1_teamagainstsplitted <- strsplit(l6_form_t1_teamagainst_flattened,"")[[1]]
#     final_t1_teamagainst[index_t1_teamagainst,index_t1_teamagainst_cols] <- l6_form_t1_teamagainst[index_t1_teamagainst_cols]
#   }
# }
#
# t1_teamagainst <- cbind(t1_teams,final_t1_teamagainst)
# write.xlsx(t1_teamagainst,"Divisions/T1.xlsx",append = T , sheetName = "Teamagainst")
#
#
























