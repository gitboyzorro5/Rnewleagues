Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
final_mls_correctscoreform <- matrix(nrow = length(mls_teams),ncol = mls_totalrounds )
l6_form_mls_correctscoreformsplitted <- c()
form_mls_correctscoreform <- c()
for(index_mls_correctscoreform in 1:length(mls_teams))
{
  for(index_mls_correctscoreform_cols in 1:mls_totalrounds)
  {
    index_mls_correctscoreform  <- row.names(mls_csform_h) == mls_teams[index_mls_correctscoreform]
    form_mls_correctscoreform <- mls_csform_h[index_mls_correctscoreform ]
    deleted_form_mls_correctscoreform <- form_mls_correctscoreform[!form_mls_correctscoreform[] == ""]
    l6_form_mls_correctscoreform <- tail(deleted_form_mls_correctscoreform,mls_last_n_games)
    l6_form_mls_correctscoreform <- as.character(l6_form_mls_correctscoreform)
    #l6_form_mls_correctscoreform_flattened <- stri_paste(l6_form_mls_correctscoreform,collapse = '')
    #l6_form_mls_correctscoreformsplitted <- strsplit(l6_form_mls_correctscoreform_flattened,"")[[1]]
    final_mls_correctscoreform[index_mls_correctscoreform,index_mls_correctscoreform_cols] <- l6_form_mls_correctscoreform[index_mls_correctscoreform_cols]
  }
}

mls_correctscoreform <- cbind(mls_teams,final_mls_correctscoreform)
write.xlsx(mls_correctscoreform,"Divisions/MLS.xlsx",append = T , sheetName = "CSform")
#########################################################################################################################
final_arg_correctscoreform <- matrix(nrow = length(arg_teams),ncol = arg_totalrounds )
l6_form_arg_correctscoreformsplitted <- c()
form_arg_correctscoreform <- c()
for(index_arg_correctscoreform in 1:length(arg_teams))
{
  for(index_arg_correctscoreform_cols in 1:arg_totalrounds)
  {
    index_arg_correctscoreform  <- row.names(arg_csform_h) == arg_teams[index_arg_correctscoreform]
    form_arg_correctscoreform <- arg_csform_h[index_arg_correctscoreform ]
    deleted_form_arg_correctscoreform <- form_arg_correctscoreform[!form_arg_correctscoreform[] == ""]
    l6_form_arg_correctscoreform <- tail(deleted_form_arg_correctscoreform,arg_last_n_games)
    l6_form_arg_correctscoreform <- as.character(l6_form_arg_correctscoreform)
    #l6_form_arg_correctscoreform_flattened <- stri_paste(l6_form_arg_correctscoreform,collapse = '')
    #l6_form_arg_correctscoreformsplitted <- strsplit(l6_form_arg_correctscoreform_flattened,"")[[1]]
    final_arg_correctscoreform[index_arg_correctscoreform,index_arg_correctscoreform_cols] <- l6_form_arg_correctscoreform[index_arg_correctscoreform_cols]
  }
}

arg_correctscoreform <- cbind(arg_teams,final_arg_correctscoreform)
write.xlsx(arg_correctscoreform,"Divisions/ARG.xlsx",append = T , sheetName = "CSform")
############################################################################################################################
# final_d2_correctscoreform <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
# l6_form_d2_correctscoreformsplitted <- c()
# form_d2_correctscoreform <- c()
# for(index_d2_correctscoreform in 1:length(d2_teams))
# {
#   for(index_d2_correctscoreform_cols in 1:d2_totalrounds)
#   {
#     index_d2_correctscoreform  <- row.names(d2_csform_h) == d2_teams[index_d2_correctscoreform]
#     form_d2_correctscoreform <- d2_csform_h[index_d2_correctscoreform ]
#     deleted_form_d2_correctscoreform <- form_d2_correctscoreform[!form_d2_correctscoreform[] == ""]
#     l6_form_d2_correctscoreform <- tail(deleted_form_d2_correctscoreform,d2_last_n_games)
#     l6_form_d2_correctscoreform <- as.character(l6_form_d2_correctscoreform)
#     #l6_form_d2_correctscoreform_flattened <- stri_paste(l6_form_d2_correctscoreform,collapse = '')
#     #l6_form_d2_correctscoreformsplitted <- strsplit(l6_form_d2_correctscoreform_flattened,"")[[1]]
#     final_d2_correctscoreform[index_d2_correctscoreform,index_d2_correctscoreform_cols] <- l6_form_d2_correctscoreform[index_d2_correctscoreform_cols]
#   }
# }
#
# d2_correctscoreform <- cbind(d2_teams,final_d2_correctscoreform)
# write.xlsx(d2_correctscoreform,"Divisions/D2.xlsx",append = T , sheetName = "CSform")
# #############################################################################################################################
# final_e0_correctscoreform <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
# l6_form_e0_correctscoreformsplitted <- c()
# form_e0_correctscoreform <- c()
# for(index_e0_correctscoreform in 1:length(e0_teams))
# {
#   for(index_e0_correctscoreform_cols in 1:e0_totalrounds)
#   {
#     index_e0_correctscoreform  <- row.names(e0_csform_h) == e0_teams[index_e0_correctscoreform]
#     form_e0_correctscoreform <- e0_csform_h[index_e0_correctscoreform ]
#     deleted_form_e0_correctscoreform <- form_e0_correctscoreform[!form_e0_correctscoreform[] == ""]
#     l6_form_e0_correctscoreform <- tail(deleted_form_e0_correctscoreform,e0_last_n_games)
#     l6_form_e0_correctscoreform <- as.character(l6_form_e0_correctscoreform)
#     #l6_form_e0_correctscoreform_flattened <- stri_paste(l6_form_e0_correctscoreform,collapse = '')
#     #l6_form_e0_correctscoreformsplitted <- strsplit(l6_form_e0_correctscoreform_flattened,"")[[1]]
#     final_e0_correctscoreform[index_e0_correctscoreform,index_e0_correctscoreform_cols] <- l6_form_e0_correctscoreform[index_e0_correctscoreform_cols]
#   }
# }
#
# e0_correctscoreform <- cbind(e0_teams,final_e0_correctscoreform)
# write.xlsx(e0_correctscoreform,"Divisions/E0.xlsx",append = T , sheetName = "CSform")
# ################################################################################################################################
# final_e1_correctscoreform <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
# l6_form_e1_correctscoreformsplitted <- c()
# form_e1_correctscoreform <- c()
# for(index_e1_correctscoreform in 1:length(e1_teams))
# {
#   for(index_e1_correctscoreform_cols in 1:e1_totalrounds)
#   {
#     index_e1_correctscoreform  <- row.names(e1_csform_h) == e1_teams[index_e1_correctscoreform]
#     form_e1_correctscoreform <- e1_csform_h[index_e1_correctscoreform ]
#     deleted_form_e1_correctscoreform <- form_e1_correctscoreform[!form_e1_correctscoreform[] == ""]
#     l6_form_e1_correctscoreform <- tail(deleted_form_e1_correctscoreform,e1_last_n_games)
#     l6_form_e1_correctscoreform <- as.character(l6_form_e1_correctscoreform)
#     #l6_form_e1_correctscoreform_flattened <- stri_paste(l6_form_e1_correctscoreform,collapse = '')
#     #l6_form_e1_correctscoreformsplitted <- strsplit(l6_form_e1_correctscoreform_flattened,"")[[1]]
#     final_e1_correctscoreform[index_e1_correctscoreform,index_e1_correctscoreform_cols] <- l6_form_e1_correctscoreform[index_e1_correctscoreform_cols]
#   }
# }
#
# e1_correctscoreform <- cbind(e1_teams,final_e1_correctscoreform)
# write.xlsx(e1_correctscoreform,"Divisions/E1.xlsx",append = T , sheetName = "CSform")
# ################################################################################################################################
# final_e2_correctscoreform <- matrix(nrow = length(e2_teams),ncol = e2_totalrounds )
# l6_form_e2_correctscoreformsplitted <- c()
# form_e2_correctscoreform <- c()
# for(index_e2_correctscoreform in 1:length(e2_teams))
# {
#   for(index_e2_correctscoreform_cols in 1:e2_totalrounds)
#   {
#     index_e2_correctscoreform  <- row.names(e2_csform_h) == e2_teams[index_e2_correctscoreform]
#     form_e2_correctscoreform <- e2_csform_h[index_e2_correctscoreform ]
#     deleted_form_e2_correctscoreform <- form_e2_correctscoreform[!form_e2_correctscoreform[] == ""]
#     l6_form_e2_correctscoreform <- tail(deleted_form_e2_correctscoreform,e2_last_n_games)
#     l6_form_e2_correctscoreform <- as.character(l6_form_e2_correctscoreform)
#     #l6_form_e2_correctscoreform_flattened <- stri_paste(l6_form_e2_correctscoreform,collapse = '')
#     #l6_form_e2_correctscoreformsplitted <- strsplit(l6_form_e2_correctscoreform_flattened,"")[[1]]
#     final_e2_correctscoreform[index_e2_correctscoreform,index_e2_correctscoreform_cols] <- l6_form_e2_correctscoreform[index_e2_correctscoreform_cols]
#   }
# }
#
# e2_correctscoreform <- cbind(e2_teams,final_e2_correctscoreform)
# write.xlsx(e2_correctscoreform,"Divisions/E2.xlsx",append = T , sheetName = "CSform")
# #########################################################################################################################
# final_e3_correctscoreform <- matrix(nrow = length(e3_teams),ncol = e3_totalrounds )
# l6_form_e3_correctscoreformsplitted <- c()
# form_e3_correctscoreform <- c()
# for(index_e3_correctscoreform in 1:length(e3_teams))
# {
#   for(index_e3_correctscoreform_cols in 1:e3_totalrounds)
#   {
#     index_e3_correctscoreform  <- row.names(e3_csform_h) == e3_teams[index_e3_correctscoreform]
#     form_e3_correctscoreform <- e3_csform_h[index_e3_correctscoreform ]
#     deleted_form_e3_correctscoreform <- form_e3_correctscoreform[!form_e3_correctscoreform[] == ""]
#     l6_form_e3_correctscoreform <- tail(deleted_form_e3_correctscoreform,e3_last_n_games)
#     l6_form_e3_correctscoreform <- as.character(l6_form_e3_correctscoreform)
#     #l6_form_e3_correctscoreform_flattened <- stri_paste(l6_form_e3_correctscoreform,collapse = '')
#     #l6_form_e3_correctscoreformsplitted <- strsplit(l6_form_e3_correctscoreform_flattened,"")[[1]]
#     final_e3_correctscoreform[index_e3_correctscoreform,index_e3_correctscoreform_cols] <- l6_form_e3_correctscoreform[index_e3_correctscoreform_cols]
#   }
# }
#
# e3_correctscoreform <- cbind(e3_teams,final_e3_correctscoreform)
# write.xlsx(e3_correctscoreform,"Divisions/E3.xlsx",append = T , sheetName = "CSform")
# ################################################################################################################
# final_ec_correctscoreform <- matrix(nrow = length(ec_teams),ncol = ec_totalrounds )
# l6_form_ec_correctscoreformsplitted <- c()
# form_ec_correctscoreform <- c()
# for(index_ec_correctscoreform in 1:length(ec_teams))
# {
#   for(index_ec_correctscoreform_cols in 1:ec_totalrounds)
#   {
#     index_ec_correctscoreform  <- row.names(ec_csform_h) == ec_teams[index_ec_correctscoreform]
#     form_ec_correctscoreform <- ec_csform_h[index_ec_correctscoreform ]
#     deleted_form_ec_correctscoreform <- form_ec_correctscoreform[!form_ec_correctscoreform[] == ""]
#     l6_form_ec_correctscoreform <- tail(deleted_form_ec_correctscoreform,ec_last_n_games)
#     l6_form_ec_correctscoreform <- as.character(l6_form_ec_correctscoreform)
#     #l6_form_ec_correctscoreform_flattened <- stri_paste(l6_form_ec_correctscoreform,collapse = '')
#     #l6_form_ec_correctscoreformsplitted <- strsplit(l6_form_ec_correctscoreform_flattened,"")[[1]]
#     final_ec_correctscoreform[index_ec_correctscoreform,index_ec_correctscoreform_cols] <- l6_form_ec_correctscoreform[index_ec_correctscoreform_cols]
#   }
# }
#
# ec_correctscoreform <- cbind(ec_teams,final_ec_correctscoreform)
# write.xlsx(ec_correctscoreform,"Divisions/EC.xlsx",append = T , sheetName = "CSform")
# ##########################################################################################################################
# final_f1_correctscoreform <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
# l6_form_f1_correctscoreformsplitted <- c()
# form_f1_correctscoreform <- c()
# for(index_f1_correctscoreform in 1:length(f1_teams))
# {
#   for(index_f1_correctscoreform_cols in 1:f1_totalrounds)
#   {
#     index_f1_correctscoreform  <- row.names(f1_csform_h) == f1_teams[index_f1_correctscoreform]
#     form_f1_correctscoreform <- f1_csform_h[index_f1_correctscoreform ]
#     deleted_form_f1_correctscoreform <- form_f1_correctscoreform[!form_f1_correctscoreform[] == ""]
#     l6_form_f1_correctscoreform <- tail(deleted_form_f1_correctscoreform,f1_last_n_games)
#     l6_form_f1_correctscoreform <- as.character(l6_form_f1_correctscoreform)
#     #l6_form_f1_correctscoreform_flattened <- stri_paste(l6_form_f1_correctscoreform,collapse = '')
#     #l6_form_f1_correctscoreformsplitted <- strsplit(l6_form_f1_correctscoreform_flattened,"")[[1]]
#     final_f1_correctscoreform[index_f1_correctscoreform,index_f1_correctscoreform_cols] <- l6_form_f1_correctscoreform[index_f1_correctscoreform_cols]
#   }
# }
#
# f1_correctscoreform <- cbind(f1_teams,final_f1_correctscoreform)
# write.xlsx(f1_correctscoreform,"Divisions/F1.xlsx",append = T , sheetName = "CSform")
# #####################################################################################################################################
# final_f2_correctscoreform <- matrix(nrow = length(f2_teams),ncol = f2_totalrounds )
# l6_form_f2_correctscoreformsplitted <- c()
# form_f2_correctscoreform <- c()
# for(index_f2_correctscoreform in 1:length(f2_teams))
# {
#   for(index_f2_correctscoreform_cols in 1:f2_totalrounds)
#   {
#     index_f2_correctscoreform  <- row.names(f2_csform_h) == f2_teams[index_f2_correctscoreform]
#     form_f2_correctscoreform <- f2_csform_h[index_f2_correctscoreform ]
#     deleted_form_f2_correctscoreform <- form_f2_correctscoreform[!form_f2_correctscoreform[] == ""]
#     l6_form_f2_correctscoreform <- tail(deleted_form_f2_correctscoreform,f2_last_n_games)
#     l6_form_f2_correctscoreform <- as.character(l6_form_f2_correctscoreform)
#     #l6_form_f2_correctscoreform_flattened <- stri_paste(l6_form_f2_correctscoreform,collapse = '')
#     #l6_form_f2_correctscoreformsplitted <- strsplit(l6_form_f2_correctscoreform_flattened,"")[[1]]
#     final_f2_correctscoreform[index_f2_correctscoreform,index_f2_correctscoreform_cols] <- l6_form_f2_correctscoreform[index_f2_correctscoreform_cols]
#   }
# }
#
# f2_correctscoreform <- cbind(f2_teams,final_f2_correctscoreform)
# write.xlsx(f2_correctscoreform,"Divisions/F2.xlsx",append = T , sheetName = "CSform")
# ###################################################################################################################################
# final_g1_correctscoreform <- matrix(nrow = length(g1_teams),ncol = g1_totalrounds )
# l6_form_g1_correctscoreformsplitted <- c()
# form_g1_correctscoreform <- c()
# for(index_g1_correctscoreform in 1:length(g1_teams))
# {
#   for(index_g1_correctscoreform_cols in 1:g1_totalrounds)
#   {
#     index_g1_correctscoreform  <- row.names(g1_csform_h) == g1_teams[index_g1_correctscoreform]
#     form_g1_correctscoreform <- g1_csform_h[index_g1_correctscoreform ]
#     deleted_form_g1_correctscoreform <- form_g1_correctscoreform[!form_g1_correctscoreform[] == ""]
#     l6_form_g1_correctscoreform <- tail(deleted_form_g1_correctscoreform,g1_last_n_games)
#     l6_form_g1_correctscoreform <- as.character(l6_form_g1_correctscoreform)
#     #l6_form_g1_correctscoreform_flattened <- stri_paste(l6_form_g1_correctscoreform,collapse = '')
#     #l6_form_g1_correctscoreformsplitted <- strsplit(l6_form_g1_correctscoreform_flattened,"")[[1]]
#     final_g1_correctscoreform[index_g1_correctscoreform,index_g1_correctscoreform_cols] <- l6_form_g1_correctscoreform[index_g1_correctscoreform_cols]
#   }
# }
#
# g1_correctscoreform <- cbind(g1_teams,final_g1_correctscoreform)
# write.xlsx(g1_correctscoreform,"Divisions/G1.xlsx",append = T , sheetName = "CSform")
# ####################################################################################################################################
# final_i1_correctscoreform <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
# l6_form_i1_correctscoreformsplitted <- c()
# form_i1_correctscoreform <- c()
# for(index_i1_correctscoreform in 1:length(i1_teams))
# {
#   for(index_i1_correctscoreform_cols in 1:i1_totalrounds)
#   {
#     index_i1_correctscoreform  <- row.names(i1_csform_h) == i1_teams[index_i1_correctscoreform]
#     form_i1_correctscoreform <- i1_csform_h[index_i1_correctscoreform ]
#     deleted_form_i1_correctscoreform <- form_i1_correctscoreform[!form_i1_correctscoreform[] == ""]
#     l6_form_i1_correctscoreform <- tail(deleted_form_i1_correctscoreform,i1_last_n_games)
#     l6_form_i1_correctscoreform <- as.character(l6_form_i1_correctscoreform)
#     #l6_form_i1_correctscoreform_flattened <- stri_paste(l6_form_i1_correctscoreform,collapse = '')
#     #l6_form_i1_correctscoreformsplitted <- strsplit(l6_form_i1_correctscoreform_flattened,"")[[1]]
#     final_i1_correctscoreform[index_i1_correctscoreform,index_i1_correctscoreform_cols] <- l6_form_i1_correctscoreform[index_i1_correctscoreform_cols]
#   }
# }
#
# i1_correctscoreform <- cbind(i1_teams,final_i1_correctscoreform)
# write.xlsx(i1_correctscoreform,"Divisions/I1.xlsx",append = T , sheetName = "CSform")
# ###################################################################################################################################
# final_i2_correctscoreform <- matrix(nrow = length(i2_teams),ncol = i2_totalrounds )
# l6_form_i2_correctscoreformsplitted <- c()
# form_i2_correctscoreform <- c()
# for(index_i2_correctscoreform in 1:length(i2_teams))
# {
#   for(index_i2_correctscoreform_cols in 1:i2_totalrounds)
#   {
#     index_i2_correctscoreform  <- row.names(i2_csform_h) == i2_teams[index_i2_correctscoreform]
#     form_i2_correctscoreform <- i2_csform_h[index_i2_correctscoreform ]
#     deleted_form_i2_correctscoreform <- form_i2_correctscoreform[!form_i2_correctscoreform[] == ""]
#     l6_form_i2_correctscoreform <- tail(deleted_form_i2_correctscoreform,i2_last_n_games)
#     l6_form_i2_correctscoreform <- as.character(l6_form_i2_correctscoreform)
#     #l6_form_i2_correctscoreform_flattened <- stri_paste(l6_form_i2_correctscoreform,collapse = '')
#     #l6_form_i2_correctscoreformsplitted <- strsplit(l6_form_i2_correctscoreform_flattened,"")[[1]]
#     final_i2_correctscoreform[index_i2_correctscoreform,index_i2_correctscoreform_cols] <- l6_form_i2_correctscoreform[index_i2_correctscoreform_cols]
#   }
# }
#
# i2_correctscoreform <- cbind(i2_teams,final_i2_correctscoreform)
# write.xlsx(i2_correctscoreform,"Divisions/I2.xlsx",append = T , sheetName = "CSform")
# #############################################################################################################################
# final_n1_correctscoreform <- matrix(nrow = length(n1_teams),ncol = n1_totalrounds )
# l6_form_n1_correctscoreformsplitted <- c()
# form_n1_correctscoreform <- c()
# for(index_n1_correctscoreform in 1:length(n1_teams))
# {
#   for(index_n1_correctscoreform_cols in 1:n1_totalrounds)
#   {
#     index_n1_correctscoreform  <- row.names(n1_csform_h) == n1_teams[index_n1_correctscoreform]
#     form_n1_correctscoreform <- n1_csform_h[index_n1_correctscoreform ]
#     deleted_form_n1_correctscoreform <- form_n1_correctscoreform[!form_n1_correctscoreform[] == ""]
#     l6_form_n1_correctscoreform <- tail(deleted_form_n1_correctscoreform,n1_last_n_games)
#     l6_form_n1_correctscoreform <- as.character(l6_form_n1_correctscoreform)
#     #l6_form_n1_correctscoreform_flattened <- stri_paste(l6_form_n1_correctscoreform,collapse = '')
#     #l6_form_n1_correctscoreformsplitted <- strsplit(l6_form_n1_correctscoreform_flattened,"")[[1]]
#     final_n1_correctscoreform[index_n1_correctscoreform,index_n1_correctscoreform_cols] <- l6_form_n1_correctscoreform[index_n1_correctscoreform_cols]
#   }
# }
#
# n1_correctscoreform <- cbind(n1_teams,final_n1_correctscoreform)
# write.xlsx(n1_correctscoreform,"Divisions/N1.xlsx",append = T , sheetName = "CSform")
# #############################################################################################################################
# final_p1_correctscoreform <- matrix(nrow = length(p1_teams),ncol = p1_totalrounds )
# l6_form_p1_correctscoreformsplitted <- c()
# form_p1_correctscoreform <- c()
# for(index_p1_correctscoreform in 1:length(p1_teams))
# {
#   for(index_p1_correctscoreform_cols in 1:p1_totalrounds)
#   {
#     index_p1_correctscoreform  <- row.names(p1_csform_h) == p1_teams[index_p1_correctscoreform]
#     form_p1_correctscoreform <- p1_csform_h[index_p1_correctscoreform ]
#     deleted_form_p1_correctscoreform <- form_p1_correctscoreform[!form_p1_correctscoreform[] == ""]
#     l6_form_p1_correctscoreform <- tail(deleted_form_p1_correctscoreform,p1_last_n_games)
#     l6_form_p1_correctscoreform <- as.character(l6_form_p1_correctscoreform)
#     #l6_form_p1_correctscoreform_flattened <- stri_paste(l6_form_p1_correctscoreform,collapse = '')
#     #l6_form_p1_correctscoreformsplitted <- strsplit(l6_form_p1_correctscoreform_flattened,"")[[1]]
#     final_p1_correctscoreform[index_p1_correctscoreform,index_p1_correctscoreform_cols] <- l6_form_p1_correctscoreform[index_p1_correctscoreform_cols]
#   }
# }
#
# p1_correctscoreform <- cbind(p1_teams,final_p1_correctscoreform)
# write.xlsx(p1_correctscoreform,"Divisions/P1.xlsx",append = T , sheetName = "CSform")
# #############################################################################################################################################
# final_sc0_correctscoreform <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# l6_form_sc0_correctscoreformsplitted <- c()
# form_sc0_correctscoreform <- c()
# for(index_sc0_correctscoreform in 1:length(sc0_teams))
# {
#   for(index_sc0_correctscoreform_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_correctscoreform  <- row.names(sc0_csform_h) == sc0_teams[index_sc0_correctscoreform]
#     form_sc0_correctscoreform <- sc0_csform_h[index_sc0_correctscoreform ]
#     deleted_form_sc0_correctscoreform <- form_sc0_correctscoreform[!form_sc0_correctscoreform[] == ""]
#     l6_form_sc0_correctscoreform <- tail(deleted_form_sc0_correctscoreform,sc0_last_n_games)
#     l6_form_sc0_correctscoreform <- as.character(l6_form_sc0_correctscoreform)
#     #l6_form_sc0_correctscoreform_flattened <- stri_paste(l6_form_sc0_correctscoreform,collapse = '')
#     #l6_form_sc0_correctscoreformsplitted <- strsplit(l6_form_sc0_correctscoreform_flattened,"")[[1]]
#     final_sc0_correctscoreform[index_sc0_correctscoreform,index_sc0_correctscoreform_cols] <- l6_form_sc0_correctscoreform[index_sc0_correctscoreform_cols]
#   }
# }
#
# sc0_correctscoreform <- cbind(sc0_teams,final_sc0_correctscoreform)
# write.xlsx(sc0_correctscoreform,"Divisions/SC0.xlsx",append = T , sheetName = "CSform")
# ##############################################################################################################################################
# final_sc1_correctscoreform <- matrix(nrow = length(sc1_teams),ncol = sc1_totalrounds )
# l6_form_sc1_correctscoreformsplitted <- c()
# form_sc1_correctscoreform <- c()
# for(index_sc1_correctscoreform in 1:length(sc1_teams))
# {
#   for(index_sc1_correctscoreform_cols in 1:sc1_totalrounds)
#   {
#     index_sc1_correctscoreform  <- row.names(sc1_csform_h) == sc1_teams[index_sc1_correctscoreform]
#     form_sc1_correctscoreform <- sc1_csform_h[index_sc1_correctscoreform ]
#     deleted_form_sc1_correctscoreform <- form_sc1_correctscoreform[!form_sc1_correctscoreform[] == ""]
#     l6_form_sc1_correctscoreform <- tail(deleted_form_sc1_correctscoreform,sc1_last_n_games)
#     l6_form_sc1_correctscoreform <- as.character(l6_form_sc1_correctscoreform)
#     #l6_form_sc1_correctscoreform_flattened <- stri_paste(l6_form_sc1_correctscoreform,collapse = '')
#     #l6_form_sc1_correctscoreformsplitted <- strsplit(l6_form_sc1_correctscoreform_flattened,"")[[1]]
#     final_sc1_correctscoreform[index_sc1_correctscoreform,index_sc1_correctscoreform_cols] <- l6_form_sc1_correctscoreform[index_sc1_correctscoreform_cols]
#   }
# }
#
# sc1_correctscoreform <- cbind(sc1_teams,final_sc1_correctscoreform)
# write.xlsx(sc1_correctscoreform,"Divisions/SC1.xlsx",append = T , sheetName = "CSform")
# ##########################################################################################################################
# final_sc2_correctscoreform <- matrix(nrow = length(sc2_teams),ncol = sc2_totalrounds )
# l6_form_sc2_correctscoreformsplitted <- c()
# form_sc2_correctscoreform <- c()
# for(index_sc2_correctscoreform in 1:length(sc2_teams))
# {
#   for(index_sc2_correctscoreform_cols in 1:sc2_totalrounds)
#   {
#     index_sc2_correctscoreform  <- row.names(sc2_csform_h) == sc2_teams[index_sc2_correctscoreform]
#     form_sc2_correctscoreform <- sc2_csform_h[index_sc2_correctscoreform ]
#     deleted_form_sc2_correctscoreform <- form_sc2_correctscoreform[!form_sc2_correctscoreform[] == ""]
#     l6_form_sc2_correctscoreform <- tail(deleted_form_sc2_correctscoreform,sc2_last_n_games)
#     l6_form_sc2_correctscoreform <- as.character(l6_form_sc2_correctscoreform)
#     #l6_form_sc2_correctscoreform_flattened <- stri_paste(l6_form_sc2_correctscoreform,collapse = '')
#     #l6_form_sc2_correctscoreformsplitted <- strsplit(l6_form_sc2_correctscoreform_flattened,"")[[1]]
#     final_sc2_correctscoreform[index_sc2_correctscoreform,index_sc2_correctscoreform_cols] <- l6_form_sc2_correctscoreform[index_sc2_correctscoreform_cols]
#   }
# }
#
# sc2_correctscoreform <- cbind(sc2_teams,final_sc2_correctscoreform)
# write.xlsx(sc2_correctscoreform,"Divisions/SC2.xlsx",append = T , sheetName = "CSform")
# #####################################################################################################################
# final_sc3_correctscoreform <- matrix(nrow = length(sc3_teams),ncol = sc3_totalrounds )
# l6_form_sc3_correctscoreformsplitted <- c()
# form_sc3_correctscoreform <- c()
# for(index_sc3_correctscoreform in 1:length(sc3_teams))
# {
#   for(index_sc3_correctscoreform_cols in 1:sc3_totalrounds)
#   {
#     index_sc3_correctscoreform  <- row.names(sc3_csform_h) == sc3_teams[index_sc3_correctscoreform]
#     form_sc3_correctscoreform <- sc3_csform_h[index_sc3_correctscoreform ]
#     deleted_form_sc3_correctscoreform <- form_sc3_correctscoreform[!form_sc3_correctscoreform[] == ""]
#     l6_form_sc3_correctscoreform <- tail(deleted_form_sc3_correctscoreform,sc3_last_n_games)
#     l6_form_sc3_correctscoreform <- as.character(l6_form_sc3_correctscoreform)
#     #l6_form_sc3_correctscoreform_flattened <- stri_paste(l6_form_sc3_correctscoreform,collapse = '')
#     #l6_form_sc3_correctscoreformsplitted <- strsplit(l6_form_sc3_correctscoreform_flattened,"")[[1]]
#     final_sc3_correctscoreform[index_sc3_correctscoreform,index_sc3_correctscoreform_cols] <- l6_form_sc3_correctscoreform[index_sc3_correctscoreform_cols]
#   }
# }
#
# sc3_correctscoreform <- cbind(sc3_teams,final_sc3_correctscoreform)
# write.xlsx(sc3_correctscoreform,"Divisions/SC3.xlsx",append = T , sheetName = "CSform")
# #######################################################################################################################
# final_sp1_correctscoreform <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
# l6_form_sp1_correctscoreformsplitted <- c()
# form_sp1_correctscoreform <- c()
# for(index_sp1_correctscoreform in 1:length(sp1_teams))
# {
#   for(index_sp1_correctscoreform_cols in 1:sp1_totalrounds)
#   {
#     index_sp1_correctscoreform  <- row.names(sp1_csform_h) == sp1_teams[index_sp1_correctscoreform]
#     form_sp1_correctscoreform <- sp1_csform_h[index_sp1_correctscoreform ]
#     deleted_form_sp1_correctscoreform <- form_sp1_correctscoreform[!form_sp1_correctscoreform[] == ""]
#     l6_form_sp1_correctscoreform <- tail(deleted_form_sp1_correctscoreform,sp1_last_n_games)
#     l6_form_sp1_correctscoreform <- as.character(l6_form_sp1_correctscoreform)
#     #l6_form_sp1_correctscoreform_flattened <- stri_paste(l6_form_sp1_correctscoreform,collapse = '')
#     #l6_form_sp1_correctscoreformsplitted <- strsplit(l6_form_sp1_correctscoreform_flattened,"")[[1]]
#     final_sp1_correctscoreform[index_sp1_correctscoreform,index_sp1_correctscoreform_cols] <- l6_form_sp1_correctscoreform[index_sp1_correctscoreform_cols]
#   }
# }
#
# sp1_correctscoreform <- cbind(sp1_teams,final_sp1_correctscoreform)
# write.xlsx(sp1_correctscoreform,"Divisions/SP1.xlsx",append = T , sheetName = "CSform")
# ############################################################################################################################################
# final_sp2_correctscoreform <- matrix(nrow = length(sp2_teams),ncol = sp2_totalrounds )
# l6_form_sp2_correctscoreformsplitted <- c()
# form_sp2_correctscoreform <- c()
# for(index_sp2_correctscoreform in 1:length(sp2_teams))
# {
#   for(index_sp2_correctscoreform_cols in 1:sp2_totalrounds)
#   {
#     index_sp2_correctscoreform  <- row.names(sp2_csform_h) == sp2_teams[index_sp2_correctscoreform]
#     form_sp2_correctscoreform <- sp2_csform_h[index_sp2_correctscoreform ]
#     deleted_form_sp2_correctscoreform <- form_sp2_correctscoreform[!form_sp2_correctscoreform[] == ""]
#     l6_form_sp2_correctscoreform <- tail(deleted_form_sp2_correctscoreform,sp2_last_n_games)
#     l6_form_sp2_correctscoreform <- as.character(l6_form_sp2_correctscoreform)
#     #l6_form_sp2_correctscoreform_flattened <- stri_paste(l6_form_sp2_correctscoreform,collapse = '')
#     #l6_form_sp2_correctscoreformsplitted <- strsplit(l6_form_sp2_correctscoreform_flattened,"")[[1]]
#     final_sp2_correctscoreform[index_sp2_correctscoreform,index_sp2_correctscoreform_cols] <- l6_form_sp2_correctscoreform[index_sp2_correctscoreform_cols]
#   }
# }
#
# sp2_correctscoreform <- cbind(sp2_teams,final_sp2_correctscoreform)
# write.xlsx(sp2_correctscoreform,"Divisions/SP2.xlsx",append = T , sheetName = "CSform")
# ##############################################################################################################################
# final_t1_correctscoreform <- matrix(nrow = length(t1_teams),ncol = t1_totalrounds )
# l6_form_t1_correctscoreformsplitted <- c()
# form_t1_correctscoreform <- c()
# for(index_t1_correctscoreform in 1:length(t1_teams))
# {
#   for(index_t1_correctscoreform_cols in 1:t1_totalrounds)
#   {
#     index_t1_correctscoreform  <- row.names(t1_csform_h) == t1_teams[index_t1_correctscoreform]
#     form_t1_correctscoreform <- t1_csform_h[index_t1_correctscoreform ]
#     deleted_form_t1_correctscoreform <- form_t1_correctscoreform[!form_t1_correctscoreform[] == ""]
#     l6_form_t1_correctscoreform <- tail(deleted_form_t1_correctscoreform,t1_last_n_games)
#     l6_form_t1_correctscoreform <- as.character(l6_form_t1_correctscoreform)
#     #l6_form_t1_correctscoreform_flattened <- stri_paste(l6_form_t1_correctscoreform,collapse = '')
#     #l6_form_t1_correctscoreformsplitted <- strsplit(l6_form_t1_correctscoreform_flattened,"")[[1]]
#     final_t1_correctscoreform[index_t1_correctscoreform,index_t1_correctscoreform_cols] <- l6_form_t1_correctscoreform[index_t1_correctscoreform_cols]
#   }
# }
#
# t1_correctscoreform <- cbind(t1_teams,final_t1_correctscoreform)
# write.xlsx(t1_correctscoreform,"Divisions/T1.xlsx",append = T , sheetName = "CSform")
#






















