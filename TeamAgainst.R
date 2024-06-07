#Create Home and Away teams against
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#create matrices
#MLS
mls_form_team_against_h <- tapply(MLS$AwayTeam, MLS[c("HomeTeam", "Date")],median)
mls_form_team_against_a <- tapply(MLS$HomeTeam, MLS[c("AwayTeam", "Date")],median)
#ARG
arg_form_team_against_h <- tapply(ARG$AwayTeam, ARG[c("HomeTeam", "Date")],median)
arg_form_team_against_a <- tapply(ARG$HomeTeam, ARG[c("AwayTeam", "Date")],median)
#D2
# d2_form_team_against_h <- tapply(D2$AwayTeam, D2[c("HomeTeam", "Date")],median)
# d2_form_team_against_a <- tapply(D2$HomeTeam, D2[c("AwayTeam", "Date")],median)
# #E0
# e0_form_team_against_h <- tapply(E0$AwayTeam, E0[c("HomeTeam", "Date")],median)
# e0_form_team_against_a <- tapply(E0$HomeTeam, E0[c("AwayTeam", "Date")],median)
# #E1
# e1_form_team_against_h <- tapply(E1$AwayTeam, E1[c("HomeTeam", "Date")],median)
# e1_form_team_against_a <- tapply(E1$HomeTeam, E1[c("AwayTeam", "Date")],median)
# #E2
# e2_form_team_against_h <- tapply(E2$AwayTeam, E2[c("HomeTeam", "Date")],median)
# e2_form_team_against_a <- tapply(E2$HomeTeam, E2[c("AwayTeam", "Date")],median)
# #E3
# e3_form_team_against_h <- tapply(E3$AwayTeam, E3[c("HomeTeam", "Date")],median)
# e3_form_team_against_a <- tapply(E3$HomeTeam, E3[c("AwayTeam", "Date")],median)
# #EC
# ec_form_team_against_h <- tapply(EC$AwayTeam, EC[c("HomeTeam", "Date")],median)
# ec_form_team_against_a <- tapply(EC$HomeTeam, EC[c("AwayTeam", "Date")],median)
# #F1
# f1_form_team_against_h <- tapply(F1$AwayTeam, F1[c("HomeTeam", "Date")],median)
# f1_form_team_against_a <- tapply(F1$HomeTeam, F1[c("AwayTeam", "Date")],median)
# #F2
# f2_form_team_against_h <- tapply(F2$AwayTeam, F2[c("HomeTeam", "Date")],median)
# f2_form_team_against_a <- tapply(F2$HomeTeam, F2[c("AwayTeam", "Date")],median)
# #G1
# g1_form_team_against_h <- tapply(G1$AwayTeam, G1[c("HomeTeam", "Date")],median)
# g1_form_team_against_a <- tapply(G1$HomeTeam, G1[c("AwayTeam", "Date")],median)
# #I1
# i1_form_team_against_h <- tapply(I1$AwayTeam, I1[c("HomeTeam", "Date")],median)
# i1_form_team_against_a <- tapply(I1$HomeTeam, I1[c("AwayTeam", "Date")],median)
# #I2
# i2_form_team_against_h <- tapply(I2$AwayTeam, I2[c("HomeTeam", "Date")],median)
# i2_form_team_against_a <- tapply(I2$HomeTeam, I2[c("AwayTeam", "Date")],median)
# #N1
# n1_form_team_against_h <- tapply(N1$AwayTeam, N1[c("HomeTeam", "Date")],median)
# n1_form_team_against_a <- tapply(N1$HomeTeam, N1[c("AwayTeam", "Date")],median)
# #P1
# p1_form_team_against_h <- tapply(P1$AwayTeam, P1[c("HomeTeam", "Date")],median)
# p1_form_team_against_a <- tapply(P1$HomeTeam, P1[c("AwayTeam", "Date")],median)
# #SC0
# sc0_form_team_against_h <- tapply(SC0$AwayTeam, SC0[c("HomeTeam", "Date")],median)
# sc0_form_team_against_a <- tapply(SC0$HomeTeam, SC0[c("AwayTeam", "Date")],median)
# #SC1
# sc1_form_team_against_h <- tapply(SC1$AwayTeam, SC1[c("HomeTeam", "Date")],median)
# sc1_form_team_against_a <- tapply(SC1$HomeTeam, SC1[c("AwayTeam", "Date")],median)
# #SC2
# sc2_form_team_against_h <- tapply(SC2$AwayTeam, SC2[c("HomeTeam", "Date")],median)
# sc2_form_team_against_a <- tapply(SC2$HomeTeam, SC2[c("AwayTeam", "Date")],median)
# #SC3
# sc3_form_team_against_h <- tapply(SC3$AwayTeam, SC3[c("HomeTeam", "Date")],median)
# sc3_form_team_against_a <- tapply(SC3$HomeTeam, SC3[c("AwayTeam", "Date")],median)
# #SP1
# sp1_form_team_against_h <- tapply(SP1$AwayTeam, SP1[c("HomeTeam", "Date")],median)
# sp1_form_team_against_a <- tapply(SP1$HomeTeam, SP1[c("AwayTeam", "Date")],median)
# #SP2
# sp2_form_team_against_h <- tapply(SP2$AwayTeam, SP2[c("HomeTeam", "Date")],median)
# sp2_form_team_against_a <- tapply(SP2$HomeTeam, SP2[c("AwayTeam", "Date")],median)
# #T1
# t1_form_team_against_h <- tapply(T1$AwayTeam, T1[c("HomeTeam", "Date")],median)
# t1_form_team_against_a <- tapply(T1$HomeTeam, T1[c("AwayTeam", "Date")],median)
#remove  na values
mls_form_team_against_h[is.na(mls_form_team_against_h)] <- ""
mls_form_team_against_a[is.na(mls_form_team_against_a)] <- ""
arg_form_team_against_h[is.na(arg_form_team_against_h)] <- ""
arg_form_team_against_a[is.na(arg_form_team_against_a)] <- ""
# d2_form_team_against_h[is.na(d2_form_team_against_h)] <- ""
# d2_form_team_against_a[is.na(d2_form_team_against_a)] <- ""
# e0_form_team_against_h[is.na(e0_form_team_against_h)] <- ""
# e0_form_team_against_a[is.na(e0_form_team_against_a)] <- ""
# e1_form_team_against_h[is.na(e1_form_team_against_h)] <- ""
# e1_form_team_against_a[is.na(e1_form_team_against_a)] <- ""
# e2_form_team_against_h[is.na(e2_form_team_against_h)] <- ""
# e2_form_team_against_a[is.na(e2_form_team_against_a)] <- ""
# e3_form_team_against_h[is.na(e3_form_team_against_h)] <- ""
# e3_form_team_against_a[is.na(e3_form_team_against_a)] <- ""
# ec_form_team_against_h[is.na(ec_form_team_against_h)] <- ""
# ec_form_team_against_a[is.na(ec_form_team_against_a)] <- ""
# f1_form_team_against_h[is.na(f1_form_team_against_h)] <- ""
# f1_form_team_against_a[is.na(f1_form_team_against_a)] <- ""
# f2_form_team_against_h[is.na(f2_form_team_against_h)] <- ""
# f2_form_team_against_a[is.na(f2_form_team_against_a)] <- ""
# g1_form_team_against_h[is.na(g1_form_team_against_h)] <- ""
# g1_form_team_against_a[is.na(g1_form_team_against_a)] <- ""
# i1_form_team_against_h[is.na(i1_form_team_against_h)] <- ""
# i1_form_team_against_a[is.na(i1_form_team_against_a)] <- ""
# i2_form_team_against_h[is.na(i2_form_team_against_h)] <- ""
# i2_form_team_against_a[is.na(i2_form_team_against_a)] <- ""
# n1_form_team_against_h[is.na(n1_form_team_against_h)] <- ""
# n1_form_team_against_a[is.na(n1_form_team_against_a)] <- ""
# p1_form_team_against_h[is.na(p1_form_team_against_h)] <- ""
# p1_form_team_against_a[is.na(p1_form_team_against_a)] <- ""
# sc0_form_team_against_h[is.na(sc0_form_team_against_h)] <- ""
# sc0_form_team_against_a[is.na(sc0_form_team_against_a)] <- ""
# sc1_form_team_against_h[is.na(sc1_form_team_against_h)] <- ""
# sc1_form_team_against_a[is.na(sc1_form_team_against_a)] <- ""
# sc2_form_team_against_h[is.na(sc2_form_team_against_h)] <- ""
# sc2_form_team_against_a[is.na(sc2_form_team_against_a)] <- ""
# sc3_form_team_against_h[is.na(sc3_form_team_against_h)] <- ""
# sc3_form_team_against_a[is.na(sc3_form_team_against_a)] <- ""
# sp1_form_team_against_h[is.na(sp1_form_team_against_h)] <- ""
# sp1_form_team_against_a[is.na(sp1_form_team_against_a)] <- ""
# sp2_form_team_against_h[is.na(sp2_form_team_against_h)] <- ""
# sp2_form_team_against_a[is.na(sp2_form_team_against_a)] <- ""
# t1_form_team_against_h[is.na(t1_form_team_against_h)] <- ""
# t1_form_team_against_a[is.na(t1_form_team_against_a)] <- ""

#combine the matrices
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
#ARG
for(arg_rowh_f_against in 1:nrow(arg_form_team_against_h)) {
  for(arg_colh_f_against in 1:ncol(arg_form_team_against_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f_against in 1:nrow(arg_form_team_against_a)) {
      for(arg_cola_f_against in 1:ncol(arg_form_team_against_a)) {
        ifelse(!arg_form_team_against_a[arg_rowa_f_against,arg_cola_f_against]=="",arg_form_team_against_h[arg_rowa_f_against,arg_cola_f_against] <- arg_form_team_against_a[arg_rowa_f_against,arg_cola_f_against],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowh_f_against in 1:nrow(d2_form_team_against_h)) {
#   for(d2_colh_f_against in 1:ncol(d2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowa_f_against in 1:nrow(d2_form_team_against_a)) {
#       for(d2_cola_f_against in 1:ncol(d2_form_team_against_a)) {
#         ifelse(!d2_form_team_against_a[d2_rowa_f_against,d2_cola_f_against]=="",d2_form_team_against_h[d2_rowa_f_against,d2_cola_f_against] <- d2_form_team_against_a[d2_rowa_f_against,d2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowh_f_against in 1:nrow(e0_form_team_against_h)) {
#   for(e0_colh_f_against in 1:ncol(e0_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowa_f_against in 1:nrow(e0_form_team_against_a)) {
#       for(e0_cola_f_against in 1:ncol(e0_form_team_against_a)) {
#         ifelse(!e0_form_team_against_a[e0_rowa_f_against,e0_cola_f_against]=="",e0_form_team_against_h[e0_rowa_f_against,e0_cola_f_against] <- e0_form_team_against_a[e0_rowa_f_against,e0_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowh_f_against in 1:nrow(e1_form_team_against_h)) {
#   for(e1_colh_f_against in 1:ncol(e1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowa_f_against in 1:nrow(e1_form_team_against_a)) {
#       for(e1_cola_f_against in 1:ncol(e1_form_team_against_a)) {
#         ifelse(!e1_form_team_against_a[e1_rowa_f_against,e1_cola_f_against]=="",e1_form_team_against_h[e1_rowa_f_against,e1_cola_f_against] <- e1_form_team_against_a[e1_rowa_f_against,e1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowh_f_against in 1:nrow(e2_form_team_against_h)) {
#   for(e2_colh_f_against in 1:ncol(e2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowa_f_against in 1:nrow(e2_form_team_against_a)) {
#       for(e2_cola_f_against in 1:ncol(e2_form_team_against_a)) {
#         ifelse(!e2_form_team_against_a[e2_rowa_f_against,e2_cola_f_against]=="",e2_form_team_against_h[e2_rowa_f_against,e2_cola_f_against] <- e2_form_team_against_a[e2_rowa_f_against,e2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowh_f_against in 1:nrow(e3_form_team_against_h)) {
#   for(e3_colh_f_against in 1:ncol(e3_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowa_f_against in 1:nrow(e3_form_team_against_a)) {
#       for(e3_cola_f_against in 1:ncol(e3_form_team_against_a)) {
#         ifelse(!e3_form_team_against_a[e3_rowa_f_against,e3_cola_f_against]=="",e3_form_team_against_h[e3_rowa_f_against,e3_cola_f_against] <- e3_form_team_against_a[e3_rowa_f_against,e3_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowh_f_against in 1:nrow(ec_form_team_against_h)) {
#   for(ec_colh_f_against in 1:ncol(ec_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowa_f_against in 1:nrow(ec_form_team_against_a)) {
#       for(ec_cola_f_against in 1:ncol(ec_form_team_against_a)) {
#         ifelse(!ec_form_team_against_a[ec_rowa_f_against,ec_cola_f_against]=="",ec_form_team_against_h[ec_rowa_f_against,ec_cola_f_against] <- ec_form_team_against_a[ec_rowa_f_against,ec_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowh_f_against in 1:nrow(f1_form_team_against_h)) {
#   for(f1_colh_f_against in 1:ncol(f1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowa_f_against in 1:nrow(f1_form_team_against_a)) {
#       for(f1_cola_f_against in 1:ncol(f1_form_team_against_a)) {
#         ifelse(!f1_form_team_against_a[f1_rowa_f_against,f1_cola_f_against]=="",f1_form_team_against_h[f1_rowa_f_against,f1_cola_f_against] <- f1_form_team_against_a[f1_rowa_f_against,f1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowh_f_against in 1:nrow(f2_form_team_against_h)) {
#   for(f2_colh_f_against in 1:ncol(f2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowa_f_against in 1:nrow(f2_form_team_against_a)) {
#       for(f2_cola_f_against in 1:ncol(f2_form_team_against_a)) {
#         ifelse(!f2_form_team_against_a[f2_rowa_f_against,f2_cola_f_against]=="",f2_form_team_against_h[f2_rowa_f_against,f2_cola_f_against] <- f2_form_team_against_a[f2_rowa_f_against,f2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowh_f_against in 1:nrow(g1_form_team_against_h)) {
#   for(g1_colh_f_against in 1:ncol(g1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowa_f_against in 1:nrow(g1_form_team_against_a)) {
#       for(g1_cola_f_against in 1:ncol(g1_form_team_against_a)) {
#         ifelse(!g1_form_team_against_a[g1_rowa_f_against,g1_cola_f_against]=="",g1_form_team_against_h[g1_rowa_f_against,g1_cola_f_against] <- g1_form_team_against_a[g1_rowa_f_against,g1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowh_f_against in 1:nrow(i1_form_team_against_h)) {
#   for(i1_colh_f_against in 1:ncol(i1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowa_f_against in 1:nrow(i1_form_team_against_a)) {
#       for(i1_cola_f_against in 1:ncol(i1_form_team_against_a)) {
#         ifelse(!i1_form_team_against_a[i1_rowa_f_against,i1_cola_f_against]=="",i1_form_team_against_h[i1_rowa_f_against,i1_cola_f_against] <- i1_form_team_against_a[i1_rowa_f_against,i1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowh_f_against in 1:nrow(i2_form_team_against_h)) {
#   for(i2_colh_f_against in 1:ncol(i2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowa_f_against in 1:nrow(i2_form_team_against_a)) {
#       for(i2_cola_f_against in 1:ncol(i2_form_team_against_a)) {
#         ifelse(!i2_form_team_against_a[i2_rowa_f_against,i2_cola_f_against]=="",i2_form_team_against_h[i2_rowa_f_against,i2_cola_f_against] <- i2_form_team_against_a[i2_rowa_f_against,i2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowh_f_against in 1:nrow(n1_form_team_against_h)) {
#   for(n1_colh_f_against in 1:ncol(n1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowa_f_against in 1:nrow(n1_form_team_against_a)) {
#       for(n1_cola_f_against in 1:ncol(n1_form_team_against_a)) {
#         ifelse(!n1_form_team_against_a[n1_rowa_f_against,n1_cola_f_against]=="",n1_form_team_against_h[n1_rowa_f_against,n1_cola_f_against] <- n1_form_team_against_a[n1_rowa_f_against,n1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowh_f_against in 1:nrow(p1_form_team_against_h)) {
#   for(p1_colh_f_against in 1:ncol(p1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowa_f_against in 1:nrow(p1_form_team_against_a)) {
#       for(p1_cola_f_against in 1:ncol(p1_form_team_against_a)) {
#         ifelse(!p1_form_team_against_a[p1_rowa_f_against,p1_cola_f_against]=="",p1_form_team_against_h[p1_rowa_f_against,p1_cola_f_against] <- p1_form_team_against_a[p1_rowa_f_against,p1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowh_f_against in 1:nrow(sc0_form_team_against_h)) {
#   for(sc0_colh_f_against in 1:ncol(sc0_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowa_f_against in 1:nrow(sc0_form_team_against_a)) {
#       for(sc0_cola_f_against in 1:ncol(sc0_form_team_against_a)) {
#         ifelse(!sc0_form_team_against_a[sc0_rowa_f_against,sc0_cola_f_against]=="",sc0_form_team_against_h[sc0_rowa_f_against,sc0_cola_f_against] <- sc0_form_team_against_a[sc0_rowa_f_against,sc0_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowh_f_against in 1:nrow(sc1_form_team_against_h)) {
#   for(sc1_colh_f_against in 1:ncol(sc1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowa_f_against in 1:nrow(sc1_form_team_against_a)) {
#       for(sc1_cola_f_against in 1:ncol(sc1_form_team_against_a)) {
#         ifelse(!sc1_form_team_against_a[sc1_rowa_f_against,sc1_cola_f_against]=="",sc1_form_team_against_h[sc1_rowa_f_against,sc1_cola_f_against] <- sc1_form_team_against_a[sc1_rowa_f_against,sc1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowh_f_against in 1:nrow(sc2_form_team_against_h)) {
#   for(sc2_colh_f_against in 1:ncol(sc2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowa_f_against in 1:nrow(sc2_form_team_against_a)) {
#       for(sc2_cola_f_against in 1:ncol(sc2_form_team_against_a)) {
#         ifelse(!sc2_form_team_against_a[sc2_rowa_f_against,sc2_cola_f_against]=="",sc2_form_team_against_h[sc2_rowa_f_against,sc2_cola_f_against] <- sc2_form_team_against_a[sc2_rowa_f_against,sc2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowh_f_against in 1:nrow(sc3_form_team_against_h)) {
#   for(sc3_colh_f_against in 1:ncol(sc3_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowa_f_against in 1:nrow(sc3_form_team_against_a)) {
#       for(sc3_cola_f_against in 1:ncol(sc3_form_team_against_a)) {
#         ifelse(!sc3_form_team_against_a[sc3_rowa_f_against,sc3_cola_f_against]=="",sc3_form_team_against_h[sc3_rowa_f_against,sc3_cola_f_against] <- sc3_form_team_against_a[sc3_rowa_f_against,sc3_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowh_f_against in 1:nrow(sp1_form_team_against_h)) {
#   for(sp1_colh_f_against in 1:ncol(sp1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowa_f_against in 1:nrow(sp1_form_team_against_a)) {
#       for(sp1_cola_f_against in 1:ncol(sp1_form_team_against_a)) {
#         ifelse(!sp1_form_team_against_a[sp1_rowa_f_against,sp1_cola_f_against]=="",sp1_form_team_against_h[sp1_rowa_f_against,sp1_cola_f_against] <- sp1_form_team_against_a[sp1_rowa_f_against,sp1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowh_f_against in 1:nrow(sp2_form_team_against_h)) {
#   for(sp2_colh_f_against in 1:ncol(sp2_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowa_f_against in 1:nrow(sp2_form_team_against_a)) {
#       for(sp2_cola_f_against in 1:ncol(sp2_form_team_against_a)) {
#         ifelse(!sp2_form_team_against_a[sp2_rowa_f_against,sp2_cola_f_against]=="",sp2_form_team_against_h[sp2_rowa_f_against,sp2_cola_f_against] <- sp2_form_team_against_a[sp2_rowa_f_against,sp2_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowh_f_against in 1:nrow(t1_form_team_against_h)) {
#   for(t1_colh_f_against in 1:ncol(t1_form_team_against_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowa_f_against in 1:nrow(t1_form_team_against_a)) {
#       for(t1_cola_f_against in 1:ncol(t1_form_team_against_a)) {
#         ifelse(!t1_form_team_against_a[t1_rowa_f_against,t1_cola_f_against]=="",t1_form_team_against_h[t1_rowa_f_against,t1_cola_f_against] <- t1_form_team_against_a[t1_rowa_f_against,t1_cola_f_against],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
