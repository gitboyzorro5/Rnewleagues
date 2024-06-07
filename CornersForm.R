#Create Home and Away Form
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#create home and away coscform matrices
mls_coscform_h <- tapply(MLS$COSC, MLS[c("HomeTeam", "Date")],median)
mls_coscform_a <- tapply(MLS$COSC, MLS[c("AwayTeam", "Date")],median)
arg_coscform_h <- tapply(ARG$COSC, ARG[c("HomeTeam", "Date")],median)
arg_coscform_a <- tapply(ARG$COSC, ARG[c("AwayTeam", "Date")],median)
# d2_coscform_h <- tapply(D2$COSC, D2[c("HomeTeam", "Date")],median)
# d2_coscform_a <- tapply(D2$COSC, D2[c("AwayTeam", "Date")],median)
# e0_coscform_h <- tapply(E0$COSC, E0[c("HomeTeam", "Date")],median)
# e0_coscform_a <- tapply(E0$COSC, E0[c("AwayTeam", "Date")],median)
# e1_coscform_h <- tapply(E1$COSC, E1[c("HomeTeam", "Date")],median)
# e1_coscform_a <- tapply(E1$COSC, E1[c("AwayTeam", "Date")],median)
# e2_coscform_h <- tapply(E2$COSC, E2[c("HomeTeam", "Date")],median)
# e2_coscform_a <- tapply(E2$COSC, E2[c("AwayTeam", "Date")],median)
# e3_coscform_h <- tapply(E3$COSC, E3[c("HomeTeam", "Date")],median)
# e3_coscform_a <- tapply(E3$COSC, E3[c("AwayTeam", "Date")],median)
# ec_coscform_h <- tapply(EC$COSC, EC[c("HomeTeam", "Date")],median)
# ec_coscform_a <- tapply(EC$COSC, EC[c("AwayTeam", "Date")],median)
# f1_coscform_h <- tapply(F1$COSC, F1[c("HomeTeam", "Date")],median)
# f1_coscform_a <- tapply(F1$COSC, F1[c("AwayTeam", "Date")],median)
# f2_coscform_h <- tapply(F2$COSC, F2[c("HomeTeam", "Date")],median)
# f2_coscform_a <- tapply(F2$COSC, F2[c("AwayTeam", "Date")],median)
# g1_coscform_h <- tapply(G1$COSC, G1[c("HomeTeam", "Date")],median)
# g1_coscform_a <- tapply(G1$COSC, G1[c("AwayTeam", "Date")],median)
# i1_coscform_h <- tapply(I1$COSC, I1[c("HomeTeam", "Date")],median)
# i1_coscform_a <- tapply(I1$COSC, I1[c("AwayTeam", "Date")],median)
# i2_coscform_h <- tapply(I2$COSC, I2[c("HomeTeam", "Date")],median)
# i2_coscform_a <- tapply(I2$COSC, I2[c("AwayTeam", "Date")],median)
# n1_coscform_h <- tapply(N1$COSC, N1[c("HomeTeam", "Date")],median)
# n1_coscform_a <- tapply(N1$COSC, N1[c("AwayTeam", "Date")],median)
# p1_coscform_h <- tapply(P1$COSC, P1[c("HomeTeam", "Date")],median)
# p1_coscform_a <- tapply(P1$COSC, P1[c("AwayTeam", "Date")],median)
# sc0_coscform_h <- tapply(SC0$COSC, SC0[c("HomeTeam", "Date")],median)
# sc0_coscform_a <- tapply(SC0$COSC, SC0[c("AwayTeam", "Date")],median)
# sc1_coscform_h <- tapply(SC1$COSC, SC1[c("HomeTeam", "Date")],median)
# sc1_coscform_a <- tapply(SC1$COSC, SC1[c("AwayTeam", "Date")],median)
# sc2_coscform_h <- tapply(SC2$COSC, SC2[c("HomeTeam", "Date")],median)
# sc2_coscform_a <- tapply(SC2$COSC, SC2[c("AwayTeam", "Date")],median)
# sc3_coscform_h <- tapply(SC3$COSC, SC3[c("HomeTeam", "Date")],median)
# sc3_coscform_a <- tapply(SC3$COSC, SC3[c("AwayTeam", "Date")],median)
# sp1_coscform_h <- tapply(SP1$COSC, SP1[c("HomeTeam", "Date")],median)
# sp1_coscform_a <- tapply(SP1$COSC, SP1[c("AwayTeam", "Date")],median)
# sp2_coscform_h <- tapply(SP2$COSC, SP2[c("HomeTeam", "Date")],median)
# sp2_coscform_a <- tapply(SP2$COSC, SP2[c("AwayTeam", "Date")],median)
# t1_coscform_h <- tapply(T1$COSC, T1[c("HomeTeam", "Date")],median)
# t1_coscform_a <- tapply(T1$COSC, T1[c("AwayTeam", "Date")],median)

#remove na values
mls_coscform_h[is.na(mls_coscform_h)] <- ""
mls_coscform_a[is.na(mls_coscform_a)] <- ""
arg_coscform_h[is.na(arg_coscform_h)] <- ""
arg_coscform_a[is.na(arg_coscform_a)] <- ""
# d2_coscform_h[is.na(d2_coscform_h)] <- ""
# d2_coscform_a[is.na(d2_coscform_a)] <- ""
# e0_coscform_h[is.na(e0_coscform_h)] <- ""
# e0_coscform_a[is.na(e0_coscform_a)] <- ""
# e1_coscform_h[is.na(e1_coscform_h)] <- ""
# e1_coscform_a[is.na(e1_coscform_a)] <- ""
# e2_coscform_h[is.na(e2_coscform_h)] <- ""
# e2_coscform_a[is.na(e2_coscform_a)] <- ""
# e3_coscform_h[is.na(e3_coscform_h)] <- ""
# e3_coscform_a[is.na(e3_coscform_a)] <- ""
# ec_coscform_h[is.na(ec_coscform_h)] <- ""
# ec_coscform_a[is.na(ec_coscform_a)] <- ""
# f1_coscform_h[is.na(f1_coscform_h)] <- ""
# f1_coscform_a[is.na(f1_coscform_a)] <- ""
# f2_coscform_h[is.na(f2_coscform_h)] <- ""
# f2_coscform_a[is.na(f2_coscform_a)] <- ""
# g1_coscform_h[is.na(g1_coscform_h)] <- ""
# g1_coscform_a[is.na(g1_coscform_a)] <- ""
# i1_coscform_h[is.na(i1_coscform_h)] <- ""
# i1_coscform_a[is.na(i1_coscform_a)] <- ""
# i2_coscform_h[is.na(i2_coscform_h)] <- ""
# i2_coscform_a[is.na(i2_coscform_a)] <- ""
# n1_coscform_h[is.na(n1_coscform_h)] <- ""
# n1_coscform_a[is.na(n1_coscform_a)] <- ""
# p1_coscform_h[is.na(p1_coscform_h)] <- ""
# p1_coscform_a[is.na(p1_coscform_a)] <- ""
# sc0_coscform_h[is.na(sc0_coscform_h)] <- ""
# sc0_coscform_a[is.na(sc0_coscform_a)] <- ""
# sc1_coscform_h[is.na(sc1_coscform_h)] <- ""
# sc1_coscform_a[is.na(sc1_coscform_a)] <- ""
# sc2_coscform_h[is.na(sc2_coscform_h)] <- ""
# sc2_coscform_a[is.na(sc2_coscform_a)] <- ""
# sc3_coscform_h[is.na(sc3_coscform_h)] <- ""
# sc3_coscform_a[is.na(sc3_coscform_a)] <- ""
# sp1_coscform_h[is.na(sp1_coscform_h)] <- ""
# sp1_coscform_a[is.na(sp1_coscform_a)] <- ""
# sp2_coscform_h[is.na(sp2_coscform_h)] <- ""
# sp2_coscform_a[is.na(sp2_coscform_a)] <- ""
# t1_coscform_h[is.na(t1_coscform_h)] <- ""
# t1_coscform_a[is.na(t1_coscform_a)] <- ""

#combine the matrices
#MLS
for(mls_rowh_f_cosc in 1:nrow(mls_coscform_h)) {
  for(mls_colh_f_cosc in 1:ncol(mls_coscform_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_cosc in 1:nrow(mls_coscform_a)) {
      for(mls_cola_f_cosc in 1:ncol(mls_coscform_a)) {
        ifelse(!mls_coscform_a[mls_rowa_f_cosc,mls_cola_f_cosc]=="",mls_coscform_h[mls_rowa_f_cosc,mls_cola_f_cosc] <- mls_coscform_a[mls_rowa_f_cosc,mls_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowh_f_cosc in 1:nrow(arg_coscform_h)) {
  for(arg_colh_f_cosc in 1:ncol(arg_coscform_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f_cosc in 1:nrow(arg_coscform_a)) {
      for(arg_cola_f_cosc in 1:ncol(arg_coscform_a)) {
        ifelse(!arg_coscform_a[arg_rowa_f_cosc,arg_cola_f_cosc]=="",arg_coscform_h[arg_rowa_f_cosc,arg_cola_f_cosc] <- arg_coscform_a[arg_rowa_f_cosc,arg_cola_f_cosc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowh_f_cosc in 1:nrow(d2_coscform_h)) {
#   for(d2_colh_f_cosc in 1:ncol(d2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowa_f_cosc in 1:nrow(d2_coscform_a)) {
#       for(d2_cola_f_cosc in 1:ncol(d2_coscform_a)) {
#         ifelse(!d2_coscform_a[d2_rowa_f_cosc,d2_cola_f_cosc]=="",d2_coscform_h[d2_rowa_f_cosc,d2_cola_f_cosc] <- d2_coscform_a[d2_rowa_f_cosc,d2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowh_f_cosc in 1:nrow(e0_coscform_h)) {
#   for(e0_colh_f_cosc in 1:ncol(e0_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowa_f_cosc in 1:nrow(e0_coscform_a)) {
#       for(e0_cola_f_cosc in 1:ncol(e0_coscform_a)) {
#         ifelse(!e0_coscform_a[e0_rowa_f_cosc,e0_cola_f_cosc]=="",e0_coscform_h[e0_rowa_f_cosc,e0_cola_f_cosc] <- e0_coscform_a[e0_rowa_f_cosc,e0_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowh_f_cosc in 1:nrow(e1_coscform_h)) {
#   for(e1_colh_f_cosc in 1:ncol(e1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowa_f_cosc in 1:nrow(e1_coscform_a)) {
#       for(e1_cola_f_cosc in 1:ncol(e1_coscform_a)) {
#         ifelse(!e1_coscform_a[e1_rowa_f_cosc,e1_cola_f_cosc]=="",e1_coscform_h[e1_rowa_f_cosc,e1_cola_f_cosc] <- e1_coscform_a[e1_rowa_f_cosc,e1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowh_f_cosc in 1:nrow(e2_coscform_h)) {
#   for(e2_colh_f_cosc in 1:ncol(e2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowa_f_cosc in 1:nrow(e2_coscform_a)) {
#       for(e2_cola_f_cosc in 1:ncol(e2_coscform_a)) {
#         ifelse(!e2_coscform_a[e2_rowa_f_cosc,e2_cola_f_cosc]=="",e2_coscform_h[e2_rowa_f_cosc,e2_cola_f_cosc] <- e2_coscform_a[e2_rowa_f_cosc,e2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowh_f_cosc in 1:nrow(e3_coscform_h)) {
#   for(e3_colh_f_cosc in 1:ncol(e3_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowa_f_cosc in 1:nrow(e3_coscform_a)) {
#       for(e3_cola_f_cosc in 1:ncol(e3_coscform_a)) {
#         ifelse(!e3_coscform_a[e3_rowa_f_cosc,e3_cola_f_cosc]=="",e3_coscform_h[e3_rowa_f_cosc,e3_cola_f_cosc] <- e3_coscform_a[e3_rowa_f_cosc,e3_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowh_f_cosc in 1:nrow(ec_coscform_h)) {
#   for(ec_colh_f_cosc in 1:ncol(ec_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowa_f_cosc in 1:nrow(ec_coscform_a)) {
#       for(ec_cola_f_cosc in 1:ncol(ec_coscform_a)) {
#         ifelse(!ec_coscform_a[ec_rowa_f_cosc,ec_cola_f_cosc]=="",ec_coscform_h[ec_rowa_f_cosc,ec_cola_f_cosc] <- ec_coscform_a[ec_rowa_f_cosc,ec_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowh_f_cosc in 1:nrow(f1_coscform_h)) {
#   for(f1_colh_f_cosc in 1:ncol(f1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowa_f_cosc in 1:nrow(f1_coscform_a)) {
#       for(f1_cola_f_cosc in 1:ncol(f1_coscform_a)) {
#         ifelse(!f1_coscform_a[f1_rowa_f_cosc,f1_cola_f_cosc]=="",f1_coscform_h[f1_rowa_f_cosc,f1_cola_f_cosc] <- f1_coscform_a[f1_rowa_f_cosc,f1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowh_f_cosc in 1:nrow(f2_coscform_h)) {
#   for(f2_colh_f_cosc in 1:ncol(f2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowa_f_cosc in 1:nrow(f2_coscform_a)) {
#       for(f2_cola_f_cosc in 1:ncol(f2_coscform_a)) {
#         ifelse(!f2_coscform_a[f2_rowa_f_cosc,f2_cola_f_cosc]=="",f2_coscform_h[f2_rowa_f_cosc,f2_cola_f_cosc] <- f2_coscform_a[f2_rowa_f_cosc,f2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowh_f_cosc in 1:nrow(g1_coscform_h)) {
#   for(g1_colh_f_cosc in 1:ncol(g1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowa_f_cosc in 1:nrow(g1_coscform_a)) {
#       for(g1_cola_f_cosc in 1:ncol(g1_coscform_a)) {
#         ifelse(!g1_coscform_a[g1_rowa_f_cosc,g1_cola_f_cosc]=="",g1_coscform_h[g1_rowa_f_cosc,g1_cola_f_cosc] <- g1_coscform_a[g1_rowa_f_cosc,g1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowh_f_cosc in 1:nrow(i1_coscform_h)) {
#   for(i1_colh_f_cosc in 1:ncol(i1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowa_f_cosc in 1:nrow(i1_coscform_a)) {
#       for(i1_cola_f_cosc in 1:ncol(i1_coscform_a)) {
#         ifelse(!i1_coscform_a[i1_rowa_f_cosc,i1_cola_f_cosc]=="",i1_coscform_h[i1_rowa_f_cosc,i1_cola_f_cosc] <- i1_coscform_a[i1_rowa_f_cosc,i1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowh_f_cosc in 1:nrow(i2_coscform_h)) {
#   for(i2_colh_f_cosc in 1:ncol(i2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowa_f_cosc in 1:nrow(i2_coscform_a)) {
#       for(i2_cola_f_cosc in 1:ncol(i2_coscform_a)) {
#         ifelse(!i2_coscform_a[i2_rowa_f_cosc,i2_cola_f_cosc]=="",i2_coscform_h[i2_rowa_f_cosc,i2_cola_f_cosc] <- i2_coscform_a[i2_rowa_f_cosc,i2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowh_f_cosc in 1:nrow(n1_coscform_h)) {
#   for(n1_colh_f_cosc in 1:ncol(n1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowa_f_cosc in 1:nrow(n1_coscform_a)) {
#       for(n1_cola_f_cosc in 1:ncol(n1_coscform_a)) {
#         ifelse(!n1_coscform_a[n1_rowa_f_cosc,n1_cola_f_cosc]=="",n1_coscform_h[n1_rowa_f_cosc,n1_cola_f_cosc] <- n1_coscform_a[n1_rowa_f_cosc,n1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowh_f_cosc in 1:nrow(p1_coscform_h)) {
#   for(p1_colh_f_cosc in 1:ncol(p1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowa_f_cosc in 1:nrow(p1_coscform_a)) {
#       for(p1_cola_f_cosc in 1:ncol(p1_coscform_a)) {
#         ifelse(!p1_coscform_a[p1_rowa_f_cosc,p1_cola_f_cosc]=="",p1_coscform_h[p1_rowa_f_cosc,p1_cola_f_cosc] <- p1_coscform_a[p1_rowa_f_cosc,p1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowh_f_cosc in 1:nrow(sc0_coscform_h)) {
#   for(sc0_colh_f_cosc in 1:ncol(sc0_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowa_f_cosc in 1:nrow(sc0_coscform_a)) {
#       for(sc0_cola_f_cosc in 1:ncol(sc0_coscform_a)) {
#         ifelse(!sc0_coscform_a[sc0_rowa_f_cosc,sc0_cola_f_cosc]=="",sc0_coscform_h[sc0_rowa_f_cosc,sc0_cola_f_cosc] <- sc0_coscform_a[sc0_rowa_f_cosc,sc0_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowh_f_cosc in 1:nrow(sc1_coscform_h)) {
#   for(sc1_colh_f_cosc in 1:ncol(sc1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowa_f_cosc in 1:nrow(sc1_coscform_a)) {
#       for(sc1_cola_f_cosc in 1:ncol(sc1_coscform_a)) {
#         ifelse(!sc1_coscform_a[sc1_rowa_f_cosc,sc1_cola_f_cosc]=="",sc1_coscform_h[sc1_rowa_f_cosc,sc1_cola_f_cosc] <- sc1_coscform_a[sc1_rowa_f_cosc,sc1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowh_f_cosc in 1:nrow(sc2_coscform_h)) {
#   for(sc2_colh_f_cosc in 1:ncol(sc2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowa_f_cosc in 1:nrow(sc2_coscform_a)) {
#       for(sc2_cola_f_cosc in 1:ncol(sc2_coscform_a)) {
#         ifelse(!sc2_coscform_a[sc2_rowa_f_cosc,sc2_cola_f_cosc]=="",sc2_coscform_h[sc2_rowa_f_cosc,sc2_cola_f_cosc] <- sc2_coscform_a[sc2_rowa_f_cosc,sc2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowh_f_cosc in 1:nrow(sc3_coscform_h)) {
#   for(sc3_colh_f_cosc in 1:ncol(sc3_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowa_f_cosc in 1:nrow(sc3_coscform_a)) {
#       for(sc3_cola_f_cosc in 1:ncol(sc3_coscform_a)) {
#         ifelse(!sc3_coscform_a[sc3_rowa_f_cosc,sc3_cola_f_cosc]=="",sc3_coscform_h[sc3_rowa_f_cosc,sc3_cola_f_cosc] <- sc3_coscform_a[sc3_rowa_f_cosc,sc3_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowh_f_cosc in 1:nrow(sp1_coscform_h)) {
#   for(sp1_colh_f_cosc in 1:ncol(sp1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowa_f_cosc in 1:nrow(sp1_coscform_a)) {
#       for(sp1_cola_f_cosc in 1:ncol(sp1_coscform_a)) {
#         ifelse(!sp1_coscform_a[sp1_rowa_f_cosc,sp1_cola_f_cosc]=="",sp1_coscform_h[sp1_rowa_f_cosc,sp1_cola_f_cosc] <- sp1_coscform_a[sp1_rowa_f_cosc,sp1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowh_f_cosc in 1:nrow(sp2_coscform_h)) {
#   for(sp2_colh_f_cosc in 1:ncol(sp2_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowa_f_cosc in 1:nrow(sp2_coscform_a)) {
#       for(sp2_cola_f_cosc in 1:ncol(sp2_coscform_a)) {
#         ifelse(!sp2_coscform_a[sp2_rowa_f_cosc,sp2_cola_f_cosc]=="",sp2_coscform_h[sp2_rowa_f_cosc,sp2_cola_f_cosc] <- sp2_coscform_a[sp2_rowa_f_cosc,sp2_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowh_f_cosc in 1:nrow(t1_coscform_h)) {
#   for(t1_colh_f_cosc in 1:ncol(t1_coscform_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowa_f_cosc in 1:nrow(t1_coscform_a)) {
#       for(t1_cola_f_cosc in 1:ncol(t1_coscform_a)) {
#         ifelse(!t1_coscform_a[t1_rowa_f_cosc,t1_cola_f_cosc]=="",t1_coscform_h[t1_rowa_f_cosc,t1_cola_f_cosc] <- t1_coscform_a[t1_rowa_f_cosc,t1_cola_f_cosc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
