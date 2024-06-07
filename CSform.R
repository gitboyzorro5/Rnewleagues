#Create Home and Away Form
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#create home and away csform matrices
mls_csform_h <- tapply(MLS$CS, MLS[c("HomeTeam", "Date")],median)
mls_csform_a <- tapply(MLS$CS, MLS[c("AwayTeam", "Date")],median)
arg_csform_h <- tapply(ARG$CS, ARG[c("HomeTeam", "Date")],median)
arg_csform_a <- tapply(ARG$CS, ARG[c("AwayTeam", "Date")],median)
# d2_csform_h <- tapply(D2$CS, D2[c("HomeTeam", "Date")],median)
# d2_csform_a <- tapply(D2$CS, D2[c("AwayTeam", "Date")],median)
# e0_csform_h <- tapply(E0$CS, E0[c("HomeTeam", "Date")],median)
# e0_csform_a <- tapply(E0$CS, E0[c("AwayTeam", "Date")],median)
# e1_csform_h <- tapply(E1$CS, E1[c("HomeTeam", "Date")],median)
# e1_csform_a <- tapply(E1$CS, E1[c("AwayTeam", "Date")],median)
# e2_csform_h <- tapply(E2$CS, E2[c("HomeTeam", "Date")],median)
# e2_csform_a <- tapply(E2$CS, E2[c("AwayTeam", "Date")],median)
# e3_csform_h <- tapply(E3$CS, E3[c("HomeTeam", "Date")],median)
# e3_csform_a <- tapply(E3$CS, E3[c("AwayTeam", "Date")],median)
# ec_csform_h <- tapply(EC$CS, EC[c("HomeTeam", "Date")],median)
# ec_csform_a <- tapply(EC$CS, EC[c("AwayTeam", "Date")],median)
# f1_csform_h <- tapply(F1$CS, F1[c("HomeTeam", "Date")],median)
# f1_csform_a <- tapply(F1$CS, F1[c("AwayTeam", "Date")],median)
# f2_csform_h <- tapply(F2$CS, F2[c("HomeTeam", "Date")],median)
# f2_csform_a <- tapply(F2$CS, F2[c("AwayTeam", "Date")],median)
# g1_csform_h <- tapply(G1$CS, G1[c("HomeTeam", "Date")],median)
# g1_csform_a <- tapply(G1$CS, G1[c("AwayTeam", "Date")],median)
# i1_csform_h <- tapply(I1$CS, I1[c("HomeTeam", "Date")],median)
# i1_csform_a <- tapply(I1$CS, I1[c("AwayTeam", "Date")],median)
# i2_csform_h <- tapply(I2$CS, I2[c("HomeTeam", "Date")],median)
# i2_csform_a <- tapply(I2$CS, I2[c("AwayTeam", "Date")],median)
# n1_csform_h <- tapply(N1$CS, N1[c("HomeTeam", "Date")],median)
# n1_csform_a <- tapply(N1$CS, N1[c("AwayTeam", "Date")],median)
# p1_csform_h <- tapply(P1$CS, P1[c("HomeTeam", "Date")],median)
# p1_csform_a <- tapply(P1$CS, P1[c("AwayTeam", "Date")],median)
# sc0_csform_h <- tapply(SC0$CS, SC0[c("HomeTeam", "Date")],median)
# sc0_csform_a <- tapply(SC0$CS, SC0[c("AwayTeam", "Date")],median)
# sc1_csform_h <- tapply(SC1$CS, SC1[c("HomeTeam", "Date")],median)
# sc1_csform_a <- tapply(SC1$CS, SC1[c("AwayTeam", "Date")],median)
# sc2_csform_h <- tapply(SC2$CS, SC2[c("HomeTeam", "Date")],median)
# sc2_csform_a <- tapply(SC2$CS, SC2[c("AwayTeam", "Date")],median)
# sc3_csform_h <- tapply(SC3$CS, SC3[c("HomeTeam", "Date")],median)
# sc3_csform_a <- tapply(SC3$CS, SC3[c("AwayTeam", "Date")],median)
# sp1_csform_h <- tapply(SP1$CS, SP1[c("HomeTeam", "Date")],median)
# sp1_csform_a <- tapply(SP1$CS, SP1[c("AwayTeam", "Date")],median)
# sp2_csform_h <- tapply(SP2$CS, SP2[c("HomeTeam", "Date")],median)
# sp2_csform_a <- tapply(SP2$CS, SP2[c("AwayTeam", "Date")],median)
# t1_csform_h <- tapply(T1$CS, T1[c("HomeTeam", "Date")],median)
# t1_csform_a <- tapply(T1$CS, T1[c("AwayTeam", "Date")],median)

#remove na values
mls_csform_h[is.na(mls_csform_h)] <- ""
mls_csform_a[is.na(mls_csform_a)] <- ""
arg_csform_h[is.na(arg_csform_h)] <- ""
arg_csform_a[is.na(arg_csform_a)] <- ""
# d2_csform_h[is.na(d2_csform_h)] <- ""
# d2_csform_a[is.na(d2_csform_a)] <- ""
# e0_csform_h[is.na(e0_csform_h)] <- ""
# e0_csform_a[is.na(e0_csform_a)] <- ""
# e1_csform_h[is.na(e1_csform_h)] <- ""
# e1_csform_a[is.na(e1_csform_a)] <- ""
# e2_csform_h[is.na(e2_csform_h)] <- ""
# e2_csform_a[is.na(e2_csform_a)] <- ""
# e3_csform_h[is.na(e3_csform_h)] <- ""
# e3_csform_a[is.na(e3_csform_a)] <- ""
# ec_csform_h[is.na(ec_csform_h)] <- ""
# ec_csform_a[is.na(ec_csform_a)] <- ""
# f1_csform_h[is.na(f1_csform_h)] <- ""
# f1_csform_a[is.na(f1_csform_a)] <- ""
# f2_csform_h[is.na(f2_csform_h)] <- ""
# f2_csform_a[is.na(f2_csform_a)] <- ""
# g1_csform_h[is.na(g1_csform_h)] <- ""
# g1_csform_a[is.na(g1_csform_a)] <- ""
# i1_csform_h[is.na(i1_csform_h)] <- ""
# i1_csform_a[is.na(i1_csform_a)] <- ""
# i2_csform_h[is.na(i2_csform_h)] <- ""
# i2_csform_a[is.na(i2_csform_a)] <- ""
# n1_csform_h[is.na(n1_csform_h)] <- ""
# n1_csform_a[is.na(n1_csform_a)] <- ""
# p1_csform_h[is.na(p1_csform_h)] <- ""
# p1_csform_a[is.na(p1_csform_a)] <- ""
# sc0_csform_h[is.na(sc0_csform_h)] <- ""
# sc0_csform_a[is.na(sc0_csform_a)] <- ""
# sc1_csform_h[is.na(sc1_csform_h)] <- ""
# sc1_csform_a[is.na(sc1_csform_a)] <- ""
# sc2_csform_h[is.na(sc2_csform_h)] <- ""
# sc2_csform_a[is.na(sc2_csform_a)] <- ""
# sc3_csform_h[is.na(sc3_csform_h)] <- ""
# sc3_csform_a[is.na(sc3_csform_a)] <- ""
# sp1_csform_h[is.na(sp1_csform_h)] <- ""
# sp1_csform_a[is.na(sp1_csform_a)] <- ""
# sp2_csform_h[is.na(sp2_csform_h)] <- ""
# sp2_csform_a[is.na(sp2_csform_a)] <- ""
# t1_csform_h[is.na(t1_csform_h)] <- ""
# t1_csform_a[is.na(t1_csform_a)] <- ""

#combine the matrices
#MLS
for(mls_rowh_f_cs in 1:nrow(mls_csform_h)) {
  for(mls_colh_f_cs in 1:ncol(mls_csform_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f_cs in 1:nrow(mls_csform_a)) {
      for(mls_cola_f_cs in 1:ncol(mls_csform_a)) {
        ifelse(!mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs]=="",mls_csform_h[mls_rowa_f_cs,mls_cola_f_cs] <- mls_csform_a[mls_rowa_f_cs,mls_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowh_f_cs in 1:nrow(arg_csform_h)) {
  for(arg_colh_f_cs in 1:ncol(arg_csform_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f_cs in 1:nrow(arg_csform_a)) {
      for(arg_cola_f_cs in 1:ncol(arg_csform_a)) {
        ifelse(!arg_csform_a[arg_rowa_f_cs,arg_cola_f_cs]=="",arg_csform_h[arg_rowa_f_cs,arg_cola_f_cs] <- arg_csform_a[arg_rowa_f_cs,arg_cola_f_cs],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowh_f_cs in 1:nrow(d2_csform_h)) {
#   for(d2_colh_f_cs in 1:ncol(d2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowa_f_cs in 1:nrow(d2_csform_a)) {
#       for(d2_cola_f_cs in 1:ncol(d2_csform_a)) {
#         ifelse(!d2_csform_a[d2_rowa_f_cs,d2_cola_f_cs]=="",d2_csform_h[d2_rowa_f_cs,d2_cola_f_cs] <- d2_csform_a[d2_rowa_f_cs,d2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowh_f_cs in 1:nrow(e0_csform_h)) {
#   for(e0_colh_f_cs in 1:ncol(e0_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowa_f_cs in 1:nrow(e0_csform_a)) {
#       for(e0_cola_f_cs in 1:ncol(e0_csform_a)) {
#         ifelse(!e0_csform_a[e0_rowa_f_cs,e0_cola_f_cs]=="",e0_csform_h[e0_rowa_f_cs,e0_cola_f_cs] <- e0_csform_a[e0_rowa_f_cs,e0_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowh_f_cs in 1:nrow(e1_csform_h)) {
#   for(e1_colh_f_cs in 1:ncol(e1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowa_f_cs in 1:nrow(e1_csform_a)) {
#       for(e1_cola_f_cs in 1:ncol(e1_csform_a)) {
#         ifelse(!e1_csform_a[e1_rowa_f_cs,e1_cola_f_cs]=="",e1_csform_h[e1_rowa_f_cs,e1_cola_f_cs] <- e1_csform_a[e1_rowa_f_cs,e1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowh_f_cs in 1:nrow(e2_csform_h)) {
#   for(e2_colh_f_cs in 1:ncol(e2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowa_f_cs in 1:nrow(e2_csform_a)) {
#       for(e2_cola_f_cs in 1:ncol(e2_csform_a)) {
#         ifelse(!e2_csform_a[e2_rowa_f_cs,e2_cola_f_cs]=="",e2_csform_h[e2_rowa_f_cs,e2_cola_f_cs] <- e2_csform_a[e2_rowa_f_cs,e2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowh_f_cs in 1:nrow(e3_csform_h)) {
#   for(e3_colh_f_cs in 1:ncol(e3_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowa_f_cs in 1:nrow(e3_csform_a)) {
#       for(e3_cola_f_cs in 1:ncol(e3_csform_a)) {
#         ifelse(!e3_csform_a[e3_rowa_f_cs,e3_cola_f_cs]=="",e3_csform_h[e3_rowa_f_cs,e3_cola_f_cs] <- e3_csform_a[e3_rowa_f_cs,e3_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowh_f_cs in 1:nrow(ec_csform_h)) {
#   for(ec_colh_f_cs in 1:ncol(ec_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowa_f_cs in 1:nrow(ec_csform_a)) {
#       for(ec_cola_f_cs in 1:ncol(ec_csform_a)) {
#         ifelse(!ec_csform_a[ec_rowa_f_cs,ec_cola_f_cs]=="",ec_csform_h[ec_rowa_f_cs,ec_cola_f_cs] <- ec_csform_a[ec_rowa_f_cs,ec_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowh_f_cs in 1:nrow(f1_csform_h)) {
#   for(f1_colh_f_cs in 1:ncol(f1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowa_f_cs in 1:nrow(f1_csform_a)) {
#       for(f1_cola_f_cs in 1:ncol(f1_csform_a)) {
#         ifelse(!f1_csform_a[f1_rowa_f_cs,f1_cola_f_cs]=="",f1_csform_h[f1_rowa_f_cs,f1_cola_f_cs] <- f1_csform_a[f1_rowa_f_cs,f1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowh_f_cs in 1:nrow(f2_csform_h)) {
#   for(f2_colh_f_cs in 1:ncol(f2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowa_f_cs in 1:nrow(f2_csform_a)) {
#       for(f2_cola_f_cs in 1:ncol(f2_csform_a)) {
#         ifelse(!f2_csform_a[f2_rowa_f_cs,f2_cola_f_cs]=="",f2_csform_h[f2_rowa_f_cs,f2_cola_f_cs] <- f2_csform_a[f2_rowa_f_cs,f2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowh_f_cs in 1:nrow(g1_csform_h)) {
#   for(g1_colh_f_cs in 1:ncol(g1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowa_f_cs in 1:nrow(g1_csform_a)) {
#       for(g1_cola_f_cs in 1:ncol(g1_csform_a)) {
#         ifelse(!g1_csform_a[g1_rowa_f_cs,g1_cola_f_cs]=="",g1_csform_h[g1_rowa_f_cs,g1_cola_f_cs] <- g1_csform_a[g1_rowa_f_cs,g1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowh_f_cs in 1:nrow(i1_csform_h)) {
#   for(i1_colh_f_cs in 1:ncol(i1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowa_f_cs in 1:nrow(i1_csform_a)) {
#       for(i1_cola_f_cs in 1:ncol(i1_csform_a)) {
#         ifelse(!i1_csform_a[i1_rowa_f_cs,i1_cola_f_cs]=="",i1_csform_h[i1_rowa_f_cs,i1_cola_f_cs] <- i1_csform_a[i1_rowa_f_cs,i1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowh_f_cs in 1:nrow(i2_csform_h)) {
#   for(i2_colh_f_cs in 1:ncol(i2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowa_f_cs in 1:nrow(i2_csform_a)) {
#       for(i2_cola_f_cs in 1:ncol(i2_csform_a)) {
#         ifelse(!i2_csform_a[i2_rowa_f_cs,i2_cola_f_cs]=="",i2_csform_h[i2_rowa_f_cs,i2_cola_f_cs] <- i2_csform_a[i2_rowa_f_cs,i2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowh_f_cs in 1:nrow(n1_csform_h)) {
#   for(n1_colh_f_cs in 1:ncol(n1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowa_f_cs in 1:nrow(n1_csform_a)) {
#       for(n1_cola_f_cs in 1:ncol(n1_csform_a)) {
#         ifelse(!n1_csform_a[n1_rowa_f_cs,n1_cola_f_cs]=="",n1_csform_h[n1_rowa_f_cs,n1_cola_f_cs] <- n1_csform_a[n1_rowa_f_cs,n1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowh_f_cs in 1:nrow(p1_csform_h)) {
#   for(p1_colh_f_cs in 1:ncol(p1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowa_f_cs in 1:nrow(p1_csform_a)) {
#       for(p1_cola_f_cs in 1:ncol(p1_csform_a)) {
#         ifelse(!p1_csform_a[p1_rowa_f_cs,p1_cola_f_cs]=="",p1_csform_h[p1_rowa_f_cs,p1_cola_f_cs] <- p1_csform_a[p1_rowa_f_cs,p1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowh_f_cs in 1:nrow(sc0_csform_h)) {
#   for(sc0_colh_f_cs in 1:ncol(sc0_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowa_f_cs in 1:nrow(sc0_csform_a)) {
#       for(sc0_cola_f_cs in 1:ncol(sc0_csform_a)) {
#         ifelse(!sc0_csform_a[sc0_rowa_f_cs,sc0_cola_f_cs]=="",sc0_csform_h[sc0_rowa_f_cs,sc0_cola_f_cs] <- sc0_csform_a[sc0_rowa_f_cs,sc0_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowh_f_cs in 1:nrow(sc1_csform_h)) {
#   for(sc1_colh_f_cs in 1:ncol(sc1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowa_f_cs in 1:nrow(sc1_csform_a)) {
#       for(sc1_cola_f_cs in 1:ncol(sc1_csform_a)) {
#         ifelse(!sc1_csform_a[sc1_rowa_f_cs,sc1_cola_f_cs]=="",sc1_csform_h[sc1_rowa_f_cs,sc1_cola_f_cs] <- sc1_csform_a[sc1_rowa_f_cs,sc1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowh_f_cs in 1:nrow(sc2_csform_h)) {
#   for(sc2_colh_f_cs in 1:ncol(sc2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowa_f_cs in 1:nrow(sc2_csform_a)) {
#       for(sc2_cola_f_cs in 1:ncol(sc2_csform_a)) {
#         ifelse(!sc2_csform_a[sc2_rowa_f_cs,sc2_cola_f_cs]=="",sc2_csform_h[sc2_rowa_f_cs,sc2_cola_f_cs] <- sc2_csform_a[sc2_rowa_f_cs,sc2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowh_f_cs in 1:nrow(sc3_csform_h)) {
#   for(sc3_colh_f_cs in 1:ncol(sc3_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowa_f_cs in 1:nrow(sc3_csform_a)) {
#       for(sc3_cola_f_cs in 1:ncol(sc3_csform_a)) {
#         ifelse(!sc3_csform_a[sc3_rowa_f_cs,sc3_cola_f_cs]=="",sc3_csform_h[sc3_rowa_f_cs,sc3_cola_f_cs] <- sc3_csform_a[sc3_rowa_f_cs,sc3_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowh_f_cs in 1:nrow(sp1_csform_h)) {
#   for(sp1_colh_f_cs in 1:ncol(sp1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowa_f_cs in 1:nrow(sp1_csform_a)) {
#       for(sp1_cola_f_cs in 1:ncol(sp1_csform_a)) {
#         ifelse(!sp1_csform_a[sp1_rowa_f_cs,sp1_cola_f_cs]=="",sp1_csform_h[sp1_rowa_f_cs,sp1_cola_f_cs] <- sp1_csform_a[sp1_rowa_f_cs,sp1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowh_f_cs in 1:nrow(sp2_csform_h)) {
#   for(sp2_colh_f_cs in 1:ncol(sp2_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowa_f_cs in 1:nrow(sp2_csform_a)) {
#       for(sp2_cola_f_cs in 1:ncol(sp2_csform_a)) {
#         ifelse(!sp2_csform_a[sp2_rowa_f_cs,sp2_cola_f_cs]=="",sp2_csform_h[sp2_rowa_f_cs,sp2_cola_f_cs] <- sp2_csform_a[sp2_rowa_f_cs,sp2_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowh_f_cs in 1:nrow(t1_csform_h)) {
#   for(t1_colh_f_cs in 1:ncol(t1_csform_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowa_f_cs in 1:nrow(t1_csform_a)) {
#       for(t1_cola_f_cs in 1:ncol(t1_csform_a)) {
#         ifelse(!t1_csform_a[t1_rowa_f_cs,t1_cola_f_cs]=="",t1_csform_h[t1_rowa_f_cs,t1_cola_f_cs] <- t1_csform_a[t1_rowa_f_cs,t1_cola_f_cs],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
