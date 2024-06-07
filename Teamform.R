#Create Home and Away Form
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#create home and away form matrices
mls_form_h <- tapply(MLS$FTR, MLS[c("HomeTeam", "Date")],median)
mls_form_a <- tapply(MLS$FTR, MLS[c("AwayTeam", "Date")],median)
arg_form_h <- tapply(ARG$FTR, ARG[c("HomeTeam", "Date")],median)
arg_form_a <- tapply(ARG$FTR, ARG[c("AwayTeam", "Date")],median)
# d2_form_h <- tapply(D2$FTR, D2[c("HomeTeam", "Date")],median)
# d2_form_a <- tapply(D2$FTR, D2[c("AwayTeam", "Date")],median)
# e0_form_h <- tapply(E0$FTR, E0[c("HomeTeam", "Date")],median)
# e0_form_a <- tapply(E0$FTR, E0[c("AwayTeam", "Date")],median)
# e1_form_h <- tapply(E1$FTR, E1[c("HomeTeam", "Date")],median)
# e1_form_a <- tapply(E1$FTR, E1[c("AwayTeam", "Date")],median)
# e2_form_h <- tapply(E2$FTR, E2[c("HomeTeam", "Date")],median)
# e2_form_a <- tapply(E2$FTR, E2[c("AwayTeam", "Date")],median)
# e3_form_h <- tapply(E3$FTR, E3[c("HomeTeam", "Date")],median)
# e3_form_a <- tapply(E3$FTR, E3[c("AwayTeam", "Date")],median)
# ec_form_h <- tapply(EC$FTR, EC[c("HomeTeam", "Date")],median)
# ec_form_a <- tapply(EC$FTR, EC[c("AwayTeam", "Date")],median)
# f1_form_h <- tapply(F1$FTR, F1[c("HomeTeam", "Date")],median)
# f1_form_a <- tapply(F1$FTR, F1[c("AwayTeam", "Date")],median)
# f2_form_h <- tapply(F2$FTR, F2[c("HomeTeam", "Date")],median)
# f2_form_a <- tapply(F2$FTR, F2[c("AwayTeam", "Date")],median)
# g1_form_h <- tapply(G1$FTR, G1[c("HomeTeam", "Date")],median)
# g1_form_a <- tapply(G1$FTR, G1[c("AwayTeam", "Date")],median)
# i1_form_h <- tapply(I1$FTR, I1[c("HomeTeam", "Date")],median)
# i1_form_a <- tapply(I1$FTR, I1[c("AwayTeam", "Date")],median)
# i2_form_h <- tapply(I2$FTR, I2[c("HomeTeam", "Date")],median)
# i2_form_a <- tapply(I2$FTR, I2[c("AwayTeam", "Date")],median)
# n1_form_h <- tapply(N1$FTR, N1[c("HomeTeam", "Date")],median)
# n1_form_a <- tapply(N1$FTR, N1[c("AwayTeam", "Date")],median)
# p1_form_h <- tapply(P1$FTR, P1[c("HomeTeam", "Date")],median)
# p1_form_a <- tapply(P1$FTR, P1[c("AwayTeam", "Date")],median)
# sc0_form_h <- tapply(SC0$FTR, SC0[c("HomeTeam", "Date")],median)
# sc0_form_a <- tapply(SC0$FTR, SC0[c("AwayTeam", "Date")],median)
# sc1_form_h <- tapply(SC1$FTR, SC1[c("HomeTeam", "Date")],median)
# sc1_form_a <- tapply(SC1$FTR, SC1[c("AwayTeam", "Date")],median)
# sc2_form_h <- tapply(SC2$FTR, SC2[c("HomeTeam", "Date")],median)
# sc2_form_a <- tapply(SC2$FTR, SC2[c("AwayTeam", "Date")],median)
# sc3_form_h <- tapply(SC3$FTR, SC3[c("HomeTeam", "Date")],median)
# sc3_form_a <- tapply(SC3$FTR, SC3[c("AwayTeam", "Date")],median)
# sp1_form_h <- tapply(SP1$FTR, SP1[c("HomeTeam", "Date")],median)
# sp1_form_a <- tapply(SP1$FTR, SP1[c("AwayTeam", "Date")],median)
# sp2_form_h <- tapply(SP2$FTR, SP2[c("HomeTeam", "Date")],median)
# sp2_form_a <- tapply(SP2$FTR, SP2[c("AwayTeam", "Date")],median)
# t1_form_h <- tapply(T1$FTR, T1[c("HomeTeam", "Date")],median)
# t1_form_a <- tapply(T1$FTR, T1[c("AwayTeam", "Date")],median)

#remove na values
mls_form_h[is.na(mls_form_h)] <- ""
mls_form_a[is.na(mls_form_a)] <- ""
arg_form_h[is.na(arg_form_h)] <- ""
arg_form_a[is.na(arg_form_a)] <- ""
# d2_form_h[is.na(d2_form_h)] <- ""
# d2_form_a[is.na(d2_form_a)] <- ""
# e0_form_h[is.na(e0_form_h)] <- ""
# e0_form_a[is.na(e0_form_a)] <- ""
# e1_form_h[is.na(e1_form_h)] <- ""
# e1_form_a[is.na(e1_form_a)] <- ""
# e2_form_h[is.na(e2_form_h)] <- ""
# e2_form_a[is.na(e2_form_a)] <- ""
# e3_form_h[is.na(e3_form_h)] <- ""
# e3_form_a[is.na(e3_form_a)] <- ""
# ec_form_h[is.na(ec_form_h)] <- ""
# ec_form_a[is.na(ec_form_a)] <- ""
# f1_form_h[is.na(f1_form_h)] <- ""
# f1_form_a[is.na(f1_form_a)] <- ""
# f2_form_h[is.na(f2_form_h)] <- ""
# f2_form_a[is.na(f2_form_a)] <- ""
# g1_form_h[is.na(g1_form_h)] <- ""
# g1_form_a[is.na(g1_form_a)] <- ""
# i1_form_h[is.na(i1_form_h)] <- ""
# i1_form_a[is.na(i1_form_a)] <- ""
# i2_form_h[is.na(i2_form_h)] <- ""
# i2_form_a[is.na(i2_form_a)] <- ""
# n1_form_h[is.na(n1_form_h)] <- ""
# n1_form_a[is.na(n1_form_a)] <- ""
# p1_form_h[is.na(p1_form_h)] <- ""
# p1_form_a[is.na(p1_form_a)] <- ""
# sc0_form_h[is.na(sc0_form_h)] <- ""
# sc0_form_a[is.na(sc0_form_a)] <- ""
# sc1_form_h[is.na(sc1_form_h)] <- ""
# sc1_form_a[is.na(sc1_form_a)] <- ""
# sc2_form_h[is.na(sc2_form_h)] <- ""
# sc2_form_a[is.na(sc2_form_a)] <- ""
# sc3_form_h[is.na(sc3_form_h)] <- ""
# sc3_form_a[is.na(sc3_form_a)] <- ""
# sp1_form_h[is.na(sp1_form_h)] <- ""
# sp1_form_a[is.na(sp1_form_a)] <- ""
# sp2_form_h[is.na(sp2_form_h)] <- ""
# sp2_form_a[is.na(sp2_form_a)] <- ""
# t1_form_h[is.na(t1_form_h)] <- ""
# t1_form_a[is.na(t1_form_a)] <- ""
#replace wins with W and losses with L
#MLS
mls_form_h <- sub("A","L",mls_form_h)
mls_form_h <- sub("H","W",mls_form_h)
mls_form_a <- sub("A","W",mls_form_a)
mls_form_a <- sub("H","L",mls_form_a)
#ARG
arg_form_h <- sub("A","L",arg_form_h)
arg_form_h <- sub("H","W",arg_form_h)
arg_form_a <- sub("A","W",arg_form_a)
arg_form_a <- sub("H","L",arg_form_a)
# #D2
# d2_form_h <- sub("A","L",d2_form_h)
# d2_form_h <- sub("H","W",d2_form_h)
# d2_form_a <- sub("A","W",d2_form_a)
# d2_form_a <- sub("H","L",d2_form_a)
# #E0
# e0_form_h <- sub("A","L",e0_form_h)
# e0_form_h <- sub("H","W",e0_form_h)
# e0_form_a <- sub("A","W",e0_form_a)
# e0_form_a <- sub("H","L",e0_form_a)
# #E1
# e1_form_h <- sub("A","L",e1_form_h)
# e1_form_h <- sub("H","W",e1_form_h)
# e1_form_a <- sub("A","W",e1_form_a)
# e1_form_a <- sub("H","L",e1_form_a)
# #E2
# e2_form_h <- sub("A","L",e2_form_h)
# e2_form_h <- sub("H","W",e2_form_h)
# e2_form_a <- sub("A","W",e2_form_a)
# e2_form_a <- sub("H","L",e2_form_a)
# #E3
# e3_form_h <- sub("A","L",e3_form_h)
# e3_form_h <- sub("H","W",e3_form_h)
# e3_form_a <- sub("A","W",e3_form_a)
# e3_form_a <- sub("H","L",e3_form_a)
# #EC
# ec_form_h <- sub("A","L",ec_form_h)
# ec_form_h <- sub("H","W",ec_form_h)
# ec_form_a <- sub("A","W",ec_form_a)
# ec_form_a <- sub("H","L",ec_form_a)
# #F1
# f1_form_h <- sub("A","L",f1_form_h)
# f1_form_h <- sub("H","W",f1_form_h)
# f1_form_a <- sub("A","W",f1_form_a)
# f1_form_a <- sub("H","L",f1_form_a)
# #F2
# f2_form_h <- sub("A","L",f2_form_h)
# f2_form_h <- sub("H","W",f2_form_h)
# f2_form_a <- sub("A","W",f2_form_a)
# f2_form_a <- sub("H","L",f2_form_a)
# #G1
# g1_form_h <- sub("A","L",g1_form_h)
# g1_form_h <- sub("H","W",g1_form_h)
# g1_form_a <- sub("A","W",g1_form_a)
# g1_form_a <- sub("H","L",g1_form_a)
# #I1
# i1_form_h <- sub("A","L",i1_form_h)
# i1_form_h <- sub("H","W",i1_form_h)
# i1_form_a <- sub("A","W",i1_form_a)
# i1_form_a <- sub("H","L",i1_form_a)
# #I2
# i2_form_h <- sub("A","L",i2_form_h)
# i2_form_h <- sub("H","W",i2_form_h)
# i2_form_a <- sub("A","W",i2_form_a)
# i2_form_a <- sub("H","L",i2_form_a)
# #N1
# n1_form_h <- sub("A","L",n1_form_h)
# n1_form_h <- sub("H","W",n1_form_h)
# n1_form_a <- sub("A","W",n1_form_a)
# n1_form_a <- sub("H","L",n1_form_a)
# #P1
# p1_form_h <- sub("A","L",p1_form_h)
# p1_form_h <- sub("H","W",p1_form_h)
# p1_form_a <- sub("A","W",p1_form_a)
# p1_form_a <- sub("H","L",p1_form_a)
# #SC0
# sc0_form_h <- sub("A","L",sc0_form_h)
# sc0_form_h <- sub("H","W",sc0_form_h)
# sc0_form_a <- sub("A","W",sc0_form_a)
# sc0_form_a <- sub("H","L",sc0_form_a)
# #SC1
# sc1_form_h <- sub("A","L",sc1_form_h)
# sc1_form_h <- sub("H","W",sc1_form_h)
# sc1_form_a <- sub("A","W",sc1_form_a)
# sc1_form_a <- sub("H","L",sc1_form_a)
# #SC2
# sc2_form_h <- sub("A","L",sc2_form_h)
# sc2_form_h <- sub("H","W",sc2_form_h)
# sc2_form_a <- sub("A","W",sc2_form_a)
# sc2_form_a <- sub("H","L",sc2_form_a)
# #SC3
# sc3_form_h <- sub("A","L",sc3_form_h)
# sc3_form_h <- sub("H","W",sc3_form_h)
# sc3_form_a <- sub("A","W",sc3_form_a)
# sc3_form_a <- sub("H","L",sc3_form_a)
# #SP1
# sp1_form_h <- sub("A","L",sp1_form_h)
# sp1_form_h <- sub("H","W",sp1_form_h)
# sp1_form_a <- sub("A","W",sp1_form_a)
# sp1_form_a <- sub("H","L",sp1_form_a)
# #SP2
# sp2_form_h <- sub("A","L",sp2_form_h)
# sp2_form_h <- sub("H","W",sp2_form_h)
# sp2_form_a <- sub("A","W",sp2_form_a)
# sp2_form_a <- sub("H","L",sp2_form_a)
# #T1
# t1_form_h <- sub("A","L",t1_form_h)
# t1_form_h <- sub("H","W",t1_form_h)
# t1_form_a <- sub("A","W",t1_form_a)
# t1_form_a <- sub("H","L",t1_form_a)
#combine the matrices
#MLS
for(mls_rowh_f in 1:nrow(mls_form_h)) {
  for(mls_colh_f in 1:ncol(mls_form_h)) {

    # print(my_matrix[row, col])
    for(mls_rowa_f in 1:nrow(mls_form_a)) {
      for(mls_cola_f in 1:ncol(mls_form_a)) {
        ifelse(!mls_form_a[mls_rowa_f,mls_cola_f]=="",mls_form_h[mls_rowa_f,mls_cola_f] <- mls_form_a[mls_rowa_f,mls_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowh_f in 1:nrow(arg_form_h)) {
  for(arg_colh_f in 1:ncol(arg_form_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa_f in 1:nrow(arg_form_a)) {
      for(arg_cola_f in 1:ncol(arg_form_a)) {
        ifelse(!arg_form_a[arg_rowa_f,arg_cola_f]=="",arg_form_h[arg_rowa_f,arg_cola_f] <- arg_form_a[arg_rowa_f,arg_cola_f],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowh_f in 1:nrow(d2_form_h)) {
#   for(d2_colh_f in 1:ncol(d2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowa_f in 1:nrow(d2_form_a)) {
#       for(d2_cola_f in 1:ncol(d2_form_a)) {
#         ifelse(!d2_form_a[d2_rowa_f,d2_cola_f]=="",d2_form_h[d2_rowa_f,d2_cola_f] <- d2_form_a[d2_rowa_f,d2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowh_f in 1:nrow(e0_form_h)) {
#   for(e0_colh_f in 1:ncol(e0_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowa_f in 1:nrow(e0_form_a)) {
#       for(e0_cola_f in 1:ncol(e0_form_a)) {
#         ifelse(!e0_form_a[e0_rowa_f,e0_cola_f]=="",e0_form_h[e0_rowa_f,e0_cola_f] <- e0_form_a[e0_rowa_f,e0_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowh_f in 1:nrow(e1_form_h)) {
#   for(e1_colh_f in 1:ncol(e1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowa_f in 1:nrow(e1_form_a)) {
#       for(e1_cola_f in 1:ncol(e1_form_a)) {
#         ifelse(!e1_form_a[e1_rowa_f,e1_cola_f]=="",e1_form_h[e1_rowa_f,e1_cola_f] <- e1_form_a[e1_rowa_f,e1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowh_f in 1:nrow(e2_form_h)) {
#   for(e2_colh_f in 1:ncol(e2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowa_f in 1:nrow(e2_form_a)) {
#       for(e2_cola_f in 1:ncol(e2_form_a)) {
#         ifelse(!e2_form_a[e2_rowa_f,e2_cola_f]=="",e2_form_h[e2_rowa_f,e2_cola_f] <- e2_form_a[e2_rowa_f,e2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowh_f in 1:nrow(e3_form_h)) {
#   for(e3_colh_f in 1:ncol(e3_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowa_f in 1:nrow(e3_form_a)) {
#       for(e3_cola_f in 1:ncol(e3_form_a)) {
#         ifelse(!e3_form_a[e3_rowa_f,e3_cola_f]=="",e3_form_h[e3_rowa_f,e3_cola_f] <- e3_form_a[e3_rowa_f,e3_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowh_f in 1:nrow(ec_form_h)) {
#   for(ec_colh_f in 1:ncol(ec_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowa_f in 1:nrow(ec_form_a)) {
#       for(ec_cola_f in 1:ncol(ec_form_a)) {
#         ifelse(!ec_form_a[ec_rowa_f,ec_cola_f]=="",ec_form_h[ec_rowa_f,ec_cola_f] <- ec_form_a[ec_rowa_f,ec_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowh_f in 1:nrow(f1_form_h)) {
#   for(f1_colh_f in 1:ncol(f1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowa_f in 1:nrow(f1_form_a)) {
#       for(f1_cola_f in 1:ncol(f1_form_a)) {
#         ifelse(!f1_form_a[f1_rowa_f,f1_cola_f]=="",f1_form_h[f1_rowa_f,f1_cola_f] <- f1_form_a[f1_rowa_f,f1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowh_f in 1:nrow(f2_form_h)) {
#   for(f2_colh_f in 1:ncol(f2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowa_f in 1:nrow(f2_form_a)) {
#       for(f2_cola_f in 1:ncol(f2_form_a)) {
#         ifelse(!f2_form_a[f2_rowa_f,f2_cola_f]=="",f2_form_h[f2_rowa_f,f2_cola_f] <- f2_form_a[f2_rowa_f,f2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowh_f in 1:nrow(g1_form_h)) {
#   for(g1_colh_f in 1:ncol(g1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowa_f in 1:nrow(g1_form_a)) {
#       for(g1_cola_f in 1:ncol(g1_form_a)) {
#         ifelse(!g1_form_a[g1_rowa_f,g1_cola_f]=="",g1_form_h[g1_rowa_f,g1_cola_f] <- g1_form_a[g1_rowa_f,g1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowh_f in 1:nrow(i1_form_h)) {
#   for(i1_colh_f in 1:ncol(i1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowa_f in 1:nrow(i1_form_a)) {
#       for(i1_cola_f in 1:ncol(i1_form_a)) {
#         ifelse(!i1_form_a[i1_rowa_f,i1_cola_f]=="",i1_form_h[i1_rowa_f,i1_cola_f] <- i1_form_a[i1_rowa_f,i1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowh_f in 1:nrow(i2_form_h)) {
#   for(i2_colh_f in 1:ncol(i2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowa_f in 1:nrow(i2_form_a)) {
#       for(i2_cola_f in 1:ncol(i2_form_a)) {
#         ifelse(!i2_form_a[i2_rowa_f,i2_cola_f]=="",i2_form_h[i2_rowa_f,i2_cola_f] <- i2_form_a[i2_rowa_f,i2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowh_f in 1:nrow(n1_form_h)) {
#   for(n1_colh_f in 1:ncol(n1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowa_f in 1:nrow(n1_form_a)) {
#       for(n1_cola_f in 1:ncol(n1_form_a)) {
#         ifelse(!n1_form_a[n1_rowa_f,n1_cola_f]=="",n1_form_h[n1_rowa_f,n1_cola_f] <- n1_form_a[n1_rowa_f,n1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowh_f in 1:nrow(p1_form_h)) {
#   for(p1_colh_f in 1:ncol(p1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowa_f in 1:nrow(p1_form_a)) {
#       for(p1_cola_f in 1:ncol(p1_form_a)) {
#         ifelse(!p1_form_a[p1_rowa_f,p1_cola_f]=="",p1_form_h[p1_rowa_f,p1_cola_f] <- p1_form_a[p1_rowa_f,p1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowh_f in 1:nrow(sc0_form_h)) {
#   for(sc0_colh_f in 1:ncol(sc0_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowa_f in 1:nrow(sc0_form_a)) {
#       for(sc0_cola_f in 1:ncol(sc0_form_a)) {
#         ifelse(!sc0_form_a[sc0_rowa_f,sc0_cola_f]=="",sc0_form_h[sc0_rowa_f,sc0_cola_f] <- sc0_form_a[sc0_rowa_f,sc0_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowh_f in 1:nrow(sc1_form_h)) {
#   for(sc1_colh_f in 1:ncol(sc1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowa_f in 1:nrow(sc1_form_a)) {
#       for(sc1_cola_f in 1:ncol(sc1_form_a)) {
#         ifelse(!sc1_form_a[sc1_rowa_f,sc1_cola_f]=="",sc1_form_h[sc1_rowa_f,sc1_cola_f] <- sc1_form_a[sc1_rowa_f,sc1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowh_f in 1:nrow(sc2_form_h)) {
#   for(sc2_colh_f in 1:ncol(sc2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowa_f in 1:nrow(sc2_form_a)) {
#       for(sc2_cola_f in 1:ncol(sc2_form_a)) {
#         ifelse(!sc2_form_a[sc2_rowa_f,sc2_cola_f]=="",sc2_form_h[sc2_rowa_f,sc2_cola_f] <- sc2_form_a[sc2_rowa_f,sc2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowh_f in 1:nrow(sc3_form_h)) {
#   for(sc3_colh_f in 1:ncol(sc3_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowa_f in 1:nrow(sc3_form_a)) {
#       for(sc3_cola_f in 1:ncol(sc3_form_a)) {
#         ifelse(!sc3_form_a[sc3_rowa_f,sc3_cola_f]=="",sc3_form_h[sc3_rowa_f,sc3_cola_f] <- sc3_form_a[sc3_rowa_f,sc3_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowh_f in 1:nrow(sp1_form_h)) {
#   for(sp1_colh_f in 1:ncol(sp1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowa_f in 1:nrow(sp1_form_a)) {
#       for(sp1_cola_f in 1:ncol(sp1_form_a)) {
#         ifelse(!sp1_form_a[sp1_rowa_f,sp1_cola_f]=="",sp1_form_h[sp1_rowa_f,sp1_cola_f] <- sp1_form_a[sp1_rowa_f,sp1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowh_f in 1:nrow(sp2_form_h)) {
#   for(sp2_colh_f in 1:ncol(sp2_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowa_f in 1:nrow(sp2_form_a)) {
#       for(sp2_cola_f in 1:ncol(sp2_form_a)) {
#         ifelse(!sp2_form_a[sp2_rowa_f,sp2_cola_f]=="",sp2_form_h[sp2_rowa_f,sp2_cola_f] <- sp2_form_a[sp2_rowa_f,sp2_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowh_f in 1:nrow(t1_form_h)) {
#   for(t1_colh_f in 1:ncol(t1_form_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowa_f in 1:nrow(t1_form_a)) {
#       for(t1_cola_f in 1:ncol(t1_form_a)) {
#         ifelse(!t1_form_a[t1_rowa_f,t1_cola_f]=="",t1_form_h[t1_rowa_f,t1_cola_f] <- t1_form_a[t1_rowa_f,t1_cola_f],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }

#Write out to excel

# write.xlsx(mls_form_h,'Teamform.xlsx',sheetName = "MLS")
# write.xlsx(arg_form_h,'Teamform.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_form_h,'Teamform.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_form_h,'Teamform.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_form_h,'Teamform.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_form_h,'Teamform.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_form_h,'Teamform.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_form_h,'Teamform.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_form_h,'Teamform.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_form_h,'Teamform.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_form_h,'Teamform.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_form_h,'Teamform.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_form_h,'Teamform.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_form_h,'Teamform.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_form_h,'Teamform.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_form_h,'Teamform.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_form_h,'Teamform.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_form_h,'Teamform.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_form_h,'Teamform.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_form_h,'Teamform.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_form_h,'Teamform.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_form_h,'Teamform.xlsx',sheetName = "T1", append = TRUE)

