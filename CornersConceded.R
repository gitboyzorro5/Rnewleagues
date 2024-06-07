#create Goals Conceded form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_cornersconceded_h <- tapply(MLS$ACO, MLS[c("HomeTeam", "Date")],mean)
mls_cornersconceded_a <- tapply(MLS$HCO, MLS[c("AwayTeam", "Date")],mean)
arg_cornersconceded_h <- tapply(ARG$ACO, ARG[c("HomeTeam", "Date")],mean)
arg_cornersconceded_a <- tapply(ARG$HCO, ARG[c("AwayTeam", "Date")],mean)
# d2_cornersconceded_h <- tapply(D2$ACO, D2[c("HomeTeam", "Date")],mean)
# d2_cornersconceded_a <- tapply(D2$HCO, D2[c("AwayTeam", "Date")],mean)
# e0_cornersconceded_h <- tapply(E0$ACO, E0[c("HomeTeam", "Date")],mean)
# e0_cornersconceded_a <- tapply(E0$HCO, E0[c("AwayTeam", "Date")],mean)
# e1_cornersconceded_h <- tapply(E1$ACO, E1[c("HomeTeam", "Date")],mean)
# e1_cornersconceded_a <- tapply(E1$HCO, E1[c("AwayTeam", "Date")],mean)
# e2_cornersconceded_h <- tapply(E2$ACO, E2[c("HomeTeam", "Date")],mean)
# e2_cornersconceded_a <- tapply(E2$HCO, E2[c("AwayTeam", "Date")],mean)
# e3_cornersconceded_h <- tapply(E3$ACO, E3[c("HomeTeam", "Date")],mean)
# e3_cornersconceded_a <- tapply(E3$HCO, E3[c("AwayTeam", "Date")],mean)
# ec_cornersconceded_h <- tapply(EC$ACO, EC[c("HomeTeam", "Date")],mean)
# ec_cornersconceded_a <- tapply(EC$HCO, EC[c("AwayTeam", "Date")],mean)
# f1_cornersconceded_h <- tapply(F1$ACO, F1[c("HomeTeam", "Date")],mean)
# f1_cornersconceded_a <- tapply(F1$HCO, F1[c("AwayTeam", "Date")],mean)
# f2_cornersconceded_h <- tapply(F2$ACO, F2[c("HomeTeam", "Date")],mean)
# f2_cornersconceded_a <- tapply(F2$HCO, F2[c("AwayTeam", "Date")],mean)
# g1_cornersconceded_h <- tapply(G1$ACO, G1[c("HomeTeam", "Date")],mean)
# g1_cornersconceded_a <- tapply(G1$HCO, G1[c("AwayTeam", "Date")],mean)
# i1_cornersconceded_h <- tapply(I1$ACO, I1[c("HomeTeam", "Date")],mean)
# i1_cornersconceded_a <- tapply(I1$HCO, I1[c("AwayTeam", "Date")],mean)
# i2_cornersconceded_h <- tapply(I2$ACO, I2[c("HomeTeam", "Date")],mean)
# i2_cornersconceded_a <- tapply(I2$HCO, I2[c("AwayTeam", "Date")],mean)
# n1_cornersconceded_h <- tapply(N1$ACO, N1[c("HomeTeam", "Date")],mean)
# n1_cornersconceded_a <- tapply(N1$HCO, N1[c("AwayTeam", "Date")],mean)
# p1_cornersconceded_h <- tapply(P1$ACO, P1[c("HomeTeam", "Date")],mean)
# p1_cornersconceded_a <- tapply(P1$HCO, P1[c("AwayTeam", "Date")],mean)
# sc0_cornersconceded_h <- tapply(SC0$ACO, SC0[c("HomeTeam", "Date")],mean)
# sc0_cornersconceded_a <- tapply(SC0$HCO, SC0[c("AwayTeam", "Date")],mean)
# sc1_cornersconceded_h <- tapply(SC1$ACO, SC1[c("HomeTeam", "Date")],mean)
# sc1_cornersconceded_a <- tapply(SC1$HCO, SC1[c("AwayTeam", "Date")],mean)
# sc2_cornersconceded_h <- tapply(SC2$ACO, SC2[c("HomeTeam", "Date")],mean)
# sc2_cornersconceded_a <- tapply(SC2$HCO, SC2[c("AwayTeam", "Date")],mean)
# sc3_cornersconceded_h <- tapply(SC3$ACO, SC3[c("HomeTeam", "Date")],mean)
# sc3_cornersconceded_a <- tapply(SC3$HCO, SC3[c("AwayTeam", "Date")],mean)
# sp1_cornersconceded_h <- tapply(SP1$ACO, SP1[c("HomeTeam", "Date")],mean)
# sp1_cornersconceded_a <- tapply(SP1$HCO, SP1[c("AwayTeam", "Date")],mean)
# sp2_cornersconceded_h <- tapply(SP2$ACO, SP2[c("HomeTeam", "Date")],mean)
# sp2_cornersconceded_a <- tapply(SP2$HCO, SP2[c("AwayTeam", "Date")],mean)
# t1_cornersconceded_h <- tapply(T1$ACO, T1[c("HomeTeam", "Date")],mean)
# t1_cornersconceded_a <- tapply(T1$HCO, T1[c("AwayTeam", "Date")],mean)
#remove na values

mls_cornersconceded_h[is.na(mls_cornersconceded_h)] <- ""
mls_cornersconceded_a[is.na(mls_cornersconceded_a)] <- ""
arg_cornersconceded_h[is.na(arg_cornersconceded_h)] <- ""
arg_cornersconceded_a[is.na(arg_cornersconceded_a)] <- ""
# d2_cornersconceded_h[is.na(d2_cornersconceded_h)] <- ""
# d2_cornersconceded_a[is.na(d2_cornersconceded_a)] <- ""
# e0_cornersconceded_h[is.na(e0_cornersconceded_h)] <- ""
# e0_cornersconceded_a[is.na(e0_cornersconceded_a)] <- ""
# e1_cornersconceded_h[is.na(e1_cornersconceded_h)] <- ""
# e1_cornersconceded_a[is.na(e1_cornersconceded_a)] <- ""
# e2_cornersconceded_h[is.na(e2_cornersconceded_h)] <- ""
# e2_cornersconceded_a[is.na(e2_cornersconceded_a)] <- ""
# e3_cornersconceded_h[is.na(e3_cornersconceded_h)] <- ""
# e3_cornersconceded_a[is.na(e3_cornersconceded_a)] <- ""
# ec_cornersconceded_h[is.na(ec_cornersconceded_h)] <- ""
# ec_cornersconceded_a[is.na(ec_cornersconceded_a)] <- ""
# f1_cornersconceded_h[is.na(f1_cornersconceded_h)] <- ""
# f1_cornersconceded_a[is.na(f1_cornersconceded_a)] <- ""
# f2_cornersconceded_h[is.na(f2_cornersconceded_h)] <- ""
# f2_cornersconceded_a[is.na(f2_cornersconceded_a)] <- ""
# g1_cornersconceded_h[is.na(g1_cornersconceded_h)] <- ""
# g1_cornersconceded_a[is.na(g1_cornersconceded_a)] <- ""
# i1_cornersconceded_h[is.na(i1_cornersconceded_h)] <- ""
# i1_cornersconceded_a[is.na(i1_cornersconceded_a)] <- ""
# i2_cornersconceded_h[is.na(i2_cornersconceded_h)] <- ""
# i2_cornersconceded_a[is.na(i2_cornersconceded_a)] <- ""
# n1_cornersconceded_h[is.na(n1_cornersconceded_h)] <- ""
# n1_cornersconceded_a[is.na(n1_cornersconceded_a)] <- ""
# p1_cornersconceded_h[is.na(p1_cornersconceded_h)] <- ""
# p1_cornersconceded_a[is.na(p1_cornersconceded_a)] <- ""
# sc0_cornersconceded_h[is.na(sc0_cornersconceded_h)] <- ""
# sc0_cornersconceded_a[is.na(sc0_cornersconceded_a)] <- ""
# sc1_cornersconceded_h[is.na(sc1_cornersconceded_h)] <- ""
# sc1_cornersconceded_a[is.na(sc1_cornersconceded_a)] <- ""
# sc2_cornersconceded_h[is.na(sc2_cornersconceded_h)] <- ""
# sc2_cornersconceded_a[is.na(sc2_cornersconceded_a)] <- ""
# sc3_cornersconceded_h[is.na(sc3_cornersconceded_h)] <- ""
# sc3_cornersconceded_a[is.na(sc3_cornersconceded_a)] <- ""
# sp1_cornersconceded_h[is.na(sp1_cornersconceded_h)] <- ""
# sp1_cornersconceded_a[is.na(sp1_cornersconceded_a)] <- ""
# sp2_cornersconceded_h[is.na(sp2_cornersconceded_h)] <- ""
# sp2_cornersconceded_a[is.na(sp2_cornersconceded_a)] <- ""
# t1_cornersconceded_h[is.na(t1_cornersconceded_h)] <- ""
# t1_cornersconceded_a[is.na(t1_cornersconceded_a)] <- ""
# #combine the matrices
#MLS
for(mls_rowhcc in 1:nrow(mls_cornersconceded_h)) {
  for(mls_colhcc in 1:ncol(mls_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowacc in 1:nrow(mls_cornersconceded_a)) {
      for(mls_colacc in 1:ncol(mls_cornersconceded_a)) {
        ifelse(!mls_cornersconceded_a[mls_rowacc,mls_colacc]=="",mls_cornersconceded_h[mls_rowacc,mls_colacc] <- mls_cornersconceded_a[mls_rowacc,mls_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhcc in 1:nrow(arg_cornersconceded_h)) {
  for(arg_colhcc in 1:ncol(arg_cornersconceded_h)) {

    # print(my_matrix[row, col])
    for(arg_rowacc in 1:nrow(arg_cornersconceded_a)) {
      for(arg_colacc in 1:ncol(arg_cornersconceded_a)) {
        ifelse(!arg_cornersconceded_a[arg_rowacc,arg_colacc]=="",arg_cornersconceded_h[arg_rowacc,arg_colacc] <- arg_cornersconceded_a[arg_rowacc,arg_colacc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhcc in 1:nrow(d2_cornersconceded_h)) {
#   for(d2_colhcc in 1:ncol(d2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowacc in 1:nrow(d2_cornersconceded_a)) {
#       for(d2_colacc in 1:ncol(d2_cornersconceded_a)) {
#         ifelse(!d2_cornersconceded_a[d2_rowacc,d2_colacc]=="",d2_cornersconceded_h[d2_rowacc,d2_colacc] <- d2_cornersconceded_a[d2_rowacc,d2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhcc in 1:nrow(e0_cornersconceded_h)) {
#   for(e0_colhcc in 1:ncol(e0_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowacc in 1:nrow(e0_cornersconceded_a)) {
#       for(e0_colacc in 1:ncol(e0_cornersconceded_a)) {
#         ifelse(!e0_cornersconceded_a[e0_rowacc,e0_colacc]=="",e0_cornersconceded_h[e0_rowacc,e0_colacc] <- e0_cornersconceded_a[e0_rowacc,e0_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhcc in 1:nrow(e1_cornersconceded_h)) {
#   for(e1_colhcc in 1:ncol(e1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowacc in 1:nrow(e1_cornersconceded_a)) {
#       for(e1_colacc in 1:ncol(e1_cornersconceded_a)) {
#         ifelse(!e1_cornersconceded_a[e1_rowacc,e1_colacc]=="",e1_cornersconceded_h[e1_rowacc,e1_colacc] <- e1_cornersconceded_a[e1_rowacc,e1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhcc in 1:nrow(e2_cornersconceded_h)) {
#   for(e2_colhcc in 1:ncol(e2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowacc in 1:nrow(e2_cornersconceded_a)) {
#       for(e2_colacc in 1:ncol(e2_cornersconceded_a)) {
#         ifelse(!e2_cornersconceded_a[e2_rowacc,e2_colacc]=="",e2_cornersconceded_h[e2_rowacc,e2_colacc] <- e2_cornersconceded_a[e2_rowacc,e2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhcc in 1:nrow(e3_cornersconceded_h)) {
#   for(e3_colhcc in 1:ncol(e3_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowacc in 1:nrow(e3_cornersconceded_a)) {
#       for(e3_colacc in 1:ncol(e3_cornersconceded_a)) {
#         ifelse(!e3_cornersconceded_a[e3_rowacc,e3_colacc]=="",e3_cornersconceded_h[e3_rowacc,e3_colacc] <- e3_cornersconceded_a[e3_rowacc,e3_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhcc in 1:nrow(ec_cornersconceded_h)) {
#   for(ec_colhcc in 1:ncol(ec_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowacc in 1:nrow(ec_cornersconceded_a)) {
#       for(ec_colacc in 1:ncol(ec_cornersconceded_a)) {
#         ifelse(!ec_cornersconceded_a[ec_rowacc,ec_colacc]=="",ec_cornersconceded_h[ec_rowacc,ec_colacc] <- ec_cornersconceded_a[ec_rowacc,ec_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhcc in 1:nrow(f1_cornersconceded_h)) {
#   for(f1_colhcc in 1:ncol(f1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowacc in 1:nrow(f1_cornersconceded_a)) {
#       for(f1_colacc in 1:ncol(f1_cornersconceded_a)) {
#         ifelse(!f1_cornersconceded_a[f1_rowacc,f1_colacc]=="",f1_cornersconceded_h[f1_rowacc,f1_colacc] <- f1_cornersconceded_a[f1_rowacc,f1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhcc in 1:nrow(f2_cornersconceded_h)) {
#   for(f2_colhcc in 1:ncol(f2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowacc in 1:nrow(f2_cornersconceded_a)) {
#       for(f2_colacc in 1:ncol(f2_cornersconceded_a)) {
#         ifelse(!f2_cornersconceded_a[f2_rowacc,f2_colacc]=="",f2_cornersconceded_h[f2_rowacc,f2_colacc] <- f2_cornersconceded_a[f2_rowacc,f2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhcc in 1:nrow(g1_cornersconceded_h)) {
#   for(g1_colhcc in 1:ncol(g1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowacc in 1:nrow(g1_cornersconceded_a)) {
#       for(g1_colacc in 1:ncol(g1_cornersconceded_a)) {
#         ifelse(!g1_cornersconceded_a[g1_rowacc,g1_colacc]=="",g1_cornersconceded_h[g1_rowacc,g1_colacc] <- g1_cornersconceded_a[g1_rowacc,g1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhcc in 1:nrow(i1_cornersconceded_h)) {
#   for(i1_colhcc in 1:ncol(i1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowacc in 1:nrow(i1_cornersconceded_a)) {
#       for(i1_colacc in 1:ncol(i1_cornersconceded_a)) {
#         ifelse(!i1_cornersconceded_a[i1_rowacc,i1_colacc]=="",i1_cornersconceded_h[i1_rowacc,i1_colacc] <- i1_cornersconceded_a[i1_rowacc,i1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhcc in 1:nrow(i2_cornersconceded_h)) {
#   for(i2_colhcc in 1:ncol(i2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowacc in 1:nrow(i2_cornersconceded_a)) {
#       for(i2_colacc in 1:ncol(i2_cornersconceded_a)) {
#         ifelse(!i2_cornersconceded_a[i2_rowacc,i2_colacc]=="",i2_cornersconceded_h[i2_rowacc,i2_colacc] <- i2_cornersconceded_a[i2_rowacc,i2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhcc in 1:nrow(n1_cornersconceded_h)) {
#   for(n1_colhcc in 1:ncol(n1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowacc in 1:nrow(n1_cornersconceded_a)) {
#       for(n1_colacc in 1:ncol(n1_cornersconceded_a)) {
#         ifelse(!n1_cornersconceded_a[n1_rowacc,n1_colacc]=="",n1_cornersconceded_h[n1_rowacc,n1_colacc] <- n1_cornersconceded_a[n1_rowacc,n1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhcc in 1:nrow(p1_cornersconceded_h)) {
#   for(p1_colhcc in 1:ncol(p1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowacc in 1:nrow(p1_cornersconceded_a)) {
#       for(p1_colacc in 1:ncol(p1_cornersconceded_a)) {
#         ifelse(!p1_cornersconceded_a[p1_rowacc,p1_colacc]=="",p1_cornersconceded_h[p1_rowacc,p1_colacc] <- p1_cornersconceded_a[p1_rowacc,p1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhcc in 1:nrow(sc0_cornersconceded_h)) {
#   for(sc0_colhcc in 1:ncol(sc0_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowacc in 1:nrow(sc0_cornersconceded_a)) {
#       for(sc0_colacc in 1:ncol(sc0_cornersconceded_a)) {
#         ifelse(!sc0_cornersconceded_a[sc0_rowacc,sc0_colacc]=="",sc0_cornersconceded_h[sc0_rowacc,sc0_colacc] <- sc0_cornersconceded_a[sc0_rowacc,sc0_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhcc in 1:nrow(sc1_cornersconceded_h)) {
#   for(sc1_colhcc in 1:ncol(sc1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowacc in 1:nrow(sc1_cornersconceded_a)) {
#       for(sc1_colacc in 1:ncol(sc1_cornersconceded_a)) {
#         ifelse(!sc1_cornersconceded_a[sc1_rowacc,sc1_colacc]=="",sc1_cornersconceded_h[sc1_rowacc,sc1_colacc] <- sc1_cornersconceded_a[sc1_rowacc,sc1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhcc in 1:nrow(sc2_cornersconceded_h)) {
#   for(sc2_colhcc in 1:ncol(sc2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowacc in 1:nrow(sc2_cornersconceded_a)) {
#       for(sc2_colacc in 1:ncol(sc2_cornersconceded_a)) {
#         ifelse(!sc2_cornersconceded_a[sc2_rowacc,sc2_colacc]=="",sc2_cornersconceded_h[sc2_rowacc,sc2_colacc] <- sc2_cornersconceded_a[sc2_rowacc,sc2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhcc in 1:nrow(sc3_cornersconceded_h)) {
#   for(sc3_colhcc in 1:ncol(sc3_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowacc in 1:nrow(sc3_cornersconceded_a)) {
#       for(sc3_colacc in 1:ncol(sc3_cornersconceded_a)) {
#         ifelse(!sc3_cornersconceded_a[sc3_rowacc,sc3_colacc]=="",sc3_cornersconceded_h[sc3_rowacc,sc3_colacc] <- sc3_cornersconceded_a[sc3_rowacc,sc3_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhcc in 1:nrow(sp1_cornersconceded_h)) {
#   for(sp1_colhcc in 1:ncol(sp1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowacc in 1:nrow(sp1_cornersconceded_a)) {
#       for(sp1_colacc in 1:ncol(sp1_cornersconceded_a)) {
#         ifelse(!sp1_cornersconceded_a[sp1_rowacc,sp1_colacc]=="",sp1_cornersconceded_h[sp1_rowacc,sp1_colacc] <- sp1_cornersconceded_a[sp1_rowacc,sp1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhcc in 1:nrow(sp2_cornersconceded_h)) {
#   for(sp2_colhcc in 1:ncol(sp2_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowacc in 1:nrow(sp2_cornersconceded_a)) {
#       for(sp2_colacc in 1:ncol(sp2_cornersconceded_a)) {
#         ifelse(!sp2_cornersconceded_a[sp2_rowacc,sp2_colacc]=="",sp2_cornersconceded_h[sp2_rowacc,sp2_colacc] <- sp2_cornersconceded_a[sp2_rowacc,sp2_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhcc in 1:nrow(t1_cornersconceded_h)) {
#   for(t1_colhcc in 1:ncol(t1_cornersconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowacc in 1:nrow(t1_cornersconceded_a)) {
#       for(t1_colacc in 1:ncol(t1_cornersconceded_a)) {
#         ifelse(!t1_cornersconceded_a[t1_rowacc,t1_colacc]=="",t1_cornersconceded_h[t1_rowacc,t1_colacc] <- t1_cornersconceded_a[t1_rowacc,t1_colacc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }


#
# #write out the data to excel
# write.xlsx(mls_cornersconceded_h,'GCmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_cornersconceded_h,'GCmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_cornersconceded_h,'GCmatrix.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_cornersconceded_h,'GCmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_cornersconceded_h,'GCmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_cornersconceded_h,'GCmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_cornersconceded_h,'GCmatrix.xlsx',sheetName = "T1", append = TRUE)

