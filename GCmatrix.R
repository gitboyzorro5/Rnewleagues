#create Goals Conceded form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_goalconceded_h <- tapply(MLS$FTAG, MLS[c("HomeTeam", "Date")],mean)
mls_goalconceded_a <- tapply(MLS$FTHG, MLS[c("AwayTeam", "Date")],mean)
arg_goalconceded_h <- tapply(ARG$FTAG, ARG[c("HomeTeam", "Date")],mean)
arg_goalconceded_a <- tapply(ARG$FTHG, ARG[c("AwayTeam", "Date")],mean)
# d2_goalconceded_h <- tapply(D2$FTAG, D2[c("HomeTeam", "Date")],mean)
# d2_goalconceded_a <- tapply(D2$FTHG, D2[c("AwayTeam", "Date")],mean)
# e0_goalconceded_h <- tapply(E0$FTAG, E0[c("HomeTeam", "Date")],mean)
# e0_goalconceded_a <- tapply(E0$FTHG, E0[c("AwayTeam", "Date")],mean)
# e1_goalconceded_h <- tapply(E1$FTAG, E1[c("HomeTeam", "Date")],mean)
# e1_goalconceded_a <- tapply(E1$FTHG, E1[c("AwayTeam", "Date")],mean)
# e2_goalconceded_h <- tapply(E2$FTAG, E2[c("HomeTeam", "Date")],mean)
# e2_goalconceded_a <- tapply(E2$FTHG, E2[c("AwayTeam", "Date")],mean)
# e3_goalconceded_h <- tapply(E3$FTAG, E3[c("HomeTeam", "Date")],mean)
# e3_goalconceded_a <- tapply(E3$FTHG, E3[c("AwayTeam", "Date")],mean)
# ec_goalconceded_h <- tapply(EC$FTAG, EC[c("HomeTeam", "Date")],mean)
# ec_goalconceded_a <- tapply(EC$FTHG, EC[c("AwayTeam", "Date")],mean)
# f1_goalconceded_h <- tapply(F1$FTAG, F1[c("HomeTeam", "Date")],mean)
# f1_goalconceded_a <- tapply(F1$FTHG, F1[c("AwayTeam", "Date")],mean)
# f2_goalconceded_h <- tapply(F2$FTAG, F2[c("HomeTeam", "Date")],mean)
# f2_goalconceded_a <- tapply(F2$FTHG, F2[c("AwayTeam", "Date")],mean)
# g1_goalconceded_h <- tapply(G1$FTAG, G1[c("HomeTeam", "Date")],mean)
# g1_goalconceded_a <- tapply(G1$FTHG, G1[c("AwayTeam", "Date")],mean)
# i1_goalconceded_h <- tapply(I1$FTAG, I1[c("HomeTeam", "Date")],mean)
# i1_goalconceded_a <- tapply(I1$FTHG, I1[c("AwayTeam", "Date")],mean)
# i2_goalconceded_h <- tapply(I2$FTAG, I2[c("HomeTeam", "Date")],mean)
# i2_goalconceded_a <- tapply(I2$FTHG, I2[c("AwayTeam", "Date")],mean)
# n1_goalconceded_h <- tapply(N1$FTAG, N1[c("HomeTeam", "Date")],mean)
# n1_goalconceded_a <- tapply(N1$FTHG, N1[c("AwayTeam", "Date")],mean)
# p1_goalconceded_h <- tapply(P1$FTAG, P1[c("HomeTeam", "Date")],mean)
# p1_goalconceded_a <- tapply(P1$FTHG, P1[c("AwayTeam", "Date")],mean)
# sc0_goalconceded_h <- tapply(SC0$FTAG, SC0[c("HomeTeam", "Date")],mean)
# sc0_goalconceded_a <- tapply(SC0$FTHG, SC0[c("AwayTeam", "Date")],mean)
# sc1_goalconceded_h <- tapply(SC1$FTAG, SC1[c("HomeTeam", "Date")],mean)
# sc1_goalconceded_a <- tapply(SC1$FTHG, SC1[c("AwayTeam", "Date")],mean)
# sc2_goalconceded_h <- tapply(SC2$FTAG, SC2[c("HomeTeam", "Date")],mean)
# sc2_goalconceded_a <- tapply(SC2$FTHG, SC2[c("AwayTeam", "Date")],mean)
# sc3_goalconceded_h <- tapply(SC3$FTAG, SC3[c("HomeTeam", "Date")],mean)
# sc3_goalconceded_a <- tapply(SC3$FTHG, SC3[c("AwayTeam", "Date")],mean)
# sp1_goalconceded_h <- tapply(SP1$FTAG, SP1[c("HomeTeam", "Date")],mean)
# sp1_goalconceded_a <- tapply(SP1$FTHG, SP1[c("AwayTeam", "Date")],mean)
# sp2_goalconceded_h <- tapply(SP2$FTAG, SP2[c("HomeTeam", "Date")],mean)
# sp2_goalconceded_a <- tapply(SP2$FTHG, SP2[c("AwayTeam", "Date")],mean)
# t1_goalconceded_h <- tapply(T1$FTAG, T1[c("HomeTeam", "Date")],mean)
# t1_goalconceded_a <- tapply(T1$FTHG, T1[c("AwayTeam", "Date")],mean)
#remove na values

mls_goalconceded_h[is.na(mls_goalconceded_h)] <- ""
mls_goalconceded_a[is.na(mls_goalconceded_a)] <- ""
arg_goalconceded_h[is.na(arg_goalconceded_h)] <- ""
arg_goalconceded_a[is.na(arg_goalconceded_a)] <- ""
# d2_goalconceded_h[is.na(d2_goalconceded_h)] <- ""
# d2_goalconceded_a[is.na(d2_goalconceded_a)] <- ""
# e0_goalconceded_h[is.na(e0_goalconceded_h)] <- ""
# e0_goalconceded_a[is.na(e0_goalconceded_a)] <- ""
# e1_goalconceded_h[is.na(e1_goalconceded_h)] <- ""
# e1_goalconceded_a[is.na(e1_goalconceded_a)] <- ""
# e2_goalconceded_h[is.na(e2_goalconceded_h)] <- ""
# e2_goalconceded_a[is.na(e2_goalconceded_a)] <- ""
# e3_goalconceded_h[is.na(e3_goalconceded_h)] <- ""
# e3_goalconceded_a[is.na(e3_goalconceded_a)] <- ""
# ec_goalconceded_h[is.na(ec_goalconceded_h)] <- ""
# ec_goalconceded_a[is.na(ec_goalconceded_a)] <- ""
# f1_goalconceded_h[is.na(f1_goalconceded_h)] <- ""
# f1_goalconceded_a[is.na(f1_goalconceded_a)] <- ""
# f2_goalconceded_h[is.na(f2_goalconceded_h)] <- ""
# f2_goalconceded_a[is.na(f2_goalconceded_a)] <- ""
# g1_goalconceded_h[is.na(g1_goalconceded_h)] <- ""
# g1_goalconceded_a[is.na(g1_goalconceded_a)] <- ""
# i1_goalconceded_h[is.na(i1_goalconceded_h)] <- ""
# i1_goalconceded_a[is.na(i1_goalconceded_a)] <- ""
# i2_goalconceded_h[is.na(i2_goalconceded_h)] <- ""
# i2_goalconceded_a[is.na(i2_goalconceded_a)] <- ""
# n1_goalconceded_h[is.na(n1_goalconceded_h)] <- ""
# n1_goalconceded_a[is.na(n1_goalconceded_a)] <- ""
# p1_goalconceded_h[is.na(p1_goalconceded_h)] <- ""
# p1_goalconceded_a[is.na(p1_goalconceded_a)] <- ""
# sc0_goalconceded_h[is.na(sc0_goalconceded_h)] <- ""
# sc0_goalconceded_a[is.na(sc0_goalconceded_a)] <- ""
# sc1_goalconceded_h[is.na(sc1_goalconceded_h)] <- ""
# sc1_goalconceded_a[is.na(sc1_goalconceded_a)] <- ""
# sc2_goalconceded_h[is.na(sc2_goalconceded_h)] <- ""
# sc2_goalconceded_a[is.na(sc2_goalconceded_a)] <- ""
# sc3_goalconceded_h[is.na(sc3_goalconceded_h)] <- ""
# sc3_goalconceded_a[is.na(sc3_goalconceded_a)] <- ""
# sp1_goalconceded_h[is.na(sp1_goalconceded_h)] <- ""
# sp1_goalconceded_a[is.na(sp1_goalconceded_a)] <- ""
# sp2_goalconceded_h[is.na(sp2_goalconceded_h)] <- ""
# sp2_goalconceded_a[is.na(sp2_goalconceded_a)] <- ""
# t1_goalconceded_h[is.na(t1_goalconceded_h)] <- ""
# t1_goalconceded_a[is.na(t1_goalconceded_a)] <- ""
#combine the matrices
#MLS
for(mls_rowhgc in 1:nrow(mls_goalconceded_h)) {
  for(mls_colhgc in 1:ncol(mls_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(mls_rowagc in 1:nrow(mls_goalconceded_a)) {
      for(mls_colagc in 1:ncol(mls_goalconceded_a)) {
        ifelse(!mls_goalconceded_a[mls_rowagc,mls_colagc]=="",mls_goalconceded_h[mls_rowagc,mls_colagc] <- mls_goalconceded_a[mls_rowagc,mls_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhgc in 1:nrow(arg_goalconceded_h)) {
  for(arg_colhgc in 1:ncol(arg_goalconceded_h)) {

    # print(my_matrix[row, col])
    for(arg_rowagc in 1:nrow(arg_goalconceded_a)) {
      for(arg_colagc in 1:ncol(arg_goalconceded_a)) {
        ifelse(!arg_goalconceded_a[arg_rowagc,arg_colagc]=="",arg_goalconceded_h[arg_rowagc,arg_colagc] <- arg_goalconceded_a[arg_rowagc,arg_colagc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhgc in 1:nrow(d2_goalconceded_h)) {
#   for(d2_colhgc in 1:ncol(d2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowagc in 1:nrow(d2_goalconceded_a)) {
#       for(d2_colagc in 1:ncol(d2_goalconceded_a)) {
#         ifelse(!d2_goalconceded_a[d2_rowagc,d2_colagc]=="",d2_goalconceded_h[d2_rowagc,d2_colagc] <- d2_goalconceded_a[d2_rowagc,d2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhgc in 1:nrow(e0_goalconceded_h)) {
#   for(e0_colhgc in 1:ncol(e0_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowagc in 1:nrow(e0_goalconceded_a)) {
#       for(e0_colagc in 1:ncol(e0_goalconceded_a)) {
#         ifelse(!e0_goalconceded_a[e0_rowagc,e0_colagc]=="",e0_goalconceded_h[e0_rowagc,e0_colagc] <- e0_goalconceded_a[e0_rowagc,e0_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhgc in 1:nrow(e1_goalconceded_h)) {
#   for(e1_colhgc in 1:ncol(e1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowagc in 1:nrow(e1_goalconceded_a)) {
#       for(e1_colagc in 1:ncol(e1_goalconceded_a)) {
#         ifelse(!e1_goalconceded_a[e1_rowagc,e1_colagc]=="",e1_goalconceded_h[e1_rowagc,e1_colagc] <- e1_goalconceded_a[e1_rowagc,e1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhgc in 1:nrow(e2_goalconceded_h)) {
#   for(e2_colhgc in 1:ncol(e2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowagc in 1:nrow(e2_goalconceded_a)) {
#       for(e2_colagc in 1:ncol(e2_goalconceded_a)) {
#         ifelse(!e2_goalconceded_a[e2_rowagc,e2_colagc]=="",e2_goalconceded_h[e2_rowagc,e2_colagc] <- e2_goalconceded_a[e2_rowagc,e2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhgc in 1:nrow(e3_goalconceded_h)) {
#   for(e3_colhgc in 1:ncol(e3_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowagc in 1:nrow(e3_goalconceded_a)) {
#       for(e3_colagc in 1:ncol(e3_goalconceded_a)) {
#         ifelse(!e3_goalconceded_a[e3_rowagc,e3_colagc]=="",e3_goalconceded_h[e3_rowagc,e3_colagc] <- e3_goalconceded_a[e3_rowagc,e3_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhgc in 1:nrow(ec_goalconceded_h)) {
#   for(ec_colhgc in 1:ncol(ec_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowagc in 1:nrow(ec_goalconceded_a)) {
#       for(ec_colagc in 1:ncol(ec_goalconceded_a)) {
#         ifelse(!ec_goalconceded_a[ec_rowagc,ec_colagc]=="",ec_goalconceded_h[ec_rowagc,ec_colagc] <- ec_goalconceded_a[ec_rowagc,ec_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhgc in 1:nrow(f1_goalconceded_h)) {
#   for(f1_colhgc in 1:ncol(f1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowagc in 1:nrow(f1_goalconceded_a)) {
#       for(f1_colagc in 1:ncol(f1_goalconceded_a)) {
#         ifelse(!f1_goalconceded_a[f1_rowagc,f1_colagc]=="",f1_goalconceded_h[f1_rowagc,f1_colagc] <- f1_goalconceded_a[f1_rowagc,f1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhgc in 1:nrow(f2_goalconceded_h)) {
#   for(f2_colhgc in 1:ncol(f2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowagc in 1:nrow(f2_goalconceded_a)) {
#       for(f2_colagc in 1:ncol(f2_goalconceded_a)) {
#         ifelse(!f2_goalconceded_a[f2_rowagc,f2_colagc]=="",f2_goalconceded_h[f2_rowagc,f2_colagc] <- f2_goalconceded_a[f2_rowagc,f2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhgc in 1:nrow(g1_goalconceded_h)) {
#   for(g1_colhgc in 1:ncol(g1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowagc in 1:nrow(g1_goalconceded_a)) {
#       for(g1_colagc in 1:ncol(g1_goalconceded_a)) {
#         ifelse(!g1_goalconceded_a[g1_rowagc,g1_colagc]=="",g1_goalconceded_h[g1_rowagc,g1_colagc] <- g1_goalconceded_a[g1_rowagc,g1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhgc in 1:nrow(i1_goalconceded_h)) {
#   for(i1_colhgc in 1:ncol(i1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowagc in 1:nrow(i1_goalconceded_a)) {
#       for(i1_colagc in 1:ncol(i1_goalconceded_a)) {
#         ifelse(!i1_goalconceded_a[i1_rowagc,i1_colagc]=="",i1_goalconceded_h[i1_rowagc,i1_colagc] <- i1_goalconceded_a[i1_rowagc,i1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhgc in 1:nrow(i2_goalconceded_h)) {
#   for(i2_colhgc in 1:ncol(i2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowagc in 1:nrow(i2_goalconceded_a)) {
#       for(i2_colagc in 1:ncol(i2_goalconceded_a)) {
#         ifelse(!i2_goalconceded_a[i2_rowagc,i2_colagc]=="",i2_goalconceded_h[i2_rowagc,i2_colagc] <- i2_goalconceded_a[i2_rowagc,i2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhgc in 1:nrow(n1_goalconceded_h)) {
#   for(n1_colhgc in 1:ncol(n1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowagc in 1:nrow(n1_goalconceded_a)) {
#       for(n1_colagc in 1:ncol(n1_goalconceded_a)) {
#         ifelse(!n1_goalconceded_a[n1_rowagc,n1_colagc]=="",n1_goalconceded_h[n1_rowagc,n1_colagc] <- n1_goalconceded_a[n1_rowagc,n1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhgc in 1:nrow(p1_goalconceded_h)) {
#   for(p1_colhgc in 1:ncol(p1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowagc in 1:nrow(p1_goalconceded_a)) {
#       for(p1_colagc in 1:ncol(p1_goalconceded_a)) {
#         ifelse(!p1_goalconceded_a[p1_rowagc,p1_colagc]=="",p1_goalconceded_h[p1_rowagc,p1_colagc] <- p1_goalconceded_a[p1_rowagc,p1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhgc in 1:nrow(sc0_goalconceded_h)) {
#   for(sc0_colhgc in 1:ncol(sc0_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowagc in 1:nrow(sc0_goalconceded_a)) {
#       for(sc0_colagc in 1:ncol(sc0_goalconceded_a)) {
#         ifelse(!sc0_goalconceded_a[sc0_rowagc,sc0_colagc]=="",sc0_goalconceded_h[sc0_rowagc,sc0_colagc] <- sc0_goalconceded_a[sc0_rowagc,sc0_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhgc in 1:nrow(sc1_goalconceded_h)) {
#   for(sc1_colhgc in 1:ncol(sc1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowagc in 1:nrow(sc1_goalconceded_a)) {
#       for(sc1_colagc in 1:ncol(sc1_goalconceded_a)) {
#         ifelse(!sc1_goalconceded_a[sc1_rowagc,sc1_colagc]=="",sc1_goalconceded_h[sc1_rowagc,sc1_colagc] <- sc1_goalconceded_a[sc1_rowagc,sc1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhgc in 1:nrow(sc2_goalconceded_h)) {
#   for(sc2_colhgc in 1:ncol(sc2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowagc in 1:nrow(sc2_goalconceded_a)) {
#       for(sc2_colagc in 1:ncol(sc2_goalconceded_a)) {
#         ifelse(!sc2_goalconceded_a[sc2_rowagc,sc2_colagc]=="",sc2_goalconceded_h[sc2_rowagc,sc2_colagc] <- sc2_goalconceded_a[sc2_rowagc,sc2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhgc in 1:nrow(sc3_goalconceded_h)) {
#   for(sc3_colhgc in 1:ncol(sc3_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowagc in 1:nrow(sc3_goalconceded_a)) {
#       for(sc3_colagc in 1:ncol(sc3_goalconceded_a)) {
#         ifelse(!sc3_goalconceded_a[sc3_rowagc,sc3_colagc]=="",sc3_goalconceded_h[sc3_rowagc,sc3_colagc] <- sc3_goalconceded_a[sc3_rowagc,sc3_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhgc in 1:nrow(sp1_goalconceded_h)) {
#   for(sp1_colhgc in 1:ncol(sp1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowagc in 1:nrow(sp1_goalconceded_a)) {
#       for(sp1_colagc in 1:ncol(sp1_goalconceded_a)) {
#         ifelse(!sp1_goalconceded_a[sp1_rowagc,sp1_colagc]=="",sp1_goalconceded_h[sp1_rowagc,sp1_colagc] <- sp1_goalconceded_a[sp1_rowagc,sp1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhgc in 1:nrow(sp2_goalconceded_h)) {
#   for(sp2_colhgc in 1:ncol(sp2_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowagc in 1:nrow(sp2_goalconceded_a)) {
#       for(sp2_colagc in 1:ncol(sp2_goalconceded_a)) {
#         ifelse(!sp2_goalconceded_a[sp2_rowagc,sp2_colagc]=="",sp2_goalconceded_h[sp2_rowagc,sp2_colagc] <- sp2_goalconceded_a[sp2_rowagc,sp2_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhgc in 1:nrow(t1_goalconceded_h)) {
#   for(t1_colhgc in 1:ncol(t1_goalconceded_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowagc in 1:nrow(t1_goalconceded_a)) {
#       for(t1_colagc in 1:ncol(t1_goalconceded_a)) {
#         ifelse(!t1_goalconceded_a[t1_rowagc,t1_colagc]=="",t1_goalconceded_h[t1_rowagc,t1_colagc] <- t1_goalconceded_a[t1_rowagc,t1_colagc],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
#

#
# #write out the data to excel
# write.xlsx(mls_goalconceded_h,'GCmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_goalconceded_h,'GCmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_goalconceded_h,'GCmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_goalconceded_h,'GCmatrix.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_goalconceded_h,'GCmatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_goalconceded_h,'GCmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_goalconceded_h,'GCmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_goalconceded_h,'GCmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_goalconceded_h,'GCmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_goalconceded_h,'GCmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_goalconceded_h,'GCmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_goalconceded_h,'GCmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_goalconceded_h,'GCmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_goalconceded_h,'GCmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_goalconceded_h,'GCmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_goalconceded_h,'GCmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_goalconceded_h,'GCmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_goalconceded_h,'GCmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_goalconceded_h,'GCmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_goalconceded_h,'GCmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_goalconceded_h,'GCmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_goalconceded_h,'GCmatrix.xlsx',sheetName = "T1", append = TRUE)

