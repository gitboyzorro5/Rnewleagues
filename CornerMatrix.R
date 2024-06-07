#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_totalcorners_h <- tapply(MLS$TC, MLS[c("HomeTeam", "Date")],mean)
mls_totalcorners_a <- tapply(MLS$TC, MLS[c("AwayTeam", "Date")],mean)
arg_totalcorners_h <- tapply(ARG$TC, ARG[c("HomeTeam", "Date")],mean)
arg_totalcorners_a <- tapply(ARG$TC, ARG[c("AwayTeam", "Date")],mean)
# d2_totalcorners_h <- tapply(D2$TC, D2[c("HomeTeam", "Date")],mean)
# d2_totalcorners_a <- tapply(D2$TC, D2[c("AwayTeam", "Date")],mean)
# e0_totalcorners_h <- tapply(E0$TC, E0[c("HomeTeam", "Date")],mean)
# e0_totalcorners_a <- tapply(E0$TC, E0[c("AwayTeam", "Date")],mean)
# e1_totalcorners_h <- tapply(E1$TC, E1[c("HomeTeam", "Date")],mean)
# e1_totalcorners_a <- tapply(E1$TC, E1[c("AwayTeam", "Date")],mean)
# e2_totalcorners_h <- tapply(E2$TC, E2[c("HomeTeam", "Date")],mean)
# e2_totalcorners_a <- tapply(E2$TC, E2[c("AwayTeam", "Date")],mean)
# e3_totalcorners_h <- tapply(E3$TC, E3[c("HomeTeam", "Date")],mean)
# e3_totalcorners_a <- tapply(E3$TC, E3[c("AwayTeam", "Date")],mean)
# ec_totalcorners_h <- tapply(EC$TC, EC[c("HomeTeam", "Date")],mean)
# ec_totalcorners_a <- tapply(EC$TC, EC[c("AwayTeam", "Date")],mean)
# f1_totalcorners_h <- tapply(F1$TC, F1[c("HomeTeam", "Date")],mean)
# f1_totalcorners_a <- tapply(F1$TC, F1[c("AwayTeam", "Date")],mean)
# f2_totalcorners_h <- tapply(F2$TC, F2[c("HomeTeam", "Date")],mean)
# f2_totalcorners_a <- tapply(F2$TC, F2[c("AwayTeam", "Date")],mean)
# g1_totalcorners_h <- tapply(G1$TC, G1[c("HomeTeam", "Date")],mean)
# g1_totalcorners_a <- tapply(G1$TC, G1[c("AwayTeam", "Date")],mean)
# i1_totalcorners_h <- tapply(I1$TC, I1[c("HomeTeam", "Date")],mean)
# i1_totalcorners_a <- tapply(I1$TC, I1[c("AwayTeam", "Date")],mean)
# i2_totalcorners_h <- tapply(I2$TC, I2[c("HomeTeam", "Date")],mean)
# i2_totalcorners_a <- tapply(I2$TC, I2[c("AwayTeam", "Date")],mean)
# n1_totalcorners_h <- tapply(N1$TC, N1[c("HomeTeam", "Date")],mean)
# n1_totalcorners_a <- tapply(N1$TC, N1[c("AwayTeam", "Date")],mean)
# p1_totalcorners_h <- tapply(P1$TC, P1[c("HomeTeam", "Date")],mean)
# p1_totalcorners_a <- tapply(P1$TC, P1[c("AwayTeam", "Date")],mean)
# sc0_totalcorners_h <- tapply(SC0$TC, SC0[c("HomeTeam", "Date")],mean)
# sc0_totalcorners_a <- tapply(SC0$TC, SC0[c("AwayTeam", "Date")],mean)
# sc1_totalcorners_h <- tapply(SC1$TC, SC1[c("HomeTeam", "Date")],mean)
# sc1_totalcorners_a <- tapply(SC1$TC, SC1[c("AwayTeam", "Date")],mean)
# sc2_totalcorners_h <- tapply(SC2$TC, SC2[c("HomeTeam", "Date")],mean)
# sc2_totalcorners_a <- tapply(SC2$TC, SC2[c("AwayTeam", "Date")],mean)
# sc3_totalcorners_h <- tapply(SC3$TC, SC3[c("HomeTeam", "Date")],mean)
# sc3_totalcorners_a <- tapply(SC3$TC, SC3[c("AwayTeam", "Date")],mean)
# sp1_totalcorners_h <- tapply(SP1$TC, SP1[c("HomeTeam", "Date")],mean)
# sp1_totalcorners_a <- tapply(SP1$TC, SP1[c("AwayTeam", "Date")],mean)
# sp2_totalcorners_h <- tapply(SP2$TC, SP2[c("HomeTeam", "Date")],mean)
# sp2_totalcorners_a <- tapply(SP2$TC, SP2[c("AwayTeam", "Date")],mean)
# t1_totalcorners_h <- tapply(T1$TC, T1[c("HomeTeam", "Date")],mean)
# t1_totalcorners_a <- tapply(T1$TC, T1[c("AwayTeam", "Date")],mean)
#remove na values

mls_totalcorners_h[is.na(mls_totalcorners_h)] <- ""
mls_totalcorners_a[is.na(mls_totalcorners_a)] <- ""
arg_totalcorners_h[is.na(arg_totalcorners_h)] <- ""
arg_totalcorners_a[is.na(arg_totalcorners_a)] <- ""
# d2_totalcorners_h[is.na(d2_totalcorners_h)] <- ""
# d2_totalcorners_a[is.na(d2_totalcorners_a)] <- ""
# e0_totalcorners_h[is.na(e0_totalcorners_h)] <- ""
# e0_totalcorners_a[is.na(e0_totalcorners_a)] <- ""
# e1_totalcorners_h[is.na(e1_totalcorners_h)] <- ""
# e1_totalcorners_a[is.na(e1_totalcorners_a)] <- ""
# e2_totalcorners_h[is.na(e2_totalcorners_h)] <- ""
# e2_totalcorners_a[is.na(e2_totalcorners_a)] <- ""
# e3_totalcorners_h[is.na(e3_totalcorners_h)] <- ""
# e3_totalcorners_a[is.na(e3_totalcorners_a)] <- ""
# ec_totalcorners_h[is.na(ec_totalcorners_h)] <- ""
# ec_totalcorners_a[is.na(ec_totalcorners_a)] <- ""
# f1_totalcorners_h[is.na(f1_totalcorners_h)] <- ""
# f1_totalcorners_a[is.na(f1_totalcorners_a)] <- ""
# f2_totalcorners_h[is.na(f2_totalcorners_h)] <- ""
# f2_totalcorners_a[is.na(f2_totalcorners_a)] <- ""
# g1_totalcorners_h[is.na(g1_totalcorners_h)] <- ""
# g1_totalcorners_a[is.na(g1_totalcorners_a)] <- ""
# i1_totalcorners_h[is.na(i1_totalcorners_h)] <- ""
# i1_totalcorners_a[is.na(i1_totalcorners_a)] <- ""
# i2_totalcorners_h[is.na(i2_totalcorners_h)] <- ""
# i2_totalcorners_a[is.na(i2_totalcorners_a)] <- ""
# n1_totalcorners_h[is.na(n1_totalcorners_h)] <- ""
# n1_totalcorners_a[is.na(n1_totalcorners_a)] <- ""
# p1_totalcorners_h[is.na(p1_totalcorners_h)] <- ""
# p1_totalcorners_a[is.na(p1_totalcorners_a)] <- ""
# sc0_totalcorners_h[is.na(sc0_totalcorners_h)] <- ""
# sc0_totalcorners_a[is.na(sc0_totalcorners_a)] <- ""
# sc1_totalcorners_h[is.na(sc1_totalcorners_h)] <- ""
# sc1_totalcorners_a[is.na(sc1_totalcorners_a)] <- ""
# sc2_totalcorners_h[is.na(sc2_totalcorners_h)] <- ""
# sc2_totalcorners_a[is.na(sc2_totalcorners_a)] <- ""
# sc3_totalcorners_h[is.na(sc3_totalcorners_h)] <- ""
# sc3_totalcorners_a[is.na(sc3_totalcorners_a)] <- ""
# sp1_totalcorners_h[is.na(sp1_totalcorners_h)] <- ""
# sp1_totalcorners_a[is.na(sp1_totalcorners_a)] <- ""
# sp2_totalcorners_h[is.na(sp2_totalcorners_h)] <- ""
# sp2_totalcorners_a[is.na(sp2_totalcorners_a)] <- ""
# t1_totalcorners_h[is.na(t1_totalcorners_h)] <- ""
# t1_totalcorners_a[is.na(t1_totalcorners_a)] <- ""
#combine the matrices
#MLS
for(mls_rowTC in 1:nrow(mls_totalcorners_h)) {
  for(mls_colTC in 1:ncol(mls_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(mls_rowTC in 1:nrow(mls_totalcorners_a)) {
      for(mls_colTC in 1:ncol(mls_totalcorners_a)) {
        ifelse(!mls_totalcorners_a[mls_rowTC,mls_colTC]=="",mls_totalcorners_h[mls_rowTC,mls_colTC] <- mls_totalcorners_a[mls_rowTC,mls_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowTC in 1:nrow(arg_totalcorners_h)) {
  for(arg_colTC in 1:ncol(arg_totalcorners_h)) {

    # print(my_matrix[row, col])
    for(arg_rowTC in 1:nrow(arg_totalcorners_a)) {
      for(arg_colTC in 1:ncol(arg_totalcorners_a)) {
        ifelse(!arg_totalcorners_a[arg_rowTC,arg_colTC]=="",arg_totalcorners_h[arg_rowTC,arg_colTC] <- arg_totalcorners_a[arg_rowTC,arg_colTC],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

# #D2
# for(d2_rowTC in 1:nrow(d2_totalcorners_h)) {
#   for(d2_colTC in 1:ncol(d2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowTC in 1:nrow(d2_totalcorners_a)) {
#       for(d2_colTC in 1:ncol(d2_totalcorners_a)) {
#         ifelse(!d2_totalcorners_a[d2_rowTC,d2_colTC]=="",d2_totalcorners_h[d2_rowTC,d2_colTC] <- d2_totalcorners_a[d2_rowTC,d2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowTC in 1:nrow(e0_totalcorners_h)) {
#   for(e0_colTC in 1:ncol(e0_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowTC in 1:nrow(e0_totalcorners_a)) {
#       for(e0_colTC in 1:ncol(e0_totalcorners_a)) {
#         ifelse(!e0_totalcorners_a[e0_rowTC,e0_colTC]=="",e0_totalcorners_h[e0_rowTC,e0_colTC] <- e0_totalcorners_a[e0_rowTC,e0_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowTC in 1:nrow(e1_totalcorners_h)) {
#   for(e1_colTC in 1:ncol(e1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowTC in 1:nrow(e1_totalcorners_a)) {
#       for(e1_colTC in 1:ncol(e1_totalcorners_a)) {
#         ifelse(!e1_totalcorners_a[e1_rowTC,e1_colTC]=="",e1_totalcorners_h[e1_rowTC,e1_colTC] <- e1_totalcorners_a[e1_rowTC,e1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowTC in 1:nrow(e2_totalcorners_h)) {
#   for(e2_colTC in 1:ncol(e2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowTC in 1:nrow(e2_totalcorners_a)) {
#       for(e2_colTC in 1:ncol(e2_totalcorners_a)) {
#         ifelse(!e2_totalcorners_a[e2_rowTC,e2_colTC]=="",e2_totalcorners_h[e2_rowTC,e2_colTC] <- e2_totalcorners_a[e2_rowTC,e2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowTC in 1:nrow(e3_totalcorners_h)) {
#   for(e3_colTC in 1:ncol(e3_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowTC in 1:nrow(e3_totalcorners_a)) {
#       for(e3_colTC in 1:ncol(e3_totalcorners_a)) {
#         ifelse(!e3_totalcorners_a[e3_rowTC,e3_colTC]=="",e3_totalcorners_h[e3_rowTC,e3_colTC] <- e3_totalcorners_a[e3_rowTC,e3_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowTC in 1:nrow(ec_totalcorners_h)) {
#   for(ec_colTC in 1:ncol(ec_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowTC in 1:nrow(ec_totalcorners_a)) {
#       for(ec_colTC in 1:ncol(ec_totalcorners_a)) {
#         ifelse(!ec_totalcorners_a[ec_rowTC,ec_colTC]=="",ec_totalcorners_h[ec_rowTC,ec_colTC] <- ec_totalcorners_a[ec_rowTC,ec_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowTC in 1:nrow(f1_totalcorners_h)) {
#   for(f1_colTC in 1:ncol(f1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowTC in 1:nrow(f1_totalcorners_a)) {
#       for(f1_colTC in 1:ncol(f1_totalcorners_a)) {
#         ifelse(!f1_totalcorners_a[f1_rowTC,f1_colTC]=="",f1_totalcorners_h[f1_rowTC,f1_colTC] <- f1_totalcorners_a[f1_rowTC,f1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowTC in 1:nrow(f2_totalcorners_h)) {
#   for(f2_colTC in 1:ncol(f2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowTC in 1:nrow(f2_totalcorners_a)) {
#       for(f2_colTC in 1:ncol(f2_totalcorners_a)) {
#         ifelse(!f2_totalcorners_a[f2_rowTC,f2_colTC]=="",f2_totalcorners_h[f2_rowTC,f2_colTC] <- f2_totalcorners_a[f2_rowTC,f2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowTC in 1:nrow(g1_totalcorners_h)) {
#   for(g1_colTC in 1:ncol(g1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowTC in 1:nrow(g1_totalcorners_a)) {
#       for(g1_colTC in 1:ncol(g1_totalcorners_a)) {
#         ifelse(!g1_totalcorners_a[g1_rowTC,g1_colTC]=="",g1_totalcorners_h[g1_rowTC,g1_colTC] <- g1_totalcorners_a[g1_rowTC,g1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowTC in 1:nrow(i1_totalcorners_h)) {
#   for(i1_colTC in 1:ncol(i1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowTC in 1:nrow(i1_totalcorners_a)) {
#       for(i1_colTC in 1:ncol(i1_totalcorners_a)) {
#         ifelse(!i1_totalcorners_a[i1_rowTC,i1_colTC]=="",i1_totalcorners_h[i1_rowTC,i1_colTC] <- i1_totalcorners_a[i1_rowTC,i1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowTC in 1:nrow(i2_totalcorners_h)) {
#   for(i2_colTC in 1:ncol(i2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowTC in 1:nrow(i2_totalcorners_a)) {
#       for(i2_colTC in 1:ncol(i2_totalcorners_a)) {
#         ifelse(!i2_totalcorners_a[i2_rowTC,i2_colTC]=="",i2_totalcorners_h[i2_rowTC,i2_colTC] <- i2_totalcorners_a[i2_rowTC,i2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowTC in 1:nrow(n1_totalcorners_h)) {
#   for(n1_colTC in 1:ncol(n1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowTC in 1:nrow(n1_totalcorners_a)) {
#       for(n1_colTC in 1:ncol(n1_totalcorners_a)) {
#         ifelse(!n1_totalcorners_a[n1_rowTC,n1_colTC]=="",n1_totalcorners_h[n1_rowTC,n1_colTC] <- n1_totalcorners_a[n1_rowTC,n1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowTC in 1:nrow(p1_totalcorners_h)) {
#   for(p1_colTC in 1:ncol(p1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowTC in 1:nrow(p1_totalcorners_a)) {
#       for(p1_colTC in 1:ncol(p1_totalcorners_a)) {
#         ifelse(!p1_totalcorners_a[p1_rowTC,p1_colTC]=="",p1_totalcorners_h[p1_rowTC,p1_colTC] <- p1_totalcorners_a[p1_rowTC,p1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowTC in 1:nrow(sc0_totalcorners_h)) {
#   for(sc0_colTC in 1:ncol(sc0_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowTC in 1:nrow(sc0_totalcorners_a)) {
#       for(sc0_colTC in 1:ncol(sc0_totalcorners_a)) {
#         ifelse(!sc0_totalcorners_a[sc0_rowTC,sc0_colTC]=="",sc0_totalcorners_h[sc0_rowTC,sc0_colTC] <- sc0_totalcorners_a[sc0_rowTC,sc0_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowTC in 1:nrow(sc1_totalcorners_h)) {
#   for(sc1_colTC in 1:ncol(sc1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowTC in 1:nrow(sc1_totalcorners_a)) {
#       for(sc1_colTC in 1:ncol(sc1_totalcorners_a)) {
#         ifelse(!sc1_totalcorners_a[sc1_rowTC,sc1_colTC]=="",sc1_totalcorners_h[sc1_rowTC,sc1_colTC] <- sc1_totalcorners_a[sc1_rowTC,sc1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowTC in 1:nrow(sc2_totalcorners_h)) {
#   for(sc2_colTC in 1:ncol(sc2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowTC in 1:nrow(sc2_totalcorners_a)) {
#       for(sc2_colTC in 1:ncol(sc2_totalcorners_a)) {
#         ifelse(!sc2_totalcorners_a[sc2_rowTC,sc2_colTC]=="",sc2_totalcorners_h[sc2_rowTC,sc2_colTC] <- sc2_totalcorners_a[sc2_rowTC,sc2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowTC in 1:nrow(sc3_totalcorners_h)) {
#   for(sc3_colTC in 1:ncol(sc3_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowTC in 1:nrow(sc3_totalcorners_a)) {
#       for(sc3_colTC in 1:ncol(sc3_totalcorners_a)) {
#         ifelse(!sc3_totalcorners_a[sc3_rowTC,sc3_colTC]=="",sc3_totalcorners_h[sc3_rowTC,sc3_colTC] <- sc3_totalcorners_a[sc3_rowTC,sc3_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowTC in 1:nrow(sp1_totalcorners_h)) {
#   for(sp1_colTC in 1:ncol(sp1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowTC in 1:nrow(sp1_totalcorners_a)) {
#       for(sp1_colTC in 1:ncol(sp1_totalcorners_a)) {
#         ifelse(!sp1_totalcorners_a[sp1_rowTC,sp1_colTC]=="",sp1_totalcorners_h[sp1_rowTC,sp1_colTC] <- sp1_totalcorners_a[sp1_rowTC,sp1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowTC in 1:nrow(sp2_totalcorners_h)) {
#   for(sp2_colTC in 1:ncol(sp2_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowTC in 1:nrow(sp2_totalcorners_a)) {
#       for(sp2_colTC in 1:ncol(sp2_totalcorners_a)) {
#         ifelse(!sp2_totalcorners_a[sp2_rowTC,sp2_colTC]=="",sp2_totalcorners_h[sp2_rowTC,sp2_colTC] <- sp2_totalcorners_a[sp2_rowTC,sp2_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowTC in 1:nrow(t1_totalcorners_h)) {
#   for(t1_colTC in 1:ncol(t1_totalcorners_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowTC in 1:nrow(t1_totalcorners_a)) {
#       for(t1_colTC in 1:ncol(t1_totalcorners_a)) {
#         ifelse(!t1_totalcorners_a[t1_rowTC,t1_colTC]=="",t1_totalcorners_h[t1_rowTC,t1_colTC] <- t1_totalcorners_a[t1_rowTC,t1_colTC],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }


#
# #write out the data to excel
# write.xlsx(mls_totalcorners_h,'GSmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_totalcorners_h,'GSmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_totalcorners_h,'GSmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_totalcorners_h,'GSmatrix.xlsx',sheetName = "E0", append = TRUE)
 #write.xlsx(e1_totalcorners_h,'cornermatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_totalcorners_h,'GSmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_totalcorners_h,'GSmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_totalcorners_h,'GSmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_totalcorners_h,'GSmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_totalcorners_h,'GSmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_totalcorners_h,'GSmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_totalcorners_h,'GSmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_totalcorners_h,'GSmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_totalcorners_h,'GSmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_totalcorners_h,'GSmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_totalcorners_h,'GSmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_totalcorners_h,'GSmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_totalcorners_h,'GSmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_totalcorners_h,'GSmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_totalcorners_h,'GSmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_totalcorners_h,'GSmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_totalcorners_h,'GSmatrix.xlsx',sheetName = "T1", append = TRUE)

