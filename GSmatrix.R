#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_goalscored_h <- tapply(MLS$FTHG, MLS[c("HomeTeam", "Date")],mean)
mls_goalscored_a <- tapply(MLS$FTAG, MLS[c("AwayTeam", "Date")],mean)
arg_goalscored_h <- tapply(ARG$FTHG, ARG[c("HomeTeam", "Date")],mean)
arg_goalscored_a <- tapply(ARG$FTAG, ARG[c("AwayTeam", "Date")],mean)
# d2_goalscored_h <- tapply(D2$FTHG, D2[c("HomeTeam", "Date")],mean)
# d2_goalscored_a <- tapply(D2$FTAG, D2[c("AwayTeam", "Date")],mean)
# e0_goalscored_h <- tapply(E0$FTHG, E0[c("HomeTeam", "Date")],mean)
# e0_goalscored_a <- tapply(E0$FTAG, E0[c("AwayTeam", "Date")],mean)
# e1_goalscored_h <- tapply(E1$FTHG, E1[c("HomeTeam", "Date")],mean)
# e1_goalscored_a <- tapply(E1$FTAG, E1[c("AwayTeam", "Date")],mean)
# e2_goalscored_h <- tapply(E2$FTHG, E2[c("HomeTeam", "Date")],mean)
# e2_goalscored_a <- tapply(E2$FTAG, E2[c("AwayTeam", "Date")],mean)
# e3_goalscored_h <- tapply(E3$FTHG, E3[c("HomeTeam", "Date")],mean)
# e3_goalscored_a <- tapply(E3$FTAG, E3[c("AwayTeam", "Date")],mean)
# ec_goalscored_h <- tapply(EC$FTHG, EC[c("HomeTeam", "Date")],mean)
# ec_goalscored_a <- tapply(EC$FTAG, EC[c("AwayTeam", "Date")],mean)
# f1_goalscored_h <- tapply(F1$FTHG, F1[c("HomeTeam", "Date")],mean)
# f1_goalscored_a <- tapply(F1$FTAG, F1[c("AwayTeam", "Date")],mean)
# f2_goalscored_h <- tapply(F2$FTHG, F2[c("HomeTeam", "Date")],mean)
# f2_goalscored_a <- tapply(F2$FTAG, F2[c("AwayTeam", "Date")],mean)
# g1_goalscored_h <- tapply(G1$FTHG, G1[c("HomeTeam", "Date")],mean)
# g1_goalscored_a <- tapply(G1$FTAG, G1[c("AwayTeam", "Date")],mean)
# i1_goalscored_h <- tapply(I1$FTHG, I1[c("HomeTeam", "Date")],mean)
# i1_goalscored_a <- tapply(I1$FTAG, I1[c("AwayTeam", "Date")],mean)
# i2_goalscored_h <- tapply(I2$FTHG, I2[c("HomeTeam", "Date")],mean)
# i2_goalscored_a <- tapply(I2$FTAG, I2[c("AwayTeam", "Date")],mean)
# n1_goalscored_h <- tapply(N1$FTHG, N1[c("HomeTeam", "Date")],mean)
# n1_goalscored_a <- tapply(N1$FTAG, N1[c("AwayTeam", "Date")],mean)
# p1_goalscored_h <- tapply(P1$FTHG, P1[c("HomeTeam", "Date")],mean)
# p1_goalscored_a <- tapply(P1$FTAG, P1[c("AwayTeam", "Date")],mean)
# sc0_goalscored_h <- tapply(SC0$FTHG, SC0[c("HomeTeam", "Date")],mean)
# sc0_goalscored_a <- tapply(SC0$FTAG, SC0[c("AwayTeam", "Date")],mean)
# sc1_goalscored_h <- tapply(SC1$FTHG, SC1[c("HomeTeam", "Date")],mean)
# sc1_goalscored_a <- tapply(SC1$FTAG, SC1[c("AwayTeam", "Date")],mean)
# sc2_goalscored_h <- tapply(SC2$FTHG, SC2[c("HomeTeam", "Date")],mean)
# sc2_goalscored_a <- tapply(SC2$FTAG, SC2[c("AwayTeam", "Date")],mean)
# sc3_goalscored_h <- tapply(SC3$FTHG, SC3[c("HomeTeam", "Date")],mean)
# sc3_goalscored_a <- tapply(SC3$FTAG, SC3[c("AwayTeam", "Date")],mean)
# sp1_goalscored_h <- tapply(SP1$FTHG, SP1[c("HomeTeam", "Date")],mean)
# sp1_goalscored_a <- tapply(SP1$FTAG, SP1[c("AwayTeam", "Date")],mean)
# sp2_goalscored_h <- tapply(SP2$FTHG, SP2[c("HomeTeam", "Date")],mean)
# sp2_goalscored_a <- tapply(SP2$FTAG, SP2[c("AwayTeam", "Date")],mean)
# t1_goalscored_h <- tapply(T1$FTHG, T1[c("HomeTeam", "Date")],mean)
# t1_goalscored_a <- tapply(T1$FTAG, T1[c("AwayTeam", "Date")],mean)
#remove na values

mls_goalscored_h[is.na(mls_goalscored_h)] <- ""
mls_goalscored_a[is.na(mls_goalscored_a)] <- ""
arg_goalscored_h[is.na(arg_goalscored_h)] <- ""
arg_goalscored_a[is.na(arg_goalscored_a)] <- ""
# d2_goalscored_h[is.na(d2_goalscored_h)] <- ""
# d2_goalscored_a[is.na(d2_goalscored_a)] <- ""
# e0_goalscored_h[is.na(e0_goalscored_h)] <- ""
# e0_goalscored_a[is.na(e0_goalscored_a)] <- ""
# e1_goalscored_h[is.na(e1_goalscored_h)] <- ""
# e1_goalscored_a[is.na(e1_goalscored_a)] <- ""
# e2_goalscored_h[is.na(e2_goalscored_h)] <- ""
# e2_goalscored_a[is.na(e2_goalscored_a)] <- ""
# e3_goalscored_h[is.na(e3_goalscored_h)] <- ""
# e3_goalscored_a[is.na(e3_goalscored_a)] <- ""
# ec_goalscored_h[is.na(ec_goalscored_h)] <- ""
# ec_goalscored_a[is.na(ec_goalscored_a)] <- ""
# f1_goalscored_h[is.na(f1_goalscored_h)] <- ""
# f1_goalscored_a[is.na(f1_goalscored_a)] <- ""
# f2_goalscored_h[is.na(f2_goalscored_h)] <- ""
# f2_goalscored_a[is.na(f2_goalscored_a)] <- ""
# g1_goalscored_h[is.na(g1_goalscored_h)] <- ""
# g1_goalscored_a[is.na(g1_goalscored_a)] <- ""
# i1_goalscored_h[is.na(i1_goalscored_h)] <- ""
# i1_goalscored_a[is.na(i1_goalscored_a)] <- ""
# i2_goalscored_h[is.na(i2_goalscored_h)] <- ""
# i2_goalscored_a[is.na(i2_goalscored_a)] <- ""
# n1_goalscored_h[is.na(n1_goalscored_h)] <- ""
# n1_goalscored_a[is.na(n1_goalscored_a)] <- ""
# p1_goalscored_h[is.na(p1_goalscored_h)] <- ""
# p1_goalscored_a[is.na(p1_goalscored_a)] <- ""
# sc0_goalscored_h[is.na(sc0_goalscored_h)] <- ""
# sc0_goalscored_a[is.na(sc0_goalscored_a)] <- ""
# sc1_goalscored_h[is.na(sc1_goalscored_h)] <- ""
# sc1_goalscored_a[is.na(sc1_goalscored_a)] <- ""
# sc2_goalscored_h[is.na(sc2_goalscored_h)] <- ""
# sc2_goalscored_a[is.na(sc2_goalscored_a)] <- ""
# sc3_goalscored_h[is.na(sc3_goalscored_h)] <- ""
# sc3_goalscored_a[is.na(sc3_goalscored_a)] <- ""
# sp1_goalscored_h[is.na(sp1_goalscored_h)] <- ""
# sp1_goalscored_a[is.na(sp1_goalscored_a)] <- ""
# sp2_goalscored_h[is.na(sp2_goalscored_h)] <- ""
# sp2_goalscored_a[is.na(sp2_goalscored_a)] <- ""
# t1_goalscored_h[is.na(t1_goalscored_h)] <- ""
# t1_goalscored_a[is.na(t1_goalscored_a)] <- ""
#combine the matrices
#MLS
for(mls_rowhgs in 1:nrow(mls_goalscored_h)) {
  for(mls_colhgs in 1:ncol(mls_goalscored_h)) {

    # print(my_matrix[row, col])
    for(mls_rowags in 1:nrow(mls_goalscored_a)) {
      for(mls_colags in 1:ncol(mls_goalscored_a)) {
        ifelse(!mls_goalscored_a[mls_rowags,mls_colags]=="",mls_goalscored_h[mls_rowags,mls_colags] <- mls_goalscored_a[mls_rowags,mls_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhgs in 1:nrow(arg_goalscored_h)) {
  for(arg_colhgs in 1:ncol(arg_goalscored_h)) {

    # print(my_matrix[row, col])
    for(arg_rowags in 1:nrow(arg_goalscored_a)) {
      for(arg_colags in 1:ncol(arg_goalscored_a)) {
        ifelse(!arg_goalscored_a[arg_rowags,arg_colags]=="",arg_goalscored_h[arg_rowags,arg_colags] <- arg_goalscored_a[arg_rowags,arg_colags],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhgs in 1:nrow(d2_goalscored_h)) {
#   for(d2_colhgs in 1:ncol(d2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowags in 1:nrow(d2_goalscored_a)) {
#       for(d2_colags in 1:ncol(d2_goalscored_a)) {
#         ifelse(!d2_goalscored_a[d2_rowags,d2_colags]=="",d2_goalscored_h[d2_rowags,d2_colags] <- d2_goalscored_a[d2_rowags,d2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhgs in 1:nrow(e0_goalscored_h)) {
#   for(e0_colhgs in 1:ncol(e0_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowags in 1:nrow(e0_goalscored_a)) {
#       for(e0_colags in 1:ncol(e0_goalscored_a)) {
#         ifelse(!e0_goalscored_a[e0_rowags,e0_colags]=="",e0_goalscored_h[e0_rowags,e0_colags] <- e0_goalscored_a[e0_rowags,e0_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhgs in 1:nrow(e1_goalscored_h)) {
#   for(e1_colhgs in 1:ncol(e1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowags in 1:nrow(e1_goalscored_a)) {
#       for(e1_colags in 1:ncol(e1_goalscored_a)) {
#         ifelse(!e1_goalscored_a[e1_rowags,e1_colags]=="",e1_goalscored_h[e1_rowags,e1_colags] <- e1_goalscored_a[e1_rowags,e1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhgs in 1:nrow(e2_goalscored_h)) {
#   for(e2_colhgs in 1:ncol(e2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowags in 1:nrow(e2_goalscored_a)) {
#       for(e2_colags in 1:ncol(e2_goalscored_a)) {
#         ifelse(!e2_goalscored_a[e2_rowags,e2_colags]=="",e2_goalscored_h[e2_rowags,e2_colags] <- e2_goalscored_a[e2_rowags,e2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhgs in 1:nrow(e3_goalscored_h)) {
#   for(e3_colhgs in 1:ncol(e3_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowags in 1:nrow(e3_goalscored_a)) {
#       for(e3_colags in 1:ncol(e3_goalscored_a)) {
#         ifelse(!e3_goalscored_a[e3_rowags,e3_colags]=="",e3_goalscored_h[e3_rowags,e3_colags] <- e3_goalscored_a[e3_rowags,e3_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhgs in 1:nrow(ec_goalscored_h)) {
#   for(ec_colhgs in 1:ncol(ec_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowags in 1:nrow(ec_goalscored_a)) {
#       for(ec_colags in 1:ncol(ec_goalscored_a)) {
#         ifelse(!ec_goalscored_a[ec_rowags,ec_colags]=="",ec_goalscored_h[ec_rowags,ec_colags] <- ec_goalscored_a[ec_rowags,ec_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhgs in 1:nrow(f1_goalscored_h)) {
#   for(f1_colhgs in 1:ncol(f1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowags in 1:nrow(f1_goalscored_a)) {
#       for(f1_colags in 1:ncol(f1_goalscored_a)) {
#         ifelse(!f1_goalscored_a[f1_rowags,f1_colags]=="",f1_goalscored_h[f1_rowags,f1_colags] <- f1_goalscored_a[f1_rowags,f1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhgs in 1:nrow(f2_goalscored_h)) {
#   for(f2_colhgs in 1:ncol(f2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowags in 1:nrow(f2_goalscored_a)) {
#       for(f2_colags in 1:ncol(f2_goalscored_a)) {
#         ifelse(!f2_goalscored_a[f2_rowags,f2_colags]=="",f2_goalscored_h[f2_rowags,f2_colags] <- f2_goalscored_a[f2_rowags,f2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhgs in 1:nrow(g1_goalscored_h)) {
#   for(g1_colhgs in 1:ncol(g1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowags in 1:nrow(g1_goalscored_a)) {
#       for(g1_colags in 1:ncol(g1_goalscored_a)) {
#         ifelse(!g1_goalscored_a[g1_rowags,g1_colags]=="",g1_goalscored_h[g1_rowags,g1_colags] <- g1_goalscored_a[g1_rowags,g1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhgs in 1:nrow(i1_goalscored_h)) {
#   for(i1_colhgs in 1:ncol(i1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowags in 1:nrow(i1_goalscored_a)) {
#       for(i1_colags in 1:ncol(i1_goalscored_a)) {
#         ifelse(!i1_goalscored_a[i1_rowags,i1_colags]=="",i1_goalscored_h[i1_rowags,i1_colags] <- i1_goalscored_a[i1_rowags,i1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhgs in 1:nrow(i2_goalscored_h)) {
#   for(i2_colhgs in 1:ncol(i2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowags in 1:nrow(i2_goalscored_a)) {
#       for(i2_colags in 1:ncol(i2_goalscored_a)) {
#         ifelse(!i2_goalscored_a[i2_rowags,i2_colags]=="",i2_goalscored_h[i2_rowags,i2_colags] <- i2_goalscored_a[i2_rowags,i2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhgs in 1:nrow(n1_goalscored_h)) {
#   for(n1_colhgs in 1:ncol(n1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowags in 1:nrow(n1_goalscored_a)) {
#       for(n1_colags in 1:ncol(n1_goalscored_a)) {
#         ifelse(!n1_goalscored_a[n1_rowags,n1_colags]=="",n1_goalscored_h[n1_rowags,n1_colags] <- n1_goalscored_a[n1_rowags,n1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhgs in 1:nrow(p1_goalscored_h)) {
#   for(p1_colhgs in 1:ncol(p1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowags in 1:nrow(p1_goalscored_a)) {
#       for(p1_colags in 1:ncol(p1_goalscored_a)) {
#         ifelse(!p1_goalscored_a[p1_rowags,p1_colags]=="",p1_goalscored_h[p1_rowags,p1_colags] <- p1_goalscored_a[p1_rowags,p1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhgs in 1:nrow(sc0_goalscored_h)) {
#   for(sc0_colhgs in 1:ncol(sc0_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowags in 1:nrow(sc0_goalscored_a)) {
#       for(sc0_colags in 1:ncol(sc0_goalscored_a)) {
#         ifelse(!sc0_goalscored_a[sc0_rowags,sc0_colags]=="",sc0_goalscored_h[sc0_rowags,sc0_colags] <- sc0_goalscored_a[sc0_rowags,sc0_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhgs in 1:nrow(sc1_goalscored_h)) {
#   for(sc1_colhgs in 1:ncol(sc1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowags in 1:nrow(sc1_goalscored_a)) {
#       for(sc1_colags in 1:ncol(sc1_goalscored_a)) {
#         ifelse(!sc1_goalscored_a[sc1_rowags,sc1_colags]=="",sc1_goalscored_h[sc1_rowags,sc1_colags] <- sc1_goalscored_a[sc1_rowags,sc1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhgs in 1:nrow(sc2_goalscored_h)) {
#   for(sc2_colhgs in 1:ncol(sc2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowags in 1:nrow(sc2_goalscored_a)) {
#       for(sc2_colags in 1:ncol(sc2_goalscored_a)) {
#         ifelse(!sc2_goalscored_a[sc2_rowags,sc2_colags]=="",sc2_goalscored_h[sc2_rowags,sc2_colags] <- sc2_goalscored_a[sc2_rowags,sc2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhgs in 1:nrow(sc3_goalscored_h)) {
#   for(sc3_colhgs in 1:ncol(sc3_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowags in 1:nrow(sc3_goalscored_a)) {
#       for(sc3_colags in 1:ncol(sc3_goalscored_a)) {
#         ifelse(!sc3_goalscored_a[sc3_rowags,sc3_colags]=="",sc3_goalscored_h[sc3_rowags,sc3_colags] <- sc3_goalscored_a[sc3_rowags,sc3_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhgs in 1:nrow(sp1_goalscored_h)) {
#   for(sp1_colhgs in 1:ncol(sp1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowags in 1:nrow(sp1_goalscored_a)) {
#       for(sp1_colags in 1:ncol(sp1_goalscored_a)) {
#         ifelse(!sp1_goalscored_a[sp1_rowags,sp1_colags]=="",sp1_goalscored_h[sp1_rowags,sp1_colags] <- sp1_goalscored_a[sp1_rowags,sp1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhgs in 1:nrow(sp2_goalscored_h)) {
#   for(sp2_colhgs in 1:ncol(sp2_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowags in 1:nrow(sp2_goalscored_a)) {
#       for(sp2_colags in 1:ncol(sp2_goalscored_a)) {
#         ifelse(!sp2_goalscored_a[sp2_rowags,sp2_colags]=="",sp2_goalscored_h[sp2_rowags,sp2_colags] <- sp2_goalscored_a[sp2_rowags,sp2_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhgs in 1:nrow(t1_goalscored_h)) {
#   for(t1_colhgs in 1:ncol(t1_goalscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowags in 1:nrow(t1_goalscored_a)) {
#       for(t1_colags in 1:ncol(t1_goalscored_a)) {
#         ifelse(!t1_goalscored_a[t1_rowags,t1_colags]=="",t1_goalscored_h[t1_rowags,t1_colags] <- t1_goalscored_a[t1_rowags,t1_colags],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
#

#
# #write out the data to excel
# write.xlsx(mls_goalscored_h,'GSmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_goalscored_h,'GSmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_goalscored_h,'GSmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_goalscored_h,'GSmatrix.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_goalscored_h,'GSmatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_goalscored_h,'GSmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_goalscored_h,'GSmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_goalscored_h,'GSmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_goalscored_h,'GSmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_goalscored_h,'GSmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_goalscored_h,'GSmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_goalscored_h,'GSmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_goalscored_h,'GSmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_goalscored_h,'GSmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_goalscored_h,'GSmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_goalscored_h,'GSmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_goalscored_h,'GSmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_goalscored_h,'GSmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_goalscored_h,'GSmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_goalscored_h,'GSmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_goalscored_h,'GSmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_goalscored_h,'GSmatrix.xlsx',sheetName = "T1", append = TRUE)

