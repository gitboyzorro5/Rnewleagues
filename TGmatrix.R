#create Total Goals form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_totalgoals_h <- tapply(MLS$TG, MLS[c("HomeTeam", "Date")],mean)
mls_totalgoals_a <- tapply(MLS$TG, MLS[c("AwayTeam", "Date")],mean)
arg_totalgoals_h <- tapply(ARG$TG, ARG[c("HomeTeam", "Date")],mean)
arg_totalgoals_a <- tapply(ARG$TG, ARG[c("AwayTeam", "Date")],mean)
# d2_totalgoals_h <- tapply(D2$TG, D2[c("HomeTeam", "Date")],mean)
# d2_totalgoals_a <- tapply(D2$TG, D2[c("AwayTeam", "Date")],mean)
# e0_totalgoals_h <- tapply(E0$TG, E0[c("HomeTeam", "Date")],mean)
# e0_totalgoals_a <- tapply(E0$TG, E0[c("AwayTeam", "Date")],mean)
# e1_totalgoals_h <- tapply(E1$TG, E1[c("HomeTeam", "Date")],mean)
# e1_totalgoals_a <- tapply(E1$TG, E1[c("AwayTeam", "Date")],mean)
# e2_totalgoals_h <- tapply(E2$TG, E2[c("HomeTeam", "Date")],mean)
# e2_totalgoals_a <- tapply(E2$TG, E2[c("AwayTeam", "Date")],mean)
# e3_totalgoals_h <- tapply(E3$TG, E3[c("HomeTeam", "Date")],mean)
# e3_totalgoals_a <- tapply(E3$TG, E3[c("AwayTeam", "Date")],mean)
# ec_totalgoals_h <- tapply(EC$TG, EC[c("HomeTeam", "Date")],mean)
# ec_totalgoals_a <- tapply(EC$TG, EC[c("AwayTeam", "Date")],mean)
# f1_totalgoals_h <- tapply(F1$TG, F1[c("HomeTeam", "Date")],mean)
# f1_totalgoals_a <- tapply(F1$TG, F1[c("AwayTeam", "Date")],mean)
# f2_totalgoals_h <- tapply(F2$TG, F2[c("HomeTeam", "Date")],mean)
# f2_totalgoals_a <- tapply(F2$TG, F2[c("AwayTeam", "Date")],mean)
# g1_totalgoals_h <- tapply(G1$TG, G1[c("HomeTeam", "Date")],mean)
# g1_totalgoals_a <- tapply(G1$TG, G1[c("AwayTeam", "Date")],mean)
# i1_totalgoals_h <- tapply(I1$TG, I1[c("HomeTeam", "Date")],mean)
# i1_totalgoals_a <- tapply(I1$TG, I1[c("AwayTeam", "Date")],mean)
# i2_totalgoals_h <- tapply(I2$TG, I2[c("HomeTeam", "Date")],mean)
# i2_totalgoals_a <- tapply(I2$TG, I2[c("AwayTeam", "Date")],mean)
# n1_totalgoals_h <- tapply(N1$TG, N1[c("HomeTeam", "Date")],mean)
# n1_totalgoals_a <- tapply(N1$TG, N1[c("AwayTeam", "Date")],mean)
# p1_totalgoals_h <- tapply(P1$TG, P1[c("HomeTeam", "Date")],mean)
# p1_totalgoals_a <- tapply(P1$TG, P1[c("AwayTeam", "Date")],mean)
# sc0_totalgoals_h <- tapply(SC0$TG, SC0[c("HomeTeam", "Date")],mean)
# sc0_totalgoals_a <- tapply(SC0$TG, SC0[c("AwayTeam", "Date")],mean)
# sc1_totalgoals_h <- tapply(SC1$TG, SC1[c("HomeTeam", "Date")],mean)
# sc1_totalgoals_a <- tapply(SC1$TG, SC1[c("AwayTeam", "Date")],mean)
# sc2_totalgoals_h <- tapply(SC2$TG, SC2[c("HomeTeam", "Date")],mean)
# sc2_totalgoals_a <- tapply(SC2$TG, SC2[c("AwayTeam", "Date")],mean)
# sc3_totalgoals_h <- tapply(SC3$TG, SC3[c("HomeTeam", "Date")],mean)
# sc3_totalgoals_a <- tapply(SC3$TG, SC3[c("AwayTeam", "Date")],mean)
# sp1_totalgoals_h <- tapply(SP1$TG, SP1[c("HomeTeam", "Date")],mean)
# sp1_totalgoals_a <- tapply(SP1$TG, SP1[c("AwayTeam", "Date")],mean)
# sp2_totalgoals_h <- tapply(SP2$TG, SP2[c("HomeTeam", "Date")],mean)
# sp2_totalgoals_a <- tapply(SP2$TG, SP2[c("AwayTeam", "Date")],mean)
# t1_totalgoals_h <- tapply(T1$TG, T1[c("HomeTeam", "Date")],mean)
# t1_totalgoals_a <- tapply(T1$TG, T1[c("AwayTeam", "Date")],mean)
# #remove na values

mls_totalgoals_h[is.na(mls_totalgoals_h)] <- ""
mls_totalgoals_a[is.na(mls_totalgoals_a)] <- ""
arg_totalgoals_h[is.na(arg_totalgoals_h)] <- ""
arg_totalgoals_a[is.na(arg_totalgoals_a)] <- ""
# d2_totalgoals_h[is.na(d2_totalgoals_h)] <- ""
# d2_totalgoals_a[is.na(d2_totalgoals_a)] <- ""
# e0_totalgoals_h[is.na(e0_totalgoals_h)] <- ""
# e0_totalgoals_a[is.na(e0_totalgoals_a)] <- ""
# e1_totalgoals_h[is.na(e1_totalgoals_h)] <- ""
# e1_totalgoals_a[is.na(e1_totalgoals_a)] <- ""
# e2_totalgoals_h[is.na(e2_totalgoals_h)] <- ""
# e2_totalgoals_a[is.na(e2_totalgoals_a)] <- ""
# e3_totalgoals_h[is.na(e3_totalgoals_h)] <- ""
# e3_totalgoals_a[is.na(e3_totalgoals_a)] <- ""
# ec_totalgoals_h[is.na(ec_totalgoals_h)] <- ""
# ec_totalgoals_a[is.na(ec_totalgoals_a)] <- ""
# f1_totalgoals_h[is.na(f1_totalgoals_h)] <- ""
# f1_totalgoals_a[is.na(f1_totalgoals_a)] <- ""
# f2_totalgoals_h[is.na(f2_totalgoals_h)] <- ""
# f2_totalgoals_a[is.na(f2_totalgoals_a)] <- ""
# g1_totalgoals_h[is.na(g1_totalgoals_h)] <- ""
# g1_totalgoals_a[is.na(g1_totalgoals_a)] <- ""
# i1_totalgoals_h[is.na(i1_totalgoals_h)] <- ""
# i1_totalgoals_a[is.na(i1_totalgoals_a)] <- ""
# i2_totalgoals_h[is.na(i2_totalgoals_h)] <- ""
# i2_totalgoals_a[is.na(i2_totalgoals_a)] <- ""
# n1_totalgoals_h[is.na(n1_totalgoals_h)] <- ""
# n1_totalgoals_a[is.na(n1_totalgoals_a)] <- ""
# p1_totalgoals_h[is.na(p1_totalgoals_h)] <- ""
# p1_totalgoals_a[is.na(p1_totalgoals_a)] <- ""
# sc0_totalgoals_h[is.na(sc0_totalgoals_h)] <- ""
# sc0_totalgoals_a[is.na(sc0_totalgoals_a)] <- ""
# sc1_totalgoals_h[is.na(sc1_totalgoals_h)] <- ""
# sc1_totalgoals_a[is.na(sc1_totalgoals_a)] <- ""
# sc2_totalgoals_h[is.na(sc2_totalgoals_h)] <- ""
# sc2_totalgoals_a[is.na(sc2_totalgoals_a)] <- ""
# sc3_totalgoals_h[is.na(sc3_totalgoals_h)] <- ""
# sc3_totalgoals_a[is.na(sc3_totalgoals_a)] <- ""
# sp1_totalgoals_h[is.na(sp1_totalgoals_h)] <- ""
# sp1_totalgoals_a[is.na(sp1_totalgoals_a)] <- ""
# sp2_totalgoals_h[is.na(sp2_totalgoals_h)] <- ""
# sp2_totalgoals_a[is.na(sp2_totalgoals_a)] <- ""
# t1_totalgoals_h[is.na(t1_totalgoals_h)] <- ""
# t1_totalgoals_a[is.na(t1_totalgoals_a)] <- ""
# #combine the matrices
#MLS
for(mls_rowh in 1:nrow(mls_totalgoals_h)) {
  for(mls_colh in 1:ncol(mls_totalgoals_h)) {

   # print(my_matrix[row, col])
    for(mls_rowa in 1:nrow(mls_totalgoals_a)) {
      for(mls_cola in 1:ncol(mls_totalgoals_a)) {
        ifelse(!mls_totalgoals_a[mls_rowa,mls_cola]=="",mls_totalgoals_h[mls_rowa,mls_cola] <- mls_totalgoals_a[mls_rowa,mls_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowh in 1:nrow(arg_totalgoals_h)) {
  for(arg_colh in 1:ncol(arg_totalgoals_h)) {

    # print(my_matrix[row, col])
    for(arg_rowa in 1:nrow(arg_totalgoals_a)) {
      for(arg_cola in 1:ncol(arg_totalgoals_a)) {
        ifelse(!arg_totalgoals_a[arg_rowa,arg_cola]=="",arg_totalgoals_h[arg_rowa,arg_cola] <- arg_totalgoals_a[arg_rowa,arg_cola],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowh in 1:nrow(d2_totalgoals_h)) {
#   for(d2_colh in 1:ncol(d2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowa in 1:nrow(d2_totalgoals_a)) {
#       for(d2_cola in 1:ncol(d2_totalgoals_a)) {
#         ifelse(!d2_totalgoals_a[d2_rowa,d2_cola]=="",d2_totalgoals_h[d2_rowa,d2_cola] <- d2_totalgoals_a[d2_rowa,d2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowh in 1:nrow(e0_totalgoals_h)) {
#   for(e0_colh in 1:ncol(e0_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowa in 1:nrow(e0_totalgoals_a)) {
#       for(e0_cola in 1:ncol(e0_totalgoals_a)) {
#         ifelse(!e0_totalgoals_a[e0_rowa,e0_cola]=="",e0_totalgoals_h[e0_rowa,e0_cola] <- e0_totalgoals_a[e0_rowa,e0_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowh in 1:nrow(e1_totalgoals_h)) {
#   for(e1_colh in 1:ncol(e1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowa in 1:nrow(e1_totalgoals_a)) {
#       for(e1_cola in 1:ncol(e1_totalgoals_a)) {
#         ifelse(!e1_totalgoals_a[e1_rowa,e1_cola]=="",e1_totalgoals_h[e1_rowa,e1_cola] <- e1_totalgoals_a[e1_rowa,e1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowh in 1:nrow(e2_totalgoals_h)) {
#   for(e2_colh in 1:ncol(e2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowa in 1:nrow(e2_totalgoals_a)) {
#       for(e2_cola in 1:ncol(e2_totalgoals_a)) {
#         ifelse(!e2_totalgoals_a[e2_rowa,e2_cola]=="",e2_totalgoals_h[e2_rowa,e2_cola] <- e2_totalgoals_a[e2_rowa,e2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowh in 1:nrow(e3_totalgoals_h)) {
#   for(e3_colh in 1:ncol(e3_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowa in 1:nrow(e3_totalgoals_a)) {
#       for(e3_cola in 1:ncol(e3_totalgoals_a)) {
#         ifelse(!e3_totalgoals_a[e3_rowa,e3_cola]=="",e3_totalgoals_h[e3_rowa,e3_cola] <- e3_totalgoals_a[e3_rowa,e3_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowh in 1:nrow(ec_totalgoals_h)) {
#   for(ec_colh in 1:ncol(ec_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowa in 1:nrow(ec_totalgoals_a)) {
#       for(ec_cola in 1:ncol(ec_totalgoals_a)) {
#         ifelse(!ec_totalgoals_a[ec_rowa,ec_cola]=="",ec_totalgoals_h[ec_rowa,ec_cola] <- ec_totalgoals_a[ec_rowa,ec_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowh in 1:nrow(f1_totalgoals_h)) {
#   for(f1_colh in 1:ncol(f1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowa in 1:nrow(f1_totalgoals_a)) {
#       for(f1_cola in 1:ncol(f1_totalgoals_a)) {
#         ifelse(!f1_totalgoals_a[f1_rowa,f1_cola]=="",f1_totalgoals_h[f1_rowa,f1_cola] <- f1_totalgoals_a[f1_rowa,f1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowh in 1:nrow(f2_totalgoals_h)) {
#   for(f2_colh in 1:ncol(f2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowa in 1:nrow(f2_totalgoals_a)) {
#       for(f2_cola in 1:ncol(f2_totalgoals_a)) {
#         ifelse(!f2_totalgoals_a[f2_rowa,f2_cola]=="",f2_totalgoals_h[f2_rowa,f2_cola] <- f2_totalgoals_a[f2_rowa,f2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowh in 1:nrow(g1_totalgoals_h)) {
#   for(g1_colh in 1:ncol(g1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowa in 1:nrow(g1_totalgoals_a)) {
#       for(g1_cola in 1:ncol(g1_totalgoals_a)) {
#         ifelse(!g1_totalgoals_a[g1_rowa,g1_cola]=="",g1_totalgoals_h[g1_rowa,g1_cola] <- g1_totalgoals_a[g1_rowa,g1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowh in 1:nrow(i1_totalgoals_h)) {
#   for(i1_colh in 1:ncol(i1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowa in 1:nrow(i1_totalgoals_a)) {
#       for(i1_cola in 1:ncol(i1_totalgoals_a)) {
#         ifelse(!i1_totalgoals_a[i1_rowa,i1_cola]=="",i1_totalgoals_h[i1_rowa,i1_cola] <- i1_totalgoals_a[i1_rowa,i1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowh in 1:nrow(i2_totalgoals_h)) {
#   for(i2_colh in 1:ncol(i2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowa in 1:nrow(i2_totalgoals_a)) {
#       for(i2_cola in 1:ncol(i2_totalgoals_a)) {
#         ifelse(!i2_totalgoals_a[i2_rowa,i2_cola]=="",i2_totalgoals_h[i2_rowa,i2_cola] <- i2_totalgoals_a[i2_rowa,i2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowh in 1:nrow(n1_totalgoals_h)) {
#   for(n1_colh in 1:ncol(n1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowa in 1:nrow(n1_totalgoals_a)) {
#       for(n1_cola in 1:ncol(n1_totalgoals_a)) {
#         ifelse(!n1_totalgoals_a[n1_rowa,n1_cola]=="",n1_totalgoals_h[n1_rowa,n1_cola] <- n1_totalgoals_a[n1_rowa,n1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowh in 1:nrow(p1_totalgoals_h)) {
#   for(p1_colh in 1:ncol(p1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowa in 1:nrow(p1_totalgoals_a)) {
#       for(p1_cola in 1:ncol(p1_totalgoals_a)) {
#         ifelse(!p1_totalgoals_a[p1_rowa,p1_cola]=="",p1_totalgoals_h[p1_rowa,p1_cola] <- p1_totalgoals_a[p1_rowa,p1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowh in 1:nrow(sc0_totalgoals_h)) {
#   for(sc0_colh in 1:ncol(sc0_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowa in 1:nrow(sc0_totalgoals_a)) {
#       for(sc0_cola in 1:ncol(sc0_totalgoals_a)) {
#         ifelse(!sc0_totalgoals_a[sc0_rowa,sc0_cola]=="",sc0_totalgoals_h[sc0_rowa,sc0_cola] <- sc0_totalgoals_a[sc0_rowa,sc0_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowh in 1:nrow(sc1_totalgoals_h)) {
#   for(sc1_colh in 1:ncol(sc1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowa in 1:nrow(sc1_totalgoals_a)) {
#       for(sc1_cola in 1:ncol(sc1_totalgoals_a)) {
#         ifelse(!sc1_totalgoals_a[sc1_rowa,sc1_cola]=="",sc1_totalgoals_h[sc1_rowa,sc1_cola] <- sc1_totalgoals_a[sc1_rowa,sc1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowh in 1:nrow(sc2_totalgoals_h)) {
#   for(sc2_colh in 1:ncol(sc2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowa in 1:nrow(sc2_totalgoals_a)) {
#       for(sc2_cola in 1:ncol(sc2_totalgoals_a)) {
#         ifelse(!sc2_totalgoals_a[sc2_rowa,sc2_cola]=="",sc2_totalgoals_h[sc2_rowa,sc2_cola] <- sc2_totalgoals_a[sc2_rowa,sc2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowh in 1:nrow(sc3_totalgoals_h)) {
#   for(sc3_colh in 1:ncol(sc3_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowa in 1:nrow(sc3_totalgoals_a)) {
#       for(sc3_cola in 1:ncol(sc3_totalgoals_a)) {
#         ifelse(!sc3_totalgoals_a[sc3_rowa,sc3_cola]=="",sc3_totalgoals_h[sc3_rowa,sc3_cola] <- sc3_totalgoals_a[sc3_rowa,sc3_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowh in 1:nrow(sp1_totalgoals_h)) {
#   for(sp1_colh in 1:ncol(sp1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowa in 1:nrow(sp1_totalgoals_a)) {
#       for(sp1_cola in 1:ncol(sp1_totalgoals_a)) {
#         ifelse(!sp1_totalgoals_a[sp1_rowa,sp1_cola]=="",sp1_totalgoals_h[sp1_rowa,sp1_cola] <- sp1_totalgoals_a[sp1_rowa,sp1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowh in 1:nrow(sp2_totalgoals_h)) {
#   for(sp2_colh in 1:ncol(sp2_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowa in 1:nrow(sp2_totalgoals_a)) {
#       for(sp2_cola in 1:ncol(sp2_totalgoals_a)) {
#         ifelse(!sp2_totalgoals_a[sp2_rowa,sp2_cola]=="",sp2_totalgoals_h[sp2_rowa,sp2_cola] <- sp2_totalgoals_a[sp2_rowa,sp2_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowh in 1:nrow(t1_totalgoals_h)) {
#   for(t1_colh in 1:ncol(t1_totalgoals_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowa in 1:nrow(t1_totalgoals_a)) {
#       for(t1_cola in 1:ncol(t1_totalgoals_a)) {
#         ifelse(!t1_totalgoals_a[t1_rowa,t1_cola]=="",t1_totalgoals_h[t1_rowa,t1_cola] <- t1_totalgoals_a[t1_rowa,t1_cola],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
#
#
# #write out the data to excel
# write.xlsx(mls_totalgoals_h,'TGform.xlsx',sheetName = "MLS")
# write.xlsx(arg_totalgoals_h,'TGform.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_totalgoals_h,'TGform.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_totalgoals_h,'TGform.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_totalgoals_h,'TGform.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_totalgoals_h,'TGform.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_totalgoals_h,'TGform.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_totalgoals_h,'TGform.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_totalgoals_h,'TGform.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_totalgoals_h,'TGform.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_totalgoals_h,'TGform.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_totalgoals_h,'TGform.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_totalgoals_h,'TGform.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_totalgoals_h,'TGform.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_totalgoals_h,'TGform.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_totalgoals_h,'TGform.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_totalgoals_h,'TGform.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_totalgoals_h,'TGform.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_totalgoals_h,'TGform.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_totalgoals_h,'TGform.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_totalgoals_h,'TGform.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_totalgoals_h,'TGform.xlsx',sheetName = "T1", append = TRUE)



