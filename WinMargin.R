#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_winmargin_h <- tapply(MLS$FTHG - MLS$FTAG, MLS[c("HomeTeam", "Date")],mean)
mls_winmargin_a <- tapply(MLS$FTAG - MLS$FTHG, MLS[c("AwayTeam", "Date")],mean)

arg_winmargin_h <- tapply(ARG$FTHG - ARG$FTAG, ARG[c("HomeTeam", "Date")],mean)
arg_winmargin_a <- tapply(ARG$FTAG - ARG$FTHG, ARG[c("AwayTeam", "Date")],mean)

# d2_winmargin_h <- tapply(D2$FTHG - D2$FTAG, D2[c("HomeTeam", "Date")],mean)
# d2_winmargin_a <- tapply(D2$FTAG - D2$FTHG, D2[c("AwayTeam", "Date")],mean)
#
# e0_winmargin_h <- tapply(E0$FTHG - E0$FTAG, E0[c("HomeTeam", "Date")],mean)
# e0_winmargin_a <- tapply(E0$FTAG - E0$FTHG, E0[c("AwayTeam", "Date")],mean)
#
# e1_winmargin_h <- tapply(E1$FTHG - E1$FTAG, E1[c("HomeTeam", "Date")],mean)
# e1_winmargin_a <- tapply(E1$FTAG - E1$FTHG, E1[c("AwayTeam", "Date")],mean)
#
# e2_winmargin_h <- tapply(E2$FTHG - E2$FTAG, E2[c("HomeTeam", "Date")],mean)
# e2_winmargin_a <- tapply(E2$FTAG - E2$FTHG, E2[c("AwayTeam", "Date")],mean)
#
# e3_winmargin_h <- tapply(E3$FTHG - E3$FTAG, E3[c("HomeTeam", "Date")],mean)
# e3_winmargin_a <- tapply(E3$FTAG - E3$FTHG, E3[c("AwayTeam", "Date")],mean)
#
# ec_winmargin_h <- tapply(EC$FTHG - EC$FTAG, EC[c("HomeTeam", "Date")],mean)
# ec_winmargin_a <- tapply(EC$FTAG - EC$FTHG, EC[c("AwayTeam", "Date")],mean)
#
# f1_winmargin_h <- tapply(F1$FTHG - F1$FTAG, F1[c("HomeTeam", "Date")],mean)
# f1_winmargin_a <- tapply(F1$FTAG - F1$FTHG, F1[c("AwayTeam", "Date")],mean)
#
# f2_winmargin_h <- tapply(F2$FTHG - F2$FTAG, F2[c("HomeTeam", "Date")],mean)
# f2_winmargin_a <- tapply(F2$FTAG - F2$FTHG, F2[c("AwayTeam", "Date")],mean)
#
# g1_winmargin_h <- tapply(G1$FTHG - G1$FTAG, G1[c("HomeTeam", "Date")],mean)
# g1_winmargin_a <- tapply(G1$FTAG - G1$FTHG, G1[c("AwayTeam", "Date")],mean)
#
# i1_winmargin_h <- tapply(I1$FTHG - I1$FTAG, I1[c("HomeTeam", "Date")],mean)
# i1_winmargin_a <- tapply(I1$FTAG - I1$FTHG, I1[c("AwayTeam", "Date")],mean)
#
# i2_winmargin_h <- tapply(I2$FTHG - I2$FTAG, I2[c("HomeTeam", "Date")],mean)
# i2_winmargin_a <- tapply(I2$FTAG - I2$FTHG, I2[c("AwayTeam", "Date")],mean)
#
# n1_winmargin_h <- tapply(N1$FTHG - N1$FTAG, N1[c("HomeTeam", "Date")],mean)
# n1_winmargin_a <- tapply(N1$FTAG - N1$FTHG, N1[c("AwayTeam", "Date")],mean)
#
# p1_winmargin_h <- tapply(P1$FTHG - P1$FTAG, P1[c("HomeTeam", "Date")],mean)
# p1_winmargin_a <- tapply(P1$FTAG - P1$FTHG, P1[c("AwayTeam", "Date")],mean)
#
# sp1_winmargin_h <- tapply(SP1$FTHG - SP1$FTAG, SP1[c("HomeTeam", "Date")],mean)
# sp1_winmargin_a <- tapply(SP1$FTAG - SP1$FTHG, SP1[c("AwayTeam", "Date")],mean)
#
# sp2_winmargin_h <- tapply(SP2$FTHG - SP2$FTAG, SP2[c("HomeTeam", "Date")],mean)
# sp2_winmargin_a <- tapply(SP2$FTAG - SP2$FTHG, SP2[c("AwayTeam", "Date")],mean)
#
# sc0_winmargin_h <- tapply(SC0$FTHG - SC0$FTAG, SC0[c("HomeTeam", "Date")],mean)
# sc0_winmargin_a <- tapply(SC0$FTAG - SC0$FTHG, SC0[c("AwayTeam", "Date")],mean)
#
# sc1_winmargin_h <- tapply(SC1$FTHG - SC1$FTAG, SC1[c("HomeTeam", "Date")],mean)
# sc1_winmargin_a <- tapply(SC1$FTAG - SC1$FTHG, SC1[c("AwayTeam", "Date")],mean)
#
# sc2_winmargin_h <- tapply(SC2$FTHG - SC2$FTAG, SC2[c("HomeTeam", "Date")],mean)
# sc2_winmargin_a <- tapply(SC2$FTAG - SC2$FTHG, SC2[c("AwayTeam", "Date")],mean)
#
# sc3_winmargin_h <- tapply(SC3$FTHG - SC3$FTAG, SC3[c("HomeTeam", "Date")],mean)
# sc3_winmargin_a <- tapply(SC3$FTAG - SC3$FTHG, SC3[c("AwayTeam", "Date")],mean)
#
# t1_winmargin_h <- tapply(T1$FTHG - T1$FTAG, T1[c("HomeTeam", "Date")],mean)
# t1_winmargin_a <- tapply(T1$FTAG - T1$FTHG, T1[c("AwayTeam", "Date")],mean)

mls_winmargin_h[is.na(mls_winmargin_h)] <- ""
mls_winmargin_a[is.na(mls_winmargin_a)] <- ""
arg_winmargin_h[is.na(arg_winmargin_h)] <- ""
arg_winmargin_a[is.na(arg_winmargin_a)] <- ""
# d2_winmargin_h[is.na(d2_winmargin_h)] <- ""
# d2_winmargin_a[is.na(d2_winmargin_a)] <- ""
# e0_winmargin_h[is.na(e0_winmargin_h)] <- ""
# e0_winmargin_a[is.na(e0_winmargin_a)] <- ""
# e1_winmargin_h[is.na(e1_winmargin_h)] <- ""
# e1_winmargin_a[is.na(e1_winmargin_a)] <- ""
# e2_winmargin_h[is.na(e2_winmargin_h)] <- ""
# e2_winmargin_a[is.na(e2_winmargin_a)] <- ""
# e3_winmargin_h[is.na(e3_winmargin_h)] <- ""
# e3_winmargin_a[is.na(e3_winmargin_a)] <- ""
# ec_winmargin_h[is.na(ec_winmargin_h)] <- ""
# ec_winmargin_a[is.na(ec_winmargin_a)] <- ""
# f1_winmargin_h[is.na(f1_winmargin_h)] <- ""
# f1_winmargin_a[is.na(f1_winmargin_a)] <- ""
# f2_winmargin_h[is.na(f2_winmargin_h)] <- ""
# f2_winmargin_a[is.na(f2_winmargin_a)] <- ""
# g1_winmargin_h[is.na(g1_winmargin_h)] <- ""
# g1_winmargin_a[is.na(g1_winmargin_a)] <- ""
# i1_winmargin_h[is.na(i1_winmargin_h)] <- ""
# i1_winmargin_a[is.na(i1_winmargin_a)] <- ""
# i2_winmargin_h[is.na(i2_winmargin_h)] <- ""
# i2_winmargin_a[is.na(i2_winmargin_a)] <- ""
# n1_winmargin_h[is.na(n1_winmargin_h)] <- ""
# n1_winmargin_a[is.na(n1_winmargin_a)] <- ""
# p1_winmargin_h[is.na(p1_winmargin_h)] <- ""
# p1_winmargin_a[is.na(p1_winmargin_a)] <- ""
# sc0_winmargin_h[is.na(sc0_winmargin_h)] <- ""
# sc0_winmargin_a[is.na(sc0_winmargin_a)] <- ""
# sc1_winmargin_h[is.na(sc1_winmargin_h)] <- ""
# sc1_winmargin_a[is.na(sc1_winmargin_a)] <- ""
# sc2_winmargin_h[is.na(sc2_winmargin_h)] <- ""
# sc2_winmargin_a[is.na(sc2_winmargin_a)] <- ""
# sc3_winmargin_h[is.na(sc3_winmargin_h)] <- ""
# sc3_winmargin_a[is.na(sc3_winmargin_a)] <- ""
# sp1_winmargin_h[is.na(sp1_winmargin_h)] <- ""
# sp1_winmargin_a[is.na(sp1_winmargin_a)] <- ""
# sp2_winmargin_h[is.na(sp2_winmargin_h)] <- ""
# sp2_winmargin_a[is.na(sp2_winmargin_a)] <- ""
# t1_winmargin_h[is.na(t1_winmargin_h)] <- ""
# t1_winmargin_a[is.na(t1_winmargin_a)] <- ""
########################################################
#MLS
for(mls_rowhwm in 1:nrow(mls_winmargin_h)) {
  for(mls_colhwm in 1:ncol(mls_winmargin_h)) {

    # print(my_matrix[row, col])
    for(mls_rowawm in 1:nrow(mls_winmargin_a)) {
      for(mls_colawm in 1:ncol(mls_winmargin_a)) {
        ifelse(!mls_winmargin_a[mls_rowawm,mls_colawm]=="",mls_winmargin_h[mls_rowawm,mls_colawm] <- mls_winmargin_a[mls_rowawm,mls_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhwm in 1:nrow(arg_winmargin_h)) {
  for(arg_colhwm in 1:ncol(arg_winmargin_h)) {

    # print(my_matrix[row, col])
    for(arg_rowawm in 1:nrow(arg_winmargin_a)) {
      for(arg_colawm in 1:ncol(arg_winmargin_a)) {
        ifelse(!arg_winmargin_a[arg_rowawm,arg_colawm]=="",arg_winmargin_h[arg_rowawm,arg_colawm] <- arg_winmargin_a[arg_rowawm,arg_colawm],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhwm in 1:nrow(d2_winmargin_h)) {
#   for(d2_colhwm in 1:ncol(d2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowawm in 1:nrow(d2_winmargin_a)) {
#       for(d2_colawm in 1:ncol(d2_winmargin_a)) {
#         ifelse(!d2_winmargin_a[d2_rowawm,d2_colawm]=="",d2_winmargin_h[d2_rowawm,d2_colawm] <- d2_winmargin_a[d2_rowawm,d2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhwm in 1:nrow(e0_winmargin_h)) {
#   for(e0_colhwm in 1:ncol(e0_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowawm in 1:nrow(e0_winmargin_a)) {
#       for(e0_colawm in 1:ncol(e0_winmargin_a)) {
#         ifelse(!e0_winmargin_a[e0_rowawm,e0_colawm]=="",e0_winmargin_h[e0_rowawm,e0_colawm] <- e0_winmargin_a[e0_rowawm,e0_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhwm in 1:nrow(e1_winmargin_h)) {
#   for(e1_colhwm in 1:ncol(e1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowawm in 1:nrow(e1_winmargin_a)) {
#       for(e1_colawm in 1:ncol(e1_winmargin_a)) {
#         ifelse(!e1_winmargin_a[e1_rowawm,e1_colawm]=="",e1_winmargin_h[e1_rowawm,e1_colawm] <- e1_winmargin_a[e1_rowawm,e1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhwm in 1:nrow(e2_winmargin_h)) {
#   for(e2_colhwm in 1:ncol(e2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowawm in 1:nrow(e2_winmargin_a)) {
#       for(e2_colawm in 1:ncol(e2_winmargin_a)) {
#         ifelse(!e2_winmargin_a[e2_rowawm,e2_colawm]=="",e2_winmargin_h[e2_rowawm,e2_colawm] <- e2_winmargin_a[e2_rowawm,e2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhwm in 1:nrow(e3_winmargin_h)) {
#   for(e3_colhwm in 1:ncol(e3_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowawm in 1:nrow(e3_winmargin_a)) {
#       for(e3_colawm in 1:ncol(e3_winmargin_a)) {
#         ifelse(!e3_winmargin_a[e3_rowawm,e3_colawm]=="",e3_winmargin_h[e3_rowawm,e3_colawm] <- e3_winmargin_a[e3_rowawm,e3_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhwm in 1:nrow(ec_winmargin_h)) {
#   for(ec_colhwm in 1:ncol(ec_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowawm in 1:nrow(ec_winmargin_a)) {
#       for(ec_colawm in 1:ncol(ec_winmargin_a)) {
#         ifelse(!ec_winmargin_a[ec_rowawm,ec_colawm]=="",ec_winmargin_h[ec_rowawm,ec_colawm] <- ec_winmargin_a[ec_rowawm,ec_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhwm in 1:nrow(f1_winmargin_h)) {
#   for(f1_colhwm in 1:ncol(f1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowawm in 1:nrow(f1_winmargin_a)) {
#       for(f1_colawm in 1:ncol(f1_winmargin_a)) {
#         ifelse(!f1_winmargin_a[f1_rowawm,f1_colawm]=="",f1_winmargin_h[f1_rowawm,f1_colawm] <- f1_winmargin_a[f1_rowawm,f1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhwm in 1:nrow(f2_winmargin_h)) {
#   for(f2_colhwm in 1:ncol(f2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowawm in 1:nrow(f2_winmargin_a)) {
#       for(f2_colawm in 1:ncol(f2_winmargin_a)) {
#         ifelse(!f2_winmargin_a[f2_rowawm,f2_colawm]=="",f2_winmargin_h[f2_rowawm,f2_colawm] <- f2_winmargin_a[f2_rowawm,f2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhwm in 1:nrow(g1_winmargin_h)) {
#   for(g1_colhwm in 1:ncol(g1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowawm in 1:nrow(g1_winmargin_a)) {
#       for(g1_colawm in 1:ncol(g1_winmargin_a)) {
#         ifelse(!g1_winmargin_a[g1_rowawm,g1_colawm]=="",g1_winmargin_h[g1_rowawm,g1_colawm] <- g1_winmargin_a[g1_rowawm,g1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhwm in 1:nrow(i1_winmargin_h)) {
#   for(i1_colhwm in 1:ncol(i1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowawm in 1:nrow(i1_winmargin_a)) {
#       for(i1_colawm in 1:ncol(i1_winmargin_a)) {
#         ifelse(!i1_winmargin_a[i1_rowawm,i1_colawm]=="",i1_winmargin_h[i1_rowawm,i1_colawm] <- i1_winmargin_a[i1_rowawm,i1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhwm in 1:nrow(i2_winmargin_h)) {
#   for(i2_colhwm in 1:ncol(i2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowawm in 1:nrow(i2_winmargin_a)) {
#       for(i2_colawm in 1:ncol(i2_winmargin_a)) {
#         ifelse(!i2_winmargin_a[i2_rowawm,i2_colawm]=="",i2_winmargin_h[i2_rowawm,i2_colawm] <- i2_winmargin_a[i2_rowawm,i2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhwm in 1:nrow(n1_winmargin_h)) {
#   for(n1_colhwm in 1:ncol(n1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowawm in 1:nrow(n1_winmargin_a)) {
#       for(n1_colawm in 1:ncol(n1_winmargin_a)) {
#         ifelse(!n1_winmargin_a[n1_rowawm,n1_colawm]=="",n1_winmargin_h[n1_rowawm,n1_colawm] <- n1_winmargin_a[n1_rowawm,n1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhwm in 1:nrow(p1_winmargin_h)) {
#   for(p1_colhwm in 1:ncol(p1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowawm in 1:nrow(p1_winmargin_a)) {
#       for(p1_colawm in 1:ncol(p1_winmargin_a)) {
#         ifelse(!p1_winmargin_a[p1_rowawm,p1_colawm]=="",p1_winmargin_h[p1_rowawm,p1_colawm] <- p1_winmargin_a[p1_rowawm,p1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhwm in 1:nrow(sc0_winmargin_h)) {
#   for(sc0_colhwm in 1:ncol(sc0_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowawm in 1:nrow(sc0_winmargin_a)) {
#       for(sc0_colawm in 1:ncol(sc0_winmargin_a)) {
#         ifelse(!sc0_winmargin_a[sc0_rowawm,sc0_colawm]=="",sc0_winmargin_h[sc0_rowawm,sc0_colawm] <- sc0_winmargin_a[sc0_rowawm,sc0_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhwm in 1:nrow(sc1_winmargin_h)) {
#   for(sc1_colhwm in 1:ncol(sc1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowawm in 1:nrow(sc1_winmargin_a)) {
#       for(sc1_colawm in 1:ncol(sc1_winmargin_a)) {
#         ifelse(!sc1_winmargin_a[sc1_rowawm,sc1_colawm]=="",sc1_winmargin_h[sc1_rowawm,sc1_colawm] <- sc1_winmargin_a[sc1_rowawm,sc1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhwm in 1:nrow(sc2_winmargin_h)) {
#   for(sc2_colhwm in 1:ncol(sc2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowawm in 1:nrow(sc2_winmargin_a)) {
#       for(sc2_colawm in 1:ncol(sc2_winmargin_a)) {
#         ifelse(!sc2_winmargin_a[sc2_rowawm,sc2_colawm]=="",sc2_winmargin_h[sc2_rowawm,sc2_colawm] <- sc2_winmargin_a[sc2_rowawm,sc2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhwm in 1:nrow(sc3_winmargin_h)) {
#   for(sc3_colhwm in 1:ncol(sc3_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowawm in 1:nrow(sc3_winmargin_a)) {
#       for(sc3_colawm in 1:ncol(sc3_winmargin_a)) {
#         ifelse(!sc3_winmargin_a[sc3_rowawm,sc3_colawm]=="",sc3_winmargin_h[sc3_rowawm,sc3_colawm] <- sc3_winmargin_a[sc3_rowawm,sc3_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhwm in 1:nrow(sp1_winmargin_h)) {
#   for(sp1_colhwm in 1:ncol(sp1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowawm in 1:nrow(sp1_winmargin_a)) {
#       for(sp1_colawm in 1:ncol(sp1_winmargin_a)) {
#         ifelse(!sp1_winmargin_a[sp1_rowawm,sp1_colawm]=="",sp1_winmargin_h[sp1_rowawm,sp1_colawm] <- sp1_winmargin_a[sp1_rowawm,sp1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhwm in 1:nrow(sp2_winmargin_h)) {
#   for(sp2_colhwm in 1:ncol(sp2_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowawm in 1:nrow(sp2_winmargin_a)) {
#       for(sp2_colawm in 1:ncol(sp2_winmargin_a)) {
#         ifelse(!sp2_winmargin_a[sp2_rowawm,sp2_colawm]=="",sp2_winmargin_h[sp2_rowawm,sp2_colawm] <- sp2_winmargin_a[sp2_rowawm,sp2_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhwm in 1:nrow(t1_winmargin_h)) {
#   for(t1_colhwm in 1:ncol(t1_winmargin_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowawm in 1:nrow(t1_winmargin_a)) {
#       for(t1_colawm in 1:ncol(t1_winmargin_a)) {
#         ifelse(!t1_winmargin_a[t1_rowawm,t1_colawm]=="",t1_winmargin_h[t1_rowawm,t1_colawm] <- t1_winmargin_a[t1_rowawm,t1_colawm],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
#













