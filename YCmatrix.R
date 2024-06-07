#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_MLS, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
mls_yellowscored_h <- tapply(MLS$HY, MLS[c("HomeTeam", "Date")],mean)
mls_yellowscored_a <- tapply(MLS$AY, MLS[c("AwayTeam", "Date")],mean)
arg_yellowscored_h <- tapply(ARG$HY, ARG[c("HomeTeam", "Date")],mean)
arg_yellowscored_a <- tapply(ARG$AY, ARG[c("AwayTeam", "Date")],mean)
# d2_yellowscored_h <- tapply(D2$HY, D2[c("HomeTeam", "Date")],mean)
# d2_yellowscored_a <- tapply(D2$AY, D2[c("AwayTeam", "Date")],mean)
# e0_yellowscored_h <- tapply(E0$HY, E0[c("HomeTeam", "Date")],mean)
# e0_yellowscored_a <- tapply(E0$AY, E0[c("AwayTeam", "Date")],mean)
# e1_yellowscored_h <- tapply(E1$HY, E1[c("HomeTeam", "Date")],mean)
# e1_yellowscored_a <- tapply(E1$AY, E1[c("AwayTeam", "Date")],mean)
# e2_yellowscored_h <- tapply(E2$HY, E2[c("HomeTeam", "Date")],mean)
# e2_yellowscored_a <- tapply(E2$AY, E2[c("AwayTeam", "Date")],mean)
# e3_yellowscored_h <- tapply(E3$HY, E3[c("HomeTeam", "Date")],mean)
# e3_yellowscored_a <- tapply(E3$AY, E3[c("AwayTeam", "Date")],mean)
# ec_yellowscored_h <- tapply(EC$HY, EC[c("HomeTeam", "Date")],mean)
# ec_yellowscored_a <- tapply(EC$AY, EC[c("AwayTeam", "Date")],mean)
# f1_yellowscored_h <- tapply(F1$HY, F1[c("HomeTeam", "Date")],mean)
# f1_yellowscored_a <- tapply(F1$AY, F1[c("AwayTeam", "Date")],mean)
# f2_yellowscored_h <- tapply(F2$HY, F2[c("HomeTeam", "Date")],mean)
# f2_yellowscored_a <- tapply(F2$AY, F2[c("AwayTeam", "Date")],mean)
# g1_yellowscored_h <- tapply(G1$HY, G1[c("HomeTeam", "Date")],mean)
# g1_yellowscored_a <- tapply(G1$AY, G1[c("AwayTeam", "Date")],mean)
# i1_yellowscored_h <- tapply(I1$HY, I1[c("HomeTeam", "Date")],mean)
# i1_yellowscored_a <- tapply(I1$AY, I1[c("AwayTeam", "Date")],mean)
# i2_yellowscored_h <- tapply(I2$HY, I2[c("HomeTeam", "Date")],mean)
# i2_yellowscored_a <- tapply(I2$AY, I2[c("AwayTeam", "Date")],mean)
# n1_yellowscored_h <- tapply(N1$HY, N1[c("HomeTeam", "Date")],mean)
# n1_yellowscored_a <- tapply(N1$AY, N1[c("AwayTeam", "Date")],mean)
# p1_yellowscored_h <- tapply(P1$HY, P1[c("HomeTeam", "Date")],mean)
# p1_yellowscored_a <- tapply(P1$AY, P1[c("AwayTeam", "Date")],mean)
# sc0_yellowscored_h <- tapply(SC0$HY, SC0[c("HomeTeam", "Date")],mean)
# sc0_yellowscored_a <- tapply(SC0$AY, SC0[c("AwayTeam", "Date")],mean)
# sc1_yellowscored_h <- tapply(SC1$HY, SC1[c("HomeTeam", "Date")],mean)
# sc1_yellowscored_a <- tapply(SC1$AY, SC1[c("AwayTeam", "Date")],mean)
# sc2_yellowscored_h <- tapply(SC2$HY, SC2[c("HomeTeam", "Date")],mean)
# sc2_yellowscored_a <- tapply(SC2$AY, SC2[c("AwayTeam", "Date")],mean)
# sc3_yellowscored_h <- tapply(SC3$HY, SC3[c("HomeTeam", "Date")],mean)
# sc3_yellowscored_a <- tapply(SC3$AY, SC3[c("AwayTeam", "Date")],mean)
# sp1_yellowscored_h <- tapply(SP1$HY, SP1[c("HomeTeam", "Date")],mean)
# sp1_yellowscored_a <- tapply(SP1$AY, SP1[c("AwayTeam", "Date")],mean)
# sp2_yellowscored_h <- tapply(SP2$HY, SP2[c("HomeTeam", "Date")],mean)
# sp2_yellowscored_a <- tapply(SP2$AY, SP2[c("AwayTeam", "Date")],mean)
# t1_yellowscored_h <- tapply(T1$HY, T1[c("HomeTeam", "Date")],mean)
# t1_yellowscored_a <- tapply(T1$AY, T1[c("AwayTeam", "Date")],mean)
#remove na values

mls_yellowscored_h[is.na(mls_yellowscored_h)] <- ""
mls_yellowscored_a[is.na(mls_yellowscored_a)] <- ""
arg_yellowscored_h[is.na(arg_yellowscored_h)] <- ""
arg_yellowscored_a[is.na(arg_yellowscored_a)] <- ""
# d2_yellowscored_h[is.na(d2_yellowscored_h)] <- ""
# d2_yellowscored_a[is.na(d2_yellowscored_a)] <- ""
# e0_yellowscored_h[is.na(e0_yellowscored_h)] <- ""
# e0_yellowscored_a[is.na(e0_yellowscored_a)] <- ""
# e1_yellowscored_h[is.na(e1_yellowscored_h)] <- ""
# e1_yellowscored_a[is.na(e1_yellowscored_a)] <- ""
# e2_yellowscored_h[is.na(e2_yellowscored_h)] <- ""
# e2_yellowscored_a[is.na(e2_yellowscored_a)] <- ""
# e3_yellowscored_h[is.na(e3_yellowscored_h)] <- ""
# e3_yellowscored_a[is.na(e3_yellowscored_a)] <- ""
# ec_yellowscored_h[is.na(ec_yellowscored_h)] <- ""
# ec_yellowscored_a[is.na(ec_yellowscored_a)] <- ""
# f1_yellowscored_h[is.na(f1_yellowscored_h)] <- ""
# f1_yellowscored_a[is.na(f1_yellowscored_a)] <- ""
# f2_yellowscored_h[is.na(f2_yellowscored_h)] <- ""
# f2_yellowscored_a[is.na(f2_yellowscored_a)] <- ""
# g1_yellowscored_h[is.na(g1_yellowscored_h)] <- ""
# g1_yellowscored_a[is.na(g1_yellowscored_a)] <- ""
# i1_yellowscored_h[is.na(i1_yellowscored_h)] <- ""
# i1_yellowscored_a[is.na(i1_yellowscored_a)] <- ""
# i2_yellowscored_h[is.na(i2_yellowscored_h)] <- ""
# i2_yellowscored_a[is.na(i2_yellowscored_a)] <- ""
# n1_yellowscored_h[is.na(n1_yellowscored_h)] <- ""
# n1_yellowscored_a[is.na(n1_yellowscored_a)] <- ""
# p1_yellowscored_h[is.na(p1_yellowscored_h)] <- ""
# p1_yellowscored_a[is.na(p1_yellowscored_a)] <- ""
# sc0_yellowscored_h[is.na(sc0_yellowscored_h)] <- ""
# sc0_yellowscored_a[is.na(sc0_yellowscored_a)] <- ""
# sc1_yellowscored_h[is.na(sc1_yellowscored_h)] <- ""
# sc1_yellowscored_a[is.na(sc1_yellowscored_a)] <- ""
# sc2_yellowscored_h[is.na(sc2_yellowscored_h)] <- ""
# sc2_yellowscored_a[is.na(sc2_yellowscored_a)] <- ""
# sc3_yellowscored_h[is.na(sc3_yellowscored_h)] <- ""
# sc3_yellowscored_a[is.na(sc3_yellowscored_a)] <- ""
# sp1_yellowscored_h[is.na(sp1_yellowscored_h)] <- ""
# sp1_yellowscored_a[is.na(sp1_yellowscored_a)] <- ""
# sp2_yellowscored_h[is.na(sp2_yellowscored_h)] <- ""
# sp2_yellowscored_a[is.na(sp2_yellowscored_a)] <- ""
# t1_yellowscored_h[is.na(t1_yellowscored_h)] <- ""
# t1_yellowscored_a[is.na(t1_yellowscored_a)] <- ""
#combine the matrices
#MLS
for(mls_rowhys in 1:nrow(mls_yellowscored_h)) {
  for(mls_colhys in 1:ncol(mls_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(mls_roways in 1:nrow(mls_yellowscored_a)) {
      for(mls_colays in 1:ncol(mls_yellowscored_a)) {
        ifelse(!mls_yellowscored_a[mls_roways,mls_colays]=="",mls_yellowscored_h[mls_roways,mls_colays] <- mls_yellowscored_a[mls_roways,mls_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhys in 1:nrow(arg_yellowscored_h)) {
  for(arg_colhys in 1:ncol(arg_yellowscored_h)) {

    # print(my_matrix[row, col])
    for(arg_roways in 1:nrow(arg_yellowscored_a)) {
      for(arg_colays in 1:ncol(arg_yellowscored_a)) {
        ifelse(!arg_yellowscored_a[arg_roways,arg_colays]=="",arg_yellowscored_h[arg_roways,arg_colays] <- arg_yellowscored_a[arg_roways,arg_colays],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhys in 1:nrow(d2_yellowscored_h)) {
#   for(d2_colhys in 1:ncol(d2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_roways in 1:nrow(d2_yellowscored_a)) {
#       for(d2_colays in 1:ncol(d2_yellowscored_a)) {
#         ifelse(!d2_yellowscored_a[d2_roways,d2_colays]=="",d2_yellowscored_h[d2_roways,d2_colays] <- d2_yellowscored_a[d2_roways,d2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhys in 1:nrow(e0_yellowscored_h)) {
#   for(e0_colhys in 1:ncol(e0_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_roways in 1:nrow(e0_yellowscored_a)) {
#       for(e0_colays in 1:ncol(e0_yellowscored_a)) {
#         ifelse(!e0_yellowscored_a[e0_roways,e0_colays]=="",e0_yellowscored_h[e0_roways,e0_colays] <- e0_yellowscored_a[e0_roways,e0_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhys in 1:nrow(e1_yellowscored_h)) {
#   for(e1_colhys in 1:ncol(e1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_roways in 1:nrow(e1_yellowscored_a)) {
#       for(e1_colays in 1:ncol(e1_yellowscored_a)) {
#         ifelse(!e1_yellowscored_a[e1_roways,e1_colays]=="",e1_yellowscored_h[e1_roways,e1_colays] <- e1_yellowscored_a[e1_roways,e1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhys in 1:nrow(e2_yellowscored_h)) {
#   for(e2_colhys in 1:ncol(e2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_roways in 1:nrow(e2_yellowscored_a)) {
#       for(e2_colays in 1:ncol(e2_yellowscored_a)) {
#         ifelse(!e2_yellowscored_a[e2_roways,e2_colays]=="",e2_yellowscored_h[e2_roways,e2_colays] <- e2_yellowscored_a[e2_roways,e2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhys in 1:nrow(e3_yellowscored_h)) {
#   for(e3_colhys in 1:ncol(e3_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_roways in 1:nrow(e3_yellowscored_a)) {
#       for(e3_colays in 1:ncol(e3_yellowscored_a)) {
#         ifelse(!e3_yellowscored_a[e3_roways,e3_colays]=="",e3_yellowscored_h[e3_roways,e3_colays] <- e3_yellowscored_a[e3_roways,e3_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhys in 1:nrow(ec_yellowscored_h)) {
#   for(ec_colhys in 1:ncol(ec_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_roways in 1:nrow(ec_yellowscored_a)) {
#       for(ec_colays in 1:ncol(ec_yellowscored_a)) {
#         ifelse(!ec_yellowscored_a[ec_roways,ec_colays]=="",ec_yellowscored_h[ec_roways,ec_colays] <- ec_yellowscored_a[ec_roways,ec_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhys in 1:nrow(f1_yellowscored_h)) {
#   for(f1_colhys in 1:ncol(f1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_roways in 1:nrow(f1_yellowscored_a)) {
#       for(f1_colays in 1:ncol(f1_yellowscored_a)) {
#         ifelse(!f1_yellowscored_a[f1_roways,f1_colays]=="",f1_yellowscored_h[f1_roways,f1_colays] <- f1_yellowscored_a[f1_roways,f1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhys in 1:nrow(f2_yellowscored_h)) {
#   for(f2_colhys in 1:ncol(f2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_roways in 1:nrow(f2_yellowscored_a)) {
#       for(f2_colays in 1:ncol(f2_yellowscored_a)) {
#         ifelse(!f2_yellowscored_a[f2_roways,f2_colays]=="",f2_yellowscored_h[f2_roways,f2_colays] <- f2_yellowscored_a[f2_roways,f2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhys in 1:nrow(g1_yellowscored_h)) {
#   for(g1_colhys in 1:ncol(g1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_roways in 1:nrow(g1_yellowscored_a)) {
#       for(g1_colays in 1:ncol(g1_yellowscored_a)) {
#         ifelse(!g1_yellowscored_a[g1_roways,g1_colays]=="",g1_yellowscored_h[g1_roways,g1_colays] <- g1_yellowscored_a[g1_roways,g1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhys in 1:nrow(i1_yellowscored_h)) {
#   for(i1_colhys in 1:ncol(i1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_roways in 1:nrow(i1_yellowscored_a)) {
#       for(i1_colays in 1:ncol(i1_yellowscored_a)) {
#         ifelse(!i1_yellowscored_a[i1_roways,i1_colays]=="",i1_yellowscored_h[i1_roways,i1_colays] <- i1_yellowscored_a[i1_roways,i1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhys in 1:nrow(i2_yellowscored_h)) {
#   for(i2_colhys in 1:ncol(i2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_roways in 1:nrow(i2_yellowscored_a)) {
#       for(i2_colays in 1:ncol(i2_yellowscored_a)) {
#         ifelse(!i2_yellowscored_a[i2_roways,i2_colays]=="",i2_yellowscored_h[i2_roways,i2_colays] <- i2_yellowscored_a[i2_roways,i2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhys in 1:nrow(n1_yellowscored_h)) {
#   for(n1_colhys in 1:ncol(n1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_roways in 1:nrow(n1_yellowscored_a)) {
#       for(n1_colays in 1:ncol(n1_yellowscored_a)) {
#         ifelse(!n1_yellowscored_a[n1_roways,n1_colays]=="",n1_yellowscored_h[n1_roways,n1_colays] <- n1_yellowscored_a[n1_roways,n1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhys in 1:nrow(p1_yellowscored_h)) {
#   for(p1_colhys in 1:ncol(p1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_roways in 1:nrow(p1_yellowscored_a)) {
#       for(p1_colays in 1:ncol(p1_yellowscored_a)) {
#         ifelse(!p1_yellowscored_a[p1_roways,p1_colays]=="",p1_yellowscored_h[p1_roways,p1_colays] <- p1_yellowscored_a[p1_roways,p1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhys in 1:nrow(sc0_yellowscored_h)) {
#   for(sc0_colhys in 1:ncol(sc0_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_roways in 1:nrow(sc0_yellowscored_a)) {
#       for(sc0_colays in 1:ncol(sc0_yellowscored_a)) {
#         ifelse(!sc0_yellowscored_a[sc0_roways,sc0_colays]=="",sc0_yellowscored_h[sc0_roways,sc0_colays] <- sc0_yellowscored_a[sc0_roways,sc0_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhys in 1:nrow(sc1_yellowscored_h)) {
#   for(sc1_colhys in 1:ncol(sc1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_roways in 1:nrow(sc1_yellowscored_a)) {
#       for(sc1_colays in 1:ncol(sc1_yellowscored_a)) {
#         ifelse(!sc1_yellowscored_a[sc1_roways,sc1_colays]=="",sc1_yellowscored_h[sc1_roways,sc1_colays] <- sc1_yellowscored_a[sc1_roways,sc1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhys in 1:nrow(sc2_yellowscored_h)) {
#   for(sc2_colhys in 1:ncol(sc2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_roways in 1:nrow(sc2_yellowscored_a)) {
#       for(sc2_colays in 1:ncol(sc2_yellowscored_a)) {
#         ifelse(!sc2_yellowscored_a[sc2_roways,sc2_colays]=="",sc2_yellowscored_h[sc2_roways,sc2_colays] <- sc2_yellowscored_a[sc2_roways,sc2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhys in 1:nrow(sc3_yellowscored_h)) {
#   for(sc3_colhys in 1:ncol(sc3_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_roways in 1:nrow(sc3_yellowscored_a)) {
#       for(sc3_colays in 1:ncol(sc3_yellowscored_a)) {
#         ifelse(!sc3_yellowscored_a[sc3_roways,sc3_colays]=="",sc3_yellowscored_h[sc3_roways,sc3_colays] <- sc3_yellowscored_a[sc3_roways,sc3_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhys in 1:nrow(sp1_yellowscored_h)) {
#   for(sp1_colhys in 1:ncol(sp1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_roways in 1:nrow(sp1_yellowscored_a)) {
#       for(sp1_colays in 1:ncol(sp1_yellowscored_a)) {
#         ifelse(!sp1_yellowscored_a[sp1_roways,sp1_colays]=="",sp1_yellowscored_h[sp1_roways,sp1_colays] <- sp1_yellowscored_a[sp1_roways,sp1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhys in 1:nrow(sp2_yellowscored_h)) {
#   for(sp2_colhys in 1:ncol(sp2_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_roways in 1:nrow(sp2_yellowscored_a)) {
#       for(sp2_colays in 1:ncol(sp2_yellowscored_a)) {
#         ifelse(!sp2_yellowscored_a[sp2_roways,sp2_colays]=="",sp2_yellowscored_h[sp2_roways,sp2_colays] <- sp2_yellowscored_a[sp2_roways,sp2_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhys in 1:nrow(t1_yellowscored_h)) {
#   for(t1_colhys in 1:ncol(t1_yellowscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_roways in 1:nrow(t1_yellowscored_a)) {
#       for(t1_colays in 1:ncol(t1_yellowscored_a)) {
#         ifelse(!t1_yellowscored_a[t1_roways,t1_colays]=="",t1_yellowscored_h[t1_roways,t1_colays] <- t1_yellowscored_a[t1_roways,t1_colays],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }

#
#
# #write out the data to excel
# write.xlsx(mls_yellowscored_h,'YCmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_yellowscored_h,'YCmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_yellowscored_h,'YCmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_yellowscored_h,'YCmatrix.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_yellowscored_h,'YCmatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_yellowscored_h,'YCmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_yellowscored_h,'YCmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_yellowscored_h,'YCmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_yellowscored_h,'YCmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_yellowscored_h,'YCmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_yellowscored_h,'YCmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_yellowscored_h,'YCmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_yellowscored_h,'YCmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_yellowscored_h,'YCmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_yellowscored_h,'YCmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_yellowscored_h,'YCmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_yellowscored_h,'YCmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_yellowscored_h,'YCmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_yellowscored_h,'YCmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_yellowscored_h,'YCmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_yellowscored_h,'YCmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_yellowscored_h,'YCmatrix.xlsx',sheetName = "T1", append = TRU
