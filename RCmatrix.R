#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#tgv2 <- with(MLS, tapply(TG, list(HomeTeam, AwarTeam), FUN = mean))
#with(sorted_MLS, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awarteam_tg <- with(sorted_MLS, tapply(TG, list(AwarTeam, Date), FUN = mean))

#create home and awar matrices
mls_redscored_h <- tapply(MLS$HR, MLS[c("HomeTeam", "Date")],mean)
mls_redscored_a <- tapply(MLS$AR, MLS[c("AwayTeam", "Date")],mean)
arg_redscored_h <- tapply(ARG$HR, ARG[c("HomeTeam", "Date")],mean)
arg_redscored_a <- tapply(ARG$AR, ARG[c("AwayTeam", "Date")],mean)
# d2_redscored_h <- tapply(D2$HR, D2[c("HomeTeam", "Date")],mean)
# d2_redscored_a <- tapply(D2$AR, D2[c("AwayTeam", "Date")],mean)
# e0_redscored_h <- tapply(E0$HR, E0[c("HomeTeam", "Date")],mean)
# e0_redscored_a <- tapply(E0$AR, E0[c("AwayTeam", "Date")],mean)
# e1_redscored_h <- tapply(E1$HR, E1[c("HomeTeam", "Date")],mean)
# e1_redscored_a <- tapply(E1$AR, E1[c("AwayTeam", "Date")],mean)
# e2_redscored_h <- tapply(E2$HR, E2[c("HomeTeam", "Date")],mean)
# e2_redscored_a <- tapply(E2$AR, E2[c("AwayTeam", "Date")],mean)
# e3_redscored_h <- tapply(E3$HR, E3[c("HomeTeam", "Date")],mean)
# e3_redscored_a <- tapply(E3$AR, E3[c("AwayTeam", "Date")],mean)
# ec_redscored_h <- tapply(EC$HR, EC[c("HomeTeam", "Date")],mean)
# ec_redscored_a <- tapply(EC$AR, EC[c("AwayTeam", "Date")],mean)
# f1_redscored_h <- tapply(F1$HR, F1[c("HomeTeam", "Date")],mean)
# f1_redscored_a <- tapply(F1$AR, F1[c("AwayTeam", "Date")],mean)
# f2_redscored_h <- tapply(F2$HR, F2[c("HomeTeam", "Date")],mean)
# f2_redscored_a <- tapply(F2$AR, F2[c("AwayTeam", "Date")],mean)
# g1_redscored_h <- tapply(G1$HR, G1[c("HomeTeam", "Date")],mean)
# g1_redscored_a <- tapply(G1$AR, G1[c("AwayTeam", "Date")],mean)
# i1_redscored_h <- tapply(I1$HR, I1[c("HomeTeam", "Date")],mean)
# i1_redscored_a <- tapply(I1$AR, I1[c("AwayTeam", "Date")],mean)
# i2_redscored_h <- tapply(I2$HR, I2[c("HomeTeam", "Date")],mean)
# i2_redscored_a <- tapply(I2$AR, I2[c("AwayTeam", "Date")],mean)
# n1_redscored_h <- tapply(N1$HR, N1[c("HomeTeam", "Date")],mean)
# n1_redscored_a <- tapply(N1$AR, N1[c("AwayTeam", "Date")],mean)
# p1_redscored_h <- tapply(P1$HR, P1[c("HomeTeam", "Date")],mean)
# p1_redscored_a <- tapply(P1$AR, P1[c("AwayTeam", "Date")],mean)
# sc0_redscored_h <- tapply(SC0$HR, SC0[c("HomeTeam", "Date")],mean)
# sc0_redscored_a <- tapply(SC0$AR, SC0[c("AwayTeam", "Date")],mean)
# sc1_redscored_h <- tapply(SC1$HR, SC1[c("HomeTeam", "Date")],mean)
# sc1_redscored_a <- tapply(SC1$AR, SC1[c("AwayTeam", "Date")],mean)
# sc2_redscored_h <- tapply(SC2$HR, SC2[c("HomeTeam", "Date")],mean)
# sc2_redscored_a <- tapply(SC2$AR, SC2[c("AwayTeam", "Date")],mean)
# sc3_redscored_h <- tapply(SC3$HR, SC3[c("HomeTeam", "Date")],mean)
# sc3_redscored_a <- tapply(SC3$AR, SC3[c("AwayTeam", "Date")],mean)
# sp1_redscored_h <- tapply(SP1$HR, SP1[c("HomeTeam", "Date")],mean)
# sp1_redscored_a <- tapply(SP1$AR, SP1[c("AwayTeam", "Date")],mean)
# sp2_redscored_h <- tapply(SP2$HR, SP2[c("HomeTeam", "Date")],mean)
# sp2_redscored_a <- tapply(SP2$AR, SP2[c("AwayTeam", "Date")],mean)
# t1_redscored_h <- tapply(T1$HR, T1[c("HomeTeam", "Date")],mean)
# t1_redscored_a <- tapply(T1$AR, T1[c("AwayTeam", "Date")],mean)
# #remove na values

mls_redscored_h[is.na(mls_redscored_h)] <- ""
mls_redscored_a[is.na(mls_redscored_a)] <- ""
arg_redscored_h[is.na(arg_redscored_h)] <- ""
arg_redscored_a[is.na(arg_redscored_a)] <- ""
# d2_redscored_h[is.na(d2_redscored_h)] <- ""
# d2_redscored_a[is.na(d2_redscored_a)] <- ""
# e0_redscored_h[is.na(e0_redscored_h)] <- ""
# e0_redscored_a[is.na(e0_redscored_a)] <- ""
# e1_redscored_h[is.na(e1_redscored_h)] <- ""
# e1_redscored_a[is.na(e1_redscored_a)] <- ""
# e2_redscored_h[is.na(e2_redscored_h)] <- ""
# e2_redscored_a[is.na(e2_redscored_a)] <- ""
# e3_redscored_h[is.na(e3_redscored_h)] <- ""
# e3_redscored_a[is.na(e3_redscored_a)] <- ""
# ec_redscored_h[is.na(ec_redscored_h)] <- ""
# ec_redscored_a[is.na(ec_redscored_a)] <- ""
# f1_redscored_h[is.na(f1_redscored_h)] <- ""
# f1_redscored_a[is.na(f1_redscored_a)] <- ""
# f2_redscored_h[is.na(f2_redscored_h)] <- ""
# f2_redscored_a[is.na(f2_redscored_a)] <- ""
# g1_redscored_h[is.na(g1_redscored_h)] <- ""
# g1_redscored_a[is.na(g1_redscored_a)] <- ""
# i1_redscored_h[is.na(i1_redscored_h)] <- ""
# i1_redscored_a[is.na(i1_redscored_a)] <- ""
# i2_redscored_h[is.na(i2_redscored_h)] <- ""
# i2_redscored_a[is.na(i2_redscored_a)] <- ""
# n1_redscored_h[is.na(n1_redscored_h)] <- ""
# n1_redscored_a[is.na(n1_redscored_a)] <- ""
# p1_redscored_h[is.na(p1_redscored_h)] <- ""
# p1_redscored_a[is.na(p1_redscored_a)] <- ""
# sc0_redscored_h[is.na(sc0_redscored_h)] <- ""
# sc0_redscored_a[is.na(sc0_redscored_a)] <- ""
# sc1_redscored_h[is.na(sc1_redscored_h)] <- ""
# sc1_redscored_a[is.na(sc1_redscored_a)] <- ""
# sc2_redscored_h[is.na(sc2_redscored_h)] <- ""
# sc2_redscored_a[is.na(sc2_redscored_a)] <- ""
# sc3_redscored_h[is.na(sc3_redscored_h)] <- ""
# sc3_redscored_a[is.na(sc3_redscored_a)] <- ""
# sp1_redscored_h[is.na(sp1_redscored_h)] <- ""
# sp1_redscored_a[is.na(sp1_redscored_a)] <- ""
# sp2_redscored_h[is.na(sp2_redscored_h)] <- ""
# sp2_redscored_a[is.na(sp2_redscored_a)] <- ""
# t1_redscored_h[is.na(t1_redscored_h)] <- ""
# t1_redscored_a[is.na(t1_redscored_a)] <- ""
#combine the matrices
#MLS
for(mls_rowhrs in 1:nrow(mls_redscored_h)) {
  for(mls_colhrs in 1:ncol(mls_redscored_h)) {

    # print(my_matrix[row, col])
    for(mls_rowars in 1:nrow(mls_redscored_a)) {
      for(mls_colars in 1:ncol(mls_redscored_a)) {
        ifelse(!mls_redscored_a[mls_rowars,mls_colars]=="",mls_redscored_h[mls_rowars,mls_colars] <- mls_redscored_a[mls_rowars,mls_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#ARG
for(arg_rowhrs in 1:nrow(arg_redscored_h)) {
  for(arg_colhrs in 1:ncol(arg_redscored_h)) {

    # print(my_matrix[row, col])
    for(arg_rowars in 1:nrow(arg_redscored_a)) {
      for(arg_colars in 1:ncol(arg_redscored_a)) {
        ifelse(!arg_redscored_a[arg_rowars,arg_colars]=="",arg_redscored_h[arg_rowars,arg_colars] <- arg_redscored_a[arg_rowars,arg_colars],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
# #D2
# for(d2_rowhrs in 1:nrow(d2_redscored_h)) {
#   for(d2_colhrs in 1:ncol(d2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(d2_rowars in 1:nrow(d2_redscored_a)) {
#       for(d2_colars in 1:ncol(d2_redscored_a)) {
#         ifelse(!d2_redscored_a[d2_rowars,d2_colars]=="",d2_redscored_h[d2_rowars,d2_colars] <- d2_redscored_a[d2_rowars,d2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E0
# for(e0_rowhrs in 1:nrow(e0_redscored_h)) {
#   for(e0_colhrs in 1:ncol(e0_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e0_rowars in 1:nrow(e0_redscored_a)) {
#       for(e0_colars in 1:ncol(e0_redscored_a)) {
#         ifelse(!e0_redscored_a[e0_rowars,e0_colars]=="",e0_redscored_h[e0_rowars,e0_colars] <- e0_redscored_a[e0_rowars,e0_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E1
# for(e1_rowhrs in 1:nrow(e1_redscored_h)) {
#   for(e1_colhrs in 1:ncol(e1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e1_rowars in 1:nrow(e1_redscored_a)) {
#       for(e1_colars in 1:ncol(e1_redscored_a)) {
#         ifelse(!e1_redscored_a[e1_rowars,e1_colars]=="",e1_redscored_h[e1_rowars,e1_colars] <- e1_redscored_a[e1_rowars,e1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E2
# for(e2_rowhrs in 1:nrow(e2_redscored_h)) {
#   for(e2_colhrs in 1:ncol(e2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e2_rowars in 1:nrow(e2_redscored_a)) {
#       for(e2_colars in 1:ncol(e2_redscored_a)) {
#         ifelse(!e2_redscored_a[e2_rowars,e2_colars]=="",e2_redscored_h[e2_rowars,e2_colars] <- e2_redscored_a[e2_rowars,e2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #E3
# for(e3_rowhrs in 1:nrow(e3_redscored_h)) {
#   for(e3_colhrs in 1:ncol(e3_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(e3_rowars in 1:nrow(e3_redscored_a)) {
#       for(e3_colars in 1:ncol(e3_redscored_a)) {
#         ifelse(!e3_redscored_a[e3_rowars,e3_colars]=="",e3_redscored_h[e3_rowars,e3_colars] <- e3_redscored_a[e3_rowars,e3_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #EC
# for(ec_rowhrs in 1:nrow(ec_redscored_h)) {
#   for(ec_colhrs in 1:ncol(ec_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(ec_rowars in 1:nrow(ec_redscored_a)) {
#       for(ec_colars in 1:ncol(ec_redscored_a)) {
#         ifelse(!ec_redscored_a[ec_rowars,ec_colars]=="",ec_redscored_h[ec_rowars,ec_colars] <- ec_redscored_a[ec_rowars,ec_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F1
# for(f1_rowhrs in 1:nrow(f1_redscored_h)) {
#   for(f1_colhrs in 1:ncol(f1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f1_rowars in 1:nrow(f1_redscored_a)) {
#       for(f1_colars in 1:ncol(f1_redscored_a)) {
#         ifelse(!f1_redscored_a[f1_rowars,f1_colars]=="",f1_redscored_h[f1_rowars,f1_colars] <- f1_redscored_a[f1_rowars,f1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #F2
# for(f2_rowhrs in 1:nrow(f2_redscored_h)) {
#   for(f2_colhrs in 1:ncol(f2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(f2_rowars in 1:nrow(f2_redscored_a)) {
#       for(f2_colars in 1:ncol(f2_redscored_a)) {
#         ifelse(!f2_redscored_a[f2_rowars,f2_colars]=="",f2_redscored_h[f2_rowars,f2_colars] <- f2_redscored_a[f2_rowars,f2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #G1
# for(g1_rowhrs in 1:nrow(g1_redscored_h)) {
#   for(g1_colhrs in 1:ncol(g1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(g1_rowars in 1:nrow(g1_redscored_a)) {
#       for(g1_colars in 1:ncol(g1_redscored_a)) {
#         ifelse(!g1_redscored_a[g1_rowars,g1_colars]=="",g1_redscored_h[g1_rowars,g1_colars] <- g1_redscored_a[g1_rowars,g1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I1
# for(i1_rowhrs in 1:nrow(i1_redscored_h)) {
#   for(i1_colhrs in 1:ncol(i1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i1_rowars in 1:nrow(i1_redscored_a)) {
#       for(i1_colars in 1:ncol(i1_redscored_a)) {
#         ifelse(!i1_redscored_a[i1_rowars,i1_colars]=="",i1_redscored_h[i1_rowars,i1_colars] <- i1_redscored_a[i1_rowars,i1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #I2
# for(i2_rowhrs in 1:nrow(i2_redscored_h)) {
#   for(i2_colhrs in 1:ncol(i2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(i2_rowars in 1:nrow(i2_redscored_a)) {
#       for(i2_colars in 1:ncol(i2_redscored_a)) {
#         ifelse(!i2_redscored_a[i2_rowars,i2_colars]=="",i2_redscored_h[i2_rowars,i2_colars] <- i2_redscored_a[i2_rowars,i2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #N1
# for(n1_rowhrs in 1:nrow(n1_redscored_h)) {
#   for(n1_colhrs in 1:ncol(n1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(n1_rowars in 1:nrow(n1_redscored_a)) {
#       for(n1_colars in 1:ncol(n1_redscored_a)) {
#         ifelse(!n1_redscored_a[n1_rowars,n1_colars]=="",n1_redscored_h[n1_rowars,n1_colars] <- n1_redscored_a[n1_rowars,n1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #P1
# for(p1_rowhrs in 1:nrow(p1_redscored_h)) {
#   for(p1_colhrs in 1:ncol(p1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(p1_rowars in 1:nrow(p1_redscored_a)) {
#       for(p1_colars in 1:ncol(p1_redscored_a)) {
#         ifelse(!p1_redscored_a[p1_rowars,p1_colars]=="",p1_redscored_h[p1_rowars,p1_colars] <- p1_redscored_a[p1_rowars,p1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC0
# for(sc0_rowhrs in 1:nrow(sc0_redscored_h)) {
#   for(sc0_colhrs in 1:ncol(sc0_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowars in 1:nrow(sc0_redscored_a)) {
#       for(sc0_colars in 1:ncol(sc0_redscored_a)) {
#         ifelse(!sc0_redscored_a[sc0_rowars,sc0_colars]=="",sc0_redscored_h[sc0_rowars,sc0_colars] <- sc0_redscored_a[sc0_rowars,sc0_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC1
# for(sc1_rowhrs in 1:nrow(sc1_redscored_h)) {
#   for(sc1_colhrs in 1:ncol(sc1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc1_rowars in 1:nrow(sc1_redscored_a)) {
#       for(sc1_colars in 1:ncol(sc1_redscored_a)) {
#         ifelse(!sc1_redscored_a[sc1_rowars,sc1_colars]=="",sc1_redscored_h[sc1_rowars,sc1_colars] <- sc1_redscored_a[sc1_rowars,sc1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC2
# for(sc2_rowhrs in 1:nrow(sc2_redscored_h)) {
#   for(sc2_colhrs in 1:ncol(sc2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc2_rowars in 1:nrow(sc2_redscored_a)) {
#       for(sc2_colars in 1:ncol(sc2_redscored_a)) {
#         ifelse(!sc2_redscored_a[sc2_rowars,sc2_colars]=="",sc2_redscored_h[sc2_rowars,sc2_colars] <- sc2_redscored_a[sc2_rowars,sc2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SC3
# for(sc3_rowhrs in 1:nrow(sc3_redscored_h)) {
#   for(sc3_colhrs in 1:ncol(sc3_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc3_rowars in 1:nrow(sc3_redscored_a)) {
#       for(sc3_colars in 1:ncol(sc3_redscored_a)) {
#         ifelse(!sc3_redscored_a[sc3_rowars,sc3_colars]=="",sc3_redscored_h[sc3_rowars,sc3_colars] <- sc3_redscored_a[sc3_rowars,sc3_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP1
# for(sp1_rowhrs in 1:nrow(sp1_redscored_h)) {
#   for(sp1_colhrs in 1:ncol(sp1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp1_rowars in 1:nrow(sp1_redscored_a)) {
#       for(sp1_colars in 1:ncol(sp1_redscored_a)) {
#         ifelse(!sp1_redscored_a[sp1_rowars,sp1_colars]=="",sp1_redscored_h[sp1_rowars,sp1_colars] <- sp1_redscored_a[sp1_rowars,sp1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #SP2
# for(sp2_rowhrs in 1:nrow(sp2_redscored_h)) {
#   for(sp2_colhrs in 1:ncol(sp2_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(sp2_rowars in 1:nrow(sp2_redscored_a)) {
#       for(sp2_colars in 1:ncol(sp2_redscored_a)) {
#         ifelse(!sp2_redscored_a[sp2_rowars,sp2_colars]=="",sp2_redscored_h[sp2_rowars,sp2_colars] <- sp2_redscored_a[sp2_rowars,sp2_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
# #T1
# for(t1_rowhrs in 1:nrow(t1_redscored_h)) {
#   for(t1_colhrs in 1:ncol(t1_redscored_h)) {
#
#     # print(my_matrix[row, col])
#     for(t1_rowars in 1:nrow(t1_redscored_a)) {
#       for(t1_colars in 1:ncol(t1_redscored_a)) {
#         ifelse(!t1_redscored_a[t1_rowars,t1_colars]=="",t1_redscored_h[t1_rowars,t1_colars] <- t1_redscored_a[t1_rowars,t1_colars],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }



# #write out the data to excel
# write.xlsx(mls_redscored_h,'RCmatrix.xlsx',sheetName = "MLS")
# write.xlsx(arg_redscored_h,'RCmatrix.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_redscored_h,'RCmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_redscored_h,'RCmatrix.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_redscored_h,'RCmatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_redscored_h,'RCmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_redscored_h,'RCmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_redscored_h,'RCmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_redscored_h,'RCmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_redscored_h,'RCmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_redscored_h,'RCmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_redscored_h,'RCmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_redscored_h,'RCmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_redscored_h,'RCmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_redscored_h,'RCmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_redscored_h,'RCmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_redscored_h,'RCmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_redscored_h,'RCmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_redscored_h,'RCmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_redscored_h,'RCmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_redscored_h,'RCmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_redscored_h,'RCmatrix.xlsx',sheetName = "T1", append = TRUE)



