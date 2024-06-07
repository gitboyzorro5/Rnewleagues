#create last total games
mls_last_n_games <- mls_games_played[1]
arg_last_n_games <- arg_games_played[1]
# d2_last_n_games <- d2_games_played[1]
# e0_last_n_games <- e0_games_played[1]
# e1_last_n_games <- e1_games_played[1]
# e2_last_n_games <- e2_games_played[1]
# e3_last_n_games <- e3_games_played[1]
# ec_last_n_games <- ec_games_played[1]
# f1_last_n_games <- f1_games_played[1]
# f2_last_n_games <- f2_games_played[1]
# g1_last_n_games <- g1_games_played[1]
# i1_last_n_games <- i1_games_played[1]
# i2_last_n_games <- i2_games_played[1]
# n1_last_n_games <- n1_games_played[1]
# p1_last_n_games <- p1_games_played[1]
# sc0_last_n_games <- sc0_games_played[1]
# sc1_last_n_games <- sc1_games_played[1]
# sc2_last_n_games <- sc2_games_played[1]
# sc3_last_n_games <- sc3_games_played[1]
# sp1_last_n_games <- sp1_games_played[1]
# sp2_last_n_games <- sp2_games_played[1]
# t1_last_n_games <- t1_games_played[1]
# ########################################
#
# ###################################################################################
# b1_krounds <- tail(unique(B1_rounds$b1_matchday),1)
# b1_noofcurrentmatchdaygames <- nrow(B1_rounds[B1_rounds$b1_matchday == b1_krounds,])
# ifelse(b1_noofcurrentmatchdaygames == b1_eachround,b1_krounds <- tail(unique(B1_rounds$b1_matchday),1),b1_krounds <- tail(unique(B1_rounds$b1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# d1_krounds <- tail(unique(D1_rounds$d1_matchday),1)
# d1_noofcurrentmatchdaygames <- nrow(D1_rounds[D1_rounds$d1_matchday == d1_krounds,])
# ifelse(d1_noofcurrentmatchdaygames == d1_eachround,d1_krounds <- tail(unique(D1_rounds$d1_matchday),1),d1_krounds <- tail(unique(D1_rounds$d1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# d2_krounds <- tail(unique(D2_rounds$d2_matchday),1)
# d2_noofcurrentmatchdaygames <- nrow(D2_rounds[D2_rounds$d2_matchday == d2_krounds,])
# ifelse(d2_noofcurrentmatchdaygames == d2_eachround,d2_krounds <- tail(unique(D2_rounds$d2_matchday),1),d2_krounds <- tail(unique(D2_rounds$d2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# e0_krounds <- tail(unique(E0_rounds$e0_matchday),1)
# e0_noofcurrentmatchdaygames <- nrow(E0_rounds[E0_rounds$e0_matchday == e0_krounds,])
# ifelse(e0_noofcurrentmatchdaygames == e0_eachround,e0_krounds <- tail(unique(E0_rounds$e0_matchday),1),e0_krounds <- tail(unique(E0_rounds$e0_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# e1_krounds <- tail(unique(E1_rounds$e1_matchday),1)
# e1_noofcurrentmatchdaygames <- nrow(E1_rounds[E1_rounds$e1_matchday == e1_krounds,])
# ifelse(e1_noofcurrentmatchdaygames == e1_eachround,e1_krounds <- tail(unique(E1_rounds$e1_matchday),1),e1_krounds <- tail(unique(E1_rounds$e1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# e2_krounds <- tail(unique(E2_rounds$e2_matchday),1)
# e2_noofcurrentmatchdaygames <- nrow(E2_rounds[E2_rounds$e2_matchday == e2_krounds,])
# ifelse(e2_noofcurrentmatchdaygames == e2_eachround,e2_krounds <- tail(unique(E2_rounds$e2_matchday),1),e2_krounds <- tail(unique(E2_rounds$e2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# e3_krounds <- tail(unique(E3_rounds$e3_matchday),1)
# e3_noofcurrentmatchdaygames <- nrow(E3_rounds[E3_rounds$e3_matchday == e3_krounds,])
# ifelse(e3_noofcurrentmatchdaygames == e3_eachround,e3_krounds <- tail(unique(E3_rounds$e3_matchday),1),e3_krounds <- tail(unique(E3_rounds$e3_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# ec_krounds <- tail(unique(EC_rounds$ec_matchday),1)
# ec_noofcurrentmatchdaygames <- nrow(EC_rounds[EC_rounds$ec_matchday == ec_krounds,])
# ifelse(ec_noofcurrentmatchdaygames == ec_eachround,ec_krounds <- tail(unique(EC_rounds$ec_matchday),1),ec_krounds <- tail(unique(EC_rounds$ec_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# f1_krounds <- tail(unique(F1_rounds$f1_matchday),1)
# f1_noofcurrentmatchdaygames <- nrow(F1_rounds[F1_rounds$f1_matchday == f1_krounds,])
# ifelse(f1_noofcurrentmatchdaygames == f1_eachround,f1_krounds <- tail(unique(F1_rounds$f1_matchday),1),f1_krounds <- tail(unique(F1_rounds$f1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# f2_krounds <- tail(unique(F2_rounds$f2_matchday),1)
# f2_noofcurrentmatchdaygames <- nrow(F2_rounds[F2_rounds$f2_matchday == f2_krounds,])
# ifelse(f2_noofcurrentmatchdaygames == f2_eachround,f2_krounds <- tail(unique(F2_rounds$f2_matchday),1),f2_krounds <- tail(unique(F2_rounds$f2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# g1_krounds <- tail(unique(G1_rounds$g1_matchday),1)
# g1_noofcurrentmatchdaygames <- nrow(G1_rounds[G1_rounds$g1_matchday == g1_krounds,])
# ifelse(g1_noofcurrentmatchdaygames == g1_eachround,g1_krounds <- tail(unique(G1_rounds$g1_matchday),1),g1_krounds <- tail(unique(G1_rounds$g1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# i1_krounds <- tail(unique(I1_rounds$i1_matchday),1)
# i1_noofcurrentmatchdaygames <- nrow(I1_rounds[I1_rounds$i1_matchday == i1_krounds,])
# ifelse(i1_noofcurrentmatchdaygames == i1_eachround,i1_krounds <- tail(unique(I1_rounds$i1_matchday),1),i1_krounds <- tail(unique(I1_rounds$i1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# i2_krounds <- tail(unique(I2_rounds$i2_matchday),1)
# i2_noofcurrentmatchdaygames <- nrow(I2_rounds[I2_rounds$i2_matchday == i2_krounds,])
# ifelse(i2_noofcurrentmatchdaygames == i2_eachround,i2_krounds <- tail(unique(I2_rounds$i2_matchday),1),i2_krounds <- tail(unique(I2_rounds$i2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# n1_krounds <- tail(unique(N1_rounds$n1_matchday),1)
# n1_noofcurrentmatchdaygames <- nrow(N1_rounds[N1_rounds$n1_matchday == n1_krounds,])
# ifelse(n1_noofcurrentmatchdaygames == n1_eachround,n1_krounds <- tail(unique(N1_rounds$n1_matchday),1),n1_krounds <- tail(unique(N1_rounds$n1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# p1_krounds <- tail(unique(P1_rounds$p1_matchday),1)
# p1_noofcurrentmatchdaygames <- nrow(P1_rounds[P1_rounds$p1_matchday == p1_krounds,])
# ifelse(p1_noofcurrentmatchdaygames == p1_eachround,p1_krounds <- tail(unique(P1_rounds$p1_matchday),1),p1_krounds <- tail(unique(P1_rounds$p1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1)
# sp1_noofcurrentmatchdaygames <- nrow(SP1_rounds[SP1_rounds$sp1_matchday == sp1_krounds,])
# ifelse(sp1_noofcurrentmatchdaygames == sp1_eachround,sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1),sp1_krounds <- tail(unique(SP1_rounds$sp1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1)
# sp2_noofcurrentmatchdaygames <- nrow(SP2_rounds[SP2_rounds$sp2_matchday == sp2_krounds,])
# ifelse(sp2_noofcurrentmatchdaygames == sp2_eachround,sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1),sp2_krounds <- tail(unique(SP2_rounds$sp2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1)
# sc0_noofcurrentmatchdaygames <- nrow(SC0_rounds[SC0_rounds$sc0_matchday == sc0_krounds,])
# ifelse(sc0_noofcurrentmatchdaygames == sc0_eachround,sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1),sc0_krounds <- tail(unique(SC0_rounds$sc0_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1)
# sc1_noofcurrentmatchdaygames <- nrow(SC1_rounds[SC1_rounds$sc1_matchday == sc1_krounds,])
# ifelse(sc1_noofcurrentmatchdaygames == sc1_eachround,sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1),sc1_krounds <- tail(unique(SC1_rounds$sc1_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1)
# sc2_noofcurrentmatchdaygames <- nrow(SC2_rounds[SC2_rounds$sc2_matchday == sc2_krounds,])
# ifelse(sc2_noofcurrentmatchdaygames == sc2_eachround,sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1),sc2_krounds <- tail(unique(SC2_rounds$sc2_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1)
# sc3_noofcurrentmatchdaygames <- nrow(SC3_rounds[SC3_rounds$sc3_matchday == sc3_krounds,])
# ifelse(sc3_noofcurrentmatchdaygames == sc3_eachround,sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1),sc3_krounds <- tail(unique(SC3_rounds$sc3_matchday),1) - 1 )
# ###################################################################################
# ###################################################################################
# t1_krounds <- tail(unique(T1_rounds$t1_matchday),1)
# t1_noofcurrentmatchdaygames <- nrow(T1_rounds[T1_rounds$t1_matchday == t1_krounds,])
# ifelse(t1_noofcurrentmatchdaygames == t1_eachround,t1_krounds <- tail(unique(T1_rounds$t1_matchday),1),t1_krounds <- tail(unique(T1_rounds$t1_matchday),1) - 1 )
# ###################################################################################














