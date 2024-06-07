library('xlsx')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")

##############################################################################################
mls_totalrounds <-  (length(mls_teams) - 1 )*2
mls_totalmatches <- (length(mls_teams)*(length(mls_teams) - 1))
mls_eachround <- ceiling(mls_totalmatches / mls_totalrounds)

mls_matchesplayed <-  nrow(MLS)

MLS_rounds <- MLS

if(mls_matchesplayed %% mls_eachround == 0)
{
  mls_currentround <- mls_matchesplayed / mls_eachround
  mls_matchday <- c()
  mls_matchday <- rep(1:mls_currentround, each = mls_eachround)
}else if(mls_matchesplayed %% mls_eachround != 0)

{

  mls_modulus <- mls_matchesplayed %% mls_eachround
  mls_currentround <- (mls_matchesplayed - mls_modulus) / mls_eachround
  mls_matchday <- c()
  mls_matchday_vec1 <- c()
  mls_matchday_vec2 <- c()
  mls_matchday_vec1 <- rep(1:mls_currentround, each = mls_eachround)
  mls_matchday_vec2[1:mls_modulus] <- c(mls_currentround + 1)
  mls_matchday <- append(mls_matchday_vec1,mls_matchday_vec2)
}
MLS_rounds <- cbind(MLS_rounds,mls_matchday)

#####################################################################################################
#####################################################################################################
#####################################################################################################
arg_totalrounds <-  (length(arg_teams) - 1 )*2
arg_totalmatches <- (length(arg_teams)*(length(arg_teams) - 1))
arg_eachround <- arg_totalmatches / arg_totalrounds

arg_matchesplayed <-  nrow(ARG)

ARG_rounds <- ARG

if(arg_matchesplayed %% arg_eachround == 0)
{
  arg_currentround <- arg_matchesplayed / arg_eachround
  arg_matchday <- c()
  arg_matchday <- rep(1:arg_currentround, each = arg_eachround)
}else if(arg_matchesplayed %% arg_eachround != 0)

{

  arg_modulus <- arg_matchesplayed %% arg_eachround
  arg_currentround <- (arg_matchesplayed - arg_modulus) / arg_eachround
  arg_matchday <- c()
  arg_matchday_vec1 <- c()
  arg_matchday_vec2 <- c()
  arg_matchday_vec1 <- rep(1:arg_currentround, each = arg_eachround)
  arg_matchday_vec2[1:arg_modulus] <- c(arg_currentround + 1)
  arg_matchday <- append(arg_matchday_vec1,arg_matchday_vec2)
}
ARG_rounds <- cbind(ARG_rounds,arg_matchday)
#####################################################################################################
##############################################################################################
# d2_totalrounds <-  (length(d2_teams) - 1 )*2
# d2_totalmatches <- (length(d2_teams)*(length(d2_teams) - 1))
# d2_eachround <- d2_totalmatches / d2_totalrounds
#
# d2_matchesplayed <-  nrow(D2)
#
# D2_rounds <- D2
#
# if(d2_matchesplayed %% d2_eachround == 0)
# {
#   d2_currentround <- d2_matchesplayed / d2_eachround
#   d2_matchday <- c()
#   d2_matchday <- rep(1:d2_currentround, each = d2_eachround)
# }else if(d2_matchesplayed %% d2_eachround != 0)
#
# {
#
#   d2_modulus <- d2_matchesplayed %% d2_eachround
#   d2_currentround <- (d2_matchesplayed - d2_modulus) / d2_eachround
#   d2_matchday <- c()
#   d2_matchday_vec1 <- c()
#   d2_matchday_vec2 <- c()
#   d2_matchday_vec1 <- rep(1:d2_currentround, each = d2_eachround)
#   d2_matchday_vec2[1:d2_modulus] <- c(d2_currentround + 1)
#   d2_matchday <- append(d2_matchday_vec1,d2_matchday_vec2)
# }
# D2_rounds <- cbind(D2_rounds,d2_matchday)
# #####################################################################################################
# ##############################################################################################
# e0_totalrounds <-  (length(e0_teams) - 1 )*2
# e0_totalmatches <- (length(e0_teams)*(length(e0_teams) - 1))
# e0_eachround <- e0_totalmatches / e0_totalrounds
#
# e0_matchesplayed <-  nrow(E0)
#
# E0_rounds <- E0
#
# if(e0_matchesplayed %% e0_eachround == 0)
# {
#   e0_currentround <- e0_matchesplayed / e0_eachround
#   e0_matchday <- c()
#   e0_matchday <- rep(1:e0_currentround, each = e0_eachround)
# }else if(e0_matchesplayed %% e0_eachround != 0)
#
# {
#
#   e0_modulus <- e0_matchesplayed %% e0_eachround
#   e0_currentround <- (e0_matchesplayed - e0_modulus) / e0_eachround
#   e0_matchday <- c()
#   e0_matchday_vec1 <- c()
#   e0_matchday_vec2 <- c()
#   e0_matchday_vec1 <- rep(1:e0_currentround, each = e0_eachround)
#   e0_matchday_vec2[1:e0_modulus] <- c(e0_currentround + 1)
#   e0_matchday <- append(e0_matchday_vec1,e0_matchday_vec2)
# }
# E0_rounds <- cbind(E0_rounds,e0_matchday)
# #####################################################################################################
# ##############################################################################################
# e1_totalrounds <-  (length(e1_teams) - 1 )*2
# e1_totalmatches <- (length(e1_teams)*(length(e1_teams) - 1))
# e1_eachround <- e1_totalmatches / e1_totalrounds
#
# e1_matchesplayed <-  nrow(E1)
#
# E1_rounds <- E1
#
# if(e1_matchesplayed %% e1_eachround == 0)
# {
#   e1_currentround <- e1_matchesplayed / e1_eachround
#   e1_matchday <- c()
#   e1_matchday <- rep(1:e1_currentround, each = e1_eachround)
# }else if(e1_matchesplayed %% e1_eachround != 0)
#
# {
#
#   e1_modulus <- e1_matchesplayed %% e1_eachround
#   e1_currentround <- (e1_matchesplayed - e1_modulus) / e1_eachround
#   e1_matchday <- c()
#   e1_matchday_vec1 <- c()
#   e1_matchday_vec2 <- c()
#   e1_matchday_vec1 <- rep(1:e1_currentround, each = e1_eachround)
#   e1_matchday_vec2[1:e1_modulus] <- c(e1_currentround + 1)
#   e1_matchday <- append(e1_matchday_vec1,e1_matchday_vec2)
# }
# E1_rounds <- cbind(E1_rounds,e1_matchday)
# #####################################################################################################
# ##############################################################################################
# e2_totalrounds <-  (length(e2_teams) - 1 )*2
# e2_totalmatches <- (length(e2_teams)*(length(e2_teams) - 1))
# e2_eachround <- e2_totalmatches / e2_totalrounds
#
# e2_matchesplayed <-  nrow(E2)
#
# E2_rounds <- E2
#
# if(e2_matchesplayed %% e2_eachround == 0)
# {
#   e2_currentround <- e2_matchesplayed / e2_eachround
#   e2_matchday <- c()
#   e2_matchday <- rep(1:e2_currentround, each = e2_eachround)
# }else if(e2_matchesplayed %% e2_eachround != 0)
#
# {
#
#   e2_modulus <- e2_matchesplayed %% e2_eachround
#   e2_currentround <- (e2_matchesplayed - e2_modulus) / e2_eachround
#   e2_matchday <- c()
#   e2_matchday_vec1 <- c()
#   e2_matchday_vec2 <- c()
#   e2_matchday_vec1 <- rep(1:e2_currentround, each = e2_eachround)
#   e2_matchday_vec2[1:e2_modulus] <- c(e2_currentround + 1)
#   e2_matchday <- append(e2_matchday_vec1,e2_matchday_vec2)
# }
# E2_rounds <- cbind(E2_rounds,e2_matchday)
# #####################################################################################################
# ##############################################################################################
# e3_totalrounds <-  (length(e3_teams) - 1 )*2
# e3_totalmatches <- (length(e3_teams)*(length(e3_teams) - 1))
# e3_eachround <- e3_totalmatches / e3_totalrounds
#
# e3_matchesplayed <-  nrow(E3)
#
# E3_rounds <- E3
#
# if(e3_matchesplayed %% e3_eachround == 0)
# {
#   e3_currentround <- e3_matchesplayed / e3_eachround
#   e3_matchday <- c()
#   e3_matchday <- rep(1:e3_currentround, each = e3_eachround)
# }else if(e3_matchesplayed %% e3_eachround != 0)
#
# {
#
#   e3_modulus <- e3_matchesplayed %% e3_eachround
#   e3_currentround <- (e3_matchesplayed - e3_modulus) / e3_eachround
#   e3_matchday <- c()
#   e3_matchday_vec1 <- c()
#   e3_matchday_vec2 <- c()
#   e3_matchday_vec1 <- rep(1:e3_currentround, each = e3_eachround)
#   e3_matchday_vec2[1:e3_modulus] <- c(e3_currentround + 1)
#   e3_matchday <- append(e3_matchday_vec1,e3_matchday_vec2)
# }
# E3_rounds <- cbind(E3_rounds,e3_matchday)
# #####################################################################################################
# ##############################################################################################
# ec_totalrounds <-  (length(ec_teams) - 1 )*2
# ec_totalmatches <- (length(ec_teams)*(length(ec_teams) - 1))
# ec_eachround <- ceiling(ec_totalmatches / ec_totalrounds)
#
# ec_matchesplayed <-  nrow(EC)
#
# EC_rounds <- EC
#
# if(ec_matchesplayed %% ec_eachround == 0)
# {
#   ec_currentround <- ec_matchesplayed / ec_eachround
#   ec_matchday <- c()
#   ec_matchday <- rep(1:ec_currentround, each = ec_eachround)
# }else if(ec_matchesplayed %% ec_eachround != 0)
#
# {
#
#   ec_modulus <- ec_matchesplayed %% ec_eachround
#   ec_currentround <- (ec_matchesplayed - ec_modulus) / ec_eachround
#   ec_matchday <- c()
#   ec_matchday_vec1 <- c()
#   ec_matchday_vec2 <- c()
#   ec_matchday_vec1 <- rep(1:ec_currentround, each = ec_eachround)
#   ec_matchday_vec2[1:ec_modulus] <- c(ec_currentround + 1)
#   ec_matchday <- append(ec_matchday_vec1,ec_matchday_vec2)
# }
# EC_rounds <- cbind(EC_rounds,ec_matchday)
# #####################################################################################################
# ####################################################################################################
# f1_totalrounds <-  (length(f1_teams) - 1 )*2
# f1_totalmatches <- (length(f1_teams)*(length(f1_teams) - 1))
# f1_eachround <- f1_totalmatches / f1_totalrounds
#
# f1_matchesplayed <-  nrow(F1)
#
# F1_rounds <- F1
#
# if(f1_matchesplayed %% f1_eachround == 0)
# {
#   f1_currentround <- f1_matchesplayed / f1_eachround
#   f1_matchday <- c()
#   f1_matchday <- rep(1:f1_currentround, each = f1_eachround)
# }else if(f1_matchesplayed %% f1_eachround != 0)
#
# {
#
#   f1_modulus <- f1_matchesplayed %% f1_eachround
#   f1_currentround <- (f1_matchesplayed - f1_modulus) / f1_eachround
#   f1_matchday <- c()
#   f1_matchday_vec1 <- c()
#   f1_matchday_vec2 <- c()
#   f1_matchday_vec1 <- rep(1:f1_currentround, each = f1_eachround)
#   f1_matchday_vec2[1:f1_modulus] <- c(f1_currentround + 1)
#   f1_matchday <- append(f1_matchday_vec1,f1_matchday_vec2)
# }
# F1_rounds <- cbind(F1_rounds,f1_matchday)
# #####################################################################################################
# ##############################################################################################
# f2_totalrounds <-  (length(f2_teams) - 1 )*2
# f2_totalmatches <- (length(f2_teams)*(length(f2_teams) - 1))
# f2_eachround <- f2_totalmatches / f2_totalrounds
#
# f2_matchesplayed <-  nrow(F2)
#
# F2_rounds <- F2
#
# if(f2_matchesplayed %% f2_eachround == 0)
# {
#   f2_currentround <- f2_matchesplayed / f2_eachround
#   f2_matchday <- c()
#   f2_matchday <- rep(1:f2_currentround, each = f2_eachround)
# }else if(f2_matchesplayed %% f2_eachround != 0)
#
# {
#
#   f2_modulus <- f2_matchesplayed %% f2_eachround
#   f2_currentround <- (f2_matchesplayed - f2_modulus) / f2_eachround
#   f2_matchday <- c()
#   f2_matchday_vec1 <- c()
#   f2_matchday_vec2 <- c()
#   f2_matchday_vec1 <- rep(1:f2_currentround, each = f2_eachround)
#   f2_matchday_vec2[1:f2_modulus] <- c(f2_currentround + 1)
#   f2_matchday <- append(f2_matchday_vec1,f2_matchday_vec2)
# }
# F2_rounds <- cbind(F2_rounds,f2_matchday)
# #####################################################################################################
# ##############################################################################################
# g1_totalrounds <-  (length(g1_teams) - 1 )*2
# g1_totalmatches <- (length(g1_teams)*(length(g1_teams) - 1))
# g1_eachround <- g1_totalmatches / g1_totalrounds
#
# g1_matchesplayed <-  nrow(G1)
#
# G1_rounds <- G1
#
# if(g1_matchesplayed %% g1_eachround == 0)
# {
#   g1_currentround <- g1_matchesplayed / g1_eachround
#   g1_matchday <- c()
#   g1_matchday <- rep(1:g1_currentround, each = g1_eachround)
# }else if(g1_matchesplayed %% g1_eachround != 0)
#
# {
#
#   g1_modulus <- g1_matchesplayed %% g1_eachround
#   g1_currentround <- (g1_matchesplayed - g1_modulus) / g1_eachround
#   g1_matchday <- c()
#   g1_matchday_vec1 <- c()
#   g1_matchday_vec2 <- c()
#   g1_matchday_vec1 <- rep(1:g1_currentround, each = g1_eachround)
#   g1_matchday_vec2[1:g1_modulus] <- c(g1_currentround + 1)
#   g1_matchday <- append(g1_matchday_vec1,g1_matchday_vec2)
# }
# G1_rounds <- cbind(G1_rounds,g1_matchday)
# #####################################################################################################
# ##############################################################################################
# i1_totalrounds <-  (length(i1_teams) - 1 )*2
# i1_totalmatches <- (length(i1_teams)*(length(i1_teams) - 1))
# i1_eachround <- i1_totalmatches / i1_totalrounds
#
# i1_matchesplayed <-  nrow(I1)
#
# I1_rounds <- I1
#
# if(i1_matchesplayed %% i1_eachround == 0)
# {
#   i1_currentround <- i1_matchesplayed / i1_eachround
#   i1_matchday <- c()
#   i1_matchday <- rep(1:i1_currentround, each = i1_eachround)
# }else if(i1_matchesplayed %% i1_eachround != 0)
#
# {
#
#   i1_modulus <- i1_matchesplayed %% i1_eachround
#   i1_currentround <- (i1_matchesplayed - i1_modulus) / i1_eachround
#   i1_matchday <- c()
#   i1_matchday_vec1 <- c()
#   i1_matchday_vec2 <- c()
#   i1_matchday_vec1 <- rep(1:i1_currentround, each = i1_eachround)
#   i1_matchday_vec2[1:i1_modulus] <- c(i1_currentround + 1)
#   i1_matchday <- append(i1_matchday_vec1,i1_matchday_vec2)
# }
# I1_rounds <- cbind(I1_rounds,i1_matchday)
# #####################################################################################################
# ##############################################################################################
# i2_totalrounds <-  (length(i2_teams) - 1 )*2
# i2_totalmatches <- (length(i2_teams)*(length(i2_teams) - 1))
# i2_eachround <- i2_totalmatches / i2_totalrounds
#
# i2_matchesplayed <-  nrow(I2)
#
# I2_rounds <- I2
#
# if(i2_matchesplayed %% i2_eachround == 0)
# {
#   i2_currentround <- i2_matchesplayed / i2_eachround
#   i2_matchday <- c()
#   i2_matchday <- rep(1:i2_currentround, each = i2_eachround)
# }else if(i2_matchesplayed %% i2_eachround != 0)
#
# {
#
#   i2_modulus <- i2_matchesplayed %% i2_eachround
#   i2_currentround <- (i2_matchesplayed - i2_modulus) / i2_eachround
#   i2_matchday <- c()
#   i2_matchday_vec1 <- c()
#   i2_matchday_vec2 <- c()
#   i2_matchday_vec1 <- rep(1:i2_currentround, each = i2_eachround)
#   i2_matchday_vec2[1:i2_modulus] <- c(i2_currentround + 1)
#   i2_matchday <- append(i2_matchday_vec1,i2_matchday_vec2)
# }
# I2_rounds <- cbind(I2_rounds,i2_matchday)
# #####################################################################################################
# ##############################################################################################
# n1_totalrounds <-  (length(n1_teams) - 1 )*2
# n1_totalmatches <- (length(n1_teams)*(length(n1_teams) - 1))
# n1_eachround <- n1_totalmatches / n1_totalrounds
#
# n1_matchesplayed <-  nrow(N1)
#
# N1_rounds <- N1
#
# if(n1_matchesplayed %% n1_eachround == 0)
# {
#   n1_currentround <- n1_matchesplayed / n1_eachround
#   n1_matchday <- c()
#   n1_matchday <- rep(1:n1_currentround, each = n1_eachround)
# }else if(n1_matchesplayed %% n1_eachround != 0)
#
# {
#
#   n1_modulus <- n1_matchesplayed %% n1_eachround
#   n1_currentround <- (n1_matchesplayed - n1_modulus) / n1_eachround
#   n1_matchday <- c()
#   n1_matchday_vec1 <- c()
#   n1_matchday_vec2 <- c()
#   n1_matchday_vec1 <- rep(1:n1_currentround, each = n1_eachround)
#   n1_matchday_vec2[1:n1_modulus] <- c(n1_currentround + 1)
#   n1_matchday <- append(n1_matchday_vec1,n1_matchday_vec2)
# }
# N1_rounds <- cbind(N1_rounds,n1_matchday)
# #####################################################################################################
# ##############################################################################################
# p1_totalrounds <-  (length(p1_teams) - 1 )*2
# p1_totalmatches <- (length(p1_teams)*(length(p1_teams) - 1))
# p1_eachround <- p1_totalmatches / p1_totalrounds
#
# p1_matchesplayed <-  nrow(P1)
#
# P1_rounds <- P1
#
# if(p1_matchesplayed %% p1_eachround == 0)
# {
#   p1_currentround <- p1_matchesplayed / p1_eachround
#   p1_matchday <- c()
#   p1_matchday <- rep(1:p1_currentround, each = p1_eachround)
# }else if(p1_matchesplayed %% p1_eachround != 0)
#
# {
#
#   p1_modulus <- p1_matchesplayed %% p1_eachround
#   p1_currentround <- (p1_matchesplayed - p1_modulus) / p1_eachround
#   p1_matchday <- c()
#   p1_matchday_vec1 <- c()
#   p1_matchday_vec2 <- c()
#   p1_matchday_vec1 <- rep(1:p1_currentround, each = p1_eachround)
#   p1_matchday_vec2[1:p1_modulus] <- c(p1_currentround + 1)
#   p1_matchday <- append(p1_matchday_vec1,p1_matchday_vec2)
# }
# P1_rounds <- cbind(P1_rounds,p1_matchday)
# #####################################################################################################
# ##############################################################################################
# sp1_totalrounds <-  (length(sp1_teams) - 1 )*2
# sp1_totalmatches <- (length(sp1_teams)*(length(sp1_teams) - 1))
# sp1_eachround <- sp1_totalmatches / sp1_totalrounds
#
# sp1_matchesplayed <-  nrow(SP1)
#
# SP1_rounds <- SP1
#
# if(sp1_matchesplayed %% sp1_eachround == 0)
# {
#   sp1_currentround <- sp1_matchesplayed / sp1_eachround
#   sp1_matchday <- c()
#   sp1_matchday <- rep(1:sp1_currentround, each = sp1_eachround)
# }else if(sp1_matchesplayed %% sp1_eachround != 0)
#
# {
#
#   sp1_modulus <- sp1_matchesplayed %% sp1_eachround
#   sp1_currentround <- (sp1_matchesplayed - sp1_modulus) / sp1_eachround
#   sp1_matchday <- c()
#   sp1_matchday_vec1 <- c()
#   sp1_matchday_vec2 <- c()
#   sp1_matchday_vec1 <- rep(1:sp1_currentround, each = sp1_eachround)
#   sp1_matchday_vec2[1:sp1_modulus] <- c(sp1_currentround + 1)
#   sp1_matchday <- append(sp1_matchday_vec1,sp1_matchday_vec2)
# }
# SP1_rounds <- cbind(SP1_rounds,sp1_matchday)
# #####################################################################################################
# ##############################################################################################
# sp2_totalrounds <-  (length(sp2_teams) - 1 )*2
# sp2_totalmatches <- (length(sp2_teams)*(length(sp2_teams) - 1))
# sp2_eachround <- sp2_totalmatches / sp2_totalrounds
#
# sp2_matchesplayed <-  nrow(SP2)
#
# SP2_rounds <- SP2
#
# if(sp2_matchesplayed %% sp2_eachround == 0)
# {
#   sp2_currentround <- sp2_matchesplayed / sp2_eachround
#   sp2_matchday <- c()
#   sp2_matchday <- rep(1:sp2_currentround, each = sp2_eachround)
# }else if(sp2_matchesplayed %% sp2_eachround != 0)
#
# {
#
#   sp2_modulus <- sp2_matchesplayed %% sp2_eachround
#   sp2_currentround <- (sp2_matchesplayed - sp2_modulus) / sp2_eachround
#   sp2_matchday <- c()
#   sp2_matchday_vec1 <- c()
#   sp2_matchday_vec2 <- c()
#   sp2_matchday_vec1 <- rep(1:sp2_currentround, each = sp2_eachround)
#   sp2_matchday_vec2[1:sp2_modulus] <- c(sp2_currentround + 1)
#   sp2_matchday <- append(sp2_matchday_vec1,sp2_matchday_vec2)
# }
# SP2_rounds <- cbind(SP2_rounds,sp2_matchday)
# #####################################################################################################
# ##############################################################################################
# sc0_totalrounds <-  (length(sc0_teams) - 1 )*2
# sc0_totalmatches <- (length(sc0_teams)*(length(sc0_teams) - 1))
# sc0_eachround <- sc0_totalmatches / sc0_totalrounds
#
# sc0_matchesplayed <-  nrow(SC0)
#
# SC0_rounds <- SC0
#
# if(sc0_matchesplayed %% sc0_eachround == 0)
# {
#   sc0_currentround <- sc0_matchesplayed / sc0_eachround
#   sc0_matchday <- c()
#   sc0_matchday <- rep(1:sc0_currentround, each = sc0_eachround)
# }else if(sc0_matchesplayed %% sc0_eachround != 0)
#
# {
#
#   sc0_modulus <- sc0_matchesplayed %% sc0_eachround
#   sc0_currentround <- (sc0_matchesplayed - sc0_modulus) / sc0_eachround
#   sc0_matchday <- c()
#   sc0_matchday_vec1 <- c()
#   sc0_matchday_vec2 <- c()
#   sc0_matchday_vec1 <- rep(1:sc0_currentround, each = sc0_eachround)
#   sc0_matchday_vec2[1:sc0_modulus] <- c(sc0_currentround + 1)
#   sc0_matchday <- append(sc0_matchday_vec1,sc0_matchday_vec2)
# }
# SC0_rounds <- cbind(SC0_rounds,sc0_matchday)
# #####################################################################################################
# ##############################################################################################
# sc1_totalrounds <-  (length(sc1_teams) - 1 )*2
# sc1_totalmatches <- (length(sc1_teams)*(length(sc1_teams) - 1))
# sc1_eachround <- sc1_totalmatches / sc1_totalrounds
#
# sc1_matchesplayed <-  nrow(SC1)
#
# SC1_rounds <- SC1
#
# if(sc1_matchesplayed %% sc1_eachround == 0)
# {
#   sc1_currentround <- sc1_matchesplayed / sc1_eachround
#   sc1_matchday <- c()
#   sc1_matchday <- rep(1:sc1_currentround, each = sc1_eachround)
# }else if(sc1_matchesplayed %% sc1_eachround != 0)
#
# {
#
#   sc1_modulus <- sc1_matchesplayed %% sc1_eachround
#   sc1_currentround <- (sc1_matchesplayed - sc1_modulus) / sc1_eachround
#   sc1_matchday <- c()
#   sc1_matchday_vec1 <- c()
#   sc1_matchday_vec2 <- c()
#   sc1_matchday_vec1 <- rep(1:sc1_currentround, each = sc1_eachround)
#   sc1_matchday_vec2[1:sc1_modulus] <- c(sc1_currentround + 1)
#   sc1_matchday <- append(sc1_matchday_vec1,sc1_matchday_vec2)
# }
# SC1_rounds <- cbind(SC1_rounds,sc1_matchday)
# #####################################################################################################
# ##############################################################################################
# sc2_totalrounds <-  (length(sc2_teams) - 1 )*2
# sc2_totalmatches <- (length(sc2_teams)*(length(sc2_teams) - 1))
# sc2_eachround <- sc2_totalmatches / sc2_totalrounds
#
# sc2_matchesplayed <-  nrow(SC2)
#
# SC2_rounds <- SC2
#
# if(sc2_matchesplayed %% sc2_eachround == 0)
# {
#   sc2_currentround <- sc2_matchesplayed / sc2_eachround
#   sc2_matchday <- c()
#   sc2_matchday <- rep(1:sc2_currentround, each = sc2_eachround)
# }else if(sc2_matchesplayed %% sc2_eachround != 0)
#
# {
#
#   sc2_modulus <- sc2_matchesplayed %% sc2_eachround
#   sc2_currentround <- (sc2_matchesplayed - sc2_modulus) / sc2_eachround
#   sc2_matchday <- c()
#   sc2_matchday_vec1 <- c()
#   sc2_matchday_vec2 <- c()
#   sc2_matchday_vec1 <- rep(1:sc2_currentround, each = sc2_eachround)
#   sc2_matchday_vec2[1:sc2_modulus] <- c(sc2_currentround + 1)
#   sc2_matchday <- append(sc2_matchday_vec1,sc2_matchday_vec2)
# }
# SC2_rounds <- cbind(SC2_rounds,sc2_matchday)
# #####################################################################################################
# ##############################################################################################
# sc3_totalrounds <-  (length(sc3_teams) - 1 )*2
# sc3_totalmatches <- (length(sc3_teams)*(length(sc3_teams) - 1))
# sc3_eachround <- sc3_totalmatches / sc3_totalrounds
#
# sc3_matchesplayed <-  nrow(SC3)
#
# SC3_rounds <- SC3
#
# if(sc3_matchesplayed %% sc3_eachround == 0)
# {
#   sc3_currentround <- sc3_matchesplayed / sc3_eachround
#   sc3_matchday <- c()
#   sc3_matchday <- rep(1:sc3_currentround, each = sc3_eachround)
# }else if(sc3_matchesplayed %% sc3_eachround != 0)
#
# {
#
#   sc3_modulus <- sc3_matchesplayed %% sc3_eachround
#   sc3_currentround <- (sc3_matchesplayed - sc3_modulus) / sc3_eachround
#   sc3_matchday <- c()
#   sc3_matchday_vec1 <- c()
#   sc3_matchday_vec2 <- c()
#   sc3_matchday_vec1 <- rep(1:sc3_currentround, each = sc3_eachround)
#   sc3_matchday_vec2[1:sc3_modulus] <- c(sc3_currentround + 1)
#   sc3_matchday <- append(sc3_matchday_vec1,sc3_matchday_vec2)
# }
# SC3_rounds <- cbind(SC3_rounds,sc3_matchday)
# #####################################################################################################
# ##############################################################################################
# t1_totalrounds <-  (length(t1_teams) - 1 )*2
# t1_totalmatches <- (length(t1_teams)*(length(t1_teams) - 1))
# t1_eachround <- ceiling(t1_totalmatches / t1_totalrounds)
#
# t1_matchesplayed <-  nrow(T1)
#
# T1_rounds <- T1
#
# if(t1_matchesplayed %% t1_eachround == 0)
# {
#   t1_currentround <- t1_matchesplayed / t1_eachround
#   t1_matchday <- c()
#   t1_matchday <- rep(1:t1_currentround, each = t1_eachround)
# }else if(t1_matchesplayed %% t1_eachround != 0)
#
# {
#
#   t1_modulus <- t1_matchesplayed %% t1_eachround
#   t1_currentround <- (t1_matchesplayed - t1_modulus) / t1_eachround
#   t1_matchday <- c()
#   t1_matchday_vec1 <- c()
#   t1_matchday_vec2 <- c()
#   t1_matchday_vec1 <- rep(1:t1_currentround, each = t1_eachround)
#   t1_matchday_vec2[1:t1_modulus] <- c(t1_currentround + 1)
#   t1_matchday <- append(t1_matchday_vec1,t1_matchday_vec2)
# }
# T1_rounds <- cbind(T1_rounds,t1_matchday)
#####################################################################################################
#print for future sue
#############
colnames(MLS_rounds)
MLS_roundsclone <- MLS_rounds
colnames(MLS_roundsclone)[32] <- "all_matchday"
ARG_roundsclone <- ARG_rounds
colnames(ARG_roundsclone)[32] <- "all_matchday"
# D2_roundsclone <- D2_rounds
# colnames(D2_roundsclone)[32] <- "all_matchday"
# E0_roundsclone <- E0_rounds
# colnames(E0_roundsclone)[32] <- "all_matchday"
# E1_roundsclone <- E1_rounds
# colnames(E1_roundsclone)[32] <- "all_matchday"
# E2_roundsclone <- E2_rounds
# colnames(E2_roundsclone)[32] <- "all_matchday"
# E3_roundsclone <- E3_rounds
# colnames(E3_roundsclone)[32] <- "all_matchday"
# EC_roundsclone <- EC_rounds
# colnames(EC_roundsclone)[32] <- "all_matchday"
# F1_roundsclone <- F1_rounds
# colnames(F1_roundsclone)[32] <- "all_matchday"
# F2_roundsclone <- F2_rounds
# colnames(F2_roundsclone)[32] <- "all_matchday"
# G1_roundsclone <- G1_rounds
# colnames(G1_roundsclone)[32] <- "all_matchday"
# I1_roundsclone <- I1_rounds
# colnames(I1_roundsclone)[32] <- "all_matchday"
# I2_roundsclone <- I2_rounds
# colnames(I2_roundsclone)[32] <- "all_matchday"
# N1_roundsclone <- N1_rounds
# colnames(N1_roundsclone)[32] <- "all_matchday"
# P1_roundsclone <- P1_rounds
# colnames(P1_roundsclone)[32] <- "all_matchday"
# SC0_roundsclone <- SC0_rounds
# colnames(SC0_roundsclone)[32] <- "all_matchday"
# SC1_roundsclone <- SC1_rounds
# colnames(SC1_roundsclone)[32] <- "all_matchday"
# SC2_roundsclone <- SC2_rounds
# colnames(SC2_roundsclone)[32] <- "all_matchday"
# SC3_roundsclone <- SC3_rounds
# colnames(SC3_roundsclone)[32] <- "all_matchday"
# SP1_roundsclone <- SP1_rounds
# colnames(SP1_roundsclone)[32] <- "all_matchday"
# SP2_roundsclone <- SP2_rounds
# colnames(SP2_roundsclone)[32] <- "all_matchday"
# T1_roundsclone <- T1_rounds
# colnames(T1_roundsclone)[32] <- "all_matchday"

allteamsnewleagues2024SOTMatchday <- rbind(MLS_roundsclone,ARG_roundsclone)
unlink("allteamsnewleagues2024SOTMatchday.csv")
write.csv(allteamsnewleagues2024SOTMatchday,"allteamsnewleagues2024SOTMatchday.csv")





























