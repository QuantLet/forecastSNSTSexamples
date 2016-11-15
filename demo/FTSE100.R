
## Prepare the data for the analysis: 
## determine squared and centered returns for the analysis
FTSE100_open <- FTSE100[,1]
FTSE100_close <- FTSE100[,2]
FTSE100_returns <- (FTSE100_open - FTSE100_close) / FTSE100_close
Y.norm <- FTSE100_returns^2 - mean(FTSE100_returns^2)
L <- length(Y.norm)

## define the parameters to be set by the user:

## (1) the maximum model order to be used for the stationary and for the
##     locally stationary approach
p_max <- 8

## (2) the set of potential segment lengths \mathcal{N};
##     i.e., which N to compute the predictions for
Ns <- c(0, seq(30, ceiling(L)^(4/5)))

## (3) the maximum forecasting horizon
H <- 5

## (4) and the size of the validation and test set;
##     also used for the "final part of the training set"
m <- 15

## from which we obtain the end indices of the four sets
m0 <- L - 3*m # 423
m1 <- L - 2*m # 438
m2 <- L - m   # 453
m3 <- L       # 468

## We could show the observations from the 
# Y.norm[(m0+1):m1] # final part of the training set,
# Y.norm[(m1+1):m2] # the validation set and
# Y.norm[(m2+1):m3] # the test set, respectively.

## now, compute all the linear prediction coefficients needed
coef0 <- predCoef(Y.norm, p_max, H, (m0-H+1):(m3-1), Ns)

## then, compute the MSPE on the final part of the training set
mspe <- MSPE(Y.norm, coef0, m0+1, m1, p_max, H, Ns)

## compute the minima for all N >= N_min
N_min <- min(Ns[Ns != 0])
M <- mspe$mspe
N <- mspe$N

res_e <- matrix(0, nrow = H, ncol = 5)
for (h in 1:H) {
  res_e[h, 1] <- idx1_s <- which(M[h, , N == 0] == min(M[h, , N == 0]), arr.ind = TRUE)[1]
  res_e[h, 2] <- min(M[h, , N == 0])
  idx1_ls <- which(M[h, , N != 0 & N >= N_min] == min(M[h, , N != 0 & N >= N_min]), arr.ind = TRUE)[1,]
  res_e[h, 3] <- idx1_ls[1]
  res_e[h, 4] <- N[N != 0 & N >= N_min][idx1_ls[2]]
  res_e[h, 5] <- min(M[h, , N != 0 & N >= N_min])
}

## Top rows from Table 7 in Kley et al (2016)
res_e

## compute the MSPE of the null predictor
vr <- sum(Y.norm[(m0 + 1):m1]^2) / (m1 - m0)

## Top plot from Figure 8 in Kley et al (2016)
plot(mspe, N_min = N_min, h = 1, add.for.legend=20)

## Middle plot from Figure 8 in Kley et al (2016)
plot(mspe, N_min = N_min, h = 2, add.for.legend=20)

## Bottom plot from Figure 8 in Kley et al (2016)
plot(mspe, N_min = N_min, h = 3, add.for.legend=20)

## compute MSPE on the validation set 
mspe <- MSPE(Y.norm, coef0, m1 + 1, m2, p_max, H, Ns)
M <- mspe$mspe
N <- mspe$N

## Compare the stationary approach with the locally stationary approach,
## both for the optimally chosen p_s and (p_ls, N_ls) that achieved the
## minimal MSPE on the final part of the training set.
res_v <- matrix(0, nrow = H, ncol = 3)
for (h in 1:H) {
  res_v[h, 1] <- M[h, res_e[h, 1], N == 0]
  res_v[h, 2] <- M[h, res_e[h, 3], N == res_e[h, 4]]
  res_v[h, 3] <- res_v[h, 1] / res_v[h, 2]
}

## compute MSPE on the validation set 
mspe <- MSPE(Y.norm, coef0, m2 + 1, m3, p_max, H, Ns)
M <- mspe$mspe
N <- mspe$N

## Compare the stationary approach with the locally stationary approach,
## both for the optimally chosen p_s and (p_ls, N_ls) that achieved the
## minimal MSPE on the final part of the training set.
res_t <- matrix(0, nrow = H, ncol = 3)
for (h in 1:H) {
  res_t[h, 1] <- M[h, res_e[h, 1], N == 0]
  res_t[h, 2] <- M[h, res_e[h, 3], N == res_e[h, 4]]
  res_t[h, 3] <- res_t[h, 1] / res_t[h, 2]
}

## Bottom rows from Table 7 in Kley et al (2016)
cbind(res_v, res_t)
