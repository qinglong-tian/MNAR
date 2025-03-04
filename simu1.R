library(fastGHQuad)
library(tidyverse)
source("simulate_data.R")
Rcpp::sourceCpp("bspline_recursive.cpp")
source("add_splines.R")
source("estep.R")
source("mstep.R")
source("loglikelihood.R")
####################################

set.seed(47)
n <- 1000
coef1 <- c(1, 1)
sd <- 1
ghn <- 8
bn <- 3
q <- 3
max_iter <- 500
tol <- 1e-5


X <- matrix(truncnorm::rtruncnorm(n), ncol = 1)
Y <- simuY(cbind(1, X), coef1, sd)
Z <- X
U <- NULL # \pi(Y)
nsieves <- bn + q

yy <- log(Y / (1 - Y))
YU <- cbind(1, yy, yy^2)
coef2 <- c(1, -1, .5)
Obs <- simuMiss(YU, coef2, use_logit = T)
table(Obs)

dat <- cbind(Y, Obs, Z, U)
colnames(dat) <- c("Y", "Obs", "X1")
dat %>% as.data.frame %>% mutate(OBS = as.factor(Obs), yy = log(Y / (1 -
                                                                       Y))) %>%
  ggplot(aes(x = X)) + geom_density(aes(fill = OBS), alpha = 0.5)

yObs <- Y[which(Obs == 1)]
xObs <- X[which(Obs == 1), ]
yLM <- log(yObs / (1 - yObs))
lmMAR <- lm(yLM ~ xObs)
hn <- min(sqrt(diag(vcov(lmMAR))))

df_MNAR <- list(data = dat,
                Z_indices = 3,
                U_indices = NULL)
(emEstimate <- main(
  df_MNAR,
  coef(lmMAR) + 0.5,
  sigma(lmMAR) + 0.5,
  runif(bn + q, min = -1, max = 1),
  bn,
  q,
  ghn,
  max_iter,
  tol
))

# MAR
dat_mar <- dat %>% as.data.frame %>% filter(Obs == 1)
lm(Y ~ X1, data = dat_mar)

# Compute covariance matrix
beta_mle <- emEstimate$Beta
sd_mle <- emEstimate$Sigma
tau_mle <- emEstimate$Tau

temp <- ProfileCov(
  df_MNAR,
  min(hn, 1 / sqrt(n)),
  beta_mle,
  sd_mle,
  tau_mle,
  bn,
  q,
  ghn,
  nsieves,
  1,
  max_iter,
  tol
)

# Tune the value of hn
hnVec <- c(0.05 * hn , 0.1 * hn, 0.5 * hn, hn, 1 / sqrt(n), 0.025, 2 * hn, 5 *
             hn)
outList <- list()
for (i in 1:length(hnVec))
{
  outList[[i]] <- list(
    delta = hnVec[i],
    ProfileCov(
      df_MNAR,
      hnVec[i],
      beta_mle,
      sd_mle,
      tau_mle,
      bn,
      q,
      ghn,
      nsieves,
      1,
      max_iter,
      tol
    )
  )
}
outList
