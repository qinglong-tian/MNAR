organize_data <- function(df_MNAR)
{
  zIndex <- df_MNAR$Z_indices
  uIndex <- df_MNAR$U_indices
  
  dat <- df_MNAR$data
  dat_non_missing <- dat[dat[,"Obs"] == 1,]
  
  # Outputs
  yVec <- dat_non_missing[,"Y"]
  yVec <- log(yVec/(1-yVec))
  
  if (is.null(zIndex))
  {
    zMat <- NULL
  }
  else
  {
    zMat <- dat_non_missing[,zIndex]
  }
  
  if (is.null(uIndex))
  {
    uMat <- NULL
  }
  else
  {
    uMat <- dat_non_missing[,uIndex]
  }
  
  return(list(yVec = yVec,
              zMat = zMat,
              uMat = uMat))
}

pseudo_loglikelihood_app <- function(theta, df_MNAR)
{
  # Extract Parameters
  beta <- theta[-length(theta)]
  sig <- theta[length(theta)]
  
  # Extract Data
  yzu <- organize_data(df_MNAR)
  yVec <- yzu$yVec
  XObs <- cbind(yzu$zMat, yzu$uMat)
  
  dataFull <- df_MNAR$data
  dataFullX <- dataFull[,-c(1, 2)]
  
  mean_vec <- cbind(1, dataFullX) %*% matrix(beta, ncol = 1)
  
  log_int <- numeric(length(yVec))
  log_int2 <- numeric(length(yVec))
  for (i in 1:length(yVec))
  {
    log_int[i] <- log(mean(dnorm(
      yVec[i], mean = mean_vec, sd = sig
    )))
    log_int2[i] <-
      log(dnorm(yVec[i], mean = sum(c(1, XObs[i,]) * beta), sd = sig))
  }
  loglik <- sum(log_int2 - log_int)
  
  return(-loglik)
}