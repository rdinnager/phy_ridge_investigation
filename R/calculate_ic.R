#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param RR_models
calculate_ic <- function(RR_models) {

  mod <- RR_models$glmnet
  rtp <- RR_models$rtp
  y <- RR_models$y
  penalty.factor <- RR_models$penalty.factor
  ic <- crits(mod, rtp, y, alpha = 0, penalty.factor = penalty.factor)

  ic

}

crits <- function(fit, x, y, alpha = 1, penalty.factor = NULL){

  lambda <- fit$lambda
  df <- fit$df
  n <- length(y)
  nlambda <- length(lambda)
  p <- ncol(x)

  loocv <- numeric(nlambda)
  loocv2 <- numeric(nlambda)

  if(!is.null(penalty.factor)) {
    scal <- penalty.factor * (n / sum(penalty.factor)) * p
  } else {
    scal <- 1
  }

  if(alpha == 0){
    xs <- x

    I <- diag(ncol(x))
    xx <- t(xs)%*%xs
    for(i in 1:nlambda){

      aux <- solve(xx + I * lambda[i] * scal * n)
      hatm <- xs%*%aux%*%t(xs)
      df[i] <- sum(diag(hatm))

      ymat <- matrix(y, ncol = 1)
      onemhat <- diag(n) - hatm

      loocv[i] <- t(ymat) %*% onemhat %*% (diag(n) * (diag(onemhat)^-2)) %*% onemhat %*% ymat



      # B <- diag(1 / (1 - diag(hatm)))
      # ff <- (B%*%(diag(ncol(hatm)) - hatm))%*%matrix(y, ncol = 1)
      # loocv[i] <- sum(ff^2) / n



    # df2 <- df
    # t2 <- system.time({
    # svd <- sparsesvd(x)
    # D <- svd$d^2
    # for(i in 1:length(lambda)){
    #   aux <- sum(D * (1 / (D + lambda[i] * scal)))
    #   df2[i] <- sum(diag(xs%*%aux%*%t(xs)))
    # }
    # })
    }


  }

  coef <- coef(fit)

  yhat <- cbind(1, x) %*% coef
  residuals <- (y - yhat)
  mse <- colMeans(residuals^2)
  sse <- colSums(residuals^2)

  # nvar <- df + 1
  # bic <- 2 * n * log(mse) + nvar * log(n)
  # aic <- 2 * n * log(mse) + 2*nvar
  # aicc <- aic + (2 * nvar * (nvar+1)) / (n - nvar - 1)
  #hqc <- n * log(mse) + 2 * nvar * log(log(n))

  sigma <- sqrt(sse / (n-df))

  tLL <- fit$nulldev - fit$nulldev * (1 - fit$dev.ratio)
  k <- df
  aicc <- -tLL + 2 * k + 2 * k * (k + 1) / (n - k - 1)
  aic <- -tLL + 2 * k
  bic <- log(n) * k - tLL

  bic_l <- bic + 1.05 * lchoose(ncol(x), k)

  # k <- df2 + 1
  # aicc2 <- -tLL + 2 * k + 2 * k * (k + 1) / (n - k - 1)
  # aic2 <- -tLL + 2 * k
  # bic2 <- log(n) * k - tLL

  rate_est <- (sigma^2) / (lambda * n)

  list(bic = bic, aic = aic, aicc = aicc, bic_l = bic_l, sigma = sigma, df = df,
       mse = mse, rate_est = rate_est, loocv = loocv)
}

make_rtp_loo <- function(rtp) {
  pars <- apply(rtp, 1, function(x) tail(which(x > 0), 2))
}

