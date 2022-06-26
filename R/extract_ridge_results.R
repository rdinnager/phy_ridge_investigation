#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param RR_models
#' @param traits_sim_brownian
#' @param rates
extract_ridge_results <- function(info_criteria, traits_sim_brownian, rates, trees_sim) {

  tree <- trees_sim[[1]]$tree
  tree_fact <- sum(tree$edge.length)

  #mod <- RR_models$glmnet
  aics <- info_criteria


  best_mod_aic <- which.min(aics$aic)
  best_mod_aicc <- which.min(aics$aicc)
  best_mod_bic <- which.min(aics$bic)
  best_mod_loocv <- which.min(aics$loocv)

  rate_est_aic <- aics$rate_est[best_mod_aic]
  rate_est_aicc <- aics$rate_est[best_mod_aicc]
  rate_est_bic <- aics$rate_est[best_mod_bic]
  rate_est_loocv <- aics$rate_est[best_mod_loocv]

  rate_true <- rates[[1]]
  y_var <- var(traits_sim_brownian[[1]][ , , 1])

  res <- tibble(rate_true = rate_true,
                rate_est_aic = rate_est_aic,
                rate_est_aicc = rate_est_aicc,
                rate_est_bic = rate_est_bic,
                rate_est_loocv = rate_est_loocv,
                y_var = y_var,
                tree_fact = tree_fact)

  return(res)


}
