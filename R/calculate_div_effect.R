#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param RR_models
#' @param trees_sim
calculate_div_effect <- function(RR_models, trees_sim, info_criteria) {

  phy <- trees_sim[[1]]$tree
  div_lamb <- trees_sim[[1]]$lamb[trees_sim[[1]]$rates]
  div_mu <- trees_sim[[1]]$mu[trees_sim[[1]]$rates]

  rrates <- RR_models$RRPhylo$rates
  edges1 <- match_edges2rates(rownames(rrates), phy)
  rrates <- rrates[edges1, ]

  lambda <- RR_models$glmnet$lambda[which.min(info_criteria$bic)]
  frates <- coef(RR_models$glmnet, s = lambda)[-1, , drop = FALSE]
  edge2 <- match_edges2rates(rownames(frates), phy)
  frates <- frates[edge2, ]

}

match_edges2rates <- function(nams, phy) {
  tips <- which(nams %in% phy$tip.label)
  nams[tips] <- as.character(match(nams[tips], phy$tip.label))
  nodes <- as.integer(nams)
  edges <- match(phy$edge[ , 2], nodes)
  edges
}
