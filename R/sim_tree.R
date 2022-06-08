#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

sim_tree <- function() {

  #curve(dlnorm(x, 0.5^2, 1))
  tree <- sim_ClaDS(0.5, 0.2, time_stop = 100, taxa_stop = 100, lamb_max = 3,
                    sigma_lamb = 0.25)
  #plot(test$tree)
  tree$tree$edge.length <- tree$tree$edge.length / max(vcv(tree$tree))
  
  tree

}
