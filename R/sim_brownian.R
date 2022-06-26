#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trees_sim
sim_brownian <- function(trees_sim, rate) {

  sim <- sim.char(trees_sim[[1]]$tree,
                         rate[[1]])
  # sims <- map(trees_sim,
  #             ~ sim.char(.x$tree,
  #                        1))

  sim

}
