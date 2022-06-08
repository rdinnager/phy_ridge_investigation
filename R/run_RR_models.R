#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trees_sim
#' @param traits_sim_brownian
run_RR_models <- function(trees_sim, traits_sim_brownian) {
  
  tree <- trees_sim[[1]]$tree
  tree$edge.length <- tree$edge.length
  
  y <- traits_sim_brownian[[1]][ , 1, 1]
  
  res1 <- RRphylo(tree,
                  y)
  
  # y2 <- matrix(y, ncol = 1)
  # y2 <- cbind(y2, rnorm(length(y), sd = 0.001))
  # 
  # rownames(y2) <- names(y)
  # 
  # res2 <- RRphylo(tree,
  #                 y2)
  
  tree2 <- tree
  tree2$edge.length <- sqrt(tree2$edge.length)
  
  res3 <- RRphylo(tree2,
                  y)
  
  plot(extract_div_rates(trees_sim[[1]]), RRrates_by_edge(res1$rates, trees_sim[[1]]$tree))
  plot(trees_sim[[1]]$tree$edge.length, abs(RRrates_by_edge(res3$rates, trees_sim[[1]]$tree)))
  
  list(mod1 = res1, mod2 = res3)

}

extract_div_rates <- function(clads_tree) {
  
  rats <- clads_tree$lamb - clads_tree$mu
  rats <- rats[clads_tree$rates]
  rats
  
}

RRrates_by_edge <- function(RR_rates, tree) {
  rr <- c((Ntip(tree) + 1):(Nnode(tree) + Ntip(tree)), 1:Ntip(tree))
  rrates <- RR_rates[rr]
  
  rbe <- rrates[tree$edge[ , 2]]
  rbe
}
