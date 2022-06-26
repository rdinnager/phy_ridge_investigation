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

  res1 <- try(RRphylo(tree,
                  y))

  rtp <- make_root2tip(tree)

  edges_nums <- c((Ntip(tree) + 2):(Ntip(tree) + Nnode(tree)), 1:Ntip(tree))
  edge_ord <- match(edges_nums, tree$edge[ , 2])
  lens <- tree$edge.length[edge_ord]

  res1_glmnet <- try(glmnet(rtp, y, standardize = FALSE, nlambda = 500, lambda.min.ratio = 0.00001 / nrow(rtp), alpha = 0,
                        penalty.factor = lens))
  #aics <- try(crits(res1_glmnet, rtp, y, alpha = 0, penalty.factor = lens))


  # tree2 <- tree
  # tree2$edge.length <- sqrt(tree$edge.length)
  # res1_fibre <- try(fibre(y ~ p(tree2)))

  return(list(glmnet = res1_glmnet, RRPhylo = res1,
              rtp = rtp, y = y, penalty.factor = lens))

  #res2_glmnet2 <- cv.glmnet(rtp2, y, standardize = FALSE, nlambda = 500, lambda.min.ratio = 0.00001 / nrow(rtp2), alpha = 0)


  plot(extract_div_rates(trees_sim[[1]]), abs(RRrates_by_edge(res1$rates, trees_sim[[1]]$tree)))
  plot(extract_div_rates(trees_sim[[1]]), abs(RRrates_by_edge(res3$rates, trees_sim[[1]]$tree) / sqrt(trees_sim[[1]]$tree$edge.length)))

  plot(trees_sim[[1]]$tree$edge.length, sqrt(abs(RRrates_by_edge(res1$rates, trees_sim[[1]]$tree))))
  plot(trees_sim[[1]]$tree$edge.length, sqrt(abs(RRrates_by_edge(res3$rates, trees_sim[[1]]$tree) / sqrt(trees_sim[[1]]$tree$edge.length))))

  summary(lm(abs(RRrates_by_edge(res1$rates, trees_sim[[1]]$tree)) ~ extract_div_rates(trees_sim[[1]])))
  summary(lm(abs(RRrates_by_edge(res3$rates, trees_sim[[1]]$tree) / sqrt(trees_sim[[1]]$tree$edge.length)) ~ extract_div_rates(trees_sim[[1]])))

  plot(trees_sim[[1]]$tree$edge.length, extract_div_rates(trees_sim[[1]]))

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

