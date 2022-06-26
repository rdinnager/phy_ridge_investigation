## Load your packages, e.g. library(targets).
source("./packages.R")

conflict_prefer("map", "purrr")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

future::plan(future::multisession(workers = 6))

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style

  tar_target(test_sim, sim_tree()),

  tar_rep(trees_sim, list(sim_tree()),
          100, 10,
          iteration = "list"),

  tar_rep(rates, list(runif(1, 0.5, 5)),
          100, 10,
          iteration = "list"),

  tar_rep2(traits_sim_brownian,
           list(sim_brownian(trees_sim, rates)),
           trees_sim,
           rates,
           iteration = "list"),

  tar_rep2(RR_models, run_RR_models(trees_sim, traits_sim_brownian),
           traits_sim_brownian,
           trees_sim,
           iteration = "list"),

  tar_rep2(info_criteria, calculate_ic(RR_models),
           RR_models,
           iteration = "list"),

  tar_rep2(RR_results, extract_ridge_results(info_criteria, traits_sim_brownian, rates, trees_sim),
           info_criteria,
           traits_sim_brownian,
           rates,
           trees_sim),

  tar_rep2(div_effect, calculate_div_effect(RR_models, trees_sim, info_criteria),
           RR_models,
           trees_sim,
           info_criteria),

  tar_render(report, "doc/report.Rmd")

)
