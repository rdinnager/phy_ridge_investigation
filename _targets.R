## Load your packages, e.g. library(targets).
source("./packages.R")

conflict_prefer("map", "purrr")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style
  
  tar_target(test_sim, sim_tree()),
  
  tar_rep(trees_sim, list(sim_tree()),
          100, 10,
          iteration = "list"),
  
  tar_rep2(traits_sim_brownian, 
           list(sim_brownian(trees_sim)),
           trees_sim,
           iteration = "list"),
  
  tar_rep2(RR_models, run_RR_models(trees_sim, traits_sim_brownian),
           traits_sim_brownian,
           trees_sim,
           iteration = "list"),
  
  tar_render(report, "doc/report.Rmd")

)
