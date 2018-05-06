library(rstan)
library(dplyr)
library(tidyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

xxx_calc_1pl_stan_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person = factor(person),
      person_1ind = as.integer(as.integer(person)),
      item = factor(item),
      item_1ind = as.integer(as.integer(item)),
      success = as.numeric(success)
    )
}

xxx_calc_1pl_stan_prep_data_list <- function(stan_data){
  list(
    I = length(levels(stan_data$item)),
    P = length(levels(stan_data$person)),
    O = nrow(stan_data),
    items = stan_data$item_1ind,
    persons = stan_data$person_1ind,
    success = stan_data$success
  )
}


calc_1pl_stan <- function(dat, iter = 4000, warmup = floor(iter/4), chains = 4){
  t1 = Sys.time()
  
  stan_data = xxx_calc_1pl_stan_prep_data(dat)
  stan_data_list = xxx_calc_1pl_stan_prep_data_list(stan_data)
  
  
  stan_1pl_fit = stan(
    file = '1pl_stan.stan',
    data = stan_data_list,
    chains = chains,
    warmup = warmup,
    iter = iter
  )
  
  diffs_calc = apply(extract(stan_1pl_fit)$diffs, -1, mean)
  diffs_calc_ci = t(apply(extract(stan_1pl_fit)$diffs, -1, quantile, c(0.025,0.975)))
  skills_calc = apply(extract(stan_1pl_fit)$skills, -1, mean)
  skills_calc_ci = t(apply(extract(stan_1pl_fit)$skills, -1, quantile, c(0.025,0.975)))
  
  
  item_params = dat$items %>%
    rename(diffs_orig = diff) %>%
    mutate(
      diffs_calc = diffs_calc,
      diffs_calc_ci_l = diffs_calc_ci[,1],
      diffs_calc_ci_u = diffs_calc_ci[,2]
    )
  
  person_params = dat$persons %>%
    rename(skills_orig = skill) %>%
    mutate(
      skills_calc = skills_calc,
      skills_calc_ci_l = skills_calc_ci[,1],
      skills_calc_ci_u = skills_calc_ci[,2]
    )
  
  res = list(
    items = item_params,
    persons = person_params
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}





