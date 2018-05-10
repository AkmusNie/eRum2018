library(greta)
library(dplyr)
library(tidyr)

# if you want to use an existing python environment installed in your system
tensorflow::use_python('/usr/local/anaconda3/bin/python')

# function converting data to the format required
xxx_calc_1pl_greta_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person = factor(person),
      person_1ind = as.integer(as.integer(person)),
      item = factor(item),
      item_1ind = as.integer(as.integer(item)),
      success = as.numeric(success)
    )
}

# function building a model
xxx_calc_1pl_greta_prep_model <- function(greta_data){
  item_idx = as.integer(greta_data$item)
  person_idx = as.integer(greta_data$person)
  y = as_data(as.integer(greta_data$success))
  
  # item difficulties an student skill levels 
  diffs = normal(0, 6, dim = length(levels(greta_data$item)))    # weak prior
  skills = normal(0, 1, dim = length(levels(greta_data$person))) # strong prior
  
  # item difficulties an student skill levels (proper values related for each observation)
  item_diffs = diffs[item_idx]
  person_skills = skills[person_idx]
  
  # logit and probability of giving a correct answer 
  logit = person_skills - item_diffs
  prob = ilogit(logit)
  distribution(y) = binomial(1, prob)
  
  # skill_exp_mean = as_data(0)
  # skill_est_mean = mean(skills)
  # distribution(skill_exp_mean) = normal(skill_est_mean, 0.001)

  # skill_exp_sd = as_data(1)
  # skill_est_sd = sqrt(mean((skills - skill_est_mean) ^ 2))
  # distribution(skill_exp_sd) = normal(skill_est_sd, 0.001)
  
  
  m <- model(diffs, skills)
}

xxx_calc_1pl_greta_get_stats = function(d){
  q = quantile(d, probs = c(0.025, 0.5, 0.975))
  tibble(
    q_2_5p = q[1],
    median = q[2],
    mean = mean(d),
    q_97_5p = q[3],
    se = sd(d)
  )
}

# function calling other functions. For details please take a look at their comments
calc_1pl_greta <- function(dat, iter = 16000, warmup = floor(iter/4)){
  t1 = Sys.time()
  
  greta_data = xxx_calc_1pl_greta_prep_data(dat)
  greta_model = xxx_calc_1pl_greta_prep_model(greta_data)
  
  # runs model samplings
  greta_draws <- mcmc(greta_model, warmup = warmup, n_samples = iter)
  
  # START extraction of parameters values (it may be implemented much more efficiently)
  greta_draws_m = as.matrix(greta_draws[[1]])

  greta_draws_tidy = greta_draws_m %>%
    as_tibble() %>%
    gather() %>%
    mutate(
      var_name = {
        tmp = regexpr('[a-zA-Z]+', .$key)
        regmatches(.$key, tmp)
      },
      index = as.integer({
        tmp = regexpr('[0-9]+', .$key)
        regmatches(.$key, tmp)
      })
    ) %>%
    select(var_name, index, value)
  
  greta_draws_stats = greta_draws_tidy %>%
    group_by(var_name, index) %>%
    do(xxx_calc_1pl_greta_get_stats(.$value))
  
  
  diffs_sampled = greta_draws_stats %>%
    filter(var_name == 'diffs')
  
  skills_sampled = greta_draws_stats %>%
    filter(var_name == 'skills')
  # END extraction of parameters values
  
  
  # put estimations in the output structure
  item_params = dat$items %>%
    rename(diffs_orig = diff) %>%
    mutate(
      diffs_calc = diffs_sampled$mean,
      diffs_calc_ci_l = diffs_sampled$q_2_5p,
      diffs_calc_ci_u = diffs_sampled$q_97_5p
    )
  
  person_params = dat$persons %>%
    rename(skills_orig = skill) %>%
    mutate(
      skills_calc = skills_sampled$mean,
      skills_calc_ci_l = skills_sampled$q_2_5p,
      skills_calc_ci_u = skills_sampled$q_97_5p
    )
  
  res = list(
    items = item_params,
    persons = person_params
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}





