library(plotly)

source('1pl_generate_data.R')
source('1pl_irt.R')
source('1pl_me.R')
source('1pl_tf.R')
source('1pl_greta.R')
source('1pl_stan.R')

# generating simulated data
dat_1pl = generate_data_1pl(100, 1000, skill_mean = 0, diff_mean = 0)

# estimation using the specialised package TAM
res_1pl_irt = calc_1pl_irt(dat_1pl)
# estimation using logistic regression with random effects
res_1pl_me = calc_1pl_me(dat_1pl)
# estimation using MLE with logloss and gradient descent (tensorflow)
res_1pl_tf = calc_1pl_tf(dat_1pl) # does up to 50 000 iterations, ends earlier if loss stops changing (minimum 10 000 iterations)
# estimation using greta (probabilitic programming)
res_1pl_greta = calc_1pl_greta(dat_1pl) # 16 000 samples (4000 warmup samples)
# estimation using stan (probabilitic programming)
res_1pl_stan = calc_1pl_stan(dat_1pl) # 4 chains, each with 4000 samples (1000 warmup samples)


save(dat_1pl, res_1pl_irt, res_1pl_me, res_1pl_tf, res_1pl_greta, res_1pl_stan, file = 'data/1pl.RData', compress = TRUE)

# difficulty original vs values estimated
plot_ly() %>%
  add_markers(
    data = res_1pl_irt$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    name = 'irt'
  ) %>%
  add_markers(
    data = res_1pl_me$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    name = 'me'
  ) %>%
  add_markers(
    data = res_1pl_tf$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    name = 'tf'
  ) %>%
  add_markers(
    data = res_1pl_stan$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    name = 'stan'
  ) %>%
  add_markers(
    data = res_1pl_greta$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    name = 'greta'
  )


# skill original vs values estimated
plot_ly() %>%
  add_markers(
    data = res_1pl_irt$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'irt'
  ) %>%
  add_markers(
    data = res_1pl_me$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'me'
  ) %>%
  add_markers(
    data = res_1pl_tf$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'tf'
  ) %>%
  add_markers(
    data = res_1pl_stan$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'stan'
  ) %>%
  add_markers(
    data = res_1pl_greta$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'greta'
  )

# Mean square errors
res_1pl_irt$items %>% transmute(diff_diff = abs(diffs_orig - diffs_calc) ^ 2) %>% summarise(mse = mean(diff_diff))
res_1pl_me$items %>% transmute(diff_diff = abs(diffs_orig - diffs_calc) ^ 2) %>% summarise(mse = mean(diff_diff))
res_1pl_tf$items %>% transmute(diff_diff = abs(diffs_orig - diffs_calc) ^ 2) %>% summarise(mse = mean(diff_diff))
res_1pl_greta$items %>% transmute(diff_diff = abs(diffs_orig - diffs_calc) ^ 2) %>% summarise(mse = mean(diff_diff))
res_1pl_stan$items %>% transmute(diff_diff = abs(diffs_orig - diffs_calc) ^ 2) %>% summarise(mse = mean(diff_diff))





# The same estimation as above, but for a larger sample
dat_1pl = generate_data_1pl(500, 5000, skill_mean = 0, diff_mean = 0)

res_1pl_irt = calc_1pl_irt(dat_1pl)
res_1pl_me = calc_1pl_me(dat_1pl)
res_1pl_tf = calc_1pl_tf(dat_1pl)       # up to 50 000, ends earlier if loss value change stalls (minimum 10 000 iterations)
res_1pl_greta = calc_1pl_greta(dat_1pl) # 16 000 samples (4000 warmup samples)
# res_1pl_stan = calc_1pl_stan(dat_1pl) # this one is skipped as it would take too long to run it


save(dat_1pl, res_1pl_irt, res_1pl_me, res_1pl_tf, res_1pl_tf_gpu, res_1pl_greta, res_1pl_greta_gpu, file = 'data/1pl_big.RData', compress = TRUE)
