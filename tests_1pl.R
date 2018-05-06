library(plotly)

source('1pl_generate_data.R')
source('1pl_functions.R')
source('1pl_irt.R')
source('1pl_me.R')
source('1pl_tf.R')
source('1pl_greta.R')
source('1pl_stan.R')

dat_1pl = generate_data_1pl(100, 1000, skill_mean = 0, diff_mean = 0)

res_1pl_irt = calc_1pl_irt(dat_1pl)
res_1pl_me = calc_1pl_me(dat_1pl)
res_1pl_tf = calc_1pl_tf(dat_1pl) # up to 50 000, ends earlier if loss value change stalls (minimum 10 000 iterations)
res_1pl_greta = calc_1pl_greta(dat_1pl) # 16 000 samples (4000 warmup samples)
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

plot_ly() %>%
  add_markers(
    data = res_1pl_irt$items,
    x = ~diffs_orig_centered,
    y = ~diffs_calc_centered,
    name = 'irt'
  ) %>%
  add_markers(
    data = res_1pl_me$items,
    x = ~diffs_orig_centered,
    y = ~diffs_calc_centered,
    name = 'me'
  ) %>%
  add_markers(
    data = res_1pl_tf$items,
    x = ~diffs_orig_centered,
    y = ~diffs_calc_centered,
    name = 'tf'
  ) %>%
  # add_markers(
  #   data = res_1pl_stan$items,
  #   x = ~diffs_orig_centered,
  #   y = ~diffs_calc_centered,
  #   name = 'stan'
  # ) %>%
  add_markers(
    data = res_1pl_greta$items,
    x = ~diffs_orig_centered,
    y = ~diffs_calc_centered,
    name = 'greta'
  )




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
  # add_markers(
  #   data = res_1pl_stan$persons,
  #   x = ~skills_orig_centered,
  #   y = ~skills_calc_centered,
  #   name = 'stan'
  # ) %>%
  add_markers(
    data = res_1pl_greta$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'greta'
  )




plot_ly() %>%
  add_markers(
    data = res_1pl_irt$persons,
    x = ~skills_orig_centered,
    y = ~skills_calc_centered,
    name = 'irt'
  ) %>%
  add_markers(
    data = res_1pl_me$persons,
    x = ~skills_orig_centered,
    y = ~skills_calc_centered,
    name = 'me'
  ) %>%
  add_markers(
    data = res_1pl_tf$persons,
    x = ~skills_orig_centered,
    y = ~skills_calc_centered,
    name = 'tf'
  ) %>%
  # add_markers(
  #   data = res_1pl_stan$persons,
  #   x = ~skills_orig_centered,
  #   y = ~skills_calc_centered,
  #   name = 'stan'
  # ) %>%
  add_markers(
    data = res_1pl_greta$persons,
    x = ~skills_orig_centered,
    y = ~skills_calc_centered,
    name = 'greta'
  )


cor(res_1pl_irt$diffs$diffs_orig_centered, res_1pl_irt$diffs$diffs_calc_centered)
mean((res_1pl_irt$diffs$diffs_orig_centered - res_1pl_irt$diffs$diffs_calc_centered)^2)
cor(res_1pl_me$diffs$diffs_orig_centered, res_1pl_me$diffs$diffs_calc_centered)
mean((res_1pl_me$diffs$diffs_orig_centered - res_1pl_me$diffs$diffs_calc_centered)^2)
cor(res_1pl_tf$diffs$diffs_orig_centered, res_1pl_tf$diffs$diffs_calc_centered)
mean((res_1pl_tf$diffs$diffs_orig_centered - res_1pl_tf$diffs$diffs_calc_centered)^2)
cor(res_1pl_stan$diffs$diffs_orig_centered, res_1pl_stan$diffs$diffs_calc_centered)
mean((res_1pl_stan$diffs$diffs_orig_centered - res_1pl_stan$diffs$diffs_calc_centered)^2)

cor(res_1pl_irt$skills$skills_orig_centered, res_1pl_irt$skills$skills_calc_centered)
mean((res_1pl_irt$skills$skills_orig_centered - res_1pl_irt$skills$skills_calc_centered)^2)
cor(res_1pl_me$skills$skills_orig_centered, res_1pl_me$skills$skills_calc_centered)
mean((res_1pl_me$skills$skills_orig_centered - res_1pl_me$skills$skills_calc_centered)^2)
cor(res_1pl_tf$skills$skills_orig_centered, res_1pl_tf$skills$skills_calc_centered)
mean((res_1pl_tf$skills$skills_orig_centered - res_1pl_tf$skills$skills_calc_centered)^2)
cor(res_1pl_stan$skills$skills_orig_centered, res_1pl_stan$skills$skills_calc_centered)
mean((res_1pl_stan$skills$skills_orig_centered - res_1pl_stan$skills$skills_calc_centered)^2)

