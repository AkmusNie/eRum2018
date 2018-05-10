# devtools::install_github("rstudio/tensorflow")
# tensorflow::install_tensorflow()

library(tensorflow)
library(dplyr)
library(tidyr)

# if you want to use an existing python environment installed in your system
use_python('/usr/local/anaconda3/bin/python')

# function converting data to the format required
xxx_calc_1pl_tf_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person = factor(person),
      person_0ind = as.integer(as.integer(person)-1),
      item = factor(item),
      item_0ind = as.integer(as.integer(item)-1),
      success = as.numeric(success)
    )
}

# function building a model
xxx_calc_1pl_tf_prep_model <- function(tf_data){
  
  # tensor containing item difficulties
  t_diffs = tf$Variable(
    tf$random_normal(
      shape = shape(length(levels(tf_data$item))),
      mean = 0,
      stddev = 3.0
    ),
    name = 'diffs'
  )
  
  # tensor containing user skill levels
  t_skills = tf$Variable(
    tf$random_normal(
      shape = shape(length(levels(tf_data$person))),
      mean = 0,
      stddev = 3.0
    ),
    name = 'skills'
  )
  
  # selection of the proper variable with skill and difficulty for each row of observations
  t_diffs_gathered = tf$gather(t_diffs, tf_data$item_0ind)
  t_skills_gathered = tf$gather(t_skills, tf_data$person_0ind)
  
  # logit and probability of giving a correct answer 
  t_rel_diff = t_skills_gathered - t_diffs_gathered
  y_ = tf$sigmoid(t_rel_diff)
  
  y = tf_data$success
  
  # main loss function. It is responsible for Maximum Likelihood Estimation
  loss_each = tf$losses$log_loss(y, y_)
  loss_main = tf$reduce_mean(loss_each, name = 'loss')
  
  # mean and variation of skill values
  mm = tf$nn$moments(t_skills, axes = 0L)
  
  loss = loss_main +            # main loss function, to which we add components related to mean and sd
    0.1 * tf$abs(mm[[1]]) +     # skill level should have mean = 0, and sd = 1
    0.05 * tf$abs(mm[[2]] - 1)  # this way other parameters are adjusted to standarised skills values
  
  list(
    t_diffs = t_diffs,
    t_skills = t_skills,
    t_diffs_gathered = t_diffs_gathered,
    t_skills_gathered = t_skills_gathered,
    y_ = y_,
    loss_main = loss_main,
    skills_mean = mm[[1]],
    skills_sd = tf$sqrt(mm[[2]]),
    loss = loss
  )
}

# function establishing the optimisation process
xxx_calc_1pl_tf_init_train <- function(m, sess, learning_rate = 0.001){
  # we will use Adam optimiser to minimize loss value (maximize total likelihood)
  optimizer = tf$train$AdamOptimizer(learning_rate) # Adam is an improved version of gradient descent
  train = optimizer$minimize(m$loss)
  
  # initialisation of variables
  sess$run(tf$global_variables_initializer())
  
  m$optimizer = optimizer
  m$train = train
  
  m
}

# function for iterative improvement of skill and difficulty estimations
xxx_calc_1pl_tf_train <- function(m, sess, stop_threshold, step_size, window_size, min_step, step_limit){
  step = 0
  stalled = FALSE
  
  loss = sess$run(m$loss)
  
  losses = tibble(
    step = step,
    loss_total = sess$run(m$loss),
    loss_main = sess$run(m$loss_main),
    skills_mean = sess$run(m$skills_mean),
    skills_sd = sess$run(m$skills_sd)
  )
  cat(
    sprintf(
      "[%s] %6d: %2.8f (wm: %2.8f) lm: %2.8f sm: %+2.8f ssd: %2.8f\n",
      as.character(Sys.time()),
      step, loss, loss,
      sess$run(m$loss_main),
      sess$run(m$skills_mean),
      sess$run(m$skills_sd)
    )
  )
  
  while (
    (step < min_step) | # do at least min_step steps
    (step >= (window_size * step_size) & !stalled & (step < step_limit)) # end when progress will stall or maximum of steps will be achieved
  ) {
    for (i in 1:step_size) {
      # one iteration of estimations optimisations (forward and backward propagation)
      sess$run(m$train)
      losses = rbind(
        losses,
        tibble(
          step = step + i,
          loss_total = sess$run(m$loss),
          loss_main = sess$run(m$loss_main),
          skills_mean = sess$run(m$skills_mean),
          skills_sd = sess$run(m$skills_sd)
        )
      )
    }
    step = step + step_size
    curr = sess$run(m$loss)
    
    last = mean(tail(losses$loss_total, step_size))
    stalled = FALSE
    if(step >= window_size * step_size) {
      stalled = TRUE
      for(w in 2:window_size){
        longer = mean(tail(losses$loss_total, w * step_size))
        stalled = stalled & (2 * abs(longer - last) / (longer + last) < stop_threshold)
      }
    }
    
    cat(
      sprintf(
        "[%s] %6d: %2.8f (wm: %2.8f) lm: %2.8f sm: %+2.8f ssd: %2.8f\n",
        as.character(Sys.time()),
        step, curr, last,
        sess$run(m$loss_main),
        sess$run(m$skills_mean),
        sess$run(m$skills_sd)
      )
    )
  }
  
  losses
}

# function calling other functions. For details please take a look at their comments
calc_1pl_tf <- function(dat, stop_threshold = 0.0001, step_size = 1000, window_size = 10, min_step = 10000, step_limit = 50000, learning_rate = 0.005){
  t1 = Sys.time()
  
  tf_data = xxx_calc_1pl_tf_prep_data(dat)
  
  m = xxx_calc_1pl_tf_prep_model(tf_data)
  
  sess = tf$Session()
  m = xxx_calc_1pl_tf_init_train(m, sess, learning_rate)
  
  losses = xxx_calc_1pl_tf_train(m, sess, stop_threshold, step_size, window_size, min_step, step_limit)

  diffs_calc = sess$run(m$t_diffs)
  skills_calc = sess$run(m$t_skills)

  # difficulty values extraction
  item_params = tibble(
    item = dat$items$item,
    diffs_orig = dat$items$diff,
    diffs_calc = diffs_calc
  )
  
  # skill levels extraction
  person_params = tibble(
    person = dat$persons$person,
    skills_orig = dat$persons$skill,
    skills_calc = skills_calc
  )
  
  res = list(
    items = item_params,
    persons = person_params,
    losses = losses
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}


