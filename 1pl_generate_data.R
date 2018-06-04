library(dplyr)

generate_data_1pl <- function(items = 100, persons = 1000, noise_sd = 0, sample_perc = 1, skill_mean = 0, diff_mean = 0){
  items_t = tibble(
    item = factor(sprintf('item%05d',1:items)),
    diff = rnorm(items, diff_mean, 1)
  )
  
  persons_t = tibble(
    person = factor(sprintf('person%05d', 1:persons)),
    skill = rnorm(persons, skill_mean, 1)
  )
  
  full_data = persons_t %>%
    mutate(tmp = 1) %>%
    inner_join(mutate(items_t, tmp = 1), by = 'tmp') %>%
    select(-tmp) %>%
    mutate(
      logit = skill - diff + rnorm(n(), 0, noise_sd),
      prob = 1/(1 + exp(-logit)),
      success = prob >= runif(n())
    )
  
  test_data = full_data %>%
    select(person, item, success) %>%
    sample_frac(sample_perc)
  
  list(
    items = items_t,
    persons = persons_t,
    full_data = full_data,
    test_data = test_data
  )
}
