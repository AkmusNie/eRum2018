library(TAM)
library(dplyr)
library(tidyr)


calc_1pl_irt <- function(dat){
  t1 = Sys.time()
  
  # convert data to a matrix-like form
  resp_matrix = dat$test_data %>%
    spread(item, success) %>%
    mutate(person = as.character(person)) %>%
    as.data.frame() %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames("person")
  
  tam_res = tam(resp_matrix)
  
  # extract estimated parameters, and put them in the output structure
  item_params = dat$items %>%
    rename(diffs_orig = diff) %>%
    inner_join(
      tibble(
        item = factor(
          colnames(resp_matrix),
          levels = levels(dat$items$item)
        ),
        diffs_calc = tam_res$item$xsi.item
      ),
      by = 'item'
    )
  
  person_params = dat$persons %>%
    rename(skills_orig = skill) %>%
    inner_join(
      tibble(
        person = factor(
            rownames(resp_matrix),
            levels = levels(dat$persons$person)
          ),
        skills_calc = tam_res$person$EAP
      ),
      by = 'person'
    )
  
  t2 = Sys.time()
  
  res = list(
    items = item_params,
    persons = person_params
  )
  
  res$time = t2 - t1
  
  res
}
