data {
  int<lower=1> I;
  int<lower=1> P;
  int<lower=1> O;
  int<lower=1, upper=I> items[O];
  int<lower=1, upper=P> persons[O];
  int<lower=0, upper=1> success[O];
}
parameters {
  vector[I] diffs;
  vector[P] skills;
}
model {
  diffs ~ normal(0,6);
  skills ~ normal(0,6);
  
  success ~ bernoulli_logit(skills[persons] - diffs[items]);
}
