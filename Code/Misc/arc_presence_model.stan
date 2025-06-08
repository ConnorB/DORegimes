data {
  int<lower=1> N;
  int<lower=1> K;
  array[N, K] int<lower=-1, upper=1> X;  // arc presence: 0/1, -1 = missing
}

parameters {
  vector<lower=0, upper=1>[K] p;   // marginal probabilities
  cholesky_factor_corr[K] L_corr; // correlation structure
  matrix[N, K] z;                  // latent normal variables
}
transformed parameters {
  matrix[N, K] y;
  y = z * L_corr';
}
model {
  // Priors
  p ~ beta(2, 2);
  L_corr ~ lkj_corr_cholesky(2);

  // Likelihood
  for (n in 1:N) {
    for (k in 1:K) {
      if (X[n, k] != -1) {
        real theta = inv_Phi(p[k]);
        target += bernoulli_logit_lpmf(X[n, k] | theta + y[n, k]);
      }
    }
  }
}
