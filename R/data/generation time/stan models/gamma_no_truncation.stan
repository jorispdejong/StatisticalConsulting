data {
    int<lower = 0> N; // number of records
    vector<lower = 0>[N] E_L;
    vector<lower = 0>[N] E_R;
    vector<lower = 0>[N] S_L;
    vector<lower = 0>[N] S_R;
}

parameters {
    real<lower=0> mean_SI;
    real<lower=0> sd_SI;

    vector<lower = 0, upper = 1>[N] e_raw;
    vector<lower = 0, upper = 1>[N] s_raw;
}

transformed parameters {
    real<lower = 0> param1 = (mean_SI/sd_SI)^2;
    real<lower = 0> param2 = mean_SI/(sd_SI^2);

    vector<lower = min(S_L), upper = max(S_R)>[N] s;
    vector<lower = min(E_L), upper = max(E_R)>[N] e;

    s = S_L + (S_R - S_L) .* s_raw;
    for (k in 1:N) 
        if (E_R[k] > s[k]) 
            e[k] = E_L[k] + (s[k] - E_L[k]) * e_raw[k];
        else
            e[k] = E_L[k] + (E_R[k] - E_L[k]) * e_raw[k];
}

model {
    mean_SI ~ normal(5.0, 10.0);
    sd_SI ~ cauchy(0, 5.0);

    e_raw ~ normal(0.5, 1.0);
    s_raw ~ normal(0.5, 1.0);

    s - e ~ gamma(param1, param2);
}

generated quantities {
    vector[N] log_likelihood;
    for (k in 1:N) 
        log_likelihood[k] = gamma_lpdf(s[k] - e[k] | param1, param2);
}
