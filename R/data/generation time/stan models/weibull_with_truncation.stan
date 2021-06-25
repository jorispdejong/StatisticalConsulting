functions {
    real[] fstar_ode(real t, real[] z, real[] theta, data real[] x_r, int[] x_i) {
        int N = x_i[1]; // number of records

        real e[N] = theta[1:N];
        real param1 = theta[N+1];
        real param2 = theta[N+2];

        real upper_bound = x_r[1];
        real r = x_r[2];

        real dzdt[N];
        real tstar[N];

        for (k in 1:N) {
            tstar[k] = upper_bound - e[k];
            dzdt[k] = - expm1(-((1.0-t)*tstar[k]/param2)^param1) * r * tstar[k] * exp(-r*tstar[k]*t) / (1.0 - exp(-r*tstar[k]*t));
        }

        return dzdt;
    }
}

data {
    int<lower = 0> N; // number of records
    vector<lower = 0>[N] E_L;
    vector<lower = 0>[N] E_R;
    vector<lower = 0>[N] S_L;
    vector<lower = 0>[N] S_R;
    real<lower = 0> r;
    real<lower = 0> upper_bound;
}

transformed data {
    int X_i[1] = {N};

    real X_r[2] = {upper_bound, r};
}

parameters {
    real<lower = 0> mean_SI;
    real<lower = 0> param1;

    vector<lower = 0, upper = 1>[N] e_raw;
    vector<lower = 0, upper = 1>[N] s_raw;
}

transformed parameters {
    real<lower = 0> param2 = mean_SI/tgamma(1.0+1.0/param1);

    vector<lower = min(S_L), upper = max(S_R)>[N] s;
    vector<lower = min(E_L), upper = max(E_R)>[N] e;
    vector<lower = 0, upper = max(S_R)>[N] t;

    real Z[N]; 
    
    {
        vector[N] s_;
        real theta[N+2];
        real Z0[N];
        
        s_ = S_L + (S_R - S_L) .* s_raw;
        for (k in 1:N) {
            if (s_[k] < E_L[k])
                s[k] = E_L[k];
            else 
                s[k] = s_[k];

            if (E_R[k] > s[k]) 
                e[k] = E_L[k] + (s[k] - 1e-3 - E_L[k]) * e_raw[k];
            else if (E_L[k] > s[k])
                e[k] = s[k] - 1e-3;
            else
                e[k] = E_L[k] + (E_R[k] - E_L[k]) * e_raw[k];
        }
        t = s - e;

        for (k in 1:N) {
            Z0[k] = 0.0;
            theta[k] = s[k] - t[k];
        }
        theta[N+1] = param1;
        theta[N+2] = param2;

        Z = to_array_1d(integrate_ode_rk45(fstar_ode, Z0, 0.001, {1.0}, theta, X_r, X_i, 1e-5, 1e-3, 5e2));
    }
}

model {
    mean_SI ~ normal(5.0, 10.0);
    param1 ~ exponential(0.0001);

    e_raw ~ normal(0.5, 1.0);
    s_raw ~ normal(0.5, 1.0);

    for (k in 1:N) 
        target += weibull_lpdf(t[k] | param1, param2) - log(Z[k]);
}

generated quantities {
    real sd_SI = param2*sqrt(tgamma(1.0+2.0/param1)-(tgamma(1.0+1.0/param1))^2);

    vector[N] log_likelihood;
    for (k in 1:N) 
        log_likelihood[k] = weibull_lpdf(t[k] | param1, param2) - log(Z[k]);
}
