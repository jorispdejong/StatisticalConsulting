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
            dzdt[k] = exp(lognormal_lcdf(tstar[k]*(1.0-t) | param1, param2)) * r * tstar[k] * exp(-r*tstar[k]*t) / (1.0 - exp(-r*tstar[k]*t));
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
    real<lower = 0> upper_bound;
    real<lower = 0> r;
}

transformed data {
    int X_i[1] = {N};

    real X_r[2] = {upper_bound, r};
}

parameters {
    real logmean_SI;
    real logsd_SI;

    vector<lower = 0, upper = 1>[N] e_raw;
    vector<lower = 0, upper = 1>[N] s_raw;
}

transformed parameters {
    real<lower = 0> param2 = sqrt(log((exp(2*(logsd_SI-logmean_SI))+1.0)));
    real param1 = logmean_SI - param2^2/2.0;

    vector<lower = min(E_L), upper = max(E_R)>[N] e;
    vector<lower = min(S_L), upper = max(S_R)>[N] s;

    real Z[N]; 
    
    {
        real theta[N+2];
        real Z0[N];
        
        s = S_L + (S_R - S_L) .* s_raw;
        for (k in 1:N) 
            if (E_R[k] > s[k]) 
                e[k] = E_L[k] + (s[k] - E_L[k]) * e_raw[k];
            else
                e[k] = E_L[k] + (E_R[k] - E_L[k]) * e_raw[k];

        for (k in 1:N) {
            Z0[k] = 0.0;
            theta[k] = e[k];
        }
        theta[N+1] = param1;
        theta[N+2] = param2;

        Z = to_array_1d(integrate_ode_rk45(fstar_ode, Z0, 0.001, {1.0}, theta, X_r, X_i, 1e-5, 1e-3, 5e2));
    }
}

model {
    logmean_SI ~ std_normal();
    logsd_SI ~ std_normal();

    e_raw ~ normal(0.5, 1.0);
    s_raw ~ normal(0.5, 1.0);

    for (k in 1:N) 
        target += lognormal_lpdf(s[k] - e[k] | param1, param2) - log(Z[k]);
}

generated quantities {
    real<lower = 0> mean_SI = exp(param1 + param2^2/2);
    real<lower = 0> sd_SI = sqrt((exp(param2^2)-1)*exp(2*param1+param2^2));

    vector[N] log_likelihood;
    for (k in 1:N)
        log_likelihood[k] = lognormal_lpdf(s[k] - e[k] | param1, param2) - log(Z[k]);
}
