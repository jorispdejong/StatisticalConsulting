data {
int<lower = 0> N; // number of records
vector<lower = 0>[N] E_L;
vector<lower = 0>[N] E_R;
vector<lower = 0>[N] S_L;
vector<lower = 0>[N] S_R;
}

parameters {
real logmean_SI;
real logsd_SI;

vector<lower = 0, upper = 1>[N] s_raw;
vector<lower = 0, upper = 1>[N] e_raw;
}

transformed parameters {
real<lower = 0> param2 = sqrt(log((exp(2*(logsd_SI-logmean_SI))+1.0)));
real param1 = logmean_SI - param2^2/2.0;

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
logmean_SI ~ std_normal();
logsd_SI ~ std_normal();

e_raw ~ normal(0.5, 1.0);
s_raw ~ normal(0.5, 1.0);

s - e ~ lognormal(param1, param2);
}

generated quantities {
real<lower = 0> mean_SI = exp(param1 + param2^2/2);
real<lower = 0> sd_SI = sqrt((exp(param2^2)-1)*exp(2*param1+param2^2));

vector[N] log_likelihood;
for (k in 1:N) 
log_likelihood[k] = lognormal_lpdf(s[k] - e[k] | param1, param2);
}
