
// will this fix my underflow?
for (k = 0; k < Nk; k++) {
  
  p_z[k] = log((phi_counts(k,doc[n]) + beta(k,doc[n]))) - 
    log(n_z[k] + k_beta(k,doc[n])) + 
    log((theta_counts(d,k) + alpha[k])) - 
    log(n_d[d] + sum_alpha);
  
  p_z[k] = exp(p_z[k]);
  
}

