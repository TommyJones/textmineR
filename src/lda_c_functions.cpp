// Functions to make a collapsed gibbs sampler for LDA

// NOTE TO TOMMY: USE ROXYGEN DOCUMENTATION FOR ALL EXPORTED FUNCTIONS
// DO SO WHEN INTEGRATING BACK INTO textmineR

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
#include <R.h>
#include <cmath>
#include <Rcpp.h>
#define ARMA_64BIT_WORD
using namespace Rcpp;

// Make a lexicon for looping over in the gibbs sampler
//[[Rcpp::export]]
List create_lexicon(IntegerMatrix &Cd, 
                    NumericMatrix &Phi, 
                    arma::sp_mat &dtm,
                    NumericVector alpha,
                    bool freeze_topics) {
  
  // ***************************************************************************
  // Initialize some variables
  // ***************************************************************************
  
  double sum_alpha = sum(alpha);
  
  List docs(dtm.n_rows); 
  
  List Zd(dtm.n_rows);
  
  int Nk = Cd.ncol();
  
  NumericVector qz(Nk);
  
  IntegerVector topic_index = seq_len(Nk) - 1;
  
  // ***************************************************************************
  // Go through each document and split it into a lexicon and then sample a 
  // topic for each token within that document
  // ***************************************************************************
  for (int d = 0; d < dtm.n_rows; d++) {
    
    // make a temporary vector to hold token indices
    int nd = 0;
    
    for (int v = 0; v < dtm.n_cols; v++) {
      nd += dtm(d,v);
    }
    
    IntegerVector doc(nd);
    
    IntegerVector zd(nd);
    
    IntegerVector z(1);
    
    // fill in with token indices
    int j = 0; // index of doc, advances when we have non-zero entries 
    
    for (int v = 0; v < dtm.n_cols; v++) {
      
      if (dtm(d,v) > 0) { // if non-zero, add elements to doc
        
        // calculate probability of topics based on initially-sampled Phi and Cd
        for (int k = 0; k < Nk; k++) {
          qz[k] = Phi(k, v) * ((double)Cd(d, k) + alpha[k]) / ((double)nd + sum_alpha - 1);
        }
        
        int idx = j + dtm(d,v); // where to stop the loop below
        
        while (j < idx) {
          
          doc[j] = v;
          
          z = RcppArmadillo::sample(topic_index, 1, false, qz);
          
          zd[j] = z[0];
          
          j += 1;
        }
        
      }
    }
    
    // fill in docs[d] with the matrix we made
    docs[d] = doc;
    
    Zd[d] = zd;
    
    R_CheckUserInterrupt();
    
  }
  
  // ***************************************************************************
  // Calculate Cd, Cv, and Ck from the sampled topics
  // ***************************************************************************
  IntegerMatrix Cd_out(dtm.n_rows, Nk);
  
  IntegerVector Ck(Nk);
  
  IntegerMatrix Cv(Nk, dtm.n_cols);
  
  for (int d = 0; d < Zd.length(); d++) {
    
    IntegerVector zd = Zd[d]; 
    
    IntegerVector doc = docs[d];
    
    for (int n = 0; n < zd.length(); n++) {
      
      Cd_out(d, zd[n]) += 1;
      
      Ck[zd[n]] += 1;
      
      if (! freeze_topics) {
        Cv(zd[n], doc[n]) += 1;
      }
      
    } 
    
  }
  
  // ***************************************************************************
  // Prepare output and expel it from this function
  // ***************************************************************************
  
  return List::create(Named("docs") = docs,
                      Named("Zd") = Zd,
                      Named("Cd") = Cd_out,
                      Named("Cv") = Cv,
                      Named("Ck") = Ck);
  
}

// main lda function
// assumes that count matrices are handed to it
// [[Rcpp::export]]
List fit_lda_c(List &docs,
               int &Nk,
               NumericMatrix &beta,
               NumericVector alpha,
               IntegerMatrix Cd,
               IntegerMatrix Cv,
               IntegerVector Ck,
               List Zd,
               NumericMatrix &Phi,
               int &iterations,
               int &burnin,
               bool &freeze_topics,
               bool &calc_likelihood,
               bool &optimize_alpha) {
  
  // ***********************************************************************
  // Check quality of inputs to minimize risk of crashing the program
  // ***********************************************************************
  
  
  
  
  // ***********************************************************************
  // Variables and other set up
  // ***********************************************************************
  
  int Nv = Cv.cols();
  
  int Nd = Cd.rows();
  
  NumericVector k_alpha = alpha * Nk;
  
  NumericMatrix v_beta = beta * Nv;
  
  double sum_alpha = sum(alpha);
  
  double sum_beta = sum(beta(1, _));
  
  int sumtokens = sum(Ck);
  
  double phi_kv(0.0);
  
  int t, d, n, k, v; // indices for loops
  
  NumericVector qz(Nk);
  
  IntegerVector topic_index = seq_len(Nk) - 1;
  
  qz = qz + 1; // uniform initialization
  
  IntegerVector z(1); // for sampling topics
  
  // related to burnin and averaging
  IntegerMatrix Cv_sum(Nk, Nv);
  
  NumericMatrix Cv_mean(Nk, Nv);
  
  IntegerMatrix Cd_sum(Nd, Nk);
  
  NumericMatrix Cd_mean(Nd, Nk);
  
  // related to the likelihood calculation
  NumericMatrix log_likelihood(2, iterations);
  
  double lgbeta(0.0); // calculated immediately below
  
  double lgalpha(0.0); // calculated immediately below
  
  double lg_alpha_len(0.0); // calculated immediately below
  
  double lg_beta_count1(0.0); // calculated at the bottom of the iteration loop
  
  double lg_beta_count2(0.0); // calculated at the bottom of the iteration loop
  
  double lg_alpha_count(0.0); // calculated at the bottom of the iteration loop
  
  if (calc_likelihood && ! freeze_topics) { // if calc_likelihood, actually populate this stuff
    
    for (n = 0; n < Nv; n++) {
      lgbeta += lgamma(beta[n]);
    }
    
    lgbeta = (lgbeta - lgamma(sum_beta)) * Nk; // rcpp sugar here
    
    for (k = 0; k < Nk; k++) {
      lgalpha += lgamma(alpha[k]);
    }
    
    lgalpha = (lgalpha - lgamma(sum_alpha)) * Nd;
    
    for (d = 0; d < Nd; d++) {
      IntegerVector doc = docs[d];
      
      lg_alpha_len += lgamma(sum_alpha + doc.length());
    }
    
    lg_alpha_len *= -1;
  }
  
  
  
  // ***********************************************************************
  // BEGIN ITERATIONS
  // ***********************************************************************
  
  for (t = 0; t < iterations; t++) {
    
    for (d = 0; d < Nd; d++) {
      
      R_CheckUserInterrupt();
      
      IntegerVector doc = docs[d];
      
      IntegerVector zd = Zd[d];
      
      for (n = 0; n < doc.length(); n++) {
        
        // discount counts from previous run ***
        Cd(d, zd[n]) -= 1; 
        
        
        if (! freeze_topics) {
          Cv(zd[n], doc[n]) -= 1; 
          
          Ck[zd[n]] -= 1;
        }
        
        
        // update probabilities of each topic ***
        for (int k = 0; k < qz.length(); k++) {
          
          // get the correct term depending on if we freeze topics or not
          if (freeze_topics) {
            phi_kv = Phi(k, doc[n]);
          } else {
            phi_kv = ((double)Cv(k, doc[n]) + beta(k, doc[n])) /
              ((double)Ck[k] + sum_beta);
          }
          
          qz[k] =  phi_kv * ((double)Cd(d, k) + alpha[k]) / 
            ((double)doc.length() + sum_alpha - 1);
          
        }
        
        
        // sample a topic ***
        z = RcppArmadillo::sample(topic_index, 1, false, qz);
        
        // update counts ***
        Cd(d, z[0]) += 1; 
        
        if (! freeze_topics) {
          
          Cv(z[0], doc[n]) += 1; 
          
          Ck[z[0]] += 1;
          
        }
        
        // record this topic for this token/doc combination
        zd[n] = z[0];
        
      } // end loop over each token in doc
    } // end loop over docs
    
    // calc likelihood ***
    if (calc_likelihood && ! freeze_topics) {
      
      // calculate lg_beta_count1, lg_beta_count2, lg_alph_count for this iter
      // start by zeroing them out
      lg_beta_count1 = 0.0;
      lg_beta_count2 = 0.0;
      lg_alpha_count = 0.0;
      
      for (k = 0; k < Nk; k++) {
        
        lg_beta_count1 += lgamma(sum_beta + Ck[k]);
        
        for (d = 0; d < Nd; d++) {
          lg_alpha_count += lgamma(alpha[k] + Cd(d,k));
        }
        
        for (v = 0; v < Nv; v++) {
          lg_beta_count2 += lgamma(beta(k,v) + Cv(k,v));
        }
        
      }
      
      lg_beta_count1 *= -1;
      
      log_likelihood(0, t) = t;
      
      log_likelihood(1, t) = lgalpha + lgbeta + lg_alpha_len + lg_alpha_count + 
        lg_beta_count1 + lg_beta_count2;
      
    }
    // optimize alpha ***
    if (optimize_alpha && ! freeze_topics) {
      
      NumericVector new_alpha(Nk);
      
      for (k = 0; k < Nk; k++) {
        
        new_alpha[k] += (double)Ck[k] / (double)sumtokens * (double)sum_alpha;
        
        new_alpha[k] += (new_alpha[k] + alpha[k]) / 2;
        
      }
      
      alpha = new_alpha;
      
    }
    
    // aggregate counts after burnin ***
    if (burnin > -1 && t >= burnin) {
      
      for (k = 0; k < Nk; k++) {
        for (d = 0; d < Nd; d++) {
          
          Cd_sum(d, k) += Cd(d, k);
          
        }
        if (! freeze_topics) {
          for (v = 0; v < Nv; v++) {
            
            Cv_sum(k, v) += Cv(k, v);
            
          }
        }
      }
      
    }
    
  } // end iterations
  
  // ***********************************************************************
  // Cleanup and return list
  // ***********************************************************************
  
  // change sum over iterations to average over iterations ***
  
  if (burnin >-1) {
    
    double diff = iterations - burnin;
    
    // average over chain after burnin 
    for (k = 0; k < Nk; k++) {
      
      for (d = 0; d < Nd; d++) {
        Cd_mean(d,k) = ((double)Cd_sum(d,k) / diff);
      }
      
      for (v = 0; v < Nv; v++) {
        Cv_mean(k,v) = ((double)Cv_sum(k,v) / diff);
      }
    }
  }
  
  // Return the final list ***
  
  return List::create(Named("Cd") = Cd,
                      Named("Cv") = Cv,
                      Named("Ck") = Ck,
                      Named("Cd_mean") = Cd_mean,
                      Named("Cv_mean") = Cv_mean,
                      Named("log_likelihood") = log_likelihood,
                      Named("alpha") = alpha,
                      Named("beta") = beta);  
}

