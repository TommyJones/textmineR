// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>
#include <R.h>
#include <cmath>
#include <Rcpp.h>
#define ARMA_64BIT_WORD
using namespace Rcpp;


// This is a collapsed gibbs sampler for LDA.
// THIS IS A MODIFICATION FOR AN UPDATE METHOD
// Pre-processing and post-processing is assumed in R
// [[Rcpp::export]]
List fit_lda_c2(List &docs, int &Nk, int &Nd, int &Nv, NumericVector alph, 
               NumericMatrix &beta, int &iterations, int &burnin,
               bool &optimize_alpha, bool &calc_likelihood) {
  
  // print a status so we can see where we are
  // std::cout << "declaring variables\n";
  
  // Declare some initial variables
  
  NumericVector alpha = alph; // don't overwrite inputs
  
  int sumtokens(0); // number of unique tokens
  
  double sum_alpha = sum(alpha); // rcpp sugar here, I guess
  
  NumericMatrix k_beta = beta * Nk; // rcpp sugar here, I guess
  
  // Declare data structures
  int i, d, n, k; // indices for loops
  
  NumericVector p_z(Nk);
  
  IntegerVector z1; // placeholder for topic sampled
  
  int z = 0; // placeholder for topic sampled
  
  int v = 0; // placeholder for word index
  
  IntegerVector topic_sample = seq_len(Nk) -1;
  
  List z_dn(Nd) ; // count of topic/term assignments by document
  
  IntegerMatrix theta_counts(Nd, Nk); // count of topics over documents
  
  IntegerMatrix phi_counts(Nk, Nv); // count of terms over topics
  
  IntegerVector n_d(Nd); // total count of term document totals
  
  IntegerVector n_z(Nk); // total count of topic totals
  
  IntegerMatrix theta_sums(Nd, Nk); // initialize matrix for averaging over iterations if burnin > -1
  
  IntegerMatrix phi_sums(Nk, Nv); // initialize matrix for averaging over iterations if burnin > -1
  
  NumericMatrix ll(iterations / 10, 3); // if calc_likelihood, store it here
  
  double lgbeta(0.0); // if calc_likelihood, we need this term
  
  double lgalpha(0.0); // if calc_likelihood, we need this term
  
  if (calc_likelihood) { // if calc_likelihood, actually populate this stuff
    
    for (n = 0; n < Nv; n++) {
      lgbeta += lgamma(beta[n]);
    }
    
    lgbeta = (lgbeta - lgamma(sum(beta))) * Nk; // rcpp sugar here
    
    for (k = 0; k < Nk; k++) {
      lgalpha += lgamma(alpha[k]);
    }
    
    lgalpha = (lgalpha - lgamma(sum_alpha)) * Nd;
  }
  
  
  // Assign initial values at random
  // std::cout << "assigning initial values \n";
  
  for(d = 0; d < Nd; d++){
    IntegerVector doc = docs[d];
    
    IntegerVector z_dn_row(doc.length());
    
    for(n = 0; n < doc.length(); n++){
      
      // sample a topic at random
      z1 = RcppArmadillo::sample(topic_sample, 1, false, NumericVector::create());
      
      z = z1[0];
      
      theta_counts(d,z) += 1;
      
      v = doc[n];
      
      phi_counts(z,v) += 1;
      
      n_d[d] = n_d[d] + 1; // count the number of tokens in the document
      
      n_z[z] += 1; // count the that topic overall
      
      z_dn_row[n] = z; // # count that topic for that word in the document
      
    }
    
    z_dn[d] = z_dn_row; // update topic-doc-word tracking
    
  }
  
  sumtokens = sum(n_d); // rcpp sugar, also get sum of tokens
  
  // Gibbs iterations
  // std::cout << "beginning Gibbs \n";
  for (i = 0; i < iterations; i++) { // for each iteration
    
    for (d = 0; d < Nd; d++) { // for each document
      // std::cout << "document " << d << "\n";
      
      R_CheckUserInterrupt();
      
      IntegerVector doc = docs[d]; // placeholder for a document
      
      IntegerVector z_dn_row = z_dn[d]; // placeholder for doc-word-topic assigment
      
      for (n = 0; n < n_d[d]; n++) { // for each word in that document
        
        // discount for the n-th word with topic z
        z = z_dn_row[n];
        
        theta_counts(d,z) -= 1; 
        
        phi_counts(z,doc[n]) -= 1;
        
        n_z[z] -= 1;
        
        // sample topic index
        for (k = 0; k < Nk; k++) {
          
          p_z[k] = (phi_counts(k,doc[n]) + beta(k,doc[n])) / (n_z[k] + k_beta(k,doc[n])) *
            (theta_counts(d,k) + alpha[k]) / (n_d[d] + sum_alpha);
                                                                
        }
        
        
        // update counts
        z1 = RcppArmadillo::sample(topic_sample, 1, false, p_z);
        
        z = z1[0];
        
        theta_counts(d,z) += 1; // update document topic count
        
        phi_counts(z,doc[n]) += 1; // update topic word count
        
        // n_d[d] = n_d[d] + 1; // count that topic in that document overall
        
        n_z[z] += 1; // count the that topic overall
        
        z_dn_row[n] = z; // # count that topic for that word in the document
        
      }
    }
    
    // if using burnin, update sums
    if (burnin > -1 && i >= burnin) {
      
      for (k = 0; k < Nk; k++) {
        
        for (d = 0; d < Nd; d++) {
          theta_sums(d,k) += theta_counts(d,k);
        }
        
        for (v = 0; v < Nv; v++) {
          phi_sums(k,v) += phi_counts(k,v);
        }
      }
      
    }
    
    // if calculating log likelihood, do so every 10 iterations
    if (calc_likelihood && i % 10 == 0) {
      
      // get phi probability matrix @ this iteration
      NumericMatrix phi_prob(Nk,Nv);
      
      double denom(0.0);
      
      double lp_beta(0.0); // log probability of beta prior
      
      for (k = 0; k < Nk; k++) {
        
        // get the denominator
        for (v = 0; v < Nv; v++) {
          denom += phi_counts(k,v) + beta[v];
        }
        
        // get the probability
        for (v = 0; v < Nv; v++) {
          phi_prob(k,v) = ((double)phi_counts(k,v) + beta[v]) / denom;
          
          lp_beta += (beta[v] - 1) * log(phi_prob(k,v));
        }
      }
      
      lp_beta += lgbeta;
      
      // for each document, get the log probability of the words
      
      double lp_alpha(0.0); // log probability of alpha prior
      
      double lpd(0.0); // log probability of documents
      
      for (d = 0; d < Nd; d++) {
        
        NumericVector theta_prob(Nk); // probability of each topic in this document
        
        IntegerVector doc = docs[d];
        
        NumericVector lp(doc.length()); // log probability of each word under the model
        
        double denom(0.0);
        
        for (k = 0; k < Nk; k++) {
          denom += (double)theta_counts(d,k) + alpha[k];
        }
        
        for (k = 0; k < Nk; k++) {
          theta_prob[k] = ((double)theta_counts(d,k) + alpha[k]) / denom;
          
          lp_alpha += (alpha[k] - 1) * log(theta_prob[k]);
        }
        
        for (n = 0; n < doc.length(); n++) {
          
          lp[n] = 0.0;
          
          for (k = 0; k < Nk; k++) {
            
            lp[n] += theta_prob[k] * phi_prob(k,doc[n]);
            
          }
          
          lpd += log(lp[n]);
        }
        
      }
      
      lp_alpha += lgalpha;
      
      ll(i / 10, 0) = i;
      
      ll(i / 10, 1) = lpd; // log probability of whole corpus under the model w/o priors
      
      ll(i / 10, 2) = lpd + lp_alpha + lp_beta;
      
    }
    
    // if optimizing alpha, do so
    if (optimize_alpha) {
      NumericVector new_alpha(Nk);
      
      for (k = 0; k < Nk; k++) {
        for (d = 0; d < Nd; d++) {
          new_alpha[k] += (double)theta_counts(d,k) / (double)sumtokens * (double)sum_alpha;
        }
      }
      
      alpha = new_alpha;
      
    }
    
  }
  
  
  // return the result
  // std::cout << "prepare result\n";
  
  if (burnin > -1) {
    int i_diff = iterations - burnin;
    
    NumericMatrix theta(Nd,Nk);
    
    NumericMatrix phi(Nk,Nv);
    
    // average over chain after burnin 
    for (k = 0; k < Nk; k++) {
      
      for (d = 0; d < Nd; d++) {
        theta(d,k) = (theta_sums(d,k) / i_diff);
      }
      
      for (v = 0; v < Nv; v++) {
        phi(k,v) = (phi_sums(k,v) / i_diff);
      }
    }
    
    return List::create(Named("theta") = theta,
                        Named("phi") = phi,
                        Named("log_likelihood") = ll,
                        Named("alpha") = alpha,
                        Named("beta") = beta);
    
  } else {
    return List::create(Named("theta") = theta_counts,
                        Named("phi") = phi_counts,
                        Named("log_likelihood") = ll,
                        Named("alpha") = alpha,
                        Named("beta") = beta);
  }
  
  
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

library(textmineR)

# load dtm
dtm <- textmineR::nih_sample_dtm

# fit a model from half the documents
d <- dtm[1:50,]

d <- d[,colSums(d) > 0] # drop words that aren't in the vocab

m <- FitLdaModel(d, k = 10, 
                 iterations = 200, burnin = 175,
                 optimize_alpha = TRUE, 
                 calc_likelihood = FALSE,
                 calc_coherence = TRUE,
                 calc_r2 = FALSE)

docs <- Dtm2Lexicon(dtm) # new docs plus old docs

# align vocab in intelligent way for adding new vocab
beta <- m$phi

v_diff <- setdiff(colnames(dtm), colnames(m$phi))

m_add <- matrix(0, nrow = nrow(m$phi), ncol = length(v_diff))

colnames(m_add) <- v_diff

m_add <- m_add + mean(m$phi) # arbitrary uniform prior over new words, hack MUST FIX

beta <- cbind(beta, m_add)

beta <- beta[,colnames(dtm)]

# add 3 topics HACK HACK HACK
m_add <- matrix(mean(m$phi) ,nrow = 3, ncol = ncol(beta))

beta <- rbind(beta, m_add)

# magnitude of rows of beta prior, also hack MUST FIX
beta <- beta / rowSums(beta) # normalize rows

beta <- (mean(m$beta) * ncol(beta)) * beta

# constants for c function
Nd <- length(docs)

Nk <- nrow(beta)

Nv <- ncol(dtm)
  
alpha <- c(m$alpha, rep(mean(m$alpha),Nk - length(m$alpha))) # rep(0.1, Nk)

iterations = 200

burnin = 175

optimize_alpha = TRUE

calc_likelihood = FALSE


### run C++ gibbs sampler ----

result <- fit_lda_c2(docs = docs, Nk = Nk, Nd = Nd, Nv = Nv, 
                    alph = alpha, beta = beta,
                    iterations = iterations, burnin = burnin,
                    optimize_alpha = optimize_alpha,
                    calc_likelihood = calc_likelihood)
    

phi <- result$phi

theta <- result$theta

phi <- phi + beta # t(t(phi) + beta)

phi <- phi / rowSums(phi, na.rm = TRUE)

phi[ is.na(phi) ] <- 0

theta <- t(t(theta) + alpha)

theta <- theta / rowSums(theta, na.rm = TRUE)

theta[ is.na(theta) ] <- 0

colnames(phi) <- colnames(dtm)

rownames(phi) <- paste0("t_", seq_len(Nk))

colnames(theta) <- rownames(phi)

rownames(theta) <- names(docs) # rownames(dtm)


*/
