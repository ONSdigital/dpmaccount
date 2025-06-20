
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace Eigen;
using namespace tmbutils;
using atomic::tiny_ad::isfinite; // copied from adcomp/tmb_examples/adaptive_integration.cpp


// Functions for calculating log-posterior ------------------------------------

// Functions to calculate contribution to
// log-posterior from data models.
// Assume inputs all valid (checking done in R).

// Models with no parameters estimated by TMB

template <class Type>
Type logpost_nopar_norm(vector<Type> data,
			vector<Type> val,
			vector<int> is_obs,			
			vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> sd = consts.segment(n, n);
  Type ans = 0;
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      ans += dnorm(data[i], ratio[i] * val[i], sd[i], true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_nopar_t(vector<Type> data,
		     vector<Type> val,
		     vector<int> is_obs,			
		     vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> scale = consts.segment(n, n);
  Type df = consts[2 * n];				  
  Type ans = 0;
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type x = (data[i] - ratio[i] * val[i]) / scale[i];
      ans += dt(x, df, true) - log(scale[i]);
    }
  }
  return ans;
}

template <class Type>
Type logpost_nopar_nbinom(vector<Type> data,
                          vector<Type> val,
                          vector<int> is_obs,
                          vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> disp = consts.segment(n, n);
  Type ans = 0;
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type x = data[i];
      Type mu = (ratio[i] * val[i]);
      Type var = mu + ((mu * mu) * disp[i]);
      ans += dnbinom2(x, mu, var, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_nopar_poisson(vector<Type> data,
                          vector<Type> val,
                          vector<int> is_obs,
                          vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  Type ans = 0;
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type x = data[i];
      Type lambda = (ratio[i] * val[i]);
      ans += dpois(x, lambda, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_nopar_lognorm(vector<Type> data,
                           vector<Type> val,
                           vector<int> is_obs,
                           vector<Type> consts) {
  
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> sd = consts.segment(n, n);
  
  Type ans = 0;
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type y = log(data[i]);
      Type x = log(val[i]);
      Type mu = log(ratio[i]); 
      
      ans -= ((y - x - mu) * (y - x - mu)) / (2 * (sd[i] * sd[i]));
    }
  }
  return ans;
}

template <class Type>
Type logpost_nopar(vector<Type> data,
		   vector<Type> val,
		   vector<int> is_obs,
		   vector<Type> consts,
		   int i_mod) {
  Type ans = 0;
  switch(i_mod) {
  case 1:
    ans = logpost_nopar_norm(data, val, is_obs, consts);
    break;
  case 2:
    ans = logpost_nopar_t(data, val, is_obs, consts);
    break;
  case 3:
    ans = logpost_nopar_nbinom(data, val, is_obs, consts);
    break;
  case 4:
    ans = logpost_nopar_poisson(data, val, is_obs, consts);
    break;
  case 5:
    ans = logpost_nopar_lognorm(data, val, is_obs, consts);
    break;
  default:
    error("function 'logpost_nopar' cannot handle i_mod = %d", i_mod);
  }
  return ans;
}


// Models with parameters estimated by TMB

template <class Type>
Type logpost_par_norm(vector<Type> data,
		      vector<Type> val,
		      vector<int> is_obs,
		      vector<Type> par,
		      vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> sd = consts.segment(n, n);
  Type scale_mult_ratio = consts[2 * n];
  Type scale_mult_sd = consts[2 * n + 1];
  bool has_mult_ratio = scale_mult_ratio > 0;
  bool has_mult_sd = scale_mult_sd > 0;
  Type mult_ratio;
  Type mult_sd;
  if (has_mult_ratio && has_mult_sd) {
    mult_ratio = par[0];
    mult_sd = par[1];
  }
  else if (has_mult_ratio && !has_mult_sd) {
    mult_ratio = par[0];
    mult_sd = 0;
  }
  else {
    mult_ratio = 0;
    mult_sd = par[0];
  }
  Type exp_mult_ratio = exp(mult_ratio);
  Type exp_mult_sd = exp(mult_sd);
  Type ans = 0;
  if (has_mult_ratio)
    ans += dnorm(mult_ratio, Type(0), scale_mult_ratio, true);
  if (has_mult_sd)
    ans += dnorm(mult_sd, Type(0), scale_mult_sd, true);
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      ans += dnorm(data[i], exp_mult_ratio * ratio[i] * val[i], exp_mult_sd * sd[i], true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_par_t(vector<Type> data,
                      vector<Type> val,
                      vector<int> is_obs,
                      vector<Type> par,
                      vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> scale = consts.segment(n, n);
  Type df = consts[2 * n];
  Type scale_mult_ratio = consts[2 * n + 1];
  bool has_mult_ratio = scale_mult_ratio > 0;
  Type mult_ratio;
  if (has_mult_ratio ) {
    mult_ratio = par[0];
  }
  else{
    mult_ratio = 0;
  }
  Type exp_mult_ratio = exp(mult_ratio);
  Type ans = 0;
  if (has_mult_ratio)
    ans += dnorm(mult_ratio, Type(0), scale_mult_ratio, true);
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      // Note use of 'location-scale' version of t-distribution and necessary addition of Jacobian (log(scale))
      Type x = (data[i] - exp_mult_ratio * ratio[i] * val[i]) / scale[i];
      ans += dt(x, df, true) - log(scale[i]);
    }
  }
  return ans;
}

template <class Type>
Type logpost_par_nbinom(vector<Type> data,
                   vector<Type> val,
                   vector<int> is_obs,
                   vector<Type> par,
                   vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> disp = consts.segment(n, n);
  Type scale_mult_ratio = consts[2 * n];
  bool has_mult_ratio = scale_mult_ratio > 0;
  Type mult_ratio;
  if (has_mult_ratio ) {
    mult_ratio = par[0];
  }
  else{
    mult_ratio = 0;
  }
  Type exp_mult_ratio = exp(mult_ratio);
  Type ans = 0;
  if (has_mult_ratio)
    ans += dnorm(mult_ratio, Type(0), scale_mult_ratio, true);
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type x = data[i];
      Type mu = (exp_mult_ratio * ratio[i] * val[i]);
      Type var = mu + ((mu * mu) * disp[i]);
      ans += dnbinom2(x, mu, var, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_par_poisson(vector<Type> data,
                        vector<Type> val,
                        vector<int> is_obs,
                        vector<Type> par,
                        vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  Type scale_mult_ratio = consts[n];
  bool has_mult_ratio = scale_mult_ratio > 0;
  Type mult_ratio;
  if (has_mult_ratio ) {
    mult_ratio = par[0];
  }
  else{
    mult_ratio = 0;
  }
  Type exp_mult_ratio = exp(mult_ratio);
  Type ans = 0;
  if (has_mult_ratio)
    ans += dnorm(mult_ratio, Type(0), scale_mult_ratio, true);
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type x = data[i];
      Type lambda = (exp_mult_ratio * ratio[i] * val[i]);
      ans += dpois(x, lambda, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_par_lognorm(vector<Type> data,
                         vector<Type> val,
                         vector<int> is_obs,
                         vector<Type> par,
                         vector<Type> consts) {
  int n = data.size();
  vector<Type> ratio = consts.segment(0, n);
  vector<Type> sd = consts.segment(n, n);
  Type scale_mult_ratio = consts[2 * n];
  bool has_mult_ratio = scale_mult_ratio > 0;
  Type mult_ratio;
  if (has_mult_ratio) {
    mult_ratio = par[0];
  }
  else {
    mult_ratio = 0;
  }
  Type ans = 0;
  if (has_mult_ratio)
    ans += dnorm(mult_ratio, Type(0), scale_mult_ratio, true);
  
  for (int i = 0; i < n; i++) {
    if (is_obs[i]) {
      Type y = log(data[i]);
      Type x = log(val[i]);
      Type mu = log(ratio[i]); 
      
      ans -= ((y - x - mu - mult_ratio) * (y - x - mu - mult_ratio)) / (2 * (sd[i] * sd[i]));
    }
  }
  return ans;
}

template <class Type>
Type logpost_haspar(vector<Type> data,
		    vector<Type> val,
		    vector<int> is_obs,
		    vector<Type> par,
		    vector<Type> consts,
		    int i_mod) {
  Type ans = 0;
  switch(i_mod) {
  case 101:
    ans = logpost_par_norm(data, val, is_obs, par, consts);
    break;
  case 201:
    ans = logpost_par_t(data, val, is_obs, par, consts);
    break;
  case 301:
    ans = logpost_par_nbinom(data, val, is_obs, par, consts);
    break;
  case 401:
    ans = logpost_par_poisson(data, val, is_obs, par, consts);
    break;
  case 501:
    ans = logpost_par_lognorm(data, val, is_obs, par, consts);
    break;
  default:
    error("function 'logpost_haspar' cannot handle i_mod = %d", i_mod);
  }
  return ans;
}


// Objective function ---------------------------------------------------------

template<class Type>
Type objective_function<Type>::operator() ()
{

  // input values -------------------------------------------------------------

  // data and fixed quantities

  DATA_SCALAR(mean_stk_init);
  DATA_SCALAR(sd_stk_init);
  DATA_IVECTOR(has_bth);
  DATA_VECTOR(val_bth);
  DATA_VECTOR(val_dth);
  DATA_VECTOR(mean_bth);
  DATA_VECTOR(mean_dth);
  DATA_VECTOR(mean_ins);
  DATA_VECTOR(mean_outs);
  DATA_VECTOR(disp_bth);
  DATA_VECTOR(disp_dth);
  DATA_VECTOR(disp_ins);
  DATA_VECTOR(disp_outs);
  DATA_VECTOR(data_all_stk);
  DATA_VECTOR(data_all_ins);
  DATA_VECTOR(data_all_outs);
  DATA_IVECTOR(is_obs_all_stk);
  DATA_IVECTOR(is_obs_all_ins);
  DATA_IVECTOR(is_obs_all_outs);
  DATA_FACTOR(idx_data_all_stk);
  DATA_FACTOR(idx_data_all_ins);
  DATA_FACTOR(idx_data_all_outs);
  DATA_FACTOR(idx_par_all_stk);
  DATA_FACTOR(idx_par_all_ins);
  DATA_FACTOR(idx_par_all_outs);
  DATA_VECTOR(const_all_stk);
  DATA_VECTOR(const_all_ins);
  DATA_VECTOR(const_all_outs);
  DATA_FACTOR(idx_const_all_stk);
  DATA_FACTOR(idx_const_all_ins);
  DATA_FACTOR(idx_const_all_outs);
  DATA_IVECTOR(i_mod_all_stk);
  DATA_IVECTOR(i_mod_all_ins);
  DATA_IVECTOR(i_mod_all_outs);
  DATA_IVECTOR(has_par_all_stk);
  DATA_IVECTOR(has_par_all_ins);
  DATA_IVECTOR(has_par_all_outs);

  // parameters returned to R
  
  PARAMETER(log_val_stk_init);
  PARAMETER_VECTOR(log_val_ins);
  PARAMETER_VECTOR(log_val_outs);
  PARAMETER_VECTOR(par_all_stk);
  PARAMETER_VECTOR(par_all_ins);
  PARAMETER_VECTOR(par_all_outs);


  // quantities used in calculations ------------------------------------------

  // disp = 1/shape = scale = 1/rate in gamma distribution
  // = 1/size in negative binomial distribution

  // fixed values
  int K = val_dth.size();
  int n_dataset_stk = i_mod_all_stk.size();
  int n_dataset_ins = i_mod_all_ins.size();
  int n_dataset_outs = i_mod_all_outs.size();
  vector<Type> size_bth = 1 / disp_bth;
  vector<Type> size_dth = 1 / disp_dth;
  vector<Type> size_ins = 1 / disp_ins;
  vector<Type> size_outs = 1 / disp_outs;
  Type val_stk_init = exp(log_val_stk_init);
  vector<Type> val_ins = exp(log_val_ins);
  vector<Type> val_outs = exp(log_val_outs);
  int sd_stk_init_positive = sd_stk_init > 0;
  int sd_stk_init_finite = isfinite(sd_stk_init);

  vector<vector<Type> > data_split_stk = split(data_all_stk, idx_data_all_stk);       
  vector<vector<Type> > data_split_ins = split(data_all_ins, idx_data_all_ins);       
  vector<vector<Type> > data_split_outs = split(data_all_outs, idx_data_all_outs);
  vector<vector<int> > is_obs_split_stk = split(is_obs_all_stk, idx_data_all_stk);       
  vector<vector<int> > is_obs_split_ins = split(is_obs_all_ins, idx_data_all_ins);       
  vector<vector<int> > is_obs_split_outs = split(is_obs_all_outs, idx_data_all_outs);
  vector<vector<Type> > const_split_stk = split(const_all_stk, idx_const_all_stk);       
  vector<vector<Type> > const_split_ins = split(const_all_ins, idx_const_all_ins);       
  vector<vector<Type> > const_split_outs = split(const_all_outs, idx_const_all_outs);

  // parameters for data models
  vector<vector<Type> > par_split_stk = split(par_all_stk, idx_par_all_stk);       
  vector<vector<Type> > par_split_ins = split(par_all_ins, idx_par_all_ins);       
  vector<vector<Type> > par_split_outs = split(par_all_outs, idx_par_all_outs);
  
  // population accounting equation
  vector<Type> val_stk(K+1);
  val_stk[0] = val_stk_init;
  vector<Type> exposure(K);
  
  for (int k = 0; k < K; k++){
    val_stk[k+1] = val_stk[k] - val_dth[k] + val_ins[k] - val_outs[k];
    exposure[k] = 0.25 * (val_stk[k] + val_stk[k+1]);
  }
  
  // negative log posterior ---------------------------------------------------
  
  Type ans = 0;

  // contribution from val_stk_init
  if (sd_stk_init_positive) {
    ans -= log_val_stk_init;
    if (sd_stk_init_finite) {
      ans -= dnorm(val_stk_init, mean_stk_init, sd_stk_init, true);
    }
  }
  
  // contribution from val_bth
  for (int k = 0; k < K; k++) {
    if (has_bth[k]) {
      Type prob_bth_k = size_bth[k] / (mean_bth[k] * exposure[k] + size_bth[k]);
      ans -= dnbinom(val_bth[k], size_bth[k], prob_bth_k, true);
    }
  }

  // contribution from val_dth, val_ins, val_outs

  vector<Type> mean_ins_lexis = 0.5 * mean_ins; // ins for one Lexis triangle; mean_ins refers to a whole year
  vector<Type> prob_ins = size_ins / (mean_ins_lexis + size_ins);
  ans -= dnbinom(val_ins, size_ins, prob_ins, true).sum();
  vector<Type> prob_dth = size_dth / (mean_dth * exposure + size_dth);
  vector<Type> prob_outs = size_outs / (mean_outs * exposure + size_outs);
  ans -= dnbinom(val_dth, size_dth, prob_dth, true).sum();
  ans -= dnbinom(val_outs, size_outs, prob_outs, true).sum();
  ans -= log_val_ins.sum() + log_val_outs.sum(); // Jacobians

  
  // contribution from data - stock

  // if initial stock is known, ie sd_stk_init == 0,
  // implying that the initial stock is births,
  // then data models are only used with the K subsequent
  // values for stock; otherwise data models are used
  // with all K+1 values
  int n_val_stk_datamod = sd_stk_init_positive ? K+1 : K;
  vector<Type> val_stk_datamod(n_val_stk_datamod);
  if (sd_stk_init_positive)
    val_stk_datamod = val_stk;
  else
    val_stk_datamod = val_stk.segment(1, K);
  for (int i_dataset_stk = 0; i_dataset_stk < n_dataset_stk; i_dataset_stk++) {
    vector<Type> data_stk = data_split_stk[i_dataset_stk];
    vector<int> is_obs_stk = is_obs_split_stk[i_dataset_stk];
    vector<Type> const_stk = const_split_stk[i_dataset_stk];
    int i_mod_stk = i_mod_all_stk[i_dataset_stk];
    if (has_par_all_stk[i_dataset_stk]) {
      vector<Type> par_stk = par_split_stk[i_dataset_stk];
      ans -= logpost_haspar(data_stk,
			    val_stk_datamod,
			    is_obs_stk,
			    par_stk,
			    const_stk,
			    i_mod_stk);
    }
    else {
      ans -= logpost_nopar(data_stk,
			   val_stk_datamod,
			   is_obs_stk,
			   const_stk,
			   i_mod_stk);
    }
  }

  // contribution from data - in-migration
  
  for (int i_dataset_ins = 0; i_dataset_ins < n_dataset_ins; i_dataset_ins++) {
    vector<Type> data_ins = data_split_ins[i_dataset_ins];
    vector<int> is_obs_ins = is_obs_split_ins[i_dataset_ins];
    vector<Type> const_ins = const_split_ins[i_dataset_ins];
    int i_mod_ins = i_mod_all_ins[i_dataset_ins];
    if (has_par_all_ins[i_dataset_ins]) {
      vector<Type> par_ins = par_split_ins[i_dataset_ins];
      ans -= logpost_haspar(data_ins,
			    val_ins,
			    is_obs_ins,
			    par_ins,
			    const_ins,
			    i_mod_ins);
    }
    else {
      ans -= logpost_nopar(data_ins,
			   val_ins,
			   is_obs_ins,
			   const_ins,
			   i_mod_ins);
    }
  }

  // contribution from data - out-migration
  
  for (int i_dataset_outs = 0; i_dataset_outs < n_dataset_outs; i_dataset_outs++) {
    vector<Type> data_outs = data_split_outs[i_dataset_outs];
    vector<int> is_obs_outs = is_obs_split_outs[i_dataset_outs];
    vector<Type> const_outs = const_split_outs[i_dataset_outs];
    int i_mod_outs = i_mod_all_outs[i_dataset_outs];
    if (has_par_all_outs[i_dataset_outs]) {
      vector<Type> par_outs = par_split_outs[i_dataset_outs];
      ans -= logpost_haspar(data_outs,
			    val_outs,
			    is_obs_outs,
			    par_outs,
			    const_outs,
			    i_mod_outs);
    }
    else {
      ans -= logpost_nopar(data_outs,
			   val_outs,
			   is_obs_outs,
			   const_outs,
			   i_mod_outs);
    }
  }
  
  return ans;
}
