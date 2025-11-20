// This is a full hierarchical GAM time-series, with spatial gam parameters 
// as well as a gam-based Seasonal adjustment and sruvey-wide random year-effects

// iCAR function, from Morris et al. 2019
// Morris, M., K. Wheeler-Martin, D. Simpson, S. J. Mooney, A. Gelman, and C. DiMaggio (2019). 
// Bayesian hierarchical spatial models: Implementing the Besag York Mollié model in stan. 
// Spatial and Spatio-temporal Epidemiology 31:100301.


 functions {
   real icar_normal_lpdf(vector bb, int ns, array[] int n1, array[] int n2) {
     return -0.5 * dot_self(bb[n1] - bb[n2])
       + normal_lpdf(sum(bb) | 0, 0.001 * ns); //soft sum to zero constraint on bb
  }
 }


data {
  int<lower=0> nstrata;
  int<lower=0> ncounts;
  int<lower=0> nsites;
  int<lower=0> nyears;
  
  // data for spline s(date)
  int<lower=1> ndays;  // number of days in the basis function for season
  int<lower=1> nknots_season;  // number of knots in the basis function for season
  matrix[ndays, nknots_season] season_basispred; // basis function matrix
  
  // data for spline s(year)
  int<lower=1> nknots_year;  // number of knots in the basis function for year
  matrix[nyears, nknots_year] year_basispred; // basis function matrix
  
  //data for spatial iCAR among strata
  int<lower=1> N_edges;
  array[N_edges] int<lower=1, upper=nstrata> node1;  // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=nstrata> node2;// and node1[i] < node2[i]

  array[ncounts] int<lower=0> count;              // count observations
  array[ncounts] int<lower=1> strat;              // strata indicators
  array[ncounts] int<lower=1> site;              // site indicators
  //real year[ncounts];              // centered years
  array[ncounts] int<lower=1> year_raw; // year index
  array[ncounts] int<lower=1> date;  // day indicator in the season
  array[ncounts] int<lower=1> seas_strat;

  
  //indexes for re-scaling predicted counts within strata based on site-level intercepts
  int<lower=1> max_sites; //dimension 1 of sites matrix
  array[max_sites,nstrata] int<lower=0> sites; //matrix of which sites are in each stratum
  array[nstrata] int<lower=1> nsites_strat; //number of unique sites in each stratum
  array[nstrata,2] int<lower=1> seasons; //matrix of which strata have season pattern 1 or 2
  
}

parameters {
  vector[nsites] alpha_raw;             // intercepts
  vector[nyears] year_effect_raw;             // continental year-effects
  matrix[nstrata,nknots_year] b_raw;         // spatial effect slopes (0-centered deviation from continental mean slope B)
  vector[nknots_year] B_raw;             // GAM coefficients year
  
  vector[nknots_season] B_season_raw1;         // GAM coefficients
  vector[nknots_season] B_season_raw2;         // GAM coefficients

 real<lower=0> sdnoise;    // sd of over-dispersion
  vector[ncounts] noise_raw; // count-level variance
 real<lower=0> sdalpha;    // sd of site effects
  real<lower=0> sdyear;    // sd of year effects
  array[2] real<lower=0> sdseason;    // sd of season effects
  real<lower=0> sdyear_gam;    // sd of GAM coefficients
  real<lower=0> sdyear_gam_strat; //sd of strata level gams for each knot
  real ALPHA1; // overall intercept
}

transformed parameters { 
  vector[ncounts] E;           // log_scale additive likelihood
  vector[ndays] season_pred1 = season_basispred*(sdseason[1]*B_season_raw1);
  vector[ndays] season_pred2 = season_basispred*(sdseason[2]*B_season_raw2);
  matrix[ndays,2] season_pred;
    matrix[nyears,nstrata] year_pred;
  vector[nyears] Y_pred; 
  vector[nsites] alpha;
  vector[nknots_year] B;
   matrix[nstrata,nknots_year] b;
   vector[nyears] year_effect;             // continental year-effects

 
  alpha = sdalpha*alpha_raw + ALPHA1;// + beta_size*site_size;
  B = sdyear_gam*B_raw;
  year_effect = sdyear*year_effect_raw;
  

    season_pred[,1] = season_pred1;
 season_pred[,2] = season_pred2;
 
 
    for(k in 1:nknots_year){
    b[,k] = (sdyear_gam_strat * b_raw[,k]) + B[k];
  }
  
  
  Y_pred = year_basispred * B; 
  
      for(s in 1:nstrata){
     year_pred[,s] = (year_basispred * transpose(b[s,]));
}

  for(i in 1:ncounts){
    real noise = sdnoise*noise_raw[i];
    E[i] = year_pred[year_raw[i],strat[i]] + alpha[site[i]] + year_effect[year_raw[i]] + season_pred[date[i],seas_strat[i]] + noise;
  }
  
  }
  
  
model { 
  sdnoise ~ std_normal(); //prior on scale of extra Poisson log-normal variance
  noise_raw ~ student_t(3,0,1); // count-level random effect
  sdyear ~ normal(0,0.2); //prior on scale of annual fluctuations - 
  // above is informative so that 95% of the prior includes yearly fluctuations fall
  // between 33% decrease and a 50% increase
  sdalpha ~ student_t(3,0,1); //prior on scale of site level variation
  sdyear_gam ~ student_t(10,0,1); //prior on sd of gam hyperparameters
  sdyear_gam_strat ~ gamma(2,4);//boundary avoiding mildly informative prior 
 //nu ~ gamma(2,0.1); // prior on df for t-distribution of heavy tailed site-effects from https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  ALPHA1 ~ student_t(3,0,1);// overall species intercept 
  sdseason ~ student_t(3,0,1);//variance of GAM parameters
  B_season_raw1 ~ std_normal();//GAM parameters
  B_season_raw2 ~ std_normal();//GAM parameters

  count ~ poisson_log(E); //vectorized count likelihood
  alpha_raw ~ student_t(3,0,1); // random site-effects
  B_raw ~ std_normal();// prior on GAM hyperparameters
  year_effect_raw ~ std_normal(); //prior on ▲annual fluctuations
  sum(year_effect_raw) ~ normal(0,0.001*nyears);//sum to zero constraint on year-effects
  sum(alpha_raw) ~ normal(0,0.001*nsites);//sum to zero constraint on site-effects

  
    for(k in 1:nknots_year){
  b_raw[,k] ~ icar_normal(nstrata, node1, node2);
  }
  

}

generated quantities {

  array[nyears] real<lower=0> N;
  array[nyears] real<lower=0> NSmooth;
  array[nyears] real<lower=0> N_alt;
  array[nyears] real<lower=0> NSmooth_alt;
  array[nstrata,nyears] real<lower=0> n;
  array[nstrata,nyears] real<lower=0> nsmooth;
    real seas_max1 = max(season_pred1)/2;
 real seas_max2 = max(season_pred2)/2;
 vector[2] seas_max;
       vector[ncounts] log_lik;

 
 // log_lik calculation for looic

  for(i in 1:ncounts){
  log_lik[i] = poisson_log_lpmf(count[i] | E[i]);
  }
  
   seas_max[1] = seas_max1;
 seas_max[2] = seas_max2;
 
      for(s in 1:nstrata){

        
  for(y in 1:nyears){
           array[nsites_strat[s]] real atmp;
           array[nsites_strat[s]] real atmp_smo;
            
        //a stratum-scaling component that tracks the alphas for sites in stratum
        for(j in 1:nsites_strat[s]){
          atmp[j] = exp(year_pred[y,s] + year_effect[y] + seas_max[seasons[s,2]] + alpha[sites[j,s]] + 0.5*(sdnoise^2));
          atmp_smo[j] = exp(year_pred[y,s] + seas_max[seasons[s,2]] + 0.5*(sdyear^2)  + alpha[sites[j,s]] + 0.5*(sdnoise^2));
        }
        n[s,y] = mean(atmp);
        nsmooth[s,y] = mean(atmp_smo);
    }
  }
  
    for(y in 1:nyears){

      N[y] = exp(ALPHA1 + Y_pred[y] + year_effect[y] + seas_max[1] + 0.5*(sdnoise^2)  );
      NSmooth[y] = exp(ALPHA1 + Y_pred[y] + seas_max[1] + 0.5*(sdyear^2) + 0.5*(sdnoise^2));
     
      N_alt[y] = exp(ALPHA1 + Y_pred[y] + year_effect[y] + seas_max[1] + 0.5*(sdnoise^2) + 0.5*(sdalpha^2));
      NSmooth_alt[y] = exp(ALPHA1 + Y_pred[y] + seas_max[1] + 0.5*(sdyear^2) + 0.5*(sdnoise^2)  + 0.5*(sdalpha^2));
      
    }
    
}

