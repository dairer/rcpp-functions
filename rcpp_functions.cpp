#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

double calc_indiv_chi(NumericVector, NumericVector, double);

// [[Rcpp::export]]
NumericVector to_unif(NumericVector x) {
  // Function to transform a random vector into a uniformly distributed vector using rank.
  
  // size of input vector
  double len_of_x = x.length(); 
  
  // declare vector to store uniformly distributed vector 
  NumericVector in_unif (len_of_x); 
  
  // Itterate over elements and rank
  for(int i=0; i < len_of_x; i++){
    double count = 0;
    for(int j=0; j < len_of_x; j++)
      if(x[j] <= x[i]) 
        count ++;
      in_unif[i] = count/(len_of_x+1);
  }
  return in_unif;
}

// [[Rcpp::export]]
NumericVector calc_chi(NumericVector x, NumericVector y, int series = 1, double quantile = 0.9){
  
  double len_of_x = x.length(); 
  double step = 0.01;
  static int num_records = (int)1/step;
  NumericVector chis (num_records-1);
  
  if(series == 0){
    NumericVector chis = NumericVector::create(calc_indiv_chi(x,y,quantile));
    return chis;
  }else{
    for(double k = 0; k<1; k+=step){
      chis[(int)(k*num_records)] = calc_indiv_chi(x,y,k);
    }
  }
  return chis;
}


double calc_indiv_chi(NumericVector x, NumericVector y, double quantile){
  
  double len_of_x = x.length(); 
  double count_x_exceed = 0;
  double count_both_exceed = 0;
  
  // Itterate over elements and rank
  for(int i=0; i < len_of_x; i++){
    if(x[i] > quantile){
      count_x_exceed ++;
      if(y[i] > quantile)
        count_both_exceed ++;
    }
  }
  return (count_both_exceed/count_x_exceed);
}
