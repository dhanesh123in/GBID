# GBID
2016 Kaggle's Grupo Bimbo Inventory Demand Problem

Final Rank: 241

List of files:

1. `script.R` - xgboost models based on starter scripts with 
  
  a. more efficient lagged demand computations for last 5 weeks
  
  b. overall frequencies for Agencies, Routes, Clients and Products. 
  
  c. 1 week lagged demand for 11th week data is computed after predicting 10th week demand

2. `impute_v2.R & helper.R` - xgboost models with 

  a. 5 week lagged demands at client-product-agency-route, client-product-agency and client-product levels; 
  
  b. weekly frequencies for Agencies, Routes, Clients and Products; 
  
  c. weekly 2-factor cross-tab counts for Agency, Client, Product and Routes; 
  
  d. added client_ID categories;
  
  e. added product shortnames, quantity, weight & brand;  
  
  f. 2 sets of models were built - one with 1wk lagged demand for 10th week prediction, and one without for 11th week prediction.

3. `impute_v6.R & helper_v5.R` - xgboost models with 

  a. 8 week lagged demands client-product levels, 3 month, 4 month and 5 month moving averages (that skips; 
  
  b. weekly frequencies for Agencies, Routes, Clients and Products
  
  d. added client_ID categories;
  
  e. added product shortnames, quantity, weight & brand;
  
  f. added agency town and state information
  
  g. 2 sets of models were built - one with 1wk lagged demand for 10th week prediction, and one without for 11th week prediction.
  
4. `impute_v2_v6.R` - Average of results from `impute_v2.R` and `impute_v6.R`

5. **BEST** `impute_v2_v6_results2.R` - Average of results from `script.R`, `impute_v2.R` and `impute_v6.R`
  
  
  
  
