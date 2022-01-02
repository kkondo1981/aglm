# What is this?
AGLM demos for various datasets.

# Results of demo
Results of `cva.aglm()`, measured on the local PC of @kkondo1981 with the following settings.
 - CPU: Intel Core i9-12900K
 - Memory: 32GB
 - `sessionInfo()`:
  ```
  R version 4.1.2 (2021-11-01)
  Platform: x86_64-w64-mingw32/x64 (64-bit)
  Running under: Windows 10 x64 (build 19043)
  
  Matrix products: default
  
  locale:
  [1] LC_COLLATE=Japanese_Japan.932  LC_CTYPE=Japanese_Japan.932    LC_MONETARY=Japanese_Japan.932 LC_NUMERIC=C                  
  [5] LC_TIME=Japanese_Japan.932    
  
  attached base packages:
  [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
  
  other attached packages:
  [1] aglm_0.4.0        doParallel_1.0.16 iterators_1.0.13  foreach_1.5.1     glmnet_4.1-3      Matrix_1.3-4      rmdformats_1.0.3 
  [8] knitr_1.37       
  
  loaded via a namespace (and not attached):
   [1] Rcpp_1.0.7       bookdown_0.24    codetools_0.2-18 lattice_0.20-45  assertthat_0.2.1 digest_0.6.29    grid_4.1.2      
   [8] evaluate_0.14    rlang_0.4.12     rmarkdown_2.11   splines_4.1.2    tools_4.1.2      yaml_2.2.1       survival_3.2-13 
  [15] xfun_0.29        fastmap_1.1.0    compiler_4.1.2   shape_1.4.6      htmltools_0.5.2
  ```

| Dataset            | # of used rows | # of used variables | family                  | time for GLM | score for GLM | time for AGLM | score for AGLM |
| ------------------ |:--------------:|:-------------------:| ----------------------- |:------------:|:-------------:|:-------------:|:--------------:|
| dataCar            | 4624           | 5                   | Gamma(link=log)         | 8 sec.      | 1.58000       | 9 sec.       | 1.58130        |
| freMTPL2           | 10000          | 9+offset            | Poisson(link=log)       | 6 sec.      | 0.31764       | 228 sec.      | 0.31206        |
| Banknotes          | 1000           | 4                   | Binomial(link=logit)    | 3 sec.       | 0.03808       | 6 sec.       | 0.00259        |
| MultinomialExample | 500            | 30                  | Multinomial(link=logit) | 4 sec.       | 1.41506       | 66 sec.      | 1.46170        |

- Both GLM and AGLM, regularization terms and cross-validation to determine alpha and lambda are enabled.
- Scores are mean CV deviances per sample.
