# What is this?
AGLM demos for various datasets.

# Results of demo
Results of `cva.aglm()`, measured on the local PC of @kkondo1981 with the following settings.
 - CPU: Intel Core i7-9700
 - Memory: 32GB
 - `sessionInfo()`:
  ```
  R version 4.0.2 (2020-06-22)
  Platform: x86_64-w64-mingw32/x64 (64-bit)
  Running under: Windows 10 x64 (build 19042)
  
  Matrix products: default
  
  locale:
  [1] LC_COLLATE=Japanese_Japan.932  LC_CTYPE=Japanese_Japan.932    LC_MONETARY=Japanese_Japan.932 LC_NUMERIC=C                  
  [5] LC_TIME=Japanese_Japan.932    
  
  attached base packages:
  [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
  
  other attached packages:
   [1] aglm_0.4.0         doParallel_1.0.16  iterators_1.0.13   foreach_1.5.1      CASdatasets_1.0-11 sp_1.4-5           xts_0.12.1        
   [8] zoo_1.8-9          rmdformats_1.0.2   knitr_1.33        
  
  loaded via a namespace (and not attached):
   [1] splines_4.0.2     lattice_0.20-41   rlang_0.4.11      tools_4.0.2       grid_4.0.2        glmnet_4.1-1      xfun_0.23         htmltools_0.5.1.1
   [9] yaml_2.2.1        survival_3.1-12   digest_0.6.27     assertthat_0.2.1  bookdown_0.22     Matrix_1.2-18     codetools_0.2-16  shape_1.4.6      
  [17] evaluate_0.14     rmarkdown_2.8     compiler_4.0.2   
  ```

| Dataset            | # of used rows | # of used variables | family                  | time for GLM | score for GLM | time for AGLM | score for AGLM |
| ------------------ |:--------------:|:-------------------:| ----------------------- |:------------:|:-------------:|:-------------:|:--------------:|
| dataCar            | 4624           | 5                   | Gamma(link=log)         | 13 sec.      | 1.58000       | 19 sec.       | 1.58130        |
| freMTPL2           | 10000          | 9+offset            | Poisson(link=log)       | 11 sec.      | 0.31764       | 358 sec.      | 0.31206        |
| Banknotes          | 1000           | 4                   | Binomial(link=logit)    | 2 sec.       | 0.03808       | 12 sec.       | 0.00259        |
| MultinomialExample | 500            | 30                  | Multinomial(link=logit) | 4 sec.       | 1.41506       | 130 sec.      | 1.46170        |

- Both GLM and AGLM, regularization terms and cross-validation to determine alpha and lambda are enabled.
- Scores are mean CV deviances per sample.
