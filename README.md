### What is it?
A handy tool for actuarial modeling, which is designed to achieve both accuracy and accountability.

### Installation
To install the latest version from `github` :
```r
install.packages("devtools")
devtools::install_github("kkondo1981/aglm", build_vignettes=TRUE)
```

### Usage
See vignettes and manuals in package.

### Building Package from Source
```shell
Rcmd.exe INSTALL --no-multiarch --with-keep.source aglm
```
Or execute the "Build -> Install and Restart" command of RStudio.

### Testing Package
```r
devtools::test()  # If failed to some tests, fix it before building
```
Or execute the "Build -> Test Package" command of RStudio.

### Building Documents
```r
devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))
```
Or execute the "Build -> Document" command of RStudio.
