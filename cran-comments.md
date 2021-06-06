# Test environments
- local
  - x86_64-w64-mingw32
- r-hub
  - ubuntu-gcc-release
  - fedora-clang-devel
- win-builder (for R-release)
  - x86_64-w64-mingw32


# Results of `R CMD check --as-cran`

Results are identical in all the environments.

```
Status: 1 NOTE
```

## Note #1
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kenji Kondo <kkondo.odnokk@gmail.com>'

New submission
```

This note just says this is the first submission of me, and there is actually no problem.


# `revdepcheck` results
There are currently no downstream dependencies for this package.
