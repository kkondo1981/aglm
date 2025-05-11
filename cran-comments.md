# Test environments
- local
  - x86_64-w64-mingw32
- r-hub
  - linux(R-devel)
  - macos(R-devel)
  - windows(R-devel)
- win-builder (for R-release)
  - x86_64-w64-mingw32


# Results of `R CMD check --as-cran`

Got 2 notes as below, but I guess it's not so critical and acceptable ones.

## local
```
* checking top-level files ... NOTE
Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being installed.
```

## win-builer
```
* checking CRAN incoming feasibility ... [12s] NOTE
Maintainer: 'Kenji Kondo <kkondo.odnokk@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://www.casact.org/about/awards-prizes-scholarships/charles-hachemeister-prize#:~:text=This%20prize%20was%20established%20in,between%20the%20CAS%20and%20ASTIN.
    From: README.md
    Status: 403
    Message: Forbidden
```

## others
```
Status: OK
```

# `revdepcheck` results
```
OK: 0
BROKEN: 0
Total time: <1 min
```
