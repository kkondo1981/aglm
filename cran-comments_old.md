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


# Resubmission

I got some instructions from CRAN and fixed them as below.

## Instruction #1

```
The Description field is intended to be a (one paragraph) description
of what the package does and why it may be useful.
Please add more details about the package functionality and implemented
methods in your Description text.

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")
```

I fixed the description field of my DESCRIPTION file.
For consistency, I also fixed the beginning of 'aglm-package.Rd'.

I got a new note like below as a result, and just ignore it because these words are names of us and our method.
```
Possibly mis-spelled words in DESCRIPTION:
  AGLM (10:75, 10:131, 10:339)
  Fujita (10:356)
  Hirokazu (10:395)
  Iwasawa (10:404)
  Kenji (10:379)
  Kondo (10:385)
  Suguru (10:349)
  Tanaka (10:371)
  Toyoto (10:364)
```

Additionally, I noticed 'URL' and 'BugReports' fields are useful and added them as below:
```
URL: https://github.com/kkondo1981/aglm
BugReports: https://github.com/kkondo1981/aglm/issues
```


## Instruction #2
```
Please rather use the Authors@R field and declare Maintainer, Authors
and Contributors with their appropriate roles with person() calls.
e.g. something like:
Authors@R: c(person("Alice", "Developer", role = c("aut", "cre","cph"),
                      email = "alice.developer@some.domain.net"),
               person("Bob", "Dev", role = "aut") )

Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles, instead of writing "others".
```

I removed the 'Authors' and 'Maintainer' fields, added an 'Authors@R' field, and wrote down all the contributors instead of writing 'others' as below:
```
Authors@R: c(
    person("Kenji", "Kondo", role=c("aut", "cre", "cph"), email="kkondo.odnokk@gmail.com"),
    person("Kazuhisa", "Takahashi", role=c("ctb")),
    person("Hikari", "Banno", role=c("ctb"))
    )
```

I also fixed \\author tags in the following files for consistency:
- aglm-package.Rd
- aglm-Rd
- cv.aglm.Rd
- cva.aglm.Rd
- plot.AccurateGLM.Rd
- predict.AccurateGLM.Rd


## Instruction #3
```
Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      coef.AccurateGLM.Rd: \value
      deviance.AccurateGLM.Rd: \value
      plot.AccurateGLM.Rd: \value
      print.AccurateGLM.Rd: \value
```

I added \\value tags to the abovementioned Rd files.
I also checked all the explanations in \\value tags, and believe that they are enough.


## Instruction #4
```
\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.
```

I replaced \\dontrun{} in examples of the following functions to \\donttest{} because they take time > 5 sec:
- `cv.aglm()`
- `cva.aglm()`


## Instruction #5
```
Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
...
par(mfrow=c(2,2))            # somewhere after
...
e.g.: plot-aglm.R
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.
```

I added two immidiate calls of `on.exit()` in 'plot-aglm.R' to restore `par` and `devAskNewPage`.
Because there are possibly two calls `on.exit()` in one function, I set `add=TRUE` when call `on.exit()`.
