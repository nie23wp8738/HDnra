## Test environments

-   local OS X install, R 4.3.2
-   github actions testing for devel, release, and ubuntu, windows, and macOS
-   R-hub windows-x86_64-devel (r-devel)
-   R-hub ubuntu-gcc-release (r-release)
-   R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

The main note is as follows:

1. **checking installed package size ... NOTE**

    The installed size is 5.5Mb, with the `data` sub-directory taking up 4.9Mb. This is because the package contains necessary example datasets for the package functionality, and further compression or removal would affect user experience. These datasets are integral to the examples and testing procedures provided by the package.

## Comments on previous submission

1. Fixed LaTeX errors related to the use of `χ` by replacing it with `$\\chi$` in the Roxygen2 documentation, allowing proper PDF generation.
2. Updated CRAN URL to use the canonical format `https://CRAN.R-project.org/package=HDNRA`.
3. Cleaned up temporary files like `HDNRA-manual.tex.
4. Renamed the function `BS1996.TS.NART` to `BS1996.TS.NABT` for consistency.
5. Fixed several typos in the documentation and function names.

## Reverse dependencies

`devtools::revdep()` showed that `HDNRA` is not currently a dependency of any other package.
