## Test environments

-   local OS X install, R 4.3.2
-   github actions testing for devel, release, and ubuntu, windows, and macOSX
-   R-hub windows-x86_64-devel (r-devel)
-   R-hub ubuntu-gcc-release (r-release)
-   R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors \| 0 warnings \| 5 note

The main notes are as following:

1 \* checking CRAN incoming feasibility ... [3s/18s] NOTE

```         
Maintainer: ‘Pengfei Wang <nie23.wp8738@e.ntu.edu.sg>’

New submission

License components with restrictions and base license permitting such:
  GPL (>= 3) + file LICENSE
```

2 \* checking installed package size ... NOTE

```         
 installed size is  6.5Mb
  sub-directories of 1Mb or more:
    data   4.8Mb
    libs   1.4Mb
```

3 \* checking HTML version of manual ... NOTE

The following is only found on Windows (Server 2022, R-devel 64-bit):

```         
Skipping checking math rendering: package 'V8' unavailable
```

The following is found with *Fedora Linux, R-devel, clang, gfortran* and *Ubuntu Linux 20.04.1 LTS, R-release, GCC*

```         
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```

This also seems to be a recurring issue on Rhub [R-hub issue #560](https://github.com/r-hub/rhub/issues/548) and so can likely be ignored.

The following two that are only found on Windows (Server 2022, R-devel 64-bit):

4 \* checking for non-standard things in the check directory ... NOTE

```         
Found the following files/directories:
  ''NULL''
```

As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored.

5 \* checking for detritus in the temp directory ... NOTE

```         
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Reverse dependencies

`devtools::revdep()` showed that `HDNRA` is not currently a dependency of any other package.
