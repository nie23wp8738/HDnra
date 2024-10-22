# HDNRA 2.0.1

* Renamed the function `BS1996.TS.NART` to `BS1996.TS.NABT` for consistency.
* Fixed several typos in the documentation and function names.
* Corrected several typographical errors in the documentation and function names to improve clarity and usability.
* Replaced deprecated `save-always` with `actions/cache@v3` in GitHub Actions workflow.
* Enhanced GitHub Actions performance by implementing caching for R package dependencies, leading to faster build times and improved CI efficiency.

# HDNRA 2.0.0

* The function name has been changed. The title, example, output format of the function have been changed, and some typos have also been corrected.

* Added a helper function to 'HDNRA.cpp' file in order to improve computation speed; updated the code of all functions to facilitate faster computation.

* Added an 'NRtest.object' to output an S3 class 'NRtest' for our package, and also constructed a corresponding print function to output the appropriate format.

* Added a 'zzz.R' file to manage package startup messages and initialization.

# HDNRA 1.0.0

* Initial CRAN submission.
