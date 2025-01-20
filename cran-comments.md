## Resubmission

This is a resubmission to address the previously received comments. In this version I have:

-   Updated the package version to v0.2.0

-   Added <https://www.apra.gov.au/> to the package description.

-   Updated all exported functions documentation to use \\donttest{} instead of 
    \\dontrun{}

-   Introduced `download_apra()` as a new exported function.

-   Replaced the `read_x()` and `read_x_local()` style functions with 
    `read_apra()` and `read_apra_local()`. This should simplify future package 
    development and make using the package easier for users.
    
-   Added the `apra_stat_pubs` dataset, documenting the statistical publications
    available with `read_apra`.

## R CMD check results

0 errors \| 0 warnings \| 1 note

-   This is a new release.
