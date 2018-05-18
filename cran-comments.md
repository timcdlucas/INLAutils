# resubmission after archive

* The previous version of this package was archived as a dependency,
  ggfortify was archived.
* I have removed ggfortify as a  dependency.
* The package still heavily relies on INLA which is not on CRAN.
  My CI tests it with and without INLA installed with no issues.
  https://travis-ci.org/timcdlucas/INLAutils/builds/334919171?utm_source=github_status&utm_medium=notification
  Last time I submitted, the CRAN Solaris machine gave test errors.
  I was told to fix the issue, but when I explained the issue and asked
  for advice on how to proceed, I got no reply. Therefore I am unsure
  whether the Solaris will give errors again. Any advice on this issue
  would be appreciated.

## Test environments
* local ubuntu 16.04 R 3.4.1 (2017-06-30)
* travis CI ubuntu 14.04 (unstable) (2018-01-29 r74183)
* travis CI ubuntu 14.04 R 3.4.2 (2017-01-27)
* winbuilder R 3.4.3 (2017-11-30)
* winbuilder R (unstable) (2018-01-28 r74177)


## R CMD check results
There were no ERRORs.

The following warnings as discussed above.


Maintainer: 'Tim Lucas <timcdlucas@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-01-27 as it depends on the archived
    'ggfortify'.

Suggests or Enhances not in mainstream repositories:
  INLA
Availability using Additional_repositories specification:
  INLA   yes   https://www.math.ntnu.no/inla/R/stable

