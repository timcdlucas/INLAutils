# resubmission

Fixed wrong date format in DESCRIPTION
Removed README from build as images were causing issues.

## Test environments
* local ubuntu 16.04 R 3.4.0 (2017-04-21)
* travis CI ubuntu 12.04 devel (2017-07-28 r72983)
* travis CI ubuntu 12.04 R 3.4.0 (2017-03-06)
* winbuilder R 3.4.1 (2017-06-30)
* winbuilder R devel (2017-07-27 r72981)


## R CMD check results
There were no ERRORs or WARNINGs.

## Notes

Maintainer: 'Tim Lucas <timcdlucas@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  INLA (3:30, 11:48, 12:54)
  ggplot (4:9, 12:19)

These words are spelled correctly.


Suggests or Enhances not in mainstream repositories:
  INLA
  
This package is based around adding to the INLA (http://www.r-inla.org/) package. I understand this package cannot be on CRAN. I believe other packages on CRAN depend on INLA so hopefully this will not be a problem.


The examples and tests are a bit slow (but less than 15 seconds). Given that they are demonstrating analyses on spatial data, they are inevitably a bit slow. If this is a problem I will make them faster (and less realistic).