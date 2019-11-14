
<!-- README.md is generated from README.Rmd. Please edit that file -->

# myanmarMCCTdata: Myanmar Mother and Child Cash Transfer (MCCT) Programme Evaluation Data Handler

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/validmeasures/myanmarMCCTdata.svg?branch=master)](https://travis-ci.org/validmeasures/myanmarMCCTdata)
<!-- badges: end -->

In support of the Myanmar Mother and Child Cash Transfer programme
evaluation, this package has been developed to support the handling,
processing, analysis of data collected for the study. The package
includes functions to retrieve data from the online server database, to
appropriately structure datasets, to check and clean data, to recode
data to respective indicator sets, to estimate indicators and to perform
appropriate comparative analysis specified by the analysis.

This package has been developed for use with [R language for statistical
computing](https://cran.r-project.org). R can be downloaded freely from
[here](https://cran.r-project.org) and installed on Windows, MacOs and
various Linux distributions. Guidance and support in installing R is
available from the [Comprehensive R Archive Network
(CRAN)](https://cran.r-project.org).

## Installation

Once you have installed R on your system, you can install
`myanmarMCCTdata` from
[GitHub](https://github.com/validmeasures/myanmarMCCTdata) with the
following commands in R:

``` r
if(!require(remotes) install.packages("remotes")
remotes::install_github("validmeasures/myanmarMCCTdata")
```

## Usage

### Data retrieval and handling

Data collected for the Myanmar MCCT Programme Evaluation is stored in an
ODK Aggregate server hosted via [ONA](https://ona.io). Access to this
server and to the data stored is restricted to those with explicit
permissions and require a `username` and `password`.

Those with access can retrieve the Myanmar MCCT Programme Evaluation
data from within R without having to manually download the data from the
ONA server. This is facilitated by the **`get_mcct_data()`** function
included in this package. To use this function, one requires the
following information:

1.  **form ID** - form/s are given unique identifiers. For the Myanmar
    MCCT Programme Evaluation, each survey form developed in ODK was
    given either of these unique identifiers:
    
      - `baseline_mcct_final` - main survey form
      - `baseline_mcct_anthro_final` - child and mother anthropometry
        form
      - `baseline_townshipprofile_final` - township profile form
      - `baseline_villprofile_final` - village profile form

2.  **username** - this is the ODK Aggregate username for the
    person/organisation with permission to access and retrieve data from
    the server.

3.  **password** - this is the corresponding ODK password for the
    person/organisation with permission to access and retrieve data from
    the server.

4.  **starting date and end date** - Depending on whether you want to
    retrieve all the data or just part of the data, you will have to
    specify the starting date of the survey and the end date of the
    survey. If the data collection is still on-going, you may opt not to
    specify an end date and the current date will be used.

Given these, data for the main form of the Myanmar MCCT Programme
Evaluation can be retrieved via the following commands in R:

``` r
surveyData <- get_mcct_data(id = "baseline_mcct_final",
                            start = "2019-08-06",
                            username = "ENTER_USERNAME_HERE",
                            password = "ENTER_PASSWORD_HERE")
```

A similar command is used to retrieve the data from the other forms used
in the Myanmar MCCT Programme Evaluation:

``` r
surveyData <- get_mcct_data(id = "baseline_mcct_anthro_final",
                            start = "2019-08-06",
                            username = "ENTER_USERNAME_HERE",
                            password = "ENTER_PASSWORD_HERE")
```

It should be noted that the commands above only retrieves the non-repeat
components of the specified forms. If these forms have repeat
components, these repeat components can be retrieved as well by
specifying the argument `rep` as TRUE and then indicating the name/s of
the repeating component/s in the argument `rep.names` as follows:

``` r
surveyData <- get_mcct_data(id = "baseline_mcct_final",
                            start = "2019-08-06",
                            username = "ENTER_USERNAME_HERE",
                            password = "ENTER_PASSWORD_HERE",
                            rep = TRUE,
                            rep.name = "grp_hh")
```

In the command above, it is asking for the main form to be retrieved
along with the repeat component with a name `grp_hh`. The resulting
object is a list of two data.frames. The first is the data.frame for the
main form and the second is the data.frame for the repeat component
named `grp_hh`.

In the case of the Myanmar MCCT Programme Evaluation, there are multiple
repeats within the main form (`baseline_mcct_final`), the anthropometric
form (`baseline_mcct_anthro_final`) and the village profile form
(`baseline_villprofile_final`). To retrieve all these forms including
their respective repeats, the following commands can be used:

``` r
## List the various repeat names in baseline_mcct_final
rep.names <- c("grp_hh",              ## HH members dataset
               "support_gov_rep",     ## HH dataset
               "support_ngo_rep",     ## HH dataset
               "support_cso_rep",     ## HH dateset
               "support_ro_rep",      ## HH dataset
               "support_ho_rep",      ## HH dataset
               "support_orgoth_rep",  ## HH dataset
               "child_vc_rep",        ## Vaccination and child illness dataset
               "grp_q2_5_to_q2_7",    ## IYCF dataset
               "ancnow_rep",          ## ANC dataset
               "ancpast_rep")         ## ANC dataset

## Retrieve data and the repeats
surveyData <- get_mcct_data(id = "baseline_mcct_final",
                            start = "2019-08-06",
                            username = "ENTER_USERNAME_HERE",
                            password = "ENTER_PASSWORD_HERE",
                            filename = "main_form",
                            rep = TRUE,
                            rep.name = rep.names)

## List the various repeat names in baseline_mcct_anthro_final
rep.names <- c("grp_family",
               "grp_hh",
               "childanthro_rep",
               "mom_anthro_rep")

## Retrieve anthro datasets and the repeats
anthroData <- get_mcct_data(id = "baseline_mcct_anthro_final",
                            start = "2019-08-06",
                            username = "ENTER_USERNAME_HERE",
                            password = "ENTER_PASSWORD_HERE",
                            filename = "anthro_form",
                            rep = TRUE,
                            rep.name = rep.names)

## List the various repeat names in baseline_villprofile_final
rep.names <- c("respondent_rpt",
               "cbo_yes_grp",
               "credit_rep")

## Retrieve village profile data and the repeats
villageData <- get_mcct_data(id = "baseline_villprofile_final",
                             start = "2019-08-06",
                             username = "ENTER_USERNAME_HERE",
                             password = "ENTER_PASSWORD_HERE",
                             filename = "village_form",
                             rep = TRUE,
                             rep.name = rep.names)
```
