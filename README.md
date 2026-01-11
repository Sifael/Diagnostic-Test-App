
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `Diagnostic Analysis Application`

This is a sample application using the golem_framework for simple diagnostic application. 


## Installation/Use

To use this application, simply download the code with git clone and run the application below.

```bash
git clone https://github.com/Sifael/Diagnostic-Test-App.git
```

## Run

You can launch the application by running:

``` r
interviewcaseapp::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-12-16 00:58:12 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading interviewcaseapp
#> Warning: replacing previous import 'DT::dataTableOutput' by
#> 'shiny::dataTableOutput' when loading 'interviewcaseapp'
#> Warning: replacing previous import 'bs4Dash::column' by 'shiny::column' when
#> loading 'interviewcaseapp'
#> Warning: replacing previous import 'bs4Dash::actionButton' by
#> 'shiny::actionButton' when loading 'interviewcaseapp'
#> Warning: replacing previous import 'bs4Dash::tabsetPanel' by
#> 'shiny::tabsetPanel' when loading 'interviewcaseapp'
#> Warning: replacing previous import 'bs4Dash::insertTab' by 'shiny::insertTab'
#> when loading 'interviewcaseapp'
#> Warning: replacing previous import 'DT::renderDataTable' by
#> 'shiny::renderDataTable' when loading 'interviewcaseapp'
#> Warning: replacing previous import 'bs4Dash::navbarMenu' by 'shiny::navbarMenu'
#> when loading 'interviewcaseapp'
#> 
#> Attaching package: 'janitor'
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
#> ── R CMD check results ──────────────────────── interviewcaseapp 0.0.0.9000 ────
#> Duration: 1.3s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependencies missing from DESCRIPTION Imports/Depends entries:
#>     'DT', 'bs4Dash'
#>   
#>   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error: Failure in `/private/var/folders/_2/7914_fy16dg236fdj7xmrjg80000gn/T/RtmphCUXe0/R_LIBS894746e60235/interviewcaseapp/interviewcaseapp-tests/testthat.Rout.fail`
#> `ui` has class 'shiny.tag', not class 'shiny.tag.list'.
#> ── Failure ('test-mod_data_upload.R:31:3'): module ui works ────────────────────
#> `ui` has class 'shiny.tag', not class 'shiny.tag.list'.
#> ── Failure ('test-mod_download_handler.R:31:3'): module ui works ───────────────
#> `ui` has class 'shiny.tag', not class 'shiny.tag.list'.
#> 
#> [ FAIL 7 | WARN 0 | SKIP 0 | PASS 104 ]
#> Error:
#> ! Test failures.
#> Execution halted
```
