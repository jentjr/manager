<!-- README.md is generated from README.Rmd. Please edit that file -->
manager
=======

[![Build Status](https://travis-ci.org/jentjr/manager.svg?branch=master)](https://travis-ci.org/jentjr/manager) [![Build status](https://ci.appveyor.com/api/projects/status/wmatiqqb5e8v01lp/branch/master?svg=true)](https://ci.appveyor.com/project/jentjr/manager/branch/master)

`manager` provides a set of tools for plotting and analyzing groundwater data as well as reading data from external sources such as MANAGES and gINT.

Example
-------

``` r
data <- read_manages3("C:/path/to/Site.mdb")
```

Installation
------------

To install the `manager` package you must first make sure you have a working development environment.
\* **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
\* **Mac**: Install Xcode from the Mac App Store.
\* **Linux**: Install a compiler and various development libraries (details vary across differnet flavors of Linux).

Then, install the `devtools` package from CRAN with

``` r
install.packages("devtools")
```

After you have `devtools` installed you can install `manager` using the command

``` r
devtools::install_github("jentjr/manager")
```

Eventually, the package might be submitted to CRAN, but until then you'll have to install with `devtools`.

Shiny App
---------

A [shiny app](http://shiny.rstudio.com) is included with the package. It can be launched locally by running `manager::manager()`, or you can browse to the [shinyapps.io](http://shinyapps.io) website for [manager](http://jentjr.shinyapps.io/manager). In order to read a MANAGES, or gINT database you must be running a local server with `RODBC` installed. `R` must either be in 32-bit, or 64-bit mode depending on which drivers are installed for microsoft access. MANAGER has only been tested in 32-bit mode.
