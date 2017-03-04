<!-- README.md is generated from README.Rmd. Please edit that file -->
manager
=======

[![Build Status](https://travis-ci.org/jentjr/manager.svg?branch=master)](https://travis-ci.org/jentjr/manager)

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

License
-------

MIT
