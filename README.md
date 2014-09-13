# gwstats

Tools for plotting and analyzing groundwater data.  
  
[![Build Status](https://travis-ci.org/jentjr/gwstats.svg?branch=master)](https://travis-ci.org/jentjr/gwstats)  

To install the `gwstats` package you must first make sure you have a working 
development environment.  
* **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).  
* **Mac**: Install Xcode from the Mac App Store.  
* **Linux**: Install a compiler and various development libraries (details vary across differnet flavors of Linux).  

Then, install the `devtools` package from CRAN with 
```R
install.packages("devtools")
```

After you have `devtools` installed you can install `gwstats` using the command 
```R
devtools::install_github("jentjr/gwstats")
```

If you don't want to mess with installing the package, or learning R, you can 
try the shiny app at: https://gwstats.shinyapps.io/gwstats/. Users can upload a 
.csv or .xls file. The file must have the following columns with the column 
names exactly as they appear:

location_id | sample_date | param_name | lt_measure | analysis_result | default_unit  
----------- | ----------- | ---------- | ---------- | --------------- | ------------
MW-1 | 2008-01-01 | Arsenic, dissolved | < | 0.01 | ug/L
MW-1 | 2008-01-01 | Boron, dissolved | | 0.24 | mg/L  

If you have installed the package, it is possible to connect to a MANAGES access 
database and read in data using the command 
```R
connect_manages("path/to/Site.mdb")
```

Currently, it is only possible to connect to a MANAGES database while running R 
in 32-bit mode. It is also not possible at the moment to connect to a MANAGES
database using the shiny app at https://gwstats.shinyapps.io/gwstats. Howevever, 
if you are running R in 32-bit mode you can launch the shiny app locally using the 
command 
```R
gwstats()
```

`gwstats` is rapidly evolving. Track (and contribute to) its development 
at https://github.com/jentjr/gwstats.