[![Build Status](https://travis-ci.org/tobiaskley/forecastSNSTSexamples.svg?branch=master)](https://travis-ci.org/tobiaskley/forecastSNSTSexamples)


# forecastSNSTSexamples
Data examples for the R package 'forecastSNSTS'

The aim of the `forecastSNSTS` package is to distribute the data and R code to replicate the empirical examples form Section 5 in the following paper (click to open):

[Predictive, finite-sample model choice for time series under stationarity and non-stationarity (2017/06/03)](http://personal.lse.ac.uk/kley/forecastSNSTS.pdf)

Older version of the package can be used to replicate the examples in older versions of the paper. In particular, version 1.1-0 can be used to replicate the examples in
[Predictive, finite-sample model choice for time series under stationarity and non-stationarity (2016/11/15)](https://arxiv.org/abs/1611.04460) 

## Replicating the examples

First, if you have not done so already, install R from http://www.r-project.org (click on download R, select a location close to you, and download R for your platform). Once you have the latest version of R installed and started execute the following commands on the R shell:

 ```
 install.packages("devtools")
 devtools::install_github("tobiaskley/forecastSNSTSexamples")
 ```

This will first install the R package ``devtools`` and then use it to install the latest (development) version of ``forecastSNSTSexamples`` from the GitHub repository.

If you want to install an older version of the examples you can, for example, call
 ```
 devtools::install_github("tobiaskley/forecastSNSTSexamples", ref="v1.1-0")
 ```

Now that you have R and ``forecastSNSTSexamples`` installed you can load the package and access the help files:

```
library(forecastSNSTSexamples)
help("forecastSNSTSexamples")
```

To access the (documentation provided with the) data

```
library(forecastSNSTSexamples)
help("LondonHPI")
help("Hohenpeissenberg")
help("FTSE100")
```

Finally, to replicate the empirical examples from Section 5 in the paper call

```
library(forecastSNSTSexamples)
demo("LondonHPI")
demo("Hohenpeissenberg")
demo("FTSE100")
```