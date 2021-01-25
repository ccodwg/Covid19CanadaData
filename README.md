
# Covid19CanadaData: Download Canadian COVID-19 Data

<!-- badges: start -->
<!-- badges: end -->

The goal of Covid19CanadaData is to facilitate the acquisition of Canadian COVID-19 data from the following sources:

* The [COVID-19 Canada Open Data Working Group](https://opencovid.ca/) (CCODWG) daily [COVID-19 in Canada dataset](https://github.com/ishaberry/Covid19Canada) via the [JSON API](https://opencovid.ca/api/)
* The [Archive of COVID-19 Data from Canadian Government Sources](https://github.com/jeanpaulrsoucy/covid-19-canada-gov-data), which provides daily snapshots of COVID-19 data from various Canadian government sources (and select non-governmental sources), via the Google Drive API

As a basic toolbox for accessing the COVID-19 Canada Open Data Working Group dataset, this package is a dependency for several interrelated projects, including `Covid19CanadaTrends` and `Covid19CanadaDashboard`.

This package is not currently completely functional. For now, only the following functionality is available:

* Access to most CCODWG core datasets via the JSON API. Currently, only the time
series, summary and supplementary (other) datasets are available.
* Access to the Archive of COVID-19 Data from Canadian Government Sources has
not yet been implemented.

## Installation

You can install the development version of Covid19CanadaData from [GitHub](https://github.com/jeanpaulrsoucy/Covid19CanadaData) with:

``` r
# install.packages("devtools")
devtools::install_github("jeanpaulrsoucy/Covid19CanadaData")
```

## Examples

### COVID-19 Canada Open Data Working Group dataset

Below are some example commands for downloading data from the COVID-19 Canada Open Data Working Group dataset:

``` r
# get case time series for Toronto during the first half of March 2020
dl_ccodwg("timeseries", "cases", loc = 3595, after = "2020-03-01", before = "2020-03-15")

# get most recent Canada-wide summary
dl_ccodwg("summary", loc = "canada")

# get list of province names and population values
dl_ccodwg("other", "prov")

# get date the CCODWG dataset was last updated
ccodwg_update_date()
```
