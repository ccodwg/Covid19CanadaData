
# Covid19CanadaData: Download Canadian COVID-19 Data

<!-- badges: start -->
<!-- badges: end -->

The goal of Covid19CanadaData is to facilitate the acquisition of Canadian COVID-19 data from the following sources:

* Live versions of Canadian COVID-19 datasets available on the Internet
* The [COVID-19 Canada Open Data Working Group](https://opencovid.ca/) (CCODWG) daily [COVID-19 in Canada dataset](https://github.com/ishaberry/Covid19Canada) via the [JSON API](https://opencovid.ca/api/)
* The [Archive of Canadian COVID-19 Data](https://github.com/ccodwg/Covid19CanadaArchive), which provides daily snapshots of COVID-19 data from various Canadian government sources (and select non-governmental sources), via live URLs (for current versions) and the Google Drive API (for
archived versions)

As a basic toolbox for accessing the COVID-19 Canada Open Data Working Group
dataset, this package is a dependency for several interrelated projects,
including `Covid19CanadaTrends` and `Covid19CanadaDashboard`.

Not all features of this package are available yet. Currently, the following functionality is enabled:

* Access to most CCODWG core datasets via the JSON API. Currently, only the time
series, summary and supplementary (other) datasets are available.
* Access to the Archive of Canadian COVID-19 Data is currently available only
for current versions of CSV datasets via live URLs.

## Installation

You can install the development version of Covid19CanadaData from [GitHub](https://github.com/ccodwg/Covid19CanadaData) with:

``` r
# install.packages("devtools")
devtools::install_github("ccodwg/Covid19CanadaData")
```

## Examples

### Live Canadian COVID-19 datasets

Below are some example commands for downloading the live versions of data catalogued in the Archive of Canadian COVID-19 Data. Datasets are referenced using the UUID from [datasets.json](https://github.com/ccodwg/Covid19CanadaArchive/blob/master/data/datasets.json) in [Covid19CanadaArchive](https://github.com/ccodwg/Covid19CanadaArchive).

``` r
# download live versions of datasets catalogued in the Archive of Canadian COVID-19 Data

## get PHAC epidemiology update CSV
dl_current("f7db31d0-6504-4a55-86f7-608664517bdb")

## get Saskatchewan total cases CSV
dl_current("61cfdd06-7749-4ae6-9975-d8b4f10d5651")
```

### COVID-19 Canada Open Data Working Group dataset

Below are some example commands for downloading data from the COVID-19 Canada Open Data Working Group dataset:

``` r
# download Covid-19 Canada Open Data Working Group data

## get case time series for Toronto during the first half of March 2020
dl_ccodwg("timeseries", "cases", loc = 3595, after = "2020-03-01", before = "2020-03-15")

## get most recent Canada-wide summary
dl_ccodwg("summary", loc = "canada")

## get list of province names and population values
dl_ccodwg("other", "prov")

## get date the CCODWG dataset was last updated
ccodwg_update_date()
```
