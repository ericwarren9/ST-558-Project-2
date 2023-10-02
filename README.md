Using the Fed Treasury API as an Example of Interacting with APIs
================
Eric Warren

-   [1 Initial Requirements](#1-initial-requirements)

This document is a vignette to show how to get data from an
[API](https://aws.amazon.com/what-is/api/). To demonstrate, I’ll be
interacting with the [Fed Treasury
API](https://fiscaldata.treasury.gov/api-documentation/). There will be
a few functions built to interact with some of the endpoints and then
will be followed up by some exploratory data analysis with some of the
data that is retrieved from the API.

# 1 Initial Requirements

To use the functions for interacting with the Fed Treasury API, I used
the following packages:

-   [`tidyverse`](https://www.tidyverse.org/): There are many useful
    features for data manipulation and visualization within the group of
    packages associated with the `tidyverse` universe.
-   [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/):
    Allows API interaction to get the necessary data we are going to use
    in this document.
