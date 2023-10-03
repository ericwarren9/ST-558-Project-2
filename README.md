Using the Fed Treasury API as an Example of Interacting with APIs
================
Eric Warren

-   [1 Initial Requirements](#1-initial-requirements)
-   [2 API Interaction Functions](#2-api-interaction-functions)
    -   [2.1 `removeNulls()`](#21-removenulls)
    -   [2.2 `convertToCorrectType()`](#22-converttocorrecttype)
    -   [2.3 `getSecurityList()`](#23-getsecuritylist)
    -   [2.4 `getAccountTypes()`](#24-getaccounttypes)
    -   [2.5 `getUnfilteredData()`](#25-getunfiltereddata)
    -   [2.6 `filterDate()`](#26-filterdate)
    -   [2.7 `securityType()`](#27-securitytype)
    -   [2.8 `individualSecurities()`](#28-individualsecurities)
    -   [2.9 `filterPercentage()`](#29-filterpercentage)
    -   [2.10 `filterBalanceSheet()`](#210-filterbalancesheet)
    -   [2.11 `getFunctionNames()`](#211-getfunctionnames)
    -   [2.12 `chooseDataset()`](#212-choosedataset)
-   [3 Exploratory Data Analysis](#3-exploratory-data-analysis)

This document is a vignette to show how to get data from an
[API](https://aws.amazon.com/what-is/api/). To demonstrate, I’ll be
interacting with the [Fed Treasury
API](https://fiscaldata.treasury.gov/api-documentation/), which include
the the [Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities),
the [Fed Treasury API on Balance
Sheets](https://fiscaldata.treasury.gov/datasets/u-s-government-financial-report/balance-sheets),
and the [Fed Treasury API on National
Debt](https://fiscaldata.treasury.gov/datasets/debt-to-the-penny/debt-to-the-penny).
There will be a few functions built to interact with some of the
endpoints and then will be followed up by some exploratory data analysis
with some of the data that is retrieved from the API.

# 1 Initial Requirements

To use the functions for interacting with the Fed Treasury API, I used
the following packages:

-   [`tidyverse`](https://www.tidyverse.org/): There are many useful
    features for data manipulation and visualization within the group of
    packages associated with the `tidyverse` universe.
-   [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/):
    Allows API interaction to get the necessary data we are going to use
    in this document.
-   [`httr`](https://cran.r-project.org/web/packages/httr/index.html):
    Allows us to get our data from the desired URL.
-   [`gridExtra`](https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf):
    Allows plots to be arranged in a tidy way
-   [`gganimate`](https://gganimate.com/): A way to make animated plots
    that might change over a particular variable (used a lot for change
    over time)

# 2 API Interaction Functions

Here is where I am going to make some functions that will help with
fulfilling our needs with reading in and analyzing our data from the
[Fed Treasury API](https://fiscaldata.treasury.gov/api-documentation/),
which include the the [Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities),
the [Fed Treasury API on Balance
Sheets](https://fiscaldata.treasury.gov/datasets/u-s-government-financial-report/balance-sheets),
and the [Fed Treasury API on National
Debt](https://fiscaldata.treasury.gov/datasets/debt-to-the-penny/debt-to-the-penny).

## 2.1 `removeNulls()`

Once you do read in your data set immediately use this function to
remove null values since they will not help in our analysis since we
need to know all the information in the columns to get any kind of
result. We should filter our data frame or tibble to what we want with
the appropriate columns before using this function.

``` r
removeNulls <- function(df_name){
  # Copy df_name
  df <- df_name
  
  # Get null values and get rid of observations that have them and columns that are mostly null (over 50%)
  df <- df[ , colMeans(!(df == "null")) > 0.5] # Columns first
  df$countNulls <- apply(df, 1, function(x) length(which(x == "null"))) # Now rows
  df <- df[df$countNulls == 0, ]
  df <- df[1:length(df)-1]
  
  # Return updated df
  return(df)
}
```

## 2.2 `convertToCorrectType()`

This function was made since the data in our API was returning dates in
the character format. We needed a way to have the dates accurately show
up in our data set. We had the same issue with our numeric data. So we
created a function that allows the vector (or a column in our dataset)
to end up in the correct type of data (either numeric, date, or
character). Please use the function `removeNulls()` first to make sure
our data does not have missing values which will not help us in our
analyzing.

``` r
convertToCorrectType <- function(x){
  # Make conditional statement to make sure column can be date
  correctOutput <- if(all(is.na(suppressWarnings(as.Date(x, format = "%Y-%m-%d"))) == FALSE)) as.Date(x, format = "%Y-%m-%d") else if(all(is.na(suppressWarnings(as.numeric(x))) == FALSE)) as.numeric(x) else x
  return(correctOutput)
}
```

## 2.3 `getSecurityList()`

This function acts as a helper function for the user as we can now get
the list of Security Type Descriptions for the [Fed Treasury API on
Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities)
(so if we want to look at Marketable, Non-marketable, or
Interest-bearing Debt specific type securities) or if we want to subset
even further later on, we can get the exact security names that are
available (example: “Treasury Bills”, “Treasury Notes”, etc.)

``` r
getSecurityList <- function(x = "security type"){
  # Get the data in the correct format to get the list of unique values; limit is 4500 which includes all data
  jsonData <- httr::GET("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates?limit=9999")
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  tibble <- as_tibble(contents$data)
  
  # Now get the list of values depending on value inputted; default is "security type"
  outputList <- if(x == "security type") unique(tibble$security_type_desc) else if(x == "specific securities") unique(tibble$security_desc) else stop("Input must be either 'security type' to get the security type options or 'specific securities' to get the exact security names (individual options -- like Treasury Bills, Treasury Notes, etc.) The default is 'security type'.")
  
  return(outputList)
}
```

## 2.4 `getAccountTypes()`

This function acts as a helper function for the user as we can now get
the list of Account Type Descriptions for the [Fed Treasury API on
Balance
Sheets](https://fiscaldata.treasury.gov/datasets/u-s-government-financial-report/balance-sheets)
(so if we want to look at Assets, Liabilities, etc.) or if we want to
subset even further later on, we can get the exact line item names that
are available (example: “Accounts payable”, “Interest payable”, etc.)

``` r
getAccountTypes <- function(x = "account"){
  # Get the data in the correct format to get the list of unique values; limit is 4500 which includes all data
  jsonData <- httr::GET("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/balance_sheets?limit=9999")
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  tibble <- as_tibble(contents$data)
  
  # Now get the list of values depending on value inputted; default is "security type"
  outputList <- if(x == "account") unique(tibble$account_desc) else if(x == "line items") unique(tibble$line_item_desc) else stop("Input must be either 'account' to get the account type options or 'line items' to get the exact line item names (individual options -- like Accounts payable, Total assets, etc.) The default is 'account'.")
  
  return(outputList)
}
```

## 2.5 `getUnfilteredData()`

Here we can just get a data set by using this function and specifying
which full data set we want from the corresponding API. We might not
want to filter anything but if we do then we can do that in later
functions.

``` r
getUnfilteredData <- function(data = "interest rates"){
  # Get text set up
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/"
  endpoint1 <- "avg_interest_rates"
  endpoint2 <- "debt_to_penny"
  endpoint3 <- "balance_sheets"
  usedEndpoint <- if(data == "interest rates") endpoint1 else if(data == "debt") endpoint2 else if (data == "balance sheet") endpoint3 else stop("The data operator must either be 'interest rates' to get the Interest Rate API, 'balance sheet' to get the Balance Sheet API, or 'debt' to get data from the National Debt API. The default if left blank is the 'interest rates' argument.")
  limitText <- "?limit=9999"
  
  
  # Get the url
  finalString <- paste(jsonString, usedEndpoint, limitText, sep = "")
  
  # Convert to tibble
  jsonData <- httr::GET(finalString)
  
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  
  tibble <- as_tibble(contents$data)
  
  # Fix the TotalMarketable issue in the security_type_desc column if reading in the interest rates data set
  if("security_desc" %in% colnames(tibble)){
    tibble <- tibble %>% 
      mutate(security_desc = ifelse(as.character(security_desc) == "TotalMarketable", "Total Marketable", as.character(security_desc)))
  }
  else {
    tibble <- tibble
  }
  
  # Return tibble
  return(tibble)
}
```

Now we are going to start looking at functions we can make by filtering
the data and make our API more interactive for the user.

## 2.6 `filterDate()`

As an investor, we might pick a certain security or a group of them
because we are told to look at them or we feel more confident in them.
Other times people might want to pick how well securities are doing in
recent time. What if we wanted to instead look at the balance sheet? Or
what about the US Debt? And what if we want to look at either of these
metrics over a certain period of time? That is why we are creating this
function so the user can filter by the date they would like. Please note
the dates must be in a “yyyy-mm-dd” format. Also please specify your
arguments. This function is very similar to `getUnfilteredData()` except
now we are specifying the date range we want along with the data set in
question.

``` r
filterDate <- function(data = "interest rates", date = NULL, operator = NULL){
  # Get text set up
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/"
  endpoint1 <- "avg_interest_rates"
  endpoint2 <- "debt_to_penny"
  endpoint3 <- "balance_sheets"
  usedEndpoint <- if(data == "interest rates") endpoint1 else if(data == "debt") endpoint2 else if (data == "balance sheet") endpoint3 else stop("The data operator must either be 'interest rates' to get the Interest Rate API, 'balance sheet' to get the Balance Sheet API, or 'debt' to get data from the National Debt API. The default if left blank is the 'interest rates' argument.")
  limitFilterText <- "?limit=9999&filter="
  
  # Get the correct text for the data we want.
  addedText <- if ((is.null(operator) == TRUE) | (is.null(date) == TRUE)) "" else if(operator %in% c("lt", "gt", "lte", "gte", "eq")) paste("record_date:", operator, ":", URLencode(date), ",", sep = "") else stop("Please input a value that is acceptable. The date option must be in 'yyyy-mm-dd' format and the starting date for the data is '2001-01-31' for the interest rate API, '1995-09-30' for the balance sheet API, and '1993-04-01' for the debt API. Also please note the balance sheet API is updated every September 30th for the fiscal year. The input for the operator argument are either 'lt' for less than, 'lte' for less than or equal to, 'gt' for greater than, 'gte' for greater than or equal to, and 'eq' for equal to. Also make sure inputs are in quotations and spelled in the same way you can find in this message.")
  
  # Get the url
  finalString <- paste(jsonString, usedEndpoint, limitFilterText, addedText, sep = "")
  
  # Convert to tibble
  jsonData <- httr::GET(finalString)
  
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  
  tibble <- as_tibble(contents$data)
  
  # Fix the TotalMarketable issue in the security_type_desc column if reading in the interest rates data set
  if("security_desc" %in% colnames(tibble)){
    tibble <- tibble %>% 
      mutate(security_desc = ifelse(as.character(security_desc) == "TotalMarketable", "Total Marketable", as.character(security_desc)))
  }
  else {
    tibble <- tibble
  }
  
  # Return tibble
  return(tibble)
}
```

Now we are going to look at making functions for a specific data set. We
really can’t filter the debt data in any other way other than by date
but we can for the other two. Let us first look at doing this with the
[Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities).

## 2.7 `securityType()`

Here we can use our helper function from before to get the security
types that are present in the [Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities).
As we can see they are Marketable, Non-marketable, Interest-bearing
Debt. We can use this to get the data we want to analyze by a particular
security type. This function has an input of a vector with the type of
securities you want to analyze.

``` r
securityType <- function(vec = NULL){
  # Create Testing List 
  testingList <- getSecurityList()
  # Get text set up
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates?limit=9999&filter="
  
  addedText <- if (is.null(vec) == TRUE) "" else if(all(vec %in% testingList) == TRUE) paste("security_type_desc:in:(", paste(URLencode(vec), sep = "", collapse = ","), "),", sep = '') else stop("Please input a value that is acceptable. You can find the acceptable values by using the helper function getSecurityList(). Also make sure inputs are in quotations and spelled in the same way you can find in the helper function.")
  
  finalString <- paste(jsonString, addedText, sep = "")
  
  # Convert to tibble
  jsonData <- httr::GET(finalString)
  
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  
  tibble <- as_tibble(contents$data)
  
  # Fix the TotalMarketable issue in the security_type_desc column if reading in the interest rates data set
  if("security_desc" %in% colnames(tibble)){
    tibble <- tibble %>% 
      mutate(security_desc = ifelse(as.character(security_desc) == "TotalMarketable", "Total Marketable", as.character(security_desc)))
  }
  else {
    tibble <- tibble
  }
  
  # Return tibble
  return(tibble)
}
```

## 2.8 `individualSecurities()`

Like the `securityType()` function, we might want to instead filter our
data by the security itself and not the type (or group of security) in
the [Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities).
Maybe the user is really interested in investing in just Treasury Bonds
or maybe the user want to compare just Treasury Bonds to Treasury Notes
for interest rates. We can allow the user to do this in a similar manner
of how we did it with the `securityType()` function.

``` r
individualSecurities <- function(vec = NULL){
  # Create Testing List 
  testingList <- getSecurityList("specific securities")
  # Get text set up
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates?limit=9999&filter="
  
  addedText <- if (is.null(vec) == TRUE) "" else if(all(vec %in% testingList) == TRUE) paste("security_desc:in:(", paste(URLencode(vec), sep = "", collapse = ","), "),", sep = '') else stop("Please input a value that is acceptable. You can find the acceptable values by using the helper function getSecurityList('specific securities'). Also make sure inputs are in quotations and spelled in the same way you can find in the helper function.")
  
  finalString <- paste(jsonString, addedText, sep = "")
  
  # Convert to tibble
  jsonData <- httr::GET(finalString)
  
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  
  tibble <- as_tibble(contents$data)
  
  # Fix the TotalMarketable issue in the security_type_desc column if reading in the interest rates data set
  if("security_desc" %in% colnames(tibble)){
    tibble <- tibble %>% 
      mutate(security_desc = ifelse(as.character(security_desc) == "TotalMarketable", "Total Marketable", as.character(security_desc)))
  }
  else {
    tibble <- tibble
  }
  
  # Return tibble
  return(tibble)
}
```

## 2.9 `filterPercentage()`

As an investor, we want to target investments that make the most sense
with the percentage of interest we would get from that said investment.
Users might want to compare rates they found from other stocks and/or
bonds with rates that might be offered by the Treasury. The other thing
people do is short the market (including bonds). This means they expect
rates to go down over time and lock someone into buying an investment at
a predetermined rate and then after a certain period of time the seller
would have to give the current value of the investment back to the
buyer. In this case, the buyer might try to find lower bond interest
rates to negotiate the predetermined rate being lower and the seller
will want to try to find higher rates to drive the price up. So someone
can use this function to try to get data that matches their interest
rates goals either from buying a bond to possibly shorting the market.
This is used for the [Fed Treasury API on Interest
Rates](https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities).

``` r
filterPercentage <- function(percentage = NULL, operator = NULL){
  # Get text set up
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates?limit=9999&filter="
  
  addedText <- if ((is.null(operator) == TRUE) | (is.null(percentage) == TRUE)) "" else if(operator %in% c("lt", "gt", "lte", "gte", "eq")) paste("avg_interest_rate_amt:", operator, ":", URLencode(as.character(percentage)), ",", sep = "") else stop("Please input a value that is acceptable. The percentage value must be a float in x.y format where x is a number and so is y to create a float. Numbers should be between 0 and 100 (put 5% as 5 or 5.0). Please do not put the '%' sign after the number. The input for the operator argument are either 'lt' for less than, 'lte' for less than or equal to, 'gt' for greater than, 'gte' for greater than or equal to, and 'eq' for equal to. Also make sure the operator input is in quotations and spelled in the same way you can find in this message.")
  
  finalString <- paste(jsonString, addedText, sep = "")
  
  # Convert to tibble
  jsonData <- httr::GET(finalString)
  
  contents <- jsonlite::fromJSON(rawToChar(jsonData$content))
  
  tibble <- as_tibble(contents$data)
  
  # Fix the TotalMarketable issue in the security_type_desc column if reading in the interest rates data set
  if("security_desc" %in% colnames(tibble)){
  tibble <- tibble %>% 
    mutate(security_desc = ifelse(as.character(security_desc) == "TotalMarketable", "Total Marketable", as.character(security_desc)))
  }
  else {
    tibble <- tibble
  }
  
  # Return tibble
  return(tibble)
}
```

## 2.10 `filterBalanceSheet()`

Now we are going to pivot to look at our balance sheet of how we can
filter some data out using some key metrics for the [Fed Treasury API on
Balance
Sheets](https://fiscaldata.treasury.gov/datasets/u-s-government-financial-report/balance-sheets).
Please use the the `getAccountTypes()` helper function if you are not
sure what the acceptable account types are. This function will be
helpful to allow us to look at the balance sheet where we can filter by
the account type (asset, liability, etc.) and the specific line items we
want. Our last argument in the function is either specified with the
word “and” or the word “or” which means if we want the account and line
item conditions to match up in with the “and” and the “or” being we want
the account type but also the specific line items even if they do not
match up.

``` r
filterBalanceSheet <- function(account_types = NULL, line_items = NULL, operator = "or"){
  # Get json string for api
  jsonString <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/balance_sheets?limit=9999&filter="
  
  # set up acceptable options account types and line items
  acceptableAccountTypes <- getAccountTypes("account")
  acceptableLineItems <- getAccountTypes("line items")
  
  # Get additional added text for account types
  addedText1 <- if(is.null(account_types) == TRUE) "" else if(all(account_types %in% acceptableAccountTypes) == TRUE) paste("account_desc:in:(", paste(URLencode(account_types), sep = "", collapse = ","), "),", sep = '') else stop("Please input a value that is acceptable. You can find the acceptable values by using the helper function getAccountTypes('account'). Also make sure inputs are in quotations and spelled in the same way you can find in the helper function.")
    
  # Get additional added text for line items
  addedText2 <- if(is.null(line_items) == TRUE) "" else if(all(line_items %in% acceptableLineItems) == TRUE) paste("line_item_desc:in:(", paste(URLencode(line_items), sep = "", collapse = ","), "),", sep = '') else stop("Please input a value that is acceptable. You can find the acceptable values by using the helper function getAccountTypes('line items'). Also make sure inputs are in quotations and spelled in the same way you can find in the helper function.")
  
  # Set up condition for "or" being chosen
  if(operator == "or"){
    # Create string urls
    string1 <- paste(jsonString, addedText1, sep = "")
    string2 <- paste(jsonString, addedText2, sep = "")
  
    # Convert to tibble
    jsonData1 <- httr::GET(string1); contents1 <- jsonlite::fromJSON(rawToChar(jsonData1$content)); tibble1 <- as_tibble(contents1$data)
    jsonData2 <- httr::GET(string2); contents2 <- jsonlite::fromJSON(rawToChar(jsonData2$content)); tibble2 <- as_tibble(contents2$data)
    
    # Combine into one tibble and sort repeated values; arrange by date like other data sets; must do if else to get correct tibble printed out based on options selected
    if((is.null(account_types) == TRUE) & (is.null(line_items) == FALSE)){
      final_tibble <- tibble2
    }
    else if((is.null(account_types) == FALSE) & (is.null(line_items) == TRUE)){
      final_tibble <- tibble1
    }
    else{
      final_tibble <- bind_rows(tibble1, tibble2) 
      final_tibble <- final_tibble %>% 
        distinct() %>% 
        arrange(record_date)
    }
  }
  else if(operator == "and"){
    # Combine strings into one to get data
    string <- paste(jsonString, addedText1, addedText2, sep = "")
    
    # Convert to tibble
    jsonData <- httr::GET(string); contents <- jsonlite::fromJSON(rawToChar(jsonData$content)); final_tibble <- as_tibble(contents$data)
  }
  else{
    stop("Must have a valid operator. Options are 'or' and 'and'. The default is 'or'. Please select 'or' if you want the filtering of data to either fall into the category of the accounty type or the line item type. Please select 'and' if you the filtering of data to either fall into the category of the accounty type or the line item type (as is it must be in both conditions).")
  }
  return(final_tibble)
}
```

## 2.11 `getFunctionNames()`

So we have a lot of functions that all have specific arguments. I wanted
to include another function that allows the user to see the functions
available, the arguments associated with their default value, and then
the corresponding helper function if they are stuck on how to use it.
The user can input which data set they want as well to get the functions
that correspond to that data set. The data sets we can put in are
“balance sheet”, “interest rates”, and “debt” to look at corresponding
helper table of functions.

``` r
getFunctionNames <- function(dataset = NULL){
  # Show which dataset options they can be
  dataset_possibilities <- c("interest rates", "debt", "balance sheet")
  
  # Show the names
  names <- c("getUnfilteredData()", "filterDate()", "securityType()", "individualSecurities()", "filterPercentage()", "filterBalanceSheet()")
  
  # Show the arguments
  arguments <- c("data", "data, date, operator", "vec", "vec", "percentage, operator", "account_types, line_items, operator")
  
  # Show default values of functions
  default_values <- c("interest rates", "interest rates, NULL, NULL", "NULL", "NULL", "NULL, NULL", "NULL, NULL, or")
  
  # Show the options user has for values
  value_options <- c("interest rates, debt, balance sheet", "interest rates, debt, balance sheet // date in 'yyyy-mm-dd' format // lt, gt, lte , gte, eq", "look at getSecurityList('security type') for options", "look at getSecurityList('specific securities') for options", "any float between 0 and 100 // lt, gt, lte, gte, eq", "look at getAccountTypes('account') for options // look at getAccountTypes('line items') for options // or, and")
  
  # Show corresponding helper functions users can look at if stuck
  helper_function <- c("NULL", "NULL", "getSecurityList('security type')", "getSecurityList('specific securities')", "NULL", "getAccountTypes('account'), getAccountTypes('line items')")
  
  # Show which datasets are involved in a function
  datasets_involved <- c("interest rates, debt, balance sheet", "interest rates, debt, balance sheet", "interest rates", "interest rates", "interest rates", "balance sheet")
  
  # Make table showing these values
  helper_table <- data.frame(names, arguments, default_values, value_options, helper_function, datasets_involved)
  
  # Return the correct corresponding helper table
  if(is.null(dataset) == TRUE){
    return_tibble <- as_tibble(helper_table)
  }
  else if(all(dataset %in% dataset_possibilities) == TRUE){
    return_tibble <- helper_table %>% 
      filter(str_detect(datasets_involved, paste(dataset[1], dataset[2], dataset[3], sep = "|"))) %>%
      as_tibble()
  }
  else {
    stop("Must put in a dataset value that works. The options are 'interest rates', 'debt', or 'balance sheet'. You may put in more than one value with the c() function. The default value is NULL which returns all the functions from all the data sets.")
  }
  return(return_tibble)
}
```

## 2.12 `chooseDataset()`

This will be our last function we will have. This is a wrapper function
that allows the user to choose which data set they want and the filters
they want applied to it. This will make it easier on the user to get
data faster rather than trying to surfing for the correct function. In
the options for this function, please put the appropriate arguments that
would be used in the other corresponding function if used separately.

``` r
chooseDataset <- function(func = "getUnfilteredData", ...){
  if(func == "getUnfilteredData"){
    outputData <- getUnfilteredData(...)
  }
  else if(func == "filterDate"){
    outputData <- filterDate(...)
  }
  else if(func == "securityType"){
    outputData <- securityType(...)
  }
  else if(func == "individualSecurities"){
    outputData <- individualSecurities(...)
  }
  else if(func == "filterPercentage"){
    outputData <- filterPercentage(...)
  }
  else if(func == "filterBalanceSheet"){
    outputData <- filterBalanceSheet(...)
  }
  else{
    stop("Please choose an acceptable named function. The helper file for the list of named function can be found by using the function getFunctionNames(). This will also show you the correct corresponding arguments. The default is the getUnfilteredData function argument.")
  }
  
  # Lastly return the data from the corresponding function
  return(outputData)
}
```

# 3 Exploratory Data Analysis

We have now created functions to interact with our API data sources. We
are now going to interact with them. Some questions we are hoping to
answer include:

-   Which securities are the best to invest in? How they do against
    inflation?
-   Does the fiscal year’s balance sheet in assets minus liabilities (or
    known as net assets) have an impact on interest rates?
-   Does the month have an impact on the interest rates offered?

Now that we have asked some questions, let us dive into our data. First
we will retrieve the data using our wrapper function `chooseDataset()`.
We want to first choose to get the unfiltered data of the interest rates
data and then we will choose to grab the unfiltered data of the balance
sheet data. We will do this below.

``` r
# Get interest rates data
interestRates <- chooseDataset(func = "getUnfilteredData", data = "interest rates")
interestRates
```

    ## # A tibble: 4,402 × 11
    ##    record_date security_type_desc security_desc             avg_interest_rate_amt
    ##    <chr>       <chr>              <chr>                     <chr>                
    ##  1 2001-01-31  Marketable         Treasury Notes            6.096                
    ##  2 2001-01-31  Marketable         Treasury Bonds            8.450                
    ##  3 2001-01-31  Marketable         Treasury Inflation-Index… 3.772                
    ##  4 2001-01-31  Marketable         Treasury Inflation-Index… 3.866                
    ##  5 2001-01-31  Marketable         Federal Financing Bank    8.917                
    ##  6 2001-01-31  Marketable         Total Marketable          6.620                
    ##  7 2001-01-31  Non-marketable     Domestic Series           7.934                
    ##  8 2001-01-31  Non-marketable     Foreign Series            7.196                
    ##  9 2001-01-31  Non-marketable     R.E.A. Series             5.000                
    ## 10 2001-01-31  Non-marketable     State and Local Governme… 5.576                
    ## # ℹ 4,392 more rows
    ## # ℹ 7 more variables: src_line_nbr <chr>, record_fiscal_year <chr>,
    ## #   record_fiscal_quarter <chr>, record_calendar_year <chr>,
    ## #   record_calendar_quarter <chr>, record_calendar_month <chr>,
    ## #   record_calendar_day <chr>

``` r
# Get balance sheet data
balanceSheet <- chooseDataset(func = "getUnfilteredData", data = "balance sheet")
balanceSheet
```

    ## # A tibble: 1,182 × 13
    ##    record_date stmt_fiscal_year restmt_flag account_desc line_item_desc          
    ##    <chr>       <chr>            <chr>       <chr>        <chr>                   
    ##  1 1995-09-30  1994             Y           Assets       Property, plant, and eq…
    ##  2 1995-09-30  1994             Y           Assets       Other assets            
    ##  3 1995-09-30  1995             N           Assets       Total assets            
    ##  4 1995-09-30  1994             Y           Assets       Total assets            
    ##  5 1995-09-30  1995             N           Liabilities  Accounts payable        
    ##  6 1995-09-30  1994             Y           Liabilities  Accounts payable        
    ##  7 1995-09-30  1995             N           Liabilities  Interest payable        
    ##  8 1995-09-30  1994             Y           Liabilities  Interest payable        
    ##  9 1995-09-30  1995             N           Liabilities  Accrued payroll and ben…
    ## 10 1995-09-30  1994             Y           Liabilities  Accrued payroll and ben…
    ## # ℹ 1,172 more rows
    ## # ℹ 8 more variables: position_bil_amt <chr>, src_line_nbr <chr>,
    ## #   record_fiscal_year <chr>, record_fiscal_quarter <chr>,
    ## #   record_calendar_year <chr>, record_calendar_quarter <chr>,
    ## #   record_calendar_month <chr>, record_calendar_day <chr>

First, through inspection of the data, null values could be present,
which is something we do not want. We should use our function
`removeNulls()` to get our updated data sets without these values.

``` r
interestRates <- removeNulls(interestRates)
interestRates
```

    ## # A tibble: 4,374 × 11
    ##    record_date security_type_desc security_desc             avg_interest_rate_amt
    ##    <chr>       <chr>              <chr>                     <chr>                
    ##  1 2001-01-31  Marketable         Treasury Notes            6.096                
    ##  2 2001-01-31  Marketable         Treasury Bonds            8.450                
    ##  3 2001-01-31  Marketable         Treasury Inflation-Index… 3.772                
    ##  4 2001-01-31  Marketable         Treasury Inflation-Index… 3.866                
    ##  5 2001-01-31  Marketable         Federal Financing Bank    8.917                
    ##  6 2001-01-31  Marketable         Total Marketable          6.620                
    ##  7 2001-01-31  Non-marketable     Domestic Series           7.934                
    ##  8 2001-01-31  Non-marketable     Foreign Series            7.196                
    ##  9 2001-01-31  Non-marketable     R.E.A. Series             5.000                
    ## 10 2001-01-31  Non-marketable     State and Local Governme… 5.576                
    ## # ℹ 4,364 more rows
    ## # ℹ 7 more variables: src_line_nbr <chr>, record_fiscal_year <chr>,
    ## #   record_fiscal_quarter <chr>, record_calendar_year <chr>,
    ## #   record_calendar_quarter <chr>, record_calendar_month <chr>,
    ## #   record_calendar_day <chr>

``` r
balanceSheet <- removeNulls(balanceSheet)
balanceSheet
```

    ## # A tibble: 1,103 × 13
    ##    record_date stmt_fiscal_year restmt_flag account_desc line_item_desc          
    ##    <chr>       <chr>            <chr>       <chr>        <chr>                   
    ##  1 1995-09-30  1994             Y           Assets       Property, plant, and eq…
    ##  2 1995-09-30  1994             Y           Assets       Other assets            
    ##  3 1995-09-30  1995             N           Assets       Total assets            
    ##  4 1995-09-30  1994             Y           Assets       Total assets            
    ##  5 1995-09-30  1995             N           Liabilities  Accounts payable        
    ##  6 1995-09-30  1994             Y           Liabilities  Accounts payable        
    ##  7 1995-09-30  1995             N           Liabilities  Interest payable        
    ##  8 1995-09-30  1994             Y           Liabilities  Interest payable        
    ##  9 1995-09-30  1995             N           Liabilities  Accrued payroll and ben…
    ## 10 1995-09-30  1994             Y           Liabilities  Accrued payroll and ben…
    ## # ℹ 1,093 more rows
    ## # ℹ 8 more variables: position_bil_amt <chr>, src_line_nbr <chr>,
    ## #   record_fiscal_year <chr>, record_fiscal_quarter <chr>,
    ## #   record_calendar_year <chr>, record_calendar_quarter <chr>,
    ## #   record_calendar_month <chr>, record_calendar_day <chr>

It was important to remove these null values as null percentages of
interest rates or null values for the balance sheet item is not going to
really help us in determining any connections or values to look at.

Now we are going to notice that our data is all in character format,
which is not what we want. We actually want this data to be in the
correct form, so we are going to use our helper function
`convertToCorrectType()` to get this data in the right format. We are
also going to use `lapply()` to look at each column and once we get this
list in the right format, we will convert back to our tibble.

``` r
# Convert balance sheet
balanceSheet <- as_tibble(lapply(balanceSheet, convertToCorrectType))
balanceSheet
```

    ## # A tibble: 1,103 × 13
    ##    record_date stmt_fiscal_year restmt_flag account_desc line_item_desc          
    ##    <date>                 <dbl> <chr>       <chr>        <chr>                   
    ##  1 1995-09-30              1994 Y           Assets       Property, plant, and eq…
    ##  2 1995-09-30              1994 Y           Assets       Other assets            
    ##  3 1995-09-30              1995 N           Assets       Total assets            
    ##  4 1995-09-30              1994 Y           Assets       Total assets            
    ##  5 1995-09-30              1995 N           Liabilities  Accounts payable        
    ##  6 1995-09-30              1994 Y           Liabilities  Accounts payable        
    ##  7 1995-09-30              1995 N           Liabilities  Interest payable        
    ##  8 1995-09-30              1994 Y           Liabilities  Interest payable        
    ##  9 1995-09-30              1995 N           Liabilities  Accrued payroll and ben…
    ## 10 1995-09-30              1994 Y           Liabilities  Accrued payroll and ben…
    ## # ℹ 1,093 more rows
    ## # ℹ 8 more variables: position_bil_amt <dbl>, src_line_nbr <dbl>,
    ## #   record_fiscal_year <dbl>, record_fiscal_quarter <dbl>,
    ## #   record_calendar_year <dbl>, record_calendar_quarter <dbl>,
    ## #   record_calendar_month <dbl>, record_calendar_day <dbl>

``` r
# Convert interest rates
interestRates <- as_tibble(lapply(interestRates, convertToCorrectType))
interestRates
```

    ## # A tibble: 4,374 × 11
    ##    record_date security_type_desc security_desc             avg_interest_rate_amt
    ##    <date>      <chr>              <chr>                                     <dbl>
    ##  1 2001-01-31  Marketable         Treasury Notes                             6.10
    ##  2 2001-01-31  Marketable         Treasury Bonds                             8.45
    ##  3 2001-01-31  Marketable         Treasury Inflation-Index…                  3.77
    ##  4 2001-01-31  Marketable         Treasury Inflation-Index…                  3.87
    ##  5 2001-01-31  Marketable         Federal Financing Bank                     8.92
    ##  6 2001-01-31  Marketable         Total Marketable                           6.62
    ##  7 2001-01-31  Non-marketable     Domestic Series                            7.93
    ##  8 2001-01-31  Non-marketable     Foreign Series                             7.20
    ##  9 2001-01-31  Non-marketable     R.E.A. Series                              5   
    ## 10 2001-01-31  Non-marketable     State and Local Governme…                  5.58
    ## # ℹ 4,364 more rows
    ## # ℹ 7 more variables: src_line_nbr <dbl>, record_fiscal_year <dbl>,
    ## #   record_fiscal_quarter <dbl>, record_calendar_year <dbl>,
    ## #   record_calendar_quarter <dbl>, record_calendar_month <dbl>,
    ## #   record_calendar_day <dbl>

Now that our data is in the correct form we can proceed with it doing
our analysis.

The first thing we are going to do is creating some contingency tables
that will allow us to better understand some things about how securities
are offered from the United States government. First, I want to take a
look at how many of each security type is offered by the US government.
Are there more marketable than Non-marketable? What other types are
offered? We can see this below.

``` r
uniqueSecurities <- interestRates %>% select(security_type_desc, security_desc) %>% distinct()
table(uniqueSecurities$security_type_desc)
```

    ## 
    ## Interest-bearing Debt            Marketable        Non-marketable 
    ##                     1                     9                    11

As we can see from this table there’s only 3 different types of
securities offered by the United States. The first is the
Interest-bearing Debt in which there is only 1 of them. We can also see
there are Marketable Securities in which there are only 9 of them and
Non-marketable Securities in which there are only 11 of them. As, we can
see the most types of securities offered are Non-marketable with
Marketable being right behind.

Another thing I am curious to look at is if the securities offered are
providing a profit to our investment. Now most people will say “yes, of
course” because interest rates are always positive. We will show this
below to back up this claim.

``` r
# Mutate data to include column called "positive" and observations being "yes" if above zero and "no" if less than 0, "neutral" if 0, "error" if not any
positiveInterest <- interestRates %>%
  mutate(positive = ifelse(avg_interest_rate_amt > 0, "yes", 
                           ifelse(avg_interest_rate_amt < 0, "no", 
                                  ifelse(avg_interest_rate_amt == 0, "neutral", "error"))))

table(positiveInterest$positive)
```

    ## 
    ## neutral     yes 
    ##      44    4330

As we can see the vast majority of the time interest rates will tend to
a positive amount so you are making money. There was never a time where
the interest rate was negative so you cannot lose money on your
investment. But as an investor, I might try to beat the inflation rates.
I am curious how many investments beat this inflation rate. We are going
to find out by pulling inflation data from [World
Data](https://www.worlddata.info/america/usa/inflation-rates.php) which
provides all kinds of inflation rate data from all over the world. We
will scrape the table that shows the inflation rates and only select the
column with the United States.

``` r
# Get the webpage first
webpage <- rvest::read_html("https://www.worlddata.info/america/usa/inflation-rates.php")

# Now save the data as a tibble
inflationData <- rvest::html_table(rvest::html_nodes(webpage, "table"))[[1]]

# Now select only the year and the US
inflationData <- inflationData %>% 
  select(Year, `United States of America`) %>%
  rename(year = Year, inflationRate = `United States of America`)

# Strip the percentage signs from inflationRate

inflationData$inflationRate <- gsub(" %", "", inflationData$inflationRate)

# Now use our convertToCorrectType() function to get it all in correct type
inflationData$inflationRate <- convertToCorrectType(inflationData$inflationRate)

# Show the output
inflationData
```

    ## # A tibble: 63 × 2
    ##     year inflationRate
    ##    <int>         <dbl>
    ##  1  2022          8   
    ##  2  2021          4.7 
    ##  3  2020          1.23
    ##  4  2019          1.81
    ##  5  2018          2.44
    ##  6  2017          2.13
    ##  7  2016          1.26
    ##  8  2015          0.12
    ##  9  2014          1.62
    ## 10  2013          1.46
    ## # ℹ 53 more rows

Now that we have our inflation data, we can now merge this with our
interest rates data to do some comparisons on interest rates to
inflation rates. Please note, when making this vignette the inflation
rate for 2023 was not confirmed so all data for 2023 interest rates was
not used here.

``` r
interestAndInflationRates <- inner_join(interestRates, inflationData, by = c("record_calendar_year" = "year"))

interestAndInflationRates
```

    ## # A tibble: 4,238 × 12
    ##    record_date security_type_desc security_desc             avg_interest_rate_amt
    ##    <date>      <chr>              <chr>                                     <dbl>
    ##  1 2001-01-31  Marketable         Treasury Notes                             6.10
    ##  2 2001-01-31  Marketable         Treasury Bonds                             8.45
    ##  3 2001-01-31  Marketable         Treasury Inflation-Index…                  3.77
    ##  4 2001-01-31  Marketable         Treasury Inflation-Index…                  3.87
    ##  5 2001-01-31  Marketable         Federal Financing Bank                     8.92
    ##  6 2001-01-31  Marketable         Total Marketable                           6.62
    ##  7 2001-01-31  Non-marketable     Domestic Series                            7.93
    ##  8 2001-01-31  Non-marketable     Foreign Series                             7.20
    ##  9 2001-01-31  Non-marketable     R.E.A. Series                              5   
    ## 10 2001-01-31  Non-marketable     State and Local Governme…                  5.58
    ## # ℹ 4,228 more rows
    ## # ℹ 8 more variables: src_line_nbr <dbl>, record_fiscal_year <dbl>,
    ## #   record_fiscal_quarter <dbl>, record_calendar_year <dbl>,
    ## #   record_calendar_quarter <dbl>, record_calendar_month <dbl>,
    ## #   record_calendar_day <dbl>, inflationRate <dbl>

Now that we have both data points, let us look at how the securities
were performing against the inflation rates. First, we will look at how
all securities do.

``` r
# Create new column outPerformInflation; "yes" if interest is higher than inflation, "no" if inflation is higher than interest, "same" if both are equal, and lastly "error" if something is not right
interestAndInflationRates <- interestAndInflationRates %>%
  mutate(outPerformInflation = ifelse(avg_interest_rate_amt > inflationRate, "yes", 
                                      ifelse(avg_interest_rate_amt < inflationRate, "no",
                                             ifelse(avg_interest_rate_amt == inflationRate, "same", "error"))))

# Make table showing the breakdown of all securities
table(interestAndInflationRates$outPerformInflation)
```

    ## 
    ##   no same  yes 
    ## 1198    2 3038

``` r
# Show percentages
prop.table(table(interestAndInflationRates$outPerformInflation))
```

    ## 
    ##           no         same          yes 
    ## 0.2826805097 0.0004719207 0.7168475696

As we can see, it seems that securities as a whole tend to beat the
inflation rate for their corresponding year. So it seems to be that
investing is a wise move. But we should now look at how each security
type does against inflation. Maybe there is a type of security that is
doing better against inflation?

``` r
# Look at security type
securitiesTypeTable <- table(interestAndInflationRates$security_type_desc, interestAndInflationRates$outPerformInflation) 

# Show number output
securitiesTypeTable
```

    ##                        
    ##                           no same  yes
    ##   Interest-bearing Debt   42    0  222
    ##   Marketable             638    2 1074
    ##   Non-marketable         518    0 1742

``` r
# Show as percentage rates for each security type
prop.table(securitiesTypeTable, margin = 1)
```

    ##                        
    ##                                  no        same         yes
    ##   Interest-bearing Debt 0.159090909 0.000000000 0.840909091
    ##   Marketable            0.372228705 0.001166861 0.626604434
    ##   Non-marketable        0.229203540 0.000000000 0.770796460

Some interesting points to note here. We can see that the Non-marketable
securities tend to offer more options in investments that beat
inflation, but as a proportion of options offered the Interest-bearing
Debt securities tend to outperform the inflation rate better than
anything else. But we will do one last more in-depth analysis by looking
at how each individual security does against inflation. Maybe there is a
specific security that is doing better against inflation than others?

``` r
# Look at individual securities
individualSecuritiesTable <- table(interestAndInflationRates$security_desc, interestAndInflationRates$outPerformInflation) 

# Show number output
individualSecuritiesTable
```

    ##                                                 
    ##                                                   no same yes
    ##   Domestic Series                                 12    0 252
    ##   Federal Financing Bank                          24    0 218
    ##   Foreign Series                                  12    0 252
    ##   Government Account Series                       24    0 240
    ##   Government Account Series Inflation Securities 114    0  33
    ##   Hope Bonds                                      92    0  11
    ##   R.E.A. Series                                   16    0 116
    ##   Special Purpose Vehicle                         32    0   0
    ##   State and Local Government Series               84    0 180
    ##   Total Interest-bearing Debt                     42    0 222
    ##   Total Marketable                                65    0 198
    ##   Total Non-marketable                            24    0 240
    ##   Treasury Bills                                 186    0  78
    ##   Treasury Bonds                                  24    0 240
    ##   Treasury Floating Rate Notes (FRN)              92    2  14
    ##   Treasury Inflation-Indexed Bonds                 0    0  45
    ##   Treasury Inflation-Indexed Notes                 0    0  45
    ##   Treasury Inflation-Protected Securities (TIPS) 180    0  39
    ##   Treasury Notes                                  67    0 197
    ##   United States Savings Inflation Securities      73    0 189
    ##   United States Savings Securities                35    0 229

``` r
# Show as percentage rates for each security
prop.table(individualSecuritiesTable, margin = 1)
```

    ##                                                 
    ##                                                          no       same
    ##   Domestic Series                                0.04545455 0.00000000
    ##   Federal Financing Bank                         0.09917355 0.00000000
    ##   Foreign Series                                 0.04545455 0.00000000
    ##   Government Account Series                      0.09090909 0.00000000
    ##   Government Account Series Inflation Securities 0.77551020 0.00000000
    ##   Hope Bonds                                     0.89320388 0.00000000
    ##   R.E.A. Series                                  0.12121212 0.00000000
    ##   Special Purpose Vehicle                        1.00000000 0.00000000
    ##   State and Local Government Series              0.31818182 0.00000000
    ##   Total Interest-bearing Debt                    0.15909091 0.00000000
    ##   Total Marketable                               0.24714829 0.00000000
    ##   Total Non-marketable                           0.09090909 0.00000000
    ##   Treasury Bills                                 0.70454545 0.00000000
    ##   Treasury Bonds                                 0.09090909 0.00000000
    ##   Treasury Floating Rate Notes (FRN)             0.85185185 0.01851852
    ##   Treasury Inflation-Indexed Bonds               0.00000000 0.00000000
    ##   Treasury Inflation-Indexed Notes               0.00000000 0.00000000
    ##   Treasury Inflation-Protected Securities (TIPS) 0.82191781 0.00000000
    ##   Treasury Notes                                 0.25378788 0.00000000
    ##   United States Savings Inflation Securities     0.27862595 0.00000000
    ##   United States Savings Securities               0.13257576 0.00000000
    ##                                                 
    ##                                                         yes
    ##   Domestic Series                                0.95454545
    ##   Federal Financing Bank                         0.90082645
    ##   Foreign Series                                 0.95454545
    ##   Government Account Series                      0.90909091
    ##   Government Account Series Inflation Securities 0.22448980
    ##   Hope Bonds                                     0.10679612
    ##   R.E.A. Series                                  0.87878788
    ##   Special Purpose Vehicle                        0.00000000
    ##   State and Local Government Series              0.68181818
    ##   Total Interest-bearing Debt                    0.84090909
    ##   Total Marketable                               0.75285171
    ##   Total Non-marketable                           0.90909091
    ##   Treasury Bills                                 0.29545455
    ##   Treasury Bonds                                 0.90909091
    ##   Treasury Floating Rate Notes (FRN)             0.12962963
    ##   Treasury Inflation-Indexed Bonds               1.00000000
    ##   Treasury Inflation-Indexed Notes               1.00000000
    ##   Treasury Inflation-Protected Securities (TIPS) 0.17808219
    ##   Treasury Notes                                 0.74621212
    ##   United States Savings Inflation Securities     0.72137405
    ##   United States Savings Securities               0.86742424

Some things to point out here. Some securities offered like Treasury
Inflation-Indexed Bonds and Notes, Foreign Series, and Domestic Series.
Though some of these highly successful securities have been around for a
limited amount of time which might skew it. The same is true for the
lower “successful” securities that have not done as well against
inflation. We can see that TIPS, FRNs, and Hope Bonds are some that do
not do as well. As an investor, you might want to make sure there is a
lot of data backing this up. For myself, I might want to only look at
securities that have 10 years of data (or 120 observations since it is
by month). So we will filter here to make sure they have 120
observations and see what securities do the best against inflation now.

``` r
# Get counts of securities
meetsCriteria <- interestAndInflationRates %>%
  group_by(security_desc) %>%
  count() %>%
  filter(n >= 120)

# Get list that meet criteria
acceptableList <- unique(meetsCriteria$security_desc)

# Get filtered data with acceptable securities
filteredData <- interestAndInflationRates %>%
  filter(security_desc %in% acceptableList)

# Look at individual securities
individualSecuritiesTableUpdated <- table(filteredData$security_desc, filteredData$outPerformInflation) 

# Show number output
individualSecuritiesTableUpdated
```

    ##                                                 
    ##                                                   no yes
    ##   Domestic Series                                 12 252
    ##   Federal Financing Bank                          24 218
    ##   Foreign Series                                  12 252
    ##   Government Account Series                       24 240
    ##   Government Account Series Inflation Securities 114  33
    ##   R.E.A. Series                                   16 116
    ##   State and Local Government Series               84 180
    ##   Total Interest-bearing Debt                     42 222
    ##   Total Marketable                                65 198
    ##   Total Non-marketable                            24 240
    ##   Treasury Bills                                 186  78
    ##   Treasury Bonds                                  24 240
    ##   Treasury Inflation-Protected Securities (TIPS) 180  39
    ##   Treasury Notes                                  67 197
    ##   United States Savings Inflation Securities      73 189
    ##   United States Savings Securities                35 229

``` r
# Show as percentage rates for each security
prop.table(individualSecuritiesTableUpdated, margin = 1)
```

    ##                                                 
    ##                                                          no        yes
    ##   Domestic Series                                0.04545455 0.95454545
    ##   Federal Financing Bank                         0.09917355 0.90082645
    ##   Foreign Series                                 0.04545455 0.95454545
    ##   Government Account Series                      0.09090909 0.90909091
    ##   Government Account Series Inflation Securities 0.77551020 0.22448980
    ##   R.E.A. Series                                  0.12121212 0.87878788
    ##   State and Local Government Series              0.31818182 0.68181818
    ##   Total Interest-bearing Debt                    0.15909091 0.84090909
    ##   Total Marketable                               0.24714829 0.75285171
    ##   Total Non-marketable                           0.09090909 0.90909091
    ##   Treasury Bills                                 0.70454545 0.29545455
    ##   Treasury Bonds                                 0.09090909 0.90909091
    ##   Treasury Inflation-Protected Securities (TIPS) 0.82191781 0.17808219
    ##   Treasury Notes                                 0.25378788 0.74621212
    ##   United States Savings Inflation Securities     0.27862595 0.72137405
    ##   United States Savings Securities               0.13257576 0.86742424

As we can see here, the securities with the best “success” rates against
inflation are Domestic Series, Federal Financial Bank, Foreign Series,
and Treasury Bonds. The securities with the best “success” rates against
inflation are Government Account Series Inflation Securities, Treasury
Bills, and TIPS. This information really helps us in understanding what
kind of investments we might want to initially look into.

Continuing on the track of how well securities do against inflation
rates, let us create a new variable called `rateDifference` which will
be positive if the interest rate is higher than the inflation rate and
negative if vice versa. After creating this, we are going to do a 5
number summary to see what the breakdown of this difference is.

``` r
# Create new variable
interestAndInflationRates <- interestAndInflationRates %>%
  mutate(rateDifference = avg_interest_rate_amt - inflationRate)

# Show 5 number summary
summary(interestAndInflationRates$rateDifference)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -7.897  -0.257   1.177   1.245   2.926   8.304

As we can see, on average we are getting an interest rate that is about
1.245% higher than the inflation rate. We can also see that there are
some variability in this though with the worst being about -8.304% lower
interest rate than inflation rate and the best being about 8.304% higher
interest rate than inflation rate.

Now if we wanted to look at this since January 2013 (or the last 10
years of securities and inflation data) we could. We might want to do
this to try to see how things are currently looking Let us use our API
functions to get this data range (as we want to show users how to filter
from the API rather than filtering in R) and then we will combine with
the inflation data and show this difference.

``` r
# Get interest rates data
interestRatesLast10 <- chooseDataset(func = "filterDate", "interest rates", "2013-01-01", "gte")

# Remove nulls and get in right format
interestRatesLast10 <- as_tibble(lapply(removeNulls(interestRatesLast10), convertToCorrectType))

# Combine with inflation data
interestAndInflationRatesLast10 <- inner_join(interestRatesLast10, inflationData, by = c("record_calendar_year" = "year"))

# Create difference in interest and inflation numbers
interestAndInflationRatesLast10 <- interestAndInflationRatesLast10 %>%
  mutate(rateDifference = avg_interest_rate_amt - inflationRate)

# Show the summary
summary(interestAndInflationRatesLast10$rateDifference)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -7.8970 -0.9915  0.4200  0.3014  1.7685  7.8300

Now with this summary, we can see that the difference has not been as
nice as it was when we looked at all of the data. Maybe the pandemic had
something to do with this? Or possibly the government is offerring less
friendly rates as it did before. The new average we are getting an
interest rate that is about 0.301% higher than the inflation rate. This
is still slightly better than being net even, but it makes these
investments as a whole less attractive than before. Especially with the
variability, we are not as sure that we are actually going to be making
a guaranteed “positive return” after inflation.

Let us dive deeper into this and see how the different security types
and individual securities are doing. First we will examine the security
types and get the average (shown by the mean) and the variability (shown
by getting the standard deviation) and see how they compare.

``` r
interestAndInflationRatesLast10 %>%
  group_by(security_type_desc) %>%
  summarize(average = mean(rateDifference),
            standard_deviation = sd(rateDifference))
```

    ## # A tibble: 3 × 3
    ##   security_type_desc    average standard_deviation
    ##   <chr>                   <dbl>              <dbl>
    ## 1 Interest-bearing Debt  -0.257               2.36
    ## 2 Marketable             -0.531               2.62
    ## 3 Non-marketable          1.03                3.33

It is interesting to see that the average for both the Interest-bearing
Debt and Marketable securities are both at a net loss. The only one in
the money (or at a net positive) are the Non-marketable securities. This
makes sense when we think about it. Non-marketable securities aren’t as
readily available and much harder to get. The investment amount (or
principal) is usually much higher so the reward in percentage should be
higher with the elevated risk. The variability being higher also makes
sense because these securities are less regulated and thus, probably
have more range for what is being offered. Non-marketable securities are
also sold over the counter, which means that they are only available via
private transactions that make it harder for the average person to get.
After showing how great it looked to invest in the US Treasury, now it
is looking less appealing.

Maybe there are individual securities that are doing better. We are
going to do the same thing as before but this time we are going to make
sure that the securities we observe have been around for the at least
the last 10 years (or 120 months).

``` r
# criteria of being around at least last 10 years
meetsCriteria <- interestAndInflationRatesLast10 %>%
  group_by(security_type_desc, security_desc) %>%
  count() %>%
  filter(n >= 120)

# In the acceptable list
acceptable_list <- unique(meetsCriteria$security_desc)

# Find mean and sd of acceptable securities
interestAndInflationRatesLast10 %>%
  filter(security_desc %in% acceptable_list) %>%
  group_by(security_type_desc, security_desc) %>%
  summarize(average = mean(rateDifference),
            standard_deviation = sd(rateDifference))
```

    ## # A tibble: 15 × 4
    ##    security_type_desc    security_desc                 average standard_deviation
    ##    <chr>                 <chr>                           <dbl>              <dbl>
    ##  1 Interest-bearing Debt Total Interest-bearing Debt    -0.257              2.36 
    ##  2 Marketable            Federal Financing Bank          0.622              2.48 
    ##  3 Marketable            Total Marketable               -0.479              2.30 
    ##  4 Marketable            Treasury Bills                 -1.68               2.10 
    ##  5 Marketable            Treasury Bonds                  1.67               2.72 
    ##  6 Marketable            Treasury Inflation-Protected…  -1.69               2.31 
    ##  7 Marketable            Treasury Notes                 -0.654              2.29 
    ##  8 Non-marketable        Domestic Series                 5.41               2.30 
    ##  9 Non-marketable        Foreign Series                  4.97               2.24 
    ## 10 Non-marketable        Government Account Series       0.314              2.47 
    ## 11 Non-marketable        Government Account Series In…  -1.24               2.25 
    ## 12 Non-marketable        State and Local Government S…  -1.14               2.28 
    ## 13 Non-marketable        Total Non-marketable            0.298              2.47 
    ## 14 Non-marketable        United States Savings Inflat…   1.43               0.936
    ## 15 Non-marketable        United States Savings Securi…   0.538              2.37

As we can see here the Treasury Bonds and Federal Financing Bank options
are not bad for Marketable securities. We can also see that if we are
fortunate enough to invest in Non-marketable securities the best ones
against inflation are Domestic Series and Foreign Series securities. For
most regular investors, I would advice taking a look at Treasury Bonds
and Federal Financing Bank securities, if interested in investing in the
United States Treasury.

To pivot slightly, I just wanted to take a quick look at the United
States balance sheet. First let us take a look at how the US fairs with
its different accounts at the fiscal year’s end. The position of each
category is in billions of US dollars.

``` r
balanceSheetTotals <- balanceSheet %>%
  group_by(record_date, account_desc) %>%
  summarize(total = sum(position_bil_amt)) %>%
  arrange(desc(record_date))

balanceSheetTotals
```

    ## # A tibble: 84 × 3
    ##    record_date account_desc                            total
    ##    <date>      <chr>                                   <dbl>
    ##  1 2022-09-30  Assets                                19712  
    ##  2 2022-09-30  Liabilities                          147600  
    ##  3 2022-09-30  Net position                        -118038  
    ##  4 2022-09-30  Unmatched transactions and balances       3  
    ##  5 2021-09-30  Assets                                21699. 
    ##  6 2021-09-30  Liabilities                          135043. 
    ##  7 2021-09-30  Net position                        -102505. 
    ##  8 2021-09-30  Unmatched transactions and balances       4.8
    ##  9 2020-09-30  Assets                                19885. 
    ## 10 2020-09-30  Liabilities                          119377. 
    ## # ℹ 74 more rows

This shows that the US Treasury’s liabilities are much higher than its
assets. Usually this is not a good thing. In the accounting word, we
call this *asset deficiency*. This is a situation where a company’s
liabilities exceed its assets and is a sign of financial distress and
indicates that a company may default on its obligations to creditors and
may be headed for bankruptcy. This is why the US Government has to keep
raising the [debt
ceiling](https://www.cfr.org/backgrounder/what-happens-when-us-hits-its-debt-ceiling)
in order to not default. Despite this, the United States has been
considered on of the safest investments as up until this point, it has
never defaulted on a loan payment.

Something we might want to look at is what is this difference between
assets and liabilities over time and is there a cause for concern? As
investors, we want to make sure where we are putting our money is safe.
The United States has been considered a “risk-free” investment, but has
the time come for this change? Let us take a look at the growing
deficiency between the assets and liabilities over time. We are going to
show this as a line graph over time.

``` r
# Get the deficiency amount
balanceSheetDeficiency <- balanceSheetTotals %>%
  pivot_wider(names_from = account_desc, values_from = total) %>%
  mutate(Liabilities = ifelse(is.na(Liabilities), `Liabilities and net position`, Liabilities),
         deficiency = Liabilities - Assets) %>%
  rename(assets = Assets, liabilities = Liabilities) %>%
  dplyr::select(record_date, assets, liabilities, deficiency)

# Create line graph
balanceSheetDeficiency %>%
  ggplot(aes(x = record_date, y = deficiency)) +
  geom_line(color = "green") + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Fiscal Year",   
       y = "Balance Sheet Deficiency (in Trillions of USD)",   
       title = "The Balance Sheet Deficiency of the United States Treasury") +
  theme_bw()
```

<img src="README_files/figure-gfm/growing deficiencies-1.png" width="200%" height="200%" />

While we are hoping for the deficiency problem to get better, it is
actually getting worse. It seems to be growing close to an exponential
pace now. Since 2010, it has more than doubled and we can see how
rapidly it rises since 2020 with the pandemic. There might be a slight
cause for concern. Obviously, if this was a business I would recommend
not to invest in it. But since it is the United States Treasury, it
might be wishful thinking but as of right now I would only monitor the
situation and not panic yet.

Now adding to our knowledge previously discussed about interest rates,
let us take a look at how the deficiency continuing to grow affects the
types of securities interest rates on average. Again when looking at
“interest rates”, we want to see how it is performing against inflation
to see if that “risk-free” profit is still there or if we are losing our
to inflation and should look at other investments. Our line will show
the deficiency and then the colored points will show this net interest
rate (remember this is the interest rate offerred minus the inflation
rate) with the colors representing the different security types.

``` r
# First get the year of the budget so we can compare to the rate
balanceSheetDeficiencyUpdated <- balanceSheetDeficiency %>%
  mutate(year = as.numeric(substr(record_date, 1, 4))) %>%
  ungroup() %>%
  dplyr::select(year, deficiency)

# Summarize the data to find average security type net rates
netRatesType <- interestAndInflationRates %>%
  group_by(record_calendar_year, security_type_desc) %>%
  summarize(averageNetRate = mean(rateDifference)) %>%
  ungroup()

# Merge the data together
deficiencyAndRatesData <- inner_join(netRatesType, balanceSheetDeficiencyUpdated, by = c("record_calendar_year" = "year"))

# Make the graph of both measurements
# Set graph limits
ylim_defic <- c(0, max(deficiencyAndRatesData$deficiency) + 10000)
ylim_rate <- c(min(deficiencyAndRatesData$averageNetRate) - 0.2, max(deficiencyAndRatesData$averageNetRate) + 0.2)

b <- diff(ylim_defic)/diff(ylim_rate)
a <- ylim_defic[1] - b*ylim_rate[1]

# Make scaled graph
deficiencyAndRatesData %>%
  ggplot(aes(x = record_calendar_year)) +
  geom_line(aes(y = deficiency)) +
  geom_point(aes(y = a + averageNetRate*b, 
                 color = security_type_desc),
             alpha = 0.4,
             position = "jitter") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Balance Sheet Deficiency \n(in Trillions of USD)",
    sec.axis = sec_axis(~ (. - a)/b, name="Net Interest Rate (in percent)")
  ) +
  theme(text = element_text(size = 0.9),
        axis.text.x = element_text(size = 0.7),
        axis.title.x = element_text(size = 0.7),
        axis.title.y.left = element_text(size = 0.7),
        axis.title.y.right = element_text(size = 0.7),
        axis.text.y.left = element_text(size = 0.7),
        legend.key.size = element_text(size = 0.5),
        legend.text = element_text(size = 0.5),
        legend.title = element_text(size = 0.5),
        axis.text.y.right = element_text(size = 0.7)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  labs(color = "Type of Security",
    title = "Balance Deficiency Affecting Net Interest Rates",
    x = "Fiscal Year") +
  theme_bw()
```

<img src="README_files/figure-gfm/compare rates to deficiency-1.png" width="200%" height="200%" />

Here we can see that the net interest rates are decreasing over time. Is
the budget deficiency to blame? Based on the correlation between the
variables being -0.7536624 which is moderately strong so maybe this is
something to look into and really see if this is going to continue to
produce lower net interest rates with more in depth analysis.

Despite recent net returns being negative, are there some individual
securities that are performing better than expected. That is what we are
going to look at here, while comparing it to the Balance Deficiency. Is
there a security that looks to be thriving as the deficit increases?
These are all questions we hope to gloss over.

``` r
# Summarize the data to find average security type net rates
netRatesIndividual <- interestAndInflationRates %>%
  group_by(record_calendar_year, security_type_desc, security_desc) %>%
  summarize(averageNetRate = mean(rateDifference)) %>%
  ungroup()

# Merge the data together
deficiencyAndRatesIndividualData <- inner_join(netRatesIndividual, balanceSheetDeficiencyUpdated, by = c("record_calendar_year" = "year"))

# Make the graph of both measurements
# Set graph limits
ylim_defic <- c(0, max(deficiencyAndRatesIndividualData$deficiency) + 10000)
ylim_rate <- c(min(deficiencyAndRatesIndividualData$averageNetRate) - 0.2, max(deficiencyAndRatesIndividualData$averageNetRate) + 0.2)

b <- diff(ylim_defic)/diff(ylim_rate)
a <- ylim_defic[1] - b*ylim_rate[1]

# Split the data
df <- split(deficiencyAndRatesIndividualData, f = deficiencyAndRatesIndividualData$security_type_desc)

# Make scaled graph
p1 <- ggplot(df$`Interest-bearing Debt`, aes(x = record_calendar_year)) +
  geom_line(aes(y = deficiency)) +
  geom_point(aes(y = a + averageNetRate*b, 
                 color = security_desc),
             alpha = 0.3,
             position = "jitter") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Balance Sheet Deficiency \n(in Trillions of USD)",
    sec.axis = sec_axis(~ (. - a)/b, name="Net Interest Rate \n(in percent)")
  ) +
  labs(color = "individual Security",
       title = "Balance Deficiency Affecting Net Interest Rates",
       x = "Fiscal Year") +
  theme(text = element_text(size = 0.9),
        axis.text.x = element_text(size = 0.7),
        axis.title.x = element_text(size = 0.7),
        axis.title.y.left = element_text(size = 0.7),
        axis.title.y.right = element_text(size = 0.7),
        axis.text.y.left = element_text(size = 0.7),
        legend.key.size = element_text(size = 0.5),
        legend.text = element_text(size = 0.5),
        legend.title = element_text(size = 0.5),
        axis.text.y.right = element_text(size = 0.7)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  facet_wrap(vars(security_type_desc)) +
  theme_bw()

# Do it for each "facet"
p2 <- p1 %+% df$Marketable
p3 <- p1 %+% df$`Non-marketable`

# Arrange the plots and get the output we want
grid.arrange(p1, p2, p3, nrow = 3)
```

<img src="README_files/figure-gfm/compare individual rates to deficiency-1.png" width="200%" height="200%" />

As we can see, the individual securities are taking a hit too. The net
rate for each one seems to be going down for the most part. As said
before, the Treasury Bills tend to do a little better for Marketable
Securities (but they recently have had a net negative return as well)
and the Domestic Series, Foreign Series, and United States Savings
Inflation Securities have all been doing well from a Non-marketable
standpoint if you can be a part of those and seem to still give a nice
return despite the downward trend. The reason for this downward trend
could be that inflation is at recent historic highs and the interest
rates of such securities have not adjusted yet.

One potential correlation we found for looking at net rates has been the
Balance Deficiency. The next thing we can look at is the month of when
the securities are offered. Maybe it is consistently better to get one
in March than December? We should take a look and see if we find a trend
with this.

``` r
# Get Data with months being names not numbers
ratesNamedMonth <- interestAndInflationRates

# Turn into the month name; reorder levels for graph
ratesNamedMonth$named_month <- month.abb[ratesNamedMonth$record_calendar_month]
ratesNamedMonth$named_month <- factor(ratesNamedMonth$named_month, levels = month.abb)

# Split the data
df <- split(ratesNamedMonth, f = ratesNamedMonth$security_type_desc)

# Make scaled graph
p1 <- ggplot(df$`Interest-bearing Debt`, aes(x = named_month)) +
  geom_point(aes(y = rateDifference, color = security_desc),
             alpha = 0.3,
             position = "jitter") +
  labs(color = "individual Security",
       title = "Net Interest Rate Varying by Month",
       x = "Fiscal Year",
       y = "Net Interest Rate \n(in percent)") +
  theme(text = element_text(size = 0.9),
        axis.text.x = element_text(size = 0.7),
        axis.title.x = element_text(size = 0.7),
        axis.title.y = element_text(size = 0.7),
        legend.key.size = element_text(size = 0.5),
        legend.text = element_text(size = 0.5),
        legend.title = element_text(size = 0.5),
        axis.text.y = element_text(size = 0.7)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  facet_wrap(vars(security_type_desc)) +
  theme_bw()

# Do it for each "facet"
p2 <- p1 %+% df$Marketable
p3 <- p1 %+% df$`Non-marketable`

# Arrange the plots and get the output we want
grid.arrange(p1, p2, p3, nrow = 3)
```

<img src="README_files/figure-gfm/month to rate comparison-1.png" width="200%" height="200%" />

From this, there does not seem to be much difference in the net rate
from the security dependent on the month. Therefore, initially we can
say there is not an optimal time of a given year to maximize our net
rate. So from this standpoint, any time is equally good or bad to
invest.

The last thing we are going to look at in our exploratory data analysis
is showing how the interest rates and the inflation rates from the US
Treasury Securities change over the years so investors know what a fair
going rate is, depending on what inflation rate is for a given time. We
will try to see if there is a pattern so we can predict the inflation
rates and interest rates in later time so we can cash in if we see a
rate above expected

``` r
animatedPlot <- interestAndInflationRates %>%
  ggplot(aes(x = inflationRate, y = avg_interest_rate_amt, color = security_type_desc)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  facet_wrap(vars(security_type_desc), nrow = 3) + 
  theme(text = element_text(size = 0.9),
        axis.text.x = element_text(size = 0.7),
        axis.title.x = element_text(size = 0.7),
        axis.title.y = element_text(size = 0.7),
        legend.key.size = element_text(size = 0.5),
        legend.text = element_text(size = 0.5),
        legend.title = element_text(size = 0.5),
        axis.text.y = element_text(size = 0.7)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  theme_bw() +
  labs(title = 'How the Inflation Rates and Interest Rates \nwere in the Year: {frame_time}', 
       x = 'Inflation Rate (in percent)', 
       y = 'Interest Rate Offerred (in percent)') +
  transition_time(record_calendar_year) +
  ease_aes()

# Save and show the animated plot
animate(animatedPlot, width = 800, height = 800, renderer = gifski_renderer())
```

<img src="README_files/figure-gfm/looking at if the two rates affect each other-1.gif" width="200%" height="200%" />

As we can see here
