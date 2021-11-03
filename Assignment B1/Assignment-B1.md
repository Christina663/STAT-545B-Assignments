Assignment B1
================
Christina
10/31/2021

``` r
suppressPackageStartupMessages(library(datateachr)) 
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(testthat))
```

Before starting, let’s make some simple tribbles of temperature data for
our later examples:

``` r
tempC <- tribble(
  ~x, ~Celsius, 
  "a", 20, 
  "b", 17,
  "c", 25,
)

tempCF <- tribble(
  ~x, ~Celsius, ~Fahrenheit,
  "a", 20, 68,
  "b", 17, 62.6,
  "c", 25, 77,
)
```

# Exercise 1 and 2: Make a function and write documentations

**In this exercise, we’ll be making a function and fortifying it.** **In
the same code chunk where you made your function, document the function
using roxygen2 tags.**

For Exercise 1, we make a function temperature_conversion that takes in
a numerical value of temperature and convert it to another temperature
unit. There are three options for the input unit and output unit:
Fahrenheit, Celsius and Kelvin. We perform Exercise 2 in the comments
above the function code and provided documentations of the function
description, arguments and return values.

``` r
#' Convert a numerical representation of temperature between three units: Fahrenheit, Celsius and Kelvin.
#' 
#' @param input_unit unit of the input temperature; The name input_unit indicates that it's the unit of our input temperature
#' @param output_unit unit of the output temperature; The name output_unit indicates that it's the unit of our output temperature
#' @return the converted temperature in the specified output unit

temperature_conversion <- function(input_unit, output_unit, input_temp) {
  if (!((input_unit == "C") | (input_unit == "Celsius") | (input_unit == "K") | (input_unit == "Kelvin") | (input_unit == "F") | (input_unit == "Fahrenheit"))) {
    stop("Input unit invalid, must be one of Fahrenheit, Celsius, Kelvin or F, C, K")
  }
  if (!((output_unit == "C") | (output_unit == "Celsius") | (output_unit == "K") | (output_unit == "Kelvin") | (output_unit == "F") | (output_unit == "Fahrenheit"))) {
    stop("Output unit invalid, must be one of Fahrenheit, Celsius, Kelvin or F, C, K")
  }
  
  output_temp = case_when((input_unit == "C" | input_unit == "Celsius") & (output_unit == "K" | (output_unit == "Kelvin")) ~ input_temp + 273.15,
                          (input_unit == "K" | input_unit == "Kelvin") & (output_unit == "C" | (output_unit == "Celsius")) ~ input_temp - 273.15,
                          (input_unit == "F" | input_unit == "Fahrenheit") & (output_unit == "C" | (output_unit == "Celsius")) ~ (input_temp - 32) * 5 / 9,
                          (input_unit == "C" | input_unit == "Celsius") & (output_unit == "F" | (output_unit == "Fahrenheit")) ~ (input_temp * 9 / 5) + 32,
                          (input_unit == "F" | input_unit == "Fahrenheit") & (output_unit == "K" | (output_unit == "Kelvin")) ~ (input_temp - 32) * 5 / 9 + 273.15,
                          (input_unit == "K" | input_unit == "Kelvin") & (output_unit == "F" | (output_unit == "Fahrenheit")) ~ ((input_temp - 273.15) * 9 / 5) + 32,
                          TRUE ~ NA_real_)
  return (output_temp)
}
```

# Exercise 3: Include examples

**Demonstrate the usage of our function with a few examples. Use one or
more new code chunks, describing what we’re doing.**

For this exercise, we list three basic uses of the function
temperature_conversion.

``` r
temperature_conversion("Celsius", "Kelvin", 50)
```

    ## [1] 323.15

Here we convert 50 degree Celsius into Kelvin. We need to input the
input_unit “Celsius”, the output_unit “Kelvin”, and the temperature
before conversion “50”. The value returned from calling the function is
thus our temperature in Kelvin.

``` r
temperature_conversion("K", "F", 50)
```

    ## [1] -369.67

Here we convert 50 degree Kelvin into Fahrenheit. We need to input the
input_unit “K” which is another valid input representing “Kelvin”, the
output_unit F representing “Fahrenheit”, and the temperature before
conversion “50”. The value returned from calling the function is thus
the temperature in Fahrenheit.

``` r
mutate(tempC, Fahrenheit = temperature_conversion("C", "F", Celsius))
```

    ## # A tibble: 3 x 3
    ##   x     Celsius Fahrenheit
    ##   <chr>   <dbl>      <dbl>
    ## 1 a          20       68  
    ## 2 b          17       62.6
    ## 3 c          25       77

Here we take in a dataset with a column of temperature in Celsius, and
convert each temperature into Fahrenheit using the
temperature_conversion function. The result will have a new column
containing each corresponding Fahrenheit temperature value.

# Exercise 4: Test the Function

**Running examples is a good way of checking by-eye whether your
function is working as expected.**

For this exercise, we perform 4 tests that check the function’s handling
of different input types, NA input data values, invalid input_unit or
output_unit and how it operates on column variable of a dataset.

``` r
test_that("Different input types", {
  expect_equal(temperature_conversion("Celsius", "Kelvin", 100), 373.15 , tolerance = 1e-4)
  expect_equal(temperature_conversion("F", "C", 50), 10, tolerance = 1e-4)
  expect_equal(temperature_conversion("Kelvin", "F", 10), -441.67, tolerance = 1e-4)
  
})
```

    ## Test passed

``` r
test_that("NA input data values", {
  expect_equal(temperature_conversion("Kelvin", "F", NA), NA_real_)
  
})
```

    ## Test passed

``` r
test_that("Invalid input_unit or output_unit", {
  expect_error(temperature_conversion("Kelvin", "Cels", 10))
  expect_equal(temperature_conversion("K", "K", 10), NA_real_)
})
```

    ## Test passed

``` r
test_that("Operate on column variable of a dataset", {
  expect_equal(mutate(tempC, Fahrenheit = temperature_conversion("C", "F", Celsius)), tempCF, tolerance = 1e-4)
})
```

    ## Test passed
