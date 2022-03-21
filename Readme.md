# The `growthis` package


## Introduction

This package can be used to view, edit and analyse Varioscan growth experiment data.


## Getting started

### Requirements

1. R (version >4.1)
2. RStudio (version >2021.09.01)
3. Installed R packages:
    - devtools


### Install

1. Start RStudio, go to the console
2. Install the package devtools if not already present: `install.packages("devtools")`
3. Load devtools: `library(devtools)`
4. Install package `growthis`: `install_github("MichielNoback/growthis")`. This will probably ask you to update some (or quite a few) packages. 
   Preferably select `All` (option 1). This may take a while.
5. Load growthis: `library(growthis)`.
6. Test-run the Shiny app: `shiny_app()`.
   You may get an error message like this:
   
    ```
    Loading required package: shiny
    
    Listening on http://127.0.0.1:3758
    Warning: Error in loadNamespace: namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
      50: list2
      49: shiny::reactiveValues
      48: server
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    ```
   If so, quit the Shiny app (not RStudio!; press escape one or two times in the console) and run the command `install.packages("rlang")`.
   You may have to repeat for other packages that are used by growthis. 


## Usage

The shiny app can be run by simply typing `shiny_app()` in the console.

If you are prepared to use R, several functions are at your disposal. Type `growthis::` in the console and you will see all available datasets and functions.
The main functions are:

- `read_experiment_data()` to load from Excel files (this requires some metadata to be present).
- `filter_data()` to filer imported datasets.
- `plot_growthcurves()` to make plots.
- `do_growth_analysis()` will give growth statistics. This requires the input of data in "wide" format; the function `create_wide_data()` does exactly that.


In the console, type `?<function name>` to get details on their usage.
