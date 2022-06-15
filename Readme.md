# The `growthis` package


## Introduction

This package can be used to view, edit and analyse Varioskan growth experiment data generated in a 96-well Excel format.


## System Requirements

You need to have these two or three tools installed.  

1. R (version >4.1). Download from https://cran.r-project.org/  
2. RStudio (version >2021.09.01). Download from https://www.rstudio.com/products/rstudio/download/ (choose RStudio Desktop Free).  
3. [Windows only] On Windows you will need RTools, which can be downloaded at [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/). Choose the version that is advised for your R version. You can check the R version in RStudio: at startup it will display a message in the Console pane:

    ```
    R version 4.1.2 (2021-11-01) -- "Bird Hippie"
    Copyright (C) 2021 The R Foundation for Statistical Computing
    Platform: aarch64-apple-darwin21.1.0 (64-bit)
    ...
    ```
    
    So in this example the R version is 4.1.2 and I should download RTools version 4.0 (if I were on a Windows machine).

If you have an R version below 4.0 or RStudio below 2021.0 I suggest you upgrade first (same as the steps above). Minor RStudio updates can be done from within RStudio itself: via Help &rarr; Check for updates. Have a look at https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol for more info.


## Getting started

### First-time installation of the `growthis` package

Once you have R and RStudio in place you need to set up the R environment to install and work with the `growthis` package and app. Usually in R you will use the function `install.packages()` to add external libraries to R (a library is a set of functions and sometimes also data). This function will look in the central repository for all checked and registered packages: CRAN. Not all libraries are hosted in CRAN however, and `growthis` is one of them, and to get these installed we need other means. One of these is via `devtools::install_github()`. `devtools` is a package that _is_ hosted on CRAN.  

Here are the steps (with some explanation):  

1. Start RStudio and go to the Console panel. All commands below need to be entered in the Console.    
2. Type `install.packages("devtools")` and press Enter. This will **_install_** the package devtools in the running R distribution if not already present.
3. Type `library(devtools)` and press Enter. This will **_load_** devtools in the current R session.  
4. Type `install_github("MichielNoback/growthis")` and press Enter. This will install package `growthis`.  
    R will probably "ask" you to update some (or quite a few) packages. You should preferably select `All` (option 1). This may take a while on some systems. Don't be scared of all the text running by. However, if the stream stops and it say "Error..." something has gone wrong and you should ask a more experienced colleague for help.
5. Test it. Type `library(growthis)` and press Enter. This will load the growthis package. If it prints 
    `Error in library(growthis) : there is no package called ‘growthis’` something has gone wrong in step 4 (or before). Try the installation again, reading carefully any warning and error messages. If that fails, try to get help at this point.  
6. Test it for real. Type `shiny_app(launch_browser = TRUE)` and press Enter. This will run the Shiny app. 
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
   If so, quit the Shiny app (not RStudio!; press escape one or two times in the Console) and run the command `install.packages("rlang")`.
   You may have to repeat for other packages that are used by `growthis`.  
7. You can quit the app by pressing the Escape key in the Console.


### Usage of the `growthis` Shiny app

These steps will need to be repeated every time you start RStudio to work with the app.
In the Console panel, type these commands.    

1. Type `library(growthis)` and press Enter. 
2. Type `shiny_app(launch_browser = TRUE)` and press Enter. 

Alternatively, you can start the app in one command: `growthis::shiny_app(launch_browser = TRUE)` and press Enter.
You can also run the command `shiny_app()` and then open the URL (adress) which is reported in the console manually (`http://127.0.0.1:3838`).  

Quit the app by pressing the Escape key in the Console. The browser tab should be closed as well. It is not active anymore. Only closing the browser tab will not shut down the app. Note that an (inactive) running app in RStudio may drain the battery of your laptop.


### Updating the `growthis` package

The package is in development and releases follow each other frequently.
You can update the current package by typing the following in the Console:

1. Type `library(devtools)` and press Enter.
2. Type `install_github("MichielNoback/growthis")` and press Enter. 

This will update the package to the most recent version.


### Usage of the other `growthis` package functions.

If you are prepared to use R, several functions are at your disposal. Type `library(growthis)` to get it in the current session. Type `growthis::` in the console and you will see all available datasets and functions. All functions have within-R documentation. You can see these in the Help panel when you type `?functions_name` in the Console (in RStudio of course).  
The main functions are:

- `read_experiment_data()` to load from Excel files (this requires some metadata to be present).
- `filter_data()` to filter imported datasets.
- `plot_growthcurves()` to make plots.
- `do_growth_analysis()` will give growth statistics. This requires the input of data in "wide" format; the function `create_wide_data()` does exactly that.


In the console, type `?<function name>` to get details on their usage.

