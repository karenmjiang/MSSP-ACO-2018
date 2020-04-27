#  [Accountable Care Organizations 2018](https://karenmjiang.shinyapps.io/Accountable_Care_Organizations_2018)

> The app may take up to 20 seconds to load

This project looks at the generated savings or losses from ACOs for Medicare based on ACO, patient, and provider factors.

## Background
The project was developed as the final project for Gov 1005 at Harvard. 

## Details
This app was built in R, using [Flexdashboards](https://github.com/rstudio/flexdashboard) for formatting and Shiny and for reactivity, and hosted by the ShinyApps server. 

The bulk of the code is within the `app.Rmd` file. 

Raw data files are stored in the `data` folder. The `data_prep.R` script cleans and saves the data into the `clean_data` folder. The `shiny` folder stores code used for the development and testing of certain pages. 
