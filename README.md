### Revisiting COVID-19’s Impact on Cycling: An Examination of Bicycle Count Data in Montréal

# Description
This project contains the code used in the following article: <br/>
[Revisiting COVID-19’s Impact on Cycling: An Examination of Bicycle Count Data in Montréal](https://findingspress.org/article/118813-revisiting-covid-19-s-impact-on-cycling-an-examination-of-bicycle-count-data-in-montreal)

## Instructions

Open the Rproj file, then access the individual scripts from the file menu <br/>

## Script descriptions

**1-Data_Preparation** :  Consolidates Montreal bike count data from multiple years <br/>
**2-Imputation (Day)** :  Imputes missing values <br/>
**3-Descriptive_Stats** : Descriptive statistics for the data (weekend/weekday ridership, high/low season ridership, percent changes per year, effect of COVID-19) <br/>

## Folder structure 

**DataRaw** : Contains the raw bike count data obtained fro the [City of Montreal](https://donnees.montreal.ca/en/dataset/velos-comptage) <br/>
**DataTreated** : Contains all the processed data that was created with the scripts <br/>
**Outputs** : Contians all the tables and figures created with the scripts <br/>
**Scripts** : Contains the individual R scripts <br/>

## Packages
"tidyverse" <br/>
"imputeTS" <br/>
"forecast" <br/>
"openxlsx" <br/>

Note : All package installation is handled within the scripts.

# Note:

We have made minor adjustments to the imputation script for Figure 2 since publication. <br/>
For context, we selected the University_Milton sensor for this figure as it has an extensive period of missing data, <br/>
which allowed us to effectively demonstrate the merits and efficacy of our employed imputation process. <br/>

However, it's important to note that the COVID-19 pandemic impacted data in 2020, which in turn affected our imputation for 2019. <br/>
To mitigate this issue, we split sensors with missing 2019 data using January 2020 as a cutoff point. <br/>
Following imputation, the data sets were recombined. <br/>
This method is implemented starting at line 289 of script #2. <br/>
However, as this methodology generates separate imputation graphs, we opted to show University_Milton without applying this technique in Figure 2. <br/>

All findings presented in the article, except those in Figure 2, reflect the results obtained after applying this split sensor methodology. <br/>

# Author

Gavin MacGregor