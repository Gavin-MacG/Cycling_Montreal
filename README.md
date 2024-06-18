#Revisiting COVID-19’s Impact on Cycling: An Examination of Bicycle Count Data in Montréal

## Description
This project contains the code found in the following article: 
https://findingspress.org/article/118813-revisiting-covid-19-s-impact-on-cycling-an-examination-of-bicycle-count-data-in-montreal


## Packages
install.packages("tidyverse")
install.packages("imputeTS")
install.packages("forecast")
install.packages("openxlsx")


## Note:

We have made minor adjustments to the imputation script for Figure 2 since publication. 
For context, we selected the University_Milton sensor for this figure as it has an extensive period of missing data, 
which allowed us to effectively demonstrate the merits and efficacy of our employed imputation process.

However, it's important to note that the COVID-19 pandemic impacted data in 2020, which in turn affected our imputation for 2019. 
To mitigate this issue, we split sensors with missing 2019 data using January 2020 as a cutoff point. 
Following imputation, the data sets were recombined. 
This method is implemented starting at line 289 of script #2. 
However, as this methodology generates separate imputation graphs, we opted to show University_Milton without applying this technique in Figure 2. 

All findings presented in the article, except those in Figure 2, reflect the results obtained after applying this split sensor methodology.