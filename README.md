# R-crawler

A custom crawler using 'rvest'(https://cran.r-project.org/web/packages/rvest/index.html) library, built to extract data from "Algorithms for Molecular Biology" (https://almob.biomedcentral.com/articles) until the input year.

The Crawler extracts links from all pages and iterates through each link, extracts data and appends to a dataframe. The dataframe is saved as a excel workbook using the "openxlsx" library

Xpath of each column was extracted manually from the journal and used to extract data.
