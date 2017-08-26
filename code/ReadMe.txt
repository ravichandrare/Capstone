HABase - Web-based Spectra Analysis

The main aim of the project is to automate the process of MALDI-Quant, which processes the protein spectrum of bacteria. HABase web-based application using machine-learning algorithms to optimize and automatically adjust the critical steps of setting signal to noise ratios, selecting and aligning peaks, and cluster analysis of protein spectra. The main goal of project is to make bioinformatics analyses accessible to the microbiologists responsible for assessing water quality and seafood safety.

Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

Prerequisites

Mac OS or Windows OS or Ubuntu

Install

1. R programming Language: 

https://cran.rstudio.com

2. RStudio 

https://www.rstudio.com/products/rstudio/download/

3.  Open RStudio and install following packages:

Copy Paste following line in Console window and hit enter

install.packages("shiny")
install.packages("shinythemes")
install.packages("MALDIquant")
install.packages("MALDIquantForeign")
install.packages("pvclust")
install.packages("caret")
install.packages("ggplot2")

4. Put the server.R and ui.R file in single file and then open in RStudio.

5. Click Run button 

Built With

R Programming Language 
RStudio - Development Environment
RShiny - Web Framework

Versioning

We use BitBucket for versioning. 

Acknowledgments

Sebastian Gibb
