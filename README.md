# Performance Based Contracting for Colorado Community Corrections

The Urban Institute conducted a baseline outcome assessment and developed a performance-based contracting plan for the Colorado Department of Public Safety, Office of Community Corrections. This repositoty contains the code for the analysis. The full report, including a detailed technical appendix, is available [here](https://www.urban.org/research/publication/performance-based-contracting-colorado-community-corrections).


## Purpose of code

### 1. Develop baseline outcome assessment that is stratified by risk level
* Use FY14-18 for successful completion, employment, LSI change, technical violation, and escape outcomes
*	Use FY14-17 for recidivism outcomes
*	Create tables and graphs showing means by risk level
* Note outcomes are calculated at two levels:
  * Average of all clients released of a given risk level (state-level)
  * Average of all programs of a given risk level, based on their average risk score (program-level)


### 2. Evaluate program performance in FY19 against those baseline measurements (FY18 for recidivism)
*	Requires categorizing programs by risk level
*	Create tables and graphs showing percentage of programs meeting those baseline targets by risk level
* Note program risk level is calculated two ways:
  * Whether 50% or more of clients in a program are high/very high risk
  * Whether the average risk score of the program falls into the high/very high range (29+)

### 3. Examine reliability of outcomes and other assessment tools used by state (PACE and Core)
*	Correlation analysis for outcomes
*	Cronbach’s alpha analysis for outcomes, PACE, and Core
*	Create tables of results


## R packages 
Analysis completed in R 3.6.3. The packages used are listed below. Note that the urbnthemes package is used to format the graphs according to the Urban Institute style.

```
library(tidyverse)
library(haven)
library(readxl)
library(ltm) 
library(Hmisc)

install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnthemes")
library(urbnthemes)

set_urbn_defaults(style = "print")
```

## Data

All data were provided by the Colorado Department of Public Safety, Office of Community Corrections. There are three files for analysis:

* file of individual-level data on community correction clients
* program scores on the PACE assessment
* program scores on the Core Security Audit

Recidivism data come from Colorado court data and were merged into individual-level data.

None of the data are publicly accessible. 

## Code files

###	01_baseline_targets_co.R
*	create state-level and program-level baseline targets
*	conduct significance tests on each outcome by risk group
*	graph targets by risk
*	save table of baseline targets

###	02_eval_performance_co.R
*	calculate how many programs met baseline targets
  * administrative outcomes in FY19
  *	recidivism outcomes in FY18
*	graph percent meeting mean by outcome and risk group
*	save data set of performance by outcome and risk group

###	03_reliability_co.R
*	descriptive statistics for PACE, Core, state-level outcomes, and program-level outcomes overall (mean, median, n)
*	Cronbach’s alpha and correlation analysis


## Creators

Ashlin Oglesby-Neal and Libby Doyle
