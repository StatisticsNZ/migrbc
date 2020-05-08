
# migrbc

### Introduction

This package provides mechanisms for classifying border crossings using a rules-based methodology. The goal of performing this type of classification is to identify any potential long-term migrants. A long-term migration is defined as a border crossing involving a change in residence status. A border crossing counts as a long-term migration to/from a country if it entails a change from non-residence to residence or residence to non-residence. The rules-based classification that used to determine a long-term migration is defined by a threshold duration and a test duration, alternatively named window size. Under a 12/16 rule, for instance, the threshold duration is 12 months and the test duration (window size) is 16 months. With a 9/12 rule, the threshold duration is 9 months and the test duration (window size)  is 12 months. For more information about the methodology applied, please visit Stats NZ (2020) <https://www.stats.govt.nz/methods/defining-migrants-using-travel-histories-and-the-1216-month-rule>.

The main functions are:
* `initialize_logger`:  is used to initialize the futile.logger so that the user can be notified with the current status of running RBC.

* `run_rbc`: determines long-term migration statuses, and pre-crossing and post-crossing residence statuses, for all crossings where these statuses are not known.

* `pre_process`: provides a mechanism to divide large data into small chunks.

* `plot_mig_hist*`: by given a sequence of border crossings for a person, draw a diagram describing that person's migration history.

### Installation
#### Install development version `migrbc` from GitHub:
devtools::install_github("StatisticsNZ/migrbc")

#### Install stable version `migrbc` from CRAN: 
devtools::install.packages("migrbc")

#### Documentation
https://statisticsnz.github.io/migrbc/

#### Setting up a Logger (Optional)
We applied futile.logger for troubleshooting and hence, provided an utility function for initializing the logger. More information about the logger can be found with the link (https://CRAN.R-project.org/package=futile.logger). The parameter log_level is a number in the set of 1, 2, 4, 6, 8, and 9:
```
futile.logger::FATAL: 1
futile.logger::ERROR: 2
futile.logger::WARN:  4
futile.logger::INFO:  6
futile.logger::DEBUG: 8
futile.logger::TRACE: 9
```
##### Suppresse log messages to the console: 
migrbc::initialize_logger(log_level = 1)
 
##### Display all type of log messages to the console
migrbc::initialize_logger(log_level = 9)

---

__Copyright and Licensing__

The package is Crown copyright (c) 2020, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the MIT License.

<br /><a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This document is Crown copyright (c) 2020, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
