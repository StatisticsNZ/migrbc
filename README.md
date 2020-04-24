
# migrbc

### Introduction

Package `migrbc` provides mechanisms for classifying border crossings using a rules-based methodology. The goal of performing this type of classification is to identify any potential long-term migrants. A long-term migration is defined as a border crossing involving a change in residence status. A border crossing counts as a long-term migration to/from a country if it entails a change from non-residence to residence or residence to non-residence. The rules-based classification that used to determine a long-term migration is defined by a threshold duration and a test duration, alternatively named window size. Under a "12/16" rule, for instance, the threshold duration is 12 months and the test duration (window size) is 16 months. With a "9/12" rule, the threshold duration is 9 months and the test duration (window size)  is 12 months. 

The main functions are:

* `run_rbc`: determines long-term migration statuses, and pre-crossing and post-crossing residence statuses, for all crossings where these statuses are not known.

* `pre_process`: provides a mechanism to divide large data into small chunks.

* `plot_mig_hist*`: by given a sequence of border crossings for a person, draw a diagram describing that person's migration history.

### Installation

Install development version `migrbc` from github:

```{r, echo = FALSE}
library(devtools)
install_github("StatisticsNZ/migrbc")
```

Install stable version `migrbc` from CRAN: 

```{r, echo = FALSE}
install.packages("migrbc")
```
### Documentation

Vignettes PDF URL:

https://github.com/StatisticsNZ/migrbc/tree/master/doc/migrbc.pdf


---
__Copyright and Licensing__

The package is Crown copyright (c) 2020, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the MIT License.

<br /><a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This document is Crown copyright (c) 2020, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
