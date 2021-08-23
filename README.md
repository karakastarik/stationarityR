# stationarityR

## Automating Time Series Stationarity Tests

This package automates Kwiatkowski–Phillips–Schmidt–Shin (KPSS), Augmented Dickey–Fuller (ADF) and Phillips–Perron (PP) unit root tests and saves the results as a dataframe.

### Getting Started

```devtools``` must be installed to access the ```stationarityR``` package. If not installed:

```
install.packages("devtools", dependencies = TRUE) 
```
Then you can use following code to install ```stationarityR``` package:
```
library(devtools)
devtools::install_github("karakastarik/stationarityR")
library(stationarityR)
```
### Functions

For 0.1.0 release, only ```summary_all(model,lag)```, ```summary_kpss(model,lag)```,```summary_adf(model,lag)``` and ```summary_pp(model,lag)``` functions are available and these functions returns a dataframe which calculates all possible combinations for unit root tests. In the functions, ```model``` is fitted ```lm(y ~ x)``` object and ```lag``` is integer lag length. For example, if the ```lag``` value is 10, results will come for lag lengths of 1:10.

### Output

You can see the example dataframe which returned by ```summary_all(model,lag)``` function below.

![Output](https://github.com/karakastarik/stationarityR/blob/main/www/output.PNG)
The function uses KPSS, ADF and PP unit root tests and decides whether the series is stationary or not according to the lag length, type and significance level (1%, 2.5%, 5% and 10%) as seen above. If the value is ```pass```, it means the series is stationary, and if it is ```fail```, it means the series is not stationary. You can reach this conclusion by examining statistics and critical values(10pct, 5pct...).
