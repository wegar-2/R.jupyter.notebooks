# R.jupyter.notebooks


## Repo Structure 

This repo contains my jupyter notebooks with R code. This repo is just a bucket for various analyses - there is no single theme here (cf. 'Contents' section below for brief descriptions of datasets analyzed).

The main folder contains jupyter notebooks and subfolders.

The subfolders are:

* *data* - I store data used in the analyses here
* *scripts_archive* - I store code used in jupyter notebooks here (I find it more convenient to write code in R first and move it to jupyter notebooks afterwards)

Please note that this repo is not structured as a proper [R package](https://r-pkgs.org/).


## Contents

The analyses here are - frankly - ramblings of mine. 

I just happen to get an urge to look at a data set I might come across every now and then so I store the results of such peeks into data in here to keep things tidy.

* [Bitcoin mining electricity consumption](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/bitcoin_electricity_consumption.ipynb) - I calculate a very, **very** rough estimate of the percentage of world's electricity generation consumed by Bitcoin mining using data from 
* [Comparison of GDP of Great Powers](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/great_powers_gdps_comparison.ipynb) - comparison of GDPs of potential geopolitical blocks. Incomplete! *There is one crucial flaw in this analysis - I have not included India in it with yet*. Also, more scenarios should be analyzed as regards the potential blocks
* [Graphic demonstration of Simpson's paradox in R](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/great_powers_gdps_comparison.ipynb) - very simple demonstration of [Simpson's paradox](https://en.wikipedia.org/wiki/Simpson%27s_paradox) on generated data
* [Oil Brent & WTI prices comparison](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/oil_brent_and_wti_plots.ipynb) - simple plots of prices of two core western oil benchmarks: WTI and Brent
* [GFZ Kp and ap indexes plotting](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/R_GFZ_geomagnetic_EDA.ipynb) - plots of time series of Kp and ap magnetic indexes downloaded from the website of [GFZ Helmholtz Centre Potsdam](https://www.gfz-potsdam.de/en/section/geomagnetism/data-products-services/geomagnetic-kp-index). The data used in this analysis can be retrieved using R code stored in [my different repo](https://github.com/wegar-2/okeanos.astro)
* [Euribor rates EDA](https://github.com/wegar-2/R.jupyter.notebooks/blob/master/euribor_22years_EDA_part1.ipynb) - exploratory data analysis of Euribor rates. Note: Euribor rates dataset used in this notebook not posted in this repo - I downloaded the data before EMMI restricted access to it, so I am not sure if I can post it here (I'd rather not get sued for infringement of IP)