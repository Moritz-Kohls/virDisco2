### virDisco2
A refined approach for generating decoy reads together with a strategy of resampling from the original findings is proposed in order to estimate diagnostic measures more accurately and to assess the robustness of results. The first step of the new approach is to estimate the statistical distributions from the original mapping file. Next, these distributions are used to simulate two new paired-end read-files (step 2) which are again mapped versus the virus reference genomes (step 3). From the new mapping results, mapping and error rates are calculated (step 4) which can help to assess the robustness of primary results.

### Installation
If you want to install R-packages from GitHub, at first you need to install the R-package **devtools** from CRAN:

```r
install.packages("devtools")
library(devtools)
```
After this, you can install packages from GitHub by using the R-function ```install_github``` and specifying the author and package name. For example, if you want to install the R-package **virDisco2**, just type the following:

```r
install_github("Moritz-Kohls/virDisco2")
```
