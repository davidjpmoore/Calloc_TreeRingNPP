#install required packages
#author: Dave Moore
#Date: 07/06/2015
#Purpose: Set up script to install all required packages before entering the workflow of Calloc_TreeRingNPP

### If you have trouble with this script - please read :http://www.r-bloggers.com/installing-r-packages/ 
install.packages("dplR")

#"Dendrochronology Program Library in R" http://cran.r-project.org/web/packages/dplR/dplR.pdf
install.packages("lattice")

#- graphics - http://cran.r-project.org/web/packages/lattice/index.html

install.packages("reshape")

#- data handling - melt and cast - http://cran.r-project.org/web/packages/reshape/

install.packages("car")

#- Fox Book functions - http://cran.r-project.org/web/packages/car/index.html

install.packages("mgcv")

#- generalised additive mixed effects models - http://cran.r-project.org/web/packages/mgcv/index.html

install.packages("nlme")

#- Gaussian linear and nonlinear mixed-effects models - http://cran.r-project.org/web/packages/nlme/index.html

install.packages("lmeSplines")

#- splines splines spines - http://cran.r-project.org/web/packages/lmeSplines/index.html

install.packages("MASS")

#- bag of stats functions -Venables and Ripley - http://cran.r-project.org/web/packages/MASS/index.html

install.packages("MuMIn")

#- information criteria & Model selection -http://cran.r-project.org/web/packages/MuMIn/index.html

install.packages("ggplot2")

#- Wickham's Graphics Grammar - http://cran.r-project.org/web/packages/ggplot2/index.html

install.packages("grid")

#Package ‘grid’ was removed from the CRAN repository. Formerly available versions can be obtained from the archive.

install.packages("splines")

#- Package ‘splines’ was removed from the CRAN repository. Formerly available versions can be obtained from the archive. Do not remove that package from the script. It is the basis for all of the gap filling to come. You can still pull the package out of the archive here: http://cran.r-project.org/src/contrib/Archive/splines/
#
#
#How do I install a package that has been removed from the archive: http://stackoverflow.com/questions/24194409/how-do-i-install-a-package-that-has-been-archived-from-cran
#
#
