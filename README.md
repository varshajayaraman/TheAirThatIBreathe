# TheAirThatIBreathe
The visualisation can be viewed at http://shiny.evl.uic.edu:3838/p3g3/The_Air_That_I_Breathe/

MORE ABOUT THE PROJECT: https://sthiru5.people.uic.edu/CS424/project3.html

Please follow the below to run the code.

Software required to run the code: R and R Studio

To download and install R: From site https://cran.cnr.berkeley.edu/ and installing it. (R (20178-12-20 Eggshell Igloo))

To download and install R-Studio: From site https://www.rstudio.com/products/rstudio/download/ and installing it. (R-Studio (1.1.463))

Libraries needed to run the application:

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(splitstackshape)
library(cdlTools)
library(AotClient)
library(tidyr)
library(darksky)
library(rgdal)
library(sp)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(httr)
library(jsonlite)
library(rjson)
library(ropenaq)
library(promises)
library(future)
library(plyr)
plan(multiprocess)

To see what libraries are currently installed, in the terminal at the > prompt, type installed.packages()[,1:2]

If any library is missing, install them using install.packages(). Example: install.packages(shiny)

Now to run the application, from the folder, open app.R file in R Studio. Once the file opens, click on "Run App" option on the right top of the file window. This opens the application.
