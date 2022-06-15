# Common tools that are not installed by default
install.packages('tidyverse')
install.packages('data.table')
install.packages('Hmisc')
install.packages('cowplot')

# Interactive dataviz
install.packages('plotly')
install.packages(c("dashCoreComponents", "dashHtmlComponents", "dashTable", "dashboard"))
install.packages(c("fiery", "routr", "reqres", "htmltools", "base64enc", "plotly", "mime", "crayon", "devtools"))
library(devtools)
# installs dashHtmlComponents, dashCoreComponents, and dashTable
# and will update the component libraries when a new package is released
install_github("plotly/dashR", upgrade = TRUE)
# install.packages('')

# Utilities for working with the ops db
remotes::install_github("inform-health-informatics/wranglEHR")

