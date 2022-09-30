# Shiny App deployment with binder


Mon test bus-dashboard: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/glouarn/ShinyApp-binder?urlpath=shiny/bus-dashboard/HEAD)


### URL addresses for RStudio and Shiny environments

The Binder repository can be used to allow anyone to access an RStudio environment containing our code and data right
in their web browser. It also allows hosting a Shiny app. For those purposes, we have to append a bit of text to the
URL of our Binder repository, which we can find out at [mybinder.org](https://mybinder.org/) when we enter
the URL of our original repository from GitHub or Figshare, etc.

- For the RStudio environment, we must add the following at the end of the URL: `?urlpath=rstudio`

  - Example: http://mybinder.org/v2/gh/binder-examples/r/master?urlpath=rstudio

- For the Shiny app environment, we must add the following at the end of the URL: `?urlpath=shiny`. In this case, we
also have to note that if the Shiny app files are located in a folder, this folder should be specified in the URL,
after a slash. We would then also have to put in a trailing slash at the end of the URL, and to avoid spaces in the
name of the repository, placing instead a hyphen (the reason is that spaces are converted to `%20`).

  - Example: http://mybinder.org/v2/gh/binder-examples/r/master?urlpath=shiny/bus-dashboard/
