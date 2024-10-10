
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osmx

The primary purpose of the {osxm}-package is to render the now infamous
“Mælaaborð Íslenzkra Ralla” shinyapp.

The `smxapp` relies on 2 other packages:

- {mardata}: Package that contains historical data
- {[ovog](https://heima.hafro.is/~einarhj/pkg/ovog)}: A package that
  reads and transforms zip-files dumped from hafvog.

The {osmx}-packages does the following:

- Merges the historical and currently collected data
- Creates various summary and quality control tables (TODO: Explain each
  table)
- Provides plots- and table-functions that are used in the shinyapp
- Provides a shiny-app template for easy exploration of the data

## Installation

You can install {ovog} and {osmx} from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
remotes::install_github("einarhjorleifsson/ovog")
remotes::install_github("einarhjorleifsson/osmx")
```

The mardata is a MFRI data-package that is not publicly available. If
you are within the Institute’s firewall you can install via:

    remotes::install_local("R:/R/Pakkar/mardata", force = TRUE)

## Why two packages?

The reason for the process relying on two packages is that each package
is limited to doing fewer things betters. User may only be interested in
accessing the contemporaneous measurments in hafvog, e.g. in cruises
other than groundfish surveys. Having two package will hopefully reduce
maintenance overhead.

## What will happen to the xe-package?

In previous workflow the data contemporaneously collected were accessed
directly from an Oracle database (XE). That dependency generated at
least some consternation with regards to:

- Connection nuances
- Having to rely on an old {dbplyr}-version
- Read access and password problems

The xe-package will still be around as is, but will no longer be
maintained.

## Setting up a project

It is recommended that you setup your contemporaneous survey as an
RStudio project. The steps within RStudio are:

- File -\> New Project … -\> New Directory
- Suggest you setp a directory “data-raw”
  - In this directory put your dumped hafvog zip files, including
    “stodtoflur.zip” and “stillingar…zip”
- If you have not installed the packages as instructed above, do so now
- Once done open up the smxapp skeleton (you may have to restart your
  project) by:
  - File -\> New File -\> R Markdown … -\> From Template -\> smxapp
    {osmx}
  - Save the file in the root of the project, e.g. SMH2024.Rmd
- Run the through the first two chunks of code, line by line. Check
  carefully messages.
- Press “Run Document” to start the shiny

The structure of your project should in the end look something like
this:

    ├── SMH2024.Rmd
    ├── data-raw
    │   ├── TB2-2024.zip
    │   ├── TTH1-2024.zip
    │   ├── stillingar_SMH_rall_(haust).zip
    │   └── stodtoflur.zip
    ├── data2
    │   └── res.rds
    └── SMH2024.Rproj
