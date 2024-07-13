# divbscan
Ivann Schlosser

This repo contains the code of a clustering algorithm for urban
analytics.

## Setup

You will need a C/C++ compiler, which comes with the developer tools of
R. It has a few dependencies and calls python from within `R` with the
[`reticulate`](https://rstudio.github.io/reticulate/) package.

### Python configuration

To configure the python virtual environment, the
[`micromamba`](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html)
tool is used, however, other tools should also work as long as the
dependencies are installed. Those are mentioned in the `env.yaml` file.

### Initiate the vitrual environment

To set up a micromamba virtual environment, run the following command in
a terminal:

``` bash
micromamba create -f env.yaml
```

This will install the python dependencies of the project. Now you need
to link the python environment with R. To do so, run the following:

In the terminal:

``` bash
micromamba activate decon-neighb
which python
```

Copy the output of this command, which should look something like:
`~/micromamba/envs/decon-neighb`.

Now in R, run the following by replacing ENV_PATH with the output of the
previous command:

``` r
Sys.unsetenv(c("RETICULATE_PYTHON","RETICULATE_PYTHON_ENV"))
reticulate::use_condaenv("ENV_PATH")
```

This will override the default configuration of reticulate and point it
at the right python virtual environment.

### R dependencies

The R dependencies are managed with the `renv` package, but this can be
bypassed if all the packages are installed manually.

## Running

There are multiple ways to start working with this repository, the
easiest being to analyze a city from the predefined set. It will still
require downloading the corresponding OSM file from
[geofabrick](http://download.geofabrik.de).

The necessary OSM file will be either the country or region containing
the city. For reusability reasons, it might be better to download the
whole country. Say if you are running the analysis on several cities
across France, it’s easier to get a single osm file for the country,
than each region individually.

The OSM files, should be put in the `data/osm_extracts/` folder. For
example, if working on France, the OSM extract will be looked for at the
following location in the directory:
`data/osm_extracts/australia-latest.osm.pbf`

### Predefined cities

Currently, a set of cities are supported, this essentially means that
their bbox and associated `.pbf` files are located. A new city can be
added manually quite easily, this will be covered in another section.

To visualise the available cities, run
`names(rlist::list.load("cities.rds"))`. Select the one yyou want, it is
recommended to start with a smaller one to see that everything works
initially.

Open the `params.R` script and in the first line of code assign to the
`city` variable whichever city you chose. Default is `aix-en-provence`.

### Other dependencies

The workflow relies on the [osmium](https://osmcode.org/osmium-tool/)
library, please install it separately.

## Simulation

You can now run a simulation by calling the `divbscan_ny.R` once the raw
osm extract is at the right location and you have entered a city name in
the parameters.

Run for example in your R console :

``` r
source("divbscan_ny.R")
```

Future updates of this repository will aim to automate as much as
possible the *setup* step.
