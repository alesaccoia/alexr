# Requirements

install.packages(devtools)	

## Mac

brew install udunits gdal

## Debian/Ubuntu

sudo add-apt-repository ppa:nextgis/ppa
sudo apt-get update
sudo apt-cache policy libgdal-dev
sudo apt-get install libproj-dev
sudo apt-get install libudunits2-dev


# Install instructions

devtools::install_github('alesaccoia/alexr', auth_token)

# Enabling Multiprocessing

devtools::install_github("hadley/multidplyr")Some modules use multiprocessing, to enable them please run

devtools::install_github("hadley/multidplyr")
