# SmartEDA 0.3.1
## Enhancements
* Added 'bins', 'plot', 'round' and 'top' options to plot bar graph in `ExpCatStat`
* Added 'theme' option to customise the graph theme in both `ExpCatViz` and `ExpNumViz`
* Added 'gtitle' option to add additional chart title on both `ExpCatViz` and `ExpNumViz`
* Removed 'Label' option from `ExpCatStat`
* Changed input parameter name from 'gp' to 'target' in `ExpCatViz`

## Bug fixes
* Fixed a formula issues on odds calculation in `ExpCatStat`

# SmartEDA 0.3.0
## New Features
* Added function `ExpOutQQ` to plot Quantile-Quantile Plots for outlier checking
* Added function `ExpParcoord` for Parallel Co-ordinate plots

## Enhancements
* Added "rdata","value" option to plot bars graph in `ExpCatViz`
* Added "dcast" option to reshape the data in `ExpCustomStat`
* Added "dcast","val" option to customise the summary statistics in `ExpNumStat`
* Added "Template" option to read rma in `ExpNumStat`

## Bug fixes
* Fixed a bug in `ExpData`

# SmartEDA 0.2.0
## New Features
* Added function `ExpCustomStat` to customise the summary statistics.
* Added both counts and percentages in `ExpData` under option Type=1 and removed DV option from the parameter list.

## Bug fixes
* Fixed a bug in `ExpCatViz` function for not running the grid
* Fixed a bug in `ExpData` function, not running for some variable types. 

# SmartEDA 0.1.0

* Added a `NEWS.md` file to track changes to the package.
