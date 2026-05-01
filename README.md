# hcr-shape-explorer

An R Shiny app for interactively exploring the shape of the asymptotic Hillary step harvest control rule (HCR).

## Overview

The app plots the HCR curve and lets users adjust all parameters in real time via grouped sidebar sliders. A selected point is overlaid on the curve to show the output multiplier at any given stock depletion level.

The sidebar groups parameters into four collapsible sections:

- **Selected point** — `Current SB/SB_{F=0}` (position of the highlighted point on the x-axis)
- **Breakpoints (x-axis)** — `sbsbf0_min`, `sbsbf0_step_min`, `sbsbf0_step_max`, `sbsbf0_max`
- **Levels (y-axis)** — `out_min`, `step_height`, `out_max`
- **Curve shape** — Curvature (κ), the steepness of the asymptotic left branch

The app has two tabs:

- **Plot** — interactive Plotly chart with the HCR curve, color-coded guide lines at each breakpoint and level (with edge labels), and the selected point highlighted in orange. Two value boxes above the plot give an at-a-glance readout of the current selected coordinates.
- **Data** — tabulated HCR curve values (SB/SB_{F=0} from 0 to 2 in steps of 0.05), with a short caption explaining the table and exports to CSV, Excel, PDF, JSON parameters, and R-vector parameters.

### Features

- **Modern Bootstrap 5 layout** built on `bslib` (`page_sidebar`, `card`, `accordion`, `value_box`)
- **Color-coded slider labels** with swatches that match the corresponding guide lines on the plot, so each slider can be visually traced to its line
- **Tooltip help icons** on every slider label
- **Breakpoint validation** that automatically clamps adjacent sliders to keep `Min < Step start < Step end < Max`, with a warning banner if the ordering is ever violated
- **Reset defaults** button
- **Copy params** button (copies the current parameter set as plain text to the clipboard)
- **Copy share link** button (uses Shiny URL bookmarking so a colleague can open the exact same view)
- **Parameter downloads** as JSON or as a ready-to-paste R named vector

## Requirements

- R (≥ 4.0)
- Packages: `shiny`, `bslib`, `bsicons`, `plotly`, `DT`, `jsonlite`

## Usage

```r
shiny::runApp("app.r")
```

## License

The code contained in this repository is licensed under the GNU GENERAL PUBLIC LICENSE version 3 ([GPLv3](https://www.gnu.org/licenses/gpl-3.0.html)).

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.