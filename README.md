# hcr-shape-explorer

An R Shiny app for interactively exploring the shape of the asymptotic Hillary step harvest control rule (HCR).

## Overview

The app plots the HCR curve and lets users adjust all parameters in real time via sidebar sliders. A selected point is overlaid on the curve to show the output multiplier at any given stock depletion level. Key controls include:

- **Current SB/SB\_{F=0}** — position of the selected point on the x-axis
- **Curve x-axis breakpoints** — `sbsbf0_min`, `sbsbf0_step_min`, `sbsbf0_step_max`, `sbsbf0_max`
- **Curve y-axis levels** — `out_min`, `step_height`, `out_max`
- **Curvature (κ)** — steepness of the asymptotic left branch

The app has two tabs:

- **Plot** — interactive Plotly chart with the HCR curve, guide lines at each breakpoint/level, and the selected point annotated with its coordinates
- **Data** — tabulated HCR curve values (SB/SB\_{F=0} from 0 to 2 in steps of 0.05), exportable to CSV, Excel, or PDF

Additional features: tooltip help icons on every slider label, a **Reset defaults** button, and a **Copy** button that copies the current parameter set to the clipboard.

## Requirements

- R (≥ 4.0)
- Packages: `shiny`, `plotly`, `DT`

## Usage

```r
shiny::runApp("app.r")
```

## License

The code contained in this repository is licensed under the GNU GENERAL PUBLIC LICENSE version 3 ([GPLv3](https://www.gnu.org/licenses/gpl-3.0.html)).

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.