# ggsced

[![License: MIT](https://img.shields.io/badge/License-GPL2-blue.svg)](https://opensource.org/licenses/gpl-2-0)
[![R package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)

**Utilities and helpers for Single-Case Experimental Design (SCED) using ggplot2**

## Overview

The `ggsced` package extends the powerful `ggplot2` visualization framework to provide specialized tools for creating high-quality graphics for Single-Case Experimental Design (SCED) research. SCED studies are a crucial methodology in behavioral and educational research, where individual participants serve as their own controls through carefully designed experimental phases. This approaches rests on careful visual inspection of data presented in graphs that clearly delineate phase changes and patterns.

## Purpose

Single-case experimental designs require specific visualization conventions that are not easily achieved with standard plotting approaches. The `ggsced` package bridges the gap between the flexibility of `ggplot2` and the specific visualization needs of single-case researchers by providing:

- **Professional Phase Change Lines**: Clear visual demarcation between experimental phases that meet publication standards
- **Multiple Baseline Design Support**: Staggered intervention implementation across participants with precise phase change timing
- **Complex Data Pattern Visualization**: Support for multiple dependent variables plotted simultaneously
- **Publication-Ready Graphics**: APA and journal-compliant figures with consistent styling

## Key Features

### Core Visualization Functions

- **`gg_sced()`**: Primary function for adding phase change lines to existing ggplot objects
- **`gg_sced_style_x()`** and **`gg_sced_style_y()`**: Styling functions for axes that follow SCED conventions with broken axis appearance
- **`gg_sced_get_panels()`**: Utilities for managing multi-panel layouts in faceted designs

### Design Pattern Support

- **Multiple Baseline Designs**: Staggered intervention implementation across participants
- **Alternating Treatment Designs**: Rapid alternation between different intervention conditions
- **Functional Analysis Designs**: Multiple dependent variables with distinct visual markers
- **Complex Phase Patterns**: Support for multiple intervention phases within studies

### Professional Styling

- **Consistent Visual Standards**: Publication-quality aesthetics with proper fonts and scaling
- **Broken Axis Convention**: SCED-specific axis styling with disconnected x/y axes
- **Flexible Customization**: Modular design allows adaptation to diverse research needs
- **Multi-panel Support**: Seamless integration with `ggplot2` faceting for participant comparisons

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("miyamot0/ggsced")

# Load the package
library(ggsced)
```

## Quick Start

```r
library(ggsced)
library(ggplot2)
library(tidyverse)

# Load example data
data_set <- Gilroyetal2015

# Create base plot
p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  facet_grid(Participant ~ .) +
  gg_sced_style_x(0.02, lwd = 2) +
  gg_sced_style_y(0.05, lwd = 2) +
  theme_minimal()

# Define staggered phase change lines for multiple baseline design
staggered_pls <- list(
  '1' = c(4.5,  11.5, 18.5),
  '2' = c(13.5, 20.5, 23.5),
  '3' = c(23.5, 23.5, 23.5)
)

# Add phase change lines
final_plot <- gg_sced(p, staggered_pls)
```

## Example Figures

![Gilroy et al. (2015) Figure](https://github.com/miyamot0/ggsced/blob/main/figs/GilroyEtAl2015.svg?raw=true)

## Example Datasets

The package includes several real research datasets for learning and demonstration:

- **`Gilroyetal2015`**: Multiple baseline design data
- **`Gilroyetal2021`**: Alternating treatment design
- **Additional datasets**: Various challenge and simulation data for testing

## Documentation

- **Comprehensive vignette**: Detailed examples with real research data
- **Function documentation**: Complete help files for all exported functions
- **Demo files**: Executable examples in the `demo/` directory
- **Test suite**: Extensive testing to ensure reliability

## Dependencies

- `ggplot2`: Core plotting functionality
- `grid`: Low-level graphics operations
- `gtable`: Plot layout management
- `ggh4x`: Extended ggplot2 functionality

## Author and Contact

**Shawn P. Gilroy, Ph.D.**  
Louisiana State University  
📧 [sgilroy1@lsu.edu](mailto:sgilroy1@lsu.edu)  
🆔 ORCID: [0000-0002-1097-8366](https://orcid.org/0000-0002-1097-8366)

## Bug Reports and Feature Requests

If you encounter any issues or have suggestions for improvements, please:

1. **Check existing issues**: Browse the [GitHub Issues](https://github.com/miyamot0/ggsced/issues) to see if your issue has already been reported
2. **Create a new issue**: If your issue is new, please [open a new issue](https://github.com/miyamot0/ggsced/issues/new) with:
   - A clear, descriptive title
   - Detailed description of the problem or feature request
   - Minimal reproducible example (if reporting a bug)
   - Your session info (`sessionInfo()`)
   - Expected vs. actual behavior

## Contributing

We welcome contributions to the `ggsced` package! Please see our [contribution guidelines](https://github.com/miyamot0/ggsced/blob/main/CONTRIBUTING.md) for details on:

- Code style conventions
- Testing requirements
- Pull request process
- Documentation standards

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Citation

If you use `ggsced` in your research, please cite it appropriately:

```
Gilroy, S. P. (2026). ggsced: Utilities and helpers for Single-Case
Experimental Design using ggplot2. R package version 0.1.0.
https://github.com/miyamot0/ggsced
```

## Acknowledgments

This package was developed to support the single-case research community with publication-quality visualization tools. Special thanks to the researchers who provided data for demonstration examples and to the broader R community for the foundation provided by `ggplot2` and related packages.

---

**Keywords**: single-case design, SCED, behavioral research, data visualization, ggplot2, R package
