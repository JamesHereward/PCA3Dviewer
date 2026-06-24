# PCA3Dviewer

Interactive **3D PCA viewer** for genetic data. `PCA3Dviewer` runs a principal
component analysis on a [`genind`](https://adegenet.r-forge.r-project.org/)
object (from **adegenet**) and writes a single, self-contained HTML file with an
interactive 3D scatter plot rendered with [three.js](https://threejs.org/).
No server, no internet at view time beyond the two CDN script tags — just open
the file in a browser.

## Features

- **Rotatable / zoomable** 3D scatter (drag to rotate, scroll to zoom).
- **Selectable PC axes** — choose which principal components map to X / Y / Z.
- **Population legend filter** — click a population to isolate it.
- **Sample search** — type a sample name to highlight it with an arrow + label.
- **Dark / light mode** toggle.
- **Adjustable point size and opacity** sliders.
- **Switchable colour palettes** (Default, Okabe-Ito, Set2, Dark2, Viridis,
  Set3); palettes are interpolated so no colour repeats when you have more
  populations than palette colours.
- **PNG / PDF export** at 3× resolution.
- **Right-click a point** to copy its sample ID to the clipboard.

## Installation

```r
# install.packages("remotes")
remotes::install_github("JamesHereward/PCA3Dviewer")
```

This pulls in the required dependencies (**adegenet**, **ade4**).

## Usage

```r
library(PCA3Dviewer)

# From in-memory objects
gi <- readRDS("my_genind.rds")                 # a genind object
pm <- read.table("popmap.txt", header = TRUE)  # columns: id, pop
pca_3d_viewer(gi, pm, output = "3D_PCA_viewer.html")

# Or straight from file paths
pca_3d_viewer("my_genind.rds", "popmap.txt")
```

### The popmap

`popmap` is a data frame (or whitespace-delimited file with a header) that maps
each individual to a population. It must contain the columns `id` and `pop`:

```
id              pop
Sample_001      Darwin_NT
Sample_002      Keep_River_NP_NT
...
```

`id` values are matched against `adegenet::indNames(x)`.

### Arguments

| Argument  | Description                                                                 |
|-----------|-----------------------------------------------------------------------------|
| `x`       | A `genind` object, or a path to an `.rds` file containing one.               |
| `popmap`  | Data frame / file with `id` and `pop` columns.                              |
| `output`  | Output HTML path (default `"3D_PCA_viewer.html"`).                           |
| `n_pc`    | Number of principal components to compute / make selectable (default `10`). |
| `scale`   | Scale variables to unit variance in the PCA (default `FALSE`).               |
| `palette` | Optional vector of hex colours to use as the default palette.                |
| `open`    | Open the result in a browser (default `interactive()`).                      |

## License

MIT © James Hereward
