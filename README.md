# PCA3Dviewer

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHereward/PCA3Dviewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHereward/PCA3Dviewer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Interactive **3D PCA viewer** for genetic data. `PCA3Dviewer` runs a principal
component analysis on a [`genind`](https://adegenet.r-forge.r-project.org/)
object (from **adegenet**) and writes a single, self-contained HTML file with an
interactive 3D scatter plot rendered with [three.js](https://threejs.org/).
The three.js and jsPDF libraries are bundled and inlined, so the file is fully
self-contained — no server and no internet needed to view it; just open it in a
browser.

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
Sample_002      Cairns_QLD
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
| `palette` | Optional vector of hex colours for the default palette; if named, matched to populations by name. |
| `fixed_colours` | Optional named vector of colours that stay the same under every palette (e.g. a grey background group). |
| `open`    | Open the result in a browser (default `interactive()`).                      |

### Colours by name, with a grey background group

```r
pca_3d_viewer(gi, pm,
              palette       = c(chinense = "#CC3336", orientale = "#2572EB"),
              fixed_colours = c(unknown = "grey70"))
```

`unknown` stays grey whichever palette is chosen in the viewer; the other
palettes are spread over the remaining populations.

### From a PCAngsd covariance matrix

For low-coverage or museum samples, PCAngsd runs the PCA from genotype
likelihoods instead of called genotypes. `pcangsd_3d_viewer()` builds the same
viewer from its covariance matrix. PCAngsd does not store sample names, so pass
the IDs in the order of the Beagle file it was given:

```r
pcangsd_3d_viewer("pcangsd.cov", "samples.txt", "popmap.txt",
                  output = "pcangsd_3D.html")
```

The PCs are the eigenvectors of the covariance matrix, and each PC's % variance
is its eigenvalue over the sum of all eigenvalues. `popmap` is optional here;
without it every sample is in one group.

## License

MIT © James Hereward
