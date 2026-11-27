# PCA3Dviewer (development version)

* New `pcangsd_3d_viewer()`: builds the same viewer from a PCAngsd covariance
  matrix (`.cov`) and a list of sample IDs, so PCAs from genotype likelihoods
  (low-coverage or museum samples) can be viewed without a `genind` object.
* `palette` can now be a named vector, matched to populations by name instead
  of by order of appearance. Populations without a name get built-in colours,
  with a warning.
* New `fixed_colours` argument: a named vector of colours for populations that
  keep the same colour under every palette in the viewer's palette switcher
  (e.g. a grey "unknown" group). Accepts R colour names as well as hex codes.

# PCA3Dviewer 0.1.0

* Initial release.
* `pca_3d_viewer()` runs a PCA on a `genind` object and writes a single,
  self-contained interactive 3D HTML viewer (three.js).
* Features: selectable PC axes, population legend filter, sample search with
  on-plot highlighting, dark/light mode, point size and opacity sliders,
  switchable colour palettes (interpolated so colours never repeat), and
  PNG/PDF export.
* The `three.js` and `jsPDF` libraries are bundled and inlined, so the
  generated HTML needs no internet access to view.
