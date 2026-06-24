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
