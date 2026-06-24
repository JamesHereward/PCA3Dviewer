## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release, so CRAN's incoming checks report the expected
  "New submission" NOTE.

## Notes for the reviewer

* The package bundles two unmodified, minified third-party JavaScript libraries
  in `inst/js/` (three.js, r128; jsPDF, 2.5.1). Both are MIT licensed, retain
  their original license headers, and their copyright holders are credited in
  `Authors@R` (role "cph") and documented in `inst/js/LICENSES.md`. They are
  inlined into the generated HTML so the output is fully self-contained and
  needs no internet access.

* `pca_3d_viewer()` writes an HTML file. The runnable example writes to
  `tempfile()`. The default `output` path is only used in interactive sessions.

* The function can open the result in a browser, but only when
  `open = TRUE` (default `interactive()`); examples and tests use
  `open = FALSE`.

## Test environments

* local: Ubuntu 24.04, R 4.6.0
* win-builder: R-devel (planned)
* R-hub: planned
