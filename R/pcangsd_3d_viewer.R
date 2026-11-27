#' Interactive 3D PCA viewer for a PCAngsd covariance matrix
#'
#' Builds the same interactive 3D viewer as \code{\link{pca_3d_viewer}}, but
#' from the covariance matrix written by PCAngsd (\code{<prefix>.cov}) instead
#' of a \code{genind} object. PCAngsd works from genotype likelihoods, so it
#' suits low-coverage and museum samples whose hard genotype calls would be
#' unreliable. The principal components are the eigenvectors of the covariance
#' matrix, as in the PCAngsd documentation, and the \% variance shown for each
#' PC is its eigenvalue over the sum of all eigenvalues.
#'
#' @param cov Path to the PCAngsd covariance matrix (a whitespace-delimited
#'   square matrix with no header), or the matrix itself.
#' @param samples Sample IDs in the order of the Beagle file given to PCAngsd
#'   (PCAngsd does not record them): a character vector, or a path to a file
#'   with one ID per line (only the first column is used).
#' @param popmap Optional \code{data.frame} or file with the columns \code{id}
#'   and \code{pop}, as for \code{\link{pca_3d_viewer}}. If \code{NULL}, all
#'   samples go in one group, \code{"all"}.
#' @param output Path of the HTML file to write.
#' @param n_pc Number of principal components to make selectable (at least 3).
#' @param palette,fixed_colours,open As for \code{\link{pca_3d_viewer}}.
#'
#' @return (Invisibly) the normalised path to the written HTML file.
#'
#' @examples
#' \donttest{
#' # A toy covariance matrix with two groups
#' set.seed(1)
#' g <- rep(c(-1, 1), each = 10)
#' z <- cbind(g, rnorm(20), rnorm(20), rnorm(20)) %*% diag(c(3, 1, 0.7, 0.5))
#' cv <- tcrossprod(scale(z, scale = FALSE)) / 4
#' ids <- sprintf("s%02d", 1:20)
#' pm  <- data.frame(id = ids, pop = ifelse(g < 0, "west", "east"))
#' out <- pcangsd_3d_viewer(cv, ids, pm,
#'                          output = tempfile(fileext = ".html"), open = FALSE)
#' }
#'
#' \dontrun{
#' pcangsd_3d_viewer("pcangsd.cov", "samples.txt", "popmap.txt")
#' }
#'
#' @export
pcangsd_3d_viewer <- function(cov, samples, popmap = NULL,
                              output = "3D_PCA_viewer.html",
                              n_pc   = 10,
                              palette = NULL,
                              fixed_colours = NULL,
                              open   = interactive()) {

  ## ---- Covariance matrix ----
  if (is.character(cov)) {
    if (!file.exists(cov)) stop("File not found: ", cov, call. = FALSE)
    cov <- as.matrix(utils::read.table(cov, header = FALSE))
  }
  cov <- as.matrix(cov)
  storage.mode(cov) <- "double"
  n <- nrow(cov)
  if (n != ncol(cov)) stop("`cov` must be a square matrix.", call. = FALSE)
  if (n < 4) stop("At least 4 samples are needed for a 3D PCA.", call. = FALSE)

  ## ---- Sample IDs ----
  if (length(samples) == 1L && is.character(samples) && file.exists(samples)) {
    samples <- utils::read.table(samples, header = FALSE, colClasses = "character",
                                 comment.char = "")[[1]]
  }
  samples <- as.character(samples)
  if (length(samples) != n) {
    stop("`samples` has ", length(samples), " IDs but `cov` is ", n, " x ", n,
         ".", call. = FALSE)
  }

  ## ---- Populations ----
  if (is.null(popmap)) {
    pop <- rep("all", n)
  } else {
    if (is.character(popmap)) {
      if (!file.exists(popmap)) stop("File not found: ", popmap, call. = FALSE)
      popmap <- .read_popmap(popmap)
    }
    popmap <- as.data.frame(popmap, stringsAsFactors = FALSE)
    if (!all(c("id", "pop") %in% names(popmap))) {
      stop("`popmap` must contain columns named 'id' and 'pop'.", call. = FALSE)
    }
    pop <- as.character(popmap$pop[match(samples, popmap$id)])
    if (anyNA(pop)) {
      warning(sum(is.na(pop)),
              " individual(s) had no matching `id` in `popmap`; ",
              "they will appear with population 'NA'.", call. = FALSE)
      pop[is.na(pop)] <- "NA"
    }
  }

  ## ---- PCA: eigen-decomposition of the covariance matrix ----
  e  <- eigen((cov + t(cov)) / 2, symmetric = TRUE)
  nf <- max(3L, min(as.integer(n_pc), n - 1L))
  var_pct <- round(e$values / sum(e$values) * 100, 1)

  scores           <- as.data.frame(e$vectors[, seq_len(nf), drop = FALSE])
  colnames(scores) <- paste0("PC", seq_len(nf))
  scores$id        <- samples
  scores$pop       <- pop

  .build_viewer(scores, var_pct, output = output, palette = palette,
                fixed_colours = fixed_colours, open = open)
}
