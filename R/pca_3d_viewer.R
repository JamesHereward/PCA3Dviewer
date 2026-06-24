#' Interactive 3D PCA viewer for genind data
#'
#' Runs a principal component analysis on a \code{genind} object (from
#' \pkg{adegenet}) and writes a single, self-contained HTML file containing an
#' interactive 3D scatter plot rendered with three.js. The viewer supports
#' selectable principal-component axes, a clickable population legend (filter),
#' a sample search box that highlights the matched sample with an arrow and
#' label, dark mode, adjustable point size and opacity, switchable colour
#' palettes, and PNG/PDF export. Right-clicking a point copies its sample ID to
#' the clipboard.
#'
#' @param x A \code{genind} object, or a path to an \code{.rds} file containing
#'   one.
#' @param popmap A \code{data.frame} with (at least) the columns \code{id} and
#'   \code{pop}, or a path to a whitespace-delimited table with a header row
#'   containing those columns. \code{id} values are matched against
#'   \code{adegenet::indNames(x)}.
#' @param output Path of the HTML file to write. Defaults to
#'   \code{"3D_PCA_viewer.html"} in the working directory.
#' @param n_pc Number of principal components to compute and make selectable in
#'   the viewer. Capped at the maximum supported by the data.
#' @param scale Logical; passed to \code{ade4::dudi.pca()}. If \code{TRUE},
#'   variables are scaled to unit variance. Default \code{FALSE}.
#' @param palette Optional character vector of hex colours used as the
#'   \dQuote{Default} palette (one per population, recycled if shorter than the
#'   number of populations). If \code{NULL} (default) a curated 12-colour
#'   palette is used for up to 12 populations, otherwise evenly-spaced
#'   ggplot-style hues are generated so no colour repeats.
#' @param open Logical; if \code{TRUE} the written HTML file is opened in the
#'   default browser via \code{utils::browseURL()}. Defaults to
#'   \code{interactive()}.
#'
#' @return (Invisibly) the normalised path to the written HTML file.
#'
#' @examples
#' \dontrun{
#' library(PCA3Dviewer)
#'
#' # From in-memory objects
#' gi  <- readRDS("my_genind.rds")
#' pm  <- read.table("popmap.txt", header = TRUE)
#' pca_3d_viewer(gi, pm, output = "pca.html")
#'
#' # Or straight from file paths
#' pca_3d_viewer("my_genind.rds", "popmap.txt")
#' }
#'
#' @importFrom grDevices hcl
#' @importFrom utils read.table browseURL
#' @export
pca_3d_viewer <- function(x, popmap,
                          output = "3D_PCA_viewer.html",
                          n_pc   = 10,
                          scale  = FALSE,
                          palette = NULL,
                          open   = interactive()) {

  ## ---- Resolve the genind object ----
  if (is.character(x)) {
    if (!file.exists(x)) stop("File not found: ", x, call. = FALSE)
    x <- readRDS(x)
  }
  if (!inherits(x, "genind")) {
    stop("`x` must be a 'genind' object or a path to an .rds file containing one.",
         call. = FALSE)
  }

  ## ---- Resolve the popmap ----
  if (is.character(popmap)) {
    if (!file.exists(popmap)) stop("File not found: ", popmap, call. = FALSE)
    popmap <- utils::read.table(popmap, header = TRUE, stringsAsFactors = FALSE)
  }
  popmap <- as.data.frame(popmap, stringsAsFactors = FALSE)
  if (!all(c("id", "pop") %in% names(popmap))) {
    stop("`popmap` must contain columns named 'id' and 'pop'.", call. = FALSE)
  }

  ## ---- Attach populations in genind order ----
  ind         <- adegenet::indNames(x)
  pop_ordered <- popmap$pop[match(ind, popmap$id)]
  if (anyNA(pop_ordered)) {
    warning(sum(is.na(pop_ordered)),
            " individual(s) had no matching `id` in `popmap`; ",
            "they will appear with population 'NA'.", call. = FALSE)
    pop_ordered[is.na(pop_ordered)] <- "NA"
  }
  adegenet::pop(x) <- pop_ordered

  ## ---- PCA ----
  X  <- adegenet::tab(x, freq = TRUE, NA.method = "mean")
  nf <- max(3L, min(as.integer(n_pc), ncol(X), nrow(X) - 1L))
  pca_result <- ade4::dudi.pca(X, scale = scale, scannf = FALSE, nf = nf)
  eig_total  <- sum(pca_result$eig)
  var_pct    <- round((pca_result$eig / eig_total) * 100, 1)

  scores           <- as.data.frame(pca_result$li)
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
  scores$id        <- ind
  scores$pop       <- as.character(adegenet::pop(x))

  ## ---- Colour palette ----
  populations <- unique(scores$pop)
  n_pops      <- length(populations)
  if (is.null(palette)) {
    palette_base <- c(
      "#5E4FBE", "#1D9E75", "#D85A30", "#D4537E",
      "#378ADD", "#639922", "#BA7517", "#E24B4A",
      "#888780", "#7BCCC4", "#F768A1", "#41B6C4"
    )
    if (n_pops <= length(palette_base)) {
      pal <- palette_base[seq_len(n_pops)]
    } else {
      hues <- seq(15, 375, length.out = n_pops + 1)[seq_len(n_pops)]
      pal  <- grDevices::hcl(h = hues, c = 100, l = 65)
    }
  } else {
    pal <- rep(palette, length.out = n_pops)
  }
  colour_map        <- pal
  names(colour_map) <- populations
  scores$colour     <- colour_map[scores$pop]

  ## ---- JSON payloads ----
  pc_cols   <- grep("^PC", colnames(scores), value = TRUE)
  n_pc_out  <- length(pc_cols)
  pc_mat    <- as.matrix(scores[, pc_cols])
  pc_arrays <- apply(pc_mat, 1, function(r)
    paste0("[", paste(sprintf("%f", r), collapse = ","), "]"))

  pts_rows <- sprintf(
    '{"id":"%s","pop":"%s","colour":"%s","pc":%s}',
    scores$id, scores$pop, scores$colour, pc_arrays)
  points_json <- paste0("[", paste(pts_rows, collapse = ","), "]")

  leg_rows    <- sprintf('{"pop":"%s","colour":"%s"}',
                         populations, colour_map[populations])
  legend_json <- paste0("[", paste(leg_rows, collapse = ","), "]")

  pops_json   <- paste0("[", paste(sprintf('"%s"', populations), collapse = ","), "]")
  defpal_json <- paste0("[", paste(sprintf('"%s"', colour_map[populations]),
                                   collapse = ","), "]")
  varpct_json <- paste0("[", paste(sprintf("%.1f", var_pct[seq_len(n_pc_out)]),
                                   collapse = ","), "]")

  ## ---- Assemble and write the HTML ----
  html_lines <- .viewer_html(points_json, legend_json, varpct_json,
                             pops_json, defpal_json)
  writeLines(html_lines, output)
  if (isTRUE(open)) {
    try(utils::browseURL(output), silent = TRUE)
  }
  message("Saved: ", output)
  invisible(normalizePath(output, winslash = "/", mustWork = FALSE))
}


## Internal: returns the self-contained HTML as a character vector.
## The five data lines are injected; everything else is the static viewer.
.viewer_html <- function(points_json, legend_json, varpct_json,
                         pops_json, defpal_json) {
  c(
  "<!DOCTYPE html>",
  "<html><head><meta charset='utf-8'>",
  "<style>",
  "* { box-sizing:border-box; margin:0; padding:0; }",
  "body { background:#fff; font-family:Arial,sans-serif; overflow:hidden; }",
  "canvas { display:block; }",
  "#tooltip { position:fixed; display:none; z-index:100;",
  "  background:rgba(255,255,255,0.95); border:1px solid #ccc; border-radius:5px;",
  "  padding:6px 10px; font-size:12px; pointer-events:none; white-space:nowrap;",
  "  box-shadow:0 2px 6px rgba(0,0,0,0.15); }",
  "#toast { position:fixed; bottom:60px; left:50%; transform:translateX(-50%);",
  "  background:rgba(30,30,30,0.85); color:#fff; padding:7px 18px;",
  "  border-radius:20px; font-size:13px; display:none; pointer-events:none;",
  "  z-index:300; max-width:80vw; word-break:break-all; text-align:center; }",
  "#legend { position:fixed; bottom:20px; left:16px; z-index:100;",
  "  background:rgba(255,255,255,0.92); border:1px solid #ddd;",
  "  border-radius:6px; padding:8px 12px; font-size:12px; }",
  ".lrow { display:flex; align-items:center; gap:6px; margin:3px 0;",
  "  cursor:pointer; border-radius:4px; padding:2px 4px; transition:opacity 0.2s; }",
  ".lrow:hover { background:rgba(0,0,0,0.05); }",
  ".ldot { width:10px; height:10px; border-radius:50%; flex-shrink:0; }",
  "#axlbl { position:fixed; bottom:20px; left:50%; transform:translateX(-50%);",
  "  font-size:12px; color:#666; text-align:center; pointer-events:none; }",
  "#tools { position:fixed; top:16px; right:16px; z-index:150;",
  "  display:flex; flex-direction:column; gap:6px; align-items:stretch; }",
  ".tbtn { background:#fff; border:1px solid #d0d0d0; border-radius:6px;",
  "  padding:7px 14px; font-size:13px; cursor:pointer; color:#333; text-align:center;",
  "  box-shadow:0 2px 6px rgba(0,0,0,0.12); user-select:none; }",
  ".tbtn:hover { background:#f5f5f5; }",
  "#spin.on { background:#5E4FBE; color:#fff; border-color:#5E4FBE; }",
  "#leftpanel { position:fixed; top:16px; left:16px; z-index:150;",
  "  display:flex; flex-direction:column; gap:8px; align-items:flex-start; }",
  "#axes { background:rgba(255,255,255,0.92); border:1px solid #ddd; border-radius:6px;",
  "  padding:8px 12px; font-size:12px; display:flex; gap:10px; align-items:center;",
  "  box-shadow:0 2px 6px rgba(0,0,0,0.12); }",
  "#axes label { display:flex; align-items:center; gap:4px; color:#555; }",
  "#axes select { font-size:12px; padding:2px 4px; border:1px solid #ccc; border-radius:4px; }",
  "#search { display:flex; gap:6px; align-items:center; background:rgba(255,255,255,0.92);",
  "  border:1px solid #ddd; border-radius:6px; padding:6px 8px;",
  "  box-shadow:0 2px 6px rgba(0,0,0,0.12); }",
  "#searchbox { font-size:13px; padding:4px 8px; border:1px solid #ccc; border-radius:4px;",
  "  width:150px; outline:none; }",
  "#clearsearch { padding:4px 9px; font-size:13px; }",
  ".tslider { background:#fff; border:1px solid #d0d0d0; border-radius:6px;",
  "  padding:7px 12px; font-size:12px; color:#333; box-shadow:0 2px 6px rgba(0,0,0,0.12);",
  "  display:flex; flex-direction:column; gap:3px; }",
  ".tslider input[type=range] { width:100%; }",
  ".tslider select { width:100%; font-size:12px; padding:2px 4px;",
  "  border:1px solid #ccc; border-radius:4px; }",
  "/* ---- dark mode ---- */",
  "body.dark { background:#1a1a1a; color:#eee; }",
  "body.dark #tooltip { background:rgba(40,40,40,0.95); border-color:#555; color:#eee; }",
  "body.dark #legend { background:rgba(40,40,40,0.92); border-color:#555; color:#eee; }",
  "body.dark .lrow:hover { background:rgba(255,255,255,0.08); }",
  "body.dark .tbtn { background:#2a2a2a; border-color:#555; color:#eee; }",
  "body.dark .tbtn:hover { background:#3a3a3a; }",
  "body.dark .tslider { background:#2a2a2a; border-color:#555; color:#eee; }",
  "body.dark .tslider select { background:#2a2a2a; color:#eee; border-color:#555; }",
  "body.dark #axes, body.dark #search { background:rgba(40,40,40,0.92); border-color:#555; }",
  "body.dark #axes label { color:#ccc; }",
  "body.dark #axes select, body.dark #searchbox {",
  "  background:#2a2a2a; color:#eee; border-color:#555; }",
  "body.dark #axlbl { color:#aaa; }",
  "</style></head>",
  "<body>",
  "<canvas id='c'></canvas>",
  "<div id='tooltip'></div>",
  "<div id='toast'></div>",
  "<div id='legend'></div>",
  "<div id='axlbl'></div>",
  "<div id='tools'>",
  "  <div id='spin'  class='tbtn'>&#x21bb; Spin</div>",
  "  <div id='reset' class='tbtn'>&#x27f2; Reset view</div>",
  "  <div id='dark'  class='tbtn'>&#x263e; Dark mode</div>",
  "  <div id='bpng'  class='tbtn'>Save PNG</div>",
  "  <div id='bpdf'  class='tbtn'>Save PDF</div>",
  "  <div class='tslider'><span>Size</span>",
  "    <input type='range' id='dotsize' min='0.3' max='3' step='0.1' value='1'></div>",
  "  <div class='tslider'><span>Opacity</span>",
  "    <input type='range' id='dotopacity' min='0.1' max='1' step='0.05' value='1'></div>",
  "  <div class='tslider'><span>Palette</span>",
  "    <select id='selpal'></select></div>",
  "</div>",
  "<div id='leftpanel'>",
  "  <div id='axes'>",
  "    <label>X <select id='selx'></select></label>",
  "    <label>Y <select id='sely'></select></label>",
  "    <label>Z <select id='selz'></select></label>",
  "  </div>",
  "  <div id='search'>",
  "    <input id='searchbox' list='samplelist' placeholder='Search sample...' autocomplete='off'>",
  "    <datalist id='samplelist'></datalist>",
  "    <div id='clearsearch' class='tbtn'>&#x2715;</div>",
  "  </div>",
  "</div>",
  "<script src='https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js'></script>",
  "<script src='https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js'></script>",
  "<script>",
  paste0("var PTS    = ", points_json, ";"),
  paste0("var LEGEND = ", legend_json, ";"),
  paste0("var VARPCT = ", varpct_json, ";"),
  paste0("var POPS   = ", pops_json, ";"),
  paste0("var DEFPAL = ", defpal_json, ";"),
  "var AXIDX = {x:0, y:1, z:2};",
  "",
  "var canvas = document.getElementById('c');",
  "var W = window.innerWidth, H = window.innerHeight;",
  "var renderer = new THREE.WebGLRenderer({canvas:canvas, antialias:true});",
  "renderer.setSize(W, H);",
  "renderer.setPixelRatio(window.devicePixelRatio);",
  "renderer.setClearColor(0xffffff);",
  "var scene  = new THREE.Scene();",
  "var camera = new THREE.PerspectiveCamera(45, W/H, 0.01, 1000);",
  "",
  "function arrMin(a){ return Math.min.apply(null,a); }",
  "function arrMax(a){ return Math.max.apply(null,a); }",
  "var cx, cy, cz, sc;",
  "function recomputeScale() {",
  "  var xs = PTS.map(function(p){ return p.pc[AXIDX.x]; });",
  "  var ys = PTS.map(function(p){ return p.pc[AXIDX.y]; });",
  "  var zs = PTS.map(function(p){ return p.pc[AXIDX.z]; });",
  "  cx = (arrMax(xs)+arrMin(xs))/2;",
  "  cy = (arrMax(ys)+arrMin(ys))/2;",
  "  cz = (arrMax(zs)+arrMin(zs))/2;",
  "  sc = 3 / Math.max(arrMax(xs)-arrMin(xs), arrMax(ys)-arrMin(ys), arrMax(zs)-arrMin(zs));",
  "}",
  "function toBox(v, c) { return (v - c) * sc; }",
  "",
  "var B = 1.5;",
  "var boxMat  = new THREE.LineBasicMaterial({color:0xcccccc, transparent:true, opacity:0.5});",
  "var gridMat = new THREE.LineBasicMaterial({color:0xe0e0e0, transparent:true, opacity:0.6});",
  "var LBLCOLOR = '#666666';",
  "function addBoxEdges() {",
  "  var edges = [",
  "    [-B,-B,-B],[B,-B,-B], [B,-B,-B],[B,B,-B], [B,B,-B],[-B,B,-B], [-B,B,-B],[-B,-B,-B],",
  "    [-B,-B,B],[B,-B,B],   [B,-B,B],[B,B,B],   [B,B,B],[-B,B,B],   [-B,B,B],[-B,-B,B],",
  "    [-B,-B,-B],[-B,-B,B], [B,-B,-B],[B,-B,B],  [B,B,-B],[B,B,B],   [-B,B,-B],[-B,B,B]",
  "  ];",
  "  for (var i=0; i<edges.length; i+=2) {",
  "    var g = new THREE.BufferGeometry().setFromPoints([",
  "      new THREE.Vector3(edges[i][0],edges[i][1],edges[i][2]),",
  "      new THREE.Vector3(edges[i+1][0],edges[i+1][1],edges[i+1][2])]);",
  "    scene.add(new THREE.Line(g, boxMat));",
  "  }",
  "}",
  "addBoxEdges();",
  "",
  "function addGridFace(axis, val, steps) {",
  "  var mat = gridMat;",
  "  for (var i=0; i<=steps; i++) {",
  "    var v = -B + i*(2*B/steps);",
  "    var pts;",
  "    if (axis==='x') pts = [[val,-B,v],[val,B,v],[val,v,-B],[val,v,B]];",
  "    if (axis==='y') pts = [[-B,val,v],[B,val,v],[v,val,-B],[v,val,B]];",
  "    if (axis==='z') pts = [[-B,v,val],[B,v,val],[v,-B,val],[v,B,val]];",
  "    for (var j=0; j<pts.length; j+=2) {",
  "      var g = new THREE.BufferGeometry().setFromPoints([",
  "        new THREE.Vector3(pts[j][0],pts[j][1],pts[j][2]),",
  "        new THREE.Vector3(pts[j+1][0],pts[j+1][1],pts[j+1][2])]);",
  "      scene.add(new THREE.Line(g, mat));",
  "    }",
  "  }",
  "}",
  "addGridFace('x', -B, 5);",
  "addGridFace('y', -B, 5);",
  "addGridFace('z', -B, 5);",
  "",
  "function makeTextTexture(text) {",
  "  var c = document.createElement('canvas');",
  "  c.width = 128; c.height = 32;",
  "  var ctx = c.getContext('2d');",
  "  ctx.clearRect(0,0,128,32);",
  "  ctx.font = '18px Arial';",
  "  ctx.fillStyle = LBLCOLOR;",
  "  ctx.textAlign = 'center';",
  "  ctx.fillText(text, 64, 22);",
  "  return new THREE.CanvasTexture(c);",
  "}",
  "function makeLabel(pos) {",
  "  var sp = new THREE.Sprite(new THREE.SpriteMaterial({transparent:true}));",
  "  sp.scale.set(0.5, 0.125, 1);",
  "  sp.position.set(pos[0], pos[1], pos[2]);",
  "  scene.add(sp);",
  "  return sp;",
  "}",
  "var labelSprites = {",
  "  x: makeLabel([0, -B-0.35, -B-0.1]),",
  "  y: makeLabel([-B-0.1, 0, -B-0.35]),",
  "  z: makeLabel([-B-0.5, -B-0.1, 0])",
  "};",
  "function axisText(idx) { return 'PC' + (idx+1) + ' (' + VARPCT[idx] + '%)'; }",
  "function updateLabels() {",
  "  ['x','y','z'].forEach(function(a) {",
  "    if (labelSprites[a].material.map) labelSprites[a].material.map.dispose();",
  "    labelSprites[a].material.map = makeTextTexture(axisText(AXIDX[a]));",
  "    labelSprites[a].material.needsUpdate = true;",
  "  });",
  "  document.getElementById('axlbl').textContent =",
  "    axisText(AXIDX.x) + '  |  ' + axisText(AXIDX.y) + '  |  ' + axisText(AXIDX.z);",
  "}",
  "",
  "var geo = new THREE.SphereGeometry(0.022, 10, 10);",
  "var meshes = [];",
  "PTS.forEach(function(pt) {",
  "  var m = new THREE.Mesh(geo, new THREE.MeshBasicMaterial({color: pt.colour}));",
  "  m.userData = {id: pt.id, pop: pt.pop, colour: pt.colour, pc: pt.pc};",
  "  scene.add(m); meshes.push(m);",
  "});",
  "function positionPoints() {",
  "  recomputeScale();",
  "  meshes.forEach(function(m) {",
  "    var pc = m.userData.pc;",
  "    m.position.set(toBox(pc[AXIDX.x],cx), toBox(pc[AXIDX.y],cy), toBox(pc[AXIDX.z],cz));",
  "  });",
  "  updateHighlight();",
  "}",
  "positionPoints();",
  "updateLabels();",
  "",
  "var sph = {theta:0.75, phi:1.25, r:5};",
  "function updCam() {",
  "  camera.position.set(",
  "    sph.r*Math.sin(sph.phi)*Math.sin(sph.theta),",
  "    sph.r*Math.cos(sph.phi),",
  "    sph.r*Math.sin(sph.phi)*Math.cos(sph.theta));",
  "  camera.lookAt(0, 0, 0);",
  "}",
  "updCam();",
  "",
  "var drag = false, prev = {x:0, y:0};",
  "canvas.addEventListener('mousedown', function(e) {",
  "  if (e.button === 0) { drag = true; prev = {x:e.clientX, y:e.clientY}; }",
  "});",
  "window.addEventListener('mouseup', function() { drag = false; });",
  "window.addEventListener('mousemove', function(e) {",
  "  if (!drag) return;",
  "  sph.theta -= (e.clientX - prev.x) * 0.005;",
  "  sph.phi = Math.max(0.1, Math.min(Math.PI-0.1, sph.phi + (e.clientY-prev.y)*0.005));",
  "  prev = {x:e.clientX, y:e.clientY}; updCam();",
  "});",
  "canvas.addEventListener('wheel', function(e) {",
  "  sph.r = Math.max(1, Math.min(20, sph.r + e.deltaY*0.005));",
  "  updCam(); e.preventDefault();",
  "}, {passive: false});",
  "",
  "var ray  = new THREE.Raycaster();",
  "var m2d  = new THREE.Vector2();",
  "var tip  = document.getElementById('tooltip');",
  "var toast = document.getElementById('toast');",
  "var hov  = null;",
  "var toastTimer = null;",
  "",
  "function ndc(e) {",
  "  var r = canvas.getBoundingClientRect();",
  "  m2d.x =  ((e.clientX - r.left) / r.width)  * 2 - 1;",
  "  m2d.y = -((e.clientY - r.top)  / r.height) * 2 + 1;",
  "}",
  "",
  "window.addEventListener('mousemove', function(e) {",
  "  if (drag) { tip.style.display = 'none'; return; }",
  "  ndc(e); ray.setFromCamera(m2d, camera);",
  "  var h = ray.intersectObjects(meshes);",
  "  if (h.length > 0) {",
  "    var m = h[0].object;",
  "    if (hov && hov !== m) {",
  "      hov.material.color.set((activePop && hov.userData.pop !== activePop) ? 0xcccccc : hov.userData.colour);",
  "    }",
  "    hov = m;",
  "    m.material.color.set(0xffffff);",
  "    tip.style.display = 'block';",
  "    tip.style.left = (e.clientX+12) + 'px';",
  "    tip.style.top  = (e.clientY-28) + 'px';",
  "    tip.textContent = m.userData.id + ' (' + m.userData.pop + ')';",
  "    canvas.style.cursor = 'pointer';",
  "  } else {",
  "    if (hov) {",
  "      hov.material.color.set((activePop && hov.userData.pop !== activePop) ? 0xcccccc : hov.userData.colour);",
  "    }",
  "    hov = null; tip.style.display = 'none'; canvas.style.cursor = 'default';",
  "  }",
  "});",
  "",
  "function showToast(msg) {",
  "  toast.textContent = msg; toast.style.display = 'block';",
  "  clearTimeout(toastTimer);",
  "  toastTimer = setTimeout(function() { toast.style.display = 'none'; }, 1500);",
  "}",
  "function cpytxt(t) {",
  "  navigator.clipboard.writeText(t).catch(function() {",
  "    var ta = document.createElement('textarea');",
  "    ta.value = t; ta.style.cssText = 'position:fixed;opacity:0;';",
  "    document.body.appendChild(ta); ta.select();",
  "    document.execCommand('copy'); document.body.removeChild(ta);",
  "  });",
  "}",
  "canvas.addEventListener('contextmenu', function(e) {",
  "  e.preventDefault();",
  "  ndc(e); ray.setFromCamera(m2d, camera);",
  "  var h = ray.intersectObjects(meshes);",
  "  if (h.length > 0) { cpytxt(h[0].object.userData.id); showToast('Copied: ' + h[0].object.userData.id); }",
  "});",
  "",
  "var ld = document.getElementById('legend');",
  "var activePop = null;",
  "var dotOpacity = 1;",
  "// Selectable colour palettes (cycled by population index)",
  "var PALETTES = {",
  "  'Default':   DEFPAL,",
  "  'Okabe-Ito': ['#E69F00','#56B4E9','#009E73','#F0E442','#0072B2','#D55E00','#CC79A7','#000000'],",
  "  'Set2':      ['#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3'],",
  "  'Dark2':     ['#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666'],",
  "  'Viridis':   ['#440154','#46327E','#365C8D','#277F8E','#1FA187','#4AC16D','#A0DA39','#FDE725'],",
  "  'Set3':      ['#8DD3C7','#FBB4AE','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5','#BC80BD','#CCEBC5']",
  "};",
  "var curPal = 'Default';",
  "var legendDots = {};",
  "function hexToRgb(h) { h = h.replace('#',''); return [parseInt(h.substr(0,2),16), parseInt(h.substr(2,2),16), parseInt(h.substr(4,2),16)]; }",
  "function rgbToHex(c) { return '#' + c.map(function(v){ var s = Math.round(Math.max(0,Math.min(255,v))).toString(16); return s.length < 2 ? '0'+s : s; }).join(''); }",
  "function rampColour(arr, t) {",
  "  if (arr.length === 1) return arr[0];",
  "  var p = t * (arr.length - 1), i = Math.floor(p), f = p - i;",
  "  if (i >= arr.length - 1) return arr[arr.length - 1];",
  "  var a = hexToRgb(arr[i]), b = hexToRgb(arr[i+1]);",
  "  return rgbToHex([a[0]+(b[0]-a[0])*f, a[1]+(b[1]-a[1])*f, a[2]+(b[2]-a[2])*f]);",
  "}",
  "function colourHexFor(pop) {",
  "  var i = POPS.indexOf(pop); if (i < 0) i = 0;",
  "  var n = POPS.length;",
  "  var arr = PALETTES[curPal] || DEFPAL;",
  "  // Few enough populations: use the exact palette colours.",
  "  // More populations than anchors: interpolate so no colour repeats.",
  "  if (n <= arr.length) return arr[i];",
  "  return rampColour(arr, n <= 1 ? 0 : i / (n - 1));",
  "}",
  "function applyPalette(name) {",
  "  curPal = name;",
  "  meshes.forEach(function(m) {",
  "    m.userData.colour = colourHexFor(m.userData.pop);",
  "    if (m !== hov && (activePop === null || m.userData.pop === activePop)) {",
  "      m.material.color.set(m.userData.colour);",
  "    }",
  "  });",
  "  Object.keys(legendDots).forEach(function(pop) {",
  "    legendDots[pop].style.background = colourHexFor(pop);",
  "  });",
  "}",
  "function applyPopFilter(pop) {",
  "  activePop = pop;",
  "  meshes.forEach(function(m) {",
  "    if (pop === null || m.userData.pop === pop) {",
  "      m.material.color.set(m.userData.colour);",
  "      m.material.opacity = dotOpacity; m.material.transparent = dotOpacity < 1;",
  "    } else {",
  "      m.material.color.set(0xcccccc);",
  "      m.material.opacity = 0.25; m.material.transparent = true;",
  "    }",
  "  });",
  "  document.querySelectorAll('.lrow').forEach(function(row) {",
  "    var rowPop = row.dataset.pop;",
  "    row.style.opacity    = (pop === null || rowPop === pop) ? '1' : '0.4';",
  "    row.style.fontWeight = (rowPop === pop) ? '600' : '400';",
  "  });",
  "}",
  "LEGEND.forEach(function(l) {",
  "  var r = document.createElement('div'); r.className = 'lrow'; r.dataset.pop = l.pop;",
  "  var d = document.createElement('div'); d.className = 'ldot'; d.style.background = colourHexFor(l.pop);",
  "  var s = document.createElement('span'); s.textContent = l.pop;",
  "  legendDots[l.pop] = d;",
  "  r.appendChild(d); r.appendChild(s); ld.appendChild(r);",
  "  r.addEventListener('click', function() { applyPopFilter(activePop === l.pop ? null : l.pop); });",
  "});",
  "",
  "// Palette selector",
  "var selpal = document.getElementById('selpal');",
  "Object.keys(PALETTES).forEach(function(name) {",
  "  var o = document.createElement('option'); o.value = name; o.textContent = name;",
  "  selpal.appendChild(o);",
  "});",
  "selpal.addEventListener('change', function() { applyPalette(selpal.value); });",
  "",
  "// ---- Sample search + highlight (arrow + name label) ----",
  "var slist = document.getElementById('samplelist');",
  "PTS.forEach(function(p){ var o=document.createElement('option'); o.value=p.id; slist.appendChild(o); });",
  "var highlightMesh = null, highlightArrow = null, highlightLabel = null;",
  "function roundRect(ctx,x,y,w,h,r){",
  "  ctx.beginPath();",
  "  ctx.moveTo(x+r,y); ctx.arcTo(x+w,y,x+w,y+h,r); ctx.arcTo(x+w,y+h,x,y+h,r);",
  "  ctx.arcTo(x,y+h,x,y,r); ctx.arcTo(x,y,x+w,y,r); ctx.closePath();",
  "}",
  "function makeNameTexture(text){",
  "  var c = document.createElement('canvas'); var ctx = c.getContext('2d');",
  "  var font = 'bold 28px Arial'; ctx.font = font;",
  "  var w = Math.ceil(ctx.measureText(text).width) + 28, h = 44;",
  "  c.width = w; c.height = h; ctx = c.getContext('2d'); ctx.font = font;",
  "  ctx.fillStyle = dark ? 'rgba(255,255,255,0.95)' : 'rgba(0,0,0,0.95)';",
  "  roundRect(ctx,1,1,w-2,h-2,10); ctx.fill();",
  "  ctx.fillStyle = dark ? '#000000' : '#ffffff'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';",
  "  ctx.fillText(text, w/2, h/2+1);",
  "  return new THREE.CanvasTexture(c);",
  "}",
  "function clearHighlight(){",
  "  if (highlightArrow) { scene.remove(highlightArrow); highlightArrow = null; }",
  "  if (highlightLabel) {",
  "    if (highlightLabel.material.map) highlightLabel.material.map.dispose();",
  "    scene.remove(highlightLabel); highlightLabel = null;",
  "  }",
  "  highlightMesh = null;",
  "}",
  "function updateHighlight(){",
  "  if (!highlightMesh || !highlightArrow) return;",
  "  var p = highlightMesh.position, off = 0.75;",
  "  highlightArrow.position.set(p.x, p.y + off, p.z);",
  "  highlightArrow.setDirection(new THREE.Vector3(0,-1,0));",
  "  highlightArrow.setLength(off - 0.06, 0.16, 0.11);",
  "  highlightLabel.position.set(p.x, p.y + off + 0.18, p.z);",
  "}",
  "function highlightSample(q){",
  "  clearHighlight();",
  "  q = (q || '').trim().toLowerCase();",
  "  if (!q) return;",
  "  var m = null;",
  "  for (var i=0; i<meshes.length; i++){ if (meshes[i].userData.id.toLowerCase() === q){ m = meshes[i]; break; } }",
  "  if (!m){ for (var j=0; j<meshes.length; j++){ if (meshes[j].userData.id.toLowerCase().indexOf(q) >= 0){ m = meshes[j]; break; } } }",
  "  if (!m){ showToast('Not found: ' + q); return; }",
  "  highlightMesh = m;",
  "  highlightArrow = new THREE.ArrowHelper(new THREE.Vector3(0,-1,0), new THREE.Vector3(0,0,0), 0.7, dark ? 0xffffff : 0x000000, 0.16, 0.11);",
  "  highlightArrow.line.material.depthTest = false; highlightArrow.cone.material.depthTest = false;",
  "  highlightArrow.renderOrder = 998; scene.add(highlightArrow);",
  "  var tex = makeNameTexture(m.userData.id);",
  "  highlightLabel = new THREE.Sprite(new THREE.SpriteMaterial({map:tex, transparent:true, depthTest:false}));",
  "  var hgt = 0.16; highlightLabel.scale.set(hgt * tex.image.width / tex.image.height, hgt, 1);",
  "  highlightLabel.renderOrder = 999; scene.add(highlightLabel);",
  "  updateHighlight();",
  "}",
  "var searchbox = document.getElementById('searchbox');",
  "searchbox.addEventListener('change', function(){ highlightSample(searchbox.value); });",
  "searchbox.addEventListener('keydown', function(e){ if (e.key === 'Enter') highlightSample(searchbox.value); });",
  "document.getElementById('clearsearch').addEventListener('click', function(){ searchbox.value = ''; clearHighlight(); });",
  "",
  "// Axis (PC) selectors",
  "var selx = document.getElementById('selx');",
  "var sely = document.getElementById('sely');",
  "var selz = document.getElementById('selz');",
  "function fillSelect(sel, current) {",
  "  for (var i=0; i<VARPCT.length; i++) {",
  "    var o = document.createElement('option');",
  "    o.value = i; o.textContent = 'PC' + (i+1);",
  "    if (i === current) o.selected = true;",
  "    sel.appendChild(o);",
  "  }",
  "}",
  "fillSelect(selx, AXIDX.x); fillSelect(sely, AXIDX.y); fillSelect(selz, AXIDX.z);",
  "function onAxisChange() {",
  "  AXIDX.x = parseInt(selx.value, 10);",
  "  AXIDX.y = parseInt(sely.value, 10);",
  "  AXIDX.z = parseInt(selz.value, 10);",
  "  positionPoints(); updateLabels();",
  "}",
  "selx.addEventListener('change', onAxisChange);",
  "sely.addEventListener('change', onAxisChange);",
  "selz.addEventListener('change', onAxisChange);",
  "",
  "// Dot size slider",
  "var dotSlider = document.getElementById('dotsize');",
  "function applyDotSize() {",
  "  var s = parseFloat(dotSlider.value);",
  "  meshes.forEach(function(m) { m.scale.setScalar(s); });",
  "}",
  "dotSlider.addEventListener('input', applyDotSize);",
  "",
  "// Dot transparency slider",
  "var opSlider = document.getElementById('dotopacity');",
  "function applyDotOpacity() {",
  "  dotOpacity = parseFloat(opSlider.value);",
  "  meshes.forEach(function(m) {",
  "    if (activePop === null || m.userData.pop === activePop) {",
  "      m.material.opacity = dotOpacity; m.material.transparent = dotOpacity < 1;",
  "    }",
  "  });",
  "}",
  "opSlider.addEventListener('input', applyDotOpacity);",
  "",
  "var spinning = false;",
  "var spinBtn  = document.getElementById('spin');",
  "spinBtn.addEventListener('click', function() {",
  "  spinning = !spinning;",
  "  spinBtn.classList.toggle('on', spinning);",
  "});",
  "",
  "// Reset view (rotation + zoom) to defaults",
  "document.getElementById('reset').addEventListener('click', function() {",
  "  sph.theta = 0.75; sph.phi = 1.25; sph.r = 5; updCam();",
  "});",
  "",
  "// Dark / light mode toggle",
  "var dark = false;",
  "var darkBtn = document.getElementById('dark');",
  "function setDark(on){",
  "  dark = on;",
  "  document.body.classList.toggle('dark', on);",
  "  renderer.setClearColor(on ? 0x1a1a1a : 0xffffff);",
  "  boxMat.color.set(on ? 0x666666 : 0xcccccc);",
  "  gridMat.color.set(on ? 0x3a3a3a : 0xe0e0e0);",
  "  LBLCOLOR = on ? '#cccccc' : '#666666';",
  "  updateLabels();",
  "  if (highlightMesh) highlightSample(highlightMesh.userData.id);",
  "  darkBtn.innerHTML = on ? '\\u2600 Light mode' : '\\u263e Dark mode';",
  "}",
  "darkBtn.addEventListener('click', function(){ setDark(!dark); });",
  "",
  "// High-resolution snapshot of the WebGL scene (returns a PNG data URL)",
  "var EXPORT_SCALE = 3;",
  "function snapshotURL() {",
  "  var oldPR = renderer.getPixelRatio();",
  "  renderer.setPixelRatio(EXPORT_SCALE);",
  "  renderer.setSize(W, H);",
  "  renderer.render(scene, camera);",
  "  var url = canvas.toDataURL('image/png');",
  "  renderer.setPixelRatio(oldPR);",
  "  renderer.setSize(W, H);",
  "  renderer.render(scene, camera);",
  "  return url;",
  "}",
  "document.getElementById('bpng').addEventListener('click', function() {",
  "  var a = document.createElement('a');",
  "  a.href = snapshotURL(); a.download = '3D_PCA.png'; a.click();",
  "});",
  "document.getElementById('bpdf').addEventListener('click', function() {",
  "  var url = snapshotURL();",
  "  var w = W * EXPORT_SCALE, h = H * EXPORT_SCALE;",
  "  var pdf = new jspdf.jsPDF({orientation: (w >= h ? 'landscape' : 'portrait'), unit: 'pt', format: [w, h]});",
  "  pdf.addImage(url, 'PNG', 0, 0, w, h);",
  "  pdf.save('3D_PCA.pdf');",
  "});",
  "",
  "function animate() {",
  "  requestAnimationFrame(animate);",
  "  if (spinning && !drag) { sph.theta -= 0.003; updCam(); }",
  "  renderer.render(scene, camera);",
  "}",
  "animate();",
  "window.addEventListener('resize', function() {",
  "  W = window.innerWidth; H = window.innerHeight;",
  "  renderer.setSize(W, H); camera.aspect = W/H; camera.updateProjectionMatrix();",
  "});",
  "</script>",
  "</body></html>"
  )
}
