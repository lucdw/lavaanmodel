make_tikz <- function(nodes, edges, cex = 1.1, outfile = NULL) {
  latexsymbols <- c(
    "varGamma", "varSigma", "varDelta", "varUpsilon", "varTheta", "varPhi",
    "varLambda", "varPsi", "varXi", "varOmega", "varPi",
    "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
    "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho",
    "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega",
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho",
    "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"
  )
  nodenaam <- function(nm, blk) {gsub("_", "", paste0("B", blk, nm))}
  nodelabel <- function(nm) {
    if (nm == "") return("")
    startnm <- strsplit(nm, "_", fixed = TRUE)[[1L]][1L]
    if (startnm %in% latexsymbols) nm <- paste0("\\", nm)
    nm <- gsub("_([[:digit:]]*)", "_{\\1}", nm)
    paste0("$", nm, "$")
  }
  mlrij <- attr(nodes, "mlrij", exact = TRUE)
  if (is.null(mlrij)) stop("nodes hasn't been processed by position_nodes !")
  tikzmid <- character(0) # to avoid warning in package check
  zz <- textConnection("tikzmid", "w", local = TRUE)
  texstart <- c(
    "\\documentclass{article}",
    "\\usepackage{amsmath, amssymb}",
    "\\usepackage{amsfonts}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[english]{babel}",
    "\\usepackage{xcolor}",
    "\\usepackage{color}",
    "\\usepackage{tikz}")
  tikzstart <- c(
    "\\usetikzlibrary {shapes.geometric}",
    "\\tikzset{",
    "bend angle=45,",
    paste0("x={(", cex, "cm,0cm)}, y={(0cm,", cex, "cm)},"),
    "lv/.style={circle, draw=green!60, fill=green!15, semithick},",
    "cv/.style={regular polygon, regular polygon sides=6, draw=red!60, fill=red!15, semithick},",
    "ov/.style={rectangle, draw=blue!60, fill=blue!5, semithick},",
    "wov/.style={rectangle, draw=cyan!60, fill=cyan!5, rounded corners, semithick},",
    "bov/.style={rectangle, draw=brown!60, fill=brown!5, rounded corners, semithick},",
    "const/.style={regular polygon, regular polygon sides=3, shape border rotate=180, draw=blue!60, fill=blue!5, semithick}",
    "}")
  texmid <- "\\begin{document}"
  writeLines(
    "\\begin{tikzpicture}",
    zz)
  maxrij <- max(nodes$rij)
  maxcol <- max(nodes$kolom)
  if (mlrij > 0L) {
    writeLines(paste("\\draw (0, ", maxrij - mlrij, ") -- (", maxcol, ",", maxrij - mlrij,
    ");", sep = ""), zz)
  }
  for (j in seq.int(nrow(nodes))) {
    writeLines(paste(
      "\\node[", nodes$tiepe[j], "] (", nodenaam(nodes$naam[j], nodes$blok[j]),
      ") at (", nodes$kolom[j], ",", maxrij - nodes$rij[j], ") {",
      nodelabel(nodes$naam[j]), "};", sep = ""), zz)
  }
  for (j in seq.int(nrow(edges))) {
    van <- which(nodes$id == edges$van[j])
    vannaam <- nodenaam(nodes$naam[van], nodes$blok[van])
    naar <- which(nodes$id == edges$naar[j])
    naarnaam <- nodenaam(nodes$naam[naar], nodes$blok[naar])
    if (van == naar) { # self
      if (nodes$kolom[van] == 1L) {
        writeLines(paste("\\path[->] (", vannaam,
                         ") edge [loop left] node {",
                         nodelabel(edges$label[j] ), "} ();",
                         sep = ""), zz)
      } else if (nodes$rij[van] == maxrij) {
        writeLines(paste("\\path[->] (", vannaam,
                         ") edge [loop below] node {",
                         nodelabel(edges$label[j] ), "} ();",
                         sep = ""), zz)
      } else {
        writeLines(paste("\\path[->] (", vannaam,
                         ") edge [loop above] node {",
                         nodelabel(edges$label[j] ), "} ();",
                         sep = ""), zz)
      }
    } else {
      bending <- " "
      if (nodes$kolom[van] == nodes$kolom[naar] &&
          nodes$kolom[van] %in% c(1L, maxcol)) {
        if ((nodes$kolom[van] == 1L) == (nodes$rij[van] < nodes$rij[naar])) {
          bending <- " [bend right] "
        } else {
          bending <- " [bend left] "
        }
      } else if (nodes$rij[van] == nodes$rij[naar] &&
                 nodes$rij[van] %in% c(1L, maxrij)) {
        if ((nodes$rij[van] < 3L) == (nodes$kolom[van] < nodes$kolom[naar])) {
          bending <- " [bend left] "
        } else {
          bending <- " [bend right] "
        }
      }
      if (edges$tiepe[j] == "p") {
        writeLines(paste("\\path[->] (", vannaam, ") edge", bending,
                         "node[above, sloped] {",
                         nodelabel(edges$label[j] ), "} (",
                         naarnaam, ");", sep = ""), zz)
      } else {
        writeLines(paste("\\path[<->] (", vannaam, ") edge", bending,
                         "node[above, sloped] {",
                         nodelabel(edges$label[j] ), "} (",
                         naarnaam, ");", sep = ""), zz)
      }
    }
  }
  writeLines("\\end{tikzpicture}", zz)
  texend <- "\\end{document}"
  close(zz)
  if (!is.null(outfile)) {
    cat(paste(c(texstart, tikzstart, texmid, tikzmid, texend), collapse="\n"),
        file=outfile)
    }
  return(invisible(c(tikzstart, tikzmid)))
}
