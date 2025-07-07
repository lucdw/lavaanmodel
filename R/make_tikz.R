make_tikz <- function(nodes, edges, preamble = FALSE) {
  A <- matrix(NA, nrow = nrow(nodes), ncol = nrow(nodes))
  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)
  boxsize <- 0.4 / max(rijen, kolommen)
  for (j in seq.int(nrow(edges))) {
     van <- which(nodes$id == edges$naar[j])
     naar <- which(nodes$id == edges$van[j])
     A[van, naar] <- edges$label[j]
     if (edges$label[j] == "") A[van, naar] <- ""
     if (edges$tiepe[j] == "d") A[naar, van] <- ""
  }
  pos <- matrix(0, nrow = nrow(nodes), ncol = 2L)
  boxtypes <- character(nrow(nodes))
  boxprops <- numeric(nrow(nodes))
  boxcols <- integer(nrow(nodes))
  for (j in seq.int(nrow(nodes))) {
    pos[j, 2L] <- 1 - nodes$rij[j] / (max(nodes$rij) + 1)
    pos[j, 1L] <- nodes$kolom[j] / (max(nodes$kolom) + 1)
    boxtypes[j] <- switch(nodes$tiepe[j],
                          lv = "circle",
                          ov = "rect",
                          wov = "rect",
                          bov = "rect",
                          cv = "hexa",
                          const = "multi")
    boxprops[j] <- switch(nodes$tiepe[j],
                          lv = 1,
                          ov = 0.5,
                          wov = 0.5,
                          bov = 0.5,
                          cv = 1,
                          const = 0.7)
    boxcols[j] <- switch(nodes$tiepe[j],
                          lv = 7,
                          ov = 2,
                          wov = 3,
                          bov = 4,
                          cv = 5,
                          const = 6)
  }
  namen <- sub("_([[:digit:]]*)$", "[\\1]", nodes$naam)
  namen <- str2expression(namen)
  plotmat(A, pos=pos, lwd=1, curve=0, box.lwd=2, cex.txt=0.8, box.col=boxcols,
          name = namen, box.cex=0.8, box.size = boxsize, arr.length=0.2,
          arr.pos = 0.55, box.type = boxtypes, shadow.size = 0,
          nr = 3, main="SEM diagram", box.prop = boxprops, self.cex = 0.4)
}
