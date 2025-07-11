delta_node_edge <- function(edgevan, edgenaar, node) {
  van <- edgevan - node
  naar <- edgenaar - node
  if (van[1L] * naar[2L] - van[2L] * naar[1L] == 0) return(0)
  discr <- solve(matrix(c(van, naar), nrow=2, byrow=TRUE))
  a <- sum(discr[1L, ])
  b <- sum(discr[2L, ])
  return(1/sqrt(a*a + b*b))
}
delta_nodes <- function(node1, node2) {
  delta <- node2 - node1
  sqrt(sum(delta*delta))
}
position_nodes <- function(nodes, edges, allowbottom = FALSE) {
  if (any(nodes$blok > 0L)) { # multilevel, only level:1 and level:2 accepted
    nodes1 <- nodes[nodes$blok == 2L, ]
    nodes1$blok <- 0L
    nodes2 <- nodes[nodes$blok == 1L, ]
    nodes2$blok <- 0L
    nodes1 <- position_nodes(nodes1, edges)
    nodes2 <- position_nodes(nodes2, edges)
    rijen1 <- max(nodes1$rij)
    nodes2$rij <- nodes2$rij + rijen1 + 1L
    nodes1$blok <- 2L
    nodes2$blok <- 1L
    nodes <- rbind(nodes1, nodes2)
    attr(nodes, "mlrij") <- rijen1 + 1L
    return(nodes)
  }
  # structural part
  strucs <- which(nodes$voorkeur == "l")
  maxindicatoren <- max(nodes$indicatoren[strucs])
  nodes$rij[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
  nodes$kolom[strucs] <- 2L
  strucs <- which(nodes$voorkeur == "r")
  maxindicatoren <- max(nodes$indicatoren[strucs])
  nodes$rij[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
  nodes$kolom[strucs] <- 99L
  if (any(nodes$voorkeur == "m")) {
    if (length(strucs) > 1L && allowbottom) {
      vanaf <- integer(length(strucs) / 2)
      nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
    }
  }
  if (any(nodes$voorkeur == "m")) {
    strucs <- which(nodes$voorkeur == "m")
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$kolom[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$rij[strucs] <- 2L
  }
  if (any(nodes$voorkeur == "b")) {
    strucs <- which(nodes$voorkeur == "b")
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$kolom[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$rij[strucs] <- 99L
  }
  # indicators
  allindicators <- nodes$id[nodes$voorkeur == ""]
  strucs <- which(nodes$voorkeur == "l")
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    rijtje <- nodes$rij[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (nodes$rij[k] == 0) {
        nodes$rij[k] <- rijtje
        rijtje <- rijtje + 1L
        nodes$kolom[k] <- 1L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "r")
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    rijtje <- nodes$rij[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (nodes$rij[k] == 0) {
        nodes$rij[k] <- rijtje
        rijtje <- rijtje + 1L
        nodes$kolom[k] <- 100L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "m")
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    kolompje <- nodes$kolom[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (nodes$rij[k] == 0) {
        nodes$kolom[k] <- kolompje
        kolompje <- kolompje + 1L
        nodes$rij[k] <- 1L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "b")
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    kolompje <- nodes$kolom[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (nodes$rij[k] == 0) {
        nodes$kolom[k] <- kolompje
        kolompje <- kolompje + 1L
        nodes$rij[k] <- 100L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  # remove the holes in rows and columns
  rijen <- sort(unique(nodes$rij))
  nodes$rij <- match(nodes$rij, rijen)
  kolommen <- sort(unique(nodes$kolom))
  nodes$kolom <- match(nodes$kolom, kolommen)
  attr(nodes, "mlrij") <- 0L
  return(nodes)
}
