# extract info from model
get_model_info <- function(model = NULL, infile = NULL) {
  if (!is.null(infile)) {
    stopifnot(file.exists(infile))
    model <- readLines(infile)
  }
  if (is.list(model) && !is.null(model$op) && !is.null(model$lhs) &&
      !is.null(model$rhs) && !is.null(model$label) &&
      !is.null(model$fixed)) {
    tbl <- as.data.frame(model)
  } else if (is.character(model)) {
    tbl <- lavParseModelString(model.syntax = model,
                               as.data.frame. = TRUE)
  } else {
    stop("model, or content of infile, not in interpretable form!")
  }
  if (is.null(tbl$block)) {
    tbl$block <- 0L
  } else {
    if (all(tbl$block == tbl$block[1L])) tbl$block <- 0L
  }
  lvs <- unique(tbl$lhs[tbl$op == "=~" | tbl$op == "<~"])
  indicators <- unique(tbl$rhs[tbl$op == "=~" | tbl$op == "<~"])
  regdest <- unique(tbl$lhs[tbl$op == "~" | tbl$op == "~1"])
  regsrc <-  unique(tbl$rhs[tbl$op == "~" | tbl$op == "~1"])
  vars <- unique(tbl$lhs[tbl$op == "~~" & tbl$lhs == tbl$rhs])
  covars <- union(tbl$lhs[tbl$op == "~~" & tbl$lhs != tbl$rhs],
                  tbl$rhs[tbl$op == "~~" & tbl$lhs != tbl$rhs])
  maxedges <- nrow(tbl)
  maxnodes <- 2 * maxedges
  nodes <- data.frame(
    id = integer(maxnodes),
    naam = character(maxnodes),
    tiepe = character(maxnodes), # ov, lv, cv, wov, bov, const
            # cv: composite; wov = within; bov = between; const = intercept
    blok = integer(maxnodes),
    voorkeur = character(maxnodes), # l = links, r = rechts, m = midden
    indicatoren = integer(maxnodes),
    rij = integer(maxnodes),
    kolom = integer(maxnodes)
  )
  edges <- data.frame(
    id = integer(maxedges),
    label = character(maxedges),
    van = integer(maxedges),
    naar = integer(maxedges),
    tiepe = character(maxedges) # p = pijl, d = dubbele pijl, s = self
  )
  curnode <- 0L
  curedge <- 0L
  for (i in seq.int(nrow(tbl))) {
    if (tbl$op[i] == "=~") {
      #### =~ : is manifested by ####
      # lhs node
      jl <- match(tbl$lhs[i], nodes$naam, nomatch = 0L)
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- "lv"
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        nodes$tiepe[jl] <- "lv"
      }
      # rhs node
      jr <- match(tbl$rhs[i], nodes$naam, nomatch = 0L)
      nodetype <- switch(tbl$block[i] + 1L, "ov", "wov", "bov")
      if (is.null(nodetype)) nodetype <- "ov"
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- paste(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jl
      edges$naar[curedge] <- jr
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "<~") {
      #### <~ : is a result of ####
      # lhs node
      jl <- match(tbl$lhs[i], nodes$naam, nomatch = 0L)
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- "cv"
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        nodes$tiepe[jl] <- "cv"
      }
      # rhs node
      jr <- match(tbl$rhs[i], nodes$naam, nomatch = 0L)
      nodetype <- switch(tbl$block[i] + 1L, "ov", "wov", "bov")
      if (is.null(nodetype)) nodetype <- "ov"
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- paste(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "~") {
      #### ~ : is regressed on ####
      # lhs node
      jl <- match(tbl$lhs[i], nodes$naam, nomatch = 0L)
      nodetype <- switch(tbl$block[i] + 1L, "ov", "wov", "bov")
      if (is.null(nodetype)) nodetype <- "ov"
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$voorkeur[curnode] <- "r"
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
        if (nodes$voorkeur[jl] == "") {
          nodes$voorkeur[jl] <- "r"
        } else if (nodes$voorkeur[jl] == "l") {
          nodes$voorkeur[jl] <- "m"
        }
      }
      # rhs node
      jr <- match(tbl$rhs[i], nodes$naam, nomatch = 0L)
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$voorkeur[curnode] <- "l"
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
        if (nodes$voorkeur[jr] == "") {
          nodes$voorkeur[jr] <- "l"
        } else if (nodes$voorkeur[jr] == "r") {
          nodes$voorkeur[jr] <- "m"
        }
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- paste(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "~1") {
      #### ~1 : intercept ####
      # lhs node
      jl <- match(tbl$lhs[i], nodes$naam, nomatch = 0L)
      nodetype <- switch(tbl$block[i] + 1L, "ov", "wov", "bov")
      if (is.null(nodetype)) nodetype <- "ov"
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$voorkeur[curnode] <- "r"
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
        if (nodes$voorkeur[jl] == "") {
          nodes$voorkeur[jl] <- "r"
        } else if (nodes$voorkeur[jl] == "l") {
          nodes$voorkeur[jl] <- "m"
        }
      }
      # rhs node
      jr <- 0L
      curnode <- curnode + 1L
      jr <- curnode
      nodes$id[curnode] <- curnode
      nodes$naam[curnode] <- "1"
      nodes$tiepe[curnode] <- "const"
      nodes$blok[curnode] <- tbl$block[i]
      nodes$voorkeur[curnode] <- "l"
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- paste(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "~~") {
      #### ~~ : is correlated with ####
      # lhs node
      jl <- match(tbl$lhs[i], nodes$naam, nomatch = 0L)
      nodetype <- switch(tbl$block[i] + 1L, "ov", "wov", "bov")
      if (is.null(nodetype)) nodetype <- "ov"
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
      }
      # rhs node
      jr <- match(tbl$rhs[i], nodes$naam, nomatch = 0L)
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- paste(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- ifelse(jl==jr, "s", "d")
    }
  }
  latents <- nodes$id[nodes$tiepe == "lv" | nodes$tiepe == "cv"]
  for (j in latents) {
    nodes$indicatoren[j] = sum(tbl$lhs == nodes$naam[j] & (
      tbl$op == "=~" | tbl$op == "<~"))
  }
  nodes <- nodes[seq.int(curnode), ]
  edges <- edges[seq.int(curedge), ]
  edges$label <- trimws(edges$label)
  return(list(nodes=nodes, edges=edges))
}
