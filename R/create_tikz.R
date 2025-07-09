create_tikz <- function(model, infile = NULL, allowbottom = FALSE, cex = 1.1, outfile = NULL) {
  tmp <- get_model_info(model, infile = infile)
  nodes <- position_nodes(tmp$nodes, tmp$edges)
  make_tikz(nodes, tmp$edges, cex, outfile)
}
