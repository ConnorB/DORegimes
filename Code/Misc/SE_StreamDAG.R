# Talladega -----------------------------------------------------------------------
talNet <- graph_from_literal(
  TLA02 --+ TLA01,
  TLA01 --+ TLM15,
  TLM16 --+ TLM15,
  TLA03 --+ TLA02,
  TLAG4 --+ TLA03,
  TLA05 --+ TLA04,
  TLA07 --+ TLA04,
  TLA04 --+ TLAG4,
  TLA06 --+ TLA05,
  TLA08 --+ TLA07,
  TLB02 --+ TLB01,
  TLB01 --+ TLM05,
  TLB03 --+ TLB02,
  TLB05 --+ TLB02,
  TLB04 --+ TLB03,
  TLC01 --+ TLM02,
  TLM03 --+ TLM02,
  TLMG3 --+ TLM03,
  TLC05 --+ TLCG2,
  TLC02 --+ TLCG2,
  TLC04 --+ TLC03,
  TLC03 --+ TLCG2,
  TLC06 --+ TLC05,
  TLC07 --+ TLC06,
  TLCG2 --+ TLCG1,
  TLM04 --+ TLMG3,
  TLCG1 --+ TLC01,
  TLM02 --+ TLM01,
  TLM05 --+ TLM04,
  TLM06 --+ TLM05,
  TLM07 --+ TLM06,
  TLM08 --+ TLM07,
  TLM09 --+ TLM08,
  TLM11 --+ TLM09,
  TLM10 --+ TLM11,
  TLM12 --+ TLM10,
  TLZ01 --+ TLM11,
  TLM13 --+ TLM12,
  TLM14 --+ TLM13,
  TLM15 --+ TLM14,
  TLMG5 --+ TLM16,
  TLM18 --+ TLM17,
  TLM17 --+ TLMG5,
  TLY01 --+ TLM18,
  TLX01 --+ TLM19,
  TLM19 --+ TLM18,
  TLM21 --+ TLM20,
  TLM20 --+ TLM19,
  TLM22 --+ TLM21,
  TLX03 --+ TLX01,
  TLX02 --+ TLX01,
  TLY02 --+ TLY01,
  TLY03 --+ TLY01,
  TLZ03 --+ TLZ01,
  TLZ02 --+ TLZ01
)
# Weyerhaeuser ------------------------------------------------------------
whrNet <- graph_from_literal(
  WHM14 --+ WHM13,
  WHM13 --+ WHM12,
  WHM12 --+ WHM11,
  WHM11 --+ WHM10,
  WHM10 --+ WHM09,
  WHM09 --+ WHM08,
  WHM08 --+ WHM07,
  WHM07 --+ WHM06,
  WHM06 --+ WHM05,
  WHM05 --+ WHM04,
  WHM04 --+ WHM03,
  WHM03 --+ WHM02,
  WHM02 --+ WHM01,
  WHB07 --+ WHB06,
  WHB06 --+ WHB05,
  WHB05 --+ WHB04,
  WHB04 --+ WHB03,
  WHB03 --+ WHB02,
  WHB02 --+ WHB01,
  WHB01 --+ WHM02,
  WHA06 --+ WHA05,
  WHA05 --+ WHA04,
  WHA04 --+ WHA03,
  WHA03 --+ WHA02,
  WHA02 --+ WHA01,
  WHA01 --+ WHM02
)
# Paint Rock --------------------------------------------------------------
prfNet <- graph_from_literal(
  PRM23 --+ PRM22,
  PRM22 --+ PRM21,
  PRM21 --+ PRM20,
  PRM20 --+ PRM19,
  PRM19 --+ PRM18,
  PRM18 --+ PRM17,
  PRM17 --+ PRM16,
  PRA02 --+ PRA01,
  PRA01 --+ PRM16,
  PRM16 --+ PRM15,
  PRM15 --+ PRM14,
  PRM14 --+ PRM13,
  PRM13 --+ PRM12,
  PRM12 --+ PRM11,
  PRM11 --+ PRM10,
  PRM10 --+ PRM09,
  PRB03 --+ PRB02,
  PRB02 --+ PRB01,
  PRB01 --+ PRM09,
  PRM09 --+ PRM08,
  PRM08 --+ PRM07,
  PRM07 --+ PRM06,
  PRM06 --+ PRM05,
  PRM05 --+ PRM04,
  PRM04 --+ PRM03,
  PRM03 --+ PRM02,
  PRM02 --+ PRM01)



# StreamDAG List ----------------------------------------------------------
seNets <- list(PRF = prfNet, TAL = talNet, WHR = whrNet)
rm(prfNet, talNet, whrNet)

reconnect_and_remove_nodes <- function(graph, keep_nodes) {
  # Identify nodes to remove
  remove_nodes <- setdiff(V(graph)$name, keep_nodes)
  
  # For each node to remove
  for (v in remove_nodes) {
    if (!v %in% V(graph)$name) next  # Skip if already removed
    
    # Get predecessors and successors
    preds <- neighbors(graph, v, mode = "in")$name
    succs <- neighbors(graph, v, mode = "out")$name
    
    # Create new edges from predecessors to successors
    if (length(preds) > 0 && length(succs) > 0) {
      new_edges <- expand.grid(from = preds, to = succs, stringsAsFactors = FALSE)
      for (i in seq_len(nrow(new_edges))) {
        from <- new_edges$from[i]
        to <- new_edges$to[i]
        
        # Avoid duplicate edges and self-loops
        if (from != to && !are_adjacent(graph, from, to)) {
          graph <- add_edges(graph, c(from, to))
        }
      }
    }
    
    # Remove the node
    graph <- delete_vertices(graph, v)
  }
  
  return(graph)
}
