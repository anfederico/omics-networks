#' @title An omics network object
#'
#' @import R6
#' @import igraph
#' @import dplyr
#' @import magrittr
#' 
#' @export
network <- R6::R6Class("network",
    private = list(
        edge.fn = NULL,
        node.fn = NULL
    ),
    public = list(
        ig = NULL,
        initialize = function(ig, 
                              node.fn=node.properties(),
                              edge.fn=edge.properties()
                              ) {
            self$ig <- ig
            igraph::E(self$ig)$name <- igraph::as_ids(igraph::E(self$ig))
            df <- igraph::as_data_frame(self$ig, what="edges")
            if (ncol(df) == 0) {
                igraph::E(self$ig)$from <- df$from
                igraph::E(self$ig)$to <- df$to
            }
            private$node.fn <- node.fn
            private$edge.fn <- edge.fn
        },
        peek = function(...) {
            cat("Omics Network Object\n")
            print(self$ig)
            invisible(self)
        },
        node = function(attr) {
            if (attr %not_in% igraph::vertex_attr_names(self$ig)) {
                stop("Available node attributes: ", paste(igraph::vertex_attr_names(self$ig), collapse=","))
            }
            return(igraph::get.vertex.attribute(self$ig, attr))
        },
        node.annotate = function(values, attr) {
            self$ig <- igraph::set_vertex_attr(graph=self$ig, name=attr, value=values)
        },
        node.attributes = function() {
            igraph::as_data_frame(self$ig, what="vertices")
        },
        node.filter = function(conditions, attr="name") {
            v <- rlang::parse_exprs(conditions)
            self$node.attributes() %>%
                dplyr::filter(!!!v) %>%
                dplyr::pull({{attr}})
        },
        node.recompute = function() {
            node.attr <- mapply(function(fn) {
                fn(self$ig)
            }, private$node.fn, SIMPLIFY=FALSE, USE.NAMES=TRUE)
            for (attr in names(node.attr)) {
                self$node.annotate(node.attr[[attr]], attr)
            }
        },
        node.neighbors = function(ids, attr="name", degree=1, neighbors.only=TRUE, return.names=FALSE) {
            stopifnot(attr %in% igraph::vertex_attr_names(self$ig))
            stopifnot(degree >= 1)
            v <- self$node("name")[match(ids, self$node(attr))]
            v.names <- v
            for (i in seq_len(degree)) {
                v.id <- unique(unlist(igraph::adjacent_vertices(self$ig, v.names, mode="all")))
                v.adjacent <-  self$node("name")[v.id]
                v.names <- unique(c(v.names, v.adjacent))
            }
            if (neighbors.only) {
                v.names <- setdiff(v.names, v)
            }
            if (return.names) {
                return(v.names)
            } else {
                return(self$node(attr)[match(v.names, self$node("name"))])
            }
        },
        edge = function(attr) {
            if (attr %not_in% igraph::edge_attr_names(self$ig)) {
                stop("Available edge attributes: ", paste(igraph::edge_attr_names(self$ig), collapse=","))
            }
            return(igraph::get.edge.attribute(self$ig, attr))
        },
        edge.annotate = function(values, attr) {
            self$ig <- igraph::set_edge_attr(graph=self$ig, name=attr, value=values)
        },
        edge.attributes = function() {
            igraph::as_data_frame(self$ig, what="edges") %>%
            dplyr::relocate(name, from, to)  
        },
        edge.filter = function(conditions, attr="name") {
            v <- rlang::parse_exprs(conditions)
            self$edge.attributes() %>%
                dplyr::filter(!!!v) %>%
                dplyr::pull({{attr}})
        },
        edge.recompute = function() {
            edge.attr <- mapply(function(fn) {
                fn(self$ig)
            }, private$edge.fn, SIMPLIFY=FALSE, USE.NAMES=TRUE)
            for (attr in names(edge.attr)) {
                self$edge.annotate(edge.attr[[attr]], attr)
            }
        },
        graph.recompute = function() {
            self$node.recompute()
            self$edge.recompute()
        },
        graph.simplify = function(recompute=TRUE, remove.multiple=TRUE, remove.loops=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::simplify(ig.c$ig, remove.multiple=remove.multiple, remove.loops=remove.loops)
            if (recompute) {
                ig.c$graph.recompute()
            }
            return(ig.c)
        },
        graph.delete.isolates = function(recompute=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::delete_vertices(ig.c$ig, which(igraph::degree(ig.c$ig) == 0))
            if (recompute) {
                ig.c$graph.recompute()
            }
            return(ig.c)
        },
        graph.subset.nodes = function(ids, attr="name", degree=0, recompute=TRUE) {
            stopifnot(degree >= 0)
            if (degree == 0) {
                v.names <- self$node("name")[match(ids, self$node(attr))]
            } else {
                v.names <- self$node.neighbors(ids, attr, degree, neighbors.only=FALSE, return.names=TRUE)   
            }
            ig.c <- self$clone()
            ig.c$ig <- igraph::induced_subgraph(ig.c$ig, v.names)
            if (recompute) {
                ig.c$graph.recompute()
            }
            return(ig.c)
        },
        graph.subset.edges = function(ids, recompute=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::subgraph.edges(ig.c$ig, ids)
            if (recompute) {
                ig.c$graph.recompute()
            }
            return(ig.c)
        },
        plot = function(vertex.size=4,
                        vertex.color="#EEEEEE",
                        vertex.frame.color="#000040",
                        vertex.label=NA,
                        vertex.label.family="Helvetica",
                        vertex.label.color="#000000",
                        vertex.label.font=2,
                        vertex.label.cex=0.75,
                        vertex.label.dist=0,
                        edge.width=1,
                        edge.color="#3D3D3D",
                        edge.arrow.size=0.2,
                        layout=NULL, 
                        seed=1,
                        ...) {
            
            if (is.null(layout)) {
                set.seed(seed)
                layout <- igraph::layout_with_graphopt(self$ig, 
                                                       start=NULL, 
                                                       niter=1000,
                                                       charge=0.005,
                                                       mass=30, 
                                                       spring.length=0,
                                                       spring.constant=1, 
                                                       max.sa.movement=5)
            }
            plot(self$ig,
                 vertex.size=vertex.size,
                 vertex.color=vertex.color,
                 vertex.frame.color=vertex.frame.color,
                 vertex.label=vertex.label,
                 vertex.label.family=vertex.label.family,
                 vertex.label.color=vertex.label.color,
                 vertex.label.font=vertex.label.font,
                 vertex.label.cex=vertex.label.cex,
                 vertex.label.dist=vertex.label.dist,
                 edge.width=edge.width,
                 edge.color=edge.color,
                 layout=layout,
                 ...)
        }
    )
)