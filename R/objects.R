#' @title Omics Network Object
#' @import R6
#' @import igraph
#' @import dplyr
#' @import magrittr
#' 
#' @export
omics.network <- R6::R6Class("omics.network",
    public = list(
        ig = NULL,
        initialize = function(ig, 
                              node.fn=list(),
                              edge.fn=list()
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
            self$graph.update()
        },
        peek = function(...) {
            cat("Omics Network Object\n")
            print(self$ig)
            invisible(self)
        },
        nodes = function(attr) {
            if (attr %not_in% igraph::vertex_attr_names(self$ig)) {
                stop("Available node attributes: ", paste(igraph::vertex_attr_names(self$ig), collapse=","))
            }
            return(igraph::get.vertex.attribute(self$ig, attr))
        },
        nodes.map = function(ids, attr_1, attr_2) {
            self$nodes(attr_2)[match(ids, self$nodes(attr_1))]
        },
        nodes.annotate = function(values, attr) {
            self$ig <- igraph::set_vertex_attr(graph=self$ig, name=attr, value=values)
        },
        nodes.attributes = function() {
            igraph::as_data_frame(self$ig, what="vertices")
        },
        nodes.filter = function(conditions, attr="name") {
            v <- rlang::parse_exprs(conditions)
            self$nodes.attributes() %>%
                dplyr::filter(!!!v) %>%
                dplyr::pull({{attr}})
        },
        nodes.update = function() {
            node.attr <- mapply(function(fn) {
                fn(self$ig)
            }, private$node.fn, SIMPLIFY=FALSE, USE.NAMES=TRUE)
            for (attr in names(node.attr)) {
                self$nodes.annotate(node.attr[[attr]], attr)
            }
        },
        nodes.neighbors = function(ids, attr="name", degree=1, neighbors.only=TRUE, return.names=FALSE) {
            stopifnot(attr %in% igraph::vertex_attr_names(self$ig))
            stopifnot(degree >= 1)
            v <- self$nodes.map(ids, attr, "name")
            v.names <- v
            for (i in seq_len(degree)) {
                v.id <- unique(unlist(igraph::adjacent_vertices(self$ig, v.names, mode="all")))
                v.adjacent <-  self$nodes("name")[v.id]
                v.names <- unique(c(v.names, v.adjacent))
            }
            if (neighbors.only) {
                v.names <- setdiff(v.names, v)
            }
            if (return.names) {
                return(v.names)
            } else {
                return(self$nodes.map(v.names, "name", attr))
            }
        },
        edges = function(attr) {
            if (attr %not_in% igraph::edge_attr_names(self$ig)) {
                stop("Available edge attributes: ", paste(igraph::edge_attr_names(self$ig), collapse=","))
            }
            return(igraph::get.edge.attribute(self$ig, attr))
        },
        edges.map = function(ids, attr_1, attr_2) {
            self$edges(attr_2)[match(ids, self$edges(attr_1))]
        },
        edges.annotate = function(values, attr) {
            self$ig <- igraph::set_edge_attr(graph=self$ig, name=attr, value=values)
        },
        edges.attributes = function() {
            igraph::as_data_frame(self$ig, what="edges") %>%
            dplyr::relocate(name, from, to)  
        },
        edges.filter = function(conditions, attr="name") {
            v <- rlang::parse_exprs(conditions)
            self$edges.attributes() %>%
                dplyr::filter(!!!v) %>%
                dplyr::pull({{attr}})
        },
        edges.update = function() {
            edge.attr <- mapply(function(fn) {
                fn(self$ig)
            }, private$edge.fn, SIMPLIFY=FALSE, USE.NAMES=TRUE)
            for (attr in names(edge.attr)) {
                self$edges.annotate(edge.attr[[attr]], attr)
            }
        },
        graph.update = function() {
            self$nodes.update()
            self$edges.update()
        },
        graph.on.change = function(update) {
            if (update) {
                self$graph.update()
            }
        },
        graph.delete.nodes = function(ids, attr="name", update=TRUE) {
            v.names <- self$nodes.map(ids, attr, "name")
            ig.c <- self$clone()
            ig.c$ig <- igraph::delete_vertices(ig.c$ig, v.names)
            ig.c$graph.on.change(update)
            return(ig.c)
        },
        graph.delete.edges = function(ids, attr="name", update=TRUE) {
            e.names <- self$edges.map(ids, attr, "name")
            ig.c <- self$clone()
            ig.c$ig <- igraph::delete_edges(ig.c$ig, e.names)
            ig.c$graph.on.change(update)
            return(ig.c)
        },
        graph.simplify = function(remove.multiple=TRUE, remove.loops=TRUE, update=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::simplify(ig.c$ig, remove.multiple=remove.multiple, remove.loops=remove.loops)
            ig.c$graph.on.change(update)
            return(ig.c)
        },
        graph.delete.isolates = function(update=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::delete_vertices(ig.c$ig, which(igraph::degree(ig.c$ig) == 0))
            ig.c$graph.on.change(update)
            return(ig.c)
        },
        graph.subset.nodes = function(ids, attr="name", degree=0, update=TRUE) {
            stopifnot(degree >= 0)
            if (degree == 0) {
                v.names <- self$nodes.map(ids, attr, "name")
            } else {
                v.names <- self$nodes.neighbors(ids, attr, degree, neighbors.only=FALSE, return.names=TRUE)   
            }
            ig.c <- self$clone()
            ig.c$ig <- igraph::induced_subgraph(ig.c$ig, v.names)
            ig.c$graph.on.change(update)
            return(ig.c)
        },
        graph.subset.edges = function(ids, update=TRUE) {
            ig.c <- self$clone()
            ig.c$ig <- igraph::subgraph.edges(ig.c$ig, ids)
            ig.c$graph.on.change(update)
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
                        no.margin=TRUE,
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
            if (no.margin) {
                par(mar=c(0,0,0,0))
            }
            set.seed(seed)
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
    ),
    private = list(
        edge.fn = NULL,
        node.fn = NULL
    )
)
