#' DGIdb GraphQL endpoint
#'
#' Uses env var `DGIDB_URL` if set, otherwise defaults to public endpoint.
endpoint <- Sys.getenv("DGIDB_URL", "https://dgidb.org/api/graphql")

#' Run a GraphQL POST request and parse JSON
#' @keywords internal
run_gql <- function(query, variables = NULL, timeout_sec = 20) {
  resp <- httr::POST(
    url    = endpoint,
    body   = list(query = query, variables = variables),
    encode = "json",
    httr::timeout(timeout_sec)
  )

  status <- httr::status_code(resp)
  text_resp <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(text_resp, simplifyVector = TRUE),
                     error = function(e) list())

  list(
    status = status,
    data   = parsed$data,
    errors = parsed$errors
  )
}

#' Pretty-print a GraphQL response subset
#' @keywords internal
print_result <- function(title, res, node_path) {
  cat("==", title, "==\n")

  if (inherits(res, "error")) {
    cat("Request error:", conditionMessage(res), "\n\n")
    return(invisible())
  }

  if (res$status != 200) {
    cat("HTTP status:", res$status, "\n\n")
    return(invisible())
  }

  if (!is.null(res$errors)) {
    cat("GraphQL errors:\n")
    print(res$errors)
    cat("\n")
    return(invisible())
  }

  nodes <- res$data
  for (k in node_path) {
    if (is.null(nodes[[k]])) {
      nodes <- NULL
      break
    }
    nodes <- nodes[[k]]
  }

  if (is.null(nodes)) {
    cat("No nodes found.\n\n")
    return(invisible())
  }

  n_nodes <- if (is.data.frame(nodes)) nrow(nodes) else length(nodes)
  cat("OK. Nodes:", n_nodes, "\n")

  show_name <- NULL
  if (is.data.frame(nodes) && "name" %in% names(nodes)) {
    show_name <- nodes$name[[1]]
  } else if (is.list(nodes) && length(nodes) > 0 && !is.null(nodes[[1]]$name)) {
    show_name <- nodes[[1]]$name
  }
  if (!is.null(show_name)) {
    cat("Example name:", show_name, "\n")
  }
  cat("\n")
}

#' Run DGIdb query for one gene set and write CSV outputs
#'
#' @param label Label for the gene set (used in logs/output paths)
#' @param genes Character vector of gene symbols
#' @param out_dir Output directory; created if missing
#' @param approve Logical; if TRUE keep only FDA approved drugs; if FALSE keep all.
#' @return Invisibly returns NULL; writes `dgidb_hits.csv` and `dgidb_raw.csv`
#' @export
run_gene_set <- function(label, genes, out_dir, approve = TRUE) {
  genes <- unique(genes[!is.na(genes) & nzchar(genes)])
  if (length(genes) == 0) {
    cat(label, ": no genes, skipped.\n\n")
    return(invisible())
  }
  gene_args <- paste(sprintf('\"%s\"', genes), collapse = ", ")
  cat("== Running set:", label, "Genes:", length(genes), "==\n")

  q <- sprintf(
    "\n    {\n      genes(names: [%s]) {\n        nodes {\n          name\n          interactions {\n            drug { name conceptId }\n            interactionTypes { type }\n            interactionScore\n            sources { sourceDbName }\n          }\n        }\n      }\n    }\n    ",
    gene_args
  )

  res <- tryCatch(run_gql(q), error = function(e) e)
  print_result(paste(label, "Gene list -> Drug interactions"), res, c("genes", "nodes"))

  if (inherits(res, "error") || res$status != 200 || !is.null(res$errors)) {
    cat(label, ": aggregation skipped due to request error.\n\n")
    return(invisible())
  }

  nodes <- res$data$genes$nodes
  if (is.data.frame(nodes)) {
    nodes <- lapply(seq_len(nrow(nodes)), function(i) {
      list(name = nodes$name[[i]], interactions = nodes$interactions[[i]])
    })
  }

  rows <- list()
  if (!is.null(nodes) && length(nodes) > 0) {
    for (g in nodes) {
      inters <- g$interactions
      if (is.null(inters)) next
      if (is.data.frame(inters)) {
        inters <- lapply(seq_len(nrow(inters)), function(i) as.list(inters[i, ]))
      }
      for (it in inters) {
        srcs <- it$sources
        if (is.data.frame(srcs)) {
          srcs_vec <- srcs$sourceDbName
        } else if (is.list(srcs)) {
          srcs_vec <- sapply(srcs, function(x) {
            if (!is.null(x$sourceDbName)) x$sourceDbName else NA_character_
          })
        } else {
          srcs_vec <- NA_character_
        }
        srcs_vec <- srcs_vec[!is.na(srcs_vec) & nzchar(srcs_vec)]
        src_str <- if (length(srcs_vec)) paste(sort(unique(srcs_vec)), collapse = ";") else NA_character_
        rows[[length(rows) + 1]] <- data.frame(
          gene = g$name,
          drug = it$drug$name,
          drug_concept_id = it$drug$conceptId,
          interaction_score = it$interactionScore,
          interaction_types = paste(it$interactionTypes$type, collapse = ";"),
          sources = src_str,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    cat(label, ": No gene-drug interactions returned.\n\n")
    return(invisible())
  }

  df <- do.call(rbind, rows)
  df$key <- paste(df$drug, df$drug_concept_id, sep = "|")
  agg <- aggregate(
    interaction_score ~ drug + drug_concept_id + key,
    data = df,
    FUN = function(x) c(total = sum(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
  )
  scores_mat <- agg$interaction_score
  agg$total_score <- scores_mat[, "total"]
  agg$max_score <- scores_mat[, "max"]
  agg$interaction_score <- NULL

  genes_by_key <- tapply(df$gene, df$key, function(x) sort(unique(x)))
  agg$gene_count <- sapply(agg$key, function(k) length(genes_by_key[[k]]))
  agg$genes <- sapply(agg$key, function(k) paste(genes_by_key[[k]], collapse = ";"))
  collapse_sources <- function(vals) {
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) return(NA_character_)
    vals <- unlist(strsplit(vals, ";"))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) return(NA_character_)
    paste(sort(unique(vals)), collapse = ";")
  }
  unique_keys <- unique(df$key)
  sources_by_key <- sapply(unique_keys, function(k) collapse_sources(df$sources[df$key == k]), USE.NAMES = TRUE)
  agg$sources <- sources_by_key[agg$key]
  agg <- agg[order(-agg$gene_count, -agg$total_score, -agg$max_score), ]

  fetch_drug_meta <- function(drug_names, chunk = 150) {
    metas <- list()
    n <- length(drug_names)
    if (n == 0) return(metas)
    for (i in seq(1, n, by = chunk)) {
      sub <- drug_names[i:min(i + chunk - 1, n)]
      sub_arg <- paste(sprintf('\"%s\"', sub), collapse = ", ")
      q_meta <- sprintf(
        "\n        {\n          drugs(names: [%s]) {\n            nodes {\n              name\n              conceptId\n              approved\n              drugApprovalRatings { rating source { sourceDbName } }\n            }\n          }\n        }\n        ",
        sub_arg
      )
      resm <- tryCatch(run_gql(q_meta, timeout_sec = 40), error = function(e) e)
      if (!inherits(resm, "error") && resm$status == 200 && is.null(resm$errors)) {
        nd <- resm$data$drugs$nodes
        if (is.null(nd)) next
        if (is.data.frame(nd)) {
          nd <- lapply(seq_len(nrow(nd)), function(j) {
            list(
              name = nd$name[[j]],
              conceptId = nd$conceptId[[j]],
              approved = nd$approved[[j]]
            )
          })
        }
        for (d in nd) {
          metas[[length(metas) + 1]] <- data.frame(
            drug = d$name,
            drug_concept_id = d$conceptId,
            approved = d$approved,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(metas) == 0) return(NULL)
    unique(do.call(rbind, metas))
  }

  drug_meta <- fetch_drug_meta(unique(agg$drug))
  if (!is.null(drug_meta)) {
    agg <- merge(agg, drug_meta, by = c("drug", "drug_concept_id"), all.x = TRUE, sort = FALSE)
  }

  if ("approved" %in% names(agg) && isTRUE(approve)) {
    approved_rows <- !is.na(agg$approved) & agg$approved
    agg <- agg[approved_rows, ]
    cat(label, ": Filtered to approved drugs only. Remaining:", nrow(agg), "\n")
  }
  agg <- agg[order(-agg$gene_count, -agg$total_score, -agg$max_score), ]

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- file.path(out_dir, "dgidb_hits.csv")
  out_raw <- file.path(out_dir, "dgidb_raw.csv")
  write.csv(agg, out_file, row.names = FALSE)
  write.csv(df, out_raw, row.names = FALSE)

  cat("==", label, "aggregated drug hits ==\n")
  cat("Total candidate drugs:", nrow(agg), "\n")
  cat("Top 10 by gene_count/score:\n")
  print(utils::head(agg[, c("drug", "gene_count", "total_score", "max_score")], 10))
  cat("\nFiles written to:\n -", out_file, "\n -", out_raw, "\n\n")
}

#' Read genes from CSV, optionally filtered by direction
#' @keywords internal
read_genes_with_dir <- function(file, direction_value = NULL) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  if (!"gene" %in% names(df)) stop("Column 'gene' not found in ", file)
  if (!is.null(direction_value) && "direction" %in% names(df)) {
    df <- df[df$direction == direction_value, ]
  }
  unique(df$gene[!is.na(df$gene) & nzchar(df$gene)])
}

#' Build gene sets for a chosen mode
#'
#' @param mode "group" or "subtype"
#' @param base_tables Directory containing input tables
#' @param base_out Output base directory
#' @param group_filename Case-control file name (needs gene/direction)
#' @param subtype_filename Subtype file name (needs gene/direction/subtype)
#' @return List of gene sets with label/genes/out
#' @export
build_gene_sets <- function(mode = c("group", "subtype"),
                            base_tables,
                            base_out,
                            group_filename = "group.csv",
                            subtype_filename = "subtype.csv") {
  mode <- match.arg(mode)
  gene_sets <- list()

  if (mode == "group") {
    group_file <- file.path(base_tables, group_filename)
    if (file.exists(group_file)) {
      gene_sets <- append(
        gene_sets,
        list(
          list(
            label = "group_up",
            genes = read_genes_with_dir(group_file, "up"),
            out   = file.path(base_out, "dgidb_group", "up")
          ),
          list(
            label = "group_down",
            genes = read_genes_with_dir(group_file, "down"),
            out   = file.path(base_out, "dgidb_group", "down")
          )
        )
      )
    } else {
      cat("Warning: group file not found at", group_file, "- skipping group sets.\n")
    }
    return(gene_sets)
  }

  # subtype mode
  subtype_file <- file.path(base_tables, subtype_filename)
  if (!file.exists(subtype_file)) {
    cat("Warning: subtype file not found at", subtype_file, "- no subtype sets built.\n")
    return(gene_sets)
  }

  df_sub <- read.csv(subtype_file, stringsAsFactors = FALSE)
  needed_cols <- c("gene", "direction", "subtype")
  if (!all(needed_cols %in% names(df_sub))) {
    stop("Subtype file must contain columns: ", paste(needed_cols, collapse = ", "))
  }
  df_sub$direction <- tolower(df_sub$direction)
  df_sub$subtype <- as.character(df_sub$subtype)

  subtypes <- sort(unique(df_sub$subtype[!is.na(df_sub$subtype) & nzchar(df_sub$subtype)]))
  if (length(subtypes) == 0) {
    cat("Warning: no subtype values detected in", subtype_file, "\n")
    return(gene_sets)
  }

  for (st in subtypes) {
    sub_df <- df_sub[df_sub$subtype == st, ]
    for (dirv in c("up", "down")) {
      genes <- sub_df$gene[sub_df$direction == dirv]
      genes <- unique(genes[!is.na(genes) & nzchar(genes)])
      if (length(genes) == 0) next
      gene_sets[[length(gene_sets) + 1]] <- list(
        label = sprintf("subtype_%s_%s", tolower(st), dirv),
        genes = genes,
        out   = file.path(base_out, "dgidb_subtype", tolower(st), dirv)
      )
    }
  }

  gene_sets
}

#' Run a list of gene sets
#' @keywords internal
run_gene_sets <- function(gene_sets, approve = TRUE) {
  for (gs in gene_sets) {
    run_gene_set(gs$label, gs$genes, gs$out, approve = approve)
  }
}

#' Main entry point for DGIdbr package
#'
#' @param mode "group" or "subtype"
#' @param base_tables Input directory for tables
#' @param base_out Output base directory
#' @param group_filename Group file name (default group.csv)
#' @param subtype_filename Subtype file name (default subtype.csv)
#' @param approve Logical; if TRUE keep only FDA approved drugs. If FALSE, keep all.
#' @return Invisibly returns NULL
#' @export
#' @name DGIdbr
#' @examples
#' \dontrun{
#' DGIdbr(mode = "group", base_tables = ".", group_filename = "group.csv", base_out = ".")
#' }
DGIdbr <- function(mode = "group",
                   base_tables = file.path("results", "nsNMF", "diff", "Tables"),
                   base_out = file.path("results", "nsNMF", "drug", "Tables"),
                   group_filename = "group.csv",
                   subtype_filename = "subtype.csv",
                   approve = TRUE) {
  suppressPackageStartupMessages({
    library(httr)
    library(jsonlite)
  })

  gs <- build_gene_sets(
    mode = mode,
    base_tables = base_tables,
    base_out = base_out,
    group_filename = group_filename,
    subtype_filename = subtype_filename
  )
  if (length(gs) == 0) {
    cat("No gene sets found in", base_tables, "\n")
    return(invisible(NULL))
  }
  run_gene_sets(gs, approve = approve)
  cat("DGIdb run finished.\n")
  invisible(NULL)
}
