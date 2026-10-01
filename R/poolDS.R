poolDSOLDVERSION <- function(res,
                   poolFuns = list(
                     MeanDS = "poolMeanDS",  # attention, lhs starts with capital letter!!!!
                     VarDS  = "poolVarDS",
                     QuantileMeanDS = "poolQuantileMeanDS",
                     NumNaDS = "poolNumNaDS",
                     HistogramDS1 = "poolHistogramDS1",
                     HistogramDS2 = "poolHistogramDS2",
                     Table1DDS = "poolTable1DDS",
                     IsValidDS = "poolIsValidDS"
                   )) {

  stopifnot(is.list(res), length(res) > 0)

  resolve_fun <- function(f) {
    if (is.function(f)) return(f)
    get(f, mode = "function", inherits = TRUE)
  }

  is_leaf_result <- function(x) {
    is.list(x) && any(names(x) %in% c(
      "EstimatedMean", "Variance", "Sum", "SumOfSquares",
      "Nvalid", "Nmissing", "Ntotal", "ValidityMessage"
    ))
  }

  pool_one_method <- function(method_name, pool_fun) {
    server_blocks <- lapply(res, `[[`, method_name)
    server_blocks <- server_blocks[!vapply(server_blocks, is.null, logical(1))]

    if (length(server_blocks) == 0) {
      return(NULL)
    }

    ## Unstratified case:
    ## res$server1$MeanDS$EstimatedMean, etc.
    if (all(vapply(server_blocks, is_leaf_result, logical(1)))) {
      return(pool_fun(server_blocks))
    }

    ## Stratified case:
    ## res$server1$MeanDS$`pooled$GENDER.1`, etc.
    strata <- unique(unlist(lapply(server_blocks, names), use.names = FALSE))

    pooled_strata <- lapply(strata, function(stratum) {
      x <- lapply(server_blocks, `[[`, stratum)
      x <- x[!vapply(x, is.null, logical(1))]
      pool_fun(x)
    })

    names(pooled_strata) <- strata
    pooled_strata
  }

  out <- lapply(names(poolFuns), function(method_name) {
    pool_fun <- resolve_fun(poolFuns[[method_name]])
    pool_one_method(method_name, pool_fun)
  })

  names(out) <- names(poolFuns)
  list(pooled = out[!vapply(out, is.null, logical(1))])
}


###############
# neue version
###############

# poolDS <- function(
#     res,
#     poolFuns = list(
#       MeanDS         = "poolMeanDS",
#       VarDS          = "poolVarDS",
#       QuantileMeanDS = "poolQuantileMeanDS",
#       NumNaDS        = "poolBySumDS",
#       HistogramDS1   = "poolHistogramDS1",
#       HistogramDS2   = "poolHistogramDS2",
#       Table1DDS      = "poolTable1DDS",
#       IsValidDS      = "poolIsValidDS",
#       Length         = "poolBySumDS"
#     ),
#     extras = list(
#       poolQuantileMeanDS = c("Length", "NumNaDS")
#     )
# ) {
#
#   stopifnot(is.list(res), length(res) > 0L)
#
#   resolve_fun <- function(f) {
#     if (is.function(f)) return(f)
#     get(f, mode = "function", inherits = TRUE)
#   }
#
#   fun_label <- function(f) {
#     if (is.character(f)) f else deparse(substitute(f))
#   }
#
#   is_leaf_result <- function(x) {
#     !is.list(x) ||
#       any(names(x) %in% c(
#         "EstimatedMean", "Variance", "Sum", "SumOfSquares",
#         "Nvalid", "Nmissing", "Ntotal", "ValidityMessage"
#       ))
#   }
#
#   get_server_method <- function(server, method) {
#     res[[server]][[method]]
#   }
#
#   get_strata <- function(method) {
#     blocks <- lapply(names(res), get_server_method, method = method)
#     blocks <- blocks[!vapply(blocks, is.null, logical(1))]
#
#     if (length(blocks) == 0L) return(character(0))
#
#     if (all(vapply(blocks, is_leaf_result, logical(1)))) {
#       return(NA_character_)
#     }
#
#     unique(unlist(lapply(blocks, names), use.names = FALSE))
#   }
#
#   collect_values <- function(method, stratum = NA_character_) {
#     vals <- lapply(names(res), function(server) {
#       x <- res[[server]][[method]]
#       if (is.null(x)) return(NULL)
#
#       if (is.na(stratum)) {
#         x
#       } else {
#         x[[stratum]]
#       }
#     })
#
#     names(vals) <- names(res)
#     vals[!vapply(vals, is.null, logical(1))]
#   }
#
#   collect_extras <- function(extra_methods, stratum = NA_character_) {
#     out <- lapply(extra_methods, function(method) {
#       collect_values(method, stratum)
#     })
#
#     names(out) <- extra_methods
#     out
#   }
#
#   pool_one <- function(method, pool_fun_spec) {
#     pool_fun <- resolve_fun(pool_fun_spec)
#
#     pool_fun_name <- if (is.character(pool_fun_spec)) {
#       pool_fun_spec
#     } else {
#       names(extras)[vapply(extras, identical, logical(1), pool_fun_spec)][1]
#     }
#
#     extra_methods <- extras[[pool_fun_name]]
#
#     strata <- get_strata(method)
#     if (length(strata) == 0L) return(NULL)
#
#     out <- lapply(strata, function(stratum) {
#       vals <- collect_values(method, stratum)
#
#       if (length(vals) == 0L) return(NULL)
#
#       if (!is.null(extra_methods)) {
#         ex <- collect_extras(extra_methods, stratum)
#         pool_fun(vals, extras = ex)
#       } else {
#         pool_fun(vals)
#       }
#     })
#
#     if (length(strata) == 1L && is.na(strata)) {
#       out[[1L]]
#     } else {
#       names(out) <- strata
#       out
#     }
#   }
#
#   out <- lapply(names(poolFuns), function(method) {
#     pool_one(method, poolFuns[[method]])
#   })
#
#   names(out) <- names(poolFuns)
#   list(pooled = out[!vapply(out, is.null, logical(1))])
# }
#
#

poolDS <- function(
    res,
    poolFuns = list(
      MeanDS         = "poolMeanDS",
      VarDS          = "poolVarDS",
      QuantileMeanDS = "poolQuantileMeanDS",
      NumNaDS        = "poolBySumDS",
      HistogramDS1   = "poolHistogramDS1",
      HistogramDS2   = "poolHistogramDS2",
      Table1DDS      = "poolTable1DDS",
      IsValidDS      = "poolIsValidDS",
      Length         = "poolBySumDS",
      ClassDS        = "poolByUniqueDS"
    ),
    extras = list(
      poolQuantileMeanDS = c("Length", "NumNaDS")
    )
) {
  stopifnot(is.list(res), length(res) > 0L)

  resolve_fun <- function(f) {
    if (is.function(f)) return(f)
    get(f, mode = "function", inherits = TRUE)
  }

  fun_label <- function(f) {
    if (is.character(f)) f else deparse(substitute(f))
  }

  is_leaf_result <- function(x) {
    !is.list(x) ||
      any(names(x) %in% c(
        "EstimatedMean", "Variance", "Sum", "SumOfSquares",
        "Nvalid", "Nmissing", "Ntotal", "ValidityMessage"
      ))
  }

  get_server_method <- function(server, method) {
    res[[server]][[method]]
  }

  get_strata <- function(method) {
    blocks <- lapply(names(res), get_server_method, method = method)
    blocks <- blocks[!vapply(blocks, is.null, logical(1))]

    if (length(blocks) == 0L) return(character(0))

    if (all(vapply(blocks, is_leaf_result, logical(1)))) {
      return(NA_character_)
    }

    #unique(unlist(lapply(blocks, names), use.names = FALSE))
    block_names <- lapply(blocks, dimnames)

    strata_names <- lapply(block_names, names) |> unlist() |> unique()

    unique_block_names <- lapply(strata_names, function(nm) {
      unique(unlist(lapply(block_names, `[[`, nm), use.names = FALSE))
    })

    names(unique_block_names) <- strata_names

    unique_block_names
  }

  collect_values <- function(method, stratum = NA_character_) {
    vals <- lapply(names(res), function(server) {
      x <- res[[server]][[method]]
      if (is.null(x)) return(NULL)

      if (is.na(stratum)) {
        x
      } else {
        x[[stratum]]
      }
    })

    names(vals) <- names(res)
    vals[!vapply(vals, is.null, logical(1))]
  }
  collect_val <- function(method, stratum) {
    vals <- lapply(names(res), function(server) {
      x <- res[[server]][[method]]

      if (is.null(x)) return(NULL)

      if (is.na(stratum)) {
        return(x)
        #idx <- 1L
      } else {
        idx <- stratum #unlist(strata_combos_str[i,,drop=F], use.names = FALSE)
      }

     # if (!is.na(stratum)) { # if there are strata (data grouped)
        # check if parameter combination in the results from current server
        valid <- Map(`%in%`, idx, dimnames(x))
        if (!all(unlist(valid))) return(NULL)
     # }

      #if (length(strata)==1 && length(strata[[1]])==1 && is.na(strata)) { # check this for unstratified case!!!
      #  x
      #} else {
      #  # res$server1$MeanDS[1,1,1]
      #  # res$server1$MeanDS["pooled$country.3", "pooled$sex.1", "pooled$bmi_cat_all_T0.1"]
      #  do.call(`[`, list(x, unlist(strata_combos_str[i,,drop=F], use.names = FALSE) ))
      #  #x[[strata]]
      #}

      rs <- do.call(`[`, c(list(x), idx ))
      #rs <- do.call(`[[`, list(x, idx )) # funktioniert nicht wenn element ein array ist (mehrere gruppenvariablen)
      # würde nicht funktionieren wenn faktorstufen fehlen: do.call(`[[`, list(x, i ))

      #if ( is.null(rs)  ) {#if ( length(rs)==1 && is.null(rs[[1]])  ) {
      #  rs <- NULL
      #}

      # flatten the list
      if (is.list(rs) && length(rs)==1) rs <- rs[[1]]

      rs
    })

    names(vals) <- names(res)
    vals[!vapply(vals, is.null, logical(1))]
  }

  collect_extras <- function(extra_methods, stratum = NA_character_) {
    out <- lapply(extra_methods, function(method) {
      #collect_values(method, stratum)
      collect_val(method, stratum)
    })

    names(out) <- extra_methods

    check <- sapply(out, function(x) {if (is.list(x) && length(x)==0) {return(FALSE)} else {return(TRUE)} })
    if (!all(check)) {
      warning(paste0("The following is missing, but needed for pooling: ", paste0(names(which(!check))) ,collapse=", ")  )
    }
    out
  }

  pool_one <- function(method, pool_fun_spec) {
    pool_fun <- resolve_fun(pool_fun_spec)

    pool_fun_name <- if (is.character(pool_fun_spec)) {
      pool_fun_spec
    } else {
      names(extras)[vapply(extras, identical, logical(1), pool_fun_spec)][1]
    }

    extra_methods <- extras[[pool_fun_name]]

    strata <- get_strata(method)
    if (length(strata) == 0L) return(NULL)

    if (length(strata) == 1L) { #(and:  && is.na(strata))
      strata_combos <- matrix(NA)
      strata_combos_str <- lapply(strata_combos, as.character) |> as.data.frame()
    } else {
      strata_combos <- do.call(expand.grid, strata)
      strata_combos_str <- lapply(strata_combos, as.character) |> as.data.frame()
    }

    # dummy version with for loop

    # collects values for i-th group combination from strata_combos_str
    out <- lapply(1:nrow(strata_combos), function(i) {
      strata <- unlist(strata_combos_str[i,,drop=F], use.names = FALSE)
      # collect values
      vals <- collect_val(method, strata)
      # replace errors with NA (use ValidityMessage only when it exists)
      #vals_ok <-  sapply(vals, function(x) {x$ValidityMessage == "VALID ANALYSIS"})
      vals_ok <- sapply(vals, function(x) {
        !is.list(x) || is.null(x$ValidityMessage) || x$ValidityMessage == "VALID ANALYSIS"
      })
      vals_any_ok <- sapply(vals_ok, function(x) any(x))
      vals_all_ok <- sapply(vals_ok, function(x) all(x))
      vals[!vals_all_ok] <- NA #vals[!vals_any_ok] <- NA is not enough for the current pooling function

      if (length(vals) == 0L) return(NULL)

      if (!is.null(extra_methods)) {
        ex <- collect_extras(extra_methods, strata)
        pool_fun(vals, extras = ex)
      } else {
        pool_fun(vals)
      }
    })

    # in baumstruktur erneut einbinden???
    set_in_tree <- function(tree, path, value) {
      if (length(path) == 0) {
        return(value)
      }

      nm <- path[1]

      if (length(path) == 1) {
        tree[[nm]] <- value
      } else {
        if (is.null(tree[[nm]]) || !is.list(tree[[nm]])) {
          tree[[nm]] <- list()
        }

        tree[[nm]] <- set_in_tree(tree[[nm]], path[-1], value)
      }

      tree
    }
    ####
    if (length(strata_combos_str)==1 && is.na(strata_combos_str)) {
      out <- unlist(out , recursive=F)
      out <- unlist(out )
      if (is.list(out)) stop("WTF")
      return(  out  )
    } else {
    out_tree <- list()
    for (i in 1:nrow(strata_combos_str)) {
      out_tree <- set_in_tree(out_tree, unlist(strata_combos_str[i,,drop=F]), out[[i]])
    }
    #if (length(strata) == 1L && is.na(strata)) {
    #  out[[1L]]
    #} else {
    #  names(out) <- strata
    #  out
    #}
    out_tree
    }

  }

  #out <- lapply(names(poolFuns), function(method) {
  #  pool_one(method, poolFuns[[method]])
  #})
  out <- lapply(names(poolFuns), function(method) {
    tryCatch(
      pool_one(method, poolFuns[[method]]),
      error = function(e) {
        warning(
          sprintf("pool_one failed for method '%s': %s", method, conditionMessage(e)),
          call. = FALSE
        )
        NULL
      }
    )
  })

  names(out) <- names(poolFuns)
  list(pooled = out[!vapply(out, is.null, logical(1))])
}
