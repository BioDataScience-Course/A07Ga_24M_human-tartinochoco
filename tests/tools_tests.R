# Functions to test projects
# Copyright (c) 2023, Philippe Grosjean (phgrosjean@sciviews.org) &
#   Guyliann Engels (Guyliann.Engels@umons.ac.be)
# Version 2.1.0 (use.data = TRUE/FALSE added for chart() objects)

suppressMessages(library(learnitgrid))

# More functions and code ------------------------------------------------

cache_data <- learnitgrid::copy_cache()

# In learnitgrid 0.9.0 CC is indeed copy_cache() and there is not clane_cache()
CC <- clean_cache <- function(only_if_copied = FALSE, cache_data = cache_data) {
  if (isTRUE(only_if_copied) && is.null(cache_data)) # Nothing copied, so do not delete
    return()
  if (fs::dir_exists("data/data_cache/"))
    fs::dir_delete("data/data_cache/")
}

sddReporter <- learnitgrid::project_reporter()
learnitgrid::hook_last_chunk()

# A hook to save results after a code chunk is evaluated
knitr::knit_hooks$set(record = function(before, options, envir) {
  if (!before) {
    fun_name <- options$record
    fun <- get(fun_name, mode = "function", envir = envir)
    object <- options$object
    if (is.null(object)) {
      # If the function name starts with RN, we use .Last.chunk
      # otherwise, we use same name as label
      if (substring(fun_name, 1L, 2L) == "RN") {
        object <- ".Last.chunk"
      } else {
        object <- options$label
      }
      cat(fun_name, "('", options$label, "')\n", sep = "")
    } else {
      object <- options$object
      cat(fun_name, "('", object, "', '", options$label, "')\n", sep = "")
    }
    procfun <- options$fun
    if (!is.null(procfun) & !is.function(procfun))
      procfun <- get(procfun, mode = "function", envir = envir)
    arg <- options$arg
    if (is.null(arg)) {
      if (is.null(procfun)) {
        fun(object = object, name = options$label, env = envir)
      } else {
        fun(object = object, name = options$label,
          fun = procfun, env = envir)
      }
    } else {# There is an extra argument
      if (is.null(procfun)) {
        fun(object = object, name = options$label, arg, env = envir)
      } else {
        fun(object = object, name = options$label, arg,
          fun = procfun, env = envir)
      }
    }
    NULL
  }
})

# Test ggplot or chart plots using ggcheck
chart_structure <- function(object, arg = "", ...) {
  list(
    n_layers = ggcheck::n_layers(object),
    data = if (grepl("no.data", arg, fixed = TRUE)) {
      NULL # Data not considered, only structure of the plot
    } else {
      learnitgrid::digest(ggcheck::get_data(object))
    },
    labels = ggcheck::get_labels(object),
    geoms = ggcheck::get_geoms(object),
    stats = ggcheck::get_stats(object),
    coords = ggcheck::get_coordinate_system(object),
    mapping = ggcheck::get_mappings(object)
  )
}

ROCS <- function(object_name = ".Last.chunk", arg = "", name = object_name,
  fun = chart_structure, ..., env = parent.frame())
  learnitgrid::record_res(object_name = object_name, name = name, fun = fun,
    arg = arg, ..., env = env)

RNCS <- function(name, arg = "", object_name = ".Last.chunk", fun = chart_structure,
  ..., env = parent.frame())
  learnitgrid::record_res(object_name = object_name, name = name, fun = fun,
    arg = arg, ..., env = env)
