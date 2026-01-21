source("renv/activate.R")

# Auto-start httpgd for browser-based plotting
if (interactive()) {
  if (requireNamespace("httpgd", quietly = TRUE)) {
    httpgd::hgd()
    cat("\n")
  }
}
