#' Inspect downstream skin and asset state
#'
#' @param metadata Validated downstream application metadata.
#' @param git_state Validated downstream Git evidence.
#'
#' @return A machine-readable list of validated skin and asset evidence.
#' @noRd
inspect_downstream_app_state <- function(metadata, git_state) {
  root <- git_state$repository_root
  skin_path <- file.path(root, metadata$skin$file)
  if (!file.exists(skin_path)) {
    stop("Downstream skin file does not exist: ", metadata$skin$file,
         call. = FALSE)
  }
  run_downstream_git(
    root,
    c("ls-files", "--error-unmatch", "--", metadata$skin$file),
    description = paste0("confirm `", metadata$skin$file, "` is tracked")
  )

  default_skin_path <- file.path(root, "inst", "app", "skin-default.yml")
  if (!file.exists(default_skin_path)) {
    stop(
      "Template-owned default skin does not exist: inst/app/skin-default.yml",
      call. = FALSE
    )
  }

  asset_root <- file.path(root, "inst", "app", "www")
  skin <- load_app_skin(
    override_file = skin_path,
    default_file = default_skin_path,
    asset_root = asset_root
  )
  if (!identical(
    as.numeric(skin$schema_version),
    as.numeric(metadata$skin$schema_version)
  )) {
    stop(
      "Merged skin schema version does not match `.fluvial-app.yml`.",
      call. = FALSE
    )
  }

  customer_web_prefix <- sub(
    "^inst/app/",
    "",
    metadata$skin$customer_assets
  )
  customer_web_prefix <- paste0(sub("/+$", "", customer_web_prefix), "/")
  skin_text <- collect_downstream_skin_text(skin)
  customer_references <- unique(
    skin_text[startsWith(skin_text, customer_web_prefix)]
  )

  customer_asset_paths <- sub(
    "^www/",
    "inst/app/www/",
    customer_references
  )
  for (index in seq_along(customer_asset_paths)) {
    asset_path <- customer_asset_paths[[index]]
    path_parts <- strsplit(asset_path, "/", fixed = TRUE)[[1]]
    if (grepl("\\", asset_path, fixed = TRUE) ||
        any(path_parts %in% c(".", ".."))) {
      stop(
        "Downstream skin contains an unsafe customer asset path: ",
        customer_references[[index]],
        call. = FALSE
      )
    }
    if (!file.exists(file.path(root, asset_path))) {
      stop(
        "Downstream skin references a missing customer asset: ",
        customer_references[[index]],
        call. = FALSE
      )
    }
    run_downstream_git(
      root,
      c("ls-files", "--error-unmatch", "--", asset_path),
      description = paste0("confirm customer asset `", asset_path,
                           "` is tracked")
    )
  }

  list(
    schema_version = as.integer(skin$schema_version),
    file = metadata$skin$file,
    customer_assets = metadata$skin$customer_assets,
    referenced_customer_assets = unname(customer_asset_paths)
  )
}

#' @noRd
collect_downstream_skin_text <- function(value) {
  if (is.character(value)) {
    return(unname(value))
  }
  if (!is.list(value)) {
    return(character())
  }

  unname(unlist(
    lapply(value, collect_downstream_skin_text),
    recursive = FALSE,
    use.names = FALSE
  ))
}
