#' Load application skin configuration
#'
#' Loads the template-owned skin defaults and merges an optional downstream
#' override over those defaults. The override can be supplied directly, through
#' `FLUVIAL_APP_SKIN_FILE`, or as a packaged `inst/app/skin.yml` file.
#'
#' @param override_file Optional path to a skin override file.
#' @param default_file Path to the complete template-owned default skin.
#'
#' @return A normalized, validated application skin list.
#' @noRd
load_app_skin <- function(
  override_file = NULL,
  default_file = app_sys("app/skin-default.yml"),
  asset_root = app_sys("app/www")
) {
  defaults <- read_app_skin_file(default_file)
  resolved_override <- resolve_app_skin_override(override_file)

  skin <- if (is.null(resolved_override)) {
    defaults
  } else {
    override <- read_app_skin_file(resolved_override)
    merge_app_skin(defaults, override)
  }

  skin <- normalize_app_skin(skin)
  validate_app_skin(skin, asset_root = asset_root)
  skin
}

#' @noRd
resolve_app_skin_override <- function(override_file = NULL) {
  if (!is.null(override_file)) {
    if (!is.character(override_file) ||
        length(override_file) != 1L ||
        !nzchar(override_file)) {
      stop("`override_file` must be NULL or one non-empty file path.",
           call. = FALSE)
    }
    if (!file.exists(override_file)) {
      stop("App skin override file does not exist: ", override_file,
           call. = FALSE)
    }
    return(override_file)
  }

  environment_override <- Sys.getenv("FLUVIAL_APP_SKIN_FILE", unset = "")
  if (nzchar(environment_override)) {
    if (!file.exists(environment_override)) {
      stop(
        "FLUVIAL_APP_SKIN_FILE does not exist: ",
        environment_override,
        call. = FALSE
      )
    }
    return(environment_override)
  }

  packaged_override <- app_sys("app/skin.yml")
  if (nzchar(packaged_override) && file.exists(packaged_override)) {
    return(packaged_override)
  }

  NULL
}

#' @noRd
read_app_skin_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    stop("App skin file path must be one non-empty string.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("App skin file does not exist: ", file, call. = FALSE)
  }

  config::get(
    config = "default",
    file = file,
    use_parent = FALSE
  )
}

#' @noRd
merge_app_skin <- function(defaults, override) {
  if (!is.list(defaults) || !is.list(override)) {
    return(override)
  }

  override_names <- names(override)
  if (is.null(override_names) || any(!nzchar(override_names))) {
    return(override)
  }

  merged <- defaults
  for (name in override_names) {
    if (name %in% names(defaults)) {
      merged[[name]] <- merge_app_skin(defaults[[name]], override[[name]])
    } else {
      merged[[name]] <- override[[name]]
    }
  }
  merged
}

#' @noRd
normalize_app_skin <- function(skin) {
  instruction_paths <- list(
    c("workflow", "draw_xs", "instructions"),
    c("workflow", "draw_flowline", "instructions")
  )

  for (path in instruction_paths) {
    parent <- skin[[path[[1]]]]
    if (!is.list(parent)) {
      next
    }
    section <- parent[[path[[2]]]]
    if (!is.list(section) || !(path[[3]] %in% names(section))) {
      next
    }
    instructions <- section[[path[[3]]]]
    if (is.list(instructions)) {
      skin[[path[[1]]]][[path[[2]]]][[path[[3]]]] <-
        unlist(instructions, recursive = FALSE, use.names = FALSE)
    }
  }

  skin
}

#' @noRd
validate_app_skin <- function(skin, asset_root = app_sys("app/www")) {
  assert_skin_fields(
    skin,
    path = "skin",
    required = c("schema_version", "identity", "theme", "workflow")
  )
  if (!is.numeric(skin$schema_version) ||
      length(skin$schema_version) != 1L ||
      skin$schema_version != 1) {
    stop("`skin.schema_version` must be the supported value 1.", call. = FALSE)
  }

  assert_skin_fields(
    skin$identity,
    path = "skin.identity",
    required = c("app_title", "browser_title", "favicon")
  )
  assert_skin_text(skin$identity$app_title, "skin.identity.app_title")
  assert_skin_text(skin$identity$browser_title, "skin.identity.browser_title")
  assert_skin_text(skin$identity$favicon, "skin.identity.favicon")
  validate_skin_asset(
    skin$identity$favicon,
    "skin.identity.favicon",
    asset_root = asset_root
  )

  assert_skin_fields(
    skin$theme,
    path = "skin.theme",
    required = c("bootswatch", "version")
  )
  assert_skin_text(skin$theme$bootswatch, "skin.theme.bootswatch")
  if (!is.numeric(skin$theme$version) ||
      length(skin$theme$version) != 1L ||
      !(skin$theme$version %in% c(3, 4, 5))) {
    stop("`skin.theme.version` must be one of 3, 4, or 5.", call. = FALSE)
  }

  assert_skin_fields(
    skin$workflow,
    path = "skin.workflow",
    required = c("draw_xs", "draw_flowline", "results")
  )
  validate_workflow_skin(
    skin$workflow$draw_xs,
    "skin.workflow.draw_xs",
    needs_guidance = TRUE
  )
  validate_workflow_skin(
    skin$workflow$draw_flowline,
    "skin.workflow.draw_flowline",
    needs_guidance = TRUE
  )
  validate_workflow_skin(
    skin$workflow$results,
    "skin.workflow.results",
    needs_guidance = FALSE
  )

  invisible(skin)
}

#' @noRd
validate_workflow_skin <- function(section, path, needs_guidance) {
  required <- "nav_label"
  if (needs_guidance) {
    required <- c(
      required,
      "sidebar_title",
      "instructions",
      "next_button",
      "progress_message"
    )
  }
  assert_skin_fields(section, path = path, required = required)

  text_fields <- setdiff(required, "instructions")
  for (field in text_fields) {
    assert_skin_text(section[[field]], paste(path, field, sep = "."))
  }

  if (needs_guidance) {
    instructions <- section$instructions
    if (!is.character(instructions) ||
        length(instructions) < 1L ||
        anyNA(instructions) ||
        any(!nzchar(instructions))) {
      stop("`", path, ".instructions` must contain non-empty text items.",
           call. = FALSE)
    }
  }

  invisible(section)
}

#' @noRd
assert_skin_fields <- function(value, path, required) {
  if (!is.list(value) || is.null(names(value))) {
    stop("`", path, "` must be a named mapping.", call. = FALSE)
  }

  missing <- setdiff(required, names(value))
  if (length(missing) > 0L) {
    stop(
      "`", path, "` is missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  unknown <- setdiff(names(value), required)
  if (length(unknown) > 0L) {
    stop(
      "`", path, "` contains unknown field(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(value)
}

#' @noRd
assert_skin_text <- function(value, path) {
  if (!is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)) {
    stop("`", path, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(value)
}

#' @noRd
validate_skin_asset <- function(
  value,
  path,
  asset_root = app_sys("app/www")
) {
  if (!startsWith(value, "www/")) {
    stop("`", path, "` must reference an asset below `www/`.", call. = FALSE)
  }

  relative_path <- sub("^www/", "", value)
  path_parts <- strsplit(relative_path, "/", fixed = TRUE)[[1]]
  if (!nzchar(relative_path) ||
      grepl("\\", relative_path, fixed = TRUE) ||
      any(path_parts %in% c(".", ".."))) {
    stop("`", path, "` must be a safe relative `www/` asset path.",
         call. = FALSE)
  }

  asset_path <- file.path(asset_root, relative_path)
  if (!nzchar(asset_root) || !file.exists(asset_path)) {
    stop("`", path, "` references a missing packaged asset: ", value,
         call. = FALSE)
  }

  invisible(value)
}
