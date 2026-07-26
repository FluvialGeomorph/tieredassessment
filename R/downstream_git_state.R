#' Inspect downstream Git state
#'
#' @param metadata Validated downstream application metadata.
#' @param repository Path within the downstream Git repository.
#' @param metadata_file Path to the validated metadata file.
#'
#' @return A machine-readable list of validated local Git evidence.
#' @noRd
inspect_downstream_git_state <- function(
  metadata,
  repository = ".",
  metadata_file = ".fluvial-app.yml"
) {
  assert_downstream_text(repository, "repository")
  if (!dir.exists(repository)) {
    stop("Downstream repository directory does not exist: ", repository,
         call. = FALSE)
  }

  root <- run_downstream_git(
    repository,
    c("rev-parse", "--show-toplevel"),
    description = "locate the downstream repository"
  )
  if (length(root) != 1L || !nzchar(root)) {
    stop("Git did not return one downstream repository root.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  expected_metadata_file <- file.path(root, ".fluvial-app.yml")
  if (!file.exists(expected_metadata_file) ||
      !identical(
        normalizePath(metadata_file, winslash = "/", mustWork = TRUE),
        normalizePath(expected_metadata_file, winslash = "/", mustWork = TRUE)
      )) {
    stop(
      "Validated metadata must be the repository-root `.fluvial-app.yml`.",
      call. = FALSE
    )
  }
  run_downstream_git(
    root,
    c("ls-files", "--error-unmatch", "--", ".fluvial-app.yml"),
    description = "confirm `.fluvial-app.yml` is tracked"
  )

  dirty <- run_downstream_git(
    root,
    c("status", "--porcelain", "--untracked-files=normal"),
    description = "inspect the downstream working tree"
  )
  if (length(dirty) > 0L) {
    stop(
      "Downstream repository must have a clean working tree; found: ",
      paste(dirty, collapse = ", "),
      call. = FALSE
    )
  }

  remote_name <- metadata$upstream$remote
  remote_url <- run_downstream_git(
    root,
    c("remote", "get-url", remote_name),
    description = paste0("read the `", remote_name, "` remote")
  )
  if (length(remote_url) != 1L ||
      !identical(remote_url, metadata$upstream$repository)) {
    stop(
      "Git remote `", remote_name, "` must resolve to `",
      metadata$upstream$repository, "`.",
      call. = FALSE
    )
  }
  upstream_push_url <- run_downstream_git(
    root,
    c("remote", "get-url", "--push", remote_name),
    description = paste0("read the `", remote_name, "` push URL")
  )
  if (length(upstream_push_url) != 1L ||
      !identical(upstream_push_url, "DISABLED")) {
    stop(
      "Git remote `", remote_name,
      "` must use the protected push URL `DISABLED`.",
      call. = FALSE
    )
  }

  origin_url <- run_downstream_git(
    root,
    c("remote", "get-url", "origin"),
    description = "read the customer `origin` remote"
  )
  if (length(origin_url) != 1L ||
      !nzchar(origin_url) ||
      identical(origin_url, metadata$upstream$repository)) {
    stop(
      "Git remote `origin` must resolve to the customer repository, not ",
      "canonical `ohwm2`.",
      call. = FALSE
    )
  }
  origin_push_url <- run_downstream_git(
    root,
    c("remote", "get-url", "--push", "origin"),
    description = "read the customer `origin` push URL"
  )
  if (length(origin_push_url) != 1L ||
      !identical(origin_push_url, origin_url)) {
    stop(
      "Git remote `origin` must use the same reviewed URL for fetch and push.",
      call. = FALSE
    )
  }
  push_default <- run_downstream_git(
    root,
    c("config", "--get", "remote.pushDefault"),
    description = "read Git setting `remote.pushDefault`"
  )
  if (length(push_default) != 1L || !identical(push_default, "origin")) {
    stop(
      "Git setting `remote.pushDefault` must be `origin`.",
      call. = FALSE
    )
  }

  release_ref <- paste0("refs/tags/", metadata$upstream$release)
  run_downstream_git(
    root,
    c("check-ref-format", release_ref),
    description = "validate the recorded upstream release tag"
  )
  release_commit <- run_downstream_git(
    root,
    c("rev-parse", "--verify", paste0(release_ref, "^{commit}")),
    description = paste0("resolve upstream release `",
                         metadata$upstream$release, "`")
  )
  if (length(release_commit) != 1L ||
      !grepl("^[0-9a-f]{40,64}$", release_commit)) {
    stop("Git did not resolve the upstream release to one commit.",
         call. = FALSE)
  }

  head <- run_downstream_git(
    root,
    c("rev-parse", "--verify", "HEAD^{commit}"),
    description = "resolve downstream HEAD"
  )
  if (length(head) != 1L || !grepl("^[0-9a-f]{40,64}$", head)) {
    stop("Git did not resolve downstream HEAD to one commit.", call. = FALSE)
  }

  ancestry <- run_downstream_git(
    root,
    c("merge-base", "--is-ancestor", release_commit, head),
    description = paste0(
      "prove upstream release `",
      metadata$upstream$release,
      "` is an ancestor of downstream HEAD"
    ),
    accepted_status = c(0L, 1L)
  )
  if (!identical(attr(ancestry, "git_status"), 0L)) {
    stop(
      "Upstream release `", metadata$upstream$release,
      "` is not an ancestor of downstream HEAD.",
      call. = FALSE
    )
  }

  changed_paths <- run_downstream_git(
    root,
    c(
      "diff",
      "--name-only",
      "--no-renames",
      paste0(release_commit, "..", head),
      "--"
    ),
    description = "inspect downstream divergence"
  )
  unexpected_paths <- changed_paths[
    !vapply(
      changed_paths,
      is_downstream_owned_path,
      logical(1),
      metadata = metadata
    )
  ]
  if (length(unexpected_paths) > 0L) {
    stop(
      "Downstream changes extend outside owned paths: ",
      paste(unexpected_paths, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    schema_version = 1L,
    application_id = metadata$application_id,
    repository_root = root,
    head = head,
    origin = list(
      remote = "origin",
      repository = origin_url,
      push_repository = origin_push_url
    ),
    upstream = list(
      remote = remote_name,
      repository = remote_url,
      push_repository = upstream_push_url,
      release = metadata$upstream$release,
      release_commit = release_commit
    ),
    changed_paths = unname(changed_paths)
  )
}

#' @noRd
is_downstream_owned_path <- function(path, metadata) {
  exact_paths <- c(
    ".fluvial-app.yml",
    metadata$skin$file,
    metadata$deployment$manifest
  )
  asset_prefix <- paste0(sub("/+$", "", metadata$skin$customer_assets), "/")

  path %in% exact_paths || startsWith(path, asset_prefix)
}

#' @noRd
run_downstream_git <- function(
  repository,
  arguments,
  description,
  accepted_status = 0L
) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    stop("Git is required to ", description, ".", call. = FALSE)
  }

  output <- suppressWarnings(
    system2(
      git,
      args = c("-C", shQuote(repository), arguments),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  status <- as.integer(status)

  if (!(status %in% accepted_status)) {
    detail <- paste(output, collapse = "\n")
    if (!nzchar(detail)) {
      detail <- paste0("Git exited with status ", status, ".")
    }
    stop(
      "Unable to ", description, ": ", detail,
      call. = FALSE
    )
  }

  output <- unname(output)
  if (length(accepted_status) > 1L) {
    attr(output, "git_status") <- status
  }
  output
}
