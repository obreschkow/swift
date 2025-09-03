#' Return available particle species
#'
#' @description
#' This function extracts the particle species from a custom particle list or from
#' the one stored in `swift$particles` (if no argument is provided). Only entries
#' with names matching the pattern "PartType#" are considered.
#'
#' @param container Character string pointing to the particle container `swift$particles[[container]]`.
#'
#' @return Integer vector of particle species.
#'
#' @export

availableSpecies = function(container) {

  bindSwift(particles,paste0('particles$',container))

  if (is.null(particles)) stop("particles is NULL")

  all_names = names(particles)

  if (length(all_names) == 0) stop("No entries found in particles")

  # Filter names matching "PartType" followed by digits
  valid = grepl("^PartType\\d+$", all_names)
  if (!any(valid)) stop("No valid particle species found (matching 'PartType#')")

  species_names = all_names[valid]
  species_ids = as.integer(sub("PartType", "", species_names))

  return(species_ids)
}
