#' Concatenate identical properties of different Swift/Gadget particle species
#'
#' @description Concatenate identical properties of different particle species in a Swift/Gadget snapshot
#'
#' @param dat a list containing Swift/Gadget particle data. It must have at least one sublist PartType# (with #=0,1,...). Each sublist PartType# represents one type of particle (e.g. gas, stars, dark matter) and must contain at least the particles coordinates in an N-by-3 matrix \code{Coordinates}. Other optional elements of PartType# are:\cr
#' @param field character specifying particle property to concatenate.
#' @param species vector of particle species to be included.
#'
#' @return Returns an N-vector or an N-by-3 matrix representing the property "field" of all the requested particles.
#'
#' @author Danail Obreschkow
#'
#' @export

allPart = function(dat, field = 'Coordinates', species = seq(0,5)) {

  # determine required space
  n.valid.species = 0
  d = 0
  if (field%in%c('Coordinates','Velocities')) d = 2
  for (i in species) {
    group = sprintf('PartType%d',i)
    if (!is.null(dat[[group]])) {
      if (!is.null(dat[[group]][[field]])) {
        n.valid.species = n.valid.species+1
        dm = dim(dat[[group]][[field]])
        d = max(d,length(dm))
      } else {
        stop(sprintf('Field %s does not exist for species %d.\n',field,i))
      }
    }
  }
  if (n.valid.species==0) stop(sprintf('Field %s not found in considered species.\n',field))

  # check data size
  if (d!=0 & d!=1 & d!=2) stop('unknown data format')

  # write data into output array
  x = NULL
  for (i in species) {
    group = sprintf('PartType%d',i)
    if (!is.null(dat[[group]])) {
      if (!is.null(dat[[group]][[field]])) {
        if (d==2) {
          if (is.null(x)) {
            x = dat[[group]][[field]]
          } else {
            x = rbind(x,dat[[group]][[field]])
          }
        } else {
          if (is.null(x)) {
            x = dat[[group]][[field]]
          } else {
            x = c(x,dat[[group]][[field]])
          }
        }
      } else {
        stop(sprintf('Field %s does not exist for species %d.\n',field,i))
      }
    }
  }

  return(x)
}
