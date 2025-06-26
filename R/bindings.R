bindSwiftObject <- function(name, env = parent.frame()) {
  makeActiveBinding(name, function(v) {
    if (missing(v)) {
      .internal_storage[[name]]
    } else {
      .internal_storage[[name]] <- v
    }
  }, env = env)
}

# Convenience wrappers
bindPaths     <- function(env = parent.frame()) bindSwiftObject("paths", env)
bindHalos     <- function(env = parent.frame()) bindSwiftObject("halos", env)
bindParticles <- function(env = parent.frame()) bindSwiftObject("particles", env)
