utils::globalVariables(c("id", "i.row", "oldrow"))

#' Load particle properties from another snapshot
#'
#' @description
#' Loads particle properties from another SWIFT simulation snapshot,
#' matching particles by their unique ParticleIDs to those already loaded
#' in `swift$particles`. The resulting data are stored in `swift$preparticles`
#' with exactly the same particle ordering and halo/cell associations as
#' the current snapshot, irrespective of their association in the other snapshot.
#'
#' @param snapshot Character string specifying the filename or filename
#'   pattern of the other snapshot, using a placeholder for subvolume indices;
#'   see \link{setPath} for details.
#' @param method Character string, either `halos` or `cells`, specifying
#'   which existing particle collection to match.
#' @param species Integer vector of particle types to load. If `NULL`,
#'   defaults to the species available in the current particle collection.
#' @param properties Character vector of particle properties to retrieve
#'   from the other snapshot. Defaults to `c("Masses", "Coordinates",
#'   "Velocities")`. ParticleIDs are always included. Properties absent for a given species are ignored.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @details
#' The current particles must already have been loaded using
#' \link{loadParticles}, including their `ParticleIDs` property.
#' Matching is performed independently for each particle species,
#' without using halo membership information from the other snapshot.
#'
#' Particle properties are stored in disk-backed `big.matrix` objects,
#' one per species, under `swift$preparticles[[method]]`. Their row
#' ordering is identical to that of the corresponding matrices in
#' `swift$particles[[method]]`, allowing the existing `ipart.#` and
#' `npart.#` pointers in `swift$halos` or `swift$cells` to be reused.
#'
#' Particles not found in the other snapshot have `NA` values for
#' their other properties. ParticleIDs always come from the current snapshot.
#' The numbers of matched and missing particles are recorded as `nmatched` and `nmissing` for each species.
#'
#' The other snapshot is processed subvolume by subvolume to limit
#' memory consumption. The current particle data remain unchanged.
#'
#' @return
#' Invisibly returns `NULL`. The loaded properties are stored in
#' `swift$preparticles`.
#'
#' @seealso \link{loadParticles}
#'
#' @export

loadPreParticles = function(snapshot, method, species=NULL,
                            properties=c('Masses','Coordinates','Velocities'),
                            verbose=TRUE) {

  if (verbose) cooltools::tick('Load particles in different snapshot')

  if (length(method)!=1L || !method%in%c('halos','cells')) stop('method must be "halos" or "cells"')
  current = swift$particles[[method]]
  if (is.null(current)) stop('Load current particles with loadParticles() first')
  if (is.null(swift$.paths$tmp)) stop('swift$.paths$tmp is not set')
  if (length(snapshot)!=1L || is.na(snapshot)) stop('snapshot must be a filename pattern')
  if ('Rank_bound'%in%properties) stop('Rank_bound is a membership property, not a snapshot property')
  properties = unique(setdiff(properties,'ParticleIDs'))

  available = grep('^PartType[0-9]+$',names(current),value=TRUE)
  if (is.null(species)) species = as.integer(sub('PartType','',available))
  species = unique(species)
  if (!length(species) || any(!paste0('PartType',species)%in%available))
    stop('Requested species must already be loaded in swift$particles[[method]]')

  # Resolve single-file and subvolume snapshot patterns.
  if (grepl('%d',snapshot,fixed=TRUE)) {
    pattern = sub('%d','*',snapshot,fixed=TRUE)
    files = Sys.glob(pattern)
    if (!length(files)) stop('No snapshot files match: ',pattern)
    prefix = strsplit(snapshot,'%d',fixed=TRUE)[[1]][1]
    suffix = substring(snapshot,nchar(prefix)+3L)
    numbers = substring(files,nchar(prefix)+1L,nchar(files)-nchar(suffix))
    if (any(!grepl('^[0-9]+$',numbers))) stop('Cannot identify snapshot subvolume indices')
    files = files[order(as.numeric(numbers))]
  } else {
    files = snapshot
    if (!file.exists(snapshot)) stop('Other snapshot not found: ',snapshot)
  }

  directory = file.path(swift$.paths$tmp,'preparticledata')
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  if (!dir.exists(directory)) stop('Cannot create backing directory: ',directory)

  # Stage output locally, so failed reads do not replace swift$preparticles.
  output = list()
  summary = character()
  nprogress = length(files)*length(species)
  iprogress = 0L
  for (s in species) {
    field = paste0('PartType',s)
    p = current[[field]]
    if (is.null(p$data)) {
      if (!identical(as.numeric(p$npart),0)) stop('Missing current matrix for ',field)
      output[[field]] = list(filename=NULL,npart=0,properties=character(),
                             ncolprop=integer(),colnames=character(),
                             nmatched=0,nmissing=0)
      iprogress = iprogress+length(files)
      if (verbose) cooltools::progress(sprintf('%.2f%%',100*iprogress/nprogress))
      next
    }
    idcol = match('ParticleIDs',p$colnames)
    if (is.na(idcol)) stop('Current ',field,' does not contain ParticleIDs; reload it with this property')
    ids = as.numeric(p$data[,idcol])
    if (anyNA(ids) || anyDuplicated(ids)) stop('Missing or duplicated current IDs for ',field)
    # All IDs are represented as doubles in the existing big.matrix design.
    if (any(abs(ids)>2^53)) stop('ParticleIDs above 2^53 cannot be matched exactly with double matrices')

    # Select properties available for this species, as in loadParticles().
    # Inspect the first subvolume containing the species.
    shape = NULL
    prop = character()
    for (fn in files) {
      h = hdf5r::H5File$new(fn,mode='r')
      if (h$exists(field)) {
        shape = integer()
        for (property in properties) {
          key = paste0(field,'/',property)
          if (!h$exists(key)) next
          dims = h[[key]]$dims
          if (length(dims)==1L) nc = 1L
          else if (length(dims)==2L) nc = as.integer(dims[1])
          else {
            h$close_all()
            stop('Unsupported dataset dimensions for ',key)
          }
          prop = c(prop,property)
          shape = c(shape,nc)
        }
        h$close_all()
        break
      }
      h$close_all()
    }
    if (is.null(shape)) stop('Species ',field,' not found in other snapshot')
    # Always store the current ParticleIDs as the first column.
    prop = c('ParticleIDs',prop)
    shape = c(1L,shape)
    cn = unlist(Map(function(nm,n) if (n==1L) nm else paste0(nm,'.',seq_len(n)),prop,shape),use.names=FALSE)
    base = paste0('pre_',digest::digest(list(snapshot=normalizePath(files),method=method,
                 species=s,ids=ids,properties=prop),algo='md5'),'_',s)
    # Avoid silently overwriting a live backing file.
    backing = paste0(base,'.bin')
    descriptor = paste0(base,'.txt')
    if (file.exists(file.path(directory,backing)) || file.exists(file.path(directory,descriptor))) {
      base = paste0(base,'_',format(Sys.time(),'%Y%m%d%H%M%OS6'))
      backing = paste0(base,'.bin'); descriptor = paste0(base,'.txt')
    }
    bm = bigmemory::big.matrix(nrow=length(ids),ncol=length(cn),type='double',
            backingpath=directory,backingfile=backing,descriptorfile=descriptor,
            init=NA_real_)
    bm[seq_along(ids),1L] = ids
    output[[field]] = list(filename=base,npart=length(ids),properties=prop,
                            ncolprop=shape,colnames=cn,data=bm)

    # Index current IDs only, and read other snapshot IDs one subvolume at a time.
    lookup = data.table::data.table(id=ids,row=seq_along(ids))
    data.table::setkey(lookup,id)
    matched = rep(FALSE,length(ids))
    for (fn in files) {
      h = hdf5r::H5File$new(fn,mode='r')
      if (!h$exists(paste0(field,'/ParticleIDs'))) {
        h$close_all()
        iprogress = iprogress+1L
        if (verbose) cooltools::progress(sprintf('%.2f%%',100*iprogress/nprogress))
        next
      }
      oldids = as.numeric(h[[paste0(field,'/ParticleIDs')]]$read())
      if (any(abs(oldids)>2^53,na.rm=TRUE)) {
        h$close_all(); stop('Other ParticleIDs exceed double precision')
      }
      source = data.table::data.table(id=oldids,oldrow=seq_along(oldids))
      hit = source[lookup,on='id',nomatch=0L,list(row=i.row,oldrow=oldrow)]
      if (nrow(hit)) {
        if (anyDuplicated(hit$row) || any(matched[hit$row])) {
          h$close_all(); stop('Duplicate particle IDs found across other snapshot files')
        }
        matched[hit$row] = TRUE
        sel = hit$oldrow
        dest = hit$row
        offset = 1L
        for (j in seq_along(prop)[-1L]) {
          key = paste0(field,'/',prop[j])
          if (!h$exists(key)) {h$close_all(); stop('Missing dataset ',key,' in ',fn)}
          nc = shape[j]
          if (nc==1L) {
            x = h[[key]]$read()
            bm[dest,offset+1L] = as.numeric(x[sel])
          } else {
            x = h[[key]]$read()
            if (length(dim(x))!=2L || nrow(x)!=nc) {
              h$close_all(); stop('Inconsistent dimensions for ',key)
            }
            for (k in seq_len(nc)) bm[dest,offset+k] = as.numeric(x[k,sel])
          }
          offset = offset+nc
        }
      }
      h$close_all()
      iprogress = iprogress+1L
      if (verbose) cooltools::progress(sprintf('%.2f%%',100*iprogress/nprogress))
    }
    output[[field]]$nmatched = sum(matched)
    output[[field]]$nmissing = sum(!matched)
    summary = c(summary,sprintf('%s: %s / %s matched (%.1f%%)',
                field,format(sum(matched),big.mark=',',scientific=FALSE),
                format(length(ids),big.mark=',',scientific=FALSE),
                100*sum(matched)/length(ids)))
  }
  output$addedAfterStage = if (is.null(swift$backup$stage)) 0 else swift$backup$stage
  if (is.null(swift$preparticles)) swift$preparticles = list()
  swift$preparticles[[method]] = output
  if (verbose) {
    cooltools::tock()
    cat(paste0('* ',summary,collapse='\n'),'\n',sep='')
  }
  invisible(NULL)
}
