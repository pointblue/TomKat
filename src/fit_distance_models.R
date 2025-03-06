#' Fit detection curves to formatted data
#'
#' @param data Dataframe formatted for use with package \code{Distance}, e.g. output from \code{format_data}
#' @param spec Species name or code to subset \code{data}
#' @param maxdist Maximum distance (in meters) to include in analysis, i.e. truncation distance
#' @param bins Vector of cutpoints for binned detections. See details.
#' @param modlist List of detection curves to include in analysis. See details.
#' @param debug Debug level to pass to \code{Distance}. See details.
#'
#' @details This function will first subset a formatted point count data set (i.e. output from \code{format_data}) to a
#' desired species and fit a standard list of detection functions to the data using detection bins. An AIC table
#' comparing detection function fits, including Akaike weight (w), whether fit was monotonic (mono), and the p-value
#' from a chi-square goodness-of-fit test (chisq) will then be created and printed. Any models that are monotonic and
#' have a chisq p-value>0.05 will be considered to have "good fit", and marked to be further considered (include = *).
#' Of the included models, the function will plot the results of the one with the lowest AIC score and print density
#' estimates by strata (individuals/ha). Take caution in accepting the goodness-of-fit test results, since the test
#' weights all bins equally even though the first bins are the most important. Check the plot for good agreement between
#' observed and predicted detections. It can be easy to pass the chi-square test with small sample sizes and poor
#' agreement, yet difficult to pass with large sample sizes and very good agreement. If agreement is poor, or none of
#' the models pass the chi-square test, consider lumping bins or reducing the maximum (truncation) distance.
#'
#' The value for \code{bins} defaults to the cutpoints for CADC protocol VCP25, bins = c(0,10,20,30,40,50,75,100), but
#' cutpoints can be changed for other protocols and/or dropped to lump bins together as needed. Take care in changing
#' cutpoints so that they align with the bins originally used to collect the data, particularly when combining data
#' using more than one protocol. The function will not check whether your bins make sense for your data!
#'
#' The default list of detection functions to fit were originally derived from Thomas et al. 2010 J Appl Ecol 47:5-14.
#' These include:
#' - Uniform key with cosine adjustments (unif.cos)
#' - Half-normal key with no or cosine adjustments (hn.cos)
#' - Hazard-rate key with no or polynomial adjustments (hr.poly)
#' These model names are passed to package \code{Distance}, which produces copious output as each model is running.
#' \code{Distance} will start with no adjustment terms (or the simplest adjustments allowed) and then automatically
#' tries higher order adjustments until it finds one with a higher AIC (poorer fit) than the one before it, and returns
#' the previous model with the lowest AIC. Occasionally, individual models will not fit and will produce an error
#' message, but those will be skipped and the other models will continue to run.
#'
#' Debug level defaults to 0 (none), but can be set to values 1-3 for increasing levels of debugging output.
#'
#' @return Returns a named list including:
#'
#' - The output of each model in \code{modlist}
#'
#' - \code{table}, containing the table of results printed
#'
#' - \code{D}, the density estimates (individuals/ha) for each stratum, calculated from the model with the lowest AIC
#' score (of the models with include = *, see Details)
#'
#' - \code{N}, the estimated total number of individuals per stratum, calculated from the same model as for \code{D}
#' above, based on extrapolating \code{D} by the \code{Area} of each stratum specified in \code{data}. If \code{Area}=1,
#' then \code{N} = \code{D}.
#'
#' @author Kristen Dybala, \email{kdybala@@pointblue.org}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{fdat = format.data(data=dat, strata='group')}

fit_distance_models = function(data, spec, maxdist=100, bins=c(0,10,20,30,40,50,75,100),
                               modlist=c('unif.cos','hn.cos','hr.poly'), debug=0) {
  distance = species = NULL
  
  ## list of unique surveys
  data$surveyID = paste(data$Region.Label, data$Sample.Label, sep='-')
  blank <- data[-which(duplicated(data$surveyID)),
                c('Region.Label','Area','Sample.Label','Effort','count','distance','protocol','surveyID')]
  blank$distance = NA
  blank$count = NA
  
  ## subset to species of interest
  data$species <- toupper(data$species)
  sub <- subset(data, species == toupper(spec) & distance <= maxdist)
  
  ## append blank surveys with no detections of selected species
  sub = plyr::rbind.fill(sub, blank[-which(blank$surveyID %in% sub$surveyID),])
  
  results <- run_models(sub, maxdist=maxdist, cuts=bins, modlist=modlist, debug=debug)
  
  results$table <- make_results_table(results)
  
  cat('\n',toupper(spec),' model results:\n')
  print(results$table)
  
  if (any(results$table$include == '*')) {
    tmp <- results$table[results$table$include == '*',]
    topmod <- row.names(tmp)[which(tmp$dAIC == min(tmp$dAIC))]
    modnum <- which(names(results) == topmod)
    results$D = results[[modnum]]$dht$individuals$D
    results$N = results[[modnum]]$dht$individuals$N
    # results$average.p = results[[modnum]]$dht$individuals$average.p
    # results$w = results[[modnum]]$ddf$meta.data$int.range[2]
    cat('\nDensity estimates (individuals/ha) by strata:\n')
    print(results$D)
  } else {cat('\nNo models fit well. Try different bin sizes.\n')}
  return(results)
}

run_models = function(dat, maxdist=100, cuts=NULL, formula=~1, modlist, debug) {
  ## use convert.units=0.01 to convert distances in meters to 1/100 of the side of a hectare (100m x 100m)
  ##  --> resulting densities will be in individuals/hectare
  ## start with model combinations suggested by Thomas et al. 2010 J Appl Ecol 47:5-14
  results = list()
  
  ## uniform with cosine (can't use uniform key with no adjustments)
  if ('unif.cos' %in% modlist) {
    results$unif.cos = try(Distance::ds(data=dat, transect='point', formula=formula, key='unif', adjustment='cos',
                                        cutpoints=cuts, truncation=maxdist, convert_units=0.01),TRUE)
    }
  
  ## half-normal with cosine
  if ('hn.cos' %in% modlist) {
    results$hn.cos = try(Distance::ds(data=dat, transect='point', formula=formula, key='hn', adjustment='cos',
                                      cutpoints=cuts, truncation=maxdist, convert_units=0.01),TRUE)}
  
  ## hazard rate with polynomial
  if ('hr.poly' %in% modlist) {
    results$hr.poly = try(Distance::ds(data=dat, transect='point', formula=formula, key='hr', adjustment='poly',
                                       cutpoints=cuts, truncation=maxdist, convert_units=0.01),TRUE)}
  
  x=which(lapply(1:length(results), function(x) {class(results[[x]])})=='try-error')
  if (length(x)>0) {results = results[-x]}
  return(results)
}

make_results_table = function (results, binned=T) {
  table = as.data.frame(do.call(rbind, lapply(results, function(x) {
    res = data.frame(AIC = x$ddf$criterion,
                     dAIC = NA,
                     w = NA,
                     mono = 0)})))
  table$mono = lapply(results, function(x) {
    if('monotonicity.check' %in% names(x$ddf)) {return(x$ddf$monotonicity.check)} else {return(NA)}})
  
  if (binned==T) {
    table$chisq = as.data.frame(do.call(rbind, lapply(results, function(x) {
      gofres = mrds::ddf.gof(x$ddf)
      res = data.frame(chisq = round(gofres$chisquare$chi1$p,digits=3))
      return(res)
    })))
  } else {
    gof = as.data.frame(do.call(rbind, lapply(results, function(x) {
      gofres = mrds::ddf.gof(x$ddf)
      res = data.frame(KS = round(gofres$dsgof$ks$p,digits=3),
                       CvM = round(gofres$dsgof$CvM$p,digits=3))
      return(res)
    })))
    table = cbind(table, gof)
  }
  
  table$include = ''
  j = which(names(table) %in% c('chisq','CvM'))
  table$include[which(!is.na(table[,j]) & (table$mono==T | is.na(table$mono)))]='*'
  
  i = which(table$include=='*')
  table$dAIC[i] = round(table$AIC[i]-min(table$AIC[i]),digits=3)
  table$loglik = exp(-0.5*table$dAIC)
  table$w = round(table$loglik/sum(table$loglik,na.rm=T),digits=3)
  table$loglik = NULL
  table$include[which(table[,j]<0.05)]=''
  
  table <- table[order(table$dAIC,table$AIC),]
  if (any(results$table$include == '*')) {
    tmp = table[table$include=='*',]
    topmod = row.names(tmp)[which(tmp$dAIC==min(tmp$dAIC))]
    graphics::plot(results[[which(names(results)==topmod)]],main=topmod)
  } else {
    graphics::plot(results[[which(names(results)==row.names(table)[1])]], main=row.names(table)[1])
  }
  return(table)
}
