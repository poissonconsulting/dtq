#' Cubic Metres per Second to Cubic Feet per Second
#' @param x A numeric vector.
#' @return A numeric vector.
#' @export
#' @examples
#' dtq_cms_to_cfs(1)
#' dtq_cms_to_kcfs(1)
#' dtq_cfs_to_cms(1)
#' dtq_kcfs_to_cms(1)
dtq_cms_to_cfs <- function(x) x * 35.314666212661

#' @describeIn dtq_cms_to_cfs cms to kcfs
#' @export
dtq_cms_to_kcfs <- function(x) dtq_cms_to_cfs(x / 1000)
  
#' @describeIn dtq_cms_to_cfs kcfs to cms
#' @export
dtq_kcfs_to_cms <- function(x) dtq_cfs_to_cms(x * 1000)

#' @describeIn dtq_cms_to_cfs cfs to cms
#' @export
dtq_cfs_to_cms <- function(x) x / 35.314666212661
