#' write.running.table
#'
#' Writes measures to a running table as they are generated
#'
#' @export
write.running.table <- function(x, save.path) {

  write.table(x,
              save.path,
              sep = ",",
              col.names =
                !file.exists(save.path),
              append = T,
              row.names = F)

}
