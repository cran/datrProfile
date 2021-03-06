#' profileColumn
#'
#' @param conn.info Connection info created with \code{\link{prepareConnection}}
#' @param schema Table schema
#' @param table Table name
#' @param column Column being profiled
#' @param column.datatype Column datatype
#' @param query.filter Filter applied before profile the column
#' @param limit.freq.values Distinct values shown in frequency data frame
#' @param format.show.percentage Threshold considered when showing formats'
#' percentages
#'
#' @return  columnProfile <- list(column, count.total,
#' count.distinct, perc.distinct, count.null, perc.null,
#' min.value, max.value, column.freq, format.freq = format.freq)
profileColumn <- function(conn.info,
                          schema,
                          table,
                          column,
                          column.datatype,
                          query.filter,
                          limit.freq.values = 30,
                          format.show.percentage){

  print(paste(Sys.time(),"Started profile for column", column))

  # Connects to database
  conn <- connectDB(conn.info)

  # Count(*) from table
  query.count.total <- buildQueryCountTotal(conn.info,
                                           schema = schema,
                                           table = table,
                                           query.filter = query.filter)
  count.total <- unlist(odbc::dbGetQuery(conn, query.count.total))[[1]]

  # Count(distinct column), min(column), max(column) from table
  query.column.stats <- buildQueryColumnStats(conn.info,
                                              schema = schema,
                                              table = table,
                                              column = column,
                                              query.filter = query.filter)
  column.stats <- odbc::dbGetQuery(conn, query.column.stats)

  count.distinct <- column.stats[[1]]
  min.value <- column.stats[[2]]
  max.value <- column.stats[[3]]

  # Count(*) from table where column is null
  query.count.null <- buildQueryCountNull(conn.info,
                                          schema = schema,
                                          table = table,
                                          column = column,
                                          query.filter = query.filter)
  count.null <- unlist(odbc::dbGetQuery(conn, query.count.null))[[1]]

  # Select values and frequencies
  # Column, count(*) from table group by column
  query.column.freq <- buildQueryColumnFrequency(conn.info,
                                                 schema = schema,
                                                 table = table,
                                                 column = column,
                                                 limit.freq.values = limit.freq.values,
                                                 query.filter = query.filter)
  column.freq <- odbc::dbGetQuery(conn, query.column.freq)
  names(column.freq) <- c( "value", "freq")
  column.freq$perc <- column.freq$freq / count.total

  # call profileColumnFormatFrequency
  format.freq <- profileColumnFormat(conn.info,
                                     column,
                                     column.datatype,
                                     schema,
                                     table,
                                     count.total,
                                     query.filter,
                                     format.show.percentage)

  # closes connection
  closeConnection(conn)

  # Percentage stats
  perc.distinct <- count.distinct / count.total
  perc.null <- count.null / count.total

  columnProfile <- list(column = column,
                  count.total = unclass(count.total),
                  count.distinct = count.distinct,
                  perc.distinct = perc.distinct,
                  count.null = count.null,
                  perc.null = perc.null,
                  min.value = min.value,
                  max.value = max.value,
                  column.freq = column.freq,
                  format.freq = format.freq)

  rownames(columnProfile) <- NULL

  # print(paste(Sys.time(),"Ended profile for column", column))
  # cat(sprintf("Ended statistics for column %s - at %s\n",
  #             column,
  #             Sys.time()))

  return(columnProfile)
}
