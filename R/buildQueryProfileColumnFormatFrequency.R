#' buildQueryProfileColumnFormatFrequency
#'
#' @param conn.info Connection info created with \code{\link{prepareConnection}}
#' @param ... Other parameters
#'
#' @return queries column format frequency from table
buildQueryProfileColumnFormatFrequency <- function(conn.info, ...){
  UseMethod("buildQueryProfileColumnFormatFrequency", conn.info)
}

#' buildQueryProfileColumnFormatFrequency.sqlite
#'
#' @param conn.info Connection info created with \code{\link{prepareConnection}}
#' @param schema Table Schema
#' @param table  Table Name
#' @param column Column profiled
#' @param column.datatype Column datatype defined at the rdbs
#' @param query.filter Filter applied to the profile
#' @param ... Other parameters
#'
#' @return queries column format frequency from table
#' @noRd
buildQueryProfileColumnFormatFrequency.sqlite <- function(conn.info,
                                                          schema,
                                                          table,
                                                          column,
                                                          column.datatype,
                                                          query.filter, ...){
  return(NA)
}

#' buildQueryProfileColumnFormatFrequency.teradata
#'
#' @param conn.info Connection info created with \code{\link{prepareConnection}}
#' @param schema Table Schema
#' @param table  Table Name
#' @param column Column profiled
#' @param column.datatype Column datatype defined at the rdbs
#' @param query.filter Filter applied to the profile
#' @param ... Other parameters
#'
#' @return queries column format frequency from table
#' @noRd
buildQueryProfileColumnFormatFrequency.teradata <- function(conn.info,
                                                            schema,
                                                            table,
                                                            column,
                                                            column.datatype,
                                                            query.filter, ...){
  #TODO: handle accentuation

  # Concat schema and table
  schema.table <- paste0(trimws(schema), ".", table)

  if (is.na(query.filter)){
    query <- paste0("SELECT REGEXP_REPLACE( REGEXP_REPLACE( REGEXP_REPLACE(TRIM(",
                    column,
                    "), '[A-Za-z]', 'X'),",
                    "'[ ]', 'b'), '[0-9]', '9') AS COLUMN_FORMAT,",
                    "COUNT(*) AS FREQ",
                    " FROM ", schema.table,
                    " GROUP BY REGEXP_REPLACE( ",
                    "REGEXP_REPLACE( REGEXP_REPLACE(TRIM(",
                    column, "), '[A-Za-z]', 'X'), '[ ]', 'b'), '[0-9]', '9')",
                    " ORDER BY FREQ DESC"
    )
  }
  else {
    query <- paste0("SELECT REGEXP_REPLACE( REGEXP_REPLACE( REGEXP_REPLACE(TRIM(",
                    column,
                    "), '[A-Za-z]', 'X'),",
                    "'[ ]', 'b'), '[0-9]', '9') AS COLUMN_FORMAT,",
                    "COUNT(*) AS FREQ",
                    " FROM ", schema.table,
                    " WHERE ", query.filter,
                    " GROUP BY REGEXP_REPLACE( ",
                    "REGEXP_REPLACE( REGEXP_REPLACE(TRIM(",
                    column, "), '[A-Za-z]', 'X'), '[ ]', 'b'), '[0-9]', '9')",
                    " ORDER BY FREQ DESC"
    )
  }

  return(query)
}

#' buildQueryProfileColumnFormatFrequency.sqlserver
#'
#' @param conn.info Connection info created with \code{\link{prepareConnection}}
#' @param schema Table Schema
#' @param table  Table Name
#' @param column Column profiled
#' @param column.datatype Column datatype defined at the rdbs
#' @param query.filter Filter applied to the profile
#' @param ... Other parameters
#'
#' @return queries column format frequency from table
#' @noRd
buildQueryProfileColumnFormatFrequency.sqlserver <- function(conn.info,
                                                             schema,
                                                             table,
                                                             column,
                                                             column.datatype,
                                                             query.filter, ...){
  # Concat schema and table
  schema.table <- paste0(trimws(schema), ".", table)

  # use colate to remove accents. only works in strings
  collate <- ifelse(isStringColumn(column.datatype),
                    "Collate SQL_Latin1_General_CP1253_CI_AI",
                    "")

  SELECT <-   paste("SELECT
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(", column, collate, ",
  'A', 'X'), 'B', 'X'), 'C', 'X'), 'D', 'X'), 'E', 'X'), 'F', 'X'),
  'G', 'X'), 'H', 'X'), 'I', 'X'), 'J', 'X'), 'K', 'X'), 'L', 'X'),
  'M', 'X'), 'N', 'X'), 'O', 'X'), 'P', 'X'), 'Q', 'X'), 'R', 'X'),
  'S', 'X'), 'T', 'X'), 'U', 'X'), 'V', 'X'), 'W', 'X'), 'Y', 'X'),
  'X', 'X'), 'Z', 'X'), ' ', 'b'), '0', '9'), '1', '9'), '2', '9'),
  '3', '9'), '4', '9'), '5', '9'), '6', '9'), '7', '9'), '8', '9'),
  '9', '9')
  AS COLUMN_FORMAT
  , COUNT(*) AS FREQ
  FROM")

  GROUP_BY = paste(" GROUP BY
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(  REPLACE(
  REPLACE(  REPLACE(  REPLACE(", column, collate, ",
  'A', 'X'), 'B', 'X'), 'C', 'X'), 'D', 'X'), 'E', 'X'), 'F', 'X'),
  'G', 'X'), 'H', 'X'), 'I', 'X'), 'J', 'X'), 'K', 'X'), 'L', 'X'),
  'M', 'X'), 'N', 'X'), 'O', 'X'), 'P', 'X'), 'Q', 'X'), 'R', 'X'),
  'S', 'X'), 'T', 'X'), 'U', 'X'), 'V', 'X'), 'W', 'X'), 'Y', 'X'),
  'X', 'X'), 'Z', 'X'), ' ', 'b'), '0', '9'), '1', '9'), '2', '9'),
  '3', '9'), '4', '9'), '5', '9'), '6', '9'), '7', '9'), '8', '9'),
  '9', '9')
  ORDER BY 2 DESC")

  if (is.na(query.filter)){
    query <- paste(SELECT, schema.table, GROUP_BY )
  } else{
    query <- paste(SELECT, schema.table, " WHERE ", query.filter, GROUP_BY)
  }
}
