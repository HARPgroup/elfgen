#' Supply dataframe of ecological and hydrologic data from DEQ VAHydro database
#' @description Given a dataframe of ecological and hydrologic data from VAHydro,
#' removes all stations where the ratio of DA:Q is greater than 1000, also option to aggregate to the
#' maximum richness value at each flow value
#' @param watershed.df a dataframe of DEQ VAHydro EDAS sites with ecological and hydrologic data
#' @param station_agg option to aggregate to the maximum richness value at each flow value
#' @return the watershed.df dataframe
#' @import sqldf
#' @export clean_vahydro
clean_vahydro <- function (watershed.df,station_agg = "max") {
  print(paste("LENGTH OF INPUT DATASET:  ",length(watershed.df[,1]), sep = ''))

  #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW
  watershed.df["ratio"] <- (watershed.df$DA_SQMI)/(watershed.df$MAF)
  #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
  watershed.df <-watershed.df[!(watershed.df$ratio > 1000),]

  #USE ONLY MAX NT VALUE FOR EACH STATION
  watershed.df.query <- paste('SELECT "x.metric",
                                      MAX("NT.TOTAL.UNIQUE") as "NT.TOTAL.UNIQUE",
                                      "watershed.code",
                                      hydrocode,
                                      DA_SQMI,
                                      MAF
                                 FROM "watershed.df" a
                                 GROUP BY "x.metric"'
                              ,sep='')
  watershed.df <- sqldf(watershed.df.query)
  print(paste("LENGTH OF OUTPUT DATASET: ",length(watershed.df[,1]), sep = ''))

  return(watershed.df)
} #close function
