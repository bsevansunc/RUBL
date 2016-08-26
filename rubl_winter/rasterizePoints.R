makeRecordsSpatial <- function(inData){
  require(dplyr)
  data.df <- as.data.frame(inData) #%>%
#     filter(count != 'X') %>%
#     mutate(count = as.numeric(count))
  SpatialPointsDataFrame(
    data.df[,c('lon', 'lat')],
    data = data.df %>%
      select(effortDist, effortArea, nObservers),
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
}

countRasterizedRecords <- function(inData, summaryFun){
  recordsSp <- makeRecordsSpatial(inData)
  recordsR <- rasterize(makeRecordsSpatial(inData), 
                        env.stack[[1]],
                        fun = summaryFun)
  recordsByCell <- rasterToPoints(recordsR)
  return(recordsByCell)
}

# makeRecordsSpatial(rublRecords) -> test
# 
# testR <- rasterize(makeRecordsSpatial(inData), env.stack[[1]], fun = 'count')


plot(env.stack[['tmin']])
points(
  countRasterizedRecords(
    rublRecords %>% 
      filter(count != 'X') %>%
      mutate(count = as.numeric(count),
             date = as.Date(date),
             month = lubridate::month(date)) %>% 
      filter(count > 100, month == 1)
    ),
  pch = 19
  )

t3 <- countRasterizedRecords(t2, 'sum')


plot(env.stack[['tmin']])
hist