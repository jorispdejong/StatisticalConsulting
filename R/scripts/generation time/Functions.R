##################################
### IMPORT AND PREPROCESS DATA ###
##################################
preprocessData <- function(raw_file_path="data/generation time/supplemetary table.csv",
                           output_file_path="data/generation time/data-certain-probable.csv",
                           begin_date = as.Date("2019-12-01"),
                           cutoff_date = as.Date("2020-02-12"),
                           write_to_file = T){
  library("readr")
  library("dplyr")
  df <- read_csv(raw_file_path, 
                 col_types = cols(X1 = col_skip(), 
                                  SIClassification = col_skip(), 
                                  DiagnosisCountry = col_skip(), 
                                  InfectorOnset = col_date(format = "%m/%d/%Y"), 
                                  InfecteeOnset = col_date(format = "%m/%d/%Y"), 
                                  Source = col_skip()))

  df['tstar'] = cutoff_date - begin_date
  df['dist'] = (df$SR+df$SL)/2-(df$ER+df$EL)/2
  df['SL'] = if_else(df$SL < df$EL, df$EL, df$SL)
  df['ER'] = if_else(df$ER > df$SR, df$SR, df$ER)
  
  if(write_to_file) write.table(df, output_file_path, row.names=FALSE, sep=",", quote = FALSE)

  return(df)
}