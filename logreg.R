stock = 'MSFT'
csv_in = paste(c('data/',stock,'.csv'), collapse = "")
csv_out = paste(c('data/',tolower(stock),'_ag.csv'), collapse = "")

df = read.csv(csv_in, header=TRUE)
df = subset(df, subset=Close != "null")
df$Close = as.numeric(df$Close)