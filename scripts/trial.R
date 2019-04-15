# print(getwd())
# 
# 
# data <- read.csv("datasets/Minor124.csv")
# print(length(data))
# print(nrow(data))
# 
# 
# datat=nrow(unique(data['text']))
# print(datat)
# dataaa=unique(data['text'])
# 
# write.csv(dataaa,file="main_data.csv",row.names = FALSE)

setwd('Documents/minor')

dat <- read.csv("datasets/main_dataset.csv")
print(length(dat))
print(nrow(dat))
# write.csv(dataaa,file="main.csv",row.names = TRUE,col.names = TRUE)

f=dat['text']
print(length(f))
print(nrow(f))

# for(i in 0:nrow(f)-4500)
# {
#   print(f['text'][1])
# }
