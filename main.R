read.csv("dataset/crimes.csv") -> data


data[colnames(data)!="region" & colnames(data)!="name"] -> numericData
cov(numericData) -> covNumericData


sapply(10:nrow(data),function(x){
  subset = covNumericData[sample(nrow(data),x),]
  cov(subset) -> covSubset
})