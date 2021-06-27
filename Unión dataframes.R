#Unión de OU+OV+campañas_Facebook
setwd("C:/Users/Irene/Desktop/Irene/Trabajos/Master/TFM/datos y EDA/datos") 
df1<-read.csv("ddTOT.csv", sep=";")
df2<-read.csv("Display_Formated.csv", sep="\t")

df2.filt<-df2[df2$Month!="All",]

dates.df1<-unique(df1$date)  
dates.df2<-unique(df2.filt$Month)

dates.range<-dates.df2

for (i in 1:nrow(df1)) {
  print(i)
  date<-as.character(df1$date[i])
  if (!(is.na(date))) {
    year<-as.numeric(strsplit(date, "-")[[1]][1])
    month<-as.numeric(strsplit(date, "-")[[1]][2])
    day<-as.numeric(strsplit(date, "-")[[1]][3])
    dr2use<-dates.range[grep(as.character(year),dates.range)]
    tag<-NA
    for (dr in dr2use) {
      data2use<-unlist(strsplit(as.character(dr)," - "))
      start<-data2use[1]
      end<-data2use[2]
      start.data<-unlist(strsplit(start,"-"))
      end.data<-unlist(strsplit(end,"-"))
      start.month<-as.numeric(start.data[2])
      start.day<-as.numeric(start.data[3])
      end.month<-as.numeric(end.data[2])
      end.day<-as.numeric(end.data[3])
      if ((month>=start.month & month<=end.month) & (day>=start.day & day<=end.day)) {tag<-dr}
    }
    df1$tag[i]<-tag
  } else {
    df1$tag[i]<-NA
  }
}

colnames(df1)[ncol(df1)]<-"Month"
df1$Month<-as.character(df1$Month)
df2$Month<-as.character(df2$Month)

df.merged<-merge(df1,df2,by="Month")

saveRDS(df.merged, "Output.rds")
