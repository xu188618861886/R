library(XML)
library(bitops)
library(RCurl)  
library(Rcpp)
sourceCpp("C:\\Users\\Administrator\\Desktop\\pinyin.cpp") 
library(stringr)

data<-read.csv("C:\\Users\\Administrator\\Desktop\\4B_foodtag.csv")   #读取得到数据框格式

list<-data[[1]]    #取数据框第一列作为向量

for (i in 1:46)
{
  
keyword<-list[i]   #抓取关键字

keyword=as.character(keyword)   #numeric转换为character

obj<- tolower(getLetter(keyword))  #获得关键字拼音

print(keyword)
print(obj)

newurl<-paste("http://www.meishichina.com/YuanLiao/gongxiao/",obj,"/",sep="")


wp<-getURL(newurl,.encoding="UTF-8") #用网页本身的编码

a=nchar(wp)

if(a>1)
{

wp2<-iconv(wp,"UTF-8","UTF-8") #转码

doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析

rootNode<-xmlRoot(doc)

ts<-xpathSApply(rootNode,"//div[@class='tui_c']//a",xmlValue)  #初步获取推荐食材

sc<-str_replace_all(ts,"\n","")        #整理
print(sc)

write.table( t(sc),"C:\\Users\\Administrator\\Desktop\\推荐食材.csv",row.names = keyword,col.names = F,sep=",",append=T,quote=F)  
}

else{

write.table( t(sc),"C:\\Users\\Administrator\\Desktop\\推荐食材.csv",row.names = keyword,col.names = F,sep=",",append=T,quote=F)    
  
}

}




