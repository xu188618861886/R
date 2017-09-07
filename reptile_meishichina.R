library(XML)
library(bitops)
library(RCurl)  
library(Rcpp)
sourceCpp("C:\\Users\\Administrator\\Desktop\\pinyin.cpp") 
library(stringr)

data<-read.csv("C:\\Users\\Administrator\\Desktop\\4B_foodtag.csv")   #��ȡ�õ����ݿ��ʽ

list<-data[[1]]    #ȡ���ݿ��һ����Ϊ����

for (i in 1:46)
{
  
keyword<-list[i]   #ץȡ�ؼ���

keyword=as.character(keyword)   #numericת��Ϊcharacter

obj<- tolower(getLetter(keyword))  #��ùؼ���ƴ��

print(keyword)
print(obj)

newurl<-paste("http://www.meishichina.com/YuanLiao/gongxiao/",obj,"/",sep="")


wp<-getURL(newurl,.encoding="UTF-8") #����ҳ�����ı���

a=nchar(wp)

if(a>1)
{

wp2<-iconv(wp,"UTF-8","UTF-8") #ת��

doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #ѡ��UTF-8������ҳ�Ľ���

rootNode<-xmlRoot(doc)

ts<-xpathSApply(rootNode,"//div[@class='tui_c']//a",xmlValue)  #������ȡ�Ƽ�ʳ��

sc<-str_replace_all(ts,"\n","")        #����
print(sc)

write.table( t(sc),"C:\\Users\\Administrator\\Desktop\\�Ƽ�ʳ��.csv",row.names = keyword,col.names = F,sep=",",append=T,quote=F)  
}

else{

write.table( t(sc),"C:\\Users\\Administrator\\Desktop\\�Ƽ�ʳ��.csv",row.names = keyword,col.names = F,sep=",",append=T,quote=F)    
  
}

}



