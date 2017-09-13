#美食杰禁忌食材与适宜食材爬虫
library(XML)
library(RCurl)  
library(Rcpp)
sourceCpp("C:\\Users\\applee\\Desktop\\pinyin.cpp")
library(stringr)

data<-read.csv("C:\\Users\\applee\\Desktop\\1A场景-食材映射表.csv")   #读取得到数据框格式

list<-data[[2]]    #取数据框第三列作为向量

scene_element=as.character(unlist(list))

for(number in 1:38)
{
keyword<- scene_element[number]
keyword<-trimws(keyword)  #去除收尾空格

obj<- tolower(getLetter(keyword)) #得到关键字的全拼，并将字符向量变为小写

obj

newurl_1<-paste("http://www.meishij.net/yaoshanshiliao/gongnengxing/",obj,sep="") #合并URL
wp_1<-getURL(newurl_1,.encoding="UTF-8") #用网页本身的编码-功能性
newurl_2<-paste("http://www.meishij.net/yaoshanshiliao/zangfu/",obj,sep="") #合并URL
wp_2<-getURL(newurl_2,.encoding="UTF-8") #用网页本身的编码-脏腑调理
newurl_3<-paste("http://www.meishij.net/yaoshanshiliao/renqunshanshi/",obj,sep="") #合并URL
wp_3<-getURL(newurl_3,.encoding="UTF-8") #用网页本身的编码-人群膳食
newurl_4<-paste("http://www.meishij.net/yaoshanshiliao/jibingtiaoli/",obj,sep="") #合并URL
wp_4<-getURL(newurl_4,.encoding="UTF-8") #用网页本身的编码-疾病调理
newurl_1

name<-c(newurl_1,newurl_2,newurl_3,newurl_4)
tem<-c(wp_1,wp_2,wp_3,wp_4)

index<-which(nchar(tem)>0)  #输出向量中字符串长度大于0的编号

index

if( length(index)!=0)   #判断字符串长度是否符合
{
  wp2<-iconv(tem[index],"UTF-8","UTF-8")  #字符向量编码转换
  
  wp2
  
  doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析
  rootNode<-xmlRoot(doc)
  
  rootNode
  
  
  
  suitable_advise<-xpathSApply(rootNode,"//p[@class='p2'][1]",xmlValue)                 #适宜食材建议
  suitable_advise
  
  
  taboo_advise<-xpathSApply(rootNode,"//p[@class='p2'][last()]",xmlValue)                 #禁忌食材建议
  taboo_advise
  
  
  
  taboo_material<- xpathSApply(rootNode,"//ul[@class='clearfix'] ",xmlValue)[2]  #禁忌食材
  taboo_material<-str_replace_all(taboo_material,'\\s*\t',"|")
  taboo_material=gsub("\\(","",taboo_material)     #注意匹配（要加\\
  taboo_material
  
  
  suitable_material<- xpathSApply(rootNode,"//ul[@class='clearfix'] ",xmlValue)[1]  #适宜食材
  suitable_material<-str_replace_all(suitable_material,'\\s*\t',"|")
  suitable_material=gsub("\\(","",suitable_material)     #注意匹配（要加\\
  suitable_material=gsub("\\（","",suitable_material)     #注意匹配（要加\\
  suitable_material

  a=c(keyword,suitable_advise,suitable_material,taboo_advise,taboo_material)
print(a)
write.table( t(a),"C:\\Users\\applee\\Desktop\\1B场景-食材映射表.csv",row.names = F,col.names = F,sep=",",append=T,quote=F)  
}

}






