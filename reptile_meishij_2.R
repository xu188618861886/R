#美食杰禁忌食材与适宜食材爬虫
library(XML)
library(RCurl)  
library(Rcpp)
sourceCpp("C:\\Users\\applee\\Desktop\\pinyin.cpp")
library(stringr)

data<-read.csv("C:\\Users\\applee\\Desktop\\1A场景-食材映射表.csv")   #读取得到数据框格式

list_url<-data[[8]]    #取数据框第八列作为向量,网址
list_url
newurl=as.character(unlist(list_url))

list_scene<-data[[2]]    #取数据框第二列作为向量
list_scene
scene=as.character(unlist(list_scene))

for(number in 1:38)
{

  newurl_element<- newurl[number]
  newurl_element
  
  scene_element<- scene[number]
  scene_element
  
  wp <-getURL(newurl_element,.encoding="UTF-8") #用网页本身的编码-功能性
  wp

  
  
  index<-(nchar(wp)>0)  #输出向量中字符串长度大于0的编号
  
  index
  
    wp2<-iconv(wp,"UTF-8","UTF-8")  #字符向量编码转换
    
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
    
    a=c(scene_element,suitable_advise,suitable_material,taboo_advise,taboo_material)
    print(a)
    write.table( t(a),"C:\\Users\\applee\\Desktop\\1B场景-食材映射表.csv",row.names = F,col.names = F,sep=",",append=T,quote=F)  

  
}
