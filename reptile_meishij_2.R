#��ʳ�ܽ���ʳ��������ʳ������
library(XML)
library(RCurl)  
library(Rcpp)
sourceCpp("C:\\Users\\applee\\Desktop\\pinyin.cpp")
library(stringr)

data<-read.csv("C:\\Users\\applee\\Desktop\\1A����-ʳ��ӳ���.csv")   #��ȡ�õ����ݿ��ʽ

list_url<-data[[8]]    #ȡ���ݿ�ڰ�����Ϊ����,��ַ
list_url
newurl=as.character(unlist(list_url))

list_scene<-data[[2]]    #ȡ���ݿ�ڶ�����Ϊ����
list_scene
scene=as.character(unlist(list_scene))

for(number in 1:38)
{

  newurl_element<- newurl[number]
  newurl_element
  
  scene_element<- scene[number]
  scene_element
  
  wp <-getURL(newurl_element,.encoding="UTF-8") #����ҳ�����ı���-������
  wp

  
  
  index<-(nchar(wp)>0)  #����������ַ������ȴ���0�ı��
  
  index
  
    wp2<-iconv(wp,"UTF-8","UTF-8")  #�ַ���������ת��
    
    wp2
    
    doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #ѡ��UTF-8������ҳ�Ľ���
    rootNode<-xmlRoot(doc)
    
    rootNode
    
    
    
    suitable_advise<-xpathSApply(rootNode,"//p[@class='p2'][1]",xmlValue)                 #����ʳ�Ľ���
    suitable_advise
    
    
    taboo_advise<-xpathSApply(rootNode,"//p[@class='p2'][last()]",xmlValue)                 #����ʳ�Ľ���
    taboo_advise
    
    
    
    taboo_material<- xpathSApply(rootNode,"//ul[@class='clearfix'] ",xmlValue)[2]  #����ʳ��
    taboo_material<-str_replace_all(taboo_material,'\\s*\t',"|")
    taboo_material=gsub("\\(","",taboo_material)     #ע��ƥ�䣨Ҫ��\\
    taboo_material
    
    
    suitable_material<- xpathSApply(rootNode,"//ul[@class='clearfix'] ",xmlValue)[1]  #����ʳ��
    suitable_material<-str_replace_all(suitable_material,'\\s*\t',"|")
    suitable_material=gsub("\\(","",suitable_material)     #ע��ƥ�䣨Ҫ��\\
    suitable_material=gsub("\\��","",suitable_material)     #ע��ƥ�䣨Ҫ��\\
    suitable_material
    
    a=c(scene_element,suitable_advise,suitable_material,taboo_advise,taboo_material)
    print(a)
    write.table( t(a),"C:\\Users\\applee\\Desktop\\1B����-ʳ��ӳ���.csv",row.names = F,col.names = F,sep=",",append=T,quote=F)  

  
}