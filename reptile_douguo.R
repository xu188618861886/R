#根据场景，在豆果美食上找到相应食谱，找出在自己食谱中有的，输出到表格中。

library(XML)
library(bitops)
library(RCurl)  
library(Rcpp)
library(stringr)
library(rJava)
library(xlsxjars)
library(xlsx)

scene<-read.xlsx("C:\\Users\\zhixu\\Desktop\\5A_foodtag.xlsx",sheetIndex=2,colIndex=1,encoding="UTF-8")   #加载场景
recipe<-read.xlsx("C:\\Users\\zhixu\\Desktop\\5A_foodtag.xlsx",sheetIndex=1,colIndex=2,encoding="UTF-8")   #加载食谱
ID<-read.xlsx("C:\\Users\\zhixu\\Desktop\\5A_foodtag.xlsx",sheetIndex=1,colIndex=1,encoding="UTF-8")   #加载ID
utf<-read.xlsx("C:\\Users\\zhixu\\Desktop\\utf-8.xlsx",sheetIndex=2,encoding="UTF-8")   #加载中文UTF-8编码

options(max.print=1000000)

#搜索关键词编码转换函数
cn2UTF8<-function(theme){
  tmp<-unlist(strsplit(theme,""))
  rs<-vector()
  for(i in 1:length(tmp)){
    index<-which(utf[,2]==tmp[i]) 
    code<- as.character(utf[index,1])
    rs<-append(rs,code)
  }
  kw_coding<-paste(rs,sep="",collapse ="")   #搜索关键词编码
  return(kw_coding)
}

for(number_scene in 1:36)  #场景编号
{
  tag=0    #标识符，如果场景下找到的食谱个数至少一个赋值为1，反之为0
  scene_element=as.character(unlist(scene[number_scene,]))  
  print(scene_element)
  url_base<-paste("http://www.douguo.com/caipu/",sep="",cn2UTF8(as.character(scene_element)),collapse ="")    #网页抓取内容
  cp_name_total="推荐菜谱"
  print(url_base)
  wp<-getURL(trimws(url_base),.encoding="UTF-8")  #获取url信息
  a=nchar(wp)
  a
  if((a>1))     #判断网页是否存在
  {
    for(page in 1:30)     # 抓取1到30页的食谱
    {
      index=30*(page-1)
      url<-paste(url_base,sep="","/",index,collapse ="")    #网页抓取内容
      wp<-getURL(trimws(url),.encoding="UTF-8")  #获取url信息
      wp2 <-iconv(wp,"UTF-8","UTF-8")  #转码 #UTF-8
      doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析
      rootNode<-xmlRoot(doc)
      cp_name  <- xpathSApply(rootNode,"//a[@class='cp_name']",xmlValue)   #提取食材分类数据
      cp_name_total=c(cp_name_total,cp_name)
    }
    
    #数据处理
    cp_name_total=gsub("\\（.*?\\）","",cp_name_total)
    cp_name_total=gsub("\\【.*?\\】","",cp_name_total)
    cp_name_total=gsub("\\#.*?\\#","",cp_name_total)
    cp_name_total=gsub("\\——","",cp_name_total)
    cp_name_total=gsub("利仁电饼档LRFD431试用报告","",cp_name_total)
    cp_name_total=gsub("\\+","",cp_name_total)
    
    cp_name_total
    cp_name_total <- cp_name_total[!duplicated(cp_name_total)]      #字符向量去重   
    l=length(cp_name_total)-1     #字符向量的长度-1
    cp_name_total_element="豆果美食"  #初始化
    ID_element="123"
    for(number_douguo in 1:l)     #豆果美食编号
    {
      
      cp_name_total_element=cp_name_total[number_douguo]       #豆果美食的食谱
      cp_name_total_element
      if(nchar(cp_name_total_element)>1)  
      {  
      for(number_own in 1:13123)
        {
          recipe_element=as.character(unlist(recipe[number_own,1]))          #自己的食谱编号
          ID_element=ID[number_own,]     #自己菜谱的ID
          
          if(grepl(recipe_element,cp_name_total_element))     #检查自己的菜谱是否包含在豆果美食的食谱中
            if(grepl(cp_name_total_element,recipe_element))     #检查豆果美食的菜谱是否包含在自己的食谱中
            {
              tag=1     #标识符，如果场景下找到的食谱个数至少一个赋值为1，反之为0
              ID_element=as.character(ID_element) 
              out_print= c(scene_element,ID_element,recipe_element)
              print(out_print)
              sc=c(ID_element,recipe_element)
              write.table( t(sc),"C:\\Users\\zhixu\\Desktop\\5B_foodtag-匹配.csv",row.names = scene_element,col.names = F,sep=",",append=T,quote=F) 
            }
        }
      }
    }
  }
  if(tag==0)
  {
    write.table( t(scene_element),"C:\\Users\\zhixu\\Desktop\\5B_foodtag-未匹配.csv",row.names = F,col.names = F,sep=",",append=T,quote=F) 
  }
}





