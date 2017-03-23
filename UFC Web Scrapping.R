library(rvest)
library(dplyr)
library(lubridate)

#Web-Scrapping to Get Most Recent Data
#------------------------------------------------------------------------------------------------------------------------------
EventsInfo=data.frame()
EventsUrl=c()
for (k in 1:4){
  EventUrl=read_html(paste('http://www.sherdog.com/organizations/Ultimate-Fighting-Chanmpionship-2/recent-events/',k,sep=''))
  EventInfo=EventUrl %>% html_nodes('table') %>% html_table()
  EventsInfo=rbind(EventsInfo,as.data.frame(EventInfo[2])[-1,])
  EventUrl=EventUrl %>% html_nodes('td') %>% html_nodes('a') %>% html_attr('href')
  EventsUrl=append(EventsUrl,EventUrl[-(1:8)])
}

EventsUrl=sapply(EventsUrl,function(x) paste('http://sherdog.com',x,sep=''))
Fights_Scrapped=data.frame()
for (i in seq(1,length(EventsUrl))){
  Event=read_html(EventsUrl[i])
  Result=Event %>% html_nodes('table') %>% html_table()
  #Part 1
  Result_p1=as.data.frame(Result[2])
  if (nrow(Result_p1)!=0){
    colnames(Result_p1)=c('Match','Fighter1','vs','Fighter2','Method','Round','Time')
    #Delete Title Row
    Result_p1=Result_p1[-1,]
    n=nrow(Result_p1)
    #Delete 'vs' Column
    Result_p1=Result_p1[,-3]
    #Seperate Method Column
    Result_p1$Referee=sapply(Result_p1$Method,function(x) gsub('^(.*)\\)','',x))
    Result_p1$Method_D=sapply(Result_p1$Method,function(x) gsub('\\)(.*)','',gsub('^(.*)\\(','',x)))
    Result_p1$Method=sapply(Result_p1$Method,function(x) gsub('\\(.*','',x))
    
    #Fighter Url
    Fighter_p1=Event %>% html_nodes('.fighter_result_data a') %>% html_attr('href')
    Fighter_p1=as.data.frame(matrix(Fighter_p1,ncol=2,byrow=TRUE))
    colnames(Fighter_p1)=c('Fighter1_url','Fighter2_url')
    
    rownames(Result_p1)=NULL
    Result_p1=cbind(Result_p1,Fighter_p1)
    
    #Part 2
    Result_p2=Result[1]
    #Format match number
    Result_p2=sapply(Result_p2,function(x) sub('^([a-zA-Z]* )','',x))
    Result_p2=as.data.frame(t(Result_p2))
    
    colnames(Result_p2)=c('Match','Method','Referee','Round','Time')
    
    #Name of fighters
    temp=Event %>% html_nodes('.right_side a span , .left_side a span') %>% html_text()
    Result_p2$Fighter1=temp[1]
    Result_p2$Fighter2=temp[2]
    
    #Seperate Method Column
    Result_p2$Method_D=sapply(Result_p2$Method,function(x) gsub('\\)(.*)','',gsub('^(.*)\\(','',x)))
    Result_p2$Method=sapply(Result_p2$Method,function(x) gsub('\\(.*','',x))
    
    #Fighters URL
    temp1=Event %>% html_nodes('.left_side a') %>% html_attr('href')
    Result_p2$Fighter1_url=temp1[1]
    temp2=Event %>% html_nodes('.right_side a') %>% html_attr('href')
    Result_p2$Fighter2_url=temp2[1]
    
    #Fight Date & Location
    D_L=Event %>% html_nodes('.authors_info span') %>% html_text()
    
    #Bind together
    Final=rbind(Result_p1,Result_p2)
    Final$Event_Name=EventsInfo[i,2]
    Final$Event_id=gsub('.*-','',EventsUrl[i])
    Final$Date=D_L[1]
    Final$Location=D_L[2]
    Fights_Scrapped=rbind(Fights_Scrapped,Final)
  }
  print(i)
}
Fights_Scrapped=as.data.frame(Fights_Scrapped)

Fights=sapply(Fights_Scrapped,function(x) ifelse(x=='N/A',NA,x))
Fights=as.data.frame(Fights)
Tfmt='%M:%S'
Fights$Time=strptime(Fights$Time,format=Tfmt)

Fighters_Scrapped=data.frame()
#Get Fighters' URL
Fighters_URL=sapply(unique(c(as.character(Fights_Scrapped$Fighter1_url),as.character(Fights_Scrapped$Fighter2_url))),function(x) paste('http://sherdog.com',x,sep=''))
for(j in seq(1,length(Fighters_URL))){
  Fighter=read_html(Fighters_URL[j])
  Result=Fighter %>% html_nodes('strong, .locality, .birthday span,.vcard h1') %>% html_text()
  Fighter_got=as.data.frame(t(Result[1:9]))
  colnames(Fighter_got)=c('Name','Birth_Date','Age','Birth_Place','Country','Height','Weight','Association','Class')
  Fighter_got$Fighter_id=gsub('.*-','',Fighters_URL[j])
  Fighters_Scrapped=rbind(Fighters_Scrapped,Fighter_got)
  
  print(j)
}

Fighters_Backup=Fighters_Scrapped

#Data Cleaning

Error=Fighters_Scrapped[Fighters_Scrapped$Class==' VS ',c(1:8,10,11)]
Fighters_Scrapped=Fighters_Scrapped[Fighters_Scrapped$Class!=' VS ',]

Error_p1=Error[grepl('lbs',Error$Height),]
Error=Error[!grepl('lbs',Error$Height),]



Error_p2=Error_p1[c(6,13),]
Error_p1=Error_p1[-c(6,13),]
colnames(Error_p2)=c('Birth_Date','Age','Birth_Place','Country','Height','Weight','Association','Class','Fighter_id','Url')
Error_p2$Name=c('Mirsad Bektic','Noad Lahat')

Error_p3=Error_p1[Error_p1$Association==Error_p1$Association[3],c(1:7,9,10)]
Error_p1=Error_p1[!Error_p1$Association==Error_p1$Association[3],]
colnames(Error_p3)=c('Name','Birth_Date','Age','Country','Height','Weight','Class','Fighter_id','Url')

colnames(Error_p1)=c('Name','Birth_Date','Age','Country','Height','Weight','Association','Class','Fighter_id','Url')

colnames(Error)=c('Name','Birth_Date','Age','Birth_Place','Country','Height','Weight','Class','Fighter_id','Url')


Fighters_Scrapped=rbind.fill(Fighters_Scrapped,Error,Error_p1,Error_p2,Error_p3)
Fighters_Scrapped$NickName=sapply(Fighters_Scrapped$Name,function(x) gsub('\\"','',regmatches(x,gregexpr('"[^"]*"',x))[[1]]))
Fighters_Scrapped$Name=sapply(Fighters_Scrapped$Name,function(x) gsub('\\".*\\"','',x))
Fighters_Scrapped$Age=sapply(Fighters_Scrapped$Age,function(x) gsub('AGE: ','',x))
Fighters_Scrapped$Feet=sapply(Fighters_Scrapped$Height,function(x) gsub("\\'.*",'',x))
Fighters_Scrapped$Inch=sapply(Fighters_Scrapped$Height,function(x) gsub('\\"','',gsub(".\\'",'',x)))
Fighters_Scrapped$Height=as.integer(as.character(Fighters_Scrapped$Feet))*12 + as.integer(as.character(Fighters_Scrapped$Inch))
Fighters_Scrapped$Weight=sapply(Fighters_Scrapped$Weight,function(x) gsub(' lbs','',x))
Fighters_Scrapped=sapply(Fighters_Scrapped, function(x) gsub('N/A',NA,x))
Fighters_Scrapped=as.data.frame(Fighters_Scrapped)
Fighters_Scrapped$Birth_Date=ymd(Fighters_Scrapped$Birth_Date)


#Data Formatting
#-----------------------------------------------------------------------------------------------------------------------------

Fighters_Updated=Fighters_Scrapped
Fighters_Updated$Fighter_id=as.integer(Fighters_Updated$Fighter_id)
Fighters_Updated=Fighters_Updated[!duplicated(Fighters_Updated$Fighter_id),]
rownames(Fighters_Updated)=NULL

Fighters_Updated$Birth_Date=ymd(as.character(Fighters_Updated$Birth_Date))
Fighters_Updated$Name=as.character(Fighters_Updated$Name)
Fighters_Updated$NickName=as.character(Fighters_Updated$NickName)

Fighters_Updated$Height=as.integer(Fighters_Updated$Height)
Fighters_Updated$Weight=as.integer(Fighters_Updated$Weight)
Fighters_Updated[,7:10]=sapply(Fighters_Updated[,c(7:9,11)],function(x) as.character(x))
Fighters_Updated[,7:10]=sapply(Fighters_Updated[,c(7:9,11)],function(x) ifelse(x %in% c("",'N/A'),NA,x))
for (i in seq(7,10)){
  Fighters_Updated[,i]=as.factor(Fighters_Updated[,i])
}


#Scrape Event Location Data
#-----------------------------------------------------------------------------------------------------------------------------
Stadiums=count(EventsInfo$X3)
Stadiums$x=as.character((Stadiums$x))
geoCodeDetails = function (address){
  geo_reply=geocode(address,output='all',messaging=TRUE, override_limit = TRUE)
  answer=data.frame(lat=NA,long=NA,accuracy=NA,formatted_address=NA,address_type=NA,status=NA)
  answer$status=geo_reply$status
  
  while(geo_reply$status=='OVER_QUERY_LIMIT'){
    print('OVER QUERY LIMIT - Pausing for 1 hour at:')
    time=sys.time()
    print(as.character(time))
    sys.sleep(60*60)
    geo_reply=geocode(address,output='all',messaging=TRUE,override_limit = TRUE)
    answer$status=geo_reply$status
  }
  
  if(geo_reply$status !='OK'){
    return(answer)
  }
  answer$lat=geo_reply$result[[1]]$geometry$location$lat
  answer$long=geo_reply$result[[1]]$geometry$location$lng
  if (length(geo_reply$result[[1]]$types)>0){
    answer$accuracy=geo_reply$result[[1]]$types[[1]]
  }
  answer$address_type=paste(geo_reply$result[[1]]$types,collapse=',')
  answer$formatted_address=geo_reply$result[[1]]$formatted_address
  
  return(answer)
}

Stadiums$lat=NA
Stadiums$long=NA

for (i in seq(1,nrow(Stadiums))){
  Detail=geoCodeDetails(Stadiums$x[i])
  Stadiums$lat[i]=Detail$lat
  Stadiums$long[i]=Detail$long
  Stadiums$Address=Datail$formatted_address
}

#Manually search and filling
#Stadiums[is.na(Stadiums$lat),]
Stadiums[21,3:4]=c(40.741895,-73.989308)
Stadiums[37,3:4]=c(42.878056,-78.8775)
Stadiums[69:71,3:4]=matrix(rep(c(-23.577721, -46.656048),3),3,byrow=TRUE)
Stadiums[134,3:4]=c(46.830833, -71.246389)
Stadiums[139,3:4]=c(-23.535033,-46.636617)
Stadiums$x=gsub('EVENT CANCELED - ','',Stadiums$x)

#Save Result as CSV

write.csv(Fighters_Updated,'Fighters_Updated.csv')
write.csv(Fights_Scrapped,'Fights_Updated.csv')
write.csv(EventsInfo,'EventsInfo.csv')
write.csv(Stadiums,'Stadiums.csv')

#------Some Manualy Inspecting and Cleaning for typos--------------


#Create an updating function