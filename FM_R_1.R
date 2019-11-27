#=======================================================================#
#各色線站點=============================================================#
Red=c("新北投","淡水","紅樹林","竹圍","關渡","忠義","復興崗","北投",
      "奇岩","唭哩岸","石牌","明德","芝山","士林","劍潭","圓山",
      "民權西路","雙連","中山","台北車站","台大醫院","中正紀念堂",
      "大安森林公園","大安","信義安和","台北101/世貿","象山")
Blue=c("南港展覽館","南港","昆陽","後山埤","永春","市政府","國父紀念館",
       "忠孝敦化","忠孝復興","忠孝新生","善導寺","台北車站","西門",
       "龍山寺","江子翠","新埔","板橋","府中","亞東醫院","海山","土城",
       "永寧","頂埔")
Orange=c("蘆洲","三民高中","徐匯中學","三和國中","三重國小","迴龍",
         "輔大","新莊","頭前莊","先嗇宮","三重","菜寮","台北橋","大橋頭",
         "民權西路","中山國小","行天宮","松江南京","忠孝新生","東門",
         "古亭","頂溪","永安市場","景安","南勢角")
Green=c("松山","南京三民","台北小巨蛋","南京復興","松江南京","中山",
        "北門","西門","小南門","中正紀念堂","古亭","台電大樓","公館",
        "萬隆","景美","大坪林","七張","新店區公所","新店","小碧潭")
Brown=c("南港展覽館","南港軟體園區","東湖","葫洲","大湖公園","內湖",
        "文德","港墘","西湖","劍南路","大直","松山機場","中山國中",
        "南京復興","忠孝復興","大安","科技大樓","六張犁","麟光","辛亥",
        "萬芳醫院","萬芳社區","木柵","動物園")
Yellow=c("大坪林","十四張","秀朗橋","景平","景安","中和","橋和","中原",
         "板新","板橋","新埔民生","頭前庄","幸福","新北產業園區")
Yellow2=c("大坪林","景安","板橋","頭前庄")
#======================================================================#
#前置作業:讀入ods_file=================================================#
install.packages("readODS")
library(readODS)
year = c(201701:201712,201801:201812,201901:201910)
ListIn = list();ListOut = list();n=0
for (i in year)
{
  n=n+1
  x = paste0("y",as.character(i))
  read_path = paste0("C:/Users/Taner/Desktop/DM_MRT/",i,"_cht.ods")
  ListIn[[n]] = read_ods(path = read_path,col_names = T,sheet = "進站資料")
  ListOut[[n]] = read_ods(path = read_path,col_names = T,sheet = "出站資料")
}
#======================================================================#
#test & debug==========================================================#
for (i in 1:34)
{
  print(i);print(dim(ListIn[[i]]) == dim(ListOut[[i]]))
}
dim(ListIn[[16]]);dim(ListOut[[16]])
names(ListIn[[16]])
ListIn[[16]] = ListIn[[16]][-31,];ListIn[[16]] = ListIn[[16]][,-110]
dim(ListIn[[23]]);dim(ListOut[[23]])
names(ListIn[[23]]);names(ListOut[[23]])
ListIn[[23]] = ListIn[[23]][c(-31:-35),];ListIn[[23]] = ListIn[[23]][,-110]
#remove(ListIn);remove(ListOut)
#======================================================================#
#function每日各站總流量與排名==========================================#
#總流量:進+出==========================================================#
New_Total = function(IN,OUT)
{
  DF = IN[,-1]+OUT[,-1]
  Sum = numeric();name = names(DF)
    for (i in 1:length(DF)){Sum[i] = sum(DF[[i]])}
  New = data.frame(data.frame(name,Sum)[order(-Sum),] ,rank = c(1:108))
  return(New)
}
#======================================================================#
#建立總和的list_variable===============================================#
List_Total=list();year = c(201701:201712,201801:201812,201901:201910)
for (i in 1:34)
{
  x = paste0("year",as.character(year[i]))
  List_Total[[x=i]] = New_Total(ListIn[[i]],ListOut[[i]])
  #print(head(List_Total[[i]],3))
}
#=====================================================================#
#淡水站各月趨勢=======================================================#
TS=data.frame()
for (i in 1:34)
{
  tem = List_Total[[i]][List_Total[[i]]$name == "淡水",]
  TS = rbind(TS,tem)
}
TS = cbind(TS,year=as.character(year),Sum_thd=(TS$Sum/1000))
windows()
plot(x=TS$year,y=TS$Sum_thd)
#=====================================================================#
#紅樹林站各月趨勢=====================================================#
RF=data.frame()
for (i in 1:34)
{
  tem = List_Total[[i]][List_Total[[i]]$name == "紅樹林",]
  RF = rbind(RF,tem)
}
RF = cbind(RF,year=as.character(year),Sum_thd=(RF$Sum/1000))
plot(x=RF$year,y=RF$Sum_thd)
#=====================================================================#
#function=============================================================#
MRT = function(DATA)
{
  DF = DATA[,-1]
  Sum = numeric();name = names(DF)
  for (i in 1:length(DF)){Sum[i] = sum(DF[[i]])}
  New = data.frame(data.frame(name,Sum)[order(-Sum),] ,rank = c(1:108))
  return(New)
}
#=====================================================================#
#function:選擇站名跑各月進站出站總流人數==============================#
flow = function(stepname)
{
  In = data.frame();Out = data.frame()
  for (i in 1:34)
  {
    tem1 = MRT(ListIn[[i]])
    tem2 = MRT(ListOut[[i]])
    tem1 = tem1[tem1$name == stepname,]
    tem2 = tem2[tem2$name == stepname,]
    In = rbind(In,tem1)
    Out = rbind(Out,tem2)
  }
  In = cbind(In,year=as.character(year),Sum_thd=(In$Sum/1000))
  Out = cbind(Out,year=as.character(year),Sum_thd=(Out$Sum/1000))
  return(list(In=In,Out=Out))
}
#=====================================================================#
#測試=================================================================#
R27 = flow("紅樹林")
plot(x=R27[[1]]$year,y=R27[[1]]$Sum_thd)
plot(x=R27[[2]]$year,y=R27[[2]]$Sum_thd)
#======================================================================#
#測試輸入<失敗>========================================================#
install.packages("ff")
library(ff)
your_data <-read.csv.ffdf(file ='C:/Users/Taner/Desktop/臺北捷運每日分時各站OD流量統計資料_201701.csv',
                          header = F)
read.csv("C:/Users/Taner/Desktop/test.csv",skip = 2,sep = ' ')
library(data.table)
D = fread("C:/Users/Taner/Desktop/201910.csv",ncoding ="UTF-8")
DD = fread("C:/Users/Taner/Desktop/test.csv",skip = 2)
DD
DD = read.csv("C:/Users/Taner/Desktop/201910.csv",sep = "",
              fileEncoding = "UTF-8-BOM",header = T,
              colClasses = c("Date","numeric","character","character", "numeric"))
#end===================================================================#

