#=======================================================================#
#�U��u���I=============================================================#
Red=c("�s�_��","�H��","����L","�˳�","����","���q","�_���^","�_��",
      "�_��","ԧ����","�۵P","���w","�ۤs","�h�L","�C��","��s",
      "���v���","���s","���s","�x�_����","�x�j��|","����������",
      "�j�w�˪L����","�j�w","�H�q�w�M","�x�_101/�@�T","�H�s")
Blue=c("�n��i���]","�n��","����","��s��","�ìK","���F��","��������]",
       "��������","�����_��","�����s��","���ɦx","�x�_����","���",
       "�s�s�x","���l�A","�s�H","�O��","����","�ȪF��|","���s","�g��",
       "�ù�","���H")
Orange=c("Ī�w","�T������","�}�פ���","�T�M�ꤤ","�T����p","�j�s",
         "���j","�s��","�Y�e��","���ޮc","�T��","��d","�x�_��","�j���Y",
         "���v���","���s��p","��Ѯc","�Q���n��","�����s��","�F��",
         "�j�F","����","�æw����","���w","�n�ը�")
Green=c("�Q�s","�n�ʤT��","�x�_�p���J","�n�ʴ_��","�Q���n��","���s",
        "�_��","���","�p�n��","����������","�j�F","�x�q�j��","���]",
        "�U��","����","�j�W�L","�C�i","�s���Ϥ���","�s��","�p�Ѽ�")
Brown=c("�n��i���]","�n��n����","�F��","���w","�j�򤽶�","����",
        "��w","���Y","���","�C�n��","�j��","�Q�s����","���s�ꤤ",
        "�n�ʴ_��","�����_��","�j�w","��ޤj��","���i�p","���","����",
        "�U����|","�U�ڪ���","��]","�ʪ���")
Yellow=c("�j�W�L","�Q�|�i","�q�Ծ�","����","���w","���M","���M","����",
         "�O�s","�O��","�s�H����","�Y�e��","����","�s�_���~���")
Yellow2=c("�j�W�L","���w","�O��","�Y�e��")
#======================================================================#
#�e�m�@�~:Ū�Jods_file=================================================#
install.packages("readODS")
library(readODS)
year = c(201701:201712,201801:201812,201901:201910)
ListIn = list();ListOut = list();n=0
for (i in year)
{
  n=n+1
  x = paste0("y",as.character(i))
  read_path = paste0("C:/Users/Taner/Desktop/DM_MRT/",i,"_cht.ods")
  ListIn[[n]] = read_ods(path = read_path,col_names = T,sheet = "�i�����")
  ListOut[[n]] = read_ods(path = read_path,col_names = T,sheet = "�X�����")
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
#function�C��U���`�y�q�P�ƦW==========================================#
#�`�y�q:�i+�X==========================================================#
New_Total = function(IN,OUT)
{
  DF = IN[,-1]+OUT[,-1]
  Sum = numeric();name = names(DF)
    for (i in 1:length(DF)){Sum[i] = sum(DF[[i]])}
  New = data.frame(data.frame(name,Sum)[order(-Sum),] ,rank = c(1:108))
  return(New)
}
#======================================================================#
#�إ��`�M��list_variable===============================================#
List_Total=list();year = c(201701:201712,201801:201812,201901:201910)
for (i in 1:34)
{
  x = paste0("year",as.character(year[i]))
  List_Total[[x=i]] = New_Total(ListIn[[i]],ListOut[[i]])
  #print(head(List_Total[[i]],3))
}
#=====================================================================#
#�H�����U���Ͷ�=======================================================#
TS=data.frame()
for (i in 1:34)
{
  tem = List_Total[[i]][List_Total[[i]]$name == "�H��",]
  TS = rbind(TS,tem)
}
TS = cbind(TS,year=as.character(year),Sum_thd=(TS$Sum/1000))
plot(x=TS$year,y=TS$Sum_thd)
#=====================================================================#
#����L���U���Ͷ�=====================================================#
RF=data.frame()
for (i in 1:34)
{
  tem = List_Total[[i]][List_Total[[i]]$name == "����L",]
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
#======================================================================#
#���տ�J<����>========================================================#
install.packages("ff")
library(ff)
your_data <-read.csv.ffdf(file ='C:/Users/Taner/Desktop/�O�_���B�C����ɦU��OD�y�q�έp���_201701.csv',
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
