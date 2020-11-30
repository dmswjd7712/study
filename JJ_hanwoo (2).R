setwd("/Users/eunjeongjeon/Documents/r_studi")

library(pacman)
pacman::p_load("tidyverse", "lubridate", "readxl")

# Data preparation ----

# environment ----

first_select<-c("id", "farm.code", "birth.day", "slaughter.date", "calf.cost", 
                "feed.price", "feed.amount", "finalbw", "cw", "loin",
                "backfat", "marbling", "final.grade", "a.price", "c.price")

text_f<-function(x) gsub(pattern = "\\s",   
                         replacement = "",
                         x = x)

# company A ----

a<-read.csv("a.csv", header = T)
a%>%select(all_of(first_select))%>%
  mutate(id = paste0("00", id))%>%
  mutate(birth.day = as.POSIXct(birth.day),
           slaughter.date = as.POSIXct(slaughter.date))%>%
  mutate(slaughter.month = difftime(slaughter.date, birth.day, units = 'days'))%>%
  mutate(slaughter.month = ceiling(slaughter.month/30))%>%
  mutate(company = "A")->a

# company B ----
b<-read.csv("b.csv", header = T)
b%>%mutate(id = paste0("002", id))->b
b_records <- read_excel("b_records.xls")
b_records%>%mutate(id = text_f(id))%>%
  select(id, slaughter.date)->b_records

left_join(b, b_records, by ="id")%>%select(-"slaughter.date.x")%>%
  mutate(slaughter.date = slaughter.date.y)%>%
  select(-"slaughter.date.y")->b

b%>%mutate(a.price = NA)%>%select(all_of(first_select))%>%
  mutate(birth.day = as.POSIXct(birth.day),
         slaughter.date = as.POSIXct(slaughter.date))%>%
  mutate(slaughter.month = difftime(slaughter.date, birth.day, units = 'days'))%>%
  mutate(slaughter.month = ceiling(slaughter.month/30))%>%
  mutate(company = "B")->b


# company C ----
c<-read.csv("c.csv", header = T)
c%>%select(all_of(first_select))%>%
  mutate(id = text_f(id))%>%
  mutate(birth.day = paste0("20", substr(birth.day, 1, 2),
                                "/", substr(birth.day, 4,5),
                                "/", substr(birth.day, 7,8)))%>%
  mutate(slaughter.date = paste0("20", substr(slaughter.date, 1, 2),
                            "/", substr(slaughter.date, 4,5),
                            "/", substr(slaughter.date, 7,8)))->c

c%>%mutate(birth.day = as.Date(birth.day)%>%as.POSIXct(birth.day, format = "%y%m%d"),
         slaughter.date = as.Date(slaughter.date)%>%as.POSIXct(slaughter.date))%>%
  mutate(slaughter.month = difftime(slaughter.date, birth.day, units = 'days'))%>%
  mutate(slaughter.month = ceiling(slaughter.month/30))%>%
  mutate(company = "C")->c


#company D ----

load("farm_d.rda")
d%>%mutate(feed.amount = NA)->d
d%>%select(all_of(first_select))%>%mutate(birth.day = as.Date(birth.day)%>%as.POSIXct(birth.day, format = "%y%m%d"),
           slaughter.date = as.Date(slaughter.date)%>%as.POSIXct(slaughter.date))%>%
  mutate(slaughter.month = difftime(slaughter.date, birth.day, units = 'days'))%>%
  mutate(slaughter.month = ceiling(slaughter.month/30))%>%
  mutate(company = "D")->d

# combining and saving data ----
rbind(a, b, c, d)->df

save(df, file ="raw_data201023.rda")



# EDA ----
load("raw_data201023.rda")

head(df)
unique(df$farm.code)
df%>%group_by(farm.code)%>%summarise(total = n())

### farm code2 tidy ----
df%>%mutate(farm.code2 = ifelse(farm.code == "1", "F1",
                                ifelse(farm.code == "2", "F2", 
                                       ifelse(farm.code == "3", "F3",
                                              ifelse(farm.code == "4", "F4",
                                                     ifelse(farm.code == "5", "F5",
                                                            ifelse(farm.code == "6", "F6",
                                                                   ifelse(farm.code == "7", "F7",
                                                                          ifelse(farm.code == "8", "F8",
                                                                                 ifelse(farm.code=="9", "F9",
                                                                                        ifelse(farm.code == "10", "F10",
                                                                                               ifelse(farm.code == "박진규", "F11",
                                                                                                      ifelse(farm.code == "은희철", "F12",
                                                                                                             ifelse(farm.code == "이성우", "F13",
                                                                                                                    ifelse(farm.code == "최수연", "F14",
                                                                                                                           ifelse(farm.code == "이강헌", "F15",
                                                                                                                                  ifelse(farm.code == "김민수", "F16",
                                                                                                                                         ifelse(farm.code == "박덕수", "F17",
                                                                                                                                                ifelse(farm.code == "이형진", "F18",
                                                                                                                                                       ifelse(farm.code == "이정민", "F19",
                                                                                                                                                              ifelse(farm.code == "하길명", "F20",
                                                                                                                                                                     ifelse(farm.code == "유남희", "F21",
                                                                                                                                                                            ifelse(farm.code == "이순자", "F22",
                                                                                                                                                                                   ifelse(farm.code == "권오준", "F23",
                                                                                                                                                                                          ifelse(farm.code == "백진태", "F24",
                                                                                                                                                                                                 ifelse(farm.code == "사업장 3동", "F25",
                                                                                                                                                                                                        ifelse(farm.code == "사업장 2동", "F26",
                                                                                                                                                                                                               ifelse(farm.code == "사업장 1동", "F27",
                                                                                                                                                                                                                      ifelse(farm.code == "장문수", "F28",
                                                                                                                                                                                                                             ifelse(farm.code == "김학문", "F29",
                                                                                                                                                                                                                                    ifelse(farm.code == "김형호", "F30",
                                                                                                                                                                                                                                           ifelse(farm.code == "양철섭", "F31", 
                                                                                                                                                                                                                                                  ifelse(farm.code == "황상철", "F32","F33")))))))))))))))))))))))))))))))))->df
save(df, file= "raw_data201023.rda")








# EDA start ----
load("raw_data201023.rda")

df$farm.code2<-factor(df$farm.code2, levels = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",
                                                "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20",
                                                "F21", "F22", "F23", "F24", "F25", "F26", "F27", "F28", "F29", "F30",
                                                "F31", "F32", "F33"))

df%>%ggplot(aes(company, cw, fill = farm.code2))+
  geom_violin(alpha = 0.3)+
  geom_text()
  theme_minimal()

## there are farms have same code with different company (from A and B)
  
df%>%mutate(farm.code3 = paste0(company, farm.code2))->df

'%notin%' <- Negate('%in%')

mean(df$cw, na.rm = T)

df%>%filter(farm.code3 %notin% c("DF21", "DF24"))%>%
  ggplot(aes(farm.code3, cw, fill = company))+
  geom_violin(alpha = 0.3)+
  geom_hline(yintercept = mean(df$cw, na.rm = T))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank())->p1


df%>%filter(farm.code3 %notin% c("DF21", "DF24"))%>%
  ggplot(aes(farm.code3, backfat, fill = company))+
  geom_violin(alpha = 0.3)+
  geom_hline(yintercept = mean(df$backfat, na.rm = T))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank())->p2

df%>%filter(farm.code3 %notin% c("DF21", "DF24"))%>%
  ggplot(aes(farm.code3, loin, fill = company))+
  geom_violin(alpha = 0.3)+
  geom_hline(yintercept = mean(df$loin, na.rm = T))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank())->p3


# mabling

df%>%mutate(mabling2 = substr(mabling, 1, 1))->df

df%>%mutate(mabling2 = as.numeric(mabling2))%>%summarise(mean = mean(mabling2, na.rm = T))

df%>%mutate(marbling2 = as.numeric(marbling2))%>%
  filter(farm.code3 %notin% c("DF21", "DF24"))%>%
  ggplot(aes(farm.code3, marbling2, fill = company))+
  geom_boxplot(alpha = 0.3)+
  geom_hline(yintercept = 6, color = "red")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6))->p4



library(cowplot)

ggsave(plot_grid(p1, p2, p3, p4, ncol = 1), file = "figure1.png", width = 9, height = 8, dpi = 300)


# EDA 2 (2020.11.11)_jay

names(df)[names(df)=="mabling2"]="marbling2" #특정열이름바꾸기

## 최종등급 판정-> 경락가격(2019) -> 도체가격 계산
## final.grade2 <-yield index (YI2), marbling score (grade4), auction price (A.price2)

df<-df %>% 
  mutate(YI2= ((11.06398 - 1.25149*backfat + 0.28293*loin + 0.56781*cw)/cw)*100) %>% 
  mutate(grade2 = ifelse(YI2>=62.52, "A", ifelse(60.40<YI2, "C", "B"))) %>%
  mutate(grade4 = ifelse(marbling2 %in% c(1), "3", ifelse(marbling2 %in% c(2,3), "2", 
                                                          ifelse(marbling2 %in% c(4,5), "1", 
                                                                 ifelse(marbling2 %in% c(6), "1+", 
                                                                        ifelse(marbling2 %in% c(7,8,9), "1++", 
                                                                               "")))))) %>%
  mutate(final.grade2 = paste0(grade2,grade4)) %>%
  mutate(a.price2 = ifelse(final.grade2=="A1++", 22465, 
                           ifelse(final.grade2 =="B1++", 21347, 
                                  ifelse(final.grade2 =="C1++", 19990, 
                                         ifelse(final.grade2 =="A1+", 20829, 
                                                ifelse(final.grade2 =="B1+", 20096, 
                                                       ifelse(final.grade2 =="C1+", 18833, 
                                                              ifelse(final.grade2 =="A1", 18998, 
                                                                     ifelse(final.grade2 =="B1", 18395, 
                                                                            ifelse(final.grade2=="C1", 17236,
                                                                                   ifelse(final.grade2 =="A2", 16000,
                                                                                          ifelse(final.grade2=="B2", 15387, 
                                                                                                 ifelse(final.grade2 =="C2", 14249,
                                                                                                        ifelse(final.grade2=="A3", 13093,ifelse(final.grade2 =="B3", 11566,
                                                                                                                                                ifelse(final.grade2=="C3", 10219, ""))))))))))))))))

## A.price2 (character->integer)
df$a.price2<- as.integer(df$a.price2)
df$marbling2<- as.integer(df$marbling2)


## farm.code3에 따른 분류-> 4개 도체특성 & CV 관계 살펴보기


dif2 <- function(x,y){return(exp(x-y))} #함수

df%>%
  filter(final.grade2 != "NA")%>%
  mutate(c.price2= a.price2*cw/10000)%>%
  group_by(farm.code3)%>%
  summarise(total =n(), cw= mean(cw), bf= mean(backfat), loin= mean(loin), marb= mean(marbling2),
            prm= mean(c.price2), prmd= median(c.price2), prsd= sd(c.price2))%>%
  na.omit(df)%>%
  mutate(cv = prsd/prm*100, cv2 = prsd/prmd*100, dif = prmd-prm, dif2 = dif2(prmd,prm))%>%
  filter(cv <40)%>%
  mutate(group = ifelse(cv <= quantile(cv, 0.25),'lower',
                        ifelse(cv > quantile(cv, 0.75),'higher',"median")))%>%
  mutate(company = substr(farm.code3, 1,1))->df3

df3$group<-factor(df3$group, levels = c("lower", "median", "higher"))

##참고 line992 cw 43->430으로 수정해야함 

df3%>%
  ggplot(aes(dif3, mean))+
  geom_point()+
  stat_smooth()

summary(lm(mean~dif3, data = df3))
plot(df3[,3:9])
summary(df3$cv)


## boxplot

fun_mean <- function(x){return(data.frame(y=mean(x),
                                          label=round(mean(x,na.rm=T), digit = 2)))}

df3%>%
  ggplot(aes(x = group, y= cw))+
  geom_boxplot(fill= "grey",alpha =0.5)+
  stat_summary(fun = mean, geom="point",colour="dark red", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  theme_minimal()+
  ylab("Carcass weight, kg")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->a1

df3%>%
  ggplot(aes(x = group, y= loin))+
  geom_boxplot(fill= "grey",alpha =0.5)+
  stat_summary(fun = mean, geom="point",colour="dark red", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3)+
  theme_minimal()+
  ylab(bquote(.("Loin area, ")*cm^2))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->a2

df3%>%
  ggplot(aes(x = group, y= bf))+
  geom_boxplot(fill= "grey",alpha =0.5)+
  stat_summary(fun = mean, geom="point",colour="dark red", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.3)+
  theme_minimal()+
  xlab("Coefficient of variation for carcass price")+
  ylab('Backfat thickness, mm')+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->a3

df3%>%
  ggplot(aes(x =group , y= marb))+
  geom_boxplot(fill= "grey",alpha =0.5)+
  stat_summary(fun = mean, geom="point",colour="dark red", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.0)+
  theme_minimal()+
  xlab("Coefficient of variation for carcass price")+
  ylab('Marbling score')+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->a4

plot_grid(a1,a2,a3,a4, ncol = 2, align = 'v')->he.fig1
ggsave(he.fig1, file = "he.fig1.png", width = 8.5, height = 9.0, dpi =300)

## barplot

df3%>%
  group_by(group, company)%>%
  summarise(total =n(), m= mean(cw))%>%
  mutate(p = total/sum(total)*100)%>%
  ggplot(aes(x=group, y=p, fill=company))+
  geom_bar(stat = "identity", alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_text(aes(label = paste0(company, " (", round(p, digits = 0), "%", ")")), 
            position = position_stack(vjust = 0.5), size =4)+
  xlab("Coefficient of variation for carcass price")+
  ylab("Distribution by company, %")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->he.fig2

ggsave(he.fig2, file = "he.fig2.png", width = 6.5, height = 6.0, dpi =300)


## 안된다..
df3%>%
  group_by(group, company)%>%
  summarise(total =n(), m= mean(loin))%>%
  mutate(p = total/sum(total)*100)+
  ggplot(aes(x=group, y=m, fill=company))+
  geom_bar(stat = "identity", alpha = 0.6)+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_text(aes(label = paste0(company, " (", round(p, digits = 0), "%", ")")), 
            position = position_stack(vjust = 0.5), size =4)+
  xlab("")+ylab("Loin area")


## scatter plot

df3%>%
  ggplot(aes(x=group, y=cw, color=company))+
  geom_jitter(stat = "identity", alpha = 0.6, size=3)+
  xlab("Coefficient of variation for carcass price")+
  ylab("Carcass weight, kg")+
  theme_minimal()+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->he.fig3


df3%>%
  ggplot(aes(x=group, y=loin, color=company, size=3))+
  geom_jitter(stat = "identity", alpha = 0.6, size=3)+
  xlab("Coefficient of variation for carcass price")+
  ylab(bquote(.("Loin area, ")*cm^2))+
  theme_minimal()+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->he.fig4


df3%>%
  ggplot(aes(x=group, y=bf, color=company))+
  geom_jitter(stat = "identity", alpha = 0.6, size=3)+
  xlab("Coefficient of variation for carcass price")+
  ylab("Backfat thickness, mm")+
  theme_minimal()+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->he.fig5


df3%>%
  ggplot(aes(x=group, y=marb, color=company))+
  geom_jitter(stat = "identity", alpha = 0.6, size=3)+
  xlab("Coefficient of variation for carcass price")+
  ylab("Marbling score")+
  theme_minimal()+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))->he.fig6


ggsave(he.fig3, file = "he.fig3.png", width = 6.5, height = 6.0, dpi =300)
ggsave(he.fig4, file = "he.fig4.png", width = 6.5, height = 6.0, dpi =300)
ggsave(he.fig5, file = "he.fig5.png", width = 6.5, height = 6.0, dpi =300)
ggsave(he.fig6, file = "he.fig6.png", width = 6.5, height = 6.0, dpi =300)








