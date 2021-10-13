library(shiny)
library(bs4Dash)
library(shinyauthr)
library(mongolite)
library(jsonlite)
library(shinyjs)
library(echarts4r)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
connection_string = 'mongodb+srv://Agulhas:Agulhas@cluster0.xa1nz.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
collection = mongo(collection="Users", db="Yolla", url=connection_string)
user_base<-collection$find('{}')

ui = dashboardPage(
  header<-dashboardHeader(
    title = dashboardBrand(
      title = "qrson",
      image = "https://play-lh.googleusercontent.com/K5mfnOxNAknw9-EUlWtg-yzDs8vsny_ri_Nn5nEc51-I9fFh3rv6-k2rTTEk_rGjjA=s180-rw"
    )),
  sidebar <- bs4DashSidebar(id = "sidebarmain",
                            bs4SidebarMenu(
                              bs4SidebarMenuItem(text ="Anasayfa", tabName = "Main", newTab = FALSE,startExpanded = T,bs4SidebarMenuSubItem(text ="Ayrıntılar",tabName = "Main2",newTab = FALSE,selected = T)),
                              
                              bs4SidebarMenuItem(text ="Gelir Analizleri",startExpanded = T,
                                                 # bs4SidebarMenuSubItem(text ="Saatlere Göre Yoğunluk",tabName = "Hourly_population",newTab = FALSE),
                                                 # bs4SidebarMenuSubItem(text ="Haftalık Yoğunluk Oranları",tabName = "Weekly_Ratio",newTab = FALSE),
                                                 bs4SidebarMenuSubItem(text ="Aylık Gelir",tabName = "Monthly_Income",newTab = FALSE), # a timeseries for total earnings monthly period timeline must be changable 
                                                 bs4SidebarMenuSubItem(text ="Günlük Gelirler",tabName = "Daily_Income",newTab = FALSE), #a timeseries for total earnings with last 30 days timeline must be changable 
                                                 bs4SidebarMenuSubItem(text ="İptal/Satış Oranları",tabName = "Canceled_ratio",newTab = FALSE), #Piechart for canceled / order ratio
                                                 bs4SidebarMenuSubItem(text ="Ödeme Biçimleri",tabName = "Payment_Types",newTab = FALSE), # Piechart for payment types 
                                                 # bs4SidebarMenuSubItem(text ="Müsteri/Kazanç Analizleri",tabName = "Consumer_Analysis",newTab = FALSE), # a line plot for total earning divided with total number of customers
                                                 bs4SidebarMenuSubItem(text ="İndirim Kayıpları",tabName = "Lost_Of_Discount_Analysis",newTab = FALSE) # in a infobox most discount product will be serve and a barplot with discount amounth 
                              ),
                              
                              bs4SidebarMenuItem(text ="Ürünlerin analizleri",tabName = "Products",startExpanded = T,
                                                 bs4SidebarMenuSubItem(text ="Toplam Satış Adetleri", tabName = "Total_amounth_for_each_product", newTab = FALSE), 
                                                 bs4SidebarMenuSubItem(text ="Toplam Satış Tutarları", tabName = "Total_income_for_each_product", newTab = FALSE),#it will be a barplot for total selling for each month
                                                 bs4SidebarMenuSubItem(text ="Teslimat Süreleri",tabName = "Order_deliver_time",newTab = FALSE), #for each product there are three infobox which are included
                                                 #mean maximum and minimum delivery time for choosed products
                                                 
                                                 bs4SidebarMenuSubItem(text ="En Popüler Ürünler",tabName = "Most_Popular_products",newTab = FALSE)
                                                 
                              ),
                              bs4SidebarMenuItem("Restorant Analizleri",tabName = "Restourant",startExpanded = T,
                                                 bs4SidebarMenuSubItem(text ="Masaların Satış Durumları",tabName = "Tables_Incomes_ratio",newTab = FALSE),
                                                 bs4SidebarMenuSubItem(text ="En Çok Tercih Edilen Masalar",tabName = "Most_popular_spots",newTab = FALSE),
                                                 bs4SidebarMenuSubItem(text ="Masalar-Geçirilen Vakitler",tabName = "Time_Consumer_Tables",newTab = FALSE),
                                                 # bs4SidebarMenuSubItem(text ="Ortalama vakit Analizleri",tabName = "Time_spending_Analysis",newTab = FALSE),
                                                 bs4SidebarMenuSubItem(text ="Personel Performansları",tabName = "Workers_performance",newTab = FALSE)
                              )
                            )
  ) ,
  boardbody<-dashboardBody(shinyauthr::loginUI("login",user_title = "Kullanıcı Adı",title = "Üye Girişi",pass_title = "Parola"),
                           
                           fluidRow(
                             box(id = "infobox",closable = TRUE,width = 12,
                                 
                                 fluidRow(
                                   bs4InfoBoxOutput("Thirty_Day_Income_Infobox2",width = 4),
                                   bs4InfoBoxOutput("Seven_Day_Income_Infobox3",width = 4),
                                   bs4InfoBoxOutput("Daily_Income_Infobox1",width = 4)
                                 ),
                                 
                                 fluidRow(
                                   bs4InfoBoxOutput("Most_Popular_Item",width = 4),
                                   bs4InfoBoxOutput("Mean_Order_Time",width = 4),
                                   bs4InfoBoxOutput("Max_Order_Time",width = 4)
                                   
                                 ),
                                 
                                 fluidRow(
                                   bs4InfoBoxOutput("Most_Popular_Table",width = 4),
                                   bs4InfoBoxOutput("Mean_Table_Time",width = 4),
                                   bs4InfoBoxOutput("Employee_Of_The_Month",width = 4)
                                 )
                             )
                           ),  
                           fluidRow(
                             box(id="plotbox",title = uiOutput("boxtitle"),closable = F,width = 12,
                                 tabItems(
                                   tabItem(
                                     tabName = "Main2",
                                     fluidRow(box(id = "box1",title = "Saatlik Yoğunluklar",echarts4rOutput("Hourly_Popularity"),width = 12),
                                              box(id = "box2",title = "Haftalık Yoğunluklar", echarts4rOutput("Weekly_Popularity"),width = 12)
                                     )
                                   ), 
                                   tabItem(
                                     tabName = "Daily_Income",
                                     box(title =  "Günlük Satış Gelirleri",fluidRow(column(9,echarts4rOutput("Last_Thirty_Days_timeseries")),column(3,uiOutput("time"))),width = 12)
                                     
                                   ),
                                   tabItem(
                                     tabName = "Monthly_Income",
                                     box(title="Aylık Satış Gelirleri",echarts4rOutput("All_Time_Monthly_timeseries"),width = 12)# done
                                   ),
                                   tabItem(
                                     tabName = "Canceled_ratio",
                                     box(title="İptal-Sipariş Oranları",echarts4rOutput("Cancel_order_pie"),width = 12)# done
                                     
                                   ),
                                   tabItem(
                                     tabName = "Payment_Types",
                                     box(title="Ödeme Biçimleri",echarts4rOutput("Payment_Type_pie"),width = 12)
                                     # done
                                   ),
                                   tabItem(
                                     tabName = "Lost_Of_Discount_Analysis",
                                     box(title="İndirim-Satış Geliri" ,echarts4rOutput("Discount_loss_Analysis"),width = 12)# done
                                     
                                   ),
                                   tabItem(
                                     tabName = "Total_amounth_for_each_product", #done
                                     box(title = "Aylık Satış Miktarları",fluidRow(column(9,echarts4rOutput("Each_Product_Earning_Total")),column(3,uiOutput("Product_select"))),width = 12)
                                     
                                     
                                   ),
                                   tabItem(
                                     tabName = "Total_income_for_each_product", #done
                                     box(title = "Aylık Satış Gelirleri",fluidRow(column(9,echarts4rOutput("Each_Product_Earning_Total2")),column(3,uiOutput("Product_select2"))),width = 12)
                                     
                                     
                                   ),
                                   tabItem(
                                     tabName = "Order_deliver_time", #done
                                     box(title = "Siparişlerin İletilme Süreleri",fluidRow(column(9,echarts4rOutput("Order_Deliver_Time_Bar_intervals")),column(3,uiOutput("Product_select3"))),width = 12)
                                     
                                   ),
                                   tabItem(
                                     tabName = "Most_Popular_products",  #done
                                     box(title = "En Çok Satan 10 Ürün",echarts4rOutput("Ten_Most_selled_Item"),width = 12)
                                   ),
                                   tabItem(
                                     tabName = "Tables_Incomes_ratio",
                                     box(title = "Masaların Gelir Miktarları",echarts4rOutput("Tables_Incomes"),width = 12)
                                   ),
                                   tabItem(
                                     tabName = "Most_popular_spots",
                                     box(title= "Masaların Kullanılma Miktarları" ,echarts4rOutput("Tables_Popularity"),width = 12)
                                   ),
                                   tabItem(
                                     tabName = "Time_Consumer_Tables",
                                     box(title = "Masaların Kullanım Süreleri",echarts4rOutput("Tables_Spended_Times"),width = 12)
                                   ),
                                   tabItem(
                                     tabName = "Workers_performance",
                                     box(title = "Çalışanların Aldıkları Sipariş Miktarları",echarts4rOutput("Employee_performance"),width = 12)
                                   )
                                 )
                             )
                           )
  )
  
)

server = function(input, output) { 
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sessionid_col = sessionid,
    log_out = reactive(logout_init())
  )
  
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
  })
  
  
  observeEvent(credentials()$user_auth,{
    if(credentials()$user_auth == "FALSE"){
      shinyjs::hide(id="infobox")
    }else{
      shinyjs::show(id="infobox")
    }
    
    
    
  })
  observeEvent(credentials()$user_auth,{
    if(credentials()$user_auth == "FALSE"){
      shinyjs::hide(id="plotbox")
    }else{
      shinyjs::show(id="plotbox")
    }
    
  })
  
  observeEvent(credentials()$user_auth,{
    if(credentials()$user_auth == "FALSE"){
      shinyjs::hide(id="sidebarmain")
    }else{
      shinyjs::show(id="sidebarmain")
    }
    
  })
  
  reader_order <- reactiveFileReader(intervalMillis = 10000000, session = NULL,filePath = 
                                       "./Orders_Raw.csv", readFunc = read.csv,header = T,fill=T,sep = ",",fileEncoding = "UTF8")
  
  filtered_reader_df <- reactive({
    df <- reader_order() %>% filter(restName ==user_info()$name)
    df <- df[-1,]
    df$orderTime<-as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$sendTime<-as.POSIXct(df$sendTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$closedAt<-as.POSIXct(df$closedAt,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$day <- weekdays(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$week<-week(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$daynummonth<-day(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$mnth<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$mnthlabel<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"),label = T)
    df$year<-year(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$hour<-hour(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df
  })
  reader_corder <- reactiveFileReader(intervalMillis = 1000, session = NULL,filePath = "./Corders_Raw.csv", readFunc = read.csv,header = T,fill=T,sep = ",",fileEncoding = "UTF8")
  
  
  filtered_readerc_df <- reactive({
    df <- reader_corder() %>% filter(restName ==user_info()$name)
    df <- df[-1,]
    df$orderTime<-as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$sendTime<-as.POSIXct(df$sendTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$closedAt<-as.POSIXct(df$closedAt,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
    df$day <- weekdays(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$week<-week(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$daynummonth<-day(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$mnth<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$mnthlabel<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"),label = T)
    df$year<-year(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df$hour<-hour(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
    df
  })
  
  
  
  output$Thirty_Day_Income_Infobox2 <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()
    df <- df %>% filter(isPaid == "True") 
    df<- df%>% filter(closedAt>Sys.time()-2592000,closedAt<Sys.time())
    value<-sum(df$itemVal)
    bs4InfoBox("Aylık Kazanç",paste0(value," tl"), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$Daily_Income_Infobox1 <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    start<-as.POSIXct(paste(str_split(as.character(Sys.time()),pattern = " ")[[1]][1],"00:00:00"),sep="")
    df<- df%>% filter(closedAt>start,closedAt<Sys.time())
    value<-sum(df$itemVal)
    bs4InfoBox("Günlük Kazanç",paste0(value," tl"), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$Seven_Day_Income_Infobox3 <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    df<- df%>% filter(closedAt>Sys.time()-604800,closedAt<Sys.time())
    value<-sum(df$itemVal)
    bs4InfoBox("Haftalık Kazanç",paste0(value," tl"), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$Most_Popular_Item <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-df%>%count(item,sort = T)
    bs4InfoBox("En Popüler Ürün",paste0(dff$item[1]), icon = icon("list"),
               color = "orange", fill = TRUE
    )
    
    
  })
  output$Mean_Order_Time <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-df %>% select(orderTime,sendTime)
    dff<-drop_na(dff)
    value<-round(as.numeric(mean(dff$sendTime-dff$orderTime))/60,1)
    bs4InfoBox("Ortalama Sipariş Süresi",paste0(value," Dakika"), icon = icon("list"),
               color = "orange", fill = TRUE
    )
    
    
  })
  output$Max_Order_Time <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-df %>% select(orderTime,sendTime)
    dff<-drop_na(dff)
    value<-round(as.numeric(max(dff$sendTime-dff$orderTime))/60,1)
    bs4InfoBox("Maximum Sipariş Süresi",paste0(value," Dakika"), icon = icon("list"),
               color = "orange", fill = TRUE
    )
    
    
  })
  
  
  output$Most_Popular_Table <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-distinct(df,closedAt,.keep_all = TRUE)
    dff<-dff%>%count(table,sort = T)
    value<-toupper(dff$table[1])
    bs4InfoBox("En Popüler Masa",paste0("Masa=",value), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$Mean_Table_Time <- renderbs4InfoBox({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-distinct(df,closedAt,.keep_all = TRUE)
    dff<-dff %>% select(orderTime,closedAt)
    dff<-drop_na(dff)
    value<-round(as.numeric(mean(dff$closedAt-dff$orderTime))/60,1)
    bs4InfoBox("Ortalama Hizmet Süresi",paste0(value," Dakika"), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$Employee_Of_The_Month <- renderbs4InfoBox({  
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    times<-str_split(Sys.time(),pattern = "-")[[1]][1:2]
    start<-as.POSIXct(start<-paste(times[1],times[2],"01",sep="-"))
    df<-df%>%filter(closedAt>start,closedAt<Sys.time())
    dff<- df %>% count(sentBy,sort=T) 
    dff[dff==""] <- NA
    dff<-drop_na(dff)
    bs4InfoBox("Ayın Elemanı",paste0(dff$sentBy[1]," ","İşlem:",dff$n[1]), icon = icon("list"),
               color = "warning", fill = TRUE
    )
    
    
  })
  
  output$time <- renderUI({
    req(credentials()$user_auth)
    dateRangeInput("date", label = h6("Tarih"),start = as.POSIXct(Sys.time())-2592000,end = Sys.time() )
    
    
  })
  
  rv <- reactiveValues()
  
  observe({
    
    rv$time <- input$date
    
  })
  
  
  
  output$Last_Thirty_Days_timeseries<-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df()
    df<- df%>% filter(closedAt>rv$time[1],closedAt<rv$time[2])
    dff<- df %>% group_by(year,mnth,mnthlabel,daynummonth) %>% summarise(total = sum(itemVal))
    date<-paste(dff$daynummonth,dff$mnth,dff$year,sep = "-")
    dff$time<-date
    dff %>% group_by(mnthlabel)%>%
      e_charts(time)%>%
      e_legend(show = F)%>%
      
      e_line(total) %>%
      e_tooltip() 
  })
  
  
  output$All_Time_Monthly_timeseries <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    dff<- df %>% group_by(year,mnth,mnthlabel) %>% summarise(total = sum(itemVal))
    date<-paste(dff$mnth,dff$year,sep = "-")
    dff$time<-date
    dff %>% group_by(mnthlabel)%>%
      e_charts(time)%>%
      e_bar(total) %>%
      e_tooltip() 
    
  })
  output$Hourly_Popularity <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df<-distinct(df,closedAt,.keep_all = TRUE)
    hours<-paste(df$hour,":00",sep = "")
    df$hours<-hours
    
    dff<-df%>% count(hour,hours,sort = F)
    dff %>% group_by(hour)%>%
      e_charts(hours)%>%
      e_bar(n) %>%
      e_tooltip() 
    
  })
  output$Weekly_Popularity <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    dff<-df%>% count(day,sort = T)
    dff %>% 
      e_charts(day)%>%
      e_pie(n) %>%
      e_flip_coords()%>%
      e_tooltip() 
    
  })
  
  
  output$Cancel_order_pie <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    dfc<-filtered_readerc_df()
    counts<-c(length(dfc$X),length(df$X))
    cnames<-c("İptal Sipariş","Tamamlanan Sipariş")
    dff<-data.frame(cnames,counts)
    
    dff %>% 
      e_charts(cnames)%>%
      e_pie(counts) %>%
      e_color(
        c("#84A59D","#F28482"))%>%
      e_tooltip() 
    
  })
  
  output$Payment_Type_pie <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<-df %>% count(paidBy)
    a<-dff$paidBy
    a<-gsub("cash","Nakit",a)
    a<-gsub("card","Kredi Kartı",a)
    dff$paidBy<-a
    dff %>% 
      e_charts(paidBy)%>%
      e_pie(n) %>%
      e_color(
        c("#EE6C4D","#293241"))%>%
      e_tooltip() 
    
  })
  output$Discount_loss_Analysis <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% group_by(year,mnth,mnthlabel) %>% summarise(Gelirler = sum(itemVal))
    date<-paste(dff$mnth,dff$year,sep = "-")
    dff$time<-date
    df_discount<- df %>%filter(discount =="True")
    df_discount<- df_discount %>% filter(discountPercentage > 0)
    df_discount<-drop_na(df_discount)
    discount_amounth<-((df_discount$itemVal/(1-(abs(df_discount$discountPercentage)/100)))-df_discount$itemVal)
    df_discount$dis_amounth<-discount_amounth
    dff2<- df_discount %>% group_by(year,mnth,mnthlabel) %>% summarise(Gelirler = sum(dis_amounth,na.rm = T))
    list<-list()
    for (i in 1:length(dff2$mnthlabel)) {
      discountless_total<-(dff %>% filter(mnthlabel == dff2$mnthlabel[i]))[1,4]+dff2$Gelirler[1]
      rdf<-dff %>% filter(mnthlabel == dff2$mnthlabel[i])  
      rdf$Indirimsiz<-discountless_total$Gelirler[1]          
      list[[i]]<-rdf
    }
    last<-rbindlist(list)
    last%>% 
      e_charts(time)%>%
      
      e_bar(Gelirler) %>%
      e_bar(Indirimsiz)%>%
      e_labels()%>%
      e_color(
        c("#355070","#6D597A"))%>%
      e_tooltip() 
    
  })
  
  rv2<-reactiveValues()
  rv3<-reactiveValues()
  rv4<-reactiveValues()
  observe({
    rv2$Product<-input$select
  })
  
  observe({
    rv3$Product<-input$select2
  })
  
  observe({
    rv4$Product<-input$select3
  })
  
  output$Product_select<- renderUI({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    product_list<-levels(as.factor(df$item))
    selectInput("select", label = h6("Ürün Seçiniz"), 
                choices = product_list)
  })
  
  
  output$Product_select2<- renderUI({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    product_list<-levels(as.factor(df$item))
    selectInput("select2", label = h6("Ürün Seçiniz"), 
                choices = product_list)
  })
  
  output$Product_select3<- renderUI({
    req(credentials()$user_auth)
    df<-filtered_reader_df()#restoran filtresi silinecek
    product_list<-levels(as.factor(df$item))
    selectInput("select3", label = h6("Ürün Seçiniz"), 
                choices = product_list)
  })
  
  
  # düzenlenecek
  output$Each_Product_Earning_Total <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% group_by(year,mnth,mnthlabel,item) %>% summarise(Miktar = sum(itemCount))
    dff$date<-paste(dff$mnth,dff$year,sep = "-")
    
    dff<-dff %>% filter(item == rv2$Product)
    
    dff %>% group_by(item) %>%
      e_charts(date)%>%
      e_line(Miktar) %>%
      e_color(
        c("#780116"))%>%
      e_tooltip() 
    
  })
  
  output$Each_Product_Earning_Total2 <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% group_by(year,mnth,mnthlabel,item) %>% summarise(Gelir = sum(itemVal))
    dff$date<-paste(dff$mnth,dff$year,sep = "-")
    
    dff<-dff %>% filter(item == rv3$Product)
    
    dff %>% group_by(item) %>%
      e_charts(date)%>%
      e_bar(Gelir) %>%
      e_labels()%>%
      e_color(
        c("#F07167"))%>%
      e_tooltip() 
    
  })
  
  output$Order_Deliver_Time_Bar_intervals <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    df <- df%>% dplyr::select(orderTime,sendTime,item,sentBy)
    df<-drop_na(df)
    df$interval<-as.numeric((df$sendTime-df$orderTime)/60)
    dff<-df %>% filter(item == rv4$Product)
    steps<-c(0,1,5,15,30,1923098190283)
    step_name<-c("0-1dk","1-5dk","5-15dk","15-30dk",">30dk")
    list1<-list()
    for (i in 1:5) {
      rdf<-dff %>% filter(interval > steps[i], interval< steps[i+1])
      n<-length(rdf$orderTime)
      list1[[i]]<-data.frame("Adet"=n,Zaman=step_name[i],"Ürün"=rv4$Product)
    }
    last<- rbindlist(list1)
    
    last %>% 
      e_charts(Zaman)%>%
      e_bar(Adet) %>%
      e_labels()%>%
      e_color(
        c("#14746F"))%>%
      e_tooltip() 
  })
  
  
  output$Ten_Most_selled_Item <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% count(item,sort = T)
    dff<-dff[1:10,]
    colnames(dff)<-c("item","Adet")
    dff  %>% group_by(item) %>%
      e_charts(item)%>%
      e_legend(show = T)%>%
      e_bar(Adet) %>%
      e_labels()%>%
      e_flip_coords()%>%
      e_tooltip() 
    
  })
  
  output$Tables_Incomes <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% group_by(table) %>% summarise(Gelir = sum(itemVal))
    colnames(dff)<-c("Masa","Gelir(TL)")
    dff  %>% 
      e_charts(Masa)%>%
      e_legend(show = T)%>%
      e_color(
        c("#F28482"))%>%
      e_bar(Gelir(TL)) %>%
      e_flip_coords()%>%
      e_tooltip() 
    
  })
  
  
  output$Tables_Spended_Times <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    aa<-distinct(df,closedAt,.keep_all = TRUE)
    aa$interval<-as.numeric((aa$closedAt-aa$orderTime)/3600)
    dff<- aa %>% group_by(table) %>% summarise(Zaman = sum(interval))
    colnames(dff)<-c("Masa","Zaman(Saat)")
    dff  %>% 
      e_charts(Masa)%>%
      e_legend(show = T)%>%
      e_bar(Zaman(Saat)) %>%
      e_flip_coords()%>%
      e_color(
        c("orange"))%>%
      e_tooltip() 
    
  })
  
  output$Tables_Popularity <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    aa<-distinct(df,closedAt,.keep_all = TRUE)
    dff<- aa %>% count(table) 
    colnames(dff)<-c("Masa","ToplamAdisyon(adet)")
    dff  %>% 
      e_charts(Masa)%>%
      e_legend(show = T)%>%
      e_bar(ToplamAdisyon(adet)) %>%
      e_flip_coords()%>%
      e_color(c("purple"))%>%
      e_tooltip() 
    
  })
  
  output$Employee_performance <-renderEcharts4r({
    req(credentials()$user_auth)
    df<-filtered_reader_df() #restoran filtresi silinecek
    df <- df %>% filter(isPaid == "True") 
    dff<- df %>% count(sentBy,sort=T) 
    colnames(dff)<-c("Masa","Adet")
    dff[dff==""] <- NA
    dff<-drop_na(dff)
    
    dff  %>% 
      e_charts(Masa)%>%
      e_legend(show = T)%>%
      e_bar(Adet) %>%
      e_flip_coords()%>%
      e_color(
        c("green"))%>%
      e_tooltip() 
    
  })
  
} 

shinyApp(ui = ui,server = server)
