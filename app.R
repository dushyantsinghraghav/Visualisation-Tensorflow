library(shiny)
library(ggplot2)
library(treemap)
library(ggiraph)
library(dplyr)
library(packcircles)
library(shinydashboard)
library(shinyBS)
library(viridis)
library(d3Tree)
library(treemapify)
library(tidyverse)
#reading the file space_,mision.csv

pd<-read.csv("D:/Downloads/FIT5147/Data exploration Project/space_mission.csv")
#to remove all the missing values from R 
pd<-pd[!(is.na(pd$Rocket) | pd$Rocket==""), ]
#View(pd)


#creating a dat frame 
#this code demonstrates use of pipe operator and filetr and select functions
pd2<-as.data.frame(pd%>%
                     select(Company.Name,Rocket,Status.Mission,Location)%>%
                     filter(Company.Name=="SpaceX" | Company.Name=="Arianspace" | Company.Name=="NASA" | Company.Name=="ISRO"))
#print(pd2)
#we will plot the different charts on the basis of the radio button clicked value


#we try to plot the stacked bar chart 
pd3<-as.data.frame(pd2 %>%
                     select(Company.Name,Status.Mission,Rocket,Location)%>%
                     
                     filter(pd2$Location=="LC-39A, Kennedy Space Center, Florida, USA" | pd2$Location=="Second Launch Pad, Satish Dhawan Space Centre, India"))

pd5<-as.data.frame(pd%>%
                     select(Company.Name,Rocket,Status.Mission,Location,Status.Rocket)%>%
                     filter(Company.Name=="SpaceX" | Company.Name=="Arianspace" | Company.Name=="Blue Origin" | Company.Name=="ISRO" | Company.Name=="Boeing" | Company.Name=="NASA"))
#we remove all the null values from pd3 as well 
pd3<-pd3[!(is.na(pd3$Rocket) | pd3$Rocket==""), ]
#rocket cost vector  are for stacked bar chart and horizontal bar chart
rocket_cost<-c(as.numeric(pd$Rocket))
#print(typeof(rocket_cost))
View(rocket_cost)
rocket_cost<-na.omit(rocket_cost)
#we create another vector 
rocket_cost2<-c(as.numeric(pd5$Rocket))
rocket_cost2<-na.omit(rocket_cost2)
#View(rocket_cost2)


#we try to plot the stacked bar chart 
pl1<-ggplot(pd,aes(x=Company.Name,y=Rocket,fill=Status.Mission))+geom_bar_interactive(stat="identity")+xlab("Companies")+ylab("their respective missions")+guides(fill=FALSE)+
  ggtitle("stacked bar charts")+
  scale_fill_manual(values=c("blue","red","green","brown"))
pl1
#we try to plot the bubble chart using the geom_point function of ggplot
pl2<-ggplot(pd,aes(x=Company.Name,y=Location,size=Status.Mission,color=Company.Name))+geom_point_interactive(alpha=1)
pl2
#now we create a list of all the above plots
#now we create horizontal bars chart which will is the final realisation 
pl3<-ggplot(pd,aes(x=Company.Name,color=Company.Name,fill=Company.Name))+geom_bar_interactive()
#+scale_fill_manual(values=c("blue","red","green"))
pl3
#we will plot pie chart as mentioned in five design sheet methodology
pl4<-ggplot(pd3,aes(x=Company.Name,y=Rocket,fill=Rocket))+geom_bar(stat="identity",width=2)+xlab("Comapnies")+
  ylab("Their respective rocket launches cost")+  coord_polar("x", start=0) 
pl4


#theme_void() + 
#theme(legend.position="none") 
#we will plot packed circles or packed bubble chart which is one of my layout 
#moreover these packed bubbles are based on Location and Company Name 
#they can be subsequently be plotted for Rockets as well as Satus.Mission
# Create data


# Make the plot packed bubble chart for pd data frame and it can be seen in the shiny app


data2 <- data.frame(group=paste( pd$Company.Name[1:50]), value=pd$Rocket[1:50]) 
#View(data2$value)
data2$text <- paste("name: ",data2$group, "\n", "value:", data2$value, "\n")

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data2$value,sizetype='area')
#View(packing)
# We can add these packing information to the initial data frame
data2 <- cbind(data2, packing)
#View(data2)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg2 <- circleLayoutVertices(packing, npoints=50)

#dat.gg2
#View(dat.gg2)



pl6 <- ggplot() + 
  geom_polygon_interactive(data = dat.gg2, aes(as.numeric(x),as.numeric( y), group = id, fill=id, tooltip = data2$text[id], data_id = id),inherit.aes=FALSE ,colour = "blue", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text( data=data2, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()
pl6

#widg <- ggiraph(ggobj = pl, width_svg = 7, height_svg = 7)
#widg
#plotting more packed bubble charts for more clarification on dataset data
#we first view the pd5 dataset
#the below code is used to plot packed bubble chart for pd5 data frame
#View(pd5)
data3 <- data.frame(group=paste( pd5$Company.Name[1:50]), value=pd5$Rocket[1:50]) 
#View(data2$value)
data3$text <- paste("name: ",data3$group, "\n", "value:", data3$value, "\n")

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data2$value,sizetype='area')
#View(packing)
# We can add these packing information to the initial data frame
data3 <- cbind(data3, packing)
#View(data3)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg3 <- circleLayoutVertices(packing, npoints=50)

#dat.gg2
#View(dat.gg3)




pl11 <- ggplot() + 
  geom_polygon_interactive(data = dat.gg3, aes(as.numeric(x),as.numeric( y), group = id, fill=id, tooltip = data3$text[id], data_id = id),inherit.aes=FALSE ,colour = "blue", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text( data=data2, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()
pl11


pd<-read.csv("D:/Downloads/FIT5147/Data exploration Project/space_mission.csv")

#print(pd2)
#we will plot the different charts on the basis of the radio button clicked value


#we try to plot the stacked bar chart 

#View(pd5)

#we plot treemap for pd data frame when we have the original data 
name<-c(pd$Company.Name)
value<-c(as.numeric(pd$Rocket))
label<-c(pd$Status.Mission)
region<-c(pd$Location)
econ_classification<-c(pd$Status.Mission)
new_data<-data.frame(name,value,label,region,econ_classification)
View(new_data)

pl8<-ggplot(data=new_data,aes(area =value, fill = name,label=name)) +
  geom_treemap() +
  geom_treemap_text(color="yellow",place="center")+
  
  labs(title="Companies and their location")+
  theme(legend.position ="right" )
pl8



#trying to plot a interactive ggplot lets see whether it works or 
#this code is not visible in r shiny just for understanding purpose
pl9<-ggplot(data = pd) +
  geom_col_interactive(aes(x = Company.Name, y = Status.Mission, color = Status.Mission,
                           tooltip = Company.Name, data_id = Company.Name)) +
  xlab("Companies")+
  ylab("Rocket")
ggtitle("COmparison of rocket lauches of three companies")
theme_minimal()
pl9

#treemap using treemap library
#its a static one not plotted in shiny webapp

group <- c(pd$Company.Name)
subgroup <- paste("subgroup" , c(pd$Rocket,pd$Location), sep="-")
value <- as.numeric(c(pd$Rocket))
data3 <- data.frame(group,subgroup,value)

# Custom labels:
pl7<-treemap(data3, index=c("group","subgroup"),     vSize="value", type="index",
             
             fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
             fontcolor.labels=c("white","blue"),    # Color of labels
             fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
             bg.labels=c("transparent"),              # Background color of labels
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             ),                                   # Where to place labels in the rectangle?
             overlap.labels=1,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
             
             
             inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
             
)  






ui <- dashboardPage(
  dashboardHeader(title="Various plots to show visualisation for space missions from 1957"),
  dashboardSidebar( 
    sidebarSearchForm("searchText","buttonSearch","Search"),
    
    menuItem("Space Missions", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Plots", icon = icon("th"), tabName = "Plots",
                              badgeLabel = "Choose", badgeColor = "green"),
    textInput("text_input","search the plot",value="bubble chart"),
    
    sliderInput("rocket","Rocket cost ",min=0,max=max(Rocket_cost),value=25,step=3)
  
    
  ),
  dashboardBody(
  
  
  
 
  

  

  mainPanel(
    tabsetPanel(
      type="tab",
      tabPanel("Data",tableOutput("space_mission"),
      selectInput("count","select the data you want to see",c("Company"="Company.Name","rocket"="Rocket","Country"="Location")),
      fluidRow(
      column(width=8,
             
             br(),
             br(), 
             submitButton("Submit")))),
      tabPanel("Summary",verbatimTextOutput("summ")),
    
      tabPanel("Plot",plotOutput("distPlot",width="900px",height = "500px",
                                 click = "plot_click",
                                 dblclick = "plot_dblclick",
                                 hover = "plot_hover",
                                 brush = "plot_brush"),
      fluidRow(
        column(width=8,
               
               
               
               
               radioButtons("radioInput", "DIfferent plots", choices =
                              c("Stacked bar charts" = "plot 1",
                                "geom point graph" = "plot 2",
                                "horizontal bars" = "plot 3",
                                "Pie chart" = "plot 4",
                                "packed bubbles for pd3"="plot 5",
                                "packed bubbles for pd" = "plot 6",
                                "plot of treemap"= "plot 7",
                                "treemap for pd"="plot 9",
                                "pie chart for pd5"="plot 10",
                                 "stacked  bar  chartfor pd5"="plot 11",
                                "stacked bar chart for pd3"="plot 12",
                                "horizontal bar based on rocket cost"="plot 13"
                                
                                
                                
                              ))),
               
               
               
               
               column(width=8,
                      
                      br(),
                      br(), 
                      submitButton("Submit"))),
      )
      #first main tab ends 
               
    
    
               
               
   ))))


    
    









server <- function(input, output) {
  
  
  #it is used to display the summary of the spacemission data 
  output$summ<-renderPrint({
    #summary(pd)
    #print(head(pd3))
    #using tibble to create a smaller data frame
    
    small_pd<-as_tibble( glimpse(pd3))
    print(small_pd)
  })
  #it is used to display the  data table of space mission database
  output$space_mission<-renderTable({
    pd[1:20,c("Status.Mission","Datum",input$count)]
  })
  
  output$distPlot <- renderPlot({
    
    if (input$radioInput == "plot 1")  {print(pl1)}   
    if (input$radioInput == "plot 2")  {print(pl2)}  
    if (input$radioInput == "plot 3")  {print(pl3)}  
    if (input$radioInput == "plot 4")  {print(pl4)}
    if (input$radioInput=="plot 5"){ 
      data <- data.frame(group=paste( pd2$Company.Name[1:20]), value=pd2$Rocket[1:20]) 
      #View(data$value)
      data$text <- paste("name: ",data$group, "\n", "Rocket price:", data$value)
      
      # Generate the layout. This function return a dataframe with one line per bubble. 
      # It gives its center (x and y) and its radius, proportional of the value
      packing <- circleProgressiveLayout(data$value,sizetype='area')
      #View(packing)
      # We can add these packing information to the initial data frame
      data <- cbind(data, packing)
      View(data)
      
      
      # Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
      # plot(data$radius, data$value)
      
      # The next step is to go from one center + a radius to the coordinates of a circle that
      # is drawn by a multitude of straight lines.
      dat.gg <- circleLayoutVertices(packing, npoints=50)
      dat.gg
      pl5 <- ggplot() + 
        geom_polygon_interactive(data = dat.gg, aes(as.numeric(x), as.numeric(y), group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "red", alpha = 0.6) +
        scale_fill_viridis() +
        geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
        theme_void() + 
        theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
        coord_equal()
      print(pl5)
      paste0(
        "click: ", (input$plot_click),
        "dblclick: ", (input$plot_dblclick),
        "hover: ", (input$plot_hover),
        "brush: ", (input$plot_brush)
      )
      
      }
    if (input$radioInput=="plot 6")  { print(pl6)}

 

    if (input$radioInput=="plot 7")    {print(pl8)}
 
    if (input$radioInput=="plot 9")    {
    
    
    #rendering treemap for pd of different type
    name<-c(pd$Company.Name)
    value<-c(as.numeric(pd$Rocket))
    label<-c(pd$Status.Mission)
    region<-c(pd$Location)
    econ_classification<-c(pd$Status.Mission)
    new_data<-data.frame(name,value,label,region,econ_classification)
    View(new_data)
   
    pl<-ggplot(new_data, aes(area = value, label = name, subgroup = name,
                              subgroup2 = name)) +
      geom_treemap() +
      
      geom_treemap_subgroup2_border(colour = "green", size = 3) +
      geom_treemap_subgroup_border(colour = "red", size = 5) +
      geom_treemap_subgroup_text(
        place = "left",
        colour = "blue",
        alpha = 1,
        grow = F
      ) +
      geom_treemap_subgroup2_text(
        colour = "white",
        alpha = 0.5,
        fontface = "italic"
      ) +
      
      geom_treemap_text(colour = "white", place = "middle", reflow = F)
    
    print(pl)
   
      
      
      
      
     
      }
  
  
  #rendering pie chart for pd5 data frame 
    
  if (input$radioInput=="plot 10"){
    print(ggplot(pd5,aes(x="",y=Company.Name,fill=Rocket))+geom_bar(stat="identity",width=2)+xlab("Companies")+
      ylab("Their respective rocket launches cost")+  coord_polar("x", start=0))
  }
  

  #rendering stacked bar chart
  #this stacked bar chart is meant for visualisaing dat from the pd5 data frame which has 
  #atleast 5 companies along with their locations
  
    
    
    if (input$radioInput=="plot 11")    {
      print(ggplot(pd5,aes(x=Location,y=Rocket,color=Company.Name))+geom_col())
      
    }
    
    #this stacked bar chart for pd3 data frame
    if (input$radioInput=="plot 12")
    {
      print(ggplot(pd3,aes(x=Company.Name,y=Location,color=Company.Name))+geom_col())
    }
  
  

  #rendering horizontal bar chart  based on rocket cost 
  #and taking input from the slider bar 


    
    if (input$radioInput=="plot 13")    {
      
      ggplot(pd3,aes(x=Location,color=Company.Name,fill=Company.Name))+geom_bar_interactive()
      #+scale_fill_manual(values=c("blue","red","green"))
     
    }
    
  
})
    
    }  

shinyApp(ui = ui, server = server)