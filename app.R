library(REdaS)
library(shiny)
library(ggplot2)
library(htmltab)
library(readr)
library(dplyr)
sc_bips = read.csv("statcastAugust2019.csv")
sc_bips = sc_bips[!is.na(sc_bips$hit_distance_sc),]
model4 = lm(sqrt(hit_distance_sc) ~ I((launch_speed/2.237)^2*sin(2*deg2rad(launch_angle))), data = sc_bips)
stadiums = htmltab("https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums",2)
stadiums$`Distance to center field` = parse_number(stadiums$`Distance to center field`)
stadiums = stadiums %>% arrange(Team)
nrow(stadiums)

model5 = lm(sqrt(hit_distance_sc) ~ I((launch_speed/2.237)^2*sin(2*deg2rad(launch_angle))) + as.factor(balls) + as.factor(strikes), data = sc_bips)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hit Simulation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Stadium","Stadium",choices=paste(stadiums$Team,", ",stadiums$Name,sep="")),
      #selectInput("Stadium","Stadium",stadiums$Team,selected = NULL),
      sliderInput("Speed",
                  "Speed (MPH)",
                  min = 10,
                  max = 110,
                  value = median(sc_bips$launch_speed, na.rm=T),
                  post=" MPH"),
      sliderInput("Angle",
                  "Angle",
                  min = -90,
                  max = 90,
                  value = median(sc_bips$launch_angle, na.rm=T),
                  post="°")
    ),
    mainPanel(plotOutput("distPlot"),br(),
              plotOutput("scatPlot"),
              sliderInput("cl",
                          "Confidence Level",
                          min = 1,
                          max = 99,
                          value = 95,
                          post="%"))
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hr = subset(stadiums, Team == strsplit(input$Stadium,",")[[1]][1])$`Distance to center field`
    dist = (predict(model4, data.frame(launch_speed=input$Speed, launch_angle=input$Angle)) + rnorm(300000,0,summary(model4)$sigma))^2
    h = hist(dist,200, plot = F)
    plot(h,col=c("red","white")[ifelse(h$breaks >= hr, 1, 2)], 
         main = "Simulation of 300,000 hits", xlab = paste("Distance (feet)\nHome Run Probability:",round(mean(dist>hr),3)), ylab = "Frequency", 
         xlim = c(min(h$breaks),ifelse(hr+20<max(h$breaks),max(h$breaks),hr+20)))
    abline(v=hr, col = "red", lty="dotted")
    #text(max(h$breaks)-30,max(h$counts)-100,paste("Home Run\nProbability:",round(mean(dist>hr),3)))
    text(hr, max(h$counts)-300, paste("Distance to\ncenter field\n",hr," ft.",sep=""), col = "red")
  })
  output$scatPlot <- renderPlot({
    speed = input$Speed
    angle = input$Angle
    cl = input$cl/100
    z = qnorm((1-cl)/2, lower.tail = F)
    color = "magenta"
    s = seq(I((speed/2.237)^2*sin(2*deg2rad(angle)))-150,I((speed/2.237)^2*sin(2*deg2rad(angle)))+150)
    u = (coef(model4)[1] + coef(model4)[2] * s + summary(model4)$sigma*z)^2
    l = (coef(model4)[1] + coef(model4)[2] * s - summary(model4)$sigma*z)^2
    plot(hit_distance_sc ~ I((launch_speed/2.237)^2*sin(2*deg2rad(launch_angle))), 
         data = sc_bips, pch = 16, cex = .2, 
         col = c("red","black")[ifelse(is.na(sc_bips$events),2,ifelse(sc_bips$events=="home_run",1,2))],
         main = paste(input$cl,"% Prediction Interval for Hit Distance",sep=""),
         xlab = "Velocity^2 * sin(2*angle)", ylab = "Hit Distance",ylim=c(0,max(c(u,sc_bips$hit_distance_sc))))
    points(I((speed/2.237)^2*sin(2*deg2rad(angle))), predict(model4, data.frame(launch_speed = speed, launch_angle = angle))^2, pch = 3, col = color, lwd=3)
    lines(s,u,lty="dotted", col = color, lwd = 2)
    lines(s,l,lty="dotted", col = color, lwd = 2)
    x1 = I((speed/2.237)^2*sin(2*deg2rad(angle)))
    x2 = I((speed/2.237)^2*sin(2*deg2rad(angle)))
    y1 = (predict(model4, data.frame(launch_speed = speed, launch_angle = angle))-summary(model4)$sigma*z)^2
    y2 = (predict(model4, data.frame(launch_speed = speed, launch_angle = angle))+summary(model4)$sigma*z)^2
    lines(c(x1,x2),c(y1,y2),col=color,lty="dotted", lwd = 2)
    legend("bottomright",legend=c("Hit","Home Run"), pch = c(16,16), col=c("black","red"), cex = .8,pt.cex=c(.5,.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


