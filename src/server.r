library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(readr)
library(geojsonio)
library(plyr)
library(GGally)
library("viridis")
state_data <- read_csv("~/Documents/Spring 2016/Visualization/projectn/R code/finaldraft/EDAV State Data.csv")
mapdt <- geojson_read("http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_500k.json",what = "sp")
#For more detailed map data use the 5 MB line: http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_5m.json

#Clean up dataframe and match with map geojson order

Title<-c("State","Population","Nation_Pct","Density","Inc","Pct_College",
         "Poverty_Rate","Gini","Benefits_Vs_Tax","Gun_Owner_Pct","Pct_White",
         "Pct_White_Evangelical","Pct_Nonreligious", "Pct_Urban",
         "Pct_Obese","Life_Exp","Pct_Union","RTW","Region","Democratic_Vote",
         "Pct_Biz_Owner","Pct_Smoker","Pct_Highschoolbelow","Pct_Somecollege",
         "Pct_College_Graduate","Pct_Post_Graduate","No_money_for_healthcare",
         "No_money_for_food")
colnames(state_data) <- Title
formap <- data.frame(State = c("District of Columbia","Puerto Rico"))
maptemp <- rbind.fill(state_data,formap)
Order <- mapdt$NAME
map_data<-state_data[match(Order,state_data$State),]
mapdt@data <- data.frame(mapdt@data,maptemp[match(mapdt@data[,"NAME"],maptemp[,"State"]),])


shinyServer(
  function(input, output){
    
    # Report Part
    output$hist1 <- renderPlotly({
      ggplot(data = state_data, aes(x=reorder(State,Population), y= Population, fill = Region))+
        geom_bar(stat = "identity")+
        coord_flip()+
        scale_fill_viridis(begin = .5)+
        ggtitle("General View for Population on state level in 2016")
      
    })
    
    output$hist2 <- renderPlotly({
      ggplot(data = state_data, aes(x=))
    })
    
    output$cor1 <- renderPlot({
      corrdata = state_data[,c("Population", "Poverty_Rate", "Pct_College_Graduate",
                               "Gun_Owner_Pct", "Pct_White", "Pct_White_Evangelical",
                               "Pct_Nonreligious", "Pct_Urban", "Pct_Obese", "Life_Exp", 
                               "Pct_Union", "Pct_Smoker", "Democratic_Vote")]
      colnames(corrdata) = c("pop", "poverty", "college", "gunowner", "white", "whiteeva", 
                             "nonrelig", "urban", "obese", "LifeExp.", "union", "smoker", "DVote")
      ggcorr(corrdata, label = TRUE, label_round =2, digits = 3, label_size = 3.5, nudge_x = -0.5, layout.exp = 2)
    })
    
    output$cor2 <- renderPlot({
      
    })
    
    output$com_c2 <- renderText({
      print("The strongest relationships between all the chosen variables involve life expectancy.
High life expectancy is postively correlated with college education, and is negatively correlated
with obesity, smoking, poverty, white evangelicism, and gun ownership. The first three seem 
obvious as to why life expectancy would be lower, although the other ones are less 
clear...although most states with high obesity, smoking, and poverty (the South) also tend 
to own a lot of guns and are heavily evangelical.
            
            With Democratic voting, Democratic states on average to have larger urban populations,
            longer life expectancies, lower smoking rates and obesity, stronger labor unions, 
            fewer evangelicals, ESPECIALLY fewer gun owners, more college-educated, 
            and less impoverished.")
    })
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_data$Population
    )
    
    output$str <- renderTable({
      state_data
    })
    
    ### map part
    bins <- c(0,0.3,0.35,0.40,0.45,0.49,0.51,0.55,0.60,0.65,0.70,1)
    cpal <- colorBin(palette = "RdBu",domain = mapdt$Democratic_Vote,bins = bins)
    clabels <-sprintf("%s Vote Lean:\n %g",mapdt$NAME,mapdt$Democratic_Vote)
    
    output$map <- renderLeaflet({
      leaflet(mapdt) %>% 
        addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))%>%
        setView(-100.00, 40.00, zoom = 4) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.9,
                    fillColor = ~cpal(mapdt$Democratic_Vote),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    label = clabels, 
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
        addLegend(position= "bottomleft",pal = cpal, opacity = 0.7, values = mapdt$Democratic_Vote ,title = "Vote")
    })
    
    selectdata <- reactive({mapdt@data[input$vars]}) #
    
    observe({
      varBy <- input$vars
      proxy <- leafletProxy("map", data = mapdt)
      pal <- colorNumeric(palette = "Greens",domain = selectdata(), alpha = TRUE)
      labels <-sprintf("%s with %s:\n %g",mapdt$NAME,varBy, as.numeric(unlist(selectdata()))) #(selectdata()/1e6,digits = 2)
      proxy %>% 
        clearShapes() %>% clearControls() %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.7,
                    fillColor = ~pal(selectdata()),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    label = labels, 
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
        addLegend(position= "bottomleft",pal = pal, opacity = 0.7, values =  as.numeric(unlist(selectdata())),title = "Scale")
    })
    
    output$small <- renderPlotly({
      #ggplot(selectdata())+geom_point(aes(x = as.numeric(unlist(selectdata())), y= mapdt$Democratic_Vote))+xlab(input$vars)+ylab("Democratic Lean")#+scale_color_gradient2(low ="red",high = "blue",midpoint = 0.5 ) , color = mapdt$Democratic_Vote
      plot_ly(x = as.numeric(unlist(selectdata())), y= mapdt$Democratic_Vote,color = mapdt$Democratic_Vote,colors = cpal,text =mapdt$State)      
    })
    
    observe({
      proxy <- leafletProxy("map", data = mapdt)
      pal <- colorNumeric(palette = "Greens",domain = selectdata(), alpha = TRUE)
      labels <-sprintf("%s with %s:\n %g",mapdt$NAME,input$vars, as.numeric(unlist(selectdata())))
      
      if(input$BR){
        proxy %>% 
          addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = ~cpal(mapdt$Democratic_Vote),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      label = labels, 
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px"))}
      else{
        proxy %>% clearShapes() %>% 
          addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.7,
                      fillColor = ~pal(selectdata()),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      label = labels, 
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px"))
      }
    })
    
    
    randomVals <- eventReactive(input$save, {
      runif(input$n)
    })
    
    output$fileiIn <- renderTable({
      write.csv(input$randomVals, file = "comments.csv",sep = ",")
      go = read.csv(file = "comments.csv")
      print(go)
    })
    
    output$Recent_Comments <- renderText(input$comments)
    
    
    
    # Report Part
    output$scatter1 <- renderPlot({
      x_1 <- switch(input$ycol,
                    'Gun owner' = as.numeric(7),
                    'Gini' = as.numeric(9),
                    'white' = as.numeric(10))
      y_1 <- switch(input$ycol,
                    'Gun owner' = as.numeric(7),
                    'Gini' = as.numeric(9),
                    'white' = as.numeric(10))
      
      ggplot(data = state_data, aes(x = as.numeric(state_data[7]),y = as.numeric(state_data[8]))) + geom_point()
    })
    
    output$scatter2 <- renderPlot({
      ggplot(data = state_data, aes(x = Gun_Owner_Pct, y = Pct_White))+
        geom_point()+
        geom_smooth(method=lm)
    })
    
    output$scatter3 <- renderPlotly({
      ggplot(state_data, aes(x = Poverty_Rate, y = Democratic_Vote, label = State, color = 100*Pct_Urban)) +
        geom_point() + 
        geom_smooth(method = 'lm', se = FALSE) + 
        scale_colour_gradientn("% urban", colours = c('green', 'blue')) + 
        xlab("Poverty rate")
    })
    output$com_s3 <- renderText({
      print("Despite poor people tending to vote Democratic on average, the states they are most concentrated in tend to be very Republican. 
            Also, despite the talk of 'inner city hell' and 'horrific poverty'
            in our nation's biggest cities', the poorest parts of America are mostly rural. 
            As seen, the most Democratic states have lower poverty rates and 
            are more urban (with the two large exceptions of Maine and Vermont). 
            This is possibly because more Democratic states have more generous safety nets.")
    })
    
    output$scatter4 <- renderPlotly({
      ggplot(state_data, aes(x = Pct_Obese, y = Democratic_Vote, label = State)) + 
        geom_point(aes(color = 100*state_data$Life_Exp)) + 
        geom_smooth(method = 'lm', se = FALSE) + 
        scale_colour_gradientn("Life Expectancy (years)", colours = c('red', 'purple','blue'))
    })
    
    output$com_s4 <- renderText({
      print("Similar to the other graph, obesity is correlated with Republican voting, as well as lower life expectancy. 
            While there are many Republican states with higher life expectency, 
            they are the less obese ones. Above a rate of 31%, 
            the only states left are heavily Republican southern states with
            low life expectancies (with two Midwestern exceptions).
            ")
    })
    output$scatter5 <- renderPlotly({
      ggplot(state_data, aes(x = state_data$Pct_Smoker, y = Democratic_Vote, label = State)) +
        geom_point(aes(color = Region)) +
        geom_smooth(method = 'lm', se = FALSE) +
        xlab("Smoking Rate")+
        ylab("Democratic Vote")
    })

    output$com_s5 <- renderText({
      print("Smoking rate as a whole tends to be correlated with a state being more Republican.
            However, upon further investigation, this phenomenon does not hold up within regions.
            This is likely just that the most Republican states
            ")
    })
    output$scatter6 <- renderPlotly({
      ggplot(state_data, aes(x = Pct_Urban, y = Democratic_Vote, label = State)) + 
        geom_point(aes(color = Gun_Owner_Pct)) + 
        geom_smooth(method = 'lm', se = FALSE) + 
        xlab("% Urban") + 
        ylab("Democratic Vote") + 
        scale_colour_gradientn("Gun Ownership (%)", colours = c('blue', 'purple','red')) + 
        facet_wrap(~Region)
    })
    output$com_s6 <- renderText({
      print("As shown earlier, urban populations and greatly determines a state's partisanship...
            but the strength of the relationship depends on the region. In the northeast,
            gun ownership and urban population has a minimal relationship with partisanship,
            and if anything, is slightly negatively correlated...although that is possibly caused
            by the outliers of Maine and Vermont being FAR more Democratic than typical rural,
            white states. The Midwest and West have a rather strong relationship between urban
            population and Democratic strength, and the South has a lesser one.
            Gun ownership (except in the Northeast) puts a large damper on Democratic voting,
            but since gun ownership and urban population are negatively correlated regardless of votes,
            which has a stronger effect on Democratic voting is hard to isolate.
            ")
    })
    output$heat1 <- renderPlotly({
      ggplot(state_data, aes(x = RTW, y = State)) + 
        geom_tile(aes(fill = Democratic_Vote)) + 
        scale_fill_gradientn(colours = c("red", "red", "white", "blue", "blue"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'grey', colour = 'grey')) + ylim(rev(state_data$State))
    })
    output$com_h1 <- renderText({
      print("As we can see, states that are not right-to-work (a state that has legislation 
            forbidding labor unions from forcing dues on members of a unionized workplace who are 
            not union members) tend to be more Democratic, while right-to-work states tend to be more 
            Republican. Since many of these states have been right-to-work for decades, 
            it is unknown if unions are weak because they are so Republican, or they are so 
            Republican because unions are so weak.")
    })
    
    output$heat2 <- renderPlotly({
      ggplot(state_data, aes(x = Region, y = State)) + 
        geom_tile(aes(fill = Democratic_Vote)) + 
        scale_fill_gradientn(colours = c("red", "red", "white", "blue", "blue"))+ ylim(rev(state_data$State))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'grey', colour = 'grey')) 
    })
    
    output$com_h2 <- renderText({
      print("The Northeast (with the huge exception of West Virginia) votes solidly Democratic,
            the South tends to vote solidly Republican (with the exception of a few swing states),
            and the other two regions tend to split their votes, at least by state.")
    })
    
    # More Part
    output$team1 <- renderTable({
      team_members <- data.frame(Name = c("Jun Guo", "Jieyu Yao", "Matt Dawidowicz"),
                                 Email = c("jg3555@columbia.edu","jy2806@columbia.edu",
                                           "mjd2211@columbia.edu"),
                                 Uni = c("jg3555","jy2806","mjd2211"))
      team_members 
    })
    # For references
    output$com_b1 <- renderTable({
      referece <- data.frame(Reference = c("[1] Gallup Organization,. (2017). Gallup analytics.","[2] Gallup Organization,. (2017). Gallup analytics.")
                            )
      referece      
    })
    }
  
  
)