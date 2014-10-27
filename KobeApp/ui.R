# Global Fishery Potential Shiny App
#
# User Interface Script


shinyUI(fluidPage(
  titlePanel(
    img(src="SFG-logotype-side-ucsb-062513.uc.png", height = 182, width = 587)),
  
  sidebarLayout(
    sidebarPanel(
      h3("Kobe Plot Options"),
      helpText("Select the Country(s) and Year for the desired Kobe Plot"),
      
      checkboxGroupInput("Country",
                         label="Select Country(s)",
                         choices= c("Global","Lumped","Multinational", "Indonesia","USA", "China","Philippines","Japan","Chile","Mexico","Viet Nam"),
                         selected="Global"),
      
      sliderInput("Year","Year",2005,2011,2011),
      
      selectInput("DotColor",
                  label="Label Dots",
                  choices=c("By Country"="Country","By ID Level"="IdLevel","By Database"="Dbase"),
                  selected="Dbase"),


      checkboxInput("Neis","Include NEIs",TRUE)),
      
    
    mainPanel(
      h1("Kobe Plot"),
      h3("Overview"),
      p("This Kobe plot displays the current status of global fisheries. Use the options on the left to customize
        which fisheries are included in the plot. If selecting individual countries, be sure to deselect the Global option."),
      

  plotOutput(outputId = "kobe_plot", height = "600px")
  
  ))
  
))