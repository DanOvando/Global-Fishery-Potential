# Global Fishery Potential Upside Shiny App
#
# User Interface Script


shinyUI(fluidPage(
  titlePanel(
    img(src="SFG-logotype-side-ucsb-062513.uc.png", height = 182, width = 587)),
    
    
  sidebarLayout(
    sidebarPanel(
      h3("Upside Plot Options"),
      helpText("Select the Country(s) to display for the desired upside plot"),


      selectInput("Policy",
                  label="Select Policy Option",
                  choices=c('Catch Share'='CatchShare',
                            'Close Down'='CloseDown','Fmsy'='Fmsy','Food'='Food','Opt'='Opt'),
                  selected="CatchShare"),
      
      
      selectInput("xaxis",
                  label="X-axis",
                  choices=c('% Change From Status Quo Total Biomass'='PercChangeFromSQTotalBiomass',
                            '% Change From Current Total Biomass'='PercChangeTotalBiomass',
                            '% Change From Status Quo Median Biomass'='PercChangeFromSQMedianBiomass',
                            '% Change From Current Median Biomass'='PercChangeMedianBiomass',
                            '% Change From Status Quo Food'='Food'),
                  selected= 'PercChangeFromSQTotalBiomass'),
     
      selectInput("yaxis",
                  label="Y-axis",
                  choices=c('% Change From Status Quo NPV'='NPV',
                            '% Change From Current Profits'='PercChangeTotalProfits',
                            '% Change From Status Quo Median Profits'='PercChangeFromSQMedianProfits',
                            '% Change From Current Median Profits'='PercChangeMedianProfits'),
                  selected='NPV'),
      
      selectInput("dotsize",
                  label="Dot Size",
                  choices=c('% Change From Status Quo Food'='Food',
                            '% Change From Status Quo NPV'='NPV',
                            '% Change From Status Quo Biomass'='Fish'),
                  selected='Food'),
      
      checkboxGroupInput("Country",
                         label="Select Country(s)",
                         choices= c("Global","Indonesia","USA","China","Philippines","Peru","EU", "Japan","Chile","Mexico","Viet Nam","Parties to the Nauru Agreement"),
                         selected="USA")),
    
    
    mainPanel(
      h1("Upside Plot"),
      h3("Overview"),
      p("This plot displays the potential upside of recovering a country's fisheries. 
          The x and y axes represent percent changes in profits and biomass. Dot size is proportional to the percent change in landings (Food). 
          Each dot represents a different country."),
      
      
      plotOutput(outputId = "upside_plot", height = "600px")
      
    ))
  
))