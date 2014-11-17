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
                            '% Change From Status Quo Food'='Food',
                            '% Change From Current Food'='PercChangeTotalCatch'),
                  selected= 'PercChangeTotalBiomass'),
     
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
                         choices= c('Global','Multinational','Lumped','Asia','USA','China','Indonesia','Philippines','Thailand','Russian Federation',
                                    'Japan','Viet Nam','Republic of Korea','Malaysia','Taiwan Province of China','India', 
                                    'EU','Parties to the Nauru Agreement','Peru','Chile','Mexico'),
                          selected='Global')),
    
    
    mainPanel(
      h1("Upside Plot"),
      h3("Overview"),
      p("This plot displays the potential upside of recovering a country's fisheries. 
          The options on the left can be used to specify the values that are displayed on the x and y axes, as well as the value
          used to scale the size of the dots."), 
          
      strong("Note: This application and the data contained within it are a work in progress and are subject to change."),
      
      
      plotOutput(outputId = "upside_plot", height = "600px")
      
    ))
  
))