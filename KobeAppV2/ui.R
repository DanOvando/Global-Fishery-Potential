################################################
##
## User interface script for upside model Kobe plot app
##
## By: Tyler Clavelle
## Date: 02/24/2016
##
################################################

shinyUI(fluidPage(theme = 'bootstrap.css',
  titlePanel(
    img(src="SFG-logotype-side-ucsb-062513.uc.png", height = 182, width = 587)
    ),
  
  sidebarLayout(
    sidebarPanel(
      h2('Kobe Plot'),
      h3("Options"),
      helpText("Select options for the desired kobe plot"),
      
      radioButtons('Level',
                   label = 'Select Category',
                   choices = c('Global','By Country','By ISSCAAP','By FAO Region'),
                   selected = 'Global'),
      
      conditionalPanel(
        condition="input.Level == 'By Country'",
        selectizeInput("Country",
                         label="Select Country(s)",
                         choices= sort(c("Indonesia","USA", "China","Philippines",'Peru','Myanmar','Norway','Iceland','Morocco',
                                    "Japan","Viet Nam",'Thailand','India','Taiwan Province of China','Spain','Canada','Argentina',
                                    'Republic of Korea','Malaysia','Russian Federation',"Chile","Mexico",'South Africa','Denmark','United Kingdom',
                                    'Bangladesh','Ecuador','Brazil','Namibia','New Zealand'),decreasing = F),
                         selected="USA",
                       multiple = T)
        ),
        
      conditionalPanel(
        condition="input.Level == 'By ISSCAAP'",
        selectizeInput("isscaap",
                       label='Select ISSCAAP\nCategory(s)',
                       choices=c("Cods, hakes, haddocks","Herrings, sardines, anchovies","Flounders, halibuts, soles",          
                                 "Miscellaneous coastal fishes","King crabs, squat-lobsters","Miscellaneous demersal fishes",       
                                 "Crabs, sea-spiders","Lobsters, spiny-rock lobsters","Shrimps, prawns",                     
                                 "Tunas, bonitos, billfishes","Scallops, pectens","Miscellaneous pelagic fishes",        
                                 "Sharks, rays, chimaeras","Clams, cockles, arkshells","Abalones, winkles, conchs",           
                                 "Squids, cuttlefishes, octopuses","Shads","Salmons, trouts, smelts",             
                                 "Miscellaneous aquatic invertebrates","Miscellaneous diadromous fishes","Mussels",                             
                                 "Sea-urchins and other echinoderms","Oysters","Carps, barbels and other cyprinids",  
                                 "Miscellaneous marine crustaceans","Horseshoe crabs and other arachnoids","Sturgeons, paddlefishes" ),
                       selected = "Cods, hakes, haddocks",
                       multiple = T)
      ),
      
      conditionalPanel(
        condition="input.Level == 'By FAO Region'",
        selectizeInput("Region",
                       label="Select Region(s)",
                       choices= c('Arctic Sea','Northwest Atlantic','Northeast Atlantic','West Central Atlantic','Eastern Central Atlantic','Mediterranean and Black Sea',
                                  'Southwest Atlantic','Southeast Atlantic','Western Indian Ocean','Eastern Indian Ocean','Northwest Pacific','Northeast Pacific','Western Central Pacific','Eastern Central Pacific',
                                  'Southwest Pacific','Southeast Pacific','Atlantic Antarctic','Indian Ocean Antarctic','Pacific Antarctic'),
                       selected='Northwest Atlantic',
                       multiple = F)
      ),
      
#       selectInput("DotColor",
#                   label="Label Dots",
#                   choices=c("By Database"="Dbase","By Country"='Country',"By ISSCAAP"='SpeciesCatName',"By Region"='RegionFAO'),
#                   selected="Dbase"
#       ),
      
      radioButtons('Dbase','Assessment\nLevel',choices = c('All','RAM','Unassessed'),selected = c('All')),

      sliderInput('Size','Select Mininimum\nStock Size', min = 1, max = 100000, value = 1, step = 1000, round = T),
      
      checkboxInput('ColorID','Color by taxonomic\nlevel?'),
      
      checkboxInput("Neis","Include NEIs",TRUE)),
    
    mainPanel(
#       h3("Overview"),
#       p("This Kobe plot displays the current status of global fisheries. Use the options on the left to customize
#         which fisheries are included in the plot. If selecting individual countries, be sure to deselect the Global option."),
      
      
      plotOutput(outputId = "kobe_plot", height = "600px")
      
      # downloadButton(outputId = 'downloadPDF', label = 'Download PDF')
      
    ))
  
))