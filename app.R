library(shiny)
library(shinydashboard)
library(rsconnect)
library(plotly)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)
library(rsconnect)


ui <- dashboardPage( title = "Spatial Analysis - DAY NRLM",
                     skin = ("blue"),
                     dashboardHeader(title = "1st App -v0.1"),
                     
                     dashboardSidebar(
                       selectInput("state_1", "Choose the first State:", 
                                   list('Assam','Bihar','Chhattisgarh','Gujarat','Jharkhand','Karnataka','Madhya Pradesh', 'Maharashtra','Rajasthan','Odisha','Tamil Nadu', 'Uttar Pradesh','West Bengal'), selected = 'Bihar'),
                       selectInput("state_2", "Choose the second State:", 
                                   list('Assam','Bihar','Chhattisgarh','Gujarat','Jharkhand','Karnataka','Madhya Pradesh', 'Maharashtra','Rajasthan','Odisha','Tamil Nadu', 'Uttar Pradesh','West Bengal'), selected = 'Bihar'
                       )),
                     
                     dashboardBody(
                       
                       fluidRow(
                         column(6, selectInput("Parameter_1", "Parameter for Plot 1:",
                                               
                                               list('% of Target HHs to Total HHs' = 'per_tgt_tot_HH',
                                                        'Project Duration (in years)' = 'Age',
                                                        'Total SHGs promoted' = 'Total_SHGs',
                                                        'Outstanding Loan per SHG (in Rs. lakh)' = 'out_per_SHG',
                                                        '% of loan amount NPA' = 'per_NPA_amt',
                                                        'No. of Outstanding Loan A/c' = 'Out_num_ac',
                                                        'Total Outstanding Amount (in Rs. lakh)' = 'Out_amt',
                                                        #'No. of NPA loan A/c' = 'NPA_num_ac',
                                                        #'NPA Loan Amt (in Rs. lakh)' = 'NPA_amt',
                                                        "Total CIS (in Rs. lakh)" = 'Total_CIS',
                                                        "Crisil Score (1-100)" ='Crisil_Inclusix_Scores_16',
                                                        "Target HHs - SECC" = 'Target_HHs'
                                                    
                                                    #"Crisil Rank (All India)" ='Crisil_Inclusix_ranks_16'
                                                    
                                               ))),
                         
                         
                         column(6, selectInput("Parameter_2", "Parameter for Plot 2:",
                                               
                                               list('% of Target HHs to Total HHs' = 'per_tgt_tot_HH',
                                                    'Project Duration (in years)' = 'Age',
                                                    'Total SHGs promoted' = 'Total_SHGs',
                                                    'Outstanding Loan per SHG (in Rs. lakh)' = 'out_per_SHG',
                                                    '% of loan amount NPA' = 'per_NPA_amt',
                                                    'No. of Outstanding Loan A/c' = 'Out_num_ac',
                                                    'Total Outstanding Amount (in Rs. lakh)' = 'Out_amt',
                                                    #'No. of NPA loan A/c' = 'NPA_num_ac',
                                                    #'NPA Loan Amt (in Rs. lakh)' = 'NPA_amt',
                                                    "Total CIS (in Rs. lakh)" = 'Total_CIS',
                                                    "Crisil Score (1-100)" ='Crisil_Inclusix_Scores_16',
                                                    "Target HHs - SECC" = 'Target_HHs'
                                                    
                                                    #"Crisil Rank (All India)" ='Crisil_Inclusix_ranks_16'
                                                    
                                               ),selected = 'Total_SHGs'))
                       ),
                       
                       fluidRow(
                         column(6, plotlyOutput("Plot_1")),
                         column(6, plotlyOutput("Plot_2"))
                         
                       ),
                       
                       br(),
                       br(),
                       
                       fluidRow(infoBox(title = "NRLM-MIS",icon= icon ("laptop"), href = "http://nrlm.gov.in/"),
                                infoBox(title = "NRLM Bank Linkage Portal", icon =icon ("university"),href = "http://daynrlmbl.aajeevika.gov.in"),  
                                infoBox(title = "CRISIL Inclusix Report",href = "https://www.crisil.com/content/dam/crisil/our-analysis/reports/Research/documents/2018/march/crisil-inclusix-financial-inclusion-surges-driven-by-Jan-Dhan-yojana.pdf"),  
                                infoBox(title = "Maps - Data Meet ",href = "http://projects.datameet.org/maps/"),
                       br(), 
                       br(),
                       br(),
                       box(title =  "Features:", background = "red", width = 4,collapsible = TRUE,collapsed = TRUE, p("Visualise and compare Intra and Inter State's performance across 10 parameters ranging from extent of poverty, promotion of SHGs to SHG Bank Linkage performance! ")),
                       box(title =  "Notes:", background = "teal", width = 4,collapsible = TRUE,collapsed = TRUE, p("Maps are based on Census 2011 district list, therefore recently created districts in ASA,CHH,GUJ,MP,WB are missing."),p("Maps are not to Scale. This project is just a hobby, not meant for reasearch purposes."))          
                                
                       )
                       
                       
                     ))

server <- function(input,output){
  
  full_map <- readOGR(dsn = "./Data/Census_2011", layer = "2011_Dist" ) 
  out_data <- read.csv("./Data/combined_states.csv", stringsAsFactors = FALSE)
  crisil_data <- read.csv("./Data/Crisil_2016.csv", stringsAsFactors = FALSE)
  secc_data <- read.csv("./Data/SECC_Dist_Data.csv", stringsAsFactors = FALSE)
  
  out_data$out_per_SHG <- out_data$Out_amt/out_data$Out_num_ac
  mpr_data <- read.csv("./Data/MPR_Data_Agg.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
  mpr_data <- dplyr::select(mpr_data,-State,-District)
  out_data <- dplyr::left_join(out_data,mpr_data, by= "censuscode")
  out_data <- dplyr::left_join(out_data,crisil_data, by = "censuscode")
  out_data <- dplyr::left_join(out_data,secc_data, by = "censuscode")
  out_data <- select(out_data,-state_crisil,-dist_crisil,-NIC_district_code,-statename,-districtname,-state_secc_code,-dist_secc_code,-TotPop,-TotHH,-atleast1dep,-atleast1inc)
  
  rm(full_data,mpr_data,secc_data)
  
  dataset1 <- reactive({
    
    out_data_select_1 <- dplyr::filter(out_data,State == input$state_1)
    out_data_select_1 <- dplyr:: select(out_data_select_1,State,District,censuscode,input$Parameter_1)
    select_state_1  <- full_map[full_map$ST_NM == input$state_1,]
    select_state_1@data$id <- rownames(select_state_1@data)
    select_state_1.df <- fortify(select_state_1)
    select_state_1.df <- dplyr::left_join(select_state_1.df,select_state_1@data, by = "id")
    select_state_1.df$censuscode <- as.numeric(as.character(select_state_1.df$censuscode))
    select_state_1.df <- left_join(select_state_1.df,out_data_select_1, by = "censuscode")
    select_state_1.df <- select(select_state_1.df, -District)
    
  })
  
  observe({
    
    data_1 <- dataset1()
    
    output$Plot_1 <- renderPlotly({ ggplotly(ggplot(data_1, aes(x=long, y= lat,label= DISTRICT, group= group)) + 
                                               geom_polygon(aes_string(fill=input$Parameter_1)) + theme_bw()+
                                               theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),panel.background = element_blank())
                                             +coord_equal() + scale_fill_gradient(low = "#fff7bc", high = "#feb24c", space = "Lab", na.value = "grey", guide = guide_colorbar(title = NULL))) %>%
        layout(hovermode = 'x')
    })
    
    
  })  
  
  ##### Replicating for Plot 2 - will think later
  dataset2 <- reactive({
    out_data_select_2 <- dplyr::filter(out_data,State == input$state_2)
    out_data_select_2 <- dplyr:: select(out_data_select_2,State,District,censuscode,input$Parameter_2)
    select_state_2  <- full_map[full_map$ST_NM == input$state_2,]
    select_state_2@data$id <- rownames(select_state_2@data)
    select_state_2.df <- fortify(select_state_2)
    select_state_2.df <- dplyr::left_join(select_state_2.df,select_state_2@data, by = "id")
    select_state_2.df$censuscode <- as.numeric(as.character(select_state_2.df$censuscode))
    select_state_2.df <- left_join(select_state_2.df,out_data_select_2, by = "censuscode")
    select_state_2.df <- select(select_state_2.df, -District)
    
  })
  
  observe({
    
    data_2 <- dataset2()
    
    output$Plot_2 <- renderPlotly({ ggplotly(ggplot(data_2, aes(x=long, y= lat,label= DISTRICT, group= group)) + 
                                               geom_polygon(aes_string(fill=input$Parameter_2)) + theme_bw()+
                                               theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),panel.background = element_blank())
                                             +coord_equal() + scale_fill_gradient(low = "#fff7bc", high = "#feb24c", space = "Lab", na.value = "grey", guide = guide_colorbar(title = NULL))) %>%
        layout(hovermode = 'x')
        
    })
    
    
  })  
  
}




shinyApp(ui=ui,server = server)