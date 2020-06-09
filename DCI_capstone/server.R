#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
   
     #disribution plots
    output$distPlot <- renderPlotly({
        
        
        dci_data_total<-dci_data_eda %>% 
            filter(acct_yyyy == input$summary_year) %>% 
            select('Charges' = remit_total_charged,'Payments'=remit_total_paid)
        
        dci_long <- reshape2::melt(dci_data_total)
        
         
        
        dist_plts<-ggplot(dci_long, aes(value,y = ..density..)) + facet_wrap(~variable, scales = 'free_x') +
                        geom_density(color = "red",alpha = 0.4)+
                        labs(title = "Distribution of Charges Vs Payments")+
                        geom_histogram( fill =  "chartreuse4", alpha = 0.6)+
                        labs(x="Payments($)", y = "")+
                        scale_y_continuous(breaks = seq(0,10000,1000))+
                        scale_x_continuous(breaks = seq(0,3000,500))
        
       ggplotly( dist_plts)
         
 

    })
    
    #payments summary by Treatment Types
    output$distBoxPlot <-renderPlotly({
        
        cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        dist_box_modality<-dci_data_eda %>% 
                            filter(acct_yyyy == input$summary_year) %>% 
                            select(modality,remit_total_paid) %>% 
                            ggplot(aes(  modality, remit_total_paid,alpha = 0.5)) +
                            geom_boxplot(aes(fill=modality)) +
                            theme_classic() +
                            scale_fill_manual(values = cbp1) +
                            ggtitle("Distribution of Payments across Treatment Types") +
                            labs(x="Treatment Types",y = "Payments")  +
                            theme(legend.position = "none")
        
        ggplotly(dist_box_modality)
    })
    ##################################################**Treatment Types**########################################
    
   #this below code is used to populate the selection input boxes on the left panel
    
    output$treatment_payors_sel_year <- renderUI({
      selectInput("treatment_year","Select Year", choices = c('2017','2018'))
    })
    
    output$treatment_payors <- renderUI({
      
      if(input$treatment_year == '2017'){
        dat<-c("United Health_02371001" = '02371001',			
                "UPMC Health_12320609"='12320609')}
      
      if(input$treatment_year == '2018'){
        dat<-c("United Health_02371001" = '02371001',			
               "UPMC Health_12320609"='12320609',			
               "Humana_11371008"='11371008',
               "Wellcare_21560402"='21560402',			
               "Anthem_00410417"= '00410417',			
               "Highmark Group_16181025"='16181025',			
               "Molina_00410601"= '00410601',			
               "Blue Cross_00410825"='00410825',
               "CSV" = '00410235')}
      
      
      selectInput("treatment.payors.in","Select a Payor",
                  choices =  dat)
     
  })
  
    output$distpayorBarPlot <- renderPlotly({
        
        total_payments_bar_plt<-dci_data_eda %>% 
                                # filter(pcn_payer_code == input$summary_payor) %>% 
                                # filter(acct_yyyy == input$summary_year_payor) %>% 
                                filter(pcn_payer_code == input$treatment.payors.in) %>% 
                                filter(acct_yyyy == input$treatment_year) %>% 
                                group_by(modality) %>% 
                                summarise(sum_payments= sum(remit_total_paid)) %>% 
                                ggplot(aes(x = modality, y = sum_payments, fill = modality,text=sum_payments)) +
                                geom_bar(stat = "identity", alpha=0.8) +
                                labs(
                                    x = "Treatment Types",
                                    y = "Total Payments",
                                    title = "Total Payments by Treatment Type"
                                ) +
                                scale_fill_manual(values = cbp1) +
                                theme(legend.position = "none")
                            
                            ggplotly(total_payments_bar_plt,tooltip = "text")
        
    })
    
    
    #percentage bar plot on Payments tab
    
    output$distpayorBarPlotper <-renderPlotly({
        payors_with_good_per<-dci_data_eda %>% group_by(pcn_payer_code,modality) %>% 
                                # filter(pcn_payer_code==input$summary_payor) %>% 
                                # filter(acct_yyyy == input$summary_year_payor) %>% 
          
                                filter(pcn_payer_code==input$treatment.payors.in) %>% 
                                filter(acct_yyyy == input$treatment_year) %>% 
                                summarise('Num_of_rows'=n(),
                                          'Total_charges' = sum(remit_total_charged),
                                          'Total_payments' = sum(remit_total_paid),
                                          'percentage_payments' = round((Total_payments/Total_charges) * 100,2)
                                ) %>% 
                                arrange(desc(Total_charges)) %>% 
                                ggplot(aes(reorder(x=modality,percentage_payments),y=percentage_payments,fill = modality,text=percentage_payments)) +
                                geom_segment(aes (reorder(x=modality,percentage_payments), xend =modality,y=0,yend = percentage_payments) )+
                                geom_point(color = "orange",size = 4)+
                                theme_minimal()+
                                #geom_text(aes(label=percentage_payments)) +  
                                labs(x='Treatment Types',y="Percentage of Charges Paid(%)",title = "Percentage of Charges Paid By Treatment Types")+
                               # scale_y_continuous(breaks = seq(0,20,1))+
                                theme(legend.position = "none")
        
        
        ggplotly(payors_with_good_per,tooltip = "text")
        
    })
    
    ##################################################**End of Treatment Types**######################### 
    
    ############################################****PCN PLOTS***#########################################
    #side input boxes
    
    
    output$summary_year_pcn_ou <- renderUI({
      selectInput("summary_year_pcn","Select Year", choices = c('2017','2018'))
    })
    
    output$summary_payor_pcn_ou <- renderUI({
      
      if(input$summary_year_pcn == '2017'){
        dat<-c("United Health_02371001" = '02371001',			
               "UPMC Health_12320609"='12320609')}
      
      if(input$summary_year_pcn == '2018'){
        dat<-c("United Health_02371001" = '02371001',			
               "UPMC Health_12320609"='12320609',			
               "Humana_11371008"='11371008',
               "Wellcare_21560402"='21560402',			
               "Highmark Group_16181025"='16181025',			
               "Molina_00410601"= '00410601',	
               "CSV" = '00410235')}
      
      selectInput("summary_payor_pcn","Select a Payor",
                  choices =  dat)
      
    })
    
    
  
    #pcn 1 Bar plot
    output$distPcnBarPlot <-renderPlotly({
        
        #top pcn bar plot
        Payors_bar_plt<-dci_data_eda%>% 
                        filter(acct_yyyy == input$summary_year_pcn) %>% 
                        group_by(pcn_payer_code) %>% 
                        summarise('total_charges'=sum(remit_total_charged),'total_paid' = sum(remit_total_paid)) %>% 
                        mutate('percentage_of_payments' = round(total_paid/sum(total_paid)*100,2)) %>% 
                        filter(percentage_of_payments>0.66) %>% 
                        ggplot(aes(reorder(x=pcn_payer_code,percentage_of_payments),y=percentage_of_payments,fill = pcn_payer_code,text=percentage_of_payments))+
                        theme(axis.text.x = element_text(angle = 90,hjust=1)) +
                        geom_bar(  stat = "identity")+
                        theme(legend.position = "none") +
                        labs(x="Payors",y="Percentage of Total  Payments",Title = "Percentage of Total Payments For Each Payor ")
                    
                    ggplotly(Payors_bar_plt,tooltip = "text")
    })
    
    #pcn 2 Pie chart
    output$distPcnplot <- renderPlot({
        
        
        bp<-dci_data_eda %>% 
            filter(acct_yyyy == input$summary_year_pcn) %>% 
            group_by(pcn_payer_code) %>% 
            summarise('total_charges'=sum(remit_total_charged),'total_paid' = sum(remit_total_paid)) %>% 
            mutate('percentage_of_payments' = round(total_paid/sum(total_paid)*100,2)) %>% 
            ggplot(aes(x="",y=percentage_of_payments,fill = pcn_payer_code))+
            geom_bar(width = 1, stat = "identity")+
            theme_void() 
        bp
        
        pie_chart <- bp + coord_polar("y",start = 0) +
            labs(title = "Major Payor: United Health_02371001") 
        
        pie_chart
        
    })
    
    
    
    #pcn 3 bar plot services
    output$distPcnServicesPlot <- renderPlotly({
        services_plt <- dci_data_eda%>% 
                        group_by(description) %>% 
                        filter (pcn_payer_code==input$summary_payor_pcn) %>% 
                        filter(acct_yyyy == input$summary_year_pcn) %>% 
                        summarise('total_charges'=sum(remit_total_charged),'total_paid' = sum(remit_total_paid)) %>% 
                        mutate('percentage_of_payments' = round(total_paid/total_charges*100,2)) %>% 
                        mutate('per_payment_overall' = round((total_paid/sum(total_paid))*100,2)) %>% 
                        filter(total_paid>4000) %>% 
                        ggplot(aes(reorder(x= description,total_paid),y = total_paid,fill = description,text=total_paid)) +
                        geom_col()+
                        coord_flip()+
                        theme(axis.text.x = element_text(angle = 45,hjust=1)) +
                        theme_minimal()+
                        xlab("") +
                        ylab("Payments") +
                        ggtitle("Total Payments by Services")+
                        theme(legend.position = "none") 
                    
                    ggplotly(services_plt,tooltip = "text")
        
       
        
        
    })
    
    ############################################****END OF PCN PLOTS***######################################### 
    
    
    ############################################****Medication plots***######################################### 
  output$distMedicationsplt <- renderPlotly({
      
      meds<-dci_data_eda %>% 
                  filter(description == input$meds ) %>%   
                  group_by(description) %>% 
                  summarise('TotalCharges'=sum(remit_total_charged),'TotalPaid' = sum(remit_total_paid)) %>% 
                  pivot_longer(TotalCharges:TotalPaid,names_to = "Type", values_to = "Charges")%>%
                  ggplot(aes(x=reorder(description,Charges),y=Charges,fill = Type,text = Charges) ) +
                  geom_bar(stat="identity",alpha = 0.8,position = 'dodge') +
                  theme(axis.text.x = element_text(angle = 45,hjust=1)) +
                  theme_minimal()+
                  xlab("Medication") +
                  ylab("Dollars") +
                  ggtitle("Total Charges vs Total Paid") 
              
              ggplotly(meds,tooltip = "text")
  })
    
    
    
    output$vbox1 <- renderValueBox({
        valueBox(value= paste(dci_data_eda %>% 
                     filter(description == input$meds ) %>% 
                     group_by('Description'=description) %>% 
                     summarise('total_charges'=sum(remit_total_charged),'total_paid' = sum(remit_total_paid)) %>% 
                     mutate('percentage' = round(total_paid/total_charges * 100,2)) %>% 
                     select (percentage),"%") ,
        
        subtitle = paste("Percentage for: ",input$meds),color="light-blue")
        
    })
    
    ############################################****End of Medication plots***#########################################  
    
    
    ############################################****Comparision plots***######################################### 
    
    
    output$algo_payors_ou <- renderUI({
      selectInput("algo_payors","Select a Payor", choices = c(
                                                            "United Health_02371001"='pcn_payer_code_02371001',
                                                            "CSV_00410235"='pcn_payer_code_00410235',				
                                                            "Molina_00410601"='pcn_payer_code_00410601',				
                                                            "Highmark Group_16181025"='pcn_payer_code_00410825',				
                                                            "UPMC Health_12320609"='pcn_payer_code_12320609'))
    })
    
    output$algo_modality_ou <- renderUI({
      
      if(input$algo_payors == 'pcn_payer_code_02371001'){
        dat<-c(
          "HEMO_1110"="modality_cost_code_1110",
          "CAPD HOME TRAINING_1220"="modality_cost_code_1220",
          "CCPD HOME TRAINING_1230" ="modality_cost_code_1230",
          "CAPD HOME TREATMENT_1320" =	"modality_cost_code_1320",
          "CCPD HOME TREATMENT_1330" = "modality_cost_code_1330")}
      
      if(input$algo_payors == 'pcn_payer_code_00410235'){
         dat<-c("HEMO_1110"="modality_cost_code_1110")  }
      
      if(input$algo_payors == 'pcn_payer_code_00410601'){
        dat<-c("HEMO_1110"="modality_cost_code_1110")  }
      
      if(input$algo_payors == 'pcn_payer_code_00410825'){
        dat<-c("HEMO_1110"="modality_cost_code_1110")  }
      
      if(input$algo_payors == 'pcn_payer_code_12320609'){
        dat<-c("HEMO_1110"="modality_cost_code_1110")  }
      

 
      selectInput("algo_modality","Select Treatment Types",
                  choices =  dat)
      
    })

    
    output$ComparisionPlot <- renderPlotly({
                            compare_plt_df <- compare_actuals_to_pred %>% 
                                filter(!!as.symbol(input$algo_payors) == 1 ) %>% 
                                filter(!!as.symbol(input$algo_modality) == 1)%>% 
                                select(actual,predicted,percentage_difference ) %>%
                                summarise('Actual' = mean(actual),'Predicted' = mean(predicted)) %>% 
                                pivot_longer(Actual:Predicted,names_to = "Predictions", values_to = "percentage_predicted") 
                            
                            #store the predicted percentage value in a variable for later calculations
                            get_percentage_value <- compare_plt_df  %>% 
                                                    filter(Predictions == "Predicted") %>% 
                                                    pull(percentage_predicted)
                         
                            compare_plt<-  ggplot(compare_plt_df,aes(x = Predictions, y = percentage_predicted,fill = Predictions,text = percentage_predicted )) +
                                geom_text(aes(label=round(percentage_predicted,2)), vjust=0,size=5) + 
                                geom_col( alpha= 0.7) +
                                scale_fill_manual(values = c("grey","darkslategrey"))+
                                ggtitle("Comparing Actual Payment % to Predicted %") +
                                labs(x = "Actual Vs Predicted", y = "Percentage %")+
                                theme(legend.position = "none")
                            
                            ggplotly(compare_plt,tooltip = "text")
    })
    
    ############################################****End of Comparision plots***#########################################
    
    ############################################****Input Box/predicted value***######################################### 
output$predictedvalue <- renderText ({
    #step 1
    compare_plt_df <- compare_actuals_to_pred %>%
        filter(!!as.symbol(input$algo_payors) == 1 ) %>%
        filter(!!as.symbol(input$algo_modality) == 1)%>%
        #filter(pcn_payer_code_02371001 == 1 & hcpc_code_90999==1 ) %>%
        select(actual,predicted,percentage_difference ) %>%
        summarise('Actual' = mean(actual),'Predicted' = mean(predicted)) %>%
        pivot_longer(Actual:Predicted,names_to = "Predictions", values_to = "percentage_predicted")

    #store the predicted percentage value in a variable for later calculations
    get_percentage_value <- compare_plt_df  %>%
        filter(Predictions == "Predicted") %>%
        pull(percentage_predicted)
    
    
    #step 2
    x<-input$inputcharge 
    
    (as.numeric(x) * as.numeric(as.vector(get_percentage_value)))/100
    
    })
    
    
    #########################################*End of Input Box/Predicted value*#############################################
    
    #########################################*Raw Data*#############################################
    
    output$raw_data_table <- DT::renderDataTable({
      DT::datatable( dci_data_eda %>% 
                       #filter(acct_mm==input$data_month_slider) %>% 
                       #filter(acct_yyyy ==input$data_year) %>% 
                       select("Payor" = pcn_payer_code,"Acct Month" = acct_mm,'Acct Year' = acct_yyyy,"Treatment Type Code" = modality_cost_code,
                              "Description"= description,"Revenue Code" = hcpc_code,
                              "Charged Amount" = remit_total_charged, "Paid Amount" = remit_total_paid))
                       
                       
      
      
    })
    
    #########################################*End of Raw Data*#############################################
})
