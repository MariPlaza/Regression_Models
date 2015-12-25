#Check if the required packaged are installed and upload them. 
    list.of.packages <- c("ggplot2", "GGally", "caret","lattice","gridExtra", "MASS", "corrplot")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    library(lattice)
    library(ggplot2)
    library(GGally)
    library(caret)
    library(gridExtra)
    library(MASS)
    library(corrplot)
    set.seed(1234)
#Read and Transform Data. 
    data(mtcars)
    head(mtcars)
    str(mtcars)
    summary(mtcars)
    ?mtcars
#Transforming the data    
    Corr_Matrix <- cor(mtcars)
    mtcars$cyl <- as.factor(mtcars$cyl)
    mtcars$vs <- as.factor(mtcars$vs)
    mtcars$am <- as.factor(mtcars$am)
    levels(mtcars$am) <- c("Automatic", "Manual")
    mtcars$gear <- as.factor(mtcars$gear)
    mtcars$carb <- as.factor(mtcars$carb)
#Simple Linear Regression
    #Exploring Data
        plot_am <- ggplot(data=mtcars,aes(y= mpg, x = am), fill = am)
        plot_am <- plot_am + geom_violin(colour = "darkblue", size=2)
        plot_am <- plot_am + xlab("am") + theme(panel.grid.major = element_line(colour = "white"))  + theme_bw()
        plot_am
    #Simple Linear Regression Model    
        Summary_Data <- matrix(, nrow = 4, ncol = 2)
        
        Linear_Regression <- lm(mpg ~ am,data=mtcars)
        summary(Linear_Regression)
        
        Summary_Name <- paste("Simple Linear Regression", summary(Linear_Regression)$call[2], sep = " - ")
        Summary_Data[1,1]<- coef(summary(Linear_Regression))["amManual","Estimate"]
        Summary_Data[1,2]<-summary(Linear_Regression)$adj.r.squared*100             
        
#Exploratory Analysis of Variables
    #Continuos Variables
        #Displacement (cu.in.)
            plot_disp <- ggplot(data=mtcars,aes(y= mpg, x = disp))   + geom_line(fill="darkblue", stat="identity")
            plot_disp <- plot_disp + labs(x="disp",y="Miles per Gallon (mpg)") 
            plot_disp <- plot_disp + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Gross horsepower
            plot_hp <- ggplot(data=mtcars,aes(y= mpg, x = hp))   + geom_line(fill="darkblue", stat="identity")
            plot_hp <- plot_hp + labs(x="hp", y="") 
            plot_hp <- plot_hp + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Rear axle ratio
            plot_drat <- ggplot(data=mtcars,aes(y= mpg, x = drat))   + geom_line(fill="darkblue", stat="identity")
            plot_drat <- plot_drat + labs(x="drat", y="") 
            plot_drat <- plot_drat + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Weight (lb/1000)
            plot_wt <- ggplot(data=mtcars,aes(y= mpg, x = wt))   + geom_line(fill="darkblue", stat="identity")
            plot_wt <- plot_wt + labs(x="wt", y="") 
            plot_wt <- plot_wt + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #1/4 mile time
            plot_qsec <- ggplot(data=mtcars,aes(y= mpg, x = qsec))   + geom_line(fill="darkblue", stat="identity")
            plot_qsec <- plot_qsec + labs(x="qsec", y="") 
            plot_qsec <- plot_qsec + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
    #Factor Variables
        #Number of cylinders
            plot_cyl <- ggplot(data=mtcars,aes(y= mpg, x = cyl), fill = cyl)
            plot_cyl <- plot_cyl + geom_violin(colour = "darkblue", size=2)
            plot_cyl <- plot_cyl + labs(x="cyl",y="Miles per Gallon (mpg)") + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #V/S
            plot_vs <- ggplot(data=mtcars,aes(y= mpg, x = vs), fill = vs)
            plot_vs <- plot_vs + geom_violin(colour = "darkblue", size=2)
            plot_vs <- plot_vs + xlab("V/S") + ylab("") + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Transmission
            plot_am <- ggplot(data=mtcars,aes(y= mpg, x = am), fill = am)
            plot_am <- plot_am + geom_violin(colour = "darkblue", size=2)
            plot_am <- plot_am + xlab("am") + ylab("") + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Number of forward gears
            plot_gear <- ggplot(data=mtcars,aes(y= mpg, x = gear), fill = gear)
            plot_gear <- plot_gear + geom_violin(colour = "darkblue", size=2)
            plot_gear <- plot_gear + xlab("gear")+ ylab("") + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        #Number of carburetors
            plot_carb <- ggplot(data=mtcars,aes(y= mpg, x = carb), fill = carb)
            plot_carb <- plot_carb + geom_violin(colour = "darkblue", size=2)
            plot_carb <- plot_carb + xlab("carb")+ ylab("") + theme(panel.grid.major = element_line(colour = "white")) + theme_bw()
        
    
        grid.arrange(plot_disp, plot_hp, plot_drat, plot_wt, plot_qsec, plot_cyl, plot_vs, plot_am, plot_gear, plot_carb, ncol=5, nrow=2)
        
    

        cex.before <- par("cex")
        par(cex = 0.7, oma=c(0,0,2,0))
        corrplot.mixed(Corr_Matrix, tl.cex=1/par("cex"),font.main=1, title="Graph 2: Correlation among variables",upper = "circle", 
                       lower = "number",cl.cex=1/par("cex"))
        par(cex = cex.before)
                
        
        
#Multiple Regression Model - All Variables
        Multiple_Regression_All <- lm(mpg ~ .,data=mtcars)
        summary(Multiple_Regression_All)
        
        Summary_Name <- c(Summary_Name, paste("All Regresors", summary(Multiple_Regression_All)$call[2], sep = " - "))
        Summary_Data[2,1]<- coef(summary(Multiple_Regression_All))["amManual","Estimate"]
        Summary_Data[2,2]<- summary(Multiple_Regression_All)$adj.r.squared*100
        
    
#Multiple Regression Model - Selected Variables
        Multiple_Regression_Selected <- lm(mpg ~ am + wt + hp,data=mtcars)
        summary(Multiple_Regression_Selected)
        
        Summary_Name <- c(Summary_Name, paste("Highly Correlated Regresors", summary(Multiple_Regression_Selected)$call[2], sep = " - "))
        Summary_Data[3,1]<- coef(summary(Multiple_Regression_Selected))["amManual","Estimate"]
        Summary_Data[3,2]<- summary(Multiple_Regression_Selected)$adj.r.squared*100
        
#Multiple Regression Model - MASS
        Multiple_Regression_MASS <- stepAIC(lm(mpg ~ . ,data=mtcars), direction = 'both', trace = FALSE)
        summary(Multiple_Regression_MASS)
        
        Summary_Name <- c(Summary_Name, paste("Highly Correlated Regresors", summary(Multiple_Regression_MASS)$call[2], sep = " - "))
        Summary_Data[4,1]<- coef(summary(Multiple_Regression_MASS))["amManual","Estimate"]
        Summary_Data[4,2]<- summary(Multiple_Regression_MASS)$adj.r.squared*100
        
#Regression Summary
        colnames(Summary_Data) <- c("AMManual Coefficient (Difference)","Adjusted R-squared")
        rownames(Summary_Data) <- Summary_Name
        Summary_Data
                
#Error Evaluation    
    
    par(oma=c(5,4,3,1), mfrow = c(2, 4), mar = c(0,0,1,1)+2)
    
    plot(Linear_Regression$fitted.values,Linear_Regression$residuals, main ="mpg~am")
    plot(Multiple_Regression_All$fitted.values,Multiple_Regression_All$residuals, main ="mpg~.")
    plot(Multiple_Regression_Selected$fitted.values,Multiple_Regression_Selected$residuals, main ="mpg~hp+am+wt")
    plot(Multiple_Regression_MASS$fitted.values,Multiple_Regression_MASS$residuals, main ="mpg~hp+cyl+am+wt")
    
    
    LR_Standard <- rstandard(Linear_Regression)
    qqnorm(LR_Standard, main ="mpg~am")
    qqline(LR_Standard)
    
    
    MRA_Standard <- rstandard(Multiple_Regression_All)
    qqnorm(MRA_Standard, main ="mpg~.")
    qqline(MRA_Standard)
    
    
    MRS_Standard <- rstandard(Multiple_Regression_Selected)
    qqnorm(MRS_Standard, main ="mpg~hp+am+wt")
    qqline(MRS_Standard)
    
    MRM_Standard <- rstandard(Multiple_Regression_MASS)
    qqnorm(MRM_Standard, main ="mpg~hp+cyl+am+wt")
    qqline(MRM_Standard)
    
    mtext("Graph 3: Fitted Values vs. Residuals and Normal Q-Q", outer=TRUE) 
    
    
    