1. Did the 2008 financial crisis structurally change the relationship between household debt and savings behavior across OECD countries?
ggplot(data=hh_budget, aes(x=Year))+
  geom_line(aes(y=Debt, linetype="Debt"),color="blue")+geom_line(aes(y=Savings, linetype = "Savings"))+
  geom_vline(xintercept = 2008, color="red", linetype="dashed")+
  facet_wrap(~Country)+
  labs(title="2008 Financial Crisis Debt vs Savings Comparison", y=NULL)

      #Use two geom_lines to create two different lines, one for debt and one for savings
      # geom_vline(xintercept=2008) puts a vertical line on 2008 so we can easily see before and after effects of the crisis
      # facet_wrap(~Country) creates four different graphs for each country to easily view

------------------
  
2. Is there a lagged relationship between household debt levels and unemployment — does rising debt predict unemployment increases 1–2 years later, or is causality more likely reversed?
  # actual code
#create a df that shows the previous years debt compared to this years debt and unemployment
hh_lagged <- hh_budget |> 
  arrange(Country, Year) |> 
  group_by(Country) |> 
  mutate(debt_lag1 = lag(Debt, 1),
  debt_lag2 = lag(Debt, 2)) |> 
  ungroup()

ggplot(hh_lagged_long, aes(x = Year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Prior-Year Debt vs. Current Unemployment Over Time",
       subtitle = "debt_lag1 is already shifted forward one year to align with the unemployment it's predicting",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal()
  #debt lag1 and 2 show the current years debt compared to the previous one and two years
  # the points show the debt lag and the lines show are best-fit regression line answering "Within this one country, as last year's debt goes up, does this year's unemployment tend to go up or down, on average"
    #downward sloping line means that higher debt in the previous year is associated with lower unemployment in the current year
    #upward sloping line means that higher debt in the previous year is associated with higher unemployment in the current year
  Graph findings
  # Japan and USA both have positive linear relationships: as household debt lag increases so does unemployment rate
  # Australia and Canada have negative linear relationships: as household debt lag increases unemployment rate decreases

model_lag<-lm(Unemployment ~ debt_lag1+debt_lag2, data=hh_lagged)
  summary(model_lag)
#MODEL IS SEPARATE FROM THE GRAPH
# P value and R squared are most important in determining if model does a good job of predicting dependnent variable
  # Pr(>|t|) is a t-test to get the individual p values: number 0-1 thatmeasures change of getting result by random luck
    # p < 0.05 = statistically significant
#neither debt_lag1 or debt_lag2 is a statistcally significant predictor of unemployment

  #R squared: number 0-1 that tells you how well the independent variable predicts the change in dependent variable (how well the model explains variation of unemployment)
# 0.023 (2.3%) tells us that Household Debt with 1  and 2 year lag combined has almost no explanatory power in unemployment rate
  # adjusted R squared shows the percentage of variation explained by the regression model, penalizing the score if you have unecessary independent variables
# -0.0027 (-0.27%) means the regression model is perfroming worse than with no predictors at all - guessing the avg employment rate every time
  
  # F statistic: overall estimate that tests whether model explains a meaningful amount of variation in your dependent variable - or whether it's no better than guessing
      # "Do debt_lag1 AND debt_lag2, considered together as a package, explain more variation than a model with no predictors at all?" - most of the time f stat and individual ones agree
    # higher is better cause it means stronger evidence that your predictors, as a group, explain real variation in dependent variable
#The overall p-value of 0.41 (F-test) suggests that debt_lag1 and debt_lag2, taken together, do not meaningfully improve prediction of unemployment rate beyond just guessing the average
  
#F TSTAT IS LIKE A GROUP PROJECT, T TESTS ARE HOW WELL AN INDIVIDUALCONTRIBUTED
  
hh_lagged2<-hh_budget |> 
  arrange(Country,Year) |> 
  group_by(Country) |> 
  mutate(unemp_lag1=lag(Unemployment,1)) |> 
  ungroup()
#this is the reversed model of the one above, making debt the dependent variable and unemployment the independent variable

model_reverse <- lm(Debt ~ unemp_lag1, data = hh_lagged2)
summary(model_reverse)
#after running the model we can see that the t stat (individual) p value is about the same as the previous at 0.623
#the f stat p value is 0.6 which is higher than the previous model of 0.4
  # this means that unemployment is a worse predictor of debt than debt was of unemployment.
  # neither relationship is significant, but this comparison suggests debt on unemployment is the better option if either is real at all

----------------------
  
3. Does household wealth help explain why some countries' spending stays stable when income changes, while other countries' spending closely tracks income ups and downs?

#Correct code
#STEP 1: create a df that is suitable for a graph
hh_long <- hh_budget |> 
  select(Year, Country, Expenditure, DI) |> 
  pivot_longer(cols=c(Expenditure, DI), names_to="variable", values_to="value")
#we have to create a new df using pivot longer because aes(color=...) needs a single column to assign colors to different lines, not two different columns
  #pivot_longer fixes this by combining DI and Expenditure columns under a single variable column
  #it also creates a variable column that holds the numbers for both DI and Expenditure 
----
#STEP 2: create the a graph depicting year, value, the 2 variables, and countries
ggplot(hh_long, aes(x=Year, y=value, color=variable)) +
  geom_line() +
  facet_wrap(~Country) +
  labs(title = "Expenditure vs Disposable Income Change by Country", x = NULL, y = NULL)

#STEP 3: create a table to show average wealth for each country. This allows us to compare each countries average wealth to their expenditure and disposable income to see if these change compared to their wealth.
hh_budget |> 
  group_by(Country) |> 
  summarize(avg_wealth = mean(Wealth)) |> 
  arrange(desc(avg_wealth)) 
  #Observations
    #USA and Japan have the first and second highest average wealth and their DI and Expenditure DI's effect on Expenditure get stronger
    #Canada has the third highest average wealth and their DI and Expenditure are moderately linked, but not as closely as USA and Japan
    #Australia has the lowest average wealth and have the weakest DI and Expenditure connection

#STEP 4: create a model
model_smoothing <- lm(Expenditure ~ DI * Wealth, data = hh_budget)
summary(model_smoothing)
  #This code tests whether DI and Wealth independently effect Expenditure and the research question - whether Wealth changes the strength of DI's effect on Expenditure
  #Expenditure is the dependent variable (thing being predicted/explained/effect) ~ DI and Wealth are the independent variables (predictor/cause)
  #What * does (equivalent to DI+Wealth+DI:Wealth): Does DI affect Expenditure, does Wealth affect Expenditure, AND does Wealth change how much DI affects Expenditure?. Wealth and DI mulitplied together for each individual row
  #Findings
    #P values: DI not statistically significant, Wealth and DI:Wealth have a statistically significant effect on expenditure
    #F stat (p-value): 4.158e-11 = 0.00000000004158: means moving the decimal point 11 times to the left 
      #So this is very statistically significant
    #R-squared of 0.4554(45.54%) means the model explains a strong share of the variation in expenditure - supporting the idea tht DI and Wealth (their interaction) meaningfulling drive spending differences across countries.
      #0.26-0.50 range is strong
