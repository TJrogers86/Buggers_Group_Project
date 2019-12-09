Data validation app summary documentation
Cole Nypaver, Hyeongmin Seo, TJ Rogers (Buggers)
GEOL 590

	This project consisted of the production of a shiny app which uses sample or user input data to construct a linear model, compute an analysis of variance (ANOVA) test, and finally display a series of plots via ggplot. This app was developed as a data evaluation tool with the goal of allowing the user to identify any correlations between their input or sample data and using those correlations to test proposed hypotheses.

	This app is laid out as a series of selectable tabs. The first tab, titled “data visualization” is the default tab that will be open when the user opens the app. Under the data visualization tab, the user will be prompted for input data. This data should be in the form of a CSV file with testable variables in parallel, vertical columns that have column headers as the first observation of each column. If the user does not have input data, the user has the option to choose from a couple of sample data sets (e.g. mtcars, diamonds, and iris), which will come in the form of a drop-down menu labeled ‘Sample Data’. Once the dataset has been uploaded or selected by the user, the data will be displayed as a table in the app so the user may verify that the input data is indeed the desired dataset for testing.  After data upload, the user will select two variables from two drop-down menus labeled ‘Variable 1’ and ‘Variable 2’. These selected variables will then be incorporated into a plot that is selected by the user.  

	The plot types available to the user include a boxplot, a histogram, a bar chart, and a scatter plot. By the very nature of the differing data types that will be selected for input, some number of these plots may not be useful to the users. Therefore, the user will have the option to cycle between and choose the plot that best suits their dataset and the correlations that they are seeking to draw. The selected plot will then be displayed under this tab on the right-hand side of the app. The purpose of this plotting feature in the app lies in the premise that it is often useful to visualize your data to establish qualitative relationships between select variables. This importance of initial qualitative comparison may be better visualized using Anscombe’s quartet which presents four scatter plots with nearly identical summary statistics and radically different data distributions.

	For more quantitative comparisons of the selected data, the app includes a linear model and an analysis of variance (ANOVA) test. These tests allow the user to further assess the quantitative statistical significance of the relationship between their selected variables. Both the linear model and the ANOVA test are ways of predicting the behavior of complex systems by describing the relationship between the independent (x) and the dependent (y) variables. The linear model can be described via the following equation:

					y=β0+∑ βiXi+ϵi

where y represents the dependent variable, Xi represents the independent variables or predictors, β represents linear parameter estimates to be computed, and ϵ represents the error terms. This linear regression is presented to the user in the form of a text output with residuals and coefficients. Among these values are the P-value and the F-statistic; both of which may operate as indicators of statistical significance. The P-value is a measure of selected data compatibility with the null hypothesis and represents the percent chance that the correlation between the variables may happen randomly. A low P-value (<0.05), then, represents a statistically significant relationship whereas a high P-value (>0.05) represents a lack of statistical significance and shows support for the null hypothesis. The F-statistic is the ratio of variance between two variables. Hence, an F-statistic that is larger than one indicates that the variance between variables is larger than the variance within groups and the null hypothesis can be rejected.

	The ANOVA test in this app differs from the linear regression in the type of discrete variable assessed. ANOVA tests treat discrete variables as categorical variables whereas linear regression models treat discrete variables as continuous variables. For example, if this app were used to analyze the relationship between “sepal length” and color in the default Iris dataset, an ANOVA test should be used to assess hypotheses and statistical significance. In this case, “color” is a categorical variable and “sepal length” is continuous. The ANOVA test provides a p-value and an f-statistic but does not provide an R-squared value. In addition, the one-way ANOVA provides a sum of squares and a mean squares value for the assessed variables.

	The ANOVA test in this app can either be a one-way or a two-way ANOVA. If variables 2 and 3 are identical in the app, the ANOVA test will default to a one-way ANOVA test. If variables 2 and 3 are different, the ANOVA test will default to a two-way ANOVA test. The primary difference between the one-way and two-way ANOVA is the number of groups assessed by the test. In the one-way, only a single categorical group is compared to the independent variable. In the two-way ANOVA, the means of two groups are assessed for significance with respect to the independent variable. The total sum of squares value provided by the ANOVA test represents the sum of squares of the deviations of all observations from their means. 

	Lastly, the statistical tests presented in this app provide R-squared values for each regression model. The R-squared value is a percentage representing the amount of variance within the data than can confidently accounted for using the linear model. For example, an R-squared value of 0.9 indicates that 90% of the variance in the data can be accounted for using this model. This model would be considered a good representation of the data behavior. An R-squared value of 0.1 indicates that 10% of the variance within the data can be accounted for using this model. A model with an R-squared value of 0.1 would be considered a poor representation of the presented data behavior.

Instructions for app use:
	1)In the “Data Visualization” tab, upload data in the form of a CSV file OR select 				default data from the “Data:” drop-down menu in the data visualization tab.
	2)Select the variables that you would like to assess using the app in the “Variable 				1”, “Variable 2”, “Variable 3” drop down menus on the data visualization tab. 				Note that Variable 1 and Variable 2 will be used for the ggplot objects and the 			linear model. “Variable 3” will only be used for the ANOVA test.
	3)Note here that if variables 2 and 3 are identical, the ANOVA test will default to a 				one-way ANOVA test. If variables 2 and 3 are different, the ANOVA test will 		default to a two-way ANOVA test
	4)Select the desired plot type and click the “Plot” tab.
	5)Cycle through the plots to see the varying relationships between the selected 				data.
	6)Select the “Linear Model” tab. Note that the text under this tab contains the 				aforementioned statistical values that can be copied/pasted into another text 				editor if desired

The code for this Shiny app, along with all previous versions of the app can be found at the following link: https://github.com/TJrogers86/Buggers_Group_Project

References:
Below are links to codes that we found very useful in the development of this package. Note that some elements of these referenced codes have been adapted and modified for the app presented here.

https://gist.github.com/dgrapov/dfcf6ab2339b1cf19b090cfb8dadc855
https://plot.ly/r/shinyapp-explore-diamonds/
https://www.showmeshiny.com/ggplot-my-data/
https://shiny.as.uky.edu/anova-app/

