## Welcome to my GitHub Repository

Below are  select data science projects that have piqued my interest over time. Most of them are open sourced. 

## [Financial Forecasting](https://github.com/tushark26/DS_Portfolio/tree/main/FinancialForecast) 
#### Test vs Train Methodology
Splitting data between Test & Training timeframe after after calibrating for outliers 
<img src="FinancialForecast/images/Train vs Test.png" style="width:750px;height:200px;">

#### Variability Matrix 
Over time, we notice there are certain segments that are predictable vs others. The volatile models may require bottom up or assumption based forecasts instead of relying solely on algorithmic forecasts. 

<img src="FinancialForecast/images/Variability Matrix.png" style="width:400px;height:300px;">


## [Predicting Home Prices](https://github.com/tushark26/DS_Portfolio/tree/main/Linear%20Regression) 
#### Check data for outliers 
<img src="Linear Regression/images/Price Distb Outliers.png" style="width:500px;height:250px;">

#### Sale Price vs Area
Plotting Sale Price by Total Area layered with House Style (# of story) and Year Built gives a very interesting pattern. Controlling for age i.e. houses built between 1990's-2010 the houses with 2 stories (and hence more area) tend to have a higher price than houses with 1 story or Single levels.
<img src="Linear Regression/images/Price vs Area Distb.png" style="width:500px;height:250px;">

#### Quality of Construction
The quality of construction plays a role in the sale price as well. Plotting construction quality (1-10) against salePrice indicates that majority of the homes have a quality score between 5-8 and sale price from 133k-270k. This is another good feature to consider for prediction.
<img src="Linear Regression/images/Box Plots.png" style="width:500px;height:250px;">


## [Transfer Learning](https://github.com/tushark26/DS_Portfolio/tree/main/CV) 

#### Core Idea
Two images are used in performing neural style transfer. One image is known as the content image while the other is known as the style image. The content image is the image on which we want to apply the texture or style. The style image is the image from which the texture or texture is extracted and transferred over to the content image to produce a stylized image as output

<img src="CV/Art_Generation_with_Neural_Style_Transfer/images/louvre_generated.png" style="width:750px;height:200px;">

<img src="CV/Art_Generation_with_Neural_Style_Transfer/images/NY Van Gogh.png" style="width:750px;height:200px;">
