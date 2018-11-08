# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2018

+ Team 5
+ Team members
	+ Jiayi Cui (jc4884)
	+ Zhongce Ji (zj2225)
	+ Samuel Kolins (sk3651)
	+ Sheng Wang (sw3224)
	+ Jiaqian Yu (jy2880)

+ Project summary: In this project, we created a classification engine for enhance the resolution of images.

  + Our goal is to use machine learning method to enhance the resolution of blurry and low-resolution images so that we can get high resolution images.

	+ Our baseline model is GBM Gradient Boosting Machines with depth equals to 1. Then we use XGBoost to improve our model by tuning different parameters, we use cross validation to find the best parameter, so the best depth is 4, and best nrounds is 100.
	
	+ Another improve model we have is 5 by 5 features, so we change the feature size so that more information will be included, and we also use use XGBoost and cross validation to find the best parameter.
	
	+ For the baseline model(GBM), the main file is GBM_Baseline, and also the function used in lib/GBM_Baseline folder.	
	
	+ For the XGBoost with 3 by 3 feature, the main file is XGB_3_3, and also the function used in lib/XGB folder with feature.R and superresolution.R.
	
	+ For the XGBoost with 5 by 5 feature, the main file is XGB_5_5, and also the function used in lib/XGB folder with feature2.R and superresolution2.R. (However, the result for XGBoost is not so good, we think it is due to the equal weight of all the 24 features. So we think using kernel function to give weights for different features can be explored in the future.)
	
+ Our final selected model is XGBoost with 3 by 3 features. And the errors are: MSE around 0.00025 and PSNR around 27.

**Contribution statement**: ([default](doc/a_note_on_contributions.md)) 

  + Baseline Model: Zhongce Ji(major contributor), Jiaqian Yu(major contributor)
  + Improved Model:
       + XGBoost and parameter tunning: Zhongce Ji
       + Improve Feature and Speed: Jiaqian Yu
       + SRCNN model (not selected as improved): Jiayi Cui(equal contributor)
       + Model implementation: Sheng Wang (equal contributor) 
  + Tensorflow bilinear interpolation algorithm: Samuel Kolins(little contributor)
  + Presentation and PPT: Sheng Wang
  + All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
