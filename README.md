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

	+ Our baseline model is GBM Gradient Boosting Machines with depth equals to 1. Then we use XGBoost to improve our model by tuning different parameters, we use cross validation to find the best parameter, so the best depth is 7, and best nrounds is 200.
	
	+ Another improve model we have is 5 by 5 features, so we change the feature size so that more information will be included, and we also use use XGBoost and cross validation to find the best parameter.
	
	+ For the baseline model(GBM), the main file is GBM_Baseline, and also the function used in lib/GBM_Baseline folder.	
	
	+ For the XGBoost with 3 by 3 feature, the main file is XGB_3_3, and also the function used in lib/XGB folder.
	
	+ For the XGBoost with 5 by 5 feature, the main file is XGB_5_5, and also the function used in lib/XGB folder, the feature function we new feature2 and superresolution function we use superresolution2
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) 

  + Baseline Model: Zhongce Ji(main contribution), Jiaqian Yu(main contribution)
  + Improved Model:
       XGBoost part: Zhongce Ji
       Improved Feature part: Jiaqian Yu
       CNN part: Jiayi Cui(equal contribution)
       Model implementation and parameter tunning: Sheng Wang (equal contribution) 
  + Tensorflow bilinear interpolation algorithm: Samuel Kolins(little contribution)
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
