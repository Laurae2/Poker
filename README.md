# Poker

This is the source code for a solution scoring 0.85 on the Poker dataset.

Please see the following link for the notebook with descriptions: https://rawgit.com/Laurae2/Poker/master/Poker_Workbook.html

* Features used: 37
* Computation time (i7-4600U, 16GB RAM): approx. 1 hour for 0.805-0.805 Public/Private LB (you can achieve 0.8506-0.8503 Public/Private LB in less than 10 minutes using the feature engineering and XGBoost alone)
* Models used: Extreme Gradient Boosting, Random Forest, Gradient Boosted Machines, Deep Learning / MLP

N.B: Power 4 Magic weighted models coming from a blackbox. Your mileage may vary due to computation determinism and multithreading.

   IA Model / Supervised ML   | 5-fold Cross-Validation | Public LB | Private LB
----------------------------- | ----------------------- | --------- | ----------
Extreme Gradient Boosting     |    0.850154+0.002617    | 0.8506203 |  0.8503818
        h2o Random Forest     |    0.847355+0.001501    | 0.8473699 |  0.8468554
h2o Gradient Boosted Machines |    0.848027+0.001419    | 0.8485799 |  0.8485444
      h2o Deep Learning / MLP |    0.843473+0.001568    | 0.8429358 |  0.8432577

   Ensemble Type              | Public LB | Private LB
----------------------------- | --------- | ----------
         Best model (XGBoost) | 0.8506203 |  0.8503818
       Average of all models  | 0.8487509 |  0.8485156
       Power 2 of all models  | 0.8492364 |  0.8489594
       Power 4 of all models  | 0.8500361 |  0.8497416
       Power 8 of all models  | 0.8502813 |  0.8500659
      Power 16 of all models  | 0.8502647 |  0.8501988
     Power 12 weighted models | 0.8506915 |  0.8503820
Power 4 Magic weighted models | 0.8505044 |  0.8505464

Garbage collect information at the end (notice: approx. 2GB max usage, can go under 2GB if cleaning up all data everytime):

       |      used |   (Mb) | gc trigger |   (Mb) |  max used |   (Mb)
------ | --------- | ------ | ---------- | ------ | --------- | ------
Ncells |   1444124 |   77.2 |    4397788 |  234.9 |   5684620 |  303.6
Vcells | 180365034 | 1376.1 |  267283192 | 2039.3 | 267271001 | 2039.2
