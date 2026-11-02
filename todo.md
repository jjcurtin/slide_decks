Add alcohol lagged models
calibration plots for AUD?
Then maybe GPS - can talk about AUD
Then fairness AUD followed by OUD
Then global feature importance.   Followed by importance by day.
Move personalized reocmmendations to end before Optimize






After we present the EMA models for AUD, we can then talk about their calibration (I believe I have a calibration plot somewhere). Then we can talk about the lagged AUD models where we predict into the future. Then we can intropduce the benefits of GPS as a second bucket conceptually and then with GPS findings from the AUD sample. We have a draft of a paper and analyses for those findings in the ~/github/study_gps repo. After that, we can introduce the idea of fairness and show the problems with the AUD sample and the improvements in the OUD sample. Then maybe we can talk about global feature importance for the AUD and OUD models. We can then talk about personalized recommendations and end with the optimize grant. 


How to tell the GPS story honestly. In AUD, GPS alone gives 0.72. In OUD, adding GPS to daily surveys adds nothing to accuracy (0.94 vs. 0.94). Yet location features rank among the OUD top 10 when you count how often each feature is the top driver of a day's prediction. So the consistent message is "GPS is lower burden and brings in different risk factors you can act on," not "GPS improves prediction." The feature-importance section (row 6) is a natural place to deliver that payoff. Whether to mention the OUD ablation result outright is your call; someone in Q&A may ask what GPS adds on top of EMA.


Calibration for both studies? fig_cal plots the AUD model's beta-calibrated probabilities. The OUD model is well calibrated without that adjustment (Brier 0.014). You could show AUD only, as you proposed, or add OUD briefly in the OUD section.


Communications sensing. The "second kind of sensing" slide in row 4 is the natural place to mention it briefly as a future direction

Maybe also talk about Rudra APP.   Algorithm native in DTx


Where to introduce the problems with current DTx

- Poor self-monitoring so do it by algorithm AND teach it to patient
- Personalized support based on self-monitoring
