
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pricing\_game\_submit

This repo contains the code I used in the [AIcrowd Insurance Pricing
Games 2021
competition.](https://www.aicrowd.com/challenges/insurance-pricing-game)

With this solution, I finished with a market share of 19% and a profit
of 90108. That secured me the second place of this competition with over
200 teams.

Here are some extracts of the write-up I did on the competition forum.

# KPI and model selection

Like most participants, I did not look at the RMSE leaderboard, I merely
looked at tweedie deviance.

My main tool to decide if a model was good whas to replicate the
competition framework with different models. Of course, it is impossible
to replicate the variety of all participants model but I tried a few
simple models and algorithms that I did not want to use and just use
them to compete with my chosen models. Then, I simulated a perfect
market like in the competition and looked at the best loss-ratios. Not
profit, since that depends too much on the chosen profit-margin. I also
did a lot of simulations where the profit-margin was random to help me
decide the model and the margin.

# Chosen models

About models, like I said I created a lot of them, mostly xgboost and
GAMs (tweedie, freq/sev, by-coverage model, …). I thought about
ensembling them but discarded the idea by fear of not subscribing
enough. On my simulation, my ensembled model had really low market share
(even after removing the models used in the ensemble) and I thought that
you need around 15% to perform well.

I was obviously wrong since the top participants shared mostly stacked
solutions!

Like some other participants, I used a different model for the renewal
(the 60K risks in the training data) and for the new businesses.

The renewal model was evaluated on a train/test (year 1-3 for train,
year 4 for test), with past claims features.

The new business model was evaluated on a 5 fold cross validation
framework.

Final renewal model is a xgboost freq+sev. Final new business model is a
tweedie-GAMM.

# Feature Engineering and model loading

I did not spend much time on feature engineering (probably not enough).

The feature that gave me headaches was vh\_make\_model. I tried target
encoding, I tried embedding, I tried (and kept mixed models), but all
these methods were too conservative and did not surcharge the high risk
enough.

Then, around week 7 (I joined late and am not that smart), I realized
that I do not care about getting a good estimate of all risks, I just
don’t want to under price a risk. So I added a post-model correction for
two features, vh\_make\_model and city. I looked at the model residuals
by modality, then applied a multiplicative correction with the ratio
claim\_amount / prediction. Of course, I want to be more conservative
about discount that surcharge, so I ended with no discount (0
credibility for vehicle with less claims than expected) and full
surcharge (full credibility for vehicles with more claims than
expected).

Doing that, I knew that there was a significant portion of the portfolio
that I could not subscribe, so I reduced my margin for the others to
still get around my target of 15% (I ended with 19%). For week 10, I was
4th, with only 5%, I felt it would not be enough, hence the margin
reduction.

# Conclusion

Like I said, my solution is really simple, the asymmetric correction for
`vh_make_model` and `city` is a very simple idea but it seems to have
made a huge difference performance-wise.
