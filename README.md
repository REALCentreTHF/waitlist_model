# Elective waitlist model

This is a simple time-dependant model that models how many people are on the waitlist, how long they've been waiting, at any point in time provided a stream of projected elective referrals. Though the skeletal model is rather rudimentary, it is here to provide a basic approach to elective waitlist modelling and can ideally be improved upon to account for additional complexity regarding time to treatment, severity, and patient severity stratification.

## How does this model work?

This is a simple model to estimate the elective waitlist over time where the activity $x_t$ done in each period t corresponds to the following, where $c$ is the constant of capacity available to do an activity at any period. Under this model, capacity is assumed constant at every period, but can be easily editted to either include manual inputs or a fixed function (Eg: assuming 3% annual growth in capacity or whatever)

$x_t = \uptheta_1* x_1 + ... + \uptheta_{i_t} * x_{i_t}$ 

In this case, theta is the proportion of activity committed to each group in waiting, where theta is a policy-adjusted variable such that it sums up to 1. In this case, the wait list is given by: 

$w_t = \sum_{i=0} a_{i_t}*x_{i_t} - \uptheta_{i_t} *c$

Where a represents the proportion of waiters that drop off at every given period t based on how long they've been waiting.

## What does this model estimate?

This analysis can thus derive three key variables:

1. The size of the waitlist $w_t$ at any given point in time t
2. The average wait times $r_t$ at any given point in time
3. The proportion of the waitlist that has breached its 18+ week (or any) target $b_t$

## Diagnostics

A basic examination using data from Apr21 till now reveals our model matches what happened in reality rather will, with minor variations and differences likely due to the probablistic nature of guessing how many will 'drop off' the wait list at any given point and the variation in capacity (which here we assumed constant).

![Alt Text](https://github.com/zeyadissa/WaitlistModel/blob/main/res/diagnostic.gif)

## COVID-19 analog: a model of a huge uptick in referrals

Tentative results are positive: here we model a massive shock to the system (in this case, being an exxagerated scenario of COVID-19 where in one month 10 million new clocks started. For the sake of suspension of disbelief, please invent your own disease name)

![Alt Text](https://github.com/zeyadissa/WaitlistModel/blob/main/res/animation.gif)

Crucially we see the impact on average wait times and the proportion of breaches (Which shoot up from 30 to 60%). This is very rudimentary, but it showcases how a shock to the system can break it in the long-term, with no sign of recovery even 2 years down the line (at best, stagnation...)
