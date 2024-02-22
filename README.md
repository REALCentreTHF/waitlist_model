This is a simple model to estimate the elective waitlist over time where the activity $x_t$ done in each period t corresponds to the following, where $c$ is the constant of capacity available to do an activity at any period. Under this model, capacity is assumed constant at every period, but can be easily editted to either include manual inputs or a fixed function (Eg: assuming 3% annual growth in capacity or whatever)

$x_t = \uptheta_1* x_1 + ... + \uptheta_{i_t} * x_{i_t}$ 

In this case, theta is the proportion of activity committed to each group in waiting, where theta is a policy-adjusted variable such that it sums up to 1. In this case, the wait list is given by: 

$w_t = \sum_{i=0}^{\i} a_{i_t}*z_{i_t} - \uptheta_{i_t} *c$

Where a represents the proportion of waiters that drop off at every given period t based on how long they've been waiting.

This analysis can thus derive three key variables:

1. The size of the waitlist $w_t$ at any given point in time t
2. The average wait times $r_t$ at any given point in time
3. The proportion of the waitlist that has breached its 18+ week (or any) target $b_t$

Tentative results are positive: here we model a massive shock to the system (in this case, being an exxagerated scenario of COVID-19 where in one month 10 million new clocks started. For the sake of suspension of disbelief, please invent your own disease name)

![Alt Text](https://media.giphy.com/media/vFKqnCdLPNOKc/giphy.gif)
