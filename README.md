This is a simple model to estimate the elective waitlist over time where the activity $x_t$ done in each period t corresponds to the following, where $c$ is the constant of capacity available to do an activity at any period. Under this model, capacity is assumed constant at every period, but can be easily editted to either include manual inputs or a fixed function (Eg: assuming 3% annual growth in capacity or whatever)

$x_t = \uptheta_1* x_1 + ... + \uptheta_i_t * x{i{t}$ 

In this case, theta is the proportion of activity committed to each group in waiting, where theta is a policy-adjusted variable such that it sums up to 1. In this case, the wait list is given by: 

$w_t = \sum(a_i_t*z_i_t - \uptheta_i_t *c)$
