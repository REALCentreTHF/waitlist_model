This is a simple model to estimate the elective waitlist over time

Where the activity v_t done in each period t corresponds to the following, where c is the constant of capacity available to do an activity at any period.
v_t=∅_0 c+〖∅_1 c+∅_2 c+⋯+∅〗_i c
In this case, theta is the proportion of activity committed to each group in waiting, where theta is a policy-adjusted variable such that:
∑_(i=0)^i▒〖∅_i〗_t =1
Where the total wait list at any given period is the sum of all waiters plus an additional set of clock starts represented by z_(0_t ) assuming no activity is closed is:
w_t=∑_(i=1)^i▒〖a_(i_t ) z_(i_t ) 〗-∅_i c
In this case, the waitlist at any given period:
w_t= a_0 〖z_(0_t )-∅_0 c+〖a_1 z〗_(1_t )-∅_1 c+〖〖a_2 z〗_(2_t )-∅〗_2 c+⋯+〖a_i z〗_(i_t )-∅〗_i c
The solution for this equation thus depends on which policy solution one is exploring. In this case, assuming a period x set as a breach target (in this case x = 18w).
The number of patients who are in breach in each period is determined by:
b_t=〖[a_(x-1) z_(x-1)〗_(t-1)-∅_(x-1) c]+∑_(i=x)^i▒〖〖a_i z_i〗_t-∅_i c〗
Where the first term represents the ‘recurring’ activity of the backlog, and the second term represents the ‘non-recurring’ term. Should the first term be equal to zero, the backlog remains constant (assuming capacity is sufficient to meet it). Should the second term be zero, the backlog will be eliminated.
Where the wait time is given by:
r_t=(∑_(i=0)^i▒〖〖i(a〗_i z_(i_t )-∅_i c)〗)/|i| 
This analysis will thus estimate:
1. The number of breaches b_t
2. The average wait times r_t
3. The size of the waitlist w_t

