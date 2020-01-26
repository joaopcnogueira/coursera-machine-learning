x = rand(90,1);
y = 2 *  x;

m = length(y);

x = [ones(m,1), x];
%%% finding theta by using normal equation
% theta = pinv(x'*x)*x'*y;

%%% finding theta by using batch gradient descent
theta = zeros(2,1);
n_iters = 1000;
alpha = 0.1;

costJ = zeros(n_iters,1);

for iter=1:n_iters
  h = x * theta;
  errors = h - y;
  %theta = theta - (alpha/m) * sum(errors .* x)';
  theta = theta - (alpha/m) * (x' * errors);
  
  costJ(iter) = (1/(2*m)) * errors' * errors;
endfor
plot(costJ);