% Linear regression model to predict the profit of a food truck in a city with a 
% given population size
% The file ex1data1.txt contains the dataset for our linear regression prob-
% lem. The first column is the population of a city and the second column is
% the profit of a food truck in that city. A negative value for profit indicates a
% loss.

data = load('ex1data1.txt');
X = data(:,1); y = data(:,2);
m = length(y); % the total of training examples
% setting up the design matrix by adding a column of ones
X = [ones(m,1), X];

theta = zeros(2,1); % initializing the parameters
alpha = 0.01;       % learning rate
n_iters = 1500;     % number of iterations
J_history = zeros(n_iters,1); % vector to plot the cost function

for iter=1:n_iters
  % updating the vector theta to minimizing the cost function
  h = X*theta;
  errors = h-y;
  theta = theta - (alpha/m) * sum(errors.*X)';  
  
  % computing the cost function for each iteration
  sqrErrors = errors.^2;
  J_history(iter) = 1/(2*m) * sum(sqrErrors); 
end
% Predict values for population sizes of 35,000 and 70,000
predict1 = [1, 3.5] *theta;
fprintf('For population = 35,000, we predict a profit of $%f\n', predict1*10000);
predict2 = [1, 7] * theta;
fprintf('For population = 70,000, we predict a profit of $%f\n', predict2*10000);
