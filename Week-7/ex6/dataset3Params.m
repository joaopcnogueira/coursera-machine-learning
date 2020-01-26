function [C, sigma] = dataset3Params(X, y, Xval, yval)
%DATASET3PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = DATASET3PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

min_error = 15628; % just a big number

for C_test = [0.01 0.03 0.1 0.3 1 3 10 30]
  for sigma_test = [0.01 0.03 0.1 0.3 1 3 10 30]
    model = svmTrain(X, y, C_test, @(x1,x2) gaussianKernel(x1, x2, sigma_test));
    predictions = svmPredict(model, Xval);
    predictions_errors = mean(double(predictions ~= yval));
    
    if predictions_errors < min_error
      min_error = predictions_errors;
      C = C_test;
      sigma = sigma_test;
    end
end


%%% Another implementation

%results = eye(64,3);
%pos = 0;
%min_error = 15628; % just a big number
%
%for C_test = [0.01 0.03 0.1 0.3 1 3 10 30]
%  for sigma_test = [0.01 0.03 0.1 0.3 1 3 10 30]
%    model = svmTrain(X, y, C_test, @(x1,x2) gaussianKernel(x1, x2, sigma_test));
%    predictions = svmPredict(model, Xval);
%    predictions_errors = mean(double(predictions ~= yval));
%    
%    pos = pos + 1;
%    results(pos,:) = [C_test, sigma_test, predictions_errors];
%    
%  end
%end
%
%sorted_results = sortrows(results, 3); 
%
%C = sorted_results(1,1);
%sigma = sorted_results(1,2);





% =========================================================================

end
