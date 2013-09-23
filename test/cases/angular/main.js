// plugin=angular
// environment=browser
// environment=jquery
// loadfiles=config.js, filters.js

angular.module('sample', ['ngResource', 'sample.config', 'sample.filters'])

.run(function($scope, someNumber) {
  someNumber; //: number
  $scope.myName; //: string
  $scope.myConfig; //: {myColor}
  $scope.version; //: string
  $scope.user.finally; //: fn(callback: fn()) -> Promise
})

.controller('GreetingCtrl', ['$scope', 'User', 'myConfig', 'version', function($scope, User, myConfig, version) {
  $scope.myName = 'John';
  $scope.myConfig = myConfig;
  $scope.version = version;

  $scope.user = User.get({login: 'sqs'});
}])

.constant('version', 'v1.2.3')

.factory('User', ['$resource', function($resource) {
  return $resource('https://api.github.com/users/:login');
}])

;
