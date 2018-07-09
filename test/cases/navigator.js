// environment=browser

navigator; //: navigator

navigator.onLine; //: bool

navigator.permissions.query({name: 'geolocation'}).then(function (status) {
  status; //: PermissionStatus
})
