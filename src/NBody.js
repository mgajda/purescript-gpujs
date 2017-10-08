"use strict"

var ctx = document.getElementById ("canvas").getContext("2d")

var draw = function (body) {
  ctx.beginPath ()
  ctx.arc (body[0][0], body [0][1], 6, 0, 6.29)
  ctx.stroke ()
}
var clear = function () {
  ctx.clearRect (0, 0, 1300, 800)
}
  

exports.animate = function (step) {
  return function again (sys) { 
    return function () {
      clear()
      console.log (sys)
      setTimeout (again (step (sys)), 300)
      sys.coords.forEach (draw) 
    }
  }
}

      

