"use strict"

var ctx = document.getElementById ("canvas").getContext("2d")

function scale (x) { return 0.1 * Math.log (x) }

function draw (body, mass) {

  ctx.beginPath ()
  ctx.arc (body [0][0], body [0][1], scale (mass) , 0, 6.29)
  ctx.stroke ()
}

function clear () {
  ctx.clearRect (0, 0, 1300, 1300)
}
  

exports.animate = function (step) {
  return function again (sys) { 
    return function () {
      clear()
      console.log (sys)

      requestAnimationFrame (again (step (sys)))
      for (var b = 0; b < sys.coords.length; b++)
        draw (sys.coords [b], sys.masses [b])
    }
  }
}
