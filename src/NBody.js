"use strict"

exports.animate = function (draw) {
  return function (step) {
    return function again (sys) {
      return function () {
        requestAnimationFrame (again (step (sys)))
        draw (sys) ()
      }
    }
  }
}
