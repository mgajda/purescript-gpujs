"use strict"

exports.clearSvg = function () {
  return function () {
    var node = document.getElementById ("mySVG")
    while (node.firstChild) {
      node.removeChild (node.firdsChild)
    }
  }
}

var svgNS = "http://www.w3.org/2000/svg";

exports.putCircle = function (body) {
  return function () {
    var myCircle = document.createElementNS(svgNS,"circle");
    myCircle.setAttributeNS(null,"cx",body.q [0]);
    myCircle.setAttributeNS(null,"cy",body.q [1]);
    myCircle.setAttributeNS(null,"r",6);
    myCircle.setAttributeNS(null,"fill","black");
    myCircle.setAttributeNS(null,"stroke","none");

    document.getElementById("mySVG").appendChild(myCircle);
  }
}
