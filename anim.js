var gpu = new GPU ();
var step = gpu.createKernel (function (masses, positions, velocities) {
    var f1 = 0.0;
    var f2 = 0.0;
    var d1;
    var d2;
    var r;
    var pf = 0.0;
    if (this.thread.y == 0.0) {
        for (var body = 0; body <= 100; body++) {
            if (body == this.thread.x) {
                break;
            } else {
                d1 = positions[body][0.0] - positions[this.thread.x][0.0];
                d2 = positions[body][1.0] - positions[this.thread.x][1.0];
                r = Math.sqrt(Math.pow(d1, 2.0) + Math.pow(d2, 2.0));
                d1 = d1 / r;
                d2 = d2 / r;
                pf = masses[body] * masses[this.thread.x] * 6.674e-11 / Math.pow(r, 2.0);
                f1 = f1 + d1 * pf;
                f2 = f2 + d2 * pf;
            };
            if (this.thread.z == 0.0) {
                return 0.03 * f1 / masses[this.thread.x];
            } else {
                return 0.03 * f2 / masses[this.thread.x];
            };
        };
    } else {
        if (this.thread.z == 0.0) {
            return positions[this.thread.x][0.0] + velocities[this.thread.x][0.0];
        } else {
            return positions[this.thread.x][1.0] + velocities[this.thread.x][1.0];
        };
    };
}, {dimensions: [100,2,2]})

var masses = []
  , velocities = []
  , positions = []

rand = function (max) { return Math.random() * max }

for (var i = 0; i < 100; i++)
{ masses [i] = rand (1e4)
  velocities [i] = [rand (6), rand (6)]    
  positions [i] = [rand (800), rand (600)]
}

draw = function (ps) {
  for (var i = 0; i < ps.length(); i++)
    console.log (ps [i])
}

animate = function (draw) {
  return function again (ps, vs) {
    var nsys = step (masses, ps, vs)
    var nps, nvs;
    for (var i; i < nsys.length(); i++) {
      nvs [i] = nsys [i] [0]
      nps [i] = nsys [i] [1]
    }
    //requestAnimationFrame (again (nps, nvs))
    draw (ps)
  }
} (draw) (positions, velocities)
