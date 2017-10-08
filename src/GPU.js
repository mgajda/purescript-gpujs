"use strict"

var gpu = new GPU ()

exports.makeK0 = function (opts) {
  return function (body) {
    return gpu.createKernel (new Function (body), opts)
  }
}

exports.makeK1 = function (opts) {
  return function (arg1) {
    return function (body) {
      return function (par1) {
        return gpu.createKernel (new Function (arg1, body), opts)(par1)
      }
    }
  }
}

exports.makeK2 = function (opts) {
  return function (arg1) {
    return function (arg2) { 
      return function (body) {
        console.log (body)
        var kernel = gpu.createKernel (new Function (arg1, arg2, body), opts)
        return function (par1) {
          return function (par2) {
            return kernel (par1, par2)
          }
        }
      }
    }
  }
}

exports.makeK3 = function (opts) {
  return function (arg1) {
    return function (arg2) {
      return function (arg3) {
        return function (body) {
          var kernel = gpu.createKernel (new Function (arg1, arg2, arg3, body), opts )
          return function (par1) {
            return function (par2) {
              return function (par3) {
                return kernel (par1, par2, par3)
              }
            }
          }
        }
      }
    }
  }
}

exports.makeK4 = function (opts) {
  return function (arg1) { 
    return function (arg2) {
      return function (arg3) {
        return function (arg4) {
          return function (body) {
            return function (par1) {
              return function (par2) {
                return function (par3) {
                  return function (par4) {
                    return gpu.createKernel (new Function (arg1, arg2, arg3, arg4, body), opts)(par1, par2, par3, par4)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
