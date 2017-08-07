"use strict"

var gpu = new GPU ()

exports.makeK0 = function (opts) {
  return function (body) {
    return gpu.createKernel (new Function (body), opts)
  }
}

exports.makeK1 = function (opts) {
  return function (body) {
    return function (arg1) {
      return function (par1) {
        return gpu.createKernel (new Function (arg1, body), opts)(par1)
      }
    }
  }
}

exports.makeK2 = function (opts) {
  return function (body) {
    return function (arg1) {
      return function (arg2) { 
        return function (par1) {
          return function (par2) {
            return gpu.createKernel (new Function (arg1, arg2, body), opts)(par1, par2)
          }
        }
      }
    }
  }
}

exports.makeK3 = function (opts) {
  return function (body) {
    return function (arg1) {
      return function (arg2) {
        return function (arg3) {
          return function (par1) {
            return function (par2) {
              return function (par3) {
                return gpu.createKernel (new Function (arg1, arg2, arg3, body), { dimensions: [100,2,2]} ) (par1, par2, par3)
              }
            }
          }
        }
      }
    }
  }
}

exports.makeK4 = function (opts) {
  return function (body) {
    return function (arg1) { 
      return function (arg2) {
        return function (arg3) {
          return function (arg4) {
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

exports.makeK5 = function (opts) {
  return function (body) {
    return function (arg1) { 
      return function (arg2) {
        return function (arg3) {
          return function (arg4) {
            return function (arg5) {
              return function (par1) {
                return function (par2) {
                  return function (par3) {
                    return function (par4) {
                      return function (par5) {
                        return gpu.createKernel (new Function (arg1, arg2, arg3, arg4, arg5, body), opts) (par1, par2, par3, par4, par5)
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
  }
}
