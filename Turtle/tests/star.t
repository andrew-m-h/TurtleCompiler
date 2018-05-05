turtle Star
// Draw a star.

var x = 300
var y = 200
var scale = 50
var topY = y +8*scale
{
  up
  moveto (x, y)
  down
  moveto (x+3*scale, y+6*scale)
  moveto (x-3*scale, y+6*scale)
  moveto (x, y)
  up
  moveto (x, topY)
  down
  moveto (x+3*scale, y+2*scale)
  moveto (x-3*scale, y+2*scale)
  moveto (x, topY)
  up
}




