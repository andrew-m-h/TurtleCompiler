turtle StarFun
// Draw a star using a function.

var globX = 300

fun star (x, y, scale)
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
//Just draw one for now
{
  star (globX,200,50)
}



