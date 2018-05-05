turtle Twinkle
// Draw a few stars

var posX
var posY
var size

// Draw a single star
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
// Main body of program
{
  posX = 500
  posY = 500
  size = 25
  while (0 < size) {
    star (posX, posY, size)
    posX = posX - 80
    posY = posY - 4*size
    size = size - 4
  }  
}
