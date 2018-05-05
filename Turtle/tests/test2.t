turtle test2
// File test2.p -- test read from input
// Data test2.d
var startX = 300
var startY = 200
{ up
  moveto (startX, startY)
  down
  read (startX) //100
  read (startY) //400
  moveto (startX, startY)
}