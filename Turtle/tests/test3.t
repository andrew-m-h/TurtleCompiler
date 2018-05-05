turtle test3
// test3.p -- test simple function
fun line (x1, y1, x2, y2) {
  up
  moveto (x1,y1)
  down
  moveto (x2,y2)
  up
}

{ line (200,600,600,200)
  line (200,200,600,600)
}
 