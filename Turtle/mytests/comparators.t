turtle comparators
// Demonstates the new comparators added as syntactic sugar

{ 
  up()
  moveto(1, 1)	
  down()		
  if (1 != 2) {
    // draw a vertical line
    moveto(1, 10)
  } else {
    // draw a horizontal line
    moveto(10, 1)
  }

  up()
  moveto(40, 1)	
  down()		
  if (1 > 1) {
    // draw a vertical line
    moveto(40, 10)
  } else {
    // draw a horizontal line
    moveto(50, 1)
  }

  up()
  moveto(80, 1)	
  down()		
  if (1 >= 1) {
    // draw a vertical line
    moveto(80, 10)
  } else {
    // draw a horizontal line
    moveto(90, 1)
  }

  up()
  moveto(120, 1)	
  down()		
  if (1 <= -5) {
    // draw a vertical line
    moveto(120, 10)
  } else {
    // draw a horizontal line
    moveto(130, 1)
  }
   
}
