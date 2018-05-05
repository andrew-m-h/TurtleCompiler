turtle divoperator
//Demonstrates the new division operator

{ 
  up()
  moveto(100, 100)	
  down()
  moveto(100, 100 + 100)	

  //make second line half as tall as first line
  up()
  moveto(110, 100)	
  down()
  moveto(110, 100 + (100 / 2))	

}
