turtle divtest


var degree
var x = 200
var y = 400
var length = 360 

// A rounding division function
fun div (dividend, divisor)
  var quotient = 0
{
  while (divisor < dividend) {
    quotient = quotient + 1
    dividend = dividend - divisor
  }
  if (dividend + dividend < divisor) {
    return quotient }
  else {
    return quotient + 1 }
}

// Another necessary calculation ...
fun root3on2 (x) {
  return div (x * 886, 1000)
}

fun const (x) {return 100}

{
//degree = div (length,2)
down
moveto(x,y)
moveto (x-const(y),y+const(x))
}



