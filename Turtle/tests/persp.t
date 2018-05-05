turtle perspective

// A simple recursive test

var width
var rate
var x = 200
var y = 300

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

// The recursive function
fun disappear (x, y, width, scale)
  var increment = div (width, scale * 2)
{
  if (0 < width) {
    up
    moveto (x, y)
    down
    moveto (x + width, y)
    disappear (x + increment,
               y + 2*increment,
               width - scale,
               scale) }
}

{
read (width) 				// about 500
read (rate)				// about 20
disappear (x, y, width, rate)
}

