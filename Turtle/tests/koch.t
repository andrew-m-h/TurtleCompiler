turtle Koch

// The Koch Curve fractal:  _/\_
//
// Input: The degree of the fractal
//        (keep it small, max 3 or go make a cuppa...)
//

var degree
var x = 200
var y = 400
var length = 360 // easy to divide by 3 a few times

// A rounding division function
fun div (dividend, divisor)
  var quotient = 0
{
  while (divisor < dividend) {
    quotient = quotient + 1
    dividend = dividend - divisor
  }
  if (dividend + dividend < divisor) {
    return quotient 
  } else {
    return quotient + 1
  }
}

// Another rough calculation ...
fun root3on2 (x) {
  return div (x * 89, 100)
}

// And another ...
fun root3on6 (x) {
  return div (x * 29, 100)
}

// Now the fractal function ...
// angle is 0 for _, 1 for / and -1 for \

fun kochx (length, x, y, angle)
  var endx
  var halfLength = div (length, 2)
{
		if (angle == 0) {
      			endx = x + length
    		} else { if (angle == 1) {
      			endx = x + halfLength
      		} else { if (angle == 2) {
      			endx = x - halfLength
    	  	} else { if (angle == 3) {
      			endx = x - length
		} else { if (angle == -1) {
			endx = x + halfLength
    		} else { if (angle == -2) {
      			endx = x - halfLength
    		}}}}}}
    		return endx
}

fun kochy (length, x, y, angle)
  var endy
  var vertOffset = root3on2 (length)
{
		if (angle == 0) {
      			endy = y
    		} else { if (angle == 1) {
      			endy = y + vertOffset
      		} else { if (angle == 2) {
			endy = y + vertOffset
    		} else { if (angle == 3) {
      			endy = y
		} else { if (angle == -1) {
			endy = y - vertOffset
    		} else { if (angle == -2) {
      			endy = y - vertOffset
    		}}}}}}
    		return endy
}

fun fixAngle (angle) {
	while (angle < -2) {
		angle = angle + 6
	}
	while (3 < angle) {
		angle = angle - 6
	}
	return angle
}

fun koch (degree, length, x, y, angle)
  var endx
  var endy
  var thirdLength
  var halfLength = div (length, 2)
  var thirdOffset
{
	up
	moveto (x,y)
	if (degree == 0) {
		endx = kochx(length, x, y, angle)
		endy = kochy(length, x, y, angle)
    		down
    		moveto (endx, endy)
	} else { // degree != 0
		thirdLength = div (length, 3)
		thirdOffset = root3on6 (length)
		angle = fixAngle(angle)
		koch (degree-1, thirdLength, x, y, angle)
		endx = kochx(thirdLength, x, y, angle)
		endy = kochy(thirdLength, x, y, angle)
		x    = endx
		y    = endy
		angle = fixAngle(angle+1)
		koch (degree-1, thirdLength, x, y, angle)
		endx = kochx(thirdLength, x, y, angle)
		endy = kochy(thirdLength, x, y, angle)
		x    = endx
		y    = endy
		angle = fixAngle(angle-2)
		koch (degree-1, thirdLength, x, y, angle)
		endx = kochx(thirdLength, x, y, angle)
		endy = kochy(thirdLength, x, y, angle)
		x    = endx
		y    = endy
		angle = fixAngle(angle+1)
		koch (degree-1, thirdLength, x, y, angle)
	}
}

// try it easy first...

{
read (degree)
koch (degree,length,150,200,0)
}

