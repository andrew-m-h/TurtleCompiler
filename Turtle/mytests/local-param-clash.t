// Cannot have the same variable name in a local variable
// and param within scope
turtle clash2
       
var x = 1

fun clash(c, d, e)
    var e = 4
    {
	moveto(c, d)
    }


{
clash(1, 2, 3)
}
