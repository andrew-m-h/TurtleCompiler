function test {
    echo "testing $1 begin"
    stack exec TurtleCompiler < tests/$1.t | diff -s - tests/$1.p
}

for i in `seq 0 4` 
do
    test "test$i"
done;

test divtst
test iftest
test koch
test nested
test persp
test readtst
test ret
test rettst
test starfun
test star
test twinkle

