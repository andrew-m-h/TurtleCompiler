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

function failtest {
    echo "testing $1 begin"
    stack exec TurtleCompiler -- mytests/$1.t
}

failtest bad-return
failtest comparators
failtest fun-redec
failtest garbage
failtest global-var-redec
failtest local-param-clash
failtest local-var-redec
failtest no-cket
failtest param-redec
failtest trivial-syntax-error
failtest undeclared-fun
failtest vardec-in-body
failtest wrong-param-count
