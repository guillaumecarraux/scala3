import scala.annotation.lastUse

class A

object Verif:
    def test1 =
        val a = A()

        println(a: @lastUse)
        println(a)    // error

    def test2 =
        val a = A()

        if "hi".length() == 1 then
            println(a: @lastUse)
        else
            println("hello")

        println(a) // error

    def test3 =
        val a = A()

        while true do
            println(a: @lastUse) // error

    def test4 =
        val a = A()
        List(1).foreach(x => println(a: @lastUse)) // error
        println(a)

    def test5 =
        val a = A()
        for x <- 1 to 10 do
            println(a: @lastUse) // error

    def test6 =
        val a = A()

        def f = () => a

        f()

        println(a: @lastUse)

        f() // error

    def test7 =
        val a = A()

        println(a: @lastUse)

        val f = () => a // error

    def test8 =
        val a = A()

        val f = () => a

        println(a: @lastUse)
        f()