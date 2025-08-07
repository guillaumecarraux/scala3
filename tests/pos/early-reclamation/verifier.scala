import scala.annotation.lastUse

class Obj{}



object Verif:

    private def use(o: Obj): Unit =
        val x = o


    def test1 =
        val o = Obj()

        use(o)
        val y = o
        use(o)
        use(o: @lastUse)

    def test2 =
        val o = Obj()

        use(o)
        if "hi".length() == 2 then
            use(o: @lastUse)
        else
            if "hello".length == 3 then
                use(o)
            else
                use(o:  @lastUse)

    // what else ???